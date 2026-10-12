;;;; agent-wire.lisp — the agent's wire: Server-Sent Events.
;;;;
;;;; doc/agent.org §3 stacks an LLM client as transport → wire format → protocol →
;;;; tools. The transport is the networking track's HTTP-REQUEST (initramfs/http.lisp:
;;;; https through the host's libssl, chunked bodies, an ON-CHUNK callback that hands
;;;; the body over as it arrives). The wire format is JSON (json.lisp) and this:
;;;;
;;;;   SSE FRAMING. A streamed Messages-API reply is not one JSON document but a
;;;;   text/event-stream: lines of "event: NAME" and "data: JSON", each event ended by
;;;;   a blank line (the WHATWG "server-sent events" format, the one a browser's
;;;;   EventSource reads). SSE-FEED takes the body's bytes AS THEY ARRIVE — in
;;;;   arbitrary pieces, a line or a multi-byte character cut anywhere — and calls back
;;;;   once per complete event. Pure; host-tested (poc/agent-test.lisp).
;;;;
;;;; Until HTTP-REQUEST landed, this file also carried a plain-http stand-in with the
;;;; same signature, so the agent could be written and tested in parallel; it was
;;;; removed once the real client was in the image (doc/agent.org §14.1).
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

;;; ===========================================================================
;;; 1. Server-Sent Events
;;; ===========================================================================
;;;
;;; The format, in full (it is small): the stream is UTF-8 lines, ended by LF, CRLF
;;; or a lone CR. A line "field: value" (one optional space after the colon is
;;; dropped) sets a field; the fields that matter are EVENT (the event's name) and
;;; DATA (its payload — several data lines in one event are joined with newlines).
;;; A line starting with ":" is a comment (servers use it as a keep-alive). A BLANK
;;; line dispatches the event built so far and starts the next one.

(defstruct (sse-parser (:constructor make-sse-parser (on-event)))
  "Incremental SSE state. ON-EVENT is called as (funcall on-event NAME DATA)."
  on-event
  (line (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (after-cr nil)        ; the previous byte was CR: a following LF belongs to the same break
  (event nil)           ; the EVENT field so far, or NIL (= "message")
  (data '()))           ; DATA lines so far, newest first

(defun sse-line (parser bytes)
  "One complete line (its bytes, terminator removed)."
  (let ((line (sb-ext:octets-to-string bytes :external-format :utf-8)))
    (cond
      ((zerop (length line))                       ; blank: dispatch
       (when (sse-parser-data parser)
         (funcall (sse-parser-on-event parser)
                  (or (sse-parser-event parser) "message")
                  (format nil "~{~a~^~%~}" (reverse (sse-parser-data parser)))))
       (setf (sse-parser-event parser) nil (sse-parser-data parser) '()))
      ((char= (char line 0) #\:) nil)              ; a comment / keep-alive
      (t
       (let* ((colon (position #\: line))
              (field (if colon (subseq line 0 colon) line))
              (value (if colon
                         (let ((v (subseq line (1+ colon))))
                           (if (and (plusp (length v)) (char= (char v 0) #\Space)) (subseq v 1) v))
                         "")))
         (cond ((string= field "event") (setf (sse-parser-event parser) value))
               ((string= field "data") (push value (sse-parser-data parser)))
               ;; id / retry matter to a browser reconnecting an EventSource; a
               ;; Messages stream is one request, so they are read and ignored
               (t nil)))))))

(defun sse-feed (parser octets &key (start 0) (end (length octets)))
  "Feed a piece of the stream. Bytes are buffered until a line ends, so a UTF-8
   character split between two pieces is decoded whole."
  (let ((buf (sse-parser-line parser)))
    (loop for i from start below end
          for b = (aref octets i)
          do (cond
               ((= b 10)                                   ; LF
                (if (sse-parser-after-cr parser)
                    (setf (sse-parser-after-cr parser) nil) ; the LF of a CRLF: already ended
                    (progn (sse-line parser (copy-seq buf)) (setf (fill-pointer buf) 0))))
               ((= b 13)                                   ; CR (alone or before LF)
                (sse-line parser (copy-seq buf)) (setf (fill-pointer buf) 0)
                (setf (sse-parser-after-cr parser) t))
               (t (setf (sse-parser-after-cr parser) nil)
                  (vector-push-extend b buf))))
    parser))

(defun sse-finish (parser)
  "End of stream: a final event not followed by a blank line is dispatched anyway."
  (let ((buf (sse-parser-line parser)))
    (when (plusp (fill-pointer buf))
      (sse-line parser (copy-seq buf)) (setf (fill-pointer buf) 0)))
  (sse-line parser (make-array 0 :element-type '(unsigned-byte 8))))

;;; ===========================================================================
;;; 2. The one URL question the client asks: is this the real API, over TLS?
;;; ===========================================================================

(defun agent-parse-url (url)
  "\"http://host:port/path\" => (values SCHEME HOST PORT PATH). Only what an API
   endpoint needs: no user-info, no fragment."
  (let* ((sep (or (search "://" url) (error "not a URL: ~a" url)))
         (scheme (string-downcase (subseq url 0 sep)))
         (rest (subseq url (+ sep 3)))
         (slash (or (position #\/ rest) (length rest)))
         (authority (subseq rest 0 slash))
         (path (if (< slash (length rest)) (subseq rest slash) "/"))
         (colon (position #\: authority :from-end t))
         (host (if colon (subseq authority 0 colon) authority))
         (port (if colon
                   (parse-integer authority :start (1+ colon))
                   (if (string= scheme "https") 443 80))))
    (values scheme host port path)))
