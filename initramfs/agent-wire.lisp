;;;; agent-wire.lisp — the agent's wire: Server-Sent Events, and a plain-HTTP fallback.
;;;;
;;;; doc/agent.org §3 stacks an LLM client as transport → wire format → protocol →
;;;; tools. This file is the bottom two that are not JSON:
;;;;
;;;;   1. SSE FRAMING. A streamed Messages-API reply is not one JSON document but a
;;;;      text/event-stream: lines of "event: NAME" and "data: JSON", each event ended
;;;;      by a blank line (the WHATWG "server-sent events" format, the same one a
;;;;      browser's EventSource reads). SSE-FEED takes the body's bytes AS THEY ARRIVE
;;;;      — in arbitrary pieces, a line or a multi-byte character cut anywhere — and
;;;;      calls back once per complete event. Pure; host-tested.
;;;;
;;;;   2. A PLAIN-HTTP TRANSPORT (AGENT-PLAIN-HTTP), with exactly the signature the
;;;;      networking track's HTTP-REQUEST promises (initramfs/http.lisp, owned there):
;;;;
;;;;        (fn url &key method headers body want on-chunk timeout)
;;;;          => (values status headers body)
;;;;
;;;;      It speaks http:// only — no TLS, no redirects, one request per connection
;;;;      (Connection: close) — which is all that a loopback mock server, or the
;;;;      TLS-terminating broker of doc/agent.org §4, ever needs. When HTTP-REQUEST
;;;;      exists in the image (https://, DNS, the host's libssl), the client uses it
;;;;      instead; this one is the stand-in that let the agent be written and tested
;;;;      before that landed, and stays as the transport to a broker.
;;;;
;;;; Why read with SOCKET-RECEIVE rather than a Lisp stream: a stream's READ-SEQUENCE
;;;; waits until its buffer is FULL, and an SSE stream trickles — the first token
;;;; would sit in a half-full 4 KB buffer until 4 KB of tokens had piled up behind
;;;; it. SOCKET-RECEIVE returns whatever has arrived, which is the whole point of
;;;; streaming.
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
;;; 2. URLs, and headers whatever their shape
;;; ===========================================================================

(defun parse-url (url)
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

(defun header-value (headers name)
  "NAME's value in HEADERS, case-insensitively, whatever shape HEADERS has: an alist
   of (name . value), a plist, or a hash table. The networking track's HTTP-REQUEST
   and ours need not agree on the shape for the client to read either."
  (cond
    ((hash-table-p headers)
     (loop for k being the hash-keys of headers using (hash-value v)
           when (string-equal (string k) name) return v))
    ((and (consp headers) (consp (first headers)))
     (cdr (assoc name headers :test (lambda (a b) (string-equal a (string b))))))
    ((consp headers)
     (loop for (k v) on headers by #'cddr when (string-equal name (string k)) return v))
    (t nil)))

;;; ===========================================================================
;;; 3. The plain-HTTP transport
;;; ===========================================================================

(defun dotted-quad (host)
  "HOST as a 4-octet vector when it is a dotted-quad literal, else NIL."
  (let ((parts (ignore-errors
                 (loop with start = 0
                       for dot = (position #\. host :start start)
                       collect (parse-integer host :start start :end dot)
                       while dot do (setf start (1+ dot))))))
    (and (= (length parts) 4) (every (lambda (p) (<= 0 p 255)) parts)
         (coerce parts '(vector (unsigned-byte 8))))))

(defun resolve-address (host)
  "HOST's IPv4 address: a literal as is; a name through the networking track's
   RESOLVE-HOST when the image has it (doc/networking.org: a DNS client in Lisp, no
   glibc NSS), else the host SBCL's resolver (for the host tests)."
  (or (dotted-quad host)
      (and (fboundp 'resolve-host) (funcall 'resolve-host host))
      (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host))))

(defstruct (byte-reader (:constructor make-byte-reader (socket timeout)))
  "A tiny buffered reader over a socket that returns what has ARRIVED (see header)."
  socket timeout
  (buf (make-array 16384 :element-type '(unsigned-byte 8)))
  (pos 0) (end 0) (eof nil))

(defun br-fill (r)
  "Wait (at most TIMEOUT seconds) for bytes and read what is there. NIL at EOF."
  (when (byte-reader-eof r) (return-from br-fill nil))
  (let ((fd (sb-bsd-sockets:socket-file-descriptor (byte-reader-socket r))))
    (unless (sb-sys:wait-until-fd-usable fd :input (byte-reader-timeout r))
      (error "HTTP: no data for ~a s" (byte-reader-timeout r))))
  (multiple-value-bind (buf n)
      (sb-bsd-sockets:socket-receive (byte-reader-socket r) (byte-reader-buf r) nil)
    (declare (ignore buf))
    (if (or (null n) (zerop n))
        (progn (setf (byte-reader-eof r) t) nil)
        (progn (setf (byte-reader-pos r) 0 (byte-reader-end r) n) t))))

(defun br-byte (r)
  (when (>= (byte-reader-pos r) (byte-reader-end r))
    (unless (br-fill r) (return-from br-byte nil)))
  (prog1 (aref (byte-reader-buf r) (byte-reader-pos r)) (incf (byte-reader-pos r))))

(defun br-line (r)
  "One CRLF-ended header/chunk-size line, as a Latin-1 string (header lines are
   ASCII by RFC 9112), or NIL at EOF."
  (let ((out (make-string-output-stream)) (any nil))
    (loop for b = (br-byte r)
          do (cond ((null b) (return (and any (get-output-stream-string out))))
                   ((= b 10) (return (string-right-trim '(#\Return) (get-output-stream-string out))))
                   (t (setf any t) (write-char (code-char b) out))))))

(defun br-take (r n on-piece)
  "Pass the next N body bytes to ON-PIECE, as pieces of whatever has arrived; NIL N
   means 'until EOF'. The pieces are fresh vectors (the callee may keep them)."
  (loop
    (when (and n (<= n 0)) (return))
    (when (>= (byte-reader-pos r) (byte-reader-end r))
      (unless (br-fill r)
        (when n (error "HTTP: connection closed ~d bytes early" n))
        (return)))
    (let* ((avail (- (byte-reader-end r) (byte-reader-pos r)))
           (k (if n (min n avail) avail)))
      (funcall on-piece (subseq (byte-reader-buf r) (byte-reader-pos r) (+ (byte-reader-pos r) k)))
      (incf (byte-reader-pos r) k)
      (when n (decf n k)))))

(defun agent-plain-http (url &key (method :get) headers body (want :string) on-chunk (timeout 120))
  "An HTTP/1.1 request over plain TCP — the stand-in for HTTP-REQUEST (see header).
   HEADERS is an alist of (name . value) strings. BODY is a string (sent as UTF-8) or
   octets. With ON-CHUNK, the response body is handed over piece by piece as it
   arrives (de-chunked) and the third value is NIL; otherwise it is returned whole, as
   a string or (WANT :octets) a byte vector. Response headers come back as an alist
   with lower-cased names."
  (multiple-value-bind (scheme host port path) (parse-url url)
    (unless (string= scheme "http")
      (error "~a: this transport speaks plain http:// only — https needs HTTP-REQUEST ~
              (initramfs/http.lisp, the networking track) or a TLS broker (doc/agent.org §4)"
             url))
    (let* ((octets (cond ((null body) (make-array 0 :element-type '(unsigned-byte 8)))
                         ((stringp body) (sb-ext:string-to-octets body :external-format :utf-8))
                         (t body)))
           (head (with-output-to-string (s)
                   (format s "~a ~a HTTP/1.1~c~c" (string-upcase (string method)) path #\Return #\Newline)
                   (format s "Host: ~a:~d~c~c" host port #\Return #\Newline)
                   (dolist (h headers)
                     (format s "~a: ~a~c~c" (car h) (cdr h) #\Return #\Newline))
                   (when (or body (member method '(:post :put)))
                     (format s "Content-Length: ~d~c~c" (length octets) #\Return #\Newline))
                   (format s "Connection: close~c~c~c~c" #\Return #\Newline #\Return #\Newline)))
           (socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
      (unwind-protect
           (progn
             (sb-bsd-sockets:socket-connect socket (resolve-address host) port)
             (let ((all (concatenate '(vector (unsigned-byte 8))
                                     (sb-ext:string-to-octets head :external-format :latin-1)
                                     octets)))
               (sb-bsd-sockets:socket-send socket all nil))
             (let* ((r (make-byte-reader socket timeout))
                    (status-line (or (br-line r) (error "HTTP: empty response from ~a" url)))
                    (status (parse-integer status-line :start (1+ (position #\Space status-line))
                                                       :junk-allowed t))
                    (resp-headers
                      (loop for line = (br-line r)
                            while (and line (plusp (length line)))
                            collect (let ((c (position #\: line)))
                                      (cons (string-downcase (subseq line 0 c))
                                            (string-trim " " (subseq line (1+ c)))))))
                    (pieces '())
                    (sink (or on-chunk (lambda (p) (push p pieces)))))
               (cond
                 ((search "chunked" (or (header-value resp-headers "transfer-encoding") ""))
                  ;; chunked transfer: "<hex size>[;ext]" CRLF <bytes> CRLF ... "0" CRLF CRLF
                  (loop for size-line = (br-line r)
                        for size = (and size-line
                                        (parse-integer size-line :radix 16 :junk-allowed t))
                        while (and size (plusp size))
                        do (br-take r size sink)
                           (br-line r)))                    ; the CRLF after the data
                 ((header-value resp-headers "content-length")
                  (br-take r (parse-integer (header-value resp-headers "content-length")) sink))
                 (t (br-take r nil sink)))
               (values status resp-headers
                       (unless on-chunk
                         (let ((all (apply #'concatenate '(vector (unsigned-byte 8)) (reverse pieces))))
                           (if (eq want :octets)
                               all
                               (sb-ext:octets-to-string all :external-format :utf-8)))))))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))
