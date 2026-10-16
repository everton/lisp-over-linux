;;;; mock-anthropic.lisp — a stand-in for the Messages API, for testing the agent.
;;;;
;;;;     sbcl --script host-client/mock-anthropic.lisp [PORT]     ; serve until killed
;;;;     (load "host-client/mock-anthropic.lisp") (lol.mock:start-mock 18014)  ; in a test
;;;;
;;;; HOST-side, never shipped. It speaks plain HTTP on PORT (default 18014) and answers
;;;; POST /v1/messages with a STREAMED reply shaped like the real one: chunked transfer
;;;; encoding, SSE events, a thinking block with a signature that must come back
;;;; unchanged, text deltas, tool_use with its input streamed as input_json_delta
;;;; fragments cut at awkward places, message_delta usage — and the failures a client
;;;; must survive: a 529 overloaded_error with retry-after, an error EVENT in the middle
;;;; of a stream, and a reply slow enough to cancel. The bytes go out in small pieces,
;;;; so UTF-8 characters and SSE lines are split across reads.
;;;;
;;;; Which reply you get depends on the words of the last user message (see REPLY-FOR),
;;;; so a test steers it by what it asks. A guest reaches it at 10.0.2.2 (QEMU's SLIRP
;;;; host address); doc/agent.org §0.4 says why the real agent must not, which is why
;;;; this is a test fixture and not the broker.
;;;;
;;;; It also CHECKS what the client sends (CHECK-REQUEST) and answers a malformed
;;;; request with a 400, like the real API — thinking blocks not echoed back, a
;;;; tool_use without its tool_result, an API key sent to a host that is not
;;;; api.anthropic.com. *REQUESTS* keeps every request for a test to look at.
;;;;
;;;; All original code, under this project's MIT licence.

(require :sb-bsd-sockets)

(unless (find-package :lol.json)
  (load (merge-pathnames "../initramfs/json.lisp"
                         (make-pathname :directory (pathname-directory *load-truename*)))))

(defpackage :lol.mock
  (:use :cl :lol.json)
  (:export #:start-mock #:stop-mock #:*requests* #:*problems* #:*pages-served* #:serve-forever))

(in-package :lol.mock)

(defvar *requests* '() "Every request body (a JSON-OBJECT), newest first, with its headers.")
(defvar *problems* '() "What CHECK-REQUEST objected to, newest first.")
(defvar *seen* (make-hash-table :test 'equal) "How often each prompt was asked (for fail-once replies).")
(defvar *server* nil)
(defvar *thread* nil)

;;; ---- writing a reply --------------------------------------------------------

(defun send-octets (sock octets)
  (sb-bsd-sockets:socket-send sock octets nil))

(defun send-text (sock text)
  (send-octets sock (sb-ext:string-to-octets text :external-format :utf-8)))

(defun crlf () (format nil "~c~c" #\Return #\Newline))

(defun send-chunk (sock octets)
  "One chunk of a chunked body: size in hex, CRLF, bytes, CRLF."
  (when (plusp (length octets))
    (send-text sock (format nil "~x~a" (length octets) (crlf)))
    (send-octets sock octets)
    (send-text sock (crlf))))

(defun send-event-bytes (sock text &key (piece 7) (pause 0))
  "TEXT (an SSE event) as several chunks of PIECE bytes — so a client sees lines and
   UTF-8 characters cut in half."
  (let ((o (sb-ext:string-to-octets text :external-format :utf-8)))
    (loop for i from 0 below (length o) by piece
          do (send-chunk sock (subseq o i (min (length o) (+ i piece))))
             (when (plusp pause) (sleep pause)))))

(defun sse (sock type json &key (piece 7) (pause 0))
  (send-event-bytes sock (format nil "event: ~a~%data: ~a~%~%" type (to-json json))
                    :piece piece :pause pause))

(defun start-stream (sock)
  (send-text sock (format nil "HTTP/1.1 200 OK~aContent-Type: text/event-stream~aTransfer-Encoding: chunked~aConnection: close~a~a"
                          (crlf) (crlf) (crlf) (crlf) (crlf)))
  ;; a comment line first: the parser must skip it
  (send-event-bytes sock (format nil ": keep-alive~%~%")))

(defun end-stream (sock) (send-text sock (format nil "0~a~a" (crlf) (crlf))))

(defun send-json-error (sock status type message &key retry-after)
  (let ((body (sb-ext:string-to-octets
               (to-json (jobj "type" "error" "error" (jobj "type" type "message" message)))
               :external-format :utf-8)))
    ;; (the retry-after line is built apart: a ~@[ … ~] that consumes TWO arguments
    ;; when given NIL skips only one, which shifted every later argument and made
    ;; this FORMAT signal for any error without a retry-after — the 401, the 404)
    (send-text sock (format nil "HTTP/1.1 ~d Error~aContent-Type: application/json~aContent-Length: ~d~a~aConnection: close~a~a"
                            status (crlf) (crlf) (length body) (crlf)
                            (if retry-after (format nil "retry-after: ~a~a" retry-after (crlf)) "")
                            (crlf) (crlf)))
    (send-octets sock body)))

;;; ---- the scripted conversation ------------------------------------------------

(defun last-user-text (body)
  "The text of the last user message (its last text block), or NIL when it carries tool results instead."
  (let* ((ms (jget body "messages"))
         (last (and (plusp (length ms)) (aref ms (1- (length ms)))))
         (content (jget last "content")))
    (cond ((stringp content) content)
          ;; the LAST text block: a client folds a new question into an unanswered
          ;; user message, and it is the new question that should steer the reply
          (t (let ((texts (loop for b across content when (equal (jget b "type") "text") collect (jget b "text"))))
               (car (last texts)))))))

(defun last-tool-results (body)
  (let* ((ms (jget body "messages"))
         (last (aref ms (1- (length ms)))))
    (and (vectorp (jget last "content"))
         (loop for b across (jget last "content") when (equal (jget b "type") "tool_result") collect b))))

(defun usage (body &key (output 0))
  (let ((n (length (to-json body))))
    (jobj "input_tokens" (floor n 8) "cache_creation_input_tokens" (if (= 1 (length (jget body "messages"))) 600 0)
          "cache_read_input_tokens" (if (> (length (jget body "messages")) 1) 600 0)
          "output_tokens" output)))

(defun message-start (sock body)
  (sse sock "message_start"
       (jobj "type" "message_start"
             "message" (jobj "id" (format nil "msg_mock_~d" (length *requests*)) "type" "message"
                             "role" "assistant" "model" (jget body "model") "content" #()
                             "stop_reason" :null "usage" (usage body :output 1)))))

(defun thinking-block (sock index)
  "A thinking block the way Opus 5.5 sends one: a summary, and a signature the client
   must send back byte for byte."
  (sse sock "content_block_start" (jobj "type" "content_block_start" "index" index
                                        "content_block" (jobj "type" "thinking" "thinking" "")))
  (sse sock "content_block_delta" (jobj "type" "content_block_delta" "index" index
                                        "delta" (jobj "type" "thinking_delta" "thinking" "Considering the question.")))
  (sse sock "content_block_delta" (jobj "type" "content_block_delta" "index" index
                                        "delta" (jobj "type" "signature_delta" "signature" "sig-mock-")))
  (sse sock "content_block_delta" (jobj "type" "content_block_delta" "index" index
                                        "delta" (jobj "type" "signature_delta" "signature" "0042")))
  (sse sock "content_block_stop" (jobj "type" "content_block_stop" "index" index)))

(defun text-block (sock index text &key (pause 0))
  (sse sock "content_block_start" (jobj "type" "content_block_start" "index" index
                                        "content_block" (jobj "type" "text" "text" "")))
  (loop for start from 0 below (length text) by 9
        do (sse sock "content_block_delta"
                (jobj "type" "content_block_delta" "index" index
                      "delta" (jobj "type" "text_delta" "text" (subseq text start (min (length text) (+ start 9)))))
                :pause pause))
  (sse sock "content_block_stop" (jobj "type" "content_block_stop" "index" index)))

(defun tool-block (sock index id name input-json)
  "A tool_use whose input arrives as input_json_delta fragments of five characters —
   cut inside strings and escapes, as eager streaming does."
  (sse sock "content_block_start" (jobj "type" "content_block_start" "index" index
                                        "content_block" (jobj "type" "tool_use" "id" id "name" name "input" (jobj))))
  (loop for start from 0 below (length input-json) by 5
        do (sse sock "content_block_delta"
                (jobj "type" "content_block_delta" "index" index
                      "delta" (jobj "type" "input_json_delta"
                                    "partial_json" (subseq input-json start (min (length input-json) (+ start 5)))))))
  (sse sock "content_block_stop" (jobj "type" "content_block_stop" "index" index)))

(defun finish (sock stop output)
  (sse sock "message_delta" (jobj "type" "message_delta"
                                  "delta" (jobj "type" "message_delta" "stop_reason" stop "stop_sequence" :null)
                                  "usage" (jobj "output_tokens" output)))
  (sse sock "message_stop" (jobj "type" "message_stop")))

(defvar *current-headers* '() "The headers of the request being answered.")
(defvar *pages-served* 0 "How many times GET /page.html was answered.")

(defun request-host (body)
  "host:port the client addressed (its Host header) — how a page URL the mock hands
   out reaches the mock again, from the host (127.0.0.1) or a guest (10.0.2.2)."
  (declare (ignore body))
  (or (cdr (assoc "host" *current-headers* :test #'string=)) "127.0.0.1:18014"))


(defparameter +tool-steers+
  (list (list "inspectme" "lisp-inspect" "{\"name\": \"+fallback-models+\"}")
        (list "applicable" "lisp-applicable" "{\"name\": \"present\", \"classes\": [\"subj-defn\"]}")
        (list "backtrace" "lisp-backtrace" "{}")
        (list "browse" "browse" "{\"what\": \"senders\", \"name\": \"draw-alien\"}")
        (list "fetchpage" "web-fetch"
              (lambda (body) (format nil "{\"url\": \"http://~a/page.html\"}" (request-host body))))
        (list "showkey" "lisp-describe" "{\"name\": \"*agent-api-key*\"}"))
  "A word in the question -> the one tool call the mock answers it with: (WORD TOOL
   INPUT), INPUT a JSON string or a function of the request body that returns one.")

(defun tool-steer (text)
  (find-if (lambda (e) (search (first e) text)) +tool-steers+))

(defparameter +mock-page+
  "<!doctype html><html><head><title>Mock page</title><style>body{color:red}</style>
<script>var hidden = 'not text';</script></head><body><h1>The mock&#39;s page</h1>
<p>Fish &amp; chips, <b>caf&eacute;</b> &lt;3</p><!-- a comment --><ul><li>one</li><li>two</li></ul></body></html>"
  "What GET /page.html serves: a page web-fetch must reduce to its words.")

(defun reply-for (sock body)
  "Choose and send the reply. The words that steer it:
     overload   first time a 529 (retry-after 1), then a normal answer
     busy       ALWAYS a 529 overloaded_error (retry-after 1): the retries run out
     badkey     ALWAYS a 401 authentication_error, as for a wrong key
     cutoff     ALWAYS some text, then the connection simply closes mid-stream
     midstream  first time an error EVENT after some text, then a normal answer
     eval       a lisp-eval tool call (a WRITE: gated)
     source     a lisp-source tool call (read-only)
     slow       a long answer, a word every 0.3 s (to cancel)
     and the words of +TOOL-STEERS+ — inspectme, applicable, backtrace, browse,
     fetchpage, showkey — each one call of the tool it names (agent-tools.lisp)
   and after tool results, a closing answer that quotes the first one."
  (let* ((text (or (last-user-text body) ""))
         (results (last-tool-results body))
         (count (incf (gethash text *seen* 0))))
    (cond
      ((and (search "overload" text) (= count 1))
       (send-json-error sock 529 "overloaded_error" "Overloaded" :retry-after 1))
      ((search "busy" text)
       (send-json-error sock 529 "overloaded_error" "Overloaded" :retry-after 1))
      ((search "badkey" text)
       (send-json-error sock 401 "authentication_error" "invalid x-api-key"))
      ((search "cutoff" text)
       ;; a dropped connection, not an error event: the chunked body just stops, with
       ;; no terminating chunk, no message_delta and no message_stop
       (start-stream sock)
       (message-start sock body)
       (text-block sock 1 "This answer will be cut off in the middle of a sen"))
      (t
       (start-stream sock)
       (message-start sock body)
       (thinking-block sock 0)
       (cond
         (results
          (let* ((r (first results))
                 (content (jget r "content"))
                 (content (if (stringp content) content (to-json content))))
            (text-block sock 1 (format nil "The tool answered: ~a~a"
                                       (subseq content 0 (min 60 (length content)))
                                       (if (eq (jget r "is_error") t) " (an error)" "")))
            (finish sock "end_turn" 20)))
         ((and (search "midstream" text) (= count 1))
          (text-block sock 1 "This reply will break off ")
          (sse sock "error" (jobj "type" "error" "error" (jobj "type" "overloaded_error" "message" "Overloaded"))))
         ((tool-steer text)
          (destructuring-bind (word name input) (tool-steer text)
            (declare (ignore word))
            (tool-block sock 1 (format nil "toolu_mock_~a" (substitute #\_ #\- name)) name
                        (if (functionp input) (funcall input body) input))
            (finish sock "tool_use" 15)))
         ((search "eval" text)
          (text-block sock 1 "I will evaluate it.")
          (tool-block sock 2 "toolu_mock_eval" "lisp-eval" "{\"form\": \"(+ 40 2)\", \"package\": \"cl-user\"}")
          (finish sock "tool_use" 30))
         ((search "source" text)
          (tool-block sock 1 "toolu_mock_src" "lisp-source" "{\"name\": \"draw-alien\"}")
          (finish sock "tool_use" 15))
         ((search "slow" text)
          (text-block sock 1 (format nil "~{~a ~}" (loop for i from 1 to 200 collect (format nil "word~d" i)))
                      :pause 0.3)
          (finish sock "end_turn" 200))
         (t
          (text-block sock 1 (format nil "Hello from the mock, caf~c ~c. You asked: ~a~%~%```lisp~%(defun mock-answer () 42)~%```~%That is all."
                                     (code-char #xe9) (code-char #x1F600) text))
          (finish sock "end_turn" 42)))
       (end-stream sock)))))

;;; ---- checking the request -------------------------------------------------------

(defun check-request (body headers host-is-anthropic)
  "What the real API would refuse, as a string, or NIL."
  (let ((ms (jget body "messages")))
    (or
     (and (not host-is-anthropic) (assoc "x-api-key" headers :test #'string-equal)
          "an API key was sent to a host that is not api.anthropic.com")
     (and (not (eq (jget body "stream") t)) "stream must be true")
     (and (not (stringp (jget body "model"))) "model missing")
     (and (not (integerp (jget body "max_tokens"))) "max_tokens missing")
     (and (or (not (vectorp ms)) (zerop (length ms))) "messages missing")
     (and (not (equal (jget (aref ms 0) "role") "user")) "the first message must be the user's")
     ;; every assistant thinking block must come back with the signature we gave it
     (loop for m across ms
           when (equal (jget m "role") "assistant")
             do (loop for b across (jget m "content")
                      when (and (equal (jget b "type") "thinking")
                                (not (equal (jget b "signature") "sig-mock-0042")))
                        do (return-from check-request "a thinking block came back altered")))
     ;; every tool_use must be answered by a tool_result in the very next message
     (loop for i from 0 below (length ms)
           for m = (aref ms i)
           when (equal (jget m "role") "assistant")
             do (let ((ids (loop for b across (jget m "content")
                                 when (equal (jget b "type") "tool_use") collect (jget b "id"))))
                  (when ids
                    (let* ((next (and (< (1+ i) (length ms)) (aref ms (1+ i))))
                           (answered (and next (vectorp (jget next "content"))
                                          (loop for b across (jget next "content")
                                                when (equal (jget b "type") "tool_result")
                                                  collect (jget b "tool_use_id")))))
                      (unless (subsetp ids answered :test #'equal)
                        (return-from check-request "a tool_use has no tool_result after it")))))))))

;;; ---- the server loop -------------------------------------------------------------

(defun read-request (sock)
  "Read one HTTP request: (values METHOD PATH HEADERS BODY-STRING)."
  (let ((buf (make-array 65536 :element-type '(unsigned-byte 8)))
        (data (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((more ()
             (multiple-value-bind (b n) (sb-bsd-sockets:socket-receive sock buf nil)
               (declare (ignore b))
               (when (or (null n) (zerop n)) (return-from read-request nil))
               (loop for i below n do (vector-push-extend (aref buf i) data))))
           (head-end () (search #(13 10 13 10) data)))
      (loop until (head-end) do (more))
      (let* ((he (head-end))
             (head (sb-ext:octets-to-string (subseq data 0 he) :external-format :latin-1))
             (lines (loop with start = 0 for nl = (search (crlf) head :start2 start)
                          collect (subseq head start nl) while nl do (setf start (+ nl 2))))
             (request-line (first lines))
             (headers (loop for l in (rest lines) for c = (position #\: l)
                            when c collect (cons (string-downcase (subseq l 0 c)) (string-trim " " (subseq l (1+ c))))))
             (len (let ((cl (cdr (assoc "content-length" headers :test #'string=))))
                    (if cl (parse-integer cl) 0))))
        (loop while (< (- (length data) (+ he 4)) len) do (more))
        (let ((sp1 (position #\Space request-line)))
          (values (subseq request-line 0 sp1)
                  (subseq request-line (1+ sp1) (position #\Space request-line :start (1+ sp1)))
                  headers
                  (sb-ext:octets-to-string (subseq data (+ he 4) (+ he 4 len)) :external-format :utf-8)))))))

(defun handle (sock)
  (multiple-value-bind (method path headers text) (read-request sock)
    (cond
      ((null method) nil)
      ((and (string= method "GET") (string= path "/page.html"))
       (let ((o (sb-ext:string-to-octets +mock-page+ :external-format :utf-8)))
         (incf *pages-served*)              ; not in *REQUESTS*: those are Messages calls
         (send-text sock (format nil "HTTP/1.1 200 OK~aContent-Type: text/html; charset=utf-8~aContent-Length: ~d~aConnection: close~a~a"
                                 (crlf) (crlf) (length o) (crlf) (crlf) (crlf)))
         (send-octets sock o)))
      ((not (and (string= method "POST") (string= path "/v1/messages")))
       (send-json-error sock 404 "not_found_error" (format nil "~a ~a" method path)))
      (t
       (let* ((body (handler-case (parse-json text) (error () nil)))
              (problem (if body (check-request body headers nil) "the body is not JSON")))
         (push (list :body body :headers headers) *requests*)
         (if problem
             (progn (push problem *problems*)
                    (format *error-output* "~&mock: 400 ~a~%" problem)
                    (send-json-error sock 400 "invalid_request_error" problem))
             (let ((*current-headers* headers)) (reply-for sock body))))))))

(defun serve (server)
  (loop
    (let ((client (handler-case (sb-bsd-sockets:socket-accept server)
                    (error () (return)))))
      ;; one thread per connection: a slow reply being cancelled must not hold up
      ;; the next request (the client's retry, or its next turn)
      (sb-thread:make-thread
       (lambda ()
         (unwind-protect (handler-case (handle client)
                           (error (e) (format *error-output* "~&mock: ~a~%" e)))
           (ignore-errors (sb-bsd-sockets:socket-close client))))
       :name "mock-connection"))))

(defun start-mock (port)
  "Serve on PORT in a background thread; returns the thread."
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address s) t)
    (sb-bsd-sockets:socket-bind s #(0 0 0 0) port)
    (sb-bsd-sockets:socket-listen s 8)
    (setf *server* s *requests* '() *problems* '())
    (clrhash *seen*)
    (setf *thread* (sb-thread:make-thread (lambda () (serve s)) :name "mock-anthropic"))))

(defun stop-mock ()
  (when *server*
    (ignore-errors (sb-bsd-sockets:socket-shutdown *server* :direction :io))
    (ignore-errors (sb-bsd-sockets:socket-close *server*))
    (setf *server* nil)))

(defun serve-forever (port)
  (start-mock port)
  (format t "~&mock Messages API on :~d (POST /v1/messages)~%" port) (finish-output)
  (sb-thread:join-thread *thread* :default nil))

;; Run as a script, serve until killed. A test that wants the functions only sets
;; CL-USER::*MOCK-LIBRARY-ONLY* before loading this file.
(unless (boundp 'cl-user::*mock-library-only*)
  (serve-forever (or (ignore-errors (parse-integer (car (last sb-ext:*posix-argv*)))) 18014)))
