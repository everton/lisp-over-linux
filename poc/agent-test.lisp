;;;; agent-test.lisp — host test for the agent: SSE framing, and the whole client
;;;; against a mock Messages API on loopback.
;;;;
;;;;   sbcl --script poc/agent-test.lisp        ; MOCK_PORT=18014 by default
;;;;
;;;; The transport is the image's own HTTP-REQUEST (initramfs/http.lisp), as in the
;;;; guest. The mock (host-client/mock-anthropic.lisp) streams replies the way the real API
;;;; does — chunked, in small pieces, thinking signatures, tool input fragments — and
;;;; refuses (400) the requests the real API would refuse. So each check below is
;;;; either a thing the client must show (the reply, the cost, a retry note) or a thing
;;;; it must SEND correctly (signatures echoed, every tool_use answered, no key to a
;;;; host that is not api.anthropic.com). No real API, no key, no network beyond
;;;; 127.0.0.1. HOME is pointed at a scratch directory, never the real one.
;;;;
;;;; Prints one line per check and exits non-zero on any failure.

(require :sb-introspect)
(require :sb-bsd-sockets)
(require :sb-posix)

(defparameter *root* (merge-pathnames "../" (make-pathname :directory (pathname-directory *load-truename*))))
(defparameter *scratch* (format nil "/tmp/lol-agent-test-~d/" (sb-unix:unix-getpid)))
(ensure-directories-exist (merge-pathnames ".config/lol/" *scratch*))
(sb-posix:setenv "HOME" (string-right-trim "/" *scratch*) 1)

(let ((*load-verbose* nil))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "initramfs/registry.lisp" *root*))
    (dolist (m '("initramfs/model.lisp" "initramfs/present.lisp" "poc/canvas.lisp" "poc/textview.lisp"
                 "poc/widgets.lisp" "initramfs/ansi.lisp" "initramfs/sysinfo.lisp" "poc/inflate.lisp"
                 "initramfs/files.lisp" "initramfs/file-views.lisp" "initramfs/framebuffer.lisp"
                 "initramfs/json.lisp" "initramfs/agent-wire.lisp" "initramfs/agent.lisp"))
      (funcall 'load-recording (merge-pathnames m *root*)))
    ;; the transport: the networking track's HTTP-REQUEST, loaded as poc/http-test.lisp
    ;; loads it (plain http to 127.0.0.1 needs neither DNS nor TLS, but they come along)
    (dolist (f '("net" "dhcp" "dns" "sntp" "tls" "http"))
      (load (merge-pathnames (format nil "initramfs/~a.lisp" f) *root*)))))

(defvar *mock-library-only* t)
(load (merge-pathnames "host-client/mock-anthropic.lisp" *root*))

(defparameter *port* (or (ignore-errors (parse-integer (sb-posix:getenv "MOCK_PORT"))) 18014))

(defvar *failures* 0)
(defun ok (name got &optional (want t) &key (test #'equal))
  (if (funcall test got want)
      (format t "  ok    ~a~%" name)
      (progn (incf *failures*)
             (format t "  FAIL  ~a~%        got  ~s~%        want ~s~%" name got want)))
  (finish-output))

;;; ---- SSE framing ------------------------------------------------------------

(format t "~&== SSE framing ==~%")
(defun sse-events (text &key (piece 1))
  "Feed TEXT's UTF-8 bytes PIECE at a time; return the events seen."
  (let* ((events '())
         (p (make-sse-parser (lambda (n d) (push (list n d) events))))
         (o (sb-ext:string-to-octets text :external-format :utf-8)))
    (loop for i from 0 below (length o) by piece
          do (sse-feed p o :start i :end (min (length o) (+ i piece))))
    (sse-finish p)
    (nreverse events)))

(let* ((nl (string #\Newline))
       (crlf (format nil "~c~c" #\Return #\Newline))
       (text (format nil ": comment~%event: a~%data: {\"x\":1}~%~%data: line1~%data: line2~%~%event: b~%data:no-space~%~%"))
       (want '(("a" "{\"x\":1}") ("message" "line1
line2") ("b" "no-space"))))
  (declare (ignore nl))
  (ok "events, comments, multi-line data, default name" (sse-events text) want)
  (ok "the same, one byte at a time" (sse-events text :piece 1) want)
  (ok "the same, with CRLF line ends" (sse-events (with-output-to-string (o) (loop for ch across text do (if (char= ch #\Newline) (write-string crlf o) (write-char ch o))))) want)
  (ok "a final event without its blank line is still dispatched"
      (sse-events (format nil "event: z~%data: 1")) '(("z" "1")))
  (ok "a UTF-8 character split between pieces arrives whole"
      (sse-events (format nil "data: caf~c ~c~%~%" (code-char #xe9) (code-char #x1F600)) :piece 1)
      (list (list "message" (format nil "caf~c ~c" (code-char #xe9) (code-char #x1F600))))))

;;; ---- the client against the mock ------------------------------------------------

(lol.mock:start-mock *port*)
(sleep 0.2)
(setf *agent-endpoint* (format nil "http://127.0.0.1:~d" *port*)
      *agent-settings-loaded* t           ; this test sets them itself
      *agent-retries* 4)

(ok "the transport is the image's own HTTP-REQUEST" (transport) 'http-request)

(defun texts (conv)
  "Every assistant text block, joined."
  (format nil "~{~a~^|~}"
          (loop for m across (conversation-messages conv)
                when (equal (message-role m) "assistant")
                  append (loop for b in (message-blocks m)
                               when (equal (block-type b) "text") collect (lol.json:jget (block-json b) "text")))))

(defun last-request () (getf (first lol.mock:*requests*) :body))

(format t "~&== a plain question ==~%")
(let ((c (make-conversation)))
  (run-turn c "hello there")
  (ok "the reply streamed in, UTF-8 intact"
      (and (search (format nil "caf~c ~c" (code-char #xe9) (code-char #x1F600)) (texts c)) t))
  (ok "the code block is in the text" (and (search "(defun mock-answer () 42)" (texts c)) t))
  (ok "two messages: the question and the reply" (length (conversation-messages c)) 2)
  (ok "the thinking block was kept with its signature"
      (lol.json:jget (block-json (first (message-blocks (aref (conversation-messages c) 1)))) "signature")
      "sig-mock-0042")
  (ok "usage counted" (getf (conversation-usage c) :output) 42)
  (ok "cache write counted" (getf (conversation-usage c) :cache-write) 600)
  (ok "a cost was computed (opus-5-5 prices)" (and (realp (conversation-cost c)) (plusp (conversation-cost c))))
  (ok "the conversation is idle again" (conversation-state c) :idle)
  (ok "the system prompt carries a cache breakpoint"
      (lol.json:jget* (last-request) "system" 0 "cache_control" "type") "ephemeral")
  (ok "the last tool carries one too"
      (let ((tools (lol.json:jget (last-request) "tools"))) (lol.json:jget* tools (1- (length tools)) "cache_control" "type"))
      "ephemeral")
  (ok "tools stream their input eagerly" (lol.json:jget* (last-request) "tools" 0 "eager_input_streaming") t)
  (ok "no API key went to a non-Anthropic host"
      (assoc "x-api-key" (getf (first lol.mock:*requests*) :headers) :test #'string-equal) nil)
  (ok "the same conversation serialises to the same bytes twice"
      (string= (lol.json:to-json (request-body c)) (lol.json:to-json (request-body c))))
  (run-turn c "and again")
  (ok "second turn accepted: the thinking signature went back unchanged" lol.mock:*problems* nil)
  (ok "cache read counted on the second request" (getf (conversation-usage c) :cache-read) 600))

(format t "~&== tools: read-only, then a gated write ==~%")
(let ((c (make-conversation)))
  (run-turn c "show me the source")
  (let ((call (aref (conversation-calls c) 0)))
    (ok "lisp-source ran" (call-status call) :done)
    (ok "its input arrived whole from fragments" (lol.json:jget (call-input call) "name") "draw-alien")
    (ok "its subject is the definition" (subj-defn-name (call-subject call)) 'draw-alien)
    (ok "the result went back and was quoted" (and (search "The tool answered:" (texts c)) t))
    (ok "every tool_use was answered (the mock checks)" lol.mock:*problems* nil)))

(let ((c (make-conversation)))
  (handler-bind ((tool-approval-request (lambda (cnd) (declare (ignore cnd)) (invoke-restart 'allow))))
    (run-turn c "please eval this"))
  (let ((call (aref (conversation-calls c) 0)))
    (ok "approved: lisp-eval ran" (call-status call) :done)
    (ok "its result is the value" (and (search "values: 42" (call-result call)) t))
    (ok "the model saw it" (and (search "values: 42" (texts c)) t))))

(let ((c (make-conversation)))
  (handler-bind ((tool-approval-request (lambda (cnd) (declare (ignore cnd)) (invoke-restart 'deny "not now"))))
    (run-turn c "please eval this again"))
  (let ((call (aref (conversation-calls c) 0)))
    (ok "denied: not run" (call-status call) :denied)
    (ok "the denial went back with its reason" (and (search "Reason: not now" (call-result call)) t))))

(let ((c (make-conversation)))
  (handler-bind ((tool-approval-request (lambda (cnd) (declare (ignore cnd)) (invoke-restart 'edit "(* 6 7 10)"))))
    (run-turn c "eval, but edited"))
  (ok "edited: the edited form ran" (and (search "values: 420" (call-result (aref (conversation-calls c) 0))) t)))

(let ((c (make-conversation)))
  (run-turn c "eval with nobody around")
  (ok "unhandled request: declined, nobody to approve"
      (and (search "no one was there" (call-result (aref (conversation-calls c) 0))) t)))

(let ((c (make-conversation)) (*agent-authority* :read-only))
  (run-turn c "plain question, read-only")
  (ok "read-only: no write tool is offered"
      (loop for tool across (lol.json:jget (last-request) "tools")
            never (member (lol.json:jget tool "name") '("lisp-eval" "lisp-accept" "file-write") :test #'string=))))

(let ((c (make-conversation)) (*agent-authority* :allow))
  (run-turn c "eval allowed")
  (ok ":allow: the write ran without asking" (call-status (aref (conversation-calls c) 0)) :done))

(format t "~&== failures the client must survive ==~%")
(let ((c (make-conversation)))
  (run-turn c "overload please")
  (ok "a 529 was retried and the answer came" (and (search "Hello from the mock" (texts c)) t))
  (ok "the retry is a note in the transcript"
      (and (find-if (lambda (n) (search "retrying" n)) (turn-notes (current-turn c))) t)))

(let ((c (make-conversation)))
  (run-turn c "midstream break")
  (ok "an error EVENT mid-stream was retried" (and (search "Hello from the mock" (texts c)) t))
  (ok "the broken partial reply was not kept" (length (conversation-messages c)) 2))

(let* ((c (make-conversation))
       (th (sb-thread:make-thread (lambda () (run-turn c "slow answer please")))))
  (setf (conversation-thread c) th)
  (sleep 2)
  (ok "while streaming, the live reply grows"
      (and (conversation-live c)
           (some (lambda (lb) (plusp (length (live-text lb)))) (live-reply-blocks (conversation-live c)))))
  (cancel-conversation c)
  (sb-thread:join-thread th :default nil :timeout 5)
  (ok "cancelled: idle again" (conversation-state c) :idle)
  (ok "cancelled: the note says so" (and (find "stopped (C-g)" (turn-notes (current-turn c)) :test #'equal) t))
  (run-turn c "hello after the cancel")
  (ok "the conversation goes on after a cancel" (and (search "hello after the cancel" (texts c)) t))
  (ok "...and its history was still valid" lol.mock:*problems* nil))

(format t "~&== the key and the settings ==~%")
(let ((*agent-endpoint* "https://api.anthropic.com"))
  (forget-agent-key)
  (ok "no key file: a clear error, not a request"
      (handler-case (progn (request-headers "https://api.anthropic.com/v1/messages") :sent)
        (agent-api-error (c) (api-error-kind c)))
      "no_api_key")
  (with-open-file (s (merge-pathnames ".config/lol/anthropic-api-key" *scratch*)
                     :direction :output :if-exists :supersede)
    (write-line "sk-test-not-a-real-key" s))
  (forget-agent-key)
  (ok "with a key file, x-api-key goes to api.anthropic.com"
      (cdr (assoc "x-api-key" (request-headers "https://api.anthropic.com/v1/messages") :test #'string=))
      "sk-test-not-a-real-key")
  (ok "...and never to any other host"
      (assoc "x-api-key" (request-headers "http://10.0.2.2:18014/v1/messages") :test #'string=) nil)
  (ok "...nor to api.anthropic.com over plain http"
      (assoc "x-api-key" (request-headers "http://api.anthropic.com/v1/messages") :test #'string=) nil))

(with-open-file (s (merge-pathnames ".config/lol/agent.lisp" *scratch*) :direction :output :if-exists :supersede)
  (write-string "(:model \"claude-sonnet-5-5\" :max-tokens 1234 :authority :allow :bogus 1)" s))
(let ((*agent-model* "x") (*agent-max-tokens* 1) (*agent-authority* :ask) )
  (ok "settings: read, typed, unknown keys skipped" (load-agent-settings) '(:model :max-tokens :authority))
  (ok "settings: applied" (list *agent-model* *agent-max-tokens* *agent-authority*)
      '("claude-sonnet-5-5" 1234 :allow)))
(with-open-file (s (merge-pathnames ".config/lol/agent.lisp" *scratch*) :direction :output :if-exists :supersede)
  (write-string "(:model #.(progn (setf cl-user::*pwned* t) \"evil\"))" s))
(defvar *pwned* nil)
(ok "settings: #. is refused, not run" (and (null (ignore-errors (load-agent-settings))) (not *pwned*)))

(format t "~&== the file tools stay under home ==~%")
(ok "a path outside home is refused" (handler-case (progn (agent-path "/etc/passwd") :allowed) (error () :refused)) :refused)
(ok "a ~/ path resolves under home" (agent-path "~/notes.txt")
    (concatenate 'string (string-right-trim "/" *scratch*) "/notes.txt"))
(ok "a .. escape is refused" (handler-case (progn (agent-path "~/../../etc/passwd") :allowed) (error () :refused)) :refused)

(lol.mock:stop-mock)
(format t "~&~:[ALL PASS~;~:*~d FAILURE(S)~]~%" (and (plusp *failures*) *failures*))
(sb-ext:exit :code (if (zerop *failures*) 0 1) :abort t)
