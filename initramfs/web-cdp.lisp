;;;; web-cdp.lisp — talk to a headless Chrome over the DevTools protocol, on a pipe.
;;;;
;;;; The web pane (web.lisp) does not contain a web engine. It drives one: Google
;;;; Chrome, started HEADLESS, so it owns no window and no screen, and every pixel it
;;;; renders comes back to us as a JPEG we decode and blit ourselves. Lisp stays the
;;;; only process writing /dev/fb0 (doc/processes.org §1); a web page becomes one more
;;;; thing the environment DRAWS. doc/web-browser.org tells the whole story; this file
;;;; is the wire underneath it, and knows nothing about panes, canvases or keys.
;;;;
;;;; THE DEVTOOLS PROTOCOL (CDP) is how Chrome's own developer tools talk to the
;;;; browser: JSON messages, each a COMMAND we send, {"id":7,"method":"Page.navigate",
;;;; "params":{...}}, answered by a RESPONSE carrying the same id, {"id":7,"result":
;;;; {...}} — or by {"id":7,"error":{...}}. Chrome also sends EVENTS on its own,
;;;; {"method":"Page.screencastFrame","params":{...}}, with no id. That is the whole
;;;; grammar. The vocabulary (a few hundred methods in domains: Page, Input, Target,
;;;; Runtime, Emulation, Browser) is documented by Google at
;;;; https://chromedevtools.github.io/devtools-protocol/.
;;;;
;;;; THE PIPE, NOT A SOCKET. Chrome usually serves CDP over a WebSocket. Started with
;;;; --remote-debugging-pipe it speaks it instead over two inherited file descriptors:
;;;; it READS commands from fd 3 and WRITES responses and events to fd 4, each message
;;;; terminated by a NUL byte (JSON never contains a raw NUL, so the NUL is an
;;;; unambiguous frame boundary). No HTTP upgrade, no WebSocket framing, no port that
;;;; another program could connect to. A shell gives the child those two fds: we start
;;;; `sh -c 'exec 3<&0 4>&1 ...; exec chrome ...'`, so the pipe SBCL's RUN-PROGRAM
;;;; makes for the child's stdin becomes its fd 3 and its stdout pipe becomes fd 4.
;;;;
;;;; SESSIONS. One Chrome process holds many tabs. A tab is a TARGET; we ATTACH to it
;;;; with Target.attachToTarget {flatten: true}, which answers a sessionId. From then on
;;;; a command meant for that tab carries "sessionId" beside "method", and every event
;;;; FROM that tab carries it too. One pipe, many tabs, multiplexed by one string.
;;;;
;;;; Everything here is pure Lisp over sb-unix/sb-alien (no sb-posix, per
;;;; doc/processes.org §6) and runs unchanged on the host — poc/web-test.lisp drives
;;;; the host's Chrome with it.
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

;;; ---- JSON, the subset CDP needs ---------------------------------------------
;;; Reading: an object becomes an ALIST of ("key" . value) — order kept, printable,
;;; easy to walk at the REPL; an array becomes a VECTOR (so it cannot be confused with
;;; an object); true/false/null become T / :FALSE / :NULL; numbers become integers or
;;; double-floats. Writing goes the other way from a small, explicit vocabulary:
;;; (JOBJ "k" v ...) is an object, a vector is an array, T / :FALSE / :NULL are the
;;; three constants. NIL is written as an EMPTY OBJECT, because "no params" is by far
;;; its commonest meaning here.
;;;
;;; (The agent track has a general JSON module; this one is deliberately the subset
;;; the DevTools traffic uses, with names prefixed JSON- / J, and nothing else
;;; depends on it.)

(defun jobj (&rest kvs)
  "A JSON object to WRITE: (jobj \"url\" u \"width\" 800). Keys are strings."
  (cons :obj (loop for (k v) on kvs by #'cddr collect (cons k v))))

(defun jget (obj &rest keys)
  "Walk OBJ (as READ-JSON returns it) down KEYS: a string key indexes an object, an
   integer an array. NIL when any step is missing."
  (dolist (k keys obj)
    (setf obj (cond ((and (stringp k) (listp obj)) (cdr (assoc k obj :test #'string=)))
                    ((and (integerp k) (vectorp obj) (not (stringp obj)) (< k (length obj)))
                     (aref obj k))
                    (t nil)))))

(defun write-json-string (s out)
  (write-char #\" out)
  (loop for c across s
        for code = (char-code c)
        do (case c
             (#\" (write-string "\\\"" out))
             (#\\ (write-string "\\\\" out))
             (#\Newline (write-string "\\n" out))
             (#\Return (write-string "\\r" out))
             (#\Tab (write-string "\\t" out))
             (t (if (< code 32)
                    (format out "\\u~4,'0x" code)
                    (write-char c out)))))
  (write-char #\" out))

(defun write-json (v out)
  "Write V as JSON on the character stream OUT (see the vocabulary above)."
  (cond ((null v) (write-string "{}" out))
        ((eq v t) (write-string "true" out))
        ((eq v :false) (write-string "false" out))
        ((eq v :null) (write-string "null" out))
        ((stringp v) (write-json-string v out))
        ((integerp v) (format out "~d" v))
        ((realp v) (format out "~,6f" (float v 1d0)))
        ((symbolp v) (write-json-string (string-downcase (symbol-name v)) out))
        ((and (consp v) (eq (car v) :obj))
         (write-char #\{ out)
         (loop for (pair . more) on (cdr v)
               do (write-json-string (car pair) out)
                  (write-char #\: out)
                  (write-json (cdr pair) out)
                  (when more (write-char #\, out)))
         (write-char #\} out))
        ((vectorp v)
         (write-char #\[ out)
         (loop for i from 0 below (length v)
               do (when (plusp i) (write-char #\, out))
                  (write-json (aref v i) out))
         (write-char #\] out))
        (t (error "write-json: cannot encode ~s" v))))

(defun json-string (v)
  (with-output-to-string (s) (write-json v s)))

(defun read-json (string &optional (start 0))
  "Parse one JSON value from STRING at START. -> (values value end)."
  (declare (type simple-string string) (type fixnum start))
  (let ((i start) (n (length string)))
    (declare (type fixnum i n))
    (labels ((peek () (if (< i n) (schar string i) #\Nul))
             (skip-ws ()
               (loop while (and (< i n) (member (schar string i) '(#\Space #\Tab #\Newline #\Return)))
                     do (incf i)))
             (expect (c)
               (unless (char= (peek) c)
                 (error "read-json: expected ~s at ~d" c i))
               (incf i))
             (str ()
               (expect #\")
               ;; fast path: no escapes in the run, so one SUBSEQ copies it — this is
               ;; what keeps a 300 KB base64 screencast frame cheap to parse
               (let ((end (position-if (lambda (c) (or (char= c #\") (char= c #\\))) string :start i)))
                 (unless end (error "read-json: unterminated string"))
                 (if (char= (schar string end) #\")
                     (prog1 (subseq string i end) (setf i (1+ end)))
                     (with-output-to-string (o)
                       (loop
                         (let ((c (schar string i)))
                           (cond ((char= c #\") (incf i) (return))
                                 ((char= c #\\)
                                  (let ((e (schar string (1+ i))))
                                    (incf i 2)
                                    (case e
                                      (#\n (write-char #\Newline o))
                                      (#\t (write-char #\Tab o))
                                      (#\r (write-char #\Return o))
                                      (#\b (write-char #\Backspace o))
                                      (#\f (write-char #\Page o))
                                      (#\u (let ((code (parse-integer string :start i :end (+ i 4) :radix 16)))
                                             (incf i 4)
                                             ;; a UTF-16 surrogate pair: two \u escapes, one character
                                             (when (and (<= #xD800 code #xDBFF)
                                                        (< (+ i 5) n)
                                                        (char= (schar string i) #\\)
                                                        (char= (schar string (1+ i)) #\u))
                                               (let ((lo (parse-integer string :start (+ i 2) :end (+ i 6) :radix 16)))
                                                 (when (<= #xDC00 lo #xDFFF)
                                                   (incf i 6)
                                                   (setf code (+ #x10000 (ash (- code #xD800) 10) (- lo #xDC00))))))
                                             (write-char (code-char code) o)))
                                      (t (write-char e o)))))
                                 (t (write-char c o) (incf i)))))))))
             (num ()
               (let ((s i))
                 (loop while (and (< i n) (find (schar string i) "+-0123456789.eE")) do (incf i))
                 (let ((txt (subseq string s i)))
                   (if (find-if (lambda (c) (find c ".eE")) txt)
                       (let ((*read-default-float-format* 'double-float))
                         (coerce (let ((*read-eval* nil)) (read-from-string txt)) 'double-float))
                       (parse-integer txt)))))
             (val ()
               (skip-ws)
               (let ((c (peek)))
                 (cond
                   ((char= c #\{)
                    (incf i) (skip-ws)
                    (if (char= (peek) #\}) (progn (incf i) '())
                        (loop collect (progn (skip-ws)
                                             (let ((k (str)))
                                               (skip-ws) (expect #\:)
                                               (cons k (val))))
                              do (skip-ws)
                              until (char= (peek) #\})
                              do (expect #\,)
                              finally (incf i))))
                   ((char= c #\[)
                    (incf i) (skip-ws)
                    (if (char= (peek) #\]) (progn (incf i) (vector))
                        (coerce (loop collect (val)
                                      do (skip-ws)
                                      until (char= (peek) #\])
                                      do (expect #\,)
                                      finally (incf i))
                                'simple-vector)))
                   ((char= c #\") (str))
                   ((string= "true" string :start2 i :end2 (min n (+ i 4))) (incf i 4) t)
                   ((string= "false" string :start2 i :end2 (min n (+ i 5))) (incf i 5) :false)
                   ((string= "null" string :start2 i :end2 (min n (+ i 4))) (incf i 4) :null)
                   (t (num))))))
      (values (val) i))))

;;; ---- base64: how CDP carries bytes inside JSON ------------------------------
;;; A screencast frame is a JPEG, and JSON strings cannot hold arbitrary bytes, so
;;; Chrome base64-encodes it: every 3 bytes become 4 characters from a 64-letter
;;; alphabet (A-Z a-z 0-9 + /), each carrying 6 bits; '=' pads the tail.

(defparameter +b64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defparameter +b64-decode+
  (let ((v (make-array 256 :element-type '(signed-byte 8) :initial-element -1)))
    (loop for c across +b64-alphabet+ for i from 0 do (setf (aref v (char-code c)) i))
    v))

(defun base64-decode (s)
  "The octets the base64 string S encodes (whitespace and padding ignored)."
  (declare (type simple-string s) (optimize (speed 3) (safety 0)))
  (let* ((n (length s))
         (out (make-array (floor (* n 3) 4) :element-type '(unsigned-byte 8)))
         (table +b64-decode+)
         (acc 0) (bits 0) (o 0))
    (declare (type fixnum n acc bits o)
             (type (simple-array (signed-byte 8) (256)) table)
             (type (simple-array (unsigned-byte 8) (*)) out))
    (dotimes (i n)
      (let ((d (aref table (logand 255 (char-code (schar s i))))))
        (when (>= d 0)
          (setf acc (logior (logand (ash acc 6) #xFFFFFF) d))
          (incf bits 6)
          (when (>= bits 8)
            (decf bits 8)
            (setf (aref out o) (logand 255 (ash acc (- bits))))
            (incf o)))))
    (subseq out 0 o)))

(defun base64-encode (octets)
  "OCTETS as a base64 string (used to hand a pasted file or a test page to Chrome)."
  (let ((n (length octets)))
    (with-output-to-string (o)
      (loop for i from 0 below n by 3
            do (let* ((b0 (aref octets i))
                      (b1 (if (< (+ i 1) n) (aref octets (+ i 1)) 0))
                      (b2 (if (< (+ i 2) n) (aref octets (+ i 2)) 0))
                      (w (logior (ash b0 16) (ash b1 8) b2)))
                 (write-char (char +b64-alphabet+ (ldb (byte 6 18) w)) o)
                 (write-char (char +b64-alphabet+ (ldb (byte 6 12) w)) o)
                 (write-char (if (< (+ i 1) n) (char +b64-alphabet+ (ldb (byte 6 6) w)) #\=) o)
                 (write-char (if (< (+ i 2) n) (char +b64-alphabet+ (ldb (byte 6 0) w)) #\=) o))))))

;;; ---- the Chrome process -----------------------------------------------------

(defstruct (chrome (:constructor %make-chrome))
  process            ; the SB-EXT process (the sh that became Chrome, or the container helper)
  to-fd              ; we WRITE commands here (Chrome's fd 3)
  from-fd            ; we READ responses + events here (Chrome's fd 4)
  (buf (make-array 65536 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (next-id 1)
  ;; Both tables are SYNCHRONIZED, and writes go through LOCK, because a second
  ;; thread may send: the net REPL, from which the guest checks ask the page
  ;; questions (WEB-JS) while the browser's own loop reads the replies.
  (lock (sb-thread:make-mutex :name "chrome pipe"))
  (replies (make-hash-table :synchronized t))   ; id -> a response parked for CDP-CALL
  (callbacks (make-hash-table :synchronized t)) ; id -> a function of the response, or :PARK.
                                ; A reply with neither is DROPPED: most commands (an
                                ; input event, a frame ack) are fire-and-forget, and
                                ; parking every answer would grow the table forever.
  (on-event nil)                ; function (method params session-id) for every event
  (log nil)                     ; the stderr log file, for the doc's "why did it die"
  (started (get-internal-real-time)))

(defun chrome-alive-p (c)
  (and c (chrome-process c)
       (member (sb-ext:process-status (chrome-process c)) '(:running :stopped))))

(sb-alien:define-alien-routine ("fcntl" %cdp-fcntl) sb-alien:int
  (fd sb-alien:int) (cmd sb-alien:int) (arg sb-alien:long))

(defun set-nonblocking (fd)
  "O_NONBLOCK on FD (F_GETFL = 3, F_SETFL = 4, O_NONBLOCK = #x800 on x86-64 Linux):
   a read with nothing waiting returns at once instead of sleeping, so the browser's
   event loop can drain the pipe without ever stalling on it."
  (let ((fl (%cdp-fcntl fd 3 0)))
    (%cdp-fcntl fd 4 (logior fl #x800))))

(defun shell-quote (s)
  "S wrapped for /bin/sh: single quotes, and each embedded ' written as '\\''."
  (with-output-to-string (o)
    (write-char #\' o)
    (loop for c across s do (if (char= c #\') (write-string "'\\''" o) (write-char c o)))
    (write-char #\' o)))

(defun pipe-wrapper (argv log)
  "The /bin/sh -c script that hands ARGV the DevTools pipe on fds 3 and 4.

   exec 3<&0 4>&1   our stdin pipe becomes fd 3 (Chrome reads commands there), our
                    stdout pipe becomes fd 4 (it writes replies there);
   >LOG 2>&1        Chrome's own chatter goes to a file, so it can never land in the
                    middle of the protocol stream;
   </dev/null       and its stdin is nothing.
   The final exec replaces the shell, so the process we hold IS Chrome (or, in the
   guest, the container helper that becomes it)."
  (format nil "exec 3<&0 4>&1 >>~a 2>&1 </dev/null; exec~{ ~a~}"
          (shell-quote log) (mapcar #'shell-quote argv)))

(defun start-chrome-process (argv &key (prefix #'identity) (log "/tmp/lol-chrome.log") env)
  "Start ARGV (Chrome's command line) with the DevTools pipe on fds 3/4. -> a CHROME.
   The fd juggling is done by a /bin/sh; PREFIX, a function of an argv returning an
   argv, decides where that sh runs — on the host, as it is (IDENTITY); in the guest,
   inside a container whose /bin/sh is the host's (web.lisp decides). ENV is the
   child's environment, as a list of \"NAME=value\" strings."
  (let* ((full (funcall prefix (list "/bin/sh" "-c" (pipe-wrapper argv log))))
         (p (sb-ext:run-program (first full) (rest full)
                                :input :stream :output :stream :error nil
                                :wait nil
                                :environment (or env (sb-ext:posix-environ))))
         (to (sb-sys:fd-stream-fd (sb-ext:process-input p)))
         (from (sb-sys:fd-stream-fd (sb-ext:process-output p))))
    (set-nonblocking from)
    (set-nonblocking to)                 ; a write must never wait on Chrome for good
    (%make-chrome :process p :to-fd to :from-fd from :log log)))

;;; ---- writing ------------------------------------------------------------------

(defparameter +cdp-write-seconds+ 5
  "How long a write may wait for Chrome to make room in its command pipe.")

(defun cdp-write-octets (c octets)
  "Write all of OCTETS to Chrome's command pipe. A short write means 'the rest next';
   EINTR (a signal landed) is retried. The pipe is NON-blocking: when it is full —
   64 KB that Chrome has not read, because it is hung or stopped — we wait for room
   at most +CDP-WRITE-SECONDS+. It used to block, and the writer is the
   environment's thread: a stuck Chrome froze the screen (review of 2026-10-16).
   Past the wait the pipe is CLOSED: half a message has corrupted the stream anyway,
   and Chrome exits when its command pipe closes, which the reader sees as EOF, so
   the pane says Chrome exited and offers r to restart it."
  (unless (chrome-to-fd c) (error "Chrome's command pipe is closed (it stopped reading)"))
  (sb-sys:with-pinned-objects (octets)
    (let ((sap (sb-sys:vector-sap octets)) (n (length octets)) (off 0)
          (deadline (+ (get-internal-real-time) (* +cdp-write-seconds+ internal-time-units-per-second))))
      (loop while (< off n) do
        (multiple-value-bind (w err) (sb-unix:unix-write (chrome-to-fd c) sap off (- n off))
          (cond (w (incf off w))
                ((eql err sb-unix:eintr))
                ((eql err sb-unix:ewouldblock)
                 (let ((left (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
                   (unless (and (plusp left)
                                (sb-sys:wait-until-fd-usable (chrome-to-fd c) :output left))
                     (let ((fd (shiftf (chrome-to-fd c) nil)))
                       ;; the number stays OURS until the stream is closed: a bare
                       ;; close(fd) could see it reused, and a later write land elsewhere
                       (ignore-errors (close (sb-ext:process-input (chrome-process c)) :abort t))
                       (session-log "web: Chrome did not read its pipe for ~d s (fd ~d closed)"
                                    +cdp-write-seconds+ fd))
                     (error "Chrome stopped reading its command pipe (~d s)" +cdp-write-seconds+))))
                (t (error "chrome pipe write failed (errno ~a) — has Chrome exited?" err))))))))

(defun cdp-send (c method &optional params &key session callback)
  "Send METHOD with PARAMS (a JOBJ, or NIL) — to the tab SESSION when given, else to
   the browser itself. -> the message id. CALLBACK, if given, is called with the
   response (an alist with \"result\" or \"error\") when it arrives."
  (sb-thread:with-recursive-lock ((chrome-lock c))
    (let* ((id (shiftf (chrome-next-id c) (1+ (chrome-next-id c))))
           (msg (apply #'jobj "id" id "method" method "params" params
                       (and session (list "sessionId" session))))
           (octets (sb-ext:string-to-octets (json-string msg) :external-format :utf-8))
           (framed (make-array (1+ (length octets)) :element-type '(unsigned-byte 8)
                                                    :initial-element 0)))
      (replace framed octets)              ; ... and the last byte stays 0: the NUL frame end
      (when callback (setf (gethash id (chrome-callbacks c)) callback))   ; a function or :PARK
      (cdp-write-octets c framed)
      id)))

(defun cdp-send-built (c build &key callback)
  "Send a message BUILD makes itself: BUILD is called with the message id and returns
   the whole framed octets (the JSON and its NUL). For a message too big to go through
   JOBJ and JSON-STRING character by character — the shared screen's pixels, about
   9 MB a frame (web.lisp, WEB-SHARE-SEND). -> the id."
  (sb-thread:with-recursive-lock ((chrome-lock c))
    (let ((id (shiftf (chrome-next-id c) (1+ (chrome-next-id c)))))
      (when callback (setf (gethash id (chrome-callbacks c)) callback))
      (cdp-write-octets c (funcall build id))
      id)))

;;; ---- reading ------------------------------------------------------------------

(defun cdp-read-available (c)
  "Append whatever Chrome has written to our buffer, without blocking. -> T when any
   bytes arrived, :EOF when Chrome closed its end (it exited)."
  (let ((chunk (make-array 65536 :element-type '(unsigned-byte 8)))
        (any nil))
    (loop
      (multiple-value-bind (n err)
          (sb-sys:with-pinned-objects (chunk)
            (sb-unix:unix-read (chrome-from-fd c) (sb-sys:vector-sap chunk) (length chunk)))
        (cond ((and n (plusp n))
               (setf any t)
               (let* ((buf (chrome-buf c)) (fp (fill-pointer buf)))
                 (when (> (+ fp n) (array-dimension buf 0))
                   (setf buf (adjust-array buf (max (* 2 (array-dimension buf 0)) (+ fp n)))
                         (chrome-buf c) buf))
                 (setf (fill-pointer buf) (+ fp n))
                 (replace buf chunk :start1 fp :end2 n)))
              ((and n (zerop n)) (return :eof))
              ((eql err sb-unix:eintr))
              (t (return any)))))))                       ; EAGAIN: drained

(defun cdp-take-messages (c)
  "Cut every complete (NUL-terminated) message out of the buffer and parse it.
   -> a list of messages, oldest first; an incomplete tail stays for next time."
  (let* ((buf (chrome-buf c)) (n (fill-pointer buf)) (start 0) (out '()))
    (loop for nul = (position 0 buf :start start :end n)
          while nul
          do (let ((text (sb-ext:octets-to-string buf :start start :end nul :external-format :utf-8)))
               (push (handler-case (read-json (coerce text 'simple-string))
                       (error (e) (list (cons "parse-error" (princ-to-string e)))))
                     out))
             (setf start (1+ nul)))
    (when (plusp start)
      (replace buf buf :start2 start :end2 n)
      (setf (fill-pointer buf) (- n start)))
    (nreverse out)))

(defun cdp-dispatch (c msg)
  "Route one message: a response parks under its id (or runs its callback); an event
   goes to the CHROME's ON-EVENT."
  (let ((id (jget msg "id")))
    (cond (id
           (let ((cb (gethash id (chrome-callbacks c))))
             (remhash id (chrome-callbacks c))
             (cond ((eq cb :park) (setf (gethash id (chrome-replies c)) msg))
                   (cb (handler-case (funcall cb msg)
                         (error (e) (format *error-output* "~&cdp callback: ~a~%" e)))))))
          ((jget msg "method")
           (when (chrome-on-event c)
             (funcall (chrome-on-event c) (jget msg "method") (jget msg "params")
                      (jget msg "sessionId")))))))

(defun cdp-pump (c)
  "Read and dispatch everything Chrome has sent, without blocking. -> T when anything
   was dispatched, :EOF when Chrome has gone."
  (let ((r (cdp-read-available c))
        (msgs (cdp-take-messages c)))
    (dolist (m msgs) (cdp-dispatch c m))
    (cond ((eq r :eof) :eof)
          (msgs t))))

(sb-alien:define-alien-routine ("poll" %cdp-poll) sb-alien:int
  (fds (* sb-alien:unsigned-char)) (nfds sb-alien:unsigned-long) (timeout sb-alien:int))

(defun cdp-wait-readable (c ms)
  "Sleep until Chrome's pipe has data, at most MS milliseconds (one pollfd: the fd in
   its first 4 bytes, POLLIN = 1 in the next 2)."
  (sb-alien:with-alien ((pfd (sb-alien:array sb-alien:unsigned-char 8)))
    (let ((fd (chrome-from-fd c)))
      (dotimes (i 8) (setf (sb-alien:deref pfd i) 0))
      (dotimes (b 4) (setf (sb-alien:deref pfd b) (ldb (byte 8 (* 8 b)) fd)))
      (setf (sb-alien:deref pfd 4) 1)
      (%cdp-poll (sb-alien:cast pfd (* sb-alien:unsigned-char)) 1 ms))))

(define-condition cdp-error (error)
  ((method :initarg :method :reader cdp-error-method)
   (detail :initarg :detail :reader cdp-error-detail))
  (:report (lambda (e s) (format s "Chrome refused ~a: ~a" (cdp-error-method e) (cdp-error-detail e)))))

(defun cdp-call (c method &optional params &key session (timeout 10))
  "Send METHOD and WAIT for its answer (events arriving meanwhile are dispatched as
   usual). -> the \"result\" object. Signals CDP-ERROR when Chrome answers with an
   error, and an ERROR on timeout or when Chrome exits."
  (let ((id (cdp-send c method params :session session :callback :park))
        (deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop
      (let ((reply (gethash id (chrome-replies c))))
        (when reply
          (remhash id (chrome-replies c))
          (let ((err (jget reply "error")))
            (when err (error 'cdp-error :method method :detail (or (jget err "message") err))))
          (return (jget reply "result"))))
      (when (> (get-internal-real-time) deadline)
        (error "Chrome did not answer ~a within ~ds" method timeout))
      (cdp-wait-readable c 50)
      (when (eq (cdp-pump c) :eof)
        (error "Chrome exited while we waited for ~a (its log: ~a)" method (chrome-log c))))))

(defun cdp-eval (c session expression &key (timeout 10))
  "Evaluate the JavaScript EXPRESSION in the tab SESSION and return its value (strings,
   numbers and booleans come back as themselves; objects as NIL). Runtime.evaluate with
   returnByValue, so the answer is copied out of the page rather than referenced."
  (let ((r (cdp-call c "Runtime.evaluate"
                     (jobj "expression" expression "returnByValue" t "awaitPromise" t)
                     :session session :timeout timeout)))
    (when (jget r "exceptionDetails")
      (error "page script failed: ~a"
             (or (jget r "exceptionDetails" "exception" "description")
                 (jget r "exceptionDetails" "text"))))
    (jget r "result" "value")))

(defun js-string (s)
  "S as a JavaScript string literal (JSON's string syntax is valid JavaScript)."
  (json-string s))

(defun chrome-close (c &key (grace 3))
  "Ask Chrome to quit (Browser.close), give it GRACE seconds, then SIGTERM and SIGKILL.
   Closing our ends of the pipe afterwards releases the fds."
  (when c
    (ignore-errors (cdp-send c "Browser.close"))
    (let ((p (chrome-process c)))
      (when p
        ;; only WAIT here, never read the pipe: this may run on another thread
        ;; than the one that reads it (a REPL, a test), and its few last words fit
        ;; in the pipe's buffer anyway
        (loop repeat (* grace 10)
              while (chrome-alive-p c)
              do (sleep 0.1))
        (when (chrome-alive-p c) (ignore-errors (sb-ext:process-kill p 15)) (sleep 0.5))
        (when (chrome-alive-p c) (ignore-errors (sb-ext:process-kill p 9)))
        (ignore-errors (sb-ext:process-wait p))
        (ignore-errors (sb-ext:process-close p))))
    t))
