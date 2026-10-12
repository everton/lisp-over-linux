;;;; pty-test.lisp — host test for initramfs/pty.lisp, with the VT engine on top.
;;;;
;;;;   sbcl --script poc/pty-test.lisp
;;;;
;;;; Runs real programs from the build host on real pseudo-terminals: proves the child
;;;; gets the slave as its CONTROLLING terminal (tty, ^C interrupts), that a size set
;;;; with TIOCSWINSZ reaches it (stty size, and SIGWINCH), that the end of a program is
;;;; seen (EIO on the master, the exit status), and — with poc/terminal.lisp parsing
;;;; the output — that a full-screen program (htop, when installed) takes over the
;;;; alternate screen and gives it back. Needs /dev/ptmx and /usr/bin/bash.

(load (merge-pathnames "terminal.lisp" (or *load-truename* *default-pathname-defaults*)))
(load (merge-pathnames "../initramfs/pty.lisp" (or *load-truename* *default-pathname-defaults*)))

(defvar *failures* 0)
(defvar *count* 0)

(defun check (name got want)
  (incf *count*)
  (if (equalp got want)
      (format t "PASS  ~a~%" name)
      (progn (incf *failures*)
             (format t "FAIL  ~a~%      got:  ~s~%      want: ~s~%" name got want))))

(defparameter *env* (list "TERM=xterm-256color" "PATH=/usr/bin:/bin" "LANG=C.UTF-8" "HOME=/tmp"))

(defun drain (proc &key (seconds 2) until term)
  "Read PROC's output for up to SECONDS (or until the string UNTIL appears, or EOF).
   Returns the text read; feeds TERM too when given."
  (let ((buf (make-array 4096 :element-type '(unsigned-byte 8)))
        (out (make-string-output-stream))
        (deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second)))
        (seen ""))
    (loop
      (let ((n (pty-read proc buf)))
        (cond ((eq n :eof) (return))
              ((plusp n)
               (when term (lol.terminal:term-feed term buf :end n))
               (write-string (map 'string #'code-char (subseq buf 0 n)) out)
               (setf seen (concatenate 'string seen (map 'string #'code-char (subseq buf 0 n))))
               (when (and until (search until seen)) (return)))
              (t (sleep 0.02))))
      (when (> (get-internal-real-time) deadline) (return)))
    (get-output-stream-string out)))

(defun wait-exit (proc &optional (seconds 3))
  (loop repeat (* seconds 50)
        until (not (eq (pty-reap proc) :running)) do (sleep 0.02))
  (list (pty-proc-status proc) (pty-proc-code proc)))

(defun send (proc string) (pty-write proc (lol.terminal:utf8-encode string)))

;;; 1. the slave is the controlling terminal, at the size we set
(let* ((p (pty-spawn '("/usr/bin/bash" "-c" "tty; stty size; [ -t 0 ] && echo isatty")
                     :env *env* :cols 100 :rows 30))
       (text (drain p :seconds 3)))
  (check "tty names the slave" (and (search (pty-proc-slave-name p) text) t) t)
  (check "stty size sees TIOCSWINSZ" (and (search "30 100" text) t) t)
  (check "stdin is a terminal" (and (search "isatty" text) t) t)
  (check "EOF then a clean exit" (wait-exit p) '(:exited 0)))

;;; 2. resizing a running program: the kernel's SIGWINCH reaches it
(let ((p (pty-spawn '("/usr/bin/bash" "-c" "trap 'stty size' WINCH; echo ready; while :; do sleep 0.05; done")
                    :env *env* :cols 80 :rows 24)))
  (drain p :until "ready")
  (pty-set-size (pty-proc-master p) 120 40)
  (check "SIGWINCH and the new size" (and (search "40 120" (drain p :until "40 120")) t) t)
  ;; 3. ^C is the line discipline's: it signals the FOREGROUND job of the terminal
  (send p (string (code-char 3)))
  (check "^C interrupts (a controlling terminal)" (wait-exit p) '(:signaled 2)))

;;; 4. exit codes, and closing a running program hangs it up
(let ((p (pty-spawn '("/usr/bin/bash" "-c" "exit 7") :env *env*)))
  (drain p)
  (check "exit code" (wait-exit p) '(:exited 7)))
(let ((p (pty-spawn '("/usr/bin/bash" "-c" "sleep 30") :env *env*)))
  (sleep 0.2)
  (pty-close p)
  (loop repeat 100 while *pty-orphans* do (pty-reap-orphans) (sleep 0.02))
  (check "closing the master hangs the program up" (list (pty-proc-status p) (pty-proc-code p))
         '(:signaled 1)))

;;; 5. errors are conditions, not crashes
(check "a missing program signals"
       (handler-case (progn (pty-spawn '("/nonexistent/prog") :env *env*) :no-error)
         (error () :error))
       :error)

;;; 6. a full-screen program through the engine
(when (probe-file "/usr/bin/htop")
  (let* ((term (lol.terminal:make-term 100 30))
         (p (pty-spawn '("/usr/bin/htop" "-d" "50") :env *env* :cols 100 :rows 30)))
    (drain p :seconds 2 :term term)
    (let ((reply (lol.terminal:term-take-output term))) (when reply (pty-write p reply)))
    (check "htop: on the alternate screen" (lol.terminal:term-alt-screen-p term) t)
    (check "htop: its process header is on screen"
           (and (some (lambda (l) (and (search "PID" l) (search "USER" l)))
                      (lol.terminal:term-screen-text term))
                t)
           t)
    (send p "q")
    (drain p :seconds 2 :term term)
    (check "htop: q leaves the alternate screen" (lol.terminal:term-alt-screen-p term) nil)
    (check "htop: exit 0" (wait-exit p) '(:exited 0))))

(format t "~%~d/~d passed~%" (- *count* *failures*) *count*)
(sb-ext:exit :code (if (zerop *failures*) 0 1))
