;;;; session-log-test.lisp — host test for initramfs/session-log.lisp.
;;;;
;;;;   sbcl --script poc/session-log-test.lisp
;;;;
;;;; Everything the session log does that does not need PID 1, in a scratch directory
;;;; (*LOG-DIR*): stamped lines, the per-file cap, setting the last session aside and
;;;; telling a clean shutdown from a crash, gzipping into archive/ (read back with our
;;;; own inflate AND the system's gzip), pruning, the watcher's STALL report naming
;;;; the function a thread is stuck in and the lock it holds, an unhandled error
;;;; ending only its own thread, and fd 2 captured into stderr.log.
;;;;
;;;; And the mechanism behind one kind of freeze, shown on a plain SBCL: a fatal
;;;; runtime error (lose()) WAITS in LDB for a console command — then, after
;;;; disable_lossage_handler, the same error EXITS. Run as child processes, with a
;;;; stdin that stays open, because a waiting LDB is exactly what a frozen screen is.
;;;;
;;;; Prints one line per check and exits non-zero on any failure.

(load (merge-pathnames "deflate.lisp" *load-truename*))
(load (merge-pathnames "inflate.lisp" *load-truename*))
(load (merge-pathnames "../initramfs/session-log.lisp" *load-truename*))

(defvar *failures* 0)

(defun check (name ok &optional detail)
  (if ok
      (format t "ok    ~a~%" name)
      (progn (incf *failures*)
             (format t "FAIL  ~a~@[~%      ~a~]~%" name detail)))
  (finish-output))

(defvar *scratch* (format nil "/tmp/session-log-test-~d/" (sb-unix:unix-getpid)))
(setf *log-dir* *scratch*)
(ensure-directories-exist *scratch*)

(defun slurp (path)
  (with-open-file (s path :if-does-not-exist nil :external-format :utf-8)
    (if s
        (let ((str (make-string (file-length s))))
          (subseq str 0 (read-sequence str s)))
        "")))

(defun slurp-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s) b)))

(defun spit (path text)
  (with-open-file (o path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string text o)))

(defun count-matches (needle text)
  (loop for start = 0 then (1+ pos)
        for pos = (search needle text :start2 start)
        while pos count t))

(defun files (pattern) (mapcar #'file-namestring (directory (concatenate 'string *scratch* pattern))))

(defun reset-dir ()
  (sb-ext:delete-directory *scratch* :recursive t)
  (ensure-directories-exist *scratch*))

;;; ---- lines -----------------------------------------------------------------

(format t "~&== lines ==~%")
(session-log "hello ~d" 42)
(let ((text (slurp (log-path "session"))))
  (check "a line carries a UTC stamp, the thread and the text"
         (and (search "Z [main] hello 42" text) (char= #\2 (char text 0))) text))

(let ((+log-cap+ 200))
  (declare (special +log-cap+))
  (dotimes (i 20) (append-log-line (log-path "network") (format nil "line ~d of filler text" i))))
(check "past the cap a log is set aside and started afresh"
       (and (some (lambda (n) (search "network." n)) (remove "network.log" (files "*.log") :test #'string=))
            (< (length (slurp (log-path "network"))) 400))
       (files "*.log"))

(session-log-block "a block:" (format nil "first~%second"))
(check "a block is one write: title, then indented lines"
       (search (format nil "[main] a block:~%    first~%    second") (slurp (log-path "session"))))

;;; ---- the last session, set aside -----------------------------------------------

(format t "~&== boot: setting the last session aside ==~%")
(reset-dir)
(spit (log-path "session") (format nil "2026-10-15 02:00:00Z [main] boot~%2026-10-15 02:10:00Z [main] shutdown: power off~%"))
(spit (log-path "stderr") "")
(set-aside-live-logs)
(check "a session ending in the shutdown line was clean" (getf *previous-session* :clean) *previous-session*)
(check "its session.log was renamed with a stamp"
       (and (null (probe-file (log-path "session")))
            (= 1 (length (remove-if-not (lambda (n) (search "session.2" n)) (files "*.log")))))
       (files "*.log"))
(check "an empty log is left alone" (probe-file (log-path "stderr")))

(spit (log-path "session") (format nil "2026-10-15 03:00:00Z [main] boot~%2026-10-15 03:05:00Z [lol-watch] pulse  main busy 40s~%"))
(spit (log-path "network") (format nil "2026-10-15 03:01:00Z wlan0: DHCP leased~%"))
(sleep 1.1)                                ; a different second: a different stamp
(set-aside-live-logs)
(check "a session with no shutdown line was NOT clean" (not (getf *previous-session* :clean)) *previous-session*)
(check "and its last line is kept to say so" (search "pulse  main busy 40s" (getf *previous-session* :last)))

(format t "~&== archive ==~%")
(let ((originals (mapcar (lambda (p) (cons (pathname-name p) (slurp-octets p))) (pending-logs))))
  (archive-pending)
  (check "every set-aside log was archived and removed"
         (and (null (pending-logs)) (= (length originals) (length (files "archive/*.log.gz"))))
         (list (files "*.log") (files "archive/*")))
  (dolist (o originals)
    (let ((gz (format nil "~aarchive/~a.log.gz" *scratch* (car o))))
      (check (format nil "~a.gz decodes to the original (our inflate)" (car o))
             (equalp (lol.inflate:gunzip (slurp-octets gz)) (cdr o)))
      (check (format nil "~a.gz passes gzip -t" (car o))
             (zerop (sb-ext:process-exit-code
                     (sb-ext:run-program "gzip" (list "-t" gz) :search t)))))))

(dotimes (i 35)
  (spit (format nil "~aarchive/session.2026010~dT0000~2,'0dZ.log.gz" *scratch* (1+ (floor i 10)) i) "x"))
(archive-pending)
(let ((kept (sort (remove-if-not (lambda (n) (search "session." n)) (files "archive/*.log.gz")) #'string>)))
  (check (format nil "pruned to the newest ~d" +archive-keep+) (= (length kept) +archive-keep+) (length kept))
  ;; 35 made-up ones + the real one archived above = 36: the six oldest go
  (check "the oldest went, the newest stayed"
         (and (notany (lambda (i) (find (format nil "session.20260101T0000~2,'0dZ.log.gz" i)
                                        kept :test #'string=))
                      '(0 1 2 3 4 5))
              (find "session.20260101T000006Z.log.gz" kept :test #'string=)
              (search "session.2026101" (first kept))
              t)
         kept))

;;; ---- the watcher -------------------------------------------------------------

(format t "~&== watcher: a stall ==~%")
(reset-dir)
(defvar *spin* t)
(defvar *spin-lock* (sb-thread:make-mutex :name "test lock"))
(watch-locks (lambda () (list *spin-lock*)))

(defun zz-spin-forever ()
  "Stands in for the environment's thread stuck in a loop, holding a lock."
  (sb-thread:with-mutex (*spin-lock*)
    (note-main-busy)
    (loop while *spin* do (sb-thread:thread-yield))
    (note-main-idle)))

(let ((+stall-thresholds+ '(1)))
  (declare (special +stall-thresholds+))
  (let ((spinner (sb-thread:make-thread #'zz-spin-forever :name "spinner"))
        (state '()))
    (sleep 0.3)
    (setf state (watch-step state))
    (check "no STALL before the threshold" (not (search "STALL" (slurp (log-path "session")))))
    (sleep 1.0)
    (setf *main-activity* "agent tool lisp-eval")
    (setf state (watch-step state))
    (let ((text (slurp (log-path "session"))))
      (check "a STALL is reported past the threshold, with the activity"
             (search "STALL: the environment's thread has been busy 1s — agent tool lisp-eval" text) text)
      (check "the stuck thread's backtrace names the function it is stuck in"
             (search "ZZ-SPIN-FOREVER" text))
      (check "the lock report names the lock and its holder"
             (search "lock \"test lock\" held by [spinner]" text)))
    (setf state (watch-step state))
    (check "the same threshold is not reported twice"
           (= 1 (count-matches "STALL" (slurp (log-path "session")))))
    (setf *spin* nil)
    (sb-thread:join-thread spinner)
    (setf state (watch-step state))
    (check "the end of the stall is logged"
           (search "responsive again (it was busy" (slurp (log-path "session"))))
    (check "and the watcher forgets it" (null state))))

(check "a pulse line has every field"
       (let ((l (pulse-line)))
         (and (search "pulse  main" l) (search " MB, after GC" l) (search " threads" l)))
       (pulse-line))

;;; ---- dying loudly ---------------------------------------------------------------

(format t "~&== an unhandled error on a thread ==~%")
(setf *previous-invoke-debugger-hook* sb-ext:*invoke-debugger-hook*
      sb-ext:*invoke-debugger-hook* 'log-unhandled)
(let ((th (sb-thread:make-thread (lambda () (error "zz boom ~d" 7)) :name "doomed")))
  (check "the thread ends (aborted), without taking the process"
         (eq :aborted (sb-thread:join-thread th :default :aborted))))
(let ((text (slurp (log-path "session"))))
  (check "the error is logged with its type and message"
         (search "[doomed] UNHANDLED SIMPLE-ERROR: zz boom 7" text) text)
  (check "with the backtrace that raised it" (search "(ERROR \"zz boom ~d\" 7)" text))
  (check "and what became of the thread" (search "thread [doomed] ended by that error" text)))
(setf sb-ext:*invoke-debugger-hook* *previous-invoke-debugger-hook*)

(format t "~&== a fatal runtime error: LDB waits, disabled it exits ==~%")
(defun run-lose (disable)
  "Start a plain SBCL that calls the runtime's lose(), its stdin a pipe we hold open.
   Returns :EXITED with its stderr, or :WAITING if it is still there after 6s."
  (let* ((form (format nil "(progn ~:[~;(sb-alien:alien-funcall (sb-alien:extern-alien \"disable_lossage_handler\" (function sb-alien:void)))~] (sb-alien:alien-funcall (sb-alien:extern-alien \"lose\" (function sb-alien:void sb-alien:c-string)) \"zz test lose\"))" disable))
         (p (sb-ext:run-program "sbcl" (list "--noinform" "--no-sysinit" "--no-userinit" "--eval" form)
                                :search t :wait nil :input :stream :output nil :error :stream)))
    (loop repeat 60 while (sb-ext:process-alive-p p) do (sleep 0.1))
    (if (sb-ext:process-alive-p p)
        (progn (sb-ext:process-kill p 9) (sb-ext:process-wait p) :waiting)
        (values :exited (let ((s (make-string-output-stream)))
                          (loop for l = (read-line (sb-ext:process-error p) nil) while l
                                do (write-line l s))
                          (get-output-stream-string s))))))

(check "by default, lose() WAITS in LDB (this is the frozen screen)" (eq (run-lose nil) :waiting))
(multiple-value-bind (how err) (run-lose t)
  (check "after disable_lossage_handler, lose() EXITS" (eq how :exited) how)
  (check "and says why on stderr" (search "zz test lose" (or err "")) err))

(format t "~&== fd 2 into stderr.log ==~%")
(capture-stderr)
(sb-alien:alien-funcall (sb-alien:extern-alien "write" (function sb-alien:long sb-alien:int sb-alien:c-string sb-alien:unsigned-long))
                        2 (format nil "zz from C~%") 10)
(check "a C write to fd 2 lands in stderr.log" (search "zz from C" (slurp (log-path "stderr"))))
(format *error-output* "")                 ; Lisp's *ERROR-OUTPUT* still works
(check "Lisp's *ERROR-OUTPUT* is a stream on the kept console fd"
       (eq sb-sys:*stderr* *console-stderr*))

(sb-ext:delete-directory *scratch* :recursive t)
(format t "~%~:[all checks passed~;~:*~d FAILED~]~%" (and (plusp *failures*) *failures*))
(sb-ext:exit :code (if (plusp *failures*) 1 0) :abort t)
