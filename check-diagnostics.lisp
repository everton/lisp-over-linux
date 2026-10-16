;;;; check-diagnostics.lisp — boot the image and assert the crash instrumentation.
;;;;
;;;;     sbcl --script check-diagnostics.lisp                  ; diskless: logs in /tmp/lol-log
;;;;     LOL_GUEST_DISK=test-disk.img sbcl --script check-diagnostics.lisp
;;;;                                                           ; as the user: ~/.lol/log
;;;;
;;;; What initramfs/session-log.lisp promises, checked in the running PID 1: the boot
;;;; line, the 4 GiB heap preinit asked for, kernel.panic = 10, fd 2 going to
;;;; stderr.log, the pulse, the environment's thread idle while it waits for input —
;;;; then a real STALL: the environment's thread is made to spin for 13 s and the
;;;; watcher must report it with a backtrace naming the spinning function, and then
;;;; its end. An unhandled error on a thread must end only that thread. Last, the
;;;; per-boot archiving runs inside the image on a scratch directory and its gzip is
;;;; read back by the image's own inflate.
;;;;
;;;; HOW THE STALL IS MADE. The environment's thread spends its idle time in poll(2).
;;;; We INTERRUPT-THREAD it from the net REPL's thread with a function that marks it
;;;; busy and spins: exactly what a stuck Do it or a stuck agent tool looks like from
;;;; the watcher's side. WITH-INTERRUPTS inside it, because an interruption runs with
;;;; interrupts disabled, and the watcher's own backtrace request is a second one.
;;;; What this does not exercise is POLL-INPUT-ONCE marking busy on its own; the
;;;; idle check below covers its other half.
;;;;
;;;; Not checked here, and why: lose() exiting instead of waiting in LDB is shown on a
;;;; plain SBCL by poc/session-log-test.lisp (killing PID 1 would end the run), and a
;;;; crash being recognised at the NEXT boot needs two boots (also host-tested).
;;;;
;;;; All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(use-package :lol.guest)

(defun row (label value)
  (format t "~&  ~44a ~a~%" label value)
  (finish-output)
  value)

(defun yes (form &optional (seconds 5.0))
  (string= (probe form seconds) "T"))

(defun checks ()
  (format t "~&~%== the session log ==~%")
  ;; helpers in the guest: read a log back, search it
  (probe "(defun zz-log (&optional (base \"session\")) (with-open-file (i (log-path base) :if-does-not-exist nil :external-format :utf-8) (if i (let ((s (make-string (file-length i)))) (subseq s 0 (read-sequence s i))) \"\")))" 20.0)
  (probe "(defun zz-has (needle &optional (base \"session\")) (and (search needle (zz-log base)) t))" 20.0)
  (row "log directory" (probe "(log-dir)"))
  (check "session.log has the boot line" (yes "(zz-has \"] boot: pid 1 \")"))
  (check "and says what it found of the last session" (yes "(zz-has \"previous session: \")"))
  (check "the heap is the 4 GiB preinit asked for"
         (eql (row "dynamic-space-size MB" (probe-int "(round (sb-ext:dynamic-space-size) 1048576)")) 4096))
  (check "kernel.panic = 10 (a dead PID 1 reboots)"
         (string= (row "/proc/sys/kernel/panic" (probe "(with-open-file (s \"/proc/sys/kernel/panic\") (read-line s))"))
                  "\"10\""))
  (check "unhandled errors go to LOG-UNHANDLED"
         (yes "(eq sb-ext:*invoke-debugger-hook* 'log-unhandled)"))
  (check "a C write to fd 2 lands in stderr.log"
         (progn (probe "(sb-alien:alien-funcall (sb-alien:extern-alien \"write\" (function sb-alien:long sb-alien:int sb-alien:c-string sb-alien:unsigned-long)) 2 (format nil \"zz-fd2~%\") 7)")
                (yes "(zz-has \"zz-fd2\" \"stderr\")")))
  (check "the watcher thread is alive" (yes "(and *watch-thread* (sb-thread:thread-alive-p *watch-thread*))"))
  (sleep 6)
  (check "a pulse was written" (yes "(zz-has \"] pulse  main \")"))
  (row "last pulse" (probe "(let ((l (zz-log))) (subseq l (or (search \"pulse\" l :from-end t) 0) (or (position #\\Newline l :start (or (search \"pulse\" l :from-end t) 0)) (length l))))"))

  (format t "~&~%== the environment's thread ==~%")
  (check "the environment came up" (wait-for-browser))
  (check "and the log says so" (yes "(zz-has \"entering the environment\")"))
  (sleep 1)
  (check "waiting for input, it is IDLE" (string= (row "*main-state*" (probe "*main-state*")) ":IDLE"))

  (format t "~&~%== a stall: the environment's thread spins for 13 s ==~%")
  (probe "(defun zz-wedge (secs) (let ((end (+ (get-internal-real-time) (* secs internal-time-units-per-second)))) (loop while (< (get-internal-real-time) end))))" 20.0)
  (probe "(sb-thread:interrupt-thread (sb-thread:main-thread) (lambda () (sb-sys:with-interrupts (note-main-busy) (with-main-activity (\"zz test wedge\") (zz-wedge 13)))))")
  (sleep 3)
  (check "while it spins, it is BUSY" (string= (row "*main-state*" (probe "*main-state*")) ":BUSY"))
  (sleep 11)                                ; past the 10 s threshold, plus the backtraces
  (check "a STALL was reported, with the activity"
         (yes "(zz-has \"STALL: the environment's thread has been busy 1\")"))
  (check "naming what it was doing" (yes "(zz-has \" zz test wedge\")"))
  (check "with a backtrace through the spinning function" (yes "(zz-has \"(ZZ-WEDGE 13)\")"))
  (sleep 3)
  (check "and its end" (yes "(zz-has \"responsive again (it was busy 1\")"))
  (row "the report" (probe "(let* ((l (zz-log)) (a (search \"STALL\" l))) (and a (subseq l a (min (length l) (+ a 600)))))"))

  (format t "~&~%== a stall inside C: the environment's thread in libc's sleep(3) ==~%")
  ;; the case the spinning one cannot show: stuck in a FOREIGN call (OpenSSL, a
  ;; blocking read). Does the backtrace interrupt still get an answer from there?
  ;; sleep(1) in a loop, because every signal (a GC's, ours) cuts a sleep short.
  (probe "(defun zz-csleep (secs) (let ((end (+ (get-internal-real-time) (* secs internal-time-units-per-second)))) (loop while (< (get-internal-real-time) end) do (sb-alien:alien-funcall (sb-alien:extern-alien \"sleep\" (function sb-alien:unsigned-int sb-alien:unsigned-int)) 1))))" 20.0)
  (probe "(sb-thread:interrupt-thread (sb-thread:main-thread) (lambda () (sb-sys:with-interrupts (note-main-busy) (with-main-activity (\"zz in C\") (zz-csleep 13)))))")
  (sleep 14)
  (check "a STALL in C is reported" (yes "(zz-has \" zz in C\")"))
  ;; It does answer — but the walk cannot get back through libc's sleep (built without
  ;; frame pointers), so the frame that CALLED it (ZZ-CSLEEP) is lost: frame 0 reads
  ;; "bogus stack frame" and the walk resumes further out. Measured 2026-10-16; this
  ;; is why a long C call should be named with WITH-MAIN-ACTIVITY (doc/diagnostics.org §3).
  (check "and its backtrace still came back (an answer, not a timeout)"
         (yes "(let ((l (zz-log))) (and (>= (count-if (lambda (i) (search \"thread [main] backtrace\" l :start2 i :end2 (min (length l) (+ i 24)))) (loop for i from 0 below (length l) collect i)) 2) (not (search \"no answer in\" l)) t))" 20.0))
  (row "the C stall's report" (probe "(let* ((l (zz-log)) (a (search \"STALL:\" l :from-end t))) (and a (subseq l a (min (length l) (+ a 900)))))"))
  (sleep 3)

  (format t "~&~%== the kernel's log, and privacy ==~%")
  (check "kernel.log holds the kernel's own banner" (yes "(zz-has \"Linux version\" \"kernel\")"))
  ;; tag #28: what the kernel itself now offers when it, not the Lisp, is stuck
  (check "#28: pstore registered the EFI backend (a panic's log survives)"
         (yes "(zz-has \"efi_pstore\" \"kernel\")"))
  (check "#28: Magic SysRq is on (Alt-SysRq-w/t/s/u/b/c)"
         (string= (row "/proc/sys/kernel/sysrq" (probe "(with-open-file (s \"/proc/sys/kernel/sysrq\") (read-line s))")) "\"1\""))
  (check "#28: the hung-task detector is on (120 s)"
         (string= (row "hung_task_timeout_secs" (probe "(with-open-file (s \"/proc/sys/kernel/hung_task_timeout_secs\") (read-line s))")) "\"120\""))
  (check "#28: the soft-lockup watchdog is on"
         (string= (row "/proc/sys/kernel/watchdog" (probe "(with-open-file (s \"/proc/sys/kernel/watchdog\") (read-line s))")) "\"1\""))
  (check "every log file is 0600"
         (yes "(every (lambda (b) (= #o600 (logand #o777 (nth-value 3 (sb-unix:unix-stat (log-path b)))))) '(\"session\" \"stderr\" \"kernel\"))"))

  (format t "~&~%== an unhandled error on a thread ==~%")
  (check "the thread ends, aborted"
         (string= (row "join-thread"
                       (probe "(sb-thread:join-thread (sb-thread:make-thread (lambda () (error \"zz guest boom\")) :name \"zz-doomed\") :default :aborted)" 10.0))
                  ":ABORTED"))
  (check "it is logged with its message" (yes "(zz-has \"[zz-doomed] UNHANDLED SIMPLE-ERROR: zz guest boom\")"))
  (check "PID 1 is still here" (eql (probe-int "(sb-unix:unix-getpid)") 1))

  (format t "~&~%== archiving, inside the image ==~%")
  (check "set aside + gzip + read back with the image's inflate"
         (yes "(let ((*log-dir* \"/tmp/zz-logs/\")) (ensure-directories-exist *log-dir*) (with-open-file (o (log-path \"session\") :direction :output :if-exists :supersede) (dotimes (i 500) (format o \"2026-10-16 00:00:00Z [main] pulse line ~d~%\" i))) (let ((orig (with-open-file (i (log-path \"session\") :element-type '(unsigned-byte 8)) (let ((b (make-array (file-length i) :element-type '(unsigned-byte 8)))) (read-sequence b i) b)))) (set-aside-live-logs) (archive-pending) (let ((gz (first (directory \"/tmp/zz-logs/archive/*.gz\")))) (and gz (null (pending-logs)) (not (getf *previous-session* :clean)) (equalp orig (lol.inflate:gunzip (with-open-file (i gz :element-type '(unsigned-byte 8)) (let ((b (make-array (file-length i) :element-type '(unsigned-byte 8)))) (read-sequence b i) b)))))))))" 30.0))
  (check "the guest is still alive at the end" (eql (probe-int "(+ 1 1)") 2)))

(run-guest-checks "check-diagnostics" #'checks :qmp nil)
