;;;; check-crash.lisp — kill PID 1, and assert the NEXT boot knows what happened.
;;;;
;;;;     sbcl --script check-crash.lisp          ; needs test-disk.img (make-test-disk.sh)
;;;;
;;;; check-diagnostics.lisp checks one boot. What a freeze investigation actually
;;;; relies on spans two, so this boots THREE times on a writable COPY of
;;;; test-disk.img (never the image itself; never snapshot=on, which would forget
;;;; everything at the reboot):
;;;;
;;;;   1. boot, log a marker, then make the C runtime call lose() — what heap
;;;;      exhaustion does. LDB is off (session-log.lisp, DISABLE-LDB), so the runtime
;;;;      prints to fd 2 (stderr.log) and exits; PID 1 exiting panics the kernel;
;;;;      kernel.panic=10 (preinit) reboots it; QEMU, run with -no-reboot, exits.
;;;;      Before 2026-10-16 this boot would have sat in LDB forever.
;;;;   2. boot again: session.log must say the last session ENDED WITHOUT A SHUTDOWN,
;;;;      the status line must say so, and the archive must hold the dead session's
;;;;      log (with the marker), stderr (with lose()'s message) and kernel log. Then
;;;;      power off properly.
;;;;   3. boot once more: the last session SHUT DOWN CLEANLY.
;;;;
;;;; About three minutes. All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(use-package :lol.guest)

(defun row (label value)
  (format t "~&  ~44a ~a~%" label value)
  (finish-output)
  value)

(defun yes (form &optional (seconds 8.0)) (string= (probe form seconds) "T"))

(defun boot (name)
  (preflight)
  (boot-guest name)
  (format t "~&booting (~a)…~%" name) (finish-output)
  (repl-connect)
  (wait-for-browser))

(defun wait-exit (seconds)
  "Seconds until QEMU exited, or NIL if it was still running after SECONDS."
  (loop for i from 0 below (* 2 seconds)
        unless (guest-running-p) return (/ i 2.0)
        do (sleep 0.5)))

;; a gunzipped archive file's text, searched in the guest
(defparameter +archive-has+
  "(defun zz-archive-has (base needle) (some (lambda (p) (search needle (sb-ext:octets-to-string (lol.inflate:gunzip (with-open-file (i p :element-type '(unsigned-byte 8)) (let ((b (make-array (file-length i) :element-type '(unsigned-byte 8)))) (read-sequence b i) b))) :external-format '(:utf-8 :replacement #\\?)))) (directory (format nil \"~aarchive/~a.*.gz\" (log-dir) base))))")

(defparameter +log-has+
  "(defun zz-has (needle) (with-open-file (i (log-path \"session\") :external-format :utf-8) (let ((s (make-string (file-length i)))) (and (search needle (subseq s 0 (read-sequence s i))) t))))")

(defun checks ()
  (format t "~&~%== boot 1: run, then die the way heap exhaustion dies ==~%")
  (check "boot 1: the environment came up" (boot "crash1"))
  (row "log directory" (probe "(log-dir)"))
  (sleep 6)                                  ; a pulse or two
  (probe "(session-log \"zz marker before the crash\")")
  (probe "(sb-thread:make-thread (lambda () (sleep 0.5) (sb-alien:alien-funcall (sb-alien:extern-alien \"lose\" (function sb-alien:void sb-alien:c-string)) \"zz deliberate lose\")))")
  (let ((took (wait-exit 60)))
    (row "seconds until the machine reset" took)
    (check "PID 1's death rebooted the machine (no LDB, kernel.panic=10)" took))
  (kill-guest)

  (format t "~&~%== boot 2: the crash is recognised ==~%")
  (check "boot 2: the environment came up" (boot "crash2"))
  (probe +log-has+ 20.0)
  (probe +archive-has+ 20.0)
  (check "session.log says the last session ENDED WITHOUT A SHUTDOWN"
         (yes "(zz-has \"previous session: ENDED WITHOUT A SHUTDOWN\")"))
  (check "the status line says so"
         (yes "(and *browser-status* (search \"without a shutdown\" *browser-status*) t)"))
  (sleep 3)                                  ; the archiving thread
  (row "archive" (probe "(mapcar (function file-namestring) (directory (format nil \"~aarchive/*.*\" (log-dir))))" 10.0))
  (check "the dead session's log is archived, marker included"
         (yes "(and (zz-archive-has \"session\" \"zz marker before the crash\") t)" 20.0))
  (check "its stderr holds the runtime's last words"
         (yes "(and (zz-archive-has \"stderr\" \"zz deliberate lose\") t)" 20.0))
  (check "its kernel log was kept" (yes "(and (zz-archive-has \"kernel\" \"Linux version\") t)" 20.0))
  (check "the archive is private (0600)"
         (yes "(every (lambda (p) (= #o600 (logand #o777 (nth-value 3 (sb-unix:unix-stat (namestring p)))))) (directory (format nil \"~aarchive/*.gz\" (log-dir))))"))
  (probe "(sb-thread:make-thread (lambda () (sleep 0.5) (power-off)))")
  (check "power-off ends the machine" (wait-exit 60))
  (kill-guest)

  (format t "~&~%== boot 3: a clean shutdown is recognised ==~%")
  (check "boot 3: the environment came up" (boot "crash3"))
  (probe +log-has+ 20.0)
  (check "session.log says the last session shut down cleanly"
         (yes "(zz-has \"previous session: shut down cleanly\")"))
  (check "and the status line says nothing about it"
         (not (yes "(and *browser-status* (search \"without a shutdown\" *browser-status*) t)"))))

(let ((copy (format nil "/tmp/lol-crash-~d.img" (sb-posix:getpid)))
      (ok nil))
  (unwind-protect
       (setf ok (handler-case
                    (progn
                      (unless (probe-file (merge-pathnames "test-disk.img" *root*))
                        (error "no test-disk.img — run ./make-test-disk.sh"))
                      (sb-ext:run-program "cp" (list "--sparse=always"
                                                     (namestring (merge-pathnames "test-disk.img" *root*))
                                                     copy)
                                          :search t)
                      (sb-posix:setenv "LOL_GUEST_DISK" copy 1)
                      (setf *disk-snapshot* nil)
                      (checks)
                      (report-results))
                  (serious-condition (c)
                    (format *error-output* "~&check-crash: ~a~%" c)
                    nil)))
    (kill-guest)
    (ignore-errors (delete-file copy)))
  (sb-ext:exit :code (if ok 0 1)))
