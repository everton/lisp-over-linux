;;;; check-smp.lisp — assert the guest has SEVERAL CPUs and that Lisp threads use them.
;;;;
;;;;     LOL_PORT=4026 sbcl --script check-smp.lisp        ; exits 0 if all pass
;;;;     LOL_QEMU_SMP=8 LOL_PORT=4026 sbcl --script check-smp.lisp
;;;;
;;;; Until tag #29 the kernel was built with SMP off (NR_CPUS=1): the laptop's eight
;;;; threads ran as one CPU, and the image's threads (agent turns, netd, the
;;;; recorder's writer, the run panes' readers) only ever took turns. Tag #30 turns
;;;; SMP on (doc/kernel-config.org §10). What this check asserts, in a guest booted
;;;; with LOL_QEMU_SMP CPUs (default 4, the harness's own default):
;;;;
;;;;   - the kernel brought them all up: /sys/devices/system/cpu/online, sysconf;
;;;;   - SBCL threads really run AT THE SAME TIME: N threads of the same compiled
;;;;     busy loop finish in about the time of one (on one CPU they take N times
;;;;     as long), and sched_getcpu() saw them on more than one CPU;
;;;;   - the stop-the-world GC across CPUs: N threads allocating hard while the
;;;;     environment keeps running, a GC forced in the middle, every thread's
;;;;     result right and PID 1 alive afterwards.
;;;;
;;;; No key is ever sent (no QMP). All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(in-package :lol.guest)

(defparameter *cpus*
  (let ((n (sb-posix:getenv "LOL_QEMU_SMP")))
    (if (and n (plusp (length n))) (parse-integer n) 4)))

(defun row (label value) (format t "~&  ~44a ~a~%" label value) (finish-output) value)

;;; Defined in the guest once, then called by the probes. SPIN is compiled (the net
;;; REPL's EVAL compiles), so the timing measures the CPU, not an interpreter.
(defparameter *helpers* "(progn
  (defun zz-spin (n) (declare (fixnum n) (optimize speed)) (let ((a 0)) (declare (fixnum a)) (dotimes (i n a) (setf a (logand (+ a (* i 7)) #xFFFFFF)))))
  (defun zz-cpu () (sb-alien:alien-funcall (sb-alien:extern-alien \"sched_getcpu\" (function sb-alien:int))))
  (defun zz-wall (thunk) (let ((t0 (get-internal-real-time))) (funcall thunk) (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second)))
  (defun zz-parallel (k fn) (let ((ths (loop repeat k collect (sb-thread:make-thread fn :name \"zz-smp\")))) (mapcar #'sb-thread:join-thread ths)))
  (defun zz-alloc (n) (let ((l nil)) (dotimes (i n) (push (make-array 64 :initial-element i) l) (when (> (length l) 1000) (setf l nil))) (length l)))
  t)")

;;; the host's own answer for ZZ-ALLOC, to compare the guest's with
(defun zz-alloc (n) (let ((l nil)) (dotimes (i n) (push (make-array 64 :initial-element i) l) (when (> (length l) 1000) (setf l nil))) (length l)))

(defun checks ()
  (wait-for-browser)
  (format t "~&~%== the kernel brought the CPUs up ==~%")
  ;; on one CPU every assertion below is vacuous or wrong, so say so first, as
  ;; check-compile.lisp does for a CPU with POPCNT (run with LOL_QEMU_SMP=1 on
  ;; 2026-10-19: one thread 692 ms, "1 thread at once" 724 ms, one CPU seen)
  (check "the guest was booted with more than one CPU (else this proves nothing)" (> *cpus* 1))
  (check (format nil "/sys/devices/system/cpu/online is 0-~d" (1- *cpus*))
         (string= (format nil "\"0-~d\"" (1- *cpus*))
                  (row "cpu/online" (probe "(with-open-file (s \"/sys/devices/system/cpu/online\") (read-line s))"))))
  (check (format nil "sysconf(_SC_NPROCESSORS_ONLN) = ~d" *cpus*)
         (eql *cpus* (row "sysconf 84" (probe-int "(sb-alien:alien-funcall (sb-alien:extern-alien \"sysconf\" (function sb-alien:long sb-alien:int)) 84)"))))
  (row "kernel" (probe "(with-open-file (s \"/proc/version\") (subseq (read-line s) 0 40))"))
  (row "cpufreq driver (none under QEMU)"
       (probe "(ignore-errors (with-open-file (s \"/sys/devices/system/cpu/cpu0/cpufreq/scaling_driver\") (read-line s)))"))
  (check "the helpers compiled in the guest" (string= "T" (probe *helpers* 10)))
  (format t "~&~%== Lisp threads run at the same time ==~%")
  ;; calibrate: one SPIN of about half a second, whatever the host's speed
  (let* ((n (or (probe-int "(let ((n 1000000)) (loop while (< (zz-wall (lambda () (zz-spin n))) 400) do (setf n (* n 2))) n)" 60)
                1000000))
         (one (row "one thread, ms"
                   (probe-int (format nil "(zz-wall (lambda () (zz-spin ~d)))" n) 30)))
         (many (row (format nil "~d threads at once, ms" *cpus*)
                    (probe-int (format nil "(zz-wall (lambda () (zz-parallel ~d (lambda () (zz-spin ~d)))))" *cpus* n) 60))))
    (row "loop count" n)
    (check (format nil "~d threads take < 2x one thread (one CPU: ~dx)" *cpus* *cpus*)
           (and one many (plusp one) (< many (* 2 one)))))
  (let ((seen (row "distinct CPUs the threads ran on"
                   (probe-int (format nil "(length (remove-duplicates (zz-parallel ~d (lambda () (zz-spin 20000000) (zz-cpu)))))" (* 2 *cpus*)) 60))))
    (check "sched_getcpu saw the threads on more than one CPU" (and seen (> seen 1))))
  (format t "~&~%== the GC stops every CPU's thread and resumes it ==~%")
  (check (format nil "~d threads allocating, a GC forced meanwhile: every result right" *cpus*)
         (string= (format nil "~s" (loop repeat *cpus* collect (zz-alloc 3000000)))
                  (row "results"
                       (probe (format nil "(let ((ths (loop repeat ~d collect (sb-thread:make-thread (lambda () (zz-alloc 3000000)) :name \"zz-gc\")))) (sleep 0.2) (sb-ext:gc :full t) (mapcar #'sb-thread:join-thread ths))" *cpus*) 120))))
  (check "the environment is still alive afterwards"
         (string= "T" (probe "(and (sb-thread:thread-alive-p sb-thread:*current-thread*) *fb-canvas* t)")))
  (check "no zz- thread left behind"
         (string= "0" (probe "(count-if (lambda (th) (search \"zz-\" (sb-thread:thread-name th))) (sb-thread:list-all-threads))"))))

(run-guest-checks "smp" #'checks :qmp nil)
