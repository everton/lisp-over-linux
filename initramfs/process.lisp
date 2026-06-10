;;;; process.lisp — process lifecycle helpers for the supervisor.
;;;;
;;;; The non-menu "actions" PID 1 can take: spawn a throwaway worker child,
;;;; and power the machine off. Both lean on the kernel directly — fork+exec
;;;; via run-program, and reboot(2) via a libc FFI call (no new native lib;
;;;; same trick the line editor uses for termios).

(defun worker-main (id)
  "A throwaway child: prints who it is, does a little Lisp, then exits."
  (format t "    [worker ~a] hello — my pid is ~a~%" id (sb-unix:unix-getpid))
  (finish-output)
  (let ((sum (loop for i from 1 to 1000000 summing i)))
    (format t "    [worker ~a] computed (sum 1..1,000,000) = ~:d~%" id sum))
  (finish-output)
  (sb-ext:exit :code (mod (parse-integer id) 256)))

(defun spawn-worker (id)
  "Fork+exec another copy of ourselves as a worker (run-program reaps it)."
  (sb-ext:run-program "/sbin/lisp" (list "worker" (princ-to-string id))
                      :wait nil :input t :output t :error t))

(defun power-off ()
  "Sync filesystems and ask the kernel to power off via reboot(2).
   Needs PID-1/root privileges; returns to the caller if it is refused."
  (format t "~&Syncing disks and powering off...~%")
  (finish-output)
  (sb-alien:alien-funcall (sb-alien:extern-alien "sync" (function sb-alien:void)))
  (sleep 1)
  ;; glibc reboot(howto); #x4321fedc = RB_POWER_OFF (ACPI power off).
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "reboot" (function sb-alien:int sb-alien:int))
   #x4321fedc)
  ;; only reached if the power-off was refused
  (format t "~&Power-off failed (need privileges?). Back to the menu.~%")
  (finish-output))
