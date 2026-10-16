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

(defun home-read-only-p ()
  "True when /proc/mounts lists /home mounted read-only (\"ro\" first in its options)."
  (ignore-errors
    (with-open-file (in "/proc/mounts")
      (loop for line = (read-line in nil) while line
            thereis (let ((f (split-on-space line)))
                      (and (equal (second f) "/home")
                           (eql 0 (search "ro" (fourth f)))
                           (or (= (length (fourth f)) 2) (char= (char (fourth f) 2) #\,))))))))

(defun split-on-space (line)
  (loop with start = 0 for pos = (position #\Space line :start start)
        collect (subseq line start pos) while pos do (setf start (1+ pos))))

(defun release-disk ()
  "Before powering off with the laptop's disk mounted, ask lol-rootd — the one
   root process preinit left behind (doc/storage.org) — to remount it read-only:
   we run as the user and may not unmount anything ourselves. preinit passes its
   pid in $LOL_ROOTD_PID; no variable means no disk, nothing to do. Waits up to
   three seconds for /home to turn read-only, then goes ahead regardless (the
   sync already put the data on disk; at worst the host replays the journal)."
  (let ((pid (ignore-errors (parse-integer (sb-ext:posix-getenv "LOL_ROOTD_PID")))))
    (when pid
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "kill" (function sb-alien:int sb-alien:int sb-alien:int))
       pid 10)                          ; SIGUSR1 on x86-64
      (loop repeat 30 until (home-read-only-p) do (sleep 0.1))
      (format t "~&Disk ~:[NOT remounted read-only (synced only)~;remounted read-only~].~%"
              (home-read-only-p))
      (finish-output))))

(defun power-off ()
  "Sync filesystems and ask the kernel to power off via reboot(2).
   Needs CAP_SYS_BOOT — one of the five capabilities preinit keeps when it drops
   to the user (doc/storage.org); returns to the caller if it is refused."
  (format t "~&Syncing disks and powering off...~%")
  (finish-output)
  ;; Chrome first (web.lisp): Browser.close lets it write its profile — cookies,
  ;; history — while /home is still writable, and its processes exit before
  ;; RELEASE-DISK remounts that read-only
  (when (fboundp 'web-shutdown) (ignore-errors (funcall 'web-shutdown)))
  ;; the session log's last line, while /home is still writable: the next boot reads
  ;; it to tell a shutdown from a crash (session-log.lisp, SET-ASIDE-LIVE-LOGS)
  (session-log-close "power off")
  (sb-alien:alien-funcall (sb-alien:extern-alien "sync" (function sb-alien:void)))
  (release-disk)
  (sleep 1)
  ;; glibc reboot(howto); #x4321fedc = RB_POWER_OFF (ACPI power off).
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "reboot" (function sb-alien:int sb-alien:int))
   #x4321fedc)
  ;; only reached if the power-off was refused
  (session-log-reopen "reboot(2) refused the power-off")
  (format t "~&Power-off failed (need privileges?). Back to the menu.~%")
  (finish-output))
