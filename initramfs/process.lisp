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
  (let ((pid (rootd-pid)))
    (when pid
      ;; the logs hold files on /home open for writing (stderr.log is fd 2), and the
      ;; kernel refuses to remount a filesystem read-only while one is: EBUSY.
      ;; POWER-OFF has closed them already; a direct call (check-files.lisp) has not.
      (unless *logs-closed* (session-log-close "releasing the disk"))
      (rootd-signal 10)                 ; SIGUSR1 on x86-64
      (loop repeat 30 until (home-read-only-p) do (sleep 0.1))
      (format t "~&Disk ~:[NOT remounted read-only (synced only)~;remounted read-only~].~%"
              (home-read-only-p))
      (finish-output))))

(defun rootd-pid ()
  "lol-rootd's pid — the one root process preinit leaves behind (doc/storage.org),
   passed down in $LOL_ROOTD_PID — or NIL: no variable means no disk and no user."
  (ignore-errors (parse-integer (sb-ext:posix-getenv "LOL_ROOTD_PID"))))

(defun rootd-signal (signal)
  "Send lol-rootd one request. A request is a bare signal number (preinit.c says
   which means what); there is nothing else to send. True when it was delivered."
  (let ((pid (rootd-pid)))
    (and pid
         (zerop (sb-alien:alien-funcall
                 (sb-alien:extern-alien "kill" (function sb-alien:int sb-alien:int sb-alien:int))
                 pid signal)))))

(defun stop-for-reboot (what)
  "Everything a power-off and a restart both do before reboot(2), WHAT naming which
   in the session log's last line."
  ;; Chrome first (web.lisp): Browser.close lets it write its profile — cookies,
  ;; history — while /home is still writable, and its processes exit before
  ;; RELEASE-DISK remounts that read-only
  (when (fboundp 'web-shutdown) (ignore-errors (funcall 'web-shutdown)))
  ;; the session log's last line, while /home is still writable: the next boot reads
  ;; it to tell a shutdown from a crash (session-log.lisp, SET-ASIDE-LIVE-LOGS)
  (session-log-close "~a" what)
  (sb-alien:alien-funcall (sb-alien:extern-alien "sync" (function sb-alien:void)))
  (release-disk)
  (sleep 1))

(defun %reboot (how)
  "reboot(2) with one of its commands (linux/include/uapi/linux/reboot.h). Returns
   only when the kernel refused."
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "reboot" (function sb-alien:int sb-alien:int)) how))

(defconstant +reboot-power-off+ #x4321fedc)  ; LINUX_REBOOT_CMD_POWER_OFF (ACPI)
(defconstant +reboot-restart+   #x01234567)  ; LINUX_REBOOT_CMD_RESTART: the firmware
(defconstant +reboot-kexec+     #x45584543)  ; LINUX_REBOOT_CMD_KEXEC: the loaded kernel

(defun power-off ()
  "Sync filesystems and ask the kernel to power off via reboot(2).
   Needs CAP_SYS_BOOT — one of the five capabilities preinit keeps when it drops
   to the user (doc/storage.org); returns to the caller if it is refused."
  (format t "~&Syncing disks and powering off...~%")
  (finish-output)
  (stop-for-reboot "power off")
  (%reboot +reboot-power-off+)
  ;; only reached if the power-off was refused
  (session-log-reopen "reboot(2) refused the power-off")
  (format t "~&Power-off failed (need privileges?). Back to the menu.~%")
  (finish-output))

;;; ---- restarting: kexec when the kernel can, the firmware otherwise --------
;;;
;;; A restart through the firmware goes POST -> GRUB -> its first entry, which is
;;; ours (/etc/grub.d/06_lisp, doc/micro-distro.org). kexec skips the first two:
;;; the RUNNING kernel loads the next one into memory (kexec_file_load(2), from
;;; file descriptors, the kernel parsing the bzImage itself) and, at
;;; reboot(LINUX_REBOOT_CMD_KEXEC), shuts its devices down and jumps into it. Both
;;; need CAP_SYS_BOOT and nothing more — PID 1 already holds it — and kexec needs
;;; CONFIG_KEXEC_FILE (tag #31). A kernel without it answers ENOSYS, and a restart
;;; falls back to the firmware: nothing is lost by trying.

(defconstant +sys-kexec-file-load+ 320)      ; x86-64 (arch/x86/entry/syscalls/syscall_64.tbl)
(defparameter *installed-kernel* "/mnt/host/boot/lisp/bzImage")
(defparameter *installed-initrd* "/mnt/host/boot/lisp/initramfs.cpio")

(defun kexec-cmdline (&optional (line (ignore-errors (with-open-file (in "/proc/cmdline")
                                                       (read-line in nil "")))))
  "The next kernel's command line: this one's (/proc/cmdline), minus what is not
   ours to pass. Our bzImage carries CONFIG_CMDLINE (console=… initrd=…) and the
   kernel PREPENDS it to whatever it is given (CMDLINE_BOOL without OVERRIDE), so
   passing those again would double them; BOOT_IMAGE= is GRUB's. What is left —
   lol.user=, lol.disk=, a debugging option typed at GRUB — carries over. LINE
   stands in for /proc/cmdline (the host test)."
  (format nil "~{~a~^ ~}"
          (remove-if (lambda (tok)
                       (or (string= tok "")
                           (some (lambda (p) (eql 0 (search p tok)))
                                 '("console=" "initrd=" "BOOT_IMAGE="))))
                     (split-on-space (string-trim '(#\Space #\Newline) (or line ""))))))

(defun %kexec-file-load (kernel-fd initrd-fd cmdline)
  "kexec_file_load(2): 0, or -1 with errno set. The length counts the NUL."
  (sb-alien:alien-funcall
   (sb-alien:extern-alien
    "syscall" (function sb-alien:long sb-alien:long sb-alien:int sb-alien:int
                        sb-alien:unsigned-long sb-alien:c-string sb-alien:unsigned-long))
   +sys-kexec-file-load+ kernel-fd initrd-fd (1+ (length cmdline)) cmdline 0))

(defun kexec-available-p ()
  "Can this kernel kexec, and may we? Asked with two bad file descriptors, so nothing
   is loaded: EBADF means the call got as far as looking at them; ENOSYS is a kernel
   without CONFIG_KEXEC_FILE, EPERM a process without CAP_SYS_BOOT."
  (and (minusp (%kexec-file-load -1 -1 ""))
       (eql (sb-alien:get-errno) 9)))            ; EBADF

(defun kexec-load (&key (kernel *installed-kernel*) (initrd *installed-initrd*)
                        (cmdline (kexec-cmdline)))
  "Load KERNEL + INITRD for a later reboot(LINUX_REBOOT_CMD_KEXEC). Returns T, or
   NIL and a reason (\"ENOSYS: this kernel has no kexec\" …). Loading changes
   nothing until that reboot: a loaded kernel can be replaced or simply ignored."
  (let ((kfd (sb-unix:unix-open kernel sb-unix:o_rdonly 0))
        (ifd (sb-unix:unix-open initrd sb-unix:o_rdonly 0)))
    (unwind-protect
         (cond ((null kfd) (values nil (format nil "cannot open ~a" kernel)))
               ((null ifd) (values nil (format nil "cannot open ~a" initrd)))
               (t
                (let ((r (%kexec-file-load kfd ifd cmdline))
                      (errno (sb-alien:get-errno)))
                  (if (zerop r)
                      t
                      (values nil (case errno
                                    (38 "ENOSYS: this kernel has no kexec (before tag #31)")
                                    (1 "EPERM: no CAP_SYS_BOOT")
                                    (t (format nil "kexec_file_load: ~a"
                                               (sb-int:strerror errno)))))))))
      (when kfd (sb-unix:unix-close kfd))
      (when ifd (sb-unix:unix-close ifd)))))

(defun restart-machine ()
  "Restart into the image installed in /boot/lisp: by kexec when this kernel can
   load it, else through the firmware (and GRUB's first entry). The setting
   :restart-via :firmware skips kexec, for hardware that comes back from it badly.
   Returns to the caller only if the kernel refused both."
  (multiple-value-bind (loaded why)
      (if (eq (setting :restart-via) :firmware)
          (values nil "the :restart-via setting says :firmware")
          (kexec-load))
    (format t "~&Syncing disks and restarting ~:[through the firmware (~a)~;by kexec~*~]...~%"
            loaded why)
    (finish-output)
    (stop-for-reboot (if loaded "restart (kexec)" "restart (firmware)"))
    (when loaded (%reboot +reboot-kexec+))
    (%reboot +reboot-restart+)
    (session-log-reopen "reboot(2) refused the restart")
    (format t "~&Restart failed (need privileges?). Back to the menu.~%")
    (finish-output)))
