;;;; check-update.lisp — updating the image from inside it, in a real guest.
;;;;
;;;;     sbcl --script check-update.lisp            ; exits 0 if every assertion passes
;;;;     LOL_UPDATE_DIR=/some/scratch sbcl --script check-update.lisp
;;;;
;;;; Run it from the build host's Debian: inside the environment there is no KVM and
;;;; QEMU's fat: drive cannot write its /var/tmp file. Needs test-disk.img
;;;; (make-test-disk.sh) and a build whose kernel has kexec (tag #31).
;;;;
;;;; Like check-persist.lisp it COPIES test-disk.img and attaches the copy writable;
;;;; the copy is grown by 512 MB first (truncate + resize2fs: no root needed), since an
;;;; update writes ~80 MB twice — into ~/.lol/stage/ and into /boot/lisp. debugfs
;;;; then plants, as build.sh would leave them:
;;;;   - /boot (root's) — the disk's root has none, and a host always does;
;;;;   - the checkout's iso_root/{efi/boot/bootx64.efi, initramfs.cpio} at the path
;;;;     the build baked into *BUILD-ROOT* — THIS build's own pair, owned by 1000 —
;;;;     and a build-info naming a made-up stamp, so the guest sees a build to install.
;;;;
;;;;   boot 1  the build is pending; "zzupdate" typed into the Workspace; SuperL-q and
;;;;           "5" (Install the new build and restart). The guest must come back IN
;;;;           THE SAME QEMU: the harness passes -no-reboot, so a restart through the
;;;;           firmware would end QEMU — staying alive is what proves kexec.
;;;;   boot 2  a new boot (uptime, the net REPL reconnected); the previous session
;;;;           ended with "shutdown: restart (kexec)" and cleanly; /boot/lisp holds the
;;;;           planted stamp; nothing is pending; the stage was emptied; /mnt/host is
;;;;           read-only again; console= is not doubled on the new cmdline; the
;;;;           Workspace text is back (the desk was saved on the way out).
;;;;   between debugfs: /boot/lisp's three files are root's, the pair the checkout's.
;;;;
;;;; doc/sbcl-init.org, "Updating from inside". All original code, MIT.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))

(in-package :lol.guest)

(defun row (label value)
  (format t "~&  ~44a ~a~%" label value) (finish-output) value)

(defun yes (form &optional (seconds 3.0)) (string= (probe form seconds) "T"))

(defparameter *scratch*
  (format nil "~a/lol-update-~d/"
          (string-right-trim "/" (or (sb-posix:getenv "LOL_UPDATE_DIR") "/tmp")) (sb-posix:getpid)))
(defparameter *copy* (concatenate 'string *scratch* "disk.img"))
(defparameter *root-path* (string-right-trim "/" (namestring (truename *root*))))
(defparameter *user-home* "/home/everton")      ; make-test-disk.sh: /home/<build user>
(defparameter *debugfs* "/sbin/debugfs")
(defparameter *stamp* "check-update 2026-01-01T00:00:00Z"
  "The planted build-info's stamp: not the running image's, so the build is pending.")

(defun sh (program &rest args)
  (with-output-to-string (o)
    (sb-ext:run-program program args :search t :output o :error o :wait t)))

(defun debugfs (&rest commands)
  (let ((f (concatenate 'string *scratch* "debugfs.cmd")))
    (with-open-file (o f :direction :output :if-exists :supersede)
      (format o "~{~a~%~}" commands))
    (sh *debugfs* "-w" "-f" f *copy*)))

(defun file-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s)))

(defun plant ()
  "/boot, then the checkout's iso_root/ folders (each owned by 1000) and the build."
  (let* ((iso (concatenate 'string *root-path* "/iso_root/"))
         (info (concatenate 'string *scratch* "build-info"))
         (cmds (list "mkdir /boot"))
         (dir *user-home*))
    (with-open-file (o info :direction :output :if-exists :supersede)
      (format o "(:stamp ~s :bzimage ~d :initramfs ~d)~%" *stamp*
              (file-octets (concatenate 'string iso "efi/boot/bootx64.efi"))
              (file-octets (concatenate 'string iso "initramfs.cpio"))))
    (flet ((owned (p)
             (push (format nil "sif ~a uid 1000" p) cmds)
             (push (format nil "sif ~a gid 1000" p) cmds)))
      ;; every folder from the home down to iso_root/efi/boot (mkdir of one that
      ;; exists — the checkout's, planted by make-test-disk.sh — is a harmless error)
      (dolist (p (remove "" (split-on-slash (concatenate 'string (subseq iso (length *user-home*))
                                                         "efi/boot"))
                         :test #'string=))
        (setf dir (concatenate 'string dir "/" p))
        (push (format nil "mkdir ~a" dir) cmds)
        (owned dir))
      (dolist (f (list (cons (concatenate 'string iso "efi/boot/bootx64.efi") "efi/boot/bootx64.efi")
                       (cons (concatenate 'string iso "initramfs.cpio") "initramfs.cpio")
                       (cons info "build-info")))
        (let ((to (concatenate 'string iso (cdr f))))
          (push (format nil "write ~a ~a" (car f) to) cmds)
          (owned to))))
    (apply #'debugfs (reverse cmds))))

(defun split-on-slash (s)
  (loop with start = 0 for pos = (position #\/ s :start start)
        collect (subseq s start pos) while pos do (setf start (1+ pos))))

(defun stat-field (path field)
  "FIELD (\"User:\", \"Size:\") of PATH in the copy, as debugfs's stat prints it."
  (let* ((out (sh *debugfs* "-R" (format nil "stat ~a" path) *copy*))
         (i (search field out)))
    (and i (parse-integer out :start (+ i (length field)) :junk-allowed t))))

(defun wait-repl-gone (seconds)
  "True once the net REPL stops answering — the old kernel has gone."
  (loop repeat seconds
        unless (ignore-errors (string= (probe "1" 2.0) "1")) do (return t)
        do (sleep 1)))

(defun boot-1 ()
  (format t "~&~%==== boot 1 ====~%") (finish-output)
  (preflight)
  (boot-guest "update")
  (qmp-connect)
  (repl-connect)
  (wait-for-browser 120)
  (row "running build" (probe "*build-stamp*"))
  (row "build-root" (probe "*build-root*"))
  (check "boot 1: persistence is on (the copy's /home, rw)" (yes "(persistence-available-p)"))
  (check "boot 1: the planted build is pending"
         (string= (format nil "~s" *stamp*) (row "pending" (probe "(getf (pending-build) :stamp)" 10.0))))
  (check "boot 1: this kernel can kexec (tag #31), and PID 1 may"
         (yes "(kexec-available-p)"))
  (qmp-type "zzupdate") (sleep 1)
  (check "boot 1: typed into the Workspace"
         (yes "(and (search \"zzupdate\" (lol.textview:tv-text (pane-tv (first (first *trails*))))) t)"))
  (qmp-hold "meta_l" "q") (sleep 2)
  (qmp-key "5")                                        ; Install the new build and restart
  (check "boot 1: the old kernel went away" (wait-repl-gone 240))
  (sleep 5)
  (check "boot 1 -> 2: QEMU is still running (kexec, not the firmware: -no-reboot)"
         (guest-running-p)))

(defun boot-2 ()
  (format t "~&~%==== boot 2 (kexec'd) ====~%") (finish-output)
  (repl-connect)
  (wait-for-browser 120)
  (check "boot 2: a fresh boot"
         (let ((up (row "uptime (s)" (probe-int "(floor (with-open-file (in \"/proc/uptime\") (read in)))"))))
           (and up (< up 300))))
  (let ((last (row "previous session's last line" (probe "(getf *previous-session* :last)"))))
    (check "boot 2: the previous session ended by kexec" (and last (search "restart (kexec)" last))))
  (check "boot 2: ... and cleanly" (yes "(and (getf *previous-session* :clean) t)"))
  (check "boot 2: /boot/lisp holds the planted build"
         (string= (format nil "~s" *stamp*) (row "installed" (probe "(getf (installed-build) :stamp)"))))
  (check "boot 2: nothing pending any more" (yes "(null (pending-build))"))
  (check "boot 2: the stage was emptied" (yes "(null (probe-file (lol-path \"stage/initramfs.cpio\")))"))
  (check "boot 2: /mnt/host is read-only"
         (yes "(with-open-file (in \"/proc/mounts\") (loop for l = (read-line in nil) while l thereis (and (search \" /mnt/host \" l) (search \" ro,\" l) t)))"))
  (check "boot 2: console= is not doubled on the kexec'd cmdline"
         (eql 2 (row "console= on /proc/cmdline"
                     (probe-int "(with-open-file (in \"/proc/cmdline\") (let ((l (read-line in))) (loop for i = (search \"console=\" l) then (search \"console=\" l :start2 (1+ i)) while i count t)))"))))
  (check "boot 2: the Workspace text is back"
         (yes "(and (search \"zzupdate\" (lol.textview:tv-text (pane-tv (first (first *trails*))))) t)"))
  (kill-guest)
  (wait-guest-exit 30))

(defun between ()
  (format t "~&~%==== after (debugfs on the copy) ====~%")
  (dolist (f '("bzImage" "initramfs.cpio" "build-info"))
    (check (format nil "/boot/lisp/~a is root's" f)
           (eql 0 (stat-field (format nil "/boot/lisp/~a" f) "User:"))))
  (check "/boot/lisp/initramfs.cpio is the checkout's size"
         (eql (file-octets (concatenate 'string *root-path* "/iso_root/initramfs.cpio"))
              (row "size" (stat-field "/boot/lisp/initramfs.cpio" "Size:")))))

(defun main ()
  (let ((ok nil))
    (unwind-protect
         (setf ok
               (handler-case
                   (progn
                     (ensure-directories-exist *scratch*)
                     (format t "~&copying test-disk.img to ~a (+512 MB)~%" *copy*)
                     (sh "cp" "--sparse=always" (namestring (merge-pathnames "test-disk.img" *root*)) *copy*)
                     (sh "truncate" "-s" "+512M" *copy*)
                     (row "resize2fs" (subseq (sh "/sbin/resize2fs" *copy*) 0 20))
                     (setf *disk-copy* *copy*)
                     (plant)
                     (check "the build is planted, owned by 1000"
                            (eql 1000 (stat-field (concatenate 'string *root-path* "/iso_root/build-info") "User:")))
                     (boot-1) (boot-2) (between)
                     (report-results))
                 (serious-condition (c)
                   (format *error-output* "~&update: ~a~%" c)
                   (report-results)
                   nil)))
      (kill-guest)
      (sh "rm" "-rf" *scratch*))
    (sb-ext:exit :code (if ok 0 1))))

(main)
