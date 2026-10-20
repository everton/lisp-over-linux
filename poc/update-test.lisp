;;;; update-test.lisp — host test of updating the image from inside it
;;;; (initramfs/update.lisp, process.lisp's restart half, and preinit.c's
;;;; ROOTD_SIG_INSTALL — doc/sbcl-init.org, "Updating from inside").
;;;;
;;;;   sbcl --script poc/update-test.lisp
;;;;
;;;; lol-rootd's install is the REAL C (poc/rootd-install-harness.c #includes
;;;; preinit.c), compiled here with gcc and run under `unshare -rm` — root of a private
;;;; user + mount namespace, so it can remount without being root — with a scratch
;;;; folder bound at its /mnt/host. The fake builds are small files with the right
;;;; magic numbers, under /tmp (or $LOL_UPDATE_TEST_DIR: ~10 MB). Nothing touches the real /boot, ~/.lol or iso_root/. Checks:
;;;;   - build-info read with *READ-EVAL* off; a checkout build only when its sizes
;;;;     match build-info (a build under way is not offered);
;;;;   - PENDING-BUILD: none without lol-rootd, one when /boot/lisp holds another;
;;;;   - INSTALL-PENDING-BUILD: the pair and build-info land in /boot/lisp byte for
;;;;     byte, the stage is emptied, the view is read-only again; a second install
;;;;     keeps the first as *.prev;
;;;;   - refusals leave the installed image alone: wrong magic, a stage file that is
;;;;     a symlink (rootd's O_NOFOLLOW), a truncated build;
;;;;   - KEXEC-CMDLINE drops what the kernel adds itself; KEXEC-LOAD on this kernel
;;;;     fails politely (ENOSYS before tag #31, EPERM without CAP_SYS_BOOT).
;;;;
;;;; The half this cannot show: rootd reading the stage AS THE USER (setfsuid). The
;;;; namespace maps one uid, so the harness runs do_install as uid 0.

(defvar *here* (make-pathname :name nil :type nil :defaults *load-truename*))
(load (merge-pathnames "host-logging.lisp" *here*))   ; the modules log (session-log.lisp)
(handler-bind ((warning #'muffle-warning))
  (dolist (f '("lol-home" "files" "settings" "process" "update"))
    (load (merge-pathnames (format nil "../initramfs/~a.lisp" f) *here*))))

(sb-alien:define-alien-routine ("setenv" %setenv) sb-alien:int
  (name sb-alien:c-string) (value sb-alien:c-string) (overwrite sb-alien:int))

(defvar *failures* 0)
(defun check (name ok)
  (format t "~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *failures*)))

(defvar *base* (format nil "~a/lol-update-test-~d/"
                       (string-right-trim "/" (or (sb-ext:posix-getenv "LOL_UPDATE_TEST_DIR") "/tmp"))
                       (sb-unix:unix-getpid)))
(defun under (p) (concatenate 'string *base* p))

(defun octets (path)
  "PATH's contents, or NIL when it cannot be read."
  (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (when in
      (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
        (read-sequence v in) v))))

(defun write-octets (path v)
  (ensure-directories-exist path)
  (with-open-file (out path :element-type '(unsigned-byte 8) :direction :output
                            :if-exists :supersede)
    (write-sequence v out)))

(defun fake-file (size seed &rest marks)
  "SIZE octets of a pattern from SEED, with MARKS = offset string … written in."
  (let ((v (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size) (setf (aref v i) (mod (+ i seed) 251)))
    (loop for (off s) on marks by #'cddr
          do (loop for c across s for j from off do (setf (aref v j) (char-code c))))
    v))

(defun make-build (stamp seed &key (magic "HdrS") (cpio-size (+ (expt 2 20) 4096)))
  "A fake finished build in the scratch checkout's iso_root/, as build.sh leaves it."
  (let ((bz (fake-file (+ (expt 2 20) 512) seed 0 "MZ" #x202 magic))
        (cpio (fake-file cpio-size seed 0 "070701")))
    (write-octets (under "checkout/iso_root/efi/boot/bootx64.efi") bz)
    (write-octets (under "checkout/iso_root/initramfs.cpio") cpio)
    (with-open-file (out (under "checkout/iso_root/build-info") :direction :output
                                                               :if-exists :supersede)
      (format out "(:stamp ~s :bzimage ~d :initramfs ~d)~%" stamp (length bz) (length cpio)))
    (list bz cpio)))

(defun installed (name) (under (concatenate 'string "host/boot/lisp/" name)))
(defun same-file-p (a b) (ignore-errors (equalp (octets a) (octets b))))

;;; ---- the harness: lol-rootd's install, real C, in a namespace ---------------

(defvar *harness* (under "rootd-install-harness"))
(ensure-directories-exist (under "home/"))
(ensure-directories-exist (under "host/boot/"))   ; a host has /boot; rootd makes lisp/ in it
(let ((cc (sb-ext:run-program "/usr/bin/gcc"
                              (list "-O2" "-w" "-o" *harness*
                                    (namestring (merge-pathnames "rootd-install-harness.c" *here*)))
                              :output *error-output* :error *error-output*)))
  (unless (zerop (sb-ext:process-exit-code cc))
    (format t "FAIL  could not compile the harness~%") (sb-ext:exit :code 1)))

(%setenv "HOME" (string-right-trim "/" (under "home/")) 1)   ; LOL-PATH "stage/" -> here
(setf *build-root* (under "checkout/")
      *installed-dir* (under "host/boot/lisp/")
      *install-timeout* 20)

(check "no build-root: no checkout build" (let ((*build-root* nil)) (null (checkout-build))))
(check "no lol-rootd: nothing pending" (null (pending-build)))

(defvar *rootd*
  (sb-ext:run-program "/usr/bin/unshare" (list "-rm" *harness* (under "home") (under "host"))
                      :wait nil :output :stream :error *error-output*))
(let ((line (ignore-errors (read-line (sb-ext:process-output *rootd*)))))
  (unless (equal line "ready")
    (format t "FAIL  the harness did not start (~s) — no unprivileged user namespaces?~%" line)
    (sb-ext:exit :code 1)))
;; unshare without --fork EXECs the harness: the same pid
(%setenv "LOL_ROOTD_PID" (princ-to-string (sb-ext:process-pid *rootd*)) 1)

;;; ---- build-info --------------------------------------------------------------

(with-open-file (out (under "evil-info") :direction :output)
  (write-string "(:stamp #.(sb-ext:exit :code 3) :bzimage 1 :initramfs 1)" out))
(check "build-info with #. is refused, not evaluated" (null (read-build-info (under "evil-info"))))
(check "no build-info yet: no checkout build" (null (checkout-build)))

(defvar *b1* (make-build "aaaaaaa 2026-10-20T12:03:44Z" 1))
(check "a whole build is offered" (equal (getf (checkout-build) :stamp) "aaaaaaa 2026-10-20T12:03:44Z"))
(check "build-label" (equal (build-label (checkout-build)) "aaaaaaa, built 2026-10-20 12:03 UTC"))
(check "nothing installed yet: it is pending" (pending-build))

;;; ---- the first install ---------------------------------------------------------

(check "install returns the build"
       (equal (getf (ignore-errors (install-pending-build)) :stamp) "aaaaaaa 2026-10-20T12:03:44Z"))
(check "bzImage installed byte for byte"
       (same-file-p (installed "bzImage") (under "checkout/iso_root/efi/boot/bootx64.efi")))
(check "initramfs.cpio installed byte for byte"
       (same-file-p (installed "initramfs.cpio") (under "checkout/iso_root/initramfs.cpio")))
(check "build-info installed" (equal (installed-build) (checkout-build)))
(check "rootd said ok (and the view is read-only again)" (equal (read-install-status) "ok"))
(check "the stage is emptied" (null (probe-file (under "home/.lol/stage/initramfs.cpio"))))
(check "no temporary files left in /boot/lisp" (null (probe-file (installed ".bzImage.new"))))
(check "installed: no longer pending" (null (pending-build)))

;;; ---- a second build replaces it, the first kept as .prev ------------------------

(defvar *b2* (make-build "bbbbbbb+dirty 2026-10-20T13:00:00Z" 2))
(check "a newer build is pending" (equal (getf (pending-build) :stamp) "bbbbbbb+dirty 2026-10-20T13:00:00Z"))
(check "second install" (ignore-errors (install-pending-build)))
(check "the new bzImage is installed" (equalp (octets (installed "bzImage")) (first *b2*)))
(check "the old one is bzImage.prev" (equalp (octets (installed "bzImage.prev")) (first *b1*)))
(check "the old cpio is initramfs.cpio.prev" (equalp (octets (installed "initramfs.cpio.prev")) (second *b1*)))

;;; ---- refusals: the installed image stays ----------------------------------------

(make-build "ccccccc 2026-10-20T14:00:00Z" 3 :magic "XXXX")
(let ((err (handler-case (progn (install-pending-build) nil) (error (c) (princ-to-string c)))))
  (check (format nil "a bzImage without HdrS is refused (~a)" err) (and err (search "magic" err))))
(check "... and the installed build is unchanged" (equalp (octets (installed "bzImage")) (first *b2*)))

(make-build "ddddddd 2026-10-20T15:00:00Z" 4)
(let ((cpio (under "checkout/iso_root/initramfs.cpio")))       ; a build still being written
  (write-octets cpio (subseq (octets cpio) 0 (expt 2 20))))
(check "a cpio shorter than build-info says is not offered" (null (checkout-build)))
(check "... and install refuses it"
       (handler-case (progn (install-pending-build) nil) (error () t)))

;; rootd's own guard: a stage file that is a symlink (never made by the Lisp, which
;; copies — so this is someone else's doing) is not followed
(let ((stage (under "home/.lol/stage/")))
  (ensure-directories-exist stage)
  (dolist (f '("bzImage" "initramfs.cpio" "build-info")) (ignore-errors (delete-file (concatenate 'string stage f))))
  (write-octets (concatenate 'string stage "real-bz") (fake-file (+ (expt 2 20) 512) 5 0 "MZ" #x202 "HdrS"))
  (sb-ext:run-program "/bin/ln" (list "-s" "real-bz" (concatenate 'string stage "bzImage")))
  (write-octets (concatenate 'string stage "initramfs.cpio") (fake-file (+ (expt 2 20) 4096) 5 0 "070701"))
  (with-open-file (out (concatenate 'string stage "build-info") :direction :output)
    (write-string "(:stamp \"eeeeeee x\" :bzimage 1 :initramfs 1)" out))
  (ignore-errors (delete-file *install-status-file*))
  (rootd-signal +rootd-sig-install+)
  (let ((answer (loop repeat 100 thereis (read-install-status) do (sleep 0.1))))
    (check (format nil "a symlinked stage file is refused (~a)" answer)
           (and answer (eql 0 (search "error:" answer)) (search "bzImage" answer)))))
(check "... and the installed build is unchanged" (equalp (octets (installed "bzImage")) (first *b2*)))

;;; ---- restarting -------------------------------------------------------------------

(check "kexec-cmdline drops console=, initrd=, BOOT_IMAGE="
       (equal (kexec-cmdline (format nil "console=ttyS0,115200 console=tty0 initrd=initramfs.cpio ~
                                          BOOT_IMAGE=/boot/lisp/bzImage lol.disk=none  quiet~%"))
              "lol.disk=none quiet"))
(check "kexec-cmdline of the bare built-in line is empty"
       (equal (kexec-cmdline "console=ttyS0,115200 console=tty0 initrd=initramfs.cpio") ""))
(multiple-value-bind (ok why) (kexec-load :kernel (installed "bzImage") :initrd (installed "initramfs.cpio"))
  (check (format nil "kexec-load here fails politely (~a)" why) (and (not ok) (stringp why))))
(multiple-value-bind (ok why) (kexec-load :kernel (under "nope") :initrd (under "nope"))
  (check "kexec-load of a missing file says so" (and (not ok) (search "cannot open" why))))

(sb-ext:process-kill *rootd* 15)
(sb-ext:process-wait *rootd*)
(ignore-errors (delete-file *install-status-file*))
(sb-ext:run-program "/bin/rm" (list "-rf" *base*))
(format t "~:[all ok~;~:*~d FAILED~]~%" (and (plusp *failures*) *failures*))
(sb-ext:exit :code (if (zerop *failures*) 0 1))
