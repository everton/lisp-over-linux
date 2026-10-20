;;;; update.lisp — install the next build of this image, from inside it, and restart.
;;;;
;;;; doc/sbcl-init.org, "Updating from inside". The environment's terminal builds
;;;; the image as well as Debian does (build.sh in a container, doc/containers.org),
;;;; but it cannot install it: the host's GRUB boots /boot/lisp/{bzImage,
;;;; initramfs.cpio}, which are root's, on a filesystem we only see read-only, and a
;;;; container has no sudo (setuid is off there, by design). Installing used to mean
;;;; booting Debian for one `sudo cp`. Now:
;;;;
;;;;   1. build.sh leaves the pair in the checkout's iso_root/, then build-info, a
;;;;      plist naming the build and the two sizes, written only once both are final;
;;;;   2. PENDING-BUILD compares the checkout's build-info with /boot/lisp's, and
;;;;      the leave menu (SuperL-q) offers "Install the new build and restart";
;;;;   3. INSTALL-PENDING-BUILD copies the three files into ~/.lol/stage/ and sends
;;;;      lol-rootd one signal (42, preinit.c's ROOTD_SIG_INSTALL): it reads the
;;;;      stage as the user, checks each file looks like what it claims, writes
;;;;      /boot/lisp (keeping the previous pair as *.prev) and answers in
;;;;      /tmp/lol-install.status;
;;;;   4. RESTART-MACHINE (process.lisp) boots it — by kexec when this kernel has it
;;;;      (tag #31 on), else through the firmware and GRUB's first entry, which is ours.
;;;;
;;;; Whoever may install an image may run any code as root at the next boot; that
;;;; is what the request is FOR, and doc/storage.org §6 records it.

(defvar *build-root* nil
  "The checkout this image was built from, with a trailing slash — baked by build.sh.
   The next build is looked for in its iso_root/.")

(defconstant +rootd-sig-install+ 42)            ; ROOTD_SIG_INSTALL in preinit.c
(defparameter *installed-dir* "/mnt/host/boot/lisp/")
(defparameter *install-status-file* "/tmp/lol-install.status")
(defparameter *install-timeout* 60
  "Seconds to wait for lol-rootd's answer: it copies ~80 MB and fsyncs it.")

(defparameter +build-files+
  ;; (build-info key, path in the checkout, name in the stage and in /boot/lisp)
  '((:bzimage   "iso_root/efi/boot/bootx64.efi" "bzImage")
    (:initramfs "iso_root/initramfs.cpio"       "initramfs.cpio")
    (:info      "iso_root/build-info"           "build-info")))

(defun read-build-info (path)
  "The plist in a build-info file — (:stamp \"rev date\" :bzimage N :initramfs N) —
   or NIL when there is none or it is not one. Read with *READ-EVAL* off."
  (ignore-errors
    (with-open-file (in path)
      (with-standard-io-syntax
        (let* ((*read-eval* nil)
               (p (read in nil)))
          (and (consp p) (stringp (getf p :stamp)) p))))))

(defun file-octets (path)
  (ignore-errors (with-open-file (s path :element-type '(unsigned-byte 8)) (file-length s))))

(defun checkout-path (key)
  (and *build-root* (concatenate 'string *build-root* (second (assoc key +build-files+)))))

(defun build-whole-p (info path-of)
  "Do the files PATH-OF names (a function of a +BUILD-FILES+ key) have the sizes
   INFO says? A build under way has already removed build-info (build.sh), but one
   that started after we read it has not — the sizes catch that."
  (and info
       (eql (file-octets (funcall path-of :bzimage)) (getf info :bzimage))
       (eql (file-octets (funcall path-of :initramfs)) (getf info :initramfs))))

(defun checkout-build ()
  "The finished build waiting in the checkout: its build-info plist, or NIL."
  (let ((info (and *build-root* (read-build-info (checkout-path :info)))))
    (and (build-whole-p info #'checkout-path) info)))

(defun installed-build ()
  "The build-info of the image in /boot/lisp — NIL when there is none, or when it
   was installed by a build.sh older than build-info."
  (read-build-info (concatenate 'string *installed-dir* "build-info")))

(defun pending-build ()
  "The checkout's build when it can be installed and is not yet: there is a disk
   (lol-rootd exists only with one) and /boot/lisp holds another build."
  (let ((new (and (rootd-pid) (checkout-build))))
    (and new
         (not (equal (getf new :stamp) (getf (installed-build) :stamp)))
         new)))

(defun build-label (info)
  "\"2060c7d+dirty, built 2026-10-20 12:03 UTC\" from a build-info's stamp."
  (let* ((stamp (getf info :stamp))
         (sp (position #\Space stamp))
         (at (and sp (subseq stamp (1+ sp)))))
    (if (and at (>= (length at) 16))
        (format nil "~a, built ~a ~a UTC" (subseq stamp 0 sp)
                (subseq at 0 10) (subseq at 11 16))
        stamp)))

(defun read-install-status ()
  "lol-rootd's one-line answer, once it is whole (it ends in a newline), or NIL."
  (let ((text (ignore-errors
                (with-open-file (in *install-status-file*)
                  (let ((s (make-string (file-length in))))
                    (subseq s 0 (read-sequence s in)))))))
    (and text (plusp (length text)) (char= (char text (1- (length text))) #\Newline)
         (string-right-trim '(#\Newline) text))))

(defun install-pending-build ()
  "Stage the checkout's build in ~/.lol/stage/ and have lol-rootd install it into
   /boot/lisp. Returns its build-info, or signals an ERROR whose text says why —
   in which case the image installed before is still the one that boots."
  (let ((info (checkout-build))
        (stage (lol-path "stage/")))
    (cond ((null (rootd-pid)) (error "no disk on this boot: nothing to install into"))
          ((null info) (error "no finished build in ~aiso_root/" *build-root*))
          ((null stage) (error "no home to stage the build in")))
    (with-main-activity ("installing build ~a" (getf info :stamp))
      (ensure-directories-exist stage)
      (flet ((staged (key) (concatenate 'string stage (third (assoc key +build-files+)))))
        (dolist (f +build-files+)
          (ignore-errors (sb-unix:unix-unlink (staged (first f)))) ; COPY-FILE never overwrites
          (copy-file (checkout-path (first f)) (staged (first f))))
        ;; a build that finished again while we copied would be a mismatched pair
        (unless (and (equal (read-build-info (staged :info)) info) (build-whole-p info #'staged))
          (error "the build changed while it was being staged — try again"))
        (ignore-errors (sb-unix:unix-unlink *install-status-file*))
        (unless (rootd-signal +rootd-sig-install+)
          (error "lol-rootd could not be signalled"))
        (let ((answer (loop repeat (* 10 *install-timeout*)
                            thereis (read-install-status)
                            do (sleep 0.1))))
          (cond ((null answer)
                 (error "lol-rootd did not answer in ~d s" *install-timeout*))
                ((string= answer "ok")
                 (session-log "update: installed ~a into ~a" (getf info :stamp) *installed-dir*)
                 ;; the stage has served: 80 MB in the home is not worth keeping
                 (dolist (f +build-files+) (ignore-errors (sb-unix:unix-unlink (staged (first f)))))
                 info)
                (t (session-log "update: lol-rootd refused ~a: ~a" (getf info :stamp) answer)
                   (dolist (f +build-files+) (ignore-errors (sb-unix:unix-unlink (staged (first f)))))
                   (error "lol-rootd: ~a" answer))))))))
