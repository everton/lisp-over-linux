;;;; check-files.lisp — assert STORAGE and the FILE MANAGER in the booted guest.
;;;;
;;;;     LOL_GUEST_DISK=test-disk.img sbcl --script check-files.lisp   ; exits 0 if all pass
;;;;
;;;; Needs the stand-in disk (./make-test-disk.sh): the harness attaches it with
;;;; snapshot=on, so everything the test creates vanishes with the guest. Checks,
;;;; in boot order (doc/storage.org, doc/file-manager.org, doc/previews.org):
;;;;
;;;;   - preinit found the disk, mounted /home rw and /mnt/host ro;
;;;;   - PID 1 runs as the user with exactly the five kept capabilities;
;;;;   - SuperL-f (a real held Super over QMP) opens the file manager at $HOME;
;;;;   - "+" then a typed name makes a folder — on the disk, owned by the user;
;;;;   - libstbi.so loads in the guest and decodes the progressive JPEG;
;;;;   - the Spotter's path grammar completes "~/Pic" to Pictures/;
;;;;   - q on an opened image goes back, and the environment survives it;
;;;;   - power-off asks lol-rootd to remount the disk read-only.
;;;;
;;;; LOL_SHOTS=DIR keeps a screendump of the file manager on Pictures/.
;;;; All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(use-package :lol.guest)

(defun row (label value)
  (format t "~&  ~44a ~a~%" label value)
  (finish-output)
  value)

(defun shot (name)
  (let ((dir (sb-posix:getenv "LOL_SHOTS")))
    (when (and dir (plusp (length dir)))
      (qmp (format nil "{\"execute\":\"screendump\",\"arguments\":{\"filename\":\"~a/~a.ppm\"}}"
                   (string-right-trim "/" dir) name)))))

(defun checks ()
  (unless (sb-posix:getenv "LOL_GUEST_DISK")
    (format t "~&LOL_GUEST_DISK is not set — this check needs the test disk.~%"))
  (format t "~&~%== storage: the mounts and who we are ==~%")
  (let ((home (row "/home in /proc/mounts"
                   (probe "(with-open-file (s \"/proc/mounts\") (loop for l = (read-line s nil) while l when (search \" /home \" l) return (subseq l 0 (position #\\, l))))")))
        (host (row "/mnt/host in /proc/mounts"
                   (probe "(with-open-file (s \"/proc/mounts\") (loop for l = (read-line s nil) while l when (search \" /mnt/host \" l) return (subseq l 0 (position #\\, l))))"))))
    (check "/home is the disk, read-write" (search "ext4 rw" home))
    (check "/mnt/host is the disk, read-only" (search "ext4 ro" host)))
  (check "PID 1 runs as uid 1000"
         (= 1000 (row "uid" (probe-int "(sb-unix:unix-getuid)"))))
  (check "exactly the five kept capabilities (CapEff 4403400)"
         (search "4403400"
                 (row "CapEff" (probe "(with-open-file (s \"/proc/self/status\") (loop for l = (read-line s nil) while l when (search \"CapEff\" l) return (string-trim '(#\\Tab #\\Space) (subseq l 7))))"))))
  (let ((home (row "HOME" (probe "(sb-ext:posix-getenv \"HOME\")"))))
    (format t "~&~%== the file manager ==~%")
    (wait-for-browser)
    (qmp-hold "meta_l" "f") (sleep 2)
    (let ((dir (row "focused folder after SuperL-f"
                    (probe "(ignore-errors (subj-dir-path (pane-subject (current-pane))))"))))
      (check "SuperL-f opens the file manager at HOME"
             (and dir home (string= (string-trim "\"" dir) (string-trim "\"" home)))))
    ;; "+" is Shift-= on a US keyboard; QMP sends the two keys like a person.
    (qmp-hold "shift" "equal") (sleep 1)
    (qmp-type "made-by-test") (qmp-key "ret") (sleep 2)
    (let ((owner (row "owner of ~/made-by-test"
                      (probe "(multiple-value-bind (ok dev ino mode nlink uid) (sb-unix:unix-stat (concatenate 'string (sb-ext:posix-getenv \"HOME\") \"/made-by-test\")) (declare (ignore dev ino nlink)) (and ok (list uid (logand mode #o170000))))"))))
      (check "+ made a folder on the disk, owned by the user"
             (string= owner "(1000 16384)"))))
  (format t "~&~%== previews ==~%")
  (let ((jpeg (row "decode Pictures/progressive.jpg"
                   (probe "(multiple-value-bind (w h px fmt) (lol.image:decode-image-file (concatenate 'string (sb-ext:posix-getenv \"HOME\") \"/Pictures/progressive.jpg\")) (declare (ignore px)) (list w h fmt))" 6.0))))
    (check "libstbi.so loads in the guest; progressive JPEG decodes"
           (string= jpeg "(320 200 :JPEG)")))
  ;; answer a boolean: the labels themselves contain "~/", and the harness's
  ;; marker regex refuses "~" (so the echo of a FORMAT string can never match)
  (let ((hit (row "Spotter has Pictures/ for ~/Pic"
                  (probe "(and (find-if (lambda (r) (search \"Pictures/\" (getf r :label))) (spotter-results \"~/Pic\" nil 10)) t)"))))
    (check "the Spotter completes a ~/ path" (string= hit "T")))
  ;; leave a picture of the file manager on Pictures/, preview beside it
  (probe "(progn (trail-push (subj-dir (concatenate 'string (sb-ext:posix-getenv \"HOME\") \"/Pictures\") :select \"progressive.jpg\")) t)")
  (qmp-key "down") (qmp-key "up") (sleep 2)
  (shot "file-manager")
  ;; q on an opened image is BACK. It used to return :quit and kill the whole
  ;; environment (dropping to the supervisor menu). Popped means the folder is
  ;; focused again; alive means the loop still reads keys, so a down moves the row.
  (format t "~&~%== q is back, never leave ==~%")
  (qmp-key "ret") (sleep 2)
  (row "opened" (probe "(type-of (pane-subject (current-pane)))"))
  (qmp-key "q") (sleep 1.5)
  (check "q on an opened image goes back to its folder"
         (search "Pictures" (row "focused after q"
                                 (probe "(ignore-errors (subj-dir-path (pane-subject (current-pane))))"))))
  (let ((before (probe-int "(pane-sel (current-pane))")))
    (qmp-key "up") (sleep 1.5)
    (check "the environment still reads keys after q"
           (/= before (row "selection after up" (probe-int "(pane-sel (current-pane))")))))
  (format t "~&~%== power-off releases the disk ==~%")
  (probe "(progn (release-disk) t)" 6.0)
  (check "lol-rootd remounted /home read-only"
         (string= "T" (row "home-read-only-p" (probe "(and (home-read-only-p) t)")))))

(run-guest-checks "files" #'checks)
