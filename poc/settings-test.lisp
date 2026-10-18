;;;; settings-test.lisp — host test for initramfs/settings.lisp.
;;;;
;;;;     sbcl --script poc/settings-test.lisp
;;;;
;;;; The settings file is READ with *PACKAGE* bound to KEYWORD, so that bare words in it
;;;; become keywords. Until 2026-10-18 that made the bare words NIL and T come back as
;;;; :NIL and :T — both TRUE — so a saved (:MUTED NIL) booted the sound muted
;;;; (mixer.lisp's RESTORE-AUDIO; found by check-sound.lisp against the user's real
;;;; settings) and a saved (:TAP-TO-CLICK T) never turned tapping on (fb-browser.lisp
;;;; tests EQ T). This proves both round-trip, with HOME pointed at a scratch
;;;; directory — never the real ~/.lol.
;;;; All original code, under this project's MIT licence.

(require :sb-posix)
(let ((here (make-pathname :directory (pathname-directory *load-truename*))))
  (dolist (f '("../initramfs/lol-home.lisp" "../initramfs/files.lisp" "../initramfs/settings.lisp"))
    (load (merge-pathnames f here))))

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~58a ~:[FAIL~;PASS~]~%" name ok)
  (unless ok (incf *fails*)))

(defvar *home* (format nil "/tmp/lol-settings-test-~d" (sb-unix:unix-getpid)))
(ensure-directories-exist (concatenate 'string *home* "/"))
(sb-posix:setenv "HOME" *home* 1)

(defun write-raw (text)
  "Put TEXT in ~/.lol/settings as a person editing it would."
  (let ((p (concatenate 'string *home* "/.lol/settings")))
    (ensure-directories-exist p)
    (with-open-file (s p :direction :output :if-exists :supersede) (write-string text s))))

(check "the scratch home is the one used" (eql 0 (search *home* (settings-path))))

(format t "~&== written by hand ==~%")
(write-raw "(:muted nil :tap-to-click t :font-size 18 :name word)")
(check "a bare nil reads as NIL, not :NIL" (null (setting :muted :absent)))
(check "a bare t reads as T, not :T" (eq (setting :tap-to-click) t))
(check "a number stays a number" (eql (setting :font-size) 18))
(check "other bare words are still keywords" (eq (setting :name) :word))
(check "a missing key gives the default" (eq (setting :nothing :default) :default))

(format t "~&== written by SAVE-SETTING ==~%")
(check "save :muted nil" (save-setting :muted nil))
(check "save :volume 56" (save-setting :volume 56))
(check ":muted NIL comes back NIL (it booted the sound muted)" (null (setting :muted :absent)))
(check ":volume 56 comes back 56" (eql (setting :volume) 56))
(check "save :muted t" (save-setting :muted t))
(check ":muted T comes back T" (eq (setting :muted) t))
(check "the other keys are kept" (eql (setting :font-size) 18))

(sb-ext:run-program "rm" (list "-rf" *home*) :search t)
(format t "~&~:[ALL PASS~;~:*~d FAILED~]~%" (and (plusp *fails*) *fails*))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
