;;;; mixer-test.lisp — host test for initramfs/mixer.lisp (doc/screencast.org §3.1).
;;;;
;;;;     sbcl --script poc/mixer-test.lisp
;;;;
;;;; The byte-level helpers round-trip, and — when the build host has a sound card the
;;;; user may open (Debian's logind gives the seat's user an ACL on /dev/snd/*) — the
;;;; ioctls read it: the controls listed, a volume and its switch found, the percent in
;;;; range. READ-ONLY: it never writes the host's mixer (that would change the volume
;;;; you are listening at). Writing is proved in the guest by check-sound.lisp.
;;;; All original code, under this project's MIT licence.

(load (merge-pathnames "../initramfs/mixer.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~50a ~:[FAIL~;PASS~]~%" name ok)
  (unless ok (incf *fails*)))

(format t "~&== the byte helpers ==~%")
(let ((b (snd-bytes 16)))
  (setf (snd-u32 b 0) #xc4c85512)
  (check "u32 round-trips" (= (snd-u32 b 0) #xc4c85512))
  (check "u32 is little-endian" (equal (coerce (subseq b 0 4) 'list) '(#x12 #x55 #xc8 #xc4)))
  (setf (snd-s64 b 8) -5)
  (check "s64 round-trips a negative" (= (snd-s64 b 8) -5))
  (setf (snd-s64 b 8) 87)
  (check "s64 round-trips a positive" (= (snd-s64 b 8) 87)))
(let ((b (snd-bytes 64)))
  (replace b (map 'vector #'char-code "Master Playback Volume") :start1 16)
  (check "a control's name is read up to its NUL"
         (string= (snd-id-name b 0) "Master Playback Volume")))

(format t "~&== the host's card (read only) ==~%")
(if (not (with-control-fd (fd sb-unix:o_rdonly) t))
    (format t "~&  (no card the user may open on this host: skipped)~%")
    (let ((controls (mixer-controls)))
      (check "the card lists controls" (plusp (length controls)))
      (check "every control has a name and a count"
             (every (lambda (c) (and (plusp (length (getf c :name))) (integerp (getf c :count)))) controls))
      (let ((vol (volume-control controls)))
        (format t "~&  volume control: ~s ~s..~s ~s~%"
                (getf vol :name) (getf vol :min) (getf vol :max) (getf vol :values))
        (check "a volume control is found" vol)
        (check "its range is a range" (and vol (< (getf vol :min) (getf vol :max))))
        (check "its values are inside it"
               (and vol (every (lambda (v) (<= (getf vol :min) v (getf vol :max))) (getf vol :values))))
        (check "the percent is 0-100" (<= 0 (volume-percent) 100))
        (check "its switch is found beside it" (mute-control controls))
        (format t "~&  ~d% ~:[on~;muted~] via ~a~%" (volume-percent) (volume-muted-p)
                (getf (mute-control controls) :name)))))

(format t "~&~:[ALL PASS~;~:*~d FAILED~]~%" (and (plusp *fails*) *fails*))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
