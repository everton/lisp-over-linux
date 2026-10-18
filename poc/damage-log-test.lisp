;;;; damage-log-test.lisp — host test for initramfs/damage-log.lisp.
;;;;
;;;;     sbcl --script poc/damage-log-test.lisp
;;;;
;;;; A scripted "screen": a canvas changed in rects at known times, each change logged
;;;; the way the recorder logs a repair, and the pointer moved. Replaying the log must
;;;; give, at every frame time, exactly the screen as it was then — pixel for pixel —
;;;; with the pointer drawn on the frame and never into the log's own screen. Also: a
;;;; torn last record (a crash mid-write) ends the replay cleanly, and a file that is not
;;;; a log is refused.
;;;; All original code, under this project's MIT licence.

(let ((here (make-pathname :directory (pathname-directory *load-truename*))))
  (dolist (f '("ttf.lisp" "canvas.lisp" "deflate.lisp" "inflate.lisp" "../initramfs/damage-log.lisp"))
    (load (merge-pathnames f here))))

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~60a ~:[FAIL~;PASS~]~%" name ok)
  (unless ok (incf *fails*)))

(defparameter *w* 64) (defparameter *h* 48)
(defparameter *file* (format nil "/tmp/lol-dmg-test-~d.dmg" (sb-unix:unix-getpid)))

;;; the script: at each time (µs), fill a rect with a colour; snapshots kept per time
(defparameter *changes*
  '((0       0  0 64 48 #x101010)       ; the first record is the whole screen
    (100000  4  4 10 10 #xff0000)
    (250000 20  8 30  5 #x00ff00)
    (400000  0 40 64  8 #x0000ff)
    (900000 10 10 20 20 #x808080)))
(defparameter *pointer* '((150000 5 5 nil) (600000 30 30 nil) (800000 0 0 t)))

(defun paint (canvas x y w h colour)
  (lol.canvas:fill-rect canvas x y w h colour))

(defun screen-at (us)
  "The scripted screen as it is at time US (the oracle)."
  (let ((c (lol.canvas:make-canvas *w* *h*)))
    (loop for (tm x y w h col) in *changes* when (<= tm us) do (paint c x y w h col))
    c))

(defun dot (canvas x y) (lol.canvas:fill-rect canvas x y 2 2 #xffffff))

(format t "~&== write a log ==~%")
(let ((live (lol.canvas:make-canvas *w* *h*))
      (events (sort (append (mapcar (lambda (c) (cons (first c) (cons :rect (rest c)))) *changes*)
                            (mapcar (lambda (p) (cons (first p) (cons :pointer (rest p)))) *pointer*))
                    #'< :key #'car)))
  (with-open-file (s *file* :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (dmg-write-header s *w* *h*)
    (dolist (e events)
      (destructuring-bind (us kind . args) e
        (ecase kind
          (:rect (destructuring-bind (x y w h col) args
                   (paint live x y w h col)
                   ;; what the recorder does with a repair's rect: cut it from the canvas
                   (destructuring-bind (cx cy cw ch) (clip-rect live x y w h)
                     (dmg-write-rects s us (list (list cx cy cw ch (canvas-rect-pixels live cx cy cw ch)))))))
          (:pointer (destructuring-bind (x y hidden) args (dmg-write-pointer s us x y hidden))))))))
(check "the log was written" (probe-file *file*))

(format t "~&== replay it at 10 fps ==~%")
(let ((frames '()))
  (with-open-file (s *file* :element-type '(unsigned-byte 8))
    (dmg-replay s 10 12 (lambda (c) (push (copy-seq (lol.canvas:canvas-pixels c)) frames))
                :draw-pointer #'dot))
  (setf frames (nreverse frames))
  (check "12 frames" (= 12 (length frames)))
  (loop for k from 0 below 12
        for us = (* k 100000)
        for want = (let ((c (screen-at us)))
                     (let ((p (loop with cur = nil for (tm x y hidden) in *pointer*
                                    when (<= tm us) do (setf cur (unless hidden (list x y))) finally (return cur))))
                       (when p (dot c (first p) (second p))))
                     (lol.canvas:canvas-pixels c))
        do (check (format nil "frame ~2d (t = ~,1f s) is the screen then, pointer and all" k (/ us 1e6))
                  (equalp want (nth k frames)))))

(format t "~&== a torn tail, and a file that is no log ==~%")
(let ((size (with-open-file (s *file*) (file-length s))))
  (with-open-file (in *file* :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (- size 7) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      (with-open-file (out *file* :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence bytes out)))))
(check "a log cut short replays without an error"
       (handler-case (with-open-file (s *file* :element-type '(unsigned-byte 8))
                       (dmg-replay s 10 12 (lambda (c) c)) t)
         (error (e) (format t "~&  (the error: ~a)~%" e) nil)))
(with-open-file (s *file* :direction :output :if-exists :supersede) (write-line "hello" s))
(check "a file that is not a log is refused"
       (handler-case (with-open-file (s *file* :element-type '(unsigned-byte 8)) (dmg-open s) nil)
         (error () t)))
(delete-file *file*)

(format t "~&~:[ALL PASS~;~:*~d FAILED~]~%" (and (plusp *fails*) *fails*))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
