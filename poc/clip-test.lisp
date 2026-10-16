;;;; clip-test.lisp — host test for the canvas's clip rectangle (poc/canvas.lisp,
;;;; doc/morphic.org §4.1, Morphic step 0).
;;;;
;;;;     sbcl --script poc/clip-test.lisp          ; exits 0 if every check passes
;;;;
;;;; The property every pixel writer must have, checked one writer at a time: draw it
;;;; once with no clip (the reference), then again on a fresh canvas through a clip
;;;; rect. Inside the rect the two must agree pixel for pixel; outside it the clipped
;;;; canvas must still hold the background, untouched. "Agree inside" is what makes
;;;; damage repair correct (a strip redrawn alone looks exactly like that strip of a
;;;; full frame); "untouched outside" is what makes it SAFE (a window repainting its
;;;; rect cannot scribble over the window above).
;;;;
;;;; Also: clips nest by intersection, a non-local exit restores the old clip, an
;;;; empty clip draws nothing, and a frame drawn as clipped tiles is the frame.

(let ((dir (make-pathname :directory (pathname-directory *load-truename*))))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "ttf.lisp" dir))
    (load (merge-pathnames "canvas.lisp" dir))))

(defpackage :lol.clip-test (:use :cl :lol.canvas))
(in-package :lol.clip-test)

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (name ok)
  (incf *checks*)
  (format t "~&  ~64a ~:[FAIL~;PASS~]~%" name ok)
  (unless ok (incf *fails*)))

(defparameter +w+ 160)
(defparameter +h+ 120)
(defparameter +bg+ #x101820)
(defparameter +clip+ '(37 21 70 55) "An awkward rect: odd edges, cuts every shape below.")

(defun fresh ()
  (let ((c (make-canvas +w+ +h+)))
    (fill-rect c 0 0 +w+ +h+ +bg+)
    c))

(defun inside-p (x y)
  (destructuring-bind (cx cy cw ch) +clip+
    (and (<= cx x) (< x (+ cx cw)) (<= cy y) (< y (+ cy ch)))))

(defun clipped-like (name draw)
  "Draw with DRAW (a function of a canvas) unclipped and clipped to +CLIP+; check the
   clipped one equals the reference inside the clip and the background outside, and
   that the drawing touched the clip at all (a vacuous pass proves nothing)."
  (let ((ref (fresh)) (got (fresh)))
    (funcall draw ref)
    (destructuring-bind (cx cy cw ch) +clip+
      (with-clip (got cx cy cw ch) (funcall draw got)))
    (let ((bad-in 0) (bad-out 0) (ink-in 0))
      (dotimes (y +h+)
        (dotimes (x +w+)
          (let ((r (aref (canvas-pixels ref) (+ (* y +w+) x)))
                (g (aref (canvas-pixels got) (+ (* y +w+) x))))
            (if (inside-p x y)
                (progn (unless (= r g) (incf bad-in))
                       (unless (= r +bg+) (incf ink-in)))
                (unless (= g +bg+) (incf bad-out))))))
      (check (format nil "~a: same inside the clip (~d inked)" name ink-in)
             (and (zerop bad-in) (plusp ink-in)))
      (check (format nil "~a: nothing outside it" name) (zerop bad-out)))))

(format t "~&== every primitive, through a clip ==~%")
(clipped-like "fill-rect" (lambda (c) (fill-rect c 10 10 120 90 #xFF0000)))
(clipped-like "draw-rect" (lambda (c) (draw-rect c 30 15 60 50 #x00FF00)))
(clipped-like "draw-line" (lambda (c) (draw-line c 0 0 159 119 #xFFFF00)
                                      (draw-line c 159 0 0 119 #xFFFF00)))
(clipped-like "draw-poly" (lambda (c) (draw-poly c '((20 . 20) (140 . 40) (60 . 110)) #x00FFFF)))
(clipped-like "draw-circle (outline)" (lambda (c) (draw-circle c 70 50 30 #xFF00FF)))
(clipped-like "draw-circle (filled)" (lambda (c) (draw-circle c 70 50 30 #xFF00FF :fill t)))
(let ((rgba (make-array (* 4 50 50) :element-type '(unsigned-byte 8))))
  (dotimes (i (* 50 50))                ; a diagonal half opaque, half transparent
    (setf (aref rgba (* 4 i)) 200 (aref rgba (+ 1 (* 4 i))) 100 (aref rgba (+ 2 (* 4 i))) 50
          (aref rgba (+ 3 (* 4 i))) (if (> (mod i 50) (floor i 50)) 255 0)))
  (clipped-like "blit-rgba" (lambda (c) (blit-rgba c rgba 50 50 25 15))))
(let ((src (make-array (* 60 40) :element-type '(unsigned-byte 32))))
  (dotimes (i (length src)) (setf (aref src i) (* i 977)))
  (clipped-like "blit-pixels" (lambda (c) (blit-pixels c src 60 40 20 10)))
  (clipped-like "blit-pixels (masked, sub-block)"
                (lambda (c) (blit-pixels c src 60 40 50 30 :sx 5 :sy 3 :w 50 :h 30 :mask #xFFFFFF)))
  (let ((win (make-canvas 60 40)))
    (replace (canvas-pixels win) src)
    (clipped-like "blit-canvas" (lambda (c) (blit-canvas c win 70 60)))))

(let ((ttf "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"))
  (if (probe-file ttf)
      (let ((font (load-ttf-font ttf :size 18)))
        (clipped-like "draw-string (outline, anti-aliased)"
                      (lambda (c) (dotimes (i 5)
                                    (draw-string c font "Morphic clips!" 2 (+ 2 (* 22 i)) #xE0E0E0)))))
      (format t "~&  (no DejaVu Sans Mono: the outline string check is skipped)~%")))
(let ((lolf (merge-pathnames "cozette.lolf" (make-pathname :directory (pathname-directory *load-truename*)))))
  (let ((font (load-font lolf)))
    (clipped-like "draw-string (bitmap)"
                  (lambda (c) (dotimes (i 8)
                                (draw-string c font "the bitmap fallback" 1 (+ 1 (* 15 i)) #xC0C0C0))))
    (set-font-size font 30)
    (clipped-like "draw-string (bitmap, magnified x2)"
                  (lambda (c) (dotimes (i 4)
                                (draw-string c font "x2 cells" 3 (* 30 i) #xC0C0C0))))))

(format t "~&== the clip itself ==~%")
(let ((c (fresh)))
  (check "a fresh canvas clips to itself" (equal (canvas-clip c) (list 0 0 +w+ +h+)))
  (with-clip (c 10 10 100 100)
    (with-clip (c 50 0 200 30)
      (check "clips nest by intersection" (equal (canvas-clip c) '(50 10 60 20))))
    (check "the inner clip is undone on the way out" (equal (canvas-clip c) '(10 10 100 100))))
  (block out
    (with-clip (c 5 5 5 5) (return-from out)))
  (check "a non-local exit puts the old clip back" (equal (canvas-clip c) (list 0 0 +w+ +h+)))
  (with-clip (c 300 300 10 10)
    (check "an empty intersection is a legal, empty clip"
           (equal (canvas-clip c) (list 300 300 0 0)))
    (fill-rect c 0 0 +w+ +h+ #xFFFFFF)
    (draw-line c 0 0 +w+ +h+ #xFFFFFF))
  (check "... and draws nothing" (every (lambda (p) (= p +bg+)) (canvas-pixels c)))
  (let ((pen (with-clip (c 0 100 10 10)
               (draw-string c (load-font (merge-pathnames "cozette.lolf"
                                                          (make-pathname :directory (pathname-directory *load-truename*))))
                            "abc" 7 0 #xFFFFFF))))
    (check "a string wholly outside the clip still returns its pen (chained runs)"
           (= pen (+ 7 (* 3 6))))))

(format t "~&== a frame drawn as clipped tiles is the frame ==~%")
;; The damage-repair shape in miniature: draw a busy picture once, then draw the SAME
;; picture again as twelve tiles, each through its own clip, onto a canvas holding
;; garbage. Every pixel is covered by exactly one tile, so the two must be identical.
(flet ((picture (c)
         (fill-rect c 0 0 +w+ +h+ +bg+)
         (draw-circle c 80 60 45 #x335577 :fill t)
         (draw-poly c '((5 . 5) (150 . 20) (90 . 115) (10 . 80)) #xFFAA00)
         (draw-string c (load-font (merge-pathnames "cozette.lolf"
                                                    (make-pathname :directory (pathname-directory *load-truename*))))
                      "tiles" 60 50 #xFFFFFF)))
  (let ((whole (make-canvas +w+ +h+)) (tiled (make-canvas +w+ +h+)))
    (picture whole)
    (fill-rect tiled 0 0 +w+ +h+ #x123456)           ; garbage the tiles must cover
    (loop for ty from 0 below +h+ by 40 do
      (loop for tx from 0 below +w+ by 40 do
        (with-clip (tiled tx ty 40 40) (picture tiled))))
    (check "12 clipped tiles reproduce the whole picture"
           (equalp (canvas-pixels whole) (canvas-pixels tiled)))))

(format t "~&~d checks, ~d failed~%" *checks* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
