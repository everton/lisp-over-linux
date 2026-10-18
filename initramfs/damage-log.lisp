;;;; damage-log.lisp — the screen as a log of what changed: written while recording,
;;;; replayed into frames afterwards.
;;;;
;;;; doc/screencast.org §5.2, the recorder's second mode. A live recording encodes as it
;;;; goes (about one thread at 1920x1200); a LOGGED one writes down only the rects each
;;;; repair redrew, plus where the pointer was, and encodes after the stop. Typing
;;;; repaints a line; an idle screen repaints nothing; so the log is small and costs
;;;; almost nothing while you talk. This file is only the FORMAT — writing records and
;;;; reading them back into a canvas — so a host test can prove the round trip pixel
;;;; for pixel (poc/damage-log-test.lisp); recorder.lisp does the threads and ffmpeg.
;;;;
;;;; A .dmg file, all numbers little-endian:
;;;;
;;;;   "LOLDMG1\n"                     magic, and the format's version
;;;;   (:w 1920 :h 1200)\n             a READable plist: the screen's size
;;;;   records, each starting with a kind byte and the time in µs since the log began:
;;;;     1  u32 t  u16 n  n x (u16 x y w h  u32 len  len bytes)    rects: zlib-compressed
;;;;                                                               pixels, row by row,
;;;;                                                               each pixel 4 bytes
;;;;                                                               (B G R x, the canvas's)
;;;;     2  u32 t  u16 x  u16 y  u8 hidden                       the pointer
;;;;
;;;; A u32 of microseconds is 71 minutes — longer than a segment between two pauses
;;;; will be; the writer refuses a later record rather than wrap.
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

(defparameter +dmg-magic+ "LOLDMG1")

(defun dmg-u8 (s v) (write-byte (ldb (byte 8 0) v) s))
(defun dmg-u16 (s v) (dmg-u8 s v) (dmg-u8 s (ash v -8)))
(defun dmg-u32 (s v) (dmg-u16 s (ldb (byte 16 0) v)) (dmg-u16 s (ash v -16)))

(defun dmg-write-header (s w h)
  "Begin a log on S, an (unsigned-byte 8) output stream, for a W x H screen."
  (loop for c across (format nil "~a~%~s~%" +dmg-magic+ (list :w w :h h))
        do (write-byte (char-code c) s)))

(defun pixels-octets (pixels)
  "A (unsigned-byte 32) pixel vector's bytes, as the machine stores them."
  (let* ((n (* 4 (length pixels)))
         (o (make-array n :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (pixels o)
      (sb-kernel:system-area-ub8-copy (sb-sys:vector-sap pixels) 0 (sb-sys:vector-sap o) 0 n))
    o))

(defun canvas-rect-pixels (canvas x y w h)
  "The pixels of the rect X,Y,W,H of CANVAS, row by row, as a fresh vector."
  (let* ((cw (lol.canvas:canvas-w canvas))
         (src (lol.canvas:canvas-pixels canvas))
         (out (make-array (* w h) :element-type '(unsigned-byte 32))))
    (dotimes (row h out)
      (replace out src :start1 (* row w) :end1 (* (1+ row) w) :start2 (+ x (* (+ y row) cw))))))

(defun clip-rect (canvas x y w h)
  "X Y W H cut to CANVAS, or NIL when nothing of it is on it."
  (let* ((x0 (max 0 x)) (y0 (max 0 y))
         (x1 (min (lol.canvas:canvas-w canvas) (+ x w))) (y1 (min (lol.canvas:canvas-h canvas) (+ y h))))
    (and (> x1 x0) (> y1 y0) (list x0 y0 (- x1 x0) (- y1 y0)))))

(defun dmg-write-rects (s us rects)
  "A rects record: RECTS is a list of (x y w h pixels), PIXELS a vector from
   CANVAS-RECT-PIXELS. Compresses here — call it on a thread, not the screen's."
  (dmg-u8 s 1) (dmg-u32 s us) (dmg-u16 s (length rects))
  (dolist (r rects)
    (destructuring-bind (x y w h pixels) r
      (let ((z (lol.deflate:zlib-compress (pixels-octets pixels))))
        (dmg-u16 s x) (dmg-u16 s y) (dmg-u16 s w) (dmg-u16 s h)
        (dmg-u32 s (length z))
        (write-sequence z s)))))

(defun dmg-write-pointer (s us x y hidden)
  (dmg-u8 s 2) (dmg-u32 s us)
  (dmg-u16 s (max 0 x)) (dmg-u16 s (max 0 y)) (dmg-u8 s (if hidden 1 0)))

;;; ---- reading back ---------------------------------------------------------------

(defun dmg-r8 (s) (read-byte s nil nil))
(defun dmg-r16 (s) (let ((a (dmg-r8 s)) (b (dmg-r8 s))) (and b (logior a (ash b 8)))))
(defun dmg-r32 (s) (let ((a (dmg-r16 s)) (b (dmg-r16 s))) (and b (logior a (ash b 16)))))

(defun dmg-read-line (s)
  (map 'string #'code-char (loop for b = (read-byte s) until (= b 10) collect b)))

(defun dmg-open (s)
  "Read a log's header from S. -> (values W H), or an error for a file that is not one."
  (unless (string= (dmg-read-line s) +dmg-magic+) (error "not a damage log (no ~a)" +dmg-magic+))
  (let ((plist (let ((*read-eval* nil)) (read-from-string (dmg-read-line s)))))
    (values (getf plist :w) (getf plist :h))))

(defun dmg-next (s)
  "The next record: (:rects us ((x y w h octets)...)), (:pointer us x y hidden), or NIL
   at the end (a torn last record — the recording cut by a crash — is the end too)."
  (let ((kind (dmg-r8 s)))
    (handler-case
        (case kind
          ((nil) nil)
          (1 (let ((us (dmg-r32 s)) (n (dmg-r16 s)))
               (list :rects us
                     (loop repeat n
                           collect (let* ((x (dmg-r16 s)) (y (dmg-r16 s)) (w (dmg-r16 s)) (h (dmg-r16 s))
                                          (len (dmg-r32 s))
                                          (z (make-array len :element-type '(unsigned-byte 8))))
                                     (unless (= len (read-sequence z s)) (error "torn"))
                                     (list x y w h z))))))
          (2 (list :pointer (dmg-r32 s) (dmg-r16 s) (dmg-r16 s) (= 1 (dmg-r8 s))))
          (t (error "unknown record kind ~a" kind)))
      (error () nil))))

(defun dmg-apply (canvas record)
  "Paint a :RECTS record's pixels into CANVAS."
  (dolist (r (third record))
    (destructuring-bind (x y w h z) r
      (let* ((o (lol.inflate:zlib-decompress z :size-hint (* 4 w h)))
             (dst (lol.canvas:canvas-pixels canvas))
             (cw (lol.canvas:canvas-w canvas)))
        (dotimes (row h)
          (let ((base (+ x (* (+ y row) cw))) (ob (* 4 row w)))
            (dotimes (col w)
              (let ((i (+ ob (* 4 col))))
                (setf (aref dst (+ base col))
                      (logior (aref o i) (ash (aref o (+ i 1)) 8)
                              (ash (aref o (+ i 2)) 16) (ash (aref o (+ i 3)) 24)))))))))))

(defun dmg-replay (s fps frames fn &key (draw-pointer (lambda (canvas x y) (declare (ignore canvas x y)))))
  "Turn the log on S into FRAMES frames at FPS: call FN with a canvas holding frame k,
   for k from 0. A frame at time k/FPS shows every record up to that time. The pointer,
   when shown, is drawn by DRAW-POINTER (canvas x y) on a COPY — the log's canvas stays
   the screen without it."
  (multiple-value-bind (w h) (dmg-open s)
    (let ((screen (lol.canvas:make-canvas w h))
          (frame (lol.canvas:make-canvas w h))
          (pointer nil)
          (next (dmg-next s)))
      (dotimes (k frames)
        (let ((due (round (* k 1000000) fps)))
          (loop while (and next (<= (second next) due))
                do (ecase (first next)
                     (:rects (dmg-apply screen next))
                     (:pointer (setf pointer (unless (fifth next) (list (third next) (fourth next))))))
                   (setf next (dmg-next s))))
        (replace (lol.canvas:canvas-pixels frame) (lol.canvas:canvas-pixels screen))
        (when pointer (funcall draw-pointer frame (first pointer) (second pointer)))
        (funcall fn frame))
      (values w h))))
