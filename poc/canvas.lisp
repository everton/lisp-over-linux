;;;; canvas.lisp — the pixel buffer and everything we draw into it.
;;;;
;;;; This is the WHOLE portable half of the graphics plan, in miniature. The
;;;; browser draws here and nowhere else; it never learns whether the picture
;;;; ends up on an X11 window or on /dev/fb0. See doc/code-browser.org §1.
;;;;
;;;; Pixel format: one (unsigned-byte 32) per pixel, #x00RRGGBB. On x86 that
;;;; sits in memory little-endian as bytes B,G,R,0 — which is exactly BGRX,
;;;; exactly what efifb wants (framebuffer.org §8) AND what a 24-bit-depth X
;;;; server wants on this machine. So the same buffer ships to both backends
;;;; with no conversion at all. That is not luck; it is why this format was
;;;; chosen.

(defpackage :lol.canvas
  (:use :cl)
  (:export #:canvas #:make-canvas #:canvas-w #:canvas-h #:canvas-pixels
           #:fill-rect #:draw-rect #:blit-rgba
           #:font #:load-psf #:load-font #:font-cw #:font-ch #:draw-string #:string-px
           #:rgb #:shift-p #:ctrl-p #:meta-p #:super-p))

(in-package :lol.canvas)

;;; A CSS-style colour literal: #$RRGGBB reads as the 24-bit 0xRRGGBB pixel — exactly
;;; what (rgb #xRR #xGG #xBB) makes (RGB has no alpha byte), just legible. Bare #RRGGBB
;;; can't work — the reader would take #b / #3.. as its own syntax — so # + a $ marker
;;; + six hex is as close to CSS as the reader allows. Installed on the shared readtable
;;; (so every file loaded after canvas, and Accept's runtime read-from-string, see it).
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\$
    (lambda (stream subchar arg)
      (declare (ignore subchar arg))
      (let ((s (make-string 6)))
        (dotimes (i 6) (setf (char s i) (read-char stream)))
        (parse-integer s :radix 16)))))

;;; Input modifier predicates. The "state" is a bitmask in the X11 convention
;;; (bit 0 = Shift, bit 2 = Ctrl, bit 3 = Mod1/Meta/Alt, bit 6 = Mod4/Super); the
;;; framebuffer keyboard adapter synthesises the same bits, so the toolkit reads
;;; modifiers the same way on either backend and does NOT depend on the X11 client.
(declaim (inline shift-p ctrl-p meta-p super-p))
(defun shift-p (state) (logbitp 0 state))
(defun ctrl-p  (state) (logbitp 2 state))
(defun meta-p  (state) (logbitp 3 state))         ; Emacs Meta: Esc-prefix or Alt
(defun super-p (state) (logbitp 6 state))

(deftype pixmap () '(simple-array (unsigned-byte 32) (*)))

(defstruct (canvas (:constructor %make-canvas))
  (w 0 :type fixnum)
  (h 0 :type fixnum)
  (pixels (make-array 0 :element-type '(unsigned-byte 32)) :type pixmap))

(defun make-canvas (w h)
  (%make-canvas :w w :h h
                :pixels (make-array (* w h) :element-type '(unsigned-byte 32)
                                            :initial-element 0)))

(declaim (inline rgb))
(defun rgb (r g b) (logior (ash r 16) (ash g 8) b))

;;; ---- primitives ----------------------------------------------------------
;;; Every one of these clips to the canvas, so callers never have to check.

(defun fill-rect (canvas x y w h color)
  "Solid rectangle, clipped."
  (declare (optimize (speed 3) (safety 1))
           (fixnum x y w h) ((unsigned-byte 32) color))
  (let* ((cw (canvas-w canvas)) (ch (canvas-h canvas))
         (px (canvas-pixels canvas))
         (x0 (max 0 x)) (y0 (max 0 y))
         (x1 (min cw (+ x w))) (y1 (min ch (+ y h))))
    (declare (type pixmap px) (fixnum cw ch x0 y0 x1 y1))
    (loop for yy of-type fixnum from y0 below y1 do
      (let ((row (* yy cw)))
        (declare (fixnum row))
        (loop for xx of-type fixnum from x0 below x1
              do (setf (aref px (+ row xx)) color))))))

(defun draw-rect (canvas x y w h color)
  "One-pixel outline."
  (fill-rect canvas x y w 1 color)
  (fill-rect canvas x (+ y h -1) w 1 color)
  (fill-rect canvas x y 1 h color)
  (fill-rect canvas (+ x w -1) y 1 h color))

(defun blit-rgba (canvas bytes iw ih x y &key (threshold 128))
  "Composite a raw RGBA image (top-to-bottom, 4 bytes/px) at X,Y.
   Same alpha rule as draw-alien in framebuffer.lisp: opaque enough -> paint it."
  (declare (optimize (speed 3) (safety 1))
           (type (simple-array (unsigned-byte 8) (*)) bytes)
           (fixnum iw ih x y threshold))
  (let ((px (canvas-pixels canvas))
        (cw (canvas-w canvas)) (ch (canvas-h canvas)))
    (declare (type pixmap px) (fixnum cw ch))
    (dotimes (row ih)
      (let ((dy (+ y row)))
        (declare (fixnum dy))
        (when (and (>= dy 0) (< dy ch))
          (dotimes (col iw)
            (let ((dx (+ x col)))
              (declare (fixnum dx))
              (when (and (>= dx 0) (< dx cw))
                (let ((si (* 4 (+ (* row iw) col))))
                  (declare (fixnum si))
                  (when (>= (aref bytes (+ si 3)) threshold)
                    (setf (aref px (+ (* dy cw) dx))
                          (rgb (aref bytes si)
                               (aref bytes (+ si 1))
                               (aref bytes (+ si 2))))))))))))))

;;; ---- PSF bitmap font -----------------------------------------------------
;;;
;;; PSF1 is about as simple as a font format gets, which is the point: the real
;;; plan bakes a font blob into the image exactly like alien.rgba, and this is
;;; that blob's reader. Header is 4 bytes (magic #x36 #x04, mode, charsize),
;;; then 256 glyphs of CHARSIZE bytes each; each byte is one 8-pixel row, MSB
;;; = leftmost pixel. No kerning, no antialiasing, no shaping. Crisp and fast —
;;; Smalltalk-80 and Genera both looked superb this way.

(defstruct font
  (cw 8  :type fixnum)                  ; cell width  (PSF1 is always 8)
  (ch 16 :type fixnum)                  ; cell height (the charsize byte)
  (glyphs (make-array 0) :type simple-vector))  ; 256 x (ch bytes)

(defun load-psf (path)
  "Read a PSF1 font file into a FONT."
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((hdr (make-array 4 :element-type '(unsigned-byte 8))))
      (read-sequence hdr s)
      (unless (and (= (aref hdr 0) #x36) (= (aref hdr 1) #x04))
        (error "~a is not a PSF1 font (magic ~x ~x)" path (aref hdr 0) (aref hdr 1)))
      (let* ((ch (aref hdr 3))
             (glyphs (make-array 256)))
        (dotimes (i 256)
          (let ((g (make-array ch :element-type '(unsigned-byte 8))))
            (read-sequence g s)
            (setf (svref glyphs i) g)))
        (make-font :cw 8 :ch ch :glyphs glyphs)))))

(defun load-font (path)
  "Read a .lolf bitmap font blob into a FONT (the format poc/bdf2font.py writes).
   Unlike load-psf, glyphs are indexed by codepoint 0..255 — we own the mapping,
   so there are no PSF glyph-order surprises (the ·->Ö trap; see doc/fonts.org).
   Header: magic 'LF', width byte, height byte; then 256 glyphs of HEIGHT bytes
   (one byte per row, MSB = leftmost pixel). Widths >8 are rejected by the writer,
   so one byte per row is always enough here — matching draw-string's blitter."
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((hdr (make-array 4 :element-type '(unsigned-byte 8))))
      (read-sequence hdr s)
      (unless (and (= (aref hdr 0) #x4c) (= (aref hdr 1) #x46))
        (error "~a is not a .lolf font (magic ~x ~x)" path (aref hdr 0) (aref hdr 1)))
      (let* ((w (aref hdr 2)) (h (aref hdr 3))
             (glyphs (make-array 256)))
        (dotimes (i 256)
          (let ((g (make-array h :element-type '(unsigned-byte 8))))
            (read-sequence g s)
            (setf (svref glyphs i) g)))
        (make-font :cw w :ch h :glyphs glyphs)))))

(defun string-px (font string &key (scale 1))
  "Pixel width STRING will occupy — the metric the layout code needs."
  (* (length string) (font-cw font) scale))

(defun draw-string (canvas font string x y color &key (scale 1))
  "Draw STRING with its top-left at X,Y. Returns the x just past the last glyph,
   so callers can chain runs of different colors — which is exactly how the
   source pane gets syntax highlighting."
  (declare (optimize (speed 3) (safety 1)) (fixnum x y scale))
  (let* ((px (canvas-pixels canvas))
         (cw (canvas-w canvas)) (chh (canvas-h canvas))
         (fw (font-cw font)) (fh (font-ch font))
         (glyphs (font-glyphs font))
         (pen x))
    (declare (type pixmap px) (fixnum cw chh fw fh pen)
             ((unsigned-byte 32) color))
    (loop for c across string do
      (let ((g (svref glyphs (logand (char-code c) #xff))))
        (declare (type (simple-array (unsigned-byte 8) (*)) g))
        (dotimes (row fh)
          (let ((bits (aref g row)))
            (declare ((unsigned-byte 8) bits))
            (unless (zerop bits)
              (dotimes (sy scale)
                (let ((dy (+ y (* row scale) sy)))
                  (declare (fixnum dy))
                  (when (and (>= dy 0) (< dy chh))
                    (dotimes (col fw)
                      (when (logbitp (- 7 col) bits)
                        (dotimes (sx scale)
                          (let ((dx (+ pen (* col scale) sx)))
                            (declare (fixnum dx))
                            (when (and (>= dx 0) (< dx cw))
                              (setf (aref px (+ (* dy cw) dx)) color))))))))))))
        (incf pen (* fw scale))))
    pen))
