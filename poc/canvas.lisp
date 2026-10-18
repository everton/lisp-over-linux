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
           #:fill-rect #:draw-rect #:draw-line #:draw-poly #:draw-circle #:blit-rgba
           #:with-clip #:call-with-clip #:canvas-clip #:clip-visible-p
           #:canvas-clip-x0 #:canvas-clip-y0 #:canvas-clip-x1 #:canvas-clip-y1
           #:blit-pixels #:blit-canvas
           #:font #:load-psf #:load-font #:font-cw #:font-ch #:draw-string #:string-px
           #:load-ttf-font #:font-outline-p #:font-size #:font-default-size #:font-ascent
           #:font-face #:font-description #:set-font-size #:reset-font-size
           #:font-glyph #:clear-font-cache #:font-hits #:font-misses #:glyph-w #:glyph-h
           #:glyph-left #:glyph-top #:glyph-coverage
           #:+min-font-size+ #:+max-font-size+ #:*default-font-size*
           #:rgb #:shift-p #:ctrl-p #:meta-p #:super-p))

(in-package :lol.canvas)

;;; A CSS-style colour literal: !#RRGGBB reads as the 24-bit 0xRRGGBB pixel — exactly
;;; what (rgb #xRR #xGG #xBB) makes (RGB has no alpha byte). The #RRGGBB is kept
;;; VERBATIM so colours are copy-pasteable straight from CSS and light up under Emacs
;;; rainbow-mode; the leading ! is just the reader's trigger, since bare #RRGGBB can't
;;; work (the reader claims #b / #3.. for its own syntax). ! is non-terminating, so it
;;; can still sit inside a symbol. Installed on the shared readtable, which is saved in
;;; the image — so every later module AND Accept's runtime read-from-string honour it.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\!
    (lambda (stream ch)
      (declare (ignore ch))
      (let ((hash (read-char stream nil nil)))
        (unless (eql hash #\#)
          (error "colour literal: expected !#RRGGBB, got !~@[~a~]" hash)))
      (let ((s (make-string 6)))
        (dotimes (i 6) (setf (char s i) (read-char stream)))
        (parse-integer s :radix 16)))
    t))                                 ; non-terminating: ! may still occur inside a symbol

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

;;; The CLIP RECTANGLE (doc/morphic.org §4.1). Every primitive below writes only
;;; inside it. It is stored as four half-open edges — x0 <= x < x1, y0 <= y < y1 —
;;; because that is the form the loops want: a fill is then "from the larger left
;;; edge to the smaller right edge", with no +1/-1 anywhere. A fresh canvas clips to
;;; itself, which is exactly what every primitive did before the clip existed
;;; ("clip to the canvas, so callers never have to check"); WITH-CLIP narrows it for
;;; the duration of a body. Damage repair is the reason it exists: a window
;;; repainting the rectangle it uncovered must not scribble over the window above,
;;; and a morph redrawn for one damaged rect must leave every pixel outside alone.
(defstruct (canvas (:constructor %make-canvas))
  (w 0 :type fixnum)
  (h 0 :type fixnum)
  (pixels (make-array 0 :element-type '(unsigned-byte 32)) :type pixmap)
  (clip-x0 0 :type fixnum) (clip-y0 0 :type fixnum)   ; the clip's top-left (inclusive)
  (clip-x1 0 :type fixnum) (clip-y1 0 :type fixnum))  ; ... and bottom-right (exclusive)

(defun make-canvas (w h)
  (%make-canvas :w w :h h :clip-x1 w :clip-y1 h
                :pixels (make-array (* w h) :element-type '(unsigned-byte 32)
                                            :initial-element 0)))

(defun canvas-clip (canvas)
  "The clip as a rect list (x y w h) — for looking at; WITH-CLIP is how to change it."
  (list (canvas-clip-x0 canvas) (canvas-clip-y0 canvas)
        (- (canvas-clip-x1 canvas) (canvas-clip-x0 canvas))
        (- (canvas-clip-y1 canvas) (canvas-clip-y0 canvas))))

(defun call-with-clip (canvas x y w h fn)
  "Run FN with CANVAS's clip narrowed to its intersection with the rect X,Y,W,H, then
   put the old clip back (even on a non-local exit — a debugger restart, say). Clips
   NEST by intersecting: a body cannot widen the clip it was given, which is what lets
   a window clip to its own bounds inside a damage rect and still stay in the rect.
   An empty intersection is legal and makes every primitive a no-op."
  (declare (fixnum x y w h))
  (let ((ox0 (canvas-clip-x0 canvas)) (oy0 (canvas-clip-y0 canvas))
        (ox1 (canvas-clip-x1 canvas)) (oy1 (canvas-clip-y1 canvas)))
    (let* ((nx0 (max ox0 x)) (ny0 (max oy0 y))
           (nx1 (max nx0 (min ox1 (+ x w)))) (ny1 (max ny0 (min oy1 (+ y h)))))
      (unwind-protect
           (progn (setf (canvas-clip-x0 canvas) nx0 (canvas-clip-y0 canvas) ny0
                        (canvas-clip-x1 canvas) nx1 (canvas-clip-y1 canvas) ny1)
                  (funcall fn))
        (setf (canvas-clip-x0 canvas) ox0 (canvas-clip-y0 canvas) oy0
              (canvas-clip-x1 canvas) ox1 (canvas-clip-y1 canvas) oy1)))))

(defmacro with-clip ((canvas x y w h) &body body)
  "Draw BODY into CANVAS with every pixel write confined to the rect X,Y,W,H (and to
   whatever clip was already in force). (with-clip (c 10 10 100 50) (fill-rect c 0 0 999 999 red))
   paints exactly the 100x50 rect."
  `(call-with-clip ,canvas ,x ,y ,w ,h (lambda () ,@body)))

(declaim (inline clip-visible-p))
(defun clip-visible-p (canvas x y)
  "Is pixel X,Y inside CANVAS's clip? The per-pixel test the line and circle use."
  (declare (fixnum x y))
  (and (<= (canvas-clip-x0 canvas) x) (< x (canvas-clip-x1 canvas))
       (<= (canvas-clip-y0 canvas) y) (< y (canvas-clip-y1 canvas))))

(declaim (inline rgb))
(defun rgb (r g b) (logior (ash r 16) (ash g 8) b))

;;; ---- primitives ----------------------------------------------------------
;;; Every one of these clips to the canvas's CLIP RECTANGLE (the whole canvas unless
;;; WITH-CLIP narrowed it), so callers never have to check.

(defun fill-rect (canvas x y w h color)
  "Solid rectangle, clipped."
  (declare (optimize (speed 3) (safety 1))
           (fixnum x y w h) ((unsigned-byte 32) color))
  (let* ((cw (canvas-w canvas))
         (px (canvas-pixels canvas))
         (x0 (max (canvas-clip-x0 canvas) x)) (y0 (max (canvas-clip-y0 canvas) y))
         (x1 (min (canvas-clip-x1 canvas) (+ x w))) (y1 (min (canvas-clip-y1 canvas) (+ y h))))
    (declare (type pixmap px) (fixnum cw x0 y0 x1 y1))
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

(defun draw-line (canvas x0 y0 x1 y1 color)
  "One-pixel line from X0,Y0 to X1,Y1 — integer Bresenham, clipped per pixel.

   Added for the stage (§3b): a sprite is a silhouette, and a silhouette is lines.
   Per-pixel clipping rather than a clip-to-rect pass up front, because the lines are
   short and the loop is already a bounds check away from being one."
  (declare (optimize (speed 3) (safety 1)) (fixnum x0 y0 x1 y1)
           ((unsigned-byte 32) color))
  (let* ((cw (canvas-w canvas))
         (kx0 (canvas-clip-x0 canvas)) (ky0 (canvas-clip-y0 canvas))
         (kx1 (canvas-clip-x1 canvas)) (ky1 (canvas-clip-y1 canvas))
         (px (canvas-pixels canvas))
         (dx (abs (- x1 x0))) (dy (- (abs (- y1 y0))))
         (sx (if (< x0 x1) 1 -1)) (sy (if (< y0 y1) 1 -1))
         (err (+ dx dy)))
    (declare (type pixmap px) (fixnum cw kx0 ky0 kx1 ky1 dx dy sx sy err))
    (loop
      (when (and (<= kx0 x0) (< x0 kx1) (<= ky0 y0) (< y0 ky1))
        (setf (aref px (+ (* y0 cw) x0)) color))
      (when (and (= x0 x1) (= y0 y1)) (return))
      (let ((e2 (* 2 err)))
        (declare (fixnum e2))
        (when (>= e2 dy) (incf err dy) (incf x0 sx))
        (when (<= e2 dx) (incf err dx) (incf y0 sy))))))

(defun draw-poly (canvas points color &key (close t))
  "Outline through POINTS — a list of (x . y) conses — closing the loop unless
   :CLOSE NIL. The stage's sprites are all one call to this. (Clipped because
   DRAW-LINE is.)"
  (loop for (p . rest) on points
        while rest
        do (draw-line canvas (car p) (cdr p) (car (first rest)) (cdr (first rest)) color))
  (when (and close (cdr points))
    (let ((a (first points)) (z (car (last points))))
      (draw-line canvas (car z) (cdr z) (car a) (cdr a) color))))

(defun draw-circle (canvas cx cy r color &key fill)
  "Circle outline (or a disc with :FILL t), clipped. Midpoint algorithm; the filled
   form spans each scanline rather than plotting twice."
  (declare (optimize (speed 3) (safety 1)) (fixnum cx cy r))
  (when (plusp r)
    (let ((x r) (y 0) (err 0))
      (declare (fixnum x y err))
      (loop while (>= x y) do
        (if fill
            (progn (fill-rect canvas (- cx x) (- cy y) (* 2 x) 1 color)
                   (fill-rect canvas (- cx x) (+ cy y) (* 2 x) 1 color)
                   (fill-rect canvas (- cx y) (- cy x) (* 2 y) 1 color)
                   (fill-rect canvas (- cx y) (+ cy x) (* 2 y) 1 color))
            (dolist (p (list (cons (+ cx x) (+ cy y)) (cons (+ cx y) (+ cy x))
                             (cons (- cx y) (+ cy x)) (cons (- cx x) (+ cy y))
                             (cons (- cx x) (- cy y)) (cons (- cx y) (- cy x))
                             (cons (+ cx y) (- cy x)) (cons (+ cx x) (- cy y))))
              (let ((px* (car p)) (py (cdr p)))
                (when (clip-visible-p canvas px* py)
                  (setf (aref (canvas-pixels canvas) (+ (* py (canvas-w canvas)) px*)) color)))))
        (incf y)
        (incf err (1+ (* 2 y)))
        (when (> (* 2 (- err x)) 1)
          (decf x)
          (incf err (- 1 (* 2 x))))))))

(defun blit-rgba (canvas bytes iw ih x y &key (threshold 128))
  "Composite a raw RGBA image (top-to-bottom, 4 bytes/px) at X,Y.
   Same alpha rule as draw-alien in framebuffer.lisp: opaque enough -> paint it."
  (declare (optimize (speed 3) (safety 1))
           (type (simple-array (unsigned-byte 8) (*)) bytes)
           (fixnum iw ih x y threshold))
  (let ((px (canvas-pixels canvas))
        (cw (canvas-w canvas))
        (kx0 (canvas-clip-x0 canvas)) (ky0 (canvas-clip-y0 canvas))
        (kx1 (canvas-clip-x1 canvas)) (ky1 (canvas-clip-y1 canvas)))
    (declare (type pixmap px) (fixnum cw kx0 ky0 kx1 ky1))
    (dotimes (row ih)
      (let ((dy (+ y row)))
        (declare (fixnum dy))
        (when (and (>= dy ky0) (< dy ky1))
          (dotimes (col iw)
            (let ((dx (+ x col)))
              (declare (fixnum dx))
              (when (and (>= dx kx0) (< dx kx1))
                (let ((si (* 4 (+ (* row iw) col))))
                  (declare (fixnum si))
                  (when (>= (aref bytes (+ si 3)) threshold)
                    (setf (aref px (+ (* dy cw) dx))
                          (rgb (aref bytes si)
                               (aref bytes (+ si 1))
                               (aref bytes (+ si 2))))))))))))))

(defun blit-pixels (canvas src sw sh x y &key (sx 0) (sy 0) (w sw) (h sh) (mask #xFFFFFFFF))
  "Copy the W x H block at SX,SY of the pixel vector SRC (SW x SH, rows of SW words)
   to X,Y of CANVAS, clipped — a row at a time with REPLACE, so a whole window's
   backing store composites at memory speed. MASK is LOGANDed into each word (#xFFFFFF
   strips a decoder's alpha byte, as an image pane wants); the default copies words as
   they are and takes the fast path.

   The one OPAQUE copy every pixel-vector writer outside this file should go through
   — a web frame, a decoded image, a window's backing store — so that the clip binds
   them too. A writer that indexes CANVAS-PIXELS itself escapes the clip."
  (declare (optimize (speed 3) (safety 1)) (type pixmap src)
           (fixnum sw sh x y sx sy w h) ((unsigned-byte 32) mask))
  (let* ((dst (canvas-pixels canvas)) (cw (canvas-w canvas))
         ;; the source block, cut to the source vector ...
         (w (min w (- sw sx))) (h (min h (- sh sy)))
         ;; ... and its destination, cut to the clip
         (x0 (max x (canvas-clip-x0 canvas))) (y0 (max y (canvas-clip-y0 canvas)))
         (x1 (min (+ x w) (canvas-clip-x1 canvas))) (y1 (min (+ y h) (canvas-clip-y1 canvas))))
    (declare (type pixmap dst) (fixnum cw w h x0 y0 x1 y1))
    (when (and (< x0 x1) (< y0 y1))
      (loop for dy of-type fixnum from y0 below y1
            for srow of-type fixnum = (+ (* (+ sy (- dy y)) sw) sx (- x0 x))
            for drow of-type fixnum = (+ (* dy cw) x0)
            do (if (= mask #xFFFFFFFF)
                   (replace dst src :start1 drow :end1 (+ drow (- x1 x0)) :start2 srow)
                   (loop for i of-type fixnum from 0 below (- x1 x0)
                         do (setf (aref dst (+ drow i)) (logand mask (aref src (+ srow i))))))))
    canvas))

(defun blit-canvas (canvas src x y)
  "Composite the whole canvas SRC at X,Y of CANVAS, clipped (a window's backing store
   onto the screen)."
  (blit-pixels canvas (canvas-pixels src) (canvas-w src) (canvas-h src) x y))


;;; ---- fonts: one FONT object, two kinds of glyph source -----------------------
;;;
;;; A FONT answers the three questions all layout asks — how wide is a cell (CW), how
;;; tall (CH), and what does character C look like — from one of two sources:
;;;
;;;   BITMAP   a .lolf / PSF1 blob: 256 hand-drawn glyphs at ONE size, one byte per
;;;            pixel row, MSB = leftmost pixel. Crisp, tiny, and fixed: the only way
;;;            to make it bigger is integer magnification (each pixel a 2x2 block).
;;;            Kept as the FALLBACK, for when no TrueType file could be read.
;;;   OUTLINE  a TrueType face (poc/ttf.lisp): curves, rasterised on demand at
;;;            whatever pixel SIZE the font is currently set to, anti-aliased, and
;;;            CACHED per (size, codepoint) so each glyph is computed once per size.
;;;
;;; The SIZE is a property of the FONT OBJECT and SET-FONT-SIZE changes it in place,
;;; recomputing CW / CH. That is the whole runtime-resizing design: every piece of
;;; layout code reads (font-cw font) / (font-ch font) at DRAW time, the shell holds the
;;; one font object everyone shares, so a size change followed by a redraw relays out
;;; everything. doc/fonts.org §5.

(defconstant +min-font-size+ 10 "Smallest pixel size SET-FONT-SIZE allows.")
(defconstant +max-font-size+ 40 "Largest pixel size SET-FONT-SIZE allows.")

(defparameter *default-font-size* 18
  "The em size, in pixels, an outline font starts at. Chosen for a 14-inch 1920x1080
   panel (~157 dpi): 18 px makes DejaVu Sans Mono's cell 11 x 21, so a full-screen
   pane holds about 170 columns by 45 rows — readable at arm's length, where the old
   6 x 15 bitmap was not. ~/.lol/settings overrides it (doc/fonts.org §6).")

(defstruct (glyph (:constructor make-glyph (coverage w h left top)))
  "One rasterised outline glyph, as cached: a W x H coverage bitmap (octets, 0..255)
   whose top-left sits LEFT pixels right of the pen and TOP pixels below the
   baseline (negative = above it)."
  (coverage nil :type (simple-array (unsigned-byte 8) (*)))
  (w 0 :type fixnum) (h 0 :type fixnum)
  (left 0 :type fixnum) (top 0 :type fixnum))

(defstruct font
  (cw 8  :type fixnum)                  ; CELL width in pixels — what layout reads
  (ch 16 :type fixnum)                  ; CELL height in pixels — likewise
  (glyphs (make-array 0) :type simple-vector)  ; BITMAP: 256 x (bh bytes)
  (bw 8 :type fixnum)                   ; BITMAP: the glyphs' native width ...
  (bh 16 :type fixnum)                  ; ... and height (one byte per row)
  (scale 1 :type fixnum)                ; BITMAP: integer magnification (1 = native)
  (face nil)                            ; OUTLINE: a lol.ttf:face, or NIL for a bitmap
  (size 0 :type fixnum)                 ; the current pixel size (bitmap: bh * scale)
  (default-size 0 :type fixnum)         ; what SuperL-0 goes back to
  (ascent 0 :type fixnum)               ; OUTLINE: the baseline, in px below the cell top
  (cache (make-hash-table) :type hash-table)   ; OUTLINE: size*2^21+code -> GLYPH
  (hits 0 :type fixnum) (misses 0 :type fixnum)) ; cache statistics (font-test.lisp)

(defun font-outline-p (font) (and (font-face font) t))

(defun font-description (font)
  "A one-line human name: 'DejaVu Sans Mono 18px (11x21)' or 'bitmap 6x15 x2'."
  (if (font-outline-p font)
      (format nil "~a ~dpx (~dx~d)" (lol.ttf:face-name (font-face font))
              (font-size font) (font-cw font) (font-ch font))
      (format nil "bitmap ~dx~d~@[ x~d~]" (font-bw font) (font-bh font)
              (and (> (font-scale font) 1) (font-scale font)))))

(defun set-font-size (font px)
  "Set FONT's pixel size to PX (clamped to +MIN-FONT-SIZE+..+MAX-FONT-SIZE+), in place,
   and recompute its cell. Returns the size actually set.

   OUTLINE: PX is the em — the height of the font's design square. The CELL follows
   from the font's own metrics, scaled and rounded to whole pixels (a cell grid can
   only be whole pixels): width = the advance of 'M' (every glyph's, in a monospace
   font); height = ascender + descender + lineGap, the line distance the designer
   intended; the baseline sits ascender (+ half the gap) below the cell top.

   BITMAP: the glyphs cannot be redrawn, only magnified, so PX rounds to the nearest
   whole multiple of the native height — 15, 30 for Cozette. Cheap, blocky, and it
   means SuperL-= still does something useful on the fallback font."
  (let ((px (max +min-font-size+ (min +max-font-size+ (round px)))))
    (if (font-outline-p font)
        (let* ((face (font-face font))
               (s (/ (float px 1d0) (lol.ttf:face-units-per-em face)))
               (adv (lol.ttf:advance-width face (lol.ttf:glyph-index face (char-code #\M))))
               (asc (* s (lol.ttf:face-ascender face)))
               (desc (* s (- (lol.ttf:face-descender face))))
               (gap (* s (lol.ttf:face-line-gap face))))
          (setf (font-size font) px
                (font-cw font) (max 1 (round (* s adv)))
                (font-ch font) (max 1 (round (+ asc desc gap)))
                (font-ascent font) (round (+ asc (/ gap 2)))))
        (let ((k (max 1 (round px (font-bh font)))))
          (setf (font-scale font) k
                (font-size font) (* k (font-bh font))
                (font-cw font) (* k (font-bw font))
                (font-ch font) (* k (font-bh font)))))
    (font-size font)))

(defun reset-font-size (font)
  "Back to the size FONT was loaded at (SuperL-0)."
  (set-font-size font (font-default-size font)))

(defun load-ttf-font (path &key (size *default-font-size*))
  "A FONT over the TrueType file at PATH, set to SIZE pixels."
  (let ((font (make-font :face (lol.ttf:load-ttf path))))
    (set-font-size font size)
    (setf (font-default-size font) (font-size font))
    font))

(defun font-glyph (font code &optional (size (font-size font)))
  "The cached GLYPH for character CODE at SIZE pixels, rasterising it on a miss. The
   cache key packs size and codepoint into one fixnum (codepoints stop below 2^21), so
   the table is an EQL table of small integers — the cheapest lookup there is.
   Codepoints the font lacks map to glyph 0, its own '.notdef' box (lol.ttf:glyph-index)."
  (declare (fixnum code size))
  (let* ((key (+ (* size #x200000) code))
         (hit (gethash key (font-cache font))))
    (if hit
        (progn (incf (font-hits font)) hit)
        (let* ((face (font-face font))
               (gid (lol.ttf:glyph-index face code))
               (adv (* (/ (float size 1.0) (lol.ttf:face-units-per-em face))
                       (lol.ttf:advance-width face gid)))
               ;; the cell is a ROUNDED advance; centre the glyph in it, so the
               ;; rounding error is shared by both side bearings
               (cell (* (font-cw font) (/ (float size 1.0) (font-size font)))))
          (incf (font-misses font))
          (setf (gethash key (font-cache font))
                (multiple-value-call #'make-glyph
                  (lol.ttf:rasterize-glyph face gid size :dx (/ (- cell adv) 2))))))))

(defun clear-font-cache (font)
  "Forget every rasterised glyph (after changing lol.ttf:*gamma*, say)."
  (clrhash (font-cache font)))

(defun load-psf (path)
  "Read a PSF1 font file into a (bitmap) FONT. Header: magic #x36 #x04, mode, charsize;
   then 256 glyphs of CHARSIZE bytes. PSF1 is always 8 wide."
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
        (make-font :cw 8 :ch ch :bw 8 :bh ch :size ch :default-size ch :glyphs glyphs)))))

(defun load-font (path)
  "Read a .lolf bitmap font blob into a FONT (the format poc/bdf2font.lisp writes).
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
        (make-font :cw w :ch h :bw w :bh h :size h :default-size h :glyphs glyphs)))))

(defun string-px (font string &key (scale 1))
  "Pixel width STRING will occupy — the metric the layout code needs. Every cell is
   CW wide (a monospace grid, whatever the font), so this is a multiplication."
  (* (length string) (font-cw font) scale))

(declaim (inline glyph-index))
(defun glyph-index (code)
  "The BITMAP glyph for character CODE. The font holds 256 glyphs, indexed by codepoint, so
   anything past Latin-1 has no glyph of its own -- and it used to be drawn as
   (LOGAND CODE #xFF), which is a DIFFERENT glyph: an em dash (U+2014) landed on
   #x14 and drew as a box-drawing tee, so every title of the form 'a -- b' in the
   UI read with a tee in the middle, and an ellipsis (U+2026) drew as an ampersand.
   The punctuation the UI actually uses folds to its ASCII cousin; anything else
   past 255 is a question mark, which at least looks missing rather than wrong.
   (An OUTLINE font needs none of this: its cmap covers thousands of characters.)"
  (declare (fixnum code))
  (if (< code 256)
      code
      (case code
        ((#x2013 #x2014 #x2212) 45)        ; en dash, em dash, minus -> hyphen
        (#x2026 46)                        ; ellipsis -> one full stop (one cell)
        ((#x2018 #x2019) 39)               ; curly single quotes -> apostrophe
        ((#x201C #x201D) 34)               ; curly double quotes -> straight
        (#x2192 62) (#x2190 60)            ; arrows -> > <
        (t 63))))

(defun draw-string (canvas font string x y color &key (scale 1))
  "Draw STRING with its CELL's top-left at X,Y. Returns the x just past the last
   glyph, so callers can chain runs of different colors — which is exactly how the
   source pane gets syntax highlighting. SCALE magnifies (the debugger's title)."
  (if (font-outline-p font)
      (draw-string-outline canvas font string x y color scale)
      (draw-string-bitmap canvas font string x y color (* scale (font-scale font)))))

(defun draw-string-outline (canvas font string x y color scale)
  "The anti-aliased path: each glyph's COVERAGE blends COLOR over what is already
   there, channel by channel — out = old + (new - old) * coverage/255. Full coverage
   is a plain store (most of a stem); zero is skipped (most of a cell)."
  (declare (optimize (speed 3) (safety 1)) (fixnum x y scale)
           ((unsigned-byte 32) color))
  (let* ((px (canvas-pixels canvas))
         (cw (canvas-w canvas))
         (kx0 (canvas-clip-x0 canvas)) (ky0 (canvas-clip-y0 canvas))
         (kx1 (canvas-clip-x1 canvas)) (ky1 (canvas-clip-y1 canvas))
         (size (* scale (font-size font)))
         (adv (* scale (font-cw font)))
         (base (+ y (* scale (font-ascent font))))
         (fr (ldb (byte 8 16) color)) (fg (ldb (byte 8 8) color)) (fb (ldb (byte 8 0) color))
         (pen x))
    (declare (type pixmap px) (fixnum cw kx0 ky0 kx1 ky1 size adv base pen)
             ((unsigned-byte 8) fr fg fb))
    ;; A whole line outside the clip draws nothing, so skip it without touching a
    ;; glyph — the common case when a damage rect repaints a strip of a busy frame.
    ;; The margin is a full cell above and below: no glyph reaches that far out of its
    ;; cell, so nothing the clip would have shown is skipped.
    (let ((cell (* scale (font-ch font))))
      (when (or (<= (+ y cell cell) ky0) (>= (- y cell) ky1))
        (return-from draw-string-outline (+ x (* adv (length string))))))
    (loop for c across string do
      (unless (char= c #\Space)
        (let* ((g (font-glyph font (char-code c) size))
               (cov (glyph-coverage g))
               (gw (glyph-w g)) (gh (glyph-h g))
               (ox (+ pen (glyph-left g))) (oy (+ base (glyph-top g))))
          (declare (type (simple-array (unsigned-byte 8) (*)) cov) (fixnum gw gh ox oy))
          (dotimes (row gh)
            (let ((dy (+ oy row)))
              (declare (fixnum dy))
              (when (and (>= dy ky0) (< dy ky1))
                (let ((src (* row gw)) (dst (* dy cw)))
                  (declare (fixnum src dst))
                  (dotimes (col gw)
                    (let ((a (aref cov (+ src col)))
                          (dx (+ ox col)))
                      (declare ((unsigned-byte 8) a) (fixnum dx))
                      (when (and (plusp a) (>= dx kx0) (< dx kx1))
                        (let ((i (+ dst dx)))
                          (declare (fixnum i))
                          (if (= a 255)
                              (setf (aref px i) color)
                              (let* ((old (aref px i))
                                     (ia (- 255 a)))
                                (declare ((unsigned-byte 32) old) (fixnum ia))
                                (setf (aref px i)
                                      (logior
                                       (ash (floor (+ (* fr a) (* (ldb (byte 8 16) old) ia)) 255) 16)
                                       (ash (floor (+ (* fg a) (* (ldb (byte 8 8) old) ia)) 255) 8)
                                       (floor (+ (* fb a) (* (ldb (byte 8 0) old) ia)) 255)))))))))))))))
      (incf pen adv))
    pen))

(defun draw-string-bitmap (canvas font string x y color scale)
  "The bitmap path: for each glyph row byte, stamp the lit bits (MSB = leftmost), each
   as a SCALE x SCALE block. Background pixels are left alone."
  (declare (optimize (speed 3) (safety 1)) (fixnum x y scale))
  (let* ((px (canvas-pixels canvas))
         (cw (canvas-w canvas))
         (kx0 (canvas-clip-x0 canvas)) (ky0 (canvas-clip-y0 canvas))
         (kx1 (canvas-clip-x1 canvas)) (ky1 (canvas-clip-y1 canvas))
         (fw (font-bw font)) (fh (font-bh font))
         (glyphs (font-glyphs font))
         (pen x))
    (declare (type pixmap px) (fixnum cw kx0 ky0 kx1 ky1 fw fh pen)
             ((unsigned-byte 32) color))
    ;; a bitmap glyph never leaves its cell: a line wholly outside the clip is skipped
    (when (or (<= (+ y (* fh scale)) ky0) (>= y ky1))
      (return-from draw-string-bitmap (+ x (* fw scale (length string)))))
    (loop for c across string do
      (let ((g (svref glyphs (glyph-index (char-code c)))))
        (declare (type (simple-array (unsigned-byte 8) (*)) g))
        (dotimes (row fh)
          (let ((bits (aref g row)))
            (declare ((unsigned-byte 8) bits))
            (unless (zerop bits)
              (dotimes (sy scale)
                (let ((dy (+ y (* row scale) sy)))
                  (declare (fixnum dy))
                  (when (and (>= dy ky0) (< dy ky1))
                    (dotimes (col fw)
                      (when (logbitp (- 7 col) bits)
                        (dotimes (sx scale)
                          (let ((dx (+ pen (* col scale) sx)))
                            (declare (fixnum dx))
                            (when (and (>= dx kx0) (< dx kx1))
                              (setf (aref px (+ (* dy cw) dx)) color))))))))))))
        (incf pen (* fw scale))))
    pen))
