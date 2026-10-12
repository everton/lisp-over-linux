;;;; ttf.lisp — TrueType outlines, read and rasterised in pure Lisp.
;;;;
;;;; A bitmap font (canvas.lisp's .lolf) stores every glyph as a grid of pixels at
;;;; ONE size. A TrueType font stores every glyph as an OUTLINE: closed curves in an
;;;; abstract grid of "font units" (DejaVu Sans Mono's em square is 2048 units on a
;;;; side). To draw at any size you scale the outline to pixels and ask, for every
;;;; pixel, "how much of you does the shape cover?" — that fraction, 0..255, is the
;;;; pixel's COVERAGE, and blending the text colour over the background by it is what
;;;; makes edges smooth (anti-aliasing). doc/fonts.org walks through all of it.
;;;;
;;;; This file is the whole engine, and it knows nothing about the canvas:
;;;;
;;;;   load-ttf   bytes on disk -> a FACE (the tables we need, located once)
;;;;   glyph-index   a Unicode codepoint -> a glyph number   (the cmap table)
;;;;   advance-width a glyph -> how far the pen moves        (hmtx)
;;;;   glyph-outline a glyph -> its contours as points       (loca + glyf)
;;;;   rasterize-glyph  contours at N pixels -> a coverage bitmap (the rasteriser)
;;;;
;;;; What a .ttf is, physically: a small directory of TABLES, each a 4-letter tag
;;;; (head, hhea, maxp, cmap, hmtx, loca, glyf, ...) with an offset and a length.
;;;; Every number is BIG-endian (the format comes from 1990s Apple). We read the
;;;; whole file into one octet vector and index into it; nothing is copied out.
;;;;
;;;; Deliberately NOT here: hinting (the bytecode that nudges outlines onto the pixel
;;;; grid at small sizes — a virtual machine of its own), kerning and shaping (a
;;;; monospace cell grid needs neither), CFF/OpenType-PostScript outlines (.otf with
;;;; cubic curves), and colour/emoji tables. What is lost without hinting is a little
;;;; crispness at small sizes; the gamma in RASTERIZE-GLYPH buys some of it back.
;;;;
;;;; The rasteriser is the "signed area accumulation" method popularised by Raph
;;;; Levien's font-rs: every edge deposits, into the pixels it crosses, how much area
;;;; it adds (downward edges) or removes (upward ones); one running sum along each row
;;;; then turns those deposits into the exact covered fraction of every pixel. No
;;;; scanline sorting, no edge tables, no supersampling — and exact, not sampled.

(defpackage :lol.ttf
  (:use :cl)
  (:export #:face #:face-p #:load-ttf #:parse-ttf #:face-name #:face-path
           #:face-units-per-em #:face-ascender #:face-descender #:face-line-gap
           #:face-num-glyphs #:glyph-index #:advance-width #:glyph-outline
           #:rasterize-glyph #:*gamma*))

(in-package :lol.ttf)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

;;; ---- big-endian readers ----------------------------------------------------
;;; The four integer shapes the format uses. F2DOT14 (a fixed-point -2..2 used by
;;; composite glyphs' scale factors) is the only fractional one.

(declaim (inline u8 u16 i16 u32 i8))
(defun u8 (d i) (declare (type octets d) (fixnum i)) (aref d i))
(defun i8 (d i) (let ((v (u8 d i))) (if (>= v 128) (- v 256) v)))
(defun u16 (d i)
  (declare (type octets d) (fixnum i))
  (logior (ash (aref d i) 8) (aref d (1+ i))))
(defun i16 (d i) (let ((v (u16 d i))) (if (>= v 32768) (- v 65536) v)))
(defun u32 (d i)
  (declare (type octets d) (fixnum i))
  (logior (ash (aref d i) 24) (ash (aref d (+ i 1)) 16)
          (ash (aref d (+ i 2)) 8) (aref d (+ i 3))))
(defun f2dot14 (d i) (/ (i16 d i) 16384.0))

;;; ---- the face --------------------------------------------------------------

(defstruct face
  (path "" :type string)
  (name "" :type string)              ; the full font name from the 'name' table
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (units-per-em 2048 :type fixnum)    ; head: the em square's side, in font units
  (ascender 0 :type fixnum)           ; hhea: top of the tallest letters (positive)
  (descender 0 :type fixnum)          ; hhea: bottom of g/p/y (NEGATIVE: below baseline)
  (line-gap 0 :type fixnum)           ; hhea: extra space the designer wants between lines
  (num-glyphs 0 :type fixnum)         ; maxp
  (num-hmetrics 0 :type fixnum)       ; hhea: how many glyphs have their own advance
  (hmtx 0 :type fixnum)               ; table offsets into DATA
  (loca 0 :type fixnum)
  (long-loca nil)                     ; head.indexToLocFormat: 32-bit loca entries?
  (glyf 0 :type fixnum)
  (cmap 0 :type fixnum)               ; offset of the chosen cmap SUBTABLE
  (cmap-format 0 :type fixnum))       ; 4 (BMP, segments) or 12 (all planes, groups)

(defun find-table (data tag)
  "Offset of table TAG (a 4-char string) in DATA, or NIL. The directory follows the
   12-byte header: numTables at byte 4, then 16-byte records (tag, checksum, offset,
   length)."
  (let ((n (u16 data 4)))
    (loop for r from 12 by 16 repeat n
          when (and (= (u8 data r) (char-code (char tag 0)))
                    (= (u8 data (+ r 1)) (char-code (char tag 1)))
                    (= (u8 data (+ r 2)) (char-code (char tag 2)))
                    (= (u8 data (+ r 3)) (char-code (char tag 3))))
            return (u32 data (+ r 8)))))

(defun choose-cmap (data cmap)
  "The best Unicode subtable in the cmap at CMAP -> (values offset format). Prefer
   format 12 (covers every plane — emoji, maths) over format 4 (the Basic
   Multilingual Plane only, U+0000..U+FFFF). Platform 3 is Windows, 0 is Unicode;
   encoding 10 / 4 mean 'full repertoire', 1 / 3 mean 'BMP'."
  (let ((best nil) (best-fmt 0))
    (loop for r from (+ cmap 4) by 8 repeat (u16 data (+ cmap 2))
          do (let* ((platform (u16 data r)) (encoding (u16 data (+ r 2)))
                    (off (+ cmap (u32 data (+ r 4))))
                    (fmt (u16 data off)))
               (when (and (or (= platform 0) (and (= platform 3) (member encoding '(1 10))))
                          (member fmt '(4 12))
                          (> fmt best-fmt))
                 (setf best off best-fmt fmt))))
    (values best best-fmt)))

(defun read-name (data name)
  "The font's full name (nameID 4) from the 'name' table at NAME, or \"\". Windows
   records hold UTF-16BE; Mac records (platform 1) hold one byte per character."
  (when name
    (let ((count (u16 data (+ name 2)))
          (strings (+ name (u16 data (+ name 4)))))
      (loop for r from (+ name 6) by 12 repeat count
            for platform = (u16 data r)
            for id = (u16 data (+ r 6))
            for len = (u16 data (+ r 8))
            for off = (+ strings (u16 data (+ r 10)))
            when (and (= id 4) (= platform 3))
              do (return-from read-name
                   (coerce (loop for i from off below (+ off len) by 2
                                 collect (code-char (u16 data i)))
                           'string))
            when (and (= id 4) (= platform 1))
              do (return-from read-name
                   (map 'string #'code-char (subseq data off (+ off len)))))))
  "")

(defun parse-ttf (data &optional (path ""))
  "Locate the tables of the TrueType font in octet vector DATA -> a FACE. Signals an
   error for anything we cannot draw: not a TrueType file, or outlines that are not
   'glyf' quadratics (a CFF .otf), or no Unicode cmap."
  (let ((data (coerce data 'octets)))
    (unless (and (> (length data) 12)
                 (member (u32 data 0) '(#x00010000 #x74727565)))  ; 1.0, or 'true'
      (error "~a: not a TrueType font (sfnt version ~x)" path
             (if (> (length data) 4) (u32 data 0) 0)))
    (let ((head (find-table data "head")) (hhea (find-table data "hhea"))
          (maxp (find-table data "maxp")) (cmap (find-table data "cmap"))
          (hmtx (find-table data "hmtx")) (loca (find-table data "loca"))
          (glyf (find-table data "glyf")))
      (unless (and head hhea maxp cmap hmtx loca glyf)
        (error "~a: missing a required table (no glyf: CFF outlines are unsupported)" path))
      (multiple-value-bind (sub fmt) (choose-cmap data cmap)
        (unless sub (error "~a: no Unicode cmap subtable (format 4 or 12)" path))
        (make-face :path (namestring path) :data data
                   :name (read-name data (find-table data "name"))
                   :units-per-em (u16 data (+ head 18))
                   :long-loca (= 1 (i16 data (+ head 50)))
                   :ascender (i16 data (+ hhea 4))
                   :descender (i16 data (+ hhea 6))
                   :line-gap (i16 data (+ hhea 8))
                   :num-hmetrics (u16 data (+ hhea 34))
                   :num-glyphs (u16 data (+ maxp 4))
                   :hmtx hmtx :loca loca :glyf glyf
                   :cmap sub :cmap-format fmt)))))

(defun load-ttf (path)
  "Read the TrueType file at PATH into a FACE."
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence data s)
      (parse-ttf data path))))

;;; ---- cmap: codepoint -> glyph ----------------------------------------------

(defun glyph-index (face code)
  "The glyph for Unicode codepoint CODE, or 0 — glyph 0 is '.notdef', the font's own
   'no such character' box, which is exactly the fallback we want on screen.

   Format 4 splits the BMP into SEGMENTS [start, end]; inside one, the glyph is either
   code + idDelta (mod 65536), or read from a glyphIdArray that idRangeOffset points
   at — relative to the idRangeOffset word ITSELF, a famous piece of 1990s pointer
   arithmetic. Format 12 is a plain list of groups (start end first-glyph)."
  (let ((d (face-data face)) (s (face-cmap face)))
    (ecase (face-cmap-format face)
      (4
       (when (> code #xFFFF) (return-from glyph-index 0))
       (let* ((segx2 (u16 d (+ s 6)))
              (ends s)                                   ; endCode[] at s+14
              (starts (+ s 16 segx2))                    ; after reservedPad
              (deltas (+ starts segx2))
              (ranges (+ deltas segx2)))
         ;; binary search the first segment whose end >= code
         (let ((lo 0) (hi (1- (floor segx2 2))))
           (loop while (< lo hi)
                 do (let ((mid (floor (+ lo hi) 2)))
                      (if (< (u16 d (+ ends 14 (* 2 mid))) code)
                          (setf lo (1+ mid))
                          (setf hi mid))))
           (let ((start (u16 d (+ starts (* 2 lo))))
                 (delta (u16 d (+ deltas (* 2 lo))))
                 (range (u16 d (+ ranges (* 2 lo)))))
             (cond ((< code start) 0)
                   ((zerop range) (logand (+ code delta) #xFFFF))
                   (t (let ((g (u16 d (+ ranges (* 2 lo) range (* 2 (- code start))))))
                        (if (zerop g) 0 (logand (+ g delta) #xFFFF)))))))))
      (12
       (let ((n (u32 d (+ s 12))))
         (let ((lo 0) (hi (1- n)))
           (loop while (<= lo hi)
                 do (let* ((mid (floor (+ lo hi) 2))
                           (g (+ s 16 (* 12 mid)))
                           (start (u32 d g)) (end (u32 d (+ g 4))))
                      (cond ((< code start) (setf hi (1- mid)))
                            ((> code end) (setf lo (1+ mid)))
                            (t (return-from glyph-index
                                 (+ (u32 d (+ g 8)) (- code start)))))))
           0))))))

;;; ---- hmtx: how far the pen moves -------------------------------------------

(defun advance-width (face glyph)
  "GLYPH's advance in font units. hmtx holds (advance, left-side-bearing) pairs for the
   first numberOfHMetrics glyphs; every later glyph shares the LAST advance — which is
   how a monospace font stores one width for thousands of glyphs."
  (let ((i (min glyph (1- (face-num-hmetrics face)))))
    (u16 (face-data face) (+ (face-hmtx face) (* 4 i)))))

;;; ---- glyf: the outlines ----------------------------------------------------

(defun glyph-offset (face glyph)
  "(values start end) of GLYPH's record in 'glyf'. loca is an array of offsets, one per
   glyph plus a final one; a glyph whose start equals its end has NO outline (space)."
  (let ((d (face-data face)) (l (face-loca face)))
    (if (face-long-loca face)
        (values (u32 d (+ l (* 4 glyph))) (u32 d (+ l (* 4 (1+ glyph)))))
        (values (* 2 (u16 d (+ l (* 2 glyph)))) (* 2 (u16 d (+ l (* 2 (1+ glyph)))))))))

(defun simple-glyph (d g n)
  "Contours of the simple glyph at offset G with N contours: a list of vectors of
   points, each point (x y on-curve-p) in font units.

   The encoding is compact: after the contour end-indices and some hinting bytecode
   (skipped), come one FLAGS byte per point — with a 'repeat' bit so runs share a
   byte — then all the x's, then all the y's, each a DELTA from the previous point,
   stored in 1 byte or 2 depending on flag bits."
  (let* ((ends (loop for i below n collect (u16 d (+ g 10 (* 2 i)))))
         (npts (1+ (car (last ends))))
         (p (+ g 10 (* 2 n)))
         (p (+ p 2 (u16 d p)))                 ; skip instructionLength + instructions
         (flags (make-array npts))
         (xs (make-array npts)) (ys (make-array npts)))
    ;; flags: bit 0 on-curve, 1 x-short, 2 y-short, 3 repeat, 4/5 x/y same-or-positive
    (let ((i 0))
      (loop while (< i npts)
            do (let ((f (u8 d p)))
                 (incf p)
                 (setf (aref flags i) f) (incf i)
                 (when (logbitp 3 f)
                   (let ((r (u8 d p)))
                     (incf p)
                     (loop repeat r while (< i npts)
                           do (setf (aref flags i) f) (incf i)))))))
    (flet ((coords (vec short-bit same-bit)
             (let ((v 0))
               (dotimes (i npts)
                 (let ((f (aref flags i)))
                   (cond ((logbitp short-bit f)
                          (let ((b (u8 d p))) (incf p)
                            (incf v (if (logbitp same-bit f) b (- b)))))
                         ((not (logbitp same-bit f))
                          (incf v (i16 d p)) (incf p 2))))
                 (setf (aref vec i) v)))))
      (coords xs 1 4)
      (coords ys 2 5))
    (let ((start 0))
      (loop for e in ends
            collect (prog1 (coerce (loop for i from start to e
                                         collect (list (aref xs i) (aref ys i)
                                                       (logbitp 0 (aref flags i))))
                                   'simple-vector)
                      (setf start (1+ e)))))))

(defun glyph-outline (face glyph &optional (depth 0))
  "GLYPH's contours (see SIMPLE-GLYPH), with COMPOSITE glyphs resolved. A composite —
   'é' is often 'e' plus an acute accent glyph, placed — is a list of references to
   other glyphs, each with an offset and maybe a 2x2 transform. DEPTH guards a
   malicious or broken font that refers to itself."
  (multiple-value-bind (start end) (glyph-offset face glyph)
    (when (or (>= start end) (> depth 8)) (return-from glyph-outline nil))
    (let* ((d (face-data face)) (g (+ (face-glyf face) start)) (n (i16 d g)))
      (if (>= n 0)
          (and (plusp n) (simple-glyph d g n))
          ;; composite: flags, glyphIndex, two args, optional scale, repeat while
          ;; MORE_COMPONENTS (bit 5)
          (let ((p (+ g 10)) (result '()))
            (loop
              (let* ((flags (u16 d p)) (sub (u16 d (+ p 2)))
                     (words (logbitp 0 flags)) (xy (logbitp 1 flags))
                     dx dy (a 1.0) (b 0.0) (c 0.0) (dd 1.0))
                (incf p 4)
                (if words
                    (setf dx (i16 d p) dy (i16 d (+ p 2)) p (+ p 4))
                    (setf dx (i8 d p) dy (i8 d (+ p 1)) p (+ p 2)))
                ;; ARGS_ARE_XY_VALUES clear means "match point i to point j" — an
                ;; anchoring scheme almost no font uses; we place such parts unshifted.
                (unless xy (setf dx 0 dy 0))
                (cond ((logbitp 3 flags) (setf a (f2dot14 d p) dd a) (incf p 2))
                      ((logbitp 6 flags) (setf a (f2dot14 d p) dd (f2dot14 d (+ p 2))) (incf p 4))
                      ((logbitp 7 flags) (setf a (f2dot14 d p) b (f2dot14 d (+ p 2))
                                               c (f2dot14 d (+ p 4)) dd (f2dot14 d (+ p 6)))
                       (incf p 8)))
                (dolist (contour (glyph-outline face sub (1+ depth)))
                  (push (map 'simple-vector
                             (lambda (pt)
                               (destructuring-bind (x y on) pt
                                 (list (+ (* a x) (* c y) dx) (+ (* b x) (* dd y) dy) on)))
                             contour)
                        result))
                (unless (logbitp 5 flags) (return))))
            (nreverse result))))))

;;; ---- the rasteriser ----------------------------------------------------------

(deftype accum () '(simple-array single-float (*)))

(defun accumulate-line (a w h x0 y0 x1 y1)
  "Deposit the signed area of the edge (X0,Y0)->(X1,Y1) into accumulator A, a W x H
   pixel grid stored row-major (plus slack). Coordinates are pixels, y DOWN.

   The idea, for one row the edge crosses: the edge sweeps a vertical extent DY of
   that row. Everything to the RIGHT of the edge, within those DY, is 'inside' by
   that amount (or 'outside', for an upward edge — DIR's sign). Rather than write DY
   into every pixel to the right, deposit it ONCE where the edge is; a running sum
   along the row later carries it rightward to the shape's other edge, whose deposit
   cancels it. The pixel(s) the edge actually passes through get only the part of DY
   lying to the right of the edge within them — the fractional bits below."
  (declare (optimize (speed 3) (safety 0))
           (type accum a) (fixnum w h) (single-float x0 y0 x1 y1))
  (when (< (abs (- y0 y1)) 1e-6) (return-from accumulate-line))
  (let ((dir 1.0))
    (declare (single-float dir))
    (when (> y0 y1)                       ; walk top to bottom; remember the direction
      (rotatef x0 x1) (rotatef y0 y1) (setf dir -1.0))
    (let* ((dxdy (/ (- x1 x0) (- y1 y0)))
           (x x0)
           (ystart (max 0 (floor y0)))
           (yend (min h (ceiling y1))))
      (declare (single-float dxdy x) (fixnum ystart yend))
      (when (< y0 0.0) (setf x (- x (* y0 dxdy))))
      (loop for y of-type fixnum from ystart below yend do
        (let* ((linestart (* y w))
               (dy (- (min (float (1+ y) 1.0) y1) (max (float y 1.0) y0)))
               (xnext (+ x (* dxdy dy)))
               (d (* dy dir))
               (xa (min x xnext)) (xb (max x xnext))
               (xa (max 0.0 (min xa (float w 1.0))))
               (xb (max 0.0 (min xb (float w 1.0))))
               (x0floor (ffloor xa))
               (x0i (truncate x0floor))
               (x1ceil (fceiling xb))
               (x1i (truncate x1ceil)))
          (declare (fixnum linestart x0i x1i) (single-float dy xnext d xa xb x0floor x1ceil))
          (if (<= x1i (1+ x0i))
              ;; the edge stays inside one pixel column on this row: split D between
              ;; that pixel (the part right of the edge's middle) and the next one
              (let ((xmf (- (* 0.5 (+ x xnext)) x0floor))
                    (i (+ linestart x0i)))
                (declare (single-float xmf) (fixnum i))
                (incf (aref a i) (- d (* d xmf)))
                (incf (aref a (1+ i)) (* d xmf)))
              ;; the edge crosses several columns: the covered area ramps linearly
              ;; from the first column to the last (a trapezoid, cut per pixel)
              (let* ((s (/ 1.0 (- xb xa)))
                     (x0f (- xa x0floor))
                     (a0 (* 0.5 s (- 1.0 x0f) (- 1.0 x0f)))
                     (x1f (+ (- xb x1ceil) 1.0))
                     (am (* 0.5 s x1f x1f))
                     (i (+ linestart x0i)))
                (declare (single-float s x0f a0 x1f am) (fixnum i))
                (incf (aref a i) (* d a0))
                (if (= x1i (+ x0i 2))
                    (incf (aref a (1+ i)) (* d (- 1.0 a0 am)))
                    (let ((a1 (* s (- 1.5 x0f))))
                      (declare (single-float a1))
                      (incf (aref a (1+ i)) (* d (- a1 a0)))
                      (loop for xi of-type fixnum from (+ x0i 2) below (1- x1i)
                            do (incf (aref a (+ linestart xi)) (* d s)))
                      (let ((a2 (+ a1 (* (float (- x1i x0i 3) 1.0) s))))
                        (declare (single-float a2))
                        (incf (aref a (+ linestart (1- x1i))) (* d (- 1.0 a2 am))))))
                (incf (aref a (+ linestart x1i)) (* d am))))
          (setf x xnext))))))

(defun accumulate-quad (a w h x0 y0 cx cy x1 y1)
  "A quadratic Bézier from (X0,Y0) to (X1,Y1) pulled toward the control point (CX,CY),
   flattened into straight pieces. How many: enough that no piece strays more than a
   fraction of a pixel from the curve — the curve's 'bend', |P0 - 2C + P1|, decides,
   as in font-rs. B(t) = (1-t)^2 P0 + 2t(1-t) C + t^2 P1 is evaluated by lerping twice."
  (declare (single-float x0 y0 cx cy x1 y1))
  (let* ((ddx (+ x0 (* -2 cx) x1)) (ddy (+ y0 (* -2 cy) y1))
         (devsq (+ (* ddx ddx) (* ddy ddy))))
    (if (< devsq 0.333)
        (accumulate-line a w h x0 y0 x1 y1)
        (let* ((n (1+ (floor (sqrt (sqrt (* 3.0 devsq))))))
               (px x0) (py y0))
          (loop for i from 1 below n
                do (let* ((tt (/ (float i 1.0) n))
                          (ax (+ x0 (* tt (- cx x0)))) (ay (+ y0 (* tt (- cy y0))))
                          (bx (+ cx (* tt (- x1 cx)))) (by (+ cy (* tt (- y1 cy))))
                          (nx (+ ax (* tt (- bx ax)))) (ny (+ ay (* tt (- by ay)))))
                     (accumulate-line a w h px py nx ny)
                     (setf px nx py ny)))
          (accumulate-line a w h px py x1 y1)))))

(defun walk-contour (contour emit-line emit-quad)
  "Turn one TrueType contour into lines and quadratic curves. The rule: an OFF-curve
   point is a Bézier control point; two off-curve points in a row imply an on-curve
   point exactly between them (a space-saving trick of the format). The contour is
   closed, and may even START off-curve, so we first pick an on-curve starting point."
  (let* ((n (length contour)))
    (when (< n 2) (return-from walk-contour))
    (flet ((pt (i) (svref contour (mod i n)))
           (mid (p q) (list (/ (+ (first p) (first q)) 2) (/ (+ (second p) (second q)) 2) t)))
      (let* ((first-on (position-if #'third contour))
             ;; all off-curve (it happens: a circle as four controls): invent the
             ;; on-curve point implied between the last and the first
             (start (if first-on (pt first-on) (mid (pt 0) (pt (1- n)))))
             ;; the points visited after START, in order; the walk then closes on it
             (rest (if first-on
                       (loop for k from 1 below n collect (pt (+ first-on k)))
                       (loop for k from 0 below n collect (pt k))))
             (prev start) (ctrl nil))
        (dolist (q rest)
          (if (third q)
              (progn (if ctrl (funcall emit-quad prev ctrl q) (funcall emit-line prev q))
                     (setf prev q ctrl nil))
              (progn (when ctrl
                       (let ((m (mid ctrl q)))
                         (funcall emit-quad prev ctrl m)
                         (setf prev m)))
                     (setf ctrl q))))
        ;; close back to the start
        (if ctrl (funcall emit-quad prev ctrl start) (funcall emit-line prev start))))))

(defparameter *gamma* 1.45
  "Coverage is raised to 1/*GAMMA* before it becomes a byte. Exact coverage looks
   THIN for light text on a dark ground — the eye does not respond linearly, and
   without hinting a 1-pixel stem often lands half on each of two pixels, i.e. two
   half-lit greys. Pushing mid coverage up darkens (here: brightens) stems. 1.0 is
   'mathematically exact'; 1.45 was chosen by looking at frames at 12, 18 and 28 px
   (doc/fonts.org §4). Changing it needs the glyph caches cleared.")

(defun rasterize-glyph (face glyph px &key (dx 0.0))
  "Draw GLYPH at an em size of PX pixels -> (values coverage w h left top): COVERAGE is
   a W x H octet vector (0 = untouched, 255 = fully inside), placed with its top-left
   LEFT pixels right of the pen and TOP pixels BELOW the baseline (so usually
   negative: a glyph sits above its baseline). DX shifts the outline right by a
   fraction of a pixel (used to centre a glyph in its cell). A glyph with no outline
   (space) returns a 0x0 bitmap."
  (let* ((contours (glyph-outline face glyph))
         (scale (/ (float px 1.0) (face-units-per-em face)))
         (dx (float dx 1.0)))
    (when (null contours)
      (return-from rasterize-glyph
        (values (make-array 0 :element-type '(unsigned-byte 8)) 0 0 0 0)))
    ;; the pixel-space bounding box: every point, control points included (a
    ;; quadratic never leaves the triangle of its three points, so this is safe)
    (let ((minx most-positive-single-float) (maxx most-negative-single-float)
          (miny most-positive-single-float) (maxy most-negative-single-float))
      (dolist (c contours)
        (loop for (x y) across c
              do (let ((sx (+ dx (* x scale))) (sy (- (* y scale))))
                   (setf minx (min minx sx) maxx (max maxx sx)
                         miny (min miny sy) maxy (max maxy sy)))))
      (let* ((left (floor minx)) (top (floor miny))
             (w (max 1 (- (ceiling maxx) left)))
             (h (max 1 (- (ceiling maxy) top)))
             ;; +2 slack: an edge on the right border deposits one past the row
             (a (make-array (+ (* w h) 2) :element-type 'single-float :initial-element 0.0))
             (fl (float left 1.0)) (ft (float top 1.0)))
        (flet ((xy (p) (values (- (+ dx (* (float (first p) 1.0) scale)) fl)
                               (- (- (* (float (second p) 1.0) scale)) ft))))
          (dolist (c contours)
            (walk-contour c
                          (lambda (p q)
                            (multiple-value-bind (x0 y0) (xy p)
                              (multiple-value-bind (x1 y1) (xy q)
                                (accumulate-line a w h x0 y0 x1 y1))))
                          (lambda (p c q)
                            (multiple-value-bind (x0 y0) (xy p)
                              (multiple-value-bind (cx cy) (xy c)
                                (multiple-value-bind (x1 y1) (xy q)
                                  (accumulate-quad a w h x0 y0 cx cy x1 y1))))))))
        ;; the running sum: deposits become coverage. |sum| because a contour wound
        ;; the other way deposits negatives; min 1 because overlapping contours (an
        ;; accent touching its letter) would otherwise exceed full.
        (let ((out (make-array (* w h) :element-type '(unsigned-byte 8)))
              (inv (/ 1.0 (float *gamma* 1.0)))
              (acc 0.0))
          (declare (single-float acc inv))
          (dotimes (i (* w h))
            (incf acc (aref a i))
            (let ((c (min 1.0 (abs acc))))
              (setf (aref out i)
                    (if (< c 0.002) 0 (min 255 (round (* 255.0 (expt c inv))))))))
          (values out w h left top))))))
