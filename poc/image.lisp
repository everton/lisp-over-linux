;;;; image.lisp — decode image files into canvas pixels, for file previews.
;;;;
;;;; The file manager wants "almost everything that could have a preview to have
;;;; one". This file turns the bytes of an image file into what poc/canvas.lisp
;;;; draws: a row-major (simple-array (unsigned-byte 32) (*)) of #x00RRGGBB pixels.
;;;; Design, format notes and measured timings: doc/previews.org.
;;;;
;;;; WHO DECODES WHAT. Two decoders live behind one entry point:
;;;;
;;;;   * In LISP, here: PNG (every colour type, depth and filter, Adam7, tRNS), GIF
;;;;     (first frame), BMP (1/4/8/16/24/32-bit, RLE4/RLE8, bitfields), PNM (P1-P7),
;;;;     QOI, TGA, ICO/CUR, and lossless WebP. These are "container + a simple
;;;;     coding" formats; in Lisp they are short, safe and fast enough, and PNG
;;;;     rides on poc/inflate.lisp, which the file manager needs for .gz anyway.
;;;;   * In C, through stb_image (poc/stbi-shim.c -> libstbi.so, loaded LAZILY on
;;;;     the first JPEG): JPEG baseline + progressive, PSD, HDR, PIC. JPEG is the
;;;;     one format where a decoder is a real project (Huffman + progressive
;;;;     refinement + IDCT + chroma upsampling), stb_image is a vetted public-domain
;;;;     header needing only libc+libm, and both are already in the image. If the
;;;;     .so is absent those formats return NIL + a reason; everything else works.
;;;;
;;;; THE CONTRACT, because this runs inside PID 1: DECODE-IMAGE never signals. Any
;;;; failure — corrupt file, truncated file, unsupported variant, a dimension bomb,
;;;; even running out of heap — comes back as (values NIL "reason" FORMAT). Images
;;;; over *MAX-PIXELS* (40 megapixels) are refused before their pixels are allocated.
;;;;
;;;; INSIDE, every decoder produces #xAARRGGBB pixels plus a flag "alpha is used";
;;;; one final pass (FINISH-PIXELS) composites onto the BACKGROUND and drops the
;;;; alpha byte, so the result is exactly the canvas format. Keeping alpha until
;;;; the very end means one compositing rule for nine formats.
;;;;
;;;; Dependency-free apart from LOL.INFLATE (and, at run time, the optional .so).

(defpackage :lol.image
  (:use :cl :lol.inflate)
  (:export #:decode-image #:decode-image-file #:image-format #:scale-to-fit
           #:*max-pixels* #:*checker-colors* #:stbi-available-p #:*stbi-paths*
           #:supported-formats))

(in-package :lol.image)

(deftype argb () '(simple-array (unsigned-byte 32) (*)))

(defparameter *max-pixels* 40000000
  "Refuse images with more pixels than this (w*h), before decoding them. 40 MP is
   a 7300x5500 photo; as #xRRGGBB that is 160 MB of heap, which is already a lot
   for a preview. A 'pixel bomb' — a tiny file claiming 60000x60000 — stops here.")

(defparameter *checker-colors* '(#x3A3A3A . #x545454)
  "The two greys of the transparency checkerboard (8x8 squares) that the default
   :BACKGROUND :CHECKER composites translucent images onto — the convention of
   image editors, so transparency reads as transparency on a dark UI.")

(define-condition image-error (error)
  ((message :initarg :message :reader image-error-message))
  (:report (lambda (c s) (write-string (image-error-message c) s))))

(defun bad (fmt &rest args)
  (error 'image-error :message (apply #'format nil fmt args)))

;;; ===========================================================================
;;; Byte helpers
;;; ===========================================================================
;;; Every read is bounds-checked (SAFETY 1): a truncated file signals inside the
;;; decoder and DECODE-IMAGE turns that into NIL + reason. Nothing reads past END.

(declaim (inline u8 u16le u16be u32le u32be pack))

(defun u8 (d i) (declare (type octets d) (type fixnum i)) (aref d i))
(defun u16le (d i) (declare (type octets d) (type fixnum i))
  (logior (aref d i) (ash (aref d (+ i 1)) 8)))
(defun u16be (d i) (declare (type octets d) (type fixnum i))
  (logior (ash (aref d i) 8) (aref d (+ i 1))))
(defun u32le (d i) (declare (type octets d) (type fixnum i))
  (logior (aref d i) (ash (aref d (+ i 1)) 8) (ash (aref d (+ i 2)) 16) (ash (aref d (+ i 3)) 24)))
(defun u32be (d i) (declare (type octets d) (type fixnum i))
  (logior (ash (aref d i) 24) (ash (aref d (+ i 1)) 16) (ash (aref d (+ i 2)) 8) (aref d (+ i 3))))
(defun s32le (d i) (let ((v (u32le d i))) (if (>= v #x80000000) (- v #x100000000) v)))

(defun pack (a r g b)
  "One #xAARRGGBB pixel."
  (declare (type (unsigned-byte 8) a r g b))
  (logior (ash a 24) (ash r 16) (ash g 8) b))

(defun bytes= (d i string)
  "Does D hold the ASCII STRING at index I?"
  (and (<= (+ i (length string)) (length d))
       (loop for k below (length string)
             always (= (aref d (+ i k)) (char-code (char string k))))))

(defun check-size (w h &optional (max *max-pixels*))
  (unless (and (integerp w) (integerp h) (plusp w) (plusp h))
    (bad "bad dimensions ~ax~a" w h))
  (when (> (* w h) max)
    (bad "~dx~d is ~,1f megapixels, over the ~,1f MP cap" w h (/ (* w h) 1e6) (/ max 1e6))))

(defun make-argb (n &optional (init 0))
  (make-array n :element-type '(unsigned-byte 32) :initial-element init))

(defun scale-bits (v bits)
  "Scale a BITS-wide sample V to 0..255 with rounding (5-bit 31 -> 255)."
  (if (= bits 8) v (round (* v 255) (1- (ash 1 bits)))))

;;; ===========================================================================
;;; Sniffing
;;; ===========================================================================

(defun svg-text-p (d)
  (let ((s (map 'string #'code-char (subseq d 0 (min 512 (length d))))))
    (and (search "<svg" s) t)))

(defun tga-plausible-p (d)
  "TGA has no magic number, so judge the header: legal image and colour-map types,
   a real pixel depth, non-zero size. Tried last; a false positive just fails to
   decode. A TGA 2.0 footer, when present, settles it."
  (let ((n (length d)))
    (or (and (> n 44) (bytes= d (- n 18) "TRUEVISION-XFILE."))
        (and (> n 18)
             (member (aref d 1) '(0 1))
             (member (aref d 2) '(1 2 3 9 10 11))
             (member (aref d 16) '(8 15 16 24 32))
             (member (aref d 7) '(0 15 16 24 32))
             (plusp (u16le d 12)) (plusp (u16le d 14))
             (eq (= (aref d 1) 1) (and (member (aref d 2) '(1 9)) t))))))

(defun image-format (d)
  "Identify octet vector D by its magic bytes. Returns a keyword — one we decode
   (see SUPPORTED-FORMATS), or one we recognise but cannot decode (:TIFF :AVIF :HEIC
   :JXL :PDF :SVG; :WEBP covers lossy too, decided later) — or NIL."
  (let ((n (length d)))
    (flet ((at (i &rest bytes)
             (and (<= (+ i (length bytes)) n)
                  (loop for b in bytes for k from i always (= (aref d k) b)))))
      (cond ((at 0 #x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A) :png)
            ((at 0 #xFF #xD8 #xFF) :jpeg)
            ((or (bytes= d 0 "GIF87a") (bytes= d 0 "GIF89a")) :gif)
            ((and (bytes= d 0 "RIFF") (bytes= d 8 "WEBP")) :webp)
            ((and (bytes= d 0 "BM") (> n 26)) :bmp)
            ((bytes= d 0 "qoif") :qoi)
            ((bytes= d 0 "8BPS") :psd)
            ((or (bytes= d 0 "#?RADIANCE") (bytes= d 0 "#?RGBE")) :hdr)
            ((and (at 0 #x53 #x80 #xF6 #x34) (bytes= d 88 "PICT")) :pic)
            ((and (>= n 3) (= (aref d 0) (char-code #\P)) (<= (char-code #\1) (aref d 1) (char-code #\7))
                  (member (aref d 2) '(9 10 13 32)))
             :pnm)
            ((or (at 0 #x49 #x49 #x2A #x00) (at 0 #x4D #x4D #x00 #x2A)) :tiff)
            ((and (bytes= d 4 "ftyp") (> n 12))
             (cond ((or (bytes= d 8 "avif") (bytes= d 8 "avis")) :avif)
                   ((or (bytes= d 8 "heic") (bytes= d 8 "heix") (bytes= d 8 "mif1")
                        (bytes= d 8 "msf1") (bytes= d 8 "hevc"))
                    :heic)))
            ((or (at 0 #xFF #x0A) (at 0 0 0 0 #x0C #x4A #x58 #x4C #x20)) :jxl)
            ((bytes= d 0 "%PDF") :pdf)
            ((and (or (at 0 0 0 1 0) (at 0 0 0 2 0)) (> n 22)
                  (plusp (u16le d 4)) (< (u16le d 4) 256) (zerop (aref d 9)))
             :ico)
            ((svg-text-p d) :svg)
            ((tga-plausible-p d) :tga)))))

;;; ===========================================================================
;;; PNG
;;; ===========================================================================
;;;
;;; A PNG is a signature and a list of chunks (length, 4-letter type, data, CRC).
;;; IHDR gives the geometry; PLTE the palette; tRNS the transparency of a palette
;;; or of one key colour; the IDAT chunks, CONCATENATED, are one zlib stream. Its
;;; inflated bytes are scanlines, each preceded by a FILTER byte: the encoder has
;;; replaced each byte by its difference from a prediction (left, up, their mean,
;;; or the Paeth guess), so smooth images become runs of small numbers DEFLATE
;;; compresses well. Decoding adds the prediction back — "unfiltering" — in place.
;;; Interlaced (Adam7) images are seven small sub-images, each filtered on its own.

(defparameter *adam7*
  '((0 0 8 8) (4 0 8 8) (0 4 4 8) (2 0 4 4) (0 2 2 4) (1 0 2 2) (0 1 1 2))
  "Adam7's seven passes: x0 y0 dx dy.")

(defun png-unfilter (raw o stride bpp prev)
  "Undo the filter of the scanline whose filter byte is RAW[O]; its STRIDE data
   bytes follow. PREV is the offset of the previous scanline's DATA in the same
   pass, or -1 for the first (which then predicts from zeros). BPP is the distance
   in bytes to the 'left' neighbour (whole pixel, at least 1)."
  (declare (type octets raw) (type fixnum o stride bpp prev)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((f (aref raw o)) (s (1+ o)))
    (declare (type fixnum s))
    (macrolet ((up (i) `(if (< prev 0) 0 (aref raw (+ prev ,i))))
               (left (i) `(if (< ,i bpp) 0 (aref raw (+ s (- ,i bpp)))))
               (upleft (i) `(if (or (< prev 0) (< ,i bpp)) 0 (aref raw (+ prev (- ,i bpp))))))
      (case f
        (0)
        (1 (loop for i of-type fixnum from bpp below stride
                 do (setf (aref raw (+ s i)) (logand 255 (+ (aref raw (+ s i)) (aref raw (+ s (- i bpp))))))))
        (2 (when (>= prev 0)
             (dotimes (i stride)
               (setf (aref raw (+ s i)) (logand 255 (+ (aref raw (+ s i)) (aref raw (+ prev i))))))))
        (3 (dotimes (i stride)
             (setf (aref raw (+ s i))
                   (logand 255 (+ (aref raw (+ s i)) (ash (+ (left i) (up i)) -1))))))
        (4 (dotimes (i stride)
             ;; Paeth: predict from whichever of left/up/upper-left is closest to
             ;; left+up-upleft (a cheap gradient estimate)
             (let* ((a (left i)) (b (up i)) (c (upleft i))
                    (p (- (+ a b) c))
                    (pa (abs (- p a))) (pb (abs (- p b))) (pc (abs (- p c))))
               (declare (type fixnum a b c p pa pb pc))
               (setf (aref raw (+ s i))
                     (logand 255 (+ (aref raw (+ s i))
                                    (cond ((and (<= pa pb) (<= pa pc)) a)
                                          ((<= pb pc) b)
                                          (t c))))))))
        (t (bad "PNG: bad filter type ~d" f))))))

(defun png-emit (raw s pw ctype depth pal key out di dx)
  "Convert one unfiltered scanline (data at RAW[S], PW pixels) into OUT, starting
   at index DI and stepping DX (1, or Adam7's column step). 8-bit RGB, RGBA, gray
   and palette — nearly every PNG in the wild — get straight loops; the rest goes
   through the general per-sample path."
  (declare (type octets raw) (type argb out pal) (type fixnum s pw di dx depth ctype)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (cond
    ((and (= depth 8) (= ctype 2) (null key))
     (dotimes (i pw)
       (let ((q (+ s (* 3 i))))
         (setf (aref out (+ di (* i dx)))
               (logior #xFF000000 (ash (aref raw q) 16) (ash (aref raw (+ q 1)) 8) (aref raw (+ q 2)))))))
    ((and (= depth 8) (= ctype 6))
     (dotimes (i pw)
       (let ((q (+ s (* 4 i))))
         (setf (aref out (+ di (* i dx)))
               (logior (ash (aref raw (+ q 3)) 24) (ash (aref raw q) 16)
                       (ash (aref raw (+ q 1)) 8) (aref raw (+ q 2)))))))
    ((and (= depth 8) (= ctype 3))
     (dotimes (i pw)
       (setf (aref out (+ di (* i dx))) (aref pal (aref raw (+ s i))))))
    ((and (= depth 8) (= ctype 0) (null key))
     (dotimes (i pw)
       (let ((v (aref raw (+ s i))))
         (setf (aref out (+ di (* i dx))) (logior #xFF000000 (* v #x010101))))))
    (t
     (let ((channels (ecase ctype (0 1) (2 3) (3 1) (4 2) (6 4))))
       (flet ((sample (i)                  ; the I-th sample of the row, raw
                (declare (type fixnum i))
                (case depth
                  (8 (aref raw (+ s i)))
                  (16 (logior (ash (aref raw (+ s (* 2 i))) 8) (aref raw (+ s (* 2 i) 1))))
                  (t (let* ((bit (* i depth)) (byte (aref raw (+ s (ash bit -3)))))
                       (logand (ash byte (- (+ (logand bit 7) depth) 8)) (1- (ash 1 depth)))))))
              (to8 (v)                     ; that sample on the 0..255 scale
                (declare (type fixnum v))
                (case depth
                  (16 (floor (+ v 128) 257))
                  (8 v)
                  (t (* v (floor 255 (1- (ash 1 depth))))))))
         (declare (inline sample to8))
         (dotimes (i pw)
           (let* ((k (* i channels))
                  (px (case ctype
                        (3 (aref pal (logand 255 (sample k))))
                        (0 (let* ((g (sample k)) (v (to8 g)))
                             (pack (if (and key (= g (the fixnum (first key)))) 0 255) v v v)))
                        (4 (let ((v (to8 (sample k)))) (pack (to8 (sample (1+ k))) v v v)))
                        (2 (let ((r (sample k)) (g (sample (+ k 1))) (b (sample (+ k 2))))
                             (pack (if (and key (= r (the fixnum (first key)))
                                            (= g (the fixnum (second key)))
                                            (= b (the fixnum (third key))))
                                       0 255)
                                   (to8 r) (to8 g) (to8 b))))
                        (t (pack (to8 (sample (+ k 3))) (to8 (sample k))
                                 (to8 (sample (+ k 1))) (to8 (sample (+ k 2))))))))
             (setf (aref out (+ di (* i dx))) px))))))))

(defun decode-png (d &key (verify-crc nil))
  "PNG -> (values w h argb alpha-p)."
  (declare (type octets d))
  (unless (eql (image-format d) :png) (bad "not a PNG"))
  (let ((p 8) (w 0) (h 0) (depth 0) (ctype 0) (interlace 0)
        (palette nil) (trns nil) (idats '()) (n (length d)))
    (loop
      (when (> (+ p 12) n) (bad "PNG: truncated (no IEND)"))
      (let* ((len (u32be d p))
             (type (map 'string #'code-char (subseq d (+ p 4) (+ p 8))))
             (start (+ p 8)))
        (when (> (+ start len 4) n)
          (if (string= type "IDAT")
              (progn (push (cons start n) idats) (return)) ; truncated: decode what we have
              (bad "PNG: truncated ~a chunk" type)))
        (when (and verify-crc (/= (crc32 d (+ p 4) (+ start len)) (u32be d (+ start len))))
          (bad "PNG: CRC mismatch in ~a" type))
        (cond ((string= type "IHDR")
               (setf w (u32be d start) h (u32be d (+ start 4))
                     depth (aref d (+ start 8)) ctype (aref d (+ start 9))
                     interlace (aref d (+ start 12)))
               (check-size w h)
               (unless (member depth (case ctype (0 '(1 2 4 8 16)) (3 '(1 2 4 8))
                                       ((2 4 6) '(8 16)) (t nil)))
                 (bad "PNG: colour type ~d with bit depth ~d is not valid" ctype depth)))
              ((string= type "PLTE") (setf palette (subseq d start (+ start len))))
              ((string= type "tRNS") (setf trns (subseq d start (+ start len))))
              ((string= type "IDAT") (push (cons start (+ start len)) idats))
              ((string= type "IEND") (return)))
        (setf p (+ start len 4))))
    (when (zerop w) (bad "PNG: no IHDR"))
    (when (null idats) (bad "PNG: no image data"))
    (when (and (= ctype 3) (null palette)) (bad "PNG: palette image without PLTE"))
    (let* ((channels (ecase ctype (0 1) (2 3) (3 1) (4 2) (6 4)))
           (bits (* channels depth))
           (bpp (max 1 (ash bits -3)))
           (passes (if (= interlace 1) *adam7* '((0 0 1 1))))
           (expected (loop for (x0 y0 dx dy) in passes
                           for pw = (if (> w x0) (ceiling (- w x0) dx) 0)
                           for ph = (if (> h y0) (ceiling (- h y0) dy) 0)
                           when (and (plusp pw) (plusp ph))
                             sum (* ph (1+ (ceiling (* pw bits) 8)))))
           (zdata (let* ((idats (reverse idats))
                         (total (loop for (s . e) in idats sum (- e s)))
                         (z (make-array total :element-type '(unsigned-byte 8)))
                         (q 0))
                    (loop for (s . e) in idats
                          do (replace z d :start1 q :start2 s :end2 e) (incf q (- e s)))
                    z))
           (raw (handler-case (zlib-decompress zdata :size-hint expected :max-output expected)
                  ;; a truncated or over-long stream still yields a partial picture
                  ;; in most viewers; we prefer an honest refusal
                  (inflate-error (e) (bad "PNG: ~a" (princ-to-string e)))))
           (out (make-argb (* w h)))
           (alpha-p (or (= ctype 4) (= ctype 6) (and trns t)))
           ;; palette as ready-made pixels, transparency folded in
           (pal (let ((v (make-argb 256 #xFF000000)))
                  (when palette
                    (dotimes (i (min 256 (floor (length palette) 3)))
                      (setf (aref v i)
                            (pack (if (and trns (< i (length trns))) (aref trns i) 255)
                                  (aref palette (* 3 i)) (aref palette (+ 1 (* 3 i)))
                                  (aref palette (+ 2 (* 3 i)))))))
                  v))
           ;; tRNS for gray/RGB: one colour key, compared against RAW samples
           (key (when (and trns (member ctype '(0 2)))
                  (if (= ctype 0)
                      (list (u16be trns 0))
                      (list (u16be trns 0) (u16be trns 2) (u16be trns 4))))))
      (declare (type octets raw) (type argb out pal))
      (when (< (length raw) expected) (bad "PNG: image data is short (~d of ~d bytes)" (length raw) expected))
      (let ((o 0))
        (declare (type fixnum o))
        (loop for (x0 y0 dx dy) in passes
              for pw = (if (> w x0) (ceiling (- w x0) dx) 0)
              for ph = (if (> h y0) (ceiling (- h y0) dy) 0)
              when (and (plusp pw) (plusp ph))
                do (let ((stride (ceiling (* pw bits) 8)) (prev -1))
                     (dotimes (row ph)
                       (png-unfilter raw o stride bpp prev)
                       (png-emit raw (1+ o) pw ctype depth pal key out
                                 (+ (* (+ y0 (* row dy)) w) x0) dx)
                       (setf prev (1+ o))
                       (incf o (1+ stride))))))
      (values w h out alpha-p))))


;;; ===========================================================================
;;; GIF — first frame
;;; ===========================================================================
;;;
;;; A GIF is a palette and a stream of colour INDICES compressed with LZW. LZW
;;; needs no transmitted table: encoder and decoder grow the SAME dictionary as
;;; they go — every code read adds "previous string + first character of this
;;; one". Codes start (min-code-size + 1) bits wide and widen as the table fills,
;;; up to 12 bits; a CLEAR code restarts. The one subtle case: a code that is not
;;; in the table YET (the encoder used it the moment it made it) — it can only
;;; mean "previous string + its own first character" (the "KwKwK" case).

(defun lzw-decode (data min-size npix)
  "Decode GIF LZW DATA (sub-blocks already joined) into NPIX indices."
  (declare (type octets data) (type fixnum min-size npix)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (unless (<= 1 min-size 11) (bad "GIF: bad LZW code size ~d" min-size))
  (let* ((out (make-array npix :element-type '(unsigned-byte 8) :initial-element 0))
         (prefix (make-array 4096 :element-type '(unsigned-byte 16) :initial-element 0))
         (suffix (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0))
         (stack (make-array 4097 :element-type '(unsigned-byte 8)))
         (clear (ash 1 min-size)) (eoi (1+ clear))
         (next (+ clear 2)) (size (1+ min-size))
         (old -1) (o 0) (pos 0) (bitbuf 0) (bitcnt 0) (n (length data)))
    (declare (type fixnum clear eoi next size old o pos bitcnt n)
             (type (unsigned-byte 40) bitbuf))
    (dotimes (i (min clear 256)) (setf (aref suffix i) i))
    (loop
      (when (>= o npix) (return))
      ;; next code, LSB-first like DEFLATE
      (loop while (and (< bitcnt size) (< pos n))
            do (setf bitbuf (logior bitbuf (ash (aref data pos) bitcnt)))
               (incf pos) (incf bitcnt 8))
      (when (< bitcnt size) (return))            ; data ran out: keep what we have
      (let ((code (logand bitbuf (1- (ash 1 size)))))
        (declare (type fixnum code))
        (setf bitbuf (ash bitbuf (- size))) (decf bitcnt size)
        (cond
          ((= code clear) (setf next (+ clear 2) size (1+ min-size) old -1))
          ((= code eoi) (return))
          ((< old 0)
           (when (>= code clear) (bad "GIF: LZW stream starts with code ~d" code))
           (setf (aref out o) code) (incf o) (setf old code))
          (t
           (when (> code next) (bad "GIF: LZW code ~d beyond table (~d)" code next))
           (let ((sp 0) (c (if (= code next) old code)))
             (declare (type fixnum sp c))
             (loop while (>= c clear)
                   do (setf (aref stack sp) (aref suffix c)) (incf sp)
                      (setf c (aref prefix c)))
             (setf (aref stack sp) c) (incf sp)     ; C is now the root = first char
             (let ((first c))
               (loop while (and (> sp 0) (< o npix))
                     do (decf sp) (setf (aref out o) (aref stack sp)) (incf o))
               (when (and (= code next) (< o npix))   ; KwKwK: + its first char
                 (setf (aref out o) first) (incf o))
               (when (< next 4096)
                 (setf (aref prefix next) old (aref suffix next) first)
                 (incf next)
                 (when (and (= next (ash 1 size)) (< size 12)) (incf size))))
             (setf old code))))))
    out))

(defun decode-gif (d)
  (declare (type octets d))
  (let* ((sw (u16le d 6)) (sh (u16le d 8)) (flags (aref d 10))
         (p 13) (gct nil) (transparent nil))
    (when (logbitp 7 flags)
      (let ((size (* 3 (ash 1 (1+ (logand flags 7))))))
        (setf gct (subseq d p (+ p size))) (incf p size)))
    (loop
      (case (aref d p)
        (#x21                                       ; extension
         (let ((label (aref d (1+ p))))
           (incf p 2)
           (when (and (= label #xF9) (>= (aref d p) 4) (logbitp 0 (aref d (1+ p))))
             (setf transparent (aref d (+ p 4))))  ; graphic control: transparent index
           (loop for len = (aref d p) do (incf p (1+ len)) until (zerop len))))
        (#x2C                                       ; image descriptor: the frame
         (let* ((fx (u16le d (+ p 1))) (fy (u16le d (+ p 3)))
                (fw (u16le d (+ p 5))) (fh (u16le d (+ p 7))) (ff (aref d (+ p 9)))
                (pal gct))
           (incf p 10)
           (when (logbitp 7 ff)
             (let ((size (* 3 (ash 1 (1+ (logand ff 7))))))
               (setf pal (subseq d p (+ p size))) (incf p size)))
           (unless pal (bad "GIF: no colour table"))
           ;; a frame bigger than the screen (it happens) grows the screen
           (setf sw (max sw (+ fx fw)) sh (max sh (+ fy fh)))
           (check-size sw sh)
           (check-size fw fh)
           (let* ((min-size (aref d p))
                  (data (let ((parts '()) (q (1+ p)))
                          (loop for len = (if (< q (length d)) (aref d q) 0)
                                until (zerop len)
                                do (push (subseq d (1+ q) (min (length d) (+ q 1 len))) parts)
                                   (incf q (1+ len)))
                          (apply #'concatenate 'octets (nreverse parts))))
                  (idx (lzw-decode data min-size (* fw fh)))
                  (out (make-argb (* sw sh) 0))      ; outside the frame: transparent
                  (npal (floor (length pal) 3))
                  (colors (make-argb 256 #xFF000000)))
             (dotimes (i (min 256 npal))
               (setf (aref colors i) (pack 255 (aref pal (* 3 i)) (aref pal (+ 1 (* 3 i)))
                                           (aref pal (+ 2 (* 3 i))))))
             (when transparent
               (setf (aref colors transparent) (logand #xFFFFFF (aref colors transparent))))
             ;; interlaced GIFs store rows 0,8,16.. then 4,12.. then 2,6.. then 1,3..
             (let ((rows (if (logbitp 6 ff)
                             (loop for (start step) in '((0 8) (4 8) (2 4) (1 2))
                                   nconc (loop for y from start below fh by step collect y))
                             (loop for y below fh collect y))))
               (loop for y in rows for r from 0
                     do (dotimes (x fw)
                          (let ((dx (+ fx x)) (dy (+ fy y)))
                            (when (and (< dx sw) (< dy sh))
                              (setf (aref out (+ (* dy sw) dx))
                                    (aref colors (aref idx (+ (* r fw) x)))))))))
             (return-from decode-gif
               (values sw sh out (or (and transparent t)
                                     (/= fw sw) (/= fh sh) (/= fx 0) (/= fy 0)))))))
        (#x3B (bad "GIF: no image in file"))
        (t (bad "GIF: unknown block #x~2,'0x" (aref d p)))))))

;;; ===========================================================================
;;; BMP (and the DIB inside an ICO)
;;; ===========================================================================
;;;
;;; A BMP is a 14-byte file header pointing at the pixels, then a DIB header whose
;;; SIZE says which of six historical versions it is (12 = OS/2, 40 = Windows 3,
;;; 52/56/108/124 add colour masks, alpha, colour spaces). Rows are padded to 4
;;; bytes and, if the height is positive, stored BOTTOM-UP — the Windows default.
;;; 16- and 32-bit pixels may come with BITFIELDS: one mask per channel, decoded
;;; generically by shifting the mask's bits down and rescaling them to 8 bits.

(defun mask-shift-bits (mask)
  "For a channel MASK: (values shift width)."
  (if (zerop mask)
      (values 0 0)
      (let ((shift (loop for s from 0 until (logbitp s mask) finally (return s))))
        (values shift (integer-length (ash mask (- shift)))))))

(defun decode-dib (d dib &key pixel-offset ico)
  "Decode the DIB whose header starts at D[DIB]. PIXEL-OFFSET is where the pixels
   start (BMP's file header says; in an ICO they follow the palette). ICO = the
   height is doubled and an AND mask (1 bit/pixel transparency) follows."
  (declare (type octets d))
  (let* ((hsize (u32le d dib))
         (core (= hsize 12))
         (w (if core (u16le d (+ dib 4)) (s32le d (+ dib 4))))
         (h0 (if core (u16le d (+ dib 6)) (s32le d (+ dib 8))))
         (bpp (if core (u16le d (+ dib 10)) (u16le d (+ dib 14))))
         (comp (if core 0 (u32le d (+ dib 16))))
         (clrused (if core 0 (u32le d (+ dib 32))))
         (top-down (minusp h0))
         (h (let ((a (abs h0))) (if ico (floor a 2) a)))
         (masks-at (cond ((>= hsize 52) (+ dib 40))
                         ((member comp '(3 6)) (+ dib hsize))))
         (pal-at (+ dib hsize (cond ((>= hsize 52) 0) ((= comp 3) 12) ((= comp 6) 16) (t 0))))
         (entry (if core 3 4))
         (ncolors (if (<= bpp 8) (if (plusp clrused) (min clrused 256) (ash 1 bpp)) 0))
         (pixels (or pixel-offset (+ pal-at (* entry ncolors))))
         (stride (* 4 (ceiling (* w bpp) 32)))
         (pal (make-argb 256 #xFF000000))
         (out nil) (alpha-p nil))
    (when (minusp w) (bad "BMP: negative width"))
    (check-size w h)
    (unless (member bpp '(1 2 4 8 16 24 32)) (bad "BMP: ~d bits per pixel unsupported" bpp))
    (unless (member comp '(0 1 2 3 6)) (bad "BMP: compression ~d unsupported (JPEG/PNG-in-BMP)" comp))
    (dotimes (i ncolors)
      (let ((q (+ pal-at (* i entry))))
        (setf (aref pal i) (pack 255 (aref d (+ q 2)) (aref d (+ q 1)) (aref d q)))))
    (setf out (make-argb (* w h)))
    (flet ((dest-row (y) (* w (if top-down y (- h 1 y)))))  ; file row Y -> output row
      (cond
        ((member comp '(1 2))                 ; RLE8 / RLE4: runs of palette indices
         (let ((x 0) (y 0) (q pixels) (n (length d)) (nib (= comp 2)))
           (flet ((put (i) (when (and (< x w) (< y h))
                             (setf (aref out (+ (dest-row y) x)) (aref pal i)))
                    (incf x)))
             (loop while (< (1+ q) n)
                   do (let ((a (aref d q)) (b (aref d (1+ q))))
                        (incf q 2)
                        (cond ((plusp a)       ; a run of A pixels
                               (dotimes (k a)
                                 (put (if nib (if (evenp k) (ash b -4) (logand b 15)) b))))
                              ((= b 0) (setf x 0) (incf y))          ; end of line
                              ((= b 1) (return))                     ; end of bitmap
                              ((= b 2) (incf x (aref d q)) (incf y (aref d (1+ q))) (incf q 2))
                              (t                                     ; B literal pixels
                               (dotimes (k b)
                                 (put (if nib
                                          (let ((byte (aref d (+ q (ash k -1)))))
                                            (if (evenp k) (ash byte -4) (logand byte 15)))
                                          (aref d (+ q k)))))
                               (let ((bytes (if nib (ceiling b 2) b)))
                                 (incf q (+ bytes (logand bytes 1)))))))))))
        ((<= bpp 8)
         (dotimes (y h)
           (let ((row (+ pixels (* y stride))) (o (dest-row y)))
             (dotimes (x w)
               (let* ((bit (* x bpp))
                      (i (logand (ash (aref d (+ row (ash bit -3))) (- (+ (logand bit 7) bpp) 8))
                                 (1- (ash 1 bpp)))))
                 (setf (aref out (+ o x)) (aref pal i)))))))
        ((= bpp 24)
         (dotimes (y h)
           (let ((row (+ pixels (* y stride))) (o (dest-row y)))
             (dotimes (x w)
               (let ((q (+ row (* 3 x))))
                 (setf (aref out (+ o x)) (pack 255 (aref d (+ q 2)) (aref d (+ q 1)) (aref d q))))))))
        (t                                    ; 16 / 32: channel masks
         (destructuring-bind (rm gm bm am)
             (if masks-at
                 (list (u32le d masks-at) (u32le d (+ masks-at 4)) (u32le d (+ masks-at 8))
                       (if (or (>= hsize 56) (= comp 6)) (u32le d (+ masks-at 12)) 0))
                 (if (= bpp 16)
                     (list #x7C00 #x03E0 #x001F 0)
                     (list #xFF0000 #xFF00 #xFF 0)))
           ;; with BI_RGB the masks fields of a V4/V5 header are not meaningful
           (when (and (= comp 0) masks-at)
             (setf rm (if (= bpp 16) #x7C00 #xFF0000) gm (if (= bpp 16) #x03E0 #xFF00)
                   bm (if (= bpp 16) #x1F #xFF)
                   am (if (and ico (= bpp 32)) #xFF000000 0)))
           (when (and ico (= bpp 32) (zerop am)) (setf am #xFF000000))
           (multiple-value-bind (rs rw) (mask-shift-bits rm)
             (multiple-value-bind (gs gw) (mask-shift-bits gm)
               (multiple-value-bind (bs bw) (mask-shift-bits bm)
                 (multiple-value-bind (as aw) (mask-shift-bits am)
                   (flet ((chan (v m s wd) (if (zerop wd) 0 (scale-bits (ash (logand v m) (- s)) (min wd 8)))))
                     (dotimes (y h)
                       (let ((row (+ pixels (* y stride))) (o (dest-row y)))
                         (dotimes (x w)
                           (let ((v (if (= bpp 16) (u16le d (+ row (* 2 x))) (u32le d (+ row (* 4 x))))))
                             (setf (aref out (+ o x))
                                   (pack (if (zerop aw) 255 (chan v am as aw))
                                         (chan v rm rs rw) (chan v gm gs gw) (chan v bm bs bw)))))))
                     (setf alpha-p (plusp aw))))))))))
      ;; An alpha channel that is zero everywhere is a writer that did not use it.
      (when (and alpha-p (every (lambda (px) (zerop (ash px -24))) out))
        (setf alpha-p nil)
        (map-into out (lambda (px) (logior px #xFF000000)) out))
      ;; ICO: the AND mask, 1 = transparent, only when there is no real alpha
      (when (and ico (not alpha-p))
        (let ((mstride (* 4 (ceiling w 32)))
              (mstart (+ pixels (* stride h))))
          (when (<= (+ mstart (* mstride h)) (length d))
            (dotimes (y h)
              (let ((row (+ mstart (* y mstride))) (o (dest-row y)))
                (dotimes (x w)
                  (when (logbitp (- 7 (logand x 7)) (aref d (+ row (ash x -3))))
                    (setf (aref out (+ o x)) (logand #xFFFFFF (aref out (+ o x)))
                          alpha-p t)))))))))
    (values w h out alpha-p)))

(defun decode-bmp (d)
  (decode-dib d 14 :pixel-offset (u32le d 10)))

;;; ===========================================================================
;;; ICO / CUR — a directory of images; preview the biggest
;;; ===========================================================================

(defun decode-ico (d)
  (declare (type octets d))
  (let ((count (u16le d 4)) (best nil) (best-key -1))
    (dotimes (i count)
      (let* ((e (+ 6 (* 16 i)))
             (w (let ((v (aref d e))) (if (zerop v) 256 v)))
             (h (let ((v (aref d (+ e 1)))) (if (zerop v) 256 v)))
             (bpp (u16le d (+ e 6)))
             (key (+ (* w h 64) bpp)))
        (when (> key best-key) (setf best-key key best e))))
    (unless best (bad "ICO: empty directory"))
    (let ((size (u32le d (+ best 8))) (off (u32le d (+ best 12))))
      (when (> (+ off 8) (length d)) (bad "ICO: entry beyond end of file"))
      (let ((sub (subseq d off (min (length d) (+ off size)))))
        (if (eql (image-format sub) :png)
            (decode-png sub)                      ; Vista+ icons embed whole PNGs
            (decode-dib sub 0 :ico t))))))

;;; ===========================================================================
;;; PNM — the Netpbm family: P1-P6 (+ P7, PAM)
;;; ===========================================================================

(defun decode-pnm (d)
  (declare (type octets d))
  (let ((p 2) (n (length d)) (kind (- (aref d 1) (char-code #\0))))
    (labels ((ws-p (c) (member c '(9 10 11 12 13 32)))
             (skip ()                    ; whitespace and # comments
               (loop while (< p n)
                     do (cond ((ws-p (aref d p)) (incf p))
                              ((= (aref d p) (char-code #\#))
                               (loop while (and (< p n) (/= (aref d p) 10)) do (incf p)))
                              (t (return)))))
             (token ()
               (skip)
               (let ((s p))
                 (loop while (and (< p n) (not (ws-p (aref d p)))) do (incf p))
                 (map 'string #'code-char (subseq d s p))))
             (int ()
               (let ((v (parse-integer (token) :junk-allowed t)))
                 (unless (and v (>= v 0)) (bad "PNM: bad header number")) v)))
      (let (w h (maxval 1) (depth 1))
        (if (= kind 7)
            (loop for tok = (token)            ; PAM: KEY VALUE lines up to ENDHDR
                  do (cond ((string= tok "WIDTH") (setf w (int)))
                           ((string= tok "HEIGHT") (setf h (int)))
                           ((string= tok "DEPTH") (setf depth (int)))
                           ((string= tok "MAXVAL") (setf maxval (int)))
                           ((string= tok "TUPLTYPE") (token))
                           ((string= tok "ENDHDR") (return))
                           ((string= tok "") (bad "PAM: no ENDHDR"))))
            (progn (setf w (int) h (int))
                   (unless (member kind '(1 4)) (setf maxval (int)))
                   (setf depth (if (member kind '(3 6)) 3 1))))
        (check-size w h)
        (unless (<= 1 maxval 65535) (bad "PNM: maxval ~d" maxval))
        (unless (<= 1 depth 4) (bad "PAM: depth ~d" depth))
        (incf p)                                ; the single whitespace before data
        (let* ((out (make-argb (* w h)))
               (wide (> maxval 255))
               (alpha-p (member depth '(2 4)))
               (next (case kind
                       ((2 3) (lambda () (int)))
                       (1 (lambda ()          ; P1 digits need no separators
                            (skip) (prog1 (if (= (aref d p) (char-code #\1)) 1 0) (incf p))))
                       (t (lambda ()
                            (if wide
                                (prog1 (u16be d p) (incf p 2))
                                (prog1 (aref d p) (incf p))))))))
          (flet ((to8 (v) (if (= maxval 255) v (floor (+ (* (min v maxval) 255) (floor maxval 2)) maxval))))
            (if (= kind 4)
                (let ((stride (ceiling w 8)) (base p))  ; P4: packed bits, 1 = black
                  (dotimes (y h)
                    (dotimes (x w)
                      (setf (aref out (+ (* y w) x))
                            (if (logbitp (- 7 (logand x 7)) (aref d (+ base (* y stride) (ash x -3))))
                                #xFF000000 #xFFFFFFFF)))))
                (dotimes (i (* w h))
                  (setf (aref out i)
                        (if (= kind 1)
                            (if (= 1 (funcall next)) #xFF000000 #xFFFFFFFF)
                            (case depth
                              (1 (let ((v (to8 (funcall next)))) (pack 255 v v v)))
                              (2 (let ((v (to8 (funcall next)))) (pack (to8 (funcall next)) v v v)))
                              (3 (let* ((r (to8 (funcall next))) (g (to8 (funcall next))) (b (to8 (funcall next))))
                                   (pack 255 r g b)))
                              (t (let* ((r (to8 (funcall next))) (g (to8 (funcall next)))
                                        (b (to8 (funcall next))) (a (to8 (funcall next))))
                                   (pack a r g b)))))))))
          (values w h out (and alpha-p t)))))))

;;; ===========================================================================
;;; QOI — "the Quite OK Image format": one page of spec, six opcodes
;;; ===========================================================================

(defun decode-qoi (d)
  (declare (type octets d) (optimize (speed 3) (safety 1))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((w (u32be d 4)) (h (u32be d 8)) (channels (aref d 12)))
    (check-size w h)
    (let* ((n (* w h)) (out (make-argb n))
           (index (make-argb 64 0))
           (r 0) (g 0) (b 0) (a 255) (run 0) (p 14))
      (declare (type fixnum p run n) (type (unsigned-byte 8) r g b a))
      (dotimes (i n)
        (if (plusp run)
            (decf run)
            (let ((b1 (aref d p)))
              (incf p)
              (cond ((= b1 #xFE) (setf r (aref d p) g (aref d (+ p 1)) b (aref d (+ p 2))) (incf p 3))
                    ((= b1 #xFF) (setf r (aref d p) g (aref d (+ p 1)) b (aref d (+ p 2))
                                       a (aref d (+ p 3)))
                     (incf p 4))
                    (t (case (ash b1 -6)
                         (0 (let ((px (aref index b1)))   ; QOI_OP_INDEX
                              (setf a (ldb (byte 8 24) px) r (ldb (byte 8 16) px)
                                    g (ldb (byte 8 8) px) b (ldb (byte 8 0) px))))
                         (1 (setf r (logand 255 (+ r (- (logand (ash b1 -4) 3) 2)))   ; DIFF
                                  g (logand 255 (+ g (- (logand (ash b1 -2) 3) 2)))
                                  b (logand 255 (+ b (- (logand b1 3) 2)))))
                         (2 (let* ((b2 (aref d p)) (vg (- (logand b1 63) 32)))       ; LUMA
                              (incf p)
                              (setf r (logand 255 (+ r vg -8 (ash b2 -4)))
                                    g (logand 255 (+ g vg))
                                    b (logand 255 (+ b vg -8 (logand b2 15))))))
                         (t (setf run (logand b1 63))))))           ; RUN (this + RUN more)
              (setf (aref index (mod (+ (* r 3) (* g 5) (* b 7) (* a 11)) 64))
                    (pack a r g b))))
        (setf (aref out i) (pack a r g b)))
      (values w h out (= channels 4)))))

;;; ===========================================================================
;;; TGA — Truevision Targa
;;; ===========================================================================

(defun decode-tga (d)
  (declare (type octets d))
  (let* ((idlen (aref d 0)) (cmtype (aref d 1)) (itype (aref d 2))
         (cmfirst (u16le d 3)) (cmlen (u16le d 5)) (cmbits (aref d 7))
         (w (u16le d 12)) (h (u16le d 14)) (bpp (aref d 16)) (desc (aref d 17))
         (rle (>= itype 8)) (base (logand itype 7))
         (p (+ 18 idlen))
         (alpha-bits (logand desc 15))
         (cmap nil))
    (check-size w h)
    (unless (member base '(1 2 3)) (bad "TGA: image type ~d unsupported" itype))
    (unless (member bpp '(8 15 16 24 32)) (bad "TGA: ~d bits per pixel" bpp))
    (flet ((color (q bits)                ; one stored colour at D[Q] -> argb
             (case bits
               ((15 16) (let ((v (u16le d q)))
                          (pack (if (and (= bits 16) (plusp alpha-bits)) (if (logbitp 15 v) 255 0) 255)
                                (scale-bits (ldb (byte 5 10) v) 5) (scale-bits (ldb (byte 5 5) v) 5)
                                (scale-bits (ldb (byte 5 0) v) 5))))
               (24 (pack 255 (aref d (+ q 2)) (aref d (+ q 1)) (aref d q)))
               (32 (pack (if (plusp alpha-bits) (aref d (+ q 3)) 255)
                         (aref d (+ q 2)) (aref d (+ q 1)) (aref d q)))
               (t (let ((v (aref d q))) (pack 255 v v v))))))
      (when (= cmtype 1)
        (let ((esize (ceiling cmbits 8)))
          (setf cmap (make-argb cmlen))
          (dotimes (i cmlen) (setf (aref cmap i) (color (+ p (* i esize)) cmbits)))
          (incf p (* cmlen esize))))
      (when (and (= base 1) (null cmap)) (bad "TGA: colour-mapped without a map"))
      (let* ((psize (ceiling bpp 8)) (n (* w h))
             (out (make-argb n))
             (pixel (lambda (q)
                      (case base
                        (1 (let ((i (- (if (= psize 1) (aref d q) (u16le d q)) cmfirst)))
                             (if (< -1 i (length cmap)) (aref cmap i) #xFF000000)))
                        (3 (let ((v (aref d q))) (pack 255 v v v)))
                        (t (color q bpp)))))
             (seq (make-argb n)))
        ;; first the pixels in file order, RLE expanded ...
        (if rle
            (let ((i 0))
              (loop while (< i n)
                    do (let* ((hdr (aref d p)) (count (1+ (logand hdr 127))))
                         (incf p)
                         (if (logbitp 7 hdr)
                             (let ((px (funcall pixel p)))
                               (incf p psize)
                               (loop repeat count while (< i n) do (setf (aref seq i) px) (incf i)))
                             (loop repeat count while (< i n)
                                   do (setf (aref seq i) (funcall pixel p)) (incf p psize) (incf i))))))
            (dotimes (i n) (setf (aref seq i) (funcall pixel (+ p (* i psize))))))
        ;; ... then placed: bit 5 = top-to-bottom, bit 4 = right-to-left
        (let ((top (logbitp 5 desc)) (rtl (logbitp 4 desc)))
          (dotimes (y h)
            (dotimes (x w)
              (setf (aref out (+ (* (if top y (- h 1 y)) w) (if rtl (- w 1 x) x)))
                    (aref seq (+ (* y w) x))))))
        (let ((alpha-p (some (lambda (px) (/= (ash px -24) 255)) out)))
          ;; all-zero alpha = an unused channel, not an invisible picture
          (when (and alpha-p (every (lambda (px) (zerop (ash px -24))) out))
            (setf alpha-p nil))
          (values w h out alpha-p))))))

;;; ===========================================================================
;;; The stb_image bridge — JPEG, PSD, HDR, PIC
;;; ===========================================================================
;;;
;;; libstbi.so is loaded the first time a format needs it, not at build time: a
;;; shared object loaded before SAVE-LISP-AND-DIE would be re-opened by path on
;;; every boot, and a missing file would then be a boot problem instead of a
;;; preview problem. The four entry points are looked up by name and called
;;; through SAP-ALIEN, so this file compiles and loads even where the library has
;;; never existed.

(defvar *source-directory*
  (let ((f (or *load-truename* *compile-file-truename*)))
    (and f (directory-namestring f)))
  "Where this file was loaded from — on the host, libstbi.so is built next to it.")

(defparameter *stbi-paths*
  (remove nil
          (list (sb-ext:posix-getenv "LOL_STBI")
                "/usr/lib/lol/libstbi.so"
                (and *source-directory* (concatenate 'string *source-directory* "out/libstbi.so"))))
  "Where to look for libstbi.so, in order: $LOL_STBI, the guest path, poc/out/.")

(defvar *stbi-state* nil
  "NIL = not tried yet; T = loaded; a string = why it could not be loaded.")

(defun stbi-available-p ()
  "Load libstbi.so on first call. True if its functions can be called."
  (when (null *stbi-state*)
    (setf *stbi-state*
          (or (loop for path in *stbi-paths*
                    when (and path (probe-file path))
                      do (handler-case (progn (sb-alien:load-shared-object path :dont-save t)
                                              (return t))
                           (error (e) (return (format nil "loading ~a: ~a" path e)))))
              (format nil "libstbi.so not found (looked in ~{~a~^, ~})" *stbi-paths*))))
  (and (eq *stbi-state* t)
       (sb-sys:find-dynamic-foreign-symbol-address "lol_stbi_load")
       t))

(defun stbi-fn (name)
  (or (sb-sys:find-dynamic-foreign-symbol-address name)
      (bad "libstbi.so has no ~a" name)))

(defun stbi-reason ()
  (sb-alien:alien-funcall
   (sb-alien:sap-alien (sb-sys:int-sap (stbi-fn "lol_stbi_reason"))
                       (function sb-alien:c-string))))

(defun decode-stbi (d)
  "Decode D through stb_image. -> (values w h argb alpha-p)."
  (declare (type octets d))
  (unless (stbi-available-p)
    (bad "needs libstbi.so: ~a" (if (stringp *stbi-state*) *stbi-state* "not loaded")))
  (sb-alien:with-alien ((w sb-alien:int) (h sb-alien:int) (c sb-alien:int))
    (sb-sys:with-pinned-objects (d)
      (let ((sap (sb-sys:vector-sap d)) (len (length d)))
        ;; 1. dimensions only, so the cap applies before stb allocates
        (when (zerop (sb-alien:alien-funcall
                      (sb-alien:sap-alien (sb-sys:int-sap (stbi-fn "lol_stbi_info"))
                                          (function sb-alien:int sb-alien:system-area-pointer
                                                    sb-alien:int (* sb-alien:int)
                                                    (* sb-alien:int) (* sb-alien:int)))
                      sap len (sb-alien:addr w) (sb-alien:addr h) (sb-alien:addr c)))
          (bad "stb_image: ~a" (stbi-reason)))
        (check-size w h)
        ;; 2. decode to RGBA8 in C memory, copy out, free
        (let ((p (sb-alien:alien-funcall
                  (sb-alien:sap-alien (sb-sys:int-sap (stbi-fn "lol_stbi_load"))
                                      (function sb-alien:system-area-pointer
                                                sb-alien:system-area-pointer sb-alien:int
                                                (* sb-alien:int) (* sb-alien:int) (* sb-alien:int)))
                  sap len (sb-alien:addr w) (sb-alien:addr h) (sb-alien:addr c))))
          (when (zerop (sb-sys:sap-int p)) (bad "stb_image: ~a" (stbi-reason)))
          (unwind-protect
               (let* ((n (* w h)) (out (make-argb n)))
                 (declare (type fixnum n) (type argb out))
                 (dotimes (i n)
                   ;; memory R,G,B,A = little-endian #xAABBGGRR: swap R and B
                   (let ((v (sb-sys:sap-ref-32 p (* 4 i))))
                     (declare (type (unsigned-byte 32) v))
                     (setf (aref out i)
                           (logior (logand v #xFF00FF00)
                                   (ash (logand v #xFF) 16)
                                   (logand (ash v -16) #xFF)))))
                 (values w h out (evenp c)))   ; 2 or 4 channels = had alpha
            (sb-alien:alien-funcall
             (sb-alien:sap-alien (sb-sys:int-sap (stbi-fn "lol_stbi_free"))
                                 (function sb-alien:void sb-alien:system-area-pointer))
             p)))))))

;;; ---- EXIF orientation (JPEG) ----------------------------------------------
;;; Phones store the sensor's pixels as shot and record HOW to turn them in the
;;; EXIF tag 0x0112 (1 = as is, 3 = 180, 6 = 90 CW, 8 = 90 CCW, 2/4/5/7 mirrored).
;;; stb_image ignores it, so we read the tag ourselves — a short walk over the
;;; APP1 segment's TIFF structure — and rotate the decoded pixels.

(defun jpeg-orientation (d)
  "The EXIF orientation (1-8) of JPEG D, or 1."
  (declare (type octets d))
  (ignore-errors
   (let ((p 2) (n (length d)))
     (loop while (and (< (+ p 4) n) (= (aref d p) #xFF))
           do (let ((m (aref d (1+ p))) (len (u16be d (+ p 2))))
                (when (member m '(#xDA #xD9)) (return))     ; scan data: no EXIF
                (when (and (= m #xE1) (bytes= d (+ p 4) (format nil "Exif~c~c" (code-char 0) (code-char 0))))
                  (let* ((tiff (+ p 10))
                         (le (bytes= d tiff "II"))
                         (r16 (lambda (i) (if le (u16le d i) (u16be d i))))
                         (r32 (lambda (i) (if le (u32le d i) (u32be d i))))
                         (ifd (+ tiff (funcall r32 (+ tiff 4)))))
                    (dotimes (k (funcall r16 ifd))
                      (let ((e (+ ifd 2 (* 12 k))))
                        (when (= (funcall r16 e) #x0112)
                          (let ((o (funcall r16 (+ e 8))))
                            (return-from jpeg-orientation (if (<= 1 o 8) o 1))))))))
                (incf p (+ 2 len))))
     1)))

(defun reorient (w h px orientation)
  "Apply EXIF ORIENTATION to the W x H image PX. -> (values w h px)."
  (declare (type argb px) (type fixnum w h))
  (if (or (null orientation) (= orientation 1))
      (values w h px)
      (let* ((swap (>= orientation 5))
             (nw (if swap h w)) (nh (if swap w h))
             (out (make-argb (* w h))))
        ;; for each DISPLAY pixel (x,y), the stored pixel (sx,sy) it shows
        (dotimes (y nh)
          (dotimes (x nw)
            (multiple-value-bind (sx sy)
                (ecase orientation
                  (2 (values (- w 1 x) y))
                  (3 (values (- w 1 x) (- h 1 y)))
                  (4 (values x (- h 1 y)))
                  (5 (values y x))
                  (6 (values y (- h 1 x)))
                  (7 (values (- w 1 y) (- h 1 x)))
                  (8 (values (- w 1 y) x)))
              (setf (aref out (+ (* y nw) x)) (aref px (+ (* sy w) sx))))))
        (values nw nh out))))

;;; ===========================================================================
;;; WebP lossless (VP8L)
;;; ===========================================================================
;;;
;;; A WebP file is a RIFF container. Its "VP8 " chunk is LOSSY — a VP8 video
;;; keyframe: a whole video codec's intra prediction and DCT. Out of scope (see
;;; doc/previews.org). Its "VP8L" chunk is LOSSLESS, and that is a compact design
;;; worth having: an ARGB image coded with five Huffman codes per "meta" group,
;;; LZ77 back-references with a 2-D distance map, a small colour cache, and up to
;;; four invertible TRANSFORMS applied before coding (predict each pixel from its
;;; neighbours; decorrelate colours; subtract green; or index a palette). The
;;; decoder reads the transforms, decodes the residual image, then undoes them in
;;; reverse order. Spec: RFC 9649.

(defstruct (vbits (:conc-name vb-))
  (d nil :type octets) (pos 0 :type fixnum) (end 0 :type fixnum)
  (buf 0 :type (unsigned-byte 62)) (cnt 0 :type fixnum))

(declaim (inline vb-read))
(defun vb-read (b n)
  "N bits (<= 24), LSB-first."
  (declare (type vbits b) (type (integer 0 32) n)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (loop while (< (vb-cnt b) 32)
        do (setf (vb-buf b) (logior (vb-buf b)
                                    (the (unsigned-byte 62)
                                         (ash (if (< (vb-pos b) (vb-end b)) (aref (vb-d b) (vb-pos b)) 0)
                                              (vb-cnt b)))))
           (incf (vb-pos b)) (incf (vb-cnt b) 8))
  (when (> (vb-pos b) (+ (vb-end b) 8)) (bad "WebP: truncated"))
  (prog1 (logand (vb-buf b) (1- (ash 1 n)))
    (setf (vb-buf b) (ash (vb-buf b) (- n)))
    (decf (vb-cnt b) n)))

;;; A VP8L prefix code: either "simple" (1-2 symbols listed outright) or canonical
;;; from code lengths, exactly DEFLATE's construction. We reuse the same idea —
;;; a fast table of the next FAST bits plus a canonical slow path — in a version
;;; that allows alphabets bigger than DEFLATE's (280 + cache size).

(defstruct (vhuff (:conc-name vh-))
  (single -1 :type fixnum)                    ; >= 0: the only symbol, 0 bits
  (fast nil :type (or null (simple-array (unsigned-byte 32) (*))))
  (counts nil :type (or null (simple-array fixnum (*))))
  (symbols nil :type (or null (simple-array fixnum (*)))))

(defun vh-build (lengths)
  "LENGTHS: a vector of code lengths per symbol."
  (let* ((n (length lengths))
         (used (loop for l across lengths count (plusp l))))
    (case used
      (0 (make-vhuff :single 0))
      (1 (make-vhuff :single (position-if #'plusp lengths)))
      (t
       (let ((counts (make-array 16 :element-type 'fixnum :initial-element 0))
             (symbols (make-array n :element-type 'fixnum :initial-element 0))
             (fast (make-array 1024 :element-type '(unsigned-byte 32) :initial-element 0))
             (offs (make-array 17 :element-type 'fixnum :initial-element 0))
             (next (make-array 16 :element-type 'fixnum :initial-element 0)))
         (loop for l across lengths do (incf (aref counts l)))
         (setf (aref counts 0) 0)
         (let ((left 1))
           (loop for len from 1 to 15
                 do (setf left (- (* 2 left) (aref counts len)))
                    (when (< left 0) (bad "WebP: over-subscribed code")))
           (unless (zerop left) (bad "WebP: incomplete code")))
         (loop for len from 1 below 15 do (setf (aref offs (1+ len)) (+ (aref offs len) (aref counts len))))
         (dotimes (i n) (let ((l (aref lengths i)))
                          (when (plusp l) (setf (aref symbols (aref offs l)) i) (incf (aref offs l)))))
         (let ((code 0))
           (loop for len from 1 to 15
                 do (setf code (ash (+ code (aref counts (1- len))) 1)) (setf (aref next len) code)))
         (dotimes (sym n)
           (let ((len (aref lengths sym)))
             (when (plusp len)
               (let ((code (aref next len)) (rev 0))
                 (incf (aref next len))
                 (when (<= len 10)
                   (dotimes (k len) (setf rev (logior (ash rev 1) (logand 1 (ash code (- k))))))
                   (loop for idx from rev below 1024 by (ash 1 len)
                         do (setf (aref fast idx) (logior (ash sym 4) len))))))))
         (make-vhuff :fast fast :counts counts :symbols symbols))))))

(defun vh-decode (h b)
  (declare (type vhuff h) (type vbits b)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((single (vh-single h)))
    (if (>= single 0)
        single
        (progn
          (vb-read b 0)                          ; make sure >= 32 bits are buffered
          (let* ((bits (vb-buf b))
                 (e (aref (the (simple-array (unsigned-byte 32) (*)) (vh-fast h)) (logand bits 1023))))
            (if (plusp e)
                (progn (vb-read b (logand e 15)) (ash e -4))
                (let ((counts (vh-counts h)) (symbols (vh-symbols h)) (code 0) (first 0) (index 0))
                  (declare (type fixnum code first index)
                           (type (simple-array fixnum (*)) counts symbols))
                  (loop for len from 1 to 15
                        do (setf code (logior code (logand 1 (ash bits (- 1 len)))))
                           (let ((count (aref counts len)))
                             (when (< (- code first) count)
                               (vb-read b len)
                               (return-from vh-decode (aref symbols (+ index (- code first)))))
                             (incf index count)
                             (setf first (ash (+ first count) 1) code (ash code 1))))
                  (bad "WebP: bad prefix code"))))))))

(defparameter *vp8l-clen-order* #(17 18 0 1 2 3 4 5 16 6 7 8 9 10 11 12 13 14 15))

(defun vp8l-read-code (b alphabet)
  "Read one prefix code over ALPHABET symbols."
  (let ((lengths (make-array alphabet :element-type '(unsigned-byte 8) :initial-element 0)))
    (if (= 1 (vb-read b 1))
        ;; simple code: 1 or 2 symbols, the first in 1 or 8 bits, the second in 8
        (let* ((n (1+ (vb-read b 1)))
               (s0 (vb-read b (if (= 1 (vb-read b 1)) 8 1))))
          (when (>= s0 alphabet) (bad "WebP: simple-code symbol out of range"))
          (setf (aref lengths s0) 1)
          (when (= n 2)
            (let ((s1 (vb-read b 8)))
              (when (>= s1 alphabet) (bad "WebP: simple-code symbol out of range"))
              (setf (aref lengths s1) 1))))
        (let ((clens (make-array 19 :element-type '(unsigned-byte 8) :initial-element 0))
              (ncl (+ 4 (vb-read b 4))))
          (when (> ncl 19) (bad "WebP: bad code-length count"))
          (dotimes (i ncl) (setf (aref clens (aref *vp8l-clen-order* i)) (vb-read b 3)))
          (let* ((ch (vh-build clens))
                 (max-sym (if (= 1 (vb-read b 1))
                              (let ((nbits (+ 2 (* 2 (vb-read b 3)))))
                                (+ 2 (vb-read b nbits)))
                              alphabet))
                 (prev 8) (i 0))
            (when (> max-sym alphabet) (bad "WebP: max_symbol too large"))
            (loop while (and (< i alphabet) (plusp max-sym))
                  do (decf max-sym)
                     (let ((c (vh-decode ch b)))
                       (cond ((< c 16) (setf (aref lengths i) c) (incf i)
                              (when (plusp c) (setf prev c)))
                             (t (multiple-value-bind (val rep)
                                    (case c
                                      (16 (values prev (+ 3 (vb-read b 2))))
                                      (17 (values 0 (+ 3 (vb-read b 3))))
                                      (t (values 0 (+ 11 (vb-read b 7)))))
                                  (when (> (+ i rep) alphabet) (bad "WebP: code lengths overflow"))
                                  (fill lengths val :start i :end (+ i rep))
                                  (incf i rep)))))))))
    (vh-build lengths)))

(defparameter *vp8l-dist-map*
  #((0 1) (1 0) (1 1) (-1 1) (0 2) (2 0) (1 2) (-1 2) (2 1) (-2 1) (2 2) (-2 2) (0 3) (3 0)
    (1 3) (-1 3) (3 1) (-3 1) (2 3) (-2 3) (3 2) (-3 2) (0 4) (4 0) (1 4) (-1 4) (4 1)
    (-4 1) (3 3) (-3 3) (2 4) (-2 4) (4 2) (-4 2) (0 5) (3 4) (-3 4) (4 3) (-4 3) (5 0)
    (1 5) (-1 5) (5 1) (-5 1) (2 5) (-2 5) (5 2) (-5 2) (4 4) (-4 4) (3 5) (-3 5) (5 3)
    (-5 3) (0 6) (6 0) (1 6) (-1 6) (6 1) (-6 1) (2 6) (-2 6) (6 2) (-6 2) (4 5) (-4 5)
    (5 4) (-5 4) (3 6) (-3 6) (6 3) (-6 3) (0 7) (7 0) (1 7) (-1 7) (5 5) (-5 5) (7 1)
    (-7 1) (4 6) (-4 6) (6 4) (-6 4) (2 7) (-2 7) (7 2) (-7 2) (3 7) (-3 7) (7 3) (-7 3)
    (5 6) (-5 6) (6 5) (-6 5) (8 0) (4 7) (-4 7) (7 4) (-7 4) (8 1) (8 2) (6 6) (-6 6)
    (8 3) (5 7) (-5 7) (7 5) (-7 5) (8 4) (6 7) (-6 7) (7 6) (-7 6) (8 5) (7 7) (-7 7)
    (8 6) (8 7))
  "The 120 short 2-D distances (dx dy) of VP8L, nearest first.")

(defun vp8l-prefix-value (b sym)
  "A length or distance from its prefix symbol plus extra bits."
  (declare (type vbits b) (type fixnum sym))
  (if (< sym 4)
      (1+ sym)
      (let* ((extra (1- (ash sym -1)))
             (offset (ash (+ 2 (logand sym 1)) extra)))
        (+ offset (vb-read b extra) 1))))

(defun vp8l-decode-pixels (b w h groups meta meta-bits meta-w cache cache-bits)
  "The pixel loop of VP8L-DECODE-IMAGE: literals, back-references, cache hits."
  (declare (type vbits b) (type (unsigned-byte 24) w h meta-w) (type (integer 0 9) meta-bits)
           (type (integer 0 11) cache-bits)
           (type simple-vector groups) (type (or null argb) meta cache)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((n (* w h)) (out (make-argb n)) (i 0)
         (dist-map *vp8l-dist-map*)
         (cache-shift (- cache-bits 32)))
    (declare (type fixnum n i) (type argb out) (type simple-vector dist-map))
    (macrolet ((cache-add (px)
                 `(when cache
                    (setf (aref cache (ash (logand #xFFFFFFFF (* #x1E35A7BD ,px)) cache-shift)) ,px))))
      (loop while (< i n)
            do (let* ((g (if meta
                             (let ((x (mod i w)) (y (floor i w)))
                               (aref groups (ldb (byte 16 8)
                                                 (aref meta (+ (* (ash y (- meta-bits)) meta-w)
                                                               (ash x (- meta-bits)))))))
                             (aref groups 0)))
                      (s (vh-decode (svref g 0) b)))
                 (declare (type simple-vector g) (type fixnum s))
                 (cond
                   ((< s 256)                         ; a literal ARGB pixel
                    (let* ((r (vh-decode (svref g 1) b)) (bl (vh-decode (svref g 2) b))
                           (a (vh-decode (svref g 3) b))
                           (px (logior (ash (logand a 255) 24) (ash (logand r 255) 16)
                                       (ash s 8) (logand bl 255))))
                      (declare (type fixnum r bl a) (type (unsigned-byte 32) px))
                      (setf (aref out i) px) (cache-add px) (incf i)))
                   ((< s 280)                         ; LZ77 back-reference
                    (let* ((len (vp8l-prefix-value b (- s 256)))
                           (dsym (vh-decode (svref g 4) b))
                           (dcode (vp8l-prefix-value b dsym))
                           (dist (if (> dcode 120)
                                     (- dcode 120)
                                     (let ((xy (svref dist-map (1- dcode))))
                                       (max 1 (+ (* (the fixnum (second xy)) w)
                                                 (the fixnum (first xy))))))))
                      (declare (type fixnum len dcode dist))
                      (when (> dist i) (bad "WebP: distance before start"))
                      (dotimes (k (min len (- n i)))
                        (let ((px (aref out (- i dist))))
                          (setf (aref out i) px) (cache-add px) (incf i)))))
                   (t                                 ; colour-cache index
                    (unless cache (bad "WebP: cache symbol without a cache"))
                    (let ((px (aref cache (- s 280))))
                      (setf (aref out i) px) (cache-add px) (incf i)))))))
    out))

(defun vp8l-decode-image (b w h top-level)
  "Decode one entropy-coded image of W x H ARGB pixels. TOP-LEVEL images may have
   meta prefix codes (an entropy image choosing a code group per block); the
   sub-images (transform data, the entropy image itself) may not."
  (let* ((cache-bits (if (= 1 (vb-read b 1)) (vb-read b 4) 0))
         (cache-size (if (plusp cache-bits) (ash 1 cache-bits) 0))
         (cache (and (plusp cache-bits) (make-argb cache-size 0)))
         (meta-bits 0) (meta-w 1) (meta nil) (ngroups 1))
    (when (> cache-bits 11) (bad "WebP: colour cache bits ~d" cache-bits))
    (when (and top-level (= 1 (vb-read b 1)))
      (setf meta-bits (+ 2 (vb-read b 3))
            meta-w (ceiling w (ash 1 meta-bits)))
      (setf meta (vp8l-decode-image b meta-w (ceiling h (ash 1 meta-bits)) nil))
      ;; the group number lives in the red and green bytes
      (setf ngroups (1+ (loop for px across meta maximize (ldb (byte 16 8) px)))))
    (let ((groups (make-array ngroups)))
      (dotimes (g ngroups)
        (setf (aref groups g)
              (vector (vp8l-read-code b (+ 256 24 cache-size))  ; green + lengths + cache
                      (vp8l-read-code b 256) (vp8l-read-code b 256) (vp8l-read-code b 256)
                      (vp8l-read-code b 40))))                  ; distance
      (vp8l-decode-pixels b w h groups meta meta-bits meta-w cache cache-bits))))

(declaim (inline vp8l-add))
(defun vp8l-add (a b)
  "Per-byte addition mod 256 of two ARGB words — how residuals are applied."
  (declare (type (unsigned-byte 32) a b))
  (logior (logand (+ (logand a #xFF00FF00) (logand b #xFF00FF00)) #xFF00FF00)
          (logand (+ (logand a #x00FF00FF) (logand b #x00FF00FF)) #x00FF00FF)))

(defun vp8l-predict (mode l tp tr tl)
  "The 14 predictors of the predictor transform, from Left, Top, TopRight, TopLeft.
   Each works per channel (A, R, G, B bytes) — the MACROLETs below spell out one
   channel and are expanded four times."
  (declare (type (unsigned-byte 32) l tp tr tl) (type fixnum mode)
           (optimize (speed 3) (safety 0)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (macrolet ((ch (px s) `(ldb (byte 8 ,s) ,px))
             (per-channel ((s) expr)      ; build a word from EXPR evaluated per channel
               `(logior ,@(loop for sh in '(0 8 16 24)
                                collect `(ash (let ((,s ,sh)) ,expr) ,sh)))))
    (flet ((avg2 (a b)
             (declare (type (unsigned-byte 32) a b))
             ;; per-byte floor((a+b)/2) without unpacking: the classic bit trick
             (+ (logand a b) (ash (logand (logxor a b) #xFEFEFEFE) -1))))
      (declare (inline avg2))
      (case mode
        (0 #xFF000000) (1 l) (2 tp) (3 tr) (4 tl)
        (5 (avg2 (avg2 l tr) tp))
        (6 (avg2 l tl)) (7 (avg2 l tp)) (8 (avg2 tl tp)) (9 (avg2 tp tr))
        (10 (avg2 (avg2 l tl) (avg2 tp tr)))
        (11
         ;; Select: the estimate L+T-TL is nearer L by sum|T-TL|, nearer T by
         ;; sum|L-TL|; take the nearer one, T on a tie (RFC 9649)
         (let ((pl (+ (abs (- (ch tp 0) (ch tl 0))) (abs (- (ch tp 8) (ch tl 8)))
                      (abs (- (ch tp 16) (ch tl 16))) (abs (- (ch tp 24) (ch tl 24)))))
               (pt (+ (abs (- (ch l 0) (ch tl 0))) (abs (- (ch l 8) (ch tl 8)))
                      (abs (- (ch l 16) (ch tl 16))) (abs (- (ch l 24) (ch tl 24))))))
           (if (< pl pt) l tp)))
        (12 (per-channel (s) (max 0 (min 255 (- (+ (ch l s) (ch tp s)) (ch tl s))))))
        (13 (let ((a (avg2 l tp)))
              (declare (type (unsigned-byte 32) a))
              (per-channel (s) (let ((av (ch a s)))
                                 (max 0 (min 255 (+ av (truncate (- av (ch tl s)) 2))))))))
        (t #xFF000000)))))

(defun vp8l-undo-predictor (px w h bits bw img)
  (declare (type argb px img) (type (unsigned-byte 24) w h bw) (type (integer 0 9) bits)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (dotimes (y h)
    (dotimes (x w)
      (let* ((i (+ (* y w) x))
             (pred (cond ((and (= x 0) (= y 0)) #xFF000000)
                         ((= y 0) (aref px (1- i)))
                         ((= x 0) (aref px (- i w)))
                         (t (vp8l-predict (ldb (byte 4 8) (aref img (+ (* (ash y (- bits)) bw)
                                                                        (ash x (- bits)))))
                                          (aref px (1- i)) (aref px (- i w))
                                          ;; top-right of the last column is the
                                          ;; leftmost pixel of THIS row (spec quirk)
                                          (aref px (+ (- i w) 1))
                                          (aref px (- i w 1)))))))
        (declare (type fixnum i) (type (unsigned-byte 32) pred))
        (setf (aref px i) (vp8l-add (aref px i) pred))))))

(defun vp8l-undo-cross-color (px w h bits bw img)
  (declare (type argb px img) (type (unsigned-byte 24) w h bw) (type (integer 0 9) bits)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (flet ((s8 (v) (declare (type (unsigned-byte 8) v)) (if (>= v 128) (- v 256) v)))
    (declare (inline s8))
    (dotimes (y h)
      (dotimes (x w)
        (let* ((i (+ (* y w) x))
               (m (aref img (+ (* (ash y (- bits)) bw) (ash x (- bits)))))
               (g2r (s8 (ldb (byte 8 0) m))) (g2b (s8 (ldb (byte 8 8) m)))
               (r2b (s8 (ldb (byte 8 16) m)))
               (v (aref px i))
               (g (s8 (ldb (byte 8 8) v)))
               (r (logand 255 (+ (ldb (byte 8 16) v) (ash (* g2r g) -5))))
               (bl (logand 255 (+ (ldb (byte 8 0) v) (ash (* g2b g) -5)
                                  (ash (* r2b (s8 r)) -5)))))
          (declare (type fixnum i))
          (setf (aref px i) (logior (logand v #xFF00FF00) (ash r 16) bl)))))))

(defun vp8l-undo-subtract-green (px)
  (declare (type argb px)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (dotimes (i (length px) px)
    (let* ((v (aref px i)) (g (ldb (byte 8 8) v)))
      (setf (aref px i)
            (logior (logand v #xFF00FF00)
                    (ash (logand 255 (+ (ldb (byte 8 16) v) g)) 16)
                    (logand 255 (+ (logand v 255) g)))))))

(defun vp8l-undo-color-indexing (px width h bits pal)
  "Unpack palette indices (2^BITS per pixel, in the green byte) to WIDTH colours."
  (declare (type argb px pal) (type (unsigned-byte 24) width h) (type (integer 0 3) bits)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((out (make-argb (* width h)))
         (per (ash 1 bits)) (bw (ash 8 (- bits))) (mask (1- (ash 1 bw)))
         (np (length pal))
         (xsize (ceiling width per)))     ; the packed width
    (declare (type fixnum per bw mask np xsize))
    (dotimes (y h out)
      (dotimes (x width)
        (let* ((packed (ldb (byte 8 8) (aref px (+ (* y xsize) (ash x (- bits))))))
               (idx (logand (ash packed (- (* bw (logand x (1- per))))) mask)))
          (setf (aref out (+ (* y width) x)) (if (< idx np) (aref pal idx) 0)))))))

(defun decode-vp8l (d start end)
  "Decode the VP8L bitstream D[START,END)."
  (let ((b (make-vbits :d d :pos start :end end)))
    (unless (= (vb-read b 8) #x2F) (bad "WebP: bad VP8L signature"))
    (let* ((w (1+ (vb-read b 14))) (h (1+ (vb-read b 14)))
           (alpha-hint (vb-read b 1))
           (transforms '())
           (xsize w))
      (declare (ignorable alpha-hint))
      (unless (zerop (vb-read b 3)) (bad "WebP: VP8L version"))
      (check-size w h)
      ;; transforms, each type at most once, in the order they must be UNDONE last-first
      (loop while (= 1 (vb-read b 1))
            do (let ((type (vb-read b 2)))
                 (when (find type transforms :key #'first) (bad "WebP: repeated transform"))
                 (ecase type
                   ((0 1)                      ; predictor / cross-colour: a block image
                    (let* ((bits (+ 2 (vb-read b 3)))
                           (bw (ceiling xsize (ash 1 bits)))
                           (img (vp8l-decode-image b bw (ceiling h (ash 1 bits)) nil)))
                      (push (list type bits bw img xsize) transforms)))
                   (2 (push (list 2) transforms))   ; subtract green
                   (3                          ; colour indexing: a palette, pixels packed
                    (let* ((ncolors (1+ (vb-read b 8)))
                           (pal (vp8l-decode-image b ncolors 1 nil))
                           (bits (cond ((> ncolors 16) 0) ((> ncolors 4) 1) ((> ncolors 2) 2) (t 3))))
                      ;; the palette itself is delta-coded
                      (loop for i from 1 below ncolors
                            do (setf (aref pal i) (vp8l-add (aref pal i) (aref pal (1- i)))))
                      (push (list 3 bits xsize pal) transforms)
                      (setf xsize (ceiling xsize (ash 1 bits))))))))
      (let ((px (vp8l-decode-image b xsize h t)))
        ;; undo the transforms, most recent first (TRANSFORMS is already that order)
        (dolist (tr transforms)
          (ecase (first tr)
            (2 (vp8l-undo-subtract-green px))
            (3 (destructuring-bind (bits width pal) (rest tr)
                 (setf px (vp8l-undo-color-indexing px width h bits pal))))
            ;; predictor and cross-colour work at the width current when they were
            ;; read (narrower if a colour-indexing transform packed pixels first)
            (0 (destructuring-bind (bits bw img w) (rest tr)
                 (vp8l-undo-predictor px w h bits bw img)))
            (1 (destructuring-bind (bits bw img w) (rest tr)
                 (vp8l-undo-cross-color px w h bits bw img)))))
        (values w h px t)))))

(defun decode-webp (d)
  (declare (type octets d))
  ;; walk the RIFF chunks: VP8L = lossless (ours), VP8 = lossy (not ours)
  (let ((p 12) (n (min (length d) (+ 8 (u32le d 4)))))
    (loop while (<= (+ p 8) n)
          do (let ((id (map 'string #'code-char (subseq d p (+ p 4))))
                   (len (u32le d (+ p 4))))
               (cond ((string= id "VP8L")
                      (return-from decode-webp (decode-vp8l d (+ p 8) (min n (+ p 8 len)))))
                     ((string= id "VP8 ")
                      (bad "lossy WebP (VP8) is not supported — only lossless WebP")))
               (incf p (+ 8 len (logand len 1)))))
    (bad "WebP: no image chunk")))

;;; ===========================================================================
;;; Finishing: alpha compositing, and the public entry points
;;; ===========================================================================

(defun finish-pixels (w px alpha-p background)
  "Turn #xAARRGGBB into the canvas's #x00RRGGBB, in place. With alpha in use and a
   BACKGROUND (an #xRRGGBB integer, or :CHECKER), composite: out = c*a + bg*(1-a),
   rounded. BACKGROUND NIL just drops alpha (the colour under a transparent pixel
   shows — useful to compare with tools that do the same)."
  (declare (type argb px) (type fixnum w)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (if (or (not alpha-p) (null background))
      (dotimes (i (length px)) (setf (aref px i) (logand #xFFFFFF (aref px i))))
      (let ((c0 (car *checker-colors*)) (c1 (cdr *checker-colors*))
            (checker (eq background :checker))
            (solid (if (integerp background) background 0)))
        (declare (type (unsigned-byte 32) c0 c1 solid))
        (dotimes (i (length px))
          (let* ((v (aref px i)) (a (ash v -24)))
            (setf (aref px i)
                  (cond ((= a 255) (logand v #xFFFFFF))
                        (t (let ((bg (if checker
                                         (let ((x (mod i w)) (y (floor i w)))
                                           (if (evenp (+ (ash x -3) (ash y -3))) c0 c1))
                                         solid))
                                 (na (- 255 a)))
                             (declare (type (unsigned-byte 32) bg))
                             (flet ((mix (s)
                                      (floor (+ (* (ldb (byte 8 s) v) a) (* (ldb (byte 8 s) bg) na) 127) 255)))
                               (logior (ash (mix 16) 16) (ash (mix 8) 8) (mix 0)))))))))))
  px)

(defun supported-formats ()
  "The format keywords DECODE-IMAGE can decode right now (the stb_image ones only
   if libstbi.so is loadable)."
  (append '(:png :gif :bmp :pnm :qoi :tga :ico :webp)
          (when (stbi-available-p) '(:jpeg :psd :hdr :pic))))

(defun decode-image (octets &key (background :checker) (orient t) (max-pixels *max-pixels*))
  "Decode an image file's bytes.
   -> (values WIDTH HEIGHT PIXELS FORMAT) on success, PIXELS a row-major
      (simple-array (unsigned-byte 32) (*)) of #x00RRGGBB (poc/canvas.lisp's format),
      FORMAT a keyword (:png :jpeg :gif :bmp :pnm :qoi :tga :ico :webp :psd :hdr :pic);
   -> (values NIL REASON FORMAT) on failure: REASON a one-line string, FORMAT the
      sniffed keyword or NIL. NEVER signals.
   BACKGROUND: what translucent pixels are composited onto — :CHECKER (default, the
   *CHECKER-COLORS* checkerboard), an #xRRGGBB integer, or NIL (ignore alpha).
   ORIENT: honour a JPEG's EXIF orientation. MAX-PIXELS: the size cap."
  (let ((format nil))
    (handler-case
        (let* ((*max-pixels* max-pixels)
               (d (if (typep octets 'octets) octets (coerce octets 'octets))))
          (setf format (image-format d))
          (multiple-value-bind (w h px alpha-p)
              (case format
                (:png (decode-png d))
                (:gif (decode-gif d))
                (:bmp (decode-bmp d))
                (:ico (decode-ico d))
                (:pnm (decode-pnm d))
                (:qoi (decode-qoi d))
                (:tga (decode-tga d))
                (:webp (decode-webp d))
                ((:jpeg :psd :hdr :pic) (decode-stbi d))
                ((nil) (bad "unrecognised image format"))
                (t (bad "~a images are not supported" (string-downcase format))))
            (finish-pixels w px alpha-p background)
            (if (and orient (eq format :jpeg))
                (multiple-value-bind (nw nh npx) (reorient w h px (jpeg-orientation d))
                  (values nw nh npx format))
                (values w h px format))))
      (serious-condition (e)
        (values nil
                (let ((msg (ignore-errors (princ-to-string e))))
                  (if (and msg (typep e 'image-error))
                      msg
                      (format nil "corrupt or unreadable ~@[~a ~]data (~a)"
                              (and format (string-downcase format))
                              (or msg (type-of e)))))
                format)))))

(defun read-file-octets (path &key (max-bytes (* 256 1024 1024)))
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((len (file-length s)))
      (when (> len max-bytes) (bad "file is ~d bytes, over the ~d limit" len max-bytes))
      (let ((v (make-array len :element-type '(unsigned-byte 8))))
        (read-sequence v s)
        v))))

(defun decode-image-file (path &rest keys &key background orient max-pixels)
  "DECODE-IMAGE on the contents of the file at PATH. Same values; never signals
   (an unreadable file is (values NIL reason NIL))."
  (declare (ignore background orient max-pixels))
  (let ((d (handler-case (read-file-octets path)
             (serious-condition (e)
               (return-from decode-image-file
                 (values nil (format nil "cannot read ~a: ~a" path
                                     (or (ignore-errors (princ-to-string e)) (type-of e)))
                         nil))))))
    (apply #'decode-image d keys)))

;;; ===========================================================================
;;; Scaling for thumbnails
;;; ===========================================================================

(defun scale-to-fit (w h pixels max-w max-h &key upscale)
  "Shrink the W x H #x00RRGGBB image PIXELS to fit in MAX-W x MAX-H, keeping the
   aspect ratio. -> (values NW NH NPIXELS). A BOX FILTER: each output pixel is the
   average of the source rectangle it covers, so a 4000-pixel photo shrunk to 300
   stays smooth (no aliasing), and every source pixel is read exactly once. Never
   enlarges unless UPSCALE (then nearest-neighbour, for crisp pixel art); returns
   PIXELS itself when it already fits."
  (declare (type argb pixels) (type fixnum w h max-w max-h)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((s (min (/ max-w w) (/ max-h h)))
         (s (if upscale s (min s 1)))
         (nw (max 1 (min max-w (round (* w s)))))
         (nh (max 1 (min max-h (round (* h s))))))
    (declare (type fixnum nw nh))
    (when (and (= nw w) (= nh h)) (return-from scale-to-fit (values w h pixels)))
    (let ((out (make-argb (* nw nh))))
      (if (or (> nw w) (> nh h))
          (dotimes (y nh)                       ; enlarging: nearest neighbour
            (let ((sy (floor (* y h) nh)))
              (dotimes (x nw)
                (setf (aref out (+ (* y nw) x)) (aref pixels (+ (* sy w) (floor (* x w) nw)))))))
          (let ((xs (make-array (1+ nw) :element-type 'fixnum)))
            (dotimes (x (1+ nw)) (setf (aref xs x) (floor (* x w) nw)))
            (dotimes (y nh)
              (let* ((y0 (floor (* y h) nh)) (y1 (max (1+ y0) (floor (* (1+ y) h) nh))))
                (declare (type fixnum y0 y1))
                (dotimes (x nw)
                  (let* ((x0 (aref xs x)) (x1 (max (1+ x0) (aref xs (1+ x))))
                         (r 0) (g 0) (b 0) (cnt (* (- x1 x0) (- y1 y0))))
                    (declare (type fixnum x0 x1 r g b cnt))
                    (loop for sy of-type fixnum from y0 below y1
                          do (let ((row (* sy w)))
                               (declare (type fixnum row))
                               (loop for sx of-type fixnum from x0 below x1
                                     do (let ((v (aref pixels (+ row sx))))
                                          (incf r (ldb (byte 8 16) v))
                                          (incf g (ldb (byte 8 8) v))
                                          (incf b (ldb (byte 8 0) v))))))
                    (let ((half (ash cnt -1)))
                      (setf (aref out (+ (* y nw) x))
                            (logior (ash (floor (+ r half) cnt) 16)
                                    (ash (floor (+ g half) cnt) 8)
                                    (floor (+ b half) cnt))))))))))
      (values nw nh out))))
