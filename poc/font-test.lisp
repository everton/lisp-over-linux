;;;; font-test.lisp — host test for the TrueType engine (poc/ttf.lisp) and the resizable
;;;; FONT built on it (poc/canvas.lisp).
;;;;
;;;;     sbcl --script poc/font-test.lisp [FONT.ttf]      ; exits 0 if all pass
;;;;
;;;; Runs against DejaVu Sans Mono — the font build.sh ships by default — or the TTF
;;;; named on the command line / in $LOL_FONT (then the DejaVu-specific numbers are
;;;; skipped and only the font-independent properties are checked). The numbers below
;;;; were checked against the file with an independent reader (Perl's unpack on the
;;;; raw tables, and fc-query's charset), not against this code: unitsPerEm 2048,
;;;; hhea ascender 1901 / descender -483 / lineGap 0, 3377 glyphs, a cmap whose
;;;; best subtable is format 12, and no U+1F600 in the charset.
;;;;
;;;; Also prints how long rasterising and drawing take, the numbers doc/fonts.org quotes.

(let ((dir (make-pathname :directory (pathname-directory *load-truename*))))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "ttf.lisp" dir))
    (load (merge-pathnames "canvas.lisp" dir))))

(defpackage :lol.font-test (:use :cl :lol.canvas))
(in-package :lol.font-test)

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~64a ~:[FAIL~;PASS~]~%" name ok)
  (unless ok (incf *fails*)))

(defparameter *dejavu* "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf")
(defparameter *path* (or (second sb-ext:*posix-argv*) (sb-ext:posix-getenv "LOL_FONT") *dejavu*))
(defparameter *is-dejavu* (search "DejaVuSansMono.ttf" *path*))

(unless (probe-file *path*)
  (format t "~&no font at ~a — install fonts-dejavu-core or pass a .ttf~%" *path*)
  (sb-ext:exit :code 1))

(defparameter *face* (lol.ttf:load-ttf *path*))

(defun coverage (code size)
  "(values coverage w h left top) for CODE at SIZE pixels."
  (lol.ttf:rasterize-glyph *face* (lol.ttf:glyph-index *face* code) size))

(defun ink (cov) (count-if #'plusp cov))
(defun full (cov) (count 255 cov))

(format t "~&== the tables (~a) ==~%" (lol.ttf:face-name *face*))
(when *is-dejavu*
  (check "unitsPerEm is 2048" (= 2048 (lol.ttf:face-units-per-em *face*)))
  (check "hhea ascender 1901, descender -483, lineGap 0"
         (and (= 1901 (lol.ttf:face-ascender *face*)) (= -483 (lol.ttf:face-descender *face*))
              (zerop (lol.ttf:face-line-gap *face*))))
  (check "maxp says 3377 glyphs" (= 3377 (lol.ttf:face-num-glyphs *face*)))
  (check "the name table says DejaVu Sans Mono"
         (string= "DejaVu Sans Mono" (lol.ttf:face-name *face*)))
  (check "the cmap chosen is format 12 (all planes)"
         (= 12 (lol.ttf::face-cmap-format *face*))))

(format t "~&== cmap: codepoint -> glyph ==~%")
(let ((a (lol.ttf:glyph-index *face* (char-code #\A)))
      (b (lol.ttf:glyph-index *face* (char-code #\B))))
  (check "A and B are real, distinct glyphs" (and (plusp a) (plusp b) (/= a b)))
  (when *is-dejavu* (check "A is glyph 36 (DejaVu's order)" (= 36 a))))
(check "lambda (U+03BB) is in the font" (plusp (lol.ttf:glyph-index *face* #x3BB)))
(check "the em dash (U+2014) is in the font" (plusp (lol.ttf:glyph-index *face* #x2014)))
(when *is-dejavu*
  (check "U+1F600 is NOT: glyph 0, the .notdef fallback" (zerop (lol.ttf:glyph-index *face* #x1F600))))
(check "a codepoint past Unicode is glyph 0, never an error"
       (zerop (lol.ttf:glyph-index *face* #x10FFFF)))
;; format 4 must agree with format 12 on the BMP (both subtables exist in DejaVu)
(let ((f4 (lol.ttf::copy-face *face*)))
  (let ((cmap (lol.ttf::find-table (lol.ttf::face-data *face*) "cmap"))
        (d (lol.ttf::face-data *face*)))
    (loop for r from (+ cmap 4) by 8 repeat (lol.ttf::u16 d (+ cmap 2))
          for off = (+ cmap (lol.ttf::u32 d (+ r 4)))
          when (= 4 (lol.ttf::u16 d off))
            do (setf (lol.ttf::face-cmap f4) off (lol.ttf::face-cmap-format f4) 4)))
  (when (= 4 (lol.ttf::face-cmap-format f4))
    (check "format 4 and format 12 agree on 0..#x2FFF"
           (loop for c from 0 to #x2FFF
                 always (= (lol.ttf:glyph-index f4 c) (lol.ttf:glyph-index *face* c))))))

(format t "~&== hmtx: one advance for every glyph of a monospace font ==~%")
(let ((adv (lol.ttf:advance-width *face* (lol.ttf:glyph-index *face* (char-code #\M)))))
  (check "M, i, and lambda advance alike"
         (every (lambda (c) (= adv (lol.ttf:advance-width *face* (lol.ttf:glyph-index *face* c))))
                (list (char-code #\i) (char-code #\W) #x3BB)))
  (when *is-dejavu* (check "the advance is 1233 units" (= 1233 adv))))

(format t "~&== glyf + the rasteriser ==~%")
(multiple-value-bind (cov w h) (coverage (char-code #\Space) 18)
  (check "space has no outline: a 0x0 bitmap" (and (zerop w) (zerop h) (zerop (length cov)))))
(multiple-value-bind (cov w h left top) (coverage (char-code #\l) 18)
  (check "l at 18px sits above the baseline (top < 0) and inside the cell"
         (and (minusp top) (<= 0 left) (<= (+ left w) 12)))
  (check "l has a solid stem: some pixels fully covered" (plusp (full cov)))
  (check "...and anti-aliased edges: some partly covered" (> (ink cov) (full cov)))
  (check "l is taller than it is wide" (> h w)))
(multiple-value-bind (cov w h left top) (coverage (char-code #\g) 18)
  (declare (ignore cov w left))
  (check "g has a descender: its bitmap reaches below the baseline" (> (+ top h) 0)))
(let ((dot (ink (coverage (char-code #\.) 18)))
      (m (ink (coverage (char-code #\M) 18))))
  (check "a full stop inks far fewer pixels than an M" (< (* 4 dot) m)))
(multiple-value-bind (cov) (coverage (char-code #\O) 18)
  ;; the O's counter (its hole) must be EMPTY: if winding were ignored it would fill
  (multiple-value-bind (c w h) (coverage (char-code #\O) 18)
    (declare (ignore c))
    (check "O keeps its hole (the centre pixel is background)"
           (zerop (aref cov (+ (* (floor h 2) w) (floor w 2)))))))
(let ((e (lol.ttf:glyph-outline *face* (lol.ttf:glyph-index *face* #xE9)))
      (plain (lol.ttf:glyph-outline *face* (lol.ttf:glyph-index *face* (char-code #\e)))))
  (check "e-acute is a COMPOSITE: more contours than e alone"
         (> (length e) (length plain))))
(check "a bigger size inks more pixels"
       (< (ink (coverage (char-code #\A) 12)) (ink (coverage (char-code #\A) 24))))

(format t "~&== the FONT: sizes, cells, clamping ==~%")
(let ((f (load-ttf-font *path* :size 18)))
  (check "an outline font says so" (font-outline-p f))
  (when *is-dejavu*
    (check "18px DejaVu: an 11 x 21 cell, baseline 17 down"
           (and (= 11 (font-cw f)) (= 21 (font-ch f)) (= 17 (font-ascent f)))))
  (set-font-size f 28)
  (check "set-font-size 28 grows the cell" (and (= 28 (font-size f)) (> (font-cw f) 11)))
  (check "below the range clamps to +min-font-size+" (= +min-font-size+ (set-font-size f 3)))
  (check "above the range clamps to +max-font-size+" (= +max-font-size+ (set-font-size f 99)))
  (reset-font-size f)
  (check "reset returns to the size it was loaded at" (= 18 (font-size f)))
  (check "string-px is a multiplication: 10 cells" (= (* 10 (font-cw f)) (string-px f "0123456789"))))

(format t "~&== the glyph cache ==~%")
(let* ((f (load-ttf-font *path* :size 18))
       (c (make-canvas 400 40))
       (s "hello, world"))
  (draw-string c f s 0 0 #xFFFFFF)
  (let ((m1 (font-misses f)))
    (check "first draw: one miss per DISTINCT non-space char"
           (= m1 (length (remove-duplicates (remove #\Space s)))))
    (draw-string c f s 0 0 #xFFFFFF)
    (check "second draw: no new misses, all hits" (and (= m1 (font-misses f)) (plusp (font-hits f))))
    (set-font-size f 20)
    (draw-string c f s 0 0 #xFFFFFF)
    (check "a new size is a new key: misses again" (> (font-misses f) m1))
    (check "the same glyph object comes back for the same (size, code)"
           (eq (font-glyph f (char-code #\h) 20) (font-glyph f (char-code #\h) 20)))))

(format t "~&== draw-string blends coverage over the ground ==~%")
(let* ((f (load-ttf-font *path* :size 18))
       (c (make-canvas 60 30)))
  (fill-rect c 0 0 60 30 #x102030)
  (let ((pen (draw-string c f "Ml" 2 2 #xFFFFFF)))
    (check "returns the pen after two cells" (= pen (+ 2 (* 2 (font-cw f))))))
  (let ((px (canvas-pixels c)))
    (check "some pixels became pure white (full coverage)" (find #xFFFFFF px))
    (check "some became a blend, strictly between ground and ink"
           (find-if (lambda (p) (and (/= p #x102030) (/= p #xFFFFFF)
                                     (> (ldb (byte 8 16) p) #x10) (< (ldb (byte 8 16) p) #xFF)))
                    px))
    (check "the ground outside the text is untouched" (= #x102030 (aref px (+ (* 29 60) 59))))))

(format t "~&== the bitmap fallback still works, and still resizes ==~%")
(let ((b (load-font (merge-pathnames "cozette.lolf"
                                     (make-pathname :directory (pathname-directory *load-truename*))))))
  (check "Cozette loads as a 6x15 bitmap" (and (not (font-outline-p b)) (= 6 (font-cw b)) (= 15 (font-ch b))))
  (set-font-size b 30)
  (check "size 30 doubles it: 12x30, integer magnification" (and (= 12 (font-cw b)) (= 30 (font-ch b))))
  (let ((c (make-canvas 40 40)))
    (check "and draws at that pitch" (= 24 (draw-string c b "ab" 0 0 #xFFFFFF)))))

(format t "~&== timings (for doc/fonts.org) ==~%")
(let ((f (load-ttf-font *path* :size 18))
      (c (make-canvas 1920 1080))
      (line (make-string 170 :initial-element #\x)))
  (loop for i from 0 below 170 do (setf (char line i) (code-char (+ 33 (mod (* i 7) 94)))))
  (let ((t0 (get-internal-real-time)) (n 20))
    (dotimes (k n)                         ; cold every time: the cache is emptied first
      (clear-font-cache f)
      (loop for code from 33 to 126 do (font-glyph f code)))
    (format t "~&  rasterise the 94 printable ASCII glyphs at 18px, cold: ~,2f ms~%"
            (/ (- (get-internal-real-time) t0) (/ internal-time-units-per-second 1000) n)))
  (let ((t0 (get-internal-real-time)) (n 10))
    (dotimes (k n)
      (loop for row below 51 do (draw-string c f line 0 (* row 21) #xD6D3C8)))
    (format t "~&  draw a full 1920x1080 screen of text (170x51 cells), cached: ~,1f ms~%"
            (/ (- (get-internal-real-time) t0) (/ internal-time-units-per-second 1000) n)))
  ;; the same number of glyphs with the old bitmap path, for comparison
  (let ((b (load-font (merge-pathnames "cozette.lolf"
                                       (make-pathname :directory (pathname-directory *load-truename*)))))
        (t0 (get-internal-real-time)) (n 10))
    (dotimes (k n)
      (loop for row below 51 do (draw-string c b line 0 (* row 15) #xD6D3C8)))
    (format t "~&  ... the same 170x51 glyphs with the 6x15 bitmap font:        ~,1f ms~%"
            (/ (- (get-internal-real-time) t0) (/ internal-time-units-per-second 1000) n))))

(format t "~&~:[===== ~d FAILURE(S) =====~;===== ALL PASS =====~]~%" (zerop *fails*) *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
