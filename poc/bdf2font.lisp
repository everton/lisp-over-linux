;;;; bdf2font.lisp — convert a BDF bitmap font into the tiny .lolf blob canvas.lisp reads.
;;;;
;;;;     sbcl --script poc/bdf2font.lisp IN.bdf OUT.lolf
;;;;
;;;; Why this exists (doc/code-browser.org §6.2, doc/fonts.org): we bake a permissive
;;;; bitmap font into the tool the way alien.rgba is baked in, and we want to control
;;;; the codepoint->glyph index OURSELVES. PSF's implicit glyph ordering is what
;;;; caused the "·" -> "Ö" class of bug. So this reads a BDF — honouring each glyph's
;;;; advance width, so the font renders tight — and writes glyphs indexed by their
;;;; real codepoint.
;;;;
;;;; The .lolf format (read by LOAD-FONT in poc/canvas.lisp):
;;;;     byte 0-1 : magic 0x4C 0x46 ("LF")
;;;;     byte 2   : glyph WIDTH in pixels (1..8 for the current 1-byte-per-row blitter)
;;;;     byte 3   : glyph HEIGHT in pixels
;;;;     byte 4.. : 256 glyphs, each HEIGHT bytes (1 byte per row, MSB = leftmost
;;;;                pixel), indexed by codepoint 0..255 — so #\· (U+00B7 = 183) IS
;;;;                the middle dot, which is the whole point.
;;;;
;;;; To regenerate the shipped font:
;;;;   curl -fsSL -o /tmp/cozette.bdf \
;;;;     https://github.com/slavfox/Cozette/releases/latest/download/cozette.bdf
;;;;   sbcl --script poc/bdf2font.lisp /tmp/cozette.bdf poc/cozette.lolf
;;;;
;;;; A build-host tool; nothing here ships in the image.

(defun split-words (line)
  "LINE split on runs of whitespace."
  (let ((out '()) (i 0) (n (length line)))
    (flet ((wsp (c) (member c '(#\Space #\Tab #\Return))))
      (loop while (< i n)
            do (loop while (and (< i n) (wsp (char line i))) do (incf i))
               (let ((start i))
                 (loop while (and (< i n) (not (wsp (char line i)))) do (incf i))
                 (when (> i start) (push (subseq line start i) out)))))
    (nreverse out)))

(defun starts-with (prefix line)
  (and (>= (length line) (length prefix)) (string= prefix line :end2 (length prefix))))

(defun read-lines (path)
  (with-open-file (s path :external-format :latin-1)
    (loop for line = (read-line s nil nil) while line collect line)))

(defstruct glyph dwidth bbw bbh bxoff byoff rows)

(defun parse-bdf (path)
  "Parse PATH. Returns (values CELL-W CELL-H ASCENT GLYPHS), GLYPHS being a hash of
   codepoint -> GLYPH."
  (let* ((lines (coerce (read-lines path) 'vector))
         (n (length lines))
         (glyphs (make-hash-table))
         (dwidths '())
         fbb-w fbb-h fbb-yoff)
    ;; FONTBOUNDINGBOX w h xoff yoff — the cell every glyph is placed into
    (loop for line across lines
          when (starts-with "FONTBOUNDINGBOX" line)
            do (let ((p (split-words line)))
                 (setf fbb-w (parse-integer (second p))
                       fbb-h (parse-integer (third p))
                       fbb-yoff (parse-integer (fifth p)))
                 (return)))
    (unless fbb-h (error "~a has no FONTBOUNDINGBOX" path))
    (let ((i 0))
      (loop while (< i n)
            do (if (not (starts-with "STARTCHAR" (aref lines i)))
                   (incf i)
                   (let ((code nil) (dw nil) (bbx nil) (rows '()))
                     (incf i)
                     (loop while (and (< i n) (not (starts-with "ENDCHAR" (aref lines i))))
                           do (let ((line (aref lines i)))
                                (cond
                                  ((starts-with "ENCODING" line)
                                   (setf code (parse-integer (second (split-words line))
                                                             :junk-allowed t))
                                   (incf i))
                                  ((starts-with "DWIDTH" line)
                                   (setf dw (parse-integer (second (split-words line))
                                                           :junk-allowed t))
                                   (incf i))
                                  ((starts-with "BBX" line)
                                   (let ((p (split-words line)))
                                     (setf bbx (mapcar (lambda (x) (parse-integer x))
                                                       (subseq p 1 5))))
                                   (incf i))
                                  ((string= "BITMAP" (string-right-trim " " line))
                                   (incf i)
                                   ;; hex rows until ENDCHAR or a blank line
                                   (loop while (and (< i n)
                                                    (not (starts-with "ENDCHAR" (aref lines i)))
                                                    (plusp (length (string-trim " " (aref lines i)))))
                                         do (let ((v (ignore-errors
                                                      (parse-integer
                                                       (string-trim " " (aref lines i))
                                                       :radix 16))))
                                              (when v (push v rows)))
                                            (incf i)))
                                  (t (incf i)))))
                     (when (and code (>= code 0) bbx)
                       (setf (gethash code glyphs)
                             (make-glyph :dwidth dw :bbw (first bbx) :bbh (second bbx)
                                         :bxoff (third bbx) :byoff (fourth bbx)
                                         :rows (coerce (nreverse rows) 'vector)))
                       (when (and dw (<= #x20 code #x7e)) (push dw dwidths)))
                     (incf i)))))
    ;; the cell width is the advance the ASCII range agrees on — the commonest
    ;; DWIDTH — falling back to the bounding box if the font said nothing useful
    (let ((cell-w (if dwidths
                      (let ((counts (make-hash-table)) (best nil) (best-n 0))
                        (dolist (d dwidths) (incf (gethash d counts 0)))
                        (maphash (lambda (d c) (when (> c best-n) (setf best d best-n c)))
                                 counts)
                        best)
                      fbb-w)))
      (values cell-w fbb-h (+ fbb-h fbb-yoff) glyphs))))

(defun glyph-cell (cell-w cell-h ascent g)
  "Render G into a CELL-H x CELL-W grid of 0/1, baseline-aligned — an array of rows,
   each row a list of bits left to right."
  (let ((cell (make-array (list cell-h cell-w) :element-type 'bit :initial-element 0)))
    (when g
      (let* ((bbw (glyph-bbw g)) (bbh (glyph-bbh g))
             (rows (glyph-rows g))
             (top (- ascent (+ bbh (glyph-byoff g))))
             ;; BDF pads each bitmap row out to a whole number of bytes, so the
             ;; leftmost pixel is at bit (bits-1), not (bbw-1)
             (bits (* 8 (ceiling bbw 8))))
        (dotimes (r (min bbh (length rows)))
          (let ((val (aref rows r)))
            (dotimes (c bbw)
              (when (logbitp (- bits 1 c) val)
                (let ((y (+ top r)) (x (+ (glyph-bxoff g) c)))
                  (when (and (<= 0 y) (< y cell-h) (<= 0 x) (< x cell-w))
                    (setf (aref cell y x) 1)))))))))
    cell))

(defun main (bdf out)
  (multiple-value-bind (cell-w cell-h ascent glyphs) (parse-bdf bdf)
    (when (> cell-w 8)
      (format *error-output*
              "~&width ~d > 8: the 1-byte-per-row blitter can't render it yet ~
               (see doc/fonts.org §7 Path B).~%" cell-w)
      (return-from main 1))
    (let ((bytes (make-array (+ 4 (* 256 cell-h))
                             :element-type '(unsigned-byte 8) :initial-element 0))
          (present 0))
      (setf (aref bytes 0) #x4C (aref bytes 1) #x46      ; "LF"
            (aref bytes 2) cell-w (aref bytes 3) cell-h)
      (dotimes (cp 256)
        (let ((g (gethash cp glyphs)))
          (when g (incf present))
          (let ((cell (glyph-cell cell-w cell-h ascent g)))
            (dotimes (y cell-h)
              (let ((b 0))
                (dotimes (x cell-w)                      ; pack: MSB = leftmost pixel
                  (when (= 1 (aref cell y x)) (setf b (logior b (ash 1 (- 7 x))))))
                (setf (aref bytes (+ 4 (* cp cell-h) y)) b))))))
      (with-open-file (s out :direction :output :element-type '(unsigned-byte 8)
                             :if-exists :supersede :if-does-not-exist :create)
        (write-sequence bytes s))
      (format t "~&wrote ~a: ~dx~d, ~d/256 codepoints, ~d bytes~%"
              out cell-w cell-h present (length bytes))
      0)))

(sb-ext:exit
 :code (handler-case
           (destructuring-bind (&optional bdf out) (rest sb-ext:*posix-argv*)
             (if (and bdf out)
                 (main bdf out)
                 (progn (format *error-output*
                                "~&usage: sbcl --script poc/bdf2font.lisp <in.bdf> <out.lolf>~%")
                        2)))
         (serious-condition (c)
           (format *error-output* "~&bdf2font: ~a~%" c)
           1)))
