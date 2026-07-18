;;;; textview.lisp — an editable text view: caret, selection, syntax, scrolling.
;;;;
;;;; This is the piece doc/code-browser.org §9 flags as the riskiest, because a
;;;; character grid gives you all of it for free and pixels give you none of it.
;;;; So it is worth being precise about what is actually hard here:
;;;;
;;;;   - Hit-testing (pixel -> line,column) is EASY while the font is monospaced:
;;;;     col = round((px - x0) / cell-width). With a proportional font it becomes
;;;;     a per-glyph width scan, which is more code but not more thinking. The
;;;;     structure below is already written that way (see COL-AT-X / X-OF-COL) so
;;;;     swapping in proportional metrics touches two functions, not the editor.
;;;;   - Selection is a pair of (line . col) positions that must be NORMALISED
;;;;     before use — the anchor can be after the point. Almost every selection
;;;;     bug is a missing normalisation.
;;;;   - Scrolling is a first visible line + "keep the caret on screen".
;;;;
;;;; The buffer is a vector of line strings, rebuilt on edit. Real editors use a
;;;; gap buffer or a rope; at the scale of one definition (tens of lines) this is
;;;; clearer and plenty fast — the same call the line editor already makes.

(defpackage :lol.textview
  (:use :cl :lol.canvas)
  (:export #:textview #:make-textview #:tv-draw #:tv-key #:tv-click #:tv-drag
           #:tv-text #:tv-set-text #:tv-lines #:tv-point-line #:tv-point-col
           #:tv-geometry #:tv-focus #:tv-line-height #:tv-hit-p))

(in-package :lol.textview)

(defstruct (textview (:conc-name tv-))
  (lines (vector ""))          ; simple-vector of strings, one per line
  (point-line 0) (point-col 0)
  (anchor-line 0) (anchor-col 0)   ; selection anchor; = point means no selection
  (scroll 0)                       ; first visible line
  (x 0) (y 0) (w 0) (h 0)          ; the rectangle we own, in pixels
  (pad 8)
  font
  (focus nil))

(defun tv-line-height (tv) (+ (font-ch (tv-font tv)) 3))
(defun tv-cw (tv) (font-cw (tv-font tv)))
(defun visible-lines (tv) (max 1 (floor (- (tv-h tv) (* 2 (tv-pad tv)))
                                        (tv-line-height tv))))

(defun tv-geometry (tv x y w h)
  (setf (tv-x tv) x (tv-y tv) y (tv-w tv) w (tv-h tv) h))

(defun tv-text (tv)
  (with-output-to-string (s)
    (loop for i from 0 below (length (tv-lines tv))
          do (when (plusp i) (terpri s))
             (write-string (aref (tv-lines tv) i) s))))

(defun tv-set-text (tv text)
  (let ((lines '()) (start 0))
    (loop for nl = (position #\Newline text :start start)
          do (push (subseq text start nl) lines)
             (if nl (setf start (1+ nl)) (return)))
    (setf (tv-lines tv) (coerce (nreverse lines) 'simple-vector)
          (tv-point-line tv) 0 (tv-point-col tv) 0
          (tv-anchor-line tv) 0 (tv-anchor-col tv) 0
          (tv-scroll tv) 0)))

(defun line-at (tv i) (aref (tv-lines tv) (min i (1- (length (tv-lines tv))))))

;;; ---- selection -----------------------------------------------------------

(defun selection-p (tv)
  (not (and (= (tv-point-line tv) (tv-anchor-line tv))
            (= (tv-point-col tv) (tv-anchor-col tv)))))

(defun selection-range (tv)
  "(values l0 c0 l1 c1) with (l0,c0) <= (l1,c1). NORMALISED — the anchor may well
   be after the point, and forgetting that is the classic selection bug."
  (let ((pl (tv-point-line tv)) (pc (tv-point-col tv))
        (al (tv-anchor-line tv)) (ac (tv-anchor-col tv)))
    (if (or (< al pl) (and (= al pl) (<= ac pc)))
        (values al ac pl pc)
        (values pl pc al ac))))

(defun collapse (tv)
  (setf (tv-anchor-line tv) (tv-point-line tv)
        (tv-anchor-col tv)  (tv-point-col tv)))

(defun delete-selection (tv)
  (when (selection-p tv)
    (multiple-value-bind (l0 c0 l1 c1) (selection-range tv)
      (let* ((lines (tv-lines tv))
             (head (subseq (aref lines l0) 0 c0))
             (tail (subseq (aref lines l1) (min c1 (length (aref lines l1)))))
             (new  (concatenate 'simple-vector
                                (subseq lines 0 l0)
                                (vector (concatenate 'string head tail))
                                (subseq lines (1+ l1)))))
        (setf (tv-lines tv) new
              (tv-point-line tv) l0 (tv-point-col tv) c0)
        (collapse tv)))))

;;; ---- editing -------------------------------------------------------------

(defun replace-line (tv i string)
  (let ((new (copy-seq (tv-lines tv))))
    (setf (aref new i) string (tv-lines tv) new)))

(defun insert-char (tv ch)
  (delete-selection tv)
  (let* ((l (tv-point-line tv)) (c (tv-point-col tv)) (s (line-at tv l)))
    (replace-line tv l (concatenate 'string (subseq s 0 c) (string ch) (subseq s c)))
    (incf (tv-point-col tv))
    (collapse tv)))

(defun insert-newline (tv)
  (delete-selection tv)
  (let* ((l (tv-point-line tv)) (c (tv-point-col tv)) (s (line-at tv l))
         ;; auto-indent: carry the current line's leading whitespace. Cheap, and
         ;; it is most of what makes typing Lisp bearable.
         (indent (or (position-if-not (lambda (x) (member x '(#\Space #\Tab))) s)
                     (length s)))
         (pad (make-string (min indent c) :initial-element #\Space))
         (lines (tv-lines tv)))
    (setf (tv-lines tv)
          (concatenate 'simple-vector
                       (subseq lines 0 l)
                       (vector (subseq s 0 c)
                               (concatenate 'string pad (subseq s c)))
                       (subseq lines (1+ l))))
    (setf (tv-point-line tv) (1+ l)
          (tv-point-col tv) (length pad))
    (collapse tv)))

(defun backspace (tv)
  (if (selection-p tv)
      (delete-selection tv)
      (let ((l (tv-point-line tv)) (c (tv-point-col tv)))
        (cond
          ((> c 0)
           (let ((s (line-at tv l)))
             (replace-line tv l (concatenate 'string (subseq s 0 (1- c)) (subseq s c)))
             (decf (tv-point-col tv))))
          ((> l 0)                            ; join with the previous line
           (let* ((prev (line-at tv (1- l))) (cur (line-at tv l))
                  (lines (tv-lines tv)))
             (setf (tv-lines tv)
                   (concatenate 'simple-vector
                                (subseq lines 0 (1- l))
                                (vector (concatenate 'string prev cur))
                                (subseq lines (1+ l)))
                   (tv-point-line tv) (1- l)
                   (tv-point-col tv) (length prev))))))
      )
  (collapse tv))

(defun delete-forward (tv)
  (if (selection-p tv)
      (delete-selection tv)
      (let* ((l (tv-point-line tv)) (c (tv-point-col tv)) (s (line-at tv l)))
        (cond
          ((< c (length s))
           (replace-line tv l (concatenate 'string (subseq s 0 c) (subseq s (1+ c)))))
          ((< l (1- (length (tv-lines tv))))
           (let ((lines (tv-lines tv)))
             (setf (tv-lines tv)
                   (concatenate 'simple-vector
                                (subseq lines 0 l)
                                (vector (concatenate 'string s (aref lines (1+ l))))
                                (subseq lines (+ l 2))))))))))

(defun kill-to-eol (tv)
  (let* ((l (tv-point-line tv)) (c (tv-point-col tv)) (s (line-at tv l)))
    (if (< c (length s))
        (replace-line tv l (subseq s 0 c))
        (delete-forward tv)))
  (collapse tv))

;;; ---- movement ------------------------------------------------------------

(defun clamp-point (tv)
  (setf (tv-point-line tv)
        (max 0 (min (tv-point-line tv) (1- (length (tv-lines tv))))))
  (setf (tv-point-col tv)
        (max 0 (min (tv-point-col tv) (length (line-at tv (tv-point-line tv)))))))

(defun ensure-visible (tv)
  (let ((n (visible-lines tv)))
    (cond ((< (tv-point-line tv) (tv-scroll tv))
           (setf (tv-scroll tv) (tv-point-line tv)))
          ((>= (tv-point-line tv) (+ (tv-scroll tv) n))
           (setf (tv-scroll tv) (- (tv-point-line tv) n -1))))
    (setf (tv-scroll tv) (max 0 (tv-scroll tv)))))

(defun move (tv where extend)
  (case where
    (:left  (if (> (tv-point-col tv) 0)
                (decf (tv-point-col tv))
                (when (> (tv-point-line tv) 0)
                  (decf (tv-point-line tv))
                  (setf (tv-point-col tv) (length (line-at tv (tv-point-line tv)))))))
    (:right (if (< (tv-point-col tv) (length (line-at tv (tv-point-line tv))))
                (incf (tv-point-col tv))
                (when (< (tv-point-line tv) (1- (length (tv-lines tv))))
                  (incf (tv-point-line tv))
                  (setf (tv-point-col tv) 0))))
    (:up    (decf (tv-point-line tv)))
    (:down  (incf (tv-point-line tv)))
    (:home  (setf (tv-point-col tv) 0))
    (:end   (setf (tv-point-col tv) (length (line-at tv (tv-point-line tv))))))
  (clamp-point tv)
  (unless extend (collapse tv))
  (ensure-visible tv))

;;; ---- hit testing: pixels -> (line, col) ----------------------------------
;;; Monospaced today. These two functions are the ONLY place that assumes it —
;;; proportional metrics replace their bodies and nothing else changes.

(defun x-of-col (tv col) (+ (tv-x tv) (tv-pad tv) (* col (tv-cw tv))))

(defun col-at-x (tv line px)
  (let* ((rel (- px (tv-x tv) (tv-pad tv)))
         (col (round rel (tv-cw tv))))          ; ROUND, not floor: clicking the
    (max 0 (min col (length (line-at tv line)))))) ; right half of a glyph should
                                                   ; put the caret after it

(defun line-at-y (tv py)
  (let ((row (floor (- py (tv-y tv) (tv-pad tv)) (tv-line-height tv))))
    (max 0 (min (+ (tv-scroll tv) row) (1- (length (tv-lines tv)))))))

(defun tv-hit-p (tv px py)
  "True when PX,PY lands inside the view's own rectangle. Callers gate clicks on
   this so the chrome above (tab strip) and below (status bar) don't move the
   caret — the view is the authority on its own bounds, not the caller."
  (and (<= (tv-x tv) px (+ (tv-x tv) (tv-w tv)))
       (<= (tv-y tv) py (+ (tv-y tv) (tv-h tv)))))

(defun tv-click (tv px py)
  (let* ((l (line-at-y tv py)) (c (col-at-x tv l px)))
    (setf (tv-point-line tv) l (tv-point-col tv) c)
    (collapse tv)                     ; a fresh click starts a new selection
    (ensure-visible tv)))

(defun tv-drag (tv px py)
  "Extend the selection to the pointer — the anchor stays where the click began."
  (let* ((l (line-at-y tv py)) (c (col-at-x tv l px)))
    (setf (tv-point-line tv) l (tv-point-col tv) c)
    (ensure-visible tv)))

;;; ---- the lexer -----------------------------------------------------------
;;; One left-to-right pass producing coloured runs. Same shape as colorize-lisp
;;; in line-editor.lisp, but it returns RUNS instead of writing escape codes —
;;; which is precisely the refactor the plan calls for (the model must not know
;;; how it is rendered). Paren depth is carried ACROSS lines, so the rainbow is
;;; correct in a multi-line definition.

(defparameter *c-text*    (rgb #xd6 #xd3 #xc8))
(defparameter *c-string*  (rgb #xa8 #xd0 #x78))
(defparameter *c-comment* (rgb #x5a #x63 #x55))
(defparameter *c-keyword* (rgb #xc7 #x92 #xea))
(defparameter *c-number*  (rgb #xff #x9f #x43))
(defparameter *paren-cycle*
  (vector (rgb #xff #xcb #x6b) (rgb #xc7 #x92 #xea)
          (rgb #x7f #xdb #xca) (rgb #xa8 #xff #x78)))

(defun delimiterp (c)
  (member c '(#\Space #\Tab #\( #\) #\' #\" #\; #\` #\,)))

(defun lex-line (line depth)
  "Return (values runs new-depth), runs = list of (start end . color)."
  (let ((runs '()) (n (length line)) (i 0))
    (flet ((emit (s e c) (when (< s e) (push (list* s e c) runs))))
      (loop while (< i n) do
        (let ((c (char line i)))
          (cond
            ((char= c #\;)                              ; comment to end of line
             (emit i n *c-comment*) (setf i n))
            ((char= c #\")                              ; string
             (let ((j (1+ i)))
               (loop while (< j n) do
                 (cond ((and (char= (char line j) #\\) (< (1+ j) n)) (incf j 2))
                       ((char= (char line j) #\") (incf j) (return))
                       (t (incf j))))
               (emit i (min j n) *c-string*) (setf i j)))
            ((char= c #\()
             (emit i (1+ i) (aref *paren-cycle* (mod depth 4)))
             (incf depth) (incf i))
            ((char= c #\))
             (setf depth (max 0 (1- depth)))
             (emit i (1+ i) (aref *paren-cycle* (mod depth 4)))
             (incf i))
            ((and (char= c #\:) (or (zerop i) (delimiterp (char line (1- i)))))
             (let ((j (1+ i)))
               (loop while (and (< j n) (not (delimiterp (char line j)))) do (incf j))
               (emit i j *c-keyword*) (setf i j)))
            ((and (digit-char-p c) (or (zerop i) (delimiterp (char line (1- i)))))
             (let ((j i))
               (loop while (and (< j n) (not (delimiterp (char line j)))) do (incf j))
               (emit i j *c-number*) (setf i j)))
            (t
             (let ((j i))
               (loop while (and (< j n)
                                (not (member (char line j) '(#\( #\) #\" #\;))))
                     do (incf j))
               (emit i (max j (1+ i)) *c-text*)
               (setf i (max j (1+ i)))))))))
    (values (nreverse runs) depth)))

;;; ---- drawing -------------------------------------------------------------

(defparameter *bg*      (rgb #x1b #x1e #x24))
(defparameter *gutter*  (rgb #x4a #x52 #x4d))
(defparameter *sel*     (rgb #x2f #x4f #x6a))
(defparameter *caret*   (rgb #x7f #xdb #xca))
(defparameter *cur-ln*  (rgb #x21 #x25 #x2c))
(defparameter *scroll*  (rgb #x39 #x40 #x4a))

(defun tv-draw (canvas tv &key (gutter 40))
  (let* ((font (tv-font tv))
         (lh (tv-line-height tv))
         (x0 (tv-x tv)) (y0 (tv-y tv)) (w (tv-w tv)) (h (tv-h tv))
         (pad (tv-pad tv))
         (first (tv-scroll tv))
         (last  (min (length (tv-lines tv)) (+ first (visible-lines tv))))
         (depth 0))
    (fill-rect canvas x0 y0 w h *bg*)
    ;; paren depth must be correct at the first VISIBLE line, so lex the hidden
    ;; prefix for its depth only. (A real editor caches this per line.)
    (dotimes (i first)
      (setf depth (nth-value 1 (lex-line (aref (tv-lines tv) i) depth))))

    (multiple-value-bind (sl0 sc0 sl1 sc1) (selection-range tv)
      (loop for li from first below last
            for row from 0
            for ly = (+ y0 pad (* row lh))
            do (let ((line (aref (tv-lines tv) li)))
                 ;; current-line highlight
                 (when (and (= li (tv-point-line tv)) (not (selection-p tv)))
                   (fill-rect canvas (+ x0 1) (- ly 2) (- w 2) lh *cur-ln*))
                 ;; selection band for this line
                 (when (and (selection-p tv) (<= sl0 li) (<= li sl1))
                   (let* ((a (if (= li sl0) sc0 0))
                          (b (if (= li sl1) sc1 (length line)))
                          (xa (+ (x-of-col tv a) gutter))
                          (xb (+ (x-of-col tv b) gutter)))
                     ;; a selected newline shows as a sliver past end-of-line
                     (fill-rect canvas xa (- ly 2)
                                (max 4 (- xb xa)) lh *sel*)))
                 ;; gutter line number
                 (draw-string canvas font (format nil "~3d" (1+ li))
                              (+ x0 pad) ly *gutter*)
                 ;; the text, run by coloured run
                 (multiple-value-bind (runs new-depth) (lex-line line depth)
                   (setf depth new-depth)
                   (dolist (r runs)
                     (destructuring-bind (s e . color) r
                       (draw-string canvas font (subseq line s e)
                                    (+ (x-of-col tv s) gutter) ly color))))
                 ;; the caret
                 (when (and (tv-focus tv) (= li (tv-point-line tv)))
                   (fill-rect canvas
                              (+ (x-of-col tv (tv-point-col tv)) gutter) (- ly 2)
                              2 lh *caret*)))))

    ;; scrollbar — only when there is something to scroll
    (let ((total (length (tv-lines tv))) (vis (visible-lines tv)))
      (when (> total vis)
        (let* ((track-h (- h 4))
               (thumb-h (max 20 (floor (* track-h vis) total)))
               (thumb-y (+ y0 2 (floor (* track-h first) total))))
          (fill-rect canvas (+ x0 w -6) thumb-y 4 thumb-h *scroll*))))))

;;; ---- key handling --------------------------------------------------------

(defun tv-key (tv key state)
  "KEY is a character or a keyword (see lol.x11:decode-key). Returns :accept when
   the user asks to compile the buffer, else NIL."
  (let ((shift (lol.x11:shift-p state))
        (ctrl  (lol.x11:ctrl-p state)))
    (cond
      ;; ---- Ctrl chords ----
      ((and ctrl (characterp key))
       (case (char-downcase key)
         (#\a (move tv :home shift))
         (#\e (move tv :end shift))
         (#\b (move tv :left shift))
         (#\f (move tv :right shift))
         (#\p (move tv :up shift))
         (#\n (move tv :down shift))
         (#\k (kill-to-eol tv))
         (#\d (delete-forward tv))
         (t nil))
       nil)
      ((and ctrl (eq key :return)) :accept)     ; Ctrl-Enter = Accept
      ;; ---- named keys ----
      ((eq key :left)      (move tv :left shift)  nil)
      ((eq key :right)     (move tv :right shift) nil)
      ((eq key :up)        (move tv :up shift)    nil)
      ((eq key :down)      (move tv :down shift)  nil)
      ((eq key :home)      (move tv :home shift)  nil)
      ((eq key :end)       (move tv :end shift)   nil)
      ((eq key :backspace) (backspace tv) (ensure-visible tv) nil)
      ((eq key :delete)    (delete-forward tv) nil)
      ((eq key :return)    (insert-newline tv) (ensure-visible tv) nil)
      ((eq key :tab)       (dotimes (i 2) (insert-char tv #\Space)) nil)
      ;; ---- ordinary text ----
      ((and (characterp key) (graphic-char-p key))
       (insert-char tv key) (ensure-visible tv) nil)
      (t nil))))
