;;;; clipboard-test.lisp — host test for poc/clipboard.lisp.
;;;;
;;;;   sbcl --script poc/clipboard-test.lisp
;;;;
;;;; Prints PASS/FAIL per assertion and exits non-zero if any failed. Needs nothing
;;;; but a stock SBCL: the clipboard is pure data, which is the point of keeping
;;;; it out of textview.lisp. Every case builds a FRESH board, so no case can pass
;;;; only because an earlier one left state behind.

(load (merge-pathnames "clipboard.lisp" (or *load-truename* *default-pathname-defaults*)))

(defpackage :lol.clipboard-test (:use :cl :lol.clipboard))
(in-package :lol.clipboard-test)

(defvar *failures* 0)
(defvar *count* 0)

(defun check (name got want)
  "One assertion, compared with EQUALP (strings, lists and vectors by content)."
  (incf *count*)
  (if (equalp got want)
      (format t "PASS  ~a~%" name)
      (progn (incf *failures*)
             (format t "FAIL  ~a~%      got:  ~s~%      want: ~s~%" name got want))))

(defmacro with-board ((var &rest args) &body body)
  `(let ((,var (make-clipboard ,@args))) ,@body))

(defun v (&rest lines) (coerce lines 'simple-vector))

;;; ---- history ---------------------------------------------------------------

(with-board (b)
  (check "blank refused: empty"      (multiple-value-list (clip-copy "" :board b)) '(nil :blank))
  (check "blank refused: whitespace" (multiple-value-list (clip-copy (format nil "  ~%	 ") :board b))
         '(nil :blank))
  (check "blank refusal stores nothing" (clip-count :board b) 0)
  (check "real text accepted" (clip-copy "  x " :board b) "  x ")
  (check "whitespace kept inside an accepted entry" (clip-current :board b) "  x "))

(with-board (b :max-length 5)
  (check "too-long refused" (multiple-value-list (clip-copy "123456" :board b)) '(nil :too-long))
  (check "at the cap accepted" (clip-copy "12345" :board b) "12345"))

(with-board (b)
  (clip-copy "a" :board b) (clip-copy "b" :board b) (clip-copy "c" :board b)
  (check "newest first" (clip-history :board b) '("c" "b" "a"))
  (clip-copy "a" :board b)
  (check "dedup moves to front" (clip-history :board b) '("a" "c" "b"))
  (check "dedup does not grow" (clip-count :board b) 3)
  (check "clip-nth" (clip-nth 1 :board b) "c")
  (check "clip-nth out of range" (clip-nth 9 :board b) nil))

(with-board (b :limit 3)
  (dolist (s '("1" "2" "3" "4" "5")) (clip-copy s :board b))
  (check "bounded to limit, oldest dropped" (clip-history :board b) '("5" "4" "3")))

(with-board (b)
  (clip-copy "older" :board b)
  (clip-copy "foo" :board b)
  (clip-copy (string #\Newline) :append t :board b)
  (clip-copy "bar" :append t :board b)
  (check "C-k C-k C-k accumulates into one entry"
         (clip-history :board b) (list (format nil "foo~%bar") "older"))
  (clip-copy "x " :append :before :board b)
  (check "append :before prepends (backward kill)"
         (clip-current :board b) (format nil "x foo~%bar")))

(with-board (b)
  (clip-copy "older" :board b)
  ;; C-k at end of line first: the newline alone is blank and refused...
  (check "lone newline kill refused" (clip-copy (string #\Newline) :board b) nil)
  ;; ...but the next consecutive kill picks it up instead of appending to "older".
  (clip-copy "next" :append t :board b)
  (check "refused fragment carried into the sequence"
         (clip-history :board b) (list (format nil "~%next") "older")))

(with-board (b)
  (clip-copy "one" :board b) (clip-copy "two" :board b)
  (clip-promote 1 :board b)
  (check "promote makes entry current" (clip-history :board b) '("one" "two"))
  (clip-copy "more" :append t :board b)
  (check "append after promote does not grow the promoted entry"
         (clip-history :board b) '("more" "one" "two")))

(with-board (b)
  (dolist (s '("a" "b" "c")) (clip-copy s :board b))
  (check "delete returns victim" (clip-delete 1 :board b) "b")
  (check "delete removes" (clip-history :board b) '("c" "a"))
  (check "promote bad index" (clip-promote 7 :board b) nil)
  (set-primary "p" :board b)
  (clip-clear :board b)
  (check "clear empties history" (clip-history :board b) '())
  (check "clear keeps PRIMARY" (primary :board b) "p"))

(with-board (b)
  (dolist (s '("a" "b" "c")) (clip-copy s :board b))
  (check "yank = newest" (clip-yank :board b) "c")
  (check "yank-pop 1" (clip-yank-pop :board b) "b")
  (check "yank-pop 2" (clip-yank-pop :board b) "a")
  (check "yank-pop wraps" (clip-yank-pop :board b) "c")
  (check "yank-pop -1 goes newer" (clip-yank-pop :n -1 :board b) "a")
  (check "yank after pop reads the pointer" (clip-yank :board b) "a")
  (clip-copy "d" :board b)
  (check "new copy resets the pointer" (clip-yank :board b) "d"))

(with-board (b)
  (dolist (s '("Alpha" "beta" "ALPHABET")) (clip-copy s :board b))
  (check "filter is case-insensitive, keeps real indices"
         (clip-filter "alpha" :board b) '((0 . "ALPHABET") (2 . "Alpha")))
  (check "label flattens and cuts"
         (clip-label (format nil "(defun f ()~%    1)") 14) "(defun f ()...")
  (check "label short" (clip-label (format nil "a~%b") 20) "a\\nb"))

(with-board (b)
  (check "select sets PRIMARY and history" (clip-select "sel" :board b) "sel")
  (check "PRIMARY" (primary :board b) "sel")
  (check "select reached history" (clip-current :board b) "sel")
  (clip-select "   " :board b)
  (check "blank selection still sets PRIMARY" (primary :board b) "   ")
  (check "...but not the history" (clip-history :board b) '("sel")))

;;; ---- plain text over a lines vector ----------------------------------------

(let ((lines (v "hello" "big" "world")))
  (check "region-text one line" (region-text lines 0 1 0 4) "ell")
  (check "region-text reversed, multi-line"
         (region-text lines 2 2 0 3) (format nil "lo~%big~%wo"))
  (check "region-delete" (multiple-value-list (region-delete lines 2 2 0 3))
         (list (v "helrld") 0 3))
  (check "input not mutated" lines (v "hello" "big" "world"))
  (check "insert-text one line" (multiple-value-list (insert-text lines 0 5 "!"))
         (list (v "hello!" "big" "world") 0 6))
  (check "insert-text multi-line"
         (multiple-value-list (insert-text lines 1 1 (format nil "X~%Y~%Z")))
         (list (v "hello" "bX" "Y" "Zig" "world") 3 1))
  (check "kill-line-span mid-line" (multiple-value-list (kill-line-span lines 0 2))
         '(0 2 0 5))
  (check "kill-line-span at EOL takes the newline"
         (multiple-value-list (kill-line-span lines 0 5)) '(0 5 1 0))
  (check "kill-line-span at end of buffer" (kill-line-span lines 2 5) nil))

(let ((lines (v "foo   " "bar  ")))
  (check "kill-line-span: only blanks left -> through the newline"
         (multiple-value-list (kill-line-span lines 0 3)) (quote (0 3 1 0)))
  (check "kill-line-span: blanks on the last line -> just the blanks"
         (multiple-value-list (kill-line-span lines 1 3)) (quote (1 3 1 5))))

;;; ---- rectangles: a ragged buffer, corners given in reverse -----------------
;;;
;;;   col: 0123456789
;;;   0    abcdefgh
;;;   1    ab
;;;   2    abcd
;;;   3    abcdefghij
;;; Rectangle: lines 0..3, columns 2..5 — given as (3 5) -> (0 2) to test
;;; normalisation. Line 1 ends AT the left edge, line 2 ends INSIDE the block.

(let ((lines (v "abcdefgh" "ab" "abcd" "abcdefghij")))
  (check "extract pads to full width (reversed corners)"
         (rect-extract lines 3 5 0 2) '("cde" "   " "cd " "cde"))
  (check "extract: mixed corners (top-right, bottom-left)"
         (rect-extract lines 0 5 3 2) '("cde" "   " "cd " "cde"))
  (check "delete" (rect-delete lines 3 5 0 2) (v "abfgh" "ab" "ab" "abfghij"))
  (check "kill = delete + extract"
         (multiple-value-list (rect-kill lines 3 5 0 2))
         (list (v "abfgh" "ab" "ab" "abfghij") '("cde" "   " "cd " "cde")))
  (check "open leaves lines with nothing to push"
         (rect-open lines 3 5 0 2) (v "ab   cdefgh" "ab" "ab   cd" "ab   cdefghij"))
  (check "clear: blank in place, cut a line ending inside"
         (rect-clear lines 3 5 0 2) (v "ab   fgh" "ab" "ab" "ab   fghij"))
  (check "string replaces the span, pads short lines"
         (rect-string lines 3 5 0 2 "XY") (v "abXYfgh" "abXY" "abXY" "abXYfghij"))
  (check "string on a zero-width rectangle inserts down a column"
         (rect-string lines 0 0 2 0 ";; ") (v ";; abcdefgh" ";; ab" ";; abcd" "abcdefghij"))
  (check "rectangle ops never mutate the input"
         lines (v "abcdefgh" "ab" "abcd" "abcdefghij")))

(let ((lines (v "abcdef" "a" "abcdef")))
  (check "yank pads short lines and returns bottom-right"
         (multiple-value-list (rect-yank lines '("XX" "YY" "ZZ") 0 3))
         (list (v "abcXXdef" "a  YY" "abcZZdef") 2 5))
  (check "yank past the end adds lines"
         (multiple-value-list (rect-yank lines '("11" "22") 2 1))
         (list (v "abcdef" "a" "a11bcdef" " 22") 3 3)))

(with-board (b)
  (multiple-value-bind (new rect) (rect-kill (v "abcd" "efgh") 0 1 1 3)
    (clip-set-rectangle rect :board b)
    (check "rectangle register is separate from history"
           (list (clip-rectangle :board b) (clip-history :board b))
           '(("bc" "fg") ()))
    (check "kill then yank back at the same corner restores the text"
           (rect-yank new (clip-rectangle :board b) 0 1)
           (values (v "abcd" "efgh")))))

;;; ---- verdict -----------------------------------------------------------------

(format t "~%~d/~d passed~%" (- *count* *failures*) *count*)
(sb-ext:exit :code (if (zerop *failures*) 0 1))
