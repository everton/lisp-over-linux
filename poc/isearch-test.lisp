;;;; isearch-test.lisp — host test for the text view's incremental search (C-s / C-r).
;;;;
;;;;     sbcl --script poc/isearch-test.lisp        ; exits 0 if all pass
;;;;
;;;; Drives lol.textview the way shell.lisp's BROWSER-KEY does: C-s / C-r start a search
;;;; (TV-ISEARCH-START), every key while one runs goes to TV-ISEARCH-KEY, and a key it
;;;; answers :EXIT for is then handed to TV-KEY as if no search had run.
;;;;
;;;; The expected points are EMACS'S. Each case was run in `emacs -Q --batch' (Emacs 30)
;;;; with `execute-kbd-macro' on a temp buffer holding the same text, and the point and
;;;; mark it printed are written here as 0-based columns (Emacs counts from 1). To
;;;; re-derive one: (with-temp-buffer (switch-to-buffer (current-buffer)) (insert TEXT)
;;;; (goto-char START) (execute-kbd-macro (kbd KEYS)) (message "%d %S" (point) (mark t))).

(let ((dir (make-pathname :directory (pathname-directory *load-truename*))))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "ttf.lisp" dir))
    (load (merge-pathnames "canvas.lisp" dir))
    (load (merge-pathnames "textview.lisp" dir))))

(defpackage :lol.isearch-test (:use :cl :lol.textview))
(in-package :lol.isearch-test)

(defvar *fails* 0)
(defvar *count* 0)
(defun check (name ok)
  (incf *count*)
  (format t "~&  ~66a ~:[FAIL~;PASS~]~%" name ok)
  (unless ok (incf *fails*)))

(defvar *font* (lol.canvas:load-font (merge-pathnames "cozette.lolf"
                                                       (make-pathname :directory (pathname-directory *load-truename*)))))

(defun fresh (text &optional (line 0) (col 0))
  (let ((tv (lol.textview::make-textview :font *font*)))
    (tv-geometry tv 0 0 800 600)
    (tv-set-text tv text)
    (tv-move-point tv line col)
    tv))

(defun key-of (tok)
  "An Emacs `kbd' token -> (values KEY STATE). A token that is not a key name is typed."
  (cond ((string= tok "RET") (values :return 0))
        ((string= tok "DEL") (values :backspace 0))
        ((string= tok "SPC") (values #\Space 0))
        ((and (> (length tok) 2) (string= "C-" tok :end2 2))
         (values (char tok 2) 4))
        (t nil)))

(defun press (tv k state)
  "One key, routed as BROWSER-KEY routes it."
  (if (tv-isearch tv)
      (when (eq (tv-isearch-key tv k state) :exit) (tv-key tv k state))
      (if (and (= state 4) (member k '(#\s #\r)))
          (tv-isearch-start tv (eql k #\s))
          (tv-key tv k state))))

(defun run (tv keys)
  "Press the space-separated `kbd' KEYS on TV; a word that is no key name is typed."
  (let ((start 0))
    (loop for sp = (position #\Space keys :start start)
          for tok = (subseq keys start sp)
          do (multiple-value-bind (k state) (key-of tok)
               (if k
                   (press tv k state)
                   (loop for ch across tok do (press tv ch 0))))
             (if sp (setf start (1+ sp)) (return))))
  tv)

(defun col (tv) (tv-point-col tv))
(defun pos (tv) (cons (tv-point-line tv) (tv-point-col tv)))

(defparameter *a* "foo bar foo baz foo")

(defun emacs (name text start keys want-col &optional (want-mark :any) want-string)
  "Run KEYS from column START of the one-line TEXT; compare with Emacs's point (and its
   mark, and the last search string, when given)."
  (let ((tv (fresh text 0 start)))
    (run tv keys)
    (check (format nil "~a: ~a -> ~d" name keys want-col)
           (and (= (col tv) want-col)
                (or (eq want-mark :any)
                    (equal (tv-mark tv) (and want-mark (cons 0 want-mark))))
                (or (null want-string) (equal *isearch-last* want-string))))
    tv))

(format t "~&== against Emacs: forward, repeat, wrap ==~%")
(setf *isearch-last* nil)
(emacs "a match" *a* 0 "C-s foo RET" 3 0 "foo")
(emacs "the next" *a* 0 "C-s foo C-s RET" 11 0)
(emacs "the last" *a* 0 "C-s foo C-s C-s RET" 19 0)
(emacs "failing stays" *a* 0 "C-s foo C-s C-s C-s RET" 19 0)
(emacs "then wraps" *a* 0 "C-s foo C-s C-s C-s C-s RET" 3 0)
(emacs "wrapped, on" *a* 0 "C-s foo C-s C-s C-s C-s C-s RET" 11 0)
(emacs "overwrapped" *a* 0 "C-s foo C-s C-s C-s C-s C-s C-s RET" 19 0)
(emacs "a longer string stays on its match" *a* 0 "C-s fo C-s o RET" 11 0)
(emacs "a string with no match anywhere, wrapped" *a* 0 "C-s z C-s C-s RET" 15 0)
(emacs "from mid-buffer" *a* 8 "C-s foo C-s C-s RET" 19 8)
(emacs "from mid-buffer, wraps before it" *a* 8 "C-s foo C-s C-s C-s RET" 3 8)
(emacs "one letter, wrapped" "aaa" 0 "C-s a C-s C-s C-s C-s RET" 1 0)
(emacs "only match: repeats fail" "abc" 0 "C-s abc C-s C-s RET" 3 0)

(format t "~&== against Emacs: backward, and turning round ==~%")
(emacs "backward" *a* 19 "C-r foo RET" 16 19)
(emacs "backward, stays as it grows" *a* 19 "C-r fo o RET" 16 19)
(emacs "backward, ba" *a* 19 "C-r ba RET" 12 19)
(emacs "backward, grows past its match" *a* 19 "C-r ba r RET" 4 19)
(emacs "C-r after C-s turns round on the match" *a* 0 "C-s foo C-s C-r RET" 8 0)
(emacs "and then goes back" *a* 0 "C-s foo C-s C-r C-r RET" 0 nil)
(emacs "turning round at the first match: no mark" *a* 0 "C-s foo C-r RET" 0 nil)
(emacs "abcab: C-s ab C-r" "abcab" 0 "C-s ab C-r RET" 0 nil)
(emacs "abcab: C-s ab C-s C-r" "abcab" 0 "C-s ab C-s C-r RET" 3 0)
(emacs "abcab: and forward again" "abcab" 0 "C-s ab C-s C-r C-s RET" 5 0)

(format t "~&== against Emacs: DEL undoes a step, C-g ==~%")
(emacs "DEL after C-s: the previous match" *a* 0 "C-s foo C-s DEL RET" 3 0)
(emacs "DEL again: a character" *a* 0 "C-s foo C-s DEL DEL RET" 2 0 "fo")
(emacs "DEL steps back across matches" "abc abc" 0 "C-s ab C-s DEL DEL RET" 1 0 "a")
(emacs "DEL off a failing string" *a* 0 "C-s bax DEL DEL RET" 5 0 "b")
(emacs "C-g strips the failing part" *a* 0 "C-s foox C-g RET" 3 0 "foo")
(let ((tv (emacs "C-g on a match cancels: back at the start" *a* 4 "C-s foo C-g" 4 nil)))
  (check "  and the search is over" (null (tv-isearch tv))))
(let ((tv (emacs "C-g C-g on a failing string: stripped, then cancelled" *a* 0 "C-s bax C-g" 6)))
  (check "  after the first C-g it still searches \"ba\""
         (and (tv-isearch tv) (equal (nth-value 1 (tv-isearch-prompt tv)) "ba")))
  (run tv "C-g")
  (check "  the second puts point back" (and (null (tv-isearch tv)) (= (col tv) 0))))
(let ((tv (emacs "C-g after a failing C-s: back on the last match" *a* 0 "C-s foo C-s C-s C-s C-g" 19)))
  (check "  ... still searching" (tv-isearch tv)))

(format t "~&== against Emacs: case, yanks, whitespace, other keys ==~%")
(emacs "a capital makes it exact (and it fails)" "Foo bar foo baz foo" 2 "C-s F RET" 2 nil)
(emacs "a capital skips the lower-case match" "foo bar Foo baz foo" 0 "C-s F o RET" 10 0)
(emacs "lower case ignores case" "foo Foo" 1 "C-s foo RET" 7 1)
(emacs "C-w yanks the next word" *a* 0 "C-s foo C-w RET" 7 0 "foo bar")
(emacs "C-w keeps a capital the string has" "Foo Bar xx Foo bar" 0 "C-s F C-w RET" 3 0 "Foo")
(emacs "C-w lower-cases a yank otherwise" "Foo Bar xx foo bar" 0 "C-s f C-w C-w RET" 7 0 "foo bar")
(emacs "a space matches a run of blanks" "a b  c" 0 "C-s b SPC c RET" 6 0)
(emacs "C-a ends the search and moves" *a* 0 "C-s bar C-a" 0 0)
(setf *isearch-last* "foo")
(emacs "C-s C-s: the last string again" *a* 0 "C-s C-s RET" 3 0)
(setf *isearch-last* nil)
(let ((tv (emacs "C-s C-s with no last string" *a* 0 "C-s C-s" 0)))
  (check "  says so" (equal (nth-value 3 (tv-isearch-prompt tv)) "No previous search string"))
  (run tv "RET")
  (check "  and Enter leaves point and no mark" (and (= (col tv) 0) (null (tv-mark tv)))))
(emacs "a cancelled search is not remembered" *a* 0 "C-s qux C-g" 0 nil)
(check "  (no last string)" (null *isearch-last*))

(format t "~&== against Emacs: across lines ==~%")
(let ((tv (fresh (format nil "foo~%bar foo"))))
  (run tv "C-s foo C-s C-s C-s RET")
  (check "C-s foo C-s C-s C-s on two lines: wrapped to the first" (equal (pos tv) '(0 . 3))))
(let ((tv (fresh (format nil "foo~%bar foo") 1 7)))
  (run tv "C-r foo C-r C-r C-r RET")
  (check "C-r foo C-r C-r C-r from the end: wrapped to the last" (equal (pos tv) '(1 . 4)))
  (check "  the mark at the start" (equal (tv-mark tv) '(1 . 7))))

(format t "~&== the prompt ==~%")
(flet ((prompt (keys &optional (text *a*) (start 0))
         (let ((tv (fresh text 0 start))) (run tv keys) (multiple-value-list (tv-isearch-prompt tv)))))
  (check "I-search: foo" (equal (prompt "C-s foo") '("I-search: " "foo" nil nil)))
  (check "I-search backward:" (equal (first (prompt "C-r fo" *a* 19)) "I-search backward: "))
  (check "Failing I-search: (after the last match)" (equal (first (prompt "C-s foo C-s C-s C-s")) "Failing I-search: "))
  ;; Emacs says "Overwrapped" as soon as a wrapped search is past its origin — from the
  ;; buffer's start that is at once (checked: isearch-message-prefix in Emacs 30)
  (check "Overwrapped I-search: (wrapped, past the start)"
         (equal (first (prompt "C-s foo C-s C-s C-s C-s")) "Overwrapped I-search: "))
  (check "Wrapped I-search: (wrapped, not yet back at the start)"
         (equal (first (prompt "C-s foo C-s C-s C-s" *a* 8)) "Wrapped I-search: "))
  (check "  then Overwrapped" (equal (first (prompt "C-s foo C-s C-s C-s C-s" *a* 8)) "Overwrapped I-search: "))
  (check "Failing wrapped I-search backward:"
         (equal (first (prompt "C-r zz C-r")) "Failing wrapped I-search backward: "))
  (check "the failing part starts after \"foo\"" (eql (third (prompt "C-s foox")) 3))
  (check "no prompt without a search" (null (tv-isearch-prompt (fresh "x")))))

(format t "~&== what is drawn: spans, row keys, scrolling ==~%")
(let ((tv (fresh *a*)))
  (run tv "C-s foo")
  (check "spans: the current match, then the other two"
         (equal (tv-isearch-spans tv 0 *a*) '((0 3 t) (8 11 nil) (16 19 nil))))
  (run tv "C-s")
  (check "after C-s the current one moved"
         (equal (tv-isearch-spans tv 0 *a*) '((8 11 t) (0 3 nil) (16 19 nil))))
  (run tv "x")
  (check "failing: only the match point stands on" (equal (tv-isearch-spans tv 0 *a*) '((8 11 t))))
  (run tv "RET")
  (check "no spans after the search" (null (tv-isearch-spans tv 0 *a*))))
(let* ((tv (fresh (format nil "alpha~%beta~%gamma")))
       (before (progn (setf (tv-focus tv) t) (tv-row-keys tv 0 nil))))
  (run tv "C-s beta")
  (let ((after (tv-row-keys tv 0 nil)))
    (check "row keys: the matched row differs" (not (equal (aref before 1) (aref after 1))))
    (check "row keys: a row with no match is the same" (equal (aref before 2) (aref after 2)))))
(let ((tv (fresh (format nil "~{~a~^~%~}" (loop for i below 300 collect (format nil "line ~d" i))))))
  (run tv "C-s line SPC 250")
  (check "a match far down scrolls the view to it"
         (and (= (tv-point-line tv) 250) (<= (lol.textview::tv-scroll tv) 250
                                              (+ (lol.textview::tv-scroll tv) (lol.textview::visible-lines tv) -1)))))
(let ((tv (fresh (concatenate 'string (make-string 400 :initial-element #\x) "needle"))))
  (run tv "C-s needle")
  (check "a match far right scrolls sideways" (plusp (lol.textview::tv-hscroll tv)))
  (let ((c (lol.canvas:make-canvas 800 600)))
    (tv-draw c tv)
    (let* ((x (+ (lol.textview::x-of-col tv 401)))
           (y (+ (lol.textview::tv-pad tv) 2)))
      (check "the current match is painted in its colour, under the text"
             (find lol.textview::*isearch-cur*
                   (loop for dy from 0 below (tv-line-height tv)
                         collect (aref (lol.canvas:canvas-pixels c) (+ (* (+ y dy -2) 800) x))))))))

(format t "~&== the mark: C-x C-x, and a region grown by searching ==~%")
(let ((tv (fresh *a*)))
  (run tv "C-s foo C-s RET")
  (check "Enter: point on the match, mark at the start" (and (= (col tv) 11) (equal (tv-mark tv) '(0 . 0))))
  (check "  no region shows" (null (tv-region tv)))
  (check "C-x C-x goes back, the region active" (and (tv-exchange-point-and-mark tv) (= (col tv) 0)
                                                     (tv-mark-active tv)
                                                     (equal (multiple-value-list (tv-region tv)) '(0 11 0 0))))
  (check "C-x C-x again swaps the ends" (and (tv-exchange-point-and-mark tv) (= (col tv) 11))))
(let ((tv (fresh *a*)))
  (tv-key tv #\Space 4)                       ; C-SPC: the mark, motion extends
  (run tv "C-s baz RET")
  (check "C-SPC then C-s baz: the region grows to the match"
         (equal (multiple-value-list (tv-region tv)) '(0 0 0 15))))
(check "no mark: C-x C-x says so" (null (tv-exchange-point-and-mark (fresh "x"))))
(let ((tv (fresh *a*)))
  (run tv "C-s bar")
  (tv-click tv 10 10)
  (check "a click ends the search" (null (tv-isearch tv))))

(format t "~&~d checks, ~d failed~%" *count* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
