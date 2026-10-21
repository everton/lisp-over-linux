;;;; undo-test.lisp — host test for the text view's Emacs-style undo.
;;;;
;;;;     sbcl --script poc/undo-test.lisp        ; exits 0 if all pass
;;;;
;;;; Drives lol.textview the way shell.lisp's EDIT-PANE-KEY does: a checkpoint before
;;;; each key, TV-UNDO-NOTE after it, TV-UNDO for C-_. Checks the protocol, not the
;;;; key binding (that is check-clipboard.lisp's, in the guest).

(let ((dir (make-pathname :directory (pathname-directory *load-truename*))))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "ttf.lisp" dir))
    (load (merge-pathnames "canvas.lisp" dir))
    (load (merge-pathnames "textview.lisp" dir))))

(defpackage :lol.undo-test (:use :cl :lol.textview))
(in-package :lol.undo-test)

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~60a ~:[FAIL~;PASS~]~%" name ok)
  (unless ok (incf *fails*)))

(defvar *font* (lol.canvas:load-font (merge-pathnames "cozette.lolf"
                                                       (make-pathname :directory (pathname-directory *load-truename*)))))

(defun fresh (text)
  (let ((tv (lol.textview::make-textview :font *font*)))
    (tv-geometry tv 0 0 800 600)
    (tv-set-text tv text)
    tv))

(defun key (tv k &optional (state 0))
  "One key, recorded the way the shell records it."
  (let ((before (tv-undo-checkpoint tv)))
    (tv-key tv k state)
    (tv-undo-note tv before :insert (and (characterp k) (graphic-char-p k) (zerop state)))))

(defun type-string (tv s) (loop for ch across s do (key tv ch)))
(defun text (tv) (tv-text tv))

(format t "~&== edits and undo ==~%")
(let ((tv (fresh "")))
  (type-string tv "hello")
  (check "typing builds the text" (string= (text tv) "hello"))
  (check "five self-inserts are ONE record" (= 1 (tv-undo-count tv)))
  (key tv :return)
  (type-string tv "world")
  (check "Return is a record of its own, then the next word" (= 3 (tv-undo-count tv)))
  (tv-undo tv)
  (check "C-_ takes back the last word" (string= (text tv) (format nil "hello~%")))
  (tv-undo tv)
  (check "C-_ again takes back the newline" (string= (text tv) "hello"))
  (tv-undo tv)
  (check "and again, the first word" (string= (text tv) ""))
  (check "then: no further undo information" (null (tv-undo tv)))
  (check "point is back at the start" (and (= 0 (tv-point-line tv)) (= 0 (tv-point-col tv))))

  (format t "~&== Emacs redo: any other command ends the run, then undo undoes undos ==~%")
  (key tv :right)                              ; motion: not an edit, but a command
  (tv-undo tv)
  (check "after a motion, C-_ redoes the last undo" (string= (text tv) "hello"))
  (tv-undo tv)
  (check "and the one before it" (string= (text tv) (format nil "hello~%")))
  (tv-undo tv)
  (check "back to the full text" (string= (text tv) (format nil "hello~%world")))
  (tv-undo tv)
  (check "continuing the run goes on into the old edits" (string= (text tv) (format nil "hello~%"))))

(format t "~&== amalgamation stops at 20, as in Emacs ==~%")
(let ((tv (fresh "")))
  (type-string tv (make-string 25 :initial-element #\x))
  (check "25 characters are two records" (= 2 (tv-undo-count tv)))
  (tv-undo tv)
  (check "the first C-_ removes the last 5" (= 20 (length (text tv)))))

(format t "~&== a change made without keys (a paste: a new vector) is one record ==~%")
(let ((tv (fresh "abc")))
  (let ((before (tv-undo-checkpoint tv)))
    (lol.textview:tv-replace tv (vector "abc" "pasted") 1 6)
    (tv-undo-note tv before))
  (check "the paste is undoable" (and (tv-undo tv) (string= (text tv) "abc"))))

(format t "~&== nothing to undo in a fresh buffer; a motion records nothing ==~%")
(let ((tv (fresh "abc")))
  (key tv :right)
  (check "count is 0 after motion only" (zerop (tv-undo-count tv)))
  (check "C-_ says there is nothing" (null (tv-undo tv))))

(format t "~&== M-<left> / M-<right> move by word, as M-b / M-f do ==~%")
(let ((tv (fresh "(defun foo-bar (x) x)")))
  (key tv :right 8) (key tv :right 8)
  (let ((by-arrow (tv-point-col tv)))
    (setf (tv-point-col tv) 0)
    (key tv #\f 8) (key tv #\f 8)
    (check "M-<right> twice lands where M-f twice does"
           (and (> by-arrow 1) (= by-arrow (tv-point-col tv)))))
  (let ((col (tv-point-col tv)))
    (key tv :left 8)
    (let ((by-arrow (tv-point-col tv)))
      (setf (tv-point-col tv) col)
      (key tv #\b 8)
      (check "M-<left> lands where M-b does" (and (< by-arrow col) (= by-arrow (tv-point-col tv)))))))

(format t "~&~:[===== ~d FAILURE(S) =====~;===== ALL PASS =====~]~%" (zerop *fails*) *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
