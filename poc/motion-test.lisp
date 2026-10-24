;;;; motion-test.lisp — host test for the text view's line motion at the buffer's edges.
;;;;
;;;;     sbcl --script poc/motion-test.lisp      ; exits 0 if all pass
;;;;
;;;; Down on the last row goes to the end of that line, Up on the first row to its
;;;; start (Emacs's C-n / C-p), with or without Ctrl, extending a selection when Shift
;;;; is held; PageDown / PageUp past the ends do the same.

(let ((dir (make-pathname :directory (pathname-directory *load-truename*))))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "ttf.lisp" dir))
    (load (merge-pathnames "canvas.lisp" dir))
    (load (merge-pathnames "textview.lisp" dir))))

(defpackage :lol.motion-test (:use :cl :lol.textview))
(in-package :lol.motion-test)

(defvar *fails* 0)
(defun check (name ok)
  (format t "~&  ~60a ~:[FAIL~;PASS~]~%" name ok)
  (unless ok (incf *fails*)))

(defvar *font* (lol.canvas:load-font (merge-pathnames "cozette.lolf"
                                                       (make-pathname :directory (pathname-directory *load-truename*)))))
(defconstant +shift+ 1)
(defconstant +ctrl+ 4)

(defun fresh (text line col)
  (let ((tv (lol.textview::make-textview :font *font*)))
    (tv-geometry tv 0 0 800 600)
    (tv-set-text tv text)
    (setf (tv-point-line tv) line (tv-point-col tv) col)
    (lol.textview::collapse tv)                ; the anchor at point: no selection yet
    tv))

(defun selected (tv)
  "The selection's text, or NIL with none."
  (multiple-value-bind (l0 c0 l1 c1) (tv-region tv)
    (and l0 (let ((v (tv-lines tv)))
              (with-output-to-string (o)
                (loop for l from (min l0 l1) to (max l0 l1)
                      for s = (aref v l)
                      for a = (if (= l (min l0 l1)) (if (< l0 l1) c0 (if (= l0 l1) (min c0 c1) c1)) 0)
                      for b = (if (= l (max l0 l1)) (if (> l0 l1) c0 (if (= l0 l1) (max c0 c1) c1)) (length s))
                      do (write-string s o :start a :end b)
                         (when (< l (max l0 l1)) (terpri o))))))))

(defun at (tv line col) (and (= line (tv-point-line tv)) (= col (tv-point-col tv))))

(format t "~&== Down on the last line ==~%")
(let ((tv (fresh (format nil "first~%last line") 1 3)))
  (tv-key tv :down 0)
  (check "Down goes to the end of the last line" (at tv 1 9))
  (tv-key tv :down 0)
  (check "and stays there" (at tv 1 9)))
(let ((tv (fresh (format nil "first~%last line") 1 3)))
  (tv-key tv :down +ctrl+)
  (check "Ctrl-Down too" (at tv 1 9)))
(let ((tv (fresh (format nil "first~%last line") 1 3)))
  (tv-key tv #\n +ctrl+)
  (check "C-n too" (at tv 1 9)))
(let ((tv (fresh (format nil "first~%last line") 1 3)))
  (tv-key tv :down +shift+)
  (check "Shift-Down selects to the end of the line"
         (string= "t line" (selected tv))))
(let ((tv (fresh (format nil "first~%last line") 0 2)))
  (tv-key tv :down +shift+)
  (check "from the line above, keeping the column" (at tv 1 2))
  (tv-key tv :down +shift+)
  (check "then to the end, the selection growing"
         (string= (format nil "rst~%last line") (selected tv))))

(format t "~&== Up on the first line ==~%")
(let ((tv (fresh (format nil "first line~%last") 0 4)))
  (tv-key tv :up +shift+)
  (check "Shift-Up selects to the start of the first line"
         (and (at tv 0 0) (string= "firs" (selected tv)))))

(format t "~&== pages past the ends ==~%")
(let ((tv (fresh (format nil "a~%bb~%ccc") 0 1)))
  (tv-key tv :page-down 0)
  (check "PageDown past the end: the end of the last line" (at tv 2 3)))
(let ((tv (fresh (format nil "aaa~%bb~%c") 2 1)))
  (tv-key tv :page-up 0)
  (check "PageUp past the start: the start of the first line" (at tv 0 0)))

(format t "~&~:[ALL PASS~;~:*~d FAILED~]~%" (and (plusp *fails*) *fails*))
(sb-ext:exit :code (if (plusp *fails*) 1 0))
