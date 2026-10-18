;;;; org-mode-test.lisp — host test for poc/org-mode.lisp: Org files in the shell.
;;;;
;;;;   sbcl --script poc/org-mode-test.lisp
;;;;
;;;; Loads poc/render-frame.lisp as a library (the real shell, world and keys, no
;;;; display), opens Org files written to a scratch folder, and drives them with the
;;;; keys a person would press: folding, heading motion, the structure edits, links,
;;;; the outline subject. Where Org has an answer, the expected value is EMACS'S — the
;;;; same Emacs 30.2 / Org 9.7.11 batch runs poc/org-test.lisp cites — turned 0-based.
;;;; After the screen-changing steps it checks that the incrementally repaired screen
;;;; equals the whole world painted afresh (doc/morphic.org §13), because folding
;;;; changes which line each row shows.

(defvar cl-user::*render-frame-library* t)
(load (merge-pathnames "render-frame.lisp" (make-pathname :directory (pathname-directory *load-truename*))))

(in-package :cl-user)

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (name ok)
  (incf *checks*)
  (format t "~&  ~70a ~:[FAIL~;PASS~]~%" name ok)
  (unless ok (incf *fails*)))

(defparameter +w+ 1280)
(defparameter +h+ 720)
(defun clock-string (&optional u) (declare (ignore u)) "2026-10-18 12:00:00")
(defun battery-status (&key fresh) (declare (ignore fresh)) nil)

(set-browser-font
 (open-browser-font (list "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf")
                    (merge-pathnames "cozette.lolf" *poc-dir*)))

(defvar *screen* (lol.canvas:make-canvas +w+ +h+))
(defvar *fresh* (lol.canvas:make-canvas +w+ +h+))
(defun repair () (lol.morph:world-repair *world* *screen*))
(defun screen-right-p ()
  (repair)
  (lol.canvas:fill-rect *fresh* 0 0 +w+ +h+ #x123456)
  (lol.morph:world-paint *world* *fresh* :fresh t)
  (equalp (lol.canvas:canvas-pixels *screen*) (lol.canvas:canvas-pixels *fresh*)))
(defun ev (kind &rest args)
  (lol.morph:world-dispatch *world* (apply #'lol.morph:make-event :kind kind args)))
(defun keys (script)
  (dolist (tok (split-on #\Space script))
    (multiple-value-bind (key arg state) (parse-key-token tok)
      (declare (ignore arg))
      (ev :key :key key :state state)
      (repair))))

;;; ---- the fixtures ------------------------------------------------------------------

(defparameter *dir* (format nil "/tmp/org-mode-test-~d/" (sb-posix:getpid)))
(ensure-directories-exist *dir*)
(defun put (name &rest lines)
  (with-open-file (s (concatenate 'string *dir* name) :direction :output :if-exists :supersede)
    (format s "~{~a~%~}" lines))
  (concatenate 'string *dir* name))

;; the file the Emacs folding probe read (poc/org-test.lisp's *FX*), plus two links
(defparameter *fx*
  (put "fx.org" "#+TITLE: fx" "preamble" "* A" "a body" "** A1" "a1 body" "" "** A2" "a2 body"
       "#+begin_src lisp" "* not-a-heading?" "#+end_src" "* B :tag:" "b body" "" "" "* C"))
(defparameter *fx2* (put "fx2.org" "* A" "a body" "" "* B" "b body" "** B1" "b1" "* C" "c"))
(defparameter *links*
  (put "links.org" "* Top" "see [[*Two][the second]] and [[file:fx2.org::*B][B there]]"
       "and [[file:../../nowhere.org][nothing]] and [[https://orgmode.org][Org]]"
       (format nil "and [[file:~aorg.lisp::defun analyze][the reader]]" (namestring *poc-dir*))
       "* Two" "two body"))
(defparameter *startup* (put "startup.org" "#+STARTUP: overview" "* One" "one body" "* Two" "two body"))

(defun open-file (path)
  (open-environment (subj-file path))
  (setf *world* (make-desk +w+ +h+))
  (repair)
  (current-pane))

(defun tv () (pane-tv (current-pane)))
(defun at (line &optional (col 0)) (lol.textview:tv-move-point (tv) line col) (repair))
(defun point () (list (lol.textview:tv-point-line (tv)) (lol.textview:tv-point-col (tv))))
(defun hidden () (let ((b (lol.textview:tv-hidden (tv))))
                   (and b (loop for i below (length b) when (= 1 (sbit b i)) collect i))))
(defun lines ()
  "The buffer's lines, without the empty last one a file ending in a newline has."
  (let ((l (coerce (lol.textview:tv-lines (tv)) 'list)))
    (if (equal (car (last l)) "") (butlast l) l)))

;;; ---- an Org file pane ---------------------------------------------------------------

(format t "~&== an .org file opens as Org ==~%")
(open-file *fx*)
(check "the pane is an Org pane, coloured as :org" (and (org-pane-p (current-pane))
                                                        (eq (lol.textview:tv-syntax (tv)) :org)))
(check "a headline's title is drawn in its level's colour"
       (let ((runs (org-runs (lol.textview:tv-lines (tv)) 2 "* A")))
         (equal (cddr (third runs)) (level-colour 1))))
(check "Lisp inside a src block is coloured by the Lisp lexer"
       (let ((runs (org-runs (vector "#+begin_src lisp" "(defun f (x) x)" "#+end_src") 1 "(defun f (x) x)")))
         (> (length runs) 3)))
(check "the reveal function and no #+STARTUP: nothing folded" (and (lol.textview:tv-reveal (tv)) (null (hidden))))

(format t "~&== S-TAB and TAB, against Emacs ==~%")
(keys "S-tab")
(check "S-TAB: OVERVIEW, the lines Emacs hides" (equal (hidden) '(3 4 5 6 7 8 9 11 13)))
(check "the status line says OVERVIEW" (equal *browser-status* "OVERVIEW"))
(check "the screen after folding equals a fresh paint" (screen-right-p))
(keys "S-tab")
(check "S-TAB again: CONTENTS" (equal (hidden) '(3 5 8 9 11 13)))
(keys "S-tab")
(check "S-TAB again: SHOW ALL" (null (hidden)))
(at 2)
(keys "tab")
(check "TAB on A: FOLDED" (and (equal (hidden) '(3 4 5 6 7 8 9)) (equal *browser-status* "FOLDED")))
(keys "tab")
(check "TAB again: CHILDREN" (and (equal (hidden) '(5 8 9)) (equal *browser-status* "CHILDREN")))
(keys "tab")
(check "TAB again: SUBTREE" (and (null (hidden)) (equal *browser-status* "SUBTREE")))
(keys "down up tab")                  ; from SUBTREE, with another key between
(check "TAB after another key: FOLDED, not the next state" (equal (hidden) '(3 4 5 6 7 8 9)))
(check "and the screen is right" (screen-right-p))
(at 2) (keys "down")
(check "down from a folded headline steps over the fold (to line 10)" (equal (first (point)) 10))
(keys "up")
(check "and up comes back onto the headline" (equal (first (point)) 2))
(at 2 3) (keys "right")
(check "right at the end of a folded headline: past the fold" (equal (point) '(10 0)))
(check "TAB in a body line is still completion, not folding"
       (progn (at 1) (keys "tab") (equal (hidden) '(3 4 5 6 7 8 9))))

(format t "~&== folds follow the text ==~%")
(at 1 8) (keys "ret")
(check "a line inserted above a fold: the fold moves down with its text" (equal (hidden) '(4 5 6 7 8 9 10)))
(keys "C-_")
(check "undo: the fold back where it was" (equal (hidden) '(3 4 5 6 7 8 9)))
(at 0) (keys "C-s a 1 space b o d y")
(check "C-s into a fold opens it and lands on the match"
       (and (equal (first (point)) 5) (not (lol.textview:hidden-line-p (tv) 5))))
(keys "ret")
(check "and the screen is right" (screen-right-p))

(format t "~&== Org's smart guard on folded text ==~%")
(keys "S-tab")                         ; OVERVIEW again
(at 12) (keys "C-e C-k")
(check "C-k at the end of a folded line deletes nothing ..." (equal (nth 13 (lines)) "b body"))
(check "... opens the fold and says why" (and (not (lol.textview:hidden-line-p (tv) 13))
                                              (search "folded text" *browser-status*)))
(keys "S-tab S-tab S-tab")             ; show all

(format t "~&== heading motion, against Emacs ==~%")
(flet ((from (line keys)
         (at line) (keys keys) (first (point))))
  (check "C-c C-n from a body line (Emacs 4 -> 5)" (= (from 3 "C-c C-n") 4))
  (check "C-c C-p from a body line (Emacs 6 -> 5)" (= (from 5 "C-c C-p") 4))
  (check "C-c C-p from a headline (Emacs 5 -> 3)" (= (from 4 "C-c C-p") 2))
  (check "C-c C-f from a body line: its heading's next sibling (Emacs 6 -> 8)" (= (from 5 "C-c C-f") 7))
  (check "C-c C-f on the last sibling stays (Emacs 8 -> 8)" (= (from 7 "C-c C-f") 7))
  (check "C-c C-b from a body line (Emacs 9 -> 5)" (= (from 8 "C-c C-b") 4))
  (check "C-c C-u from a body line (Emacs 6 -> 3)" (= (from 5 "C-c C-u") 2))
  (check "C-c C-u at the top level: its own heading, and Emacs's message"
         (and (= (from 3 "C-c C-u") 2) (equal *browser-status* "Already at top level of the outline"))))

(format t "~&== structure edits, against Emacs ==~%")
(open-file *fx2*)
(at 3) (keys "M-up")
(check "M-<up> on B (Emacs)" (equal (lines) '("* B" "b body" "** B1" "b1" "* A" "a body" "" "* C" "c")))
(check "the caret moved with it" (= (first (point)) 0))
(keys "C-_")
(check "undo puts it back" (equal (lines) '("* A" "a body" "" "* B" "b body" "** B1" "b1" "* C" "c")))
(at 3) (keys "M-S-right")
(check "M-S-<right>: B and B1 demoted (Emacs)" (equal (subseq (lines) 3 6) '("** B" "b body" "*** B1")))
(keys "M-left")
(check "M-<left>: B alone promoted (Emacs)" (equal (subseq (lines) 3 6) '("* B" "b body" "*** B1")))
(keys "C-_ C-_")
(at 0) (keys "M-left")
(check "M-<left> at level 1: Emacs's message, nothing changed"
       (and (search "Cannot promote to level 0" *browser-status*) (equal (first (lines)) "* A")))
(at 3 3) (keys "M-ret N E W")
(check "M-RET at the end of B: a sibling with B's blank line before it (Emacs)"
       (equal (subseq (lines) 3 7) '("* B" "" "* NEW" "b body")))
(keys "C-_ C-_")                       ; the typed word, then the heading: two records
(check "two undos take back the word and the new heading"
       (equal (lines) '("* A" "a body" "" "* B" "b body" "** B1" "b1" "* C" "c")))
(at 3) (keys "C-c C-t")
(check "C-c C-t: TODO (Emacs)" (equal (nth 3 (lines)) "* TODO B"))
(keys "C-c C-t C-c C-t")
(check "C-c C-t twice more: DONE, then none (Emacs)" (equal (nth 3 (lines)) "* B"))
(at 0) (keys "tab M-down")
(check "a folded subtree moved by M-<down> stays folded"
       (and (equal (subseq (lines) 4 6) '("* A" "a body")) (lol.textview:hidden-line-p (tv) 5)))
(check "and the screen is right" (screen-right-p))

(format t "~&== links ==~%")
(open-file *links*)
(at 1 6) (keys "C-c C-o")
(check "[[*Two]]: the heading in this file" (= (first (point)) 4))
(at 1 30) (keys "C-c C-o")
(check "[[file:fx2.org::*B]]: that file opens, on that heading"
       (and (equal (org-pane-path (current-pane)) *fx2*) (= (first (point)) 3)))
(open-file *links*)
(at 2 10) (keys "C-c C-o")
(check "a missing file says so" (search "no such file" *browser-status*))
(at 3 20) (keys "C-c C-o")
(check "[[file:…org.lisp::defun analyze]]: the LIVE definition, from the registry"
       (let ((s (pane-subject (current-pane))))
         (and (subj-defn-p s) (eq (subj-defn-name s) 'lol.org:analyze))))
(open-file *links*)
(at 0) (keys "C-c C-o")
(check "C-c C-o where there is no link says so" (search "no link" *browser-status*))

(format t "~&== the outline subject ==~%")
(let ((v (present (subj-org *fx*))))
  (check "the outline lists the file's top headings"
         (equal (mapcar #'item-label (view-items v)) '("Edit the file" "A" "not-a-heading?" "B" "C"))))
(let* ((v (present (subj-org *fx* '("A")))))
  (check "a heading lists Edit here and its children"
         (equal (mapcar #'item-label (view-items v)) '("Edit here" "A1" "A2"))))
(check "a heading's preview is its section, coloured as Org"
       (let ((v (present-preview (subj-org *fx* '("A" "A2")))))
         (and (eq (view-syntax v) :org) (string= (subseq (view-text v) 0 5) "** A2"))))
(check "a heading that is gone says so"
       (search "no longer" (item-label (first (view-items (present (subj-org *fx* '("Z"))))))))
(let ((v (present (subj-org *links* '("Top")))))
  (check "a heading's links are rows that open them"
         (equal (mapcar #'item-label (remove "links" (view-items v) :key #'item-group :test-not #'equal))
                '("the second" "B there" "nothing" "Org" "the reader"))))
(open-file *fx*)
(check "the Spotter's headings: those of the Org files open in a tab"
       (subsetp '("A" "A1" "B") (mapcar (lambda (c) (getf c :label)) (org-spotter-candidates)) :test #'equal))
(check "an .org file offers its Outline" (find "Outline" (commands-for (subj-file *fx*))
                                              :key #'command-label :test #'equal))

(format t "~&== #+STARTUP ==~%")
(open-file *startup*)
(check "#+STARTUP: overview — the file opens folded" (equal (hidden) '(2 4)))
(check "and the screen is right" (screen-right-p))

(format t "~&~d checks, ~d failed~%" *checks* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
