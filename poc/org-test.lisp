;;;; org-test.lisp — host test for poc/org.lisp.
;;;;
;;;;   sbcl --script poc/org-test.lisp
;;;;
;;;; Prints PASS/FAIL per assertion and exits non-zero if any failed. The folding and
;;;; structure-edit answers are EMACS'S: each fixture below was run through Emacs 30.2
;;;; with its Org 9.7.11 in batch mode (emacs -Q --batch; the probes and the fixtures
;;;; are in poc/org-data/, with how to rerun them) and the lines it hid, or the
;;;; buffer it left, are written here — Emacs's 1-based line numbers turned 0-based. Then every .org this
;;;; project keeps is read, so the reader is tested on the text it exists for.

(load (merge-pathnames "org.lisp" (or *load-truename* *default-pathname-defaults*)))

(defpackage :lol.org-test (:use :cl :lol.org))
(in-package :lol.org-test)

(defvar *failures* 0)
(defvar *count* 0)

(defun check (name got want)
  (incf *count*)
  (if (equalp got want)
      (format t "PASS  ~a~%" name)
      (progn (incf *failures*)
             (format t "FAIL  ~a~%      got:  ~s~%      want: ~s~%" name got want))))

(defun v (&rest lines) (coerce lines 'simple-vector))
(defun hidden-lines (bits) (and bits (loop for i below (length bits) when (= 1 (sbit bits i)) collect i)))

;;; ---- the reader ------------------------------------------------------------------

(check "a headline" (multiple-value-list (parse-headline "** TODO [#A] Fix the thing   :ui:lisp:"))
       '(2 "TODO" #\A "Fix the thing" ("ui" "lisp") 13))
(check "stars alone are not a headline" (parse-headline "***") nil)
(check "a bold word at column 0 is not a headline" (parse-headline "*bold* text") nil)
(check "a keyword-looking word that is not a keyword is title"
       (nth-value 3 (parse-headline "* NEXT thing")) "NEXT thing")
(check "tags need a blank before them"
       (multiple-value-list (parse-headline "* a:b:")) '(1 nil nil "a:b:" nil 2))
(check "a title that is only tags" (nth-value 4 (parse-headline "* :x:")) nil)

(check "line kinds"
       (mapcar #'line-kind '("* h" "" "  " "#+TITLE: x" "#+begin_src lisp" "#+END_SRC" "# c"
                             "| a |" ": fixed" ":PROPERTIES:" ":END:" "-----" "- item" "  * item"
                             "1. one" "a) x" "text" "#not-a-comment"))
       '(:headline :blank :blank :keyword :block-begin :block-end :comment :table :fixed
         :drawer :drawer-end :rule :item :item :item :item :paragraph :paragraph))

;;; The folding fixture: the file the Emacs probe read.
(defparameter *fx*
  (v "#+TITLE: fx" "preamble" "* A" "a body" "** A1" "a1 body" "" "** A2" "a2 body"
     "#+begin_src lisp" "* not-a-heading?" "#+end_src" "* B :tag:" "b body" "" "" "* C"))
(defparameter *doc* (analyze *fx*))

(check "the headlines, as Emacs lists them (org-map-entries)"
       (map 'list (lambda (h) (list (head-line h) (head-level h) (head-title h))) (doc-heads *doc*))
       '((2 1 "A") (4 2 "A1") (7 2 "A2") (10 1 "not-a-heading?") (12 1 "B") (16 1 "C")))
(check "a * line inside a src block is a headline, as in Org — it ends the block"
       (list (aref (doc-kinds *doc*) 10) (aref (doc-kinds *doc*) 11)) '(:headline :block-end))
(check "the block's lines know their block" (subseq (aref (doc-blocks *doc*) 9) 0 2) '("src" "lisp"))
(check "tags" (head-tags (aref (doc-heads *doc*) 4)) '("tag"))
(check "subtree ends" (map 'list #'head-end (doc-heads *doc*)) '(10 7 10 12 16 17))
(check "the section of a body line" (head-index-at *doc* 5) 1)
(check "before the first headline there is none" (head-index-at *doc* 1) nil)
(check "children" (head-children *doc* 0) '(1 2))
(check "parent" (head-parent *doc* 2) 0)
(check "a heading's path of titles" (head-path *doc* 2) '("A" "A2"))
(check "found again by its path" (find-head-by-path *doc* '("A" "A2")) 2)
(check "found by title" (find-head-by-title *doc* "B") 4)
(check "siblings" (list (next-sibling *doc* 1) (prev-sibling *doc* 2) (next-sibling *doc* 2)) '(2 1 nil))
(check "the analysis is cached by the vector" (eq (analyze *fx*) *doc*) t)
(check "and a new vector is analysed afresh" (eq (analyze (copy-seq *fx*)) *doc*) nil)

(let ((d (analyze (v "#+TODO: NEXT WAIT | DONE CANCELLED" "* NEXT a" "* WAIT b" "* CANCELLED c" "* TODO d"))))
  (check "#+TODO declares the keywords" (list (doc-todo d) (doc-done d)) '(("NEXT" "WAIT") ("DONE" "CANCELLED")))
  (check "and only those are keywords"
         (map 'list #'head-keyword (doc-heads d)) '("NEXT" "WAIT" "CANCELLED" nil)))

;;; ---- objects -----------------------------------------------------------------------

(let ((objs (line-objects "see [[file:a.org::*Top][the top]] and *bold*, /it/ ~code~ =v= https://x.org/a.")))
  (check "objects found, in order" (mapcar #'third objs) '(:link :bold :italic :code :verbatim :url))
  (check "a link's target" (getf (cdddr (first objs)) :target) "file:a.org::*Top")
  (check "the URL leaves the full stop out" (getf (cdddr (sixth objs)) :target) "https://x.org/a"))
(check "no bold inside a link" (mapcar #'third (line-objects "[[file:x::*a*b*]]")) '(:link))
(check "=[[file:x]]= is verbatim text, not a link (Org: the first to open wins)"
       (mapcar #'third (line-objects "write =[[file:x]]= to link")) '(:verbatim))
(check "a link before verbatim keeps both" (mapcar #'third (line-objects "[[a]] and =b=")) '(:link :verbatim))
(check "a lone star is not emphasis" (line-objects "2 * 3 * 4") nil)
(check "x*y*z is not bold (the marker needs a boundary)" (line-objects "x*y*z") nil)
(check "the link at a column" (third (link-at "go [[*Top]] now" 5)) :link)
(check "and just after its ]]" (third (link-at "go [[*Top]] now" 11)) :link)
(check "nothing elsewhere" (link-at "go [[*Top]] now" 13) nil)

(check "file link" (parse-link "file:../poc/x.lisp") '(:type :file :path "../poc/x.lisp"))
(check "file link with a text search"
       (parse-link "file:../initramfs/net.lisp::defun bring-up")
       '(:type :file :path "../initramfs/net.lisp" :search (:text "defun bring-up")))
(check "file link with a heading search" (parse-link "file:a.org::*Two")
       '(:type :file :path "a.org" :search (:heading "Two")))
(check "file link with a line number" (parse-link "file:a.txt::12") '(:type :file :path "a.txt" :search (:line 12)))
(check "a relative path is a file link" (parse-link "./b.org") '(:type :file :path "./b.org"))
(check "a heading in this file" (parse-link "*What we shipped") '(:type :heading :title "What we shipped"))
(check "a web link" (parse-link "https://orgmode.org") '(:type :url :url "https://orgmode.org"))
(check "anything else is fuzzy" (parse-link "some target") '(:type :fuzzy :text "some target"))

;;; ---- folding, against Emacs -------------------------------------------------------

(let (bits st)
  (multiple-value-setq (bits st) (cycle-subtree *doc* nil 0))
  (check "TAB on A: FOLDED (Emacs hides 4..10)" (list st (hidden-lines bits)) '(:folded (3 4 5 6 7 8 9)))
  (multiple-value-setq (bits st) (cycle-subtree *doc* bits 0 :repeat t))
  (check "TAB again: CHILDREN (Emacs: 6 9 10)" (list st (hidden-lines bits)) '(:children (5 8 9)))
  (multiple-value-setq (bits st) (cycle-subtree *doc* bits 0 :repeat t))
  (check "TAB again: SUBTREE, nothing hidden" (list st bits) '(:subtree nil))
  (multiple-value-setq (bits st) (cycle-subtree *doc* bits 0))
  (check "TAB after another command: FOLDED again" (list st (hidden-lines bits)) '(:folded (3 4 5 6 7 8 9)))
  (check "state read back" (subtree-state *doc* bits 0) :folded))
(let ((bits (nth-value 0 (cycle-subtree *doc* (nth-value 0 (cycle-subtree *doc* nil 0)) 0))))
  (check "CHILDREN, then TAB not repeated: FOLDED (Emacs)"
         (nth-value 1 (cycle-subtree *doc* bits 0)) :folded))
(let (bits st)
  (multiple-value-setq (bits st) (cycle-subtree *doc* nil 1))
  (check "TAB on A1 hides its body, not the blank line after it (Emacs: 6)" (list st (hidden-lines bits)) '(:folded (5)))
  (multiple-value-setq (bits st) (cycle-subtree *doc* bits 1 :repeat t))
  (check "A1 has no children: straight to SUBTREE (NO CHILDREN)" (list st bits) '(:subtree-no-children nil)))
(let (bits st)
  (multiple-value-setq (bits st) (cycle-subtree *doc* nil 4))
  (check "TAB on B keeps its two blank lines on screen (Emacs: 14)" (list st (hidden-lines bits)) '(:folded (13)))
  (multiple-value-setq (bits st) (cycle-subtree *doc* bits 4 :repeat t))
  (check "B: SUBTREE (NO CHILDREN)" (list st bits) '(:subtree-no-children nil))
  (multiple-value-setq (bits st) (cycle-subtree *doc* bits 4 :repeat t))
  (check "B: FOLDED" (list st (hidden-lines bits)) '(:folded (13))))
(check "TAB on an empty entry changes nothing" (multiple-value-list (cycle-subtree *doc* nil 5)) '(nil :empty))
(let (bits st)
  (multiple-value-setq (bits st) (cycle-global *doc* nil))
  (check "S-TAB: OVERVIEW (Emacs: 4..10 12 14)" (list st (hidden-lines bits)) '(:overview (3 4 5 6 7 8 9 11 13)))
  (multiple-value-setq (bits st) (cycle-global *doc* st))
  (check "S-TAB again: CONTENTS (Emacs: 4 6 9 10 12 14)" (list st (hidden-lines bits)) '(:contents (3 5 8 9 11 13)))
  (multiple-value-setq (bits st) (cycle-global *doc* st))
  (check "S-TAB again: SHOW ALL" (list st bits) '(:all nil)))

(check "#+STARTUP: overview" (hidden-lines (startup-hidden (analyze (v "#+STARTUP: overview" "* a" "x"))))
       '(2))
(check "#+STARTUP: showall content — the last one wins"
       (hidden-lines (startup-hidden (analyze (v "#+STARTUP: showall content" "* a" "x" "** b" "y")))) '(2 4))
(check "#+STARTUP: showall shows all" (startup-hidden (analyze (v "#+STARTUP: showall" "* a" "x"))) nil)
(check "no #+STARTUP: Org 9.7's default shows everything" (startup-hidden *doc*) nil)

(let* ((bits (cycle-global *doc* nil))
       (shown (reveal-line *doc* bits 8)))
  (check "reveal a line inside a fold: its lineage and its entry show"
         (mapcar (lambda (l) (visible-line-p shown l)) '(2 7 8 9 3 5))
         '(t t t t nil nil)))

;;; ---- structure edits, against Emacs -------------------------------------------------

(defparameter *fx2* (v "* A" "a body" "" "* B" "b body" "** B1" "b1" "* C" "c"))

(check "M-<up> on B: B and its child above A; A keeps its blank line (Emacs)"
       (multiple-value-list (move-subtree-up *fx2* 3))
       (list (v "* B" "b body" "** B1" "b1" "* A" "a body" "" "* C" "c") 0))
(check "M-<down> on A: the same buffer, the caret on A (Emacs: line 5)"
       (multiple-value-list (move-subtree-down *fx2* 0))
       (list (v "* B" "b body" "** B1" "b1" "* A" "a body" "" "* C" "c") 4))
(check "M-<down> on B: below C (Emacs)"
       (multiple-value-list (move-subtree-down *fx2* 3))
       (list (v "* A" "a body" "" "* C" "c" "* B" "b body" "** B1" "b1") 5))
(check "M-<down> on the last: Emacs's message"
       (handler-case (move-subtree-down *fx2* 7) (org-edit-error (e) (princ-to-string e)))
       "Cannot move past superior level or buffer limit")
(check "M-<up> on a first child: the same"
       (handler-case (move-subtree-up *fx2* 5) (org-edit-error (e) (princ-to-string e)))
       "Cannot move past superior level or buffer limit")
(check "M-S-<right> on B demotes B and B1 (Emacs)"
       (demote-subtree *fx2* 3) (v "* A" "a body" "" "** B" "b body" "*** B1" "b1" "* C" "c"))
(check "M-<right> on B demotes B alone (Emacs)"
       (demote-heading *fx2* 3) (v "* A" "a body" "" "** B" "b body" "** B1" "b1" "* C" "c"))
(check "M-<left> on B1 (Emacs)"
       (promote-heading *fx2* 5) (v "* A" "a body" "" "* B" "b body" "* B1" "b1" "* C" "c"))
(check "M-<left> at level 1: Emacs's message"
       (handler-case (promote-heading *fx2* 0) (org-edit-error (e) (princ-to-string e)))
       "Cannot promote to level 0.  UNDO to recover if necessary")
(check "a body line acts on its headline" (promote-heading *fx2* 6)
       (v "* A" "a body" "" "* B" "b body" "* B1" "b1" "* C" "c"))

(check "M-RET at the end of B: a sibling after the headline line, with B's blank before (Emacs)"
       (multiple-value-list (insert-heading *fx2* 3 3))
       (list (v "* A" "a body" "" "* B" "" "* " "b body" "** B1" "b1" "* C" "c") 5 2))
(check "M-RET inside B's stars: the same (Emacs)"
       (multiple-value-list (insert-heading *fx2* 3 1))
       (list (v "* A" "a body" "" "* B" "" "* " "b body" "** B1" "b1" "* C" "c") 5 2))
(check "M-RET at column 0 of a headline: a sibling before it"
       (multiple-value-list (insert-heading *fx2* 7 0))
       (list (v "* A" "a body" "" "* B" "b body" "** B1" "b1" "* " "* C" "c") 7 2))
(check "M-RET in a body line: split, the rest becomes the title"
       (multiple-value-list (insert-heading *fx2* 1 2))
       (list (v "* A" "a" "* body" "" "* B" "b body" "** B1" "b1" "* C" "c") 2 2))
(check "M-RET on a list item: a new item"
       (multiple-value-list (insert-heading (v "  - one" "  - two") 0 7))
       (list (v "  - one" "  - " "  - two") 1 4))
(check "M-RET on a numbered item counts on"
       (nth-value 0 (insert-heading (v "1. one") 0 6)) (v "1. one" "2. "))

(let ((l *fx2*) kw)
  (multiple-value-setq (l) (cycle-todo l 3))
  (check "C-c C-t: TODO (Emacs)" (aref l 3) "* TODO B")
  (multiple-value-setq (l) (cycle-todo l 4))
  (check "C-c C-t again, from the body: DONE (Emacs)" (aref l 3) "* DONE B")
  (let (line)
    (multiple-value-setq (l line kw) (cycle-todo l 3))
    (check "and again: none (Emacs)" (list (aref l 3) line kw) '("* B" 3 nil))))
(check "C-c C-t keeps the priority and tags"
       (aref (cycle-todo (v "* [#A] Fix :x:") 0) 0) "* TODO [#A] Fix :x:")

;;; ---- every .org this project keeps --------------------------------------------------

(defun read-lines (path)
  (with-open-file (s path :external-format :utf-8)
    (coerce (loop for x = (read-line s nil) while x collect x) 'simple-vector)))

(let* ((root (merge-pathnames "../" (make-pathname :name nil :type nil
                                                   :defaults (or *load-truename* *default-pathname-defaults*))))
       (files (append (directory (merge-pathnames "README.org" root))
                      (directory (merge-pathnames "doc/*.org" root))
                      (directory (merge-pathnames "doc/background/*.org" root))))
       (heads 0) (open-blocks '()) (links 0) (file-links 0) (bad-links '()) (worst 0))
  (dolist (f files)
    (let* ((lines (read-lines f))
           (t0 (get-internal-real-time))
           (doc (analyze lines)))
      (setf worst (max worst (- (get-internal-real-time) t0)))
      (incf heads (length (doc-heads doc)))
      ;; a block still open at the end of a file would colour the rest of it as code
      (let ((last (and (plusp (length lines)) (aref (doc-blocks doc) (1- (length lines))))))
        (when (and last (not (eq (aref (doc-kinds doc) (1- (length lines))) :block-end)))
          (push (file-namestring f) open-blocks)))
      (loop for s across lines
            do (dolist (o (line-objects s))
                 (when (eq (third o) :link)
                   (incf links)
                   (let ((tg (getf (cdddr o) :target)))
                     (when (and (> (length tg) 5) (string= "file:" tg :end2 5))
                       (incf file-links)
                       (unless (eq (getf (parse-link tg) :type) :file)
                         (push tg bad-links)))))))))
  (format t "      ~d files, ~d headlines, ~d links (~d file:), slowest pass ~,1f ms~%"
          (length files) heads links file-links
          (/ worst (/ internal-time-units-per-second 1000)))
  (check "every project .org was read" (> (length files) 25) t)
  (check "no block left open at a file's end" open-blocks nil)
  (check "every file: link parses as a file link" bad-links nil)
  (check "the docs have their headlines" (> heads 700) t))

(format t "~&~d checks, ~d failed~%" *count* *failures*)
(sb-ext:exit :code (if (zerop *failures*) 0 1))
