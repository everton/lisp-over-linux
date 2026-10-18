;;;; org.lisp — Org's structure as plain data: the outline, folding, structure edits.
;;;;
;;;; An Org file has no format beyond text: a line that starts with stars and a space
;;;; is a HEADLINE, the number of stars is its LEVEL, and its SUBTREE runs to the next
;;;; headline of the same or a higher level. Nothing marks where a section ends — the
;;;; next headline does. So the outline is a FUNCTION of the lines, recomputed whenever
;;;; they change, and can never disagree with the text. That is the one property of
;;;; Org this file is built to keep. Design and the subset chosen: doc/org.org.
;;;;
;;;; Four things live here, all pure and host-testable (sbcl --script poc/org-test.lisp):
;;;;
;;;;   1. The READER — each line classified by its first characters (ANALYZE), the
;;;;      headlines with their keyword, priority, title and tags, the blocks a line is
;;;;      inside of, #+TODO and #+STARTUP. One pass; cached on the line vector (EQ).
;;;;   2. OBJECTS within a line — links, emphasis, inline code — for the colourer and
;;;;      for "the link at the caret" (LINE-OBJECTS, PARSE-LINK).
;;;;   3. FOLDING as a hidden-line bit vector: Org's TAB and S-TAB cycles and
;;;;      #+STARTUP, each checked against Emacs 30.2's Org 9.7.11 (the host's), whose
;;;;      answers the test reproduces.
;;;;   4. STRUCTURE EDITS — promote/demote, move a subtree, a new heading, TODO
;;;;      cycling — as functions from a line vector to a NEW line vector (the editor's
;;;;      undo is free because its buffer is persistent; doc/clipboard.org §6½).
;;;;
;;;; Text model, shared with poc/textview.lisp and poc/clipboard.lisp: a buffer is a
;;;; SIMPLE-VECTOR of strings, one per line, no newline inside; a line index is
;;;; 0-based. No dependency on any other project file.

(defpackage :lol.org
  (:use :cl)
  (:export
   ;; the reader
   #:analyze #:doc-lines #:doc-kinds #:doc-blocks #:doc-heads #:doc-startup
   #:doc-todo #:doc-done #:line-kind
   #:headline #:head-line #:head-level #:head-keyword #:head-priority #:head-title
   #:head-tags #:head-end #:head-title-start
   #:parse-headline #:headline-line-p
   ;; outline queries
   #:head-at #:head-index-at #:head-children #:head-parent #:head-path
   #:find-head-by-path #:find-head-by-title #:fold-end #:entry-end
   #:next-head #:prev-head #:next-sibling #:prev-sibling
   ;; objects
   #:line-objects #:parse-link #:link-at
   ;; folding
   #:subtree-state #:cycle-subtree #:cycle-global #:startup-hidden
   #:reveal-line #:visible-line-p
   ;; structure edits
   #:org-edit-error #:promote-heading #:demote-heading #:promote-subtree #:demote-subtree
   #:move-subtree-up #:move-subtree-down #:insert-heading #:cycle-todo))

(in-package :lol.org)

;;; ---- small text helpers ------------------------------------------------------------

(declaim (inline blank-char-p))
(defun blank-char-p (c) (or (char= c #\Space) (char= c #\Tab)))

(defun blank-line-p (s) (every #'blank-char-p s))

(defun indent-of (s)
  "Index of the first non-blank character of S (its length when it is all blank)."
  (or (position-if-not #'blank-char-p s) (length s)))

(defun prefix-ci-p (prefix s start)
  "Does S hold PREFIX at START, ignoring case? (#+BEGIN_SRC and #+begin_src are one.)"
  (let ((end (+ start (length prefix))))
    (and (<= end (length s)) (string-equal prefix s :start2 start :end2 end))))

(defun words (s)
  "S split at blanks, empty pieces dropped."
  (loop with out = '() with i = 0 with n = (length s)
        do (let ((a (position-if-not #'blank-char-p s :start i)))
             (unless a (return (nreverse out)))
             (let ((b (or (position-if #'blank-char-p s :start a) n)))
               (push (subseq s a b) out)
               (setf i b)))))

;;; ---- 1. the reader -----------------------------------------------------------------
;;;
;;; A line's KIND is decided by its first characters alone, with one piece of state
;;; carried down the file: "inside which block, if any". Inside #+begin_src … #+end_src
;;; a line beginning with #+ or a colon is code, not a keyword or a drawer.
;;;
;;; A headline is NOT hidden by a block. Org itself reads "* x" at column 0 inside a
;;; src block as a headline — which is why Org asks you to escape such lines with a
;;; comma (",* x"), and why a block left open by one simply ends there. Checked with
;;; the host's Emacs 30.2 (Org 9.7.11), 2026-10-18: org-map-entries lists it. The
;;; design doc once claimed the opposite; it was wrong (doc/org.org §3.1).

(defun headline-line-p (s)
  "Is S a headline: one or more stars at column 0, then a blank?"
  (let ((n (or (position #\* s :test-not #'char=) (length s))))
    (and (plusp n) (< n (length s)) (blank-char-p (char s n)))))

(defstruct (headline (:conc-name head-))
  line            ; its line index
  level           ; the number of stars
  keyword         ; "TODO", "DONE", … or NIL
  priority        ; a character (#\A) or NIL
  title           ; the text without stars, keyword, priority or tags
  title-start     ; the column the title starts at (after "** TODO [#A] ")
  tags            ; a list of strings
  end)            ; the first line AFTER its subtree

(defun tags-start (s from)
  "Where a trailing tag group (\" :a:b:\") starts in S at or after FROM, or NIL."
  (let* ((e (or (position-if-not #'blank-char-p s :from-end t) -1)))
    (when (and (> e from) (char= (char s e) #\:))
      (let ((b (position-if (lambda (c) (or (blank-char-p c)
                                            (not (or (alphanumericp c) (find c ":_@#%")))))
                            s :end e :from-end t)))
        (when (and b (>= b from) (blank-char-p (char s b))
                   (char= (char s (1+ b)) #\:) (> e (1+ b)))
          (1+ b))))))

(defun parse-headline (s &key (todo '("TODO")) (done '("DONE")))
  "S as a headline -> (values LEVEL KEYWORD PRIORITY TITLE TAGS TITLE-START), or NIL
   when S is not one. KEYWORD is recognised only when it is one of TODO/DONE (the
   file's #+TODO line decides those)."
  (when (headline-line-p s)
    (let* ((level (position #\* s :test-not #'char=))
           (i (indent-of (subseq s level)))
           (pos (+ level i))
           keyword priority)
      ;; the keyword: the first word, when it is one of the file's keywords
      (let* ((we (or (position-if #'blank-char-p s :start pos) (length s)))
             (w (subseq s pos we)))
        (when (and (plusp (length w)) (member w (append todo done) :test #'string=))
          (setf keyword w
                pos (min (length s) (+ we (indent-of (subseq s we)))))))
      ;; the priority cookie [#A]
      (when (and (<= (+ pos 4) (length s)) (string= "[#" s :start2 pos :end2 (+ pos 2))
                 (char= (char s (+ pos 3)) #\]))
        (setf priority (char s (+ pos 2))
              pos (let ((after (+ pos 4))) (+ after (indent-of (subseq s after))))))
      (let* ((ts (tags-start s pos))
             (title (string-right-trim '(#\Space #\Tab) (subseq s pos (or ts (length s)))))
             (tags (and ts (remove "" (split-colons (string-trim '(#\Space #\Tab) (subseq s ts)))
                                   :test #'string=))))
        (values level keyword priority title tags pos)))))

(defun split-colons (s)
  (loop with out = '() with start = 0
        for i from 0 to (length s)
        do (when (or (= i (length s)) (char= (char s i) #\:))
             (push (subseq s start i) out) (setf start (1+ i)))
        finally (return (nreverse out))))

(defun line-kind (s)
  "S's kind by its first characters alone (no block state): :headline :keyword
   :block-begin :block-end :drawer :drawer-end :item :table :comment :fixed :rule
   :blank :paragraph."
  (cond
    ((headline-line-p s) :headline)
    ((blank-line-p s) :blank)
    (t (let* ((i (indent-of s)) (c (char s i)) (n (length s)))
         (cond
           ((prefix-ci-p "#+begin_" s i) :block-begin)
           ((prefix-ci-p "#+end_" s i) :block-end)
           ((and (char= c #\#) (< (1+ i) n) (char= (char s (1+ i)) #\+)) :keyword)
           ((and (char= c #\#) (or (= (1+ i) n) (blank-char-p (char s (1+ i))))) :comment)
           ((char= c #\|) :table)
           ((and (char= c #\:) (or (= (1+ i) n) (blank-char-p (char s (1+ i))))) :fixed)
           ((and (char= c #\:) (string-equal (string-right-trim '(#\Space #\Tab) (subseq s i)) ":END:"))
            :drawer-end)
           ((and (char= c #\:) (let ((e (string-right-trim '(#\Space #\Tab) s)))
                                 (and (> (length e) (1+ i)) (char= (char e (1- (length e))) #\:)
                                      (every (lambda (x) (or (alphanumericp x) (find x "_-")))
                                             (subseq e (1+ i) (1- (length e)))))))
            :drawer)
           ((and (char= c #\-) (>= (- n i) 5) (every (lambda (x) (char= x #\-))
                                                     (string-right-trim '(#\Space #\Tab) (subseq s i))))
            :rule)
           ((item-bullet-end s) :item)
           (t :paragraph))))))

(defun item-bullet-end (s)
  "When S is a list item, the column after its bullet and blank (\"  - x\" -> 4), else
   NIL. Bullets: - and + anywhere, * only indented (at column 0 it is a headline),
   1. 1) a. a) counters."
  (let* ((i (indent-of s)) (n (length s)))
    (when (< i n)
      (flet ((after (j) (cond ((= j n) j) ((blank-char-p (char s j)) (1+ j)))))
        (let ((c (char s i)))
          (cond ((or (char= c #\-) (char= c #\+) (and (char= c #\*) (plusp i))) (after (1+ i)))
                ((digit-char-p c)
                 (let ((j (or (position-if-not #'digit-char-p s :start i) n)))
                   (and (< j n) (find (char s j) ".)") (after (1+ j)))))
                ((and (alpha-char-p c) (< (1+ i) n) (find (char s (1+ i)) ".)")
                      (or (= (+ i 2) n) (blank-char-p (char s (+ i 2)))))
                 (after (+ i 2)))))))))

;;; The analysis of a whole line vector. Cached by the vector's identity: the text
;;; view makes a NEW vector on every edit (keeping unchanged line strings EQ), so the
;;; key changes exactly when the text does, and the many reads per frame — colouring
;;; each row, the outline view, the fold commands — share one pass.

(defstruct doc
  lines           ; the simple-vector analysed
  kinds           ; per line: a LINE-KIND, or :block-body inside a block
  blocks          ; per line: NIL, or (NAME LANG BEGIN-LINE) for a line inside a block
                  ;   (its #+begin and #+end lines included)
  heads           ; simple-vector of HEADLINE, in file order
  head-of         ; per line: the index of the headline whose section holds it, or -1
  todo done       ; the not-done and done keywords (#+TODO: A B | C), as strings
  startup)        ; the #+STARTUP words, downcased, in order

(defvar *analysis-cache* (make-hash-table :test 'eq :weakness :key))

(defun file-keywords (lines)
  "The #+TODO / #+SEQ_TODO / #+TYP_TODO keywords -> (values TODO DONE STARTUP)."
  (let ((todo '()) (done '()) (startup '()) (seen nil))
    (loop for s across lines
          do (let ((i (indent-of s)))
               (cond ((or (prefix-ci-p "#+TODO:" s i) (prefix-ci-p "#+SEQ_TODO:" s i)
                          (prefix-ci-p "#+TYP_TODO:" s i))
                      (setf seen t)
                      (let* ((ws (words (subseq s (1+ (position #\: s)))))
                             (bar (position "|" ws :test #'string=))
                             (strip (lambda (w) (subseq w 0 (or (position #\( w) (length w))))))
                        (cond (bar (setf todo (append todo (mapcar strip (subseq ws 0 bar)))
                                         done (append done (mapcar strip (subseq ws (1+ bar))))))
                              (ws (setf todo (append todo (mapcar strip (butlast ws)))
                                        done (append done (mapcar strip (last ws))))))))
                     ((prefix-ci-p "#+STARTUP:" s i)
                      (setf startup (append startup (mapcar #'string-downcase
                                                            (words (subseq s (+ i 10))))))))))
    (if seen (values todo done startup) (values (list "TODO") (list "DONE") startup))))

(defun analyze (lines)
  "The DOC of LINES (a simple-vector of strings), computed once per vector."
  (or (gethash lines *analysis-cache*)
      (setf (gethash lines *analysis-cache*) (analyze-uncached lines))))

(defun analyze-uncached (lines)
  (multiple-value-bind (todo done startup) (file-keywords lines)
    (let* ((n (length lines))
           (kinds (make-array n)) (blocks (make-array n :initial-element nil))
           (head-of (make-array n :initial-element -1))
           (heads '()) (open nil) (current -1) (count 0))
      (dotimes (i n)
        (let* ((s (aref lines i)) (k (line-kind s)))
          (cond
            ((eq k :headline)             ; a headline ends any block (Org's rule, above)
             (setf open nil)
             (multiple-value-bind (level kw pri title tags ts) (parse-headline s :todo todo :done done)
               (push (make-headline :line i :level level :keyword kw :priority pri :title title
                                    :tags tags :title-start ts)
                     heads))
             (setf current count) (incf count))
            (open
             (setf (aref blocks i) open)
             (when (and (eq k :block-end)
                        (string-equal (block-name s (indent-of s) "#+end_") (first open)))
               (setf open nil))
             (unless (eq k :block-end) (setf k :block-body)))
            ((eq k :block-begin)
             (let* ((i0 (indent-of s)) (name (block-name s i0 "#+begin_"))
                    (args (words (subseq s (+ i0 8 (length name))))))
               (setf open (list name (and args (string-downcase (first args))) i)
                     (aref blocks i) open))))
          (setf (aref kinds i) k (aref head-of i) current)))
      (let ((hv (coerce (nreverse heads) 'simple-vector)))
        ;; each subtree ends at the next headline of the same or a higher level
        (loop for j from 0 below (length hv)
              do (let ((lv (head-level (aref hv j))))
                   (setf (head-end (aref hv j))
                         (or (loop for k from (1+ j) below (length hv)
                                   when (<= (head-level (aref hv k)) lv)
                                     return (head-line (aref hv k)))
                             n))))
        (make-doc :lines lines :kinds kinds :blocks blocks :heads hv :head-of head-of
                  :todo todo :done done :startup startup)))))

(defun block-name (s start prefix)
  (let* ((from (+ start (length prefix)))
         (to (or (position-if #'blank-char-p s :start from) (length s))))
    (string-downcase (subseq s from to))))

;;; ---- outline queries ---------------------------------------------------------------

(defun head-index-at (doc line)
  "The index of the headline whose section holds LINE (its own line included), or NIL
   before the first headline."
  (let ((k (if (< -1 line (length (doc-head-of doc))) (aref (doc-head-of doc) line) -1)))
    (and (>= k 0) k)))

(defun head-at (doc line)
  (let ((k (head-index-at doc line))) (and k (aref (doc-heads doc) k))))

(defun head-children (doc k)
  "Indices of headline K's direct children."
  (let* ((hv (doc-heads doc)) (h (aref hv k)) (out '()) (child-level nil))
    (loop for j from (1+ k) below (length hv)
          for c = (aref hv j)
          while (< (head-line c) (head-end h))
          do ;; the first deeper headline sets the children's level: Org allows a gap
             ;; (* then ***), and those are children too
             (unless child-level (setf child-level (head-level c)))
             (when (<= (head-level c) child-level) (setf child-level (head-level c)) (push j out)))
    (nreverse out)))

(defun top-heads (doc)
  "Indices of the headlines no other headline contains."
  (let ((hv (doc-heads doc)) (out '()) (end -1))
    (dotimes (j (length hv) (nreverse out))
      (when (>= (head-line (aref hv j)) end)
        (push j out) (setf end (head-end (aref hv j)))))))

(defun head-parent (doc k)
  (let* ((hv (doc-heads doc)) (h (aref hv k)))
    (loop for j from (1- k) downto 0
          for p = (aref hv j)
          when (and (< (head-level p) (head-level h)) (> (head-end p) (head-line h)))
            return j)))

(defun head-path (doc k)
  "The titles from the outermost ancestor down to headline K — how the desk names a
   heading, so a desk saved before the file was edited still finds it."
  (loop for j = k then (head-parent doc j)
        while j collect (head-title (aref (doc-heads doc) j)) into path
        finally (return (nreverse path))))

(defun find-head-by-path (doc path)
  "The index of the headline whose HEAD-PATH is PATH, or NIL."
  (loop for k below (length (doc-heads doc))
        when (equal (head-path doc k) path) return k))

(defun find-head-by-title (doc title)
  "The first headline titled TITLE (as Org's *Heading link search: the title, with
   keyword, priority and tags not part of it), or NIL."
  (loop for k below (length (doc-heads doc))
        when (string= (head-title (aref (doc-heads doc) k)) title) return k))

(defun trim-blank-tail (lines from to)
  "The end of [FROM, TO) with blank lines at its end left out, never below FROM."
  (loop while (and (> to from) (blank-line-p (aref lines (1- to)))) do (decf to))
  to)

(defun fold-end (doc k)
  "The first line AFTER what folding headline K hides: its subtree without the blank
   lines at its end. Measured in Emacs: folding \"** A1\" over \"a1 body\" and one
   blank line hides only the body; the blank line stays on screen."
  (let ((h (aref (doc-heads doc) k)))
    (trim-blank-tail (doc-lines doc) (1+ (head-line h)) (head-end h))))

(defun entry-end (doc k)
  "The first line after headline K's own ENTRY — the text before its first child —
   without trailing blank lines. What CONTENTS hides and CHILDREN shows."
  (let* ((hv (doc-heads doc)) (h (aref hv k))
         (next (if (< (1+ k) (length hv)) (min (head-line (aref hv (1+ k))) (head-end h)) (head-end h))))
    (trim-blank-tail (doc-lines doc) (1+ (head-line h)) next)))

(defun next-head (doc line &optional hidden)
  "The line of the first headline after LINE that is visible under HIDDEN, or NIL."
  (loop for h across (doc-heads doc)
        when (and (> (head-line h) line) (visible-line-p hidden (head-line h)))
          return (head-line h)))

(defun prev-head (doc line &optional hidden)
  (let ((best nil))
    (loop for h across (doc-heads doc)
          while (< (head-line h) line)
          when (visible-line-p hidden (head-line h)) do (setf best (head-line h)))
    best))

(defun siblings-of (doc k)
  (let ((p (head-parent doc k)))
    (if p (head-children doc p) (top-heads doc))))

(defun next-sibling (doc k)
  (second (member k (siblings-of doc k))))

(defun prev-sibling (doc k)
  (let ((sibs (siblings-of doc k)))
    (loop for (a b) on sibs when (eql b k) return a)))

;;; ---- 2. objects within a line ------------------------------------------------------
;;;
;;; Each object is a list (START END KIND . PLIST): KIND :link (with :target, and the
;;; description's span :desc-start / :desc-end — the text Org shows), :url (a bare
;;; https://…), and the emphasis kinds :bold :italic :underline :verbatim :code
;;; :strike. Emphasis follows Org's boundary rule loosely: the opening marker is at
;;; the line start or after a blank or one of -('"{, the next character is not blank;
;;; the closing one has no blank before it and is followed by the line end, a blank
;;; or one of -.,;:!?')}["\. Only within one line — Org allows a pair to span two.

(defparameter +emphasis+ '((#\* . :bold) (#\/ . :italic) (#\_ . :underline)
                           (#\= . :verbatim) (#\~ . :code) (#\+ . :strike)))

(defun line-links (s)
  "The [[target][description]] and [[target]] links of S, in order."
  (let ((out '()) (i 0))
    (loop
      (let ((a (search "[[" s :start2 i)))
        (unless a (return (nreverse out)))
        (let ((close (search "]]" s :start2 (+ a 2))))
          (unless close (return (nreverse out)))
          (let* ((mid (search "][" s :start2 (+ a 2) :end2 close))
                 (target (subseq s (+ a 2) (or mid close))))
            (push (list* a (+ close 2) :link :target target
                         (if mid
                             (list :desc-start (+ mid 2) :desc-end close)
                             (list :desc-start (+ a 2) :desc-end close)))
                  out)
            (setf i (+ close 2))))))))

(defun inside-any-p (i spans)
  (some (lambda (o) (and (<= (first o) i) (< i (second o)))) spans))

(defun line-urls (s taken)
  (let ((out '()) (i 0))
    (loop
      (let ((a (let ((h (search "http" s :start2 i))) h)))
        (unless a (return (nreverse out)))
        (let ((rest-ok (or (prefix-ci-p "https://" s a) (prefix-ci-p "http://" s a))))
          (if (and rest-ok (not (inside-any-p a taken))
                   (or (zerop a) (not (alphanumericp (char s (1- a))))))
              (let ((e (or (position-if (lambda (c) (or (blank-char-p c) (find c "]>)\"'")))
                                        s :start a)
                           (length s))))
                (loop while (and (> e a) (find (char s (1- e)) ".,;:!?")) do (decf e))
                (push (list a e :url :target (subseq s a e)) out)
                (setf i e))
              (setf i (+ a 4))))))))

(defun line-emphasis (s taken)
  (let ((out '()) (n (length s)) (i 0))
    (loop while (< i n)
          do (let* ((c (char s i)) (kind (cdr (assoc c +emphasis+))))
               (if (and kind (not (inside-any-p i taken))
                        (or (zerop i) (let ((p (char s (1- i)))) (or (blank-char-p p) (find p "-('\"{"))))
                        (< (1+ i) n) (not (blank-char-p (char s (1+ i)))))
                   (let ((j (loop for j from (+ i 2) below n
                                  when (and (char= (char s j) c)
                                            (not (blank-char-p (char s (1- j))))
                                            (or (= (1+ j) n)
                                                (let ((f (char s (1+ j))))
                                                  (or (blank-char-p f) (find f "-.,;:!?')}[\"\\")))))
                                    return j)))
                     (if (and j (not (inside-any-p j taken)))
                         (progn (push (list i (1+ j) kind) out) (setf i (1+ j)))
                         (incf i)))
                   (incf i))))
    (nreverse out)))

(defun line-objects (s)
  "S's objects, sorted by start, none overlapping. As Org reads a line left to right,
   the object that OPENS first wins: \"=[[file:x]]=\" is verbatim text, not a link
   (our docs write exactly that to show a link's syntax), and the * in \"[[*Top]]\"
   is not bold. On a tie a link wins. (Org also nests — a link inside bold — which
   this flat list does not: the outer object wins.)"
  (let* ((links (line-links s))
         (urls (line-urls s links))
         (emph (line-emphasis s '()))
         (rank (lambda (o) (case (third o) (:link 0) (:url 1) (t 2))))
         (all (sort (append links urls emph)
                    (lambda (a b) (or (< (first a) (first b))
                                      (and (= (first a) (first b))
                                           (< (funcall rank a) (funcall rank b)))))))
         (out '()) (end 0))
    (dolist (o all (nreverse out))
      (when (>= (first o) end)
        (push o out)
        (setf end (second o))))))

(defun link-at (s col)
  "The link or URL object of S under column COL (or just before it, so a caret right
   after the closing ]] still finds it), or NIL."
  (find-if (lambda (o) (and (member (third o) '(:link :url))
                            (<= (first o) col) (<= col (second o))))
           (line-objects s)))

(defun parse-link (target)
  "An Org link TARGET as a plist:
     (:type :file :path P [:search S])  file:P, file:P::S, ./P, /P, ~/P
     (:type :url :url U)                https:, http:, mailto:, ftp:
     (:type :heading :title T)          *Heading — a headline of this file
     (:type :custom-id :id I)           #id
     (:type :fuzzy :text T)             anything else without a scheme: Org looks for
                                        a <<target>>, then a headline with that title
   S, a search option, becomes (:heading T), (:line N) for a number, (:custom-id I)
   for #I, else (:text X) — the first line containing X, the rule check-links.sh
   checks."
  (labels ((search-option (s)
             (cond ((zerop (length s)) nil)
                   ((char= (char s 0) #\*) (list :heading (subseq s 1)))
                   ((char= (char s 0) #\#) (list :custom-id (subseq s 1)))
                   ((every #'digit-char-p s) (list :line (parse-integer s)))
                   (t (list :text s))))
           (file (path)
             (let ((sep (search "::" path)))
               (list* :type :file :path (subseq path 0 sep)
                      (and sep (list :search (search-option (subseq path (+ sep 2)))))))))
    (progn
      (cond
        ((prefix-ci-p "file:" target 0) (file (subseq target 5)))
        ((some (lambda (p) (prefix-ci-p p target 0)) '("https:" "http:" "mailto:" "ftp:"))
         (list :type :url :url target))
        ((and (plusp (length target)) (char= (char target 0) #\*))
         (list :type :heading :title (subseq target 1)))
        ((and (plusp (length target)) (char= (char target 0) #\#))
         (list :type :custom-id :id (subseq target 1)))
        ((or (prefix-ci-p "./" target 0) (prefix-ci-p "../" target 0) (prefix-ci-p "/" target 0)
             (prefix-ci-p "~/" target 0))
         (file target))
        (t (list :type :fuzzy :text target))))))

;;; ---- 3. folding --------------------------------------------------------------------
;;;
;;; What is hidden is a SIMPLE-BIT-VECTOR over the lines (1 = hidden), or NIL for
;;; nothing hidden. The text view draws only the 0s (doc/org.org §5). Every function
;;; here takes the bits and returns NEW bits: never mutated in place, so a caller can
;;; keep the old ones (and compare).
;;;
;;; The cycles are Org's, read in org-cycle.el and checked against Emacs 30.2 /
;;; Org 9.7.11 in batch mode (poc/org-test.lisp reproduces each answer):
;;;   TAB on a headline: FOLDED -> CHILDREN -> SUBTREE -> FOLDED. CHILDREN shows the
;;;     headline's own entry and its children's headlines, each folded. A headline
;;;     with no children goes FOLDED <-> SUBTREE. Moving on to SUBTREE needs the TAB
;;;     before it to have been this cycle (Emacs: last-command); otherwise any
;;;     unfolded state goes to FOLDED. An empty entry stays as it is.
;;;   S-TAB: OVERVIEW (the top-level headlines) -> CONTENTS (every headline) -> SHOW
;;;     ALL, each step needing the previous one to have been S-TAB; else OVERVIEW.
;;;     The text before the first headline always shows.

(defun visible-line-p (hidden line)
  (or (null hidden) (>= line (length hidden)) (zerop (sbit hidden line))))

(defun bits-for (doc hidden)
  (if hidden (copy-seq hidden)
      (make-array (length (doc-lines doc)) :element-type 'bit :initial-element 0)))

(defun set-range (bits from to value)
  (loop for i from (max 0 from) below (min to (length bits)) do (setf (sbit bits i) value))
  bits)

(defun normalize (bits)
  "NIL when nothing is hidden — what every view that does not fold carries."
  (and bits (find 1 bits) bits))

(defun subtree-state (doc hidden k)
  "Headline K's state under HIDDEN: :empty (nothing below it), :folded, :subtree
   (everything shown), or :children (anything between)."
  (let* ((h (aref (doc-heads doc) k)) (a (1+ (head-line h))) (b (fold-end doc k)))
    (cond ((>= a b) :empty)
          ((not (visible-line-p hidden a)) :folded)
          ((loop for i from a below b always (visible-line-p hidden i)) :subtree)
          (t :children))))

(defun cycle-subtree (doc hidden k &key repeat)
  "Org's TAB on headline K. REPEAT: the previous command was this same cycle.
   -> (values NEW-HIDDEN STATE) — STATE is what Emacs echoes: :folded :children
   :subtree :subtree-no-children :empty."
  (let* ((h (aref (doc-heads doc) k)) (a (1+ (head-line h))) (b (fold-end doc k))
         (state (subtree-state doc hidden k))
         (kids (head-children doc k)))
    (flet ((folded () (values (normalize (set-range (bits-for doc hidden) a b 1)) :folded))
           (subtree () (values (normalize (set-range (bits-for doc hidden) a b 0))
                               (if kids :subtree :subtree-no-children))))
      (cond
        ((eq state :empty) (values hidden :empty))
        ((eq state :folded)
         (if (null kids)
             (subtree)
             (let ((bits (set-range (bits-for doc hidden) a b 0)))
               (set-range bits a (entry-end doc k) 0)
               (dolist (c kids)
                 (let ((ch (aref (doc-heads doc) c)))
                   (set-range bits (1+ (head-line ch)) (fold-end doc c) 1)))
               (values (normalize bits) :children))))
        ((and repeat (eq state :children)) (subtree))
        (t (folded))))))

(defun overview-hidden (doc)
  (let ((bits (bits-for doc nil)))
    (dolist (k (top-heads doc) (normalize bits))
      (set-range bits (1+ (head-line (aref (doc-heads doc) k))) (fold-end doc k) 1))))

(defun contents-hidden (doc)
  (let ((bits (bits-for doc nil)))
    (dotimes (k (length (doc-heads doc)) (normalize bits))
      (set-range bits (1+ (head-line (aref (doc-heads doc) k))) (entry-end doc k) 1))))

(defun cycle-global (doc last)
  "Org's S-TAB. LAST: the state the previous command left, when that command was
   S-TAB too (else NIL). -> (values NEW-HIDDEN STATE), STATE :overview :contents :all."
  (case last
    (:overview (values (contents-hidden doc) :contents))
    (:contents (values nil :all))
    (t (values (overview-hidden doc) :overview))))

(defun startup-hidden (doc)
  "What #+STARTUP shows on opening: overview, content, showall, showeverything (the
   last one named wins, as each sets the same Org variable); with none, Org's default
   org-startup-folded, which is showeverything in Org 9.7.11."
  (let ((choice (find-if (lambda (w) (member w '("overview" "fold" "content" "showall"
                                                 "showeverything" "nofold")
                                             :test #'string=))
                         (reverse (doc-startup doc)))))
    (cond ((member choice '("overview" "fold") :test #'equal) (overview-hidden doc))
          ((equal choice "content") (contents-hidden doc))
          (t nil))))

(defun reveal-line (doc hidden line)
  "HIDDEN with LINE made visible the way Org reveals a match or a link target:
   every headline above it (its lineage) and the entry it is in. -> new bits."
  (if (visible-line-p hidden line)
      hidden
      (let ((bits (bits-for doc hidden)))
        (loop for k = (head-index-at doc line) then (head-parent doc k)
              for first = t then nil
              while k
              do (let ((h (aref (doc-heads doc) k)))
                   (setf (sbit bits (head-line h)) 0)
                   (when first (set-range bits (head-line h) (max (1+ line) (entry-end doc k)) 0))))
        (setf (sbit bits line) 0)
        (normalize bits))))

;;; ---- 4. structure edits ------------------------------------------------------------
;;;
;;; Each takes the line vector and a line index and returns (values NEW-LINES
;;; NEW-LINE) — the line the caret should go to — or signals ORG-EDIT-ERROR with
;;; Emacs's own message. Answers checked against Org 9.7.11 in batch mode.

(define-condition org-edit-error (error)
  ((message :initarg :message :reader org-edit-message))
  (:report (lambda (c s) (write-string (org-edit-message c) s))))

(defun edit-error (fmt &rest args)
  (error 'org-edit-error :message (apply #'format nil fmt args)))

(defun owning-head (doc line)
  (or (head-index-at doc line) (edit-error "Before first headline at position ~d" (1+ line))))

(defun replace-lines (lines from to new)
  "LINES with [FROM, TO) replaced by the sequence NEW."
  (concatenate 'simple-vector (subseq lines 0 from) new (subseq lines to)))

(defun change-level (lines line delta)
  (let* ((s (aref lines line))
         (level (position #\* s :test-not #'char=)))
    (when (and (minusp delta) (<= level 1))
      (edit-error "Cannot promote to level 0.  UNDO to recover if necessary"))
    (let ((new (copy-seq lines)))
      (setf (aref new line)
            (if (plusp delta)
                (concatenate 'string "*" s)
                (subseq s 1)))
      new)))

(defun promote-heading (lines line)
  "M-<left>: the headline the caret is in, one level up; its children stay."
  (let* ((doc (analyze lines)) (h (aref (doc-heads doc) (owning-head doc line))))
    (values (change-level lines (head-line h) -1) line)))

(defun demote-heading (lines line)
  "M-<right>: the headline one level down."
  (let* ((doc (analyze lines)) (h (aref (doc-heads doc) (owning-head doc line))))
    (values (change-level lines (head-line h) 1) line)))

(defun shift-subtree (lines line delta)
  (let* ((doc (analyze lines)) (k (owning-head doc line)) (h (aref (doc-heads doc) k)))
    (when (and (minusp delta) (<= (head-level h) 1))
      (edit-error "Cannot promote to level 0.  UNDO to recover if necessary"))
    (let ((new lines))
      (loop for j from k below (length (doc-heads doc))
            for c = (aref (doc-heads doc) j)
            while (< (head-line c) (head-end h))
            do (setf new (change-level new (head-line c) delta)))
      (values new line))))

(defun promote-subtree (lines line) "M-S-<left>." (shift-subtree lines line -1))
(defun demote-subtree (lines line) "M-S-<right>." (shift-subtree lines line 1))

(defun move-subtree (lines line dir)
  ;; Emacs moves a subtree WITH its trailing blank lines (org-end-of-subtree t t), so
  ;; "* A / a body / (blank)" moved below B keeps its blank line under it.
  (let* ((doc (analyze lines)) (k (owning-head doc line))
         (other (if (eq dir :up) (prev-sibling doc k) (next-sibling doc k))))
    (unless other (edit-error "Cannot move past superior level or buffer limit"))
    (let* ((hv (doc-heads doc))
           (first (if (eq dir :up) other k)) (second (if (eq dir :up) k other))
           (a (head-line (aref hv first))) (m (head-line (aref hv second)))
           (e (head-end (aref hv second)))
           (upper (subseq lines a m)) (lower (subseq lines m e))
           (offset (- line (head-line (aref hv k)))))
      (values (replace-lines lines a e (concatenate 'simple-vector lower upper))
              (+ offset (if (eq dir :up) a (+ a (length lower))))))))

(defun move-subtree-up (lines line) "M-<up>: past the previous sibling." (move-subtree lines line :up))
(defun move-subtree-down (lines line) "M-<down>." (move-subtree lines line :down))

(defun insert-heading (lines line col)
  "M-RET: a new heading (or list item) at the caret's level. -> (values NEW-LINES
   LINE COL), the caret where the new title is typed.
   - On a list item: a new item with the same bullet and indentation, the rest of the
     line after the caret moved into it.
   - At column 0 of a headline: a sibling BEFORE it (Org: \"if point is at the
     beginning of a headline, insert a sibling before\").
   - In a headline's stars/keyword: a sibling right after the headline line, empty.
   - Elsewhere: the line is split at the caret and the rest becomes the new title —
     what Org does with org-M-RET-may-split-line at its default.
   Org's org-blank-before-new-entry is AUTO: we put a blank line before the new
   heading when the headline whose level we copy has one before it (the observed
   case); Emacs's heuristic also looks at other entries, and its exact answer is not
   reproduced (doc/org.org)."
  (let* ((doc (analyze lines)) (s (aref lines line))
         (bullet (and (not (headline-line-p s)) (item-bullet-end s))))
    (if bullet
        (let* ((ind (indent-of s)) (prefix (subseq s ind bullet))
               (prefix (if (digit-char-p (char prefix 0)) (renumber prefix) prefix))
               (c (max col bullet)))
          (values (replace-lines lines line (1+ line)
                                 (vector (string-right-trim '(#\Space) (subseq s 0 c))
                                         (concatenate 'string (subseq s 0 ind) prefix (subseq s c))))
                  (1+ line) (+ ind (length prefix))))
        (let* ((k (head-index-at doc line))
               (h (and k (aref (doc-heads doc) k)))
               (level (if h (head-level h) 1))
               (stars (concatenate 'string (make-string level :initial-element #\*) " "))
               (blank-before (and h (plusp (head-line h))
                                  (blank-line-p (aref lines (1- (head-line h))))))
               (blank (if blank-before (list "") '())))
          (cond
            ((and h (= line (head-line h)) (zerop col))
             (values (replace-lines lines line line (append (list stars) blank))
                     line (length stars)))
            ((and h (= line (head-line h)) (< col (head-title-start h)))
             (values (replace-lines lines (1+ line) (1+ line) (append blank (list stars)))
                     (+ line 1 (length blank)) (length stars)))
            (t
             (let ((c (min col (length s))))
               (values (replace-lines lines line (1+ line)
                                      (append (list (string-right-trim '(#\Space #\Tab) (subseq s 0 c)))
                                              blank
                                              (list (concatenate 'string stars (string-left-trim '(#\Space #\Tab) (subseq s c))))))
                       (+ line 1 (length blank)) (length stars)))))))))

(defun renumber (prefix)
  "\"3. \" -> \"4. \": the next counter of a numbered bullet."
  (let* ((e (position-if-not #'digit-char-p prefix)))
    (format nil "~d~a" (1+ (parse-integer prefix :end e)) (subseq prefix e))))

(defun cycle-todo (lines line)
  "C-c C-t: the headline's keyword to the next one of the file's sequence (TODO ->
   DONE -> none, by default). -> (values NEW-LINES LINE NEW-KEYWORD)."
  (let* ((doc (analyze lines)) (k (owning-head doc line)) (h (aref (doc-heads doc) k))
         (seq (append (doc-todo doc) (doc-done doc)))
         (next (if (head-keyword h)
                   (second (member (head-keyword h) seq :test #'string=))
                   (first seq)))
         (s (aref lines (head-line h)))
         (stars (head-level h))
         (rest (subseq s (head-title-start h)))
         (prio (if (head-priority h) (format nil "[#~c] " (head-priority h)) ""))
         (new (copy-seq lines)))
    ;; head-title-start is past the keyword AND the priority; rebuild both
    (setf (aref new (head-line h))
          (format nil "~a ~@[~a ~]~a~a" (make-string stars :initial-element #\*) next prio rest))
    (values new line next)))
