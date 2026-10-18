;;;; org-mode.lisp — Org files in the environment: colour, the outline, links, keys.
;;;;
;;;; The structure itself is pure and lives in poc/org.lisp (LOL.ORG). This file is the
;;;; shell's side of it, in the order doc/org.org §4–6 describes:
;;;;
;;;;   - the COLOURER: :syntax :org, registered with the text view (lol.textview's
;;;;     *SYNTAXES*), so any view of an .org file — the editor, a preview — reads as
;;;;     Org: headlines by level, keywords, tags, links, code, and Lisp inside a
;;;;     #+begin_src lisp block by the source lexer itself;
;;;;   - the OUTLINE as a subject, SUBJ-ORG: a file's headings are a drill-down, the
;;;;     nav split previews a heading's section, a heading drills into its children,
;;;;     its links and "Edit here"; the Spotter's "headings" source;
;;;;   - LINKS: C-c C-o (and a link row) opens what Org would open;
;;;;   - the KEYS of an Org file pane (ORG-KEY, offered every key first by
;;;;     EDIT-PANE-COMMAND): TAB / S-TAB folding, heading motion, M-RET, the M-arrows,
;;;;     C-c C-t, C-c C-j, C-c C-l.
;;;;
;;;; Every behaviour below that Org has, it has as Org does — checked against Emacs
;;;; 30.2 with its Org 9.7.11 in batch mode; the pure part's answers are asserted by
;;;; poc/org-test.lisp, the keys by check-org.lisp in the guest.

(in-package :cl-user)

;; The outline subject (below, "the outline as a subject") — first, as the pane
;; predicates read it.
(defstruct (subj-org (:constructor subj-org (path &optional heading))) path heading)

;;; ---- which panes are Org ------------------------------------------------------------

(defun org-pane-p (pane)
  "Does PANE show an Org text (an .org file, or a section of one)?"
  (and pane (pane-tv pane) (eq (view-syntax (pane-view pane)) :org)))

(defun org-pane-path (pane)
  "The file an Org pane shows, or NIL."
  (let ((d (view-data (pane-view pane))))
    (or (and (listp d) (getf d :path))
        (let ((s (pane-subject pane)))
          (cond ((subj-file-p s) (subj-file-path s))
                ((subj-org-p s) (subj-org-path s)))))))

;;; ---- the colourer ------------------------------------------------------------------
;;;
;;; Runs are (start end . colour), covering the whole line (the text view draws only
;;; what a run covers). The palette is the semantic one (poc/widgets.lisp): a heading
;;; that contains others is gold like a package or a folder, the deeper levels take the
;;; next hues; a link is blue, the colour of a link everywhere; nothing is drawn in the
;;; accent, which marks focus.

(defparameter +org-level-colours+ (list !#FFCB6B !#89DDFF !#C792EA !#A8FF78)
  "Headline colours by level, cycling after the fourth (Org's own org-level-N faces
   cycle the same way, through eight).")
(defparameter *org-link*   !#82AAFF "a link's text")
(defparameter *org-todo*   !#FF6B6B "a not-done keyword: TODO")
(defparameter *org-done*   !#A8FF78 "a done keyword: DONE")
(defparameter *org-bold*   !#FFFFFF)
(defparameter *org-italic* !#B4BAC6)

(defvar *org-lisp-states* (make-hash-table :test 'eq :weakness :key)
  "Line vector -> per line, the Lisp lexer's (DEPTH . IN-STRING) entering it, for the
   lines of a Lisp src block. Computed once per vector, like the outline.")

(defparameter +lisp-block-languages+ '("lisp" "common-lisp" "elisp" "emacs-lisp" "scheme"))

(defun org-lisp-states (doc)
  (let ((lines (lol.org:doc-lines doc)))
    (or (gethash lines *org-lisp-states*)
        (setf (gethash lines *org-lisp-states*)
              (let ((v (make-array (length lines) :initial-element nil))
                    (depth 0) (instr nil))
                (dotimes (i (length lines) v)
                  (let ((b (aref (lol.org:doc-blocks doc) i)))
                    (cond ((and b (= i (third b))) (setf depth 0 instr nil))   ; #+begin
                          ((and b (equal (first b) "src")
                                (member (second b) +lisp-block-languages+ :test #'equal)
                                (eq (aref (lol.org:doc-kinds doc) i) :block-body))
                           (setf (aref v i) (cons depth instr))
                           (multiple-value-bind (r d s) (lol.textview::lex-line (aref lines i) depth instr)
                             (declare (ignore r)) (setf depth d instr s)))))))))))

(defun level-colour (level)
  (nth (mod (1- level) (length +org-level-colours+)) +org-level-colours+))

(defun object-runs (line start end base)
  "Runs for LINE[START, END) in BASE, with its links and emphasis in their colours."
  (let ((runs '()) (at start))
    (flet ((run (a b c) (when (< a b) (push (list* a b c) runs))))
      (dolist (o (lol.org:line-objects line))
        (destructuring-bind (a b kind &rest plist) o
          (when (and (>= a at) (<= b end))
            (run at a base)
            (case kind
              (:link (let ((ds (getf plist :desc-start)) (de (getf plist :desc-end)))
                       (run a ds lol.textview::*c-comment*)
                       (run ds de *org-link*)
                       (run de b lol.textview::*c-comment*)))
              (:url (run a b *org-link*))
              (:bold (run a b *org-bold*))
              (:italic (run a b *org-italic*))
              (:verbatim (run a b lol.textview::*c-string*))
              (:code (run a b lol.textview::*c-operator*))
              (:strike (run a b lol.textview::*c-comment*))
              (t (run a b base)))
            (setf at b))))
      (run at end base))
    (nreverse runs)))

(defun org-runs (lines li line)
  "The colourer for :syntax :org (registered below)."
  (let* ((doc (lol.org:analyze lines))
         (kind (if (< li (length lines)) (aref (lol.org:doc-kinds doc) li) :paragraph))
         (n (length line))
         (dim lol.textview::*c-comment*)
         (text lol.textview::*c-text*))
    (flet ((whole (c) (list (list* 0 n c))))
      (case kind
        (:headline
         (let ((h (lol.org:head-at doc li)))
           (if (not (and h (= (lol.org:head-line h) li)))
               (whole text)
               (let* ((lv (lol.org:head-level h)) (ts (lol.org:head-title-start h))
                      (colour (level-colour lv))
                      (kw (lol.org:head-keyword h))
                      (title-end (or (lol.org::tags-start line ts) n)))
                 (append (list (list* 0 lv dim))
                         (let ((runs '()) (at lv))
                           (when kw
                             (let ((k (search kw line :start2 lv)))
                               (when k
                                 (push (list* at k colour) runs)
                                 (push (list* k (+ k (length kw))
                                              (if (member kw (lol.org:doc-done doc) :test #'string=)
                                                  *org-done* *org-todo*))
                                       runs)
                                 (setf at (+ k (length kw))))))
                           (when (< at ts)          ; the priority cookie, or just the blank before the title
                             (push (list* at ts (if (lol.org:head-priority h) lol.textview::*c-number* colour)) runs)
                             (setf at ts))
                           (nreverse runs))
                         (object-runs line (min ts title-end) title-end colour)
                         (when (< title-end n) (list (list* title-end n dim))))))))
        ((:keyword)
         (let* ((colon (position #\: line)) (cut (if colon (1+ colon) n))
                (title-p (search "#+title:" (string-downcase line))))
           (list (list* 0 cut dim)
                 (list* cut n (if title-p lol.textview::*c-defname* text)))))
        ((:block-begin :block-end :drawer :drawer-end :rule :comment) (whole dim))
        (:fixed (whole lol.textview::*c-string*))
        (:block-body
         (let ((state (aref (org-lisp-states doc) li)))
           (if state
               (values (lol.textview::lex-line line (car state) (cdr state)))
               (whole (if (equal (first (aref (lol.org:doc-blocks doc) li)) "quote")
                          text lol.textview::*c-string*)))))
        (:table
         (let ((runs '()) (at 0))
           (dotimes (i n)
             (when (find (char line i) "|+")
               (when (< at i) (push (list* at i text) runs))
               (push (list* i (1+ i) dim) runs) (setf at (1+ i))))
           (when (< at n) (push (list* at n text) runs))
           (nreverse runs)))
        (:item
         (let ((b (lol.org::item-bullet-end line)))
           (cons (list* 0 b lol.textview::*c-number*) (object-runs line b n text))))
        (t (object-runs line 0 n text))))))

(setf (gethash :org lol.textview:*syntaxes*) 'org-runs)

;;; ---- the outline as a subject ------------------------------------------------------
;;;
;;; (subj-org PATH) is a file's outline: its top headings as rows. (subj-org PATH
;;; TITLES) is one heading, named by the chain of titles from the top — how the desk
;;; saves it, so a heading is found again after lines were added above it. Presenting
;;; it lists its children, its links and "Edit here"; its PREVIEW (the nav split's
;;; look at a selected row) is the section's own text, coloured, read-only — so moving
;;; down an outline READS the document beside it, and drilling goes deeper.

(defvar *org-file-cache* '() "((PATH MTIME . LINES) …): the last few files read.")

(defun org-file-lines (path)
  "PATH's lines as a simple-vector, read once per modification time. An open file
   pane's unsaved edits are NOT seen here: the outline is of the file on disk."
  (let* ((native (sb-ext:parse-native-namestring path))
         (mtime (ignore-errors (file-write-date native)))
         (hit (assoc path *org-file-cache* :test #'string=)))
    (if (and hit mtime (eql (cadr hit) mtime))
        (cddr hit)
        (let ((lines (with-open-file (s native :external-format '(:utf-8 :replacement #\?))
                       (coerce (loop for l = (read-line s nil) while l collect l) 'simple-vector))))
          (setf *org-file-cache*
                (cons (list* path mtime lines)
                      (subseq (remove path *org-file-cache* :key #'car :test #'string=)
                              0 (min 4 (length *org-file-cache*)))))
          lines))))

(defun org-heading-label (h)
  (format nil "~@[~a ~]~a" (lol.org:head-keyword h) (lol.org:head-title h)))

(defun org-heading-items (path doc indices parent-titles group)
  (mapcar (lambda (k)
            (let* ((h (aref (lol.org:doc-heads doc) k))
                   (kids (lol.org:head-children doc k)))
              (make-item :label (org-heading-label h)
                         :detail (format nil "~@[~d sections  ~]~@[:~{~a~^:~}:  ~]line ~d"
                                         (and kids (length kids)) (lol.org:head-tags h)
                                         (1+ (lol.org:head-line h)))
                         :kind (and kids :directory)  ; it contains: gold, like a folder
                         :group group
                         :subject (subj-org path (append parent-titles
                                                         (list (lol.org:head-title h)))))))
          indices))

(defun org-link-items (path lines from to)
  "The links in LINES[FROM, TO) as rows that open them."
  (loop for i from from below to
        append (loop for o in (lol.org:line-objects (aref lines i))
                     when (member (third o) '(:link :url))
                       collect (let* ((plist (cdddr o)) (target (getf plist :target))
                                      (desc (subseq (aref lines i) (or (getf plist :desc-start) (first o))
                                                    (or (getf plist :desc-end) (second o)))))
                                 (make-item :label desc :detail target :kind :restart :group "links"
                                            :action (let ((tg target))
                                                      (lambda () (org-follow-link tg path))))))))

(defun org-top-indices (doc) (lol.org::top-heads doc))

(defmethod present ((s subj-org))
  (let* ((path (subj-org-path s))
         (lines (handler-case (org-file-lines path)
                  (error (c) (return-from present
                               (make-view :title (format nil "outline ~a" (path-name path)) :subject s
                                          :items (list (make-item :label (format nil "(cannot read: ~a)" c))))))))
         (doc (lol.org:analyze lines)))
    (if (null (subj-org-heading s))
        (make-view :title (format nil "outline ~a — ~d headings" (path-name path)
                                  (length (lol.org:doc-heads doc)))
                   :subject s
                   :items (append
                           (list (make-item :label "Edit the file" :detail (abbreviate-home path)
                                            :group "file" :action (lambda () (org-edit-at path 0))))
                           (org-heading-items path doc (org-top-indices doc) '() "headings")))
        (let ((k (lol.org:find-head-by-path doc (subj-org-heading s))))
          (if (null k)
              (make-view :title (format nil "heading ~a" (car (last (subj-org-heading s)))) :subject s
                         :items (list (make-item :label "(this heading is no longer in the file)")
                                      (make-item :label "the outline" :subject (subj-org path))))
              (let* ((h (aref (lol.org:doc-heads doc) k)))
                (make-view :title (format nil "heading ~a" (lol.org:head-title h))
                           :subject s
                           :items (append
                                   (list (make-item :label "Edit here"
                                                    :detail (format nil "~a, line ~d" (path-name path)
                                                                    (1+ (lol.org:head-line h)))
                                                    :action (let ((line (lol.org:head-line h)))
                                                              (lambda () (org-edit-at path line)))))
                                   (org-heading-items path doc (lol.org:head-children doc k)
                                                      (subj-org-heading s) "sections")
                                   (org-link-items path lines (lol.org:head-line h)
                                                   (lol.org:entry-end doc k))))))))))

(defparameter +org-preview-lines+ 400 "A section preview shows at most this many lines.")

(defmethod present-preview ((s subj-org))
  (let* ((path (subj-org-path s))
         (lines (ignore-errors (org-file-lines path)))
         (doc (and lines (lol.org:analyze lines)))
         (k (and doc (subj-org-heading s) (lol.org:find-head-by-path doc (subj-org-heading s))))
         (from (if k (lol.org:head-line (aref (lol.org:doc-heads doc) k)) 0))
         (to (if k (lol.org:head-end (aref (lol.org:doc-heads doc) k)) (length (or lines #())))))
    (if (null lines)
        (present s)
        (make-view :title (format nil "~a, line ~d" (path-name path) (1+ from))
                   :kind :reference :syntax :org :subject s
                   :text (format nil "~{~a~^~%~}"
                                 (coerce (subseq lines from (min to (+ from +org-preview-lines+))) 'list))))))

(defmethod commands-for ((s subj-org))
  (let ((path (subj-org-path s)))
    (list (make-command :label "Open the file" :surfaces '(:menu :halo)
                        :action (lambda () (org-edit-at path (or (org-subject-line s) 0))))
          (make-command :label "The whole outline" :surfaces '(:menu)
                        :target (subj-org path) :disposition :new-trail))))

(defun org-subject-line (s)
  (let* ((lines (ignore-errors (org-file-lines (subj-org-path s))))
         (doc (and lines (lol.org:analyze lines)))
         (k (and doc (subj-org-heading s) (lol.org:find-head-by-path doc (subj-org-heading s)))))
    (and k (lol.org:head-line (aref (lol.org:doc-heads doc) k)))))

;;; An .org file pane, or an .org row in the file manager, offers its outline.
(defmethod commands-for :around ((f subj-file))
  (let ((cmds (call-next-method)))
    (if (org-file-p (path-name (subj-file-path f)))
        (cons (make-command :label "Outline" :key "C-c C-j" :surfaces '(:menu :halo)
                            :target (subj-org (subj-file-path f)) :disposition :new-trail)
              cmds)
        cmds)))

(setf +title-kind-words+ (append +title-kind-words+ '("outline" "heading")))

;;; The Spotter's "headings": every heading of the Org files open in a tab.
(defun org-open-paths ()
  (let ((out '()))
    (dolist (trail *trails* (nreverse out))
      (dolist (p trail)
        (when (org-pane-p p)
          (let ((path (org-pane-path p))) (when path (pushnew path out :test #'string=))))))))

(defun org-spotter-candidates ()
  (loop for path in (org-open-paths)
        append (let* ((lines (ignore-errors (org-file-lines path)))
                      (doc (and lines (lol.org:analyze lines))))
                 (when doc
                   (loop for k below (length (lol.org:doc-heads doc))
                         collect (let ((h (aref (lol.org:doc-heads doc) k)))
                                   (list :label (lol.org:head-title h)
                                         :detail (format nil "~a, line ~d" (path-name path)
                                                         (1+ (lol.org:head-line h)))
                                         :group "headings"
                                         :kind (and (lol.org:head-children doc k) :directory)
                                         :subject (subj-org path (lol.org:head-path doc k)))))))))

(pushnew 'org-spotter-candidates *spotter-sources*)

;;; ---- opening a file at a line, and links --------------------------------------------

(defun org-pane-for (path)
  "An open pane editing PATH, or NIL."
  (dolist (trail *trails*)
    (dolist (p trail)
      (when (and (pane-tv p) (eq (view-kind (pane-view p)) :file) (equal (org-pane-path p) path))
        (return-from org-pane-for p)))))

(defun org-goto-line (tv line &key (context 2))
  "Put TV's caret on LINE (folds opening on the way) with LINE near the top."
  (lol.textview:tv-move-point tv line 0)
  (setf (lol.textview::tv-scroll tv)
        (max 0 (- (lol.textview::line-row tv (lol.textview:tv-point-line tv)) context))))

(defun org-edit-at (path line)
  "Open PATH in an editor (a new tab — or the tab already editing it) on LINE."
  (let ((p (org-pane-for path)))
    (if p
        (let ((n (position-if (lambda (trail) (member p trail)) *trails*)))
          (activate-trail n))
        (new-trail (subj-file path)))
    (let ((tv (pane-tv (or p (current-pane)))))
      (when tv (org-goto-line tv line)))))

(defun resolve-relative (path base-file)
  "PATH (from a link in BASE-FILE) as an absolute path."
  (cond ((zerop (length path)) base-file)
        ((char= (char path 0) #\/) (normalize-path path))
        ((and (> (length path) 1) (string= "~/" path :end2 2))
         (normalize-path (path-join (home-directory) (subseq path 2))))
        (t (normalize-path (path-join (path-parent base-file) path)))))

(defun search-line (lines search)
  "The line a link's search option names in LINES, or NIL."
  (when search
    (ecase (first search)
      (:line (max 0 (min (1- (second search)) (1- (length lines)))))
      (:heading (let* ((doc (lol.org:analyze lines)) (k (lol.org:find-head-by-title doc (second search))))
                  (and k (lol.org:head-line (aref (lol.org:doc-heads doc) k)))))
      (:custom-id (position-if (lambda (s) (search (format nil ":CUSTOM_ID: ~a" (second search)) s)) lines))
      (:text (position-if (lambda (s) (search (second search) s)) lines)))))

(defun registry-definition (text)
  "For a link search like \"defun bring-up-interface\": the symbol the registry has a
   definition of by that name, when exactly one does — so the link opens the LIVE
   definition (its source, Accept, Senders) rather than a line of a file."
  (let ((sp (position #\Space text)))
    (when (and sp (> (length text) 3) (string-equal "def" text :end2 3))
      (let* ((name (string-trim " ()" (subseq text (1+ sp))))
             (hits '()))
        (maphash (lambda (sym defs) (declare (ignore defs))
                   (when (and (symbolp sym) (string-equal (symbol-name sym) name)) (push sym hits)))
                 *registry*)
        (and (= 1 (length hits)) (first hits))))))

(defun org-follow-link (target base-file &optional pane)
  "Open what the Org link TARGET names, as Org would (doc/org.org §4.3). BASE-FILE is
   the file the link is in; PANE, when the link is in an open pane, is where a link
   into the same file moves the caret."
  (let ((link (lol.org:parse-link target)))
    (flet ((here (line)
             (if line
                 (if pane (org-goto-line (pane-tv pane) line) (org-edit-at base-file line))
                 (setf *browser-status* (format nil "link: no ~a in this file" target))))
           (lines-of (p) (if (and pane (equal p (org-pane-path pane)))
                             (lol.textview:tv-lines (pane-tv pane))
                             (org-file-lines p))))
      (case (getf link :type)
        (:url (if (fboundp 'web-open)
                  (funcall 'web-open (getf link :url))
                  (setf *browser-status* (format nil "link: no web browser here for ~a" (getf link :url)))))
        (:heading (here (search-line (lines-of base-file) (list :heading (getf link :title)))))
        (:custom-id (here (search-line (lines-of base-file) (list :custom-id (getf link :id)))))
        (:fuzzy (let ((lines (lines-of base-file)))
                  (here (or (search-line lines (list :text (format nil "<<~a>>" (getf link :text))))
                            (search-line lines (list :heading (getf link :text)))
                            (search-line lines (list :text (getf link :text)))))))
        (:file
         (let* ((path (resolve-relative (getf link :path) base-file))
                (search (getf link :search)))
           (cond
             ((not (path-exists-p path))
              (setf *browser-status* (format nil "link: no such file ~a" (abbreviate-home path))))
             ((eq (followed-type path) :directory) (new-trail (subj-dir path)))
             ((and (eq (first search) :text) (registry-definition (second search))
                   (string-equal (file-extension (path-name path)) "lisp"))
              (new-trail (subj-defn (registry-definition (second search)))))
             ((equal path base-file)
              (here (if search (search-line (lines-of path) search) 0)))
             (t (let ((line (and search (ignore-errors (search-line (org-file-lines path) search)))))
                  (when (and search (null line))
                    (setf *browser-status* (format nil "link: ~a not found in ~a"
                                                   (second search) (path-name path))))
                  (org-edit-at path (or line 0)))))))))))

;;; ---- folding in a pane -------------------------------------------------------------
;;;
;;; The pane's text view holds the hidden-line bits (poc/textview.lisp, "Hidden lines");
;;; LOL.ORG computes them. Org's TAB cycle needs to know whether the previous command
;;; was the same cycle (Emacs: last-command), which *ORG-LAST* remembers: every key an
;;; Org pane gets passes ORG-KEY first, which forgets it unless the key is the next TAB.

(defvar *org-last* nil "(KIND TV STATE) of the last fold command, while it is the last command.")

(defun org-reveal-fn (tv line)
  (lol.org:reveal-line (lol.org:analyze (lol.textview:tv-lines tv)) (lol.textview::tv-hidden tv) line))

(defun org-prepare-pane (pane)
  "Set a freshly opened Org pane up: Org's way of revealing, and #+STARTUP's folds."
  (let ((tv (pane-tv pane)))
    (when (and tv (not (lol.textview::tv-reveal tv)))
      (setf (lol.textview::tv-reveal tv) 'org-reveal-fn)
      (when (eq (view-kind (pane-view pane)) :file)
        (let ((bits (lol.org:startup-hidden (lol.org:analyze (lol.textview:tv-lines tv)))))
          (when bits (setf (lol.textview::tv-hidden tv) bits)))))))

(defparameter +org-state-words+
  '((:folded . "FOLDED") (:children . "CHILDREN") (:subtree . "SUBTREE")
    (:subtree-no-children . "SUBTREE (NO CHILDREN)") (:empty . "EMPTY ENTRY")
    (:overview . "OVERVIEW") (:contents . "CONTENTS") (:all . "SHOW ALL"))
  "What Emacs echoes after each fold command.")

(defun org-cycle-at-point (tv)
  "TAB on a headline: Org's subtree cycle."
  (let* ((doc (lol.org:analyze (lol.textview:tv-lines tv)))
         (k (lol.org:head-index-at doc (lol.textview:tv-point-line tv)))
         (repeat (and (eq (first *org-last*) :cycle) (eq (second *org-last*) tv))))
    (multiple-value-bind (bits state) (lol.org:cycle-subtree doc (lol.textview::tv-hidden tv) k :repeat repeat)
      (setf (lol.textview::tv-hidden tv) bits
            *browser-status* (cdr (assoc state +org-state-words+))
            *org-last* (list :cycle tv state)))))

(defun org-cycle-global (tv)
  "S-TAB: the buffer's cycle."
  (let* ((doc (lol.org:analyze (lol.textview:tv-lines tv)))
         (last (and (eq (first *org-last*) :global) (eq (second *org-last*) tv) (third *org-last*))))
    (multiple-value-bind (bits state) (lol.org:cycle-global doc last)
      (setf (lol.textview::tv-hidden tv) bits
            *browser-status* (cdr (assoc state +org-state-words+))
            *org-last* (list :global tv state)))))

;;; ---- the keys ----------------------------------------------------------------------

(defun org-structure-edit (tv fn &rest args)
  "Install what the LOL.ORG edit FN returns, or show its message."
  (handler-case
      (multiple-value-bind (lines line col) (apply fn (lol.textview:tv-lines tv) args)
        (lol.textview:tv-replace tv lines line (or col (lol.textview:tv-point-col tv)))
        t)
    (lol.org:org-edit-error (e) (setf *browser-status* (princ-to-string e)) t)))

(defun org-heading-motion (tv how)
  "C-c C-n / C-p / C-f / C-b / C-u, as Emacs moves (checked in batch mode)."
  (let* ((lines (lol.textview:tv-lines tv)) (doc (lol.org:analyze lines))
         (line (lol.textview:tv-point-line tv)) (bits (lol.textview::tv-hidden tv))
         (k (lol.org:head-index-at doc line))
         (heads (lol.org:doc-heads doc))
         (target
           (ecase how
             (:next (lol.org:next-head doc line bits))
             (:prev (lol.org:prev-head doc line bits))
             (:forward (let ((s (and k (lol.org:next-sibling doc k))))
                         (and s (lol.org:head-line (aref heads s)))))
             (:backward (let ((s (and k (lol.org:prev-sibling doc k))))
                          (and s (lol.org:head-line (aref heads s)))))
             (:up (cond ((null k) (setf *browser-status* "Before first heading") nil)
                        ((lol.org:head-parent doc k) (lol.org:head-line (aref heads (lol.org:head-parent doc k))))
                        (t (setf *browser-status* "Already at top level of the outline")
                           (lol.org:head-line (aref heads k))))))))
    (when target
      (lol.textview:tv-move-point tv target 0))
    t))

(defun org-goto (tv)
  "C-c C-j: a heading by name, in the minibuffer — Tab completes, Enter jumps."
  (let* ((doc (lol.org:analyze (lol.textview:tv-lines tv)))
         (titles (map 'list #'lol.org:head-title (lol.org:doc-heads doc))))
    (flet ((matches (text) (remove-if-not (lambda (s) (search text s :test #'char-equal)) titles)))
      (minibuffer-read "Goto heading: "
                       (lambda (text)
                         (let* ((k (or (lol.org:find-head-by-title doc text)
                                       (position (first (matches text)) titles :test #'equal))))
                           (if k
                               (org-goto-line tv (lol.org:head-line (aref (lol.org:doc-heads doc) k)))
                               (setf *browser-status* (format nil "no heading matches ~s" text)))))
                       :completer (lambda (text)
                                    (let ((m (remove-if-not (lambda (s) (and (>= (length s) (length text))
                                                                              (string-equal text s :end2 (length text))))
                                                            titles)))
                                      (cond ((null m) nil)
                                            ((null (cdr m)) (first m))
                                            (t (subseq (first m) 0
                                                       (reduce #'min (mapcar (lambda (s) (or (mismatch (first m) s :test #'char-equal) (length s))) m)))))))
                       :hint (format nil "~d headings" (length titles))))))

(defun org-insert-link (tv)
  "C-c C-l: a link at the caret — the target, then its description."
  (minibuffer-read "Link: "
                   (lambda (target)
                     (unless (zerop (length target))
                       (minibuffer-read "Description: "
                                        (lambda (desc)
                                          (with-undo (tv)
                                            (editor-insert tv (if (zerop (length desc))
                                                                  (format nil "[[~a]]" target)
                                                                  (format nil "[[~a][~a]]" target desc)))))
                                        :hint "empty for none")))
                   :hint "file:path, file:path::*Heading, *Heading, https://…"))

(defun org-guard-hidden (tv key state)
  "Org's 'smart' rule for an edit next to folded text (org-fold-catch-invisible-edits,
   default in Org 9.7.11): never delete a hidden character — so Delete / C-d / C-k at
   the end of a folded line, and Backspace at the start of the line after a fold, open
   the fold and are refused once, saying why; and Enter at the end of a folded
   headline opens it first (a new line between a headline and its hidden body would
   break the fold). Typing on the folded line itself goes through, as in Org.
   -> T when the key was consumed."
  (let* ((line (lol.textview:tv-point-line tv)) (col (lol.textview:tv-point-col tv))
         (eol (= col (length (aref (lol.textview:tv-lines tv) line))))
         (ctrl (lol.canvas:ctrl-p state)))
    (flet ((open-fold (at)
             (lol.textview:tv-show-line tv at)
             ;; the whole subtree, not just its first line
             (let* ((doc (lol.org:analyze (lol.textview:tv-lines tv))) (k (lol.org:head-index-at doc line)))
               (when k (setf (lol.textview::tv-hidden tv)
                             (lol.org::set-range (lol.org::bits-for doc (lol.textview::tv-hidden tv))
                                                 line (lol.org:fold-end doc k) 0))))))
      (cond
        ((and eol (lol.textview::tv-folded-after-p tv line)
              (or (eq key :delete) (and ctrl (member key '(#\d #\k)))))
         (open-fold (1+ line))
         (setf *browser-status* "That would delete folded text: the fold is open now — press again")
         t)
        ((and (eq key :backspace) (zerop col) (not ctrl) (plusp line)
              (lol.textview::hidden-line-p tv (1- line)))
         (open-fold (1- line))
         (setf *browser-status* "That would join this line onto folded text: open now — press again")
         t)
        ((and (eq key :return) eol (zerop state) (lol.textview::tv-folded-after-p tv line))
         (open-fold (1+ line))
         nil)                                   ; then the newline happens as usual
        (t nil)))))

(defun org-key (p key state)
  "An Org pane's own keys, offered every key first (EDIT-PANE-COMMAND). -> T when it
   took the key; NIL lets the editor have it."
  (when (org-pane-p p)
    (org-prepare-pane p)
    (let* ((tv (pane-tv p))
           (ctrl (lol.canvas:ctrl-p state)) (meta (lol.canvas:meta-p state))
           (shift (lol.canvas:shift-p state))
           (line (lol.textview:tv-point-line tv))
           (on-head (lol.org:headline-line-p (aref (lol.textview:tv-lines tv) line)))
           (last *org-last*))
      (setf *org-last* nil)                     ; only the very next TAB repeats a cycle
      (cond
        ;; C-c <key>: the Org chords (the C-c itself set the prefix in the shell)
        ((and (eq *pending-prefix* :c-c) ctrl (characterp key)
              (member (char-downcase key) '(#\n #\p #\f #\b #\u #\j #\o #\l #\t)))
         (setf *pending-prefix* nil)
         (case (char-downcase key)
           (#\n (org-heading-motion tv :next))
           (#\p (org-heading-motion tv :prev))
           (#\f (org-heading-motion tv :forward))
           (#\b (org-heading-motion tv :backward))
           (#\u (org-heading-motion tv :up))
           (#\j (org-goto tv))
           (#\l (org-insert-link tv))
           (#\t (multiple-value-bind (lines l kw)
                    (ignore-errors (lol.org:cycle-todo (lol.textview:tv-lines tv) line))
                  (if lines
                      (progn (lol.textview:tv-replace tv lines l (lol.textview:tv-point-col tv))
                             (setf *browser-status* (format nil "TODO state changed to ~@[~a~]" kw)))
                      (setf *browser-status* "C-c C-t: not in a heading's section"))))
           (#\o (let ((o (lol.org:link-at (aref (lol.textview:tv-lines tv) line)
                                          (lol.textview:tv-point-col tv))))
                  (if o
                      (org-follow-link (getf (cdddr o) :target) (org-pane-path p) p)
                      (setf *browser-status* "C-c C-o: no link at the caret")))))
         t)
        (*pending-prefix* nil)                  ; any other chord is the shell's
        ;; TAB on a headline, S-TAB anywhere
        ((and (eq key :tab) shift (not ctrl) (not meta))
         (setf *org-last* last) (org-cycle-global tv) t)
        ((and (eq key :tab) on-head (zerop state))
         (setf *org-last* last) (org-cycle-at-point tv) t)
        ;; M-RET: a heading or an item
        ((and (eq key :return) meta (not ctrl))
         (org-structure-edit tv 'lol.org:insert-heading line (lol.textview:tv-point-col tv)))
        ;; the M-arrows on a headline: move, promote, demote (M-S: the subtree)
        ((and meta (not ctrl) on-head (member key '(:up :down :left :right)))
         (org-structure-edit tv (ecase key
                                  (:up 'lol.org:move-subtree-up)
                                  (:down 'lol.org:move-subtree-down)
                                  (:left (if shift 'lol.org:promote-subtree 'lol.org:promote-heading))
                                  (:right (if shift 'lol.org:demote-subtree 'lol.org:demote-heading)))
                             line))
        ((lol.textview::tv-hidden tv) (org-guard-hidden tv key state))
        (t nil)))))
;; every pane the shell opens: an Org one gets Org's reveal and its #+STARTUP folds
(pushnew 'org-prepare-pane *pane-opened-hooks*)
