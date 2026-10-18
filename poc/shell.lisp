;;;; shell.lisp — the drill-down accordion shell: render PRESENT into pixels.
;;;;
;;;; The browser's UI, finally visible. It is a thin renderer over the headless
;;;; present protocol (initramfs/present.lisp): a TRAIL is a stack of PANEs, each
;;;; pane holds a subject and its (present subject) VIEW; the accordion shows the
;;;; ancestors as collapsed breadcrumb bars and the deepest pane expanded. Drilling
;;;; an item pushes a pane; clicking a breadcrumb pops back. The shell is the ONE
;;;; stateful layer — present/commands-for stay pure (doc/code-browser.org §4½).
;;;;
;;;; CL-USER (like the initramfs modules), using lol.canvas for drawing and
;;;; lol.textview for source panes. Loaded by run-browser.lisp after those + the
;;;; model/present layers.

(in-package :cl-user)

;;; The palette, the row metrics and the widgets themselves now live in
;;; poc/widgets.lisp (lol.widgets) — phase 3's toolkit. They are IMPORTED rather
;;; than package-qualified because this file mentions them on nearly every line of
;;; drawing code, and `lol.widgets:*ui-dim*` a hundred times reads worse than it
;;; informs. fb-browser.lisp, also CL-USER, inherits the same names for free.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(lol.widgets:*ui-bg* lol.widgets:*ui-panel* lol.widgets:*ui-hi*
            lol.widgets:*ui-rule* lol.widgets:*ui-text* lol.widgets:*ui-dim*
            lol.widgets:*ui-accent* lol.widgets:*ui-sel* lol.widgets:*ui-amber*
            lol.widgets:concept-color lol.widgets:*concept-colors*
            lol.widgets:+row-h+ lol.widgets:+bar-h+ lol.widgets:clip-text
            ;; text metrics, read from the CURRENT font at draw time (doc/fonts.org §5)
            lol.widgets:char-w lol.widgets:line-h lol.widgets:text-dy)))

(defvar *bfont* nil
  "The browser font (a lol.canvas:font), set at startup. SET-BROWSER-FONT keeps
   lol.widgets:*font* in step — the widgets need it and must not reach into CL-USER.")

(defun set-browser-font (font)
  "Install FONT for both the shell and the widget toolkit. Every entry point
   (run-browser.lisp, run-browser-fb, render-frame.lisp) goes through here, so there
   is exactly one place the two can get out of sync — namely none."
  (setf *bfont* font lol.widgets:*font* font))
;; The cohesive environment (§4¾): several concurrent TRAILS, shown as tabs, one
;; active. A trail is a list of PANEs (root first, deepest last) — one investigation,
;; or a pinned surface (Workspace, change set). *ACTIVE* indexes the focused trail;
;; all the trail-* ops act on it. SuperL-n / SuperL-Tab switch; a spotter hit or
;; SuperL-drill opens a NEW trail.
(defvar *trails* '() "The open trails — a list of trails, each a list of PANEs.")
(defvar *active* 0    "Index into *trails* of the focused trail.")
(defvar *world* nil
  "The world the environment is running in — a DESK (poc/morphic.lisp), whose morphs
   are everything on screen, these panes among them — or NIL outside it. Made fresh
   by each entry point (run-browser-fb, run-browser.lisp, render-frame.lisp) for its
   canvas size.")

;;; A PANE is a MORPH (doc/morphic.org §12): the object that holds a subject and its
;;; view IS the thing on screen that draws it and takes its events. It used to be a
;;; DEFSTRUCT drawn by one big function that left what it drew where in specials
;;; (*LIST-ROWS*, *CONTENT-TOP* …), which every window then had to keep its own copy
;;; of. Now each pane keeps its own: the slots under "where it drew" below are written
;;; when the pane draws and read when it takes a click, in the coordinates it drew in
;;; (its surface's — the screen's, or a window's).
;;;
;;; The accessors and MAKE-PANE / PANE-P are what they were, so every caller — and
;;; every check that reads (pane-top (current-pane)) over the net REPL — is unchanged.
;;; The morph's SUBJECT slot and the pane's are ONE slot (a slot specifier of the same
;;; name merges with the inherited one): there is no second record of what it shows.

(defclass pane (lol.morph:morph)
  ((lol.morph::subject :accessor pane-subject)
   (view :initarg :view :initform nil :accessor pane-view)
   (sel  :initarg :sel :initform 0 :accessor pane-sel
         :documentation "Selected item index (list views).")
   (top  :initarg :top :initform 0 :accessor pane-top
         :documentation "First visible LAYOUT SLOT (section headers occupy one too).")
   (follow :initarg :follow :initform t :accessor pane-follow
           :documentation "Should the viewport chase the selection on the next draw?
   Arrow keys say yes (move the selection, bring it into view); the wheel says no,
   because scrolling is LOOKING — the whole point of a viewport independent of the
   selection.")
   (tv :initarg :tv :initform nil :accessor pane-tv
       :documentation "A lol.textview for :source/:reference panes, else NIL.")
   (side :initarg :side :initform nil :accessor pane-side
         :documentation "A pinned, EDITABLE pane shown BESIDE this list (⇧↵), or NIL.")
   (side-stash :initarg :side-stash :initform nil :accessor pane-side-stash
               :documentation "A side pane HIDDEN rather than closed, so re-opening it
   restores the buffer instead of a blank one. Hiding an EDITABLE pane must not
   destroy what you typed into it.")
   (side-pinned :initarg :side-pinned :initform nil :accessor pane-side-pinned
                :documentation "Did the USER pin that side pane (⇧↵), or did -> merely
   focus the preview? A pinned side outlives losing focus; a focused preview must not,
   or leaving it would strand you in a different layout from the one you arrived in.")
   (editing-side :initform nil :accessor pane-editing-side
                 :documentation "Is the keyboard in this pane's right-hand column (its
   side pane) rather than in the pane itself? SuperL-o toggles it; every use is
   guarded by (pane-side p). It was one special for the whole screen (*EDITING-SIDE*)
   until each pane became a morph.")
   (preview :initform nil :accessor pane-preview
            :documentation "The cached read-only pane the nav split shows beside this
   list — what the selection opens — rebuilt only when the selection's subject
   changes (PREVIEW-FOR).")
   (preview-subject :initform nil :accessor pane-preview-subject
                    :documentation "The subject PREVIEW was built for (EQ-compared).")
   ;; ---- where it drew, for its clicks (in its surface's coordinates) -----------
   (body-top :initform 0 :accessor pane-body-top
             :documentation "The y where the expanded pane's body starts.")
   (split-x :initform nil :accessor pane-split-x
            :documentation "The left x of the right-hand column (the nav-split preview, a
   side pane) when the pane is split, else NIL: a click at x >= this is the column's.")
   (body :initform nil :accessor pane-body
         :documentation "The morph that draws this pane's BODY — its list, its text view,
   its grid, its stage, its page, its terminal (a PANE-BODY, below): the pane's child,
   laid out by the pane, hit-tested and damaged on its own. NIL while the pane is not
   expanded or beside one (a breadcrumb draws no body). What the body drew where —
   a list's rows, a grid's cells, a stage's viewport — is the body's own slots.")
   ;; ---- where it is: set by the layout of the trail it belongs to ----------------
   (mode :initform nil :accessor pane-mode
         :documentation ":COLLAPSED (an ancestor: a breadcrumb bar), :EXPANDED (the
   trail's current pane: a panel and its body) or :SIDE (in an expanded pane's
   right-hand column: its side pane, or its nav-split preview). NIL off screen.")
   (split-plan :initform nil :accessor pane-split-plan
            :documentation "An expanded pane's split, as EXPANDED-COLUMNS laid it out.")
   (sig :initform nil :accessor pane-sig
        :documentation "What its picture was last a function of (doc/morphic.org §12.6)."))
  (:documentation "A subject, presented: its view, where you are in it, and — being a
   morph — where it is on screen and what it drew there."))

(defun make-pane (&rest initargs &key subject view sel top follow tv side side-stash side-pinned)
  "A new pane (the constructor DEFSTRUCT used to provide, same keywords)."
  (declare (ignore subject view sel top follow tv side side-stash side-pinned))
  (apply #'make-instance 'pane initargs))

(defun pane-p (x) (typep x 'pane))

;;; A pane's BODY is a morph too (doc/morphic.org §13): the list, the text view, the
;;; grid, the stage, the image, the page, the terminal — whatever the view kind paints
;;; under the pane's header. The pane draws only its panel (title, tint, badge, the
;;; divider and header of a split); its body is its child, laid out by the pane at the
;;; rect EXPANDED-COLUMNS gives it, drawn by its own DRAW-MORPH, under the pointer on
;;; its own, and damaged on its own — so a key in a text view repairs the text view (its
;;; changed lines, in fact), not the list beside it. A side pane, or the nav split's
;;; preview, is a pane whose body is its only picture. The drawing itself is the
;;; functions below (DRAW-LIST-BODY, TV-DRAW, DRAW-STAGE-BODY …), called by the body's
;;; DRAW-MORPH (poc/morphic.lisp) with the body's bounds; what they drew where goes into
;;; the BODY's slots, which its clicks and the halo read back.

(defclass pane-body (lol.morph:morph)
  ((sig :initform :none :accessor body-sig
        :documentation "What its picture was last a function of (DESK-SYNC compares)."))
  (:documentation "The part of a pane under its header: one morph per pane on screen,
   of the class its view kind asks for (BODY-CLASS-FOR). Its pane is its owner."))

(defclass list-body (pane-body)
  ((rows :initform '() :accessor body-rows
         :documentation "The hit map LOL.WIDGETS:DRAW-LIST returned when the list was drawn
   last — ((y . index) …). A click reads it back rather than computing an index from y,
   because with section headers in the layout screen row N is no longer item N."))
  (:documentation "A list view: rows, sections, the selection and a scrollbar."))

(defclass text-body (pane-body)
  ((drawn :initform nil :accessor body-drawn
          :documentation "What the text view showed when last drawn — (TV FRAME LINES
   START ROWS), LOL.TEXTVIEW's own account (TV-FRAME-KEY, TV-ROW-KEYS) — so that a
   change damages only the screen rows that differ."))
  (:documentation "A text view (lol.textview): source, the Workspace, a file, a
   conversation, a run's output."))

(defclass grid-body (pane-body)
  ((cells :initform '() :accessor body-cells
          :documentation "The hit map LOL.WIDGETS:DRAW-GRID returned for the dispatch grid."))
  (:documentation "A dispatch matrix: a clickable grid of method counts."))

(defclass stage-body (pane-body)
  ((viewport :initform nil :accessor body-viewport
             :documentation "The stage's viewport (x y w h) as last drawn, for clicks."))
  (:documentation "The COLLIDE stage: the viewport with its sprites, the heads-up line
   and the transcript. All a tick can change is inside it, so a stepping stage damages
   this morph alone."))

(defclass image-body (pane-body) ()
  (:documentation "A picture, fitted into the pane on a checkerboard."))

(defclass web-body (pane-body) ()
  (:documentation "A web page: its toolbar and Chrome's newest frame (initramfs/web.lisp)."))

(defclass terminal-body (pane-body) ()
  (:documentation "A program on a pty, its grid of cells (poc/terminal-pane.lisp)."))

(defun body-pane (b)
  "The pane whose body B is."
  (lol.morph:morph-owner b))

(defun body-class-for (pane)
  "Which PANE-BODY class PANE's view is drawn by."
  (let ((kind (view-kind (pane-view pane))))
    (cond ((pane-tv pane) 'text-body)
          ((eq kind :matrix) 'grid-body)
          ((eq kind :stage) 'stage-body)
          ((eq kind :image) 'image-body)
          ((eq kind :web) 'web-body)
          ((eq kind :terminal) 'terminal-body)
          (t 'list-body))))

(defun pane-rows (p)
  "The rows P's list body drew last, ((y . index) …) — NIL when P shows no list."
  (let ((b (pane-body p))) (and (typep b 'list-body) (body-rows b))))

(defun pane-cells (p)
  "The cells P's grid body drew last."
  (let ((b (pane-body p))) (and (typep b 'grid-body) (body-cells b))))

(defun pane-stage-rect (p)
  "The viewport P's stage body drew last (x y w h), or NIL."
  (let ((b (pane-body p))) (and (typep b 'stage-body) (body-viewport b))))

(defun pane-body-rect (p)
  "Where P's body is, in the coordinates P draws in (its surface's), or NIL."
  (let ((b (pane-body p))) (and b (lol.morph:morph-owner b) (lol.morph:morph-local-bounds b))))

(defun focused-body (p)
  "The body the keyboard is in, in the expanded pane P: its side pane's when the side
   has the keys, else its own."
  (let ((side (pane-side p)))
    (if (and side (pane-editing-side p)) (pane-body side) (pane-body p))))

(defun screen-to-pane (pane x y)
  "The screen point X,Y in the coordinates PANE draws and records in — a window's,
   when PANE is in one (doc/morphic.org §12.4) -> (values X Y). A backend that knows
   only where the pointer is on the screen (the web page's hover, its wheel) asks this."
  (if (and pane (lol.morph:morph-owner pane))
      (multiple-value-bind (ox oy) (lol.morph:surface-origin pane)
        (values (- x ox) (- y oy)))
      (values x y)))

(defmethod print-object ((p pane) stream)
  "#<PANE workspace> — what it shows, which is how you tell two apart (the halo's
   caption prints a trail as its panes). No address: a printed pane reads the same in
   every run, so a rendered frame that shows one does too."
  (print-unreadable-object (p stream :type t)
    (princ (if (pane-view p) (short-title (view-title (pane-view p))) "-") stream)))

(defvar *pane-opened-hooks* '()
  "Functions of one PANE, called on every pane OPEN-PANE builds: a file type that sets
   its view up the moment it opens — an Org file folds as its #+STARTUP says
   (poc/org-mode.lisp), which loads after this file.")

(defun open-pane (subject &key preview)
  "Build a PANE for SUBJECT: present it, and for text views wrap the body in a
   textview (reusing its lexer + layout). A :SOURCE view (your code) is EDITABLE —
   it gets focus and Accept; a :REFERENCE view (a builtin) is read-only; a :FILE view
   (a real file on disk, doc/file-manager.org) is editable and saves with C-x C-s.
   PREVIEW asks for PRESENT-PREVIEW instead — the nav split's look at a selection,
   which for a file is its first lines rather than the whole file in an editor."
  (let ((v (if preview (present-preview subject) (present subject))))
    (let ((pane
            (make-pane
             :subject subject :view v
             :sel (max 0 (min (view-sel v) (max 0 (1- (length (view-items v))))))  ; view's preferred focus
             :tv (when (and (member (view-kind v) '(:source :reference :workspace :file)) (view-text v))
                   (let ((tv (lol.textview:make-textview
                              :font *bfont*
                              :syntax (view-syntax v)
                              :tint (view-tint v)
                              :focus (member (view-kind v) '(:source :workspace :file)))))
                     (lol.textview:tv-set-text tv (view-text v))
                     tv)))))
      ;; a file pane remembers the buffer it was loaded (or last saved) as: the buffer
      ;; is a persistent vector, replaced on every edit and restored by undo, so "is
      ;; it modified?" is one EQ test (FILE-PANE-DIRTY-P), not a string compare
      (when (and (eq (view-kind v) :file) (pane-tv pane) (listp (view-data v)))
        (setf (getf (view-data v) :saved-lines) (lol.textview:tv-lines (pane-tv pane))))
      (dolist (hook *pane-opened-hooks*) (ignore-errors (funcall hook pane)))
      pane)))

(defun editable-pane-p (pane)
  "True when PANE is your-own-code source — editable, with Accept."
  (and (pane-tv pane) (eq (view-kind (pane-view pane)) :source)))

(defun workspace-pane-p (pane)
  "True when PANE is the Workspace scratch buffer — editable, with eval verbs."
  (and (pane-tv pane) (eq (view-kind (pane-view pane)) :workspace)))

(defun terminal-pane-p (pane)
  "True when PANE is a TERMINAL (poc/terminal-pane.lisp): a program on a pty, which
   takes every key but the SuperL layer. Defined here, with its siblings, because the
   input routing below must ask it before that file is loaded."
  (and pane (eq (view-kind (pane-view pane)) :terminal)))

(defun file-pane-p (pane)
  "True when PANE edits a real FILE on disk (C-x C-s saves it; doc/file-manager.org)."
  (and (pane-tv pane) (eq (view-kind (pane-view pane)) :file)))

(defun lisp-file-pane-p (pane)
  "A file pane holding Lisp: it gets the Workspace's verbs too — C-x C-e Do it and
   C-c C-c define — on the form before point, so a .lisp file on disk can be edited,
   tried and saved without leaving it."
  (and (file-pane-p pane) (eq (view-syntax (pane-view pane)) :lisp)))

(defun editor-pane-p (pane)
  "Any pane you can TYPE into: a source pane, the Workspace, or a file."
  (or (editable-pane-p pane) (workspace-pane-p pane) (file-pane-p pane)))

(defun file-pane-dirty-p (pane)
  "Has a file pane's buffer changed since it was loaded or last saved?"
  (and (file-pane-p pane)
       (not (eq (lol.textview:tv-lines (pane-tv pane))
                (getf (view-data (pane-view pane)) :saved-lines)))))

(defun pane-title (pane)
  "The title a pane's header shows: its view's, plus a marker when it is a file with
   unsaved changes — Emacs's ** in the mode line, in words."
  (if (file-pane-dirty-p pane)
      (format nil "~a   [modified - C-x C-s saves]" (view-title (pane-view pane)))
      (view-title (pane-view pane))))

(defun view-kind-concept (kind)
  "A VIEW's kind (:source :matrix :class …) mapped to the Lisp CONCEPT it is a view
   OF (:function :generic-function :class …), so chrome can be tinted from the same
   palette as the rows. One mapping, used by both the pane/tab/crumb tint and the
   Spotter's recents — it existed twice for a while and the copies disagreed about
   what colour a remembered Workspace was."
  (case kind
    ((:workspace :terminal) :restart)      ; amber: a surface, not a definition
    ((:source :reference) :function)
    (:file :function)                      ; a text file reads like source
    (:image :macro)
    ((:matrix :generic :stage) :generic-function)
    (:class :class)
    (:inspector :variable)
    (t :package)))                         ; a list root is usually a package

(defun current-trail () "The focused trail (its list of panes)." (nth *active* *trails*))
(defun set-current-trail (panes) "Replace the focused trail's panes." (setf (nth *active* *trails*) panes))
(defun current-pane () "The deepest pane of the focused trail." (car (last (current-trail))))

;; --- within the active trail (drill-down accordion, unchanged semantics) ---
(defun subject-concept (subject pane)
  "The Lisp concept SUBJECT is, for tinting a Spotter recent. A SUBJ-DEFN already
   knows exactly what it is (:constant, :macro, :class …) and should say so; anything
   else falls back to what KIND of view it opened, which is as much as the shell can
   tell. Without the first case a remembered constant is tinted as a function, merely
   because constants and functions both open in a source pane."
  (or (and (subj-defn-p subject) (subj-defn-kind subject))
      (view-kind-concept (view-kind (pane-view pane)))))

(defun trail-root (subject)
  "Start the environment at SUBJECT — one trail, made active. Additional trails come
   from NEW-TRAIL (a spotter hit, SuperL-drill)."
  (let ((pane (open-pane subject)))
    (note-recent subject (view-title (pane-view pane)) nil (subject-concept subject pane))
    (setf *trails* (list (list pane)) *active* 0)))
(defun trail-push (subject) "Drill into SUBJECT in the active trail."
  (let ((pane (open-pane subject)))
    (note-recent subject (view-title (pane-view pane))       ; for the Spotter's recents
                 nil (subject-concept subject pane))
    (set-current-trail (append (current-trail) (list pane)))))
(defun trail-pop () "Pop the active trail's deepest pane (back)."
  (when (cdr (current-trail)) (set-current-trail (butlast (current-trail)))))
(defun trail-goto (n) "Collapse the active trail back to level N (a breadcrumb click)."
  (when (< n (length (current-trail))) (set-current-trail (subseq (current-trail) 0 (1+ n)))))

;; --- across trails (the tab layer) ---
(defun new-trail (subject)
  "Open SUBJECT in a NEW trail and focus it — a spotter hit / SuperL-drill."
  (let ((pane (open-pane subject)))
    (note-recent subject (view-title (pane-view pane)) nil (subject-concept subject pane))
    (setf *trails* (append *trails* (list (list pane)))
          *active* (1- (length *trails*)))))
(defun close-trail ()
  "Close the focused trail (never the last one); focus the neighbour that slides in."
  (when (cdr *trails*)
    (setf *trails* (append (subseq *trails* 0 *active*) (subseq *trails* (1+ *active*)))
          *active* (min *active* (1- (length *trails*))))))
(defun activate-trail (n) "Focus trail N if it exists." (when (< -1 n (length *trails*)) (setf *active* n)))
(defun swap-trail (n)
  "Move the focused trail to slot N (1-based on screen, 0-based here), swapping with
   whatever occupies it, and follow the focus. Reordering tabs matters once SuperL-1…9
   is muscle memory: the number is the address, so you want to choose it."
  (let ((i (1- n)) (len (length *trails*)))
    (when (and (< -1 i len) (/= i *active*))
      (rotatef (nth i *trails*) (nth *active* *trails*))
      (setf *active* i))))

(defun cycle-trail (dir) "Focus the next (+1) / previous (-1) trail, wrapping."
  (setf *active* (mod (+ *active* dir) (length *trails*))))
(defparameter +title-kind-words+
  '("modules" "module" "package" "class" "condition" "matrix" "effective" "definition"
    "function" "generic-function" "method" "macro" "struct" "type" "senders" "referrers"
    "folder" "file" "link" "image" "archive" "info" "find")
  "Leading words in a view title that name the view's KIND rather than its subject.")

(defun short-title (title)
  "TITLE reduced to the part that identifies the SUBJECT — what a tab should say.

   View titles are written for a pane header, where a kind word earns its place:
   \"package COMMON-LISP-USER\", \"modules — every definition, by the file it came
   from\". On a 16-character tab that spends the whole label on the kind and clips the
   name — \"modules — ever..\" — which is precisely backwards, because the kind is
   already there as the tab's colour stripe. So: cut any \" — \" gloss, then drop a
   leading kind word. The mock-up's tabs read \"draw\", \"parse-headers\",
   \"Playground\"; these now read the same way."
  (let* ((t1 (let ((dash (search (format nil " ~c " (code-char #x2014)) title)))
               (if dash (subseq title 0 dash) title)))
         (sp (position #\Space t1))
         (head (and sp (string-downcase (subseq t1 0 sp)))))
    (if (and head (member head +title-kind-words+ :test #'string=) (< (1+ sp) (length t1)))
        (subseq t1 (1+ sp))
        t1)))

(defun trail-title (trail)
  "The tab label for TRAIL — the ROOT pane's subject, short enough to be a tab."
  (short-title (view-title (pane-view (first trail)))))

(defvar *restore-desk-fn* nil
  "A function of no arguments that rebuilds *TRAILS* from the saved desk and returns
   true, or returns NIL when there is none. Set by initramfs/desk.lisp, which loads
   after this file (doc/session-persistence.org §2c).")

(defun open-environment (&optional root)
  "Seed the cohesive environment (§4¾), WITHOUT throwing away what is already open.

   With no ROOT: the trails as they were, when there are any — leaving to the
   supervisor's console and coming back with 'b' used to replace them with one fresh
   Workspace, losing every tab (doc/session-persistence.org §1.3); else the desk
   saved by the last session (*RESTORE-DESK-FN*); else the Workspace alone.

   With a ROOT (the menu's 'p', 'c', 'l'; a host entry point): focus a tab already
   rooted at an EQUALP subject, else open ROOT in a new one. On an empty environment
   that is ROOT plus a Workspace tab, unless ROOT is itself the Workspace."
  (cond
    ((and root *trails*)
     (let ((i (position-if (lambda (trail) (equalp (pane-subject (first trail)) root)) *trails*)))
       (if i (activate-trail i) (new-trail root))))
    (root
     (trail-root root)
     (unless (typep root 'subj-workspace) (new-trail (subj-workspace)))
     (setf *active* 0))
    (*trails* (setf *active* (min *active* (1- (length *trails*)))))
    ((and *restore-desk-fn* (ignore-errors (funcall *restore-desk-fn*))))
    (t (trail-root (subj-workspace)))))

(defun set-pane-sel (pane n)
  "Move PANE's selection to N (clamped) and re-arm FOLLOW, so the next draw brings
   the selection back into view. Every selection move goes through here — including
   the ones that land after a wheel scroll, which is the point: the wheel detaches
   the viewport from the selection, and touching the selection reattaches it.

   Re-arming must NOT happen in the draw, however tempting: the framebuffer loop
   redraws on every event, mouse motion included, so a list scrolled with the wheel
   would snap back the instant the pointer twitched."
  (let ((n-items (length (view-items (pane-view pane)))))
    (setf (pane-sel pane) (max 0 (min n (max 0 (1- n-items))))
          (pane-follow pane) t)))

(defun selected-item (pane)
  (let ((items (view-items (pane-view pane))))
    (and items (< (pane-sel pane) (length items)) (nth (pane-sel pane) items))))

;;; ---- geometry: where each breadcrumb bar lives (for click hit-testing) ----

(defparameter +list-page+ 12 "Rows a list pane moves its selection on PageUp/PageDown.")
(defparameter +wheel-lines+ 3
  "Lines the current pane scrolls per mouse-wheel notch. Lives here, not in a
   backend, because BOTH backends need it and they learn about the wheel very
   differently: the guest decodes an evdev REL_WHEEL record, the X11 host receives
   a ButtonPress on button 4/5 (doc/input.org §6-7).")
(defvar *pending-prefix* nil
  "The pending chord prefix in an editable pane: NIL, :c-c (after C-c) or :c-x
   (after C-x). The next key completes the chord — C-c C-c Accept, C-x C-e Do it,
   C-c C-p Print it, C-c C-i Inspect it.")
(defvar *last-edit* nil
  "What the previous editing key did, for the two Emacs commands that depend on it:
   (:KILL) — so another kill APPENDS to the same history entry — and
   (:YANK TV L0 C0 L1 C1) — the span the last paste inserted, which M-y replaces.
   Any other key clears it (EDIT-PANE-KEY).")

(defvar *unsaved-warned* nil
  "The file pane the last C-g warned about (it had unsaved changes), so a SECOND C-g
   on the same pane closes it. Any other pane, and the warning starts over.")

(defvar *pending-meta* nil
  "T after a lone Esc — the Emacs Meta prefix (§4¾). The next key is Meta-modified,
   so Esc . jumps to definition just like Alt-. / M-. would. Sticky for one key,
   cleared on the next keystroke.")
(defvar *browser-status* nil "A transient status message (e.g. Accept result), or NIL.")
(defvar *minibuffer* nil
  "The open minibuffer (an MB — see \"the minibuffer\" below), or NIL. Declared up here
   because the status line, drawn earlier in this file, has to know.")

;;; ---- the font: which file, and its size at run time (doc/fonts.org) -------------

(defun open-browser-font (ttf-paths bitmap-path &key size)
  "Load the browser font: the first TrueType file in TTF-PATHS that parses, at SIZE
   pixels (default lol.canvas:*default-font-size*); failing all of them, the bitmap
   .lolf at BITMAP-PATH — the fallback that always ships. A missing or broken TTF
   (build.sh leaves an EMPTY file when the build host had none) is not an error,
   only a reason to fall back; what was tried and why it failed goes to *ERROR-OUTPUT*
   so a boot log says why the text looks old."
  (or (loop for p in ttf-paths
            when (and p (probe-file p))
              do (handler-case
                     (return (lol.canvas:load-ttf-font
                              p :size (or size lol.canvas:*default-font-size*)))
                   (error (e) (format *error-output* "~&font: ~a — ~a~%" p e))))
      (let ((f (lol.canvas:load-font bitmap-path)))
        (when size (lol.canvas:set-font-size f size))
        f)))

(defun set-font (path &key (size (and *bfont* (lol.canvas:font-size *bfont*))))
  "Switch the whole environment to the TrueType file at PATH — e.g. any font under
   /mnt/host/usr/share/fonts, the laptop's own collection — keeping the current size.
   The font object is REPLACED, so the textviews that captured the old one are
   repointed too. Returns the new font's description."
  (let ((new (lol.canvas:load-ttf-font path :size (or size lol.canvas:*default-font-size*)))
        (old *bfont*))
    (set-browser-font new)
    (dolist (trail *trails*)
      (dolist (p trail)
        (dolist (pane (list p (pane-side p) (pane-side-stash p)))
          (when (and pane (pane-tv pane) (eq (lol.textview::tv-font (pane-tv pane)) old))
            (setf (lol.textview::tv-font (pane-tv pane)) new)))))
    (lol.canvas:font-description new)))

(defun change-font-size (how)
  "The SuperL-= / SuperL-- / SuperL-0 (and C-x C-+ / C-- / C-0) command. HOW is
   :bigger, :smaller or :reset. Resizes the ONE shared font in place, so the next
   frame relays out everything; says the new size in the status line; and remembers
   it in ~/.lol/settings when there is a home to write to. Steps are 2 px
   below 20 and 3 above — at 12 px one pixel is 8%, at 30 it is 3% and too small to see."
  (let* ((f *bfont*)
         (now (lol.canvas:font-size f))
         (step (if (or (> now 20) (and (= now 20) (eq how :bigger))) 3 2))
         (new (ecase how
                (:bigger (lol.canvas:set-font-size f (+ now step)))
                (:smaller (lol.canvas:set-font-size f (- now step)))
                (:reset (lol.canvas:reset-font-size f)))))
    (when (fboundp 'save-setting) (funcall 'save-setting :font-size new))
    (setf *browser-status*
          (format nil "font: ~a~@[ — at the ~(~a~) end~]"
                  (lol.canvas:font-description f)
                  (cond ((and (eq how :bigger) (= new now)) :largest)
                        ((and (eq how :smaller) (= new now)) :smallest))))
    new))
(defvar *value-prompt* nil
  "A hook: (funcall *value-prompt* LABEL) -> the string the user typed, or NIL if they
   cancelled. The caller WAITS for the answer, which only a backend with a loop to
   turn meanwhile can arrange — so the backend installs one (fb-browser.lisp: the
   value prompt overlay, poc/overlays.lisp) and the shell merely asks. Same shape as
   *COMPLETER*, which repl.lisp fills in for Tab completion. When no backend has set
   it, a gesture that needs a value says so in the status line instead of silently
   doing nothing.")
(defun open-beside (pane)
  "⇧↵ on a list row: open the selected item BESIDE the list — a pinned, editable pane in
   the right column that does NOT follow the selection (unlike the read-only preview). A
   second ⇧↵ on the same item closes it. Focus moves to the side so you can edit at once."
  (let ((it (selected-item pane)))
    (when (and it (item-subject it))
      (if (and (pane-side pane)
               (eq (pane-subject (pane-side pane)) (item-subject it)))
          (setf (pane-side pane) nil (pane-side-pinned pane) nil
                (pane-editing-side pane) nil)                ; toggle closed
          (setf (pane-side pane) (open-pane (item-subject it))
                (pane-side-pinned pane) t                    ; ⇧↵ pins it
                (pane-editing-side pane) t)))))

(defun focus-side (pane)
  "-> on a list row: move focus INTO the right-hand column and make it usable.

   The nav split already SHOWS the selection over there, but read-only and
   unfocused — a look, not a place you can be, and until now -> did nothing at all.
   This promotes that preview to the same pinned pane <S-Return> builds, so your own
   code becomes editable, a builtin scrolls, and a list can be walked; then it puts
   focus there. Unlike <S-Return> it never toggles: -> means go right, every time.

   Crucially it does NOT touch the trail. Focus is not navigation, so C-g (or
   SuperL-o) hands it straight back and what you return to is the identical
   side-by-side you left — same pane, same selection, same preview."
  (let ((it (selected-item pane)))
    (when (and it (item-subject it))
      (unless (and (pane-side pane)
                   (eq (pane-subject (pane-side pane)) (item-subject it)))
        (setf (pane-side pane) (open-pane (item-subject it))
              (pane-side-pinned pane) nil))     ; focused, not pinned — see UNFOCUS-SIDE
      (setf (pane-editing-side pane) t)
      t)))

(defun unfocus-side (pane)
  "Give focus back to the list. If the right column was merely FOCUSED (by ->), it
   goes away again and you are looking at the read-only preview you started from —
   the identical frame, down to the pixel. If you PINNED it with <S-Return>, it stays:
   pinning is a decision, and C-g should not quietly undo one."
  (setf (pane-editing-side pane) nil)
  (unless (pane-side-pinned pane)
    (setf (pane-side pane) nil)))

(defun side-list-key (pane side key state)
  "Keys for a focused side pane that is a LIST rather than text. EDIT-PANE-KEY only
   knows how to type, so without this a list in the right column is focusable and
   completely inert. Same meanings as the main list: up/down move, Return drills —
   into the MAIN trail, because Return means the same thing everywhere."
  (declare (ignore state))
  (cond
    ;; <- comes back, which it can only mean HERE: in a text side pane the same key
    ;; is the editor's, moving the caret (§4¾'s modifier split)
    ((eq key :left)      (unfocus-side pane))
    ((eq key :up)        (set-pane-sel side (1- (pane-sel side))))
    ((eq key :down)      (set-pane-sel side (1+ (pane-sel side))))
    ((eq key :page-up)   (set-pane-sel side (- (pane-sel side) +list-page+)))
    ((eq key :page-down) (set-pane-sel side (+ (pane-sel side) +list-page+)))
    ((eq key :return)
     (let ((it (selected-item side)))
       (when (and it (item-subject it))
         (setf (pane-editing-side pane) nil)     ; the trail moves; focus comes home
         (trail-push (item-subject it)))))
    (t nil))
  nil)

(defun toggle-side-focus ()
  "SuperL-o: move keyboard focus between the list and its editable side pane."
  (let ((p (current-pane)))
    (when (pane-side p)
      (setf (pane-editing-side p) (not (pane-editing-side p))))))

;;; ---- the Workspace verbs: eval the form before point (§3a) ---------------

(defun buffer-text-to-point (tv)
  "The Workspace buffer text from the start up to the caret."
  (let ((lines (lol.textview:tv-lines tv))
        (pl (lol.textview:tv-point-line tv))
        (pc (lol.textview:tv-point-col tv)))
    (with-output-to-string (s)
      (dotimes (i pl) (write-line (aref lines i) s))
      (let ((cur (aref lines pl)))
        (write-string (subseq cur 0 (min pc (length cur))) s)))))

(defun sexp-spans (text &key (start 0) (end (length text)))
  "Scan TEXT from START to END the way an editor does — NOT with the Lisp reader — and
   return (values SPANS DEPTH): SPANS, every complete s-expression seen, as
   (START END DEPTH) in the order they END, and DEPTH, how many lists are still open at
   END. Knows strings, |symbols|, #\\chars, ; and #| |# comments, the quote prefixes
   (' ` , ,@ #' #.) and #( vectors. Never reads a symbol, so text it cannot READ — a
   package that does not exist, a #. somewhere, an unbalanced snippet above — cannot
   stop it, which is why C-x C-e uses it (FORM-SPAN-BEFORE-POINT). A stray ) at the top
   is ignored; an unterminated string ends the scan."
  (let ((i start) (stack '()) (spans '()) (pending nil))
    (labels ((delim-p (c) (or (member c '(#\Space #\Tab #\Newline #\Return #\Page #\( #\) #\" #\; #\' #\` #\,))))
             (emit (s e) (push (list (or pending s) e (length stack)) spans) (setf pending nil)))
      (loop while (< i end)
            do (let ((c (char text i)))
                 (cond
                   ((member c '(#\Space #\Tab #\Newline #\Return #\Page)) (incf i))
                   ((char= c #\;) (setf i (or (position #\Newline text :start i :end end) end)))
                   ((and (char= c #\#) (< (1+ i) end) (char= (char text (1+ i)) #\|))
                    (let ((j (search "|#" text :start2 (+ i 2) :end2 end)))
                      (setf i (if j (+ j 2) end))))
                   ((or (char= c #\") (char= c #\|))
                    (let ((j (1+ i)))
                      (loop while (and (< j end) (char/= (char text j) c))
                            do (when (char= (char text j) #\\) (incf j)) (incf j))
                      (when (>= j end) (return))                 ; unterminated: stop
                      (emit i (1+ j)) (setf i (1+ j))))
                   ((char= c #\()
                    (push (or pending i) stack) (setf pending nil) (incf i))
                   ((and (char= c #\#) (< (1+ i) end) (char= (char text (1+ i)) #\())
                    (push (or pending i) stack) (setf pending nil) (incf i 2))
                   ((char= c #\))
                    (if stack
                        (let ((s (pop stack))) (push (list s (1+ i) (length stack)) spans))
                        nil)                                       ; stray: ignore
                    (setf pending nil) (incf i))
                   ((member c '(#\' #\`)) (unless pending (setf pending i)) (incf i))
                   ((char= c #\,)
                    (unless pending (setf pending i)) (incf i)
                    (when (and (< i end) (char= (char text i) #\@)) (incf i)))
                   ((and (char= c #\#) (< (1+ i) end) (member (char text (1+ i)) '(#\' #\.)))
                    (unless pending (setf pending i)) (incf i 2))
                   ((and (char= c #\#) (< (1+ i) end) (char= (char text (1+ i)) #\\))
                    ;; #\x, #\Space, #\( — at least one character after the backslash
                    (let ((j (min end (+ i 3))))
                      (loop while (and (< j end) (not (delim-p (char text j)))) do (incf j))
                      (emit i j) (setf i j)))
                   (t
                    ;; an atom: a symbol, a number, #:foo, #x1F … up to a delimiter
                    (let ((j i))
                      (loop while (and (< j end) (not (delim-p (char text j))))
                            do (when (char= (char text j) #\\) (incf j)) (incf j))
                      (emit i (min j end)) (setf i (min j end))))))))
    (values (nreverse spans) (length stack))))

(defun scan-start-before (text point)
  "Where to start scanning for the form before POINT: just after the last line, before
   POINT, that starts in column 0 with a ; — prose in a conversation, the comments
   between definitions in a file. A form's own interior is indented, so such a line
   almost never falls inside one, and starting there keeps an unbalanced snippet or an
   unterminated string ABOVE it from swallowing the form you are on."
  (let ((nl (position #\Newline text :end point :from-end t)))
    ;; the line the caret is on is the form's own; look above it
    (loop while nl
          do (let* ((ls (1+ (or (position #\Newline text :end nl :from-end t) -1))))
               (when (and (< ls nl) (char= (char text ls) #\;))
                 (return-from scan-start-before (1+ nl)))
               (setf nl (and (> ls 0) (1- ls)))))
    0))

(defun form-span-before (text point)
  "(values START END) of the s-expression just before POINT in TEXT, at POINT's own
   depth — Emacs's C-x C-e target: after (+ 1 (* 2 3)) it is the whole form, with the
   caret after (* 2 3) it is that. NIL when there is none. Scans from
   SCAN-START-BEFORE, and from the very start if that finds nothing."
  (flet ((pick (from)
           (multiple-value-bind (spans depth) (sexp-spans text :start from :end point)
             (let ((best (find depth spans :key #'third :from-end t)))
               (and best (values (first best) (second best)))))))
    (multiple-value-bind (s e) (pick (scan-start-before text point))
      (if s (values s e) (pick 0)))))

(defun toplevel-span-at (text point)
  "(values START END) of the top-level form the caret is in or right after (C-c C-e,
   Emacs's eval-defun / C-M-x), or NIL."
  (let* ((from (scan-start-before text point))
         (top (remove-if-not (lambda (s) (zerop (third s))) (sexp-spans text :start from)))
         (hit (or (find-if (lambda (s) (and (<= (first s) point) (<= point (second s)))) top)
                  (find-if (lambda (s) (<= (second s) point)) top :from-end t))))
    (and hit (values (first hit) (second hit)))))

(defun tv-text-and-point (tv)
  "(values TEXT POINT): TV's whole buffer as one string, and the caret as an index in it."
  (let* ((lines (lol.textview:tv-lines tv))
         (pl (lol.textview:tv-point-line tv))
         (pc (lol.textview:tv-point-col tv))
         (point 0))
    (values (with-output-to-string (s)
              (loop for i from 0 below (length lines)
                    for line = (aref lines i)
                    do (when (= i pl) (setf point (+ point (min pc (length line)))))
                       (when (< i pl) (incf point (1+ (length line))))
                       (write-string line s)
                       (when (< i (1- (length lines))) (write-char #\Newline s))))
            point)))

(defun read-forms-from-text (text)
  "Every form in TEXT, read in order, as a list. Signals the reader's error."
  (let ((sentinel (list :eof)) (pos 0) (forms '()))
    (loop
      (multiple-value-bind (form next) (read-from-string text nil sentinel :start pos)
        (when (eq form sentinel) (return (nreverse forms)))
        (push form forms) (setf pos next)))))

(defun eval-target-text (tv &optional toplevel)
  "The source text the eval verbs act on, and what it is: (values TEXT KIND), KIND
   :region (the selection, when there is one — every form in it), :toplevel (the
   top-level form around the caret, for C-c C-e), or :before (the s-expression just
   before the caret, at its own depth — C-x C-e). NIL when there is nothing."
  (let ((region (editor-region-text tv)))
    (if (and region (plusp (length (string-trim '(#\Space #\Tab #\Newline) region))))
        (values region :region)
        (multiple-value-bind (text point) (tv-text-and-point tv)
          (multiple-value-bind (s e) (if toplevel (toplevel-span-at text point) (form-span-before text point))
            (and s (values (subseq text s e) (if toplevel :toplevel :before))))))))

(defun form-before-point (tv &optional toplevel)
  "The form the eval verbs (C-x C-e Do it, C-c C-p Print it, C-c C-i Inspect it,
   C-c C-d Debug it) act on: the SELECTION when there is one (all its forms, as a
   PROGN), else the s-expression just before the caret — Emacs's C-x C-e — or, with
   TOPLEVEL, the top-level form around it (C-c C-e). Returns (values FORM FOUNDP).

   Only that text is READ. This used to read the whole buffer from the top up to the
   caret, and the first thing above it the reader choked on — a symbol in a package
   that does not exist, an unbalanced snippet in a conversation, a #. — made every
   form below it invisible, or silently picked one higher up. Now the form is FOUND
   by an editor's scan (SEXP-SPANS, which never reads), and a form that does not read
   is reported as itself: its value is an error saying why."
  (multiple-value-bind (text kind) (eval-target-text tv toplevel)
    (if (null text)
        (values nil nil)
        (handler-case
            (let ((forms (read-forms-from-text text)))
              (cond ((null forms) (values nil nil))
                    ((and (eq kind :region) (cdr forms)) (values (cons 'progn forms) t))
                    (t (values (first forms) t))))
          (error (c)
            (values `(error "can't read ~s: ~a" ,(lol.widgets:clip-text (substitute #\Space #\Newline text) 50)
                            ,(princ-to-string c))
                    t))))))

(defun form-text-before-point (tv)
  "The SOURCE TEXT C-c C-c Accept compiles: the selection when there is one, else the
   TOP-LEVEL form the caret is in or right after — as SLIME's C-c C-c, so the caret
   may be anywhere inside the definition — or NIL. The text, rather than a re-printed
   form, is what gets recorded, so comments and layout survive into the change set."
  (let ((text (eval-target-text tv t)))
    (and text (string-trim '(#\Space #\Tab #\Newline) text))))

(defun form-echo (form)
  "FORM printed short and on one line, to name it in the status line. The Workspace
   verbs say WHICH form they ran: with several forms in a buffer, \"=> 3\" alone does
   not tell you whether the one before the caret is the one that ran."
  (let ((*print-length* 5) (*print-level* 3))
    (substitute #\Space #\Newline (format nil "~s" form))))

(defun ws-eval (form)
  "Eval FORM; return (values OK RESULT-or-CONDITION OUTPUT NOTES). OUTPUT is whatever
   the form printed — *standard-output* / *trace-output* are bound to a string stream
   so a =(format t …)= in the Workspace is captured (the notebook transcript) instead
   of vanishing behind the framebuffer. NOTES are the compiler's messages, one short
   string each (\"undefined function: PLAY\", WS-COMPILER-NOTES): EVAL compiles, and
   the compiler writes to *error-output* — on the guest the console, which nobody
   sees under KD_GRAPHICS and which, stopped by a stray XOFF, once froze the
   environment on the write (2026-10-17, line-editor.lisp's IXON note). So
   *error-output* is captured too, and what in it is not the compiler's joins OUTPUT.
   Errors are caught, never thrown — a bad form must not take down the browser (nor
   PID 1); output printed before it still returns."
  (let ((out (make-string-output-stream)) (err (make-string-output-stream)))
    (flet ((finish (ok v)
             (multiple-value-bind (printed notes)
                 (ws-compiler-notes (get-output-stream-string err))
               (values ok v (concatenate 'string (get-output-stream-string out) printed)
                       notes))))
      (handler-case
          (let ((v (let ((*standard-output* out) (*trace-output* out) (*error-output* err))
                     (eval form))))
            (finish t v))
        (serious-condition (c) (finish nil c))))))

(defun ws-compiler-notes (text)
  "Split TEXT, what reached *error-output* during an eval, into (values PRINTED NOTES).
   SBCL's compiler messages are the lines starting with ';' — a context ('; in: …'),
   then '; caught STYLE-WARNING:' and the message indented under it, and at the end a
   'compilation unit finished' summary repeating it. NOTES keeps each message once,
   joined onto one line, without the COMMON-LISP-USER:: prefix (the Workspace's own
   package): (\"undefined function: ZZ-NOTHING\"). PRINTED is every other line — what
   the form itself wrote to *error-output*."
  (let ((printed '()) (notes '()) (current nil) (in-caught nil))
    (flet ((end-note ()
             (when current
               (let ((s (ws-remove-string "COMMON-LISP-USER::"
                                       (format nil "~{~a~^ ~}" (reverse current)))))
                 (pushnew s notes :test #'string=))
               (setf current nil))))
      (with-input-from-string (in text)
        (loop for line = (read-line in nil) while line
              do (cond ((not (and (plusp (length line)) (char= (char line 0) #\;)))
                        (end-note) (setf in-caught nil) (push line printed))
                       ((search "; caught " line :end2 (min 9 (length line)))
                        (end-note) (setf in-caught t))
                       ((search "compilation unit finished" line)
                        (end-note) (setf in-caught nil))
                       (in-caught
                        (let ((s (string-trim "; " line)))
                          (if (plusp (length s)) (push s current) (progn (end-note) (setf in-caught nil)))))))
        (end-note)))
    (values (format nil "~{~a~%~}" (reverse printed)) (reverse notes))))

(defun ws-remove-string (prefix string)
  "STRING with every occurrence of PREFIX taken out."
  (with-output-to-string (s)
    (loop with start = 0
          for hit = (search prefix string :start2 start)
          do (write-string string s :start start :end (or hit (length string)))
             (if hit (setf start (+ hit (length prefix))) (loop-finish)))))
(defun ws-insert (tv string)
  "Insert STRING into TV at the caret, via the editor's own keys (so layout,
   syntax and the caret all stay consistent)."
  (loop for ch across string do
    (if (char= ch #\Newline)
        (lol.textview:tv-key tv :return 0)
        (lol.textview:tv-key tv ch 0))))

(defun ws-trim-output (output)
  "OUTPUT with a trailing newline trimmed — NIL when the form printed nothing, so a
   caller can cleanly test 'was there a transcript?'."
  (let ((s (string-right-trim '(#\Newline) (or output ""))))
    (and (plusp (length s)) s)))

(defun workspace-do-it (pane &optional toplevel)
  "Eval the form before point (C-x C-e), the selection when there is one, or with
   TOPLEVEL the top-level form around the caret (C-c C-e); show the value (or error),
   and any printed output, in the status line. The compiler's notes follow a value
   (\"=> FOO   [undefined function: BAR]\"); an error stands alone — calling an
   undefined function both warns and fails, and the failure says it all."
  (multiple-value-bind (form foundp) (form-before-point (pane-tv pane) toplevel)
    (if (not foundp)
        (setf *browser-status* (if toplevel "Do it: no form here" "Do it: no form before point"))
        (multiple-value-bind (ok res out notes) (ws-eval form)
          (let ((printed (ws-trim-output out)))
            (setf *browser-status*
                  (if ok (let ((*print-length* 40) (*print-level* 5))
                           (format nil "~a ~@[~a  ~]=> ~s~@[   [~{~a~^; ~}]~]" (form-echo form)
                                   (and printed (substitute #\Space #\Newline printed)) res notes))
                      (format nil "~a ~@[~a  ~]error: ~a" (form-echo form)
                              (and printed (substitute #\Space #\Newline printed))
                              (ws-remove-string "COMMON-LISP-USER::" (princ-to-string res))))))))))

(defun workspace-print-it (pane)
  "Eval the form before point and weave the transcript into the buffer, inline after
   the form — the notebook that accretes an editable, re-runnable session. Anything
   the form printed is woven first (verbatim), then the value; so is an error."
  (multiple-value-bind (form foundp) (form-before-point (pane-tv pane))
    (when foundp
      (multiple-value-bind (ok res out) (ws-eval form)
        (let ((printed (ws-trim-output out)))
          (ws-insert (pane-tv pane)
                     (format nil "~%~@[~a~%~]~a~%"
                             printed
                             (if ok (let ((*print-length* 40) (*print-level* 5))
                                      (format nil "=> ~s" res))
                                 (format nil "!! ~a" res))))
          (setf *browser-status* nil))))))

(defun workspace-inspect-it (pane)
  "Eval the form before point and open the value in the inspector (a new trail
   level) — the landing pad that makes 'Inspect it' from anywhere land here."
  (multiple-value-bind (form foundp) (form-before-point (pane-tv pane))
    (if (not foundp)
        (setf *browser-status* "Inspect it: no form before point")
        (multiple-value-bind (ok res) (ws-eval form)
          (if ok
              (progn (setf *browser-status* (format nil "~a => inspecting" (form-echo form)))
                     (trail-push res))
              (setf *browser-status* (format nil "~a error: ~a" (form-echo form) res)))))))

(defun workspace-debug-it (pane)
  "Eval the form before point with the debugger ARMED. We catch with HANDLER-BIND,
   not the outer handler-case, precisely because a handler-bind handler runs BEFORE
   the unwind — with the erroring stack still standing — so run-debugger-fb sees the
   live restarts and backtrace, and invoking a restart from there actually transfers
   control. An ABORT restart wraps the eval, guaranteeing a way back to the
   workspace. (run-debugger-fb lives in fb-browser.lisp; funcall avoids a load-order
   forward reference.)"
  (multiple-value-bind (form foundp) (form-before-point (pane-tv pane))
    (when foundp
      (restart-case
          (handler-bind ((serious-condition
                           (lambda (c) (funcall 'run-debugger-fb c))))
            (let ((v (eval form)))
              (setf *browser-status*
                    (let ((*print-length* 40) (*print-level* 5)) (format nil "=> ~s" v)))))
        (abort () :report "Return to the workspace"
          (setf *browser-status* "aborted"))))))

;;; ---- drawing -------------------------------------------------------------

(defun item-color (item)
  "The stripe/label colour for ITEM — its concept colour, except an effective-method
   row (kind :method) is tinted by its run PHASE, so the onion reads as a waterfall:
   around gold, before blue, primary green, after a cool blue.

   The phase is read from the item's GROUP. It used to be read by string-matching the
   DETAIL text, which meant the colour silently depended on how a sentence started —
   exactly the coupling the GROUP slot was added to remove."
  (let ((g (item-group item)))
    (or (and (eq (item-kind item) :method) (stringp g)
             (cond ((string= g "around") !#FFCB6B)
                   ((string= g "before") !#89DDFF)
                   ((string= g "after")  !#5A8CA0)))
        (concept-color (item-kind item)))))

(defun item->row (item)
  "ITEM (the present protocol's) -> a LOL.WIDGETS:ROW (the toolkit's). This little
   adapter is the whole boundary between the two layers: the widget never learns what
   a subject or a disposition is, and the protocol never learns what a stripe is."
  (let ((kc (item-color item)))
    (lol.widgets:make-row
     (item-label item)
     :detail (and (item-detail item) (princ-to-string (item-detail item)))
     :tint kc
     ;; the kind stripe: a row reads by kind (function teal, class blue, macro
     ;; purple, an onion layer by its phase) before it is read at all
     :stripe (and (item-kind item) kc)
     ;; » = drilling opens something; amber when it opens ELSEWHERE than in place
     :marker (and (item-subject item)
                  (if (eq (item-disposition item) :in-place) :dim :bright))
     :group (item-group item))))

(defun draw-list-body (canvas pane x y w h)
  "Render a list view: the widget does rows, sections, selection and the scrollbar;
   this decides only WHAT the rows say. PANE-TOP is the widget's viewport position
   (a layout slot, headers included) and is written back so it persists across frames,
   and the rows drawn go into the hit map of the pane's own LIST-BODY morph (BODY-ROWS):
   a split draws two lists, the pane's and its side's, and each body keeps its own —
   when one special held both, whichever went second used to overwrite the other's
   map, so a click in the left list hit-tested against the right one's rows."
  (let ((rows (map 'vector #'item->row (view-items (pane-view pane)))))
    (multiple-value-bind (top hits)
        (lol.widgets:draw-list canvas x y w h rows
                               :sel (pane-sel pane) :top (pane-top pane)
                               :follow-sel (pane-follow pane)
                               ;; the inspector is a name/value TABLE, not a list with
                               ;; annotations: the value is what you came to read, so it
                               ;; gets its own column and full-strength text
                               :value-col (when (eq (view-kind (pane-view pane)) :inspector)
                                            (min (* 50 (char-w)) (floor w 2))))
      (setf (pane-top pane) top)
      (let ((b (pane-body pane)))
        (when (typep b 'list-body) (setf (body-rows b) hits))))))

;;; Layout heights that depend on the text — all derived from the current font, so a
;;; font-size change relays them out on the next frame. The comments give the pixel
;;; values they had as constants under the old 6x15 bitmap font.
(defun status-h () "Height of the status line / minibuffer (22 at 6x15)." (+ (line-h) 7))
(defun status-text-y (h) "Where the status line's text goes, in a frame H tall."
  (+ (- h (status-h)) (text-dy (status-h)) 1))
(defun pane-header-text-y (body-y)
  "The y of a text drawn ON a pane's header band, given where its BODY starts (the band
   is +BAR-H+ tall and ends 4 px above the body) — the line the panel's title sits on."
  (+ (- body-y 4 +bar-h+) (text-dy +bar-h+)))

(defun draw-hints (canvas x y w hints)
  "Draw HINTS — a list of (CHORD . MEANING) — along the status line, the CHORD in the
   accent and the meaning dim. Colouring the chord is the whole point: a status line
   of uniformly grey words is read as prose and therefore not read at all, whereas
   `ret drill` with `ret` lit reads as a key and its effect. A NIL chord is plain text.
   Stops when it runs out of width rather than spilling off the edge."
  (let ((pen x))
    (dolist (h hints)
      (destructuring-bind (chord . meaning) h
        (let* ((cw (if chord (+ (lol.canvas:string-px *bfont* chord) (char-w)) 0))   ; 5 px and 12 px
               (mw (+ (lol.canvas:string-px *bfont* meaning) (* 2 (char-w)))))  ; at 6x15
          (when (> (+ pen cw mw) (- w 8)) (return))
          (when chord
            (lol.canvas:draw-string canvas *bfont* chord pen y *ui-accent*)
            (incf pen cw))
          (lol.canvas:draw-string canvas *bfont* meaning pen y *ui-dim*)
          (incf pen mw))))
    pen))

;;; ---- the clickable dispatch matrix (§3) ----------------------------------

(defun draw-matrix-body (canvas pane x y w h)
  "Render a 2-axis dispatch matrix as a clickable grid. The widget draws the cells,
   the header band and the overflow notes; this turns the model's cell CONTENTS
   (lists of methods) into the counts the grid shows — a dim mark for a real gap,
   which is the whole point of the view — and adds the line that says what a cell is
   FOR. Clicking a cell is the only way into the effective-method onion, and an
   affordance nothing announces may as well not exist (design mock-up §2 spells the
   same line out: \"effective method — select a cell…\")."
  (let* ((m (view-data (pane-view pane)))
         (rows (getf m :rows))
         (cols (getf m :cols))
         (counts (mapcar (lambda (cellrow) (mapcar #'length cellrow)) (getf m :cells))))
    (multiple-value-bind (hits bottom)
        (lol.widgets:draw-grid canvas x y (- w 2) (- h (line-h) 5) rows cols counts)
      (let ((b (pane-body pane)))              ; a click reads it back (MATRIX-CELL-CLICK)
        (when (typep b 'grid-body) (setf (body-cells b) hits)))
      ;; the caption sits directly under the grid, where the design puts it — at the
      ;; foot of a mostly-empty pane it would be furniture nobody reads
      (draw-hints canvas x (+ bottom 6) w
                  '(("click a cell" . "its effective-method onion")
                    (nil . "a dim dot is a real gap in the coverage"))))))

;;; ---- the stage (§3b): instances, moving, dispatching for real ---------------
;;; The one view whose body changes without input. Everything here reads the stage and
;;; draws; STEP-STAGE (initramfs/stage.lisp) is the only thing that advances it, driven
;;; by the browser's poll timeout in fb-browser.lisp. No colour is invented: a sprite is
;;; :class blue because it is an instance of a class, an impact flashes :method green
;;; because what you are being shown is a method, and a signal flashes :condition red.

(defun stage-transcript-h ()
  "Height of the transcript strip under the stage: a heading and five lines (92 px
   under the old 6x15 font, when it was the constant +STAGE-TRANSCRIPT-H+)."
  (+ 7 (* 6 (line-h))))

(defun stage-sprite-points (b)
  "The silhouette of body B in model space, as a list of (x . y) — scaled to its own
   radius, unrotated. Class identity is carried by SHAPE, which is what frees colour to
   keep meaning what it means everywhere else in the environment."
  (let ((r (body-r b)))
    (flet ((p (fx fy) (cons (round (* r fx)) (round (* r fy)))))
      (ecase (body-kind b)
        ;; a chevron with a notched tail — a thing with a front
        (:ship (list (p 1 0) (p -0.65 0.68) (p -0.32 0) (p -0.65 -0.68)))
        ;; a narrow dart: all nose, no body
        (:missile (list (p 1.5 0) (p -1 0.5) (p -0.5 0) (p -1 -0.5)))
        ;; an irregular rock, its jitter fixed per instance so it tumbles rather than boils
        (:asteroid
         (loop for m in (or (body-poly b) '(100 100 100 100 100 100 100 100 100))
               for i from 0
               for th = (/ (* 2 pi i) (length (body-poly b)))
               collect (cons (round (* r (/ m 100.0) (cos th)))
                             (round (* r (/ m 100.0) (sin th))))))
        ;; a hexagon: built, not born
        (:station
         (loop for i below 6
               for th = (/ (* 2 pi i) 6)
               collect (cons (round (* r (cos th))) (round (* r (sin th))))))))))

(defun stage-body-color (b stage)
  "A body's outline colour. Flash wins (it is the whole point of the frame it appears
   in), then selection, then the plain :class blue every instance wears."
  (cond ((plusp (body-flash b))
         (concept-color (if (eq (body-flash-kind b) :condition) :condition :method)))
        ((eq b (stage-selected stage)) *ui-accent*)
        (t (concept-color :class))))

(defun draw-stage-sprite (canvas b stage ox oy)
  "One body, rotated to its facing and translated into the viewport at OX,OY."
  (let* ((color (stage-body-color b stage))
         ;; the classes with a front point where they are going; the rest tumble
         (th (if (member (body-kind b) '(:ship :missile))
                 (atan (body-vy b) (body-vx b))
                 (/ (body-rot b) 1000.0)))
         (c (cos th)) (sn (sin th))
         (r (body-r b))
         (pts (mapcar (lambda (p)
                        (cons (+ ox (body-x b) (round (- (* (car p) c) (* (cdr p) sn))))
                              (+ oy (body-y b) (round (+ (* (car p) sn) (* (cdr p) c))))))
                      (stage-sprite-points b))))
    (lol.canvas:draw-poly canvas pts color)
    ;; the station's ring and core — a hexagon alone reads as a rock
    (when (eq (body-kind b) :station)
      (lol.canvas:draw-circle canvas (+ ox (body-x b)) (+ oy (body-y b))
                              (round (* r 42) 100) color)
      (lol.canvas:draw-circle canvas (+ ox (body-x b)) (+ oy (body-y b)) 3 color :fill t))
    ;; a survived hull breach is a scar the ship keeps wearing
    (when (body-breached b)
      (lol.canvas:draw-line canvas
                            (+ ox (body-x b) -4) (+ oy (body-y b) -5)
                            (+ ox (body-x b) 7)  (+ oy (body-y b) 4)
                            (concept-color :condition)))
    ;; the selection halo
    (when (eq b (stage-selected stage))
      (lol.canvas:draw-circle canvas (+ ox (body-x b)) (+ oy (body-y b))
                              (+ (body-r b) 8) *ui-accent*))
    ;; NAME is a slot reader, not a caption — it is drawn because the transcript names
    ;; the same string, and the eye has to be able to join the two. It sits to the
    ;; right of the sprite unless that would run it off the viewport, in which case it
    ;; flips to the left: a body drifting into the right-hand wall was pushing its
    ;; label out of the pane and into the divider beside it.
    (let* ((nm     (lol.examples:name (body-obj b)))
           (npx    (lol.canvas:string-px *bfont* nm))
           (docked (body-docked b))
           (right  (+ ox (body-x b) (body-r b) 6))                 ; the usual side
           (left   (- (+ ox (body-x b)) (body-r b) 6 npx))         ; ends before the sprite
           ;; A DOCKED ship sits on the station's rim, so a label beside it lands on
           ;; top of the station's own and the two read as one garbled word
           ;; ("GeneraLambda"). Docking is the only overlap that PERSISTS — collisions
           ;; are over in a frame — so it is the only one worth moving out of the way:
           ;; the ship's name goes underneath, where the radii keep the rows apart.
           (nx     (cond (docked (- (+ ox (body-x b)) (floor npx 2)))
                         ((> (+ right npx) (+ ox (stage-w stage))) left)
                         (t right)))
           (ny     (if docked
                       (+ oy (body-y b) (body-r b) 4)
                       (+ oy (body-y b) -6))))
      (lol.canvas:draw-string canvas *bfont* nm (max ox nx) ny
                              (if (eq b (stage-selected stage)) *ui-accent* *ui-dim*)))))

(defun draw-stage-transcript (canvas stage x y w h)
  "The tail of the transcript, oldest at the top. Each line is coloured by what it
   records: a value in :method green, an absent direction in :condition red, a
   disagreement in amber."
  (lol.canvas:fill-rect canvas x y w 1 *ui-rule*)
  (lol.canvas:draw-string canvas *bfont* "TRANSCRIPT" x (+ y 5) *ui-dim*)
  (let* ((step (line-h))                 ; one text line; was 14 under the 6x15 font
         (rows (max 1 (floor (- h step 7) step)))
         (tail (reverse (subseq (stage-transcript stage)
                                0 (min rows (length (stage-transcript stage))))))
         (ty (+ y step 5))
         (maxc (max 8 (floor (- w 8) (lol.canvas:font-cw *bfont*)))))
    (dolist (e tail)
      (lol.canvas:draw-string canvas *bfont*
                              (lol.widgets:clip-text (getf e :text) maxc)
                              x ty
                              (case (getf e :kind)
                                (:value (concept-color :method))
                                (:absent (concept-color :condition))
                                (:warn *ui-amber*)
                                (t *ui-dim*)))
      (incf ty step))))

(defun draw-stage-body (canvas pane x y w h)
  "Render a :stage view: the viewport with its sprites, the heads-up line naming what
   would dispatch next, and the transcript strip along the bottom.

   The stage is told the viewport size here rather than at construction, because the
   pane is the authority on how much room there is — resize the window and the world
   gets bigger, which is the behaviour you would want anyway."
  (let* ((stage (view-data (pane-view pane)))
         (vh (- h (stage-transcript-h)))
         (ox x) (oy y))
    (stage-resize stage (max 80 w) (max 80 vh))
    (let ((b (pane-body pane)))                   ; for clicks, and the halo's sprites
      (when (typep b 'stage-body) (setf (body-viewport b) (list x y w vh))))
    (lol.canvas:fill-rect canvas x y w vh !#0E1013)
    (dolist (b (stage-bodies stage))
      (draw-stage-sprite canvas b stage ox oy))
    ;; the heads-up line: compute-applicable-methods asked BEFORE the contact, so the
    ;; view tells you what will happen rather than only what did
    (let ((hud (stage-hud-line stage)))
      (lol.canvas:draw-string canvas *bfont*
                              (lol.widgets:clip-text hud (max 8 (floor (- w 8) (lol.canvas:font-cw *bfont*))))
                              (+ x 4) (- (+ y vh) (line-h) 1)
                              (if (search "will signal" hud) (concept-color :condition) *ui-dim*)))
    (unless (stage-running stage)
      (lol.canvas:draw-string canvas *bfont* "PAUSED" (+ x 4) (+ y 4) *ui-amber*))
    (draw-stage-transcript canvas stage x (+ y vh) w (stage-transcript-h))))

(defun stage-click (pane px py)
  "A click in the stage viewport selects the body under the pointer — and a selected
   body is a subject, so Enter opens the Inspector on the instance itself. NIL when the
   click missed every sprite (which clears the selection)."
  (let ((stage (view-data (pane-view pane))))
    (when (pane-stage-rect pane)
      (destructuring-bind (x y w vh) (pane-stage-rect pane)
        (when (and (<= x px (+ x w)) (<= y py (+ y vh)))
          (let ((hit (find-if (lambda (b)
                                (let ((dx (- px x (body-x b))) (dy (- py y (body-y b))))
                                  (<= (+ (* dx dx) (* dy dy))
                                      (let ((rr (+ (body-r b) 6))) (* rr rr)))))
                              (reverse (stage-bodies stage)))))
            (setf (stage-selected stage) hit)
            t))))))

(defun stage-open-workspace (pane &optional text)
  "Open (or reuse) the Workspace BESIDE the stage and focus it, optionally replacing
   its text.

   The stage is the one subject whose side pane is editable rather than a read-only
   preview (§3b) — the whole point is to type a form and watch what it does to the
   world next to it. open-beside can't serve here because it works off a selected
   LIST ROW and a stage has no rows, so this is the stage's own way in."
  (unless (pane-side pane)
    (setf (pane-side pane) (or (pane-side-stash pane)
                               (open-pane (subj-stage-workspace)))
          (pane-side-stash pane) nil))
  (setf (pane-editing-side pane) t)
  (when text
    (let ((tv (pane-tv (pane-side pane))))
      (when tv (lol.textview:tv-set-text tv text))))
  (pane-side pane))

(defun stage-take-template (pane)
  "Deliver the [define-method] restart's template, if the debugger left one.

   stage-defmethod-template hands back the defmethod the debugger already knew how to
   write — its specialisers come from the two objects that failed to dispatch — and it
   lands in the Workspace beside the stage with focus, ready for C-c C-c. The point is
   that you never retype what the system already worked out."
  (let* ((stage (view-data (pane-view pane)))
         (tpl (stage-defmethod-template stage)))
    (when tpl
      (stage-open-workspace
       pane
       (format nil ";; from [define-method] — fill in the tag, then C-c C-c to define it.~%~a~%" tpl))
      ;; and STOP the world while you write it. The pair that could not dispatch is
      ;; still on the stage and still moving, and everything else is still colliding;
      ;; leaving it running means the debugger reopens over the half-written method it
      ;; just asked you for. Space resumes when you are ready to see the difference.
      (setf (stage-running stage) nil)
      (stage-log stage "paused — writing a method for this pair" :note)
      (setf *browser-status* "defmethod template — edit the tag, C-c C-c to Accept, spc to resume")
      t)))

(defun stage-key (pane key)
  "The stage's own keys. Returns T when KEY was one of them.

   These are the same verbs COMMANDS-FOR offers on the subject — the keys are the
   accelerator, the menu is the discoverable form, and neither is the authority.
   Reached only when focus is NOT on the side pane, or 's' would spawn a ship every
   time you typed one into the Workspace."
  (let ((stage (view-data (pane-view pane))))
    (case key
      (#\Space (setf (stage-running stage) (not (stage-running stage))) t)
      (#\n (step-stage stage) t)
      (#\s (stage-spawn stage :ship) t)
      (#\a (stage-spawn stage :asteroid) t)
      (#\m (stage-spawn stage :missile) t)
      (#\t (stage-spawn stage :station) t)
      ;; w HIDES the Workspace rather than closing it: it holds text you typed, and
      ;; very likely a defmethod the debugger just handed you. Stash the pane and put
      ;; the SAME buffer back on the next w.
      (#\w (if (pane-side pane)
                (setf (pane-side-stash pane) (pane-side pane)
                      (pane-side pane) nil
                      (pane-editing-side pane) nil)
                (stage-open-workspace pane))
           t)
      (#\r (reset-collide-stage)
           (setf (pane-view pane) (present (pane-subject pane))) t)
      ;; Tab cycles the selection through the bodies. The mouse is an ACCELERATOR here,
      ;; not the way in (doc §1's rule, and the reason every list has arrow keys): a
      ;; stage you can only select on by clicking is a stage you cannot use on hardware
      ;; whose pointer has not been brought up yet.
      (:tab (let* ((bs (stage-bodies stage))
                   (i (position (stage-selected stage) bs)))
              (setf (stage-selected stage)
                    (when bs (nth (if i (mod (1+ i) (length bs)) 0) bs))))
            t)
      ;; a sprite is an INSTANCE, and an instance is a subject: Enter opens the
      ;; Inspector on it, the same view any other object gets. Nothing selected yet?
      ;; Select the first, so Enter is never simply inert.
      ((:return #\Return)
       (let ((sel (or (stage-selected stage) (first (stage-bodies stage)))))
         (if (eq sel (stage-selected stage))
             (when sel (trail-push (body-obj sel)))
             (setf (stage-selected stage) sel)))
       t)
      (t nil))))

(defun matrix-cell-click (pane px py)
  "A click on a dispatch-grid cell -> drill the effective-method onion for that
   (row-class, col-class) pair, building the full argument tuple (T elsewhere) from
   the matrix's arity and axis positions. NIL when the click missed every cell."
  (let ((hit (lol.widgets:hit-cell (pane-cells pane) px py)))
    (when hit
      (destructuring-bind (r . c) hit
        (let* ((m (view-data (pane-view pane)))
               (arity (or (getf m :arity) 2))
               (rp (getf m :row-pos)) (cp (getf m :col-pos))
               (tuple (loop for i below arity
                            collect (cond ((eql i rp) r) ((eql i cp) c) (t t)))))
          (trail-push (subj-onion (getf m :name) tuple))
          t)))))

(defun view-kind-color (view)
  "The semantic tint for a VIEW — the concept colour of what it opens on, so a Workspace,
   a package, a class, a generic function each read by colour in the chrome."
  (concept-color (view-kind-concept (view-kind view))))

(defun trail-kind-color (trail)
  "The semantic tint for TRAIL's tab — from its ROOT view (see VIEW-KIND-COLOR)."
  (view-kind-color (pane-view (first trail))))

;;; ---- the systray: the right end of the tab strip -----------------------------
;;; What the user's StumpWM mode line shows on the host, in the same order — battery,
;;; then the date and time — plus an icon for the clipboard history. The DATA comes
;;; from initramfs/sysinfo.lisp (TRAY-ITEMS); this only lays it out. It is drawn into
;;; its own rectangle so the clock can tick by repainting just that strip
;;; (DRAW-TRAY), not the whole frame. See doc/systray.org.

(defun tray-tone-color (tone)
  "A tray tone as a colour — the battery's green / yellow / red (the host script's),
   and the agent's authority: teal read-only, amber ask, red allow, dim off."
  (case tone
    ((:ok :charging) (concept-color :method))     ; green
    (:warn *ui-amber*)
    (:low (concept-color :condition))             ; red
    (:safe *ui-accent*)                           ; the agent read-only (doc/agent.org §8.9)
    (t *ui-dim*)))

(defun draw-clip-icon (canvas x y color)
  "A tiny clipboard, 10x13: the board and the clip at its top."
  (lol.canvas:draw-rect canvas x (+ y 2) 10 11 color)
  (lol.canvas:fill-rect canvas (+ x 3) y 4 4 color)
  (lol.canvas:fill-rect canvas (+ x 2) (+ y 6) 6 1 color)
  (lol.canvas:fill-rect canvas (+ x 2) (+ y 9) 4 1 color))

(defparameter +agent-icon-rows+
  #(".XXXXXXXX."
    ".X.XXXX.X."
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    ".XXXXXXXX."
    ".X.X..X.X.")
  "The agent's tray icon, 10x6 cells: Claude's little orange creature — a block of a
   body with two eyes punched out, arms at its sides, four legs. X is painted.")

(defparameter *agent-icon-color* !#D97757 "Claude's orange — the agent icon's body.")

(defun agent-icon-scale ()
  "Pixels per icon cell: as big as the tab strip comfortably holds (2 at 6x15)."
  (max 1 (floor (- +bar-h+ 8) (length +agent-icon-rows+))))

(defun agent-icon-width ()
  (* (agent-icon-scale) (length (aref +agent-icon-rows+ 0))))

(defun draw-agent-icon (canvas x color)
  "Paint the agent's icon (+AGENT-ICON-ROWS+) at X, centred in the tab strip. COLOR is
   the body's colour; the eyes are holes, so the strip shows through."
  (let* ((s (agent-icon-scale))
         (rows +agent-icon-rows+)
         (y0 (floor (- +bar-h+ (* s (length rows))) 2)))
    (loop for row across rows for r from 0
          do (loop for ch across row for c from 0
                   when (char= ch #\X)
                     do (lol.canvas:fill-rect canvas (+ x (* c s)) (+ y0 (* r s)) s s color)))
    (+ x (* s (length (aref rows 0))))))

(defun tray-item-icon-width (it)
  "Extra width an item's :ICON takes before its text (the agent's creature), or 0."
  (if (eq (getf it :icon) :agent) (+ (agent-icon-width) 5) 0))

(defun tray-width ()
  "How wide the tray will be, so the tabs can stop short of it."
  (let ((items (ignore-errors (tray-items))))
    (+ 8 14 (loop for it in items
                  sum (+ 16 (tray-item-icon-width it) (lol.canvas:string-px *bfont* (getf it :text))))
       8)))

(defun draw-tray (canvas w)
  "Paint the tray, right-aligned at W in the tab strip: [clip] | BAT 87% | 2026-10-02
   09:01. An item with :ICON :AGENT is drawn as the agent's creature, then its text.
   Returns (values X0 CLIP): the tray's left edge, and the clipboard icon's x-range
   (x0 . x1) for clicks (the TRAY morph keeps it — poc/morphic.lisp)."
  (let* ((items (ignore-errors (tray-items)))
         (tw (tray-width))
         (x0 (- w tw))
         (pen (+ x0 8))
         (clip (cons (- pen 3) (+ pen 13))))
    (lol.canvas:fill-rect canvas x0 0 tw (- +bar-h+ 1) *ui-bg*)
    (draw-clip-icon canvas pen (floor (- +bar-h+ 13) 2) (if (lol.clipboard:clip-current) *ui-accent* *ui-dim*))
    (incf pen 14)
    (dolist (it items)
      (lol.canvas:draw-string canvas *bfont* "|" (+ pen 4) (text-dy +bar-h+) *ui-rule*)
      (incf pen 16)
      (when (eq (getf it :icon) :agent)
        (setf pen (+ 5 (draw-agent-icon canvas pen (if (eq (getf it :tone) :off) *ui-dim* *agent-icon-color*)))))
      (setf pen (lol.canvas:draw-string canvas *bfont* (getf it :text) pen (text-dy +bar-h+)
                                        (if (member (getf it :kind) '(:battery :wifi :wired :agent :recording))   ; :agent — agent-ui.lisp; :recording — recorder.lisp
                                            (tray-tone-color (getf it :tone))
                                            *ui-text*))))
    (values x0 clip)))

(defvar *trail-windowed-p* nil
  "A function (TRAIL) -> true when TRAIL is shown in a window of its own rather than
   in the tab strip (poc/morphic.lisp installs it). NIL: no windows, every trail tiled.")

(defun trail-windowed-p (trail)
  (and *trail-windowed-p* (funcall *trail-windowed-p* trail)))

(defun draw-tabs (canvas w)
  "The top tab strip: one tab per open trail, each with a kind-coloured stripe and its
   SuperL-number badge; the active one is highlighted (brighter bg + accent top-rule +
   an × close), and a + after the last tab opens a fresh Workspace. The design mock-up
   (media/browser-views-design.html §1) puts the × on EVERY tab, not only the focused
   one — closing a tab you are not in is the common case. Returns its hit maps, so
   clicks and drawing can't disagree (the TAB-STRIP morph keeps them):
   (values TABS CLOSES PLUS) — ((trail-index . (x0 . x1)) …) for the tabs and for their
   × boxes, and the + button's (x0 . x1). The systray at the right end is a morph of
   its own, drawn after this one."
  (let ((x 8) (multi (cdr *trails*))                        ; align with the 8px gutter below
        (tabs '()) (closes '()) (plus nil))
    (lol.canvas:fill-rect canvas 0 0 w +bar-h+ *ui-bg*)
    (loop for trail in *trails* for i from 0
          for active = (= i *active*)
          for closer = multi                     ; every tab gets a ×, once there are >1
          for num = (format nil " ~d ~c " (1+ i) (code-char #xB7))  ; " 1 · "
          for title = (format nil "~a " (clip-text (trail-title trail) 16))
          for lw = (+ (lol.canvas:string-px *bfont* num) (lol.canvas:string-px *bfont* title))
          for tw = (+ lw (if closer (+ (char-w) 8) 0))   ; the x box
          while (or (trail-windowed-p trail)       ; a window's trail takes no room here
                    (< (+ x tw) (- w (+ (char-w) 20) (tray-width))))  ; room for the + button and the tray
          ;; a trail torn off into a window (doc/morphic.org §5) has no tab: its title
          ;; bar is its tab. It keeps its NUMBER, so SuperL-N still reaches it.
          unless (trail-windowed-p trail)
          do (progn
               (lol.canvas:fill-rect canvas x 0 tw +bar-h+ (if active *ui-hi* *ui-panel*))
               (lol.canvas:fill-rect canvas x 0 tw 2 (trail-kind-color trail))   ; kind stripe on top
               ;; number quiet, title bright (active accent, inactive plain text)
               (let ((pen (lol.canvas:draw-string canvas *bfont* num (+ x 2) (text-dy +bar-h+) *ui-dim*)))
                 (lol.canvas:draw-string canvas *bfont* title pen (text-dy +bar-h+)
                                         (if active *ui-accent* *ui-text*)))
               ;; a conversation's tab carries the agent's dot over its " · " (doc/agent.org
               ;; §8.9): colour = authority, filled = :interacting, pulsing = working
               (multiple-value-bind (dot filled)
                   (and (fboundp 'trail-agent-dot) (funcall 'trail-agent-dot trail))
                 (when dot
                   ;; the dot covers the "·" cell, so it grows with the font: 6 px at 6x15
                   (let* ((s (max 6 (- (char-w) 3)))
                          (dx (+ x 2 (lol.canvas:string-px *bfont* (format nil " ~d " (1+ i)))))
                          (dy (- (floor +bar-h+ 2) (floor s 2))))
                     (lol.canvas:fill-rect canvas (- dx 1) (- dy 1) (+ s 2) (+ s 2)
                                           (if active *ui-hi* *ui-panel*))
                     (if filled
                         (lol.canvas:fill-rect canvas dx dy s s dot)
                         (lol.canvas:draw-rect canvas dx dy s s dot)))))
               (when closer
                 (let ((cx (+ x (lol.canvas:string-px *bfont* num) (lol.canvas:string-px *bfont* title))))
                   (lol.canvas:draw-string canvas *bfont* (string (code-char 215)) cx (text-dy +bar-h+)
                                           (if active *ui-dim* *ui-rule*))   ; quieter when unfocused
                   (push (cons i (cons cx (+ cx (char-w) 8))) closes)))
               (push (cons i (cons x (+ x tw))) tabs)
               (incf x (+ tw 6))))                ; 6px gap between tabs
    ;; the + new-tab button
    (let ((px (+ x 2)))
      (lol.canvas:fill-rect canvas px 0 (+ (char-w) 8) +bar-h+ *ui-panel*)
      (lol.canvas:draw-string canvas *bfont* "+" (+ px 4) (text-dy +bar-h+) *ui-accent*)
      (setf plus (cons px (+ px (char-w) 8)))
      (setf x (+ px (char-w) 8)))
    ;; a hairline seam under the whole strip
    (lol.canvas:fill-rect canvas 0 (- +bar-h+ 1) w 1 *ui-rule*)
    (values tabs closes plus)))

(defun draw-crumb (canvas x y w pane)
  "A collapsed ancestor bar, W wide at X,Y: a kind-coloured stripe, the subject's
   title, and what was picked inside it. The stripe is what makes a deep trail
   readable at a glance — four grey bars say nothing, but gold/blue/green says
   package, class, generic."
  (let ((tint (view-kind-color (pane-view pane)))
        (sel (selected-item pane)))
    (lol.widgets:draw-bar canvas x y w
                          :marker ">" :label (view-title (pane-view pane)) :tint tint
                          :right (and sel (format nil "picked ~a" (item-label sel))))
    (lol.canvas:fill-rect canvas x y 3 +bar-h+ tint)))      ; the kind stripe

(defun preview-for (pane subject)
  "The cached read-only PANE that PANE's nav split shows for SUBJECT (its right-hand
   column), rebuilt only when the selection moves to a different subject. NIL for a nil
   subject. The preview is never focused — it is a look, not an edit surface; drilling
   (Enter) is what opens a real editable pane. Each list pane keeps its own (PANE-
   PREVIEW): a window's list and the tiled one's no longer share one cache."
  (when subject
    (unless (eq subject (pane-preview-subject pane))
      (setf (pane-preview-subject pane) subject
            (pane-preview pane) (ignore-errors (open-pane subject :preview t)))
      (let ((tv (and (pane-preview pane) (pane-tv (pane-preview pane)))))
        (when tv (setf (lol.textview:tv-focus tv) nil))))  ; read-only: no caret
    (pane-preview pane)))

;;; Each body morph draws itself (poc/morphic.lisp: the DRAW-MORPH methods of LIST-BODY,
;;; TEXT-BODY, GRID-BODY …), calling the function above or below that paints its kind;
;;; there is no one function that knows every kind any more, and nothing beside the
;;; morph tree for the halo to read: a body under the pointer IS the halo's layer.

(defparameter +non-list-kinds+ '(:matrix :stage :image :web :terminal)
  "View kinds with no textview that are NOT lists either: they draw themselves, and
   the list machinery (rows, the nav split, row hit-tests) must leave them alone.")

;;; ---- an image (doc/file-manager.org): pixels, scaled into the pane ---------------
;;; The view carries the decoded pixels in the canvas's own #x00RRGGBB layout
;;; (lol.image, poc/image.lisp, decodes into exactly that), so showing one is a
;;; scaled copy and nothing else — and since both backends present this same canvas,
;;; drawing it here is enough for /dev/fb0 and X11 alike.

(defun image-fit (iw ih w h)
  "The size IW x IH is drawn at inside W x H: shrunk to fit with its aspect kept (the
   same rounding as lol.image's SCALE-TO-FIT, so its result can be used as is); a
   small image is enlarged only by a WHOLE factor (up to 8x), so pixel art stays crisp
   instead of smearing across fractional pixels."
  (if (or (> iw w) (> ih h))
      (let ((s (min (/ w iw) (/ h ih))))
        (values (max 1 (min w (round (* iw s)))) (max 1 (min h (round (* ih s))))))
      (let ((k (max 1 (min 8 (floor w iw) (floor h ih)))))
        (values (* iw k) (* ih k)))))

(defun scaled-pixels (data nw nh)
  "DATA's pixels (a view-data plist: :width :height :pixels) resampled to NW x NH.
   lol.image's SCALE-TO-FIT (an area average, so a photo shrinks without shimmering)
   when it is loaded and the image is shrinking, else nearest neighbour — which is
   also exactly right for the whole-factor enlargement IMAGE-FIT asks for. The caller
   caches the result per size, so this runs once per pane size, not per frame."
  (let* ((iw (getf data :width)) (ih (getf data :height)) (src (getf data :pixels))
         (fit (let* ((p (find-package "LOL.IMAGE"))
                     (s (and p (find-symbol "SCALE-TO-FIT" p))))
                (and s (fboundp s) (symbol-function s)))))
    (or (and fit (or (> iw nw) (> ih nh))
             (ignore-errors
               (multiple-value-bind (w2 h2 p2) (funcall fit iw ih src nw nh)
                 (and (eql w2 nw) (eql h2 nh) p2))))
        (let ((out (make-array (* nw nh) :element-type '(unsigned-byte 32))))
          (dotimes (y nh)
            (let ((sy (* (floor (* y ih) nh) iw)))
              (dotimes (x nw)
                (setf (aref out (+ (* y nw) x))
                      (aref src (+ sy (floor (* x iw) nw)))))))
          out))))

(defun draw-image-body (canvas pane x y w h)
  "Render an :IMAGE view: the picture centred and fitted in the rect, on a
   checkerboard ground (so a transparent region is visibly transparent rather than
   'black'), with a caption of its true size and the scale it is shown at."
  (let* ((data (view-data (pane-view pane)))
         (iw (getf data :width)) (ih (getf data :height))
         (cap-h (+ (line-h) 3)) (bh (max 10 (- h cap-h))))   ; 18 at 6x15
    (multiple-value-bind (nw nh) (image-fit iw ih (- w 8) (- bh 8))
      (let* ((px (let ((cache (getf data :scaled)))
                   (if (and cache (= (first cache) nw) (= (second cache) nh))
                       (third cache)
                       (let ((p (scaled-pixels data nw nh)))
                         (setf (getf (view-data (pane-view pane)) :scaled) (list nw nh p))
                         p))))
             (ox (+ x (floor (- w nw) 2)))
             (oy (+ y (floor (- bh nh) 2))))
        (declare (type (simple-array (unsigned-byte 32) (*)) px))
        (lol.canvas:fill-rect canvas x y w bh !#101216)
        ;; the checkerboard, under the image only
        (loop for cy from 0 below nh by 8 do
          (loop for cx from 0 below nw by 8 do
            (when (evenp (+ (floor cx 8) (floor cy 8)))
              (lol.canvas:fill-rect canvas (+ ox cx) (+ oy cy) (min 8 (- nw cx)) (min 8 (- nh cy))
                                    !#1E2228))))
        ;; the copy, clipped to the canvas's clip (BLIT-PIXELS). Pixels whose top byte
        ;; is zero are opaque in the canvas layout; a decoder that keeps alpha there
        ;; marks transparency with #xFF000000-free words, which we cannot tell from
        ;; black — so draw them all, with the alpha byte masked off.
        (lol.canvas:blit-pixels canvas px nw nh ox oy :mask #xFFFFFF)
        (lol.canvas:draw-rect canvas (1- ox) (1- oy) (+ nw 2) (+ nh 2) *ui-rule*)
        (lol.canvas:draw-string canvas *bfont*
                                (format nil "~dx~d~@[ ~a~]   shown at ~d%"
                                        iw ih (getf data :format)
                                        (round (* 100 (/ nw (max 1 iw)))))
                                (+ x 4) (+ y bh 2) *ui-dim*)))))

(defun list-pane-splittable-p (pane)
  "True when PANE is a plain list whose selection has something to preview — the
   condition for the nav split (a list on the left, its selection previewed right)."
  (and (null (pane-tv pane))
       (not (member (view-kind (pane-view pane)) +non-list-kinds+))
       (let ((sel (selected-item pane)))
         (and sel (item-subject sel)))))

;;; ---- the expanded pane: a panel, its body, and maybe a column beside ----------
;;; A trail on screen is a TRAIL-AREA morph whose children are its panes
;;; (poc/morphic.lisp): the ancestors collapsed to breadcrumb bars (DRAW-CRUMB), the
;;; last one EXPANDED — drawn here. What it shows in its right-hand column, if
;;; anything, is a pane too, a child morph of this one (the side pane you pinned, or
;;; the nav split's preview), which draws its own body after this. So the split is
;;; decided before anything is drawn (EXPANDED-COLUMNS, called by the layout), and
;;; drawing only follows it.

(defun expanded-columns (cur x y w h)
  "How the expanded pane CUR, its panel at X,Y W x H, divides. -> a plist: :KIND —
   :TEXT-SIDE (a text pane with a list beside: the agent's Activity), :STAGE-SIDE (the
   stage with its Workspace), :LIST-SIDE (a list with a pinned side pane), :PREVIEW (a
   list whose selection is previewed) or :SINGLE; :BODY-Y :BODY-H; :MAIN, the (x w)
   of the pane's own body; :SPLIT-X, where the right-hand column starts (NIL when
   there is none); :SIDE, the pane shown there (for :PREVIEW, NIL when the selection
   has nothing to preview); :SIDE-RECT. The proportions are the tiled shell's as they
   have always been, written from the panel's own edges (L, R) so that they hold at
   any X."
  (let* ((l (+ x 4)) (r (+ x w 8)) (full-w (- r 24)) (gap 12)
         (body-y (+ y +bar-h+ 4)) (body-h (- h +bar-h+ 4))
         (is-list (and (null (pane-tv cur))
                       (not (member (view-kind (pane-view cur)) +non-list-kinds+)))))
    (flet ((split (kind main-w side sx sw)
             (list :kind kind :body-y body-y :body-h body-h :main (list l main-w)
                   :split-x sx :side side :side-rect (and side (list sx body-y sw body-h)))))
      (cond
        ;; A TEXT PANE with a list beside it — the agent's conversation with its
        ;; Activity (agent-ui.lisp, layout A of doc/design/agent-workbench.html): the
        ;; transcript keeps the larger part, the activity rail the rest; the focused
        ;; half wears the accent rule, SuperL-o moves focus, as for the stage.
        ((and (pane-tv cur) (pane-side cur) (null (pane-tv (pane-side cur))) (>= r 900))
         (let* ((side-w (max 300 (floor (* full-w 34) 100)))
                (text-w (- full-w side-w gap)))
           (split :text-side text-w (pane-side cur) (+ l text-w gap) side-w)))
        ;; THE STAGE with its Workspace beside (§3b): the one subject whose side pane
        ;; is editable rather than a read-only preview. Same furniture as ⇧↵ below,
        ;; but driven by 'w' rather than a selected row, and the stage keeps the
        ;; larger half because it is the thing you are watching.
        ((and (eq (view-kind (pane-view cur)) :stage) (pane-side cur))
         (let* ((stage-w (max 300 (floor (* full-w 58) 100))) (sx (+ l stage-w gap)))
           (split :stage-side stage-w (pane-side cur) sx (- r sx 12))))
        ;; ⇧↵ EDITABLE SIDE: the list on the left, a pinned editable pane on the right
        ((and is-list (pane-side cur))
         (let* ((list-w (max 260 (floor (* full-w 42) 100))) (sx (+ l list-w gap)))
           (split :list-side list-w (pane-side cur) sx (- r sx 12))))
        ;; NAV SPLIT: list left, a live READ-ONLY preview of the selection right
        ((and is-list (selected-item cur) (item-subject (selected-item cur)))
         (let* ((list-w (max 260 (floor (* full-w 42) 100))) (sx (+ l list-w gap)))
           (split :preview list-w (preview-for cur (item-subject (selected-item cur))) sx (- r sx 12))))
        ;; single pane, full width (source / matrix / a list with nothing to preview)
        (t (list :kind :single :body-y body-y :body-h body-h :main (list l full-w)))))))

(defun draw-pane-expanded (canvas cur x y w h)
  "The trail's current pane, its panel at X,Y W x H: the panel with its title, tint
   and badge; when it is split, the divider, the right-hand column's header and the
   accent rule over whichever half has the keyboard. Its body, and the column's, are
   morphs of their own (PANE-BODY), drawn over this by themselves."
  (let* ((cols (or (pane-split-plan cur) (expanded-columns cur x y w h)))
         (full-w (- (+ x w 8) 24))
         (body-y (getf cols :body-y)) (body-h (getf cols :body-h))
         (mx (first (getf cols :main))) (mw (second (getf cols :main)))
         (sx (getf cols :split-x)) (side (getf cols :side))
         (sw (third (getf cols :side-rect)))
         (edit (pane-editing-side cur)))
    (setf (pane-body-top cur) body-y            ; what its clicks are tested against
          (pane-split-x cur) sx)
    (lol.widgets:draw-panel canvas x y w h
                            ;; when the pane is about to split, its title owns only
                            ;; the left half: the right one carries the preview's
                            ;; own title on the same line, and a long path in the
                            ;; first used to run straight through the second
                            :title (if (and (null (pane-tv cur))
                                            (not (member (view-kind (pane-view cur)) +non-list-kinds+))
                                            (or (pane-side cur)
                                                (let ((it (selected-item cur))) (and it (item-subject it)))))
                                       (clip-text (pane-title cur)
                                                  (max 8 (floor (- (max 260 (floor (* full-w 42) 100)) 40)
                                                                (lol.canvas:font-cw *bfont*))))
                                       (pane-title cur))
                            :tint (view-kind-color (pane-view cur))
                            :badge (string-downcase (symbol-name (view-kind (pane-view cur)))))
    (flet ((divider () (lol.canvas:draw-rect canvas (- sx 6) body-y 1 body-h *ui-rule*))
           (header (text)
             (lol.canvas:draw-string canvas *bfont* text sx (pane-header-text-y body-y)
                                     (if edit *ui-accent* *ui-dim*)))
           (accent ()                               ; over whichever half has focus
             (if edit
                 (lol.canvas:fill-rect canvas sx (- body-y 2) sw 2 *ui-accent*)
                 (lol.canvas:fill-rect canvas mx (- body-y 2) mw 2 *ui-accent*))))
      (ecase (getf cols :kind)
        (:text-side
         (divider)
         (header (format nil "~a~@[   ~a~]" (view-title (pane-view side))
                         (if edit "up/dn · ret opens · SuperL-o back" "SuperL-o")))
         (accent))
        (:stage-side
         (divider)
         (header (format nil "beside · ~a~@[   ~a~]" (view-title (pane-view side))
                         (if edit "editing — C-c C-c define · SuperL-o stage" "SuperL-o to edit")))
         (accent))
        (:list-side
         (divider)
         (header (format nil "beside · ~a~@[   ~a~]" (view-title (pane-view side))
                         (if edit "editing — C-c C-c accept · SuperL-o list" "SuperL-o to edit")))
         (accent))
        (:preview
         (divider)
         (if side
             (lol.canvas:draw-string canvas *bfont*
                                     (clip-text (view-title (pane-view side))
                                                (max 8 (floor (- sw (* 11 (char-w))) (char-w))))  ; leave the badge room
                                     sx (pane-header-text-y body-y) *ui-dim*)
             (lol.canvas:draw-string canvas *bfont* "(no preview)" sx (+ body-y 4) *ui-dim*)))
        (:single nil)))))

(defun draw-browser (canvas w h)
  "The whole screen, drawn afresh into CANVAS: every morph of the world, back to
   front, each drawing itself — the tab strip, the tray, the breadcrumbs, the panes,
   the status line, the windows (each window's contents drawn anew, not copied).
   There is no other way to draw the browser: what used to be one function laying
   out and painting everything is the world's tree now (doc/morphic.org §12). The name
   stays for its callers — checks that time a full frame, the backdrop of a card with
   no world to draw it."
  (declare (ignore w h))
  (when *world* (lol.morph:world-paint *world* canvas :fresh t)))

(defvar *backdrop-fn* nil
  "A function (CANVAS W H) that draws what a modal card floats over — the whole screen
   as the world shows it, windows included (poc/morphic.lisp). NIL: DRAW-BROWSER.")

(defun draw-backdrop (canvas w h)
  "The page behind a floating card (the Spotter, a menu, the halo, the clipboard)."
  (if *backdrop-fn* (funcall *backdrop-fn* canvas w h) (draw-browser canvas w h)))

(defun draw-status-line (canvas w h cur)
  "The bottom line of a screen W x H: the last message, or the keys that matter for
   the pane CUR — or, while one is open, the minibuffer; then -> the rect of its ✕
   clear box, (x0 x1 y0 y1), or NIL (the STATUS-LINE morph keeps it for clicks)."
  (lol.canvas:fill-rect canvas 0 (- h (status-h)) w (status-h) *ui-hi*)
  (cond (*minibuffer* (draw-minibuffer canvas w h))
        ((isearch-tv) (draw-isearch-prompt canvas w h (isearch-tv)))
        (*browser-status*
         (lol.canvas:draw-string canvas *bfont* *browser-status* 10 (status-text-y h) *ui-accent*)
         nil)
        ((carried-label)               ; the hand carries something to drop: say what
         (draw-hints canvas 10 (status-text-y h) w (carrying-hints))
         nil)
        (t (draw-hints canvas 10 (status-text-y h) w (status-hints cur))
           nil)))

(defun status-line-content (cur)
  "What the status line shows for the pane CUR, as data: the minibuffer's state, or
   the message, or the hints — the STATUS-LINE morph's signature (poc/morphic.lisp):
   when this changes, and only then, the line is drawn again."
  (cond (*minibuffer*
         (let ((mb *minibuffer*))
           (list :minibuffer (mb-prompt mb) (mb-text mb) (mb-point mb) (mb-hint mb)
                 (mb-secret mb) (mb-single-key mb))))
        ((isearch-tv)
         (cons :isearch (multiple-value-list (lol.textview:tv-isearch-prompt (isearch-tv)))))
        (*browser-status* (list :status *browser-status*))
        ((carried-label) (list :carrying (carried-label)))
        (t (list :hints (status-hints cur)))))

(defun carried-label ()
  "The name of what the hand carries to be dropped (doc/morphic.org §13), or NIL."
  (let ((h (the-hand)))
    (and h (getf (lol.morph:hand-payload h) :label))))

(defun carrying-hints ()
  "The status line while the hand carries something: what, and where it can go."
  (list (cons nil (format nil "carrying ~a:" (carried-label)))
        (cons "Super-d" "drop at the focus") (cons "Super-S-d" "as a new tab")
        (cons "click/release" "drop there") (cons "Esc" "put it back")))

(defun status-hints (cur)
  "The keys that matter for the pane CUR, as ((CHORD . MEANING) …) — its own first,
   then the environment's, then its subject's commands."
  (let ((cmds (ignore-errors (commands-for (pane-subject cur)))))
                    (append
                     (cond ((and (pane-editing-side cur) (pane-side cur))
                            ;; name the half you go BACK to, and the verb the pane's
                            ;; own header advertises — a stage is not a list, and a
                            ;; Workspace defines rather than accepts an edit.
                            (list (cons "C-c C-c"
                                        (if (workspace-pane-p (pane-side cur))
                                            "define" "accept"))
                                  (cons "SuperL-o"
                                        (if (eq (view-kind (pane-view cur)) :stage)
                                            "back to stage" "back to list"))))
                           ;; a conversation with the agent (initramfs/agent-ui.lisp)
                           ((and (fboundp 'pane-conversation) (funcall 'pane-conversation cur))
                            '(("C-c C-a" . "ask") ("C-c C-o" . "open #N call")
                              ("C-x C-e" . "Do it") ("C-c C-c" . "define") ("C-g" . "stop")))
                           ((workspace-pane-p cur)
                            '(("C-x C-e" . "Do it") ("C-c C-p" . "Print")
                              ("C-c C-i" . "Inspect") ("C-c C-d" . "Debug")
                              ("C-c C-c" . "define") ("C-c C-a" . "Ask")))
                           ((lisp-file-pane-p cur)
                            '(("C-x C-s" . "save") ("C-x C-e" . "Do it") ("C-c C-c" . "define")
                              ("C-_" . "undo") ("C-g" . "close")))
                           ((file-pane-p cur)
                            '(("C-x C-s" . "save") ("C-_" . "undo") ("C-g" . "close")))
                           ((ignore-errors (subject-hints (pane-subject cur) cur)))
                           ((eq (view-kind (pane-view cur)) :stage)
                            '(("spc" . "run/pause") ("n" . "step") ("s/a/m/t" . "spawn")
                              ("w" . "workspace") ("r" . "reset")
                              ("tab" . "select") ("ret" . "inspect")))
                           ((editable-pane-p cur) '(("C-c C-c" . "accept")))
                           ((pane-side cur)
                            '(("up/dn" . "move") ("ret" . "drill") ("S-ret" . "beside")
                              ("SuperL-o" . "edit")))
                           ;; a FUNCTION subject can be applied; every inspector pane
                           ;; can be evaluated inside (C-c C-e). Advertise the verb
                           ;; that is special to what you are looking at, first.
                           ((functionp (pane-subject cur))
                            '(("ret" . "drill") ("C-c C-a" . "apply it")
                              ("C-c C-e" . "eval in context")))
                           ((let ((it (selected-item cur))) (and it (item-edit it)))
                            '(("up/dn" . "move") ("ret" . "drill")
                              ("C-c C-c" . "set this field")
                              ("C-c C-e" . "eval in context")))
                           ((eq (view-kind (pane-view cur)) :inspector)
                            '(("up/dn" . "move") ("ret" . "drill")
                              ("C-c C-e" . "eval in context")))
                           (t '(("up/dn" . "move") ("ret" . "drill") ("S-ret" . "beside"))))
                     '(("SuperL-<" . "back") ("SuperL-?" . "keys") ("SuperL-q" . "quit"))
                     (when cmds
                       (list (cons nil (format nil "commands: ~{~a~^ · ~}"
                                               (mapcar #'command-label cmds))))))))

;;; ---- the Spotter card (§4¾) ----------------------------------------------
;;; The DRAWING lives here, in the backend-blind layer, and only the input loop lives
;;; in fb-browser.lisp. That is the same split as everything else in the shell, and it
;;; buys something concrete: the Spotter can be rendered — and therefore looked at —
;;; by poc/render-frame.lisp on the host, instead of only by booting the guest.

(defun spotter-band (results)
  "RESULTS regrouped into contiguous bands without disturbing the ranking: each group
   keeps its members in rank order, and the GROUPS are ordered by their own best
   member. So the top row is still the best match overall — which is the row Enter
   opens, and it would be a poor trade to lose that for tidiness — while everything
   below it is banded.

   The bands themselves come from the candidates (present.lisp gives each a :GROUP):
   recent · definitions · packages & files · surfaces, which is how the design
   mock-up bands them (media/browser-views-design.html §1). Nine matches spread over
   three kinds of thing read very differently from nine matches in a heap. Their
   ORDER is not hard-coded here on purpose — a fixed list would silently drop any
   band a future candidate source invents."
  (let ((order '()))
    (dolist (r results)                        ; first appearance = that band's best rank
      (pushnew (or (getf r :group) "other") order :test #'equal))
    (setf order (nreverse order))
    (loop for g in order
          append (remove-if-not (lambda (r) (equal g (or (getf r :group) "other"))) results))))

(defun spotter-cwd ()
  "The folder ./ means in a Spotter path query: the focused pane's, when it is a file
   manager (or a file) — else NIL, which the path grammar reads as $HOME."
  (ignore-errors (and *trails* (folder-of-subject (pane-subject (current-pane))))))

(defun spotter-results (query candidates &optional (limit 200))
  "The Spotter's results for QUERY: ranked, then BANDED. One function, because the
   caller indexes this list with its selection and the renderer draws it in order — if
   the banding happened at draw time, arrowing to row 3 would open whatever row 3 was
   before the bands moved it.

   A query that is a PATH (/…, ./…, ../…, ~/…, ~, file://…) is not a fuzzy search at
   all: its rows are the folder's entries completing it, the way a shell's Tab lists
   them (SPOTTER-PATH-RESULTS, initramfs/file-views.lisp; the grammar is PATH-QUERY-P
   in initramfs/files.lisp and doc/file-manager.org)."
  (if (path-query-p query)
      (or (ignore-errors (spotter-path-results query :cwd (spotter-cwd) :limit limit)) '())
      (spotter-band (append
                     ;; an address typed here opens it; any other text can be searched
                     ;; for on the web (WEB-SPOTTER-ROWS, initramfs/web.lisp) — first
                     ;; when it looks like a URL, else after everything local
                     (and (fboundp 'web-url-like-p) (funcall 'web-url-like-p query)
                          (ignore-errors (funcall 'web-spotter-rows query)))
                     (spotter-filter query candidates limit)
                     (and (fboundp 'web-url-like-p) (not (funcall 'web-url-like-p query))
                          (ignore-errors (funcall 'web-spotter-rows query)))))))

(defun spotter-complete (query)
  "Tab in the Spotter: a path query is completed like a shell's (COMPLETE-PATH-QUERY);
   any other query is left alone."
  (if (path-query-p query)
      (or (ignore-errors (complete-path-query query :cwd (spotter-cwd))) query)
      query))

(defun spotter-kind (r)
  "The Lisp-concept keyword for a Spotter candidate: its explicit :KIND when it has
   one (the recents record theirs, since \"recent\" says nothing about what the thing
   IS), else read off its DETAIL text, which is how the candidate list already
   describes itself and cheaper than threading a field through every producer."
  (let ((d (or (getf r :detail) "")))
    (cond ((getf r :kind))                       ; an explicit concept wins (recents)
          ((search "generic" d) :generic-function)
          ((search "package" d) :package)
          ((search "module"  d) :package)
          ((search "macro"   d) :macro)
          ((search "class"   d) :class)
          ((search "condition" d) :condition)
          ((search "variable" d) :variable)
          ((search "constant" d) :constant)
          ((search "surface" d) :restart)
          (t :function))))

(defun spotter-rows (results query)
  "Spotter candidates -> widget rows: the kind's colour, the matched characters lit,
   and the band as the row's group."
  (map 'vector
       (lambda (r)
         (lol.widgets:make-row
          (getf r :label)
          :detail (getf r :detail)
          :tint (concept-color (spotter-kind r))
          :stripe (concept-color (spotter-kind r))
          :group (getf r :group)
          :match query
          :marker :bright))                    ; every hit opens in a NEW trail
       results))

(defun draw-spotter-card (canvas w h query results sel)
  "The Spotter as a floating palette over the dimmed browser: the browser is drawn as
   the backdrop, scrimmed; the card carries a title, the live match count, the query
   line, the results with the matched characters lit, and a footer of what the keys do.
   RESULTS must already be banded (see SPOTTER-RESULTS) — this only draws. Returns
   (values RECT HITS): the card (x0 y0 x1 y1) and the rows' hit map, which the
   Spotter's overlay keeps so a click routes to the row actually painted there."
  (draw-backdrop canvas w h)                         ; the browser, as backdrop …
  (let* ((rows   (spotter-rows results query))
         (cw     (min (* 130 (char-w)) (- w 80)))   ; 780 at 6x15
         (cx     (floor (- w cw) 2))
         (cy     (+ +bar-h+ 14))
         ;; the card grows with the results, up to what the screen can hold
         (nslots (+ (length rows) (length (remove-duplicates
                                           (map 'list #'lol.widgets:row-group rows)
                                           :test #'equal))))
         (chrome  (+ (lol.widgets:card-head-h) +bar-h+ 8 (status-h) 2))  ; 76 at 6x15
         (maxrows (max 3 (min nslots (floor (- h cy chrome 20) +row-h+))))
         (list-h  (* maxrows +row-h+))
         (ch      (+ chrome list-h))
         (rect    (list cx cy (+ cx cw) (+ cy ch)))
         (hits    '()))
    (let* ((body (lol.widgets:draw-card canvas cx cy cw ch
                                        :title "Spotter"
                                        ;; not ~:p — "match" pluralises to "matches",
                                        ;; and ~:p would cheerfully write "9 matchs"
                                        :right (case (length results)
                                                 (0 "no matches")
                                                 (1 "1 match")
                                                 (t (format nil "~d matches" (length results))))
                                        :dim 105))
           (qy (lol.widgets:draw-query-line canvas (+ cx 8) (+ body 2) (- cw 16) query))
           (ly (+ qy 8)))
      (if (zerop (length rows))
          (lol.canvas:draw-string canvas *bfont*
                                  "nothing matches — Backspace to widen the query"
                                  (+ cx 16) (+ ly 4) *ui-dim*)
          (multiple-value-bind (top row-hits)
              (lol.widgets:draw-list canvas (+ cx 6) ly (- cw 12) list-h rows
                                     :sel sel :top 0 :follow-sel t)
            (declare (ignore top))
            (setf hits row-hits)))
      ;; the footer says what the keys do, because the palette is modal and nothing
      ;; else on screen can say it
      (draw-hints canvas (+ cx 12) (- (+ cy ch) (line-h) 1) (- cw 16)
                  (if (path-query-p query)
                      '(("tab" . "complete") ("up/dn" . "move") ("ret" . "open the folder in a new tab")
                        ("Esc" . "close"))
                      '(("up/dn" . "move") ("ret" . "open in a new tab") ("~/ or /" . "a path")
                        ("Esc" . "close")))))
    (values rect hits)))

(defun spotter-row-at* (rect hits x y)
  "The visible Spotter row index at X,Y, or NIL — read back from what the draw
   recorded (RECT, HITS: DRAW-SPOTTER-CARD's values), so a banded list's section
   headers are correctly inert."
  (and rect
       (<= (first rect) x (third rect))
       (lol.widgets:hit-row hits y)))

(defun spotter-outside-p* (rect x y)
  "True when X,Y is outside the Spotter card RECT — a click there dismisses the palette."
  (or (null rect)
      (destructuring-bind (x0 y0 x1 y1) rect
        (not (and (<= x0 x x1) (<= y0 y y1))))))

;;; ---- the minibuffer: a one-line prompt that is not modal ------------------
;;; Emacs's answer to "ask the user something": one line at the bottom of the frame,
;;; edited in place, while everything above stays live and drawn. The other prompt in
;;; this browser, *VALUE-PROMPT*, is MODAL: its caller waits for the answer (an overlay
;;; the backend's loop turns under — poc/overlays.lisp), which is right for a debugger
;;; restart but needs a backend with a loop, and a frame rendered by
;;; poc/render-frame.lisp can never wait for one.
;;;
;;; This one is STATE, not a loop: *MINIBUFFER* holds the prompt; BROWSER-KEY hands it
;;; every key while it is up; DRAW-STATUS-LINE draws it. So it works identically on the
;;; framebuffer, on X11 and in a rendered frame, and a caller that wants to watch the
;;; text change (the file manager's live filter) gets ON-CHANGE on every keystroke.
;;; The price is that the answer arrives through a CALLBACK (ON-DONE) instead of as a
;;; return value — a closure, which in Lisp costs nothing to write.

(defstruct mb
  prompt                 ; "Rename to: " — drawn in amber before the text
  (text "")              ; what has been typed (or the initial value)
  (point 0)              ; the caret, an index into TEXT
  on-done                ; (lambda (text)) — Enter
  on-change              ; (lambda (text)) or NIL — after every edit
  on-cancel              ; (lambda ()) or NIL — Esc / C-g
  completer              ; (lambda (text)) -> new text, or NIL — Tab
  single-key             ; T: the next key answers (y/n), no Enter needed
  hint                   ; a short dim note at the right end, or NIL
  secret)                ; T: drawn as bullets — a Wi-Fi passphrase (initramfs/net-views.lisp)

(defun minibuffer-read (prompt on-done &key (initial "") on-change on-cancel completer
                                             single-key hint secret)
  "Open the minibuffer with PROMPT, pre-filled with INITIAL and the caret at its end.
   ON-DONE is called with the final text on Enter (or with the key's character, for
   a SINGLE-KEY prompt); everything else is optional. Returns at once — the answer
   comes later, through ON-DONE."
  (setf *minibuffer* (make-mb :prompt prompt :text initial :point (length initial)
                              :on-done on-done :on-change on-change :on-cancel on-cancel
                              :completer completer :single-key single-key :hint hint
                              :secret secret))
  nil)

(defun minibuffer-finish (fn &rest args)
  "Close the minibuffer, THEN call FN — so a callback that opens the next prompt (a
   rename that then asks to confirm) is not immediately closed by us. Errors in FN
   land in the status line: a callback runs in PID 1 like everything else."
  (setf *minibuffer* nil)
  (when fn
    (handler-case (apply fn args)
      (serious-condition (c) (setf *browser-status* (princ-to-string c))))))

(defun minibuffer-edited (mb)
  (when (mb-on-change mb)
    (handler-case (funcall (mb-on-change mb) (mb-text mb))
      (serious-condition (c) (setf *browser-status* (princ-to-string c))))))

(defun minibuffer-paste-text (text)
  "TEXT as one minibuffer line: a prompt holds ONE line, so newlines and tabs become
   spaces and the ends are trimmed — a URL copied with its line break pastes clean."
  (and text
       (string-trim " " (substitute-if #\Space (lambda (c) (member c '(#\Newline #\Return #\Tab))) text))))

(defun minibuffer-key (key state)
  "A key while the minibuffer is up. Enter answers, Esc / C-g cancels, Tab completes;
   the rest is a small line editor: Left/Right, C-a/C-e (Home/End), C-< / C-> and
   M-< / M-> (start / end, as in every other text here), C-b/C-f, Backspace / C-d
   (Delete), C-k kills to the end, C-u clears the line (so does the ✕ at its right),
   M-Backspace deletes a word, C-y / C-v / Shift-Insert paste the clipboard (newest
   entry, newlines made spaces). Returns NIL (BROWSER-KEY's 'carry on')."
  (let* ((mb *minibuffer*)
         (text (mb-text mb)) (pt (min (mb-point mb) (length text)))
         (ctrl (lol.canvas:ctrl-p state)) (meta (lol.canvas:meta-p state))
         (shift (lol.canvas:shift-p state)))
    (flet ((set-text (new point)
             (setf (mb-text mb) new (mb-point mb) (max 0 (min point (length new))))
             (minibuffer-edited mb)))
      (cond
        ((or (eq key :escape) (and ctrl (member key '(#\g #\G))))
         (minibuffer-finish (mb-on-cancel mb)))
        ((mb-single-key mb)
         (minibuffer-finish (mb-on-done mb) (if (characterp key) key :other)))
        ((member key '(:return #\Return #\Newline))
         (minibuffer-finish (mb-on-done mb) text))
        ((and (eq key :tab) (mb-completer mb))
         (let ((new (ignore-errors (funcall (mb-completer mb) text))))
           (when (and new (string/= new text)) (set-text new (length new)))))
        ;; paste: C-y (Emacs), C-v (everyone else), Shift-Insert (X)
        ((or (and ctrl (member key '(#\y #\Y #\v #\V))) (and shift (eq key :insert)))
         (let ((paste (minibuffer-paste-text (lol.clipboard:clip-yank))))
           (if (and paste (plusp (length paste)))
               (set-text (concatenate 'string (subseq text 0 pt) paste (subseq text pt)) (+ pt (length paste)))
               (setf *browser-status* "paste: the clipboard is empty"))))
        ((and meta (eq key :backspace))
         (let ((start (or (position-if (lambda (c) (member c '(#\/ #\Space #\. #\- #\_)))
                                       text :end (max 0 (1- pt)) :from-end t)
                          -1)))
           (set-text (concatenate 'string (subseq text 0 (1+ start)) (subseq text pt)) (1+ start))))
        ((eq key :backspace)
         (when (plusp pt) (set-text (concatenate 'string (subseq text 0 (1- pt)) (subseq text pt)) (1- pt))))
        ((or (eq key :delete) (and ctrl (member key '(#\d #\D))))
         (when (< pt (length text))
           (set-text (concatenate 'string (subseq text 0 pt) (subseq text (1+ pt))) pt)))
        ((or (eq key :left) (and ctrl (member key '(#\b #\B)))) (setf (mb-point mb) (max 0 (1- pt))))
        ((or (eq key :right) (and ctrl (member key '(#\f #\F)))) (setf (mb-point mb) (min (length text) (1+ pt))))
        ((or (eq key :home) (and ctrl (member key '(#\a #\A)))
             (and (or ctrl meta) (member key '(#\< #\,))))
         (setf (mb-point mb) 0))
        ((or (eq key :end) (and ctrl (member key '(#\e #\E)))
             (and (or ctrl meta) (member key '(#\> #\.))))
         (setf (mb-point mb) (length text)))
        ((and ctrl (member key '(#\k #\K))) (set-text (subseq text 0 pt) pt))
        ((and ctrl (member key '(#\u #\U))) (set-text "" 0))
        ((and (characterp key) (graphic-char-p key) (not ctrl) (not meta))
         (set-text (concatenate 'string (subseq text 0 pt) (string key) (subseq text pt)) (1+ pt)))
        (t nil))))
  nil)

(defun draw-minibuffer (canvas w h)
  "The minibuffer, over the status line: an amber rule, the prompt in amber, the
   text and a caret, and — when there is text — a ✕ that clears it (a click, or C-u).
   A long text scrolls so the caret is always on screen. -> the ✕'s rect (X0 X1 Y0
   Y1), or NIL."
  (let* ((mb *minibuffer*)
         (cw (lol.canvas:font-cw *bfont*))
         (y (- h (status-h)))
         (px (+ 10 (lol.canvas:string-px *bfont* (mb-prompt mb))))
         (hint-w (if (mb-hint mb) (+ 16 (lol.canvas:string-px *bfont* (mb-hint mb))) 0))
         (clear (and (plusp (length (mb-text mb))) (not (mb-single-key mb))))
         (clear-label " ✕ clear ")
         (clear-w (if clear (+ 12 (lol.canvas:string-px *bfont* clear-label)) 0))
         (room (max 4 (floor (- w px 14 hint-w clear-w) cw)))
         (text (mb-text mb)) (pt (min (mb-point mb) (length text)))
         (first (max 0 (- pt (1- room))))
         (shown (subseq text first (min (length text) (+ first room))))
         (shown (if (mb-secret mb) (make-string (length shown) :initial-element #\*) shown)))
    (lol.canvas:fill-rect canvas 0 y w (status-h) *ui-hi*)
    (lol.canvas:fill-rect canvas 0 y w 2 *ui-amber*)
    (lol.canvas:draw-string canvas *bfont* (mb-prompt mb) 10 (status-text-y h) *ui-amber*)
    (lol.canvas:draw-string canvas *bfont* shown px (status-text-y h) *ui-text*)
    (lol.canvas:fill-rect canvas (+ px (* (- pt first) cw)) (1- (status-text-y h)) 2 (1+ (line-h)) *ui-accent*)
    (let ((rect (when clear
                  (let* ((bx (- w 10 hint-w clear-w -6))
                         (bw (- clear-w 6)))
                    (lol.canvas:fill-rect canvas bx (+ y 4) bw (- (status-h) 6) *ui-panel*)
                    (lol.canvas:draw-string canvas *bfont* clear-label (+ bx 3) (status-text-y h) *ui-dim*)
                    (list bx (+ bx bw) y h)))))
      (when (mb-hint mb)
        (lol.canvas:draw-string canvas *bfont* (mb-hint mb)
                                (- w 10 (lol.canvas:string-px *bfont* (mb-hint mb))) (status-text-y h) *ui-dim*))
      rect)))

;;; ---- incremental search: C-s / C-r ------------------------------------------
;;; The search itself is the text view's (poc/textview.lisp, "Incremental search");
;;; here are only its three joints with the shell. It is NOT the minibuffer above,
;;; though it is drawn where the minibuffer is: the minibuffer takes every key until
;;; Enter, while an Emacs search ends on ANY key that is not a search key and then
;;; lets that key act — so BROWSER-KEY hands keys to the search first, and a key the
;;; search answers :EXIT for goes on through BROWSER-KEY as if no search had run.

(defvar *isearch-tv* nil
  "The text view a C-s / C-r search is running in, or NIL. Kept so a search left
   behind — focus moved elsewhere by a click or a key — can be ended.")

(defun isearch-target (p)
  "The text view P's keys go to: its focused side pane's, else its own (or NIL)."
  (when p
    (if (and (pane-editing-side p) (pane-side p))
        (pane-tv (pane-side p))
        (pane-tv p))))

(defun isearch-tv ()
  "The text view searching right now, where keyboard focus is — or NIL."
  (let ((tv (isearch-target (current-pane))))
    (and tv (eq tv *isearch-tv*) (lol.textview:tv-isearch tv) tv)))

(defun start-isearch (tv forward)
  "C-s (FORWARD) / C-r in TV: start searching. Readable in any text, editable or not."
  (end-isearch)
  (setf *isearch-tv* tv *browser-status* nil *pending-prefix* nil)
  (lol.textview:tv-isearch-start tv forward)
  nil)

(defun isearch-ended (mark-set)
  (setf *isearch-tv* nil)
  (when mark-set (setf *browser-status* "Mark saved where search started")))

(defun end-isearch ()
  "End a running search where it stands (a click, focus moving away)."
  (when (and *isearch-tv* (lol.textview:tv-isearch *isearch-tv*))
    (isearch-ended (lol.textview:tv-isearch-exit *isearch-tv*)))
  (setf *isearch-tv* nil))

(defun isearch-key (key state)
  "A key while a search runs. -> T when the search took it; NIL when BROWSER-KEY
   must go on and handle it (it ended the search). C-y / Shift-Insert yank the
   clipboard's newest entry into the string, as in Emacs."
  (let ((tv (isearch-tv)))
    (cond
      ((null tv) (end-isearch) nil)
      ((or (and (lol.canvas:ctrl-p state) (member key '(#\y #\Y)))
           (and (lol.canvas:shift-p state) (eq key :insert)))
       (lol.textview:tv-isearch-yank tv (lol.clipboard:clip-yank))
       t)
      (t (multiple-value-bind (what mark-set) (lol.textview:tv-isearch-key tv key state)
           (ecase what
             (:search t)
             (:done (isearch-ended mark-set) t)
             (:cancel (isearch-ended nil) (setf *browser-status* "Quit") t)
             (:exit (isearch-ended mark-set) nil)))))))

(defun exchange-point-and-mark (tv)
  "C-x C-x."
  (unless (lol.textview:tv-exchange-point-and-mark tv)
    (setf *browser-status* "No mark set in this buffer")))

(defparameter *isearch-fail* !#FF6B6B
  "The part of a search string that matches nothing (Emacs's isearch-fail face): the
   red the debugger names a condition in.")

(defun draw-isearch-prompt (canvas w h tv)
  "The search's echo line over the status line, Emacs's way: \"I-search: \" (or
   \"Failing wrapped I-search backward: \" …) in amber, the string, the part of it that
   matches nothing in red, and a complaint, if any, dim at the end."
  (multiple-value-bind (prompt string failing message) (lol.textview:tv-isearch-prompt tv)
    (let* ((y (- h (status-h)))
           (ty (status-text-y h))
           (px (+ 10 (lol.canvas:string-px *bfont* prompt)))
           (good (subseq string 0 (or failing (length string))))
           (bad (subseq string (length good)))
           (bx (+ px (lol.canvas:string-px *bfont* good)))
           (ex (+ bx (lol.canvas:string-px *bfont* bad))))
      (lol.canvas:fill-rect canvas 0 y w (status-h) *ui-hi*)
      (lol.canvas:fill-rect canvas 0 y w 2 *ui-amber*)
      (lol.canvas:draw-string canvas *bfont* prompt 10 ty *ui-amber*)
      (lol.canvas:draw-string canvas *bfont* good px ty *ui-text*)
      (lol.canvas:draw-string canvas *bfont* bad bx ty *isearch-fail*)
      (lol.canvas:fill-rect canvas ex (1- ty) 2 (1+ (line-h)) *ui-accent*)
      (when message
        (lol.canvas:draw-string canvas *bfont* (format nil " [~a]" message) (+ ex 4) ty *ui-dim*))
      nil)))

;;; ---- a subject's own keys, and its own hints --------------------------------
;;; Two generic functions so a subject type can bring keys to its LIST pane without
;;; this file naming it — the file manager (poc/file-manager.lisp) is the first user:
;;; in a folder, plain letters are verbs (r rename, d delete, / filter …), exactly as
;;; in dired, because a list pane has no text to type them into.

(defgeneric subject-key (subject pane key state)
  (:documentation "Handle KEY for a list PANE showing SUBJECT; true when it was
    consumed. Called before the generic list keys, only without Ctrl/Meta/Super.")
  (:method (subject pane key state)
    (declare (ignore subject pane key state))
    nil))

(defgeneric subject-signature (subject pane)
  (:documentation "What a pane showing SUBJECT draws from the subject itself, beyond its
   view — a web tab's address and load state, a terminal's fate — as a list the desk
   compares with its last (poc/morphic.lisp, the pane's MORPH-SIGNATURE): when it
   changes, that pane is drawn again. NIL by default.")
  (:method (subject pane) (declare (ignore subject pane)) nil))

(defgeneric subject-hints (subject pane)
  (:documentation "The status-line hints ((CHORD . MEANING) …) for a pane showing
    SUBJECT, or NIL for the shell's defaults.")
  (:method (subject pane) (declare (ignore subject pane)) nil))

;;; ---- input ---------------------------------------------------------------

(defun editor-replace-token (tv n replacement)
  "Delete the N chars just before point (the token being completed) and insert
   REPLACEMENT — through the editor's own keys, so layout/syntax stay consistent."
  (dotimes (i n) (lol.textview:tv-key tv :backspace 0))
  (loop for ch across replacement do (lol.textview:tv-key tv ch 0)))

(defun editor-complete (pane)
  "Tab in an editable pane: complete the symbol before point via *completer* — the
   SAME hook the REPL uses (repl.lisp's complete-symbol). Common-editor behaviour:
   no token before point (start of line / after whitespace) -> Tab indents; exactly
   one match -> fill it in; several -> extend to their common prefix, or, when that
   can't grow, list them in the status line. token-start / longest-common-prefix are
   reused from line-editor.lisp so the code editor and the REPL complete alike."
  (let* ((tv    (pane-tv pane))
         (line  (aref (lol.textview:tv-lines tv) (lol.textview:tv-point-line tv)))
         (col   (lol.textview:tv-point-col tv))
         (start (token-start line col))
         (token (subseq line start col))
         (cands (and (plusp (length token)) *completer* (funcall *completer* token))))
    (cond
      ((zerop (length token))                            ; nothing to complete: indent
       (dotimes (i 2) (lol.textview:tv-key tv #\Space 0)))
      ((null cands)
       (setf *browser-status* (format nil "no completion for ~a" token)))
      ((null (rest cands))                               ; exactly one: fill it in
       (editor-replace-token tv (- col start) (first cands))
       (setf *browser-status* nil))
      (t                                                 ; several: extend, or list them
       (let ((cp (longest-common-prefix cands)))
         (if (> (length cp) (length token))
             (progn (editor-replace-token tv (- col start) cp)
                    (setf *browser-status* (format nil "~d matches" (length cands))))
             (setf *browser-status*
                   (let ((shown (subseq cands 0 (min 15 (length cands)))))
                     (format nil "~{~a~^  ~}~@[  … (~d)~]"
                             shown (and (> (length cands) 15) (length cands)))))))))))

(defun set-field (pane)
  "Write a new value into the place the selected inspector row shows.

   The row carries the SETTER (item-edit, from model.lisp's INSPECT-PLACES); the
   shell supplies the gesture, the prompt and the error handling. The typed text is
   READ and EVALUATED, so you can put any expression in — \"a string\", 42, (list 1 2),
   or something computed — which is the whole point of an inspector in a live image
   rather than a form with typed boxes.

   The pane is rebuilt afterwards, not patched: the view is a rendering of the object,
   and re-presenting it is how the new value (and anything else that changed with it)
   shows up honestly."
  (let ((it (selected-item pane)))
    (cond
      ((or (null it) (null (item-edit it)))
       (setf *browser-status* "that field is not settable"))
      ((null *value-prompt*)
       (setf *browser-status* "no input available on this backend"))
      (t
       (let ((text (funcall *value-prompt* (item-label it))))
         ;; Cancelling must CLEAR the status line, not leave it. Without this the
         ;; previous message stays up and reads as the outcome of the cancel — in
         ;; testing, an Esc appeared to report "that field is not settable".
         (unless text (setf *browser-status* nil))
         (when text
           (handler-case (set-field-to pane it (eval (read-from-string text)))
             (serious-condition (c)
               (setf *browser-status* (format nil "set failed: ~a" c)))))))))
  nil)

(defun set-field-to (pane it new)
  "Write NEW into the place the inspector row IT of PANE shows, through the row's own
   setter (ITEM-EDIT), and show the result. The one path every way of setting a field
   takes — the typed value (SET-FIELD) and a value dropped on the row (drag and drop,
   poc/morphic.lisp). Signals what the setter signals; T when it was set."
  (funcall (item-edit it) new)
  ;; re-present: the row, its neighbours and the title all come from the object, so
  ;; read them back rather than editing the old view
  (let ((sel (pane-sel pane)))
    (setf (pane-view pane) (present (pane-subject pane)))
    (set-pane-sel pane sel))
  (setf (pane-preview-subject pane) nil)    ; the cached preview is stale
  (setf *browser-status*
        (let ((*print-length* 20) (*print-level* 3))
          (format nil "~a = ~s" (item-label it) new)))
  t)

(defun eval-in-context-pane (pane)
  "Evaluate a typed expression INSIDE the object this pane is showing — SELF bound to it
   and every slot name bound as a place (model.lisp's EVAL-IN-CONTEXT).

   This is Smalltalk's inspector code pane, and it is the gesture that makes the
   inspector open-ended: every other command here is a thing we decided in advance you
   might want, and this one is the escape hatch for everything we did not think of.

   The pane is RE-PRESENTED afterwards for the same reason SET-FIELD re-presents: the
   typed form may well have mutated the object — (incf hp 100) is the point, not an
   edge case — and the view is a rendering of the object, so it has to be read back
   rather than patched. Errors land in the status line; a form typed into a live image
   must not be able to take down PID 1."
  (let ((subject (subject-value (pane-subject pane))))
    (cond
      ((null *value-prompt*)
       (setf *browser-status* "no input available on this backend"))
      (t
       (let ((text (funcall *value-prompt* "expression (SELF = this object)")))
         (unless text (setf *browser-status* nil))
         (when (and text (plusp (length (string-trim '(#\Space #\Tab) text))))
           (handler-case
               (let ((value (eval-in-context subject text)))
                 (let ((sel (pane-sel pane)))
                   (setf (pane-view pane) (present (pane-subject pane)))
                   (set-pane-sel pane sel))
                 (setf (pane-preview-subject pane) nil)
                 (setf *browser-status*
                       (let ((*print-length* 20) (*print-level* 3))
                         (format nil "~a => ~s" (string-trim '(#\Space #\Tab) text) value))))
             (serious-condition (c)
               (setf *browser-status* (format nil "eval failed: ~a" c))))))))))

(defun apply-function-pane (pane)
  "Apply the FUNCTION this pane is showing to typed arguments, and open the result.

   For a function, `evaluate in the object's context' collapses to this: the object IS
   the verb, so the useful thing to type is its arguments. The result is pushed onto the
   trail rather than summarised in the status line — you asked the function for a value
   because you want to look at the value."
  (let ((subject (subject-value (pane-subject pane))))
    (cond
      ((not (functionp subject))
       (setf *browser-status* "not a function"))
      ((null *value-prompt*)
       (setf *browser-status* "no input available on this backend"))
      (t
       (let ((text (funcall *value-prompt* "arguments (no parens)")))
         (unless text (setf *browser-status* nil))
         (when text
           ;; The CALL is guarded; opening the result is NOT inside the same guard, so
           ;; that a failure to open cannot be reported as "apply failed" — a wrong
           ;; attribution sends you looking at the wrong thing, which is exactly the
           ;; bug an inspector exists to prevent. :APPLIED distinguishes "the call
           ;; returned NIL" from "the call never happened".
           (multiple-value-bind (value applied)
               (handler-case (values (apply-function subject text) t)
                 (serious-condition (c)
                   (setf *browser-status* (format nil "apply failed: ~a" c))
                   (values nil nil)))
             (when applied
               (setf *browser-status*
                     (let ((*print-length* 20) (*print-level* 3))
                       (format nil "=> ~s" value)))
               (trail-push value)))))))))

(defun accept-pane (pane)
  "Compile the buffer into the live image + update the registry (accept-source).

   Two cases. A SOURCE pane is editing a definition that already exists, and hands
   accept-source the subject's name/kind/label as fallbacks so an edit lands on the
   right one — the specializer label especially, or an edited method would overwrite
   a sibling's source. A WORKSPACE pane is not editing anything: whatever definition
   the text names is the one being made, so the form is the only authority and the
   fallbacks are NIL. That is what makes C-c C-c in the Workspace mean 'define this',
   which is how a method written at the stage's debugger gets into the image AND into
   the change set (§3b, beat 4).

   Sets *browser-status* to the outcome."
  (let ((text (if (or (workspace-pane-p pane) (file-pane-p pane))
                  (form-text-before-point (pane-tv pane))   ; the form before the caret
                  (lol.textview:tv-text (pane-tv pane))))
        (subj (pane-subject pane)))
    (unless text
      (return-from accept-pane (setf *browser-status* "Accept: no form before point")))
    (handler-case
        (multiple-value-bind (name kind label)
            (if (or (workspace-pane-p pane) (file-pane-p pane)) (values nil nil nil) (accept-target subj))
          (let ((n (accept-source text name kind label)))
            ;; the Accept sandbox (sandbox.lisp) leaves the trial's pid behind:
            ;; say it, so a definition tried elsewhere first is visibly that; and
            ;; the journal's note (journal.lisp) when keeping the change failed
            (setf *browser-status*
                  (let ((trial (and (boundp '*last-sandbox*) (symbol-value '*last-sandbox*))))
                    (format nil "Accepted: ~(~a~)~@[   (tried first in pid ~a)~]~@[  - ~a~]" n
                            (and (member (getf trial :outcome) '(:accepted :error))
                                 (getf trial :pid))
                            *accept-note*)))))
      (serious-condition (c)
        (setf *browser-status* (format nil "Accept failed: ~a" c))))))

(defmacro with-undo ((tv &key insert) &body body)
  "Run BODY, a command on text view TV, and record what it changed for undo (see
   \"Undo, the Emacs way\" in textview.lisp). Every path that edits a buffer — a
   key, a menu entry, a middle-click paste — goes through this."
  (let ((v (gensym "TV")) (before (gensym "BEFORE")))
    `(let* ((,v ,tv) (,before (and ,v (lol.textview:tv-undo-checkpoint ,v))))
       (multiple-value-prog1 (progn ,@body)
         (when ,v (lol.textview:tv-undo-note ,v ,before :insert ,insert))))))

(defun undo-key-p (key state)
  "C-_ or C-/ (one key in Emacs: X sends C-/ and C-_ alike, the console sends C-_ as
   byte 31 and C-/ as DEL, see FB-READ-KEY), or C-x u — the third key of an Emacs
   undo, read here with the C-x prefix already pending."
  (or (and (lol.canvas:ctrl-p state) (member key '(#\_ #\/)))
      (and (eq *pending-prefix* :c-x) (not (lol.canvas:ctrl-p state)) (member key '(#\u #\U)))))

(defun editor-undo (tv)
  "C-_: take back the last change in TV; repeat to go further back. Any other
   command ends the run, after which C-_ undoes the undos — Emacs's redo."
  (setf *pending-prefix* nil)
  (setf *browser-status*
        (if (lol.textview:tv-undo tv)
            (format nil "Undo  (~d more)" (lol.textview:tv-undo-count tv))
            "No further undo information")))

(defun comment-toggled-lines (lines)
  "LINES (strings) commented with ;; at their common indentation — or, when every
   non-blank one is already a comment, uncommented (one ;-run and the space after it
   taken off). Blank lines are left alone. -> (values NEW-LINES COMMENTED-P)."
  (flet ((blank (l) (zerop (length (string-trim '(#\Space #\Tab) l))))
         (indent (l) (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) l) (length l))))
    (let ((body (remove-if #'blank lines)))
      (if (and body (every (lambda (l) (char= (char l (indent l)) #\;)) body))
          (values (mapcar (lambda (l)
                            (if (blank l) l
                                (let* ((i (indent l))
                                       (j (or (position #\; l :start i :test-not #'char=) (length l)))
                                       (j (if (and (< j (length l)) (char= (char l j) #\Space)) (1+ j) j)))
                                  (concatenate 'string (subseq l 0 i) (subseq l j)))))
                          lines)
                  nil)
          (let ((col (if body (reduce #'min body :key #'indent) 0)))
            (values (mapcar (lambda (l)
                              (if (blank l) l
                                  (concatenate 'string (subseq l 0 col) ";; " (subseq l col))))
                            lines)
                    t))))))

(defun editor-toggle-comment (tv)
  "C-; — comment the region's lines with ;; (or uncomment them, when all of them are
   comments already); with no region, the caret's line, and then the caret moves to
   the next line, so C-; C-; C-; does three lines (Emacs's comment-line). A region
   that ends at the start of a line does not include that line."
  (let* ((lines (lol.textview:tv-lines tv))
         (pl (lol.textview:tv-point-line tv)))
    (multiple-value-bind (al ac rl rc) (lol.textview:tv-region tv)
      (multiple-value-bind (l0 l1)
          (if al
              (multiple-value-bind (sl el ec)
                  (if (or (< al rl) (and (= al rl) (<= ac rc))) (values al rl rc) (values rl al ac))
                (values sl (if (and (zerop ec) (> el sl)) (1- el) el)))
              (values pl pl))
        (multiple-value-bind (new commented)
            (comment-toggled-lines (coerce (subseq lines l0 (1+ l1)) 'list))
          (let ((v (concatenate 'simple-vector (subseq lines 0 l0) new (subseq lines (1+ l1)))))
            (if al
                (lol.textview:tv-replace tv v l1 (length (aref v l1)))
                (let ((next (min (1+ pl) (1- (length v)))))
                  (lol.textview:tv-replace tv v next
                                           (min (lol.textview:tv-point-col tv) (length (aref v next))))))
            (setf *browser-status*
                  (format nil "~:[uncommented~;commented~] ~d line~:p" commented (1+ (- l1 l0))))))))))

(defun edit-pane-key (p key state)
  "An editable pane's key, recorded for undo: C-_ / C-/ / C-x u undo, a key that
   only starts a chord (C-c, C-x, C-x r) leaves a run of undos going, and anything
   else is EDIT-PANE-COMMAND under WITH-UNDO."
  (let ((tv (pane-tv p)))
    (cond
      ((null tv) (edit-pane-command p key state))
      ;; a conversation being answered is read-only until the turn ends (agent-ui.lisp);
      ;; moving about is still fine
      ((and (fboundp 'agent-pane-busy-p) (funcall 'agent-pane-busy-p p)
            (not (member key '(:up :down :left :right :page-up :page-down :home :end)))
            ;; ... but the verbs that only READ the buffer still work: C-c / C-x, then
            ;; C-e (Do it), C-c (Accept), C-o (open a call); and copying
            (not (and (lol.canvas:ctrl-p state)
                      (or (member key '(#\c #\x))
                          (and *pending-prefix* (member key '(#\e #\o)))
                          (and (eq *pending-prefix* :c-c) (eql key #\c))
                          ;; searching only reads, too
                          (and (null *pending-prefix*) (member key '(#\s #\r)))))))
       (setf *browser-status* "the agent is writing here — C-g stops it"
             *pending-prefix* nil)
       nil)
      ((undo-key-p key state) (editor-undo tv) nil)
      (t (let ((before (lol.textview:tv-undo-checkpoint tv)))
           (prog1 (edit-pane-command p key state)
             ;; a prefix key is not a command yet: C-x u after C-_ must continue
             ;; the run, as it does in Emacs
             (unless (and *pending-prefix* (eq (first before) (lol.textview:tv-lines tv)))
               (lol.textview:tv-undo-note
                tv before
                :insert (and (characterp key) (graphic-char-p key)
                             (not (lol.canvas:ctrl-p state)) (not (lol.canvas:meta-p state)))))))))))

(defun edit-pane-command (p key state)
  "Handle a key for an EDITABLE pane P — a source (Accept) or the Workspace (eval verbs).
   Chords ride *pending-prefix*: C-c {C-c Accept | C-p Print | C-i Inspect | C-d Debug},
   C-x C-e Do it; a bare Tab completes; anything else edits. Shared by the current pane
   and the ⇧↵ side pane, and guarded so a list side (no textview) can't crash. Returns NIL."
  (let ((c (lol.canvas:ctrl-p state))
        (tv (pane-tv p))
        (previous (shiftf *last-edit* nil)))     ; consumed: see *LAST-EDIT*
    (cond
      ;; an Org file's own keys come first: TAB folds on a headline, C-c C-n moves
      ;; by heading, M-RET … (poc/org-mode.lisp, which loads after this file)
      ((and tv (fboundp 'org-key) (funcall 'org-key p key state)) nil)
      ;; C-x r <key>: the rectangle commands — a THREE-key chord, hence its own prefix
      ((eq *pending-prefix* :c-x-r)
       (setf *pending-prefix* nil)
       (when tv (editor-rectangle tv (if (characterp key) (char-downcase key) key)))
       nil)
      ;; C-x <key> that are not the Workspace's C-x C-e: select all, rectangle mark,
      ;; and the r prefix
      ((and (eq *pending-prefix* :c-x) tv (not c) (member key '(#\h #\H #\Space #\r #\R)))
       (setf *pending-prefix* nil)
       (case (char-downcase key)
         (#\h (editor-select-all tv))
         (#\r (setf *pending-prefix* :c-x-r))
         (#\Space (lol.textview:tv-move-point tv (lol.textview:tv-point-line tv)
                                             (lol.textview:tv-point-col tv))
                  (lol.textview::set-mark tv)
                  (setf (lol.textview:tv-rect-mode tv) t
                        *browser-status* "rectangle mark — move, then C-x r k / M-w / d / o / c / t")))
       nil)
      (*pending-prefix*
       (let ((prefix *pending-prefix*))
         (setf *pending-prefix* nil)
         (cond
           ((and (eq prefix :c-c) c (eql key #\c)
                 (or (editable-pane-p p) (workspace-pane-p p) (lisp-file-pane-p p)))
            (accept-pane p))
           ;; C-x C-s — save, the Emacs way. A file pane writes its file (safely: a
           ;; temp file and a rename, poc/file-manager.lisp); anywhere else there is
           ;; no file to write, and saying so beats doing nothing.
           ;; C-x C-x — back to the mark (where a search began), the region shown
           ((and (eq prefix :c-x) c (eql key #\x) tv) (exchange-point-and-mark tv))
           ((and (eq prefix :c-x) c (eql key #\s))
            (if (file-pane-p p)
                (funcall 'save-file-pane p)
                (setf *browser-status* "C-x C-s: this pane is not a file")))
           ((and (eq prefix :c-c) c (eql key #\p) (workspace-pane-p p)) (workspace-print-it p))
           ;; C-c C-i Inspect it: Ctrl-I *is* Tab (byte 9) on any tty, so the console
           ;; delivers this chord as C-c then :tab — accept both.
           ((and (eq prefix :c-c) (or (and c (eql key #\i)) (eq key :tab))
                 (workspace-pane-p p))
            (workspace-inspect-it p))
           ((and (eq prefix :c-c) c (eql key #\d) (workspace-pane-p p)) (workspace-debug-it p))
           ;; C-c C-a — Ask it: the agent's verb (doc/agent.org §8.1, initramfs/agent-ui.lisp)
           ((and (eq prefix :c-c) (or (and c (eql key #\a)) (eql key #\?)) (fboundp 'agent-ask-at-pane))
            (funcall 'agent-ask-at-pane p))
           ;; C-c C-o — in a conversation, open what the ;; #N tool call was about (§8.6)
           ((and (eq prefix :c-c) c (eql key #\o) (fboundp 'agent-open-at-point))
            (funcall 'agent-open-at-point p))
           ((and (eq prefix :c-x) c (eql key #\e) (or (workspace-pane-p p) (lisp-file-pane-p p)))
            (workspace-do-it p))
           ;; C-c C-e — Do it on the TOP-LEVEL form around the caret (Emacs's eval-defun):
           ;; the caret may be anywhere inside it — a line of code in a conversation
           ((and (eq prefix :c-c) c (eql key #\e) (or (workspace-pane-p p) (lisp-file-pane-p p)))
            (workspace-do-it p t)))
         nil))
      ;; C-s / C-r — incremental search (after the chords: C-x C-s is still save)
      ((and c tv (member key '(#\s #\S #\r #\R)))
       (start-isearch tv (member key '(#\s #\S))) nil)
      ;; C-; comments the line or the region (or uncomments it)
      ((and c (eql key #\;) tv) (editor-toggle-comment tv) nil)
      ((and c (eql key #\c)) (setf *pending-prefix* :c-c) nil)
      ((and c (eql key #\x)) (setf *pending-prefix* :c-x) nil)
      ((and tv (clipboard-key tv key state previous)) nil)
      ((and (eq key :tab) (pane-tv p)) (editor-complete p) nil)
      (t (when (pane-tv p) (lol.textview:tv-key (pane-tv p) key state)) nil))))

(defun open-workspace (&optional fresh)
  "SuperL-e: go to a Workspace. The open Workspace tab nearest after this one (so
   pressing it again cycles through them, if you keep several), or — when none is
   open, e.g. you closed it, or FRESH (SuperL-Shift-e) — a new one. The tab strip's +,
   the right-click menu's New Workspace and the Spotter's \"workspace\" open one too."
  (let* ((n (length *trails*))
         (hit (and (not fresh)
                   (loop for k from 1 to n
                         for i = (mod (+ *active* k) n)
                         when (typep (pane-subject (first (nth i *trails*))) 'subj-workspace)
                           return i))))
    (if hit
        (activate-trail hit)
        (new-trail (subj-workspace)))
    (setf *browser-status* nil)))

(defun browser-key (key state)
  "Route a decoded key by modifier + pane type (see doc/code-browser.org §4¾).
   Returns :quit to exit. STATE is the X modifier bitmask."
  ;; an open minibuffer takes EVERY key — it is where the user is typing
  (when *minibuffer* (return-from browser-key (minibuffer-key key state)))
  ;; a TERMINAL takes every key but the SuperL layer: C-g, Esc, C-c belong to the
  ;; program on the other end, as they would in any terminal (poc/terminal-pane.lisp)
  (when (and (terminal-pane-p (current-pane)) (not (lol.canvas:super-p state)))
    (setf *pending-meta* nil *pending-prefix* nil)
    (funcall 'terminal-key (current-pane) key state)
    (return-from browser-key nil))
  (let* ((p (current-pane))
         ;; Meta = the Alt/Esc-prefix bit on this key, OR a sticky Esc from last time.
         ;; Consume the sticky flag every keystroke; the Esc branch below re-arms it.
         (meta (or (lol.canvas:meta-p state) *pending-meta*))
         ;; Fold the sticky-Esc Meta into STATE so downstream (tv-key's M-f / M-b)
         ;; sees it too, not just the M-. branch here.
         (state (if meta (logior state 8) state)))
    (setf *pending-meta* nil)
    ;; a C-s / C-r search running here takes its keys first; one that ends it (any
    ;; key that is not a search key) then carries on below and does what it does
    (when (and *isearch-tv* (isearch-key key state))
      (return-from browser-key nil))
    ;; the text-size repeat (below) lasts only while the keys are + - 0
    (when (and (eq *pending-prefix* :text-scale) (not (member key '(#\+ #\= #\- #\_ #\0))))
      (setf *pending-prefix* nil))
    (cond
      ;; A WEB PAGE (initramfs/web.lisp) takes every key but the SuperL layer: Esc,
      ;; C-g, C-c, Tab all mean what they mean in a browser there, and the page — not
      ;; the shell — is what you are typing into. WEB-KEY keeps a few browser chords.
      ((and (eq (view-kind (pane-view p)) :web) (not (lol.canvas:super-p state)))
       (funcall 'web-key p key state))
      ;; --- text size, Emacs's way: C-x C-+ (or C-=) bigger, C-x C-- smaller, C-x C-0
      ;;     the default. As in Emacs, once one has been typed a bare + - 0 repeats it
      ;;     without the C-x (the :TEXT-SCALE prefix). C-- reaches us from the console
      ;;     as C-_ (byte 31), hence #\_ too. SuperL-= / - / 0 below do the same. ---
      ((and (member *pending-prefix* '(:c-x :text-scale))
            (member key '(#\+ #\= #\- #\_ #\0)))
       (setf *pending-prefix* :text-scale)
       (change-font-size (case key ((#\+ #\=) :bigger) ((#\- #\_) :smaller) (t :reset)))
       nil)
      ;; M-. — jump to the definition of the symbol at point, the keyboard twin of
      ;; Ctrl-click. Must precede the editable branch, or the "." would self-insert.
      ((and meta (eql key #\.) (pane-tv p)) (jump-at-point (pane-tv p)))
      ;; M-, — SENDERS of the symbol at point: the other half of the pair the design
      ;; mock-up names (§1, "M-. / M-, Senders / Implementors"), and the key
      ;; commands-for has always advertised for it. Same resolution as the jump, so
      ;; anything you can M-. you can also ask who calls.
      ((and meta (eql key #\,) (pane-tv p)) (senders-at-point (pane-tv p)))
      ;; Esc alone = the Emacs Meta prefix (sticky for the next key); it must NOT quit
      ;; the browser. Leaving via C-g at the root (below) is the deliberate exit.
      ((eq key :escape) (setf *pending-meta* t) nil)
      ;; Shift-F10 — the context menu from the keyboard (the Menu key does the same,
      ;; via the keyboard's evdev; see fb-browser.lisp)
      ((and (eq key :f10) (lol.canvas:shift-p state)) (keyboard-context-menu) nil)
      ;; C-g = back, everywhere (the console can't deliver SuperL, so Ctrl-g is the
      ;; portable "back" that also works while editing a source pane). At the root there
      ;; is nothing to pop, so C-g does NOTHING — leaving the environment is SuperL-q
      ;; only, so a stray C-g can't drop you out by accident.
      ((and (lol.canvas:ctrl-p state) (member key '(#\g #\G)))
       (setf *pending-prefix* nil *browser-status* nil)
       (cond
         ;; a conversation that is answering: C-g stops the turn (doc/agent.org §8.7)
         ((and (fboundp 'agent-cancel-at-pane) (funcall 'agent-cancel-at-pane p)))
         ;; focus is in the right column: hand it back to the list and STOP. Popping
         ;; the trail as well would throw away the split you are standing in, when
         ;; all you asked for was to stop editing.
         ((and (pane-editing-side p) (pane-side p)) (unfocus-side p))
         ;; closing a file with unsaved edits: the first C-g only warns. Emacs asks
         ;; "Modified buffer exists; kill anyway?" — the same question, one key cheaper.
         ((and (file-pane-dirty-p p) (not (eq *unsaved-warned* p)))
          (setf *browser-status* "unsaved changes - C-x C-s saves, C-g again discards them"
                *unsaved-warned* p))
         ((cdr (current-trail)) (setf *unsaved-warned* nil) (trail-pop)))
       nil)
      ;; --- SuperL layer: tab navigation, the Spotter + the cheatsheet, over ANY pane
      ;;     (never the editor's). Super arrives via the keyboard's evdev (folded into
      ;;     STATE, bit 6), so it works on the bare console too — see fb-browser. ---
      ((lol.canvas:super-p state)
       (setf *pending-prefix* nil)
       (cond
         ;; on a web page, SuperL-Left / -Right are the PAGE's back and forward: the
         ;; browser's usual Alt-Left never arrives on the bare console, whose keymap
         ;; spends Alt-Left/Right on switching virtual terminals (defkeymap.map)
         ((and (member key '(:left :right)) (eq (view-kind (pane-view (current-pane))) :web))
          (funcall 'web-go (view-data (pane-view (current-pane))) (if (eq key :left) -1 1)) nil)
         ((eq key :left) (trail-pop) (setf *browser-status* nil) nil)          ; back in trail
         ((eq key :tab) (cycle-trail (if (lol.canvas:shift-p state) -1 1)) nil) ; cycle tabs
         ((and (characterp key) (char<= #\1 key #\9))                          ; jump to tab N
          (activate-trail (- (char-code key) (char-code #\1))) nil)
         ;; SuperL-⇧-n moves THIS trail to slot n, swapping with whatever is there —
         ;; the design's binding table (§1). Shift-digit arrives as the shifted
         ;; character on any keyboard, so match those rather than a Shift bit.
         ((and (characterp key) (position key ")!@#$%^&*(" ))
          (swap-trail (position key ")!@#$%^&*(")) nil)
         ((member key '(#\w #\W)) (close-trail) nil)                           ; close this tab
         ((member key '(#\o #\O)) (toggle-side-focus) nil)                     ; focus list <-> side
         ((eql key #\Space)                                                    ; the Spotter
          (when (fboundp 'open-spotter) (funcall 'open-spotter)) nil)
         ;; SuperL-Return: a terminal (a shell — doc/terminal.org), the binding tiling
         ;; window managers taught every hand. SuperL-Shift-Return keeps the old
         ;; meaning: open the selection in a NEW tab (SuperL-click does the same).
         ((and (member key '(:return #\Return)) (not (lol.canvas:shift-p state)))
          (if (fboundp 'open-default-terminal)
              (funcall 'open-default-terminal)
              (setf *browser-status* "no terminal in this build"))
          nil)
         ((member key '(:return #\Return))                                     ; open selection in a NEW tab
          (let ((it (selected-item (current-pane))))
            (when (and it (item-subject it)) (new-trail (item-subject it))))
          nil)
         ((member key '(#\e #\E))                                              ; a Workspace (or a new one)
          (open-workspace (or (eql key #\E) (lol.canvas:shift-p state))) nil)
         ((member key '(#\m #\M)) (open-modules) nil)                         ; the module index
         ((member key '(#\f #\F)) (funcall 'open-files) nil)                   ; the file manager
         ((and (member key '(#\b #\B)) (fboundp 'web-open))                   ; a web page (web.lisp)
          (funcall 'web-open) nil)
         ((member key '(#\n #\N))                                              ; the network view
          (when (fboundp 'open-network) (funcall 'open-network)) nil)
         ((member key '(#\r #\R))                     ; the screen recorder (recorder.lisp):
          (when (fboundp 'recorder-key)               ; r record/pause/resume, S-r stop
            (funcall 'recorder-key (or (eql key #\R) (lol.canvas:shift-p state))))
          nil)
         ((member key '(#\l #\L))                                              ; the CL-USER package
          (new-trail (find-package :cl-user)) nil)
         ((member key '(#\h #\H)) (open-clipboard-manager) nil)               ; clipboard history
         ((member key '(#\a #\A))                                              ; the agent's conversation
          (when (fboundp 'agent-show) (funcall 'agent-show)) nil)
         ;; text size (doc/fonts.org): = and + bigger, - smaller, 0 the default
         ((member key '(#\= #\+)) (change-font-size :bigger) nil)
         ((eql key #\-) (change-font-size :smaller) nil)
         ((eql key #\0) (change-font-size :reset) nil)
         ((member key '(#\? #\/))                                              ; Super-? — every binding
          (when (fboundp 'open-cheatsheet) (funcall 'open-cheatsheet)) nil)
         ((member key '(#\q #\Q)) (leave-menu))                             ; shut down / console
         (t nil)))
      ;; --- ⇧↵ editable SIDE pane has focus: its keys edit it (Accept, verbs, typing);
      ;;     SuperL-o (above) hands focus back to the list/stage. This MUST precede the
      ;;     stage branch: a stage's letters spawn things, and typing "ship" into the
      ;;     Workspace beside it would otherwise launch four objects. ---
      ((and (pane-editing-side p) (pane-side p))
       (if (pane-tv (pane-side p))
           (edit-pane-key (pane-side p) key state)
           (side-list-key p (pane-side p) key state)))
      ;; --- the stage (§3b): its own verbs, before the list keys, because a stage has
      ;;     no rows for up/dn to move and letters here mean "spawn one of those". ---
      ((and (eq (view-kind (pane-view p)) :stage)
            (not (lol.canvas:ctrl-p state))
            (stage-key p key))
       nil)
      ;; --- editable panes: source (Accept) and workspace (eval verbs). Chords go
      ;;     through *pending-prefix*: C-c then {C-c Accept | C-p Print | C-i Inspect},
      ;;     C-x then C-e Do it. Any other key edits. ---
      ((editor-pane-p p) (edit-pane-key p key state))
      ;; --- read-only reference pane: scroll only ---
      ((and (pane-tv p) (or (and (lol.canvas:meta-p state) (member key '(#\w #\W)))
                            (and (lol.canvas:ctrl-p state) (eq key :insert))))
       (editor-copy (pane-tv p)) nil)                   ; copying is reading, so allowed
      ((and (pane-tv p) (eq *pending-prefix* :c-x) (lol.canvas:ctrl-p state) (member key '(#\x #\X)))
       (setf *pending-prefix* nil)
       (exchange-point-and-mark (pane-tv p)) nil)                    ; C-x C-x
      ;; C-x starts the text-size chord here too; any other key drops a pending one
      ((and (pane-tv p) (lol.canvas:ctrl-p state) (member key '(#\x #\X)))
       (setf *pending-prefix* :c-x) nil)
      ((and (pane-tv p) (eq *pending-prefix* :c-x)) (setf *pending-prefix* nil) nil)
      ;; C-s / C-r — searching is reading, so a read-only text has it too
      ((and (pane-tv p) (lol.canvas:ctrl-p state) (member key '(#\s #\S #\r #\R)))
       (start-isearch (pane-tv p) (member key '(#\s #\S))) nil)
      ((pane-tv p)
       (case key
         (:up        (lol.textview:tv-key (pane-tv p) :up 0))
         (:down      (lol.textview:tv-key (pane-tv p) :down 0))
         (:page-up   (lol.textview:tv-key (pane-tv p) :page-up 0))
         (:page-down (lol.textview:tv-key (pane-tv p) :page-down 0))
         ;; q is BACK, never "leave": it once returned :quit, a relic of the browser
         ;; as a menu-launched app, and killed the whole environment from a viewer
         ;; (an image, say). Leaving is SuperL-q, which asks.
         (#\q (trail-pop) nil)
         ;; a read-only text pane may still have VERBS: a run pane's k (stop) and
         ;; r (run again), initramfs/runs.lisp. Same hook as a list's letters.
         (t (unless (or (lol.canvas:ctrl-p state) meta)
              (ignore-errors (subject-key (pane-subject p) p key state)))
            nil)))
      ;; --- a subject's own keys on its list (the file manager's letters) ---
      ((and (null (pane-tv p))
            (not (lol.canvas:ctrl-p state)) (not meta)
            (not (member *pending-prefix* '(:c-c :c-x)))
            (ignore-errors (subject-key (pane-subject p) p key state)))
       nil)
      ;; --- list pane: plain-key navigation (nothing to edit) ---
      (t
       (cond
         ;; a C-x chord on a list that was not C-x C-+ / C-- / C-0 (caught above): drop it
         ((eq *pending-prefix* :c-x) (setf *pending-prefix* nil) nil)
         ((eql key #\q) (trail-pop) nil)               ; back, as above — never leave
         ((eq key :up)   (set-pane-sel p (1- (pane-sel p))))
         ((eq key :down) (set-pane-sel p (1+ (pane-sel p))))
         ((eq key :page-up)   (set-pane-sel p (- (pane-sel p) +list-page+)))
         ((eq key :page-down) (set-pane-sel p (+ (pane-sel p) +list-page+)))
         ((eq key :return)
          (if (lol.canvas:shift-p state)
              (open-beside p)                            ; ⇧↵ — open BESIDE (editable split)
              (let ((it (selected-item p)))              ; ↵ — drill in place
                (when (and it (item-subject it)) (trail-push (item-subject it))))))
         ;; C-c C-c — Accept, which everywhere else in the browser means "write this
         ;; into the running image". On an inspector row it writes a value into the
         ;; place that row shows. Arrives as a chord through *pending-prefix*, the
         ;; same way the editable panes take it.
         ((and *pending-prefix* (eq *pending-prefix* :c-c))
          (setf *pending-prefix* nil)
          (when (lol.canvas:ctrl-p state)
            (cond
              ;; C-c C-c — Accept: write the typed value into the place this row shows.
              ((member key '(#\c #\C)) (set-field p))
              ;; C-c C-e — Evaluate in context: the Smalltalk code pane, on any subject.
              ((member key '(#\e #\E)) (eval-in-context-pane p))
              ;; C-c C-a — Apply it when the subject IS a function (that binding came
              ;; first); on any other subject it is Ask it, the agent's verb.
              ((member key '(#\a #\A))
               (if (or (functionp (pane-subject p)) (not (fboundp 'agent-ask-at-pane)))
                   (apply-function-pane p)
                   (funcall 'agent-ask-at-pane p)))))
          ;; C-c ? — Ask it on any list, a function's view included
          (when (and (eql key #\?) (fboundp 'agent-ask-at-pane))
            (funcall 'agent-ask-at-pane p))
          nil)
         ((and (lol.canvas:ctrl-p state) (member key '(#\c #\C)))
          (setf *pending-prefix* :c-c) nil)
         ;; C-x on a list starts a chord too — only C-x C-+ / C-- / C-0 complete it here
         ((and (lol.canvas:ctrl-p state) (member key '(#\x #\X)))
          (setf *pending-prefix* :c-x) nil)
         ;; -> moves focus into the right-hand column (see FOCUS-SIDE). <- stays
         ;; "back", so the two arrows are not a pair here: -> goes deeper into the
         ;; frame you are looking at, <- leaves it.
         ((eq key :right) (focus-side p))
         ((member key '(:backspace :left)) (trail-pop))
         (t nil))))))

;;; ---- jump-to-definition: Ctrl-click a symbol in a source pane -------------

(defun token-span (string col)
  "The columns [START END) of the symbol-token in STRING under column COL, or NIL
   when COL is whitespace or a delimiter. A token is a maximal run of non-delimiter
   characters — enough to catch =foo-bar=, =*var*=, and =pkg:name= without a real
   reader."
  (flet ((delim (ch) (member ch '(#\Space #\Tab #\( #\) #\' #\" #\` #\, #\;))))
    (when (and string (<= 0 col) (< col (length string)) (not (delim (char string col))))
      (let ((start col) (end col) (n (length string)))
        (loop while (and (> start 0) (not (delim (char string (1- start))))) do (decf start))
        (loop while (and (< end n) (not (delim (char string end)))) do (incf end))
        (values start end)))))

(defun click-token (string col)
  "The symbol-token in STRING under column COL, or NIL (see TOKEN-SPAN)."
  (multiple-value-bind (start end) (token-span string col)
    (and start (subseq string start end))))

(defun resolve-symbol (token)
  "TOKEN (a raw source word) -> an EXISTING symbol, or NIL. Understands =pkg:name=
   and =pkg::name=; otherwise tries CL-USER then CL. Never interns (so clicking a
   typo can't pollute a package)."
  (when (and token (plusp (length token)))
    (let* ((tok (string-trim "#'`,@" token))
           (c (position #\: tok)))
      (when (plusp (length tok))
        (if c
            (let* ((dbl (and (< (1+ c) (length tok)) (char= (char tok (1+ c)) #\:)))
                   (pkg (find-package (string-upcase (subseq tok 0 c))))
                   (nm  (subseq tok (+ c (if dbl 2 1)))))
              (and pkg (plusp (length nm)) (find-symbol (string-upcase nm) pkg)))
            (or (find-symbol (string-upcase tok) :cl-user)
                (find-symbol (string-upcase tok) :cl)))))))

(defun jumpable-p (sym)
  "A symbol worth jumping to: it names a callable, a variable, or a class."
  (and (symbolp sym) sym
       (or (fboundp sym) (boundp sym) (find-class sym nil))))

(defun token-at-point (tv)
  "The source token under TV's caret, or the one just before it — so a gesture fires
   with the caret resting past the end of a symbol, which is where it usually rests.
   Shared by M-. and M-, so the two can never disagree about what \"at point\" means."
  (let ((lines (lol.textview:tv-lines tv))
        (line  (lol.textview:tv-point-line tv))
        (col   (lol.textview:tv-point-col tv)))
    (when (< line (length lines))
      (let ((s (aref lines line)))
        (or (click-token s col)
            (and (> col 0) (click-token s (1- col))))))))

(defun jump-at-point (tv)
  "Resolve the symbol at TV's caret and drill into its definition on a new trail
   level (source if we recorded it, else a reference — PRESENT on a SUBJ-DEFN handles
   both). The keyboard M-. and the Ctrl-click share this once the caret is placed.
   Tries the caret column, then one to its left, so M-. also fires with the caret
   resting just past a symbol. No-op when there's no jumpable symbol there."
  (let ((sym (resolve-symbol (token-at-point tv))))
    (when (jumpable-p sym)
      (setf *pending-prefix* nil *browser-status* nil)
      (trail-push (subj-defn sym))
      t)))

(defun senders-at-point (tv)
  "M-, — open SENDERS of the symbol at TV's caret. Smalltalk's senders, reached the
   same way as the jump: resolve the token under (or just before) point, and drill a
   SUBJ-XREF rather than the definition. No-op when there is no such symbol."
  (let ((sym (resolve-symbol (token-at-point tv))))
    (when (and (symbolp sym) sym (fboundp sym))
      (setf *pending-prefix* nil *browser-status* nil)
      (trail-push (subj-xref :senders sym))
      t)))

(defun jump-to-definition (tv x y)
  "Ctrl-click in a source pane: place the caret at X,Y and jump (see JUMP-AT-POINT)."
  (when (lol.textview:tv-hit-p tv x y)
    (lol.textview:tv-click tv x y)                     ; place the caret at the click
    (jump-at-point tv)))

(defun browser-scroll (lines)
  "Scroll the current pane by LINES (+ down / toward the end) — the mouse wheel.
   BOTH kinds of pane now scroll their VIEWPORT and leave the cursor alone, which is
   what scrolling means: you are looking somewhere else, not moving. A text pane does
   it with tv-wheel (point stays put); a list pane does it by moving PANE-TOP with
   PANE-FOLLOW off for that frame, so the widget does not immediately drag the view
   back to the selection. The widget clamps, so this cannot run off either end."
  (let ((p (current-pane)))
    (cond
      ;; Ctrl held: the wheel resizes the text, as SuperL-= / SuperL-- do — one step a
      ;; notch, up bigger (every browser's Ctrl-wheel). The held Ctrl lives in the
      ;; keyboard's evdev state (fb-browser.lisp), read by name: this file loads first.
      ((and (boundp '*ctrl-down*) (symbol-value '*ctrl-down*))
       (dotimes (i (min 3 (max 1 (round (abs lines) (max 1 +wheel-lines+)))))
         (change-font-size (if (minusp lines) :bigger :smaller))))
      ;; a web page scrolls itself: the wheel is sent to Chrome (initramfs/web.lisp)
      ((eq (view-kind (pane-view p)) :web) (funcall 'web-wheel lines))
      ((terminal-pane-p p) (funcall 'terminal-wheel p lines))   ; history, or the program's
      ((pane-tv p) (lol.textview:tv-wheel (pane-tv p) lines))
      (t (setf (pane-top p) (max 0 (+ (pane-top p) lines))
               (pane-follow p) nil)))))

;;; ---- press and drag: selecting text with the pointer ------------------------
;;; A click that places the caret in a text view also arms a DRAG: while the button
;;; stays down, pointer motion extends the selection from that caret to the pointer
;;; (TV-DRAG — the anchor stays where the press began). The backend tells us the
;;; button is still held; what the press landed in is THE HAND's to remember
;;; (LOL.MORPH:HAND-PRESS, poc/morph.lisp) — the press in progress is the pointer's
;;; state, not a view's, and by the time the pointer moves it may be over another pane.
;;; It was a special per kind of view (*DRAG-TV*, *TERM-DRAG*, *WEB-PRESS* …) until the
;;; hand kept it.

(defparameter +drag-slop+ 3
  "Pixels the pointer must travel, held down, before it selects. A clickpad's press
   rolls the fingertip, which nudges the pointer a pixel or two; without this slop
   every press would select a character or two instead of just placing the caret.")

(defun the-hand ()
  "The world's hand — the pointer, as a morph — or NIL outside the environment."
  (and *world* (lol.morph:world-hand *world*)))

(defun hand-press ()
  "The press in progress, as its morph described it (LOL.MORPH:HAND-PRESS), or NIL."
  (let ((h (the-hand))) (and h (lol.morph:hand-press h))))

(defun (setf hand-press) (press)
  (let ((h (the-hand))) (when h (setf (lol.morph:hand-press h) press)))
  press)

(defun press-kind (&optional (press (hand-press)))
  "Whose the press in progress is: :TEXT, :TERMINAL, :WEB, :DRAG — or NIL."
  (first press))

(defun browser-drag (x y)
  "The left button is still held and the pointer is at X,Y: extend what the press in
   progress extends — a text selection, a terminal's, a drag in a web page. Returns T
   when the frame changed (so the caller redraws), NIL when there is no drag or the
   pointer is still inside the slop."
  (let ((press (hand-press)))
    (case (press-kind press)
      ;; a press in a web page drags IN the page (selecting, sliding): web.lisp's
      (:web (funcall 'web-drag x y))
      (:terminal (funcall 'terminal-drag x y))
      (:text
       (let ((origin (getf (rest press) :origin)) (tv (getf (rest press) :tv)))
         (cond ((and origin
                     (<= (abs (- x (car origin))) +drag-slop+)
                     (<= (abs (- y (cdr origin))) +drag-slop+))
                nil)
               (t (setf (getf (rest press) :origin) nil)
                  (lol.textview:tv-drag tv x y)
                  t))))
      (t nil))))

(defun arm-drag (tv x y)
  "A press just placed TV's caret at X,Y: a drag from here selects in TV."
  (setf (hand-press) (list :text :tv tv :origin (cons x y))))

;;; Double and triple click, as X does it: a second press on the same spot soon after
;;; the first selects the WORD there, a third the whole LINE (its newline included, as
;;; xterm does), and a fourth starts over with a plain caret. What gets selected goes
;;; straight to PRIMARY and the history, like a drag's — that is the point of it in X:
;;; double-click a word, middle-click it somewhere else. The hand counts the run
;;; (LOL.MORPH:HAND-COUNT-CLICK), as it counts a web page's.

(defparameter +multi-click-ms+ 400
  "How soon a press must follow the last one to count as the next click of a run.")

(defun press-count (tv x y)
  "This press on TV at X,Y is the Nth of a run (1, 2, 3, then 1 again): the same view,
   within +DRAG-SLOP+ of the last press and +MULTI-CLICK-MS+ after it. Without a world
   (no hand to remember the last press) every press is the first."
  (let ((h (the-hand)))
    (if h
        (lol.morph:hand-count-click h tv x y :slop +drag-slop+ :ms +multi-click-ms+ :max 3)
        1)))

(defun text-press (tv x y)
  "A left press in text view TV at X,Y: place the caret and arm a drag, or — as the
   second or third click of a run — select the word or the line there."
  (lol.textview:tv-click tv x y)
  (let ((n (press-count tv x y))
        (l (lol.textview:tv-point-line tv))
        (lines (lol.textview:tv-lines tv)))
    (case n
      (1 (arm-drag tv x y))
      (2 (multiple-value-bind (start end)
             ;; the glyph UNDER the pointer (floor), not the caret gap nearest it
             (token-span (aref lines l)
                         (+ (lol.textview::tv-hscroll tv)
                            (floor (- x (lol.textview::tv-x tv) (lol.textview::tv-pad tv)
                                      (lol.textview::tv-gutter-w tv))
                                   (lol.textview::tv-cw tv))))
           (when start (lol.textview:tv-select-span tv l start l end))))
      (3 (if (< (1+ l) (length lines))
             (lol.textview:tv-select-span tv l 0 (1+ l) 0)
             (lol.textview:tv-select-span tv l 0 l (length (aref lines l))))))
    (when (> n 1)
      (let ((text (editor-region-text tv)))
        (when text
          (lol.clipboard:clip-select text)
          (clip-status "selected" text))))))

(defun minibuffer-click (rect x y)
  "A click while the minibuffer is up: on its ✕ (RECT, as the status line drew it),
   clear the text and keep the prompt open (returns T); anywhere else NIL — the caller
   ends the prompt, as before."
  (when (and *minibuffer* rect (<= (first rect) x (second rect)) (<= (third rect) y (fourth rect)))
    (setf (mb-text *minibuffer*) "" (mb-point *minibuffer*) 0)
    (minibuffer-edited *minibuffer*)
    t))

;;; ---- a press, wherever it lands -----------------------------------------------
;;; A left press goes to the morph under the pointer (doc/morphic.org §12.5): a tab or
;;; the + (TAB-STRIP-CLICK), a breadcrumb (back to that level), the clipboard icon, the
;;; minibuffer's ✕, a window's title bar or grip — and anything inside the expanded
;;; pane is PANE-CLICK. Every one of them that is the browser's begins the same way.

(defun press-prelude ()
  "What any press in the browser does first: it ends a drag (only a press in text
   re-arms one), and it ends an open prompt — a click is a change of subject, like
   C-g (the live filter keeps what was typed: its ON-CANCEL decides that, not us)."
  (setf (hand-press) nil)
  (when *minibuffer* (minibuffer-finish (mb-on-cancel *minibuffer*))))

(defun tab-strip-click (tabs closes plus x)
  "A press at X in the tab strip, whose hit maps are TABS, CLOSES and PLUS (as
   DRAW-TABS returned them): + opens a fresh Workspace, a tab's × closes THAT tab (not
   merely the focused one), a tab activates its trail."
  (cond
    ((and plus (<= (car plus) x (cdr plus))) (new-trail (subj-workspace)))
    ((find-if (lambda (e) (<= (cadr e) x (cddr e))) closes)
     (let ((hit (find-if (lambda (e) (<= (cadr e) x (cddr e))) closes)))
       (activate-trail (car hit))
       (close-trail)))
    (t (let ((tab (find-if (lambda (e) (<= (cadr e) x (cddr e))) tabs)))
         (when tab (activate-trail (car tab)))))))

(defun pane-click (p x y &optional ctrl super)
  "A left press at X,Y (where P was drawn) in the expanded pane P: on its own body, or
   in its right-hand column (a side pane, the nav split's preview) — whichever side of
   the split X is on; a press on the panel around them (a gutter) is decided the same
   way, so it keeps the meaning it always had. -> the body morph the press was for (its
   caller damages it), or NIL. Uses the geometry P and its bodies recorded when they
   drew (PANE-BODY-TOP, PANE-SPLIT-X, a list body's rows), so click and draw can never
   disagree.

   In a side-by-side, a plain click in the half that does NOT have keyboard focus is
   a focus move and nothing more — SuperL-o with a pointer. It still positions you
   inside that half (the caret in text, the selection on a row), because that is what
   you pointed at, but it does not drill: crossing the split is not navigation, and a
   focus move that navigated would throw away the split you were working in. A click
   in the half that already has focus behaves as it always did, and Ctrl-click /
   SuperL-click keep their meanings either side, because a modified click is an
   explicit command rather than a 'put me over there'."
  (end-isearch)                          ; a press ends a search, as in Emacs
  (let ((split (pane-split-x p)))
    (cond
      ;; the column, on the header band above it: nothing (as it always was)
      ((and split (>= x split) (< y (pane-body-top p))) nil)
      ((and split (>= x split))
       (column-press p x y ctrl super)
       (let ((side (or (pane-side p) (pane-preview p)))) (and side (pane-body side))))
      (t (let ((b (pane-body p)))
           (when b (body-press b p x y ctrl super))
           b)))))

(defgeneric body-press (body pane x y ctrl super)
  (:documentation "A left press at X,Y on BODY, the own body of the expanded pane PANE
   (not its right-hand column: COLUMN-PRESS). CTRL and SUPER say which modifiers were
   held. By default — a list, a picture — the row there is selected and drilled."))

;; a list: the row there — SuperL-click opens it in a NEW trail; a plain click also
;; returns focus to the list (away from any side pane)
(defmethod body-press ((b pane-body) p x y ctrl super)
  (declare (ignore x ctrl))
  (when (>= y (pane-body-top p))
    (let ((came-from-side (pane-editing-side p)))
      ;; UNFOCUS-SIDE rather than a bare (setf (pane-editing-side p) nil): a side the
      ;; pointer or -> merely FOCUSED goes away again and you are looking at the
      ;; live preview you started from, while one you PINNED with <S-Return> stays.
      ;; Same rule C-g follows, so the two ways back agree.
      (unfocus-side p)
      ;; which row was actually painted here? The body's rows answer; a click on a
      ;; group header hits no row and is correctly inert.
      (let* ((items (view-items (pane-view p)))
             (i (lol.widgets:hit-row (pane-rows p) y)))
        (when (and i (< i (length items)))
          (set-pane-sel p i)
          ;; If the side pane had focus, taking it back WAS this click's job.
          ;; Select the row and stop — drilling as well would navigate out of the
          ;; split you were working in, which is the one thing a focus move must
          ;; never do. SuperL-click stays an explicit "open in a new trail".
          (unless (and came-from-side (not super))
            (let ((it (nth i items)))
              (when (item-subject it)
                (if super (new-trail (item-subject it))
                    (trail-push (item-subject it)))))))))))

;; text: Ctrl-click jumps to definition; a plain click places the caret (and arms a
;; drag) — in a read-only pane too, because SELECTING text to copy it is reading, not
;; editing. The caret is simply not drawn there.
(defmethod body-press ((b text-body) p x y ctrl super)
  (declare (ignore super))
  (setf (pane-editing-side p) nil)
  (cond
    (ctrl (jump-to-definition (pane-tv p) x y))
    ((lol.textview:tv-hit-p (pane-tv p) x y)
     (text-press (pane-tv p) x y))))

;; the dispatch matrix: a cell drills its effective-method onion
(defmethod body-press ((b grid-body) p x y ctrl super)
  (declare (ignore ctrl super))
  (matrix-cell-click p x y))

;; the stage: the instance under the pointer is selected (a miss clears the
;; selection), and focus comes back to the stage's half. That it is only the stage's
;; OWN half is the point — a stage with its Workspace beside it (§3b) is a
;; side-by-side, and the Workspace half is the column's (COLUMN-PRESS).
(defmethod body-press ((b stage-body) p x y ctrl super)
  (declare (ignore ctrl super))
  (stage-click p x y)
  (setf (pane-editing-side p) nil))

;; a web page: its toolbar, or a click sent into the page (initramfs/web.lisp)
(defmethod body-press ((b web-body) p x y ctrl super)
  (when (>= y (pane-body-top p)) (funcall 'web-click p x y ctrl super)))

;; a terminal: the program's click if it asked for the mouse, else a selection
(defmethod body-press ((b terminal-body) p x y ctrl super)
  (declare (ignore ctrl super))
  (when (>= y (pane-body-top p)) (funcall 'terminal-press p x y)))

(defun column-press (p x y ctrl super)
  "A left press at X,Y in the right-hand column of the expanded pane P. A pinned side
   pane: focus it, and place the caret (Ctrl jumps) or select the row. The nav split's
   preview: a matrix preview is LIVE — clicking a cell drills its effective-method onion
   straight from here; a source preview jumps on Ctrl-click; otherwise the click puts
   you over there (SuperL-click opens the previewed selection in a new trail)."
  (cond
    ((pane-side p)
     (setf (pane-editing-side p) t)
     (let* ((side (pane-side p)) (tv (pane-tv side)))
       (cond
         ((and tv (lol.textview:tv-hit-p tv x y))
          (if ctrl
              (jump-to-definition tv x y)
              (text-press tv x y)))
         ;; a list in the right column: select the row that was drawn there, read
         ;; back from its OWN hit map (the side pane's list body)
         ((null tv)
          (let ((i (lol.widgets:hit-row (pane-rows side) y)))
            (when i (set-pane-sel side i)))))))
    (t
     (let ((sel (selected-item p))
           (pv (pane-preview p)))
       (cond
         ((and pv (eq (view-kind (pane-view pv)) :matrix))
          (or (matrix-cell-click pv x y)               ; a cell -> its onion
              (and sel (item-subject sel) (trail-push (item-subject sel)))))  ; miss -> open matrix
         ((and ctrl pv (pane-tv pv)) (jump-to-definition (pane-tv pv) x y))
         ;; A plain click on the preview means "put me over there" — exactly the
         ;; promotion -> performs. It does not drill: the row it would have opened is
         ;; one click away on the left, and crossing the split is a focus move.
         ;; FOCUS-SIDE is NIL when there is nothing previewable, so a click on an
         ;; empty column falls through rather than swallowing itself.
         ((and (not super) (focus-side p)))
         ((and sel (item-subject sel))
          (if super (new-trail (item-subject sel)) (trail-push (item-subject sel)))))))))

;;; ---- what is under the pointer --------------------------------------------
;;; The right-button menu and the middle-button halo both start from the same
;;; question: WHAT did you point at? Not which pixel — which THING: a row (and the
;;; subject it opens), a word in some text (and the symbol it names), an instance on
;;; the stage, a tab. One function answers it, from the hit maps the last frame
;;; recorded, so the menus and the drawing can never disagree about what is where.

(defun stage-body-at (pane px py)
  "The stage body under PX,PY in PANE's viewport, or NIL. The hit test STAGE-CLICK
   performs, without selecting anything."
  (let ((stage (view-data (pane-view pane))))
    (when (pane-stage-rect pane)
      (destructuring-bind (x y w vh) (pane-stage-rect pane)
        (when (and (<= x px (+ x w)) (<= y py (+ y vh)))
          (find-if (lambda (b)
                     (let ((dx (- px x (body-x b))) (dy (- py y (body-y b))))
                       (<= (+ (* dx dx) (* dy dy))
                           (let ((rr (+ (body-r b) 6))) (* rr rr)))))
                   (reverse (stage-bodies stage))))))))

(defun text-target (pane x y)
  "A target for a click at X,Y inside PANE's text view: the word there and, if it
   names one, the symbol."
  (let* ((tv (pane-tv pane))
         (l (lol.textview::line-at-y tv y))
         (c (lol.textview::col-at-x tv l x))
         (line (aref (lol.textview:tv-lines tv) l))
         (at (if (click-token line c) c (max 0 (1- c))))
         ;; past the end of the line is empty space, not the line's last word
         (token (and (<= x (+ (lol.textview::x-of-col tv (length line)) 4))
                     (click-token line at)))
         (sym (resolve-symbol token)))
    (multiple-value-bind (start end) (and token (token-span line at))
      (list :kind :text :pane pane :tv tv
            :editable (editor-pane-p pane)
            :token token :symbol sym :line l :col c :start start :end end))))

(defun row-target (pane hits y)
  "A target for a click at height Y on PANE's list, whose painted rows are HITS."
  (let* ((items (view-items (pane-view pane)))
         (i (lol.widgets:hit-row hits y))
         (it (and i (< i (length items)) (nth i items))))
    (when it
      (list :kind :row :pane pane :index i :item it :subject (item-subject it)))))

(defun pane-target (p x y)
  "What is under X,Y (where P was drawn) in the expanded pane P, as a plist with a
   :KIND of :TEXT :ROW :SPRITE or :PANE (the pane's own subject, when nothing more
   specific was hit). Never NIL: pointing at empty space still points at the pane you
   are in. (A tab is the tab strip's to answer: TAB-TARGET, poc/morphic.lisp.)"
  (progn
    (flet ((pane-itself (pane) (list :kind :pane :pane pane :subject (pane-subject pane))))
      (cond
        ;; the right-hand column: a pinned side pane, or the read-only preview
        ((and (pane-split-x p) (>= x (pane-split-x p)) (>= y (pane-body-top p)))
         (let ((side (or (pane-side p) (pane-preview p))))
           (cond ((null side) (pane-itself p))
                 ((and (pane-tv side) (lol.textview:tv-hit-p (pane-tv side) x y))
                  (text-target side x y))
                 ((null (pane-tv side))
                  (or (row-target side (pane-rows side) y) (pane-itself side)))
                 (t (pane-itself side)))))
        ((and (pane-tv p) (lol.textview:tv-hit-p (pane-tv p) x y))
         (text-target p x y))
        ((eq (view-kind (pane-view p)) :stage)
         (let ((b (stage-body-at p x y)))
           (if b
               (list :kind :sprite :pane p :body b :subject (body-obj b))
               (pane-itself p))))
        ((and (null (pane-tv p)) (not (member (view-kind (pane-view p)) +non-list-kinds+)))
         (or (row-target p (pane-rows p) y) (pane-itself p)))
        (t (pane-itself p))))))

;;; ---- running a COMMAND (present.lisp's affordance protocol) ---------------
;;; COMMANDS-FOR has always described, per type, what a subject affords and on
;;; which SURFACES — :menu, :halo, :palette, :key — and until now only the status line
;;; read it. The right-button menu shows the :menu ones and the halo the :halo ones,
;;; which is what makes both EXTENSIBLE: a new subject type gets its own entries by
;;; adding a COMMANDS-FOR method, and neither menu has to learn about it.

(defun run-command (cmd)
  "Do what COMMAND says: call its ACTION, or open its TARGET by its DISPOSITION. The
   two prompting verbs are named by their KEY, because what they open is a prompt
   about the target rather than the target itself."
  (let ((target (command-target cmd)))
    (cond
      ((command-action cmd)
       (handler-case (let ((out (make-string-output-stream)))
                       (let ((*standard-output* out)) (funcall (command-action cmd)))
                       ;; an action that PRINTS (Disassemble) reports in the status line
                       (let ((s (ws-trim-output (get-output-stream-string out))))
                         (when s (setf *browser-status*
                                       (lol.widgets:clip-text (substitute #\Space #\Newline s) 200)))))
         (serious-condition (c)
           (setf *browser-status* (format nil "~a failed: ~a" (command-label cmd) c)))))
      ((equal (command-key cmd) "C-c C-e") (eval-in-context-pane (open-pane target)))
      ((equal (command-key cmd) "C-c C-a") (apply-function-pane (open-pane target)))
      ((null target) nil)
      ((eq (command-disposition cmd) :new-trail) (new-trail target))
      (t (trail-push target)))))

(defun subject-commands (subject surface)
  "SUBJECT's commands that show on SURFACE (:menu or :halo), errors swallowed: a
   menu that cannot be built must not take the browser with it."
  (and subject
       (remove-if-not (lambda (c) (member surface (command-surfaces c)))
                      (ignore-errors (commands-for subject)))))

(defun source-subject (object)
  "The code that PRODUCED OBJECT, as a subject to open — the halo's 'Source' handle,
   Smalltalk's 'browse the class'. A symbol: its definition. A function: the
   definition of its name. A definition: itself. An instance: its class's
   definition — for a CLOS object the code that generated it is its class. NIL when
   there is nothing recorded to show."
  (flet ((defn (name) (and name (symbolp name) (jumpable-p name) (subj-defn name))))
    (cond
      ((subj-defn-p object) object)
      ;; the other SUBJ-* structures are the browser's own handles on a VIEW (a
      ;; module, an xref, a matrix), not objects some code of yours produced — their
      ;; "source" would be present.lisp's defstruct, true and useless
      ((and (typep object 'structure-object)
            (let ((n (symbol-name (type-of object))))
              (and (> (length n) 5) (string= "SUBJ-" n :end2 5))))
       nil)
      ((symbolp object) (defn object))
      ((functionp object)
       (let ((name (ignore-errors (sb-kernel:%fun-name object))))
         (defn (if (consp name) (second name) name))))   ; (setf foo) -> foo
      ((typep object '(or standard-object structure-object condition))
       (let ((cname (class-name (class-of object))))
         (or (defn cname) (find-class cname nil))))
      (t nil))))

;;; ---- the right-button menu ------------------------------------------------
;;; The obvious things first, then whatever the thing under the pointer says it
;;; affords. An entry is a plist (:label :key :action :group); :GROUP becomes a
;;; section header, so the generic edit verbs and the type's own commands read as
;;; separate lists.

(defun menu-entry (label action &key key group tone)
  "One entry. TONE is a Lisp-concept keyword the halo colours its handle by."
  (list :label label :action action :key key :group group :tone tone))

(defun command-entries (subject surface &optional (group "commands"))
  "SUBJECT's commands for SURFACE as menu entries."
  (mapcar (lambda (c)
            (menu-entry (command-label c) (lambda () (run-command c))
                        :key (command-key c) :group group))
          (subject-commands subject surface)))

(defun text-menu-entries (target)
  "The edit verbs for a right-click in text, then the symbol under the pointer."
  (let* ((tv (getf target :tv))
         (sel (lol.textview::selection-p tv))
         (editable (getf target :editable))
         (sym (getf target :symbol))
         (g "edit"))
    (append
     (remove nil
             (list (when (and editable sel) (menu-entry "Cut" (lambda () (with-undo (tv) (editor-cut tv))) :key "C-w" :group g))
                   (when sel (menu-entry "Copy" (lambda () (editor-copy tv)) :key "M-w" :group g))
                   (when editable (menu-entry "Paste" (lambda () (with-undo (tv) (editor-paste tv))) :key "C-y" :group g))
                   (when editable (menu-entry "Paste the selection" (lambda () (with-undo (tv) (editor-paste-primary tv)))
                                              :key "middle" :group g))
                   (when (and editable (plusp (lol.textview:tv-undo-count tv)))
                     (menu-entry "Undo" (lambda () (editor-undo tv)) :key "C-_" :group g))
                   (menu-entry "Select all" (lambda () (editor-select-all tv)) :key "C-x h" :group g)))
     ;; run the code under the pointer, where code runs: the Workspace, a conversation,
     ;; a Lisp file — the top-level form at the click, or the selection
     (let ((p (getf target :pane)))
       (when (and p (or (workspace-pane-p p) (lisp-file-pane-p p)))
         (list (menu-entry (if sel "Do it: the selection" "Do it: the form here")
                           (lambda ()
                             (unless sel (lol.textview:tv-move-point tv (getf target :line) (getf target :col)))
                             (workspace-do-it p t))
                           :key "C-c C-e" :group "eval"))))
     (when (and sym (symbolp sym))
       (let ((gs (format nil "~(~a~)" sym)))
         (remove nil
                 (list (when (jumpable-p sym)
                         (menu-entry "Go to definition" (lambda () (trail-push (subj-defn sym)))
                                     :key "M-." :group gs))
                       (when (fboundp sym)
                         (menu-entry "Senders" (lambda () (trail-push (subj-xref :senders sym)))
                                     :key "M-," :group gs))
                       (menu-entry "Inspect" (lambda () (trail-push sym)) :key "C-c C-i" :group gs))))))))

(defun row-menu-entries (target)
  "Open it, three ways, and copy its name — then the subject's own commands."
  (let ((subject (getf target :subject))
        (item (getf target :item)))
    (append
     (when subject
       (list (menu-entry "Open" (lambda () (trail-push subject)) :key "ret" :group "open")
             (menu-entry "Open in a new tab" (lambda () (new-trail subject)) :key "SuperL-S-ret" :group "open")
             (menu-entry "Open beside"
                         (lambda ()
                           (set-pane-sel (getf target :pane) (getf target :index))
                           (open-beside (getf target :pane)))
                         :key "S-ret" :group "open")))
     (list (menu-entry "Copy name" (lambda () (copy-to-clipboard (item-label item)))
                       :group "open"))
     (command-entries subject :menu))))

(defun environment-menu-entries ()
  "What any right-click ends with: the places you can always go."
  (list (menu-entry "New Workspace" (lambda () (new-trail (subj-workspace))) :group "environment")
        (menu-entry "Module index" (lambda () (open-modules)) :key "SuperL-m" :group "environment")
        (menu-entry "Files" (lambda () (funcall 'open-files)) :key "SuperL-f" :group "environment")
        (menu-entry "Find anything..." (lambda () (when (fboundp 'open-spotter) (funcall 'open-spotter)))
                    :key "SuperL-spc" :group "environment")
        (menu-entry "Clipboard history..." (lambda () (open-clipboard-manager))
                    :key "SuperL-h" :group "environment")
        (menu-entry "Keys" (lambda () (when (fboundp 'open-cheatsheet) (funcall 'open-cheatsheet)))
                    :key "SuperL-?" :group "environment")))

(defun context-menu-entries (target)
  "Everything the right-button menu offers for TARGET."
  (append
   (case (getf target :kind)
     (:text   (text-menu-entries target))
     (:row    (row-menu-entries target))
     (:sprite (append (list (menu-entry "Inspect" (lambda () (trail-push (inspect-subject (getf target :subject))))
                                        :group "instance"))
                      (command-entries (getf target :subject) :menu "instance")))
     (:tab    (let ((i (getf target :index)))
                (list (menu-entry "Close tab" (lambda () (activate-trail i) (close-trail)) :group "tab")
                      (menu-entry "Move to slot 1" (lambda () (activate-trail i) (swap-trail 1)) :group "tab"))))
     (:pane   (command-entries (getf target :subject) :menu)))
   (environment-menu-entries)))

(defun entry-rows (entries)
  "Menu entries -> widget rows (label, key hint as the detail, group as a section).
   An entry marked :DISABLED (a web page's disabled <option>) is drawn dim."
  (map 'vector (lambda (e)
                 (lol.widgets:make-row (getf e :label) :detail (getf e :key)
                                       :group (getf e :group)
                                       :tint (if (getf e :disabled) *ui-dim* *ui-text*)))
       entries))

(defvar *menu-width* nil
  "NIL: the menu card's usual width (fifty cells). A number of pixels instead sizes
   the card to its content — what a web page's <select> wants, whose list should be
   at least as wide as the control it drops from (web.lisp, WEB-PICK-OPTION).")

(defun draw-context-menu (canvas w h entries sel x y)
  "The browser, then the right-button menu as a card whose corner is at X,Y — moved
   left/up when it would run off the screen, the way every desktop does it. Returns
   (values RECT HITS): the card (x0 y0 x1 y1) and its rows' hit map, for the chooser
   overlay to route clicks by."
  (draw-backdrop canvas w h)
  (let* ((rows (entry-rows entries))
         (nslots (+ (length rows)
                    (length (remove-duplicates (map 'list #'lol.widgets:row-group rows)
                                               :test #'equal))))
         (cw (if *menu-width*
                 (min (- w 16) (max *menu-width*
                                    (+ 40 (* (char-w)
                                             (reduce #'max entries :initial-value 4
                                                     :key (lambda (e) (length (getf e :label))))))))
                 (+ 20 (* 50 (char-w)))))     ; 320 at 6x15
         (ch (min (- h 40) (+ 32 (* nslots +row-h+))))
         (cx (max 4 (min x (- w cw 8))))
         (cy (max (+ +bar-h+ 2) (min y (- h ch 8)))))
    (lol.widgets:draw-card canvas cx cy cw ch)              ; no title: the list starts at the top
    (multiple-value-bind (top hits)
        (lol.widgets:draw-list canvas (+ cx 4) (+ cy 6) (- cw 8) (- ch 12) rows
                               :sel sel :top 0 :follow-sel t)
      (declare (ignore top))
      (values (list cx cy (+ cx cw) (+ cy ch)) hits))))

(defun menu-entry-at (rect hits x y)
  "The context menu entry index at X,Y, :OUTSIDE when X,Y is off the card, or NIL —
   by RECT and HITS, DRAW-CONTEXT-MENU's values."
  (if (or (null rect)
          (destructuring-bind (x0 y0 x1 y1) rect
            (not (and (<= x0 x x1) (<= y0 y y1)))))
      :outside
      (lol.widgets:hit-row hits y)))

;;; ---- the halo (middle button) ---------------------------------------------
;;; Smalltalk's halo: middle-click a thing and a ring of HANDLES appears round it,
;;; each one a thing you can do to THAT object. In Morphic the thing is always a
;;; morph — an on-screen object that draws itself — and pressing the middle button
;;; again walks out to the morph that CONTAINS it. Ours does the same over LAYERS:
;;;
;;;     the word under the pointer       the symbol it names         Source, Senders
;;;     the row / the sprite             what it opens / stands for  Open, Inspect
;;;     the text view, the list          a TEXTVIEW / VIEW instance  Inspect, Code
;;;     the pane                         a PANE instance (a morph)   Inspect, Code
;;;     what the pane shows              its subject                 Open its view
;;;     the trail, the shell / window    the morphs that hold it     Inspect, Code
;;;
;;; The first two are what the pointer is ON (PANE-TARGET). The rest are the morph
;;; tree itself (HALO-LAYERS, poc/morphic.lisp): the morph under the point and each
;;; morph that holds it, each offering itself and what it draws — so any pixel can
;;; answer "which instance is this, and which code painted it", because every thing on
;;; screen is an object that draws itself. Inspect opens the INSTANCE, fields settable
;;; in place (C-c C-c) and evaluable inside (C-c C-e); Code opens the drawing
;;; function's definition, which C-c C-c recompiles for everyone.
;;;
;;; What the halo is about must never be a guess, so the chosen layer is shown, not
;;; described: it stays bright and outlined while everything else dims, and a caption
;;; names every layer under the pointer with the current one lit.

(defparameter +halo-radius+ 74 "Distance of the handles from the clicked point.")

(defun halo-layer (name title object rect role &optional drawers)
  "One layer the halo can be about. ROLE is :THING (what the pointer is on), :ELEMENT
   (an on-screen instance, with the DRAWERS that paint it) or :SUBJECT (what a pane
   shows). RECT is (x y w h), what the halo outlines."
  (list :name name :title title :object object :rect rect :role role :drawers drawers))

(defun target-layers (target)
  "The innermost layers: the thing the pointer is ON, before what contains it."
  (case (getf target :kind)
    (:text
     (let ((tv (getf target :tv)) (tok (getf target :token)) (sym (getf target :symbol)))
       (when (and tok (getf target :start))
         (let* ((lh (lol.textview:tv-line-height tv))
                (x0 (lol.textview::x-of-col tv (getf target :start)))
                (x1 (lol.textview::x-of-col tv (getf target :end)))
                (y (+ (lol.textview::tv-y tv) (lol.textview::tv-pad tv)
                      (* (- (lol.textview::line-row tv (getf target :line)) (lol.textview::tv-scroll tv)) lh))))
           (list (halo-layer (clip-text tok 20)
                             (if sym (format nil "the symbol ~s" sym) (format nil "the word ~s" tok))
                             (or sym tok) (list (- x0 2) (- y 1) (+ (- x1 x0) 4) (+ lh 1))
                             :thing))))))
    (:row
     (let* ((pane (getf target :pane)) (item (getf target :item))
            (rec (pane-body-rect pane))
            (hit (find (getf target :index) (pane-rows pane) :key #'cdr))
            (rect (if (and rec hit)
                      (list (first rec) (car hit) (third rec) +row-h+)
                      rec))
            (label (clip-text (item-label item) 20)))
       (remove nil
               (list (when (getf target :subject)
                       (halo-layer label (format nil "what the row opens: ~a" (item-label item))
                                   (getf target :subject) rect :thing))
                     (halo-layer "row" (format nil "the row ~s itself" (item-label item))
                                 item rect :element '(item->row lol.widgets:draw-list))))))
    (:sprite
     (list (halo-layer (object-title (getf target :subject))
                       "what the sprite stands for" (getf target :subject)
                       (sprite-rect (getf target :pane) (getf target :body)) :thing)))
    (t nil)))

(defun sprite-rect (pane b)
  "The square body B of PANE's stage occupies (where PANE drew it), or NIL."
  (let ((vp (pane-stage-rect pane)))
    (when vp
      (let ((r (body-r b)))
        (list (- (+ (first vp) (body-x b)) r) (- (+ (second vp) (body-y b)) r) (* 2 r) (* 2 r))))))

(defun halo-entries (layer)
  "The handles for LAYER. An element offers its instance and the code that draws it;
   a thing offers its definition and its uses; what a pane shows can be opened."
  (let* ((object (getf layer :object))
         (role (getf layer :role))
         (src (source-subject object))
         (g "halo")
         (inspect (unless (stringp object)    ; a word that names nothing: only Copy
                    (menu-entry "Inspect" (lambda () (trail-push (inspect-subject object)))
                                :group g :tone :variable)))
         (class-of-it (when src
                        (menu-entry (if (or (symbolp object) (subj-defn-p object)
                                            (functionp object))
                                        "Source"
                                        (format nil "Class: ~(~a~)" (type-of object)))
                                    (lambda () (trail-push src)) :group g :tone :class)))
         (base
           (remove nil
                   (if (eq role :element)
                       ;; an element: the instance first, then the code that paints it
                       (append (list inspect)
                               (loop for d in (getf layer :drawers)
                                     when (jumpable-p d)
                                       collect (let ((d d))
                                                 (menu-entry (format nil "Code: ~(~a~)" d)
                                                             (lambda () (trail-push (subj-defn d)))
                                                             :group g :tone :function)))
                               (list class-of-it))
                       ;; a thing: where it goes, where it came from, who uses it
                       (list (when (and (not (symbolp object)) (not (stringp object)))
                               (menu-entry (if (eq role :subject) "Open its view" "Open")
                                           (lambda () (trail-push object))
                                           :group g :tone :package))
                             class-of-it
                             (when (and (symbolp object) object (fboundp object))
                               (menu-entry "Senders" (lambda () (trail-push (subj-xref :senders object)))
                                           :group g :tone :generic-function))
                             (when (and (symbolp object) object (find-class object nil))
                               (menu-entry "Class" (lambda () (trail-push (find-class object)))
                                           :group g :tone :class))
                             inspect))))
         (own (if (member role '(:element)) '()
                  (and (not (stringp object)) (command-entries object :halo g)))))
    (append (and (typep object 'lol.morph:morph)  ; a morph's own handles first (§12.5):
                 (mapcar (lambda (a) (menu-entry (getf a :label) (getf a :action)
                                                 :group "halo" :tone :restart))
                         (lol.morph:morph-affordances object)))   ; a window's Move, Resize …
            base
            (remove-if (lambda (e) (find (getf e :label) base :key (lambda (b) (getf b :label))
                                                              :test #'string-equal))
                       own)
            (list (menu-entry "Copy" (lambda () (copy-to-clipboard
                                                 (if (stringp object) object
                                                     (let ((*print-length* 20) (*print-level* 4))
                                                       (prin1-to-string object)))))
                              :group g :tone :macro)))))


(defun halo-handle-xy (i n cx cy)
  "Where handle I of N sits around CX,CY: evenly round a circle, the first at the top."
  (let ((th (- (/ (* 2 pi i) n) (/ pi 2))))
    (values (round (+ cx (* +halo-radius+ (cos th))))
            (round (+ cy (* +halo-radius+ (sin th)))))))

(defun clamp-rect (rect w h)
  "RECT (x y w h) cut to the screen W x H, or NIL when nothing is left."
  (destructuring-bind (x y rw rh) rect
    (let ((x0 (max 0 x)) (y0 (max 0 y)) (x1 (min w (+ x rw))) (y1 (min h (+ y rh))))
      (and (< x0 x1) (< y0 y1) (list x0 y0 (- x1 x0) (- y1 y0))))))

(defun spotlight (canvas rect factor)
  "Dim the whole canvas to FACTOR/256 EXCEPT RECT, which keeps its pixels: the halo's
   way of showing which element it is about. The canvas has no alpha, so the rect's
   rows are copied out, the scrim runs, and they are put back."
  (let* ((px (lol.canvas:canvas-pixels canvas)) (cw (lol.canvas:canvas-w canvas))
         (rows (when rect
                 (destructuring-bind (x y rw rh) rect
                   (loop for r from y below (+ y rh)
                         collect (subseq px (+ (* r cw) x) (+ (* r cw) x rw)))))))
    (lol.widgets:scrim canvas factor)
    (when rect
      (destructuring-bind (x y rw rh) rect
        (declare (ignore rw rh))
        (loop for row in rows for r from y
              do (replace px row :start1 (+ (* r cw) x)))))))

(defun draw-halo-caption (canvas w h layers depth top)
  "The caption card: every layer under the pointer, innermost first, the current one
   lit; what the current one IS; and the keys that walk the layers."
  (let* ((layer (nth depth layers))
         (sep " > ")
         (names (mapcar (lambda (l) (getf l :name)) layers))
         (crumb-w (+ (loop for n in names sum (lol.canvas:string-px *bfont* n))
                     (* (1- (length names)) (lol.canvas:string-px *bfont* sep))))
         (what (clip-text (format nil "~a  -  ~a" (getf layer :title)
                                  (object-title (getf layer :object)))
                          110))
         (hint "Up/Tab/middle: the thing around it   Down: back in   Left/Right: handle   1-9   Esc")
         (cw (min (- w 16) (+ 24 (max crumb-w (lol.canvas:string-px *bfont* what)
                                      (lol.canvas:string-px *bfont* hint)))))
         (step (+ (line-h) 3))                  ; one caption line: 18 at 6x15
         (ch (+ 8 (* 3 step)))                  ; three of them: 62 at 6x15
         (x (max 8 (floor (- w cw) 2)))
         (y (if (eq top :top) (+ +bar-h+ 6) (- h (status-h) ch 8))))
    (lol.canvas:fill-rect canvas x y cw ch *ui-panel*)
    (lol.canvas:draw-rect canvas x y cw ch *ui-rule*)
    (let ((pen (+ x 12)))
      (loop for n in names for i from 0
            do (when (plusp i)
                 (setf pen (lol.canvas:draw-string canvas *bfont* sep pen (+ y 8) *ui-rule*)))
               (when (= i depth)
                 (lol.canvas:fill-rect canvas (- pen 2) (+ y 6) (+ (lol.canvas:string-px *bfont* n) 4)
                                       (1+ (line-h)) *ui-hi*))
               (setf pen (lol.canvas:draw-string canvas *bfont* n pen (+ y 8)
                                                 (if (= i depth) *ui-accent* *ui-dim*)))))
    (lol.canvas:draw-string canvas *bfont* what (+ x 12) (+ y 8 step) *ui-text*)
    (lol.canvas:draw-string canvas *bfont* hint (+ x 12) (+ y 8 step step) *ui-dim*)))

(defun draw-halo (canvas w h layers depth entries sel cx cy)
  "The browser with layer DEPTH of LAYERS spotlit — bright and outlined in the accent
   while the rest dims — its handles hung off a small element's outline, or ringed
   round CX,CY on a large one (a dot per handle, coloured by what it opens: code, an
   instance, a view), and the caption naming the layers. Returns the handles' hit
   map, ((index cx cy x0 x1) …), for the chooser overlay to route clicks by."
  (draw-backdrop canvas w h)
  (let* ((layer (nth depth layers))
         (handles '())
         (rect (and (getf layer :rect) (clamp-rect (getf layer :rect) w h)))
         (n (length entries))
         (cx (max (+ +halo-radius+ 90) (min cx (- w +halo-radius+ 90))))
         (cy (max (+ +bar-h+ +halo-radius+ 14) (min cy (- h +halo-radius+ 30)))))
    (spotlight canvas rect 110)
    (when rect
      (destructuring-bind (x y rw rh) rect
        (lol.canvas:draw-rect canvas x y rw rh *ui-accent*)
        (lol.canvas:draw-rect canvas (1+ x) (1+ y) (max 1 (- rw 2)) (max 1 (- rh 2)) *ui-accent*)))
    (draw-halo-caption canvas w h layers depth (if (> cy (floor h 2)) :top :bottom))
    (flet ((handle (i e hx hy lx)
             ;; one handle: a dot coloured by what it opens, its numbered label at LX
             (let* ((label (format nil "~d ~a" (1+ i) (getf e :label)))
                    (lw (lol.canvas:string-px *bfont* label))
                    (on (= i sel)))
               (lol.canvas:draw-circle canvas hx hy 8 (concept-color (or (getf e :tone) :restart))
                                       :fill t)
               (when on (lol.canvas:draw-circle canvas hx hy 11 *ui-accent*))
               ;; the label's box is the text's cell plus a pixel each side (16 at 6x15)
               (lol.canvas:fill-rect canvas (- lx 3) (- hy (ceiling (1+ (line-h)) 2)) (+ lw 6)
                                     (1+ (line-h)) (if on *ui-hi* *ui-panel*))
               (lol.canvas:draw-string canvas *bfont* label lx (- hy (floor (line-h) 2) -1)
                                       (if on *ui-accent* *ui-text*))
               (push (list i hx hy (- (min lx hx) 12) (+ (max (+ lx lw) hx) 12)) handles)))
           (label-w (i e) (lol.canvas:string-px *bfont* (format nil "~d ~a" (1+ i) (getf e :label)))))
      (if (and rect (<= (fourth rect) 60))
          ;; a SMALL element (a word, a row, a tab): the handles hang off its outline
          ;; as a strip, below it (above when it is near the bottom), so nothing covers
          ;; the thing the halo is about — Morphic puts them on the bounds, too
          (destructuring-bind (x y rw rh) rect
            (declare (ignore rw))
            (let* ((total (loop for e in entries for i from 0 sum (+ (label-w i e) 34)))
                   (hy (if (> (+ y rh 40) (- h (status-h) 70)) (- y 16) (+ y rh 16)))
                   (pen (max 8 (min x (- w total 8)))))
              (loop for e in entries for i from 0
                    do (handle i e (+ pen 8) hy (+ pen 20))
                       (incf pen (+ (label-w i e) 34)))))
          ;; a LARGE one: a ring round the point clicked, inside the outline
          (progn
            (lol.canvas:draw-circle canvas cx cy (- +halo-radius+ 22) *ui-rule*)
            (loop for e in entries for i from 0
                  do (multiple-value-bind (hx hy) (halo-handle-xy i n cx cy)
                       (handle i e hx hy
                               (if (< hx (- cx 8))         ; labels point away from the centre
                                   (- hx 12 (label-w i e))
                                   (+ hx 12))))))))
    handles))

(defun halo-handle-at (handles x y)
  "The halo handle index at X,Y (its dot or its label) among HANDLES (DRAW-HALO's
   value), :OUTSIDE elsewhere."
  (let ((hit (find-if (lambda (h)
                        (destructuring-bind (i hx hy x0 x1) h
                          (declare (ignore i hx))
                          (and (<= x0 x x1) (<= (abs (- y hy)) (max 10 (ceiling (line-h) 2))))))
                      handles)))
    (if hit (first hit) :outside)))

;;; ---- the gestures, backend-blind ------------------------------------------
;;; These decide WHAT to show; the chooser overlay (poc/overlays.lisp, RUN-CHOOSER)
;;; shows it and waits for the pick while the backend's loop turns. *CHOOSER* is the
;;; hook a backend installs, like *VALUE-PROMPT*.

(defvar *chooser* nil
  "A hook: (funcall *chooser* KIND ENTRIES X Y &optional LAYERS) shows a modal menu
   (:menu) or halo (:halo, about LAYERS — see HALO-LAYERS) and returns the chosen
   entry, or NIL. Installed by the framebuffer backend (RUN-CHOOSER); NIL elsewhere,
   and then the gestures say so.")

(defun choose-and-run (kind entries x y &optional layers)
  "Offer ENTRIES through *CHOOSER* and run the one picked."
  (cond
    ((null entries) nil)
    ((null *chooser*) (setf *browser-status* "no menus on this backend"))
    (t (let ((e (funcall *chooser* kind entries x y layers)))
         (when e
           (handler-case (funcall (getf e :action))
             (serious-condition (c)
               (setf *browser-status* (format nil "~a failed: ~a" (getf e :label) c)))))))))

(defun context-menu-at (target wx wy)
  "The right-button menu for TARGET (what a press landed on: PANE-TARGET's or the tab
   strip's answer, or NIL for nothing in particular), its card at the screen point
   WX,WY."
  (choose-and-run :menu (context-menu-entries target) wx wy))

(defun open-halo (wx wy)
  "The halo on whatever is under the screen point WX,WY (HALO-LAYERS walks the morph
   tree there: poc/morphic.lisp)."
  (let ((layers (halo-layers wx wy)))
    (if layers
        (choose-and-run :halo (halo-entries (first layers)) wx wy layers)
        (setf *browser-status* "nothing here to put a halo on"))))

(defun pane-middle-click (p x y wx wy &optional force-halo)
  "Middle button at X,Y in the expanded pane P (the screen point WX,WY). In EDITABLE
   text it is X's middle-click paste: the PRIMARY selection (whatever you last
   selected with the mouse) goes in at the pointer. Anywhere else — and in text when
   FORCE-HALO (SuperL held) or there is nothing selected to paste — it opens the HALO
   on the thing under the pointer.

   That is the disambiguation, and it follows what each place is FOR: text you can
   edit is where a paste can land, so the paste wins there; a row, a sprite, a word in
   read-only text cannot receive a paste, so there the button is free for the halo."
  (let ((target (pane-target p x y)))
    (cond
      ;; a terminal is text you can type into, so the paste wins there too
      ((and (terminal-pane-p p) (not force-halo) (>= y (pane-body-top p)))
       (funcall 'terminal-middle p))
      ((and (eq (getf target :kind) :text) (getf target :editable)
            (not force-halo) (primary-selection))
       (paste-primary-at target))
      (t (open-halo wx wy)))))

(defun open-modules ()
  "The module index — every definition by the file it came from. Focus its tab if one
   is open, else open one. (It used to be the environment's first tab; the environment
   now opens on the Workspace alone.)"
  (let ((i (position-if (lambda (trail) (typep (pane-subject (first trail)) 'subj-modules))
                        *trails*)))
    (if i (activate-trail i) (new-trail (subj-modules)))))

(defvar *leave-action* nil
  "How the environment was left: :POWER-OFF or :CONSOLE (the supervisor's menu), set
   by LEAVE-MENU. The backend reads it once it has given the screen back.")

(defun leave-menu ()
  "SuperL-q: ask how to leave — down to the supervisor's console menu (the REPL, the
   worker, the other roots), or shut down, saving the desk or not
   (doc/session-persistence.org §4). Returns :QUIT with *LEAVE-ACTION* set (:CONSOLE,
   :SAVE-POWER-OFF or :POWER-OFF), or NIL when the menu is dismissed. The console
   comes first so that a reflexive Return is the step you can come back from. On a
   diskless boot nothing can be kept, and the one shut-down entry says so instead of
   offering a save that would write to RAM. With no menus on this backend (the X11
   host) there is nothing to power off, so it simply quits."
  (if (null *chooser*)
      (progn (setf *leave-action* :console) :quit)
      (let* ((screen (and *world* (lol.morph:morph-bounds *world*)))
             (x (- (floor (or (third screen) 800) 2) 100))
             (y (- (floor (or (fourth screen) 600) 2) 40))
             (keep (and (fboundp 'persistence-available-p) (funcall 'persistence-available-p)))
             (e (funcall *chooser* :menu
                         (cons (menu-entry "Supervisor console" :console :key "1" :group "leave")
                               (if keep
                                   (list (menu-entry "Save and shut down" :save-power-off
                                                     :key "2" :group "leave")
                                         (menu-entry "Shut down without saving" :power-off
                                                     :key "3" :group "leave"))
                                   (list (menu-entry "Shut down (no disk - nothing is kept)"
                                                     :power-off :key "2" :group "leave"))))
                         x y)))
        (when (and e (or (not (member (getf e :action) '(:power-off :save-power-off)))
                         (unsaved-files-resolved-p x y)))
          (setf *leave-action* (getf e :action))
          :quit))))

(defun unsaved-files-resolved-p (x y)
  "Before shutting down: are there file panes with unsaved changes, and if so, what
   becomes of them? Asks once for all of them — save all, discard, or cancel — and
   returns true when it is fine to go on. A file you did not save used to vanish at
   power-off without a word (doc/session-persistence.org §1.3)."
  (let ((dirty (loop for trail in *trails*
                     append (loop for p in trail
                                  when (file-pane-dirty-p p) collect p
                                  when (and (pane-side p) (file-pane-dirty-p (pane-side p)))
                                    collect (pane-side p)))))
    (or (null dirty)
        (let ((e (funcall *chooser* :menu
                          (list (menu-entry (format nil "Save ~d modified file~:p" (length dirty))
                                            :save :key "1" :group "unsaved")
                                (menu-entry "Discard their changes" :discard :key "2" :group "unsaved")
                                (menu-entry "Cancel" :cancel :key "3" :group "unsaved"))
                          x y)))
          (case (getf e :action)
            (:discard t)
            (:save
             (dolist (p dirty) (ignore-errors (funcall 'save-file-pane p)))
             (let ((left (count-if #'file-pane-dirty-p dirty)))
               (or (zerop left)
                   (progn (setf *browser-status*
                                (format nil "~d file~:p could not be saved - not shutting down" left))
                          nil))))
            (t nil))))))

(defun keyboard-context-menu ()
  "The Menu key, or Shift-F10: the right-button menu without a pointer, anchored at
   the caret in a text pane or at the selected row in a list. The keyboard-first rule
   (doc/code-browser.org §1): anything the pointer can reach, a key can too."
  (let* ((p (current-pane))
         (tv (cond ((and (pane-editing-side p) (pane-side p)) (pane-tv (pane-side p)))
                   (t (pane-tv p)))))
    (multiple-value-bind (x y)
        (if tv
            (values (lol.textview::x-of-col tv (lol.textview:tv-point-col tv))
                    (+ (lol.textview::tv-y tv) (lol.textview::tv-pad tv) 4
                       (* (- (lol.textview::line-row tv (lol.textview:tv-point-line tv)) (lol.textview::tv-scroll tv))
                          (lol.textview:tv-line-height tv))))
            (let ((hit (find (pane-sel p) (pane-rows p) :key #'cdr)))
              (values 40 (if hit (+ (car hit) 4) (pane-body-top p)))))
      ;; X,Y are where P drew (a window's coordinates, inside one); the card goes
      ;; where that is on the screen
      (destructuring-bind (wx wy &rest r)
          (if (lol.morph:morph-owner p) (lol.morph:to-world-rect p (list x y 0 0)) (list x y))
        (declare (ignore r))
        (context-menu-at (pane-target p x y) wx wy)))))

;;; ---- drag and drop: a subject, carried by the hand ---------------------------
;;; Morphic's hand carries what you pick up and offers it to whatever is under it when
;;; you let go (doc/morphic.org §13). Here what is carried is a SUBJECT — a row's (a
;;; definition, a class, a file, an inspector field's value), a sprite's instance, the
;;; symbol at the caret — and each kind of morph says what a drop on it means
;;; (ACCEPT-DROP, poc/morphic.lisp): a text you can edit inserts a READABLE REFERENCE to
;;; it, the tab strip opens it as a new trail, a pane drills it there, an inspector's
;;; settable field is set to its VALUE. These two functions are what the subject is,
;;; for those two purposes; they agree, so that the text a drop inserts evaluates to the
;;; value a drop on a field would store.

(defun drop-value (subject)
  "What SUBJECT is as a VALUE to store -> (values VALUE T), or (values NIL NIL) when
   it is a browser's handle on a view (a module, a cross-reference …) rather than a
   value. A definition is its name (the symbol), a file its path, an inspected value
   itself."
  (cond
    ((subj-object-p subject) (values (subj-object-value subject) t))
    ((subj-defn-p subject) (values (subj-defn-name subject) t))
    ((subj-file-p subject) (values (subj-file-path subject) t))
    ((subj-dir-p subject) (values (subj-dir-path subject) t))
    ;; the other SUBJ-* structures are the browser's own handles on a view
    ((and (typep subject 'structure-object)
          (let ((n (symbol-name (type-of subject))))
            (and (> (length n) 5) (string= "SUBJ-" n :end2 5))))
     (values nil nil))
    (t (values subject t))))

(defun drop-reference (subject)
  "Text that reads back as SUBJECT's value (DROP-VALUE) — what a drop on a text you can
   edit inserts — or NIL when there is none: a symbol quoted, a class as (find-class
   'name), a named function as #'name, a package as (find-package \"NAME\"), a path as
   a string, a number or a string as itself; anything else only if it prints readably.
   Printed from CL-USER, so a symbol of another package comes out qualified."
  (multiple-value-bind (v ok) (drop-value subject)
    (when ok
      (let ((*package* (find-package :cl-user)) (*print-pretty* nil)
            (*print-length* nil) (*print-level* nil))
        (flet ((named (thing) (let ((n (ignore-errors (sb-kernel:%fun-name thing))))
                                (and n (or (symbolp n) (and (consp n) (eq (first n) 'setf))) n))))
          (typecase v
            ((or keyword (member t nil)) (prin1-to-string v))
            (symbol (format nil "'~s" v))
            (class (and (class-name v) (eq (find-class (class-name v) nil) v)
                        (format nil "(find-class '~s)" (class-name v))))
            (function (let ((n (named v)))
                        (and n (fboundp n) (eq (fdefinition n) v) (format nil "#'~s" n))))
            (package (format nil "(find-package ~s)" (package-name v)))
            ((or number string character pathname) (prin1-to-string v))
            ;; anything else only if it prints readably — and reads back without
            ;; #. (SBCL prints a hash table readably as #.(…), an eval at read time:
            ;; not a reference anyone wants dropped into their code)
            (t (ignore-errors
                 (let ((text (let ((*print-readably* t)) (prin1-to-string v))))
                   (and (let ((*read-eval* nil)) (read-from-string text) t) text))))))))))

(defun drop-label (subject)
  "A short name for SUBJECT, for the status line while it is carried."
  (multiple-value-bind (v ok) (drop-value subject)
    (clip-text (if ok (object-title v) (view-title (present subject))) 40)))

(defun drop-reference-at (tv text &optional x y)
  "Insert TEXT into the editable text view TV — at X,Y (where TV is drawn) when given,
   else at the caret — as one undoable edit, with a space either side where it would
   otherwise run into a word."
  (when (and x y) (lol.textview:tv-click tv x y))
  (let* ((line (aref (lol.textview:tv-lines tv) (lol.textview:tv-point-line tv)))
         (c (lol.textview:tv-point-col tv))
         (before (and (plusp c) (char line (1- c))))
         (after (and (< c (length line)) (char line c)))
         (text (format nil "~:[~; ~]~a~:[~; ~]"
                       (and before (not (member before '(#\Space #\( #\' #\` #\,))))
                       text
                       (and after (not (member after '(#\Space #\))))))))
    (with-undo (tv) (editor-insert tv text))
    t))

;;; ---- the clipboard, wired into the editor ---------------------------------
;;; poc/clipboard.lisp holds the state and does the text surgery — pure functions
;;; over a vector of lines. This is the part that knows about panes and keys: which
;;; text view a command acts on, what counts as a CONSECUTIVE kill (C-k C-k builds
;;; one entry), and where the last paste landed (so M-y can swap it). The key table
;;; and the reasons behind it: doc/clipboard.org.

(defun clip-status (verb text)
  "Say what just happened to the clipboard, briefly."
  (setf *browser-status*
        (if text
            (format nil "~a: ~a" verb (lol.clipboard:clip-label text 60))
            (format nil "~a: nothing (blank text is not kept)" verb))))

(defun copy-to-clipboard (string)
  "Put STRING in the clipboard history (blank strings are refused, and say so)."
  (clip-status "copied" (lol.clipboard:clip-copy string)))

(defun primary-selection () (lol.clipboard:primary))

(defun editor-region-text (tv)
  "The selected text of TV, or NIL."
  (multiple-value-bind (al ac pl pc) (lol.textview:tv-region tv)
    (and al (lol.clipboard:region-text (lol.textview:tv-lines tv) al ac pl pc))))

(defun editor-copy (tv)
  "M-w / Ctrl-Insert: the region into the history. The selection stays, so a
   copy is not also a deselect — the pointer user expects to still see it."
  (let ((text (editor-region-text tv)))
    (if text
        (clip-status "copied" (lol.clipboard:clip-copy text))
        (setf *browser-status* "copy: no selection"))))

(defun editor-cut (tv)
  "C-w / Shift-Delete: the region into the history, and out of the buffer."
  (multiple-value-bind (al ac pl pc) (lol.textview:tv-region tv)
    (if (not al)
        (setf *browser-status* "cut: no selection")
        (let ((text (lol.clipboard:region-text (lol.textview:tv-lines tv) al ac pl pc)))
          (multiple-value-bind (lines l c)
              (lol.clipboard:region-delete (lol.textview:tv-lines tv) al ac pl pc)
            (lol.textview:tv-replace tv lines l c))
          (clip-status "cut" (lol.clipboard:clip-copy text))))))

(defun editor-insert (tv text)
  "Insert TEXT at TV's caret — replacing the selection, as every editor since the
   Mac does — and remember the span for M-y."
  (multiple-value-bind (al ac pl pc) (lol.textview:tv-region tv)
    (when al
      (multiple-value-bind (lines l c)
          (lol.clipboard:region-delete (lol.textview:tv-lines tv) al ac pl pc)
        (lol.textview:tv-replace tv lines l c))))
  (let ((l0 (lol.textview:tv-point-line tv)) (c0 (lol.textview:tv-point-col tv)))
    (multiple-value-bind (lines l1 c1)
        (lol.clipboard:insert-text (lol.textview:tv-lines tv) l0 c0 text)
      (lol.textview:tv-replace tv lines l1 c1)
      (setf *last-edit* (list :yank tv l0 c0 l1 c1)))))

(defun editor-paste (tv)
  "C-y / Shift-Insert: the newest history entry, at the caret."
  (let ((text (lol.clipboard:clip-yank)))
    (if text
        (editor-insert tv text)
        (setf *browser-status* "paste: the clipboard is empty"))))

(defun editor-yank-pop (tv previous)
  "M-y straight after a paste: swap what it inserted for the entry before it, and
   again for the one before that. PREVIOUS is the last edit; anything but a paste in
   this same view and M-y says so instead (Emacs: 'Previous command was not a yank')."
  (if (not (and (eq (first previous) :yank) (eq (second previous) tv)))
      (setf *browser-status* "M-y: only straight after a paste (C-y)")
      (destructuring-bind (l0 c0 l1 c1) (cddr previous)
        (lol.clipboard:clip-yank-pop)
        (multiple-value-bind (lines l c)
            (lol.clipboard:region-delete (lol.textview:tv-lines tv) l0 c0 l1 c1)
          (lol.textview:tv-replace tv lines l c))
        (editor-insert tv (lol.clipboard:clip-yank))
        (setf *browser-status*
              (format nil "M-y: ~a" (lol.clipboard:clip-label (lol.clipboard:clip-yank) 60))))))

(defun editor-kill (tv l0 c0 l1 c1 previous &optional (where :after))
  "Kill the text between two positions: out of the buffer, into the history —
   APPENDED to the last entry when the previous command was a kill too (WHERE says
   which end: a backward kill goes on the front)."
  (let ((text (lol.clipboard:region-text (lol.textview:tv-lines tv) l0 c0 l1 c1)))
    (multiple-value-bind (lines l c)
        (lol.clipboard:region-delete (lol.textview:tv-lines tv) l0 c0 l1 c1)
      (lol.textview:tv-replace tv lines l c))
    (lol.clipboard:clip-copy text :append (and (eq (first previous) :kill) where))
    (setf *last-edit* (list :kill))
    (setf *browser-status* nil)))

(defun editor-kill-line (tv previous)
  "C-k: to the end of the line, or the newline itself at the end of one."
  (multiple-value-bind (l0 c0 l1 c1)
      (lol.clipboard:kill-line-span (lol.textview:tv-lines tv)
                                    (lol.textview:tv-point-line tv)
                                    (lol.textview:tv-point-col tv))
    (when l0 (editor-kill tv l0 c0 l1 c1 previous))))

(defun editor-kill-word (tv dir previous)
  "M-d (DIR :right) / M-Backspace (:left): kill to the word boundary the motion
   keys use. Found by MOVING there with the anchor left behind, then killing the
   region that motion made."
  (lol.textview:tv-move-point tv (lol.textview:tv-point-line tv) (lol.textview:tv-point-col tv))
  (lol.textview::move-word tv dir t)
  (multiple-value-bind (al ac pl pc) (lol.textview:tv-region tv)
    (when al
      (editor-kill tv al ac pl pc previous (if (eq dir :left) :before :after)))))

(defun editor-select-all (tv) (lol.textview:tv-select-all tv))

(defun editor-paste-primary (tv)
  "Paste the PRIMARY selection at the caret (the menu's 'Paste the selection')."
  (let ((text (primary-selection)))
    (if text (editor-insert tv text) (setf *browser-status* "nothing selected to paste"))))

(defun paste-primary-at (target)
  "X's middle click: put the caret where the pointer is, then paste PRIMARY there."
  (let ((tv (getf target :tv)))
    (with-undo (tv)
      (lol.textview:tv-move-point tv (getf target :line) (getf target :col))
      (editor-paste-primary tv))))

(defun browser-drag-end ()
  "The button came up after a press in text: if the drag selected something, that
   selection becomes PRIMARY and enters the history — once, now, rather than on
   every motion event, or the history would fill with every intermediate stretch."
  (let ((press (hand-press)))
    (case (press-kind press)
      (:web (funcall 'web-release))                    ; a press in a web page
      (:terminal (funcall 'terminal-release))
      (:text
       (let ((text (editor-region-text (getf (rest press) :tv))))
         (when (and text (null (getf (rest press) :origin)))   ; it really was a drag
           (lol.clipboard:clip-select text)
           (clip-status "selected" text)))))))

;;; Rectangles: the region's two corners are the rectangle's (C-x SPC shows it as
;;; one, but any region works, as in Emacs).

(defun editor-rectangle (tv key)
  "C-x r KEY. k kill, M-w copy, y yank, d delete, o open, c clear, t string."
  (setf *browser-status* nil)                  ; the rectangle-mark hint has done its job
  (let ((lines (lol.textview:tv-lines tv)))
    (multiple-value-bind (al ac pl pc) (lol.textview:tv-region tv)
      (flet ((corners () (values al ac pl pc))
             (top-left () (values (min al pl) (min ac pc))))
        (cond
          ((eql key #\y)
           (let ((rect (lol.clipboard:clip-rectangle)))
             (if (null rect)
                 (setf *browser-status* "C-x r y: no rectangle killed yet")
                 (multiple-value-bind (new l c)
                     (lol.clipboard:rect-yank lines rect (lol.textview:tv-point-line tv)
                                              (lol.textview:tv-point-col tv))
                   (lol.textview:tv-replace tv new l c)))))
          ((null al) (setf *browser-status* "C-x r: no region — C-x SPC, then move"))
          (t
           (multiple-value-bind (tl tc) (top-left)
             (multiple-value-bind (a b c d) (corners)
               (case key
                 (#\k (multiple-value-bind (new rect) (lol.clipboard:rect-kill lines a b c d)
                        (lol.clipboard:clip-set-rectangle rect)
                        (lol.textview:tv-replace tv new tl tc)))
                 (#\w (lol.clipboard:clip-set-rectangle (lol.clipboard:rect-extract lines a b c d))
                      (setf *browser-status* "rectangle copied"))
                 (#\d (lol.textview:tv-replace tv (lol.clipboard:rect-delete lines a b c d) tl tc))
                 (#\o (lol.textview:tv-replace tv (lol.clipboard:rect-open lines a b c d) tl tc))
                 (#\c (lol.textview:tv-replace tv (lol.clipboard:rect-clear lines a b c d) tl tc))
                 (#\t (let ((s (and *value-prompt* (funcall *value-prompt* "string rectangle"))))
                        (when s
                          (lol.textview:tv-replace
                           tv (lol.clipboard:rect-string lines a b c d s) tl tc))))
                 (t (setf *browser-status* "C-x r: k M-w y d o c t")))))))))))

(defun clipboard-key (tv key state previous)
  "The clipboard's keys in an editable text view; T when KEY was one of them.
   Emacs and PC conventions side by side (doc/clipboard.org has the table)."
  (let ((ctrl (lol.canvas:ctrl-p state))
        (meta (lol.canvas:meta-p state))
        (shift (lol.canvas:shift-p state)))
    (cond
      ((and ctrl (member key '(#\w #\W)))          (editor-cut tv) t)
      ((and meta (member key '(#\w #\W)))          (editor-copy tv) t)
      ((and ctrl (member key '(#\y #\Y)))          (editor-paste tv) t)
      ((and meta (member key '(#\y #\Y)))          (editor-yank-pop tv previous) t)
      ((and ctrl (member key '(#\k #\K)))          (editor-kill-line tv previous) t)
      ((and meta (member key '(#\d #\D)))          (editor-kill-word tv :right previous) t)
      ((and meta (eq key :backspace))              (editor-kill-word tv :left previous) t)
      ((and shift (eq key :insert))                (editor-paste tv) t)
      ((and ctrl (eq key :insert))                 (editor-copy tv) t)
      ((and shift (eq key :delete))                (editor-cut tv) t)
      (t nil))))

(defun focused-editor ()
  "The editable text view keyboard focus is in, or NIL — where a paste goes."
  (let* ((p (current-pane))
         (q (if (and (pane-editing-side p) (pane-side p)) (pane-side p) p)))
    (and (pane-tv q) (editor-pane-p q) (pane-tv q))))

;;; ---- the clipboard manager (SuperL-h): Parcellite's history, as a palette -----

(defun draw-clipboard-card (canvas w h query results sel)
  "The browser, dimmed, and the history as a card: the filter line, one row per
   entry (newest first, numbered by its place in the history), the current one
   marked. RESULTS is (INDEX . STRING) pairs from CLIP-FILTER. Returns (values RECT
   HITS), the card (x0 y0 x1 y1) and its rows' hit map, for its overlay's clicks."
  (draw-backdrop canvas w h)
  (let* ((cw (min (+ 40 (* 120 (char-w))) (- w 80)))      ; 760 at 6x15
         (cx (floor (- w cw) 2))
         (cy (+ +bar-h+ 14))
         (chrome (+ (lol.widgets:card-head-h) +bar-h+ 8 (status-h) 2))  ; as the Spotter's
         (maxrows (max 3 (min (max 1 (length results)) (floor (- h cy chrome 20) +row-h+))))
         (ch (+ chrome (* maxrows +row-h+)))
         (maxc (max 10 (floor (- cw 120) (lol.canvas:font-cw *bfont*))))
         (rows (map 'vector
                    (lambda (r)
                      (lol.widgets:make-row (lol.clipboard:clip-label (cdr r) maxc)
                                            :detail (if (zerop (car r)) "current"
                                                        (format nil "#~d" (car r)))
                                            :tint *ui-text*
                                            :stripe (if (zerop (car r)) *ui-accent* nil)))
                    results)))
    (let* ((rect (list cx cy (+ cx cw) (+ cy ch)))
           (hits '())
           (body (lol.widgets:draw-card canvas cx cy cw ch
                                        :title "Clipboard history"
                                        :right (format nil "~d kept" (lol.clipboard:clip-count))
                                        :dim 105))
           (qy (lol.widgets:draw-query-line canvas (+ cx 8) (+ body 2) (- cw 16) query))
           (ly (+ qy 8)))
      (if (zerop (length rows))
          (lol.canvas:draw-string canvas *bfont*
                                  (if (plusp (lol.clipboard:clip-count))
                                      "nothing matches — Backspace to widen"
                                      "empty — copy, cut, kill or select something first")
                                  (+ cx 16) (+ ly 4) *ui-dim*)
          (multiple-value-bind (top row-hits)
              (lol.widgets:draw-list canvas (+ cx 6) ly (- cw 12) (* maxrows +row-h+) rows
                                     :sel sel :top 0 :follow-sel t)
            (declare (ignore top))
            (setf hits row-hits)))
      (draw-hints canvas (+ cx 12) (- (+ cy ch) (line-h) 1) (- cw 16)
                  '(("up/dn" . "move") ("ret" . "make current + paste")
                    ("Del" . "forget it") ("type" . "filter") ("Esc" . "close")))
      (values rect hits))))

(defun clipboard-row-at (rect hits x y)
  "The manager's row index at X,Y, :OUTSIDE off the card RECT, or NIL (RECT, HITS:
   DRAW-CLIPBOARD-CARD's values)."
  (if (or (null rect)
          (destructuring-bind (x0 y0 x1 y1) rect
            (not (and (<= x0 x x1) (<= y0 y y1)))))
      :outside
      (lol.widgets:hit-row hits y)))

(defun clipboard-choose (index)
  "The manager's Enter: entry INDEX becomes current, and goes in at the caret of
   the focused editor if there is one (Parcellite pastes on selection too)."
  (lol.clipboard:clip-promote index)
  (let ((tv (focused-editor)))
    (if tv
        (editor-insert tv (lol.clipboard:clip-current))
        (clip-status "current" (lol.clipboard:clip-current)))))

(defun open-clipboard-manager ()
  "SuperL-h, or the tray's clipboard icon: the history as an overlay that waits for
   a pick (poc/overlays.lisp); the pick is pasted HERE, in the caller's context — the
   editor that had the caret, a window's included."
  (let ((i (and (fboundp 'run-clipboard-manager) (funcall 'run-clipboard-manager))))
    (cond ((integerp i) (clipboard-choose i))
          ((not (and (fboundp 'run-clipboard-manager) (boundp '*world-cycle*)
                     (symbol-value '*world-cycle*)))
           (setf *browser-status* "the clipboard manager needs a backend with a loop (the framebuffer)")))))
