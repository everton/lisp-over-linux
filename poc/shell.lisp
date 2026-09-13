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
            lol.widgets:+row-h+ lol.widgets:+bar-h+ lol.widgets:clip-text)))

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

(defstruct pane
  subject view
  (sel 0)                 ; selected item index (list views)
  (top 0)                 ; first visible LAYOUT SLOT (section headers occupy one too)
  (follow t)              ; should the viewport chase the selection on the next draw?
                          ; Arrow keys say yes (move the selection, bring it into view);
                          ; the wheel says no, because scrolling is LOOKING — the whole
                          ; point of a viewport independent of the selection.
  tv                      ; a lol.textview for :source/:reference panes, else NIL
  side)                   ; a pinned, EDITABLE pane shown BESIDE this list (⇧↵), or NIL

(defun open-pane (subject)
  "Build a PANE for SUBJECT: present it, and for text views wrap the body in a
   textview (reusing its lexer + layout). A :SOURCE view (your code) is EDITABLE —
   it gets focus and Accept; a :REFERENCE view (a builtin) is read-only."
  (let ((v (present subject)))
    (make-pane
     :subject subject :view v
     :sel (max 0 (min (view-sel v) (max 0 (1- (length (view-items v))))))  ; view's preferred focus
     :tv (when (and (member (view-kind v) '(:source :reference :workspace)) (view-text v))
           (let ((tv (lol.textview:make-textview
                      :font *bfont*
                      :focus (member (view-kind v) '(:source :workspace)))))
             (lol.textview:tv-set-text tv (view-text v))
             tv)))))

(defun editable-pane-p (pane)
  "True when PANE is your-own-code source — editable, with Accept."
  (and (pane-tv pane) (eq (view-kind (pane-view pane)) :source)))

(defun workspace-pane-p (pane)
  "True when PANE is the Workspace scratch buffer — editable, with eval verbs."
  (and (pane-tv pane) (eq (view-kind (pane-view pane)) :workspace)))

(defun current-trail () "The focused trail (its list of panes)." (nth *active* *trails*))
(defun set-current-trail (panes) "Replace the focused trail's panes." (setf (nth *active* *trails*) panes))
(defun current-pane () "The deepest pane of the focused trail." (car (last (current-trail))))

;; --- within the active trail (drill-down accordion, unchanged semantics) ---
(defun trail-root (subject)
  "Start the environment at SUBJECT — one trail, made active. Additional trails come
   from NEW-TRAIL (a spotter hit, SuperL-drill)."
  (setf *trails* (list (list (open-pane subject))) *active* 0))
(defun trail-push (subject) "Drill into SUBJECT in the active trail."
  (set-current-trail (append (current-trail) (list (open-pane subject)))))
(defun trail-pop () "Pop the active trail's deepest pane (back)."
  (when (cdr (current-trail)) (set-current-trail (butlast (current-trail)))))
(defun trail-goto (n) "Collapse the active trail back to level N (a breadcrumb click)."
  (when (< n (length (current-trail))) (set-current-trail (subseq (current-trail) 0 (1+ n)))))

;; --- across trails (the tab layer) ---
(defun new-trail (subject)
  "Open SUBJECT in a NEW trail and focus it — a spotter hit / SuperL-drill."
  (setf *trails* (append *trails* (list (list (open-pane subject))))
        *active* (1- (length *trails*))))
(defun close-trail ()
  "Close the focused trail (never the last one); focus the neighbour that slides in."
  (when (cdr *trails*)
    (setf *trails* (append (subseq *trails* 0 *active*) (subseq *trails* (1+ *active*)))
          *active* (min *active* (1- (length *trails*))))))
(defun activate-trail (n) "Focus trail N if it exists." (when (< -1 n (length *trails*)) (setf *active* n)))
(defun cycle-trail (dir) "Focus the next (+1) / previous (-1) trail, wrapping."
  (setf *active* (mod (+ *active* dir) (length *trails*))))
(defun trail-title (trail)
  "The tab label for TRAIL — the ROOT pane's view title (what the tab shows)."
  (view-title (pane-view (first trail))))

(defun open-environment (root)
  "Seed the cohesive environment (§4¾): ROOT as the first, focused tab, PLUS a
   Workspace tab so the editor, the Workspace, and the Spotter all live together —
   unless ROOT is itself the Workspace (no duplicate). Everything else is a
   Super-Space / Super-drill away."
  (trail-root root)
  (unless (typep root 'subj-workspace) (new-trail (subj-workspace)))
  (setf *active* 0))

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
(defvar *crumb-rects* '() "alist (pane-index . y-top) for the collapsed bars, set on draw.")
(defvar *content-top* 0  "screen-y where the expanded pane's body starts, set on draw.")
(defvar *split-x* nil    "left screen-x of the nav-split preview column when a list pane is
   split, else NIL. Clicks at x >= this land in the read-only preview, not the list.")
(defvar *preview-pane* nil    "cached read-only pane rendered in the nav-split right column.")
(defvar *preview-subject* nil "the subject *PREVIEW-PANE* was built for (EQ-compared).")
(defvar *tab-rects* '()  "alist (trail-index . (x0 . x1)) for the top tab strip, set on draw.")
(defvar *pending-prefix* nil
  "The pending chord prefix in an editable pane: NIL, :c-c (after C-c) or :c-x
   (after C-x). The next key completes the chord — C-c C-c Accept, C-x C-e Do it,
   C-c C-p Print it, C-c C-i Inspect it.")
(defvar *pending-meta* nil
  "T after a lone Esc — the Emacs Meta prefix (§4¾). The next key is Meta-modified,
   so Esc . jumps to definition just like Alt-. / M-. would. Sticky for one key,
   cleared on the next keystroke.")
(defvar *browser-status* nil "A transient status message (e.g. Accept result), or NIL.")
(defvar *editing-side* nil
  "T when keyboard focus is on the current pane's editable SIDE pane (⇧↵ beside) rather
   than the list. SuperL-o toggles it; guarded everywhere by (pane-side (current-pane)).")

(defun open-beside (pane)
  "⇧↵ on a list row: open the selected item BESIDE the list — a pinned, editable pane in
   the right column that does NOT follow the selection (unlike the read-only preview). A
   second ⇧↵ on the same item closes it. Focus moves to the side so you can edit at once."
  (let ((it (selected-item pane)))
    (when (and it (item-subject it))
      (if (and (pane-side pane)
               (eq (pane-subject (pane-side pane)) (item-subject it)))
          (setf (pane-side pane) nil *editing-side* nil)      ; toggle closed
          (setf (pane-side pane) (open-pane (item-subject it)) *editing-side* t)))))

(defun toggle-side-focus ()
  "SuperL-o: move keyboard focus between the list and its editable side pane."
  (when (pane-side (current-pane))
    (setf *editing-side* (not *editing-side*))))

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

(defun form-before-point (tv)
  "Read successive forms from the buffer start up to the caret and return the LAST
   complete one — the sexp before point, Emacs's C-x C-e target. Comments are
   skipped by the reader; an incomplete trailing form is ignored. NIL if none."
  (let ((text (buffer-text-to-point tv)) (last nil) (pos 0))
    (loop
      (multiple-value-bind (form next)
          (ignore-errors (read-from-string text nil :eof :start pos))
        (when (or (null form) (eq form :eof)) (return))
        (setf last form pos next)))
    last))

(defun ws-eval (form)
  "Eval FORM; return (values OK RESULT-or-CONDITION OUTPUT). OUTPUT is whatever the
   form printed — *standard-output* / *trace-output* are bound to a string stream so
   a =(format t …)= in the Workspace is captured (the notebook transcript) instead of
   vanishing behind the framebuffer. Errors are caught, never thrown — a bad form
   must not take down the browser (nor PID 1); output printed before it still returns."
  (let ((out (make-string-output-stream)))
    (handler-case
        (let ((v (let ((*standard-output* out) (*trace-output* out)) (eval form))))
          (values t v (get-output-stream-string out)))
      (serious-condition (c) (values nil c (get-output-stream-string out))))))

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

(defun workspace-do-it (pane)
  "Eval the form before point; show the value (or error), and note any printed
   output, in the status line."
  (let ((form (form-before-point (pane-tv pane))))
    (if (null form)
        (setf *browser-status* "Do it: no form before point")
        (multiple-value-bind (ok res out) (ws-eval form)
          (let ((printed (ws-trim-output out)))
            (setf *browser-status*
                  (if ok (let ((*print-length* 40) (*print-level* 5))
                           (format nil "~@[~a  ~]=> ~s"
                                   (and printed (substitute #\Space #\Newline printed)) res))
                      (format nil "~@[~a  ~]error: ~a"
                              (and printed (substitute #\Space #\Newline printed)) res))))))))

(defun workspace-print-it (pane)
  "Eval the form before point and weave the transcript into the buffer, inline after
   the form — the notebook that accretes an editable, re-runnable session. Anything
   the form printed is woven first (verbatim), then the value; so is an error."
  (let ((form (form-before-point (pane-tv pane))))
    (when form
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
  (let ((form (form-before-point (pane-tv pane))))
    (when form
      (multiple-value-bind (ok res) (ws-eval form)
        (if ok (trail-push res)
            (setf *browser-status* (format nil "error: ~a" res)))))))

(defun workspace-debug-it (pane)
  "Eval the form before point with the debugger ARMED. We catch with HANDLER-BIND,
   not the outer handler-case, precisely because a handler-bind handler runs BEFORE
   the unwind — with the erroring stack still standing — so run-debugger-fb sees the
   live restarts and backtrace, and invoking a restart from there actually transfers
   control. An ABORT restart wraps the eval, guaranteeing a way back to the
   workspace. (run-debugger-fb lives in fb-browser.lisp; funcall avoids a load-order
   forward reference.)"
  (let ((form (form-before-point (pane-tv pane))))
    (when form
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

(defvar *list-rows* '()
  "The hit map LOL.WIDGETS:DRAW-LIST returned for the pane drawn last — ((y . index) …).
   A click reads it back rather than computing an index from y, because with section
   headers in the layout screen row N is no longer item N.")

(defun draw-list-body (canvas pane x y w h)
  "Render a list view: the widget does rows, sections, selection and the scrollbar;
   this decides only WHAT the rows say. PANE-TOP is the widget's viewport position
   (a layout slot, headers included) and is written back so it persists across frames."
  (let ((rows (map 'vector #'item->row (view-items (pane-view pane)))))
    (multiple-value-bind (top hits)
        (lol.widgets:draw-list canvas x y w h rows
                               :sel (pane-sel pane) :top (pane-top pane)
                               :follow-sel (pane-follow pane)
                               ;; the inspector is a name/value TABLE, not a list with
                               ;; annotations: the value is what you came to read, so it
                               ;; gets its own column and full-strength text
                               :value-col (when (eq (view-kind (pane-view pane)) :inspector)
                                            (min 300 (floor w 2))))
      (setf (pane-top pane) top
            *list-rows* hits))))

;;; ---- the clickable dispatch matrix (§3) ----------------------------------

(defvar *matrix-cells* '()
  "The hit map LOL.WIDGETS:DRAW-GRID returned for the dispatch grid drawn last, so a
   click and the drawing can never disagree (same discipline as *crumb-rects*).")

(defun draw-matrix-body (canvas pane x y w h)
  "Render a 2-axis dispatch matrix as a clickable grid. The widget draws the cells,
   the header band and the column overflow; this turns the model's cell CONTENTS
   (lists of methods) into the counts the grid shows — a dim mark for a real gap,
   which is the whole point of the view."
  (let* ((m (view-data (pane-view pane)))
         (rows (getf m :rows))
         (cols (getf m :cols))
         (counts (mapcar (lambda (cellrow) (mapcar #'length cellrow)) (getf m :cells))))
    (setf *matrix-cells*
          (lol.widgets:draw-grid canvas x y w h rows cols counts))))

(defun matrix-cell-click (pane px py)
  "A click on a dispatch-grid cell -> drill the effective-method onion for that
   (row-class, col-class) pair, building the full argument tuple (T elsewhere) from
   the matrix's arity and axis positions. NIL when the click missed every cell."
  (let ((hit (lol.widgets:hit-cell *matrix-cells* px py)))
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
  (case (view-kind view)
    (:workspace *ui-amber*)
    ((:source :reference) (concept-color :function))
    ((:matrix :generic) (concept-color :generic-function))
    (:class (concept-color :class))
    (:inspector (concept-color :variable))
    (t (concept-color :package))))     ; a list root is usually a package

(defun trail-kind-color (trail)
  "The semantic tint for TRAIL's tab — from its ROOT view (see VIEW-KIND-COLOR)."
  (view-kind-color (pane-view (first trail))))

(defvar *tab-close-rect* nil "x-range (x0 . x1) of the active tab's × close hit-box, or NIL.")
(defvar *tab-plus-rect* nil  "x-range (x0 . x1) of the + new-tab hit-box, or NIL.")

(defun draw-tabs (canvas w)
  "The top tab strip: one tab per open trail, each with a kind-coloured stripe and its
   SuperL-number badge; the active one is highlighted (brighter bg + accent top-rule +
   an × close), and a + after the last tab opens a fresh Workspace. Records *tab-rects*
   / *tab-close-rect* / *tab-plus-rect* so clicks and drawing can't disagree; the app
   name sits at the right when there's room."
  (setf *tab-rects* '() *tab-close-rect* nil *tab-plus-rect* nil)
  (lol.canvas:fill-rect canvas 0 0 w +bar-h+ *ui-bg*)
  (let ((x 8) (multi (cdr *trails*)))                        ; align with the 8px gutter below
    (loop for trail in *trails* for i from 0
          for active = (= i *active*)
          for closer = (and active multi)        ; only the active tab shows ×, if >1
          for num = (format nil " ~d ~c " (1+ i) (code-char #xB7))  ; " 1 · "
          for title = (format nil "~a " (clip-text (trail-title trail) 16))
          for lw = (+ (lol.canvas:string-px *bfont* num) (lol.canvas:string-px *bfont* title))
          for tw = (+ lw (if closer 14 0))
          while (< (+ x tw) (- w 26))             ; leave room for the + button
          do (progn
               (lol.canvas:fill-rect canvas x 0 tw +bar-h+ (if active *ui-hi* *ui-panel*))
               (lol.canvas:fill-rect canvas x 0 tw 2 (trail-kind-color trail))   ; kind stripe on top
               ;; number quiet, title bright (active accent, inactive plain text)
               (let ((pen (lol.canvas:draw-string canvas *bfont* num (+ x 2) 3 *ui-dim*)))
                 (lol.canvas:draw-string canvas *bfont* title pen 3
                                         (if active *ui-accent* *ui-text*)))
               (when closer
                 (let ((cx (+ x (lol.canvas:string-px *bfont* num) (lol.canvas:string-px *bfont* title))))
                   (lol.canvas:draw-string canvas *bfont* (string (code-char 215)) cx 3 *ui-dim*)
                   (setf *tab-close-rect* (cons cx (+ cx 14)))))
               (push (cons i (cons x (+ x tw))) *tab-rects*)
               (incf x (+ tw 6))))                ; 6px gap between tabs
    ;; the + new-tab button
    (let ((px (+ x 2)))
      (lol.canvas:fill-rect canvas px 0 14 +bar-h+ *ui-panel*)
      (lol.canvas:draw-string canvas *bfont* "+" (+ px 4) 3 *ui-accent*)
      (setf *tab-plus-rect* (cons px (+ px 14)))
      (setf x (+ px 14)))
    ;; a hairline seam under the whole strip
    (lol.canvas:fill-rect canvas 0 (- +bar-h+ 1) w 1 *ui-rule*)
    ;; app name, right-aligned, if it still fits
    (let* ((s "lisp-over-linux") (sx (- w (lol.canvas:string-px *bfont* s) 8)))
      (when (> sx (+ x 8))
        (lol.canvas:draw-string canvas *bfont* s sx 3 *ui-dim*)))))

(defun draw-hints (canvas x y w hints)
  "Draw HINTS — a list of (CHORD . MEANING) — along the status line, the CHORD in the
   accent and the meaning dim. Colouring the chord is the whole point: a status line
   of uniformly grey words is read as prose and therefore not read at all, whereas
   `ret drill` with `ret` lit reads as a key and its effect. A NIL chord is plain text.
   Stops when it runs out of width rather than spilling off the edge."
  (let ((pen x))
    (dolist (h hints)
      (destructuring-bind (chord . meaning) h
        (let* ((cw (if chord (+ (lol.canvas:string-px *bfont* chord) 5) 0))
               (mw (+ (lol.canvas:string-px *bfont* meaning) 12)))
          (when (> (+ pen cw mw) (- w 8)) (return))
          (when chord
            (lol.canvas:draw-string canvas *bfont* chord pen y *ui-accent*)
            (incf pen cw))
          (lol.canvas:draw-string canvas *bfont* meaning pen y *ui-dim*)
          (incf pen mw))))
    pen))

(defun draw-crumb (canvas y w pane index)
  "A collapsed ancestor bar: a kind-coloured stripe, the subject's title, and what
   was picked inside it. The stripe is what makes a deep trail readable at a glance —
   four grey bars say nothing, but gold/blue/green says package, class, generic."
  (let ((tint (view-kind-color (pane-view pane)))
        (sel (selected-item pane)))
    (lol.widgets:draw-bar canvas 8 y (- w 16)
                          :marker ">" :label (view-title (pane-view pane)) :tint tint
                          :right (and sel (format nil "picked ~a" (item-label sel))))
    (lol.canvas:fill-rect canvas 8 y 3 +bar-h+ tint))       ; the kind stripe
  (push (cons index y) *crumb-rects*))

(defun preview-for (subject)
  "The cached read-only PANE for SUBJECT (the nav-split right column), rebuilt only
   when the selection moves to a different subject. NIL for a nil subject. The preview
   is never focused — it is a look, not an edit surface; drilling (Enter) is what opens
   a real editable pane."
  (when subject
    (unless (eq subject *preview-subject*)
      (setf *preview-subject* subject
            *preview-pane* (ignore-errors (open-pane subject)))
      (let ((tv (and *preview-pane* (pane-tv *preview-pane*))))
        (when tv (setf (lol.textview:tv-focus tv) nil))))  ; read-only: no caret
    *preview-pane*))

(defun draw-pane-body (canvas pane x y w h)
  "Render PANE's body — a source/reference textview, the dispatch matrix, or a list —
   into the rect. The one place that knows how each view kind paints, so the expanded
   pane and the nav-split preview stay identical."
  (cond
    ((pane-tv pane)
     (lol.textview:tv-geometry (pane-tv pane) x y w h)
     (lol.textview:tv-draw canvas (pane-tv pane)))
    ((eq (view-kind (pane-view pane)) :matrix)
     (draw-matrix-body canvas pane x y w h))
    (t (draw-list-body canvas pane x y w h))))

(defun list-pane-splittable-p (pane)
  "True when PANE is a plain list whose selection has something to preview — the
   condition for the nav split (a list on the left, its selection previewed right)."
  (and (null (pane-tv pane))
       (not (eq (view-kind (pane-view pane)) :matrix))
       (let ((sel (selected-item pane)))
         (and sel (item-subject sel)))))

(defun draw-browser (canvas w h)
  (setf *crumb-rects* '())
  (setf *split-x* nil)
  (lol.canvas:fill-rect canvas 0 0 w h *ui-bg*)
  ;; the tab strip replaces the old static title bar
  (draw-tabs canvas w)
  (let* ((ancestors (butlast (current-trail)))
         (cur (current-pane))
         (y (+ +bar-h+ 4)))
    ;; ancestor breadcrumb bars
    (loop for p in ancestors for i from 0
          do (draw-crumb canvas y w p i) (incf y (+ +bar-h+ 3)))
    ;; the expanded current pane
    (let ((body-y (+ y +bar-h+ 4)) (body-h (- h y +bar-h+ 4 24)))
      (setf *content-top* body-y)                 ; remember it for click hit-testing
      (lol.widgets:draw-panel canvas 8 y (- w 16) (- h y 24)
                              :title (view-title (pane-view cur))
                              :tint (view-kind-color (pane-view cur))
                              :badge (string-downcase (symbol-name (view-kind (pane-view cur)))))
      (let ((is-list (and (null (pane-tv cur))
                          (not (eq (view-kind (pane-view cur)) :matrix)))))
        (cond
          ;; ⇧↵ EDITABLE SIDE: the list on the left, a pinned editable pane on the right;
          ;; the focused half wears an accent top-rule (SuperL-o moves focus).
          ((and is-list (pane-side cur))
           (let* ((full-w (- w 24)) (list-w (max 260 (floor (* full-w 42) 100)))
                  (gap 12) (sx (+ 12 list-w gap)) (sw (- w sx 12)) (side (pane-side cur)))
             (setf *split-x* sx)
             (draw-list-body canvas cur 12 body-y list-w body-h)
             (lol.canvas:draw-rect canvas (- sx (floor gap 2)) body-y 1 body-h *ui-rule*)
             (lol.canvas:draw-string canvas *bfont*
               (format nil "beside · ~a~@[   ~a~]" (view-title (pane-view side))
                       (if *editing-side* "editing — C-c C-c accept · SuperL-o list" "SuperL-o to edit"))
               sx (- body-y 15) (if *editing-side* *ui-accent* *ui-dim*))
             ;; accent top-rule over whichever half has focus
             (if *editing-side*
                 (lol.canvas:fill-rect canvas sx (- body-y 2) sw 2 *ui-accent*)
                 (lol.canvas:fill-rect canvas 12 (- body-y 2) list-w 2 *ui-accent*))
             (draw-pane-body canvas side sx body-y sw body-h)))
          ;; NAV SPLIT: list left, a live READ-ONLY preview of the selection right.
          ((and is-list (selected-item cur) (item-subject (selected-item cur)))
           (let* ((full-w (- w 24)) (list-w (max 260 (floor (* full-w 42) 100)))
                  (gap 12) (prev-x (+ 12 list-w gap)) (prev-w (- w prev-x 12))
                  (preview (preview-for (item-subject (selected-item cur)))))
             (setf *split-x* prev-x)
             (draw-list-body canvas cur 12 body-y list-w body-h)
             (lol.canvas:draw-rect canvas (- prev-x (floor gap 2)) body-y 1 body-h *ui-rule*)
             (if preview
                 (progn
                   (lol.canvas:draw-string canvas *bfont* (view-title (pane-view preview))
                                           prev-x (- body-y 15) *ui-dim*)
                   (draw-pane-body canvas preview prev-x body-y prev-w body-h))
                 (lol.canvas:draw-string canvas *bfont* "(no preview)"
                                         prev-x (+ body-y 4) *ui-dim*))))
          ;; single pane, full width (source / matrix / a list with nothing to preview)
          (t (draw-pane-body canvas cur 12 body-y (- w 24) body-h)))))
    ;; status line
    (let ((cmds (ignore-errors (commands-for (pane-subject cur)))))
      (lol.canvas:fill-rect canvas 0 (- h 22) w 22 *ui-hi*)
      (if *browser-status*
          (lol.canvas:draw-string canvas *bfont* *browser-status* 10 (- h 18) *ui-accent*)
          (draw-hints canvas 10 (- h 18) w
                      (append
                       (cond ((and *editing-side* (pane-side cur))
                              '(("C-c C-c" . "accept") ("SuperL-o" . "back to list")))
                             ((workspace-pane-p cur)
                              '(("C-x C-e" . "Do it") ("C-c C-p" . "Print")
                                ("C-c C-i" . "Inspect") ("C-c C-d" . "Debug")))
                             ((editable-pane-p cur) '(("C-c C-c" . "accept")))
                             ((pane-side cur)
                              '(("up/dn" . "move") ("ret" . "drill") ("S-ret" . "beside")
                                ("SuperL-o" . "edit")))
                             (t '(("up/dn" . "move") ("ret" . "drill") ("S-ret" . "beside"))))
                       '(("SuperL-<" . "back") ("SuperL-q" . "quit"))
                       (when cmds
                         (list (cons nil (format nil "commands: ~{~a~^ · ~}"
                                                 (mapcar #'command-label cmds)))))))))))

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

(defun accept-pane (pane)
  "Compile the edited source buffer into the live image + update the registry
   (accept-source), for any editable subject — a whole definition (subj-defn) or a
   single method (subj-method). Sets *browser-status* to the outcome."
  (let ((text (lol.textview:tv-text (pane-tv pane)))
        (subj (pane-subject pane)))
    (handler-case
        (multiple-value-bind (name kind label) (accept-target subj)
          (let ((n (accept-source text name kind label)))
            (setf *browser-status* (format nil "Accepted: ~(~a~)" n))))
      (serious-condition (c)
        (setf *browser-status* (format nil "Accept failed: ~a" c))))))

(defun edit-pane-key (p key state)
  "Handle a key for an EDITABLE pane P — a source (Accept) or the Workspace (eval verbs).
   Chords ride *pending-prefix*: C-c {C-c Accept | C-p Print | C-i Inspect | C-d Debug},
   C-x C-e Do it; a bare Tab completes; anything else edits. Shared by the current pane
   and the ⇧↵ side pane, and guarded so a list side (no textview) can't crash. Returns NIL."
  (let ((c (lol.canvas:ctrl-p state)))
    (cond
      (*pending-prefix*
       (let ((prefix *pending-prefix*))
         (setf *pending-prefix* nil)
         (cond
           ((and (eq prefix :c-c) c (eql key #\c) (editable-pane-p p)) (accept-pane p))
           ((and (eq prefix :c-c) c (eql key #\p) (workspace-pane-p p)) (workspace-print-it p))
           ;; C-c C-i Inspect it: Ctrl-I *is* Tab (byte 9) on any tty, so the console
           ;; delivers this chord as C-c then :tab — accept both.
           ((and (eq prefix :c-c) (or (and c (eql key #\i)) (eq key :tab))
                 (workspace-pane-p p))
            (workspace-inspect-it p))
           ((and (eq prefix :c-c) c (eql key #\d) (workspace-pane-p p)) (workspace-debug-it p))
           ((and (eq prefix :c-x) c (eql key #\e) (workspace-pane-p p)) (workspace-do-it p)))
         nil))
      ((and c (eql key #\c)) (setf *pending-prefix* :c-c) nil)
      ((and c (eql key #\x) (workspace-pane-p p)) (setf *pending-prefix* :c-x) nil)
      ((and (eq key :tab) (pane-tv p)) (editor-complete p) nil)
      (t (when (pane-tv p) (lol.textview:tv-key (pane-tv p) key state)) nil))))

(defun browser-key (key state)
  "Route a decoded key by modifier + pane type (see doc/code-browser.org §4¾).
   Returns :quit to exit. STATE is the X modifier bitmask."
  (let* ((p (current-pane))
         ;; Meta = the Alt/Esc-prefix bit on this key, OR a sticky Esc from last time.
         ;; Consume the sticky flag every keystroke; the Esc branch below re-arms it.
         (meta (or (lol.canvas:meta-p state) *pending-meta*))
         ;; Fold the sticky-Esc Meta into STATE so downstream (tv-key's M-f / M-b)
         ;; sees it too, not just the M-. branch here.
         (state (if meta (logior state 8) state)))
    (setf *pending-meta* nil)
    (cond
      ;; M-. — jump to the definition of the symbol at point, the keyboard twin of
      ;; Ctrl-click. Must precede the editable branch, or the "." would self-insert.
      ((and meta (eql key #\.) (pane-tv p)) (jump-at-point (pane-tv p)))
      ;; Esc alone = the Emacs Meta prefix (sticky for the next key); it must NOT quit
      ;; the browser. Leaving via C-g at the root (below) is the deliberate exit.
      ((eq key :escape) (setf *pending-meta* t) nil)
      ;; C-g = back, everywhere (the console can't deliver SuperL, so Ctrl-g is the
      ;; portable "back" that also works while editing a source pane). At the root there
      ;; is nothing to pop, so C-g does NOTHING — leaving the environment is SuperL-q
      ;; only, so a stray C-g can't drop you out by accident.
      ((and (lol.canvas:ctrl-p state) (member key '(#\g #\G)))
       (setf *pending-prefix* nil *browser-status* nil)
       (when (cdr (current-trail)) (trail-pop))
       nil)
      ;; --- SuperL layer: tab navigation, the Spotter + the cheatsheet, over ANY pane
      ;;     (never the editor's). Super arrives via the keyboard's evdev (folded into
      ;;     STATE, bit 6), so it works on the bare console too — see fb-browser. ---
      ((lol.canvas:super-p state)
       (setf *pending-prefix* nil)
       (cond
         ((eq key :left) (trail-pop) (setf *browser-status* nil) nil)          ; back in trail
         ((eq key :tab) (cycle-trail (if (lol.canvas:shift-p state) -1 1)) nil) ; cycle tabs
         ((and (characterp key) (char<= #\1 key #\9))                          ; jump to tab N
          (activate-trail (- (char-code key) (char-code #\1))) nil)
         ((member key '(#\w #\W)) (close-trail) nil)                           ; close this tab
         ((member key '(#\o #\O)) (toggle-side-focus) nil)                     ; focus list <-> side
         ((eql key #\Space)                                                    ; the Spotter
          (when (fboundp 'run-spotter-fb) (funcall 'run-spotter-fb)) nil)
         ((member key '(:return #\Return))                                     ; open selection in a NEW tab
          (let ((it (selected-item (current-pane))))
            (when (and it (item-subject it)) (new-trail (item-subject it))))
          nil)
         ((member key '(#\? #\/))                                              ; Super-? — every binding
          (when (fboundp 'run-cheatsheet-fb) (funcall 'run-cheatsheet-fb)) nil)
         ((member key '(#\q #\Q)) :quit)
         (t nil)))
      ;; --- ⇧↵ editable SIDE pane has focus: its keys edit it (Accept, verbs, typing);
      ;;     SuperL-o (above) hands focus back to the list. ---
      ((and *editing-side* (pane-side p)) (edit-pane-key (pane-side p) key state))
      ;; --- editable panes: source (Accept) and workspace (eval verbs). Chords go
      ;;     through *pending-prefix*: C-c then {C-c Accept | C-p Print | C-i Inspect},
      ;;     C-x then C-e Do it. Any other key edits. ---
      ((or (editable-pane-p p) (workspace-pane-p p)) (edit-pane-key p key state))
      ;; --- read-only reference pane: scroll only ---
      ((pane-tv p)
       (case key
         (:up        (lol.textview:tv-key (pane-tv p) :up 0))
         (:down      (lol.textview:tv-key (pane-tv p) :down 0))
         (:page-up   (lol.textview:tv-key (pane-tv p) :page-up 0))
         (:page-down (lol.textview:tv-key (pane-tv p) :page-down 0))
         (#\q :quit)
         (t nil)))
      ;; --- list pane: plain-key navigation (nothing to edit) ---
      (t
       (cond
         ((eql key #\q) :quit)
         ((eq key :up)   (set-pane-sel p (1- (pane-sel p))))
         ((eq key :down) (set-pane-sel p (1+ (pane-sel p))))
         ((eq key :page-up)   (set-pane-sel p (- (pane-sel p) +list-page+)))
         ((eq key :page-down) (set-pane-sel p (+ (pane-sel p) +list-page+)))
         ((eq key :return)
          (if (lol.canvas:shift-p state)
              (open-beside p)                            ; ⇧↵ — open BESIDE (editable split)
              (let ((it (selected-item p)))              ; ↵ — drill in place
                (when (and it (item-subject it)) (trail-push (item-subject it))))))
         ((member key '(:backspace :left)) (trail-pop))
         (t nil))))))

;;; ---- jump-to-definition: Ctrl-click a symbol in a source pane -------------

(defun click-token (string col)
  "The symbol-token in STRING under column COL, or NIL when COL is whitespace or a
   delimiter. A token is a maximal run of non-delimiter characters — enough to
   catch =foo-bar=, =*var*=, and =pkg:name= without a real reader."
  (flet ((delim (ch) (member ch '(#\Space #\Tab #\( #\) #\' #\" #\` #\, #\;))))
    (when (and string (<= 0 col) (< col (length string)) (not (delim (char string col))))
      (let ((start col) (end col) (n (length string)))
        (loop while (and (> start 0) (not (delim (char string (1- start))))) do (decf start))
        (loop while (and (< end n) (not (delim (char string end)))) do (incf end))
        (subseq string start end)))))

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

(defun jump-at-point (tv)
  "Resolve the symbol at TV's caret and drill into its definition on a new trail
   level (source if we recorded it, else a reference — PRESENT on a SUBJ-DEFN handles
   both). The keyboard M-. and the Ctrl-click share this once the caret is placed.
   Tries the caret column, then one to its left, so M-. also fires with the caret
   resting just past a symbol. No-op when there's no jumpable symbol there."
  (let* ((lines (lol.textview:tv-lines tv))
         (line  (lol.textview:tv-point-line tv))
         (col   (lol.textview:tv-point-col tv))
         (tok   (and (< line (length lines))
                     (let ((s (aref lines line)))
                       (or (click-token s col)
                           (and (> col 0) (click-token s (1- col)))))))
         (sym   (resolve-symbol tok)))
    (when (jumpable-p sym)
      (setf *pending-prefix* nil *browser-status* nil)
      (trail-push (subj-defn sym))
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
    (if (pane-tv p)
        (lol.textview:tv-wheel (pane-tv p) lines)
        (setf (pane-top p) (max 0 (+ (pane-top p) lines))
              (pane-follow p) nil))))

(defun browser-click (x y &optional ctrl super)
  "Click: the top tab strip activates a trail; a breadcrumb bar pops to that level; a
   list item drills (SuperL-click opens it in a NEW trail instead); Ctrl-click on a
   symbol in a source pane jumps to its definition. Uses the geometry DRAW-BROWSER
   recorded (*tab-rects*, *crumb-rects*, *content-top*), so click and draw can never
   disagree."
  ;; the tab strip owns the top +bar-h+ band: + opens a fresh Workspace, the active
  ;; tab's × closes it, else a tab click activates that trail. (i x0 . x1) per entry.
  (when (< y +bar-h+)
    (cond
      ((and *tab-plus-rect* (<= (car *tab-plus-rect*) x (cdr *tab-plus-rect*)))
       (new-trail (subj-workspace)))
      ((and *tab-close-rect* (<= (car *tab-close-rect*) x (cdr *tab-close-rect*)))
       (close-trail))
      (t (let ((tab (find-if (lambda (e) (<= (cadr e) x (cddr e))) *tab-rects*)))
           (when tab (activate-trail (car tab))))))
    (return-from browser-click))
  ;; a breadcrumb bar? each entry is (pane-index . y-top); test its y-top (cdr).
  (let ((hit (find-if (lambda (e) (<= (cdr e) y (+ (cdr e) +bar-h+))) *crumb-rects*))
        (p (current-pane)))
    (cond
      (hit (trail-goto (car hit)))
      ;; text pane: Ctrl-click jumps to definition; a plain click places the caret
      ;; in an editable pane (source or workspace), and is inert in a read-only one.
      ((pane-tv p)
       (cond
         (ctrl (jump-to-definition (pane-tv p) x y))
         ((and (or (editable-pane-p p) (workspace-pane-p p))
               (lol.textview:tv-hit-p (pane-tv p) x y))
          (lol.textview:tv-click (pane-tv p) x y))))
      ;; dispatch matrix: a cell drills its effective-method onion
      ((eq (view-kind (pane-view p)) :matrix)
       (matrix-cell-click p x y))
      ;; nav-split preview column: a matrix preview is LIVE — clicking a cell drills its
      ;; effective-method onion straight from here (no need to open the matrix first); a
      ;; source preview jumps on Ctrl-click; otherwise the click opens the previewed
      ;; selection for real (SuperL-click in a new trail).
      ((and *split-x* (>= x *split-x*) (>= y *content-top*))
       (cond
         ;; ⇧↵ editable side pane: a click focuses it and places the caret (Ctrl jumps).
         ((pane-side p)
          (setf *editing-side* t)
          (let ((tv (pane-tv (pane-side p))))
            (when (and tv (lol.textview:tv-hit-p tv x y))
              (if ctrl (jump-to-definition tv x y) (lol.textview:tv-click tv x y)))))
         (t
          (let ((sel (selected-item p))
                (pv *preview-pane*))
            (cond
              ((and pv (eq (view-kind (pane-view pv)) :matrix))
               (or (matrix-cell-click pv x y)               ; a cell -> its onion
                   (and sel (item-subject sel) (trail-push (item-subject sel)))))  ; miss -> open matrix
              ((and ctrl pv (pane-tv pv)) (jump-to-definition (pane-tv pv) x y))
              ((and sel (item-subject sel))
               (if super (new-trail (item-subject sel)) (trail-push (item-subject sel)))))))))
      ;; else an item in the current list view — SuperL-click opens it in a NEW trail; a
      ;; plain click here also returns focus to the list (away from any side pane).
      ((>= y *content-top*)
       (setf *editing-side* nil)
       ;; which row was actually painted here? *list-rows* answers; a click on a group
       ;; header hits no row and is correctly inert.
       (let* ((items (view-items (pane-view p)))
              (i (lol.widgets:hit-row *list-rows* y)))
         (when (and i (< i (length items)))
           (set-pane-sel p i)
           (let ((it (nth i items)))
             (when (item-subject it)
               (if super (new-trail (item-subject it)) (trail-push (item-subject it)))))))))))
