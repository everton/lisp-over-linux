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
  side                    ; a pinned, EDITABLE pane shown BESIDE this list (⇧↵), or NIL
  side-stash              ; a side pane HIDDEN rather than closed, so re-opening it
                          ; restores the buffer instead of a blank one. Hiding an
                          ; EDITABLE pane must not destroy what you typed into it.
  side-pinned)            ; did the USER pin that side pane (⇧↵), or did -> merely
                          ; focus the preview? A pinned side outlives losing focus;
                          ; a focused preview must not, or leaving it would strand you
                          ; in a different layout from the one you arrived in.

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

(defun view-kind-concept (kind)
  "A VIEW's kind (:source :matrix :class …) mapped to the Lisp CONCEPT it is a view
   OF (:function :generic-function :class …), so chrome can be tinted from the same
   palette as the rows. One mapping, used by both the pane/tab/crumb tint and the
   Spotter's recents — it existed twice for a while and the copies disagreed about
   what colour a remembered Workspace was."
  (case kind
    (:workspace :restart)                  ; amber: a surface, not a definition
    ((:source :reference) :function)
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
    "function" "generic-function" "method" "macro" "struct" "type" "senders" "referrers")
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
(defvar *last-edit* nil
  "What the previous editing key did, for the two Emacs commands that depend on it:
   (:KILL) — so another kill APPENDS to the same history entry — and
   (:YANK TV L0 C0 L1 C1) — the span the last paste inserted, which M-y replaces.
   Any other key clears it (EDIT-PANE-KEY).")

(defvar *pending-meta* nil
  "T after a lone Esc — the Emacs Meta prefix (§4¾). The next key is Meta-modified,
   so Esc . jumps to definition just like Alt-. / M-. would. Sticky for one key,
   cleared on the next keystroke.")
(defvar *browser-status* nil "A transient status message (e.g. Accept result), or NIL.")
(defvar *value-prompt* nil
  "A hook: (funcall *value-prompt* LABEL) -> the string the user typed, or NIL if they
   cancelled. Reading a line is a modal input loop, which only a BACKEND can run — so
   the backend installs one (fb-browser.lisp does) and the shell merely asks. Same
   shape as *COMPLETER*, which repl.lisp fills in for Tab completion. When no backend
   has set it, a gesture that needs a value says so in the status line instead of
   silently doing nothing.")
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
          (setf (pane-side pane) nil (pane-side-pinned pane) nil
                *editing-side* nil)                          ; toggle closed
          (setf (pane-side pane) (open-pane (item-subject it))
                (pane-side-pinned pane) t                    ; ⇧↵ pins it
                *editing-side* t)))))

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
      (setf *editing-side* t)
      t)))

(defun unfocus-side (pane)
  "Give focus back to the list. If the right column was merely FOCUSED (by ->), it
   goes away again and you are looking at the read-only preview you started from —
   the identical frame, down to the pixel. If you PINNED it with <S-Return>, it stays:
   pinning is a decision, and C-g should not quietly undo one."
  (setf *editing-side* nil)
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
         (setf *editing-side* nil)               ; the trail moves; focus comes home
         (trail-push (item-subject it)))))
    (t nil))
  nil)

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
   skipped by the reader; an incomplete trailing form is ignored.

   Returns (values FORM FOUNDP). FOUNDP matters because NIL is a perfectly good form:
   the first version stopped the scan on any form that read as NIL, so a buffer
   containing a bare =nil= or =()= anywhere made every form BELOW it invisible, and
   Do it would silently evaluate something from higher up the buffer instead of the
   one before the caret. A sentinel distinguishes \"read nothing more\" from \"read NIL\",
   and a read ERROR (an incomplete trailing form, which is the normal case while you
   are still typing) is caught separately from a legitimate NIL."
  (let* ((text (buffer-text-to-point tv))
         (sentinel (list :eof))               ; fresh and uninterned: nothing can be EQ to it
         (last nil) (foundp nil) (pos 0))
    (loop
      (multiple-value-bind (form next)
          (handler-case (read-from-string text nil sentinel :start pos)
            (error () (values sentinel pos)))
        (when (eq form sentinel) (return))
        (setf last form foundp t pos next)))
    (values last foundp)))

(defun form-text-before-point (tv)
  "The SOURCE TEXT of the form FORM-BEFORE-POINT would return, or NIL — the same scan,
   but keeping the characters rather than the object.

   Accept (C-c C-c) in the Workspace needs this. It hands ACCEPT-SOURCE a string, which
   reads its FIRST form; given the whole buffer, it compiled whatever stood at the top
   — the primer's (+ 1 2) — and reported \"Accepted: 3\" for a (defun greet …) under
   the caret, and filed the entire buffer as that definition's source. The text,
   rather than a re-printed form, is what gets recorded, so comments and layout in
   the definition survive into the change set."
  (let* ((text (buffer-text-to-point tv))
         (sentinel (list :eof))
         (last nil) (pos 0))
    (loop
      (multiple-value-bind (form next)
          (handler-case (read-from-string text nil sentinel :start pos)
            (error () (values sentinel pos)))
        (when (eq form sentinel) (return))
        (setf last (cons pos next) pos next)))
    (and last
         (string-trim '(#\Space #\Tab #\Newline) (subseq text (car last) (cdr last))))))

(defun form-echo (form)
  "FORM printed short and on one line, to name it in the status line. The Workspace
   verbs say WHICH form they ran: with several forms in a buffer, \"=> 3\" alone does
   not tell you whether the one before the caret is the one that ran."
  (let ((*print-length* 5) (*print-level* 3))
    (substitute #\Space #\Newline (format nil "~s" form))))

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
  (multiple-value-bind (form foundp) (form-before-point (pane-tv pane))
    (if (not foundp)
        (setf *browser-status* "Do it: no form before point")
        (multiple-value-bind (ok res out) (ws-eval form)
          (let ((printed (ws-trim-output out)))
            (setf *browser-status*
                  (if ok (let ((*print-length* 40) (*print-level* 5))
                           (format nil "~a ~@[~a  ~]=> ~s" (form-echo form)
                                   (and printed (substitute #\Space #\Newline printed)) res))
                      (format nil "~a ~@[~a  ~]error: ~a" (form-echo form)
                              (and printed (substitute #\Space #\Newline printed)) res))))))))

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

(defvar *side-rows* '()
  "The hit map for a LIST drawn in the right-hand column. Separate from *LIST-ROWS*
   because the split draws two lists in one frame, and whichever went second used to
   overwrite the other's map — so a click in the left list hit-tested against the
   right one's rows. Reachable much more easily now that -> focuses the side.")

(defvar *list-rows* '()
  "The hit map LOL.WIDGETS:DRAW-LIST returned for the pane drawn last — ((y . index) …).
   A click reads it back rather than computing an index from y, because with section
   headers in the layout screen row N is no longer item N.")

(defun draw-list-body (canvas pane x y w h &optional (into :main))
  "Render a list view: the widget does rows, sections, selection and the scrollbar;
   this decides only WHAT the rows say. PANE-TOP is the widget's viewport position
   (a layout slot, headers included) and is written back so it persists across frames.
   INTO says which hit map to record — :MAIN for the expanded pane, :SIDE for the
   right-hand column, which is drawn in the same frame and must not overwrite it."
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
      (setf (pane-top pane) top)
      (if (eq into :side) (setf *side-rows* hits) (setf *list-rows* hits)))))

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

;;; ---- the clickable dispatch matrix (§3) ----------------------------------

(defvar *matrix-cells* '()
  "The hit map LOL.WIDGETS:DRAW-GRID returned for the dispatch grid drawn last, so a
   click and the drawing can never disagree (same discipline as *crumb-rects*).")

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
        (lol.widgets:draw-grid canvas x y (- w 2) (- h 20) rows cols counts)
      (setf *matrix-cells* hits)
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

(defparameter +stage-transcript-h+ 92 "Height of the transcript strip under the stage.")
(defvar *stage-rect* nil "The viewport rect (x y w h) of the last stage drawn, for clicks.")

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
         (pts (mapcar (lambda (p)
                        (cons (+ ox (body-x b) (round (- (* (car p) c) (* (cdr p) sn))))
                              (+ oy (body-y b) (round (+ (* (car p) sn) (* (cdr p) c))))))
                      (stage-sprite-points b))))
    (lol.canvas:draw-poly canvas pts color)
    ;; the station's ring and core — a hexagon alone reads as a rock
    (when (eq (body-kind b) :station)
      (lol.canvas:draw-circle canvas (+ ox (body-x b)) (+ oy (body-y b))
                              (round (* (body-r b) 42) 100) color)
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
  (let* ((rows (max 1 (floor (- h 22) 14)))
         (tail (reverse (subseq (stage-transcript stage)
                                0 (min rows (length (stage-transcript stage))))))
         (ty (+ y 20))
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
      (incf ty 14))))

(defun draw-stage-body (canvas pane x y w h)
  "Render a :stage view: the viewport with its sprites, the heads-up line naming what
   would dispatch next, and the transcript strip along the bottom.

   The stage is told the viewport size here rather than at construction, because the
   pane is the authority on how much room there is — resize the window and the world
   gets bigger, which is the behaviour you would want anyway."
  (let* ((stage (view-data (pane-view pane)))
         (vh (- h +stage-transcript-h+))
         (ox x) (oy y))
    (stage-resize stage (max 80 w) (max 80 vh))
    (setf *stage-rect* (list x y w vh))
    (lol.canvas:fill-rect canvas x y w vh !#0E1013)
    (dolist (b (stage-bodies stage))
      (draw-stage-sprite canvas b stage ox oy))
    ;; the heads-up line: compute-applicable-methods asked BEFORE the contact, so the
    ;; view tells you what will happen rather than only what did
    (let ((hud (stage-hud-line stage)))
      (lol.canvas:draw-string canvas *bfont*
                              (lol.widgets:clip-text hud (max 8 (floor (- w 8) (lol.canvas:font-cw *bfont*))))
                              (+ x 4) (+ y vh -16)
                              (if (search "will signal" hud) (concept-color :condition) *ui-dim*)))
    (unless (stage-running stage)
      (lol.canvas:draw-string canvas *bfont* "PAUSED" (+ x 4) (+ y 4) *ui-amber*))
    (draw-stage-transcript canvas stage x (+ y vh) w +stage-transcript-h+)))

(defun stage-click (pane px py)
  "A click in the stage viewport selects the body under the pointer — and a selected
   body is a subject, so Enter opens the Inspector on the instance itself. NIL when the
   click missed every sprite (which clears the selection)."
  (let ((stage (view-data (pane-view pane))))
    (when *stage-rect*
      (destructuring-bind (x y w vh) *stage-rect*
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
  (setf *editing-side* t)
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
                      *editing-side* nil)
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
  (concept-color (view-kind-concept (view-kind view))))

(defun trail-kind-color (trail)
  "The semantic tint for TRAIL's tab — from its ROOT view (see VIEW-KIND-COLOR)."
  (view-kind-color (pane-view (first trail))))

(defvar *tab-close-rects* '()
  "alist ((trail-index . (x0 . x1)) …) of every tab's × close hit-box. The design
   mock-up (media/browser-views-design.html §1) puts a × on EVERY tab, not only the
   focused one — closing a tab you are not in is the common case, and having to focus
   it first is a step the picture does not ask for.")
(defvar *tab-plus-rect* nil  "x-range (x0 . x1) of the + new-tab hit-box, or NIL.")

;;; ---- the systray: the right end of the tab strip -----------------------------
;;; What the user's StumpWM mode line shows on the host, in the same order — battery,
;;; then the date and time — plus an icon for the clipboard history. The DATA comes
;;; from initramfs/sysinfo.lisp (TRAY-ITEMS); this only lays it out. It is drawn into
;;; its own rectangle so the clock can tick by repainting just that strip
;;; (DRAW-TRAY), not the whole frame. See doc/systray.org.

(defvar *tray-rect* nil "The tray's rectangle (x y w h), set on draw.")
(defvar *tray-clip-rect* nil "The clipboard icon's x-range (x0 . x1), for clicks.")

(defun tray-tone-color (tone)
  "A battery tone as a colour — the host script's green / yellow / red."
  (case tone
    ((:ok :charging) (concept-color :method))     ; green
    (:warn *ui-amber*)
    (:low (concept-color :condition))             ; red
    (t *ui-dim*)))

(defun draw-clip-icon (canvas x y color)
  "A tiny clipboard, 10x13: the board and the clip at its top."
  (lol.canvas:draw-rect canvas x (+ y 2) 10 11 color)
  (lol.canvas:fill-rect canvas (+ x 3) y 4 4 color)
  (lol.canvas:fill-rect canvas (+ x 2) (+ y 6) 6 1 color)
  (lol.canvas:fill-rect canvas (+ x 2) (+ y 9) 4 1 color))

(defun tray-width ()
  "How wide the tray will be, so the tabs can stop short of it."
  (let ((items (ignore-errors (tray-items))))
    (+ 8 14 (loop for it in items sum (+ 16 (lol.canvas:string-px *bfont* (getf it :text)))) 8)))

(defun draw-tray (canvas w)
  "Paint the tray, right-aligned in the tab strip: [clip] | BAT 87% | 2026-10-02 09:01.
   Records *TRAY-RECT* and *TRAY-CLIP-RECT*. Returns the tray's left edge."
  (let* ((items (ignore-errors (tray-items)))
         (tw (tray-width))
         (x0 (- w tw))
         (pen (+ x0 8)))
    (lol.canvas:fill-rect canvas x0 0 tw (- +bar-h+ 1) *ui-bg*)
    (draw-clip-icon canvas pen 3 (if (lol.clipboard:clip-current) *ui-accent* *ui-dim*))
    (setf *tray-clip-rect* (cons (- pen 3) (+ pen 13)))
    (incf pen 14)
    (dolist (it items)
      (lol.canvas:draw-string canvas *bfont* "|" (+ pen 4) 3 *ui-rule*)
      (incf pen 16)
      (setf pen (lol.canvas:draw-string canvas *bfont* (getf it :text) pen 3
                                        (if (eq (getf it :kind) :battery)
                                            (tray-tone-color (getf it :tone))
                                            *ui-text*))))
    (setf *tray-rect* (list x0 0 tw (- +bar-h+ 1)))
    x0))

(defun draw-tabs (canvas w)
  "The top tab strip: one tab per open trail, each with a kind-coloured stripe and its
   SuperL-number badge; the active one is highlighted (brighter bg + accent top-rule +
   an × close), and a + after the last tab opens a fresh Workspace. Records *tab-rects*
   / *tab-close-rect* / *tab-plus-rect* so clicks and drawing can't disagree; the app
   name sits at the right when there's room."
  (setf *tab-rects* '() *tab-close-rects* '() *tab-plus-rect* nil)
  (lol.canvas:fill-rect canvas 0 0 w +bar-h+ *ui-bg*)
  (let ((x 8) (multi (cdr *trails*)))                        ; align with the 8px gutter below
    (loop for trail in *trails* for i from 0
          for active = (= i *active*)
          for closer = multi                     ; every tab gets a ×, once there are >1
          for num = (format nil " ~d ~c " (1+ i) (code-char #xB7))  ; " 1 · "
          for title = (format nil "~a " (clip-text (trail-title trail) 16))
          for lw = (+ (lol.canvas:string-px *bfont* num) (lol.canvas:string-px *bfont* title))
          for tw = (+ lw (if closer 14 0))
          while (< (+ x tw) (- w 26 (tray-width)))  ; room for the + button and the tray
          do (progn
               (lol.canvas:fill-rect canvas x 0 tw +bar-h+ (if active *ui-hi* *ui-panel*))
               (lol.canvas:fill-rect canvas x 0 tw 2 (trail-kind-color trail))   ; kind stripe on top
               ;; number quiet, title bright (active accent, inactive plain text)
               (let ((pen (lol.canvas:draw-string canvas *bfont* num (+ x 2) 3 *ui-dim*)))
                 (lol.canvas:draw-string canvas *bfont* title pen 3
                                         (if active *ui-accent* *ui-text*)))
               (when closer
                 (let ((cx (+ x (lol.canvas:string-px *bfont* num) (lol.canvas:string-px *bfont* title))))
                   (lol.canvas:draw-string canvas *bfont* (string (code-char 215)) cx 3
                                           (if active *ui-dim* *ui-rule*))   ; quieter when unfocused
                   (push (cons i (cons cx (+ cx 14))) *tab-close-rects*)))
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
    ;; the systray where the app's name used to sit
    (draw-tray canvas w)))

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

(defun draw-pane-body (canvas pane x y w h &optional (into :main))
  "Render PANE's body — a source/reference textview, the dispatch matrix, or a list —
   into the rect. The one place that knows how each view kind paints, so the expanded
   pane and the nav-split preview stay identical. INTO is passed to DRAW-LIST-BODY."
  (cond
    ((pane-tv pane)
     (lol.textview:tv-geometry (pane-tv pane) x y w h)
     (lol.textview:tv-draw canvas (pane-tv pane)))
    ((eq (view-kind (pane-view pane)) :matrix)
     (draw-matrix-body canvas pane x y w h))
    ((eq (view-kind (pane-view pane)) :stage)
     (draw-stage-body canvas pane x y w h))
    (t (draw-list-body canvas pane x y w h into))))

(defun list-pane-splittable-p (pane)
  "True when PANE is a plain list whose selection has something to preview — the
   condition for the nav split (a list on the left, its selection previewed right)."
  (and (null (pane-tv pane))
       (not (member (view-kind (pane-view pane)) '(:matrix :stage)))
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
                          (not (member (view-kind (pane-view cur)) '(:matrix :stage))))))
        (cond
          ;; THE STAGE with its Workspace beside (§3b): the one subject whose side pane
          ;; is editable rather than a read-only preview. Same furniture as ⇧↵ below,
          ;; but driven by 'w' rather than a selected row, and the stage keeps the
          ;; larger half because it is the thing you are watching.
          ((and (eq (view-kind (pane-view cur)) :stage) (pane-side cur))
           (let* ((full-w (- w 24)) (stage-w* (max 300 (floor (* full-w 58) 100)))
                  (gap 12) (sx (+ 12 stage-w* gap)) (sw (- w sx 12)) (side (pane-side cur)))
             (setf *split-x* sx)
             (draw-stage-body canvas cur 12 body-y stage-w* body-h)
             (lol.canvas:draw-rect canvas (- sx (floor gap 2)) body-y 1 body-h *ui-rule*)
             (lol.canvas:draw-string canvas *bfont*
               (format nil "beside · ~a~@[   ~a~]" (view-title (pane-view side))
                       (if *editing-side*
                           "editing — C-c C-c define · SuperL-o stage"
                           "SuperL-o to edit"))
               sx (- body-y 15) (if *editing-side* *ui-accent* *ui-dim*))
             (if *editing-side*
                 (lol.canvas:fill-rect canvas sx (- body-y 2) sw 2 *ui-accent*)
                 (lol.canvas:fill-rect canvas 12 (- body-y 2) stage-w* 2 *ui-accent*))
             (draw-pane-body canvas side sx body-y sw body-h :side)))
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
             (draw-pane-body canvas side sx body-y sw body-h :side)))
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
                   (draw-pane-body canvas preview prev-x body-y prev-w body-h :side))
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
                              ;; name the half you go BACK to, and the verb the pane's
                              ;; own header advertises — a stage is not a list, and a
                              ;; Workspace defines rather than accepts an edit.
                              (list (cons "C-c C-c"
                                          (if (workspace-pane-p (pane-side cur))
                                              "define" "accept"))
                                    (cons "SuperL-o"
                                          (if (eq (view-kind (pane-view cur)) :stage)
                                              "back to stage" "back to list"))))
                             ((workspace-pane-p cur)
                              '(("C-x C-e" . "Do it") ("C-c C-p" . "Print")
                                ("C-c C-i" . "Inspect") ("C-c C-d" . "Debug")
                                ("C-c C-c" . "define")))
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
                                                 (mapcar #'command-label cmds)))))))))))

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

(defun spotter-results (query candidates &optional (limit 200))
  "The Spotter's results for QUERY: ranked, then BANDED. One function, because the
   caller indexes this list with its selection and the renderer draws it in order — if
   the banding happened at draw time, arrowing to row 3 would open whatever row 3 was
   before the bands moved it."
  (spotter-band (spotter-filter query candidates limit)))

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

(defvar *spotter-hits* '() "The hit map of the Spotter's visible rows — see HIT-ROW.")
(defvar *spotter-rect* nil "The card rect (x0 y0 x1 y1), for click routing / dismissal.")

(defun draw-spotter-card (canvas w h query results sel)
  "The Spotter as a floating palette over the dimmed browser: the browser is drawn as
   the backdrop, scrimmed; the card carries a title, the live match count, the query
   line, the results with the matched characters lit, and a footer of what the keys do.
   RESULTS must already be banded (see SPOTTER-RESULTS) — this only draws. Records
   *spotter-rect* / *spotter-hits* so a click routes to the row actually painted there."
  (draw-browser canvas w h)                          ; the browser, as backdrop …
  (let* ((rows   (spotter-rows results query))
         (cw     (min 780 (- w 80)))
         (cx     (floor (- w cw) 2))
         (cy     (+ +bar-h+ 14))
         ;; the card grows with the results, up to what the screen can hold
         (nslots (+ (length rows) (length (remove-duplicates
                                           (map 'list #'lol.widgets:row-group rows)
                                           :test #'equal))))
         (maxrows (max 3 (min nslots (floor (- h cy 96) +row-h+))))
         (list-h  (* maxrows +row-h+))
         (ch      (+ 24 20 8 list-h 24)))
    (setf *spotter-rect* (list cx cy (+ cx cw) (+ cy ch)))
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
          (multiple-value-bind (top hits)
              (lol.widgets:draw-list canvas (+ cx 6) ly (- cw 12) list-h rows
                                     :sel sel :top 0 :follow-sel t)
            (declare (ignore top))
            (setf *spotter-hits* hits)))
      ;; the footer says what the keys do, because the palette is modal and nothing
      ;; else on screen can say it
      (draw-hints canvas (+ cx 12) (+ cy ch -16) (- cw 16)
                  '(("up/dn" . "move") ("ret" . "open in a new tab") ("Esc" . "close"))))))

(defun spotter-row-at* (x y)
  "The visible Spotter row index at X,Y, or NIL — read back from what the draw
   recorded, so a banded list's section headers are correctly inert."
  (and *spotter-rect*
       (<= (first *spotter-rect*) x (third *spotter-rect*))
       (lol.widgets:hit-row *spotter-hits* y)))

(defun spotter-outside-p* (x y)
  "True when X,Y is outside the Spotter card — a click there dismisses the palette."
  (or (null *spotter-rect*)
      (destructuring-bind (x0 y0 x1 y1) *spotter-rect*
        (not (and (<= x0 x x1) (<= y0 y y1))))))

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
           (handler-case
               (let ((new (eval (read-from-string text))))
                 (funcall (item-edit it) new)
                 ;; re-present: the row, its neighbours and the title all come from
                 ;; the object, so read them back rather than editing the old view
                 (let ((sel (pane-sel pane)))
                   (setf (pane-view pane) (present (pane-subject pane)))
                   (set-pane-sel pane sel))
                 (setf *preview-subject* nil)          ; the cached preview is stale
                 (setf *browser-status*
                       (let ((*print-length* 20) (*print-level* 3))
                         (format nil "~a = ~s" (item-label it) new))))
             (serious-condition (c)
               (setf *browser-status* (format nil "set failed: ~a" c)))))))))
  nil)

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
  (let ((subject (pane-subject pane)))
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
                   (setf (pane-view pane) (present subject))
                   (set-pane-sel pane sel))
                 (setf *preview-subject* nil)
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
  (let ((subject (pane-subject pane)))
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
  (let ((text (if (workspace-pane-p pane)
                  (form-text-before-point (pane-tv pane))   ; the form before the caret
                  (lol.textview:tv-text (pane-tv pane))))
        (subj (pane-subject pane)))
    (unless text
      (return-from accept-pane (setf *browser-status* "Accept: no form before point")))
    (handler-case
        (multiple-value-bind (name kind label)
            (if (workspace-pane-p pane) (values nil nil nil) (accept-target subj))
          (let ((n (accept-source text name kind label)))
            (setf *browser-status* (format nil "Accepted: ~(~a~)" n))))
      (serious-condition (c)
        (setf *browser-status* (format nil "Accept failed: ~a" c))))))

(defun edit-pane-key (p key state)
  "Handle a key for an EDITABLE pane P — a source (Accept) or the Workspace (eval verbs).
   Chords ride *pending-prefix*: C-c {C-c Accept | C-p Print | C-i Inspect | C-d Debug},
   C-x C-e Do it; a bare Tab completes; anything else edits. Shared by the current pane
   and the ⇧↵ side pane, and guarded so a list side (no textview) can't crash. Returns NIL."
  (let ((c (lol.canvas:ctrl-p state))
        (tv (pane-tv p))
        (previous (shiftf *last-edit* nil)))     ; consumed: see *LAST-EDIT*
    (cond
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
                 (or (editable-pane-p p) (workspace-pane-p p)))
            (accept-pane p))
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
      ((and c (eql key #\x)) (setf *pending-prefix* :c-x) nil)
      ((and tv (clipboard-key tv key state previous)) nil)
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
         ;; focus is in the right column: hand it back to the list and STOP. Popping
         ;; the trail as well would throw away the split you are standing in, when
         ;; all you asked for was to stop editing.
         ((and *editing-side* (pane-side p)) (unfocus-side p))
         ((cdr (current-trail)) (trail-pop)))
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
         ;; SuperL-⇧-n moves THIS trail to slot n, swapping with whatever is there —
         ;; the design's binding table (§1). Shift-digit arrives as the shifted
         ;; character on any keyboard, so match those rather than a Shift bit.
         ((and (characterp key) (position key ")!@#$%^&*(" ))
          (swap-trail (position key ")!@#$%^&*(")) nil)
         ((member key '(#\w #\W)) (close-trail) nil)                           ; close this tab
         ((member key '(#\o #\O)) (toggle-side-focus) nil)                     ; focus list <-> side
         ((eql key #\Space)                                                    ; the Spotter
          (when (fboundp 'run-spotter-fb) (funcall 'run-spotter-fb)) nil)
         ((member key '(:return #\Return))                                     ; open selection in a NEW tab
          (let ((it (selected-item (current-pane))))
            (when (and it (item-subject it)) (new-trail (item-subject it))))
          nil)
         ((member key '(#\m #\M)) (open-modules) nil)                         ; the module index
         ((member key '(#\l #\L))                                              ; the CL-USER package
          (new-trail (find-package :cl-user)) nil)
         ((member key '(#\h #\H)) (open-clipboard-manager) nil)               ; clipboard history
         ((member key '(#\? #\/))                                              ; Super-? — every binding
          (when (fboundp 'run-cheatsheet-fb) (funcall 'run-cheatsheet-fb)) nil)
         ((member key '(#\q #\Q)) :quit)
         (t nil)))
      ;; --- ⇧↵ editable SIDE pane has focus: its keys edit it (Accept, verbs, typing);
      ;;     SuperL-o (above) hands focus back to the list/stage. This MUST precede the
      ;;     stage branch: a stage's letters spawn things, and typing "ship" into the
      ;;     Workspace beside it would otherwise launch four objects. ---
      ((and *editing-side* (pane-side p))
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
      ((or (editable-pane-p p) (workspace-pane-p p)) (edit-pane-key p key state))
      ;; --- read-only reference pane: scroll only ---
      ((and (pane-tv p) (or (and (lol.canvas:meta-p state) (member key '(#\w #\W)))
                            (and (lol.canvas:ctrl-p state) (eq key :insert))))
       (editor-copy (pane-tv p)) nil)                   ; copying is reading, so allowed
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
              ;; C-c C-a — Apply it: only meaningful when the subject IS a function.
              ((member key '(#\a #\A)) (apply-function-pane p))))
          nil)
         ((and (lol.canvas:ctrl-p state) (member key '(#\c #\C)))
          (setf *pending-prefix* :c-c) nil)
         ;; -> moves focus into the right-hand column (see FOCUS-SIDE). <- stays
         ;; "back", so the two arrows are not a pair here: -> goes deeper into the
         ;; frame you are looking at, <- leaves it.
         ((eq key :right) (focus-side p))
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
    (if (pane-tv p)
        (lol.textview:tv-wheel (pane-tv p) lines)
        (setf (pane-top p) (max 0 (+ (pane-top p) lines))
              (pane-follow p) nil))))

;;; ---- press and drag: selecting text with the pointer ------------------------
;;; A click that places the caret in a text view also arms a DRAG: while the button
;;; stays down, pointer motion extends the selection from that caret to the pointer
;;; (TV-DRAG — the anchor stays where the press began). The backend tells us the
;;; button is still held; this file only remembers WHICH view the press landed in,
;;; because by the time the pointer moves it may be over a different pane.

(defvar *drag-tv* nil
  "The text view the last left press placed the caret in, or NIL: what a drag
   extends. Set by BROWSER-CLICK, cleared by the next click.")
(defvar *drag-origin* nil
  "Where that press happened, (X . Y), until the pointer has moved far enough to be
   a drag; NIL once it is one. See +DRAG-SLOP+.")
(defparameter +drag-slop+ 3
  "Pixels the pointer must travel, held down, before it selects. A clickpad's press
   rolls the fingertip, which nudges the pointer a pixel or two; without this slop
   every press would select a character or two instead of just placing the caret.")

(defun browser-drag (x y)
  "The left button is still held and the pointer is at X,Y: extend the selection in
   the view the press landed in. Returns T when the frame changed (so the caller
   redraws), NIL when there is no drag or the pointer is still inside the slop."
  (when *drag-tv*
    (when (and *drag-origin*
               (<= (abs (- x (car *drag-origin*))) +drag-slop+)
               (<= (abs (- y (cdr *drag-origin*))) +drag-slop+))
      (return-from browser-drag nil))
    (setf *drag-origin* nil)
    (lol.textview:tv-drag *drag-tv* x y)
    t))

(defun arm-drag (tv x y)
  "A press just placed TV's caret at X,Y: a drag from here selects in TV."
  (setf *drag-tv* tv *drag-origin* (cons x y)))

(defun browser-click (x y &optional ctrl super)
  "Click: the top tab strip activates a trail; a breadcrumb bar pops to that level; a
   list item drills (SuperL-click opens it in a NEW trail instead); Ctrl-click on a
   symbol in a source pane jumps to its definition. Uses the geometry DRAW-BROWSER
   recorded (*tab-rects*, *crumb-rects*, *content-top*), so click and draw can never
   disagree.

   In a side-by-side, a plain click in the half that does NOT have keyboard focus is
   a focus move and nothing more — SuperL-o with a pointer. It still positions you
   inside that half (the caret in text, the selection on a row), because that is what
   you pointed at, but it does not drill: crossing the split is not navigation, and a
   focus move that navigated would throw away the split you were working in. A click
   in the half that already has focus behaves as it always did, and Ctrl-click /
   SuperL-click keep their meanings either side, because a modified click is an
   explicit command rather than a 'put me over there'."
  (setf *drag-tv* nil *drag-origin* nil)    ; only a caret placed below re-arms it
  ;; the tab strip owns the top +bar-h+ band: + opens a fresh Workspace, the active
  ;; tab's × closes it, else a tab click activates that trail. (i x0 . x1) per entry.
  (when (< y +bar-h+)
    (cond
      ((and *tray-clip-rect* (<= (car *tray-clip-rect*) x (cdr *tray-clip-rect*)))
       (open-clipboard-manager))
      ((and *tab-plus-rect* (<= (car *tab-plus-rect*) x (cdr *tab-plus-rect*)))
       (new-trail (subj-workspace)))
      ((find-if (lambda (e) (<= (cadr e) x (cddr e))) *tab-close-rects*)
       ;; close THAT tab, not merely the focused one
       (let ((hit (find-if (lambda (e) (<= (cadr e) x (cddr e))) *tab-close-rects*)))
         (activate-trail (car hit))
         (close-trail)))
      (t (let ((tab (find-if (lambda (e) (<= (cadr e) x (cddr e))) *tab-rects*)))
           (when tab (activate-trail (car tab))))))
    (return-from browser-click))
  ;; a breadcrumb bar? each entry is (pane-index . y-top); test its y-top (cdr).
  (let ((hit (find-if (lambda (e) (<= (cdr e) y (+ (cdr e) +bar-h+))) *crumb-rects*))
        (p (current-pane)))
    (cond
      (hit (trail-goto (car hit)))
      ;; text pane: Ctrl-click jumps to definition; a plain click places the caret
      ;; (and arms a drag) — in a read-only pane too, because SELECTING text to copy
      ;; it is reading, not editing. The caret is simply not drawn there.
      ((pane-tv p)
       (cond
         (ctrl (jump-to-definition (pane-tv p) x y))
         ((lol.textview:tv-hit-p (pane-tv p) x y)
          (lol.textview:tv-click (pane-tv p) x y)
          (arm-drag (pane-tv p) x y))))
      ;; dispatch matrix: a cell drills its effective-method onion
      ((eq (view-kind (pane-view p)) :matrix)
       (matrix-cell-click p x y))
      ;; the stage: a click in the stage's OWN half selects the instance under the
      ;; pointer (a miss clears the selection) and brings focus back to that half.
      ;; The half test is the point — a stage with its Workspace beside it (§3b) is a
      ;; side-by-side, and this clause used to swallow every click in the frame,
      ;; including the ones aimed at the Workspace. That made the other half
      ;; unreachable by pointer: SuperL-o was the only way in, and clicking to place
      ;; the caret did nothing at all.
      ((and (eq (view-kind (pane-view p)) :stage)
            (not (and *split-x* (>= x *split-x*))))
       (stage-click p x y)
       (setf *editing-side* nil))
      ;; nav-split preview column: a matrix preview is LIVE — clicking a cell drills its
      ;; effective-method onion straight from here (no need to open the matrix first); a
      ;; source preview jumps on Ctrl-click; otherwise the click opens the previewed
      ;; selection for real (SuperL-click in a new trail).
      ((and *split-x* (>= x *split-x*) (>= y *content-top*))
       (cond
         ;; ⇧↵ editable side pane: a click focuses it and places the caret (Ctrl jumps).
         ((pane-side p)
          (setf *editing-side* t)
          (let* ((side (pane-side p)) (tv (pane-tv side)))
            (cond
              ((and tv (lol.textview:tv-hit-p tv x y))
               (if ctrl
                   (jump-to-definition tv x y)
                   (progn (lol.textview:tv-click tv x y) (arm-drag tv x y))))
              ;; a list in the right column: select the row that was drawn there,
              ;; read back from its OWN hit map (see *SIDE-ROWS*)
              ((null tv)
               (let ((i (lol.widgets:hit-row *side-rows* y)))
                 (when i (set-pane-sel side i)))))))
         (t
          (let ((sel (selected-item p))
                (pv *preview-pane*))
            (cond
              ((and pv (eq (view-kind (pane-view pv)) :matrix))
               (or (matrix-cell-click pv x y)               ; a cell -> its onion
                   (and sel (item-subject sel) (trail-push (item-subject sel)))))  ; miss -> open matrix
              ((and ctrl pv (pane-tv pv)) (jump-to-definition (pane-tv pv) x y))
              ;; A plain click on the preview means "put me over there" — exactly the
              ;; promotion -> performs, and until now the only focus state the pointer
              ;; could not reach. It does not drill: the row it would have opened is
              ;; one click away on the left, and crossing the split is a focus move.
              ;; FOCUS-SIDE is NIL when there is nothing previewable, so a click on an
              ;; empty column falls through rather than swallowing itself.
              ((and (not super) (focus-side p)))
              ((and sel (item-subject sel))
               (if super (new-trail (item-subject sel)) (trail-push (item-subject sel)))))))))
      ;; else an item in the current list view — SuperL-click opens it in a NEW trail; a
      ;; plain click here also returns focus to the list (away from any side pane).
      ((>= y *content-top*)
       (let ((came-from-side *editing-side*))
         ;; UNFOCUS-SIDE rather than a bare (setf *editing-side* nil): a side the
         ;; pointer or -> merely FOCUSED goes away again and you are looking at the
         ;; live preview you started from, while one you PINNED with <S-Return> stays.
         ;; Same rule C-g follows, so the two ways back agree.
         (unfocus-side p)
         ;; which row was actually painted here? *list-rows* answers; a click on a group
         ;; header hits no row and is correctly inert.
         (let* ((items (view-items (pane-view p)))
                (i (lol.widgets:hit-row *list-rows* y)))
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
                       (trail-push (item-subject it)))))))))))))

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
    (when *stage-rect*
      (destructuring-bind (x y w vh) *stage-rect*
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
         (token (or (click-token line c) (and (> c 0) (click-token line (1- c)))))
         (sym (resolve-symbol token)))
    (list :kind :text :pane pane :tv tv
          :editable (or (editable-pane-p pane) (workspace-pane-p pane))
          :token token :symbol sym :line l :col c)))

(defun row-target (pane hits y)
  "A target for a click at height Y on PANE's list, whose painted rows are HITS."
  (let* ((items (view-items (pane-view pane)))
         (i (lol.widgets:hit-row hits y))
         (it (and i (< i (length items)) (nth i items))))
    (when it
      (list :kind :row :pane pane :index i :item it :subject (item-subject it)))))

(defun pointer-target (x y)
  "What is under X,Y, as a plist with a :KIND of :TAB :TEXT :ROW :SPRITE or :PANE
   (the pane's own subject, when nothing more specific was hit). Never NIL inside the
   frame: pointing at empty space still points at the pane you are in."
  (let ((p (current-pane)))
    (flet ((pane-itself (pane) (list :kind :pane :pane pane :subject (pane-subject pane))))
      (cond
        ((< y +bar-h+)
         (let ((tab (find-if (lambda (e) (<= (cadr e) x (cddr e))) *tab-rects*)))
           (and tab (list :kind :tab :index (car tab)))))
        ;; the right-hand column: a pinned side pane, or the read-only preview
        ((and *split-x* (>= x *split-x*) (>= y *content-top*))
         (let ((side (or (pane-side p) *preview-pane*)))
           (cond ((null side) (pane-itself p))
                 ((and (pane-tv side) (lol.textview:tv-hit-p (pane-tv side) x y))
                  (text-target side x y))
                 ((null (pane-tv side))
                  (or (row-target side *side-rows* y) (pane-itself side)))
                 (t (pane-itself side)))))
        ((and (pane-tv p) (lol.textview:tv-hit-p (pane-tv p) x y))
         (text-target p x y))
        ((eq (view-kind (pane-view p)) :stage)
         (let ((b (stage-body-at p x y)))
           (if b
               (list :kind :sprite :pane p :subject (body-obj b))
               (pane-itself p))))
        ((and (null (pane-tv p)) (not (eq (view-kind (pane-view p)) :matrix)))
         (or (row-target p *list-rows* y) (pane-itself p)))
        (t (pane-itself p))))))

(defun target-object (target)
  "The Lisp object a TARGET stands for — what Inspect would open, what the halo is
   ABOUT. A word in text stands for its symbol; a row for its subject."
  (case (getf target :kind)
    (:text (getf target :symbol))
    ((:row :sprite :pane) (getf target :subject))
    (t nil)))

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

(defun menu-entry (label action &key key group)
  (list :label label :action action :key key :group group))

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
             (list (when (and editable sel) (menu-entry "Cut" (lambda () (editor-cut tv)) :key "C-w" :group g))
                   (when sel (menu-entry "Copy" (lambda () (editor-copy tv)) :key "M-w" :group g))
                   (when editable (menu-entry "Paste" (lambda () (editor-paste tv)) :key "C-y" :group g))
                   (when editable (menu-entry "Paste the selection" (lambda () (editor-paste-primary tv))
                                              :key "middle" :group g))
                   (menu-entry "Select all" (lambda () (editor-select-all tv)) :key "C-x h" :group g)))
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
             (menu-entry "Open in a new tab" (lambda () (new-trail subject)) :key "SuperL-ret" :group "open")
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
        (menu-entry "Find anything..." (lambda () (when (fboundp 'run-spotter-fb) (funcall 'run-spotter-fb)))
                    :key "SuperL-spc" :group "environment")
        (menu-entry "Clipboard history..." (lambda () (open-clipboard-manager))
                    :key "SuperL-h" :group "environment")
        (menu-entry "Keys" (lambda () (when (fboundp 'run-cheatsheet-fb) (funcall 'run-cheatsheet-fb)))
                    :key "SuperL-?" :group "environment")))

(defun context-menu-entries (target)
  "Everything the right-button menu offers for TARGET."
  (append
   (case (getf target :kind)
     (:text   (text-menu-entries target))
     (:row    (row-menu-entries target))
     (:sprite (append (list (menu-entry "Inspect" (lambda () (trail-push (getf target :subject)))
                                        :group "instance"))
                      (command-entries (getf target :subject) :menu "instance")))
     (:tab    (let ((i (getf target :index)))
                (list (menu-entry "Close tab" (lambda () (activate-trail i) (close-trail)) :group "tab")
                      (menu-entry "Move to slot 1" (lambda () (activate-trail i) (swap-trail 1)) :group "tab"))))
     (:pane   (command-entries (getf target :subject) :menu)))
   (environment-menu-entries)))

(defun entry-rows (entries)
  "Menu entries -> widget rows (label, key hint as the detail, group as a section)."
  (map 'vector (lambda (e)
                 (lol.widgets:make-row (getf e :label) :detail (getf e :key)
                                       :group (getf e :group) :tint *ui-text*))
       entries))

(defvar *menu-rect* nil "The context menu card (x0 y0 x1 y1), for click routing.")
(defvar *menu-hits* '() "The context menu's painted rows — see HIT-ROW.")

(defun draw-context-menu (canvas w h entries sel x y)
  "The browser, then the right-button menu as a card whose corner is at X,Y — moved
   left/up when it would run off the screen, the way every desktop does it."
  (draw-browser canvas w h)
  (let* ((rows (entry-rows entries))
         (nslots (+ (length rows)
                    (length (remove-duplicates (map 'list #'lol.widgets:row-group rows)
                                               :test #'equal))))
         (cw 320)
         (ch (min (- h 40) (+ 32 (* nslots +row-h+))))
         (cx (max 4 (min x (- w cw 8))))
         (cy (max (+ +bar-h+ 2) (min y (- h ch 8)))))
    (setf *menu-rect* (list cx cy (+ cx cw) (+ cy ch)))
    (let ((body (lol.widgets:draw-card canvas cx cy cw ch)))
      (multiple-value-bind (top hits)
          (lol.widgets:draw-list canvas (+ cx 4) (- body 18) (- cw 8) (- ch 12) rows
                                 :sel sel :top 0 :follow-sel t)
        (declare (ignore top))
        (setf *menu-hits* hits)))))

(defun menu-entry-at (x y)
  "The context menu entry index at X,Y, :OUTSIDE when X,Y is off the card, or NIL."
  (if (or (null *menu-rect*)
          (destructuring-bind (x0 y0 x1 y1) *menu-rect*
            (not (and (<= x0 x x1) (<= y0 y y1)))))
      :outside
      (lol.widgets:hit-row *menu-hits* y)))

;;; ---- the halo (middle button) ---------------------------------------------
;;; Smalltalk's halo: middle-click a thing and a ring of HANDLES appears around it,
;;; each one a thing you can do to THAT object — browse the code that made it, inspect
;;; it, see who uses it. Morphic's handles are coloured dots around the morph's
;;; bounds; ours ring the point you clicked, because the things here (a word, a row, a
;;; sprite) are small. The handles come from the same COMMANDS-FOR, its :HALO surface.

(defparameter +halo-radius+ 74 "Distance of the handles from the clicked point.")

(defun halo-entries (object)
  "The halo's handles for OBJECT: Source (the code that produced it) and Inspect
   always lead, then the object's own :halo commands, then Copy."
  (let* ((src (source-subject object))
         (own (command-entries object :halo "halo"))
         (base (remove nil
                       (list (when src (menu-entry "Source" (lambda () (trail-push src)) :group "halo"))
                             (menu-entry "Inspect" (lambda () (trail-push object)) :group "halo")
                             (when (and (symbolp object) object (find-class object nil))
                               (menu-entry "Class" (lambda () (trail-push (find-class object))) :group "halo"))))))
    (append base
            (remove-if (lambda (e) (find (getf e :label) base :key (lambda (b) (getf b :label))
                                                              :test #'string-equal))
                       own)
            (list (menu-entry "Copy" (lambda () (copy-to-clipboard
                                                 (let ((*print-length* 20) (*print-level* 4))
                                                   (prin1-to-string object))))
                              :group "halo")))))

(defvar *halo-handles* '() "((index cx cy x0 x1) …) of the handles drawn last.")
(defvar *halo-title* nil
  "What the halo calls its object — the row's label or the word clicked, which is how
   you knew it on screen. NIL falls back to the printed object, which for a browser
   subject is a struct printout nobody recognises.")

(defun halo-handle-xy (i n cx cy)
  "Where handle I of N sits around CX,CY: evenly round a circle, the first at the top."
  (let ((th (- (/ (* 2 pi i) n) (/ pi 2))))
    (values (round (+ cx (* +halo-radius+ (cos th))))
            (round (+ cy (* +halo-radius+ (sin th)))))))

(defun draw-halo (canvas w h object entries sel cx cy)
  "The browser, dimmed, with the halo's handles ringed round CX,CY: a coloured dot per
   handle and its name beside it, the selected one lit, and the object named at the
   centre so you can see WHAT the halo is about before you choose."
  (draw-browser canvas w h)
  (lol.widgets:scrim canvas 150)
  (let* ((n (length entries))
         (cx (max (+ +halo-radius+ 90) (min cx (- w +halo-radius+ 90))))
         (cy (max (+ +bar-h+ +halo-radius+ 14) (min cy (- h +halo-radius+ 30)))) ; below the tabs
         (name (lol.widgets:clip-text (or *halo-title*
                                          (let ((*print-length* 5) (*print-level* 2))
                                            (prin1-to-string object)))
                                      28))
         (nw (lol.canvas:string-px *bfont* name)))
    (setf *halo-handles* '())
    (lol.canvas:draw-circle canvas cx cy (- +halo-radius+ 22) *ui-rule*)
    (lol.canvas:fill-rect canvas (- cx (floor nw 2) 6) (- cy 9) (+ nw 12) 18 *ui-panel*)
    (lol.canvas:draw-string canvas *bfont* name (- cx (floor nw 2)) (- cy 6) *ui-accent*)
    (loop for e in entries for i from 0
          do (multiple-value-bind (hx hy) (halo-handle-xy i n cx cy)
               (let* ((label (format nil "~d ~a" (1+ i) (getf e :label)))
                      (lw (lol.canvas:string-px *bfont* label))
                      (left (< hx (- cx 8)))                ; labels point away from the centre
                      (lx (if left (- hx 12 lw) (+ hx 12)))
                      (on (= i sel)))
                 (lol.canvas:draw-circle canvas hx hy 8
                                         (concept-color (case i (0 :class) (1 :variable) (2 :function)
                                                          (3 :generic-function) (t :restart)))
                                         :fill t)
                 (when on (lol.canvas:draw-circle canvas hx hy 11 *ui-accent*))
                 (lol.canvas:fill-rect canvas (- lx 3) (- hy 8) (+ lw 6) 16 (if on *ui-hi* *ui-panel*))
                 (lol.canvas:draw-string canvas *bfont* label lx (- hy 6) (if on *ui-accent* *ui-text*))
                 (push (list i hx hy (- (min lx hx) 12) (+ (max (+ lx lw) hx) 12)) *halo-handles*))))))

(defun halo-handle-at (x y)
  "The halo handle index at X,Y (its dot or its label), :OUTSIDE elsewhere."
  (let ((hit (find-if (lambda (h)
                        (destructuring-bind (i hx hy x0 x1) h
                          (declare (ignore i hx))
                          (and (<= x0 x x1) (<= (- hy 10) y (+ hy 10)))))
                      *halo-handles*)))
    (if hit (first hit) :outside)))

;;; ---- the gestures, backend-blind ------------------------------------------
;;; The backends own the modal loops (fb-browser.lisp: RUN-CHOOSER-FB); these decide
;;; WHAT to show. *CHOOSER* is the hook a backend installs, like *VALUE-PROMPT*.

(defvar *chooser* nil
  "A hook: (funcall *chooser* KIND ENTRIES X Y &optional OBJECT) shows a modal menu
   (:menu) or halo (:halo) and returns the chosen entry, or NIL. Installed by the
   framebuffer backend; NIL elsewhere, and then the gestures say so.")

(defun choose-and-run (kind entries x y &optional object)
  "Offer ENTRIES through *CHOOSER* and run the one picked."
  (cond
    ((null entries) nil)
    ((null *chooser*) (setf *browser-status* "no menus on this backend"))
    (t (let ((e (funcall *chooser* kind entries x y object)))
         (when e
           (handler-case (funcall (getf e :action))
             (serious-condition (c)
               (setf *browser-status* (format nil "~a failed: ~a" (getf e :label) c)))))))))

(defun browser-context-menu (x y)
  "Right button at X,Y: the menu for whatever is there."
  (choose-and-run :menu (context-menu-entries (pointer-target x y)) x y))

(defun browser-middle-click (x y &optional force-halo)
  "Middle button at X,Y. In EDITABLE text it is X's middle-click paste: the PRIMARY
   selection (whatever you last selected with the mouse) goes in at the pointer.
   Anywhere else — and in text when FORCE-HALO (SuperL held) or there is nothing
   selected to paste — it opens the HALO on the thing under the pointer.

   That is the disambiguation, and it follows what each place is FOR: text you can
   edit is where a paste can land, so the paste wins there; a row, a sprite, a word in
   read-only text cannot receive a paste, so there the button is free for the halo."
  (let ((target (pointer-target x y)))
    (if (and (eq (getf target :kind) :text) (getf target :editable)
             (not force-halo) (primary-selection))
        (paste-primary-at target)
        (let ((object (target-object target))
              (*halo-title* (case (getf target :kind)
                              (:row (item-label (getf target :item)))
                              (:text (getf target :token))
                              (t nil))))
          (if object
              (choose-and-run :halo (halo-entries object) x y object)
              (setf *browser-status* "nothing here to put a halo on"))))))

(defun open-modules ()
  "The module index — every definition by the file it came from. Focus its tab if one
   is open, else open one. (It used to be the environment's first tab; the environment
   now opens on the Workspace alone.)"
  (let ((i (position-if (lambda (trail) (typep (pane-subject (first trail)) 'subj-modules))
                        *trails*)))
    (if i (activate-trail i) (new-trail (subj-modules)))))

(defun keyboard-context-menu ()
  "The Menu key, or Shift-F10: the right-button menu without a pointer, anchored at
   the caret in a text pane or at the selected row in a list. The keyboard-first rule
   (doc/code-browser.org §1): anything the pointer can reach, a key can too."
  (let* ((p (current-pane))
         (tv (cond ((and *editing-side* (pane-side p)) (pane-tv (pane-side p)))
                   (t (pane-tv p)))))
    (multiple-value-bind (x y)
        (if tv
            (values (lol.textview::x-of-col tv (lol.textview:tv-point-col tv))
                    (+ (lol.textview::tv-y tv) (lol.textview::tv-pad tv) 4
                       (* (- (lol.textview:tv-point-line tv) (lol.textview::tv-scroll tv))
                          (lol.textview:tv-line-height tv))))
            (let ((hit (find (pane-sel p) *list-rows* :key #'cdr)))
              (values 40 (if hit (+ (car hit) 4) *content-top*))))
      (browser-context-menu x y))))

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
    (lol.textview:tv-move-point tv (getf target :line) (getf target :col))
    (editor-paste-primary tv)))

(defun browser-drag-end ()
  "The button came up after a press in text: if the drag selected something, that
   selection becomes PRIMARY and enters the history — once, now, rather than on
   every motion event, or the history would fill with every intermediate stretch."
  (when *drag-tv*
    (let ((text (editor-region-text *drag-tv*)))
      (when (and text (null *drag-origin*))          ; it really was a drag
        (lol.clipboard:clip-select text)
        (clip-status "selected" text)))))

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
         (q (if (and *editing-side* (pane-side p)) (pane-side p) p)))
    (and (pane-tv q) (or (editable-pane-p q) (workspace-pane-p q)) (pane-tv q))))

;;; ---- the clipboard manager (SuperL-h): Parcellite's history, as a palette -----

(defvar *clip-hits* '() "The clipboard manager's painted rows — see HIT-ROW.")
(defvar *clip-rect* nil "The manager card (x0 y0 x1 y1).")

(defun draw-clipboard-card (canvas w h query results sel)
  "The browser, dimmed, and the history as a card: the filter line, one row per
   entry (newest first, numbered by its place in the history), the current one
   marked. RESULTS is (INDEX . STRING) pairs from CLIP-FILTER."
  (draw-browser canvas w h)
  (let* ((cw (min 760 (- w 80)))
         (cx (floor (- w cw) 2))
         (cy (+ +bar-h+ 14))
         (maxrows (max 3 (min (max 1 (length results)) (floor (- h cy 96) +row-h+))))
         (ch (+ 24 20 8 (* maxrows +row-h+) 24))
         (maxc (max 10 (floor (- cw 120) (lol.canvas:font-cw *bfont*))))
         (rows (map 'vector
                    (lambda (r)
                      (lol.widgets:make-row (lol.clipboard:clip-label (cdr r) maxc)
                                            :detail (if (zerop (car r)) "current"
                                                        (format nil "#~d" (car r)))
                                            :tint *ui-text*
                                            :stripe (if (zerop (car r)) *ui-accent* nil)))
                    results)))
    (setf *clip-rect* (list cx cy (+ cx cw) (+ cy ch)))
    (let* ((body (lol.widgets:draw-card canvas cx cy cw ch
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
          (multiple-value-bind (top hits)
              (lol.widgets:draw-list canvas (+ cx 6) ly (- cw 12) (* maxrows +row-h+) rows
                                     :sel sel :top 0 :follow-sel t)
            (declare (ignore top))
            (setf *clip-hits* hits)))
      (draw-hints canvas (+ cx 12) (+ cy ch -16) (- cw 16)
                  '(("up/dn" . "move") ("ret" . "make current + paste")
                    ("Del" . "forget it") ("type" . "filter") ("Esc" . "close"))))))

(defun clipboard-row-at (x y)
  "The manager's row index at X,Y, :OUTSIDE off the card, or NIL."
  (if (or (null *clip-rect*)
          (destructuring-bind (x0 y0 x1 y1) *clip-rect*
            (not (and (<= x0 x x1) (<= y0 y y1)))))
      :outside
      (lol.widgets:hit-row *clip-hits* y)))

(defun clipboard-choose (index)
  "The manager's Enter: entry INDEX becomes current, and goes in at the caret of
   the focused editor if there is one (Parcellite pastes on selection too)."
  (lol.clipboard:clip-promote index)
  (let ((tv (focused-editor)))
    (if tv
        (editor-insert tv (lol.clipboard:clip-current))
        (clip-status "current" (lol.clipboard:clip-current)))))

(defun open-clipboard-manager ()
  "SuperL-h, or the tray's clipboard icon."
  (if (fboundp 'run-clipboard-fb)
      (funcall 'run-clipboard-fb)
      (setf *browser-status* "the clipboard manager needs the framebuffer backend")))
