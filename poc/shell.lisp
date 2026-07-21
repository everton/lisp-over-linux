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

;;; palette (the tool's own colours, from scene.lisp)
(defparameter *sh-bg*     (lol.canvas:rgb #x14 #x16 #x1a))
(defparameter *sh-panel*  (lol.canvas:rgb #x1b #x1e #x24))
(defparameter *sh-hi*     (lol.canvas:rgb #x24 #x29 #x31))
(defparameter *sh-rule*   (lol.canvas:rgb #x2e #x34 #x3d))
(defparameter *sh-text*   (lol.canvas:rgb #xd6 #xd3 #xc8))
(defparameter *sh-dim*    (lol.canvas:rgb #x6b #x72 #x7d))
(defparameter *sh-accent* (lol.canvas:rgb #x7f #xdb #xca))
(defparameter *sh-sel*    (lol.canvas:rgb #x2f #x4f #x4a))
(defparameter *sh-amber*  (lol.canvas:rgb #xff #x9f #x43))

(defvar *bfont* nil "The browser font (a lol.canvas:font), set at startup.")
(defvar *trail* '() "The current trail: a list of PANEs, root first, deepest last.")

(defstruct pane
  subject view
  (sel 0)                 ; selected item index (list views)
  (top 0)                 ; first visible item (list scrolling)
  tv)                     ; a lol.textview for :source/:reference panes, else NIL

(defun open-pane (subject)
  "Build a PANE for SUBJECT: present it, and for text views wrap the body in a
   textview (reusing its lexer + layout). A :SOURCE view (your code) is EDITABLE —
   it gets focus and Accept; a :REFERENCE view (a builtin) is read-only."
  (let ((v (present subject)))
    (make-pane
     :subject subject :view v
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

(defun trail-root (subject) (setf *trail* (list (open-pane subject))))
(defun trail-push (subject) (setf *trail* (append *trail* (list (open-pane subject)))))
(defun trail-pop  () (when (cdr *trail*) (setf *trail* (butlast *trail*))))
(defun trail-goto (n) (when (< n (length *trail*)) (setf *trail* (subseq *trail* 0 (1+ n)))))
(defun current-pane () (car (last *trail*)))

(defun selected-item (pane)
  (let ((items (view-items (pane-view pane))))
    (and items (< (pane-sel pane) (length items)) (nth (pane-sel pane) items))))

;;; ---- geometry: where each breadcrumb bar lives (for click hit-testing) ----

(defparameter +bar-h+ 20)
(defparameter +row-h+ 17)
(defparameter +list-page+ 12 "Rows a list pane moves its selection on PageUp/PageDown.")
(defvar *crumb-rects* '() "alist (pane-index . y-top) for the collapsed bars, set on draw.")
(defvar *content-top* 0  "screen-y where the expanded pane's body starts, set on draw.")
(defvar *pending-prefix* nil
  "The pending chord prefix in an editable pane: NIL, :c-c (after C-c) or :c-x
   (after C-x). The next key completes the chord — C-c C-c Accept, C-x C-e Do it,
   C-c C-p Print it, C-c C-i Inspect it.")
(defvar *pending-meta* nil
  "T after a lone Esc — the Emacs Meta prefix (§4¾). The next key is Meta-modified,
   so Esc . jumps to definition just like Alt-. / M-. would. Sticky for one key,
   cleared on the next keystroke.")
(defvar *browser-status* nil "A transient status message (e.g. Accept result), or NIL.")

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

(defun draw-item (canvas x y w item selected)
  (when selected
    (lol.canvas:fill-rect canvas x (- y 2) w +row-h+ *sh-sel*)
    (lol.canvas:fill-rect canvas x (- y 2) 2 +row-h+ *sh-accent*))
  (let* ((label (item-label item))
         (pen (lol.canvas:draw-string canvas *bfont* label (+ x 8) y
                                       (if selected *sh-accent* *sh-text*))))
    (when (item-detail item)
      (setf pen (lol.canvas:draw-string canvas *bfont* (format nil "  ~a" (item-detail item))
                                         pen y *sh-dim*)))
    (when (item-subject item)                     ; a drill affordance
      (lol.canvas:draw-string canvas *bfont* ">"
                              (- (+ x w) 16) y
                              (if (eq (item-disposition item) :in-place) *sh-dim* *sh-amber*)))))

(defun draw-list-body (canvas pane x y w h)
  "Render a list view's items with selection + scrolling."
  (let* ((items (view-items (pane-view pane)))
         (rows (max 1 (floor h +row-h+)))
         (sel (pane-sel pane)))
    ;; keep the selection in view
    (when (< sel (pane-top pane)) (setf (pane-top pane) sel))
    (when (>= sel (+ (pane-top pane) rows)) (setf (pane-top pane) (- sel rows -1)))
    (loop for i from (pane-top pane) below (min (length items) (+ (pane-top pane) rows))
          for iy from y by +row-h+
          do (draw-item canvas x (+ iy 4) w (nth i items) (= i sel)))
    (when (> (length items) rows)                 ; a scroll hint
      (lol.canvas:draw-string canvas *bfont*
                              (format nil "~a-~a/~a" (1+ (pane-top pane))
                                      (min (length items) (+ (pane-top pane) rows)) (length items))
                              (- (+ x w) 90) (+ y h -14) *sh-dim*))))

;;; ---- the clickable dispatch matrix (§3) ----------------------------------

(defvar *matrix-cells* '()
  "alist ((x y w h row-name col-name) …) of the dispatch grid's clickable cells,
   rebuilt each draw so a click and the draw can never disagree (like *crumb-rects*).")
(defparameter +mx-cell-w+ 78)
(defparameter +mx-row-h+  18)

(defun mx-short (name maxlen)
  "A class/specializer name trimmed to MAXLEN chars for a matrix header or cell."
  (let ((s (string-downcase (princ-to-string name))))
    (if (> (length s) maxlen) (concatenate 'string (subseq s 0 (1- maxlen)) "…") s)))

(defun draw-matrix-body (canvas pane x y w h)
  "Render a 2-axis dispatch matrix as a clickable grid: the row/col headers are the
   specializer classes, each cell shows how many PRIMARY methods cover that (row,col)
   pair (a dim 0 is a real gap — the whole point of the view). A cell click drills
   the effective-method onion for that class pair; cells are recorded in
   *matrix-cells* so the hit-test always matches what was drawn."
  (setf *matrix-cells* '())
  (let* ((m     (view-data (pane-view pane)))
         (rows  (getf m :rows))
         (cols  (getf m :cols))
         (cells (getf m :cells))
         (hdr-w 130)                                        ; row-header column width
         (x0    (+ x hdr-w))                                ; where the cell grid starts
         (ncols (max 1 (floor (- w hdr-w) +mx-cell-w+))))   ; how many columns fit
    ;; column headers
    (loop for c in cols for ci from 0 below ncols
          for cx = (+ x0 (* ci +mx-cell-w+))
          do (lol.canvas:draw-string canvas *bfont* (mx-short c 12) (+ cx 2) y *sh-amber*))
    (when (> (length cols) ncols)
      (lol.canvas:draw-string canvas *bfont* (format nil "…+~a" (- (length cols) ncols))
                              (- (+ x w) 44) y *sh-dim*))
    ;; rows, each a header + its cells
    (loop for r in rows for cellrow in cells for ri from 0
          for ry = (+ y +mx-row-h+ (* ri +mx-row-h+))
          while (< ry (- (+ y h) 4))
          do (lol.canvas:draw-string canvas *bfont* (mx-short r 20) x ry *sh-text*)
             (loop for cell in cellrow for c in cols for ci from 0 below ncols
                   for cx = (+ x0 (* ci +mx-cell-w+))
                   for n = (length cell)
                   do (lol.canvas:draw-rect canvas cx (- ry 2) +mx-cell-w+ +mx-row-h+ *sh-rule*)
                      (lol.canvas:draw-string canvas *bfont*
                        (if (zerop n) "·" (princ-to-string n))
                        (+ cx (floor +mx-cell-w+ 2) -3) ry
                        (if (zerop n) *sh-dim* *sh-accent*))
                      (push (list cx (- ry 2) +mx-cell-w+ +mx-row-h+ r c) *matrix-cells*)))))

(defun matrix-cell-click (pane px py)
  "A click on a dispatch-grid cell -> drill the effective-method onion for that
   (row-class, col-class) pair, building the full argument tuple (T elsewhere) from
   the matrix's arity and axis positions. NIL when the click missed every cell."
  (let ((hit (find-if (lambda (cell)
                        (destructuring-bind (cx cy cw ch r c) cell
                          (declare (ignore r c))
                          (and (<= cx px (+ cx cw)) (<= cy py (+ cy ch)))))
                      *matrix-cells*)))
    (when hit
      (destructuring-bind (cx cy cw ch r c) hit
        (declare (ignore cx cy cw ch))
        (let* ((m (view-data (pane-view pane)))
               (arity (or (getf m :arity) 2))
               (rp (getf m :row-pos)) (cp (getf m :col-pos))
               (tuple (loop for i below arity
                            collect (cond ((eql i rp) r) ((eql i cp) c) (t t)))))
          (trail-push (subj-onion (getf m :name) tuple))
          t)))))

(defun draw-crumb (canvas y w pane index)
  "A collapsed ancestor bar: subject title + what was picked inside it."
  (lol.canvas:fill-rect canvas 8 y (- w 16) +bar-h+ *sh-hi*)
  (lol.canvas:draw-string canvas *bfont* ">" 16 (+ y 3) *sh-dim*)
  (lol.canvas:draw-string canvas *bfont* (view-title (pane-view pane)) 34 (+ y 3) *sh-text*)
  (let ((sel (selected-item pane)))
    (when sel
      (let ((s (format nil "picked ~a" (item-label sel))))
        (lol.canvas:draw-string canvas *bfont* s
                                (- w 8 (lol.canvas:string-px *bfont* s)) (+ y 3) *sh-accent*))))
  (push (cons index y) *crumb-rects*))

(defun draw-browser (canvas w h)
  (setf *crumb-rects* '())
  (lol.canvas:fill-rect canvas 0 0 w h *sh-bg*)
  ;; title strip
  (lol.canvas:fill-rect canvas 0 0 w +bar-h+ *sh-hi*)
  (lol.canvas:draw-string canvas *bfont* "lisp-over-linux · browser" 10 3 *sh-accent*)
  (let* ((ancestors (butlast *trail*))
         (cur (current-pane))
         (y (+ +bar-h+ 4)))
    ;; ancestor breadcrumb bars
    (loop for p in ancestors for i from 0
          do (draw-crumb canvas y w p i) (incf y (+ +bar-h+ 3)))
    ;; the expanded current pane
    (let ((body-y (+ y +bar-h+ 4)) (body-h (- h y +bar-h+ 4 24)))
      (setf *content-top* body-y)                 ; remember it for click hit-testing
      (lol.canvas:fill-rect canvas 8 y (- w 16) (- h y 24) *sh-panel*)
      (lol.canvas:draw-rect canvas 8 y (- w 16) (- h y 24) *sh-rule*)
      (lol.canvas:fill-rect canvas 9 y (- w 18) +bar-h+ *sh-hi*)
      (lol.canvas:draw-string canvas *bfont* "v" 16 (+ y 3) *sh-accent*)
      (lol.canvas:draw-string canvas *bfont* (view-title (pane-view cur)) 34 (+ y 3) *sh-accent*)
      (cond
        ((pane-tv cur)                              ; source / reference: the textview
         (lol.textview:tv-geometry (pane-tv cur) 12 body-y (- w 24) body-h)
         (lol.textview:tv-draw canvas (pane-tv cur)))
        ((eq (view-kind (pane-view cur)) :matrix)   ; the clickable dispatch grid
         (draw-matrix-body canvas cur 12 body-y (- w 24) body-h))
        (t (draw-list-body canvas cur 12 body-y (- w 24) body-h))))
    ;; status line
    (let ((cmds (ignore-errors (commands-for (pane-subject cur)))))
      (lol.canvas:fill-rect canvas 0 (- h 22) w 22 *sh-hi*)
      (if *browser-status*
          (lol.canvas:draw-string canvas *bfont* *browser-status* 10 (- h 18) *sh-accent*)
          (lol.canvas:draw-string canvas *bfont*
            (format nil "~a  SuperL-< back  q quit~@[   commands: ~a~]"
                    (cond ((workspace-pane-p cur)
                           "C-x C-e Do it  C-c C-p Print  C-c C-i Inspect  C-c C-d Debug")
                          ((editable-pane-p cur) "editing: C-c C-c accepts")
                          (t "up/dn move  ret drill"))
                    (and cmds (format nil "~{~a~^ · ~}" (mapcar #'command-label cmds))))
            10 (- h 18) *sh-dim*)))))

;;; ---- input ---------------------------------------------------------------

(defun accept-pane (pane)
  "Compile the edited source buffer into the live image + update the registry
   (accept-source). Sets *browser-status* to the outcome."
  (let ((text (lol.textview:tv-text (pane-tv pane)))
        (subj (pane-subject pane)))
    (handler-case
        (let ((name (accept-source text (subj-defn-name subj) (subj-defn-kind subj))))
          (setf *browser-status* (format nil "Accepted: ~(~a~)" name)))
      (serious-condition (c)
        (setf *browser-status* (format nil "Accept failed: ~a" c))))))

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
      ;; portable "back" that also works while editing a source pane). At the root,
      ;; there is nothing to pop, so C-g leaves the browser.
      ((and (lol.canvas:ctrl-p state) (member key '(#\g #\G)))
       (setf *pending-prefix* nil *browser-status* nil)
       (if (cdr *trail*) (progn (trail-pop) nil) :quit))
      ;; --- SuperL layer: trail navigation + the cheatsheet, over ANY pane (never the
      ;;     editor's). On the framebuffer Super now arrives via the keyboard's evdev
      ;;     (folded into STATE), so it works on the bare console too — see fb-browser. ---
      ((lol.canvas:super-p state)
       (setf *pending-prefix* nil)
       (case key
         (:left (trail-pop) (setf *browser-status* nil))   ; back
         ((#\? #\/) (when (fboundp 'run-cheatsheet-fb)      ; Super-? — every binding
                      (funcall 'run-cheatsheet-fb)))
         (#\q :quit)
         (t nil)))
      ;; --- editable panes: source (Accept) and workspace (eval verbs). Chords go
      ;;     through *pending-prefix*: C-c then {C-c Accept | C-p Print | C-i Inspect},
      ;;     C-x then C-e Do it. Any other key edits. ---
      ((or (editable-pane-p p) (workspace-pane-p p))
       (let ((c (lol.canvas:ctrl-p state)))
         (cond
           (*pending-prefix*
            (let ((prefix *pending-prefix*))
              (setf *pending-prefix* nil)
              (cond
                ((and (eq prefix :c-c) c (eql key #\c) (editable-pane-p p)) (accept-pane p))
                ((and (eq prefix :c-c) c (eql key #\p) (workspace-pane-p p)) (workspace-print-it p))
                ;; C-c C-i Inspect it: Ctrl-I *is* Tab (byte 9) on any tty, so the
                ;; console delivers this chord as C-c then :tab — accept both.
                ((and (eq prefix :c-c) (or (and c (eql key #\i)) (eq key :tab))
                      (workspace-pane-p p))
                 (workspace-inspect-it p))
                ((and (eq prefix :c-c) c (eql key #\d) (workspace-pane-p p)) (workspace-debug-it p))
                ((and (eq prefix :c-x) c (eql key #\e) (workspace-pane-p p)) (workspace-do-it p)))
              nil))
           ((and c (eql key #\c)) (setf *pending-prefix* :c-c) nil)
           ((and c (eql key #\x) (workspace-pane-p p)) (setf *pending-prefix* :c-x) nil)
           (t (lol.textview:tv-key (pane-tv p) key state) nil))))
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
         ((eq key :up)   (setf (pane-sel p) (max 0 (1- (pane-sel p)))))
         ((eq key :down) (setf (pane-sel p)
                               (min (max 0 (1- (length (view-items (pane-view p)))))
                                    (1+ (pane-sel p)))))
         ((eq key :page-up)   (setf (pane-sel p) (max 0 (- (pane-sel p) +list-page+))))
         ((eq key :page-down) (setf (pane-sel p)
                                    (min (max 0 (1- (length (view-items (pane-view p)))))
                                         (+ (pane-sel p) +list-page+))))
         ((eq key :return)
          (let ((it (selected-item p)))
            (when (and it (item-subject it)) (trail-push (item-subject it)))))
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
  "Scroll the current pane by LINES (+ down / toward the end) — the mouse wheel. A
   text pane scrolls its viewport, leaving the caret put; a list pane moves the
   selection instead (its scroll position is anchored to the selection, so the view
   follows). No-op past the ends."
  (let ((p (current-pane)))
    (if (pane-tv p)
        (lol.textview:tv-wheel (pane-tv p) lines)
        (let ((n (length (view-items (pane-view p)))))
          (when (plusp n)
            (setf (pane-sel p) (max 0 (min (1- n) (+ (pane-sel p) lines)))))))))

(defun browser-click (x y &optional ctrl)
  "Click: a breadcrumb bar pops to that level; an item in the current list drills;
   Ctrl-click on a symbol in a source pane jumps to its definition. Uses the
   geometry DRAW-BROWSER recorded (*crumb-rects*, *content-top*), so click and draw
   can never disagree."
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
      ;; else an item in the current list view
      ((>= y *content-top*)
       (let* ((items (view-items (pane-view p)))
              (i (+ (pane-top p) (floor (- y *content-top*) +row-h+))))
         (when (< i (length items))
           (setf (pane-sel p) i)
           (let ((it (nth i items)))
             (when (item-subject it) (trail-push (item-subject it))))))))))
