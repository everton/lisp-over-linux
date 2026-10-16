;;;; overlays.lisp — the browser's modal cards as morphs (doc/morphic.org, step 4).
;;;;
;;;; The Spotter, the menus and the halo, the clipboard manager and the value prompt
;;;; were once loops of their own in the framebuffer backend (fb-browser.lisp's
;;;; RUN-SPOTTER-FB, RUN-CHOOSER-FB, RUN-CLIPBOARD-FB, DBG-PROMPT-VALUE): each polled
;;;; the input itself, drew whole frames itself, and froze everything else while it was
;;;; up. Here each is an OVERLAY (poc/morphic.lisp): a morph on top of the desk that the
;;;; world draws and routes input to. What is left of each loop is what was always the
;;;; interesting part — which key means what, and what is drawn — and the drawing is
;;;; still the shell's own (DRAW-SPOTTER-CARD, DRAW-CONTEXT-MENU, DRAW-HALO,
;;;; DRAW-CLIPBOARD-CARD), unchanged: only its backdrop is now the world's.
;;;;
;;;; Backend-blind, like the shell: the overlays take LOL.MORPH events, so they work in
;;;; the guest, under X11 and in a host test. The ones that answer a caller (a menu's
;;;; pick, a typed value) wait in RUN-OVERLAY, which needs the backend's loop to turn
;;;; (*WORLD-CYCLE*); the Spotter answers nobody and opens on any backend.
;;;;
;;;; One rule they all keep from the loops they replace: a key that arrived only on the
;;;; keyboard's evdev (a Ctrl chord the console sends no byte for, Shift-PageUp — the
;;;; events marked :EVDEV) was never read by a modal loop, so it is ignored here too.
;;;; The debugger and the cheatsheet, which live in fb-browser.lisp, follow it as well.

(in-package :cl-user)

(defun overlay-key (e)
  "E's key and modifier state when E is a key a modal reads, else NIL."
  (and (eq (lol.morph:event-kind e) :key)
       (not (getf (lol.morph:event-data e) :evdev))
       (values (lol.morph:event-key e) (lol.morph:event-state e))))

(defun card-damage-rect (r)
  "A card rect (x0 y0 x1 y1), as the shell's drawers record it, as the rect (x y w h)
   it paints — its drop shadow included (DRAW-CARD offsets it 5 right, 6 down)."
  (and r (destructuring-bind (x0 y0 x1 y1) r
           (list x0 y0 (+ 6 (- x1 x0)) (+ 7 (- y1 y0))))))

;;; ===========================================================================
;;; The Spotter (SuperL-Space): it answers nobody, so it never waits
;;; ===========================================================================

(defparameter +spotter-cap+ 60 "Max results to rank/show — a few screenfuls.")

(defclass spotter-overlay (overlay)
  ((cands   :initarg :cands :accessor spotter-cands)
   (query   :initform "" :accessor spotter-query)
   (results :initform '() :accessor spotter-hits-list)
   (sel     :initform 0 :accessor spotter-sel))
  (:documentation "Fuzzy-find any definition, package or surface and open it in a new
   trail: a card over the dimmed screen, re-filtered on every key."))

(defun emacs-arrow (key state)
  "C-p / C-n as :UP / :DOWN, any other KEY as itself — for the modal lists (the
   Spotter, the menus), where nothing is edited so the Emacs line keys are free. A
   control byte reaches us as its letter plus the Ctrl bit (FB-READ-KEY), so without
   this C-n would type an n into the Spotter's query."
  (cond ((not (logbitp 2 state)) key)
        ((member key '(#\p #\P)) :up)
        ((member key '(#\n #\N)) :down)
        (t key)))

(defun open-spotter ()
  "SuperL-Space: put the Spotter up and return at once. Enter opens the hit in a NEW
   trail (NEW-TRAIL) and closes it; Esc, or a click off the card, closes it."
  (when *world*
    (let* ((cands (ignore-errors (spotter-candidates)))
           (ov (make-instance 'spotter-overlay :cands cands)))
      (setf (spotter-hits-list ov) (ignore-errors (spotter-results "" cands +spotter-cap+)))
      (open-overlay ov))))

(defmethod overlay-draw ((o spotter-overlay) canvas w h)
  (draw-spotter-card canvas w h (spotter-query o) (spotter-hits-list o) (spotter-sel o)))

(defun spotter-open-hit (o hit)
  (close-overlay o)
  (when (and hit (getf hit :subject)) (new-trail (getf hit :subject))))

(defmethod overlay-event ((o spotter-overlay) e)
  (flet ((refilter ()
           (setf (spotter-hits-list o) (ignore-errors (spotter-results (spotter-query o) (spotter-cands o)
                                                                       +spotter-cap+))
                 (spotter-sel o) 0)
           (overlay-changed o))
         (moved () (overlay-changed o (card-damage-rect *spotter-rect*))))
    (multiple-value-bind (key state) (overlay-key e)
      (if key
          (let ((key (emacs-arrow key state)) (query (spotter-query o)))
            (cond
              ((eq key :escape) (close-overlay o))
              ((member key '(:return #\Return))
               (spotter-open-hit o (nth (spotter-sel o) (spotter-hits-list o))))
              ;; Tab completes a PATH query the way a shell does (common prefix; a
              ;; single folder gets its slash) — SPOTTER-COMPLETE, shell.lisp
              ((eq key :tab)
               (let ((new (spotter-complete query)))
                 (unless (string= new query) (setf (spotter-query o) new) (refilter))))
              ((eq key :up) (setf (spotter-sel o) (max 0 (1- (spotter-sel o)))) (moved))
              ((eq key :down)
               (setf (spotter-sel o) (min (max 0 (1- (length (spotter-hits-list o)))) (1+ (spotter-sel o))))
               (moved))
              ((eq key :backspace)
               (when (plusp (length query))
                 (setf (spotter-query o) (subseq query 0 (1- (length query)))) (refilter)))
              ((and (characterp key) (graphic-char-p key) (not (logbitp 2 state)))
               (setf (spotter-query o) (concatenate 'string query (string key))) (refilter))))
          (when (eq (lol.morph:event-kind e) :down)          ; the left button, as ever
            (let* ((x (lol.morph:event-x e)) (y (lol.morph:event-y e))
                   (i (spotter-row-at* x y)) (results (spotter-hits-list o)))
              (cond ((and i (< i (length results))) (spotter-open-hit o (nth i results)))
                    ;; a click away dismisses
                    ((spotter-outside-p* x y) (close-overlay o)))))))))

;;; ===========================================================================
;;; Menus and halos: one chooser, which waits for the pick
;;; ===========================================================================
;;; The right-button menu and the middle-button halo are the same interaction — a
;;; short list of things you can do, pick one or leave — drawn two ways. The shell
;;; decides WHAT is offered (context-menu-entries, halo-entries) and how each looks
;;; (draw-context-menu, draw-halo); this only reads the events. It is installed as the
;;; shell's *CHOOSER* hook.

(defvar *chooser-state* nil
  "While a chooser is up: (:kind K :sel I :label L :count N), refreshed on every draw;
   NIL otherwise. Only for looking at from outside — the guest checks read it over the
   net REPL (check-web.lisp).")

(defclass chooser-overlay (overlay)
  ((kind    :initarg :kind :reader chooser-kind)
   (entries :initarg :entries :accessor chooser-entries)
   (x :initarg :x :reader chooser-x) (y :initarg :y :reader chooser-y)
   (layers  :initarg :layers :initform nil :reader chooser-layers)
   (sel     :initform 0 :accessor chooser-sel)
   (depth   :initform 0 :accessor chooser-depth)
   (typed   :initform "" :accessor chooser-typed)
   (typed-at :initform 0 :accessor chooser-typed-at))
  (:documentation "A :MENU card at X,Y, a :HALO round it, or a :SELECT (a web page's
   <select> popup drawn as our menu). See RUN-CHOOSER."))

(defun run-chooser (kind entries x y &optional layers)
  "Show ENTRIES as a :MENU card at X,Y or a :HALO round it, and return the entry
   picked, or NIL. Keys: up/down move (left/right for the ring), Return picks, 1-9
   pick directly, Esc leaves. A click picks what is under it; a click anywhere else
   leaves. Hovering a row selects it.

   A halo is about one of LAYERS (shell.lisp's HALO-LAYERS: the word, the element
   holding it, the pane, the screen, the morphs). Up, Tab and the middle button walk
   OUT to the thing around the current one, Down walks back IN — Morphic's repeated
   middle-click — and each step rebuilds the handles for the new layer.

   A :SELECT is the menu card standing in for a web page's <select> popup
   (doc/web-browser.org §4.4): it opens on the entry marked :SELECTED, never lands on
   or picks one marked :DISABLED, and TYPING jumps to the next entry whose label
   starts with what was typed (within a second, letters accumulate: \"ba\" finds
   Banana past Apple), the way every desktop's list box behaves — so digits are
   text here, not the 1-9 shortcuts of a menu. Home/End and PageUp/PageDown move
   too, and Space picks when nothing is being typed."
  (when (and entries *world*)
    (let ((o (make-instance 'chooser-overlay :kind kind :entries entries :x x :y y :layers layers)))
      (setf (chooser-sel o) (or (position-if (lambda (e) (getf e :selected)) entries) 0))
      (unless (chooser-enabled-p o (chooser-sel o))
        (setf (chooser-sel o) (chooser-step o (chooser-sel o) 1)))
      (unwind-protect (run-overlay o)
        (setf *chooser-state* nil)))))

(defun chooser-n (o) (length (chooser-entries o)))
(defun chooser-enabled-p (o i)
  (and (< -1 i (chooser-n o)) (not (getf (nth i (chooser-entries o)) :disabled))))
(defun chooser-step (o from delta)
  "The next ENABLED entry from FROM, or FROM."
  (loop for i = (+ from delta) then (+ i delta)
        while (< -1 i (chooser-n o))
        when (chooser-enabled-p o i) return i
        finally (return from)))
(defun chooser-pick (o i) (and (chooser-enabled-p o i) (nth i (chooser-entries o))))

(defmethod overlay-draw ((o chooser-overlay) canvas w h)
  (let ((entries (chooser-entries o)) (sel (chooser-sel o)))
    (setf *chooser-state* (list :kind (chooser-kind o) :sel sel :count (length entries)
                                :label (getf (nth sel entries) :label)))
    (if (eq (chooser-kind o) :halo)
        (draw-halo canvas w h (chooser-layers o) (chooser-depth o) entries sel (chooser-x o) (chooser-y o))
        (draw-context-menu canvas w h entries sel (chooser-x o) (chooser-y o)))))

(defun chooser-moved (o)
  "Only the selection moved. A menu's card stays where it is, so only the card is
   drawn again; a halo's handles are all over the screen, so all of it is."
  (if (eq (chooser-kind o) :halo)
      (overlay-changed o)
      (overlay-changed o (card-damage-rect *menu-rect*))))

(defun chooser-type-ahead (o ch)
  (let ((now (get-internal-real-time)) (n (chooser-n o)))
    (setf (chooser-typed o) (if (< (- now (chooser-typed-at o)) internal-time-units-per-second)
                                (concatenate 'string (chooser-typed o) (string ch))
                                (string ch))
          (chooser-typed-at o) now)
    ;; a fresh first letter searches from the entry AFTER the current one, so pressing
    ;; b again walks Banana -> Blueberry; a longer prefix keeps the current entry if it
    ;; still matches
    (let* ((typed (chooser-typed o))
           (start (if (= 1 (length typed)) (1+ (chooser-sel o)) (chooser-sel o))))
      (loop for k below n
            for i = (mod (+ start k) n)
            when (and (chooser-enabled-p o i)
                      (let ((l (string-left-trim " " (getf (nth i (chooser-entries o)) :label))))
                        (and (>= (length l) (length typed))
                             (string-equal typed l :end2 (length typed)))))
              do (setf (chooser-sel o) i) (return)))))

(defun chooser-walk (o delta)
  "To another layer of the halo: new handles, the first one lit."
  (setf (chooser-depth o) (mod (+ (chooser-depth o) delta) (length (chooser-layers o)))
        (chooser-entries o) (halo-entries (nth (chooser-depth o) (chooser-layers o)))
        (chooser-sel o) 0)
  (overlay-changed o))

(defmethod overlay-event ((o chooser-overlay) e)
  (let* ((halo (eq (chooser-kind o) :halo)) (select (eq (chooser-kind o) :select))
         (n (chooser-n o)) (sel (chooser-sel o)) (entries (chooser-entries o)))
    (flet ((answer (entry) (close-overlay o entry))
           (move-to (i) (unless (eql i sel) (setf (chooser-sel o) i) (chooser-moved o))))
      (multiple-value-bind (key state) (overlay-key e)
        (if key
            (progn
              ;; C-p / C-n move the selection; in a halo they go round the handles,
              ;; since Up and Down there walk the layers
              (let ((k (emacs-arrow key state)))
                (unless (eq k key)
                  (setf key (if halo (if (eq k :up) :left :right) k))))
              (cond
                ((member key '(:escape :eof)) (answer nil))
                ((and select (logbitp 2 state) (member key '(#\g #\G))) (answer nil))
                ((member key '(:return #\Return)) (answer (if select (chooser-pick o sel) (nth sel entries))))
                (select
                 (case key
                   ((:up :left) (move-to (chooser-step o sel -1)))
                   ((:down :right) (move-to (chooser-step o sel 1)))
                   (:home (move-to (chooser-step o -1 1)))
                   (:end (move-to (chooser-step o n -1)))
                   (:page-up (move-to (chooser-step o (max -1 (- sel 10)) 1)))
                   (:page-down (move-to (chooser-step o (min n (+ sel 10)) -1)))
                   (:tab (answer (chooser-pick o sel)))
                   (t (cond ((and (eql key #\Space)
                                  (> (- (get-internal-real-time) (chooser-typed-at o))
                                     internal-time-units-per-second))
                             (answer (chooser-pick o sel)))
                            ((and (characterp key) (graphic-char-p key) (not (logbitp 2 state)))
                             (chooser-type-ahead o key)
                             (chooser-moved o))))))
                ((and halo (eq key :tab)) (chooser-walk o (if (logbitp 0 state) -1 1)))
                ((and halo (eq key :up)) (chooser-walk o 1))
                ((and halo (eq key :down)) (chooser-walk o -1))
                ((member key '(:up :left)) (move-to (mod (1- sel) n)))
                ((member key '(:down :right :tab)) (move-to (mod (1+ sel) n)))
                ((and (characterp key) (digit-char-p key) (<= 1 (digit-char-p key) n))
                 (answer (nth (1- (digit-char-p key)) entries)))))
            (let ((kind (lol.morph:event-kind e))
                  (px (lol.morph:event-x e)) (py (lol.morph:event-y e)))
              (flet ((hit () (if halo (halo-handle-at px py) (menu-entry-at px py))))
                (case kind
                  (:middle (if halo
                               (chooser-walk o 1)
                               (let ((i (hit)))
                                 (cond ((eq i :outside) (answer nil))
                                       ((and (integerp i) select) (when (chooser-pick o i) (answer (chooser-pick o i))))
                                       ((integerp i) (answer (nth i entries)))))))
                  ((:down :right)
                   (let ((i (hit)))
                     (cond ((eq i :outside) (answer nil))
                           ;; a disabled option is not a choice: the card stays up
                           ((and (integerp i) select) (when (chooser-pick o i) (answer (chooser-pick o i))))
                           ((integerp i) (answer (nth i entries))))))
                  ;; hovering a row selects it, the way every menu behaves
                  ((:move :drag)
                   (let ((i (hit)))
                     (when (and (integerp i) (/= i sel) (or (not select) (chooser-enabled-p o i)))
                       (move-to i))))))))))))

;;; ===========================================================================
;;; The clipboard manager (SuperL-h, doc/clipboard.org)
;;; ===========================================================================
;;; Parcellite's history window as a palette, the Spotter's shape: type to filter,
;;; arrows to move, Enter makes the entry current (and the caller pastes it where the
;;; caret was — OPEN-CLIPBOARD-MANAGER), Delete forgets it, Esc leaves.

(defclass clipboard-overlay (overlay)
  ((query :initform "" :accessor clip-query)
   (sel   :initform 0 :accessor clip-sel)))

(defun clip-results (o) (lol.clipboard:clip-filter (clip-query o)))

(defun run-clipboard-manager ()
  "The clipboard history, waiting for a pick: -> the history INDEX chosen, or NIL."
  (when *world*
    (run-overlay (make-instance 'clipboard-overlay))))

(defmethod overlay-draw ((o clipboard-overlay) canvas w h)
  (let ((results (clip-results o)))
    (setf (clip-sel o) (max 0 (min (clip-sel o) (1- (length results)))))
    (draw-clipboard-card canvas w h (clip-query o) results (clip-sel o))))

(defmethod overlay-event ((o clipboard-overlay) e)
  (let ((results (clip-results o)))
    (multiple-value-bind (key state) (overlay-key e)
      (if key
          (let ((ctrl (logbitp 2 state)) (query (clip-query o)))
            (cond
              ((or (member key '(:escape :eof)) (and ctrl (eql key #\g))) (close-overlay o))
              ((member key '(:return #\Return))
               (close-overlay o (and results (car (nth (clip-sel o) results)))))
              ((or (eq key :up) (and ctrl (eql key #\p)))
               (decf (clip-sel o)) (overlay-changed o (card-damage-rect *clip-rect*)))
              ((or (eq key :down) (and ctrl (eql key #\n)))
               (incf (clip-sel o)) (overlay-changed o (card-damage-rect *clip-rect*)))
              ((eq key :delete)
               (when results (lol.clipboard:clip-delete (car (nth (clip-sel o) results))))
               (overlay-changed o))
              ((eq key :backspace)
               (when (plusp (length query))
                 (setf (clip-query o) (subseq query 0 (1- (length query))) (clip-sel o) 0)
                 (overlay-changed o)))
              ((and (characterp key) (graphic-char-p key) (not ctrl))
               (setf (clip-query o) (concatenate 'string query (string key)) (clip-sel o) 0)
               (overlay-changed o))))
          (when (eq (lol.morph:event-kind e) :down)
            (let ((i (clipboard-row-at (lol.morph:event-x e) (lol.morph:event-y e))))
              (cond ((eq i :outside) (close-overlay o))
                    ((and (integerp i) (< i (length results)))
                     (close-overlay o (car (nth i results)))))))))))

;;; ===========================================================================
;;; The value prompt: one expression, typed in a band at the bottom
;;; ===========================================================================
;;; The shell's *VALUE-PROMPT*: a restart that needs a value, the inspector's "set
;;; this field", a frame local, a rectangle's string. Not the minibuffer (shell.lisp),
;;; which answers through a callback: these callers wait for the text.

(defclass prompt-overlay (overlay)
  ((label :initarg :label :reader prompt-label)
   (buf   :initform "" :accessor prompt-buf))
  (:default-initargs :opaque t))

(defun prompt-band-h () "Two lines of text: 44 px at 6x15." (+ 14 (* 2 (line-h))))

(defun run-value-prompt (label)
  "Read one expression for LABEL in a band along the bottom of the screen, over
   whatever is there: Enter -> the typed STRING, Esc -> NIL."
  (when *world*
    (destructuring-bind (x y w h) (lol.morph:morph-bounds *world*)
      (declare (ignore x y))
      (let ((band (prompt-band-h)))
        (run-overlay (make-instance 'prompt-overlay :label label)
                     (list 0 (- h band) w band))))))

(defmethod overlay-draw ((o prompt-overlay) canvas w h)
  (let ((band (prompt-band-h)))
    (lol.canvas:fill-rect canvas 0 (- h band) w band *ui-hi*)
    (lol.canvas:fill-rect canvas 0 (- h band) w 2 *ui-amber*)
    (lol.canvas:draw-string canvas *bfont*
      ;; "accepts", not "invokes": this line is shared — a restart INVOKES with the
      ;; value, the inspector SETS a field to it (shell.lisp's set-field)
      (format nil "value for [~(~a~)] — an expression, Enter accepts, Esc cancels:" (prompt-label o))
      12 (+ (- h band) 6) *ui-amber*)
    (lol.canvas:draw-string canvas *bfont* (format nil "~a_" (prompt-buf o)) 12 (- h (line-h) 3) *ui-text*)))

(defmethod overlay-event ((o prompt-overlay) e)
  (let ((key (overlay-key e)) (buf (prompt-buf o)))
    (when key
      (cond
        ((eq key :escape) (close-overlay o nil))
        ((member key '(:return #\Return)) (close-overlay o buf))
        ((member key '(:backspace #\Backspace #\Rubout))
         (when (plusp (length buf))
           (setf (prompt-buf o) (subseq buf 0 (1- (length buf))))
           (overlay-changed o)))
        ((and (characterp key) (graphic-char-p key))
         (setf (prompt-buf o) (concatenate 'string buf (string key)))
         (overlay-changed o))))))
