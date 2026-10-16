;;;; morphic.lisp — the browser as the first window of a Lisp window system.
;;;;
;;;; poc/morph.lisp is Morphic's core and knows nothing about the browser. This file
;;;; is the browser's side of it (doc/morphic.org §2, steps 1–5):
;;;;
;;;;   THE DESK      the world the environment runs in: one per screen, *WORLD*.
;;;;   SHELL-MORPH   the whole tiled browser — tabs, breadcrumbs, the pane, the status
;;;;                 line — as ONE morph. Its DRAW-MORPH is DRAW-BROWSER and its
;;;;                 HANDLE-EVENT is BROWSER-KEY / BROWSER-CLICK / BROWSER-DRAG, so on
;;;;                 the day it arrived nothing on screen changed: the screen was a
;;;;                 world with one full-screen child, and every check passed as before.
;;;;   TRAIL-WINDOW  a trail torn off the tab strip (SuperL-g, or a tab dragged out):
;;;;                 its breadcrumbs and pane under a minimal title bar, over the shell,
;;;;                 moved and resized opaquely, re-tiled with SuperL-g again or by
;;;;                 dropping it on the strip. Its halo acts on it (step 3): Move,
;;;;                 Resize, Raise, Close, Re-tile; Inspect shows its bounds, settable.
;;;;   OVERLAY       a modal card on top — the Spotter, a menu, the halo, the debugger
;;;;                 (step 4; the cards themselves are poc/overlays.lisp and, for the
;;;;                 debugger and the cheatsheet, fb-browser.lisp). It takes the input;
;;;;                 everything else goes on behind it. One that answers a caller waits
;;;;                 by turning the backend's own loop (RUN-OVERLAY).
;;;;   STEPPING      the shell and each window step the pane they show when it changes
;;;;                 on its own — a running stage, a live run (step 5) — and damage
;;;;                 only what changed; the terminals' rows and the clock are painted
;;;;                 straight in and composited by the world (NOTE-PAINTED).
;;;;
;;;; Backend-blind like the shell: the guest's loop (initramfs/fb-browser.lisp) and the
;;;; X11 host loop (poc/run-browser.lisp) both build events, hand them to
;;;; LOL.MORPH:WORLD-DISPATCH, and send whatever LOL.MORPH:WORLD-REPAIR redrew.

(in-package :cl-user)

(defvar *world* nil
  "The desk the environment is running in (a DESK), or NIL outside it. Made fresh by
   each entry point (run-browser-fb, run-browser.lisp, render-frame.lisp) for its
   canvas size.")


;;; ===========================================================================
;;; The shell morph (step 1)
;;; ===========================================================================

(defclass shell-morph (lol.morph:morph)
  ((press-tab :initform nil :accessor shell-press-tab
              :documentation "The tab a left press landed on, while the button is held:
   dragged far enough out of the strip, that trail becomes a window (TEAR-OFF-TRAIL)."))
  (:documentation "The tiled browser, whole, as one morph. It cannot yet say WHICH of
   its parts changed, so any change damages all of it — exactly the redraw-everything
   behaviour it had before it was a morph (doc/morphic.org §4.2). Finer damage can come
   one view at a time, without changing the protocol."))

(defmethod lol.morph:morph-opaque-p ((m shell-morph)) t)   ; DRAW-BROWSER fills its bounds
(defmethod lol.morph:morph-name ((m shell-morph)) "the tiled shell")

(defmethod lol.morph:draw-morph ((m shell-morph) canvas clip)
  (declare (ignore clip))
  (destructuring-bind (x y w h) (lol.morph:morph-bounds m)
    (declare (ignore x y))
    (draw-shell-now canvas w h)))

(defun shell-changed (&optional rect (m (and *world* (desk-shell *world*))))
  "The browser's state changed: RECT of it (default: all of it) must be redrawn."
  (when m (lol.morph:invalidate m rect)))

(defun browser-event (e)
  "Run event E through the browser's own entry points, against whatever trail and hit
   maps are current — the tiled shell's, or a window's inside WITH-WINDOW-CONTEXT,
   with E's coordinates already that window's. Returns (values RESULT CHANGED):
   RESULT is :QUIT to leave, else T when the event was the browser's; CHANGED says
   the browser's state changed, so its picture must be redrawn."
  (let ((x (lol.morph:event-x e)) (y (lol.morph:event-y e))
        (state (lol.morph:event-state e)))
    (case (lol.morph:event-kind e)
      (:key (if (eq :quit (browser-key (lol.morph:event-key e) state))
                (values :quit nil)
                (values t t)))
      (:menu (keyboard-context-menu) (values t t))
      (:wheel (browser-scroll (* (- (lol.morph:event-notches e)) +wheel-lines+)) (values t t))
      (:down (browser-click x y (lol.canvas:ctrl-p state) (lol.canvas:super-p state))
             (values t t))
      (:right (browser-context-menu x y) (values t t))
      (:middle (browser-middle-click x y (lol.canvas:super-p state)) (values t t))
      ;; a drag extends the selection the press started; it redraws only if it did
      (:drag (values t (and (browser-drag x y) t)))
      ;; a drag that selected something hands it to PRIMARY and the clipboard (X's
      ;; rule). It changes nothing on screen, as it never did.
      (:up (browser-drag-end) (values t nil))
      (t (values nil nil)))))

(defun tab-at (x y)
  "The index of the tab (not its close box, not +, not the tray) under X,Y in the
   tiled shell's strip, or NIL."
  (and (< y +bar-h+)
       (not (find-if (lambda (e) (<= (cadr e) x (cddr e))) *tab-close-rects*))
       (car (find-if (lambda (e) (<= (cadr e) x (cddr e))) *tab-rects*))))

(defmethod lol.morph:handle-event ((m shell-morph) e)
  "Every event the browser understands, routed exactly as the old loop routed it. A
   press, a menu or the halo on the shell gives it the focus (from a window that had
   it); the wheel scrolls it without taking the focus, as in every desktop."
  (let ((kind (lol.morph:event-kind e)))
    (when (member kind '(:down :right :middle))
      (setf *active* (tiled-index)))
    (when (eq kind :down)
      (setf (shell-press-tab m) (tab-at (lol.morph:event-x e) (lol.morph:event-y e))))
    (cond
      ;; a tab pulled down out of the strip: that trail becomes a window, carried by
      ;; the hand from here as if its title bar had been grabbed (doc/morphic.org §5)
      ((and (eq kind :drag) (shell-press-tab m)
            (>= (lol.morph:event-y e) (* 2 +bar-h+)))
       (let ((i (shiftf (shell-press-tab m) nil)))
         (tear-off-by-pointer i (lol.morph:event-x e) (lol.morph:event-y e)))
       t)
      (t
       (when (eq kind :up) (setf (shell-press-tab m) nil))
       (let ((was-damaged (lol.morph:world-damaged-p *world*))
             (pane-before (current-pane)))
         (multiple-value-bind (result changed)
             (if (eq kind :wheel)
                 (let ((*active* (tiled-index))) (browser-event e))
                 (browser-event e))
           ;; typed INTO a terminal: the echo comes back as output and repaints only
           ;; its rows (terminal-pane.lisp), so the frame is not damaged — unless it
           ;; already was, or the key was not the console's (an evdev chord)
           (when (and changed
                      (not (and (eq kind :key)
                                (getf (lol.morph:event-data e) :tty)
                                (not was-damaged)
                                (fboundp 'terminal-key-was-cheap-p)
                                (funcall 'terminal-key-was-cheap-p pane-before
                                         (lol.morph:event-state e)))))
             (shell-changed nil m))
           result))))))

;;; ===========================================================================
;;; The desk: the world the environment runs in
;;; ===========================================================================

(defclass desk (lol.morph:world)
  ((shell :initarg :shell :accessor desk-shell
          :documentation "The tiled shell, always the bottom child.")
   (tiled-root :initform nil :accessor desk-tiled-root
               :documentation "The root pane of the trail the tiled shell shows: the
   tiled trail focused last. (*ACTIVE* may be a window's.)")
   (grab :initform nil :accessor desk-grab
         :documentation "The window SuperL-g put in keyboard move/resize mode, or NIL.")
   (font-size :initform nil :accessor desk-font-size
              :documentation "The font size the windows were drawn at, to notice a change."))
  (:documentation "The environment's world: the tiled shell at the bottom of the
   z-order, trail windows above it."))

(defvar *windows-to-restore* nil
  "Windows for the next MAKE-DESK to tear off again: ((ROOT-PANE x y w h) …), back to
   front. Set from the desk file at boot (desk.lisp) and from the last world when you
   come back from the console (RUN-BROWSER-FB), so a session's windows survive both.")

(defun make-desk (w h)
  "A world W x H holding the tiled shell, focused, all of it damaged: the first
   repair draws the whole frame. Windows waiting in *WINDOWS-TO-RESTORE* are torn off
   again where they were (cut to this screen)."
  (let* ((shell (make-instance 'shell-morph :bounds (list 0 0 w h)))
         (desk (make-instance 'desk :bounds (list 0 0 w h) :shell shell)))
    (lol.morph:add-morph desk shell)
    (setf (lol.morph:world-focus desk) shell
          (desk-tiled-root desk) (and *trails* (first (nth *active* *trails*)))
          (desk-font-size desk) (and *bfont* (lol.canvas:font-size *bfont*)))
    (let ((layout (shiftf *windows-to-restore* nil)))
      (when layout
        (let ((*world* desk)) (restore-window-layout layout))))
    desk))

(defun desk-window-layout (&optional (desk *world*))
  "The desk's windows as ((ROOT-PANE x y w h) …), back to front: what
   RESTORE-WINDOW-LAYOUT (and the desk file, desk.lisp) needs to put them back."
  (mapcar (lambda (w) (cons (window-root w) (copy-list (lol.morph:morph-bounds w))))
          (desk-windows desk)))

(defun restore-window-layout (layout)
  "Tear off again each trail LAYOUT names by its root pane, at its bounds cut to this
   screen, in LAYOUT's order — so the z-order comes back too. A trail that is gone is
   skipped; the focus stays where it was."
  (let ((keep *active*))
    (destructuring-bind (sx sy sw sh) (lol.morph:morph-bounds *world*)
      (declare (ignore sx sy))
      (dolist (e layout)
        (let ((i (position (car e) *trails* :key #'first)))
          (when (and i (= 5 (length e)) (every #'integerp (cdr e)))
            (destructuring-bind (x y w h) (cdr e)
              (let* ((w (max lol.morph::+min-carried-w+ (min w sw)))
                     (h (max lol.morph::+min-carried-h+ (min h (- sh +bar-h+))))
                     (x (max (- (* 4 (char-w)) w) (min x (- sw (* 4 (char-w))))))
                     (y (max +bar-h+ (min y (- sh +bar-h+)))))
                (tear-off-trail i (list x y w h))))))))
    (setf *active* (min keep (max 0 (1- (length *trails*)))))))

(defun environment-changed ()
  "Something other than an event changed what the environment shows — the agent
   streamed, a run printed, a service woke, the stage ticked. Until those become
   morphs that step themselves (doc/morphic.org step 5), they damage the shell, and
   every window showing something live (a terminal, a page, a conversation …)."
  (shell-changed)
  (refresh-live-windows))

;;; ===========================================================================
;;; Windows (step 2): a trail torn off the tab strip
;;; ===========================================================================
;;;
;;; A TRAIL-WINDOW shows one trail — its breadcrumbs and its current pane — drawn by
;;; the same DRAW-TRAIL-AREA the tiled shell uses, under a minimal title bar. The trail
;;; itself stays in *TRAILS*: a trail is either in the tab strip or in a window, and
;;; everything that walks *TRAILS* (the terminal and web sweeps, the desk file, the
;;; unsaved-files question at SuperL-q) keeps seeing it. The window finds its trail by
;;; the trail's ROOT PANE, the one thing about a trail that never changes identity
;;; (drilling replaces the list, never its first pane).
;;;
;;; FOCUS is *ACTIVE*, as it always was: the focused trail. When it is a window's,
;;; that window has the keyboard; when it is a tiled one, the shell has. So SuperL-Tab
;;; and SuperL-1..9 reach windows too — and raise them — with no code of their own
;;; (WORLD-AFTER-DISPATCH below makes the world agree after every event).
;;;
;;; Each window draws into a BACKING STORE, a canvas of its own size, which the
;;; world's repair then copies onto the screen (clipped). The shell's drawing code
;;; puts things at fixed offsets from 0,0 and records where it put them in specials
;;; (*LIST-ROWS*, *CONTENT-TOP* …) that its click handlers read back; in a canvas of
;;; its own a window's pane is simply at 0,0, and a window keeps its OWN values of
;;; those specials (WITH-WINDOW-CONTEXT), so its hit maps and the shell's never mix.
;;; It also makes an opaque drag cheap: moving a window re-renders nothing in it.

(defparameter +window-specials+
  '(*crumb-rects* *content-top* *split-x* *preview-pane* *preview-subject*
    *tab-rects* *tab-close-rects* *tab-plus-rect* *list-rows* *side-rows* *drawn*
    *matrix-cells* *stage-rect* *stage-body-rect* *editing-side* *drag-tv* *drag-origin* *term-drag*
    *last-press*)
  "The shell's per-frame specials — hit maps, caches and focus-within-a-pane state —
   of which every window keeps its own values.")

(defparameter +window-specials-late+
  '("*WEB-PAGE-RECT*" "*WEB-BAR-RECTS*" "*WEB-BAR-Y*" "*WEB-PRESS*" "*TERM-POINTER*")
  "The same, from modules that load after this one (web.lisp): found by name.")

(defun window-special-symbols ()
  (append +window-specials+
          (loop for name in +window-specials-late+
                for s = (find-symbol name :cl-user)
                when (and s (boundp s)) collect s)))

(defun fresh-special-value (sym)
  "What a new window starts with for SYM: the empty value, not the shell's."
  (if (eq sym '*content-top*) 0 nil))

(defclass trail-window (lol.morph:morph)
  ((root     :initarg :root :accessor window-root
             :documentation "The ROOT PANE of the trail shown: how the window finds it.")
   (backing  :initform nil :accessor window-backing
             :documentation "The window's own canvas, its size; NIL until first drawn.")
   (stale    :initform t :accessor window-stale
             :documentation "Must the backing store be drawn again before it is shown?
   T: all of it; a rect (x y w h, the window's own coordinates): only that part, drawn
   through a clip (a stage stepping inside it); NIL: it is up to date.")
   (specials :initform '() :accessor window-specials
             :documentation "The window's own values of the shell's hit-map specials.")
   (renders  :initform 0 :accessor window-renders
             :documentation "How many times its contents were drawn (not just copied)."))
  (:documentation "One trail, torn off the tab strip: a title bar over its breadcrumbs
   and current pane. Opaque; draws into a backing store; moves by being copied."))

(defmethod lol.morph:morph-opaque-p ((w trail-window)) t)
(defmethod lol.morph:morph-name ((w trail-window))
  (let ((trail (window-trail w)))
    (format nil "window · ~a" (if trail (trail-title trail) "?"))))

(defun desk-windows (&optional (desk *world*))
  "The desk's windows, back to front."
  (and desk (remove-if-not (lambda (m) (typep m 'trail-window)) (lol.morph:morph-children desk))))

(defun window-trail-index (w)
  (position (window-root w) *trails* :key #'first))

(defun window-trail (w)
  (let ((i (window-trail-index w))) (and i (nth i *trails*))))

(defun trail-window-of (trail)
  (and trail *world* (find (first trail) (desk-windows) :key #'window-root)))

(defun windowed-trail-p (trail) (and (trail-window-of trail) t))

(setf *trail-windowed-p* 'windowed-trail-p)

(defun tiled-index ()
  "Index into *TRAILS* of the trail the tiled shell shows. With no windows, simply
   *ACTIVE* — every trail is tiled. Otherwise the tiled trail the desk saw focused
   last, else the first tiled one."
  (if (null (desk-windows))
      *active*
      (let ((i (and (desk-tiled-root *world*)
                    (position (desk-tiled-root *world*) *trails* :key #'first))))
        (if (and i (not (windowed-trail-p (nth i *trails*))))
            i
            (or (position-if-not #'windowed-trail-p *trails*) 0)))))

(defun focused-window ()
  "The window holding the keyboard focus, or NIL when the shell has it."
  (and *trails* (trail-window-of (nth *active* *trails*))))

(defvar *window-context* nil
  "The window whose drawing or event is being handled, or NIL for the shell.")
(defvar *shell-specials* nil
  "The shell's values of the window specials, saved on entering a window's context,
   so that a backdrop drawn from inside one (a menu opened in a window) draws the shell
   with the shell's own hit maps (DRAW-WORLD-BACKDROP).")

(defun call-with-window-context (w fn)
  "Run FN with W's own values of the shell's per-frame specials, saving them back
   afterwards. Inside, FN draws or handles input in window-local coordinates."
  (let* ((syms (window-special-symbols))
         (vals (mapcar (lambda (s)
                         (let ((e (assoc s (window-specials w))))
                           (if e (cdr e) (fresh-special-value s))))
                       syms))
         (shell (if *window-context* *shell-specials* (mapcar #'symbol-value syms))))
    (progv syms vals
      (let ((*window-context* w) (*shell-specials* shell))
        (unwind-protect (funcall fn)
          (setf (window-specials w) (mapcar (lambda (s) (cons s (symbol-value s))) syms)))))))

(defmacro with-window-context ((w) &body body)
  `(call-with-window-context ,w (lambda () ,@body)))

;;; ---- drawing a window ------------------------------------------------------------

(defun window-close-x (w)
  "Local x where the title bar's close box starts."
  (- (third (lol.morph:morph-bounds w)) (char-w) 10))

(defun window-grip-size ()
  "The resize grip's side: half a line, so it grows with the font."
  (max 8 (floor (line-h) 2)))

(defun draw-window-title (canvas w win focused)
  "The minimal title bar (doc/morphic.org §9, decision 6), in the tab strip's own
   style: the trail's kind stripe on top, its SuperL number quiet, its title bright
   (the accent when focused), and a close box at the right."
  (let ((trail (window-trail win)) (i (window-trail-index win)))
    (lol.canvas:fill-rect canvas 0 0 w +bar-h+ (if focused *ui-hi* *ui-panel*))
    (when trail (lol.canvas:fill-rect canvas 0 0 w 2 (trail-kind-color trail)))
    (let* ((num (format nil " ~d ~c " (1+ (or i 0)) (code-char #xB7)))
           (room (max 4 (floor (- w (* 3 (char-w)) 24 (lol.canvas:string-px *bfont* num))
                               (char-w))))
           (pen (lol.canvas:draw-string canvas *bfont* num 4 (text-dy +bar-h+) *ui-dim*)))
      (lol.canvas:draw-string canvas *bfont* (clip-text (if trail (trail-title trail) "") room)
                              pen (text-dy +bar-h+) (if focused *ui-accent* *ui-text*)))
    (lol.canvas:draw-string canvas *bfont* (string (code-char 215))
                            (window-close-x win) (text-dy +bar-h+)
                            (if focused *ui-dim* *ui-rule*))
    (lol.canvas:fill-rect canvas 0 (- +bar-h+ 1) w 1 *ui-rule*)
    (note-drawn 0 0 w +bar-h+ win '(draw-window-title) "the title bar")))

(defun draw-window-frame (canvas w h focused)
  "The window's outline (the accent when it has the focus) and its resize grip: three
   short diagonals in the bottom-right corner, sized from the cell."
  (lol.canvas:draw-rect canvas 0 0 w h (if focused *ui-accent* *ui-rule*))
  (let ((g (window-grip-size)))
    (loop for k from 1 to 3
          for d = (floor (* g k) 3)
          do (lol.canvas:draw-line canvas (- w 3) (- h 3 d) (- w 3 d) (- h 3) *ui-dim*))))

(defun render-window (win)
  "Draw WIN's contents into its backing store: the title bar, then its trail exactly
   as the tiled shell draws one (DRAW-TRAIL-AREA), in the window's own context. When
   only a part is stale (a rect), the same drawing runs through a clip to it: the
   layout is the whole window's, the pixels written only that part's."
  (destructuring-bind (x y w h) (lol.morph:morph-bounds win)
    (declare (ignore x y))
    (let ((c (window-backing win)) (part (window-stale win)))
      (unless (and c (= (lol.canvas:canvas-w c) w) (= (lol.canvas:canvas-h c) h))
        (setf c (setf (window-backing win) (lol.canvas:make-canvas w h))
              part t))
      (destructuring-bind (cx cy cw ch) (if (consp part) part (list 0 0 w h))
        (lol.canvas:with-clip (c cx cy cw ch)
          (let ((i (window-trail-index win)) (focused (eq win (focused-window))))
            (lol.canvas:fill-rect c 0 0 w h *ui-bg*)
            (when i
              (let ((*active* i))
                (with-window-context (win)
                  (setf *crumb-rects* '() *split-x* nil *drawn* '())
                  (note-drawn 0 0 w h win '(render-window) "the window")
                  (draw-window-title c w win focused)
                  (draw-trail-area c w h (+ +bar-h+ 4) 6)
                  (draw-window-frame c w h focused)))))))
      (incf (window-renders win))
      (setf (window-stale win) nil))))

(defmethod lol.morph:draw-morph ((win trail-window) canvas clip)
  (declare (ignore clip))
  (destructuring-bind (x y w h) (lol.morph:morph-bounds win)
    (let ((b (window-backing win)))
      (when (or (window-stale win) (null b)
                (/= w (lol.canvas:canvas-w b)) (/= h (lol.canvas:canvas-h b)))
        (render-window win)))
    (lol.canvas:blit-canvas canvas (window-backing win) x y)))

(defun window-changed (win &optional local)
  "WIN's contents changed: draw them again, and show it. LOCAL, a rect in the
   window's own coordinates, says only that part did (a stage's body): only it is
   drawn again and repaired."
  (cond
    ((or (null local) (eq (window-stale win) t) (null (window-backing win)))
     (setf (window-stale win) t)
     (lol.morph:invalidate win))
    (t
     (setf (window-stale win)
           (if (consp (window-stale win)) (lol.morph:rect-union (window-stale win) local) local))
     (destructuring-bind (x y w h) (lol.morph:morph-bounds win)
       (declare (ignore w h))
       (lol.morph:invalidate win (list (+ x (first local)) (+ y (second local))
                                       (third local) (fourth local)))))))

(defun pane-live-p (p)
  "Does pane P change on its own — a terminal, a web page, the stage, a run's output,
   a live worker, a conversation?"
  (and p
       (or (member (view-kind (pane-view p)) '(:web :terminal :stage))
             (and (fboundp 'pane-conversation) (funcall 'pane-conversation p))
             (let ((tn (type-of (pane-subject p))))
               (and (symbolp tn)
                    (member (symbol-name tn) '("SUBJ-REMOTE" "RUN" "SUBJ-TERMINAL" "SUBJ-WEB")
                            :test #'string=))))))

(defun window-live-p (win)
  "Is WIN's current pane live (PANE-LIVE-P)? Such windows are redrawn whenever the
   world is told something changed without an event (ENVIRONMENT-CHANGED)."
  (let ((trail (window-trail win))) (and trail (pane-live-p (car (last trail))))))

(defun refresh-live-windows ()
  (dolist (w (desk-windows)) (when (window-live-p w) (window-changed w))))

(defun service-woke ()
  "A service's fd woke the poll. The terminals and the live workers say exactly
   which pane changed (TERMINAL-ROWS-TO-WORLD, DAMAGE-PANE); a web page's frame does
   not, so windows showing a page are drawn again — and the shell's page too while a
   window has the focus, since the web service then skips its own fast path (it paints
   at the shell's geometry, which the focused pane is not in)."
  (dolist (w (desk-windows))
    (let ((p (window-pane w)))
      (when (and p (eq (view-kind (pane-view p)) :web))
        (window-changed w))))
  (let ((p (holder-pane (desk-shell *world*))))
    (when (and (focused-window) p (eq (view-kind (pane-view p)) :web))
      (shell-changed))))

(defun status-rect ()
  (destructuring-bind (x y w h) (lol.morph:morph-bounds *world*)
    (declare (ignore x y))
    (list 0 (- h (status-h)) w (status-h))))

(defun shell-painted (rects)
  "The shell painted RECTS of itself straight into the screen's canvas — the fast
   paths that skip a whole DRAW-BROWSER: the clock's tick, a terminal's rows, a web
   frame. The world composites whatever lies over them (windows, an overlay's scrim)
   and sends them (LOL.MORPH:NOTE-PAINTED). Outside a world: nothing to do."
  (when *world*
    (lol.morph:note-painted *world* rects (desk-shell *world*))))

;;; ---- the shell and the windows, drawn together --------------------------------------

(defun draw-shell-now (canvas w h)
  "DRAW-BROWSER with the tiled trail current, and the focused window's pane (if a
   window has the focus) advertised in the status line."
  (let* ((focus (focused-window))
         (*status-pane* (and focus (car (last (nth *active* *trails*)))))
         (*active* (tiled-index)))
    (draw-browser canvas w h)))

(defun draw-world-backdrop (canvas w h)
  "What a modal card floats over (DRAW-BACKDROP): the whole screen as the world shows
   it — the shell, then every window on top. From inside a window's context the shell
   is drawn with the shell's own specials, never the window's."
  (if *window-context*
      (progv (window-special-symbols) *shell-specials*
        (let ((*window-context* nil)) (draw-shell-now canvas w h)))
      (draw-shell-now canvas w h))
  (dolist (win (desk-windows))
    (when (window-backing win)
      (destructuring-bind (x y ww wh) (lol.morph:morph-bounds win)
        (declare (ignore ww wh))
        (lol.canvas:blit-canvas canvas (window-backing win) x y)))))

(setf *backdrop-fn* 'draw-world-backdrop)

;;; ---- tearing off, re-tiling, closing ------------------------------------------------

(defun default-window-bounds ()
  "Where a window torn off with the keyboard goes: about 60% of the screen, centred,
   each further window stepped down and right by a title bar, so none hides another."
  (destructuring-bind (x y sw sh) (lol.morph:morph-bounds *world*)
    (declare (ignore x y))
    (let* ((w (max 320 (floor (* sw 3) 5))) (h (max 240 (floor (* sh 5) 8)))
           (n (length (desk-windows)))
           (step (+ +bar-h+ 6))
           (x0 (+ (floor (- sw w) 2) (* n step))) (y0 (+ (floor (- sh h) 2) (* n step))))
      (list (max 0 (min x0 (- sw w))) (max +bar-h+ (min y0 (- sh h))) w h))))

(defun tear-off-trail (i &optional bounds)
  "Show trail I in a window of its own (BOUNDS, else DEFAULT-WINDOW-BOUNDS), focused
   and on top. The last tiled trail stays tiled — the shell always shows one; the
   status line says so and NIL is returned. Returns the window."
  (let ((trail (nth i *trails*)))
    (cond
      ((null trail) nil)
      ((trail-window-of trail))
      ((<= (count-if-not #'windowed-trail-p *trails*) 1)
       (setf *browser-status*
             "the last tab stays tiled — open another (SuperL-e, SuperL-m …) to tear this one off")
       (shell-changed (status-rect))
       nil)
      (t
       (let ((win (make-instance 'trail-window :root (first trail)
                                               :bounds (or bounds (default-window-bounds)))))
         (lol.morph:add-morph *world* win)
         ;; the tiled area shows its neighbour now: the nearest tiled trail before it
         (setf (desk-tiled-root *world*)
               (first (nth (or (position-if-not #'windowed-trail-p *trails* :end i :from-end t)
                               (position-if-not #'windowed-trail-p *trails*))
                           *trails*)))
         (setf *active* i)
         (shell-changed)
         win)))))

(defun tear-off-by-pointer (i px py)
  "A tab dragged out of the strip at PX,PY: tear trail I off with its title bar under
   the pointer, and let the hand carry it from here (an opaque drag, decision 2)."
  (destructuring-bind (x y w h) (default-window-bounds)
    (declare (ignore x y))
    (let* ((sw (third (lol.morph:morph-bounds *world*)))
           (wx (max 0 (min (- px (floor w 3)) (- sw w))))
           (wy (max +bar-h+ (- py (floor +bar-h+ 2))))
           (win (tear-off-trail i (list wx wy w h))))
      (when win
        (lol.morph:hand-pick-up (lol.morph:world-hand *world*) win :move)))))

(defun retile-window (win)
  "Put WIN's trail back in the tab strip, focused, and the window away."
  (let ((i (window-trail-index win)))
    (when (eq (desk-grab *world*) win) (setf (desk-grab *world*) nil))
    (lol.morph:remove-morph win)
    (when i
      (setf *active* i (desk-tiled-root *world*) (first (nth i *trails*))))
    (setf (lol.morph:world-focus *world*) (desk-shell *world*))
    (shell-changed)))

(defun close-window-trail (win)
  "The close box: close WIN's trail, as SuperL-w would (never the last trail — but a
   window's never is, since the shell always keeps one)."
  (let ((i (window-trail-index win)))
    (when i
      (setf *active* i)
      (close-trail)
      (shell-changed))))                  ; the tabs renumber

(defmethod lol.morph:drop-morph ((win trail-window) world x y)
  "Let go of a carried window. On the tab strip: back into it (re-tile). Elsewhere it
   stays where it is, pulled back on screen if its title bar went off it."
  (declare (ignore x))
  ;; a carry the halo's Move or Resize began was in grab mode: putting it down ends that
  (when (eq (desk-grab world) win)
    (setf (desk-grab world) nil *browser-status* nil)
    (shell-changed (status-rect)))
  (if (< y +bar-h+)
      (retile-window win)
      (destructuring-bind (wx wy ww wh) (lol.morph:morph-bounds win)
        (declare (ignore wh))
        (destructuring-bind (sx sy sw sh) (lol.morph:morph-bounds world)
          (declare (ignore sx sy))
          (let ((nx (max (- (* 4 (char-w)) ww) (min wx (- sw (* 4 (char-w))))))
                (ny (max 0 (min wy (- sh +bar-h+)))))
            (lol.morph:move-morph win nx ny))))))

;;; ---- a window's events -----------------------------------------------------------

(defun local-event (e win)
  "E with its coordinates made WIN's own."
  (destructuring-bind (x y w h) (lol.morph:morph-bounds win)
    (declare (ignore w h))
    (let ((c (copy-structure e)))
      (setf (lol.morph:event-x c) (- (lol.morph:event-x e) x)
            (lol.morph:event-y c) (- (lol.morph:event-y e) y))
      c)))

(defun offset-layer (layer dx dy)
  (let ((r (getf layer :rect)))
    (if r
        (let ((l (copy-list layer)))
          (setf (getf l :rect) (list (+ (first r) dx) (+ (second r) dy) (third r) (fourth r)))
          l)
        layer)))

(defun window-chooser (chooser win)
  "CHOOSER (the backend's menu/halo loop) as seen from inside WIN: the browser asks
   for a menu at window coordinates; the card goes where that is on screen, and the
   halo's layer outlines with it."
  (and chooser
       (destructuring-bind (ox oy w h) (lol.morph:morph-bounds win)
         (declare (ignore w h))
         (lambda (kind entries x y &optional layers)
           (funcall chooser kind entries (+ x ox) (+ y oy)
                    (mapcar (lambda (l) (offset-layer l ox oy)) layers))))))

(defun trails-same-except-p (before after root)
  "Are the trail lists BEFORE and AFTER the same, but for the one rooted at ROOT?"
  (and (= (length before) (length after))
       (every (lambda (a b) (or (eq a b) (and (eq (first a) root) (eq (first b) root))))
              before after)))

(defun window-browser-event (win e)
  "Run E through the browser inside WIN — its trail current, its hit maps in place,
   coordinates its own. Damage: the window if anything changed, the status line (the
   messages and hints of the focused pane live there), and the whole shell if the
   event changed any OTHER trail (a new tab, a closed one)."
  (let ((before (copy-list *trails*)) (root (window-root win)))
    (multiple-value-bind (result changed)
        (let ((*chooser* (window-chooser *chooser* win)))
          (with-window-context (win) (browser-event (local-event e win))))
      (when changed
        (window-changed win)
        (shell-changed (status-rect)))
      (unless (trails-same-except-p before *trails* root) (shell-changed))
      result)))

(defun in-grip-p (win lx ly)
  (destructuring-bind (x y w h) (lol.morph:morph-bounds win)
    (declare (ignore x y))
    (let ((g (+ 4 (window-grip-size))))
      (and (>= lx (- w g)) (>= ly (- h g))))))

(defmethod lol.morph:handle-event ((win trail-window) e)
  "A press focuses and raises the window, then: the close box closes it, the title bar
   picks it up (the hand carries it while the button is held), the grip resizes it,
   anything else goes to its trail. The wheel scrolls it without taking the focus."
  (let* ((kind (lol.morph:event-kind e))
         (i (window-trail-index win)))
    (unless i (return-from lol.morph:handle-event nil))
    (when (member kind '(:down :right :middle))
      (setf *active* i)
      (lol.morph:raise-morph win))
    (destructuring-bind (x y w h) (lol.morph:morph-bounds win)
      (declare (ignore w h))
      (let ((lx (- (lol.morph:event-x e) x)) (ly (- (lol.morph:event-y e) y)))
        (cond
          ((and (eq kind :down) (< ly +bar-h+))
           (if (>= lx (window-close-x win))
               (close-window-trail win)
               (lol.morph:hand-pick-up (lol.morph:world-hand *world*) win :move))
           t)
          ((and (eq kind :down) (in-grip-p win lx ly))
           (lol.morph:hand-pick-up (lol.morph:world-hand *world*) win :resize)
           t)
          ((eq kind :wheel)
           (let ((*active* i)) (window-browser-event win e)))
          (t (window-browser-event win e)))))))

;;; ---- the desk's own keys: SuperL-g and the grab mode -----------------------------------

(defun grab-status ()
  (setf *browser-status*
        "window: arrows move · S-arrows ×8 · C-arrows resize · t or SuperL-g re-tile · Ret/Esc done")
  (shell-changed (status-rect)))

(defun clamp-window-position (win x y)
  "X,Y pulled back so that WIN's title bar stays on screen."
  (destructuring-bind (sx sy sw sh) (lol.morph:morph-bounds *world*)
    (declare (ignore sx sy))
    (let ((ww (third (lol.morph:morph-bounds win))))
      (values (max (- (* 4 (char-w)) ww) (min x (- sw (* 4 (char-w)))))
              (max 0 (min y (- sh +bar-h+)))))))

(defun grab-key (desk key state)
  "A key while SuperL-g's grab mode is on: arrows move the window a cell (Shift:
   eight), Ctrl-arrows resize it, t or SuperL-g re-tile it, Return or Esc finish. Any
   other key finishes too, and then goes where keys go. -> T when consumed."
  (let ((win (desk-grab desk)))
    (unless (member win (desk-windows desk))
      (setf (desk-grab desk) nil)
      (return-from grab-key nil))
    (let* ((shift (lol.canvas:shift-p state)) (ctrl (lol.canvas:ctrl-p state))
           (k (if shift 8 1))
           (dx (* k (char-w))) (dy (* k (line-h))))
      (destructuring-bind (x y w h) (lol.morph:morph-bounds win)
        (flet ((done () (setf (desk-grab desk) nil *browser-status* nil)
                 ;; the halo's Move / Resize carry the window with no button held:
                 ;; Return or Esc puts it down where it is
                 (let ((hand (lol.morph:world-hand desk)))
                   (when (eq (lol.morph:hand-carrying hand) win)
                     (setf (lol.morph:hand-carrying hand) nil)))
                 (shell-changed (status-rect))))
          (cond
            ((and (member key '(:left :right :up :down)) ctrl)
             (lol.morph:resize-morph win
                                     (max lol.morph::+min-carried-w+
                                          (+ w (case key (:right dx) (:left (- dx)) (t 0))))
                                     (max lol.morph::+min-carried-h+
                                          (+ h (case key (:down dy) (:up (- dy)) (t 0)))))
             t)
            ((member key '(:left :right :up :down))
             (multiple-value-bind (nx ny)
                 (clamp-window-position win
                                        (+ x (case key (:right dx) (:left (- dx)) (t 0)))
                                        (+ y (case key (:down dy) (:up (- dy)) (t 0))))
               (lol.morph:move-morph win nx ny))
             t)
            ((or (member key '(#\t #\T))
                 (and (lol.canvas:super-p state) (member key '(#\g #\G))))
             (done) (retile-window win) t)
            ((member key '(:return #\Return :escape))
             (done) t)
            (t (done) nil)))))))

(defmethod lol.morph:handle-event ((desk desk) e)
  "The desk sees every event first. It brings the world in line with the trails (the
   focus can have moved between events — a modal, a restored desk), then keeps only
   the window keys: SuperL-g, and the keys of the grab mode it starts. SuperL-g on a
   tiled trail tears it off into a window (and grabs it); on a window, grabs it —
   arrows then move it."
  (desk-sync desk)
  ;; an overlay on top (the Spotter, a menu, the debugger …) is MODAL for input: it
  ;; takes every event, wherever the pointer is — a click off its card is its business
  ;; too (it closes). Nothing else on screen stops: services, steps and the clock go
  ;; on behind it (doc/morphic.org §10, step 4).
  (let ((ov (top-overlay desk)))
    (when ov
      (overlay-event ov e)
      (return-from lol.morph:handle-event t)))
  (when (eq (lol.morph:event-kind e) :key)
    (let ((key (lol.morph:event-key e)) (state (lol.morph:event-state e)))
      (cond
        ((desk-grab desk) (grab-key desk key state))
        ((and (lol.canvas:super-p state) (member key '(#\g #\G)) (not *minibuffer*))
         (let ((win (or (focused-window) (tear-off-trail *active*))))
           (when win
             (setf (desk-grab desk) win)
             (grab-status)))
         t)))))

(defmethod lol.morph:world-after-dispatch ((desk desk) e)
  (declare (ignore e))
  (desk-sync desk))

(defun desk-sync (desk)
  "Make the world agree with the trails: a window whose trail is gone goes; the focus
   follows *ACTIVE* (a window that gets it is raised, and both title bars restyle); a
   font size change redraws everything. Run before and after every event."
  (dolist (w (desk-windows desk))
    (unless (window-trail-index w)
      (when (eq (desk-grab desk) w) (setf (desk-grab desk) nil))
      (lol.morph:remove-morph w)))
  (when *trails*
    (setf *active* (max 0 (min *active* (1- (length *trails*)))))
    (let* ((trail (nth *active* *trails*))
           (win (trail-window-of trail))
           (new (or win (desk-shell desk)))
           (old (lol.morph:world-focus desk)))
      (unless win
        (unless (eq (desk-tiled-root desk) (first trail))
          (setf (desk-tiled-root desk) (first trail))
          (shell-changed)))
      (unless (eq new old)
        (when (typep old 'trail-window) (window-changed old))
        (when win (lol.morph:raise-morph win) (window-changed win))
        (setf (lol.morph:world-focus desk) new)
        (shell-changed (status-rect)))))
  (let ((fs (and *bfont* (lol.canvas:font-size *bfont*))))
    (unless (eql fs (desk-font-size desk))
      (setf (desk-font-size desk) fs)
      (dolist (w (desk-windows desk)) (setf (window-stale w) t))
      (lol.morph:invalidate desk)))
  nil)

;;; ---- the halo knows about morphs --------------------------------------------------
;;; The halo's layers (shell.lisp, HALO-LAYERS) go from the word under the pointer out
;;; to the screen. Outside the screen, now, is the world: each morph under the point
;;; becomes a layer, topmost first, so walking OUT with Up ends on the shell morph —
;;; and Inspect on it shows a CLOS instance with its bounds (doc/morphic.org §6).
;;; Inside a window the point and the rects are the window's own, as the rest of that
;;; halo's layers are (WINDOW-CHOOSER puts them back on screen).

(defun morph-halo-layers (x y)
  (when *world*
    (destructuring-bind (ox oy &rest r)
        (if *window-context* (lol.morph:morph-bounds *window-context*) '(0 0))
      (declare (ignore r))
      ;; (not the overlays: the halo is itself one, and a halo is about what is under it)
      (loop for m in (remove-if (lambda (m) (typep m 'overlay))
                                (lol.morph:morphs-at *world* (+ x ox) (+ y oy)))
            for (bx by bw bh) = (lol.morph:morph-bounds m)
            collect (halo-layer (lol.morph:morph-name m)
                                (format nil "the morph ~a — ~(~a~)" (lol.morph:morph-name m)
                                        (class-name (class-of m)))
                                m (list (- bx ox) (- by oy) bw bh) :element
                                '(lol.morph:draw-morph lol.morph:handle-event))))))

(setf *halo-outer-layers* 'morph-halo-layers)

;;; ===========================================================================
;;; Overlays (step 4): the modal cards are morphs in the one loop
;;; ===========================================================================
;;;
;;; The Spotter, the menus and the halo, the clipboard manager, the value prompt, the
;;; debugger, the cheatsheet: each used to be a loop of its own — poll, read a key,
;;; draw a whole frame, present it — nested inside whatever opened it, and while it
;;; ran nothing else on screen moved. Each is now an OVERLAY: a morph on top of the
;;; desk, which the world draws like any other (clipped, back to front, so a scrim
;;; dims whatever is under it as it is NOW) and which takes every input event while it
;;; is up (the desk routes them to it: DESK's HANDLE-EVENT). Its own code is only what
;;; it was always about — which keys mean what, and what it draws.
;;;
;;; Two ways to be opened:
;;;
;;;   OPEN-OVERLAY  puts it up and returns at once. The Spotter, the cheatsheet and the
;;;                 debugger's source viewer: nobody waits for an answer; what they do
;;;                 (open a trail) they do themselves, from their event handler.
;;;   RUN-OVERLAY   puts it up and WAITS for its answer — a menu's pick, a typed value,
;;;                 a restart — because the caller is a function that wants a return
;;;                 value (CHOOSE-AND-RUN, SET-FIELD), or, for the debugger, because the
;;;                 erroring stack must stay standing under it. It waits by turning the
;;;                 backend's loop (*WORLD-CYCLE*) — the SAME loop — until the overlay
;;;                 closes. So a terminal keeps scrolling behind a menu, the clock ticks
;;;                 behind the debugger. Squeak's modal dialogs wait the same way (a
;;;                 loop of "world doOneCycle" until the dialog is gone — from the
;;;                 literature, not checked against a Squeak image here).
;;;
;;; What does stop behind a WAITING overlay: stepping (*BLOCKING-OVERLAYS*), because
;;; the computation that waits may be the one a step would run again — a stage whose
;;; contact raised the debugger would raise it again every 33 ms — and the agent's tick
;;; (it runs tool calls on this thread). Input, the services, repair and the clock go on.

(defvar *world-cycle* nil
  "One turn of the backend's loop, as a function of no arguments: repair and send,
   wait for something to happen, route it, run the services, step the morphs; :QUIT
   when the environment must end. The guest installs FB-CYCLE (fb-browser.lisp).
   RUN-OVERLAY turns it while it waits. NIL — a headless render, the X11 host — means
   nothing can wait: a blocking overlay answers NIL at once.")

(defvar *world-quit* nil
  "Set when a turn taken inside RUN-OVERLAY saw the environment end (the console went
   away): the overlay closes, and the outer loop ends too.")

(defvar *blocking-overlays* 0
  "How many RUN-OVERLAY waits are on the stack. While any is, nothing steps.")

(defclass overlay (lol.morph:morph)
  ((done   :initform nil :accessor overlay-done
           :documentation "Closed: the wait (if any) ends.")
   (result :initform nil :accessor overlay-result
           :documentation "What RUN-OVERLAY returns: the pick, the text, …")
   (opaque :initarg :opaque :initform nil :reader overlay-opaque
           :documentation "Does it paint every pixel of its bounds (the debugger)? A
   card over a scrim does not: what is under it shows, dimmed."))
  (:documentation "A modal card on top of the desk (see above). Subclasses define
   OVERLAY-DRAW and OVERLAY-EVENT; OVERLAY-CHANGED says it must be drawn again."))

(defmethod lol.morph:morph-opaque-p ((o overlay)) (overlay-opaque o))
(defmethod lol.morph:morph-name ((o overlay))
  (format nil "overlay · ~(~a~)" (class-name (class-of o))))

(defgeneric overlay-draw (overlay canvas w h)
  (:documentation "Paint OVERLAY over CANVAS, a screen W x H that already holds what
   is beneath it (the world drew that first). The shell's card drawers fit as they
   are: their backdrop is switched off (NO-BACKDROP), since the world drew it.")
  (:method ((o overlay) canvas w h) (declare (ignore canvas w h)) nil))

(defgeneric overlay-event (overlay event)
  (:documentation "Every input event while OVERLAY is on top. Close it (CLOSE-OVERLAY)
   to answer; OVERLAY-CHANGED when what it shows changed.")
  (:method ((o overlay) e) (declare (ignore e)) nil))

(defun no-backdrop (canvas w h)
  "The backdrop of a card drawn as a morph: nothing — the world drew it already."
  (declare (ignore canvas w h))
  nil)

(defmethod lol.morph:draw-morph ((o overlay) canvas clip)
  (declare (ignore clip))
  (destructuring-bind (x y w h) (lol.morph:morph-bounds *world*)
    (declare (ignore x y))
    (let ((*backdrop-fn* 'no-backdrop)) (overlay-draw o canvas w h))))

(defun desk-overlays (&optional (desk *world*))
  (and desk (remove-if-not (lambda (m) (typep m 'overlay)) (lol.morph:morph-children desk))))

(defun top-overlay (&optional (desk *world*))
  "The overlay that has the input: the topmost one still open."
  (find-if (lambda (o) (not (overlay-done o))) (reverse (desk-overlays desk))))

(defun overlay-changed (ov &optional rect)
  "OV must be drawn again — all of it, or RECT (screen coordinates) of it."
  (lol.morph:invalidate ov rect))

(defun open-overlay (ov &optional bounds)
  "Put OV on top of the desk (BOUNDS, else the whole screen) and return it at once."
  (setf (lol.morph:morph-bounds ov) (or bounds (copy-list (lol.morph:morph-bounds *world*))))
  (lol.morph:add-morph *world* ov)
  ov)

(defun close-overlay (ov &optional result)
  "OV answers RESULT and goes; what it covered is repaired."
  (setf (overlay-done ov) t (overlay-result ov) result)
  (when (lol.morph:morph-owner ov) (lol.morph:remove-morph ov))
  result)

(defun call-at-top-level (fn)
  "Run FN as the loop's own top level would: with the shell's values of the hit-map
   specials, not those of a window whose event is being handled. A blocking overlay
   opened from INSIDE a window (a menu, the debugger) turns the whole loop from there,
   and that loop draws the shell and dispatches to everything — so the window's
   bindings are set aside for the duration, and whatever the shell recorded meanwhile
   (its hit maps) is written back to the top level before the window's come back."
  (if (null *window-context*)
      (funcall fn)
      (let ((syms (window-special-symbols)))
        (progv syms (mapcar #'sb-ext:symbol-global-value syms)
          (unwind-protect
               (let ((*window-context* nil) (*shell-specials* nil)
                     (*chooser* (sb-ext:symbol-global-value '*chooser*)))
                 (funcall fn))
            (dolist (s syms) (setf (sb-ext:symbol-global-value s) (symbol-value s))))))))

(defun run-overlay (ov &optional bounds)
  "Open OV and wait for its answer, turning the backend's loop meanwhile (see above);
   -> OVERLAY-RESULT. However the wait ends — an answer, or a restart that unwinds
   straight through it (the debugger's) — the overlay goes. With no loop to turn
   (*WORLD-CYCLE* NIL) or no world, nothing can wait: NIL at once."
  (cond
    ((or (null *world*) (null *world-cycle*)) nil)
    (t
     (open-overlay ov bounds)
     (let ((*blocking-overlays* (1+ *blocking-overlays*)))
       (unwind-protect
            (call-at-top-level
             (lambda ()
               (loop until (overlay-done ov)
                     do (when (eq :quit (funcall *world-cycle*))
                          (setf *world-quit* t)
                          (close-overlay ov nil)))))
         (unless (overlay-done ov) (close-overlay ov nil))))
     (overlay-result ov))))

;;; ===========================================================================
;;; The halo acts on morphs (step 3)
;;; ===========================================================================
;;;
;;; The halo already lists each morph under the pointer as a layer (MORPH-HALO-LAYERS
;;; above), so walking out with Up reaches the window, then the shell. On a WINDOW's
;;; layer it now offers what Morphic's halo is for — handles that act on the morph
;;; itself: Move and Resize (the hand carries it, no button held, until a click puts it
;;; down; arrows and C-arrows work meanwhile, Return or Esc stop), Raise, Close and
;;; Re-tile. They come first, before Inspect and the Code handles every element has.
;;; Inspect on the window shows the TRAIL-WINDOW instance, and its BOUNDS field is
;;; settable: C-c C-c, type (list 100 80 700 450), and the window moves — through
;;; SET-MORPH-BOUNDS, so the screen hears of it (INSPECT-SLOT-SETTER below).

(defun focus-window (win)
  "Give WIN the focus (its trail becomes *ACTIVE*); DESK-SYNC raises it."
  (let ((i (window-trail-index win)))
    (when i (setf *active* i))
    (lol.morph:raise-morph win)))

(defun halo-carry (win mode)
  "The halo's Move (MODE :MOVE) or Resize (:RESIZE): the hand picks WIN up where the
   pointer is, so moving the pointer moves (or resizes) it with no button held; a
   click puts it down. Grab mode is on meanwhile, so the arrows (C-arrows) move
   (resize) it too, and Return or Esc put it down where it is."
  (focus-window win)
  (lol.morph:hand-pick-up (lol.morph:world-hand *world*) win mode)
  (setf (desk-grab *world*) win
        *browser-status* (if (eq mode :resize)
                             "resizing: move the pointer, click to finish · C-arrows too · Ret/Esc done"
                             "moving: move the pointer, click to put it down · arrows too · Ret/Esc done"))
  (shell-changed (status-rect)))

(defmethod lol.morph:morph-affordances ((w trail-window))
  (list (list :label "Move"    :action (lambda () (halo-carry w :move)))
        (list :label "Resize"  :action (lambda () (halo-carry w :resize)))
        (list :label "Raise"   :action (lambda () (focus-window w)))
        (list :label "Close"   :action (lambda () (close-window-trail w)))
        (list :label "Re-tile" :action (lambda () (retile-window w)))))

(defun morph-halo-entries (layer)
  "The halo's handles for a morph's layer: its MORPH-AFFORDANCES, as chooser entries."
  (let ((m (getf layer :object)))
    (when (typep m 'lol.morph:morph)
      (mapcar (lambda (a) (menu-entry (getf a :label) (getf a :action)
                                      :group "halo" :tone :restart))
              (lol.morph:morph-affordances m)))))

(setf *halo-extra-entries* 'morph-halo-entries)

;; the inspector's fields of a morph (model.lisp, INSPECT-PLACES): BOUNDS is set the
;; way that damages the screen; OWNER and CHILDREN are the tree, which only ADD-MORPH /
;; REMOVE-MORPH may change — shown, not settable
(defmethod inspect-slot-setter ((m lol.morph:morph) (slot (eql 'lol.morph::bounds)))
  (lambda (new) (lol.morph:set-morph-bounds m new)))
(defmethod inspect-slot-setter ((m lol.morph:morph) (slot (eql 'lol.morph::owner))) nil)
(defmethod inspect-slot-setter ((m lol.morph:morph) (slot (eql 'lol.morph::children))) nil)

;;; ===========================================================================
;;; Stepping (step 5): live panes step themselves
;;; ===========================================================================
;;;
;;; The shell morph and each window HOLD a pane (their trail's current one). When that
;;; pane changes on its own, the holder steps: MORPH-STEP-MS answers a period from the
;;; pane — a running stage 33 ms, a run pane whose program is live (or whose view is
;;; behind its output) 100 ms — and NIL otherwise, so a holder whose pane is still costs
;;; nothing and the poll sleeps. STEP-MORPH advances the pane and damages exactly what
;;; it changed: a stage its BODY (the viewport and the transcript — not the tabs, the
;;; breadcrumbs or the status line), a run pane its holder. A stage in a hidden tab
;;; does not step at all: nothing holds it.

(defgeneric holder-pane (morph)
  (:documentation "The pane MORPH shows, or NIL.")
  (:method ((m lol.morph:morph)) nil))

(defun window-pane (w) (let ((tr (window-trail w))) (and tr (car (last tr)))))

(defmethod holder-pane ((m shell-morph))
  (and *trails* (car (last (nth (tiled-index) *trails*)))))
(defmethod holder-pane ((w trail-window)) (window-pane w))

(defun holder-trail (m)
  (cond ((typep m 'shell-morph) (and *trails* (nth (tiled-index) *trails*)))
        ((typep m 'trail-window) (window-trail m))))

(defun holder-special (m sym)
  "MORPH's value of the shell special SYM: the shell's at the top level, a window's
   from its own saved set."
  (if (typep m 'trail-window)
      (cdr (assoc sym (window-specials m)))
      (and (boundp sym) (symbol-value sym))))

(defun holder-changed (m &optional rect)
  "Damage RECT (the holder's own coordinates; NIL = all) of a pane holder."
  (if (typep m 'trail-window) (window-changed m rect) (shell-changed rect m)))

(defparameter +stage-frame-ms+ 33
  "Milliseconds between stage ticks — about thirty a second. Fast enough that motion
   reads as motion, slow enough that a whole repaint in Lisp keeps up on the guest.")

(defparameter +stage-max-catchup+ 6
  "Most ticks one step may run. Without a cap, a stage left behind a modal debugger
   would try to replay the whole pause in a single frame the moment it came back.")

(defvar *stage-last-ticks* (make-hash-table :test 'eq :weakness :key)
  "Each stage's internal-real-time at its previous tick, so it advances by WALL CLOCK.")

(defun pane-running-stage (p)
  (and p (pane-view p) (eq (view-kind (pane-view p)) :stage)
       (let ((st (view-data (pane-view p)))) (and (stage-running st) st))))

(defun pane-step-ms (p)
  "How often a holder showing P must step: a running stage's frame, a run pane's
   tick, or NIL — P is still."
  (cond ((null p) nil)
        ((pane-running-stage p) +stage-frame-ms+)
        ((run-pane-step-ms p))
        (t nil)))

(defmethod lol.morph:morph-step-ms ((m shell-morph))
  (and (zerop *blocking-overlays*) (pane-step-ms (holder-pane m))))
(defmethod lol.morph:morph-step-ms ((w trail-window))
  (and (zerop *blocking-overlays*) (pane-step-ms (holder-pane w))))

(defun step-stage-pane (p)
  "Advance P's stage by however many ticks the wall clock says are due. -> :LAYOUT
   when the step changed more than the stage's body (the debugger came up, or a
   [define-method] template opened the Workspace beside it), else T.

   Not one tick per step: a full repaint in Lisp can cost more than a frame period, so
   one-per-frame would make the world's speed a function of how expensive the pane is
   to draw. Taking the elapsed time and stepping the arrears keeps the simulation rate
   fixed at +STAGE-FRAME-MS+ regardless of what the renderer manages.

   A contact here can signal — that is the point of the whole view — and the signal
   reaches RUN-DEBUGGER-FB through the HANDLER-BIND below exactly as one from the
   Workspace does (WORKSPACE-DEBUG-IT), with the stack still standing and the stage's
   own restarts on the rail. (It used to be TICK-LIVE-STAGE in fb-browser.lisp, for
   the current pane only; as a step it runs for every pane on screen.)"
  (let* ((st (pane-running-stage p))
         (now (get-internal-real-time))
         (elapsed (round (* 1000 (- now (gethash st *stage-last-ticks* now)))
                         internal-time-units-per-second))
         (due (max 1 (min +stage-max-catchup+ (floor elapsed +stage-frame-ms+))))
         (layout nil))
    (setf (gethash st *stage-last-ticks*) now)
    ;; ARMED, exactly as workspace-debug-it arms an eval, and for the same reason:
    ;; HANDLER-BIND runs its handler BEFORE the unwind, so the debugger sees the live
    ;; restarts and the standing stack.
    (restart-case
        (handler-bind ((serious-condition
                         (lambda (c) (setf layout t) (funcall 'run-debugger-fb c))))
          (dotimes (i due) (step-stage st)))
      (abort ()
        :report "Return to the stage (paused)"
        ;; Pausing is the honest outcome: nothing resolved the contact, and the two
        ;; bodies are still overlapping. Resuming would re-signal a second later — a
        ;; modal that reopens on a timer is not a debugger, it is a trap.
        (setf (stage-running st) nil)
        (stage-log st "aborted — stage paused; the contact is unresolved" :warn)))
    ;; [define-method] left a template behind: hand it to the Workspace beside the
    ;; stage — here, not in the restart, whose job is to unwind
    (when (and (pane-view p) (eq (view-kind (pane-view p)) :stage) (stage-take-template p))
      (setf layout t))
    (if layout :layout t)))

(defun step-holder (m)
  (let ((p (holder-pane m)))
    (cond ((null p) nil)
          ((pane-running-stage p)
           (if (eq :layout (step-stage-pane p))
               (holder-changed m)
               (holder-changed m (holder-special m '*stage-body-rect*))))
          ((step-run-panes p) (holder-changed m)))))

(defmethod lol.morph:step-morph ((m shell-morph) now)
  (declare (ignore now))
  (step-holder m))
(defmethod lol.morph:step-morph ((w trail-window) now)
  (declare (ignore now))
  (step-holder w))

;;; ---- what the services changed, exactly ---------------------------------------------

(defun visible-trails ()
  "The trails on screen: the tiled one, and every window's."
  (if *world*
      (remove nil (cons (holder-trail (desk-shell *world*))
                        (mapcar #'window-trail (desk-windows))))
      (list (current-trail))))

(defun holder-shows-pane-p (m p)
  (or (eq p (holder-special m '*preview-pane*))
      (some (lambda (q) (or (eq q p) (eq (pane-side q) p))) (holder-trail m))))

(defun damage-pane (p)
  "P changed on its own (a live worker's view arrived): redraw whichever morph shows
   it, and nothing else — a pane nobody shows costs nothing."
  (when *world*
    (let ((sh (desk-shell *world*)))
      (when (holder-shows-pane-p sh p) (shell-changed nil sh)))
    (dolist (w (desk-windows))
      (when (holder-shows-pane-p w p) (window-changed w)))))

(defun terminal-rows-to-world (canvas)
  "The terminals' programs wrote: repaint only the ROWS that changed. The tiled
   terminal's straight into the screen's CANVAS — the world composites anything over
   them and sends them (SHELL-PAINTED); a window's into its backing store, damaged
   rect by rect, so the repair copies just those rows. A terminal that cannot be
   updated this way (a side pane, a grid that no longer fits) is drawn again whole."
  (let ((sh (desk-shell *world*)))
    (when (terminal-pane-p (holder-pane sh))
      (let ((rects (let ((*active* (tiled-index))) (terminal-paint-dirty canvas))))
        (if rects (shell-painted rects) (shell-changed)))))
  (dolist (w (desk-windows))
    (when (and (terminal-pane-p (window-pane w)) (window-trail-index w))
      (if (or (window-stale w) (null (window-backing w)))
          (window-changed w)
          (let ((rects (let ((*active* (window-trail-index w)))
                         (with-window-context (w) (terminal-paint-dirty (window-backing w))))))
            (if rects
                (destructuring-bind (x y ww wh) (lol.morph:morph-bounds w)
                  (declare (ignore ww wh))
                  (dolist (r rects)
                    (lol.morph:invalidate w (list (+ x (first r)) (+ y (second r)) (third r) (fourth r)))))
                (window-changed w)))))))
