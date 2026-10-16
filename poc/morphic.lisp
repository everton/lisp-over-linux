;;;; morphic.lisp — the browser as the first window of a Lisp window system.
;;;;
;;;; poc/morph.lisp is Morphic's core and knows nothing about the browser. This file
;;;; is the browser's side of it (doc/morphic.org §2, steps 1–2):
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
;;;;                 dropping it on the strip.
;;;;
;;;; Backend-blind like the shell: the guest's loop (initramfs/fb-browser.lisp) and the
;;;; X11 host loop (poc/run-browser.lisp) both build events, hand them to
;;;; LOL.MORPH:WORLD-DISPATCH, and send whatever LOL.MORPH:WORLD-REPAIR redrew.

(in-package :cl-user)

(defvar *world* nil
  "The desk the environment is running in (a DESK), or NIL outside it. Made fresh by
   each entry point (run-browser-fb, run-browser.lisp, render-frame.lisp) for its
   canvas size.")

(defvar *screen-overdrawn* nil
  "Set when something drew the WHOLE screen behind the world's back — a modal loop
   (the Spotter, a menu, the debugger) presents its own frames. The world then repairs
   everything, since it no longer knows what is on the glass.")

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

(defun make-desk (w h)
  "A world W x H holding the tiled shell, focused, all of it damaged: the first
   repair draws the whole frame."
  (let* ((shell (make-instance 'shell-morph :bounds (list 0 0 w h)))
         (desk (make-instance 'desk :bounds (list 0 0 w h) :shell shell)))
    (lol.morph:add-morph desk shell)
    (setf (lol.morph:world-focus desk) shell
          (desk-tiled-root desk) (and *trails* (first (nth *active* *trails*)))
          (desk-font-size desk) (and *bfont* (lol.canvas:font-size *bfont*)))
    desk))

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
    *matrix-cells* *stage-rect* *editing-side* *drag-tv* *drag-origin* *term-drag*
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
             :documentation "Must the backing store be drawn again before it is shown?")
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
   as the tiled shell draws one (DRAW-TRAIL-AREA), in the window's own context."
  (destructuring-bind (x y w h) (lol.morph:morph-bounds win)
    (declare (ignore x y))
    (let ((c (window-backing win)))
      (unless (and c (= (lol.canvas:canvas-w c) w) (= (lol.canvas:canvas-h c) h))
        (setf c (setf (window-backing win) (lol.canvas:make-canvas w h))))
      (let ((i (window-trail-index win)) (focused (eq win (focused-window))))
        (lol.canvas:fill-rect c 0 0 w h *ui-bg*)
        (when i
          (let ((*active* i))
            (with-window-context (win)
              (setf *crumb-rects* '() *split-x* nil *drawn* '())
              (note-drawn 0 0 w h win '(render-window) "the window")
              (draw-window-title c w win focused)
              (draw-trail-area c w h (+ +bar-h+ 4) 6)
              (draw-window-frame c w h focused)))))
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

(defun window-changed (win)
  "WIN's contents changed: draw them again, and show it."
  (setf (window-stale win) t)
  (lol.morph:invalidate win))

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
  "A service's fd woke the poll (a pty, Chrome's pipe, a worker): redraw the live
   windows — and the shell's live pane too while a window has the focus, since the
   services then skip the fast paths that would have painted it."
  (refresh-live-windows)
  (when (and (focused-window)
             (pane-live-p (car (last (nth (tiled-index) *trails*)))))
    (shell-changed)))

(defun status-rect ()
  (destructuring-bind (x y w h) (lol.morph:morph-bounds *world*)
    (declare (ignore x y))
    (list 0 (- h (status-h)) w (status-h))))

(defun recomposite-windows (canvas rects)
  "Something painted the SHELL's pixels straight into CANVAS over RECTS — the fast
   paths that skip the world: the clock's tick, a terminal's rows, a web frame. Copy
   back whatever windows lie over them, so they stay on top. No windows, no work."
  (dolist (win (desk-windows))
    (when (window-backing win)
      (destructuring-bind (x y w h) (lol.morph:morph-bounds win)
        (dolist (r rects)
          (let ((i (lol.morph:rect-intersect r (list x y w h))))
            (when i
              (destructuring-bind (ix iy iw ih) i
                (lol.canvas:with-clip (canvas ix iy iw ih)
                  (lol.canvas:blit-canvas canvas (window-backing win) x y))))))))))

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
   font size change redraws everything; a modal's frames mean the whole screen is
   repaired. Run before and after every event."
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
  (when (shiftf *screen-overdrawn* nil)
    (lol.morph:invalidate desk)))

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
      (loop for m in (lol.morph:morphs-at *world* (+ x ox) (+ y oy))
            for (bx by bw bh) = (lol.morph:morph-bounds m)
            collect (halo-layer (lol.morph:morph-name m)
                                (format nil "the morph ~a — ~(~a~)" (lol.morph:morph-name m)
                                        (class-name (class-of m)))
                                m (list (- bx ox) (- by oy) bw bh) :element
                                '(lol.morph:draw-morph lol.morph:handle-event))))))

(setf *halo-outer-layers* 'morph-halo-layers)
