;;;; morph.lisp — Morphic's three objects: the MORPH, the WORLD and the HAND.
;;;;
;;;; doc/morphic.org is the design; this file is its backend-blind core (Phase 7 of
;;;; the code browser, steps 1–5). It knows nothing about the browser, the
;;;; framebuffer or X11 — only rectangles, a canvas to draw into and events to route.
;;;; The browser's side (the tiled shell as one morph, trails torn off into windows)
;;;; is poc/morphic.lisp; the loops that feed events in and send pixels out are
;;;; initramfs/fb-browser.lisp (the guest) and poc/run-browser.lisp (X11).
;;;;
;;;; In one paragraph, the vocabulary (Maloney & Smith, UIST 1995, carried into
;;;; Squeak): a MORPH is an object that is on screen — it has BOUNDS, an OWNER and
;;;; CHILDREN, and it answers "draw yourself" and "here is an event". The WORLD is the
;;;; root morph, the whole screen; the order of its children is the Z-ORDER, later on
;;;; top. The HAND is the pointer as an object: it remembers which morph a press went
;;;; to (so the drag and the release go there too) and which morph it is CARRYING
;;;; (a window being dragged). DAMAGE: a morph that changed says which rectangle of
;;;; the screen is now wrong (INVALIDATE); once per cycle the world redraws only those
;;;; rectangles, and in each one only the morphs that overlap it (WORLD-REPAIR).
;;;; STEPPING: a morph that changes on its own asks to be stepped every N ms; the
;;;; world keeps the deadlines (WORLD-STEP, WORLD-STEP-TIMEOUT), and with nothing to
;;;; step the backend's poll sleeps until input.
;;;;
;;;; Morphs are CLOS objects (STANDARD-CLASS), on purpose: the model layer, the
;;;; Inspector, ALL-INSTANCES and the halo already understand CLOS instances, so a
;;;; window is inspectable — its bounds as editable fields — with no code written for
;;;; that. Coordinates are WORLD pixels, not owner-relative (as in Squeak): hit-testing
;;;; and damage are then plain rectangle comparisons, with no transform stack.
;;;;
;;;; A rectangle is a list (x y w h) throughout, as everywhere else in the shell.

(defpackage :lol.morph
  (:use :cl)
  (:export
   ;; rectangles
   #:rect-area #:rect-empty-p #:rect-intersect #:rect-union #:rect-subtract
   #:rect-contains-p #:rect-contains-point-p #:rect-overlap-p
   ;; the morph
   #:morph #:morph-bounds #:morph-owner #:morph-children #:morph-step-ms #:morph-subject
   #:morph-draws #:morph-opaque-p #:morph-name
   #:draw-morph #:handle-event #:step-morph #:morph-affordances #:drop-morph
   #:add-morph #:remove-morph #:raise-morph #:morph-world #:morph-at #:morphs-at
   #:move-morph #:resize-morph #:set-morph-bounds #:morph-descendants #:morph-steps
   ;; damage
   #:invalidate #:merge-damage #:note-painted
   ;; the world and the hand
   #:world #:make-world #:world-hand #:world-focus #:world-damage #:world-last-repair
   #:world-repairs #:world-damaged-p #:world-repair #:world-dispatch #:world-step
   #:world-step-timeout #:world-after-dispatch #:world-painted
   #:hand #:hand-x #:hand-y #:hand-capture #:hand-carrying #:hand-mode
   #:hand-grab-dx #:hand-grab-dy #:hand-pick-up #:hand-drop #:now-ms
   #:+max-damage-rects+
   ;; events
   #:event #:make-event #:event-kind #:event-key #:event-state #:event-x #:event-y
   #:event-notches #:event-data))

(in-package :lol.morph)

;;; ---- rectangles ------------------------------------------------------------

(defun rect-area (r) (* (third r) (fourth r)))
(defun rect-empty-p (r) (or (null r) (<= (third r) 0) (<= (fourth r) 0)))

(defun rect-intersect (a b)
  "The rect A and B share, or NIL when they share no pixel."
  (destructuring-bind (ax ay aw ah) a
    (destructuring-bind (bx by bw bh) b
      (let ((x0 (max ax bx)) (y0 (max ay by))
            (x1 (min (+ ax aw) (+ bx bw))) (y1 (min (+ ay ah) (+ by bh))))
        (and (< x0 x1) (< y0 y1) (list x0 y0 (- x1 x0) (- y1 y0)))))))

(defun rect-overlap-p (a b) (and (rect-intersect a b) t))

(defun rect-union (a b)
  "The smallest rect holding both A and B."
  (destructuring-bind (ax ay aw ah) a
    (destructuring-bind (bx by bw bh) b
      (let ((x0 (min ax bx)) (y0 (min ay by))
            (x1 (max (+ ax aw) (+ bx bw))) (y1 (max (+ ay ah) (+ by bh))))
        (list x0 y0 (- x1 x0) (- y1 y0))))))

(defun rect-contains-p (outer inner)
  "Does OUTER hold every pixel of INNER?"
  (destructuring-bind (ox oy ow oh) outer
    (destructuring-bind (ix iy iw ih) inner
      (and (<= ox ix) (<= oy iy) (<= (+ ix iw) (+ ox ow)) (<= (+ iy ih) (+ oy oh))))))

(defun rect-contains-point-p (r x y)
  (destructuring-bind (rx ry rw rh) r
    (and (<= rx x) (< x (+ rx rw)) (<= ry y) (< y (+ ry rh)))))

(defun rect-subtract (a b)
  "A minus B, as at most four disjoint rects: the bands of A above and below B, then
   the parts left and right of B within B's rows. This is what a MOVED window
   uncovers — its old bounds minus its new ones — and so what the morphs below it
   must repaint; everything inside the new bounds the window covers itself."
  (let ((i (rect-intersect a b)))
    (if (null i)
        (list a)
        (destructuring-bind (ax ay aw ah) a
          (destructuring-bind (ix iy iw ih) i
            (remove-if #'rect-empty-p
                       (list (list ax ay aw (- iy ay))                              ; above
                             (list ax (+ iy ih) aw (- (+ ay ah) (+ iy ih)))         ; below
                             (list ax iy (- ix ax) ih)                              ; left
                             (list (+ ix iw) iy (- (+ ax aw) (+ ix iw)) ih))))))))  ; right

;;; ---- the morph -------------------------------------------------------------

(defclass morph ()
  ((bounds   :initarg :bounds :initform (list 0 0 0 0) :accessor morph-bounds
             :documentation "(x y w h) in WORLD pixels.")
   (owner    :initform nil :accessor morph-owner
             :documentation "The morph this one is a child of; NIL for the world.")
   (children :initform '() :accessor morph-children
             :documentation "Back to front: the LAST child is drawn last, so on top.")
   (step-ms  :initarg :step-ms :initform nil :accessor morph-step-ms
             :documentation "Step this morph every N ms (liveness); NIL = never.")
   (subject  :initarg :subject :initform nil :accessor morph-subject
             :documentation "What the morph shows, if anything (a trail, a subject).")
   (draws    :initform 0 :accessor morph-draws
             :documentation "How many times DRAW-MORPH ran on it: the damage tests'
   evidence that a repair drew only the morphs it had to.")
   (step-due :initform nil :accessor morph-step-due
             :documentation "Internal-real-time (ms) at which it is next stepped; NIL
   while it does not step. Kept by WORLD-STEP.")
   (steps    :initform 0 :accessor morph-steps
             :documentation "How many times STEP-MORPH ran on it (the tests read it)."))
  (:documentation "Something on screen: a rectangle that can draw itself and take
   events. Subclasses override DRAW-MORPH and HANDLE-EVENT; everything else —
   z-order, hit-testing, damage, the hand — the world does for them."))

(defmethod print-object ((m morph) stream)
  (print-unreadable-object (m stream :type t :identity t)
    (format stream "~{~d~^ ~}" (morph-bounds m))))

(defgeneric morph-name (morph)
  (:documentation "A short human name, for the halo and the inspector.")
  (:method ((m morph)) (string-downcase (class-name (class-of m)))))

(defgeneric draw-morph (morph canvas clip)
  (:documentation "Paint MORPH into CANVAS. CLIP is the damaged rect being repaired;
   the canvas is already clipped to it (lol.canvas:with-clip), so a morph may simply
   draw everything and only the damaged pixels change — or use CLIP to skip work.")
  (:method ((m morph) canvas clip) (declare (ignore canvas clip)) nil))

(defgeneric handle-event (morph event)
  (:documentation "Take EVENT. Return NIL to let it bubble to the owner; anything else
   consumes it (:QUIT leaves the loop).")
  (:method ((m morph) event) (declare (ignore event)) nil))

(defgeneric step-morph (morph now-ms)
  (:documentation "Liveness: called every MORPH-STEP-MS. INVALIDATE if you changed.
   MORPH-STEP-MS is read afresh every cycle, so a morph may compute it (a method on
   the reader): NIL the moment it has nothing to animate, and the world sleeps.")
  (:method ((m morph) now) (declare (ignore now)) nil))

(defgeneric morph-affordances (morph)
  (:documentation "What the halo may offer on this morph (step 3: move, resize, …).")
  (:method ((m morph)) nil))

(defgeneric morph-opaque-p (morph)
  (:documentation "Does MORPH paint every pixel of its bounds? A repair starts from
   the topmost opaque morph that covers the damaged rect: nothing under it can show.")
  (:method ((m morph)) nil))

(defgeneric drop-morph (morph world x y)
  (:documentation "The hand let go of MORPH at X,Y (after a carry).")
  (:method ((m morph) world x y) (declare (ignore world x y)) nil))

(defun morph-world (m)
  "The world M lives in (walking owners), or NIL when it is not on screen."
  (loop for o = m then (morph-owner o)
        while o
        when (typep o 'world) return o))

(defun morph-descendants (m)
  "M's children and theirs, back to front (each child before its own children)."
  (loop for c in (morph-children m)
        collect c
        append (morph-descendants c)))

(defun add-morph (owner m)
  "Put M on top of OWNER's children, and mark its rect damaged."
  (when (morph-owner m) (remove-morph m))
  (setf (morph-owner m) owner
        (morph-children owner) (append (morph-children owner) (list m)))
  (invalidate m)
  m)

(defun remove-morph (m)
  "Take M off screen: what it covered is damaged (the morphs under it show again)."
  (let ((owner (morph-owner m)))
    (when owner
      (invalidate m)                     ; while it can still find its world
      (setf (morph-children owner) (remove m (morph-children owner))
            (morph-owner m) nil)
      (let ((w (morph-world owner)))
        (when w
          (when (eq (world-focus w) m) (setf (world-focus w) nil))
          (let ((h (world-hand w)))
            (when (eq (hand-capture h) m) (setf (hand-capture h) nil))
            (when (eq (hand-carrying h) m) (setf (hand-carrying h) nil)))))))
  m)

(defun raise-morph (m)
  "Bring M to the top of its owner's z-order. Damages it only if that changes
   anything — raising the top morph again costs nothing."
  (let ((owner (morph-owner m)))
    (when (and owner (not (eq m (car (last (morph-children owner))))))
      (setf (morph-children owner) (append (remove m (morph-children owner)) (list m)))
      (invalidate m)))
  m)

(defun morphs-at (m x y)
  "Every morph under X,Y in M's tree, TOPMOST first (the order a click looks in)."
  (let ((out '()))
    (labels ((walk (n)
               (when (rect-contains-point-p (morph-bounds n) x y)
                 (push n out)
                 (mapc #'walk (morph-children n)))))
      (mapc #'walk (morph-children m)))
    out))

(defun morph-at (m x y)
  "The topmost morph under X,Y in M's tree, or M itself when nothing is."
  (or (first (morphs-at m x y)) m))

(defun %offset-tree (m dx dy)
  (destructuring-bind (x y w h) (morph-bounds m)
    (setf (morph-bounds m) (list (+ x dx) (+ y dy) w h)))
  (dolist (c (morph-children m)) (%offset-tree c dx dy)))

(defun move-morph (m x y)
  "Move M (and its subtree) so its top-left is X,Y. Damage: what M uncovered (its
   old bounds minus its new) for the morphs beneath, and its new bounds for itself.
   Not the union: on a short drag the union is nearly two windows of shell, while
   the two pieces are a thin L of shell plus a window that only needs copying."
  (let* ((old (morph-bounds m))
         (dx (- x (first old))) (dy (- y (second old))))
    (unless (and (zerop dx) (zerop dy))
      (%offset-tree m dx dy)
      (dolist (r (rect-subtract old (morph-bounds m))) (invalidate m r))
      (invalidate m))
    m))

(defun resize-morph (m w h)
  "Give M a new size (top-left fixed): damages the old and new bounds."
  (destructuring-bind (x y ow oh) (morph-bounds m)
    (unless (and (= w ow) (= h oh))
      (invalidate m)
      (setf (morph-bounds m) (list x y w h))
      (invalidate m)))
  m)

(defun set-morph-bounds (m rect)
  "Put M at RECT (x y w h) — a move then a resize, each damaging what it must. This,
   not (SETF MORPH-BOUNDS), is how bounds are CHANGED: the accessor writes the slot
   and nothing else, so the screen would never hear of it. The inspector's bounds
   field calls this (poc/morphic.lisp, INSPECT-SLOT-SETTER), which is why editing a
   window's bounds there moves the window on screen (doc/morphic.org §6)."
  (unless (and (listp rect) (= 4 (length rect)) (every #'integerp rect))
    (error "bounds are a list of four integers (x y w h), not ~s" rect))
  (destructuring-bind (x y w h) rect
    (move-morph m x y)
    (resize-morph m (max 1 w) (max 1 h)))
  (morph-bounds m))

;;; ---- events -----------------------------------------------------------------
;;; One struct, built by each backend's loop from what it already decodes:
;;;
;;;   :key     KEY STATE        a decoded key and the X-style modifier mask
;;;   :menu                     the keyboard's Menu key (fb: from the evdev)
;;;   :down    X Y STATE        the left button pressed (STATE: Ctrl bit 2, Super 6)
;;;   :drag    X Y              moved with the left button held
;;;   :up      X Y              the left button released
;;;   :right   X Y              the right button (a context menu)
;;;   :middle  X Y STATE        the middle button (the halo)
;;;   :wheel   X Y NOTCHES      the wheel; NOTCHES > 0 is up, toward the start
;;;   :move    X Y              a bare pointer move: changes nothing on screen
;;;
;;; DATA is a plist for what only one backend knows (:tty T for a key that came from
;;; the console rather than the keyboard's evdev).

(defstruct event
  kind key (state 0) (x 0) (y 0) (notches 0) data)

;;; ---- the hand -----------------------------------------------------------------

(defclass hand (morph)
  ((x :initform 0 :accessor hand-x)
   (y :initform 0 :accessor hand-y)
   (capture  :initform nil :accessor hand-capture
             :documentation "The morph the last press went to: its drag and its
   release go there too, wherever the pointer has wandered — as in any toolkit, a
   drag that leaves the window it started in keeps talking to that window.")
   (carrying :initform nil :accessor hand-carrying
             :documentation "The morph the hand is dragging, or NIL.")
   (mode     :initform :move :accessor hand-mode
             :documentation ":MOVE drags the carried morph; :RESIZE drags its corner.")
   (grab-dx  :initform 0 :accessor hand-grab-dx)
   (grab-dy  :initform 0 :accessor hand-grab-dy))
  (:documentation "The pointer as a morph. It is NOT drawn into the canvas: the arrow
   is composited on the way out (doc/framebuffer.org §9.5), so a bare move still costs
   microseconds. What it adds is what it carries."))

(defparameter +min-carried-w+ 120 "A resize never makes a morph narrower than this.")
(defparameter +min-carried-h+ 80  "... or shorter than this.")

(defun hand-pick-up (hand morph mode)
  "Start carrying MORPH: :MOVE keeps the grab point under the pointer, :RESIZE keeps
   the bottom-right corner there. The morph stays in its owner's z-order (raised); we
   do not reparent it into the hand as Squeak does, because with world coordinates
   and opaque dragging there is nothing that would buy."
  (destructuring-bind (x y w h) (morph-bounds morph)
    (setf (hand-carrying hand) morph
          (hand-mode hand) mode
          (hand-grab-dx hand) (if (eq mode :resize) (- (+ x w) (hand-x hand)) (- (hand-x hand) x))
          (hand-grab-dy hand) (if (eq mode :resize) (- (+ y h) (hand-y hand)) (- (hand-y hand) y))))
  (raise-morph morph))

(defun hand-follow (hand)
  "The pointer moved while carrying: move (or resize) the carried morph to match."
  (let ((m (hand-carrying hand)))
    (destructuring-bind (x y w h) (morph-bounds m)
      (declare (ignore w h))
      (if (eq (hand-mode hand) :resize)
          (resize-morph m (max +min-carried-w+ (- (+ (hand-x hand) (hand-grab-dx hand)) x))
                          (max +min-carried-h+ (- (+ (hand-y hand) (hand-grab-dy hand)) y)))
          (move-morph m (- (hand-x hand) (hand-grab-dx hand))
                        (- (hand-y hand) (hand-grab-dy hand)))))))

(defun hand-drop (hand world)
  "Let go of what the hand carries; the morph is told where (DROP-MORPH)."
  (let ((m (shiftf (hand-carrying hand) nil)))
    (when m (drop-morph m world (hand-x hand) (hand-y hand)))
    m))

;;; ---- the world ----------------------------------------------------------------

(defparameter +max-damage-rects+ 16
  "More damaged rects than this in one cycle and the world repairs the whole screen:
   past a point, many small repairs cost more than one big one.")

(defclass world (morph)
  ((hand   :initform (make-instance 'hand) :accessor world-hand)
   (focus  :initform nil :accessor world-focus
           :documentation "The morph keys go to (then bubble up through owners).")
   (damage :initform '() :accessor world-damage
           :documentation "Rects invalidated since the last repair, newest first.")
   (last-repair :initform nil :accessor world-last-repair
                :documentation "What the last WORLD-REPAIR did: (:rects R :full F :ms MS
   :drawn ((morph . times) …) :per-rect ((rect morph …) …)) — for the tests and for
   measuring: which morphs drew, and in which rect.")
   (repairs :initform 0 :accessor world-repairs
            :documentation "How many repairs drew anything, ever.")
   (painted :initform '() :accessor world-painted
            :documentation "((RECT . MORPH) …): rects MORPH has already painted into the
   canvas itself, since the last repair (NOTE-PAINTED). The repair draws only what lies
   ABOVE that morph there, then sends them."))
  (:documentation "The root morph: the screen. Its children are the z-order."))

(defun make-world (w h &rest initargs)
  (apply #'make-instance 'world :bounds (list 0 0 w h) initargs))

(defun invalidate (m &optional rect)
  "Say that RECT (default: M's bounds) is wrong on screen. Cut to the world; a morph
   that is not on screen invalidates nothing."
  (let ((w (if (typep m 'world) m (morph-world m))))
    (when w
      (let ((r (rect-intersect (or rect (morph-bounds m)) (morph-bounds w))))
        (when r (push r (world-damage w))))))
  nil)

(defun world-damaged-p (world) (and (world-damage world) t))

(defun note-painted (world rects morph)
  "MORPH has just painted RECTS of itself straight into the screen's canvas — a fast
   path that is cheaper than its whole DRAW-MORPH: a terminal's changed rows, a web
   page's new frame, the systray's clock. Those pixels are right for MORPH but may now
   lie on top of something that should cover them: a window, an overlay's scrim. The
   next repair draws, in each rect, only the morphs ABOVE MORPH (back to front,
   clipped), and sends the rects. So a fast path need know nothing about what is over
   it — the world composites."
  (dolist (r rects)
    (let ((r (rect-intersect r (morph-bounds world))))
      (when r (push (cons r morph) (world-painted world)))))
  nil)

(defun merge-damage (rects screen)
  "RECTS made fewer: any two that OVERLAP become their union, until none do. Two that
   merely touch stay apart — merging a window's new bounds with the strip it just
   uncovered would turn a window-sized copy plus a thin strip of shell into a
   window-and-a-half of shell. Returns (values RECTS FULL-P); FULL-P when there were
   more than +MAX-DAMAGE-RECTS+, or one covered the screen, and RECTS is then the
   screen alone."
  (let ((rs (remove-if #'rect-empty-p (remove-duplicates rects :test #'equal))))
    (loop
      (let ((pair (loop for (a . rest) on rs
                        thereis (loop for b in rest
                                      when (rect-overlap-p a b) return (cons a b)))))
        (if pair
            (setf rs (cons (rect-union (car pair) (cdr pair))
                           (remove (cdr pair) (remove (car pair) rs :count 1) :count 1)))
            (return))))
    (if (or (> (length rs) +max-damage-rects+)
            (some (lambda (r) (rect-contains-p r screen)) rs))
        (values (list screen) t)
        (values rs nil))))

(defun world-repair (world canvas)
  "Redraw every damaged rect of WORLD into CANVAS, and only those: for each merged
   rect, clip to it and draw the morphs that overlap it, back to front — starting
   from the topmost OPAQUE morph that covers the whole rect, since nothing beneath
   that one can show. Before that, the rects a morph painted itself (NOTE-PAINTED)
   get only what lies above that morph — FIRST, because a damaged rect is drawn whole
   and anything composited over it afterwards would be composited twice (a scrim
   dimmed twice). Returns (values RECTS FULL-P): what the backend must now send to the
   screen (all of it when FULL-P). Nothing to do: (values NIL NIL)."
  (let ((damage (shiftf (world-damage world) '()))
        (painted (shiftf (world-painted world) '())))
    (if (and (null damage) (null painted))
        (values nil nil)
      (multiple-value-bind (rects full)
          (if damage (merge-damage damage (morph-bounds world)) (values '() nil))
        (let ((t0 (get-internal-real-time)) (drawn '()) (per-rect '()) (sent '())
              (all (morph-descendants world)))
          (flet ((draw-in (r todo &optional (record t))
                   (when record (push (cons r todo) per-rect))
                   (destructuring-bind (x y w h) r
                     (lol.canvas:with-clip (canvas x y w h)
                       (dolist (m todo)
                         (incf (morph-draws m))
                         (let ((e (assoc m drawn))) (if e (incf (cdr e)) (push (cons m 1) drawn)))
                         (draw-morph m canvas r)))))
                 (from-cover (over r)
                   (let ((base (position-if (lambda (m) (and (morph-opaque-p m)
                                                             (rect-contains-p (morph-bounds m) r)))
                                            over :from-end t)))
                     (if base (nthcdr base over) over))))
            (unless full
              (loop for (r . below) in (reverse painted)
                    unless (some (lambda (d) (rect-contains-p d r)) rects)
                      do (let ((above (remove-if-not (lambda (m) (rect-overlap-p (morph-bounds m) r))
                                                     (cdr (member below all)))))
                           (when above (draw-in r (from-cover above r) nil))
                           (push r sent))))
            (dolist (r rects)
              (draw-in r (from-cover (remove-if-not (lambda (m) (rect-overlap-p (morph-bounds m) r)) all)
                                     r))))
          (setf sent (nreverse sent))
          ;; LAST-REPAIR and REPAIRS describe repairs of DAMAGE — what the tests read;
          ;; a turn that only composited painted rects (the clock) leaves them alone
          (when rects
            (incf (world-repairs world))
            (setf (world-last-repair world)
                  (list :rects rects :full full :painted sent
                        :ms (/ (round (* 10000 (- (get-internal-real-time) t0))
                                      internal-time-units-per-second) 10.0)
                        :drawn (nreverse drawn) :per-rect (nreverse per-rect))))
          (values (append rects sent) full))))))

(defun bubble (m event)
  "Offer EVENT to M, then to each owner in turn, until one consumes it."
  (loop for n = m then (morph-owner n)
        while n
        for r = (handle-event n event)
        when r return r))

(defgeneric world-after-dispatch (world event)
  (:documentation "Called after every event WORLD-DISPATCH routed, whoever took it:
   the world's chance to tidy up what the event changed (the desk reconciles its
   windows with the trails there — poc/morphic.lisp).")
  (:method ((w world) event) (declare (ignore event)) nil))

(defun world-dispatch (world event)
  "Route EVENT: the world itself sees every event first (window-manager keys, say);
   then keys go to the FOCUS and pointer events to the morph under the pointer —
   except that a drag or a release goes to the morph that took the press (the hand's
   capture), and while the hand CARRIES a morph, the pointer moves it instead.
   Returns what the consuming HANDLE-EVENT returned (:QUIT to leave)."
  (let ((hand (world-hand world)) (kind (event-kind event)))
    (unless (member kind '(:key :menu))
      (setf (hand-x hand) (event-x event) (hand-y hand) (event-y event)))
    (prog1
        (cond
          ((and (hand-carrying hand) (member kind '(:drag :move)))
           (hand-follow hand) t)
          ;; carried without a button held (the halo's Move): the press that ends the
          ;; carry is the hand's, not the window's under it — its release drops
          ((and (hand-carrying hand) (eq kind :down)) t)
          ((and (hand-carrying hand) (eq kind :up))
           (setf (hand-capture hand) nil)
           (hand-drop hand world) t)
          ((handle-event world event))
          ((member kind '(:key :menu))
           (and (world-focus world) (bubble (world-focus world) event)))
          (t
           (let ((target (if (and (hand-capture hand) (member kind '(:drag :up)))
                             (hand-capture hand)
                             (morph-at world (event-x event) (event-y event)))))
             (when (eq kind :down) (setf (hand-capture hand) target))
             (prog1 (bubble target event)
               (when (eq kind :up) (setf (hand-capture hand) nil))))))
      (world-after-dispatch world event))))

;;; ---- stepping (liveness) ---------------------------------------------------------
;;; A morph that changes on its own — a running stage, a run pane whose program is
;;; printing — answers a period from MORPH-STEP-MS; the world keeps each one's next
;;; deadline and calls STEP-MORPH when it comes round. The backend's poll waits until
;;; the earliest deadline (WORLD-STEP-TIMEOUT), and when nothing steps that is -1:
;;; sleep until input. That is the whole of "idle costs nothing".

(defun now-ms ()
  (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second))

(defun world-step-timeout (world &optional (now (now-ms)))
  "Ms until the next morph is due a step, or -1 when none steps. A morph that has just
   started stepping gets its first deadline here, one period out."
  (let ((soonest nil))
    (dolist (m (morph-descendants world))
      (let ((ms (morph-step-ms m)))
        (if (null ms)
            (setf (morph-step-due m) nil)
            (let ((due (or (morph-step-due m) (setf (morph-step-due m) (+ now ms)))))
              (setf soonest (min (or soonest most-positive-fixnum) (max 0 (- due now))))))))
    (or soonest -1)))

(defun world-step (world &optional (now (now-ms)))
  "Step every morph whose deadline has come round, and set its next one. A morph's
   STEP-MORPH damages what it changed; nothing else is drawn. Returns how many stepped."
  (let ((n 0))
    (dolist (m (morph-descendants world))
      (let ((ms (morph-step-ms m)))
        (cond ((null ms) (setf (morph-step-due m) nil))
              ((null (morph-step-due m)) (setf (morph-step-due m) (+ now ms)))
              ((>= now (morph-step-due m))
               (setf (morph-step-due m) (+ now ms))
               (incf (morph-steps m)) (incf n)
               (step-morph m now)))))
    n))
