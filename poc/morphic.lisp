;;;; morphic.lisp — the browser as a tree of morphs: the first windows of a Lisp
;;;; window system.
;;;;
;;;; poc/morph.lisp is Morphic's core and knows nothing about the browser. This file
;;;; is the browser's side of it (doc/morphic.org §2 and §12). Everything the browser
;;;; puts on screen is a morph, in one tree:
;;;;
;;;;   THE DESK      the world the environment runs in: one per screen, *WORLD*.
;;;;   SHELL-MORPH   the tiled browser, a container in fixed bands: the TAB-STRIP (with
;;;;                 the systray, a TRAY, at its right end), a TRAIL-AREA, and the
;;;;                 STATUS-LINE (or the minibuffer) at the bottom.
;;;;   TRAIL-AREA    one trail on screen. Its children are the trail's PANEs — a pane is
;;;;                 a morph (shell.lisp) — laid out as the accordion: every ancestor
;;;;                 collapsed to a breadcrumb bar, the last one expanded; the expanded
;;;;                 pane's right-hand column (a side pane, the nav split's preview) is
;;;;                 a pane too, its child. Tiling is this one layout policy.
;;;;   TRAIL-WINDOW  a trail torn off the tab strip (SuperL-g, or a tab dragged out): a
;;;;                 SURFACE (a morph with a backing store, poc/morph.lisp) holding a
;;;;                 TITLE-BAR, a TRAIL-AREA of the same pane morphs and a WINDOW-FRAME.
;;;;                 Tearing off and re-tiling move the pane morphs between trail areas.
;;;;                 Its halo acts on it: Move, Resize, Raise, Close, Re-tile.
;;;;   OVERLAY       a modal card on top — the Spotter, a menu, the halo, the debugger
;;;;                 (the cards are poc/overlays.lisp and, for the debugger and the
;;;;                 cheatsheet, fb-browser.lisp). It takes the input; everything else
;;;;                 goes on behind it. One that answers a caller waits by turning the
;;;;                 backend's own loop (RUN-OVERLAY).
;;;;
;;;; Each morph draws itself and takes its own events, in its surface's coordinates;
;;;; what it drew where (a pane's rows, the strip's tabs) is its own slots. There is no
;;;; other way to be on screen: no frame-wide hit maps, no drawing function that lays
;;;; out the whole browser, no window context to swap them in and out.
;;;;
;;;; DAMAGE is per morph (§12.6). An event damages the morph it reached; everything
;;;; else is found by DESK-SYNC, which runs before every repair and after every event:
;;;; it lays the tree out from the trails (a morph moved, added or removed is damaged by
;;;; that), and compares each morph's SIGNATURE — the few values its picture is a
;;;; function of — with the one it had, damaging only those that changed.
;;;;
;;;; Backend-blind like the shell: the guest's loop (initramfs/fb-browser.lisp) and the
;;;; X11 host loop (poc/run-browser.lisp) both build events, hand them to
;;;; LOL.MORPH:WORLD-DISPATCH, and send whatever LOL.MORPH:WORLD-REPAIR redrew.

(in-package :cl-user)

;;; *WORLD*, the desk the environment runs in, is declared in shell.lisp, which
;;; loads first and asks for it (DRAW-BROWSER, the leave menu).

;;; ===========================================================================
;;; Signatures: how a morph says what its picture depends on
;;; ===========================================================================
;;;
;;; A morph's SIGNATURE is a list of the values its picture is a function of — for the
;;; tab strip, each trail's title, tint and number and which tab is lit; for a pane, its
;;; view, selection and text. DESK-SYNC compares each morph's signature with the one
;;; it had (EQUAL: strings by content, everything else — a view, a buffer — by
;;; identity) and damages the morphs whose picture changed, whatever changed it: a key,
;;; the agent streaming, a service, the REPL. NIL means "nothing beyond its bounds",
;;; which layout already watches.

(defgeneric morph-signature (morph)
  (:documentation "The values MORPH's picture is a function of (see above), or NIL.")
  (:method ((m lol.morph:morph)) nil))

(defclass watched-morph (lol.morph:morph)
  ((sig :initform :none :accessor morph-sig
        :documentation "The signature the morph had when last compared."))
  (:documentation "A morph whose picture DESK-SYNC watches through its signature."))

(defgeneric morph-sig-place (morph)
  (:documentation "Where MORPH's last signature is kept: (values GETTER SETTER).")
  (:method ((m watched-morph)) (values (lambda () (morph-sig m)) (lambda (v) (setf (morph-sig m) v))))
  (:method ((p pane)) (values (lambda () (pane-sig p)) (lambda (v) (setf (pane-sig p) v))))
  (:method ((b pane-body)) (values (lambda () (body-sig b)) (lambda (v) (setf (body-sig b) v)))))

(defgeneric check-morph (morph)
  (:documentation "DESK-SYNC's question to each morph: damage whatever of you changed
   since you were last drawn. By default, the whole morph when its SIGNATURE changed; a
   text view answers row by row (TEXT-BODY, below).")
  (:method ((m lol.morph:morph)) (check-signature m)))

(defun check-signature (m)
  "Damage M if its picture changed since it was last compared."
  (multiple-value-bind (get set) (morph-sig-place m)
    (let ((now (morph-signature m)))
      (unless (equal now (funcall get))
        (funcall set now)
        (lol.morph:invalidate m)))))

;;; ===========================================================================
;;; The tiled shell: a container in fixed bands
;;; ===========================================================================

(defclass shell-morph (lol.morph:morph)
  ((strip  :initarg :strip :reader shell-strip)
   (area   :initarg :area :reader shell-area)
   (status :initarg :status :reader shell-status))
  (:documentation "The tiled browser: the tab strip on top, the status line at the
   bottom, and between them the trail area that shows the tiled trail. Its own pixels
   are only the page behind them (the gutters)."))

(defmethod lol.morph:morph-opaque-p ((m shell-morph)) t)
(defmethod lol.morph:morph-name ((m shell-morph)) "the tiled shell")

(defmethod lol.morph:draw-morph ((m shell-morph) canvas clip)
  (declare (ignore clip))
  (destructuring-bind (x y w h) (lol.morph:morph-local-bounds m)
    (lol.canvas:fill-rect canvas x y w h *ui-bg*)))

(defclass tab-strip (watched-morph)
  ((tabs   :initform '() :accessor strip-tabs
           :documentation "((trail-index . (x0 . x1)) …) of the tabs as last drawn.")
   (closes :initform '() :accessor strip-closes
           :documentation "The same for each tab's × close box.")
   (plus   :initform nil :accessor strip-plus
           :documentation "(x0 . x1) of the + new-tab button.")
   (tray   :initform nil :accessor strip-tray)
   (press-tab :initform nil :accessor strip-press-tab
              :documentation "The tab a left press landed on, while the button is held:
   dragged far enough out of the strip, that trail becomes a window (TEAR-OFF-TRAIL)."))
  (:documentation "One tab per tiled trail (a trail in a window has its title bar
   instead), and the + after them. What it drew where are its own slots (DRAW-TABS
   returns them), which its clicks read back."))

(defmethod lol.morph:morph-opaque-p ((s tab-strip)) t)
(defmethod lol.morph:morph-name ((s tab-strip)) "the tab strip")

(defmethod lol.morph:draw-morph ((s tab-strip) canvas clip)
  (declare (ignore clip))
  (destructuring-bind (x y w h) (lol.morph:morph-local-bounds s)
    (declare (ignore x y h))
    ;; the lit tab is the TILED trail's, even while a window has the keyboard
    (let ((*active* (tiled-index)))
      (multiple-value-bind (tabs closes plus) (draw-tabs canvas w)
        (setf (strip-tabs s) tabs (strip-closes s) closes (strip-plus s) plus)))))

(defmethod morph-signature ((s tab-strip))
  (list (lol.morph:morph-local-bounds s) (tiled-index) (tray-width)
        (loop for trail in *trails*
              collect (list (trail-windowed-p trail) (trail-title trail) (trail-kind-color trail)
                            (and (fboundp 'trail-agent-dot)
                                 (multiple-value-list (funcall 'trail-agent-dot trail)))))))

(defclass tray (watched-morph)
  ((clip :initform nil :accessor tray-clip
         :documentation "(x0 . x1) of the clipboard icon, for clicks."))
  (:documentation "The systray at the right end of the tab strip: the clipboard icon,
   the battery, the clock (doc/systray.org). The clock ticks by painting the tray alone
   (TICK-TRAY), so it is the one item its signature leaves out."))

(defmethod lol.morph:morph-opaque-p ((m tray)) t)
(defmethod lol.morph:morph-name ((m tray)) "the systray")

(defmethod lol.morph:draw-morph ((m tray) canvas clip)
  (declare (ignore clip))
  (destructuring-bind (x y w h) (lol.morph:morph-local-bounds m)
    (declare (ignore y h))
    (multiple-value-bind (x0 clip) (draw-tray canvas (+ x w))
      (declare (ignore x0))
      (setf (tray-clip m) clip))))

(defmethod morph-signature ((m tray))
  (list (lol.morph:morph-local-bounds m) (and (lol.clipboard:clip-current) t)
        (remove :clock (ignore-errors (tray-items)) :key (lambda (it) (getf it :kind)))))

(defclass status-line (watched-morph)
  ((clear :initform nil :accessor status-clear
          :documentation "The minibuffer's ✕ clear box (x0 x1 y0 y1) as last drawn."))
  (:documentation "The bottom line: the last message, or the keys of the focused pane —
   or the minibuffer while one is open (shell.lisp, DRAW-STATUS-LINE)."))

(defmethod lol.morph:morph-opaque-p ((m status-line)) t)
(defmethod lol.morph:morph-name ((m status-line)) "the status line")

(defmethod lol.morph:draw-morph ((m status-line) canvas clip)
  (declare (ignore clip))
  (destructuring-bind (x y w h) (lol.morph:morph-local-bounds m)
    (declare (ignore x))
    ;; it advertises the FOCUSED pane's keys: a window's, when a window has them
    (setf (status-clear m) (and *trails* (draw-status-line canvas w (+ y h) (current-pane))))))

(defmethod morph-signature ((m status-line))
  (list (lol.morph:morph-local-bounds m)
        (and *trails* (status-line-content (current-pane)))))

;;; ===========================================================================
;;; A trail on screen: the accordion, as a layout
;;; ===========================================================================

(defclass trail-area (lol.morph:morph)
  ((root :initform nil :accessor area-root
         :documentation "The ROOT PANE of the trail shown — how the area finds its trail
   in *TRAILS* (drilling replaces the list, never its first pane).")
   (pad-top :initarg :pad-top :initform 4 :reader area-pad-top)
   (pad-bottom :initarg :pad-bottom :initform 2 :reader area-pad-bottom))
  (:documentation "One trail on screen, its panes as its children: the ancestors
   collapsed to breadcrumb bars, the last one expanded to the rest of the height — the
   accordion, as a layout policy (LAYOUT-TRAIL-AREA). The shell has one, under its tab
   strip; every window has one, under its title bar."))

(defmethod lol.morph:morph-name ((a trail-area))
  (let ((trail (area-trail a)))
    (format nil "trail · ~a" (if trail (trail-title trail) "-"))))

(defun area-trail-index (a)
  (and (area-root a) (position (area-root a) *trails* :key #'first)))

(defun area-trail (a)
  (let ((i (area-trail-index a))) (and i (nth i *trails*))))

(defun area-expanded (a)
  "The trail's current pane, as laid out in area A."
  (find :expanded (lol.morph:morph-children a) :key #'pane-mode))

(defun morph-area (m)
  "The trail area M is in (M itself, if it is one), or NIL."
  (loop for o = m then (lol.morph:morph-owner o)
        while o
        when (typep o 'trail-area) return o))

(defun pane-trail-index (p)
  "The index in *TRAILS* of the trail pane P is shown in, or NIL off screen."
  (let ((a (morph-area p))) (and a (area-trail-index a))))

(defun set-bounds-if (m rect)
  "Give M the bounds RECT unless it has them already (a move, a resize: each damages
   what it must)."
  (unless (equal (lol.morph:morph-bounds m) rect)
    (if (equal (lol.morph:morph-bounds m) '(0 0 0 0))
        (progn (setf (lol.morph:morph-bounds m) (copy-list rect)) (lol.morph:invalidate m))
        (lol.morph:set-morph-bounds m rect))))

(defun set-pane-mode (p mode)
  (unless (eq (pane-mode p) mode)
    (setf (pane-mode p) mode (pane-split-plan p) nil)
    (lol.morph:invalidate p)))

(defun adopt (owner m)
  "Make M a child of OWNER (taking it from wherever it was), on top."
  (unless (eq (lol.morph:morph-owner m) owner)
    (lol.morph:add-morph owner m)))

(defun let-go (p)
  "P leaves the screen: off its owner, its own children (its body, a side pane, a
   preview) with it, and no longer laid out."
  (drop-children p)
  (when (lol.morph:morph-owner p) (lol.morph:remove-morph p))
  (setf (pane-mode p) nil (pane-split-plan p) nil))

(defun drop-children (p &optional keep)
  "Take every child of the pane P off screen but those in KEEP: its body, a side pane."
  (dolist (c (lol.morph:morph-children p))
    (unless (member c keep)
      (if (pane-p c) (let-go c) (lol.morph:remove-morph c)))))

(defun ensure-body (p)
  "P's body morph, of the class its view asks for — made anew when the view's kind
   changed (a list that became source, say)."
  (let ((want (body-class-for p)) (b (pane-body p)))
    (if (and b (eq (class-name (class-of b)) want))
        b
        (setf (pane-body p) (make-instance want)))))

(defun place-body (p rect)
  "Lay P's body out at RECT (world coordinates), as P's child."
  (let ((b (ensure-body p)))
    (adopt p b)
    (set-bounds-if b rect)
    ;; a text view knows its rect before it is first drawn (TV-FRAME-KEY compares it)
    (when (and (typep b 'text-body) (pane-tv p))
      (destructuring-bind (x y w h) (lol.morph:morph-local-bounds b)
        (lol.textview:tv-geometry (pane-tv p) x y w h)))
    b))

(defun layout-trail-area (a trail)
  "Lay TRAIL out in area A: its panes become A's children, in order — every one but
   the last a breadcrumb bar (+BAR-H+ tall, 3 px apart), the last expanded to the
   bottom — and the expanded one lays out its right-hand column. A pane of another
   trail that was here goes. The geometry is the tiled shell's as it has always been:
   8 px gutters, the first bar 4 px under the top."
  (setf (area-root a) (first trail))
  (dolist (c (lol.morph:morph-children a))
    (unless (member c trail) (let-go c)))
  (destructuring-bind (ax ay aw ah) (lol.morph:morph-bounds a)
    (let ((y (+ ay (area-pad-top a)))
          (bottom (- (+ ay ah) (area-pad-bottom a)))
          (n (length trail)))
      (loop for p in trail for i from 0
            do (adopt a p)
               (cond
                 ((< i (1- n))
                  (drop-children p)                ; a breadcrumb has no body
                  (set-pane-mode p :collapsed)
                  (set-bounds-if p (list (+ ax 8) y (- aw 16) +bar-h+))
                  (incf y (+ +bar-h+ 3)))
                 (t
                  (set-pane-mode p :expanded)
                  (set-bounds-if p (list (+ ax 8) y (- aw 16) (max 0 (- bottom y))))
                  (layout-expanded p))))))
  ;; children in the trail's order (each was added on top when it arrived)
  (unless (equal (lol.morph:morph-children a) trail)
    (setf (lol.morph:morph-children a)
          (remove-if-not (lambda (c) (member c trail)) (copy-list trail)))))

(defun layout-expanded (p)
  "Lay out the expanded pane P's insides, as EXPANDED-COLUMNS divides it: its own body
   at the main column, and what sits in its right-hand column (a side pane, the nav
   split's preview — a pane, whose body fills it) at the column's rect. Anything else
   that was there goes. Its split and its body's top are recorded for its clicks."
  (destructuring-bind (x y w h) (lol.morph:morph-local-bounds p)
    (let* ((cols (expanded-columns p x y w h))
           (side (getf cols :side))
           (main (getf cols :main)))
      (setf (pane-split-plan p) cols
            (pane-body-top p) (getf cols :body-y)
            (pane-split-x p) (getf cols :split-x))
      (let ((body (place-body p (lol.morph:to-world-rect
                                 p (list (first main) (getf cols :body-y) (second main)
                                         (max 0 (getf cols :body-h)))))))
        (drop-children p (list body side)))
      (when side
        (adopt p side)
        (set-pane-mode side :side)
        (set-bounds-if side (lol.morph:to-world-rect p (getf cols :side-rect)))
        (let ((sb (place-body side (copy-list (lol.morph:morph-bounds side)))))
          (drop-children side (list sb)))))))

;;; ---- a pane, as a morph --------------------------------------------------------

(defmethod lol.morph:morph-opaque-p ((p pane))
  (member (pane-mode p) '(:collapsed :expanded)))   ; a bar, a panel: filled. A side body is not

(defmethod lol.morph:morph-name ((p pane))
  (let ((title (if (pane-view p) (short-title (view-title (pane-view p))) "-")))
    (case (pane-mode p)
      (:collapsed (format nil "breadcrumb · ~a" title))
      (:side (format nil "side pane · ~a" title))
      (t (format nil "pane · ~a" title)))))

(defmethod lol.morph:draw-morph ((p pane) canvas clip)
  (declare (ignore clip))
  ;; drawn with *ACTIVE* at its own trail, so the code inside that asks for the
  ;; current pane (a terminal's cursor, say) gets the one of the trail it is drawing
  (let ((*active* (or (pane-trail-index p) *active*)))
    (destructuring-bind (x y w h) (lol.morph:morph-local-bounds p)
      ;; inside its bounds, always: a split's column header used to run on into the
      ;; gutter beside the pane, where only a whole repaint of the shell put it right
      (lol.canvas:with-clip (canvas x y w h)
       (case (pane-mode p)
        (:collapsed (draw-crumb canvas x y w p))
        (:expanded (draw-pane-expanded canvas p x y w h))
        ;; a side pane, a preview: its body is its whole picture
        (:side nil))))))

(defmethod morph-signature ((p pane))
  "A pane's own picture — its panel: the title (with a file's modified mark), the tint
   and badge, how it is split, and the column header and accent rule a split draws.
   What is IN it is its body's (and a side pane's) business, watched on its own."
  (let ((v (pane-view p)))
    (case (pane-mode p)
      (:collapsed
       (let ((sel (selected-item p)))
         (list :collapsed (lol.morph:morph-local-bounds p) (and v (view-title v)) (and v (view-kind v))
               (and sel (item-label sel)))))
      (:expanded
       (let* ((cols (pane-split-plan p)) (side (getf cols :side)) (it (selected-item p)))
         (list :expanded (lol.morph:morph-local-bounds p) (and v (view-title v)) (and v (view-kind v))
               (pane-title p) (getf cols :kind) (getf cols :split-x)
               (and side (pane-view side) (view-title (pane-view side)))
               (pane-editing-side p) (pane-side p) (and it (item-subject it) t))))
      (t (list (pane-mode p) (lol.morph:morph-local-bounds p))))))

;;; ---- a pane's body, as a morph -----------------------------------------------------
;;; Each draws itself with the function that paints its kind, at its own bounds and with
;;; *ACTIVE* at its pane's trail (as the pane does), and says what its picture depends
;;; on. A text view says it row by row (CHECK-MORPH below), so typing damages a line.

(defmacro with-body-drawing ((b pane x y w h) &body body)
  "Bind PANE to B's pane and X Y W H to B's bounds where it draws, with *ACTIVE* at the
   pane's trail."
  `(let* ((,pane (body-pane ,b))
          (*active* (or (and ,pane (pane-trail-index (expanded-of ,pane))) *active*)))
     (destructuring-bind (,x ,y ,w ,h) (lol.morph:morph-local-bounds ,b)
       (declare (ignorable ,x ,y ,w ,h))
       ,@body)))

(defun expanded-of (p)
  "The expanded pane P belongs to: P itself, or the pane whose column P is in."
  (if (and (eq (pane-mode p) :side) (pane-p (lol.morph:morph-owner p)))
      (lol.morph:morph-owner p)
      p))

(defmethod lol.morph:draw-morph ((b list-body) canvas clip)
  (declare (ignore clip))
  (with-body-drawing (b p x y w h)
    (draw-list-body canvas p x y w h)
    ;; drawing moves the viewport to keep the selection in view: what is on screen now
    (setf (body-sig b) (morph-signature b))))

(defmethod lol.morph:draw-morph ((b text-body) canvas clip)
  (declare (ignore clip))
  (with-body-drawing (b p x y w h)
    (let ((tv (pane-tv p)))
      (lol.textview:tv-geometry tv x y w h)
      (multiple-value-bind (depth instr) (lol.textview:tv-draw canvas tv)
        ;; what each row shows, as drawn: a later change damages only rows that differ
        (setf (body-drawn b)
              (list tv (lol.textview:tv-frame-key tv) (lol.textview:tv-lines tv) (list depth instr)
                    (lol.textview:tv-row-keys tv depth instr)))))))

(defmethod lol.morph:draw-morph ((b grid-body) canvas clip)
  (declare (ignore clip))
  (with-body-drawing (b p x y w h) (draw-matrix-body canvas p x y w h)))

(defmethod lol.morph:draw-morph ((b stage-body) canvas clip)
  (declare (ignore clip))
  (with-body-drawing (b p x y w h) (draw-stage-body canvas p x y w h)))

(defmethod lol.morph:draw-morph ((b image-body) canvas clip)
  (declare (ignore clip))
  (with-body-drawing (b p x y w h) (draw-image-body canvas p x y w h)))

(defmethod lol.morph:draw-morph ((b web-body) canvas clip)
  (declare (ignore clip))
  (with-body-drawing (b p x y w h) (funcall 'draw-web-body canvas p x y w h)))

(defmethod lol.morph:draw-morph ((b terminal-body) canvas clip)
  (declare (ignore clip))
  (with-body-drawing (b p x y w h) (funcall 'draw-terminal-body canvas p x y w h)))

;; a text view and a terminal fill their whole rect: nothing under them shows
(defmethod lol.morph:morph-opaque-p ((b text-body)) t)
(defmethod lol.morph:morph-opaque-p ((b terminal-body)) t)

(defmethod lol.morph:morph-name ((b pane-body))
  (let ((p (body-pane b)))
    (typecase b
      (text-body "text view")
      (grid-body "dispatch matrix")
      (stage-body "stage")
      (image-body "image")
      (web-body "web page")
      (terminal-body "terminal")
      (t (format nil "list of ~d rows" (if (and p (pane-view p)) (length (view-items (pane-view p))) 0))))))

(defmethod morph-signature ((b pane-body))
  "Its rect, its pane's view and what the subject itself adds (a web tab's toolbar, a
   terminal's fate)."
  (let ((p (body-pane b)))
    (and p (list (lol.morph:morph-local-bounds b) (pane-view p)
                 (subject-signature (pane-subject p) p)))))

(defmethod morph-signature ((b list-body))
  ;; the viewport is in it, as drawn (the draw records the signature after it has
  ;; scrolled to the selection, so the next comparison sees the list as it is on screen)
  (let ((p (body-pane b)))
    (and p (list (lol.morph:morph-local-bounds b) (pane-view p) (pane-sel p) (pane-top p)
                 (pane-follow p)))))

(defmethod morph-signature ((b stage-body))
  "A stage's picture: its bodies where they are, the selection, whether it runs, the
   transcript's newest line — a step damages it too, explicitly; this catches the
   rest (an eval in the Workspace beside it that moved a ship)."
  (let* ((p (body-pane b)) (st (and p (view-data (pane-view p)))))
    (and st (list (lol.morph:morph-local-bounds b) (pane-view p) (stage-selected st)
                  (stage-running st) (first (stage-transcript st))
                  (mapcar (lambda (o) (list (body-x o) (body-y o) (body-rot o) (body-flash o)
                                            (body-breached o) (body-docked o)))
                          (stage-bodies st))))))

(defmethod morph-signature ((b terminal-body))
  (let ((p (body-pane b)))
    (and p (list (lol.morph:morph-local-bounds b) (pane-view p)
                 (subject-signature (pane-subject p) p)
                 ;; the cursor is not drawn while a prompt is open
                 (and *minibuffer* t)))))

(defmethod check-morph ((p pane))
  "An expanded pane whose split is as it was repairs only its HEADER band when its
   picture changed — the title, the column's header, the accent rule all live there;
   the panel below is under its bodies. Anything else damages the whole pane."
  (multiple-value-bind (get set) (morph-sig-place p)
    (let ((old (funcall get)) (now (morph-signature p)))
      (unless (equal now old)
        (funcall set now)
        (if (and (eq (pane-mode p) :expanded) (consp old) (eq (first old) :expanded)
                 ;; the same rect, kind and split, the same side pane: then what
                 ;; differs is a title, a header, the accent — all on the header band
                 (every (lambda (i) (equal (nth i old) (nth i now))) '(1 3 5 6 9)))
            (destructuring-bind (x y w h) (lol.morph:morph-local-bounds p)
              (declare (ignore h))
              (lol.morph:invalidate-local p (list x y w (max 0 (- (pane-body-top p) y)))))
            (lol.morph:invalidate p))))))

(defmethod check-morph ((b text-body))
  "A text view damages the screen ROWS whose picture changed (LOL.TEXTVIEW's
   TV-ROW-KEYS, compared with what it drew) — or all of itself, when what changed is
   the frame: its rect, the font, the scroll, the scrollbar, or a line above the view
   (which changes the colours entering it)."
  (let* ((p (body-pane b)) (tv (and p (pane-tv p))) (drawn (body-drawn b)))
    (when (and tv drawn)
      (destructuring-bind (dtv frame lines start rows) drawn
        (if (or (not (eq dtv tv))
                (not (equal frame (lol.textview:tv-frame-key tv)))
                (not (lol.textview:tv-prefix-same-p lines (lol.textview:tv-lines tv)
                                                    ;; the first line on screen (a
                                                    ;; fold makes it differ from the row)
                                                    (lol.textview::row-line tv (lol.textview::tv-scroll tv)))))
            (lol.morph:invalidate b)
            (let ((now (lol.textview:tv-row-keys tv (first start) (second start)))
                  (run nil))                ; the first row of a run of changed rows
              ;; a run of neighbouring changed rows is one rect (a string opened at the
              ;; top changes every row below it: one band, not thirty)
              (flet ((flush (end)
                       (when run
                         (let ((a (lol.textview:tv-row-rect tv run))
                               (z (lol.textview:tv-row-rect tv (1- end))))
                           (lol.morph:invalidate-local
                            b (list (first a) (second a) (third a)
                                    (- (+ (second z) (fourth z)) (second a)))))
                         (setf run nil))))
                (dotimes (r (max (length now) (length rows)) (flush r))
                  (if (and (< r (length now)) (< r (length rows))
                           (equal (aref now r) (aref rows r)))
                      (flush r)
                      (unless run (setf run r)))))))))))

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
              :documentation "The font size the tree was laid out at, to notice a change."))
  (:documentation "The environment's world: the tiled shell at the bottom of the
   z-order, trail windows above it, overlays above those."))

(defvar *windows-to-restore* nil
  "Windows for the next MAKE-DESK to tear off again: ((ROOT-PANE x y w h) …), back to
   front. Set from the desk file at boot (desk.lisp) and from the last world when you
   come back from the console (RUN-BROWSER-FB), so a session's windows survive both.")

(defun make-shell-morph (w h)
  (let* ((strip (make-instance 'tab-strip))
         (tray (make-instance 'tray))
         (area (make-instance 'trail-area :pad-bottom 2))
         (status (make-instance 'status-line))
         (sh (make-instance 'shell-morph :bounds (list 0 0 w h)
                                         :strip strip :area area :status status)))
    (lol.morph:add-morph sh strip)
    (lol.morph:add-morph strip tray)
    (setf (strip-tray strip) tray)
    (lol.morph:add-morph sh area)
    (lol.morph:add-morph sh status)
    sh))

(defun make-desk (w h)
  "A world W x H holding the tiled shell, laid out and all of it damaged: the first
   repair draws the whole frame. Windows waiting in *WINDOWS-TO-RESTORE* are torn off
   again where they were (cut to this screen)."
  (let* ((shell (make-shell-morph w h))
         (desk (make-instance 'desk :bounds (list 0 0 w h) :shell shell)))
    ;; a pane is one morph, so it can be in one world only: a world made fresh (the
    ;; console and back, a new render) takes the trails' panes from the last one
    (dolist (trail *trails*) (dolist (p trail) (let-go p)))
    (lol.morph:add-morph desk shell)
    (setf (desk-tiled-root desk) (and *trails* (first (nth (min *active* (1- (length *trails*))) *trails*)))
          (desk-font-size desk) (and *bfont* (lol.canvas:font-size *bfont*)))
    (let ((*world* desk))
      (desk-sync desk)
      (let ((layout (shiftf *windows-to-restore* nil)))
        (when layout (restore-window-layout layout))))
    (lol.morph:invalidate desk)
    desk))

(defun desk-tray (&optional (desk *world*))
  (and desk (strip-tray (shell-strip (desk-shell desk)))))

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
    (setf *active* (min keep (max 0 (1- (length *trails*)))))
    (desk-sync *world*)))

(defun layout-shell (sh)
  "The shell's bands: the strip on top (the tray at its right end), the status line at
   the bottom, the trail area between them showing the tiled trail."
  (destructuring-bind (x y w h) (lol.morph:morph-bounds sh)
    (let ((strip (shell-strip sh)) (tw (tray-width)))
      (set-bounds-if strip (list x y w +bar-h+))
      (set-bounds-if (strip-tray strip) (list (- (+ x w) tw) y tw (- +bar-h+ 1)))
      (set-bounds-if (shell-area sh) (list x (+ y +bar-h+) w (- h +bar-h+ (status-h))))
      (set-bounds-if (shell-status sh) (list x (+ y (- h (status-h))) w (status-h)))
      (layout-trail-area (shell-area sh) (and *trails* (nth (tiled-index) *trails*))))))

(defun environment-changed ()
  "Something other than an event may have changed what the environment shows — the
   agent streamed, a service woke, a run printed. Nothing has to be guessed: the desk
   compares every morph's picture with what it was (DESK-SYNC), and damages exactly
   the morphs that changed."
  (when *world* (desk-sync *world*)))

;;; ===========================================================================
;;; Windows: a trail torn off the tab strip
;;; ===========================================================================
;;;
;;; A TRAIL-WINDOW shows one trail under a minimal title bar. The trail itself stays in
;;; *TRAILS*: a trail is either in the tab strip or in a window, and everything that
;;; walks *TRAILS* (the terminal and web sweeps, the desk file, the unsaved-files
;;; question at SuperL-q) keeps seeing it. The window finds its trail by the trail's
;;; ROOT PANE, the one thing about a trail that never changes identity.
;;;
;;; FOCUS is *ACTIVE*, as it always was: the focused trail. When it is a window's,
;;; that window's expanded pane has the keyboard; when it is a tiled one, the shell's
;;; does. So SuperL-Tab and SuperL-1..9 reach windows too — and raise them — with no
;;; code of their own (DESK-SYNC makes the world agree after every event).
;;;
;;; A window is a SURFACE (poc/morph.lisp): its subtree — the title bar, a trail area
;;; of the same pane morphs the shell uses, the frame — draws into a backing store of
;;; its own, in the window's coordinates, which the world's repair copies onto the
;;; screen. So an opaque drag copies the window and draws nothing in it, and a key in
;;; its pane redraws that pane in the store and copies that rect.

(defclass trail-window (lol.morph:surface)
  ((root  :initarg :root :accessor window-root
          :documentation "The ROOT PANE of the trail shown: how the window finds it.")
   (title :accessor window-title)
   (area  :accessor window-area)
   (frame :accessor window-frame))
  (:documentation "One trail, torn off the tab strip: a title bar over its breadcrumbs
   and current pane, a frame and a resize grip. Opaque; a surface — it moves by being
   copied."))

(defclass title-bar (watched-morph) ()
  (:documentation "A window's minimal title bar (doc/morphic.org §9, decision 6)."))
(defclass window-frame (watched-morph) ()
  (:documentation "A window's outline and its resize grip, drawn over everything in
   the window; under the pointer only at the grip (MORPH-HIT-P)."))

(defmethod lol.morph:morph-name ((w trail-window))
  (let ((trail (window-trail w)))
    (format nil "window · ~a" (if trail (trail-title trail) "?"))))
(defmethod lol.morph:morph-name ((m title-bar)) "the title bar")
(defmethod lol.morph:morph-name ((m window-frame)) "the window's frame")
(defmethod lol.morph:morph-opaque-p ((m title-bar)) t)

(defun window-renders (w)
  "How many times W's contents were drawn into its backing store (not just copied)."
  (lol.morph:surface-renders w))

(defun make-trail-window (root bounds)
  (let ((win (make-instance 'trail-window :root root :bounds bounds))
        (title (make-instance 'title-bar))
        (area (make-instance 'trail-area :pad-bottom 6))
        (frame (make-instance 'window-frame)))
    (setf (window-title win) title (window-area win) area (window-frame win) frame)
    (dolist (m (list title area frame)) (lol.morph:add-morph win m))
    win))

(defun desk-windows (&optional (desk *world*))
  "The desk's windows, back to front."
  (and desk (remove-if-not (lambda (m) (typep m 'trail-window)) (lol.morph:morph-children desk))))

(defun window-trail-index (w)
  (position (window-root w) *trails* :key #'first))

(defun window-trail (w)
  (let ((i (window-trail-index w))) (and i (nth i *trails*))))

(defun window-pane (w) (let ((tr (window-trail w))) (and tr (car (last tr)))))

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

(defun layout-window (win)
  "A window's parts: the title bar on top, the trail area under it, the frame over
   all of it — and its trail laid out in the area."
  (destructuring-bind (x y w h) (lol.morph:morph-bounds win)
    (set-bounds-if (window-title win) (list x y w +bar-h+))
    (set-bounds-if (window-area win) (list x (+ y +bar-h+) w (- h +bar-h+)))
    (set-bounds-if (window-frame win) (list x y w h))
    (layout-trail-area (window-area win) (window-trail win))))

;;; ---- drawing a window ------------------------------------------------------------

(defun window-close-x (w)
  "Window-local x where the title bar's close box starts."
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
    (lol.canvas:fill-rect canvas 0 (- +bar-h+ 1) w 1 *ui-rule*)))

(defun draw-window-frame (canvas w h focused)
  "The window's outline (the accent when it has the focus) and its resize grip: three
   short diagonals in the bottom-right corner, sized from the cell."
  (lol.canvas:draw-rect canvas 0 0 w h (if focused *ui-accent* *ui-rule*))
  (let ((g (window-grip-size)))
    (loop for k from 1 to 3
          for d = (floor (* g k) 3)
          do (lol.canvas:draw-line canvas (- w 3) (- h 3 d) (- w 3 d) (- h 3) *ui-dim*))))

(defmethod lol.morph:draw-surface-ground ((win trail-window) canvas clip)
  (declare (ignore clip))
  (destructuring-bind (x y w h) (lol.morph:morph-bounds win)
    (declare (ignore x y))
    (lol.canvas:fill-rect canvas 0 0 w h *ui-bg*)))

(defmethod lol.morph:draw-morph ((m title-bar) canvas clip)
  (declare (ignore clip))
  (let ((win (lol.morph:morph-owner m)))
    (draw-window-title canvas (third (lol.morph:morph-bounds win)) win (eq win (focused-window)))))

(defmethod morph-signature ((m title-bar))
  (let* ((win (lol.morph:morph-owner m)) (trail (window-trail win)))
    (list (lol.morph:morph-local-bounds m) (eq win (focused-window)) (window-trail-index win)
          (and trail (trail-title trail)) (and trail (trail-kind-color trail)))))

(defmethod lol.morph:draw-morph ((m window-frame) canvas clip)
  (declare (ignore clip))
  (let ((win (lol.morph:morph-owner m)))
    (destructuring-bind (x y w h) (lol.morph:morph-bounds win)
      (declare (ignore x y))
      (draw-window-frame canvas w h (eq win (focused-window))))))

(defmethod morph-signature ((m window-frame))
  (list (lol.morph:morph-local-bounds m) (eq (lol.morph:morph-owner m) (focused-window)) (window-grip-size)))

(defun in-grip-p (win lx ly)
  "Is LX,LY (window-local) on WIN's resize grip?"
  (destructuring-bind (x y w h) (lol.morph:morph-bounds win)
    (declare (ignore x y))
    (let ((g (+ 4 (window-grip-size))))
      (and (>= lx (- w g)) (>= ly (- h g))))))

(defmethod lol.morph:morph-hit-p ((m window-frame) x y)
  (let ((win (lol.morph:morph-owner m)))
    (destructuring-bind (wx wy ww wh) (lol.morph:morph-bounds win)
      (declare (ignore ww wh))
      (in-grip-p win (- x wx) (- y wy)))))

(defmethod lol.morph:morph-opaque-p ((w trail-window)) t)

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
   and on top: its pane morphs move from the shell's trail area into the window's. The
   last tiled trail stays tiled — the shell always shows one; the status line says so
   and NIL is returned. Returns the window."
  (let ((trail (nth i *trails*)))
    (cond
      ((null trail) nil)
      ((trail-window-of trail))
      ((<= (count-if-not #'windowed-trail-p *trails*) 1)
       (setf *browser-status*
             "the last tab stays tiled — open another (SuperL-e, SuperL-m …) to tear this one off")
       nil)
      (t
       (let ((win (make-trail-window (first trail) (or bounds (default-window-bounds)))))
         (lol.morph:add-morph *world* win)
         ;; the tiled area shows its neighbour now: the nearest tiled trail before it
         (setf (desk-tiled-root *world*)
               (first (nth (or (position-if-not #'windowed-trail-p *trails* :end i :from-end t)
                               (position-if-not #'windowed-trail-p *trails*))
                           *trails*)))
         (setf *active* i)
         (desk-sync *world*)
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
  "Put WIN's trail back in the tab strip, focused, and the window away: its pane
   morphs move back into the shell's trail area."
  (let ((i (window-trail-index win)))
    (when (eq (desk-grab *world*) win) (setf (desk-grab *world*) nil))
    (lol.morph:remove-morph win)
    (when i
      (setf *active* i (desk-tiled-root *world*) (first (nth i *trails*))))
    (desk-sync *world*)))

(defun close-window-trail (win)
  "The close box: close WIN's trail, as SuperL-w would (never the last trail — but a
   window's never is, since the shell always keeps one)."
  (let ((i (window-trail-index win)))
    (when i
      (setf *active* i)
      (close-trail)
      (desk-sync *world*))))

(defmethod lol.morph:drop-morph ((win trail-window) world x y)
  "Let go of a carried window. On the tab strip: back into it (re-tile). Elsewhere it
   stays where it is, pulled back on screen if its title bar went off it."
  (declare (ignore x))
  ;; a carry the halo's Move or Resize began was in grab mode: putting it down ends that
  (when (eq (desk-grab world) win)
    (setf (desk-grab world) nil *browser-status* nil))
  (if (< y +bar-h+)
      (retile-window win)
      (destructuring-bind (wx wy ww wh) (lol.morph:morph-bounds win)
        (declare (ignore wh))
        (destructuring-bind (sx sy sw sh) (lol.morph:morph-bounds world)
          (declare (ignore sx sy))
          (let ((nx (max (- (* 4 (char-w)) ww) (min wx (- sw (* 4 (char-w))))))
                (ny (max 0 (min wy (- sh +bar-h+)))))
            (lol.morph:move-morph win nx ny))))))

;;; ===========================================================================
;;; Events: each morph takes its own, in its own coordinates
;;; ===========================================================================
;;;
;;; A key goes to the world's focus — the focused trail's expanded pane — and is
;;; BROWSER-KEY's. A press goes to the deepest morph under the pointer: a tab or the +,
;;; the clipboard icon, the minibuffer's ✕, a breadcrumb, a window's title bar or grip,
;;; and everything inside an expanded pane. A side pane lets its events through to its
;;; expanded owner, which knows the split; a press in a gutter (beside a panel, between
;;; two breadcrumbs, on the status line) reaches the trail area or the shell, which
;;; hand it to the expanded pane just as the one click function used to. A drag and a
;;; release go wherever the press went (the hand's capture).
;;;
;;; DAMAGE: an event damages the pane it reached (and its right-hand column, its child).
;;; What else it changed — a tab, the status line, the layout — DESK-SYNC finds after
;;; the event.

(defun morph-holder (m)
  "The shell or the window M is in (M itself, if it is one)."
  (loop for o = m then (lol.morph:morph-owner o)
        while o
        when (or (typep o 'shell-morph) (typep o 'trail-window)) return o))

(defun area-of-holder (h)
  (if (typep h 'trail-window) (window-area h) (shell-area h)))

(defun holder-pane (h)
  "The expanded pane of the shell or window H."
  (and h (area-expanded (area-of-holder h))))

(defun damage-body (b)
  "An event changed body B: damage it — but a text view tells for itself which of its
   rows changed (CHECK-MORPH), so it is left to DESK-SYNC, which runs after every event."
  (when (and b (not (typep b 'text-body)))
    (lol.morph:invalidate b)))

(defun pane-event (p e)
  "The pointer events that reach the expanded pane P itself (E in P's coordinates):
   those its bodies let through, and a press on its panel around them. -> T when taken.
   DAMAGE: the body the event acted on — not the whole pane."
  (let ((x (lol.morph:event-x e)) (y (lol.morph:event-y e))
        (wx (lol.morph:event-wx e)) (wy (lol.morph:event-wy e))
        (state (lol.morph:event-state e)))
    (case (lol.morph:event-kind e)
      (:down (press-prelude)
             (damage-body (pane-click p x y (lol.canvas:ctrl-p state) (lol.canvas:super-p state)))
             t)
      ;; a menu or the halo: an overlay, whose going repairs what it covered; what its
      ;; command changed, the morphs' signatures find
      (:right (context-menu-at (pane-target p x y) wx wy) t)
      (:middle (pane-middle-click p x y wx wy (lol.canvas:super-p state)) t)
      ;; a drag extends the selection the press started; it redraws only if it did
      (:drag (when (browser-drag x y) (damage-body (pane-body p))) t)
      ;; a drag that selected something hands it to PRIMARY and the clipboard (X's
      ;; rule). It changes nothing on screen, as it never did.
      (:up (browser-drag-end) t)
      (t nil))))

(defparameter +pick-up-travel+ 8
  "Pixels a press on a row (or a sprite) must travel, the button held, before it picks
   the row's subject up to be dropped elsewhere — more than +DRAG-SLOP+, so a press
   that only rolls a fingertip on a clickpad still clicks.")

(defun body-click (p b x y state)
  "The press at X,Y on body B of the expanded pane P, acted on now: PANE-CLICK, and B
   (or the column's body it chose) damaged."
  (declare (ignore b))
  (damage-body (pane-click p x y (lol.canvas:ctrl-p state) (lol.canvas:super-p state))))

(defun press-subject (p x y)
  "What a press at X,Y in the expanded pane P could pick up: the subject of the row or
   the sprite there (PANE-TARGET), or NIL."
  (let ((target (ignore-errors (pane-target p x y))))
    (and (member (getf target :kind) '(:row :sprite)) (getf target :subject)
         target)))

(defun start-carry (subject from)
  "The hand picks SUBJECT up, from the morph FROM, to drop it elsewhere; the status
   line says what it carries and where it can go (CARRYING-HINTS, shell.lisp)."
  (lol.morph:hand-carry-payload (lol.morph:world-hand *world*)
                                (list :subject subject :from from :label (drop-label subject)))
  (setf *browser-status* nil))

(defmethod lol.morph:handle-event ((b pane-body) e)
  "A body under the pointer takes its own press and the drag that follows: the press
   is the body's (BODY-PRESS, through PANE-CLICK, which knows the split), and the body
   is what is damaged. A press on a ROW that opens something waits for the release —
   a press that moves first is not a click but a pick-up: the hand carries the row's
   subject away to be dropped (doc/morphic.org §13). A press on a sprite selects it at
   once, and moved, picks its instance up. Everything else — the wheel, the menus, the
   halo — goes on to its pane."
  (let ((p (expanded-of (body-pane b)))
        (x (lol.morph:event-x e)) (y (lol.morph:event-y e)) (state (lol.morph:event-state e)))
    (when (and p (eq (pane-mode p) :expanded))
      (case (lol.morph:event-kind e)
        (:down
         (press-prelude)
         (let ((target (and (typep b '(or list-body stage-body))
                            ;; a tap has no release of its own: it clicks at once
                            (not (getf (lol.morph:event-data e) :tap))
                            (press-subject p x y))))
           (cond
             ((and target (typep b 'list-body))
              (setf (hand-press) (list :row :x x :y y :state state :subject (getf target :subject))))
             (t (body-click p b x y state)
                (when target
                  (setf (hand-press) (list :row :x x :y y :clicked t
                                           :subject (getf target :subject)))))))
         t)
        (:drag
         (let ((press (hand-press)))
           (if (eq (press-kind press) :row)
               (when (or (> (abs (- x (getf (rest press) :x))) +pick-up-travel+)
                         (> (abs (- y (getf (rest press) :y))) +pick-up-travel+))
                 (start-carry (getf (rest press) :subject) b))
               (when (browser-drag x y) (damage-body b))))
         t)
        (:up
         (let ((press (hand-press)))
           ;; a press on a row that never moved: the click it was, now
           (if (eq (press-kind press) :row)
               (unless (getf (rest press) :clicked)
                 (body-click p b (getf (rest press) :x) (getf (rest press) :y)
                             (getf (rest press) :state)))
               (browser-drag-end)))
         t)
        (t nil)))))

(defun area-scroll (a e)
  "The wheel over area A: its trail's current pane scrolls — without taking the focus,
   as in every desktop."
  (let ((i (area-trail-index a)))
    (when i
      (let ((*active* i))
        (browser-scroll (* (- (lol.morph:event-notches e)) +wheel-lines+)))
      ;; what scrolled: the current pane's own body
      (let ((p (area-expanded a))) (when p (damage-body (pane-body p))))
      t)))

(defmethod lol.morph:handle-event ((p pane) e)
  (case (pane-mode p)
    (:expanded
     (case (lol.morph:event-kind e)
       (:key (pane-key p e))
       ;; the Menu key (fb: from the evdev): the right-button menu, at the caret
       (:menu (keyboard-context-menu) t)
       (t (pane-event p e))))
    ;; a breadcrumb: a press goes back to that level
    (:collapsed
     (when (eq (lol.morph:event-kind e) :down)
       (let ((trail (area-trail (morph-area p))))
         (press-prelude)
         (trail-goto (position p trail))
         t)))
    ;; a side pane: its expanded owner takes its events — it knows the split
    (t nil)))

(defun pane-key (p e)
  "A key for the focused pane P: BROWSER-KEY's. DAMAGE: the body the key was typed
   into (the side pane's, when the side has the keys) — and for a text view not even
   that: it says itself which rows changed (CHECK-MORPH), so a key that changes one line
   repairs that line. Nothing either when the key was typed INTO a terminal: the echo
   comes back as output and repaints only its rows (terminal-pane.lisp) — unless the
   key was not the console's, an evdev chord. What else a key changed (the pane's
   title, the layout, the status line, another tab) DESK-SYNC finds after the event."
  (let* ((state (lol.morph:event-state e))
         (body (focused-body p))
         (result (browser-key (lol.morph:event-key e) state)))
    (unless (and (getf (lol.morph:event-data e) :tty)
                 (fboundp 'terminal-key-was-cheap-p)
                 (funcall 'terminal-key-was-cheap-p p state))
      (damage-body body))
    (if (eq result :quit) :quit t)))

(defmethod lol.morph:handle-event ((a trail-area) e)
  "What reaches the area itself: the wheel (from anywhere over it), and a press in a
   gutter — the old click's rules: a breadcrumb's band (its bottom edge included,
   across the whole width), else the expanded pane's."
  (let ((p (area-expanded a)) (kind (lol.morph:event-kind e)))
    (cond
      ((eq kind :wheel) (area-scroll a e))
      ((null p) nil)
      ((eq kind :down)
       (let* ((y (lol.morph:event-y e))
              (crumb (find-if (lambda (c)
                                (and (eq (pane-mode c) :collapsed)
                                     (let ((cy (second (lol.morph:morph-local-bounds c))))
                                       (<= cy y (+ cy +bar-h+)))))
                              (lol.morph:morph-children a))))
         (if crumb
             (progn (press-prelude) (trail-goto (position crumb (area-trail a))) t)
             (pane-event p e))))
      (t (pane-event p e)))))

(defun tab-target (s x)
  "What a press at X in tab strip S is on: (:KIND :TAB :INDEX i), or NIL."
  (let ((tab (find-if (lambda (e) (<= (cadr e) x (cddr e))) (strip-tabs s))))
    (and tab (list :kind :tab :index (car tab)))))

(defun tab-at (s x)
  "The index of the tab (not its close box, not +) under X in strip S, or NIL."
  (and (not (find-if (lambda (e) (<= (cadr e) x (cddr e))) (strip-closes s)))
       (getf (tab-target s x) :index)))

(defmethod lol.morph:handle-event ((s tab-strip) e)
  "A press activates a tab, closes one, or opens a Workspace (+); a tab dragged down
   out of the strip becomes a window, carried by the hand from there (doc/morphic.org
   §5). The right button is a tab's menu, the middle one the halo."
  (let ((x (lol.morph:event-x e)) (y (lol.morph:event-y e)))
    (case (lol.morph:event-kind e)
      (:down (press-prelude)
             (setf (strip-press-tab s) (tab-at s x))
             (tab-strip-click (strip-tabs s) (strip-closes s) (strip-plus s) x)
             t)
      (:drag (when (and (strip-press-tab s) (>= y (* 2 +bar-h+)))
               (tear-off-by-pointer (shiftf (strip-press-tab s) nil)
                                    (lol.morph:event-wx e) (lol.morph:event-wy e)))
             t)
      (:up (setf (strip-press-tab s) nil) (browser-drag-end) t)
      (:right (context-menu-at (tab-target s x) (lol.morph:event-wx e) (lol.morph:event-wy e)) t)
      (:middle (open-halo (lol.morph:event-wx e) (lol.morph:event-wy e)) t)
      (t nil))))

(defmethod lol.morph:handle-event ((m tray) e)
  "A press on the clipboard icon opens the history (SuperL-h's card)."
  (when (eq (lol.morph:event-kind e) :down)
    (press-prelude)
    (let ((clip (tray-clip m)) (x (lol.morph:event-x e)))
      (when (and clip (<= (car clip) x (cdr clip))) (open-clipboard-manager)))
    t))

(defmethod lol.morph:handle-event ((m status-line) e)
  "A press on the minibuffer's ✕ clears its text and keeps it open; any other press
   is the shell's (it ends the prompt, as a press anywhere does)."
  (when (and (eq (lol.morph:event-kind e) :down)
             (progn (setf (hand-press) nil)
                    (minibuffer-click (status-clear m) (lol.morph:event-x e) (lol.morph:event-y e))))
    (lol.morph:invalidate m)
    t))

(defmethod lol.morph:handle-event ((sh shell-morph) e)
  "What reaches the shell itself — a press or a button on the status line, the wheel
   over the strip or the status line — is the tiled pane's, as it always was."
  (let ((a (shell-area sh)))
    (if (eq (lol.morph:event-kind e) :wheel)
        (area-scroll a e)
        (let ((p (area-expanded a)))
          (and p (pane-event p e))))))

(defmethod lol.morph:handle-event ((m title-bar) e)
  "A press on the close box closes the window's trail; anywhere else on the bar picks
   the window up (the hand carries it while the button is held). The right button
   offers the environment's menu, the middle one the halo."
  (let ((win (lol.morph:morph-owner m)))
    (case (lol.morph:event-kind e)
      (:down (if (>= (lol.morph:event-x e) (window-close-x win))
                 (close-window-trail win)
                 (lol.morph:hand-pick-up (lol.morph:world-hand *world*) win :move))
             t)
      (:right (context-menu-at nil (lol.morph:event-wx e) (lol.morph:event-wy e)) t)
      (:middle (open-halo (lol.morph:event-wx e) (lol.morph:event-wy e)) t)
      (t nil))))

(defmethod lol.morph:handle-event ((m window-frame) e)
  "A press on the grip resizes the window (the hand carries its corner)."
  (when (eq (lol.morph:event-kind e) :down)
    (lol.morph:hand-pick-up (lol.morph:world-hand *world*) (lol.morph:morph-owner m) :resize)
    t))

(defmethod lol.morph:handle-event ((w trail-window) e)
  "The wheel over a window's title bar scrolls its trail, as over its pane."
  (when (eq (lol.morph:event-kind e) :wheel)
    (area-scroll (window-area w) e)))

;;; ===========================================================================
;;; Drag and drop (doc/morphic.org §13)
;;; ===========================================================================
;;;
;;; The hand carries a SUBJECT picked up from a row, a sprite or the caret (by the
;;; pointer: a press that moves; by the keys: SuperL-p), and lets go of it over a morph
;;; (the button's release, the next press, or SuperL-d at the keyboard focus). The world
;;; asks that morph and then its owners whether they take it (LOL.MORPH:ACCEPT-DROP):
;;;
;;;   an editable text   inserts a READABLE REFERENCE where it lands ('name, #'f,
;;;                      (find-class 'c), "a/path" …: DROP-REFERENCE, shell.lisp)
;;;   an inspector field sets the field to its VALUE, through the field's own setter —
;;;                      the same path C-c C-c takes (SET-FIELD-TO); a field with no
;;;                      setter refuses, and says so
;;;   a terminal         types the reference into its program (a path, a symbol)
;;;   the tab strip      opens it as a new trail
;;;   any other pane     drills it there, in that pane's trail (a window's included)
;;;   a web page         refuses: a page takes no drops
;;;
;;; Each method answers T (taken; it says what it did), a string (refused, why), or NIL
;;; (not mine: ask my owner). The status line says what happened.

(defun focus-pane-of (p)
  "Give the keys to the trail pane P is in — and, when P is a pinned side pane, to its
   column."
  (let* ((host (expanded-of p)) (i (pane-trail-index host)))
    (when i (setf *active* i))
    (when (and (not (eq host p)) (eq (pane-side host) p))
      (setf (pane-editing-side host) t))))

(defmethod lol.morph:accept-drop ((b text-body) payload e)
  "Editable text takes a readable reference to what is dropped, where it lands (at the
   caret, dropped from the keys). A preview or read-only text is not a place to type:
   its pane drills instead."
  (let* ((p (body-pane b)) (host (and p (expanded-of p))) (tv (and p (pane-tv p))))
    (when (and tv (editor-pane-p p)
               (or (eq host p) (eq (pane-side host) p)))     ; not a nav-split preview
      (let ((text (drop-reference (getf payload :subject))))
        (if (null text)
            (format nil "~a has no readable form to insert — drop it on the tabs to inspect it, or on a field to store it"
                    (getf payload :label))
            (progn
              (drop-reference-at tv text (and e (lol.morph:event-x e)) (and e (lol.morph:event-y e)))
              (focus-pane-of p)
              (setf *browser-status* (format nil "dropped ~a: inserted ~a" (getf payload :label) text))
              t))))))

(defmethod lol.morph:accept-drop ((b list-body) payload e)
  "An inspector's field takes the VALUE dropped on it, through its own setter. Any
   other list leaves the drop to its pane (it drills)."
  (let ((p (body-pane b)))
    (when (and p (pane-view p) (eq (view-kind (pane-view p)) :inspector))
      (let* ((i (if e (lol.widgets:hit-row (body-rows b) (lol.morph:event-y e)) (pane-sel p)))
             (it (and i (nth i (view-items (pane-view p))))))
        (when it
          (multiple-value-bind (v ok) (drop-value (getf payload :subject))
            (cond
              ((null (item-edit it))
               (format nil "the field ~a is not settable — ~a was not stored" (item-label it)
                       (getf payload :label)))
              ((not ok)
               (format nil "~a is a view, not a value to store in ~a" (getf payload :label)
                       (item-label it)))
              (t (handler-case
                     (progn (set-pane-sel p i) (set-field-to p it v) (focus-pane-of p) t)
                   (serious-condition (c) (format nil "set failed: ~a" c)))))))))))

(defmethod lol.morph:accept-drop ((b terminal-body) payload e)
  (declare (ignore e))
  (let* ((p (body-pane b)) (text (drop-reference (getf payload :subject))))
    (cond ((null text) (format nil "~a has no readable form to type" (getf payload :label)))
          ((not (session-live-p (pane-session p))) "that terminal's program has ended")
          (t (terminal-paste (pane-subject p) text)
             (focus-pane-of p)
             (setf *browser-status* (format nil "dropped ~a: typed ~a" (getf payload :label) text))
             t))))

(defmethod lol.morph:accept-drop ((b web-body) payload e)
  (declare (ignore payload e))
  "a web page takes no drops — drop it on the tabs to open it")

(defmethod lol.morph:accept-drop ((p pane) payload e)
  "Any pane — tiled or in a window, its list or its panel — drills what is dropped
   there, in its own trail, which gets the keys."
  (declare (ignore e))
  (let* ((host (expanded-of p)) (i (pane-trail-index host)))
    (when i
      (setf *active* i)
      (trail-push (getf payload :subject))
      (setf *browser-status* (format nil "dropped ~a: opened here" (getf payload :label)))
      t)))

(defmethod lol.morph:accept-drop ((s tab-strip) payload e)
  "The tab strip opens what is dropped on it as a new trail."
  (declare (ignore e))
  (new-trail (getf payload :subject))
  (setf *browser-status* (format nil "dropped ~a: a new tab" (getf payload :label)))
  t)

(defmethod lol.morph:accept-drop ((m title-bar) payload e)
  "A window's title bar: as its pane — drill it in the window's trail."
  (declare (ignore e))
  (let ((i (window-trail-index (lol.morph:morph-owner m))))
    (when i
      (setf *active* i)
      (trail-push (getf payload :subject))
      (setf *browser-status* (format nil "dropped ~a: opened in the window" (getf payload :label)))
      t)))

(defmethod lol.morph:world-dropped ((desk desk) payload result)
  (cond ((stringp result) (setf *browser-status* result))
        ((null result)
         (setf *browser-status*
               (format nil "nothing there takes ~a — it is put back" (getf payload :label))))))

(defun keyboard-pick-up ()
  "SuperL-p: the hand picks up what the keyboard is on — the selected row's subject in
   a list, the selected sprite on a stage, the definition (or symbol) at the caret in
   text. -> T, or a status saying there is nothing to pick up."
  (let* ((p (current-pane)) (b (and p (focused-body p)))
         (src (if (and (pane-side p) (pane-editing-side p)) (pane-side p) p))
         (subject
           (typecase b
             (list-body (let ((it (selected-item src))) (and it (item-subject it))))
             (stage-body (let ((st (view-data (pane-view src))))
                           (and (stage-selected st) (body-obj (stage-selected st)))))
             (text-body (let ((sym (resolve-symbol (token-at-point (pane-tv src)))))
                          (and sym (symbolp sym)
                               (if (jumpable-p sym) (subj-defn sym) sym))))
             (t nil))))
    (if subject
        (progn (start-carry subject b) t)
        (progn (setf *browser-status* "nothing to pick up here: select a row, a sprite, or put the caret on a name")
               nil))))

(defun keyboard-drop (desk &optional on-tabs)
  "SuperL-d: let go of what the hand carries at the keyboard focus — the focused pane's
   body (text: at the caret; an inspector: on the selected field); with Shift, on the
   tab strip: a new tab."
  (let ((hand (lol.morph:world-hand desk)))
    (if (null (lol.morph:hand-payload hand))
        (setf *browser-status* "the hand is empty — SuperL-p picks the selection up, or drag a row")
        (lol.morph:world-drop desk nil
                              (if on-tabs
                                  (shell-strip (desk-shell desk))
                                  (let ((p (current-pane))) (or (focused-body p) p)))))))

;;; ---- the desk's own keys: SuperL-g and the grab mode -----------------------------------

(defun grab-status ()
  (setf *browser-status*
        "window: arrows move · S-arrows ×8 · C-arrows resize · t or SuperL-g re-tile · Ret/Esc done"))

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
                     (setf (lol.morph:hand-carrying hand) nil)))))
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

(defun press-focus (desk e)
  "A press (any button) gives the focus to the trail it lands in — the tiled one, or a
   window's, which comes to the top — before the morph pressed acts on it."
  (let* ((target (lol.morph:morph-at desk (lol.morph:event-wx e) (lol.morph:event-wy e)))
         (holder (morph-holder target)))
    (cond ((typep holder 'trail-window)
           (let ((i (window-trail-index holder)))
             (when i (setf *active* i)))
           (lol.morph:raise-morph holder))
          ((and holder *trails*)
           (setf *active* (tiled-index))))))

(defmethod lol.morph:handle-event ((desk desk) e)
  "The desk sees every event first. It brings the world in line with the trails (the
   focus can have moved between events — a modal, a restored desk), hands everything to
   an overlay that is up, focuses the trail a press lands in, and keeps the window
   keys: SuperL-g, and the keys of the grab mode it starts. SuperL-g on a tiled trail
   tears it off into a window (and grabs it); on a window, grabs it — arrows then move
   it."
  (desk-sync desk)
  ;; an overlay on top (the Spotter, a menu, the debugger …) is MODAL for input: it
  ;; takes every event, wherever the pointer is — a click off its card is its business
  ;; too (it closes). Nothing else on screen stops: services, steps and the clock go
  ;; on behind it (doc/morphic.org §10, step 4).
  (let ((ov (top-overlay desk)))
    (when ov
      (overlay-event ov e)
      (return-from lol.morph:handle-event t)))
  (case (lol.morph:event-kind e)
    ((:down :right :middle) (press-focus desk e) nil)
    (:key
     (let ((key (lol.morph:event-key e)) (state (lol.morph:event-state e)))
       (cond
         ((desk-grab desk) (grab-key desk key state))
         ;; drag and drop from the keys (§13): Esc puts back what the hand carries,
         ;; SuperL-p picks the selection up, SuperL-d drops it (Shift: as a new tab)
         ((and (eq key :escape) (lol.morph:hand-payload (lol.morph:world-hand desk)))
          (setf *browser-status*
                (format nil "put back: ~a" (getf (lol.morph:hand-payload (lol.morph:world-hand desk)) :label))
                (lol.morph:hand-press (lol.morph:world-hand desk)) nil)
          t)
         ((and (lol.canvas:super-p state) (member key '(#\p #\P)) (not *minibuffer*))
          (keyboard-pick-up) t)
         ((and (lol.canvas:super-p state) (member key '(#\d #\D)) (not *minibuffer*))
          (keyboard-drop desk (or (eql key #\D) (lol.canvas:shift-p state))) t)
         ((and (lol.canvas:super-p state) (member key '(#\g #\G)) (not *minibuffer*))
          (let ((win (or (focused-window) (tear-off-trail *active*))))
            (when win
              (setf (desk-grab desk) win)
              (grab-status)))
          t))))))

(defmethod lol.morph:world-after-dispatch ((desk desk) e)
  (declare (ignore e))
  (desk-sync desk))

(defmethod lol.morph:world-before-repair ((desk desk))
  (desk-sync desk))

(defun desk-sync (desk)
  "Make the world agree with the trails, and damage what changed. A window whose trail
   is gone goes; the tiled trail follows *ACTIVE* when that is a tiled one; a font size
   change damages everything (every metric moved); the tree is laid out (LAYOUT-SHELL,
   LAYOUT-WINDOW — a morph that moves is damaged by moving); the keyboard focus is the
   focused trail's expanded pane, and a window that gets it comes to the top; and each
   morph whose picture changed — by its SIGNATURE — is damaged. Run before every repair
   and around every event; cheap when nothing changed."
  (let ((*world* desk))
    (dolist (w (desk-windows desk))
      (unless (window-trail-index w)
        (when (eq (desk-grab desk) w) (setf (desk-grab desk) nil))
        (lol.morph:remove-morph w)))
    (when *trails*
      (setf *active* (max 0 (min *active* (1- (length *trails*)))))
      (let ((trail (nth *active* *trails*)))
        (unless (trail-window-of trail)
          (setf (desk-tiled-root desk) (first trail)))))
    (let ((fs (and *bfont* (lol.canvas:font-size *bfont*))))
      (unless (eql fs (desk-font-size desk))
        (setf (desk-font-size desk) fs)
        (dolist (w (desk-windows desk)) (setf (lol.morph:surface-damage w) :all))
        (lol.morph:invalidate desk)))
    (layout-shell (desk-shell desk))
    (dolist (w (desk-windows desk)) (layout-window w))
    (let* ((win (focused-window))
           (focus (if win (holder-pane win) (holder-pane (desk-shell desk)))))
      (when (and win (not (eq win (car (last (desk-windows desk))))) (not (top-overlay desk)))
        (lol.morph:raise-morph win))
      (setf (lol.morph:world-focus desk) (or focus (desk-shell desk))))
    (dolist (m (lol.morph:morph-descendants desk))
      (when (or (typep m 'watched-morph) (typep m 'pane-body) (and (pane-p m) (pane-mode m)))
        (check-morph m)))
    nil))

;;; ---- the halo knows about morphs --------------------------------------------------
;;; The halo is about the thing under the pointer and everything that holds it: the
;;; target there (the word, the row, the sprite — TARGET-LAYERS, shell.lisp), then each
;;; morph from the deepest under the point out to the shell or the window, with the
;;; layers that morph offers: a pane its body when the point is on it, then itself,
;;; then what it shows; the tab strip a tab and itself; the tray the clipboard icon and
;;; itself; the status line the minibuffer and itself. So walking OUT with Up ends on
;;; the shell morph, or the window — and Inspect on either shows a CLOS instance with
;;; its bounds (doc/morphic.org §6, §12.5). Rects are on the screen, where the halo
;;; draws them.

(defgeneric morph-drawers (morph)
  (:documentation "The functions whose code paints MORPH — the halo's Code handles.")
  (:method ((m lol.morph:morph)) '(lol.morph:draw-morph lol.morph:handle-event))
  (:method ((m shell-morph)) '(layout-shell lol.morph:draw-morph))
  (:method ((m tab-strip)) '(draw-tabs tab-strip-click))
  (:method ((m tray)) '(draw-tray tray-items))
  (:method ((m status-line)) '(draw-status-line draw-hints))
  (:method ((m trail-area)) '(layout-trail-area))
  (:method ((m title-bar)) '(draw-window-title))
  (:method ((m window-frame)) '(draw-window-frame))
  (:method ((m trail-window)) '(layout-window lol.morph:surface-refresh))
  (:method ((p pane))
    (case (pane-mode p)
      (:collapsed '(draw-crumb))
      (:side '(layout-expanded))
      (t '(draw-pane-expanded lol.widgets:draw-panel pane-click))))
  (:method ((b list-body)) '(draw-list-body item->row lol.widgets:draw-list))
  (:method ((b text-body)) '(lol.textview:tv-draw lol.textview:tv-row-keys))
  (:method ((b grid-body)) '(draw-matrix-body lol.widgets:draw-grid))
  (:method ((b stage-body)) '(draw-stage-body))
  (:method ((b image-body)) '(draw-image-body))
  (:method ((b web-body)) '(draw-web-body web-blit))
  (:method ((b terminal-body)) '(draw-terminal-body)))

(defun morph-layer (m)
  "M itself as a halo layer: its name, its instance, its rect, its drawers."
  (halo-layer (clip-text (lol.morph:morph-name m) 24)
              (format nil "the morph ~a — ~(~a~)" (lol.morph:morph-name m) (class-name (class-of m)))
              m (copy-list (lol.morph:morph-bounds m)) :element (morph-drawers m)))

(defun local-rect-in (m rect)
  "RECT, recorded where M draws, on the screen."
  (and rect (lol.morph:to-world-rect m rect)))

(defgeneric morph-halo-layers (morph wx wy)
  (:documentation "The layers MORPH offers the halo at the screen point WX,WY, inner
   first: by default, the morph itself.")
  (:method ((m lol.morph:morph) wx wy) (declare (ignore wx wy)) (list (morph-layer m))))

(defmethod morph-halo-layers ((p pane) wx wy)
  "A pane is itself, then what it shows. (Its body, under the point, is a layer of its
   own: the body morph, deeper.)"
  (declare (ignore wx wy))
  (list (morph-layer p)
        (halo-layer "shows" (format nil "what the pane shows: ~a" (view-title (pane-view p)))
                    (pane-subject p) (copy-list (lol.morph:morph-bounds p)) :subject)))

(defmethod morph-halo-layers ((b stage-body) wx wy)
  "On the stage, a sprite is the smallest thing there: its layer comes first."
  (let ((p (body-pane b)))
    (append
     (multiple-value-bind (lx ly) (lol.morph:surface-origin b)
       (let ((sprite (and p (stage-body-at p (- wx lx) (- wy ly)))))
         (when sprite
           (list (halo-layer "sprite" "sprite" sprite (local-rect-in b (sprite-rect p sprite)) :element
                             '(draw-stage-sprite stage-sprite-points))))))
     (call-next-method))))

(defmethod morph-halo-layers ((s tab-strip) wx wy)
  (declare (ignore wy))
  (let* ((lx (- wx (lol.morph:surface-origin s)))
         (tab (find-if (lambda (e) (<= (cadr e) lx (cddr e))) (strip-tabs s))))
    (append (when tab
              (let ((trail (nth (car tab) *trails*)))
                (list (halo-layer (clip-text (format nil "tab ~d · ~a" (1+ (car tab)) (trail-title trail)) 24)
                                  (format nil "tab ~d · ~a" (1+ (car tab)) (trail-title trail))
                                  trail (local-rect-in s (list (cadr tab) 0 (- (cddr tab) (cadr tab)) +bar-h+))
                                  :element '(draw-tabs trail-title)))))
            (call-next-method))))

(defmethod morph-halo-layers ((m tray) wx wy)
  (declare (ignore wy))
  (let ((clip (tray-clip m)) (lx (- wx (lol.morph:surface-origin m))))
    (append (when (and clip (<= (car clip) lx (cdr clip)))
              (list (halo-layer "the clipboard icon" "the clipboard icon" (lol.clipboard:clip-current)
                                (local-rect-in m (list (car clip) 0 16 (- +bar-h+ 1))) :element
                                '(draw-clip-icon open-clipboard-manager))))
            (call-next-method))))

(defmethod morph-halo-layers ((m status-line) wx wy)
  (declare (ignore wx wy))
  (append (when *minibuffer*
            (list (halo-layer "the minibuffer" "the minibuffer" *minibuffer*
                              (copy-list (lol.morph:morph-bounds m)) :element '(draw-minibuffer))))
          (call-next-method)))

(defun morph-target (m x y)
  "What the pointer is ON at X,Y (where M draws), asked of M and then its owners: a
   pane's word, row or sprite (PANE-TARGET), or a tab."
  (loop for o = m then (lol.morph:morph-owner o)
        while o
        for target = (typecase o
                       (pane-body nil)            ; its pane answers, which knows the split
                       (pane (and (eq (pane-mode o) :expanded) (pane-target o x y)))
                       (tab-strip (tab-target o x))
                       (trail-area (let ((p (area-expanded o))) (and p (pane-target p x y))))
                       (t nil))
        when target return target
        until (or (typep o 'shell-morph) (typep o 'trail-window))))

(defun halo-layers (wx wy)
  "Everything under the screen point WX,WY the halo can be about, innermost first: the
   thing the pointer is on, then each morph from the deepest under it out to the shell
   or the window, as that morph offers itself (MORPH-HALO-LAYERS). The overlays are
   left out — the halo is one itself, and a halo is about what is under it."
  (when *world*
    (let ((deepest (find-if-not (lambda (m) (typep m 'overlay))
                                (lol.morph:morphs-at *world* wx wy))))
      (when deepest
        (multiple-value-bind (ox oy) (lol.morph:surface-origin deepest)
          (let* ((target (morph-target deepest (- wx ox) (- wy oy)))
                 (pane (getf target :pane)))
            (append
             (mapcar (lambda (l)
                       (if (and pane (getf l :rect))
                           (let ((l (copy-list l)))
                             (setf (getf l :rect) (local-rect-in pane (getf l :rect)))
                             l)
                           l))
                     (target-layers target))
             (loop for m = deepest then (lol.morph:morph-owner m)
                   while (and m (not (typep m 'lol.morph:world)))
                   append (morph-halo-layers m wx wy)))))))))

;;; ===========================================================================
;;; Overlays: the modal cards are morphs in the one loop
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

(defun run-overlay (ov &optional bounds)
  "Open OV and wait for its answer, turning the backend's loop meanwhile (see above);
   -> OVERLAY-RESULT. However the wait ends — an answer, or a restart that unwinds
   straight through it (the debugger's) — the overlay goes. With no loop to turn
   (*WORLD-CYCLE* NIL) or no world, nothing can wait: NIL at once. (A wait opened from
   inside a window needs nothing set aside any more: nothing the browser draws or
   reads lives in specials a window would have had to swap.)"
  (cond
    ((or (null *world*) (null *world-cycle*)) nil)
    (t
     (open-overlay ov bounds)
     (let ((*blocking-overlays* (1+ *blocking-overlays*)))
       (unwind-protect
            (loop until (overlay-done ov)
                  do (when (eq :quit (funcall *world-cycle*))
                       (setf *world-quit* t)
                       (close-overlay ov nil)))
         (unless (overlay-done ov) (close-overlay ov nil))))
     (overlay-result ov))))

(defun draw-world-backdrop (canvas w h)
  "What a modal card floats over when it is drawn on its own (DRAW-BACKDROP — a frame
   rendered by render-frame.lisp): the whole world but its overlays."
  (declare (ignore w h))
  (when *world*
    (lol.morph:world-paint *world* canvas :exclude (lambda (m) (typep m 'overlay)))))

(setf *backdrop-fn* 'draw-world-backdrop)

;;; ===========================================================================
;;; The halo acts on morphs (step 3)
;;; ===========================================================================
;;;
;;; On a WINDOW's layer the halo offers what Morphic's halo is for — handles that act on
;;; the morph itself: Move and Resize (the hand carries it, no button held, until a
;;; click puts it down; arrows and C-arrows work meanwhile, Return or Esc stop), Raise,
;;; Close and Re-tile. They come first (shell.lisp's HALO-ENTRIES asks MORPH-AFFORDANCES),
;;; before Inspect and the Code handles every element has. Inspect on the window shows
;;; the TRAIL-WINDOW instance, and its BOUNDS field is settable: C-c C-c, type (list 100
;;; 80 700 450), and the window moves — through SET-MORPH-BOUNDS, so the screen hears of
;;; it (INSPECT-SLOT-SETTER below).

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
                             "moving: move the pointer, click to put it down · arrows too · Ret/Esc done")))

(defmethod lol.morph:morph-affordances ((w trail-window))
  (list (list :label "Move"    :action (lambda () (halo-carry w :move)))
        (list :label "Resize"  :action (lambda () (halo-carry w :resize)))
        (list :label "Raise"   :action (lambda () (focus-window w)))
        (list :label "Close"   :action (lambda () (close-window-trail w)))
        (list :label "Re-tile" :action (lambda () (retile-window w)))))

;; the inspector's fields of a morph (model.lisp, INSPECT-PLACES): BOUNDS is set the
;; way that damages the screen; OWNER and CHILDREN are the tree, which only ADD-MORPH /
;; REMOVE-MORPH may change — shown, not settable
(defmethod inspect-slot-setter ((m lol.morph:morph) (slot (eql 'lol.morph::bounds)))
  (lambda (new) (lol.morph:set-morph-bounds m new)))
(defmethod inspect-slot-setter ((m lol.morph:morph) (slot (eql 'lol.morph::owner))) nil)
(defmethod inspect-slot-setter ((m lol.morph:morph) (slot (eql 'lol.morph::children))) nil)

;;; ===========================================================================
;;; Stepping: a live pane steps itself
;;; ===========================================================================
;;;
;;; An EXPANDED pane that changes on its own steps: MORPH-STEP-MS answers a period from
;;; the pane — a running stage 33 ms, a run pane whose program is live (or whose view is
;;; behind its output) 100 ms — and NIL otherwise, so a still pane costs nothing and the
;;; poll sleeps. STEP-MORPH advances it and damages exactly what changed: a stage its
;;; BODY (the viewport and the transcript — the repair then draws that pane alone, under
;;; a clip: not the tab strip, the breadcrumbs or the status line, which are other
;;; morphs), a run pane itself. A stage in a hidden tab, or behind a breadcrumb, does not
;;; step at all: it is not expanded on screen.

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
  "How often P must step: a running stage's frame, a run pane's tick, or NIL — P is
   still."
  (cond ((null p) nil)
        ((pane-running-stage p) +stage-frame-ms+)
        ((run-pane-step-ms p))
        (t nil)))

(defmethod lol.morph:morph-step-ms ((p pane))
  (and (eq (pane-mode p) :expanded) (zerop *blocking-overlays*) (pane-step-ms p)))

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
   own restarts on the rail."
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

(defmethod lol.morph:step-morph ((p pane) now)
  (declare (ignore now))
  (cond ((pane-running-stage p)
         ;; the stage's body morph alone. A step that opened something beside it (the
         ;; Workspace a [define-method] template asks for) changes the layout too, and
         ;; the layout damages what it moves — the next DESK-SYNC: nothing more here
         (step-stage-pane p)
         (lol.morph:invalidate (or (pane-body p) p)))
        ;; a run's text: its body (the text view finds its own rows; a run's colours
        ;; come from its output, so the rows compare those too)
        ((step-run-panes p) (damage-body (pane-body p)))))

;;; ---- what the services changed, exactly ---------------------------------------------

(defun visible-trails ()
  "The trails on screen: the tiled one, and every window's."
  (if *world*
      (remove nil (cons (and *trails* (nth (tiled-index) *trails*))
                        (mapcar #'window-trail (desk-windows))))
      (list (current-trail))))

(defun damage-pane (p)
  "P changed on its own (a live worker's view arrived): redraw its body where it is on
   screen, and nothing else — its header, if the title changed, its signature finds; a
   pane nobody shows costs nothing."
  (when (and *world* (pane-mode p) (lol.morph:morph-world p))
    (lol.morph:invalidate (or (pane-body p) p))))

(defun paint-into (m canvas fn)
  "Let M paint part of itself straight into the canvas it draws into — the screen's
   CANVAS for a morph on the screen, a window's backing store for one in a window —
   the fast paths that skip a whole DRAW-MORPH: the clock's tick, a terminal's rows, a
   web page's frame. FN gets that canvas and returns the rects (in its coordinates) it
   painted, or :FAIL when it could not (then M is damaged whole). On the screen the
   world composites whatever lies over those rects (a window, an overlay's scrim) and
   sends them (LOL.MORPH:NOTE-PAINTED); in a window they are only copied out. -> the
   rects, or NIL."
  (let ((s (lol.morph:morph-surface m)))
    (cond
      ((null s) nil)
      ((typep s 'lol.morph:surface)
       (let ((b (lol.morph:surface-backing s)))
         (if (or (null b) (eq (lol.morph:surface-damage s) :all))
             (progn (lol.morph:invalidate m) nil)  ; not drawn yet: draw it whole
             (let ((rects (funcall fn b)))
               (if (eq rects :fail)
                   (progn (lol.morph:invalidate m) nil)
                   (progn
                     ;; the store is right there now: the window only has to be copied
                     (dolist (r rects) (lol.morph:invalidate s (lol.morph:to-world-rect m r)))
                     rects))))))
      (t
       (let ((rects (funcall fn canvas)))
         (if (eq rects :fail)
             (progn (lol.morph:invalidate m) nil)
             (progn (lol.morph:note-painted s rects m) rects)))))))

(defun tick-tray (canvas)
  "The clock ticked: repaint the tray alone, straight into the screen's CANVAS, and let
   the world composite what lies over it (PAINT-INTO)."
  (let ((tray (desk-tray)))
    (when (and tray (lol.morph:morph-world tray))
      (paint-into tray canvas
                  (lambda (c)
                    (destructuring-bind (x y w h) (lol.morph:morph-local-bounds tray)
                      (declare (ignore y h))
                      (multiple-value-bind (x0 clip) (draw-tray c (+ x w))
                        (declare (ignore x0))
                        (setf (tray-clip tray) clip))
                      (list (lol.morph:morph-local-bounds tray))))))))

(defun on-screen-panes ()
  "Every pane morph on screen: expanded, collapsed and side."
  (and *world* (remove-if-not (lambda (m) (and (pane-p m) (pane-mode m)))
                              (lol.morph:morph-descendants *world*))))

(defun terminal-rows-to-world (canvas)
  "The terminals' programs wrote: repaint only the ROWS that changed, in each terminal
   pane on screen — the tiled one's straight onto the screen's CANVAS, a window's into
   its backing store (PAINT-INTO). A terminal that cannot be updated this way (a side
   pane, a grid that no longer fits) is drawn again whole."
  (dolist (p (on-screen-panes))
    (when (terminal-pane-p p)
      (let ((*active* (or (pane-trail-index p) *active*)))
        (paint-into (or (pane-body p) p) canvas
                    (lambda (c) (or (terminal-paint-dirty p c) :fail)))))))

(defun pane-painted (pane canvas fn)
  "PAINT-INTO for a pane by the backends' services (web.lisp's frames): NIL when PANE
   is not on screen."
  (when (and *world* (pane-mode pane)) (paint-into (or (pane-body pane) pane) canvas fn)))
