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
   read-only textview (reusing its lexer + layout)."
  (let ((v (present subject)))
    (make-pane
     :subject subject :view v
     :tv (when (and (member (view-kind v) '(:source :reference)) (view-text v))
           (let ((tv (lol.textview:make-textview :font *bfont*)))
             (lol.textview:tv-set-text tv (view-text v))
             tv)))))

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
(defvar *crumb-rects* '() "alist (pane-index . y-top) for the collapsed bars, set on draw.")

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
  (let* ((n (length *trail*))
         (ancestors (butlast *trail*))
         (cur (current-pane))
         (y (+ +bar-h+ 4)))
    ;; ancestor breadcrumb bars
    (loop for p in ancestors for i from 0
          do (draw-crumb canvas y w p i) (incf y (+ +bar-h+ 3)))
    ;; the expanded current pane
    (let ((body-y (+ y +bar-h+ 4)) (body-h (- h y +bar-h+ 4 24)))
      (lol.canvas:fill-rect canvas 8 y (- w 16) (- h y 24) *sh-panel*)
      (lol.canvas:draw-rect canvas 8 y (- w 16) (- h y 24) *sh-rule*)
      (lol.canvas:fill-rect canvas 9 y (- w 18) +bar-h+ *sh-hi*)
      (lol.canvas:draw-string canvas *bfont* "v" 16 (+ y 3) *sh-accent*)
      (lol.canvas:draw-string canvas *bfont* (view-title (pane-view cur)) 34 (+ y 3) *sh-accent*)
      (if (pane-tv cur)
          (progn                                    ; source / reference: the textview
            (lol.textview:tv-geometry (pane-tv cur) 12 body-y (- w 24) body-h)
            (lol.textview:tv-draw canvas (pane-tv cur)))
          (draw-list-body canvas cur 12 body-y (- w 24) body-h)))
    ;; status line
    (let ((cmds (ignore-errors (commands-for (pane-subject cur)))))
      (lol.canvas:fill-rect canvas 0 (- h 22) w 22 *sh-hi*)
      (lol.canvas:draw-string canvas *bfont*
        (format nil "up/dn move  ret drill  bksp back  q quit~@[    commands: ~a~]"
                (and cmds (format nil "~{~a~^ · ~}"
                                  (mapcar #'command-label cmds))))
        10 (- h 18) *sh-dim*))))

;;; ---- input ---------------------------------------------------------------

(defun browser-key (key)
  "Handle a decoded key (character or keyword). Returns :quit to exit."
  (let ((p (current-pane)))
    (cond
      ((member key '(#\q :escape)) :quit)
      ((eq key :up)    (if (pane-tv p)
                           (lol.textview:tv-key (pane-tv p) :up 0)
                           (setf (pane-sel p) (max 0 (1- (pane-sel p))))))
      ((eq key :down)  (if (pane-tv p)
                           (lol.textview:tv-key (pane-tv p) :down 0)
                           (setf (pane-sel p)
                                 (min (max 0 (1- (length (view-items (pane-view p)))))
                                      (1+ (pane-sel p))))))
      ((eq key :return)
       (let ((it (selected-item p)))
         (when (and it (item-subject it)) (trail-push (item-subject it)))))
      ((member key '(:backspace :left)) (trail-pop))
      (t nil))))

(defun browser-click (x y)
  "Click: a breadcrumb bar pops to that level; an item in the current list drills."
  (declare (ignorable x))
  ;; breadcrumb?
  (let ((hit (assoc-if (lambda (yt) (<= yt y (+ yt +bar-h+))) *crumb-rects* :key #'cdr)))
    (if hit
        (trail-goto (car hit))
        ;; else, an item in the current list view
        (let ((p (current-pane)))
          (unless (pane-tv p)
            (let* ((items (view-items (pane-view p)))
                   (rows (loop for a in *crumb-rects* maximize (+ (cdr a) +bar-h+ 3)))
                   (body-y (+ (max (+ +bar-h+ 4) rows) +bar-h+ 4))
                   (i (+ (pane-top p) (floor (- y body-y) +row-h+))))
              (when (and (>= i 0) (< i (length items)))
                (setf (pane-sel p) i)
                (let ((it (nth i items)))
                  (when (item-subject it) (trail-push (item-subject it)))))))))))
