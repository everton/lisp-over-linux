;;;; morphic-test.lisp — host test for the World: its core, the shell as a tree of
;;;; morphs, windows, the halo's handles, the overlays and stepping (doc/morphic.org,
;;;; steps 1–5 and §12), against the real shell, headless.
;;;;
;;;;     sbcl --script poc/morphic-test.lisp        ; exits 0 if every check passes
;;;;
;;;; The one property damage repair must have, checked after every step of a long
;;;; script: the screen built INCREMENTALLY — each event's damage repaired alone, into
;;;; the same canvas, as the guest's loop does — is pixel for pixel the screen the whole
;;;; tree draws FROM SCRATCH (every window's contents drawn again too, not copied: so a
;;;; window whose damage was missed shows up as well). A missed rect shows up as a
;;;; difference at once, anywhere on the screen, for any event. On top of that, the
;;;; economy: which morphs a repair drew (a key in a pane draws that pane, not the
;;;; shell; a window moved is copied, not redrawn), and how big the repaired area is.
;;;;
;;;; The overlays that WAIT (a menu, the value prompt) turn a scripted stand-in for the
;;;; backend's loop (SCRIPTED-CYCLE, below), and the screen is checked inside the wait.
;;;;
;;;; Loads poc/render-frame.lisp as a library for the real stack and its key parser.

(defvar cl-user::*render-frame-library* t)
(load (merge-pathnames "render-frame.lisp" (make-pathname :directory (pathname-directory *load-truename*))))

(in-package :cl-user)

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (name ok)
  (incf *checks*)
  (format t "~&  ~70a ~:[FAIL~;PASS~]~%" name ok)
  (unless ok (incf *fails*)))

(defparameter +w+ 1280)
(defparameter +h+ 720)

;; the clock and the battery are frozen: two draws a second apart must agree
(defun clock-string (&optional u) (declare (ignore u)) "2026-10-16 12:00:00")
(defun battery-status (&key fresh) (declare (ignore fresh)) nil)

(set-browser-font
 (open-browser-font (list "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf")
                    (merge-pathnames "cozette.lolf" *poc-dir*)))

(defvar *screen* (lol.canvas:make-canvas +w+ +h+) "Built incrementally, as in the guest.")
(defvar *fresh* (lol.canvas:make-canvas +w+ +h+) "Drawn from scratch, to compare with.")

(defun repair () (lol.morph:world-repair *world* *screen*))

(defun screen-right-p ()
  "Is the incrementally repaired screen the screen the whole tree draws from scratch?"
  (repair)
  (lol.canvas:fill-rect *fresh* 0 0 +w+ +h+ #x123456)          ; garbage to cover
  (lol.morph:world-paint *world* *fresh* :fresh t)
  (equalp (lol.canvas:canvas-pixels *screen*) (lol.canvas:canvas-pixels *fresh*)))

(defun ev (kind &rest args)
  (lol.morph:world-dispatch *world* (apply #'lol.morph:make-event :kind kind args)))

(defun keys (script)
  "Dispatch SCRIPT (render-frame's key tokens), repairing after each."
  (dolist (tok (split-on #\Space script))
    (multiple-value-bind (key arg state) (parse-key-token tok)
      (declare (ignore arg))
      (ev :key :key key :state state)
      (repair))))

(defun drag (x1 y1 x2 y2 &key (steps 4))
  "Press at X1,Y1, move held in STEPS steps to X2,Y2, release — repairing each step."
  (ev :down :x x1 :y y1) (repair)
  (loop for i from 1 to steps
        do (ev :drag :x (+ x1 (round (* i (- x2 x1)) steps)) :y (+ y1 (round (* i (- y2 y1)) steps)))
           (repair))
  (ev :up :x x2 :y y2) (repair))

(defun windows () (desk-windows *world*))
(defun bounds (w) (copy-list (lol.morph:morph-bounds w)))
(defun last-repair () (lol.morph:world-last-repair *world*))
(defun drawn () (getf (last-repair) :drawn))
(defun drew-p (m) (and (assoc m (drawn)) t))
(defun shell () (desk-shell *world*))
(defun tiled-pane () (holder-pane (shell)))
(defun status () (shell-status (shell)))
(defun strip () (shell-strip (shell)))
(defun focus-holder () (morph-holder (lol.morph:world-focus *world*)))

(defun shell-area-drawn ()
  "Pixels of the SHELL the last repair redrew: the area of the rects in which any of the
   shell's morphs (the shell, its strip, tray, panes, status line) drew."
  (loop for (r . morphs) in (getf (last-repair) :per-rect)
        when (some (lambda (m) (eq (morph-holder m) (shell))) morphs)
          sum (lol.morph:rect-area r)))

(defun repaired-rects () (getf (last-repair) :rects))
(defun repaired-only-p (allowed)
  "Did the last repair draw only morphs in ALLOWED (inside windows too)?"
  (every (lambda (e) (member (car e) allowed)) (drawn)))

;;; ---- the core alone: a surface, with plain morphs (doc/morphic.org §12.4) ---------

(defclass box (lol.morph:morph)
  ((color :initarg :color :reader box-color)
   (seen :initform '() :accessor box-seen))
  (:documentation "A test morph: fills its bounds — its LOCAL bounds, so inside a
   surface it paints in the surface's coordinates — and records the events it got."))
(defmethod lol.morph:morph-opaque-p ((b box)) t)
(defmethod lol.morph:draw-morph ((b box) canvas clip)
  (declare (ignore clip))
  (destructuring-bind (x y w h) (lol.morph:morph-local-bounds b)
    (lol.canvas:fill-rect canvas x y w h (box-color b))))
(defmethod lol.morph:handle-event ((b box) e)
  (push (list (lol.morph:event-x e) (lol.morph:event-y e) (lol.morph:event-wx e) (lol.morph:event-wy e))
        (box-seen b))
  t)
(defclass test-surface (lol.morph:surface) ())
(defmethod lol.morph:draw-surface-ground ((s test-surface) canvas clip)
  (declare (ignore clip))
  (destructuring-bind (x y w h) (lol.morph:morph-bounds s)
    (declare (ignore x y))
    (lol.canvas:fill-rect canvas 0 0 w h #x202020)))
(defclass grip-only (box) ()
  (:documentation "Under the pointer only in its bottom-right 10x10 (MORPH-HIT-P)."))
(defmethod lol.morph:morph-opaque-p ((b grip-only)) nil)
(defmethod lol.morph:draw-morph ((b grip-only) canvas clip)
  (declare (ignore clip))
  (destructuring-bind (x y w h) (lol.morph:morph-local-bounds b)
    (lol.canvas:fill-rect canvas (+ x w -10) (+ y h -10) 10 10 (box-color b))))
(defmethod lol.morph:morph-hit-p ((b grip-only) x y)
  (destructuring-bind (bx by w h) (lol.morph:morph-bounds b)
    (and (>= x (+ bx w -10)) (>= y (+ by h -10)) (< x (+ bx w)) (< y (+ by h)))))

(format t "~&== the core: a surface with morphs inside ==~%")
(let* ((world (lol.morph:make-world 400 300))
       (ground (make-instance 'box :color #x111111 :bounds (list 0 0 400 300)))
       (s (make-instance 'test-surface :bounds (list 100 100 200 150)))
       (child (make-instance 'box :color #xAA0000 :bounds (list 120 130 50 40)))
       (corner (make-instance 'grip-only :color #x00AA00 :bounds (list 100 100 200 150)))
       (screen (lol.canvas:make-canvas 400 300))
       (fresh (lol.canvas:make-canvas 400 300)))
  (lol.morph:add-morph world ground)
  (lol.morph:add-morph world s)
  (lol.morph:add-morph s child)
  (lol.morph:add-morph s corner)
  (flet ((px (c x y) (aref (lol.canvas:canvas-pixels c) (+ (* y 400) x)))
         (right () (lol.morph:world-repair world screen)
                   (lol.canvas:fill-rect fresh 0 0 400 300 #x123456)
                   (lol.morph:world-paint world fresh :fresh t)
                   (equalp (lol.canvas:canvas-pixels screen) (lol.canvas:canvas-pixels fresh))))
    (lol.morph:world-repair world screen)
    (check "inside a surface a morph draws in the surface's coordinates"
           (and (= (px screen 125 135) #xAA0000) (= (px screen 105 105) #x202020)
                (= (px screen 50 50) #x111111)))
    (check "... its local bounds are its bounds less the surface's corner"
           (equal (lol.morph:morph-local-bounds child) '(20 30 50 40)))
    (let ((r0 (lol.morph:surface-renders s)))
      (lol.morph:move-morph s 140 120)
      (lol.morph:world-repair world screen)
      (check "a moved surface is copied, not drawn again"
             (= r0 (lol.morph:surface-renders s)))
      (check "... its morphs' bounds moved with it, their local bounds did not"
             (and (equal (lol.morph:morph-bounds child) '(160 150 50 40))
                  (equal (lol.morph:morph-local-bounds child) '(20 30 50 40))))
      (check "... and the screen is right" (right)))
    (let ((r0 (lol.morph:surface-renders s)))
      (lol.morph:invalidate child)
      (lol.morph:world-repair world screen)
      (check "a morph inside invalidated: the surface draws again, in that rect only"
             (and (= (1+ r0) (lol.morph:surface-renders s))
                  (equal (getf (lol.morph:world-last-repair world) :rects) (list (lol.morph:morph-bounds child)))
                  (assoc child (getf (lol.morph:world-last-repair world) :drawn))
                  (not (assoc ground (getf (lol.morph:world-last-repair world) :drawn)))))
      (check "... and the screen is right" (right)))
    (lol.morph:world-dispatch world (lol.morph:make-event :kind :down :x 170 :y 160))
    (check "an event reaches a morph in its surface's coordinates, with its screen point"
           (equal (first (box-seen child)) '(30 40 170 160)))
    (lol.morph:world-dispatch world (lol.morph:make-event :kind :down :x 335 :y 265))
    (check "MORPH-HIT-P: a morph is under the pointer only where it says"
           (and (equal (first (box-seen corner)) '(195 145 335 265))
                (eq (lol.morph:morph-at world 250 200) s)))
    (lol.morph:set-morph-bounds s (list 90 80 260 190))
    (check "a resized surface draws all of itself again, and the screen is right"
           (let ((r0 (lol.morph:surface-renders s)))
             (and (right) (> (lol.morph:surface-renders s) r0))))))

;;; ---------------------------------------------------------------------------

(open-environment (parse-subject "modules"))   ; tab 1 the module index, tab 2 a Workspace
(setf *world* (make-desk +w+ +h+))
(repair)

(format t "~&== the shell is a tree of morphs (doc/morphic.org §12) ==~%")
(check "the screen starts as one full repair, every morph of the shell drawn"
       (and (getf (last-repair) :full)
            (every (lambda (m) (drew-p m))
                   (list (shell) (strip) (strip-tray (strip)) (shell-area (shell)) (tiled-pane) (status)))))
(check "the tiled trail's panes are the trail area's children, the last one expanded"
       (let ((trail (nth (tiled-index) *trails*)))
         (and (equal (lol.morph:morph-children (shell-area (shell))) trail)
              (eq (pane-mode (car (last trail))) :expanded))))
(check "the keyboard focus is the expanded pane"
       (eq (lol.morph:world-focus *world*) (tiled-pane)))
(check "a full repair is draw-browser, byte for byte"
       (let ((c (lol.canvas:make-canvas +w+ +h+)))
         (draw-browser c +w+ +h+)
         (equalp (lol.canvas:canvas-pixels c) (lol.canvas:canvas-pixels *screen*))))
(let ((n (lol.morph:world-repairs *world*)))
  (ev :move :x 300 :y 300) (ev :move :x 320 :y 310) (repair)
  (check "a bare pointer move damages nothing" (= n (lol.morph:world-repairs *world*))))

(format t "~&== damage per morph ==~%")
(let ((p (tiled-pane)))
  (keys "down")
  (let ((side (pane-preview p)))
    (check "a key in a list repairs its list body and the preview beside it, nothing else"
           (and (member (lol.morph:morph-bounds (pane-body p)) (repaired-rects) :test #'equal)
                (every (lambda (r) (lol.morph:rect-contains-p (lol.morph:morph-bounds p) r))
                       (repaired-rects))
                (drew-p (pane-body p)) (drew-p (pane-body side))
                (repaired-only-p (list p (pane-body p) side (pane-body side))))))
  (check "... (the tabs, the status line, the shell not)"
         (and (not (drew-p (strip))) (not (drew-p (status))) (not (drew-p (shell)))))
  (check "a pane's body is its child, a morph, at the main column; the preview's, the side's"
         (and (typep (pane-body p) 'list-body) (eq (lol.morph:morph-owner (pane-body p)) p)
              (eq (lol.morph:morph-owner (pane-preview p)) p)
              (eq (lol.morph:morph-owner (pane-body (pane-preview p))) (pane-preview p))))
  (check "... the morph under a row is the list body"
         (destructuring-bind (x y w h) (lol.morph:morph-bounds (pane-body p))
           (declare (ignore w h))
           (eq (lol.morph:morph-at *world* (+ x 20) (+ y 20)) (pane-body p))))
  (check "the screen is right" (screen-right-p))
  (setf *browser-status* "a message") (repair)
  (check "a new message in the status line repairs the status line alone"
         (and (equal (repaired-rects) (list (lol.morph:morph-bounds (status))))
              (repaired-only-p (list (status)))))
  (check "the screen is right" (screen-right-p))
  (setf *browser-status* nil) (repair)
  (keys "ret")
  (let ((new (tiled-pane)))
    (check "a drill: the pane becomes a breadcrumb, the new one expands"
           (and (not (eq new p)) (eq (pane-mode p) :collapsed) (eq (pane-mode new) :expanded)))
    (check "... repaired below the tab strip only (the strip is unchanged)"
           (not (drew-p (strip))))
    (check "the screen is right" (screen-right-p))
    (keys "C-g")
    (check "C-g: back, the breadcrumb expands again" (eq (tiled-pane) p))
    (check "the screen is right" (screen-right-p))))
;; the Workspace: typing repairs the Workspace's pane
(keys "s-2")
(check "SuperL-2 switches the tiled trail: the strip and the trail area repaired"
       (and (drew-p (strip)) (drew-p (tiled-pane))))
(check "the screen is right" (screen-right-p))
(let* ((p (tiled-pane)) (b (pane-body p)) (tv (pane-tv p)))
  (keys "a b")
  (check "typing in the Workspace repairs one line of its text view, nothing else"
         (and (= 1 (length (repaired-rects)))
              (= (fourth (first (repaired-rects))) (lol.textview:tv-line-height tv))
              (lol.morph:rect-contains-p (lol.morph:morph-bounds b) (first (repaired-rects)))
              (repaired-only-p (list b))))
  (check "the screen is right" (screen-right-p))
  (keys "ret")
  (check "Return: the rows from the caret's line down (they all moved), still the text view alone"
         (and (repaired-only-p (list b)) (screen-right-p)))
  (keys "up")
  (check "a caret move up repairs the two lines it left and entered (one band)"
         (and (= 1 (length (repaired-rects)))
              (= (fourth (first (repaired-rects))) (* 2 (lol.textview:tv-line-height tv)))
              (repaired-only-p (list b))))
  (check "the screen is right" (screen-right-p))
  (keys "down bs bs bs")
  (check "the screen is right" (screen-right-p))
  ;; an open string colours every line after it: those rows differ too, and are found
  (keys "C-home \"")
  (check "a quote opened at the top: the rows below it repaint (the lexer's state moved)"
         (and (> (reduce #'+ (mapcar #'fourth (repaired-rects))) (* 2 (lol.textview:tv-line-height tv)))
              (not (getf (last-repair) :full))
              (repaired-only-p (list b))))
  (check "the screen is right" (screen-right-p))
  (keys "bs")
  (check "the screen is right" (screen-right-p))
  ;; C-s / C-r (poc/textview.lisp, "Incremental search"): the matches are part of what
  ;; a row shows, the prompt is the status line's content — both found by their keys
  (let ((origin (cons (lol.textview:tv-point-line tv) (lol.textview:tv-point-col tv))))
    (keys "C-s")
    (check "C-s: the status line shows the prompt (only it changed)"
           (and (repaired-only-p (list (status) b)) (drew-p (status))
                (lol.textview:tv-isearch tv) (screen-right-p)))
    (keys "e")
    (check "a letter moves to a match: the rows that light up, and the prompt"
           (and (repaired-only-p (list (status) b)) (drew-p b) (screen-right-p)))
    (keys "C-s C-s")
    (check "C-s again: the next match, the screen right" (screen-right-p))
    (keys "q q q")
    (check "a failing string: the screen right" (screen-right-p))
    (keys "C-g")
    (check "C-g strips the failing part: still searching" (and (lol.textview:tv-isearch tv) (screen-right-p)))
    (keys "C-r bs")
    (check "C-r, DEL: the screen right" (screen-right-p))
    (keys "C-g")
    (check "C-g again: cancelled, point back where it began, no match lit"
           (and (null (lol.textview:tv-isearch tv))
                (equal origin (cons (lol.textview:tv-point-line tv) (lol.textview:tv-point-col tv)))
                (screen-right-p)))
    (keys "C-s e ret")
    (check "Enter ends a search where it stands: the mark at its start, the screen right"
           (and (null (lol.textview:tv-isearch tv)) (equal (lol.textview:tv-mark tv) origin)
                (screen-right-p)))
    (keys "C-x C-x")
    (check "C-x C-x: back to the start, the region shown, the screen right"
           (and (equal origin (cons (lol.textview:tv-point-line tv) (lol.textview:tv-point-col tv)))
                (lol.textview:tv-mark-active tv) (screen-right-p)))
    (keys "C-g C-s e right")
    (check "a motion key ends the search AND moves" (and (null (lol.textview:tv-isearch tv)) (screen-right-p)))))
(keys "s-1")
;; a change made behind the shell's back (the agent, a service, the REPL): found by
;; the pane's signature, nothing else redrawn
(let ((p (tiled-pane)))
  (setf (pane-view p) (present (pane-subject p))) (repair)
  (check "a pane's view replaced from outside: its body is repaired, found by its signature"
         (and (member (lol.morph:morph-bounds (pane-body p)) (repaired-rects) :test #'equal)
              (not (drew-p (strip))) (not (drew-p (status)))))
  (check "the screen is right" (screen-right-p)))

(format t "~&== SuperL-o out of a focused preview: the preview follows the list again ==~%")
(new-trail (parse-subject "package:CL-USER")) (desk-sync *world*) (repair)
(keys "down right")
(let ((p (tiled-pane)))
  (check "-> focuses the selection beside the list, unpinned"
         (and (pane-side p) (pane-editing-side p) (not (pane-side-pinned p))))
  (let ((opened (pane-subject (pane-side p))))
    (keys "s-o")
    (check "SuperL-o back: the focused side goes, as with C-g"
           (and (null (pane-side p)) (not (pane-editing-side p))))
    (keys "down")
    (check "a move in the list shows the NEW selection, not the one opened before"
           (and (pane-preview p)
                (eq (pane-preview-subject p) (item-subject (selected-item p)))
                (not (eq (pane-preview-subject p) opened))))
    (check "the screen is right" (screen-right-p))))
(close-trail) (desk-sync *world*) (repair)

(format t "~&== a pane's body is a morph: a key in a text view beside a list ==~%")
(new-trail (parse-subject "package:CL-USER")) (desk-sync *world*) (repair)
(keys "down S-ret")
(let* ((p (tiled-pane)) (side (pane-side p))
       (lb (pane-body p)) (tb (and side (pane-body side))))
  (check "S-ret pins an editable text view beside the list: two bodies, a list and a text"
         (and (typep lb 'list-body) (typep tb 'text-body) (pane-editing-side p)))
  (check "the screen is right" (screen-right-p))
  (keys "x")
  (check "a key typed into the text view repairs one of its lines — not the list beside it"
         (and (= 1 (length (repaired-rects)))
              (lol.morph:rect-contains-p (lol.morph:morph-bounds tb) (first (repaired-rects)))
              (repaired-only-p (list tb))
              (not (drew-p lb)) (not (drew-p p))))
  (check "the screen is right" (screen-right-p))
  (keys "bs")
  ;; the pointer: a press on the text view goes to the text body, placing the caret
  (let ((tv (pane-tv side)))
    (destructuring-bind (x y w h) (lol.morph:morph-bounds tb)
      (declare (ignore w h))
      (check "the morph under the text is the text body"
             (eq (lol.morph:morph-at *world* (+ x 80) (+ y 40)) tb))
      (ev :down :x (+ x 80) :y (+ y (* 2 (lol.textview:tv-line-height tv)) 12))
      (ev :up :x (+ x 80) :y (+ y (* 2 (lol.textview:tv-line-height tv)) 12)) (repair)
      (check "a click in it moves the caret: two of its lines repaired, the list not"
             (and (<= 1 (length (repaired-rects)) 2) (not (drew-p lb))
                  (every (lambda (r) (lol.morph:rect-contains-p (lol.morph:morph-bounds tb) r))
                         (repaired-rects))))
      (check "the screen is right" (screen-right-p))))
  ;; a press on the list takes the focus back: the list body is the one damaged
  (destructuring-bind (x y w h) (lol.morph:morph-bounds lb)
    (declare (ignore w h))
    (let ((row (first (last (pane-rows p) 2))))
      (ev :down :x (+ x 40) :y (+ (lol.morph:surface-origin p) 0 (car row) 4))
      (ev :up :x (+ x 40) :y (+ (car row) 4)) (repair)
      (check "a press on a row: the list body takes it (the selection moved)"
             (and (drew-p lb) (not (pane-editing-side p))))
      (check "the screen is right" (screen-right-p))))
  (let ((layers (destructuring-bind (x y w h) (lol.morph:morph-bounds lb)
                  (declare (ignore w h)) (halo-layers (+ x 40) (+ y 40)))))
    (check "the halo: the list body is a layer of its own, a morph, inside its pane's"
           (let ((i (position lb layers :key (lambda (l) (getf l :object))))
                 (j (position p layers :key (lambda (l) (getf l :object)))))
             (and i j (< i j))))))
(close-trail) (setf *active* 0)       ; back to the module index, as the script expects
(repair)

(format t "~&== SuperL-g: tear off, grab, move, resize, re-tile ==~%")
(keys "s-g")
(let ((w (first (windows))))
  (check "SuperL-g tears the focused trail off into a window" (and w (= 1 (length (windows)))))
  (check "... whose expanded pane holds the focus" (eq (focus-holder) w))
  (check "... the trail's own pane morphs, moved into the window's trail area"
         (equal (lol.morph:morph-children (window-area w)) (window-trail w)))
  (check "... is in grab mode" (eq (desk-grab *world*) w))
  (check "... and has left the tab strip (one tab, the Workspace)"
         (let ((tabs (strip-tabs (strip)))) (and (= 1 (length tabs)) (= 1 (car (first tabs))))))
  (check "the screen is right" (screen-right-p))
  (let ((b0 (bounds w)) (r0 (window-renders w)))
    (keys "right right down")
    (check "arrows move it by cells"
           (equal (bounds w) (list (+ (first b0) (* 2 (char-w))) (+ (second b0) (line-h))
                                   (third b0) (fourth b0))))
    (check "... without drawing its contents again (a move is a copy)" (= r0 (window-renders w)))
    (check "... and the shell redraws only the strips it uncovered"
           (< (shell-area-drawn) (* 2 (line-h) (fourth b0))))
    (check "the screen is right after the moves" (screen-right-p))
    (keys "S-right")
    (check "Shift moves eight cells" (= (first (bounds w)) (+ (first b0) (* 10 (char-w)))))
    (keys "C-right C-down")
    (check "Ctrl-arrows resize it"
           (and (= (third (bounds w)) (+ (third b0) (char-w))) (= (fourth (bounds w)) (+ (fourth b0) (line-h)))))
    (check "... and the screen is right" (screen-right-p))
    (keys "esc")
    (check "Esc leaves the grab mode" (null (desk-grab *world*))))
  ;; a key now goes to the WINDOW's pane: that pane, in the window's store, and the copy
  (let* ((p (window-pane w)) (sel (pane-sel p)) (r0 (window-renders w)))
    (keys "down")
    (check "a key in the focused window moves ITS selection" (= (pane-sel p) (1+ sel)))
    (check "... the repair redrew that pane's list body (and its header), in the window's store, and copied it"
           (and (drew-p (pane-body p)) (drew-p w) (= (1+ r0) (window-renders w))
                (member (lol.morph:morph-bounds (pane-body p)) (repaired-rects) :test #'equal)
                (every (lambda (r) (lol.morph:rect-contains-p (lol.morph:morph-bounds p) r))
                       (repaired-rects))))
    (check "... and none of the shell, nor the window's title bar"
           (and (zerop (shell-area-drawn)) (not (drew-p (window-title w)))))
    (check "the screen is right" (screen-right-p))))

(format t "~&== the pointer: drag the title bar, the grip, raise, the tab strip ==~%")
(let* ((w (first (windows))) (b0 (bounds w)) (r0 (window-renders w)))
  (destructuring-bind (x y ww wh) b0
    (drag (+ x 40) (+ y 5) (+ x 40 -100) (+ y 5 60))
    (check "dragging the title bar moves the window (opaque: it follows each step)"
           (equal (subseq (bounds w) 0 2) (list (- x 100) (+ y 60))))
    (check "... copied, never redrawn, while it moves" (= r0 (window-renders w)))
    (check "the uncovered region is repaired: the screen is right" (screen-right-p))
    (destructuring-bind (x y ww wh) (bounds w)
      (drag (+ x ww -4) (+ y wh -4) (+ x ww 76) (+ y wh 36))
      (check "dragging the corner grip resizes it" (equal (subseq (bounds w) 2) (list (+ ww 80) (+ wh 40))))
      (check "... its panes laid out again to the new size"
             (= (third (lol.morph:morph-bounds (window-pane w))) (- (+ ww 80) 16)))
      (check "... and the screen is right" (screen-right-p)))
    (list ww wh)))
;; a second window: the CL-USER package, torn off over the first
(keys "s-l s-g esc")
(let ((w1 (first (windows))) (w2 (second (windows))))
  (check "a second window, on top and focused"
         (and w2 (eq (car (last (lol.morph:morph-children *world*))) w2)
              (eq (focus-holder) w2)))
  (check "the screen is right" (screen-right-p))
  ;; click the first one where the second does not cover it
  (let ((pt (loop for yy from (second (bounds w1)) below (+ (second (bounds w1)) (fourth (bounds w1))) by 7
                  thereis (loop for xx from (first (bounds w1)) below (+ (first (bounds w1)) (third (bounds w1))) by 7
                                when (and (> yy (+ (second (bounds w1)) +bar-h+ 40))
                                          (< 0 xx +w+) (< 0 yy +h+)
                                          (not (lol.morph:rect-contains-point-p (lol.morph:morph-bounds w2) xx yy)))
                                  return (cons xx yy)))))
    (when pt
      (ev :down :x (car pt) :y (cdr pt)) (ev :up :x (car pt) :y (cdr pt)) (repair)))
  (check "a click raises the window under it and focuses it"
         (and (eq (car (last (lol.morph:morph-children *world*))) w1)
              (eq (focus-holder) w1)
              (eq (nth *active* *trails*) (window-trail w1))))
  (check "the screen is right" (screen-right-p))
  ;; the trails are (modules: window 1) (Workspace: tiled) (CL-USER: window 2), so
  ;; from window 1 SuperL-Tab goes to the Workspace, then to window 2
  (keys "s-tab")
  (check "SuperL-Tab from a window to a tiled trail gives the shell the focus"
         (eq (focus-holder) (shell)))
  (keys "s-tab")
  (check "... and again, to the next window: raised and focused"
         (and (eq (focus-holder) w2)
              (eq (car (last (lol.morph:morph-children *world*))) w2)))
  (check "the screen is right" (screen-right-p))
  ;; drop the second window on the tab strip: it re-tiles
  (destructuring-bind (x y ww wh) (bounds w2)
    (declare (ignore ww wh))
    (drag (+ x 30) (+ y 5) (+ x 30) 5))
  (check "dropping a window on the tab strip re-tiles it"
         (and (= 1 (length (windows))) (null (trail-window-of (nth *active* *trails*)))
              (eq (focus-holder) (shell))))
  (check "... its pane morphs back in the shell's trail area"
         (equal (lol.morph:morph-children (shell-area (shell))) (nth *active* *trails*)))
  (check "the screen is right" (screen-right-p)))

(format t "~&== a tab dragged out of the strip, the close box, the halo ==~%")
(let* ((tabs (sort (copy-list (strip-tabs (strip))) #'< :key #'car))
       (tab (car (last tabs)))
       (n (length (windows))))
  (drag (+ (cadr tab) 20) 8 700 400 :steps 6)
  (let ((w (car (last (windows)))))
    (check "a tab dragged down out of the strip becomes a window" (= (length (windows)) (1+ n)))
    (check "... under the pointer, carried there"
           (and w (lol.morph:rect-contains-point-p (lol.morph:morph-bounds w) 700 400)))
    (check "the screen is right" (screen-right-p))
    (let ((ntrails (length *trails*)))
      (destructuring-bind (x y ww wh) (bounds w)
        (declare (ignore wh))
        (ev :down :x (+ x ww -10) :y (+ y 8)) (ev :up :x (+ x ww -10) :y (+ y 8)) (repair))
      (check "the close box closes the window's trail"
             (and (= (length *trails*) (1- ntrails)) (not (member w (windows)))))
      (check "the screen is right" (screen-right-p)))))
(let ((w (first (windows))))
  (destructuring-bind (x y ww wh) (bounds w)
    (declare (ignore ww wh))
    (let ((ls (halo-layers (+ x 60) (+ y 80))))
      (check "the halo's layers inside a window: its pane, then its trail area, then the window"
             (and (eq (getf (car (last ls)) :object) w)
                  (eq (getf (car (last ls 2)) :object) (window-area w))
                  (find (window-pane w) ls :key (lambda (l) (getf l :object))))))))

(format t "~&== the font, the last tab ==~%")
(keys "s-=")
(check "a font size change redraws everything, windows included" (screen-right-p))
(keys "s-0")
(let ((tiled (count-if-not #'windowed-trail-p *trails*)))
  (check "(one tiled trail left)" (= tiled 1))
  (setf *active* (position-if-not #'windowed-trail-p *trails*))
  (let ((n (length (windows))))
    (keys "s-g")
    (check "the last tiled trail stays tiled: SuperL-g refuses, and says why"
           (and (= n (length (windows))) (search "last tab" (or *browser-status* ""))))))

;;; ---- steps 3-5 ---------------------------------------------------------------
;;; A blocking overlay (a menu, the value prompt) waits by turning the backend's loop
;;; (*WORLD-CYCLE*). Here the "loop" is a script: each turn repairs, takes the next
;;; event (or runs the next function) and repairs again — what FB-CYCLE does, with the
;;; poll replaced by a list. A turn with nothing left closes whatever is on top, so a
;;; broken script cannot hang the test.

(defvar *script* '())
(defvar *nested-right* '() "SCREEN-RIGHT-P results taken INSIDE a nested wait.")

(defun scripted-cycle ()
  (repair)
  (let ((e (pop *script*)))
    (cond ((null e) (let ((o (top-overlay))) (when o (close-overlay o))))
          ((functionp e) (funcall e))
          (t (apply #'ev e)))
    (repair))
  nil)

(setf *world-cycle* 'scripted-cycle
      ;; what the framebuffer backend installs: the prompt and the chooser overlays
      *value-prompt* 'run-value-prompt *chooser* 'run-chooser)

(defun typed (string) (map 'list (lambda (c) (list :key :key c :state 0)) string))
(defun check-inside () (lambda () (push (screen-right-p) *nested-right*)))
(defun entry-labels (entries) (mapcar (lambda (e) (getf e :label)) entries))

(format t "~&== step 3: the halo acts on morphs ==~%")
(let ((w (first (windows))))
  (setf *active* (window-trail-index w)) (repair)
  (destructuring-bind (x y ww wh) (bounds w)
    (declare (ignore ww wh))
    (let* ((layers (halo-layers (+ x 60) (+ y 80)))
           (wl (find w layers :key (lambda (l) (getf l :object))))
           (entries (and wl (halo-entries wl))))
      (flet ((act (label) (funcall (getf (find label entries :key (lambda (e) (getf e :label))
                                                          :test #'string=)
                                         :action))
                          (repair)))
        (check "a window's halo layer offers Move, Resize, Raise, Close, Re-tile, first"
               (equal (subseq (entry-labels entries) 0 5) '("Move" "Resize" "Raise" "Close" "Re-tile")))
        (check "... then Inspect and the code that draws it"
               (and (member "Inspect" (entry-labels entries) :test #'string=)
                    (find-if (lambda (l) (search "layout-window" l)) (entry-labels entries))))
        ;; Move: the hand carries the window, no button held
        (ev :move :x (+ x 60) :y (+ y 80))
        (act "Move")
        (check "Move: the hand picks the window up (and grab mode is on)"
               (and (eq (lol.morph:hand-carrying (lol.morph:world-hand *world*)) w)
                    (eq (desk-grab *world*) w)))
        (let ((r0 (window-renders w)))
          (ev :move :x (+ x 160) :y (+ y 130)) (repair)
          (check "... a bare pointer move then moves it by the pointer's travel"
                 (equal (subseq (bounds w) 0 2) (list (+ x 100) (+ y 50))))
          (check "... copied, not redrawn" (= r0 (window-renders w))))
        (check "the screen is right" (screen-right-p))
        (ev :down :x (+ x 160) :y (+ y 130)) (ev :up :x (+ x 160) :y (+ y 130)) (repair)
        (check "a click puts it down where it was carried, and ends the grab"
               (and (null (lol.morph:hand-carrying (lol.morph:world-hand *world*)))
                    (null (desk-grab *world*))
                    (equal (subseq (bounds w) 0 2) (list (+ x 100) (+ y 50)))))
        ;; Resize: the same, by the corner
        (destructuring-bind (x y ww wh) (bounds w)
          (ev :move :x (+ x 50) :y (+ y 50))
          (act "Resize")
          (ev :move :x (+ x 90) :y (+ y 80)) (repair)
          (check "Resize: the pointer's travel resizes it, no button held"
                 (equal (subseq (bounds w) 2) (list (+ ww 40) (+ wh 30))))
          (check "the screen is right" (screen-right-p))
          (ev :key :key :return :state 0) (repair)
          (check "Return ends it, where it is"
                 (and (null (lol.morph:hand-carrying (lol.morph:world-hand *world*)))
                      (null (desk-grab *world*)))))))))
;; Inspect: the window's bounds as a field, set through the real gesture
(let* ((w (first (windows)))
       (places (inspect-places w))
       (field (lambda (name) (find name places :key (lambda (p) (getf p :label))))))
  (check "Inspect on a window: BOUNDS is a settable field"
         (getf (funcall field 'lol.morph::bounds) :set))
  (check "... OWNER and CHILDREN (the tree) are not"
         (and (null (getf (funcall field 'lol.morph::owner) :set))
              (null (getf (funcall field 'lol.morph::children) :set))))
  (funcall (getf (funcall field 'lol.morph::bounds) :set) (list 100 120 640 400)) (repair)
  (check "setting it moves and resizes the window on screen"
         (equal (bounds w) '(100 120 640 400)))
  (check "the screen is right" (screen-right-p))
  ;; the gesture: the window inspects ITSELF (its trail pushes the inspector), the
  ;; bounds row is selected, C-c C-c asks for a value in the prompt overlay — a wait
  ;; the scripted loop turns — and the window moves
  (setf *active* (window-trail-index w))
  (trail-push (inspect-subject w))
  (repair)                                  ; the desk lays the new pane out in the window
  (let* ((p (window-pane w))
         (row (position-if (lambda (it) (search "BOUNDS" (princ-to-string (item-label it))))
                           (view-items (pane-view p)))))
    (set-pane-sel p row) (repair)           ; found by the pane's signature
    (setf *script* (append (typed "(list 90 100 600 380)") (list (check-inside)) '((:key :key :return :state 0)))
          *nested-right* '())
    (keys "C-c C-c")
    (check "C-c C-c on the inspector's bounds row, a typed list: the window moves there"
           (equal (bounds w) '(90 100 600 380)))
    (check "... the prompt was an overlay, the world right under it"
           (equal *nested-right* '(t)))
    (check "the screen is right" (screen-right-p))
    (keys "C-g")))

(format t "~&== step 4: the overlays are morphs ==~%")
(setf *active* (position-if-not #'windowed-trail-p *trails*)) (repair)
(keys "s-space")
(let ((o (top-overlay)))
  (check "SuperL-Space puts the Spotter up as an overlay, and returns"
         (typep o 'spotter-overlay))
  (check "the screen is right (the world, dimmed, under the card)" (screen-right-p))
  (keys "d r a w")
  (check "typing filters it" (and (string= (spotter-query o) "draw") (spotter-hits-list o)))
  (check "the screen is right" (screen-right-p))
  (keys "down")
  (check "a move of the selection redraws only the card"
         (let ((card (card-damage-rect (spotter-rect o))))
           (<= (shell-area-drawn) (lol.morph:rect-area card))))
  (check "the screen is right" (screen-right-p))
  ;; the clock ticks behind it: painted straight into the canvas, the scrim and the
  ;; card composited over it by the world
  (tick-tray *screen*)
  (check "the systray's tick under the Spotter: composited, the screen is right" (screen-right-p))
  (let ((n (length *trails*)) (hit (nth 1 (spotter-hits-list o))))
    (keys "ret")
    (check "Enter opens the selected hit in a new trail, and the Spotter goes"
           (and (= (length *trails*) (1+ n)) (null (top-overlay))
                (eq (pane-subject (first (nth *active* *trails*))) (getf hit :subject)))))
  (check "the screen is right" (screen-right-p)))
;; a menu waits for its pick, turning the loop: hover, keys, the card alone redrawn
(let ((entries (list (menu-entry "one" :one) (menu-entry "two" :two) (menu-entry "three" :three)))
      (inside-area nil))
  (setf *script* (list (check-inside)
                       '(:key :key :down :state 0)
                       (lambda () (setf inside-area (shell-area-drawn)))
                       (check-inside)
                       (lambda () (destructuring-bind (x0 y0 x1 y1) (chooser-rect (top-overlay))
                                    (declare (ignore x1 y1))
                                    (ev :move :x (+ x0 30) :y (+ y0 12))))
                       (check-inside)
                       '(:key :key :return :state 0))
        *nested-right* '())
  (let ((e (run-chooser :menu entries 300 200)))
    (check "a menu waits for its pick while the loop turns (Down, a hover, Return)"
           (and e (member (getf e :action) '(:one :two :three))))
    (check "... the world right under it at every turn" (equal *nested-right* '(t t t)))
    (check "... a selection move redrew the card, not the screen"
           (and inside-area (< inside-area (floor (* +w+ +h+) 4))))
    (check "... and it is gone" (and (null (top-overlay)) (null *chooser-state*))))
  (check "the screen is right" (screen-right-p)))
;; a menu opened from INSIDE a window: nothing is set aside — the window's pane and the
;; tiled one each keep what they drew
(let* ((w (first (windows)))
       (wtop (pane-body-top (window-pane w)))
       (stop (pane-body-top (tiled-pane))))
  (setf *active* (window-trail-index w)) (repair)
  (destructuring-bind (x y ww wh) (bounds w)
    (setf *script* (list (check-inside) '(:key :key :escape :state 0)) *nested-right* '())
    (ev :right :x (+ x (floor ww 2)) :y (+ y (floor wh 2))) (repair))
  (check "a right click in a window opens its menu, Esc closes it"
         (and (equal *nested-right* '(t)) (null (top-overlay))))
  (check "... the window's pane and the tiled pane keep their own geometry"
         (and (eql wtop (pane-body-top (window-pane w)))
              (eql stop (pane-body-top (tiled-pane)))))
  (check "the screen is right" (screen-right-p)))
;; the clipboard manager and the value prompt answer through the same wait
(lol.clipboard:clip-copy "zz-one") (lol.clipboard:clip-copy "zz-two")
(setf *script* (list (check-inside) '(:key :key :down :state 0) '(:key :key :return :state 0))
      *nested-right* '())
(check "the clipboard manager waits for a pick: Down, Return -> the older entry"
       (eql 1 (run-clipboard-manager)))
(check "... the world right under it" (equal *nested-right* '(t)))
(setf *script* (append (typed "42") (list (check-inside)) '((:key :key :return :state 0))))
(setf *nested-right* '())
(check "the value prompt reads a line: \"42\"" (equal "42" (run-value-prompt "test")))
(check "... drawn as an overlay band, the world right under it" (equal *nested-right* '(t)))
(check "with no loop to turn (no backend), a menu cannot wait: NIL at once"
       (let ((*world-cycle* nil)) (null (run-chooser :menu (list (menu-entry "x" :x)) 10 10))))
(check "the screen is right" (screen-right-p))

(format t "~&== step 5: stepping ==~%")
(check "nothing on screen steps: the world's poll deadline is -1 (sleep until input)"
       (= -1 (lol.morph:world-step-timeout *world*)))
(new-trail (parse-subject "stage:30"))
(desk-sync *world*) (repair)
(let* ((p (tiled-pane))
       (st (view-data (pane-view p))))
  (setf (stage-running st) t)
  (check "a running stage on screen: its pane steps every frame"
         (= (lol.morph:morph-step-ms p) +stage-frame-ms+))
  (lol.morph:world-step-timeout *world*)
  (let ((n (lol.morph:morph-steps p)))
    (lol.morph:world-step *world* (+ (lol.morph:now-ms) 1000))
    (check "a due step steps it" (= (1+ n) (lol.morph:morph-steps p))))
  (repair)
  (check "the step damaged only the stage's body"
         (equal (repaired-rects) (list (lol.morph:morph-bounds (pane-body p)))))
  (check "... and drew only its stage body (over its pane's panel; a window over it): not the tab strip, the breadcrumbs, the status line"
         (and (drew-p (pane-body p)) (repaired-only-p (list* p (pane-body p) (windows)))))
  (check "the screen is right after a step" (screen-right-p))
  ;; a sprite is picked up by its press moving: the hand carries its instance
  (let* ((sb (pane-body p)) (vp (body-viewport sb)) (b (first (stage-bodies st))))
    (multiple-value-bind (ox oy) (lol.morph:surface-origin sb)
      (let ((x (+ ox (first vp) (body-x b))) (y (+ oy (second vp) (body-y b))))
        (setf (stage-running st) nil) (repair)
        (ev :down :x x :y y) (repair)
        (check "a press on a sprite selects it at once" (eq (stage-selected st) b))
        (ev :drag :x (+ x 30) :y (+ y 20)) (repair)
        (check "... and moved, the hand carries its instance"
               (eq (getf (lol.morph:hand-payload (lol.morph:world-hand *world*)) :subject) (body-obj b)))
        (ev :key :key :escape :state 0) (ev :up :x (+ x 30) :y (+ y 20)) (repair)
        (check "Esc puts it back" (null (lol.morph:hand-payload (lol.morph:world-hand *world*))))
        (setf (stage-running st) t))))
  ;; the same stage in a window: only that part of its backing store is drawn again
  (keys "s-g esc")
  (let ((w (trail-window-of (nth *active* *trails*))))
    (check "torn off, the stage pane steps in the window"
           (and w (eq (window-pane w) p) (lol.morph:morph-step-ms p)))
    (lol.morph:world-step-timeout *world*)
    (lol.morph:world-step *world* (+ (lol.morph:now-ms) 2000))
    (repair)
    (check "a step damages the window's stage body only"
           (let ((rects (repaired-rects)) (b (lol.morph:morph-local-bounds (pane-body p))))
             (and (= 1 (length rects)) b
                  (= (lol.morph:rect-area (first rects)) (lol.morph:rect-area b))
                  ;; (the window copies it out, and its frame is drawn over it)
                  (drew-p (pane-body p))
                  (repaired-only-p (list* p (pane-body p) w (window-frame w) (windows))))))
    (check "the screen is right" (screen-right-p))
    (setf (stage-running st) nil)
    (check "a paused stage steps nothing: back to -1"
           (= -1 (lol.morph:world-step-timeout *world*)))
    (keys "s-w")))
;; a terminal in a window: its program's rows go into the backing store, rect by rect
(when (probe-file "/bin/sh")
  (new-trail (parse-subject "terminal:printf 'one\\ntwo\\n'; sleep 1; printf 'three\\n'; sleep 30"))
  (desk-sync *world*) (repair) (pump-terminals 0.4)
  (keys "s-g esc")
  (let ((w (trail-window-of (nth *active* *trails*))))
    (lol.morph:invalidate *world*) (repair)                 ; drawn once, at its size
    (pump-terminals 1.2)                                    ; "three" arrives
    (terminal-rows-to-world *screen*)
    (let ((r0 (window-renders w)))
      (repair)
      (check "a terminal in a window: new rows reach it without drawing it again"
             (and w (= r0 (window-renders w))
                  (let ((rects (repaired-rects)))
                    (and rects (every (lambda (r) (< (fourth r) (* 3 (line-h)))) rects))))))
    (check "the screen is right" (screen-right-p))
    (keys "s-w")))
(repair)
;; a terminal in a window whose program ends: a clean exit takes the window with it,
;; a failed one keeps it, saying so (doc/terminal.org §10, *TERMINAL-CLOSE-ON-EXIT*)
(when (probe-file "/bin/sh")
  (flet ((serve-until-ended (seconds)
           ;; the guest's :terminals service, turned by the clock: every master "ready"
           (let ((deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
             (loop while (and *terminals* (< (get-internal-real-time) deadline))
                   do (terminal-service (terminal-fds)) (sleep 0.05)))
           (desk-sync *world*)))
    (dolist (case '((0 nil) (3 t)))
      (destructuring-bind (code stays) case
        (let ((tabs (length *trails*)))
          (new-trail (parse-subject (format nil "terminal:sleep 0.3; exit ~d" code)))
          (desk-sync *world*) (repair)
          (keys "s-g esc")
          (let ((w (trail-window-of (nth *active* *trails*))))
            (serve-until-ended 3)
            (check (format nil "a terminal in a window exiting ~d: the window ~:[goes~;stays~]"
                           code stays)
                   (and w (eq stays (and (member w (desk-windows)) t))
                        (= (length *trails*) (if stays (1+ tabs) tabs))))
            (check "the screen is right" (screen-right-p))
            (when stays (keys "s-w"))))))))
(repair)

(format t "~&== drag and drop: the hand carries a subject (doc/morphic.org §13) ==~%")
(defvar zz-drop-target 1 "An inspector field to drop on: this symbol's value cell.")
(defun hand () (lol.morph:world-hand *world*))
(defun row-point (p index)
  "The screen point of row INDEX in the list body of P (as last drawn)."
  (let ((hit (find index (pane-rows p) :key #'cdr)) (b (pane-body p)))
    (multiple-value-bind (ox oy) (lol.morph:surface-origin b)
      (list (+ ox (first (lol.morph:morph-local-bounds b)) 40) (+ oy (car hit) 4)))))
(defun press-drag-release (x1 y1 x2 y2)
  (ev :down :x x1 :y y1) (repair)
  (ev :drag :x (+ x1 20) :y (+ y1 12)) (repair)
  (ev :drag :x x2 :y y2) (repair)
  (ev :up :x x2 :y y2) (repair))
;; a list tiled, a Workspace in a window beside it
(new-trail (parse-subject "package:CL-USER")) (desk-sync *world*) (repair)
(let* ((list-pane (tiled-pane)) (list-trail (nth *active* *trails*)) (list-root (first list-trail))
       (row 2) (item (nth row (view-items (pane-view list-pane))))
       (sub (item-subject item)))
  (keys "s-e s-g esc")                                   ; a fresh Workspace, torn off
  (let* ((w (focused-window)) (ws (window-pane w)) (tv (pane-tv ws)) (tb (pane-body ws)))
    (lol.textview:tv-set-text tv (format nil "(list )~%")) (repair)
    (setf *active* (position list-root *trails* :key #'first)) (repair)
    (lol.morph:set-morph-bounds w (list 700 300 560 380)) (repair)
    (let ((sel0 (pane-sel list-pane)) (n0 (length list-trail)))
      (destructuring-bind (rx ry) (row-point list-pane row)
        ;; aim inside "(list )", just after "list "
        (let ((tx (+ 700 (lol.textview::x-of-col tv 6))) (ty (+ 300 (lol.textview::tv-y tv) 12)))
          (ev :down :x rx :y ry) (repair)
          (check "a press on a row waits: nothing drilled, nothing picked up yet"
                 (and (= n0 (length (nth (position list-root *trails* :key #'first) *trails*)))
                      (eq (press-kind) :row)))
          (ev :drag :x (+ rx 20) :y (+ ry 12)) (repair)
          (check "moved past the travel, it is a pick-up: the hand carries the row's subject"
                 (and (lol.morph:hand-payload (hand))
                      (eq (getf (lol.morph:hand-payload (hand)) :subject) sub)))
          (check "... the status line says what it carries (that line repaired, not the list)"
                 (and (eq :carrying (first (status-line-content (current-pane))))
                      (not (drew-p (pane-body list-pane)))))
          (check "the screen is right while carrying" (screen-right-p))
          (ev :drag :x tx :y ty) (repair)
          (ev :up :x tx :y ty) (repair)
          (let ((text (lol.textview:tv-text tv)) (ref (drop-reference sub)))
            (check "let go over the Workspace in the window: a readable reference is inserted there"
                   (and ref (search ref text) (null (lol.morph:hand-payload (hand)))))
            (check "... it reads back as the row's value (DROP-REFERENCE = DROP-VALUE)"
                   (equal (eval (read-from-string ref)) (drop-value sub)))
            (check "... the list did not drill, its selection stayed"
                   (and (= sel0 (pane-sel list-pane))
                        (= n0 (length (nth (position list-root *trails* :key #'first) *trails*)))))
            (check "... the keys went to the Workspace's window" (eq (focused-window) w))
            (check "... and the repair drew the text view's rows (in the window), not the list"
                   (and (drew-p tb) (not (drew-p (pane-body list-pane)))))
            (check "the screen is right" (screen-right-p))))
        ;; a plain click (no travel) on a row still drills, on the release
        (setf *active* (position list-root *trails* :key #'first)) (repair)
        (destructuring-bind (rx ry) (row-point list-pane row)
          (ev :down :x rx :y ry) (ev :up :x rx :y ry) (repair))
        (check "a press and release without moving is still a click: it drills"
               (= (1+ n0) (length (nth (position list-root *trails* :key #'first) *trails*))))
        (keys "C-g")
        (check "the screen is right" (screen-right-p))
        ;; onto the tab strip: a new trail
        (let ((nt (length *trails*)) (tabs-y (floor +bar-h+ 2)))
          (destructuring-bind (rx ry) (row-point list-pane row)
            (press-drag-release rx ry 500 tabs-y))
          (check "dropped on the tab strip: it opens as a new trail"
                 (and (= (1+ nt) (length *trails*))
                      (eq (pane-subject (first (nth *active* *trails*))) sub)))
          (check "the screen is right" (screen-right-p))
          (keys "s-w"))
        ;; onto a window's pane (its text is editable, so: onto the window's TITLE BAR
        ;; — the window's own — which drills in the window's trail)
        (setf *active* (position list-root *trails* :key #'first)) (repair)
        (let ((n (length (window-trail w))))
          (destructuring-bind (rx ry) (row-point list-pane row)
            (press-drag-release rx ry (+ 700 100) (+ 300 6)))
          (check "dropped on a window's title bar: drilled there, in the window's trail"
                 (and (= (1+ n) (length (window-trail w)))
                      (eq (pane-subject (window-pane w)) sub) (eq (focused-window) w)))
          (check "the screen is right" (screen-right-p))
          (keys "C-g"))))
    ;; an inspector field: zz-drop-target's value cell takes the dropped value
    (new-trail 'zz-drop-target) (desk-sync *world*) (repair)
    (keys "s-g esc")
    (let* ((iw (focused-window)) (ip (window-pane iw))
           (vrow (position-if (lambda (it) (search ":VALUE" (princ-to-string (item-label it))))
                              (view-items (pane-view ip))))
           (nrow (position-if (lambda (it) (search ":NAME" (princ-to-string (item-label it))))
                              (view-items (pane-view ip)))))
      (lol.morph:set-morph-bounds iw (list 700 120 560 300)) (repair)
      (setf *active* (position list-root *trails* :key #'first)) (repair)
      (destructuring-bind (rx ry) (row-point list-pane row)
        (destructuring-bind (vx vy) (row-point ip vrow)
          (press-drag-release rx ry vx vy)))
      (check "dropped on an inspector's settable field: the field is set to the value"
             (eq zz-drop-target (drop-value sub)))
      (check "the screen is right" (screen-right-p))
      (setf *active* (position list-root *trails* :key #'first)) (repair)
      (destructuring-bind (rx ry) (row-point list-pane row)
        (destructuring-bind (nx ny) (row-point ip nrow)
          (press-drag-release rx ry nx ny)))
      (check "on a field that is not settable: refused, and the status line says why"
             (search "not settable" (or *browser-status* "")))
      (check "the screen is right" (screen-right-p))
      ;; the keys: SuperL-p on the selected row, then SuperL-d at a focused field
      (setf zz-drop-target 0)
      (setf *active* (position list-root *trails* :key #'first)) (repair)
      (set-pane-sel list-pane row) (repair)
      (keys "s-p")
      (check "SuperL-p: the hand picks up the selected row's subject"
             (eq (getf (lol.morph:hand-payload (hand)) :subject) sub))
      (keys "esc")
      (check "Esc puts it back: the hand is empty, nothing changed"
             (and (null (lol.morph:hand-payload (hand))) (eql zz-drop-target 0)))
      (keys "s-p")
      (setf *active* (window-trail-index iw)) (set-pane-sel ip vrow) (repair)
      (keys "s-d")
      (check "SuperL-d drops it at the focus: the selected field is set"
             (and (eq zz-drop-target (drop-value sub)) (null (lol.morph:hand-payload (hand)))))
      (check "the screen is right" (screen-right-p))
      (let ((nt (length *trails*)))
        (setf *active* (position list-root *trails* :key #'first)) (repair)
        (keys "s-p s-S-d")
        (check "SuperL-S-d drops it on the tab strip: a new tab"
               (= (1+ nt) (length *trails*)))
        (keys "s-w"))
      ;; something that has no readable form: refused by the text, with a reason
      (let* ((ws (window-pane w)) (tv (pane-tv ws)) (before (lol.textview:tv-text tv)))
        (lol.morph:hand-carry-payload (hand) (list :subject (subj-object (make-hash-table))
                                                   :label "a hash table"))
        (lol.morph:world-drop *world* nil (pane-body ws))
        (check "an unreadable object dropped on text: refused, the text untouched"
               (and (search "no readable form" (or *browser-status* ""))
                    (string= before (lol.textview:tv-text tv)))))
      (setf *active* (window-trail-index iw)) (close-trail)
      (setf *active* (window-trail-index w)) (close-trail)
      (setf *active* (position list-root *trails* :key #'first)) (close-trail)
      (setf *active* 0) (repair)
      (check "the screen is right" (screen-right-p)))))

(format t "~&== the windows across a new world (the console and back; the desk file) ==~%")
(keys "s-l s-g esc")                                     ; a second window, on top
(let* ((layout (desk-window-layout)) (n (length layout)))
  (setf *windows-to-restore* layout
        *world* (make-desk +w+ +h+))
  (check "a new world tears the same trails off again: same bounds, same z-order"
         (and (>= n 2)
              (equal (desk-window-layout) layout)))
  (check "... the panes are the new world's morphs now"
         (every (lambda (w) (eq (lol.morph:morph-world (window-pane w)) *world*)) (windows)))
  (check "the screen is right" (screen-right-p)))

(format t "~&== what it costs, on this host ==~%")
(let* ((w (first (windows))) (iters 30))
  (flet ((ms (fn) (let ((t0 (get-internal-real-time)))
                    (dotimes (i iters) (funcall fn))
                    (/ (round (* 10000 (- (get-internal-real-time) t0))
                              (* iters internal-time-units-per-second)) 10.0))))
    (format t "~&  full repair (shell + windows):      ~a ms~%"
            (ms (lambda () (lol.morph:invalidate *world*) (repair))))
    (format t "~&  window moved 10 px (opaque drag):   ~a ms~%"
            (ms (lambda () (destructuring-bind (x y ww wh) (bounds w)
                             (declare (ignore ww wh))
                             (lol.morph:move-morph w (+ x 10) y) (repair)
                             (lol.morph:move-morph w x y) (repair)))))
    (format t "~&  (that is two moves per sample: there and back)~%")
    (format t "~&  key in a window (its pane):         ~a ms~%"
            (ms (lambda () (setf *active* (window-trail-index w))
                  (ev :key :key :down :state 0) (repair))))
    (setf *active* (tiled-index)) (repair)
    (format t "~&  key in the tiled pane (the pane):   ~a ms~%"
            (ms (lambda () (ev :key :key :down :state 0) (repair))))))

(format t "~&~d checks, ~d failed~%" *checks* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
