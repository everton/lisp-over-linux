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
  (check "a key in the tiled pane repairs that pane, and nothing else"
         (and (equal (repaired-rects) (list (lol.morph:morph-bounds p)))
              (repaired-only-p (list p (first (lol.morph:morph-children p))))))
  (check "... (its preview, its child, drawn with it; the tabs, the status line not)"
         (and (not (drew-p (strip))) (not (drew-p (status))) (not (drew-p (shell)))))
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
(let ((p (tiled-pane)))
  (keys "a b")
  (check "typing in the Workspace repairs the Workspace pane alone"
         (and (equal (repaired-rects) (list (lol.morph:morph-bounds p)))
              (repaired-only-p (list p))))
  (keys "bs bs")
  (check "the screen is right" (screen-right-p)))
(keys "s-1")
;; a change made behind the shell's back (the agent, a service, the REPL): found by
;; the pane's signature, nothing else redrawn
(let ((p (tiled-pane)))
  (setf (pane-view p) (present (pane-subject p))) (repair)
  (check "a pane's view replaced from outside: that pane alone is repaired"
         (and (member (lol.morph:morph-bounds p) (repaired-rects) :test #'equal)
              (not (drew-p (strip))) (not (drew-p (status)))))
  (check "the screen is right" (screen-right-p)))

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
    (check "... the repair redrew that pane, in the window's store, and copied it"
           (and (drew-p p) (drew-p w) (= (1+ r0) (window-renders w))
                (equal (repaired-rects) (list (lol.morph:morph-bounds p)))))
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
         (equal (repaired-rects) (list (lol.morph:to-world-rect p (pane-stage-body p)))))
  (check "... and drew only its pane (and a window over it): not the tab strip, the breadcrumbs, the status line"
         (and (drew-p p) (repaired-only-p (cons p (windows)))))
  (check "the screen is right after a step" (screen-right-p))
  ;; the same stage in a window: only that part of its backing store is drawn again
  (keys "s-g esc")
  (let ((w (trail-window-of (nth *active* *trails*))))
    (check "torn off, the stage pane steps in the window"
           (and w (eq (window-pane w) p) (lol.morph:morph-step-ms p)))
    (lol.morph:world-step-timeout *world*)
    (lol.morph:world-step *world* (+ (lol.morph:now-ms) 2000))
    (repair)
    (check "a step damages the window's stage body only"
           (let ((rects (repaired-rects)) (b (pane-stage-body p)))
             (and (= 1 (length rects)) b
                  (= (lol.morph:rect-area (first rects)) (lol.morph:rect-area b))
                  ;; (the window copies it out, and its frame is drawn over it)
                  (drew-p p) (repaired-only-p (list* p w (window-frame w) (windows))))))
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
