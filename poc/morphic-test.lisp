;;;; morphic-test.lisp — host test for the World and its windows (doc/morphic.org,
;;;; steps 1–2), against the real shell, headless.
;;;;
;;;;     sbcl --script poc/morphic-test.lisp        ; exits 0 if every check passes
;;;;
;;;; The one property damage repair must have, checked after every step of a long
;;;; script: the screen built INCREMENTALLY — each event's damage repaired alone, into
;;;; the same canvas, as the guest's loop does — is pixel for pixel the screen a FULL
;;;; repair would draw from scratch. A missed rect shows up as a difference at once,
;;;; anywhere on the screen, for any event. On top of that, the economy: which morphs
;;;; a repair drew (a window moved is copied, not redrawn; a key in a window redraws
;;;; the window and the status line, not the shell), and how big the repaired area is.
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
  "Is the incrementally repaired screen the screen a full repair draws?"
  (repair)
  (lol.canvas:fill-rect *fresh* 0 0 +w+ +h+ #x123456)          ; garbage to cover
  (lol.morph:invalidate *world*)
  (let ((saved (lol.morph:world-last-repair *world*)))
    (lol.morph:world-repair *world* *fresh*)
    (setf (lol.morph:world-last-repair *world*) saved))
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
(defun drawn () (getf (lol.morph:world-last-repair *world*) :drawn))
(defun drew-p (m) (and (assoc m (drawn)) t))

(defun shell-area-drawn ()
  "Pixels of SHELL the last repair redrew: the area of the rects the shell drew in."
  (loop for (r . morphs) in (getf (lol.morph:world-last-repair *world*) :per-rect)
        when (member (desk-shell *world*) morphs) sum (lol.morph:rect-area r)))

;;; ---------------------------------------------------------------------------

(open-environment (parse-subject "modules"))   ; tab 1 the module index, tab 2 a Workspace
(setf *world* (make-desk +w+ +h+))
(repair)

(format t "~&== the world with one morph ==~%")
(check "the screen starts as one full repair of the shell"
       (and (getf (lol.morph:world-last-repair *world*) :full)
            (equal (mapcar #'car (drawn)) (list (desk-shell *world*)))))
(check "a full repair is draw-browser, byte for byte"
       (let ((c (lol.canvas:make-canvas +w+ +h+)))
         (draw-browser c +w+ +h+)
         (equalp (lol.canvas:canvas-pixels c) (lol.canvas:canvas-pixels *screen*))))
(let ((n (lol.morph:world-repairs *world*)))
  (ev :move :x 300 :y 300) (ev :move :x 320 :y 310) (repair)
  (check "a bare pointer move damages nothing" (= n (lol.morph:world-repairs *world*))))
(keys "down")
(check "a key damages the shell, and the screen is right" (screen-right-p))

(format t "~&== SuperL-g: tear off, grab, move, resize, re-tile ==~%")
(keys "s-g")
(let ((w (first (windows))))
  (check "SuperL-g tears the focused trail off into a window" (and w (= 1 (length (windows)))))
  (check "... which holds the focus" (eq (lol.morph:world-focus *world*) w))
  (check "... is in grab mode" (eq (desk-grab *world*) w))
  (check "... and has left the tab strip (one tab, the Workspace)"
         (and (= 1 (length *tab-rects*)) (= 1 (car (first *tab-rects*)))))
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
  ;; a key now goes to the WINDOW's pane: the window and the status line redraw
  (let ((sel (pane-sel (car (last (window-trail w))))))
    (keys "down")
    (check "a key in the focused window moves ITS selection"
           (= (pane-sel (car (last (window-trail w)))) (1+ sel)))
    (check "... the repair drew the window"
           (drew-p w))
    (check "... and of the shell only the status line"
           (= (shell-area-drawn) (* +w+ (status-h))))
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
      (check "... and the screen is right" (screen-right-p)))
    (list ww wh)))
;; a second window: the CL-USER package, torn off over the first
(keys "s-l s-g esc")
(let ((w1 (first (windows))) (w2 (second (windows))))
  (check "a second window, on top and focused"
         (and w2 (eq (car (last (lol.morph:morph-children *world*))) w2)
              (eq (lol.morph:world-focus *world*) w2)))
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
              (eq (lol.morph:world-focus *world*) w1)
              (eq (nth *active* *trails*) (window-trail w1))))
  (check "the screen is right" (screen-right-p))
  ;; the trails are (modules: window 1) (Workspace: tiled) (CL-USER: window 2), so
  ;; from window 1 SuperL-Tab goes to the Workspace, then to window 2
  (keys "s-tab")
  (check "SuperL-Tab from a window to a tiled trail gives the shell the focus"
         (eq (lol.morph:world-focus *world*) (desk-shell *world*)))
  (keys "s-tab")
  (check "... and again, to the next window: raised and focused"
         (and (eq (lol.morph:world-focus *world*) w2)
              (eq (car (last (lol.morph:morph-children *world*))) w2)))
  (check "the screen is right" (screen-right-p))
  ;; drop the second window on the tab strip: it re-tiles
  (destructuring-bind (x y ww wh) (bounds w2)
    (declare (ignore ww wh))
    (drag (+ x 30) (+ y 5) (+ x 30) 5))
  (check "dropping a window on the tab strip re-tiles it"
         (and (= 1 (length (windows))) (null (trail-window-of (nth *active* *trails*)))
              (eq (lol.morph:world-focus *world*) (desk-shell *world*))))
  (check "the screen is right" (screen-right-p)))

(format t "~&== a tab dragged out of the strip, the close box, the halo ==~%")
(let* ((tabs (sort (copy-list *tab-rects*) #'< :key #'car))
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
    (let ((last (car (last (halo-layers (+ x 60) (+ y 80))))))
      (check "the halo's layers inside a window end on the window, then the shell"
             (let ((ls (halo-layers (+ x 60) (+ y 80))))
               (and (eq (getf last :object) (desk-shell *world*))
                    (eq (getf (car (last ls 2)) :object) w)))))))

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
    (format t "~&  key in a window (window + status):  ~a ms~%"
            (ms (lambda () (setf *active* (window-trail-index w))
                  (ev :key :key :down :state 0) (repair))))))

(format t "~&~d checks, ~d failed~%" *checks* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
