;;;; check-morphic.lisp — the World, in the booted guest (doc/morphic.org, steps 1–2).
;;;;
;;;;     sbcl --script check-morphic.lisp        ; LOL_PORT=4111 to run beside another
;;;;
;;;; A. The world with one morph (step 1). The environment is a DESK whose only child
;;;;    is the tiled shell, covering the screen and holding the focus; a key repairs
;;;;    it (the shell morph draws once), a bare pointer move repairs nothing; the
;;;;    halo's outermost layer is the SHELL-MORPH instance.
;;;;
;;;; B. Windows (step 2), every step by real input: SuperL-g on the only tab refuses;
;;;;    on a second it tears the trail off and grabs it, arrows move it a cell, Ctrl-
;;;;    arrows resize; the title bar DRAGGED with QEMU's left button moves it by exactly
;;;;    the pointer's travel, opaquely (copied at every step, never redrawn), and the
;;;;    screendump shows the region it uncovered repaired — pixel for pixel the shell
;;;;    drawn fresh — and the window where it now is; a key in the window repairs the
;;;;    window and the status line and nothing else (the DRAW counts say which morph
;;;;    drew); a bare move over it repairs nothing; the grip resizes; a click raises the
;;;;    window under it; SuperL-g t re-tiles; a tab dragged out of the strip becomes a
;;;;    window under the pointer, one dropped on the strip re-tiles, the close box
;;;;    closes; the window keys are in the SuperL-? cheatsheet; and, for the record,
;;;;    what a full repair, a window move and a key in a window cost in this guest.
;;;;
;;;; C. A live pane in a window: a terminal (on a diskless guest it runs the image's
;;;;    own REPL) torn off; keys reach its program, the answer comes back through the
;;;;    terminal service and redraws the window; SuperL-w closes it and hangs it up.
;;;;
;;;; Evidence is read back over the net REPL — the world's own counters (each morph's
;;;; MORPH-DRAWS, the world's LAST-REPAIR) — and input is real: QMP keys and pointer
;;;; motion, through evdev, as a hand would make them.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(handler-bind ((warning #'muffle-warning))
  (load (merge-pathnames "host-client/png.lisp"
                         (make-pathname :directory (pathname-directory *load-truename*)))))

(in-package :lol.guest)

(defun qmp-move (dx dy)
  "Move the emulated PS/2 mouse by DX,DY — real EV_REL records in the guest."
  (qmp-events (format nil "{\"type\":\"rel\",\"data\":{\"axis\":\"x\",\"value\":~d}}" dx)
              (format nil "{\"type\":\"rel\",\"data\":{\"axis\":\"y\",\"value\":~d}}" dy)))

(defun shell-draws () (probe-int "(lol.morph:morph-draws (desk-shell *world*))"))
(defun repairs () (probe-int "(lol.morph:world-repairs *world*)"))

(defun section-a ()
  (format t "~&~%== A. the world with one morph ==~%")
  (check "the environment runs in a desk" (string= (probe "(typep *world* 'desk)") "T"))
  (check "... whose only child is the tiled shell"
         (string= (probe "(mapcar (lambda (m) (type-of m)) (lol.morph:morph-children *world*))")
                  "(SHELL-MORPH)"))
  (check "... covering the whole screen"
         (string= (probe "(equal (lol.morph:morph-bounds (desk-shell *world*)) (list 0 0 *fb-xres* *fb-yres*))")
                  "T"))
  (check "... and holding the keyboard focus"
         (string= (probe "(eq (lol.morph:world-focus *world*) (desk-shell *world*))") "T"))
  (let ((d0 (shell-draws)) (r0 (repairs)))
    (dotimes (i 4) (qmp-move 15 9) (sleep 0.3))
    (sleep 1.0)
    (let ((d1 (shell-draws)) (r1 (repairs)))
      (format t "~&  shell draws ~a -> ~a, repairs ~a -> ~a across a bare move~%" d0 d1 r0 r1)
      (check "a bare pointer move repairs nothing" (and d0 (eql d0 d1) (eql r0 r1))))
    (qmp-key "down") (sleep 1.5)
    (let ((d2 (shell-draws)))
      (format t "~&  shell draws after a key: ~a~%" d2)
      (check "a key repairs the shell (it draws again)" (and d2 d0 (> d2 d0)))
      (check "... as one full-screen rect"
             (string= (probe "(getf (lol.morph:world-last-repair *world*) :full)") "T"))))
  ;; the halo, asked directly for the layers under a point in the pane
  (let ((last (probe "(let ((l (car (last (halo-layers 300 300))))) (type-of (getf l :object)))")))
    (format t "~&  the halo's outermost layer is about: ~a~%" last)
    (check "the halo's outermost layer is the SHELL-MORPH" (string= last "SHELL-MORPH")))
  ;; ... and the real one, opened with QEMU's middle button and walked out with Up
  (qmp-events "{\"type\":\"btn\",\"data\":{\"down\":true,\"button\":\"middle\"}}")
  (qmp-events "{\"type\":\"btn\",\"data\":{\"down\":false,\"button\":\"middle\"}}")
  (sleep 1.5)
  ;; Up walks out one layer at a time (and wraps after the outermost, so stop there)
  (let ((n (probe-int "(length (halo-layers *cursor-x* *cursor-y*))")) (seen '()))
    (when n
      (dotimes (i (1- n))
        (qmp-key "up") (sleep 0.4)
        (push (probe "(type-of (getf *halo-layer* :object))") seen)))
    (format t "~&  ~a layers; walked out through: ~{~a~^ < ~}~%" n (reverse seen))
    (check "walking a real halo OUT ends on the shell morph"
           (equal (first seen) "SHELL-MORPH")))
  (qmp-key "esc") (sleep 1.0))

(defun cursor () (list (probe-int "*cursor-x*") (probe-int "*cursor-y*")))

(defun glide-to (x y &key (step 40))
  "Move the real pointer to X,Y in steps of at most STEP pixels, like a hand."
  (loop repeat 40
        for (cx cy) = (cursor)
        while (and cx cy (or (/= cx x) (/= cy y)))
        do (qmp-move (max (- step) (min step (- x cx))) (max (- step) (min step (- y cy))))
           (sleep 0.12))
  (sleep 0.4))

(defun button (down)
  (qmp-events (format nil "{\"type\":\"btn\",\"data\":{\"down\":~a,\"button\":\"left\"}}"
                      (if down "true" "false")))
  (sleep 0.5))

(defun click-at (x y) (glide-to x y) (button t) (button nil) (sleep 0.6))

(defun drag-to (x1 y1 x2 y2 &key (steps 5))
  "Press at X1,Y1, glide held to X2,Y2 in STEPS, release."
  (glide-to x1 y1) (button t)
  (loop for i from 1 to steps
        do (glide-to (+ x1 (round (* i (- x2 x1)) steps)) (+ y1 (round (* i (- y2 y1)) steps)) :step 60))
  (button nil) (sleep 0.8))

(defun read-list (string)
  (and string (ignore-errors (let ((*read-eval* nil)) (read-from-string string)))))
(defun nwin () (probe-int "(length (desk-windows))"))
(defun wbounds (&optional (n 0))
  (read-list (probe (format nil "(lol.morph:morph-bounds (nth ~d (desk-windows)))" n))))
(defun renders (&optional (n 0))
  (probe-int (format nil "(window-renders (nth ~d (desk-windows)))" n)))

(defun screen-pixels (points)
  "The #xRRGGBB the guest's display shows at each (x y) of POINTS, from one screendump."
  (let ((ppm (namestring (path "screen.ppm"))))
    (qmp (format nil "{\"execute\":\"screendump\",\"arguments\":{\"filename\":\"~a\"}}" ppm))
    (sleep 0.5)
    (multiple-value-bind (w h rgb) (lol.png:read-ppm ppm)
      (declare (ignore h))
      (loop for (x y) in points
            collect (let ((i (* 3 (+ (* y w) x))))
                      (logior (ash (aref rgb i) 16) (ash (aref rgb (+ i 1)) 8) (aref rgb (+ i 2))))))))

(defun sample-points (rect n &key avoid)
  "N points spread over RECT (x y w h), none within 40 px of AVOID (the arrow)."
  (destructuring-bind (x y w h) rect
    (let ((pts '()))
      (loop for i from 0 below (* 4 n)
            for px = (+ x 2 (mod (* i 97) (max 1 (- w 4))))
            for py = (+ y 2 (mod (* i 53) (max 1 (- h 4))))
            while (< (length pts) n)
            unless (and avoid (< (abs (- px (first avoid))) 40) (< (abs (- py (second avoid))) 40))
              do (push (list px py) pts))
      (nreverse pts))))

(defun rect-minus (a b)
  "A minus B as at most four rects (the host's copy of lol.morph:rect-subtract)."
  (destructuring-bind (ax ay aw ah) a
    (destructuring-bind (bx by bw bh) b
      (let ((x0 (max ax bx)) (y0 (max ay by)) (x1 (min (+ ax aw) (+ bx bw))) (y1 (min (+ ay ah) (+ by bh))))
        (if (or (>= x0 x1) (>= y0 y1))
            (list a)
            (remove-if (lambda (r) (or (<= (third r) 0) (<= (fourth r) 0)))
                       (list (list ax ay aw (- y0 ay))
                             (list ax y1 aw (- (+ ay ah) y1))
                             (list ax y0 (- x0 ax) (- y1 y0))
                             (list x1 y0 (- (+ ax aw) x1) (- y1 y0)))))))))

(defun section-b ()
  (format t "~&~%== B. windows ==~%")
  (let ((cw (probe-int "(char-w)")) (lh (probe-int "(line-h)")) (bar (probe-int "+bar-h+")))
    ;; the last tiled trail stays tiled: SuperL-g on the lone Workspace refuses
    (qmp-hold "meta_l" "g") (sleep 1.0)
    (check "SuperL-g on the only tab refuses (the shell keeps one), and says so"
           (and (eql (nwin) 0)
                (string= (probe "(and (search \"last tab\" (or *browser-status* \"\")) t)") "T")))
    (qmp-hold "meta_l" "m") (sleep 2.0)                       ; the module index, a 2nd tab
    (qmp-hold "meta_l" "g") (sleep 1.5)
    (check "SuperL-g tears the module index off into a window" (eql (nwin) 1))
    (check "... focused (it holds the keys) and grabbed"
           (string= (probe "(list (eq (lol.morph:world-focus *world*) (first (desk-windows))) (eq (desk-grab *world*) (first (desk-windows))))")
                    "(T T)"))
    (check "... and gone from the tab strip" (eql (probe-int "(length *tab-rects*)") 1))
    (let ((b0 (wbounds)))
      (qmp-key "right") (sleep 0.5) (qmp-key "right") (sleep 0.5) (qmp-key "down") (sleep 0.8)
      (let ((b1 (wbounds)))
        (format t "~&  grab mode: ~a -> ~a (cell ~ax~a)~%" b0 b1 cw lh)
        (check "in grab mode the arrows move it a cell"
               (and b0 b1 (= (first b1) (+ (first b0) (* 2 cw))) (= (second b1) (+ (second b0) lh)))))
      (qmp-hold "ctrl" "right") (sleep 0.8)
      (let ((b2 (wbounds)))
        (check "... and Ctrl-arrows resize it" (and b0 b2 (= (third b2) (+ (third b0) cw)))))
      (qmp-key "esc") (sleep 0.6)
      (check "Esc ends the grab" (string= (probe "(desk-grab *world*)") "NIL")))
    ;; ---- an opaque drag by the title bar, with the real pointer
    (probe "(progn (defvar *zz-repairs* nil) (setf *zz-repairs* nil) (sb-int:encapsulate 'lol.morph:world-repair 'rec (lambda (f &rest a) (let ((n (lol.morph:world-repairs *world*))) (multiple-value-prog1 (apply f a) (unless (= n (lol.morph:world-repairs *world*)) (push (lol.morph:world-last-repair *world*) *zz-repairs*)))))) t)")
    (let* ((b0 (wbounds)) (r0 (renders)) (dx -150) (dy 90))
      (destructuring-bind (x y w h) b0
        (drag-to (+ x 60) (+ y 6) (+ x 60 dx) (+ y 6 dy))
        (let ((b1 (wbounds)) (r1 (renders))
              (n (probe-int "(length *zz-repairs*)"))
              (full (probe "(count-if (lambda (r) (getf r :full)) *zz-repairs*)"))
              (shell-max (probe-int "(reduce #'max (mapcar (lambda (r) (loop for (rect . ms) in (getf r :per-rect) when (member (desk-shell *world*) ms) sum (lol.morph:rect-area rect))) *zz-repairs*) :initial-value 0)")))
          (format t "~&  drag: ~a -> ~a; ~a repairs (~a full), largest shell area ~a px, renders ~a -> ~a~%"
                  b0 b1 n full shell-max r0 r1)
          (check "dragging the title bar moves the window by the pointer's travel"
                 (and b1 (= (first b1) (+ x dx)) (= (second b1) (+ y dy)) (= (third b1) w) (= (fourth b1) h)))
          (check "... opaquely: repaired along the way, never as the full screen"
                 (and n (>= n 3) (equal full "0")))
          (check "... copied, never redrawn (its renders did not move)" (eql r0 r1))
          (check "... and the shell redrew no more than the strips it uncovered"
                 (and shell-max (< shell-max (* w (+ (abs dy) 30)))))
          ;; the screen: the uncovered region shows the shell exactly as drawn fresh,
          ;; and the window's new place shows the window
          (when b1
            (let* ((cur (cursor))
                   (old-only (first (sort (rect-minus b0 b1) #'> :key (lambda (r) (* (third r) (fourth r))))))
                   (pts (sample-points old-only 24 :avoid cur))
                   (fresh (read-list (probe (format nil "(let ((c (lol.canvas:make-canvas *fb-xres* *fb-yres*))) (draw-shell-now c *fb-xres* *fb-yres*) (mapcar (lambda (p) (aref (lol.canvas:canvas-pixels c) (+ (* (second p) *fb-xres*) (first p)))) '~s))" pts) 10)))
                   (wpts (sample-points b1 24 :avoid cur))
                   (wfresh (read-list (probe (format nil "(let ((b (window-backing (first (desk-windows)))) (o (lol.morph:morph-bounds (first (desk-windows))))) (mapcar (lambda (p) (aref (lol.canvas:canvas-pixels b) (+ (* (- (second p) (second o)) (lol.canvas:canvas-w b)) (- (first p) (first o))))) '~s))" wpts) 10)))
                   (shown (screen-pixels (append pts wpts))))
              (format t "~&  uncovered strip ~a: ~a points; window: ~a points~%" old-only (length pts) (length wpts))
              (check "the screendump shows the uncovered region repaired (= the shell drawn fresh)"
                     (and pts fresh (equal (subseq shown 0 (length pts)) fresh)))
              (check "... and the window at its new place (= its backing store)"
                     (and wpts wfresh (equal (subseq shown (length pts)) wfresh))))))))
    ;; ---- only the damaged morphs draw: a key in the window
    (probe "(setf *zz-repairs* nil)")
    (qmp-key "down") (sleep 1.0)
    (let ((r (read-list (probe "(let ((r (first (last *zz-repairs*)))) (list (mapcar (lambda (e) (list (car e) (mapcar (lambda (m) (symbol-name (type-of m))) (cdr e)))) (getf r :per-rect)) (status-rect) (lol.morph:morph-bounds (first (desk-windows)))))"))))
      (format t "~&  a key in the window: ~a~%" (and r (first r)))
      (check "a key in the window repairs exactly the window and the status line"
             (and r (null (set-exclusive-or (mapcar #'first (first r)) (list (second r) (third r))
                                            :test #'equal))))
      (check "... the window drawn in its rect, the shell only under the status line"
             (and r (null (set-exclusive-or (first r)
                                            (list (list (second r) '("SHELL-MORPH"))
                                                  (list (third r) '("TRAIL-WINDOW")))
                                            :test #'equal)))))
    (probe "(sb-int:unencapsulate 'lol.morph:world-repair 'rec)")
    ;; ---- a bare move over the window draws nothing
    (let ((n (probe-int "(lol.morph:world-repairs *world*)")))
      (destructuring-bind (x y w h) (wbounds)
        (glide-to (+ x (floor w 2)) (+ y (floor h 2))))
      (check "a bare pointer move over a window repairs nothing"
             (eql n (probe-int "(lol.morph:world-repairs *world*)"))))
    ;; ---- the grip
    (destructuring-bind (x y w h) (wbounds)
      (drag-to (+ x w -4) (+ y h -4) (+ x w 56) (+ y h 36))
      (let ((b (wbounds)))
        (format t "~&  grip: ~a -> ~a~%" (list x y w h) b)
        (check "dragging the corner grip resizes the window"
               (and b (= (third b) (+ w 60)) (= (fourth b) (+ h 40))))))
    ;; ---- a second window, and raise on click
    (qmp-hold "meta_l" "l") (sleep 2.0)
    (qmp-hold "meta_l" "g") (sleep 1.2) (qmp-key "esc") (sleep 0.5)
    (check "a second window (CL-USER), on top"
           (and (eql (nwin) 2)
                (string= (probe "(eq (car (last (lol.morph:morph-children *world*))) (second (desk-windows)))") "T")))
    (let ((pt (read-list (probe "(let ((w1 (first (desk-windows))) (w2 (second (desk-windows)))) (destructuring-bind (x y w h) (lol.morph:morph-bounds w1) (loop for yy from (+ y +bar-h+ 30) below (min (+ y h) *fb-yres*) by 9 thereis (loop for xx from (+ x 10) below (min (+ x w) *fb-xres*) by 9 unless (lol.morph:rect-contains-point-p (lol.morph:morph-bounds w2) xx yy) return (list xx yy)))))"))))
      (when pt (click-at (first pt) (second pt)))
      (check "a click on the window underneath raises it and gives it the focus"
             (and pt (string= (probe "(let ((w (car (last (lol.morph:morph-children *world*))))) (and (typep w 'trail-window) (eq w (lol.morph:world-focus *world*)) (eq (window-root w) (first (nth *active* *trails*)))))") "T"))))
    ;; ---- re-tile with the keyboard: SuperL-g then t
    (qmp-hold "meta_l" "g") (sleep 0.8) (qmp-key "t") (sleep 1.2)
    (check "SuperL-g then t re-tiles the focused window" (eql (nwin) 1))
    ;; ---- a tab dragged out of the strip
    (let* ((tab (read-list (probe "(car (last (sort (copy-list *tab-rects*) #'< :key #'car)))")))
           (tx (and tab (+ (cadr tab) 30))))
      (when tx (drag-to tx (floor bar 2) 640 420 :steps 6))
      (check "a tab dragged down out of the strip becomes a window" (eql (nwin) 2))
      (check "... under the pointer"
             (string= (probe "(lol.morph:rect-contains-point-p (lol.morph:morph-bounds (second (desk-windows))) *cursor-x* *cursor-y*)") "T")))
    ;; ---- dropped back on the strip, it re-tiles
    (let ((b (wbounds 1)))
      (when b (drag-to (+ (first b) 40) (+ (second b) 6) (+ (first b) 40) (floor bar 2))))
    (check "a window dropped on the tab strip re-tiles" (eql (nwin) 1))
    ;; ---- the close box
    (let ((n (probe-int "(length *trails*)")) (b (wbounds)))
      (when b (click-at (+ (first b) (third b) -8) (+ (second b) (floor bar 2))))
      (check "the title bar's close box closes the window's trail"
             (and (eql (nwin) 0) n (eql (probe-int "(length *trails*)") (1- n)))))
    (check "every window key is in the SuperL-? cheatsheet"
           (string= (probe "(let ((s (format nil \"~s\" *cheatsheet*))) (and (search \"Super-g\" s) (search \"C-arrows\" s) (search \"re-tile\" s) (search \"drag a tab down\" s) (search \"the corner\" s) t))") "T"))
    ;; ---- the record: what a repair costs in this guest
    (qmp-hold "meta_l" "m") (sleep 2.0) (qmp-hold "meta_l" "g") (sleep 1.0) (qmp-key "esc") (sleep 0.5)
    (format t "~&  full repair (shell + window, drawn and sent): ~a us~%"
            (probe "(let ((t0 (get-internal-real-time))) (dotimes (i 50) (lol.morph:invalidate *world*) (multiple-value-call #'present-repair *fb-canvas* (lol.morph:world-repair *world* *fb-canvas*))) (round (* 1000000 (- (get-internal-real-time) t0)) (* 50 internal-time-units-per-second)))" 300))
    (format t "~&  window moved 10 px (repaired and sent):       ~a us~%"
            (probe "(let ((w (first (desk-windows))) (t0 (get-internal-real-time))) (dotimes (i 50) (destructuring-bind (x y ww wh) (lol.morph:morph-bounds w) (declare (ignore ww wh)) (lol.morph:move-morph w (+ x (if (evenp i) 10 -10)) y)) (multiple-value-call #'present-repair *fb-canvas* (lol.morph:world-repair *world* *fb-canvas*))) (round (* 1000000 (- (get-internal-real-time) t0)) (* 50 internal-time-units-per-second)))" 300))
    (format t "~&  a key in the window (window + status line):  ~a us~%"
            (probe "(let ((w (first (desk-windows))) (t0 (get-internal-real-time))) (dotimes (i 50) (window-changed w) (shell-changed (status-rect)) (multiple-value-call #'present-repair *fb-canvas* (lol.morph:world-repair *world* *fb-canvas*))) (round (* 1000000 (- (get-internal-real-time) t0)) (* 50 internal-time-units-per-second)))" 300))
    (format t "~&  draw-browser alone (the old full frame's draw): ~a us~%"
            (probe "(let ((t0 (get-internal-real-time))) (dotimes (i 50) (draw-browser *fb-canvas* *fb-xres* *fb-yres*)) (round (* 1000000 (- (get-internal-real-time) t0)) (* 50 internal-time-units-per-second)))" 300))
    (probe "(progn (lol.morph:invalidate *world*) t)")))

(defun section-c ()
  "A live pane in a window: a terminal (diskless, it runs the image's own REPL)."
  (format t "~&~%== C. a terminal in a window ==~%")
  (qmp-hold "meta_l" "ret") (sleep 4.0)
  (qmp-hold "meta_l" "g") (sleep 1.2) (qmp-key "esc") (sleep 0.5)
  (check "a terminal torn off into a window, focused"
         (string= (probe "(let ((w (focused-window))) (and w (terminal-pane-p (car (last (window-trail w)))) t))") "T"))
  (let ((r0 (probe-int "(window-renders (focused-window))")))
    (qmp-type "4242") (qmp-key "ret") (sleep 3.0)
    (let ((hits (probe-int "(let ((p (car (last (window-trail (focused-window)))))) (count-if (lambda (l) (search \"4242\" l)) (lol.terminal:term-screen-text (term-session-term (pane-session p)))))"))
          (r1 (probe-int "(window-renders (focused-window))")))
      (format t "~&  lines with 4242: ~a; window renders ~a -> ~a~%" hits r0 r1)
      (check "keys typed into the window reach the program, and its answer comes back"
             (and hits (>= hits 2)))
      (check "... and the window was redrawn with it (the service woke it)"
             (and r0 r1 (> r1 r0)))))
  (let ((n (probe-int "(length *terminals*)")) (nw (probe-int "(length (desk-windows))")))
    (qmp-hold "meta_l" "w") (sleep 1.5)
    (let ((n1 (probe-int "(length *terminals*)")) (nw1 (probe-int "(length (desk-windows))"))
          (left (probe "(some (lambda (tr) (terminal-pane-p (car (last tr)))) *trails*)")))
      (format t "~&  terminals ~a -> ~a, windows ~a -> ~a, a terminal trail left: ~a~%" n n1 nw nw1 left)
      (check "SuperL-w in the window closes its trail and hangs the terminal up"
             (and n nw (eql n1 (1- n)) (eql nw1 (1- nw)) (string= left "NIL"))))))

(defun checks ()
  (quiet-kernel-console)
  (wait-for-browser)
  (section-a)
  (section-b)
  (section-c)
  t)

(run-guest-checks "check-morphic" #'checks)
