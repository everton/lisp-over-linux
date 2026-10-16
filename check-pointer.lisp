;;;; check-pointer.lisp — assert that moving the mouse does NOT redraw the browser.
;;;;
;;;;     sbcl --script check-pointer.lisp
;;;;
;;;; Boots the image under QEMU, opens the browser, and checks the contract in
;;;; fb-browser.lisp's "the pointer is not part of the frame": a bare pointer move
;;;; re-sends two arrow-sized patches and calls DRAW-BROWSER zero times; a key still
;;;; redraws. Before that change every mouse packet repainted the whole screen, which
;;;; cost 6.5 ms under KVM and 67 ms under QEMU's TCG (tag #24) — under TCG the pointer
;;;; visibly trailed the hand.
;;;;
;;;; Three kinds of evidence, because each one alone could pass for the wrong reason:
;;;;
;;;;   - a COUNT of DRAW-BROWSER calls, read over the net REPL. Counted by
;;;;     encapsulating the function (sb-int:encapsulate — what TRACE is built on) with
;;;;     a PUSH rather than an INCF: the guest cannot compile untyped arithmetic at
;;;;     runtime (AGENTS.md, "Known design facts"), and the encapsulating lambda IS
;;;;     compiled there. A cons needs no arithmetic.
;;;;   - the SCREEN, via QMP's screendump: the arrow's white fill must be at the new
;;;;     position, and the old position must show the canvas's own pixel again (no
;;;;     smear left behind).
;;;;   - the CANVAS, which must not contain the arrow at all — it is composited on the
;;;;     way out, never drawn in.
;;;;
;;;; Then PRESS AND DRAG: in a source pane, QEMU's left button held across real
;;;; motion must select text, a still press must not, and a move after the release
;;;; must not extend it (doc/input.org, "Press and drag").
;;;;
;;;; Also prints what a full frame and a pointer move cost in this guest, for the
;;;; record — not asserted, since that depends on the host.

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

(defun screen-pixel (x y)
  "The #xRRGGBB the guest's display shows at X,Y, from a fresh screendump."
  (let ((ppm (namestring (path "screen.ppm"))))
    (qmp (format nil "{\"execute\":\"screendump\",\"arguments\":{\"filename\":\"~a\"}}" ppm))
    (multiple-value-bind (w h rgb) (lol.png:read-ppm ppm)
      (declare (ignore h))
      (let ((i (* 3 (+ (* y w) x))))
        (logior (ash (aref rgb i) 16) (ash (aref rgb (+ i 1)) 8) (aref rgb (+ i 2)))))))

(defvar *xres* nil "The guest's screen width, read once.")

(defun probe-plain (form-string)
  "PROBE without its IGNORE-ERRORS wrapper, parsed as an integer. A form of nothing
   but nested function calls is evaluated by SBCL's simple evaluator WITHOUT the
   compiler; wrapped in IGNORE-ERRORS it is compiled first — and the guest compiler
   USED to fail on this AREF (\"NIL is not of type SPECIALIZED-ARRAY-ELEMENT-TYPE-
   PROPERTIES\"), until initramfs/logcount-fix.lisp (check-compile.lisp asserts
   it). Kept plain anyway: one less compile per pixel read, and the index is
   computed here on the host."
  (write-string (format nil "(format t \"~~%<<~~s>>~~%\" ~a)" form-string) *repl*)
  (write-char #\Newline *repl*) (force-output *repl*)
  (flet ((answer (s) (between-markers (strip-ansi s))))
    (ignore-errors (parse-integer (string-trim " " (answer (repl-drain 10.0 #'answer)))))))

(defun canvas-pixel (x y)
  (probe-plain (format nil "(aref (lol.canvas:canvas-pixels *fb-canvas*) ~d)"
                       (+ (* y *xres*) x))))

(defun draws () (probe-int "(length *draws*)"))

(defun checks ()
  (quiet-kernel-console)
  (wait-for-browser)                                        ; boots into the module index

  ;; count every repair that drew anything (the screen's morphs are drawn by
  ;; LOL.MORPH:WORLD-REPAIR, damage by damage — doc/morphic.org §12)
  (probe "(progn (defvar *draws* nil) (setf *draws* nil) (sb-int:encapsulate 'lol.morph:world-repair 'count (lambda (f &rest args) (let ((n (lol.morph:world-repairs *world*))) (multiple-value-prog1 (apply f args) (unless (= n (lol.morph:world-repairs *world*)) (push t *draws*)))))) t)")

  ;; The arrow's (1,2) cell is W — white fill — so it is unmistakable on screen.
  (setf *xres* (probe-int "*fb-xres*"))
  (let ((x0 (probe-int "*cursor-x*")) (y0 (probe-int "*cursor-y*")))
    (dotimes (i 5) (qmp-move 12 7) (sleep 0.3))
    (sleep 1.0)
    (let* ((x1 (probe-int "*cursor-x*")) (y1 (probe-int "*cursor-y*"))
           (n-move (draws)))
      (format t "~&  pointer ~a,~a -> ~a,~a   repairs during the move: ~a~%"
              x0 y0 x1 y1 n-move)
      (check "the pointer actually moved" (and x0 x1 (/= x0 x1)))
      (check "a bare pointer move does not redraw" (eql n-move 0))
      (check "the arrow is on screen at its new place"
             (and x1 (= (screen-pixel (+ x1 1) (+ y1 2)) #xFFFFFF)))
      (check "its old place shows the canvas again (no smear)"
             (and x0 (eql (screen-pixel (+ x0 1) (+ y0 2)) (canvas-pixel (+ x0 1) (+ y0 2)))))
      (check "the canvas itself never holds the arrow"
             (let ((p (and x1 (canvas-pixel (+ x1 1) (+ y1 2))))) (and p (/= p #xFFFFFF))))))

  (qmp-key "down") (sleep 1.5)
  (let ((n (draws)))
    (format t "~&  repairs after one key: ~a~%" n)
    (check "a key still redraws" (and n (>= n 1))))

  (probe "(sb-int:unencapsulate 'lol.morph:world-repair 'count)")

  ;; PRESS AND DRAG selects text. A source pane, reached the way check-scroll.lisp
  ;; reaches one; then QEMU's left button genuinely held across real EV_REL motion —
  ;; the same BTN_LEFT a clickpad's physical press sends.
  (format t "~&~%== press and drag selects text ==~%")
  (qmp-hold "meta_l" "spc") (sleep 2.0)
  (qmp-type "draw-brow") (sleep 1.5)
  (qmp-key "ret") (sleep 3.5)
  (flet ((tv (accessor) (probe-int (format nil "(lol.textview::~a (pane-tv (current-pane)))" accessor)))
         (selected () (string= (probe "(lol.textview::selection-p (pane-tv (current-pane)))") "T"))
         (button (down)
           (qmp-events (format nil "{\"type\":\"btn\",\"data\":{\"down\":~a,\"button\":\"left\"}}"
                               (if down "true" "false")))
           (sleep 0.6))
         (glide (dx dy)                     ; in small steps, like a hand
           (dotimes (i 4) (qmp-move (round dx 4) (round dy 4)) (sleep 0.25))
           (sleep 0.8)))
    (let ((tx (tv "tv-x")) (ty (tv "tv-y"))
          (cx (probe-int "*cursor-x*")) (cy (probe-int "*cursor-y*")))
      (when (and tx ty cx cy)
        (glide (- (+ tx 80) cx) (- (+ ty 40) cy)))      ; into the text, near the top
      (button t)
      (let ((held (probe "*button-down*"))
            (line0 (tv "tv-point-line"))
            (sel0 (selected)))
        (glide 120 64)                                  ; about four lines down
        (let ((line1 (tv "tv-point-line")) (sel1 (selected)))
          (button nil)
          (let ((released (probe "*button-down*")))
            (glide 40 40)                               ; button up: just a move
            (let ((line2 (tv "tv-point-line")))
              (format t "~&  press at line ~a, dragged to ~a, after release ~a; held=~a released=~a~%"
                      line0 line1 line2 held released)
              (check "the press is seen as held" (string= held "T"))
              (check "a press alone places the caret, no selection" (not sel0))
              (check "dragging with it held selects" sel1)
              (check "the selection follows the pointer down"
                     (and line0 line1 (> line1 line0)))
              (check "the release is seen" (string= released "NIL"))
              (check "moving after the release does not extend it" (eql line2 line1)))))))
    ;; a click on the spot (press, release, no motion) collapses it again
    (button t) (button nil)
    (check "a still click clears the selection" (not (selected))))
  ;; the record, not an assertion: what a frame and a pointer move cost here. Many
  ;; iterations each, because GET-INTERNAL-REAL-TIME reads CLOCK_MONOTONIC_COARSE on
  ;; Linux (sbcl/src/code/unix.lisp), which only advances once a kernel tick — 4 ms at
  ;; our CONFIG_HZ=250. Ten frames would be timed to the nearest 400 us.
  (format t "~&  full frame (draw + present): ~a us~%"
          (probe "(let ((t0 (get-internal-real-time))) (dotimes (i 50) (draw-browser *fb-canvas* *fb-xres* *fb-yres*) (present-frame *fb-canvas*)) (round (* 1000000 (- (get-internal-real-time) t0)) (* 50 internal-time-units-per-second)))" 300))
  (format t "~&  pointer move: ~a us~%"
          (probe "(let ((t0 (get-internal-real-time))) (dotimes (i 2000) (move-cursor *fb-canvas* *cursor-x* *cursor-y*)) (round (* 1000000 (- (get-internal-real-time) t0)) (* 2000 internal-time-units-per-second)))" 300))
  t)

(run-guest-checks "check-pointer" #'checks)
