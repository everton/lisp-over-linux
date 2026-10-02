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
   is what fails on this AREF (\"NIL is not of type SPECIALIZED-ARRAY-ELEMENT-TYPE-
   PROPERTIES\") and on untyped arithmetic (the #x1A memory fault, AGENTS.md). So:
   no wrapper, and the index is computed here on the host."
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

  (probe "(progn (defvar *draws* nil) (setf *draws* nil) (sb-int:encapsulate 'draw-browser 'count (lambda (f &rest args) (push t *draws*) (apply f args))) t)")

  ;; The arrow's (1,2) cell is W — white fill — so it is unmistakable on screen.
  (setf *xres* (probe-int "*fb-xres*"))
  (let ((x0 (probe-int "*cursor-x*")) (y0 (probe-int "*cursor-y*")))
    (dotimes (i 5) (qmp-move 12 7) (sleep 0.3))
    (sleep 1.0)
    (let* ((x1 (probe-int "*cursor-x*")) (y1 (probe-int "*cursor-y*"))
           (n-move (draws)))
      (format t "~&  pointer ~a,~a -> ~a,~a   draw-browser calls during the move: ~a~%"
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
    (format t "~&  draw-browser calls after one key: ~a~%" n)
    (check "a key still redraws" (and n (>= n 1))))

  (probe "(sb-int:unencapsulate 'draw-browser 'count)")
  ;; the record, not an assertion: what a frame and a pointer move cost here. Many
  ;; iterations each, because GET-INTERNAL-REAL-TIME reads CLOCK_MONOTONIC_COARSE on
  ;; Linux (sbcl/src/code/unix.lisp), which only advances once a kernel tick — 4 ms at
  ;; our CONFIG_HZ=250. Ten frames would be timed to the nearest 400 us.
  (format t "~&  full frame (draw + present): ~a us~%"
          (probe "(let ((t0 (get-internal-real-time))) (dotimes (i 50) (draw-browser *fb-canvas* *fb-xres* *fb-yres*) (show-frame *fb-canvas*)) (round (* 1000000 (- (get-internal-real-time) t0)) (* 50 internal-time-units-per-second)))" 300))
  (format t "~&  pointer move: ~a us~%"
          (probe "(let ((t0 (get-internal-real-time))) (dotimes (i 2000) (move-cursor *fb-canvas* *cursor-x* *cursor-y*)) (round (* 1000000 (- (get-internal-real-time) t0)) (* 2000 internal-time-units-per-second)))" 300))
  t)

(run-guest-checks "check-pointer" #'checks)
