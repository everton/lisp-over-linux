;;;; check-morphic.lisp — the World, in the booted guest (doc/morphic.org, steps 1–2).
;;;;
;;;;     sbcl --script check-morphic.lisp        ; LOL_PORT=4111 to run beside another
;;;;
;;;; A. The world with one morph (step 1). The environment is a DESK whose only child
;;;;    is the tiled shell, covering the screen and holding the focus; a key repairs
;;;;    it (the shell morph draws once), a bare pointer move repairs nothing; the
;;;;    halo's outermost layer is the SHELL-MORPH instance.
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

(defun checks ()
  (quiet-kernel-console)
  (wait-for-browser)
  (section-a)
  t)

(run-guest-checks "check-morphic" #'checks)
