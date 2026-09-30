;;;; bench.lisp — the "f) frame timings" menu action: how fast does this machine render?
;;;;
;;;; Times the three things a browser frame is made of, on whatever screen the image
;;;; booted on, and prints a small table beside the reference numbers measured under
;;;; QEMU. It exists for REAL HARDWARE: the laptop has no NIC our kernel drives, so the
;;;; host's net-REPL harness (check-pointer.lisp) cannot reach it, and typing four long
;;;; timing forms into the REPL on a laptop keyboard is a good way to time a typo.
;;;; Deliberately its own file so it can be dropped from build.sh's list when it has
;;;; served (doc/framebuffer.org §9.4 — the numbers it prints are the ones that section
;;;; compares).
;;;;
;;;; What is measured, on the module index (the browser's default root), full screen:
;;;;   draw     DRAW-BROWSER into the canvas — pure Lisp, CPU and memory only
;;;;   present  LOL.FB:PRESENT-FB — one pwrite of the canvas into the framebuffer;
;;;;            on real hardware this is the write into video memory, the number
;;;;            QEMU cannot tell us
;;;;   frame    draw + SHOW-FRAME (present + the pointer patch): what a keystroke costs
;;;;   pointer  MOVE-CURSOR: what a bare mouse move costs (doc/framebuffer.org §9.5)
;;;;
;;;; Why so many repetitions: GET-INTERNAL-REAL-TIME reads CLOCK_MONOTONIC_COARSE on
;;;; Linux (sbcl/src/code/unix.lisp), which only advances once per kernel tick — every
;;;; 4 ms at our CONFIG_HZ=250. One frame cannot be timed at all; fifty can, to within
;;;; 80 microseconds.

(in-package :cl-user)

(defparameter *bench-reference*
  '(:where "QEMU + KVM, tag #24, 1280x800"
    :draw 2.2 :present 0.4 :frame 2.9 :pointer 0.012)
  "What this benchmark measured under QEMU with KVM (doc/framebuffer.org §9.4/§9.5),
   printed alongside so a run on real hardware reads as a comparison.")

(defparameter *bench-rounds* 3
  "Each number is the BEST of this many rounds. A single round moved by more than 2x
   between runs under QEMU (a garbage collection landing inside the loop, the host
   being busy); the minimum is the cost of the work itself, which is what we compare.")

(defun bench-ms (n thunk)
  "Milliseconds per call of THUNK (called with the iteration number): N calls a round,
   best of *BENCH-ROUNDS* rounds."
  (loop repeat *bench-rounds*
        minimize (let ((t0 (get-internal-real-time)))
                   (dotimes (i n) (funcall thunk i))
                   (/ (- (get-internal-real-time) t0)
                      n (/ internal-time-units-per-second 1000.0)))))

(defun bench-render (&key (frames 50) (moves 2000))
  "Time drawing, presenting, a whole frame and a pointer move on this screen, then
   print them next to *BENCH-REFERENCE*. The screen shows the browser while it runs.

   Runs on its OWN environment: *TRAILS* and *ACTIVE* are rebound, so whatever you had
   open in the browser is untouched. *FB-MOUSE* is bound true only so SHOW-FRAME
   composites the pointer the way it does with a real mouse — nothing reads it."
  (unless *bfont* (set-browser-font (lol.canvas:load-font "/cozette.lolf")))
  (multiple-value-bind (xres yres) (lol.fb:fb-geometry)
    (let ((canvas (lol.canvas:make-canvas xres yres))
          (*trails* '()) (*active* 0)
          (*fb-mouse* t) (*cursor-x* 100) (*cursor-y* 100)
          draw present frame pointer)
      (open-environment (subj-modules))
      (lol.fb:with-graphics-console          ; so fbcon's cursor does not draw over us
        ;; WARM UP, untimed. The first frames after boot are not typical: the module
        ;; index builds its rows and caches on first draw, and the fresh 4 MB canvas
        ;; pushes the heap toward a collection. Measured without this, the first run
        ;; after a boot reported draw 7.4 ms / frame 12.6 ms and the second 3.4 / 3.0
        ;; — the first run was timing the warm-up. A full GC after it means no
        ;; collection is already owed when the clock starts.
        (dotimes (i 10) (draw-browser canvas xres yres) (show-frame canvas))
        (sb-ext:gc :full t)
        (setf draw    (bench-ms frames (lambda (i) (declare (ignore i))
                                         (draw-browser canvas xres yres)))
              present (bench-ms frames (lambda (i) (declare (ignore i))
                                         (lol.fb:present-fb canvas)))
              frame   (bench-ms frames (lambda (i) (declare (ignore i))
                                         (draw-browser canvas xres yres)
                                         (show-frame canvas)))
              ;; jiggle one pixel back and forth: each call erases and redraws
              pointer (bench-ms moves (lambda (i)
                                        (let ((old *cursor-x*))
                                          (setf *cursor-x* (if (evenp i) 101 100))
                                          (move-cursor canvas old *cursor-y*))))))
      (let ((ref *bench-reference*))
        (format t "~&~%frame timings — this machine, ~dx~d  (vs ~a)~%"
                xres yres (getf ref :where))
        (flet ((row (label ms ref-ms)
                 (format t "  ~8a ~10,3f ms   ~10,3f ms~%" label ms ref-ms)))
          (format t "  ~8a ~13a   ~13a~%" "" "here" "reference")
          (row "draw"    draw    (getf ref :draw))
          (row "present" present (getf ref :present))
          (row "frame"   frame   (getf ref :frame))
          (row "pointer" pointer (getf ref :pointer)))
        (format t "  (best of ~d rounds of ~d frames / ~d pointer moves; the clock ticks every 4 ms)~%"
                *bench-rounds* frames moves))
      (finish-output)
      (list :xres xres :yres yres
            :draw draw :present present :frame frame :pointer pointer))))
