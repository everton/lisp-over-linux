;;;; check-scroll.lisp — assert the browser's scrolling behaviour in the BOOTED guest.
;;;;
;;;;     sbcl --script check-scroll.lisp        ; exits 0 if all assertions pass
;;;;
;;;; poc/render-frame.lisp checks what a frame LOOKS like. This checks what the
;;;; machine DOES with real input, which no host render can: it boots the actual
;;;; image under QEMU, drives the framebuffer browser from outside — synthetic key
;;;; presses, and wheel events that QEMU's PS/2 mouse turns into genuine evdev
;;;; REL_WHEEL records — then reads the browser's OWN state back over the net REPL.
;;;; The assertions are on PANE-TOP / PANE-SEL / TV-SCROLL, never on pixels.
;;;;
;;;; Run ./build.sh first if you changed the Lisp; this boots iso_root as it stands.
;;;;
;;;; The boot/QMP/net-REPL machinery lives in host-client/guest-harness.lisp, which
;;;; is where this file's hard-won pieces (QMP-HOLD and the <<…>> PROBE trick) moved
;;;; when check-worker.lisp became the second guest assertion. Read that header
;;;; before writing a third.
;;;;
;;;; TWO WAYS THIS TEST PREVIOUSLY PROVED NOTHING, both fixed below and both worth
;;;; remembering when writing the next one: the module index is ~19 rows and FITS on
;;;; an 800-line screen, so its viewport cannot move and every wheel assertion
;;;; passed vacuously; and drilling whatever happened to be under the cursor landed
;;;; on a three-line defparameter, which is shorter than its viewport and so cannot
;;;; scroll either. Hence the flat CL-USER list, and reaching a long definition
;;;; deliberately through the Spotter.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(use-package :lol.guest)

(defun state (label)
  "Read the focused pane's scroll state. Returns (SEL TOP TV-SCROLL POINT)."
  (let ((kind   (probe "(view-kind (pane-view (current-pane)))"))
        (sel    (probe-int "(pane-sel (current-pane))"))
        (top    (probe-int "(pane-top (current-pane))"))
        (scroll (probe-int "(if (pane-tv (current-pane)) (lol.textview::tv-scroll (pane-tv (current-pane))) -1)"))
        (point  (probe-int "(if (pane-tv (current-pane)) (lol.textview:tv-point-line (pane-tv (current-pane))) -1)")))
    (format t "~&  ~22a kind=~a sel=~a top=~a scroll=~a point=~a~%"
            label kind sel top scroll point)
    (finish-output)
    (list sel top scroll point)))

(defun checks ()
  (quiet-kernel-console)
  (wait-for-browser)

  ;; 'l' — the FLAT CL-USER list, several hundred rows. NOT the module index the
  ;; image boots into: that is nineteen rows and fits on screen, so its viewport
  ;; cannot move at all. Leave it for the menu, and open 'l' from there.
  (leave-browser)
  (qmp-key "l") (sleep 0.2) (qmp-key "ret") (sleep 9)

  (format t "~&~%== A. list pane (the flat CL-USER package view) ==~%")
  (let* ((a0 (state "fresh")))
    (qmp-key "pgdn") (sleep 1.5)
    (let ((a1 (state "after PageDown")))
      (qmp-key "pgup") (sleep 1.5)
      (let ((a2 (state "after PageUp")))
        (qmp-wheel "down" 2) (sleep 0.5)
        (let ((a3 (state "after wheel-down x2")))
          (qmp-wheel "up" 1) (sleep 0.5)
          (let ((a4 (state "after wheel-up x1")))
            (qmp-key "down") (sleep 1.5)
            (let ((a5 (state "after down (re-attach)")))
              (check "list PageDown moves the selection down"
                     (and (first a0) (first a1) (> (first a1) (first a0))))
              (check "list PageUp returns it" (eql (first a2) (first a0)))
              (check "list wheel-down x2 moves the VIEWPORT +6"
                     (and (second a3) (second a2) (= (- (second a3) (second a2)) 6)))
              (check "list wheel leaves the SELECTION alone"
                     (eql (first a3) (first a2)))
              (check "list wheel-up x1 moves the viewport -3"
                     (and (second a4) (second a3) (= (- (second a3) (second a4)) 3)))
              ;; re-attaching scrolls only far enough to show the selected row — from
              ;; a viewport at 3 with the selection at 1 that is a viewport of 1, NOT
              ;; a jump back to the top
              (check "an arrow key re-attaches the viewport to the selection"
                     (and (first a5) (second a5) (first a4) (second a4)
                          (= (first a5) (1+ (first a4)))
                          (<= (second a5) (first a5))
                          (/= (second a5) (second a4))))))))))

  ;; Reach a LONG definition on purpose, through the Spotter — see the header.
  (format t "~&~%== B. source pane (a long definition, via the Spotter) ==~%")
  (qmp-hold "meta_l" "spc") (sleep 2.0)
  (qmp-type "draw-brow") (sleep 1.5)
  (qmp-key "ret") (sleep 3.5)
  (let ((b0 (state "fresh source")))
    (qmp-wheel "down" 3) (sleep 0.5)
    (let ((b1 (state "after wheel-down x3")))
      (qmp-wheel "up" 1) (sleep 0.5)
      (let ((b2 (state "after wheel-up x1")))
        (qmp-key "pgdn") (sleep 1.5)
        (let ((b3 (state "after PageDown")))
          (check "source wheel-down scrolls the viewport DOWN"
                 (and (third b0) (third b1) (> (third b1) (third b0))))
          (check "wheel-down x3 == 9 lines (3 x +wheel-lines+)"
                 (and (third b0) (third b1) (= (- (third b1) (third b0)) 9)))
          (check "source wheel-up scrolls back UP"
                 (and (third b2) (third b1) (< (third b2) (third b1))))
          (check "wheel-up x1 == 3 lines"
                 (and (third b1) (third b2) (= (- (third b1) (third b2)) 3)))
          (check "the wheel leaves point alone" (eql (fourth b0) (fourth b1)))
          (check "source PageDown moves point"
                 (and (fourth b3) (fourth b1) (/= (fourth b3) (fourth b1))))))))

  t)

(run-guest-checks "check-scroll" #'checks)
