;;;; morphic.lisp — the browser as the first morph of a Lisp window system.
;;;;
;;;; poc/morph.lisp is Morphic's core and knows nothing about the browser. This file
;;;; is the browser's side of it (doc/morphic.org §2, steps 1–2):
;;;;
;;;;   THE DESK      the world the environment runs in: one per screen, *WORLD*.
;;;;   SHELL-MORPH   the whole tiled browser — tabs, breadcrumbs, the pane, the status
;;;;                 line — as ONE morph. Its DRAW-MORPH is DRAW-BROWSER and its
;;;;                 HANDLE-EVENT is BROWSER-KEY / BROWSER-CLICK / BROWSER-DRAG, so on
;;;;                 the day it arrived nothing on screen changed: the screen was a
;;;;                 world with one full-screen child, and every check passed as before.
;;;;
;;;; Backend-blind like the shell: the guest's loop (initramfs/fb-browser.lisp) and the
;;;; X11 host loop (poc/run-browser.lisp) both build events, hand them to
;;;; LOL.MORPH:WORLD-DISPATCH, and send whatever LOL.MORPH:WORLD-REPAIR redrew.

(in-package :cl-user)

(defvar *world* nil
  "The desk the environment is running in (a DESK), or NIL outside it. Made fresh by
   each entry point (run-browser-fb, run-browser.lisp, render-frame.lisp) for its
   canvas size.")

(defvar *screen-overdrawn* nil
  "Set when something drew the WHOLE screen behind the world's back — a modal loop
   (the Spotter, a menu, the debugger) presents its own frames. The loop then repairs
   everything, since the world no longer knows what is on the glass.")

;;; ---- the shell morph ---------------------------------------------------------

(defclass shell-morph (lol.morph:morph)
  ()
  (:documentation "The tiled browser, whole, as one morph. It cannot yet say WHICH of
   its parts changed, so any change damages all of it — exactly the redraw-everything
   behaviour it had before it was a morph (doc/morphic.org §4.2). Finer damage can come
   one view at a time, without changing the protocol."))

(defmethod lol.morph:morph-opaque-p ((m shell-morph)) t)   ; DRAW-BROWSER fills its bounds
(defmethod lol.morph:morph-name ((m shell-morph)) "the tiled shell")

(defmethod lol.morph:draw-morph ((m shell-morph) canvas clip)
  (declare (ignore clip))
  (destructuring-bind (x y w h) (lol.morph:morph-bounds m)
    (declare (ignore x y))
    (draw-browser canvas w h)))

(defun shell-changed (&optional (m (and *world* (desk-shell *world*))))
  "The browser's state changed: its whole rect must be redrawn."
  (when m (lol.morph:invalidate m)))

(defmethod lol.morph:handle-event ((m shell-morph) e)
  "Every event the browser understands, routed exactly as the old loop routed it."
  (let ((x (lol.morph:event-x e)) (y (lol.morph:event-y e))
        (state (lol.morph:event-state e)))
    (case (lol.morph:event-kind e)
      (:key
       (let ((was-damaged (lol.morph:world-damaged-p *world*))
             (pane-before (current-pane)))
         (if (eq :quit (browser-key (lol.morph:event-key e) state))
             :quit
             (progn
               ;; typed INTO a terminal: the echo comes back as output and repaints only
               ;; its rows (terminal-pane.lisp), so the frame is not damaged — unless it
               ;; already was, or the key was not the console's (an evdev chord)
               (unless (and (getf (lol.morph:event-data e) :tty)
                            (not was-damaged)
                            (fboundp 'terminal-key-was-cheap-p)
                            (funcall 'terminal-key-was-cheap-p pane-before state))
                 (shell-changed m))
               t))))
      (:menu (keyboard-context-menu) (shell-changed m) t)
      (:wheel
       (browser-scroll (* (- (lol.morph:event-notches e)) +wheel-lines+))
       (shell-changed m) t)
      (:down
       (browser-click x y (lol.canvas:ctrl-p state) (lol.canvas:super-p state))
       (shell-changed m) t)
      (:right (browser-context-menu x y) (shell-changed m) t)
      (:middle (browser-middle-click x y (lol.canvas:super-p state)) (shell-changed m) t)
      ;; a drag extends the selection the press started; it redraws only if it did
      (:drag (when (browser-drag x y) (shell-changed m)) t)
      ;; a drag that selected something hands it to PRIMARY and the clipboard (X's
      ;; rule). It changes nothing on screen, as it never did.
      (:up (browser-drag-end) t)
      (t nil))))

;;; ---- the desk: the world the environment runs in ---------------------------------

(defclass desk (lol.morph:world)
  ((shell :initarg :shell :accessor desk-shell
          :documentation "The tiled shell, always the bottom child."))
  (:documentation "The environment's world: the tiled shell at the bottom of the
   z-order."))

(defun make-desk (w h)
  "A world W x H holding the tiled shell, focused, all of it damaged: the first
   repair draws the whole frame."
  (let* ((shell (make-instance 'shell-morph :bounds (list 0 0 w h)))
         (desk (make-instance 'desk :bounds (list 0 0 w h) :shell shell)))
    (lol.morph:add-morph desk shell)
    (setf (lol.morph:world-focus desk) shell)
    desk))

(defun environment-changed ()
  "Something other than an event changed what the environment shows — the agent
   streamed, a run printed, a service woke, the stage ticked. Until those become
   morphs that step themselves (doc/morphic.org step 5), they damage the shell."
  (shell-changed))

;;; ---- the halo knows about morphs --------------------------------------------------
;;; The halo's layers (shell.lisp, HALO-LAYERS) go from the word under the pointer out
;;; to the screen. Outside the screen, now, is the world: each morph under the point
;;; becomes a layer, topmost first, so walking OUT with Up ends on the shell morph —
;;; and Inspect on it shows a CLOS instance with its bounds (doc/morphic.org §6).

(defun morph-halo-layers (x y)
  (when *world*
    (loop for m in (lol.morph:morphs-at *world* x y)
          collect (halo-layer (lol.morph:morph-name m)
                              (format nil "the morph ~a — ~(~a~)" (lol.morph:morph-name m)
                                      (class-name (class-of m)))
                              m (copy-list (lol.morph:morph-bounds m)) :element
                              '(lol.morph:draw-morph lol.morph:handle-event)))))

(setf *halo-outer-layers* 'morph-halo-layers)
