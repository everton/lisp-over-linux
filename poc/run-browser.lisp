;;;; run-browser.lisp — the drill-down code browser, live in an X11 window.
;;;;
;;;;   sbcl --script poc/run-browser.lisp
;;;;
;;;; Wires the real headless layers (registry -> model -> present, from initramfs/)
;;;; to the pixel toolkit (canvas/x11/textview) via the shell. It load-RECORDS the
;;;; whole stack — including the browser's own source — so you can browse the
;;;; browser: root at CL-USER, drill a definition, read its code, drill its senders.
;;;;
;;;; Keys: up/down move · Enter drill · Backspace back · click a breadcrumb to pop
;;;; to that level · PageUp/PageDown and the mouse WHEEL scroll · SuperL-q quits (q is back).
;;;;
;;;; Esc does NOT quit — it is the Emacs Meta prefix (so Esc . is M-., jump to
;;;; definition, and Esc , is M-, Senders). Quit with q, or SuperL-q from anywhere.
;;;;
;;;; Every overlay works here as in the guest — the Spotter, the menus and the halo, the
;;;; value prompt, the clipboard manager, the cheatsheet, the debugger — because they
;;;; are morphs in the same world, and the ones that wait for an answer turn THIS
;;;; backend's loop while they wait: HOST-CYCLE, one turn of it, is installed as
;;;; *WORLD-CYCLE*, the X11 twin of the guest's FB-CYCLE (doc/morphic.org §13, doc/x11.org).
;;;; fb-browser.lisp is loaded for the debugger and the cheatsheet it defines; nothing
;;;; in it touches /dev/fb0 until RUN-BROWSER-FB, which this file never calls.
;;;;
;;;; A host test loads this file as a library (CL-USER::*RUN-BROWSER-LIBRARY* bound
;;;; first): then no display is opened, and HOST-CYCLE can be turned with any event
;;;; source and presenter (poc/x11-test.lisp).

(require :sb-bsd-sockets) (require :sb-introspect) (require :sb-posix)

(let* ((here (make-pathname :directory (pathname-directory *load-truename*))))
  (flet ((f (p) (merge-pathnames p here)))
    (handler-bind ((warning #'muffle-warning))
      ;; the session log first (agent.lisp names its locks to the stall watcher at
      ;; load time), then the registry, which DEFINES load-recording (plain load)
      (load (f "host-logging.lisp"))
      (load (f "../initramfs/registry.lisp"))
      ;; everything else is loaded AND source-captured, so it is all browsable. The
      ;; list is render-frame.lisp's *STACK* (the guest's LISP_SOURCES minus what
      ;; wants hardware) plus x11.lisp, fb.lisp and fb-browser.lisp (for the debugger
      ;; and the cheatsheet); keep the two in step.
      (dolist (m '("../initramfs/sbcl-source.lisp" "../initramfs/model.lisp"
                   "../initramfs/present.lisp" "../initramfs/tour.lisp"
                   "../initramfs/examples.lisp" "../initramfs/stage.lisp"
                   "ttf.lisp" "canvas.lisp" "x11.lisp" "textview.lisp" "widgets.lisp" "morph.lisp"
                   "fb.lisp" "../initramfs/ansi.lisp" "../initramfs/line-editor.lisp"
                   "clipboard.lisp" "org.lisp" "../initramfs/sysinfo.lisp"
                   "inflate.lisp" "image.lisp"
                   "../initramfs/files.lisp" "terminal.lisp" "../initramfs/pty.lisp"
                   "../initramfs/file-views.lisp"
                   "shell.lisp" "file-manager.lisp" "org-mode.lisp" "../initramfs/runs.lisp"
                   "../initramfs/containers.lisp" "../initramfs/json.lisp"
                   "../initramfs/agent-wire.lisp" "../initramfs/agent.lisp"
                   "../initramfs/agent-ui.lisp" "terminal-pane.lisp" "morphic.lisp"
                   "overlays.lisp" "../initramfs/framebuffer.lisp" "../initramfs/meminfo.lisp"
                   "../initramfs/fb-browser.lisp"))
        (load-recording (f m))))))

(in-package :cl-user)

;;; ---- the host's loop, one turn at a time ------------------------------------------
;;; The guest's loop (FB-CYCLE) polls evdev and the console; this one reads X events.
;;; Otherwise they are the same turn: repair what is damaged and send it, wait for
;;; something (input, or the earliest step deadline), turn it into the world's events
;;; and route them, step what steps. The X specifics are three functions, so a test
;;; can turn the very same cycle with a scripted event source and no display.

(defvar *host-canvas* nil "The canvas the world repairs into: the window's pixels.")
(defvar *host-next-event* nil
  "(lambda (wait-ms)) -> the next X event (lol.x11's EVENT), NIL when none came within
   WAIT-MS (-1: wait for one), or :CLOSED when the display went away.")
(defvar *host-present* nil "(lambda (canvas)): show the canvas (X: PutImage).")
(defvar *host-decode* nil "(lambda (keycode state)) -> a key: a character or a keyword.")

(defun x11-world-events (e)
  "X event E as the world's events (poc/morph.lisp): a key, the buttons (X has no
   scroll axis: the wheel is BUTTONS 4 (up) and 5 (down), one press per notch — the
   host's counterpart of the guest's evdev REL_WHEEL record, doc/input.org §7), a
   release, a drag (motion with button 1 held: Button1Mask, #x100) or a bare move."
  (flet ((ev (kind &rest args)
           (list (apply #'lol.morph:make-event :kind kind
                        :x (or (lol.x11:event-x e) 0) :y (or (lol.x11:event-y e) 0)
                        :state (lol.x11:event-state e) args))))
    (case (lol.x11:event-kind e)
      (:key-press
       (let ((key (funcall *host-decode* (lol.x11:event-detail e) (lol.x11:event-state e))))
         (and key (ev :key :key key))))
      (:button-press
       (case (lol.x11:event-detail e)
         (1 (ev :down))
         (2 (ev :middle))                      ; paste PRIMARY / the halo
         (3 (ev :right))                       ; the context menu
         (4 (ev :wheel :notches 1))
         (5 (ev :wheel :notches -1))))
      (:button-release (and (= 1 (lol.x11:event-detail e)) (ev :up)))
      (:motion (if (logtest #x100 (lol.x11:event-state e)) (ev :drag) (ev :move)))
      ;; part of the window was uncovered: X says which rect; that, and no more
      (:expose (lol.morph:invalidate *world* (list (lol.x11:event-x e) (lol.x11:event-y e)
                                                   (lol.x11:event-w e) (lol.x11:event-h e)))
               nil)
      (:error (format t "~&X11 error ~a~%" (lol.x11:event-detail e)) nil)
      (t nil))))

(defun host-cycle ()
  "One turn of the X11 host's loop (installed as *WORLD-CYCLE*, so an overlay that waits
   turns it too): repair and present; wait for an X event or the earliest step
   deadline; route what came, and whatever else is already pending; step. -> :QUIT
   when the display went away or the environment asked to end."
  (when (lol.morph:world-repair *world* *host-canvas*)
    ;; X takes the whole canvas (PutImage in bands): on the host that is cheap enough
    ;; not to send rects
    (funcall *host-present* *host-canvas*))
  (let ((e (funcall *host-next-event* (lol.morph:world-step-timeout *world*))))
    (loop while e
          do (when (eq e :closed) (return-from host-cycle :quit))
             (dolist (we (x11-world-events e))
               (when (or (eq :quit (lol.morph:world-dispatch *world* we)) *world-quit*)
                 (return-from host-cycle :quit)))
             (setf e (funcall *host-next-event* 0))))
  (lol.morph:world-step *world*)
  nil)

(defun x11-next-event-fn (d)
  "*HOST-NEXT-EVENT* over display D. lol.x11 can only block or not, so a wait with a
   deadline (something steps) polls every few ms — the host's dev loop, not the guest's
   poll(2)."
  (lambda (wait-ms)
    (flet ((one (wait)
             (let ((e (lol.x11:next-event d :wait wait)))
               (cond (e e) (wait :closed) (t nil)))))
      (cond ((minusp wait-ms) (one t))
            ((zerop wait-ms) (one nil))
            (t (let ((deadline (+ (get-internal-real-time)
                                  (round (* wait-ms internal-time-units-per-second) 1000))))
                 (loop (let ((e (one nil)))
                         (when e (return e))
                         (when (>= (get-internal-real-time) deadline) (return nil))
                         (sleep 0.005)))))))))

(defun install-host-backend (w h)
  "The world, the overlays' hooks and the loop for a W x H host screen."
  (setf *host-canvas* (lol.canvas:make-canvas w h)
        *world* (make-desk w h)
        *value-prompt* 'run-value-prompt     ; the shell asks; an overlay reads
        *chooser* 'run-chooser               ; ... and shows its menus and halos
        *world-cycle* 'host-cycle            ; what a waiting overlay turns
        *world-quit* nil))

(unless (and (boundp '*run-browser-library*) (symbol-value '*run-browser-library*))
  ;; the font: $LOL_FONT, else the host's DejaVu Sans Mono, else the committed bitmap —
  ;; resizable at run time with SuperL-= / SuperL-- / SuperL-0 (doc/fonts.org)
  (set-browser-font
   (open-browser-font (list (sb-ext:posix-getenv "LOL_FONT")
                            "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf")
                      (merge-pathnames "cozette.lolf"
                                       (make-pathname :directory
                                                      (pathname-directory *load-truename*)))))
  (let* ((w 1000) (h 720)
         (d (lol.x11:open-display w h :title "lol - code browser")))
    (open-environment (find-package :cl-user))           ; browser + Workspace tabs
    (format t "~&Code browser open. up/down move · Enter drill · Backspace back · SuperL-q quits (q is back).~%")
    (format t "Try: drill DRAW-ALIEN for its source, back out, drill a class, inspect a value.~%")
    (finish-output)
    (setf *host-next-event* (x11-next-event-fn d)
          *host-present* (lambda (canvas) (lol.x11:present d canvas))
          *host-decode* (lambda (code state) (lol.x11:decode-key d code state)))
    (install-host-backend w h)
    (unwind-protect
         (loop until (or (eq :quit (host-cycle)) *world-quit*))
      (setf *world-cycle* nil)
      (lol.x11:close-display d)))
  (format t "~&bye.~%"))
