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
;;;; Not available here: the Spotter, the cheatsheet and the debugger are summoned
;;;; through fb-browser.lisp, which this runner does not load (it wants /dev/fb0).
;;;; The shell reaches them via (fboundp …) and does nothing if they are absent.

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
      ;; wants hardware) plus x11.lisp; it had drifted behind the shell, which since
      ;; reads LOL.EXAMPLES symbols, and stopped loading — keep the two in step.
      (dolist (m '("../initramfs/sbcl-source.lisp" "../initramfs/model.lisp"
                   "../initramfs/present.lisp" "../initramfs/tour.lisp"
                   "../initramfs/examples.lisp" "../initramfs/stage.lisp"
                   "ttf.lisp" "canvas.lisp" "x11.lisp" "textview.lisp" "widgets.lisp" "morph.lisp"
                   "../initramfs/ansi.lisp" "../initramfs/line-editor.lisp"
                   "clipboard.lisp" "../initramfs/sysinfo.lisp"
                   "inflate.lisp" "image.lisp"
                   "../initramfs/files.lisp" "terminal.lisp" "../initramfs/pty.lisp"
                   "../initramfs/file-views.lisp"
                   "shell.lisp" "file-manager.lisp" "../initramfs/runs.lisp"
                   "../initramfs/containers.lisp" "../initramfs/json.lisp"
                   "../initramfs/agent-wire.lisp" "../initramfs/agent.lisp"
                   "../initramfs/agent-ui.lisp" "terminal-pane.lisp" "morphic.lisp"
                   "../initramfs/framebuffer.lisp" "../initramfs/meminfo.lisp"))
        (load-recording (f m))))))

;; the font: $LOL_FONT, else the host's DejaVu Sans Mono, else the committed bitmap —
;; resizable at run time with SuperL-= / SuperL-- / SuperL-0 (doc/fonts.org)
(set-browser-font
 (open-browser-font (list (sb-ext:posix-getenv "LOL_FONT")
                          "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf")
                    (merge-pathnames "cozette.lolf"
                                     (make-pathname :directory
                                                    (pathname-directory *load-truename*)))))

(let* ((w 1000) (h 720)
       (canvas (lol.canvas:make-canvas w h))
       (d (lol.x11:open-display w h :title "lol - code browser")))
  (open-environment (find-package :cl-user))           ; browser + Workspace tabs
  (format t "~&Code browser open. up/down move · Enter drill · Backspace back · SuperL-q quits (q is back).~%")
  (format t "Try: drill DRAW-ALIEN for its source, back out, drill a class, inspect a value.~%")
  (finish-output)
  ;; The world (poc/morph.lisp): the tiled shell is one morph in it, and this loop
  ;; only turns X events into the world's events and sends what it redrew — the same
  ;; shape as the guest's loop (initramfs/fb-browser.lisp), so the host backend
  ;; inherits everything the world does (doc/morphic.org §8, "the X11 dev backend").
  (setf *world* (make-desk w h))
  (let ((quit nil))
    (flet ((send (kind e &rest args)
             (when (eq :quit (lol.morph:world-dispatch
                              *world* (apply #'lol.morph:make-event :kind kind
                                             :x (lol.x11:event-x e) :y (lol.x11:event-y e)
                                             :state (lol.x11:event-state e) args)))
               (setf quit t))))
      (flet ((handle (e)
               (case (lol.x11:event-kind e)
                 (:key-press
                  (send :key e :key (lol.x11:decode-key d (lol.x11:event-detail e)
                                                        (lol.x11:event-state e))))
                 ;; X has no scroll axis: the wheel is BUTTONS 4 (up) and 5 (down), one
                 ;; press per notch — the host's counterpart of the guest's evdev
                 ;; REL_WHEEL record (doc/input.org §7). Button 1 is a real click.
                 (:button-press
                  (case (lol.x11:event-detail e)
                    (1 (send :down e))
                    ;; 2 is the middle button (paste PRIMARY / the halo), 3 the right
                    ;; (the context menu — which says so in the status line here, as
                    ;; this host backend installs no *CHOOSER*)
                    (2 (send :middle e))
                    (3 (send :right e))
                    (4 (send :wheel e :notches 1))
                    (5 (send :wheel e :notches -1))))
                 ;; motion with button 1 held (Button1Mask, #x100, in the state) is a
                 ;; drag: it extends the selection the press started, or carries a window
                 (:button-release
                  (when (= 1 (lol.x11:event-detail e)) (send :up e)))
                 (:motion
                  (when (logtest #x100 (lol.x11:event-state e)) (send :drag e)))
                 (:error (format t "~&X11 error ~a~%" (lol.x11:event-detail e)))
                 (t nil))))
        (unwind-protect
             (loop
               ;; repair what the events damaged; X takes the whole canvas (PutImage
               ;; in bands), which on the host is cheap enough not to bother with rects
               (when (lol.morph:world-repair *world* canvas)
                 (lol.x11:present d canvas))
               (let ((e (lol.x11:next-event d :wait t)))
                 (unless e (return))
                 (handle e))
               (loop for e = (lol.x11:next-event d :wait nil) while e do (handle e))
               (when quit (return)))
          (lol.x11:close-display d)))))
  (format t "~&bye.~%"))
