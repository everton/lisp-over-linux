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
;;;; to that level · PageUp/PageDown and the mouse WHEEL scroll · q quits.
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
      (load (f "../initramfs/registry.lisp"))          ; defines load-recording (plain load)
      ;; everything else is loaded AND source-captured, so it is all browsable:
      (dolist (m '("../initramfs/model.lisp" "../initramfs/present.lisp"
                   "canvas.lisp" "x11.lisp" "textview.lisp" "widgets.lisp"
                   "clipboard.lisp" "../initramfs/sysinfo.lisp"
                   "inflate.lisp" "image.lisp"
                   "../initramfs/files.lisp" "../initramfs/file-views.lisp"
                   "shell.lisp" "file-manager.lisp"
                   "../initramfs/framebuffer.lisp" "../initramfs/meminfo.lisp"))
        (load-recording (f m))))))

(set-browser-font
 (lol.canvas:load-font
  (merge-pathnames "cozette.lolf"
                   (make-pathname :directory (pathname-directory *load-truename*)))))

(let* ((w 1000) (h 720)
       (canvas (lol.canvas:make-canvas w h))
       (d (lol.x11:open-display w h :title "lol - code browser")))
  (open-environment (find-package :cl-user))           ; browser + Workspace tabs
  (format t "~&Code browser open. up/down move · Enter drill · Backspace back · q quits.~%")
  (format t "Try: drill DRAW-ALIEN for its source, back out, drill a class, inspect a value.~%")
  (finish-output)
  (let ((quit nil))
    (flet ((handle (e)
             (case (lol.x11:event-kind e)
               (:key-press
                (let ((key (lol.x11:decode-key d (lol.x11:event-detail e)
                                               (lol.x11:event-state e))))
                  (when (eq :quit (browser-key key (lol.x11:event-state e)))
                    (setf quit t))))
               ;; X has no scroll axis: the wheel is BUTTONS 4 (up) and 5 (down), one
               ;; press per notch — the host's counterpart of the guest's evdev
               ;; REL_WHEEL record (doc/input.org §7). Button 1 is a real click.
               (:button-press
                (case (lol.x11:event-detail e)
                  (1 (browser-click (lol.x11:event-x e) (lol.x11:event-y e)
                                    (lol.canvas:ctrl-p (lol.x11:event-state e))
                                    (lol.canvas:super-p (lol.x11:event-state e))))
                  ;; 2 is the middle button (paste PRIMARY / the halo), 3 the right
                  ;; (the context menu — which says so in the status line here, as
                  ;; this host backend installs no *CHOOSER*)
                  (2 (browser-middle-click (lol.x11:event-x e) (lol.x11:event-y e)
                                           (lol.canvas:super-p (lol.x11:event-state e))))
                  (3 (browser-context-menu (lol.x11:event-x e) (lol.x11:event-y e)))
                  (4 (browser-scroll (- +wheel-lines+)))
                  (5 (browser-scroll +wheel-lines+))))
               ;; motion with button 1 held (Button1Mask, #x100, in the state) is a
               ;; drag: it extends the selection the press started (BROWSER-DRAG)
               (:button-release
                (when (= 1 (lol.x11:event-detail e)) (browser-drag-end)))
               (:motion
                (when (logtest #x100 (lol.x11:event-state e))
                  (browser-drag (lol.x11:event-x e) (lol.x11:event-y e))))
               (:error (format t "~&X11 error ~a~%" (lol.x11:event-detail e)))
               (t nil))))
      (unwind-protect
           (loop
             (draw-browser canvas w h)
             (lol.x11:present d canvas)
             (let ((e (lol.x11:next-event d :wait t)))
               (unless e (return))
               (handle e))
             (loop for e = (lol.x11:next-event d :wait nil) while e do (handle e))
             (when quit (return)))
        (lol.x11:close-display d))))
  (format t "~&bye.~%"))
