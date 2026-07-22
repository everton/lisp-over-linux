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
;;;; to that level · q / Esc quits.

(require :sb-bsd-sockets) (require :sb-introspect) (require :sb-posix)

(let* ((here (make-pathname :directory (pathname-directory *load-truename*))))
  (flet ((f (p) (merge-pathnames p here)))
    (handler-bind ((warning #'muffle-warning))
      (load (f "../initramfs/registry.lisp"))          ; defines load-recording (plain load)
      ;; everything else is loaded AND source-captured, so it is all browsable:
      (dolist (m '("../initramfs/model.lisp" "../initramfs/present.lisp"
                   "canvas.lisp" "x11.lisp" "textview.lisp" "shell.lisp"
                   "../initramfs/framebuffer.lisp" "../initramfs/meminfo.lisp"))
        (load-recording (f m))))))

(setf *bfont*
      (lol.canvas:load-font
       (merge-pathnames "cozette.lolf"
                        (make-pathname :directory (pathname-directory *load-truename*)))))

(let* ((w 1000) (h 720)
       (canvas (lol.canvas:make-canvas w h))
       (d (lol.x11:open-display w h :title "lol - code browser")))
  (trail-root (find-package :cl-user))                 ; root: all recorded definitions
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
               (:button-press
                (when (= 1 (lol.x11:event-detail e))
                  (browser-click (lol.x11:event-x e) (lol.x11:event-y e)
                                 (lol.canvas:ctrl-p (lol.x11:event-state e))
                                 (lol.canvas:super-p (lol.x11:event-state e)))))
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
