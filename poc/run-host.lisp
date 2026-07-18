;;;; run-host.lisp — run the POC on the HOST, in a real X11 window.
;;;;
;;;;   sbcl --script poc/run-host.lisp          (needs $DISPLAY and $XAUTHORITY)
;;;;
;;;; This is the dev loop the plan is built around: the browser in a window on
;;;; your desktop, from a plain sbcl, with your keyboard and mouse — no QEMU, no
;;;; kernel, no reboot. The SAME canvas.lisp/scene.lisp then run unchanged in the
;;;; guest on /dev/fb0 (poc/run-guest.lisp).

(require :sb-bsd-sockets)
(require :sb-posix)

(let ((here (make-pathname :directory (pathname-directory *load-truename*))))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "canvas.lisp" here))
    (load (merge-pathnames "x11.lisp"    here))
    (load (merge-pathnames "scene.lisp"  here))))

(defun read-blob (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(let* ((here  (make-pathname :directory (pathname-directory *load-truename*)))
       (root  (merge-pathnames "../" here))
       (w 1000) (h 640)
       (font  (lol.canvas:load-psf (merge-pathnames "font16.psf" here)))
       (alien (read-blob (merge-pathnames "initramfs/alien.rgba" root)))
       (ui    (lol.scene:make-ui :w w :h h :font font :alien alien))
       (canvas (lol.canvas:make-canvas w h))
       ;; ASCII only: WM_NAME here is a STRING property, one byte per character.
       (d     (lol.x11:open-display w h :title "lisp-over-linux - code browser (POC)")))
  (format t "~&X11 window open. Arrows/click to browse, q or Esc to quit.~%")
  (finish-output)
  (let ((quit nil))
    (flet ((handle (e)
             (case (lol.x11:event-kind e)
               (:key-press
                (when (eq :quit (lol.scene:ui-key ui (lol.x11:event-detail e)))
                  (setf quit t)))
               (:button-press
                (lol.scene:ui-click ui (lol.x11:event-x e) (lol.x11:event-y e)))
               (:error
                (format t "~&X11 error code ~a~%" (lol.x11:event-detail e)))
               (t nil))))
      (unwind-protect
           (loop
             (lol.scene:draw-ui ui canvas)
             (lol.x11:present d canvas)
             ;; Block for one event, then drain anything else already queued
             ;; before repainting — otherwise a mouse drag floods us with frames.
             (let ((e (lol.x11:next-event d :wait t)))
               (unless e (return))
               (handle e))
             (loop for e = (lol.x11:next-event d :wait nil)
                   while e do (handle e))
             (when quit (return)))
        (lol.x11:close-display d))))
  (format t "~&bye.~%"))
