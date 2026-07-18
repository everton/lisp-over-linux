;;;; run-guest.lisp — the SAME scene, on the LoL machine's /dev/fb0.
;;;;
;;;; Compare with run-host.lisp. The ONLY difference between them is which
;;;; backend presents the canvas:
;;;;
;;;;    host  :  (lol.x11:present d canvas)      -> a window on your desktop
;;;;    guest :  (lol.fb:present-fb canvas)      -> the bare framebuffer, PID 1
;;;;
;;;; canvas.lisp and scene.lisp are byte-for-byte identical in both. That is the
;;;; whole architectural claim of doc/code-browser.org §1, and this file is the
;;;; half of the proof that runs on the metal.
;;;;
;;;; This is PID 1, so it must never return: we draw, then park forever.

(defun poc-main ()
  (format t "~&~%[poc] lisp-over-linux graphics POC — PID ~a~%" (sb-unix:unix-getpid))
  (finish-output)
  (sleep 1)                            ; let the kernel finish its boot chatter
  (handler-case
      (multiple-value-bind (xres yres stride bpp) (lol.fb:fb-geometry)
        (format t "[poc] framebuffer ~ax~a stride=~a bpp=~a~%" xres yres stride bpp)
        (finish-output)
        (let* ((font   (lol.canvas:load-psf "/font16.psf"))
               (alien  (with-open-file (s "/alien.rgba"
                                          :element-type '(unsigned-byte 8))
                         (let ((v (make-array (file-length s)
                                              :element-type '(unsigned-byte 8))))
                           (read-sequence v s) v)))
               ;; fill the whole panel this time — we own the screen
               (ui     (lol.scene:make-ui :w xres :h yres :font font :alien alien))
               (canvas (lol.canvas:make-canvas xres yres)))
          ;; KD_GRAPHICS stops fbcon scribbling its cursor/printk over us.
          (lol.fb:with-graphics-console
            (lol.scene:draw-ui ui canvas)
            (lol.fb:present-fb canvas)
            (format t "[poc] presented ~ax~a to /dev/fb0~%" xres yres)
            (finish-output)
            ;; PID 1 must never return.
            (loop (sleep 3600)))))
    (serious-condition (c)
      (format t "~&[poc] FAILED: ~a~%" c)
      (finish-output)
      (loop (sleep 3600)))))
