;;;; ppm2png.lisp — convert a binary PPM to a PNG.
;;;;
;;;;     sbcl --script host-client/ppm2png.lisp IN.ppm OUT.png
;;;;
;;;; QEMU's screendump writes PPM; the docs want PNG. The conversion itself is
;;;; host-client/png.lisp, which includes a DEFLATE implementation because that is
;;;; the only part of PNG that is real work. This file is just the command line.

(let ((here (make-pathname :directory (pathname-directory *load-truename*))))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "png.lisp" here))))

(sb-ext:exit
 :code (handler-case
           (destructuring-bind (&optional ppm png) (rest sb-ext:*posix-argv*)
             (cond ((not (and ppm png))
                    (format *error-output*
                            "~&usage: sbcl --script host-client/ppm2png.lisp IN.ppm OUT.png~%")
                    2)
                   (t (multiple-value-bind (out w h) (lol.png:ppm->png ppm png)
                        (format t "~&  PNG: ~a (~dx~d)~%" out w h)
                        0))))
         (serious-condition (c)
           (format *error-output* "~&ppm2png: ~a~%" c)
           1)))
