;;;; screenshot.lisp — grab the guest's screen through QMP and save it as a PNG.
;;;;
;;;;     sbcl --script host-client/screenshot.lisp QMP-SOCKET OUT.ppm [OUT.png]
;;;;
;;;; One command replacing the two blocks of embedded Python that ./build.sh and
;;;; poc/build-poc.sh each carried: one to drive QMP's screendump, one to turn the
;;;; resulting PPM into a PNG with zlib. Both are Lisp now — qmp.lisp and png.lisp,
;;;; right here in host-client/ — so neither script needs python3 any more.
;;;;
;;;; The PPM is kept as well as the PNG: QEMU writes it, and it is the format the
;;;; rendering harness (poc/render-frame.lisp) also produces, so it is useful to have
;;;; around for pixel comparisons.

(require :sb-bsd-sockets)

(let ((here (make-pathname :directory (pathname-directory *load-truename*))))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "qmp.lisp" here))
    (load (merge-pathnames "png.lisp" here))))

(defun main (args)
  (destructuring-bind (&optional socket ppm png) args
    (unless (and socket ppm)
      (format *error-output*
              "~&usage: sbcl --script host-client/screenshot.lisp QMP-SOCKET OUT.ppm [OUT.png]~%")
      (return-from main 2))
    ;; QEMU resolves the screendump path itself, so give it an absolute one — the
    ;; monitor's working directory is not ours to assume.
    (let ((ppm-path (namestring (merge-pathnames ppm))))
      (lol.qmp:with-qmp (socket)
        (lol.qmp:screendump ppm-path))
      (unless (probe-file ppm-path)
        (format *error-output* "~&screenshot: QEMU wrote no ~a~%" ppm-path)
        (return-from main 1))
      (when png
        (multiple-value-bind (out w h) (lol.png:ppm->png ppm-path png)
          (format t "~&  screenshot: ~a (~dx~d)~%" out w h)))
      0)))

(sb-ext:exit :code (handler-case (main (rest sb-ext:*posix-argv*))
                     (serious-condition (c)
                       (format *error-output* "~&screenshot: ~a~%" c)
                       1)))
