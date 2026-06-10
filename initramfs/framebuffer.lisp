;;;; framebuffer.lisp — blit the Land-of-Lisp alien onto /dev/fb0.
;;;;
;;;; The framebuffer is just memory: /dev/fb0 maps pixel (x,y) to byte offset
;;;; y*stride + x*4 (we assume 32 bpp). We read the live geometry from sysfs,
;;;; then copy our sprite into the top-right corner. Transparency is honoured
;;;; by reading each background row first and only overwriting opaque pixels.
;;;; See framebuffer.org for the efifb/fbcon background.

(defparameter +fb-dev+      "/dev/fb0")
(defparameter +alien-path+  "/alien.rgba")   ; raw RGBA, top-to-bottom
(defparameter +alien-width+  256)
(defparameter +alien-height+ 150)

(defun read-fb-geometry ()
  "Return (values xres yres stride-bytes bpp) from /sys/class/graphics/fb0.
   virtual_size is 'W,H'; stride and bits_per_pixel are plain integers."
  (flet ((line (path) (with-open-file (s path) (read-line s))))
    (let* ((vs    (line "/sys/class/graphics/fb0/virtual_size"))
           (comma (position #\, vs)))
      (values (parse-integer vs :end comma)
              (parse-integer vs :start (1+ comma) :junk-allowed t)
              (parse-integer (line "/sys/class/graphics/fb0/stride")        :junk-allowed t)
              (parse-integer (line "/sys/class/graphics/fb0/bits_per_pixel") :junk-allowed t)))))

(defun draw-alien (&key (margin 16) announce)
  "Blit the alien sprite into the top-right corner of /dev/fb0.
   Pure userland: the framebuffer is memory we seek into and write.
   :announce t prints a status line (we stay silent on routine menu redraws)."
  (handler-case
      (multiple-value-bind (xres yres stride bpp) (read-fb-geometry)
        (unless (= bpp 32)
          (format t "~&framebuffer is ~a bpp; this demo assumes 32 — skipping.~%" bpp)
          (return-from draw-alien))
        (let* ((bypp 4)
               (x0   (max 0 (- xres +alien-width+ margin)))
               (y0   margin)
               (rows (min +alien-height+ (- yres y0)))   ; clamp to screen height
               (rgba (make-array (* +alien-width+ +alien-height+ bypp)
                                 :element-type '(unsigned-byte 8)))
               (row  (make-array (* +alien-width+ bypp)
                                 :element-type '(unsigned-byte 8))))
          (with-open-file (a +alien-path+ :element-type '(unsigned-byte 8))
            (read-sequence rgba a))
          (with-open-file (fb +fb-dev+ :direction :io
                                       :element-type '(unsigned-byte 8)
                                       :if-exists :overwrite)
            (dotimes (y rows)
              (let ((dst (+ (* (+ y0 y) stride) (* x0 bypp))))
                (file-position fb dst)
                (read-sequence row fb)               ; keep the existing background
                (dotimes (x +alien-width+)
                  (let* ((si (* (+ (* y +alien-width+) x) 4))
                         (alpha (aref rgba (+ si 3))))
                    (when (>= alpha 128)             ; opaque enough -> paint it
                      (let ((di (* x bypp)))
                        ;; efifb is commonly BGRX: bytes are Blue,Green,Red,pad.
                        ;; If colours look swapped, exchange these b/r lines.
                        (setf (aref row di)       (aref rgba (+ si 2))   ; B
                              (aref row (+ di 1)) (aref rgba (+ si 1))   ; G
                              (aref row (+ di 2)) (aref rgba si))))))    ; R
                (file-position fb dst)
                (write-sequence row fb)))
            (finish-output fb))
          (when announce
            (format t "~&alien drawn at (~a,~a) on a ~ax~a framebuffer.~%"
                    x0 y0 xres yres)
            (finish-output))))
    (serious-condition (c)
      (format t "~&couldn't draw the alien: ~a~%" c) (finish-output))))
