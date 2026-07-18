;;;; fb.lisp — the /dev/fb0 backend. The other half of the proof.
;;;;
;;;; Note how little there is here. Presenting to the framebuffer is a memcpy:
;;;; the canvas is already #x00RRGGBB words = B,G,R,pad bytes = exactly BGRX,
;;;; exactly what efifb scans out. (framebuffer.org §1, §8.)
;;;;
;;;; The one wrinkle is fbcon: it shares /dev/fb0 with us and its cursor/printk
;;;; will scribble over the picture. KDSETMODE -> KD_GRAPHICS tells the kernel
;;;; console to stop rendering — it does NOT stop key delivery, so the existing
;;;; raw-mode keyboard keeps working untouched.

(defpackage :lol.fb
  (:use :cl)
  (:export #:fb-geometry #:present-fb #:with-graphics-console))

(in-package :lol.fb)

(defun fb-geometry ()
  "(values xres yres stride bpp) from sysfs — never assume stride = w*bpp."
  (flet ((line (p) (with-open-file (s p) (read-line s))))
    (let* ((vs (line "/sys/class/graphics/fb0/virtual_size"))
           (comma (position #\, vs)))
      (values (parse-integer vs :end comma)
              (parse-integer vs :start (1+ comma) :junk-allowed t)
              (parse-integer (line "/sys/class/graphics/fb0/stride") :junk-allowed t)
              (parse-integer (line "/sys/class/graphics/fb0/bits_per_pixel")
                             :junk-allowed t)))))

;;; ---- KD_GRAPHICS: stop fbcon drawing over us -----------------------------

(sb-alien:define-alien-routine ("open" %open) sb-alien:int
  (path sb-alien:c-string) (flags sb-alien:int))
(sb-alien:define-alien-routine ("close" %close) sb-alien:int (fd sb-alien:int))
(sb-alien:define-alien-routine ("ioctl" %ioctl) sb-alien:int
  (fd sb-alien:int) (request sb-alien:unsigned-long) (arg sb-alien:long))

(defconstant +kdsetmode+ #x4B3A)
(defconstant +kd-text+     0)
(defconstant +kd-graphics+ 1)

(defmacro with-graphics-console (&body body)
  "Put the VT in graphics mode for BODY; ALWAYS restore text mode on the way out
   (unwind-protect), or you are left staring at a dead console."
  `(let ((fd (%open "/dev/tty0" 2)))          ; O_RDWR
     (unwind-protect
          (progn
            (when (>= fd 0) (%ioctl fd +kdsetmode+ +kd-graphics+))
            ,@body)
       (when (>= fd 0)
         (%ioctl fd +kdsetmode+ +kd-text+)
         (%close fd)))))

;;; ---- present -------------------------------------------------------------

(defun present-fb (canvas &key (x0 0) (y0 0))
  "Blit CANVAS into /dev/fb0 at X0,Y0. Row by row, because stride may exceed
   width*4. This is the same file-position + write-sequence path draw-alien
   already uses — proven to work on this machine."
  (multiple-value-bind (xres yres stride bpp) (fb-geometry)
    (declare (ignore xres))
    (unless (= bpp 32)
      (format t "~&framebuffer is ~a bpp; this POC needs 32.~%" bpp)
      (return-from present-fb nil))
    (let* ((w (lol.canvas:canvas-w canvas))
           (h (lol.canvas:canvas-h canvas))
           (px (lol.canvas:canvas-pixels canvas))
           (row (make-array (* w 4) :element-type '(unsigned-byte 8))))
      (with-open-file (fb "/dev/fb0" :direction :io
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :overwrite)
        (dotimes (y (min h (- yres y0)))
          (let ((base (* y w)))
            (dotimes (x w)
              (let ((p (aref px (+ base x))) (i (* x 4)))
                (setf (aref row i)       (logand p #xff)           ; B
                      (aref row (+ i 1)) (logand (ash p -8) #xff)  ; G
                      (aref row (+ i 2)) (logand (ash p -16) #xff) ; R
                      (aref row (+ i 3)) 0))))                     ; X
          (file-position fb (+ (* (+ y0 y) stride) (* x0 4)))
          (write-sequence row fb))
        (finish-output fb))
      t)))
