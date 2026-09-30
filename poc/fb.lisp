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
  (:export #:fb-geometry #:present-fb #:present-fb-rect #:with-graphics-console))

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
;;;
;;; Presenting is a copy of the canvas's own memory into /dev/fb0 — no conversion,
;;; because the canvas is already BGRX (canvas.lisp's header). The first version of
;;; this function did not trust that: it unpacked every pixel into four bytes of a
;;; row buffer, re-read three sysfs files and re-opened /dev/fb0 on every frame, then
;;; did a seek + write per row. Measured in the guest at 1280x800 (tag #24, averaged
;;; over 50 frames), that was 4.0 ms a frame under KVM and 47.7 ms under QEMU's TCG —
;;; more than drawing the whole browser (2.2 ms and 18.8 ms). One pwrite straight from
;;; the pinned pixel vector is about 1 ms and 2 ms. (doc/framebuffer.org §9.4.)
;;;
;;; Two pieces make that possible:
;;;
;;; - PWRITE(2) = write at an explicit file offset, so there is no seek and no shared
;;;   file position to keep track of. The kernel's fb_io_write
;;;   (drivers/video/fbdev/core/fb_io_fops.c) copies a page at a time into the
;;;   framebuffer and advances by exactly what it copied, so one call writes it all.
;;;
;;; - SB-SYS:WITH-PINNED-OBJECTS + VECTOR-SAP = "the address of this Lisp vector's
;;;   data, and promise the garbage collector will not move it meanwhile". SBCL's GC
;;;   moves objects; the kernel reads the buffer during the syscall, so the vector has
;;;   to stay put for exactly that long.
;;;
;;; The fd and the geometry are opened/read ONCE and kept in *FB*. The one trap there
;;; is the saved image: a Lisp variable survives save-lisp-and-die but a file
;;; descriptor does not, so an init hook forgets the cache at startup.

(sb-alien:define-alien-routine ("pwrite" %pwrite) sb-alien:long
  (fd sb-alien:int) (buf sb-sys:system-area-pointer)
  (count sb-alien:unsigned-long) (offset sb-alien:long))

(defconstant +o-rdwr+    2)                 ; <asm-generic/fcntl.h>: 1 << 1
(defconstant +o-cloexec+ #x80000)           ; 1 << 19 — a spawned worker must not inherit it

(defvar *fb* nil
  "The open framebuffer as (FD XRES YRES STRIDE), or NIL until first use.")

(pushnew (lambda () (setf *fb* nil)) sb-ext:*init-hooks*)   ; an fd does not survive a save

(defun fb-target ()
  "The cached (FD XRES YRES STRIDE), opening /dev/fb0 on first use. NIL (after saying
   why) when there is no usable 32-bpp framebuffer."
  (or *fb*
      (multiple-value-bind (xres yres stride bpp) (fb-geometry)
        (if (/= bpp 32)
            (progn (format t "~&framebuffer is ~a bpp; this POC needs 32.~%" bpp) nil)
            (let ((fd (%open "/dev/fb0" (logior +o-rdwr+ +o-cloexec+))))
              (when (>= fd 0)
                (setf *fb* (list fd xres yres stride))))))))

(defun blit-fb (px pitch sx sy w h dx dy)
  "Copy the W x H rectangle at SX,SY of pixel vector PX (PITCH pixels per row) to
   screen position DX,DY, clipped to the screen. One pwrite per row — or ONE for the
   whole rectangle when both sides are the same contiguous run of rows, which is the
   full-frame case."
  (declare (type (simple-array (unsigned-byte 32) (*)) px)
           (fixnum pitch sx sy w h dx dy))
  (destructuring-bind (&optional fd xres yres stride) (fb-target)
    (unless fd (return-from blit-fb nil))
    (let ((w (min w (- xres dx))) (h (min h (- yres dy))))   ; clip right/bottom
      (declare (fixnum w h))
      (when (and (plusp w) (plusp h) (>= dx 0) (>= dy 0))
        (sb-sys:with-pinned-objects (px)
          (let ((sap (sb-sys:vector-sap px)))
            (flet ((put (src-px dst-byte bytes)
                     (declare (fixnum src-px dst-byte bytes))
                     (when (< (%pwrite fd (sb-sys:sap+ sap (* 4 src-px)) bytes dst-byte) 0)
                       (setf *fb* nil)             ; stale fd: reopen next frame
                       (return-from blit-fb nil))))
              (if (and (= sx dx 0) (= w pitch) (= stride (* 4 pitch)))
                  (put (* sy pitch) (* dy stride) (* 4 w h))
                  (dotimes (row h)
                    (put (+ (* (+ sy row) pitch) sx)
                         (+ (* (+ dy row) stride) (* 4 dx))
                         (* 4 w)))))))
        t))))

(defun present-fb (canvas &key (x0 0) (y0 0))
  "Show CANVAS on /dev/fb0 with its top-left at X0,Y0."
  (let ((w (lol.canvas:canvas-w canvas)))
    (blit-fb (lol.canvas:canvas-pixels canvas) w 0 0 w (lol.canvas:canvas-h canvas) x0 y0)))

(defun present-fb-rect (canvas x y w h)
  "Show only the W x H rectangle of a screen-sized CANVAS at X,Y, in place — what a
   moving mouse pointer needs, instead of the whole frame."
  (blit-fb (lol.canvas:canvas-pixels canvas) (lol.canvas:canvas-w canvas)
           (max x 0) (max y 0) (+ w (min x 0)) (+ h (min y 0)) (max x 0) (max y 0)))
