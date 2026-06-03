;;;; supervisor.lisp — a tiny, didactic init/supervisor in Common Lisp.
;;;;
;;;; Roles chosen by argv:
;;;;   (no args)   => interactive SUPERVISOR menu  (this is PID 1)
;;;;   "worker" N  => a short-lived WORKER process
;;;;
;;;; The supervisor menu lets you run a Lisp REPL, spawn a worker, or power
;;;; the machine off. PID 1 must NEVER return, so every path either loops or
;;;; powers off, and errors are caught so a typo can't crash init.

(defun worker-main (id)
  "A throwaway child: prints who it is, does a little Lisp, then exits."
  (format t "    [worker ~a] hello — my pid is ~a~%" id (sb-unix:unix-getpid))
  (finish-output)
  (let ((sum (loop for i from 1 to 1000000 summing i)))
    (format t "    [worker ~a] computed (sum 1..1,000,000) = ~:d~%" id sum))
  (finish-output)
  (sb-ext:exit :code (mod (parse-integer id) 256)))

(defun spawn-worker (id)
  "Fork+exec another copy of ourselves as a worker (run-program reaps it)."
  (sb-ext:run-program "/sbin/lisp" (list "worker" (princ-to-string id))
                      :wait nil :input t :output t :error t))

(defun power-off ()
  "Sync filesystems and ask the kernel to power off via reboot(2).
   Needs PID-1/root privileges; returns to the caller if it is refused."
  (format t "~&Syncing disks and powering off...~%")
  (finish-output)
  (sb-alien:alien-funcall (sb-alien:extern-alien "sync" (function sb-alien:void)))
  (sleep 1)
  ;; glibc reboot(howto); #x4321fedc = RB_POWER_OFF (ACPI power off).
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "reboot" (function sb-alien:int sb-alien:int))
   #x4321fedc)
  ;; only reached if the power-off was refused
  (format t "~&Power-off failed (need privileges?). Back to the menu.~%")
  (finish-output))

(defun run-repl ()
  "A minimal read-eval-print loop. :quit (or EOF) returns to the menu.
   Every read/eval is guarded so a bad form cannot kill PID 1."
  (let ((eof (list :eof)))                 ; unique sentinel, distinct from any input
    (format t "~&~%Lisp REPL — type :quit to return to the menu.~%")
    (loop
      (format t "~&lisp> ") (finish-output)
      (let ((form (handler-case (read *standard-input* nil eof)
                    (serious-condition (c)
                      (format t "~&read error: ~a~%" c) nil))))
        (cond
          ((or (eq form eof) (eq form :quit)) (return))
          ((null form))                     ; read error already reported; re-prompt
          (t (handler-case
                 (format t "~&=> ~s~%" (eval form))
               (serious-condition (c) (format t "~&error: ~a~%" c)))))))))

(defun show-input-devices ()
  "Diagnostic: list the input devices the kernel currently sees (from
   /proc/bus/input/devices). Tells us whether a keyboard driver bound."
  (format t "~&Input devices detected by the kernel:~%")
  (let ((any nil))
    (ignore-errors
      (with-open-file (s "/proc/bus/input/devices" :if-does-not-exist nil)
        (when s
          (loop for line = (read-line s nil nil) while line do
            (when (and (> (length line) 2) (string= (subseq line 0 2) "N:"))
              (setf any t)
              (format t "   ~a~%" line))))))
    (unless any
      (format t "   (none found — no keyboard driver bound)~%")))
  (finish-output))

;;; ---- framebuffer graphics: blit the Land-of-Lisp alien ------------------
;;;
;;; The framebuffer is just memory: /dev/fb0 maps pixel (x,y) to byte offset
;;; y*stride + x*4 (we assume 32 bpp). We read the live geometry from sysfs,
;;; then copy our sprite into the top-right corner. Transparency is honoured
;;; by reading each background row first and only overwriting opaque pixels.

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

(defun draw-alien (&optional (margin 16))
  "Blit the alien sprite into the top-right corner of /dev/fb0.
   Pure userland: the framebuffer is memory we seek into and write."
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
          (format t "~&alien drawn at (~a,~a) on a ~ax~a framebuffer.~%"
                  x0 y0 xres yres)
          (finish-output)))
    (serious-condition (c)
      (format t "~&couldn't draw the alien: ~a~%" c) (finish-output))))

(defun print-menu ()
  (format t "~&~%========= micro-lisp supervisor =========~%")
  (format t "  r) run a Lisp REPL~%")
  (format t "  w) spawn a worker process~%")
  (format t "  a) draw the Land-of-Lisp alien~%")
  (format t "  s) shut down (power off)~%")
  (format t "=========================================~%")
  (format t "choice> ") (finish-output))

(defun supervisor-main ()
  "PID 1: an interactive menu loop. Never returns."
  (format t "~%Supervisor up — I am PID ~a, SBCL ~a.~%"
          (sb-unix:unix-getpid) (lisp-implementation-version))
  (sleep 2)                       ; give USB a moment to enumerate
  (show-input-devices)            ; diagnostic: which keyboard(s) bound?
  (draw-alien)                    ; greet the user with the LoL alien
  (let ((worker-id 0))
    (loop
      (handler-case
          (progn
            (print-menu)
            (let* ((line   (read-line *standard-input* nil :eof))
                   (choice (and (stringp line)
                                (string-trim '(#\Space #\Tab #\Return) line))))
              (cond
                ((eq line :eof) (sleep 1))            ; no input source: don't spin
                ((string-equal choice "r") (run-repl))
                ((string-equal choice "w")
                 (incf worker-id)
                 (format t "~&spawning worker ~a~%" worker-id)
                 (spawn-worker worker-id))
                ((string-equal choice "a") (draw-alien))
                ((string-equal choice "s") (power-off))
                ((string= choice ""))                 ; bare Enter: just redraw
                (t (format t "~&unknown choice: ~a~%" choice)))))
        (serious-condition (c)
          (format t "~&menu error: ~a~%" c) (finish-output))))))

(defun init-toplevel ()
  (let ((args (rest sb-ext:*posix-argv*)))
    (if (and args (string= (first args) "worker"))
        (worker-main (second args))
        (supervisor-main))))
