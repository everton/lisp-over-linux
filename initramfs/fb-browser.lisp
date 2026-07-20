;;;; fb-browser.lisp — run the code browser on /dev/fb0 (the guest; no X).
;;;;
;;;; The same shell (poc/shell.lisp) that drives the browser in an X11 window on
;;;; the dev host runs here on the bare framebuffer, launched from the supervisor
;;;; menu. Output is lol.fb:present-fb into /dev/fb0 (under KD_GRAPHICS so fbcon
;;;; stops scribbling); input is BOTH the raw console keyboard AND the evdev mouse,
;;;; multiplexed with poll(2). It returns to the menu when you leave (C-g at the
;;;; root, or Esc).
;;;;
;;;; Keyboard: raw fd 0, signals OFF (so C-c C-c Accepts). Mouse: /dev/input/eventN
;;;; (the pointer, found via /proc/bus/input/devices) — relative (PS/2) or absolute
;;;; (usb-tablet), left button drills. CL-USER, loaded after the toolkit + line-editor.

(in-package :cl-user)

;;; ---- low-level fds via sb-alien (same style as net.lisp / fb.lisp) --------

(sb-alien:define-alien-routine ("open"  %fbb-open)  sb-alien:int
  (path sb-alien:c-string) (flags sb-alien:int))
(sb-alien:define-alien-routine ("read"  %fbb-read)  sb-alien:long
  (fd sb-alien:int) (buf (sb-alien:* sb-alien:unsigned-char)) (n sb-alien:unsigned-long))
(sb-alien:define-alien-routine ("close" %fbb-close) sb-alien:int (fd sb-alien:int))
(sb-alien:define-alien-routine ("poll"  %fbb-poll)  sb-alien:int
  (fds (sb-alien:* sb-alien:unsigned-char)) (nfds sb-alien:unsigned-long) (timeout sb-alien:int))

(defconstant +o-rdonly+   0)
(defconstant +o-nonblock+ #x800)
(defconstant +pollin+     1)

(defun fbb-read-byte (fd)
  "Read one byte from FD; NIL on EOF/error."
  (sb-alien:with-alien ((b (sb-alien:array sb-alien:unsigned-char 1)))
    (let ((n (%fbb-read fd (sb-alien:cast b (sb-alien:* sb-alien:unsigned-char)) 1)))
      (and (= n 1) (sb-alien:deref b 0)))))

(defun fbb-ready-p (fd timeout-ms)
  "True if FD has input within TIMEOUT-MS (poll on one fd)."
  (sb-alien:with-alien ((fds (sb-alien:array sb-alien:unsigned-char 8)))
    (dotimes (i 8) (setf (sb-alien:deref fds i) 0))
    (setf (sb-alien:deref fds 0) (logand fd #xff)
          (sb-alien:deref fds 4) +pollin+)
    (%fbb-poll (sb-alien:cast fds (sb-alien:* sb-alien:unsigned-char)) 1 timeout-ms)
    (logbitp 0 (sb-alien:deref fds 6))))

(defun fbb-wait (kb-fd mouse-fd)
  "Block until KB-FD or MOUSE-FD is readable. Return (values kb-ready mouse-ready)."
  (sb-alien:with-alien ((fds (sb-alien:array sb-alien:unsigned-char 16)))
    (dotimes (i 16) (setf (sb-alien:deref fds i) 0))
    (setf (sb-alien:deref fds 0) (logand kb-fd #xff)
          (sb-alien:deref fds 4) +pollin+)
    (when mouse-fd
      (setf (sb-alien:deref fds 8)  (logand mouse-fd #xff)
            (sb-alien:deref fds 12) +pollin+))
    (%fbb-poll (sb-alien:cast fds (sb-alien:* sb-alien:unsigned-char))
               (if mouse-fd 2 1) -1)
    (values (logbitp 0 (sb-alien:deref fds 6))
            (and mouse-fd (logbitp 0 (sb-alien:deref fds 14))))))

;;; ---- keyboard: raw bytes -> shell keys ----------------------------------

(defun fb-read-key (fd)
  "One keystroke from raw fd FD -> (values KEY STATE) for browser-key. A control
   byte becomes its letter + the Ctrl bit (so C-c C-c, C-g work); ESC[… becomes an
   arrow/page keyword; a lone ESC -> :escape (probed with a short poll so it does
   not block waiting for a sequence that never comes)."
  (let ((c (fbb-read-byte fd)))
    (cond
      ((null c) (values :eof 0))
      ((= c 27)
       (if (fbb-ready-p fd 20)                 ; a real ESC sequence follows?
           (let ((b1 (fbb-read-byte fd)))
             (if (eql b1 91)                    ; '['
                 (let ((b2 (fbb-read-byte fd)))
                   (case b2
                     (65 (values :up 0))    (66 (values :down 0))
                     (67 (values :right 0)) (68 (values :left 0))
                     (72 (values :home 0))  (70 (values :end 0))
                     (53 (fbb-read-byte fd) (values :page-up 0))   ; ESC[5~
                     (54 (fbb-read-byte fd) (values :page-down 0)) ; ESC[6~
                     (t  (values :ignore 0))))
                 (values :ignore 0)))
           (values :escape 0)))
      ((member c '(13 10)) (values :return 0))
      ((member c '(8 127)) (values :backspace 0))
      ((= c 9)             (values :tab 0))
      ((<= 1 c 26)         (values (code-char (+ c 96)) 4))   ; C-a..C-z, bit 2 = Ctrl
      (t                   (values (code-char c) 0)))))

;;; ---- mouse: evdev -> a cursor + clicks ----------------------------------

(defvar *cursor-x* 0) (defvar *cursor-y* 0)

(defun find-mouse-device ()
  "The /dev/input/eventN of a pointer, from /proc/bus/input/devices: the first
   device whose capability mask (the 'B: EV=' line, hex) has EV_REL (bit 2, a
   mouse/touchpad) or EV_ABS (bit 3, a tablet) set. NIL if none.

   We can't just grep the Handlers line for 'mouse': the mousedev module isn't
   loaded here, so a plain PS/2 mouse shows only 'Handlers=eventN' — no 'mouseN'
   token. The EV mask is what actually says 'this is a pointer'."
  (ignore-errors
    (with-open-file (s "/proc/bus/input/devices" :if-does-not-exist nil)
      (when s
        (let ((ev nil) (pointerp nil))
          (flet ((finish ()                       ; end of a device block
                   (when (and ev pointerp)
                     (return-from find-mouse-device
                       (format nil "/dev/input/~a" ev)))
                   (setf ev nil pointerp nil)))
            (loop for line = (read-line s nil nil) do
              (cond
                ((null line) (finish) (return))               ; EOF: flush last block
                ((zerop (length line)) (finish))              ; blank: block boundary
                ((and (>= (length line) 2) (string= (subseq line 0 2) "H:"))
                 (let ((p (search "event" line)))
                   (when p
                     (let ((end (or (position #\Space line :start p) (length line))))
                       (setf ev (subseq line p end))))))
                ((search "EV=" line)
                 (let* ((p (+ (search "EV=" line) 3))
                        (end (or (position #\Space line :start p) (length line)))
                        (mask (ignore-errors
                                (parse-integer line :start p :end end :radix 16))))
                   (when (and mask (or (logbitp 2 mask)        ; EV_REL
                                       (logbitp 3 mask)))      ; EV_ABS
                     (setf pointerp t))))))))))))

(defun open-mouse ()
  (let ((path (find-mouse-device)))
    (when path
      (let ((fd (%fbb-open path (logior +o-rdonly+ +o-nonblock+))))
        (and (>= fd 0) fd)))))

(defun read-mouse (fd xres yres)
  "Drain pending 24-byte input_event records; move *cursor-x/y*; return T on a
   left-button press. Handles EV_ABS (usb-tablet, 0..32767) and EV_REL (PS/2)."
  (let ((clicked nil))
    (sb-alien:with-alien ((e (sb-alien:array sb-alien:unsigned-char 24)))
      (loop
        (let ((n (%fbb-read fd (sb-alien:cast e (sb-alien:* sb-alien:unsigned-char)) 24)))
          (when (< n 24) (return))              ; nothing more (nonblocking)
          (let* ((type (logior (sb-alien:deref e 16) (ash (sb-alien:deref e 17) 8)))
                 (code (logior (sb-alien:deref e 18) (ash (sb-alien:deref e 19) 8)))
                 (v    (logior (sb-alien:deref e 20) (ash (sb-alien:deref e 21) 8)
                               (ash (sb-alien:deref e 22) 16) (ash (sb-alien:deref e 23) 24)))
                 (val  (if (>= v #x80000000) (- v #x100000000) v)))   ; sign-extend s32
            (cond
              ((= type 3)                       ; EV_ABS
               (case code
                 (0 (setf *cursor-x* (min (1- xres) (floor (* val xres) 32768))))
                 (1 (setf *cursor-y* (min (1- yres) (floor (* val yres) 32768))))))
              ((= type 2)                       ; EV_REL
               (case code
                 (0 (setf *cursor-x* (max 0 (min (1- xres) (+ *cursor-x* val)))))
                 (1 (setf *cursor-y* (max 0 (min (1- yres) (+ *cursor-y* val)))))))
              ((and (= type 1) (= code #x110) (= val 1))    ; EV_KEY BTN_LEFT press
               (setf clicked t)))))))
    clicked))

(defun draw-cursor (canvas x y)
  "A small white arrow with a black edge, at X,Y."
  (let ((white (lol.canvas:rgb #xff #xff #xff)) (black (lol.canvas:rgb 0 0 0)))
    (dotimes (i 13)
      (let ((w (if (<= i 8) (1+ i) (max 1 (- 18 i)))))     ; grow to 9, then taper
        (lol.canvas:fill-rect canvas (1- x) (+ y i -1) (+ w 2) 1 black)
        (lol.canvas:fill-rect canvas x (+ y i) w 1 white)))))

;;; ---- the loop -----------------------------------------------------------

(defun run-browser-fb ()
  "Launch the code browser full-screen on /dev/fb0, rooted at CL-USER. Blocks until
   you leave it (C-g at the root, or Esc), then restores the text console and
   returns to the supervisor menu. Any error is caught so it cannot crash PID 1."
  (handler-case
      (progn
        (unless *bfont* (setf *bfont* (lol.canvas:load-font "/cozette.lolf")))
        (trail-root (find-package :cl-user))
        (multiple-value-bind (xres yres) (lol.fb:fb-geometry)
          (let ((canvas (lol.canvas:make-canvas xres yres))
                (mouse  (open-mouse)))
            (setf *cursor-x* (floor xres 2) *cursor-y* (floor yres 2))
            (unwind-protect
                 (lol.fb:with-graphics-console         ; KD_GRAPHICS: fbcon stops drawing
                   ;; signals OFF so Ctrl-C arrives as a byte (the C-c C-c Accept chord)
                   (with-raw-mode (0 nil)
                     (loop
                       (draw-browser canvas xres yres)
                       (when mouse (draw-cursor canvas *cursor-x* *cursor-y*))
                       (lol.fb:present-fb canvas)
                       (multiple-value-bind (kb ms) (fbb-wait 0 mouse)
                         (when kb
                           (multiple-value-bind (key state) (fb-read-key 0)
                             (when (or (eq key :eof)
                                       (eq :quit (browser-key key state)))
                               (return))))
                         (when (and mouse ms (read-mouse mouse xres yres))
                           (browser-click *cursor-x* *cursor-y*))))))
              (when mouse (%fbb-close mouse))))))
    (serious-condition (c)
      (format t "~&code browser: ~a~%" c) (finish-output))))
