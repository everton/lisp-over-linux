;;;; fb-browser.lisp — run the code browser on /dev/fb0 (the guest; no X).
;;;;
;;;; The same shell (poc/shell.lisp) that drives the browser in an X11 window on
;;;; the dev host runs here on the bare framebuffer, launched from the supervisor
;;;; menu. Output is lol.fb:present-fb into /dev/fb0 (under KD_GRAPHICS so fbcon
;;;; stops scribbling); input is BOTH the raw console keyboard AND the evdev mouse,
;;;; multiplexed with poll(2). It returns to the menu when you leave (C-g at the
;;;; root). Esc is left free for a future Meta prefix, so it does not quit.
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

(defun fbb-wait (kb-fd mouse-fd kev-fd)
  "Block until KB-FD (console), MOUSE-FD, or KEV-FD (keyboard evdev) is readable.
   Absent fds are passed NIL. Returns (values kb-ready mouse-ready kev-ready).
   Three fixed pollfd slots; an absent slot stays zeroed (fd 0 / events 0), which
   poll simply never reports ready — so the revents offsets never shift."
  (sb-alien:with-alien ((fds (sb-alien:array sb-alien:unsigned-char 24)))
    (dotimes (i 24) (setf (sb-alien:deref fds i) 0))
    (flet ((set-pfd (n fd)
             (when fd
               (setf (sb-alien:deref fds (* n 8))       (logand fd #xff)
                     (sb-alien:deref fds (+ (* n 8) 4)) +pollin+))))
      (set-pfd 0 kb-fd) (set-pfd 1 mouse-fd) (set-pfd 2 kev-fd)
      (%fbb-poll (sb-alien:cast fds (sb-alien:* sb-alien:unsigned-char)) 3 -1)
      (values (logbitp 0 (sb-alien:deref fds 6))
              (and mouse-fd (logbitp 0 (sb-alien:deref fds 14)))
              (and kev-fd   (logbitp 0 (sb-alien:deref fds 22)))))))

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
             (if (eql b1 91)                    ; CSI: ESC [
                 (let ((b2 (fbb-read-byte fd)))
                   (cond
                     ((null b2) (values :ignore 0))
                     ;; a letter final byte (arrows, Home/End as ESC[H / ESC[F)
                     ((<= 65 b2 90)
                      (values (case b2 (65 :up) (66 :down) (67 :right) (68 :left)
                                       (72 :home) (70 :end) (t :ignore)) 0))
                     ;; a numeric CSI: ESC [ <digits> ~  (Home/End/Del/PgUp/PgDn on
                     ;; many terminals). Consume ALL digits AND the final ~, so the
                     ;; trailing byte never leaks into the buffer as a stray char.
                     ((<= 48 b2 57)
                      (let ((n (- b2 48)))
                        (loop for b = (fbb-read-byte fd)
                              while (and b (<= 48 b 57))
                              do (setf n (+ (* n 10) (- b 48))))  ; last read = the ~
                        (values (case n (1 :home) (7 :home) (4 :end) (8 :end)
                                        (3 :delete) (5 :page-up) (6 :page-down)
                                        (t :ignore)) 0)))
                     (t (values :ignore 0))))
                 (values :ignore 0)))
           (values :escape 0)))
      ((member c '(13 10)) (values :return 0))
      ((member c '(8 127)) (values :backspace 0))
      ((= c 9)             (values :tab 0))
      ((<= 1 c 26)         (values (code-char (+ c 96)) 4))   ; C-a..C-z, bit 2 = Ctrl
      (t                   (values (code-char c) 0)))))

;;; ---- mouse: evdev -> a cursor + clicks ----------------------------------

(defvar *cursor-x* 0) (defvar *cursor-y* 0)
(defvar *ctrl-down* nil "Is a Ctrl key held? Tracked from the keyboard's evdev.")

(defun scan-input-device (mask-ok-p)
  "The /dev/input/eventN of the first device in /proc/bus/input/devices whose
   capability mask (the 'B: EV=' line, hex) satisfies MASK-OK-P, or NIL.

   We read the EV mask rather than grepping Handlers for 'mouse'/'kbd': mousedev
   isn't loaded, so a plain PS/2 mouse shows only 'Handlers=eventN' with no 'mouseN'
   token. The EV bits are what actually classify the device."
  (ignore-errors
    (with-open-file (s "/proc/bus/input/devices" :if-does-not-exist nil)
      (when s
        (let ((ev nil) (ok nil))
          (flet ((finish ()                       ; end of a device block
                   (when (and ev ok)
                     (return-from scan-input-device
                       (format nil "/dev/input/~a" ev)))
                   (setf ev nil ok nil)))
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
                   (when (and mask (funcall mask-ok-p mask))
                     (setf ok t))))))))))))

(defun find-mouse-device ()
  "A pointer: EV_REL (bit 2, mouse/touchpad) or EV_ABS (bit 3, tablet)."
  (scan-input-device (lambda (m) (or (logbitp 2 m) (logbitp 3 m)))))

(defun find-keyboard-device ()
  "The real keyboard: EV_KEY (bit 1) and EV_REP (bit 20, autorepeat). The EV_REP
   bit is what separates it from the Power Button (also a 'kbd' handler, EV=3)."
  (scan-input-device (lambda (m) (and (logbitp 1 m) (logbitp 20 m)))))

(defun open-evdev (path)
  "Open an evdev node non-blocking; NIL on failure or no PATH."
  (when path
    (let ((fd (%fbb-open path (logior +o-rdonly+ +o-nonblock+))))
      (and (>= fd 0) fd))))

(defun open-mouse ()    (open-evdev (find-mouse-device)))
(defun open-keyboard () (open-evdev (find-keyboard-device)))

(defun read-keyboard-evdev (fd)
  "Drain the keyboard's evdev queue, tracking Ctrl held state in *ctrl-down*.
   Text still arrives through the cooked tty (fd 0); this fd exists only for the
   modifier state the tty can't report — a bare Ctrl produces no byte there."
  (sb-alien:with-alien ((e (sb-alien:array sb-alien:unsigned-char 24)))
    (loop
      (let ((n (%fbb-read fd (sb-alien:cast e (sb-alien:* sb-alien:unsigned-char)) 24)))
        (when (< n 24) (return))
        (let ((type (logior (sb-alien:deref e 16) (ash (sb-alien:deref e 17) 8)))
              (code (logior (sb-alien:deref e 18) (ash (sb-alien:deref e 19) 8)))
              (val  (logior (sb-alien:deref e 20) (ash (sb-alien:deref e 21) 8))))
          (when (and (= type 1)                          ; EV_KEY
                     (or (= code 29) (= code 97)))        ; KEY_LEFTCTRL / KEY_RIGHTCTRL
            (setf *ctrl-down* (plusp val))))))))          ; 1 press / 2 repeat -> down, 0 -> up

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

(defparameter *cursor-bits*
  #("K"
    "KK"
    "KWK"
    "KWWK"
    "KWWWK"
    "KWWWWK"
    "KWWWWWK"
    "KWWWWWWK"
    "KWWWWWWWK"
    "KWWWWWWWWK"
    "KWWWWWKKKK"
    "KWWKKWWK"
    "KWK KWWK"
    "KK  KWWK"
    "K   KWWK"
    "    KKKK")
  "The pointer as a tiny bitmap — K = black edge, W = white fill, space =
   transparent. A north-west arrow: the arrowhead, its lower-left barb, and the
   tail/stem below (a plain triangle read as 'unfinished', so draw the whole glyph).")

(defun draw-cursor (canvas x y)
  "Blit *cursor-bits* with its tip (the top-left K) at X,Y — one pixel per cell."
  (let ((white (lol.canvas:rgb #xff #xff #xff))
        (black (lol.canvas:rgb 0 0 0)))
    (loop for row across *cursor-bits* for dy from 0 do
      (loop for ch across row for dx from 0 do
        (case ch
          (#\K (lol.canvas:fill-rect canvas (+ x dx) (+ y dy) 1 1 black))
          (#\W (lol.canvas:fill-rect canvas (+ x dx) (+ y dy) 1 1 white)))))))

;;; ---- the loop -----------------------------------------------------------

(defun run-browser-fb (&optional root)
  "Launch the browser full-screen on /dev/fb0, rooted at ROOT (default: the CL-USER
   package — the code browser; pass (subj-workspace) for the Workspace). Blocks until
   you leave it (C-g at the root), then restores the text console and returns to the
   supervisor menu. Any error is caught so it cannot crash PID 1."
  (handler-case
      (progn
        (unless *bfont* (setf *bfont* (lol.canvas:load-font "/cozette.lolf")))
        (trail-root (or root (find-package :cl-user)))
        (multiple-value-bind (xres yres) (lol.fb:fb-geometry)
          (let ((canvas (lol.canvas:make-canvas xres yres))
                (mouse  (open-mouse))
                (kbd    (open-keyboard)))
            (setf *cursor-x* (floor xres 2) *cursor-y* (floor yres 2) *ctrl-down* nil)
            (unwind-protect
                 (lol.fb:with-graphics-console         ; KD_GRAPHICS: fbcon stops drawing
                   ;; signals OFF so Ctrl-C arrives as a byte (the C-c C-c Accept chord)
                   (with-raw-mode (0 nil)
                     (loop
                       (draw-browser canvas xres yres)
                       (when mouse (draw-cursor canvas *cursor-x* *cursor-y*))
                       (lol.fb:present-fb canvas)
                       (multiple-value-bind (kb ms kev) (fbb-wait 0 mouse kbd)
                         (when kev (read-keyboard-evdev kbd))   ; refresh Ctrl before a click
                         (when kb
                           (multiple-value-bind (key state) (fb-read-key 0)
                             (when (or (eq key :eof)
                                       (eq :quit (browser-key key state)))
                               (return))))
                         (when (and mouse ms (read-mouse mouse xres yres))
                           (browser-click *cursor-x* *cursor-y* *ctrl-down*))))))
              (when mouse (%fbb-close mouse))
              (when kbd (%fbb-close kbd))))))
    (serious-condition (c)
      (format t "~&code browser: ~a~%" c) (finish-output))))
