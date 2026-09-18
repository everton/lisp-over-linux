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
;;;; (the pointer, found via /proc/bus/input/devices) — relative (PS/2 / USB), absolute
;;;; (usb-tablet) or a laptop touchpad, whose absolute finger position is integrated
;;;; rather than mapped, left button drills. CL-USER, after the toolkit + line-editor.

(in-package :cl-user)

;;; ---- low-level fds via sb-alien (same style as net.lisp / fb.lisp) --------

(sb-alien:define-alien-routine ("open"  %fbb-open)  sb-alien:int
  (path sb-alien:c-string) (flags sb-alien:int))
(sb-alien:define-alien-routine ("read"  %fbb-read)  sb-alien:long
  (fd sb-alien:int) (buf (sb-alien:* sb-alien:unsigned-char)) (n sb-alien:unsigned-long))
(sb-alien:define-alien-routine ("close" %fbb-close) sb-alien:int (fd sb-alien:int))
(sb-alien:define-alien-routine ("poll"  %fbb-poll)  sb-alien:int
  (fds (sb-alien:* sb-alien:unsigned-char)) (nfds sb-alien:unsigned-long) (timeout sb-alien:int))
;; ioctl is variadic in C; for EVIOCGABS the 3rd arg is a struct input_absinfo *.
;; (net.lisp has its own %ioctl for the SIOC* calls, but it is loaded after us.)
(sb-alien:define-alien-routine ("ioctl" %fbb-ioctl) sb-alien:int
  (fd sb-alien:int) (request sb-alien:unsigned-long)
  (argp (sb-alien:* sb-alien:unsigned-char)))

(defconstant +o-rdonly+   0)
(defconstant +o-nonblock+ #x800)
(defconstant +pollin+     1)
;; +wheel-lines+ (how far a notch scrolls) lives in shell.lisp — both backends share it.

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

(defun fbb-wait (kb-fd mouse-fd kev-fd &optional (timeout-ms -1))
  "Block until KB-FD (console), MOUSE-FD, or KEV-FD (keyboard evdev) is readable, or
   until TIMEOUT-MS elapses (-1, the default, waits forever).
   Absent fds are passed NIL. Returns (values kb-ready mouse-ready kev-ready).
   Three fixed pollfd slots; an absent slot stays zeroed (fd 0 / events 0), which
   poll simply never reports ready — so the revents offsets never shift.

   The timeout exists for the stage (§3b). Every other view in the browser is a
   function of state that only input changes, so sleeping until a key or a click is
   exactly right and costs nothing. A stage advances on its own, which means the loop
   needs a way to wake on a frame deadline instead. This is that way, and it is the
   only concession the input layer makes to a view that moves."
  (sb-alien:with-alien ((fds (sb-alien:array sb-alien:unsigned-char 24)))
    (dotimes (i 24) (setf (sb-alien:deref fds i) 0))
    (flet ((set-pfd (n fd)
             (when fd
               (setf (sb-alien:deref fds (* n 8))       (logand fd #xff)
                     (sb-alien:deref fds (+ (* n 8) 4)) +pollin+))))
      (set-pfd 0 kb-fd) (set-pfd 1 mouse-fd) (set-pfd 2 kev-fd)
      (%fbb-poll (sb-alien:cast fds (sb-alien:* sb-alien:unsigned-char)) 3 timeout-ms)
      (values (logbitp 0 (sb-alien:deref fds 6))
              (and mouse-fd (logbitp 0 (sb-alien:deref fds 14)))
              (and kev-fd   (logbitp 0 (sb-alien:deref fds 22)))))))

;;; ---- keyboard: raw bytes -> shell keys ----------------------------------

(defun fb-csi-mods->state (m)
  "The xterm CSI modifier parameter (Shift=1, Alt=2, Ctrl=4, sent as m-1) mapped to
   our state bits (Shift bit0, Meta bit3, Ctrl bit2). 0/1 -> no modifier."
  (let ((k (if (> m 1) (1- m) 0)))
    (logior (if (logbitp 0 k) 1 0)      ; shift
            (if (logbitp 1 k) 8 0)      ; meta / alt
            (if (logbitp 2 k) 4 0))))   ; ctrl

(defun fb-read-csi (fd)
  "Decode a CSI sequence whose leading ESC [ is already consumed:
   ESC [ <n1> (';' <n2>)? <final>. A letter final is a cursor / Home / End key and n2
   is the (xterm) modifier — so Ctrl-Right arrives as ESC [ 1 ; 5 C; a '~' final
   selects Home/End/Del/Page by n1. All params and the final are consumed, so a
   modified sequence never leaks its tail into the buffer. Returns (values KEY STATE)."
  (let ((n1 0) (n2 0) (b (fbb-read-byte fd)))
    (loop while (and b (<= 48 b 57)) do (setf n1 (+ (* n1 10) (- b 48)) b (fbb-read-byte fd)))
    (when (eql b 59)                         ; ';' — a modifier parameter follows
      (setf b (fbb-read-byte fd))
      (loop while (and b (<= 48 b 57)) do (setf n2 (+ (* n2 10) (- b 48)) b (fbb-read-byte fd))))
    (let ((state (fb-csi-mods->state n2)))
      (cond
        ((null b) (values :ignore 0))
        ((<= 65 b 90)                        ; letter final: A/B/C/D + H/F
         (values (case b (65 :up) (66 :down) (67 :right) (68 :left)
                         (72 :home) (70 :end) (t :ignore)) state))
        ((= b 126)                           ; '~' final: n1 selects the key
         (values (case n1 (1 :home) (7 :home) (4 :end) (8 :end)
                          (3 :delete) (5 :page-up) (6 :page-down) (t :ignore)) state))
        (t (values :ignore 0))))))

(defun fb-read-key (fd)
  "One keystroke from raw fd FD -> (values KEY STATE) for browser-key. A control
   byte becomes its letter + the Ctrl bit (so C-c C-c, C-g work); ESC[… becomes an
   arrow/page keyword; ESC<printable> becomes that char + the Meta bit (Alt-key, or
   a fast Esc-then-key, so M-. jumps); a lone ESC -> :escape (probed with a short
   poll so it does not block waiting for a sequence that never comes — and so the
   Esc-as-Meta prefix, typed as two unhurried keystrokes, still reads as :escape)."
  (let ((c (fbb-read-byte fd)))
    (cond
      ((null c) (values :eof 0))
      ((= c 27)
       (if (fbb-ready-p fd 20)                 ; a real ESC sequence follows?
           (let ((b1 (fbb-read-byte fd)))
             (cond
               ((eql b1 91) (fb-read-csi fd))    ; CSI: ESC [ … (arrows, Home/End, modifiers)
               ;; ESC <printable> = Meta-<char> (bit 3): Alt-key on the console, or a
               ;; fast Esc-then-key. M-. reaches browser-key as (#\. + Meta).
               ((and b1 (<= 33 b1 126)) (values (code-char b1) 8))
               (t (values :ignore 0))))
           (values :escape 0)))
      ((member c '(13 10)) (values :return 0))
      ((member c '(8 127)) (values :backspace 0))
      ((= c 9)             (values :tab 0))
      ((= c 0)             (values #\Space 4))               ; C-Space / C-@ = NUL -> C-Space
      ((<= 1 c 26)         (values (code-char (+ c 96)) 4))   ; C-a..C-z, bit 2 = Ctrl
      (t                   (values (code-char c) 0)))))

;;; ---- mouse: evdev -> a cursor + clicks ----------------------------------

(defvar *cursor-x* 0) (defvar *cursor-y* 0)
(defvar *ctrl-down* nil "Is a Ctrl key held? Tracked from the keyboard's evdev.")
(defvar *shift-down* nil "Is a Shift key held? Tracked from the keyboard's evdev, so
  Shift-arrow can extend a selection — the cooked tty reports the modifier no more
  than it reports a bare Ctrl.")
(defvar *super-down* nil "Is a Super/Meta (Windows) key held? Tracked from the
  keyboard's evdev — the spatial layer's modifier, and what SuperL-? needs.")

(defun scan-input-device (device-ok-p)
  "The first device in /proc/bus/input/devices that DEVICE-OK-P accepts, as
   (values \"/dev/input/eventN\" EV PROP) — EV is the 'B: EV=' capability mask and
   PROP the 'B: PROP=' property mask, both hex in the file. NIL if none matches.

   We read the EV mask rather than grepping Handlers for 'mouse'/'kbd': mousedev
   isn't loaded, so a plain PS/2 mouse shows only 'Handlers=eventN' with no 'mouseN'
   token. The EV bits are what actually classify the device.

   PROP is the second half of the story and the reason a touchpad works at all:
   EV says a device emits absolute positions, PROP says whether those positions
   are *on the screen* (INPUT_PROP_DIRECT — a tablet) or *on a small pad under
   your hand* (INPUT_PROP_POINTER / INPUT_PROP_BUTTONPAD). Both print here, so
   neither costs an open(2).

   The predicate runs at the end of a device's block, because PROP is printed
   before EV and we want both in hand before deciding."
  (ignore-errors
    (with-open-file (s "/proc/bus/input/devices" :if-does-not-exist nil)
      (when s
        (let ((node nil) (ev nil) (prop 0))
          (flet ((hex (line key)
                   (let ((p (search key line)))
                     (when p
                       (let* ((start (+ p (length key)))
                              (end (or (position #\Space line :start start)
                                       (length line))))
                         (ignore-errors
                           (parse-integer line :start start :end end :radix 16))))))
                 (finish ()                       ; end of a device block
                   (when (and node ev (funcall device-ok-p ev prop))
                     (return-from scan-input-device
                       (values (format nil "/dev/input/~a" node) ev prop)))
                   (setf node nil ev nil prop 0)))
            (loop for line = (read-line s nil nil) do
              (cond
                ((null line) (finish) (return))               ; EOF: flush last block
                ((zerop (length line)) (finish))              ; blank: block boundary
                ((and (>= (length line) 2) (string= (subseq line 0 2) "H:"))
                 (let ((p (search "event" line)))
                   (when p
                     (let ((end (or (position #\Space line :start p) (length line))))
                       (setf node (subseq line p end))))))
                ((search "PROP=" line) (setf prop (or (hex line "PROP=") 0)))
                ((search "EV=" line)   (setf ev   (hex line "EV=")))))))))))

;;; What kind of pointer we found, decided once at open time and consulted by
;;; READ-MOUSE on every packet. EV bit 2 = EV_REL, bit 3 = EV_ABS; PROP bit 0 =
;;; INPUT_PROP_POINTER, bit 2 = INPUT_PROP_BUTTONPAD — either one means "a pad
;;; under your hand", not "a surface you point at".
(defvar *pointer-abs-p* nil "Does the pointer report positions (EV_ABS) rather than deltas?")
(defvar *pointer-pad-p* nil "Is that absolute pointer a touchpad rather than a tablet?")
(defvar *abs-x-min* 0) (defvar *abs-x-max* 32767)
(defvar *abs-y-min* 0) (defvar *abs-y-max* 32767)

;;; Touchpad state: where the finger was last packet, and the fractions of a pixel
;;; its motion has not yet paid out. See PAD-MOTION.
(defvar *pad-x* nil "Last ABS_X the touchpad reported, or NIL for 'no finger yet'.")
(defvar *pad-y* nil "Last ABS_Y, likewise.")
(defvar *pad-fingers* 0 "How many fingers the pad says are down (BTN_TOOL_*).")
(defvar *pad-rx* 0 "Sub-pixel remainder of horizontal pad motion, an exact rational.")
(defvar *pad-ry* 0 "Sub-pixel remainder of vertical pad motion.")
(defvar *pad-scroll* 0 "Two-finger motion not yet worth a wheel notch, in pad units.")

;;; Tap-to-click. A clickpad's physical press arrives as BTN_LEFT and always
;;; worked; a TAP is a contact that sends no button at all, and libinput is what
;;; normally turns one into a click. There is no libinput here, so we do it.
(defvar *tap-down-us* nil "When the finger landed, in microseconds, or NIL for no contact.")
(defvar *tap-x* nil "Where it landed, in pad units — the origin the travel test measures from.")
(defvar *tap-y* nil)
(defvar *tap-live* nil "Is this contact still a tap candidate?")

(defvar *tap-max-us* 180000
  "The longest contact still counted as a tap, in microseconds. Past it you were
   resting a finger, not tapping — 180ms is what libinput settled on.")
(defvar *tap-travel* 1/40
  "How far the finger may wander and still tap, as a fraction of the pad's own axis
   range. A tap is a contact that did not travel; this is the slop that forgives a
   finger rolling slightly as it lifts.")

(defvar *pad-speed* 3/2
  "How far the cursor travels for a full sweep of the pad, in screens. 1 means the
   pad maps one-to-one onto the display, which is accurate but makes you cross the
   whole pad to cross the whole screen; 3/2 asks for about two thirds of a sweep.")

(defun pointer-pad-p (ev prop)
  "EV/PROP describe a touchpad: absolute, not relative, and a POINTER/BUTTONPAD."
  (and (logbitp 3 ev) (not (logbitp 2 ev))
       (or (logbitp 0 prop) (logbitp 2 prop))))

(defun find-mouse-device ()
  "The pointer, best candidate first, as (values PATH EV PROP):

   1. a *direct* absolute device — QEMU's '-device usb-tablet'. Nobody has one by
      accident, its 0..32767 maps straight onto the screen, and it is the only
      pointer a scripted click can aim, so it wins when present.
   2. a *relative* mouse — the QEMU default PS/2 mouse, any USB mouse, or the
      relative node a HID touchpad exposes while it is still in mouse mode.
   3. a *touchpad* — correct, but its raw position needs interpreting
      (see READ-MOUSE), so we only reach for it when there is nothing simpler.

   Ranking rather than taking the first match matters on real hardware: a laptop
   has no mouse, so the touchpad is the only pointer there is, while in QEMU the
   ranking changes nothing."
  (dolist (want '(:tablet :mouse :pad))
    (multiple-value-bind (path ev prop)
        (scan-input-device
         (lambda (ev prop)
           (ecase want
             (:tablet (and (logbitp 3 ev) (not (logbitp 2 ev))
                           (not (pointer-pad-p ev prop))))
             (:mouse  (logbitp 2 ev))
             (:pad    (pointer-pad-p ev prop)))))
      (when path (return (values path ev prop))))))

(defun find-keyboard-device ()
  "The real keyboard: EV_KEY (bit 1) and EV_REP (bit 20, autorepeat). The EV_REP
   bit is what separates it from the Power Button (also a 'kbd' handler, EV=3)."
  (scan-input-device (lambda (m prop) (declare (ignore prop))
                       (and (logbitp 1 m) (logbitp 20 m)))))

(defun open-evdev (path)
  "Open an evdev node non-blocking; NIL on failure or no PATH."
  (when path
    (let ((fd (%fbb-open path (logior +o-rdonly+ +o-nonblock+))))
      (and (>= fd 0) fd))))

(defun evdev-abs-range (fd axis)
  "The (values MIN MAX) an absolute AXIS actually reports, via EVIOCGABS.

   This is the whole bug fix in one function. A 'struct input_absinfo' is six
   __s32s — value, minimum, maximum, fuzz, flat, resolution — and we want the
   middle two; EVIOCGABS(axis) is _IOR('E', 0x40+axis, 24) = #x80184540 + axis.
   Assuming 0..32767 instead (usb-tablet's range, which every emulated tablet
   happens to share) squeezes a real touchpad — a Synaptics pad reports roughly
   1472..5472 by 1408..4448 — into the top-left eighth of the screen.

   Falls back to 0..32767 when the ioctl fails or the axis is degenerate."
  (sb-alien:with-alien ((info (sb-alien:array sb-alien:unsigned-char 24)))
    (dotimes (i 24) (setf (sb-alien:deref info i) 0))
    (if (minusp (%fbb-ioctl fd (+ #x80184540 axis)
                            (sb-alien:cast info (sb-alien:* sb-alien:unsigned-char))))
        (values 0 32767)
        (flet ((s32 (off)
                 (let ((v (logior (sb-alien:deref info off)
                                  (ash (sb-alien:deref info (+ off 1)) 8)
                                  (ash (sb-alien:deref info (+ off 2)) 16)
                                  (ash (sb-alien:deref info (+ off 3)) 24))))
                   (if (>= v #x80000000) (- v #x100000000) v))))
          (let ((lo (s32 4)) (hi (s32 8)))
            (if (> hi lo) (values lo hi) (values 0 32767)))))))

(defun open-mouse ()
  "Open the pointer and remember how to read it: *pointer-abs-p* / *pointer-pad-p*
   and, for an absolute device, the axis ranges READ-MOUSE scales against."
  (multiple-value-bind (path ev prop) (find-mouse-device)
    (let ((fd (open-evdev path)))
      (when fd
        (setf *pointer-abs-p* (and (logbitp 3 ev) (not (logbitp 2 ev)))
              *pointer-pad-p* (pointer-pad-p ev prop)
              *pad-x* nil *pad-y* nil *pad-fingers* 0
              *pad-rx* 0 *pad-ry* 0 *pad-scroll* 0
              *tap-down-us* nil *tap-x* nil *tap-y* nil *tap-live* nil)
        (when *pointer-abs-p*
          (multiple-value-setq (*abs-x-min* *abs-x-max*) (evdev-abs-range fd 0))
          (multiple-value-setq (*abs-y-min* *abs-y-max*) (evdev-abs-range fd 1))))
      fd)))

(defun open-keyboard () (open-evdev (find-keyboard-device)))

(defun read-keyboard-evdev (fd)
  "Drain the keyboard's evdev queue, tracking Ctrl / Shift / Super held state in
   *ctrl-down* / *shift-down* / *super-down*. Text still arrives through the cooked
   tty (fd 0); this fd exists only for the modifier state the tty can't report — a
   bare modifier produces no byte there. Super is what makes the spatial layer (and
   its SuperL-? cheatsheet) reachable on the bare console at all."
  (sb-alien:with-alien ((e (sb-alien:array sb-alien:unsigned-char 24)))
    (loop
      (let ((n (%fbb-read fd (sb-alien:cast e (sb-alien:* sb-alien:unsigned-char)) 24)))
        (when (< n 24) (return))
        (let ((type (logior (sb-alien:deref e 16) (ash (sb-alien:deref e 17) 8)))
              (code (logior (sb-alien:deref e 18) (ash (sb-alien:deref e 19) 8)))
              (val  (logior (sb-alien:deref e 20) (ash (sb-alien:deref e 21) 8))))
          (when (= type 1)                               ; EV_KEY  (1 press / 2 repeat -> down)
            (cond
              ((or (= code 29) (= code 97))              ; KEY_LEFTCTRL / KEY_RIGHTCTRL
               (setf *ctrl-down* (plusp val)))
              ((or (= code 42) (= code 54))              ; KEY_LEFTSHIFT / KEY_RIGHTSHIFT
               (setf *shift-down* (plusp val)))
              ((or (= code 125) (= code 126))            ; KEY_LEFTMETA / KEY_RIGHTMETA (Super)
               (setf *super-down* (plusp val))))))))))

(defun abs-to-screen (val lo hi res)
  "Map an absolute reading in LO..HI onto 0..RES-1."
  (max 0 (min (1- res) (floor (* (- val lo) res) (max 1 (- hi lo))))))

(defun evdev-time-us (buf)
  "The struct timeval at the head of an input_event, as microseconds.

   Seconds then microseconds, 8 bytes each on x86-64 — the 16 bytes every record has
   carried and we have ignored since the beginning (doc/input.org §2). Only the tap
   gesture reads them, and only to take a difference, so the clock they come from
   (evdev's default is CLOCK_REALTIME) never has to mean anything absolute."
  (let ((sec 0) (usec 0))
    (dotimes (i 8) (incf sec  (ash (sb-alien:deref buf i) (* 8 i))))
    (dotimes (i 8) (incf usec (ash (sb-alien:deref buf (+ 8 i)) (* 8 i))))
    (+ (* sec 1000000) usec)))

(defun pad-motion (nx ny xres yres)
  "One touchpad packet's worth of finger position; returns the wheel notches it
   produced (0 when it moved the cursor instead).

   A pad is about 90mm wide and the screen is 1920px, so the finger's position on
   the pad is not a position on the screen — we difference successive positions
   and move the cursor by the difference, scaled so that crossing the whole pad
   crosses the whole screen (times *PAD-SPEED*). Scaling each axis by its own
   range also fixes the aspect: a pad is wider than it is tall in device units
   too, so the two factors come out nearly equal and motion stays isotropic.

   Two things keep it from teleporting. A finger landing or lifting, or a second
   finger joining, clears *PAD-X* (READ-MOUSE does that on any BTN_TOUCH /
   BTN_TOOL_* transition), so the first packet of a new contact yields no delta.
   And any jump over a quarter of the pad is discarded outright, which covers
   hardware that does not bother to report BTN_TOUCH at all."
  (let* ((xrange (max 1 (- *abs-x-max* *abs-x-min*)))
         (yrange (max 1 (- *abs-y-max* *abs-y-min*)))
         (dx (and nx *pad-x* (- nx *pad-x*)))
         (dy (and ny *pad-y* (- ny *pad-y*)))
         (wheel 0))
    (when nx (setf *pad-x* nx))
    (when ny (setf *pad-y* ny))
    ;; A tap is a contact that did not travel. BTN_TOUCH may land in a packet with
    ;; no position of its own, so take the origin from the first one that has it.
    (when *tap-live*
      (when (and nx (null *tap-x*)) (setf *tap-x* nx))
      (when (and ny (null *tap-y*)) (setf *tap-y* ny))
      (when (or (and nx *tap-x* (> (abs (- nx *tap-x*)) (round (* xrange *tap-travel*))))
                (and ny *tap-y* (> (abs (- ny *tap-y*)) (round (* yrange *tap-travel*)))))
        (setf *tap-live* nil)))
    (when (and dx (> (abs dx) (floor xrange 4))) (setf dx nil))
    (when (and dy (> (abs dy) (floor yrange 4))) (setf dy nil))
    (if (>= *pad-fingers* 2)
        ;; two fingers = the wheel this hardware does not have. Dragging them
        ;; toward you is the same gesture as rolling a wheel toward you, so it
        ;; makes the same negative notches (see §6 of doc/input.org).
        (when dy
          (incf *pad-scroll* dy)
          (let ((notch (max 1 (floor yrange 24))))
            (loop while (>= (abs *pad-scroll*) notch)
                  do (let ((s (signum *pad-scroll*)))
                       (decf *pad-scroll* (* s notch))
                       (decf wheel s)))))
        (macrolet ((glide (d span res remainder cursor limit)
                     ;; Exact rationals, not truncated pixels: at ~0.7px per pad
                     ;; unit a slow drag is a string of deltas that each round to
                     ;; zero, and dropping those would stall the cursor. Carry the
                     ;; fraction over to the next packet instead.
                     `(let* ((acc (+ ,remainder (/ (* ,d ,res *pad-speed*) ,span)))
                             (whole (truncate acc)))
                        (setf ,remainder (- acc whole)
                              ,cursor (max 0 (min ,limit (+ ,cursor whole)))))))
          (setf *pad-scroll* 0)
          (when dx (glide dx xrange xres *pad-rx* *cursor-x* (1- xres)))
          (when dy (glide dy yrange yres *pad-ry* *cursor-y* (1- yres)))))
    wheel))

(defun read-mouse (fd xres yres)
  "Drain pending 24-byte input_event records; move *cursor-x/y*. Returns
   (values CLICKED WHEEL): CLICKED is T on a left-button press — or, on a touchpad,
   on a TAP, which reports no button at all (see *TAP-MAX-US*); WHEEL is the summed
   scroll notches this drain (REL_WHEEL, + = up / away from the user, - = down), 0
   when there was none. Callers that want only the click keep working on the first
   value.

   Three kinds of pointer, sorted out once by OPEN-MOUSE:

   - EV_REL — a mouse. The values are deltas; add them.
   - EV_ABS, direct — a usb-tablet. The values are positions on the screen; scale
     them from the device's *own* range (EVDEV-ABS-RANGE), not an assumed one.
   - EV_ABS, INPUT_PROP_POINTER — a laptop touchpad. The values are positions on
     the pad, which is not the screen; hand them to PAD-MOTION.

   Absolute axes are gathered across the packet and applied at SYN_REPORT rather
   than record by record, so X and Y move together and one drag is one delta."
  (let ((clicked nil) (wheel 0) (nx nil) (ny nil))
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
                 (0 (if *pointer-pad-p*
                        (setf nx val)
                        (setf *cursor-x* (abs-to-screen val *abs-x-min* *abs-x-max* xres))))
                 (1 (if *pointer-pad-p*
                        (setf ny val)
                        (setf *cursor-y* (abs-to-screen val *abs-y-min* *abs-y-max* yres))))))
              ((= type 2)                       ; EV_REL
               (case code
                 (0 (setf *cursor-x* (max 0 (min (1- xres) (+ *cursor-x* val)))))
                 (1 (setf *cursor-y* (max 0 (min (1- yres) (+ *cursor-y* val)))))
                 (8 (incf wheel val))))          ; REL_WHEEL: +1 up / -1 down
              ((= type 1)                       ; EV_KEY — a button is just a key
               (case code
                 (#x110 (when (= val 1) (setf clicked t))        ; BTN_LEFT press
                        ;; a real press is not also a tap — a clickpad reports both
                        ;; the button and the finger resting on it
                        (setf *tap-live* nil))
                 ;; BTN_TOUCH / BTN_TOOL_{FINGER,DOUBLETAP,TRIPLETAP} only change
                 ;; value when the contact does: a finger landed, lifted, or was
                 ;; joined by another. All three move the point the pad reports, so
                 ;; all three must forget the previous one rather than subtract it.
                 ((#x14a #x145 #x14d #x14e)
                  (setf *pad-x* nil *pad-y* nil *pad-rx* 0 *pad-ry* 0 *pad-scroll* 0)
                  (case code
                    (#x14a                                               ; BTN_TOUCH
                     (if (plusp val)
                         ;; a finger landed: open a tap candidate. NX/NY are this
                         ;; packet's position — EV_ABS precedes EV_KEY in a packet,
                         ;; so it is already here — and PAD-MOTION fills them in
                         ;; from the next packet if it was not.
                         (setf *tap-down-us* (evdev-time-us
                                              (sb-alien:cast e (sb-alien:* sb-alien:unsigned-char)))
                               *tap-x* nx *tap-y* ny
                               *tap-live* *pointer-pad-p*)
                         ;; it left: a candidate that was brief enough IS the click.
                         ;; No button was ever reported — on a pad, that is the whole
                         ;; gesture, and the reason libinput exists elsewhere.
                         (let ((dt (and *tap-down-us*
                                        (- (evdev-time-us
                                            (sb-alien:cast e (sb-alien:* sb-alien:unsigned-char)))
                                           *tap-down-us*))))
                           (when (and *tap-live* dt (<= 0 dt *tap-max-us*))
                             (setf clicked t))
                           (setf *pad-fingers* 0 *tap-live* nil
                                 *tap-down-us* nil *tap-x* nil *tap-y* nil))))
                    ;; a second finger means scroll, or a right-click we have no
                    ;; button for — either way this contact stopped being a tap
                    (#x145 (when (plusp val) (setf *pad-fingers* 1)))      ; one finger
                    (#x14d (when (plusp val) (setf *pad-fingers* 2 *tap-live* nil)))   ; two
                    (#x14e (when (plusp val) (setf *pad-fingers* 3 *tap-live* nil))))))) ; three
              ((and (= type 0) (= code 0) *pointer-pad-p*)   ; EV_SYN/SYN_REPORT
               (incf wheel (pad-motion nx ny xres yres))
               (setf nx nil ny nil)))))))
    (values clicked wheel)))

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

;; The live I/O the running browser owns, published so the debugger (entered from
;; *debugger-hook*, mid-loop) can draw to the same screen and read the same input
;; without threading them through every call.
(defvar *fb-canvas* nil) (defvar *fb-xres* 0) (defvar *fb-yres* 0)
(defvar *fb-mouse* nil)  (defvar *fb-kbd* nil)
;;; ---- the stage's heartbeat (§3b) -----------------------------------------
;;; The browser has always been a pure function of state that only input changes, and
;;; the poll that sleeps forever is how it stays at zero CPU on an idle console. A
;;; stage breaks that, so it is allowed to — but ONLY while one is actually on screen
;;; and running. The moment you drill away from it the loop is back to sleeping until
;;; a key, which is why the timeout is computed per iteration rather than set once.

(defparameter +stage-frame-ms+ 33
  "Milliseconds between stage ticks — about thirty a second. Fast enough that motion
   reads as motion, slow enough that a whole repaint in Lisp keeps up on the guest.")

(defun current-stage ()
  "The stage on screen right now, or NIL. Reaches through the shell rather than
   holding its own reference, so closing the tab really does stop the clock."
  (let ((p (ignore-errors (current-pane))))
    (when (and p (eq (view-kind (pane-view p)) :stage))
      (view-data (pane-view p)))))

(defun live-stage-timeout ()
  "The poll timeout for this iteration: a frame deadline when a running stage is on
   screen, -1 (sleep until input) otherwise."
  (let ((st (current-stage)))
    (if (and st (stage-running st)) +stage-frame-ms+ -1)))

(defvar *stage-last-tick* 0
  "Internal-real-time at the previous tick, so the stage advances by WALL CLOCK.")

(defparameter +stage-max-catchup+ 6
  "Most ticks one frame may run. Without a cap, a stage left behind a modal debugger
   would try to replay the whole pause in a single frame the moment it came back.")

(defun tick-live-stage ()
  "Advance the on-screen stage by however many ticks the wall clock says are due.

   Not one tick per frame: a full 1280x800 repaint in Lisp costs more than a frame
   period, so one-per-frame would make the world's speed a function of how expensive
   the pane is to draw — motion that slows down when you open a bigger window. Taking
   the elapsed time and stepping the arrears keeps the simulation rate fixed at
   +STAGE-FRAME-MS+ regardless of what the renderer manages.

   A contact here can signal — that is the point of the whole view — and the signal
   travels through *debugger-hook* into RUN-DEBUGGER-FB exactly as one from the
   Workspace does, with the stack still standing and the stage's own restarts on the
   rail. Nothing special is needed to make that work, which is the best evidence that
   the stage belongs in this browser rather than beside it."
  (let ((st (current-stage)))
    (when (and st (stage-running st))
      (let* ((now (get-internal-real-time))
             (elapsed (round (* 1000 (- now *stage-last-tick*))
                             internal-time-units-per-second))
             (due (max 1 (min +stage-max-catchup+ (floor elapsed +stage-frame-ms+)))))
        (setf *stage-last-tick* now)
        ;; ARMED, exactly as workspace-debug-it arms an eval, and for the same reason:
        ;; HANDLER-BIND runs its handler BEFORE the unwind, so run-debugger-fb sees the
        ;; live restarts and the standing stack. The outer handler-case around
        ;; run-browser-fb would catch this too — but it catches by UNWINDING, which
        ;; drops you out of the browser to the supervisor menu with the error printed,
        ;; and by then the stack the debugger exists to show is gone.
        (restart-case
            (handler-bind ((serious-condition
                             (lambda (c) (funcall 'run-debugger-fb c))))
              (dotimes (i due) (step-stage st)))
          (abort ()
            :report "Return to the stage (paused)"
            ;; Pausing is the honest outcome: nothing resolved the contact, and the two
            ;; bodies are still overlapping. Resuming without pausing would re-signal a
            ;; second later and keep re-signalling — a modal that reopens on a timer is
            ;; not a debugger, it is a trap. The rail's other restarts (pass-through,
            ;; remove-both, use-value, define-method) are the ways to actually settle it.
            (setf (stage-running st) nil)
            (stage-log st "aborted — stage paused; the contact is unresolved" :warn)))
        ;; [define-method] left a template behind: hand it to the Workspace beside the
        ;; stage. Done HERE rather than inside the restart because the restart's job is
        ;; to unwind, and the pane it wants does not exist until we are back in the
        ;; browser's own world.
        (let ((p (current-pane)))
          (when (and p (eq (view-kind (pane-view p)) :stage))
            (stage-take-template p)))))))



(defun run-browser-fb (&optional root)
  "Launch the browser full-screen on /dev/fb0, rooted at ROOT. Blocks until you
   leave it (C-g at the root), then restores the text console and returns to the
   supervisor menu. Any error is caught so it cannot crash PID 1.

   ROOT defaults to the MODULE INDEX (subj-modules), not to the CL-USER package.
   Two reasons. It is a table of contents rather than one flat 300-plus-row list;
   and it is the only root that shows the WHOLE system — it groups by the file a
   definition came from, so the toolkit in its own lol.* packages (canvas,
   textview, fb, shell) is visible, where a CL-USER root cannot show it at all.
   The flat package view is still one keypress away (supervisor menu 'l'), and
   any package is reachable through the Spotter."
  (handler-case
      (progn
        (unless *bfont* (set-browser-font (lol.canvas:load-font "/cozette.lolf")))
        ;; installed by NAME, not by #', because run-browser-fb is compiled before
        ;; fb-prompt-value is defined further down this same file
        (setf *value-prompt* 'fb-prompt-value)    ; the shell asks; this backend reads
        (open-environment (or root (subj-modules)))
        (multiple-value-bind (xres yres) (lol.fb:fb-geometry)
          (let ((canvas (lol.canvas:make-canvas xres yres))
                (mouse  (open-mouse))
                (kbd    (open-keyboard)))
            (setf *cursor-x* (floor xres 2) *cursor-y* (floor yres 2)
                  *ctrl-down* nil *shift-down* nil *super-down* nil
                  *fb-canvas* canvas *fb-xres* xres *fb-yres* yres
                  *fb-mouse* mouse *fb-kbd* kbd)
            (unwind-protect
                 (lol.fb:with-graphics-console         ; KD_GRAPHICS: fbcon stops drawing
                   ;; signals OFF so Ctrl-C arrives as a byte (the C-c C-c Accept chord)
                   (with-raw-mode (0 nil)
                     (loop
                       (draw-browser canvas xres yres)
                       (when mouse (draw-cursor canvas *cursor-x* *cursor-y*))
                       (lol.fb:present-fb canvas)
                       ;; A running stage (§3b) is the one thing on screen that changes
                       ;; without being asked, so it — and only it — makes the loop
                       ;; wake on a deadline rather than sleeping until input.
                       (multiple-value-bind (kb ms kev)
                           (fbb-wait 0 mouse kbd (live-stage-timeout))
                         (when kev (read-keyboard-evdev kbd))   ; refresh Ctrl/Shift/Super first
                         (when kb
                           (multiple-value-bind (key state) (fb-read-key 0)
                             ;; The cooked tty sends a bare ESC[A for EVERY arrow and no
                             ;; byte for a held modifier — it lives only in the evdev
                             ;; state, so fold it in (Ctrl-arrow = word, Shift = select,
                             ;; Super = the spatial layer / SuperL-?).
                             (let ((state (logior state
                                                  (if *ctrl-down* 4 0)
                                                  (if *shift-down* 1 0)
                                                  (if *super-down* 64 0))))
                               (when (or (eq key :eof)
                                         (eq :quit (browser-key key state)))
                                 (return)))))
                         (when (and mouse ms)
                           (multiple-value-bind (clicked wheel) (read-mouse mouse xres yres)
                             ;; wheel first: a notch scrolls the current pane a few
                             ;; lines (up = toward the start), point untouched.
                             (unless (zerop wheel)
                               (browser-scroll (* (- wheel) +wheel-lines+)))
                             (when clicked
                               (browser-click *cursor-x* *cursor-y* *ctrl-down* *super-down*))))
                         ;; nothing was ready: the deadline fired, so advance the world
                         (unless (or kb ms kev) (tick-live-stage))))))
              (when mouse (%fbb-close mouse))
              (when kbd (%fbb-close kbd))))))
    (serious-condition (c)
      (format t "~&code browser: ~a~%" c) (finish-output))))

;;; ---- the debugger: a condition made interactive (§3) ---------------------
;;; Entered from *debugger-hook* with the stack STILL STANDING, so the restarts are
;;; live and invoking one actually transfers control. It is a nested modal loop over
;;; the same framebuffer + input the browser owns; it never returns without invoking
;;; a restart (there is no standard debugger to fall through to as PID 1).

(defvar *dbg-restart-top* 0 "screen-y of the first restart row, for click hit-testing.")
(defvar *dbg-frame-top* 0 "screen-y of the first backtrace row, for click hit-testing.")
(defvar *dbg-frame-x0* 0 "left edge of the clickable backtrace column (set at draw).")
(defvar *dbg-frame-x1* 0 "right edge of the clickable backtrace column (set at draw).")
(defparameter +dbg-row+ 17)
(defparameter +dbg-frame-row+ 15)

(defun split-lines (string)
  "STRING split on newlines into a list of lines (no trailing empty from a final \\n)."
  (let ((lines '()) (start 0))
    (loop for nl = (position #\Newline string :start start)
          do (push (subseq string start (or nl (length string))) lines)
             (if nl (setf start (1+ nl)) (return)))
    (let ((result (nreverse lines)))
      (if (and (cdr result) (string= "" (car (last result))))
          (butlast result)
          result))))

(defun draw-dbg-locals (canvas x0 y0 w h locals)
  "Render LOCALS (a list of (:name :value) plists from FRAME-LOCALS) as one 'name =
   value' line each, starting at X0,Y0 and stopping before the screen bottom. An
   unavailable value is dimmed; each line is clipped to the panel's right edge (W) so
   a wide value can't run off-screen. Placeholder when the frame has no debug vars."
  (if (null locals)
      (lol.canvas:draw-string canvas *bfont* "(no locals — no debug vars here)"
                              x0 y0 *ui-dim*)
      (let ((y y0)
            (maxc (max 1 (floor (- w x0 8) (lol.canvas:font-cw *bfont*)))))  ; chars that fit
        (dolist (v locals)
          (when (< y (- h 8))
            (let* ((val     (getf v :value))
                   (unavail (eq val :unavailable))
                   (name    (princ-to-string (getf v :name)))
                   (vstr    (if unavail "#<unavailable>" (princ-to-string val)))
                   (full    (format nil "~a = ~a" name vstr)))
              ;; name in teal, value in text (whole line dim when unavailable); a line that
              ;; overruns is clipped with ".." (the … glyph would blit as a stray &).
              (if (> (length full) maxc)
                  (lol.canvas:draw-string canvas *bfont*
                    (concatenate 'string (subseq full 0 (max 1 (- maxc 2))) "..")
                    x0 y (if unavail *ui-dim* *ui-text*))
                  (let ((pen (lol.canvas:draw-string canvas *bfont* name x0 y
                                                     (if unavail *ui-dim* *ui-accent*))))
                    (lol.canvas:draw-string canvas *bfont* (format nil " = ~a" vstr) pen y
                                            (if unavail *ui-dim* *ui-text*))))
              (incf y 15)))))))

;;; ---- the two-sided spine (§3): the signal rose, a restart unwinds -----------
(defparameter *dbg-red*      !#FF6B6B "the condition itself — the concept :condition colour.")
(defparameter *dbg-title-bg* !#5A1C1C "the alarm-red title band.")
(defparameter *dbg-title-fg* !#FFD0C0 "text on the title band.")
(defparameter +dbg-locals-h+ 132 "height of the bottom locals strip.")
(defvar *dbg-restart-x0* 8 "left edge of the clickable restart rail (set at draw).")
(defvar *dbg-restart-x1* 0 "right edge of the clickable restart rail (set at draw).")

(defun dbg-fit (s px)
  "Trim S to fit PX pixels of the browser font (…-elided)."
  (let ((maxc (max 1 (floor px (lol.canvas:font-cw *bfont*)))))
    (if (> (length s) maxc) (concatenate 'string (subseq s 0 (max 1 (- maxc 2))) "..") s)))

(defun draw-dbg-rail (canvas x y0 y1 color)
  "A SOLID vertical rail from Y0 to Y1 — a stack side that stands, drawn whole (not dashed)."
  (lol.canvas:fill-rect canvas x y0 2 (max 0 (- y1 y0)) color))

(defun draw-debugger (canvas w h condition restarts backtrace locals rsel fsel focus)
  "Paint the debugger as the two-sided spine of §3: the CONDITION is the header; the
   RESTARTS stand in an amber rail down the LEFT (they are the way out — the first
   thing to read, and invoking one is the unwind); the CALL STACK runs down the RIGHT
   with a teal rail hugging its left (the signal rose here and NOTHING unwound — every
   frame still stands) and the innermost frame marked !.
   The selected frame's locals fill a strip along the bottom. FOCUS (:restarts /
   :backtrace) says which rail the arrows drive."
  (lol.canvas:fill-rect canvas 0 0 w h *ui-bg*)
  (lol.canvas:fill-rect canvas 0 0 w 24 *dbg-title-bg*)
  (lol.canvas:draw-string canvas *bfont* "! DEBUGGER — an unhandled condition"
                          10 5 *dbg-title-fg*)
  (let ((y 36))
    ;; --- the header: what was signalled (the condition class is the headline) ---
    (lol.canvas:draw-string canvas *bfont* (format nil "~(~a~)" (type-of condition))
                            12 y *dbg-red* :scale 2)
    (incf y 32)
    ;; the report is often multi-line (draw-string has no newline handling): split it.
    (dolist (line (split-lines (princ-to-string condition)))
      (lol.canvas:draw-string canvas *bfont* (dbg-fit line (- w 24)) 12 y *ui-text*)
      (incf y 16))
    (incf y 2)
    (lol.canvas:draw-string canvas *bfont*
      "no handler claimed it — caught here with the whole stack still standing."
      12 y *ui-dim*)
    (incf y 26)
    ;; --- the triptych ---
    (let* ((split   (floor (* w 52) 100))
           (rail-x  28)                     ; amber restart rail, hard against the left
           (rest-x  46)                     ; restart text
           (frame-x (+ split 18))           ; stack text, right of the teal signal rail
           (top     y)
           (bottom  (- h +dbg-locals-h+ 6)))
      ;; column headers — the section title bright in its side colour, the key-hint dim
      (let ((pen (lol.canvas:draw-string canvas *bfont* "RESTARTS" rest-x top
                                         (if (eq focus :restarts) *ui-amber* *ui-dim*))))
        (lol.canvas:draw-string canvas *bfont* "  ·  0-9 · Enter (asks for a value) · unwinds"
                                pen top *ui-dim*))
      (let ((pen (lol.canvas:draw-string canvas *bfont* "CALL STACK" frame-x top
                                         (if (eq focus :backtrace) *ui-accent* *ui-dim*))))
        (lol.canvas:draw-string canvas *bfont* "  ·  Tab walks · M-. source" pen top *ui-dim*))
      (incf top 20)
      ;; a rule divides the two sides; a dim-amber restart rail (the unwind) faces a
      ;; mirrored SOLID teal signal rail (the stack stands) — §3's two forces, left and right.
      (lol.canvas:fill-rect canvas (- split 8) top 1 (- bottom top) *ui-rule*)
      (draw-dbg-rail canvas rail-x top (min bottom (+ top (* +dbg-row+ (length restarts))))
                     !#8A5A28)
      (draw-dbg-rail canvas split top (min bottom (+ top (* +dbg-frame-row+ (length backtrace))))
                     *ui-accent*)
      ;; --- left: the restart rail (amber — invoking is the unwind) ---
      (setf *dbg-restart-top* top *dbg-restart-x0* 8 *dbg-restart-x1* (- split 12))
      (let ((ry top))
        (loop for r in restarts for i from 0 while (< ry bottom) do
          (when (= i rsel)
            (lol.canvas:fill-rect canvas rail-x (- ry 2) (- split 8 rail-x) +dbg-row+ *ui-sel*)
            (lol.canvas:fill-rect canvas rail-x (- ry 2) 2 +dbg-row+ *ui-amber*))
          (lol.canvas:draw-string canvas *bfont*
            (dbg-fit (format nil "~d  ~@[[~(~a~)]  ~]~a" i (getf r :name) (getf r :report))
                     (- split rest-x 12))
            rest-x ry (if (= i rsel) *ui-text* *ui-dim*))
          (incf ry +dbg-row+)))
      ;; --- right: the call stack; frame 0 (innermost) is where it was signalled ---
      (setf *dbg-frame-top* top *dbg-frame-x0* (- frame-x 6) *dbg-frame-x1* (- w 8))
      (let ((fy top))
        (loop for fr in backtrace for i from 0 while (< fy bottom) do
          (when (= i fsel)
            (lol.canvas:fill-rect canvas (- frame-x 6) (- fy 2) (- w frame-x 2) +dbg-frame-row+
                                  (if (eq focus :backtrace) *ui-sel* *ui-hi*))
            (when (eq focus :backtrace)
              (lol.canvas:fill-rect canvas (- frame-x 6) (- fy 2) 2 +dbg-frame-row+ *ui-accent*)))
          (when (zerop i)                                    ; the signal frame
            (lol.canvas:draw-string canvas *bfont* "!" (- frame-x 16) fy *ui-amber*))
          (lol.canvas:draw-string canvas *bfont*
            (dbg-fit (format nil "~2d ~a" i fr) (- w frame-x 12))
            frame-x fy (if (= i fsel) *ui-text* *ui-dim*))
          (incf fy +dbg-frame-row+)))
      ;; --- bottom strip: the locals of the selected frame ---
      (let ((ly (- h +dbg-locals-h+)))
        (lol.canvas:fill-rect canvas 0 ly w 1 *ui-rule*)
        (lol.canvas:draw-string canvas *bfont*
          (dbg-fit (format nil "locals of frame ~d  ·  ~a" fsel (nth fsel backtrace)) (- w 24))
          12 (+ ly 7) *ui-accent*)
        (draw-dbg-locals canvas 12 (+ ly 28) w h locals)))))

(defun dbg-restart-at (x y n)
  "The restart-row index at screen X,Y — only within the restart rail (its left
   column), so a click in the stack never invokes a restart. NIL otherwise."
  (when (and (>= y *dbg-restart-top*) (<= *dbg-restart-x0* x *dbg-restart-x1*))
    (let ((i (floor (- y *dbg-restart-top*) +dbg-row+)))
      (when (< i n) i))))

(defun dbg-frame-at (x y n)
  "The backtrace-row index at screen X,Y — only within the frame column (its right
   half), so a click in the restart rail never selects a frame. NIL otherwise."
  (when (and (>= y *dbg-frame-top*) (<= *dbg-frame-x0* x *dbg-frame-x1*))
    (let ((i (floor (- y *dbg-frame-top*) +dbg-frame-row+)))
      (when (< i n) i))))

(defun dbg-invoke (restarts i)
  "Invoke restart I (a non-local exit — this never returns normally)."
  (let ((r (nth i restarts)))
    (when r (invoke-restart-interactively (getf r :restart)))))

(defun dbg-abort (restarts)
  "Invoke the ABORT restart (or the outermost), so the loop always leaves."
  (let ((i (or (position 'abort restarts :key (lambda (r) (getf r :name)))
               (1- (length restarts)))))
    (when (>= i 0) (dbg-invoke restarts i))))

(defparameter +src-page+ 20 "Lines the frame-source viewer moves on PageUp/PageDown.")

(defun run-frame-source-fb (name)
  "M-. from the debugger: show NAME's definition source in a read-only modal OVER the
   debugger — the erroring stack stays standing, so you look and come back. Scroll
   with up/down, PageUp/Down, or the wheel; Esc / q / Enter returns to the debugger.
   Reuses PRESENT (subj-defn NAME), so a recorded definition shows its source and a
   built-in shows whatever PRESENT falls back to (or a plain 'no source' note)."
  (let* ((canvas *fb-canvas*) (w *fb-xres*) (h *fb-yres*)
         (view (ignore-errors (present (subj-defn name))))
         (text (or (and view (view-text view))
                   (format nil "(no source recorded for ~(~a~) — a built-in or anonymous frame)"
                           name)))
         (tv (lol.textview:make-textview :font *bfont* :focus nil)))
    (lol.textview:tv-set-text tv text)
    (lol.textview:tv-geometry tv 12 32 (- w 24) (- h 44))
    (loop
      (lol.canvas:fill-rect canvas 0 0 w h *ui-bg*)
      (lol.canvas:fill-rect canvas 0 0 w 24 *ui-hi*)
      (lol.canvas:draw-string canvas *bfont*
        (format nil "source of ~(~a~)   —   scroll;  Esc / q / Enter  back to the debugger" name)
        12 5 *ui-accent*)
      (lol.textview:tv-draw canvas tv)
      (when *fb-mouse* (draw-cursor canvas *cursor-x* *cursor-y*))
      (lol.fb:present-fb canvas)
      (multiple-value-bind (kb ms kev) (fbb-wait 0 *fb-mouse* *fb-kbd*)
        (when kev (read-keyboard-evdev *fb-kbd*))
        (when kb
          (multiple-value-bind (key state) (fb-read-key 0)
            (declare (ignore state))
            (case key
              (:up        (lol.textview:tv-wheel tv -1))
              (:down      (lol.textview:tv-wheel tv 1))
              (:page-up   (lol.textview:tv-wheel tv (- +src-page+)))
              (:page-down (lol.textview:tv-wheel tv +src-page+))
              ((:escape #\q #\Q :return #\Return) (return)))))
        (when (and *fb-mouse* ms)
          (multiple-value-bind (clicked wheel) (read-mouse *fb-mouse* w h)
            (declare (ignore clicked))
            (unless (zerop wheel) (lol.textview:tv-wheel tv (* (- wheel) +wheel-lines+)))))))))

(defun dbg-prompt-value (name)
  "A modal input line over the debugger for a restart that needs a value: read the Lisp
   expression the user types (Enter accepts, Esc cancels). Returns the typed STRING, or
   NIL if cancelled. Draws its bar over the current debugger frame (already on canvas)."
  (let ((buf "") (canvas *fb-canvas*) (w *fb-xres*) (h *fb-yres*))
    (loop
      (lol.canvas:fill-rect canvas 0 (- h 44) w 44 *ui-hi*)
      (lol.canvas:fill-rect canvas 0 (- h 44) w 2 *ui-amber*)
      (lol.canvas:draw-string canvas *bfont*
        ;; "accepts", not "invokes": this line is shared now — a restart INVOKES with
        ;; the value, the inspector SETS a field to it (shell.lisp's set-field)
        (format nil "value for [~(~a~)] — an expression, Enter accepts, Esc cancels:" name)
        12 (- h 38) *ui-amber*)
      (lol.canvas:draw-string canvas *bfont* (format nil "~a_" buf) 12 (- h 18) *ui-text*)
      (lol.fb:present-fb canvas)
      (multiple-value-bind (kb ms kev) (fbb-wait 0 *fb-mouse* *fb-kbd*)
        (declare (ignore ms))
        (when kev (read-keyboard-evdev *fb-kbd*))
        (when kb
          (multiple-value-bind (key state) (fb-read-key 0)
            (declare (ignore state))
            (cond
              ((eq key :escape) (return nil))
              ((member key '(:return #\Return)) (return buf))
              ((member key '(:backspace #\Backspace #\Rubout))
               (when (plusp (length buf)) (setf buf (subseq buf 0 (1- (length buf))))))
              ((and (characterp key) (graphic-char-p key))
               (setf buf (concatenate 'string buf (string key)))))))))))

(defun fb-prompt-value (label)
  "The shell's *VALUE-PROMPT* on this backend: read one expression, modally, over
   whatever is on screen. The debugger's restart prompt and the inspector's \"set this
   field\" want exactly the same thing, so they are the same loop."
  (dbg-prompt-value label))

(defun dbg-invoke-with-value (restarts i)
  "Prompt for a value and invoke restart I with it — the way to answer a restart that
   takes an argument (USE-VALUE, our USE-HEADING, …). The typed text is READ then EVAL'd,
   so both a literal (=120=) and an expression (=(* 2 60)=) work; a read/eval error just
   cancels (the debugger must not error while reporting one). A non-local exit on success."
  (let* ((r (nth i restarts))
         (line (and r (dbg-prompt-value (getf r :name)))))
    (when (and line (plusp (length (string-trim '(#\Space #\Tab) line))))
      (multiple-value-bind (val ok)
          (ignore-errors (values (eval (read-from-string line)) t))
        (when ok (invoke-restart (getf r :restart) val))))))

(defun restart-wants-value-p (r-plist)
  "True when invoking this restart needs a value the user must type: its function takes a
   required argument AND it carries no :interactive supplier of its own (so Enter can't
   get the value any other way). Reads SBCL's internal restart accessors — the same kind
   of introspection the rest of this debugger already leans on — guarded so a probe that
   fails just means 'don't auto-prompt'."
  (let ((r (getf r-plist :restart)))
    (and r
         (ignore-errors (not (sb-kernel::restart-interactive-function r)))
         (ignore-errors
           (let ((ll (sb-introspect:function-lambda-list (sb-kernel::restart-function r))))
             (and (consp ll) (not (member (first ll) lambda-list-keywords))))))))

(defun dbg-enter (restarts i)
  "Invoke restart I the way it asks to be invoked: if it needs a value and offers no
   :interactive supplier, prompt for one (so plain Enter Just Works for USE-VALUE-style
   restarts); otherwise invoke it interactively — its own :interactive, or no arguments
   for the many restarts that take none. A non-local exit on success."
  (let ((r (nth i restarts)))
    (when r
      (if (restart-wants-value-p r)
          (dbg-invoke-with-value restarts i)
          (invoke-restart-interactively (getf r :restart))))))

(defun run-debugger-fb (condition)
  "Draw CONDITION, its restarts and backtrace on the framebuffer and let the user pick
   a restart to invoke — and walk the backtrace to inspect any frame's locals. Reuses
   the browser's live canvas + input (published in *fb-*). Returns only by invoking a
   restart (a non-local exit out of here). Tab moves focus between the restart column
   (Enter/0-9/click invoke) and the backtrace (up-dn/click walk frames); the locals of
   the selected frame are always shown beside it, and M-. opens the selected frame's
   definition source in a modal viewer without unwinding the stack."
  (let* ((restarts (restart-list condition))
         (frame-objs (backtrace-frame-objects 40 4))    ; skip our own hook machinery
         (backtrace (mapcar #'frame-label frame-objs))
         (rsel 0) (fsel 0) (focus :restarts) (pending-meta nil)
         (canvas *fb-canvas*) (w *fb-xres*) (h *fb-yres*))
    (when (null restarts) (return-from run-debugger-fb))  ; nothing to do; let it pass
    (loop
      ;; locals read fresh each iteration — cheap, and the loop only turns on input
      (let ((locals (ignore-errors (frame-locals (nth fsel frame-objs)))))
        (draw-debugger canvas w h condition restarts backtrace locals rsel fsel focus))
      (when *fb-mouse* (draw-cursor canvas *cursor-x* *cursor-y*))
      (lol.fb:present-fb canvas)
      (multiple-value-bind (kb ms kev) (fbb-wait 0 *fb-mouse* *fb-kbd*)
        (when kev (read-keyboard-evdev *fb-kbd*))
        (when kb
          (multiple-value-bind (key state) (fb-read-key 0)
           (let ((meta (or (lol.canvas:meta-p state) pending-meta)))
            (setf pending-meta nil)
            (cond
              ;; M-. — view the selected frame's definition source (stack stays up).
              ((and meta (eql key #\.))
               (let ((name (frame-defn-name (nth fsel frame-objs))))
                 (when name (run-frame-source-fb name))))
              ;; Esc = the Meta prefix (sticky); it must NOT quit the debugger.
              ((eq key :escape) (setf pending-meta t))
              ((and (lol.canvas:ctrl-p state) (member key '(#\g #\G))) (dbg-abort restarts))
              ((eq key :tab)                      ; move focus between the two lists
               (setf focus (if (eq focus :backtrace) :restarts :backtrace)))
              ((eq key :up)
               (if (eq focus :backtrace)
                   (setf fsel (max 0 (1- fsel)))
                   (setf rsel (max 0 (1- rsel)))))
              ((eq key :down)
               (if (eq focus :backtrace)
                   (setf fsel (min (max 0 (1- (length frame-objs))) (1+ fsel)))
                   (setf rsel (min (1- (length restarts)) (1+ rsel)))))
              ;; type a restart's number to select it (0-9) — the count is small
              ((and (characterp key) (char<= #\0 key #\9))
               (let ((i (- (char-code key) (char-code #\0))))
                 (when (< i (length restarts)) (setf rsel i))))
              ;; v — force a value even when Enter wouldn't ask (override an :interactive
              ;; default, or supply args to any restart)
              ((and (characterp key) (char-equal key #\v)) (dbg-invoke-with-value restarts rsel))
              ((member key '(:return #\Return)) (dbg-enter restarts rsel))))))
        (when (and *fb-mouse* ms (read-mouse *fb-mouse* w h))
          (let ((ri (dbg-restart-at *cursor-x* *cursor-y* (length restarts)))
                (fi (dbg-frame-at *cursor-x* *cursor-y* (length frame-objs))))
            (cond (ri (dbg-enter restarts ri))           ; a restart click invokes (asks if needed)
                  (fi (setf focus :backtrace fsel fi))))))))) ; a frame click selects it

;;; ---- the SuperL-? cheatsheet: every binding, on demand (§4¾) --------------
;;; The keys are deliberately NOT shown as permanent chrome — SuperL-? summons this
;;; overlay instead. Super reaches us only through the keyboard's own evdev (the
;;; cooked tty can't report it), same as Ctrl/Shift; the main loop folds *super-down*
;;; into the key state, so browser-key sees (super-p state) and calls in here.

(defparameter *cheatsheet*
  '(("Navigate"
     ("up / down"          "move the selection / caret")
     ("Enter"              "drill into the selection")
     ("right"              "focus the right column (edit / walk it)")
     ("Backspace / C-g"    "back  (C-g also un-focuses the right column)")
     ("PageUp / PageDown"  "page the list / source")
     ("mouse wheel"        "scroll the pane")
     ("q"                  "quit  (list & reference panes)"))
    ("Spatial  (Super)"
     ("Super-Space"        "the Spotter — find anything, opens a new tab")
     ("Super-1 … 9"        "jump to tab N")
     ("Super-Tab"          "cycle tabs (Shift: backwards)")
     ("Super-Enter/click"  "open the selection in a NEW tab")
     ("Super-w"            "close the current tab")
     ("Super-Left"         "back in the trail")
     ("Super-?"            "this cheatsheet")
     ("Super-q"            "quit the browser"))
    ("Edit  (source / workspace)"
     ("Tab"                "complete the symbol  (else indent)")
     ("C-c C-c"            "Accept — compile into the image")
     ("C-c C-c (inspector)" "set the selected field to a new value")
     ("C-x C-e"            "Do it — eval, value to status")
     ("C-c C-p"            "Print it — weave result inline")
     ("C-c C-i"            "Inspect it — open the value")
     ("C-c C-d"            "Debug it — eval, debugger armed"))
    ("Motion & selection"
     ("C-Left / C-Right"   "move by word")
     ("M-b / M-f"          "move by word")
     ("Shift + motion"     "extend the selection")
     ("C-Space"            "set the mark (motion extends)")
     ("C-a / C-e"          "line start / end")
     ("C-Home / C-End"     "buffer start / end")
     ("C-k"                "kill to end of line"))
    ("Jump"
     ("M-."                "jump to the definition at point")
     ("Ctrl-click"        "jump to the definition clicked"))
    ("Debugger"
     ("0-9 / up-down"      "select a restart")
     ("Tab"                "focus the backtrace, walk frames")
     ("M-."                "view the selected frame's source")
     ("Enter / click"      "invoke the selected restart")
     ("C-g"                "abort"))
    ("Stage  (§3b)"
     ("Space"              "run / pause the world")
     ("n"                  "one tick")
     ("s / a / m / t"      "spawn ship / asteroid / missile / station")
     ("Tab / click"        "select a sprite")
     ("Enter"              "inspect the selected instance")
     ("w"                  "the Workspace beside (C-c C-c defines)")
     ("r"                  "reset the stage")))
  "The full key map, grouped, for the SuperL-? overlay. Data only — DRAW-CHEATSHEET
   lays it out in two columns.")

(defun draw-cheatsheet (canvas w h)
  "Render *cheatsheet* as a full-screen two-column overlay. Group titles in amber,
   the keys in accent, the descriptions in text."
  (lol.canvas:fill-rect canvas 0 0 w h *ui-bg*)
  (lol.canvas:fill-rect canvas 0 0 w 22 *ui-hi*)
  (lol.canvas:draw-string canvas *bfont*
    "Keys  —  press any key or click to dismiss" 12 4 *ui-accent*)
  (let ((col-x (vector 40 (+ (floor w 2) 20)))
        (col-y (vector 44 44))
        (half  (ceiling (length *cheatsheet*) 2)))
    (loop for g in *cheatsheet* for i from 0
          for c = (if (< i half) 0 1)
          do (let ((x (aref col-x c)) (y (aref col-y c)))
               (lol.canvas:draw-string canvas *bfont* (car g) x y *ui-amber*)
               (incf y 20)
               (dolist (row (cdr g))
                 (lol.canvas:draw-string canvas *bfont* (first row)  (+ x 12)  y *ui-accent*)
                 (lol.canvas:draw-string canvas *bfont* (second row) (+ x 168) y *ui-text*)
                 (incf y 16))
               (incf y 12)
               (setf (aref col-y c) y)))))

(defun run-cheatsheet-fb ()
  "Draw the cheatsheet over the browser's live canvas and block until any key or
   click dismisses it. Reuses the *fb-* I/O the browser owns (like the debugger)."
  (let ((canvas *fb-canvas*) (w *fb-xres*) (h *fb-yres*))
    (when canvas
      (draw-cheatsheet canvas w h)
      (when *fb-mouse* (draw-cursor canvas *cursor-x* *cursor-y*))
      (lol.fb:present-fb canvas)
      (loop
        (multiple-value-bind (kb ms kev) (fbb-wait 0 *fb-mouse* *fb-kbd*)
          (when kev (read-keyboard-evdev *fb-kbd*))
          (when kb (fb-read-key 0) (return))            ; any key dismisses
          (when (and *fb-mouse* ms (read-mouse *fb-mouse* w h)) (return)))))))

;;; ---- the Spotter: SuperL-Space fuzzy finder (§4¾) -------------------------
;;; A modal overlay over the browser's live canvas + input, like the debugger and
;;; the cheatsheet, but LIVE: it accumulates a query string and re-filters the
;;; candidate set (spotter-candidates in present.lisp, ranked and banded by
;;; spotter-results in shell.lisp) on every keystroke. Enter opens the selected hit in
;;; a NEW trail (new-trail, shell.lisp).

(defparameter +spotter-cap+ 60 "Max results to rank/show — a few screenfuls.")

;;; The Spotter's DRAWING lives in poc/shell.lisp (draw-spotter-card), with the rest
;;; of the shell's drawing — only the modal input loop below is backend-specific.
;;; That split is what lets poc/render-frame.lisp render the palette on the host:
;;;   sbcl --script poc/render-frame.lisp modules --spotter coll
;;; The card's own scrim, hit map and dismissal box come back through
;;; lol.widgets:scrim / spotter-row-at* / spotter-outside-p*.

(defun run-spotter-fb ()
  "The Spotter (SuperL-Space): fuzzy-find any definition, package or surface and open
   it in a new trail. Modal over the browser's canvas + input; live-filters on each
   keystroke. Returns after Enter (opens the hit) or Esc (dismiss)."
  (let* ((canvas *fb-canvas*) (w *fb-xres*) (h *fb-yres*)
         (cands (ignore-errors (spotter-candidates)))
         (query "")
         (results (ignore-errors (spotter-results query cands +spotter-cap+)))
         (sel 0))
    (when canvas
      (loop
        (draw-spotter-card canvas w h query results sel)
        (when *fb-mouse* (draw-cursor canvas *cursor-x* *cursor-y*))
        (lol.fb:present-fb canvas)
        (flet ((refilter () (setf results (ignore-errors (spotter-results query cands +spotter-cap+))
                                  sel 0)))
          (multiple-value-bind (kb ms kev) (fbb-wait 0 *fb-mouse* *fb-kbd*)
            (when kev (read-keyboard-evdev *fb-kbd*))
            (when kb
              (multiple-value-bind (key state) (fb-read-key 0)
                (declare (ignore state))
                (cond
                  ((eq key :escape) (return))
                  ((member key '(:return #\Return))
                   (let ((hit (nth sel results)))
                     (when hit (new-trail (getf hit :subject))))
                   (return))
                  ((eq key :up)   (setf sel (max 0 (1- sel))))
                  ((eq key :down) (setf sel (min (max 0 (1- (length results))) (1+ sel))))
                  ((eq key :backspace)
                   (when (plusp (length query))
                     (setf query (subseq query 0 (1- (length query)))) (refilter)))
                  ((and (characterp key) (graphic-char-p key))
                   (setf query (concatenate 'string query (string key))) (refilter)))))
            (when (and *fb-mouse* ms (read-mouse *fb-mouse* w h))
              (let ((i (spotter-row-at* *cursor-x* *cursor-y*)))
                (cond ((and i (< i (length results)))
                       (new-trail (getf (nth i results) :subject)) (return))
                      ((spotter-outside-p* *cursor-x* *cursor-y*) (return)))))))))))  ; click-away dismisses
