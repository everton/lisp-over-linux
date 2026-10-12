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

;; struct pollfd is { int fd; short events; short revents; } — 8 bytes, so slot N
;; begins at N*8, events at +4, revents at +6. There is no poll(2) in sb-posix (we
;; checked; see doc/processes.org), so unlike termios this array stays ours to lay
;; out by hand — which makes it worth writing the one field that is easy to get
;; wrong exactly once.
;;
;; FD is a FULL 32-bit int. Writing only its low byte is correct for as long as
;; every descriptor is a single digit, and silently polls the WRONG descriptor the
;; first time one is not — a socket in a busy image, say. That is what this did
;; until 2026-09-20.
(defmacro %put-pollfd (fds n fd)
  "Write FD into pollfd slot N of FDS (a byte array) and ask for POLLIN."
  (let ((base (gensym "BASE")) (v (gensym "FD")))
    `(let ((,base (* ,n 8)) (,v ,fd))
       (setf (sb-alien:deref ,fds (+ ,base 0)) (ldb (byte 8 0)  ,v)
             (sb-alien:deref ,fds (+ ,base 1)) (ldb (byte 8 8)  ,v)
             (sb-alien:deref ,fds (+ ,base 2)) (ldb (byte 8 16) ,v)
             (sb-alien:deref ,fds (+ ,base 3)) (ldb (byte 8 24) ,v)
             (sb-alien:deref ,fds (+ ,base 4)) +pollin+))))

(defun fbb-ready-p (fd timeout-ms)
  "True if FD has input within TIMEOUT-MS (poll on one fd)."
  (sb-alien:with-alien ((fds (sb-alien:array sb-alien:unsigned-char 8)))
    (dotimes (i 8) (setf (sb-alien:deref fds i) 0))
    (%put-pollfd fds 0 fd)
    (%fbb-poll (sb-alien:cast fds (sb-alien:* sb-alien:unsigned-char)) 1 timeout-ms)
    (logbitp 0 (sb-alien:deref fds 6))))

(defvar *poll-extra-fds* '()
  "Fds besides input that the next poll watches — the services' pipes (Chrome's, for a
   web pane: see *FB-SERVICES*). FBB-WAIT binds it; POLL-INPUT-ONCE appends them.")
(defvar *poll-extra-ready* '() "Which of *POLL-EXTRA-FDS* the last poll found readable.")

(defun fbb-wait (kb-fd mouse-fd kev-fd &optional (timeout-ms -1) extra-fds)
  "Block until KB-FD (the console) is readable, or ANY open pointer, or ANY open
   keyboard's evdev — or until TIMEOUT-MS elapses (-1, the default, waits forever).
   Returns (values kb-ready pointer-ready keyboard-ready extra); the caller then reads with
   (read-mouse *fb-mouse* …) / (read-keyboard-evdev *fb-kbd*), which drain exactly
   the devices this call found ready. EXTRA is the list of EXTRA-FDS that are readable
   (or hung up: a child that exited closed its end, and its service must hear that) —
   the pipes of the processes the browser shows, see *FB-SERVICES*.

   MOUSE-FD and KEV-FD are IGNORED, and kept only so every loop's call reads as it
   always did. They were the one pointer and the one keyboard this used to poll,
   three fixed slots; now the device set is *INPUT-DEVICES*, which grows and shrinks
   underneath (doc/input.org §4¾), and POLL-INPUT-ONCE does the actual poll.

   HOUSEKEEPING NEVER RETURNS. A hotplug event or a device going away wakes the poll
   but is nothing the caller asked about, so we handle it and wait again — for
   whatever is left of TIMEOUT-MS, not a fresh one, so a frame deadline still lands
   on time. Returning (nil nil nil) for it instead would read, to the main loop, as
   'a deadline fired'.

   The timeout exists for the stage (§3b). Every other view in the browser is a
   function of state that only input changes, so sleeping until a key or a click is
   exactly right and costs nothing. A stage advances on its own, which means the loop
   needs a way to wake on a frame deadline instead. This is that way, and it is the
   only concession the input layer makes to a view that moves."
  (declare (ignore mouse-fd kev-fd))
  (let ((deadline (and (>= timeout-ms 0)
                       (+ (get-internal-real-time)
                          (round (* timeout-ms internal-time-units-per-second) 1000)))))
    (loop
      (let* ((left (if deadline
                       (max 0 (round (* 1000 (- deadline (get-internal-real-time)))
                                     internal-time-units-per-second))
                       -1))
             (rescan (input-rescan-timeout))
             (tmo (cond ((= left -1) rescan) ((= rescan -1) left) (t (min left rescan)))))
        (multiple-value-bind (kb ptr key)
            (let ((*poll-extra-fds* extra-fds)) (poll-input-once kb-fd tmo))
          (when (or kb ptr key *poll-extra-ready*)
            (return (values kb ptr key *poll-extra-ready*)))
          (when (and deadline (>= (get-internal-real-time) deadline))
            (return (values nil nil nil))))))))

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
         ;; 2 Insert (Shift-Insert pastes, Ctrl-Insert copies — the modifier is
         ;; folded in from evdev); 21 F10, and 34 is what the console keymap sends
         ;; for Shift-F10 (it calls it F20): the keyboard's way to the context menu
         (case n1
           (34 (values :f10 (logior state 1)))
           (t (values (case n1 (1 :home) (7 :home) (4 :end) (8 :end) (2 :insert)
                               (3 :delete) (5 :page-up) (6 :page-down) (21 :f10) (t :ignore))
                      state))))
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
               ;; ESC DEL = M-Backspace (Alt-Backspace on the console): kill a word back
               ((eql b1 127) (values :backspace 8))
               (t (values :ignore 0))))
           (values :escape 0)))
      ((member c '(13 10)) (values :return 0))
      ;; Backspace sends DEL (127); Ctrl-Backspace and C-h send BS (8), so BS carries
      ;; the Ctrl bit. That leaves a DEL read while Ctrl is held meaning C-/, which the
      ;; console's default keymap also sends as DEL — see the main loop.
      ((= c 127)           (values :backspace 0))
      ((= c 8)             (values :backspace 4))
      ((= c 31)            (values #\_ 4))                    ; C-_ (Ctrl-minus): undo
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
(defvar *alt-down* nil "Is an Alt key held? Tracked from the keyboard's evdev, for
  Alt-click — the middle button for a pointer that has none.")

(defun scan-input-devices ()
  "EVERY device in /proc/bus/input/devices, as a list of plists:
     (:node \"event17\" :name \"…\" :phys \"…\" :sysfs \"…\" :ev EV :prop PROP :rel REL :abs ABS)
   EV is the 'B: EV=' capability mask and PROP the 'B: PROP=' property mask. REL and
   ABS are the LOWEST word of the 'B: REL=' / 'B: ABS=' bitmaps — bit 0 of each is
   REL_X / ABS_X, which is what says 'this device can point'. A device without a
   'Handlers=eventN' has no node we could open and is left out.

   We read the EV mask rather than grepping Handlers for 'mouse'/'kbd': mousedev
   isn't loaded, so a plain PS/2 mouse shows only 'Handlers=eventN' with no 'mouseN'
   token. The EV bits are what actually classify the device.

   PROP is the second half of the story and the reason a touchpad works at all:
   EV says a device emits absolute positions, PROP says whether those positions
   are *on the screen* (INPUT_PROP_DIRECT — a tablet) or *on a small pad under
   your hand* (INPUT_PROP_POINTER / INPUT_PROP_BUTTONPAD). Both print here, so
   neither costs an open(2).

   A bitmap prints as space-separated 64-bit words, MOST significant first, so the
   lowest word — the one holding bit 0 — is the LAST token on the line, not the
   first (doc/input.org §4, 'one parsing subtlety'). For EV and PROP there is only
   ever one word, so first and last agree.

   This used to be SCAN-INPUT-DEVICE, singular: it returned the first device a
   predicate liked, and the caller opened that one alone. That is the bug the
   multi-device section below exists to fix (doc/input.org §4¾)."
  (let ((out '()))
    (ignore-errors
      (with-open-file (s "/proc/bus/input/devices" :if-does-not-exist nil)
        (when s
          (let ((b '()))
            (labels ((after (line key)
                       (let ((p (search key line)))
                         (and p (string-trim " " (subseq line (+ p (length key)))))))
                     (low-word (line key)     ; the LAST hex word of a bitmap line
                       (let ((v (after line key)))
                         (when v
                           (let ((sp (position #\Space v :from-end t)))
                             (ignore-errors
                               (parse-integer v :start (if sp (1+ sp) 0) :radix 16))))))
                     (finish ()                ; end of a device block
                       (when (getf b :node)
                         (push (list :node (getf b :node) :name (getf b :name)
                                     :phys (getf b :phys) :sysfs (getf b :sysfs)
                                     :ev (or (getf b :ev) 0) :prop (or (getf b :prop) 0)
                                     :rel (or (getf b :rel) 0) :abs (or (getf b :abs) 0))
                               out))
                       (setf b '())))
              (loop for line = (read-line s nil nil) do
                (cond
                  ((null line) (finish) (return))            ; EOF: flush the last block
                  ((zerop (length line)) (finish))           ; blank: block boundary
                  ((< (length line) 3))
                  ((string= (subseq line 0 2) "N:")
                   (setf (getf b :name) (string-trim "\"" (or (after line "Name=") ""))))
                  ((string= (subseq line 0 2) "P:") (setf (getf b :phys) (after line "Phys=")))
                  ((string= (subseq line 0 2) "S:") (setf (getf b :sysfs) (after line "Sysfs=")))
                  ((string= (subseq line 0 2) "H:")
                   (let ((p (search "event" line)))
                     (when p
                       (setf (getf b :node)
                             (subseq line p (or (position #\Space line :start p)
                                                (length line)))))))
                  ((search "PROP=" line) (setf (getf b :prop) (low-word line "PROP=")))
                  ((search "EV=" line)   (setf (getf b :ev)   (low-word line "EV=")))
                  ((search "REL=" line)  (setf (getf b :rel)  (low-word line "REL=")))
                  ((search "ABS=" line)  (setf (getf b :abs)  (low-word line "ABS="))))))))))
    (nreverse out)))

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
(defvar *tap-peak-z* nil
  "The highest ABS_PRESSURE this contact reached, or NIL when the pad reports none.")
(defvar *last-touch* nil
  "The last contact as the tap rule saw it — (:ms :peak :tap) — kept for TUNING:
   print it in the Workspace after a tap that did or did not click, and you have
   the two numbers *TAP-MIN-US* and *TAP-MIN-PRESSURE* are compared against.")
(defparameter *pad-right-zone* '(3/5 . 4/5)
  "Where a clickpad's physical press means RIGHT: the bottom-right corner, right of
   this fraction of the pad's width and below this fraction of its height. The
   software-button area every laptop with a buttonless pad uses for it.")

(defun pad-press-button (x y)
  "Which button a press on the pointer means, given the finger at pad units X,Y.
   A mouse has real buttons, so its BTN_LEFT is just :LEFT. A clickpad has ONE
   physical button, and the fingers on it say which was meant:
     three fingers                 -> :RIGHT
     two fingers                   -> :MIDDLE  (the halo, X's paste)
     one, in the bottom-right zone -> :RIGHT   (*PAD-RIGHT-ZONE*)
     one, anywhere else            -> :LEFT
   The finger count is known by then: a Synaptics packet reports BTN_TOOL_* before
   its buttons (synaptics_report_buttons comes last in synaptics_process_packet)."
  (cond ((not *pointer-pad-p*) :left)
        ((>= *pad-fingers* 3) :right)
        ((= *pad-fingers* 2) :middle)
        ((and x y
              (> x (+ *abs-x-min* (* (car *pad-right-zone*) (- *abs-x-max* *abs-x-min*))))
              (> y (+ *abs-y-min* (* (cdr *pad-right-zone*) (- *abs-y-max* *abs-y-min*)))))
         :right)
        (t :left)))

(defvar *button-down* nil
  "Is the left button held right now? On a clickpad that is the pad PHYSICALLY
   pressed. Pointer motion while it is down is a drag (BROWSER-DRAG).")

(defvar *tap-max-us* 180000
  "The longest contact still counted as a tap, in microseconds. Past it you were
   resting a finger, not tapping — 180ms is what libinput settled on.")
(defvar *tap-min-us* 30000
  "The SHORTEST contact counted as a tap. A finger brushing the pad on its way
   somewhere is over in one or two packets — a Synaptics pad sends one every
   12.5ms — and a deliberate tap is not.")
(defvar *tap-min-pressure* 60
  "How hard a tap must press, as the peak ABS_PRESSURE of the contact (0..255 on a
   Synaptics pad). The kernel already calls anything over 30 a touch; Synaptics'
   own interfacing guide puts a very light finger at about 30 and a normal one at
   about 80, and a graze lives between the two. So a contact that never pressed
   past this is not a click, however brief. A pad that reports no pressure at all
   skips the test. Tune it against *LAST-TOUCH*.")
(defvar *tap-travel* 1/40
  "How far the finger may wander and still tap, as a fraction of the pad's own axis
   range. A tap is a contact that did not travel; this is the slop that forgives a
   finger rolling slightly as it lifts.")

(defvar *pad-speed* 3/2
  "How far the cursor travels for a full sweep of the pad, in screens. 1 means the
   pad maps one-to-one onto the display, which is accurate but makes you cross the
   whole pad to cross the whole screen; 3/2 asks for about two thirds of a sweep.")

(defvar *mouse-speed* 1
  "Screen pixels per mouse count, for a RELATIVE pointer — a USB or PS/2 mouse, a
   TrackPoint. 1 is what the console and X's flat profile do, and a 1000-dpi mouse
   then crosses a 1920px screen in about 5cm. Any rational works (3/2, 2/3); the
   fraction is carried like the pad's, so a slow hand still moves the cursor. There is
   no acceleration curve: set it in the Workspace if your mouse feels slow.")
(defvar *mouse-rx* 0 "Sub-pixel remainder of relative motion (with *MOUSE-SPEED* not 1).")
(defvar *mouse-ry* 0)

(defun pointer-pad-p (ev prop)
  "EV/PROP describe a touchpad: absolute, not relative, and a POINTER/BUTTONPAD."
  (and (logbitp 3 ev) (not (logbitp 2 ev))
       (or (logbitp 0 prop) (logbitp 2 prop))))

(defun input-device-kind (dev)
  "What a scanned device (a plist from SCAN-INPUT-DEVICES) is to us, or NIL:

     :MOUSE    EV_REL and REL_X — any mouse, a TrackPoint, or the relative node a
               HID touchpad exposes while it is still in mouse mode
     :PAD      EV_ABS and ABS_X, no REL_X, and PROP says POINTER or BUTTONPAD — a
               touchpad, whose positions are on the pad, not the screen (§4½)
     :TABLET   EV_ABS and ABS_X otherwise — QEMU's usb-tablet; positions ARE screen
     :KEYBOARD EV_KEY and EV_REP (autorepeat) — the EV_REP bit is what separates a
               keyboard from the Power Button (also a 'kbd' handler, EV=3)

   POINTING IS ASKED OF THE AXIS, NOT THE EVENT TYPE. 'Has EV_REL' was the old rule,
   and a USB keyboard's 'Consumer Control' node passes it: it has EV_REL for a
   horizontal-scroll usage (REL_HWHEEL) and EV_ABS for volume, and no X axis at all.
   Asking for REL_X / ABS_X (bit 0 of the low word) keeps it out.

   A device that can point AND autorepeat is classified as a pointer; READ-MOUSE
   also tracks the modifiers it reports, so nothing is lost but its Menu key."
  (let ((ev (getf dev :ev)) (prop (getf dev :prop))
        (rel (getf dev :rel)) (abs (getf dev :abs)))
    (cond ((and (logbitp 2 ev) (logbitp 0 rel)) :mouse)
          ((and (logbitp 3 ev) (logbitp 0 abs))
           (if (or (logbitp 0 prop) (logbitp 2 prop)) :pad :tablet))
          ((and (logbitp 1 ev) (logbitp 20 ev)) :keyboard)
          (t nil))))

(defun open-evdev (path)
  "Open an evdev node non-blocking: (values FD NIL), or (values NIL ERRNO) on
   failure — ERRNO 13 (EACCES) is the interesting one, a node that devtmpfs just made
   root's and lol-rootd has not yet handed to the user (doc/input.org §4¾)."
  (when path
    (let ((fd (%fbb-open path (logior +o-rdonly+ +o-nonblock+))))
      (if (>= fd 0) (values fd nil) (values nil (sb-alien:get-errno))))))

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

(defun note-modifier-key (code val)
  "One EV_KEY record against the held-modifier state: Ctrl / Shift / Super / Alt in
   *CTRL-DOWN* / *SHIFT-DOWN* / *SUPER-DOWN* / *ALT-DOWN* (value 1 press, 2 repeat —
   both 'down' — 0 release). Returns :MENU on a Menu-key press, else NIL. Shared by
   READ-KEYBOARD-EVDEV and READ-MOUSE, so a device that is both (some wireless combo
   receivers) keeps its modifiers too. While the multi-device reader is draining a
   device these specials are THAT DEVICE's own (WITH-DEVICE-STATE)."
  (cond
    ((or (= code 29) (= code 97))   (setf *ctrl-down* (plusp val)) nil)   ; KEY_LEFTCTRL / RIGHTCTRL
    ((or (= code 42) (= code 54))   (setf *shift-down* (plusp val)) nil)  ; KEY_LEFTSHIFT / RIGHTSHIFT
    ((or (= code 125) (= code 126)) (setf *super-down* (plusp val)) nil)  ; KEY_LEFTMETA / RIGHTMETA (Super)
    ((or (= code 56) (= code 100))  (setf *alt-down* (plusp val)) nil)    ; KEY_LEFTALT / RIGHTALT
    ((and (= code 127) (= val 1))   :menu)                                ; KEY_COMPOSE: the Menu key
    (t nil)))

(defun read-keyboard-evdev (fd)
  "Drain a keyboard's evdev queue, tracking Ctrl / Shift / Super / Alt held state
   (NOTE-MODIFIER-KEY). Text still arrives through the cooked tty (fd 0); this fd
   exists only for the modifier state the tty can't report — a bare modifier
   produces no byte there. Super is what makes the spatial layer (and its SuperL-?
   cheatsheet) reachable on the bare console at all.

   FD is one keyboard's fd — or anything else (the :KEYBOARDS that *FB-KBD* holds),
   meaning every keyboard the last FBB-WAIT found readable (READ-KEYBOARDS). The
   second form is what the browser's loops pass; the first is for replaying a file of
   records by hand.

   Returns (values MENU COUNT): MENU is :MENU when the Menu key (KEY_COMPOSE, the
   one beside Right Ctrl) was pressed — the console keymap sends no byte for it
   either, and it is the keyboard's way to the right-button menu; COUNT is how many
   records were read."
  (unless (integerp fd) (return-from read-keyboard-evdev (read-keyboards)))
  (sb-alien:with-alien ((e (sb-alien:array sb-alien:unsigned-char 24)))
    (loop with menu = nil and count = 0 do
      (let ((n (%fbb-read fd (sb-alien:cast e (sb-alien:* sb-alien:unsigned-char)) 24)))
        (when (< n 24) (return (values menu count)))
        (incf count)
        (let ((type (logior (sb-alien:deref e 16) (ash (sb-alien:deref e 17) 8)))
              (code (logior (sb-alien:deref e 18) (ash (sb-alien:deref e 19) 8)))
              (val  (logior (sb-alien:deref e 20) (ash (sb-alien:deref e 21) 8))))
          (when (and (= type 1) (eq :menu (note-modifier-key code val)))   ; EV_KEY
            (setf menu :menu)))))))

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
    (if (and (>= *pad-fingers* 2) (not *button-down*))
        ;; two fingers = the wheel this hardware does not have. Not while the pad is
        ;; pressed, though: the thumb holding the press down is a finger too, and
        ;; the other one is dragging a selection, not scrolling. Dragging them
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
   (values CLICKED WHEEL BUTTON): CLICKED is T on a left-button press — or, on a touchpad,
   on a TAP, which reports no button at all (see *TAP-MAX-US*); WHEEL is the summed
   scroll notches this drain (REL_WHEEL, + = up / away from the user, - = down), 0
   when there was none. BUTTON is :LEFT, :RIGHT or :MIDDLE for the press this drain
   saw (or NIL) — on a clickpad decided by PAD-PRESS-BUTTON. Callers that want only
   the left click keep working on the first value, and a right or middle press is
   NOT a click there, so the modal overlays ignore those buttons.

   Three kinds of pointer, sorted out once by OPEN-MOUSE:

   - EV_REL — a mouse. The values are deltas; add them.
   - EV_ABS, direct — a usb-tablet. The values are positions on the screen; scale
     them from the device's *own* range (EVDEV-ABS-RANGE), not an assumed one.
   - EV_ABS, INPUT_PROP_POINTER — a laptop touchpad. The values are positions on
     the pad, which is not the screen; hand them to PAD-MOTION.

   Absolute axes are gathered across the packet and applied at SYN_REPORT rather
   than record by record, so X and Y move together and one drag is one delta.

   FD is one pointer's fd — the form a replayed file of records uses, reading the
   global state — or anything else (the :POINTERS that *FB-MOUSE* holds), meaning
   every pointer the last FBB-WAIT found readable, each read against its OWN state
   (READ-POINTERS, doc/input.org §4¾). A fourth value counts the records read."
  (unless (integerp fd) (return-from read-mouse (read-pointers xres yres)))
  (let ((clicked nil) (wheel 0) (nx nil) (ny nil) (button nil) (count 0))
    (sb-alien:with-alien ((e (sb-alien:array sb-alien:unsigned-char 24)))
      (loop
        (let ((n (%fbb-read fd (sb-alien:cast e (sb-alien:* sb-alien:unsigned-char)) 24)))
          (when (< n 24) (return))              ; nothing more (nonblocking)
          (incf count)
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
                        (setf *cursor-y* (abs-to-screen val *abs-y-min* *abs-y-max* yres))))
                 ;; ABS_PRESSURE: how hard the finger presses. Only the tap rule
                 ;; reads it, as the peak over the contact (*TAP-MIN-PRESSURE*).
                 (24 (when *tap-down-us*
                       (setf *tap-peak-z* (max val (or *tap-peak-z* 0)))))))
              ((= type 2)                       ; EV_REL
               ;; deltas, times *MOUSE-SPEED*, the fraction carried like the pad's
               (case code
                 (0 (multiple-value-bind (whole rest) (truncate (+ *mouse-rx* (* val *mouse-speed*)))
                      (setf *mouse-rx* rest
                            *cursor-x* (max 0 (min (1- xres) (+ *cursor-x* whole))))))
                 (1 (multiple-value-bind (whole rest) (truncate (+ *mouse-ry* (* val *mouse-speed*)))
                      (setf *mouse-ry* rest
                            *cursor-y* (max 0 (min (1- yres) (+ *cursor-y* whole))))))
                 (8 (incf wheel val))))          ; REL_WHEEL: +1 up / -1 down
              ((= type 1)                       ; EV_KEY — a button is just a key
               (case code
                 (#x110 (if (= val 1)                            ; BTN_LEFT press:
                            ;; on a clickpad, which button it means depends on the
                            ;; fingers; only a LEFT press clicks and can drag
                            (let ((b (pad-press-button (or nx *pad-x*) (or ny *pad-y*))))
                              (setf button b)
                              (when (eq b :left) (setf clicked t *button-down* t)))
                            ;; released: the drag, if any, is over
                            (when (= val 0) (setf *button-down* nil)))
                        ;; a real press is not also a tap — a clickpad reports both
                        ;; the button and the finger resting on it
                        (setf *tap-live* nil))
                 (#x111 (when (= val 1) (setf button :right)))   ; BTN_RIGHT
                 (#x112 (when (= val 1) (setf button :middle)))  ; BTN_MIDDLE
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
                               *tap-x* nx *tap-y* ny *tap-peak-z* nil
                               *tap-live* *pointer-pad-p*)
                         ;; it left: a candidate that was deliberate enough IS the
                         ;; click — long enough not to be a brush, short enough not
                         ;; to be a rest, and pressed hard enough not to be a graze.
                         ;; No button was ever reported — on a pad, that is the whole
                         ;; gesture, and the reason libinput exists elsewhere.
                         (let* ((dt (and *tap-down-us*
                                         (- (evdev-time-us
                                             (sb-alien:cast e (sb-alien:* sb-alien:unsigned-char)))
                                            *tap-down-us*)))
                                (tap (and *tap-live* dt
                                          (<= *tap-min-us* dt *tap-max-us*)
                                          (or (null *tap-peak-z*)
                                              (>= *tap-peak-z* *tap-min-pressure*)))))
                           (when *tap-down-us*
                             (setf *last-touch* (list :ms (and dt (round dt 1000))
                                                      :peak *tap-peak-z*
                                                      :tap (and tap t))))
                           (when tap (setf clicked t button :left))
                           (setf *pad-fingers* 0 *tap-live* nil *tap-peak-z* nil
                                 *tap-down-us* nil *tap-x* nil *tap-y* nil))))
                    ;; a second finger means scroll, or a right-click we have no
                    ;; button for — either way this contact stopped being a tap
                    (#x145 (when (plusp val) (setf *pad-fingers* 1)))      ; one finger
                    (#x14d (when (plusp val) (setf *pad-fingers* 2 *tap-live* nil)))   ; two
                    (#x14e (when (plusp val) (setf *pad-fingers* 3 *tap-live* nil))))) ; three
                 ;; any other key: a modifier, if this pointer is also a keyboard
                 (t (note-modifier-key code val))))
              ((and (= type 0) (= code 0) *pointer-pad-p*)   ; EV_SYN/SYN_REPORT
               (incf wheel (pad-motion nx ny xres yres))
               (setf nx nil ny nil)))))))
    (values clicked wheel button count)))

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
    "KWWWWWWWWWK"
    "KWWWWWWKKKKK"
    "KWWWKWWK"
    "KWWK KWWK"
    "KWK  KWWK"
    "KK    KWWK"
    "K     KWWK"
    "       KWWK"
    "       KWWK"
    "        KK")
  "The pointer as a tiny bitmap — K = black edge, W = white fill, space =
   transparent. A north-west arrow: the arrowhead, its lower-left barb, and the
   tail below it.

   The tail LEANS: it steps one pixel right every two rows, along the arrow's own
   axis, the way the classic X and Windows arrows draw it. The first version stood
   it straight up (four columns, dead vertical), and against a head that points up
   and to the left a vertical tail reads as bent — the base of the arrow looked
   twisted to the left. Drawn along the axis, head and tail are one straight object.")

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

;; The live I/O the running browser owns, published so the debugger (entered from a
;; HANDLER-BIND handler, mid-loop — see WORKSPACE-DEBUG-IT; nothing here sets
;; *DEBUGGER-HOOK*) can draw to the same screen and read the same input without
;; threading them through every call.
(defvar *fb-canvas* nil) (defvar *fb-xres* 0) (defvar *fb-yres* 0)
(defvar *fb-mouse* nil
  ":POINTERS while at least one pointer is open, else NIL. It was a single fd until
   the environment learned to read every device at once (doc/input.org §4¾); the
   loops still say (read-mouse *fb-mouse* w h), and READ-MOUSE reads a non-fd as
   'every pointer that is ready'. Its truth still means 'there is a pointer to draw'.")
(defvar *fb-kbd* nil
  ":KEYBOARDS while at least one keyboard's evdev is open, else NIL — as *FB-MOUSE*.")

;;; ---- every input device at once, and the ones that come later (§4¾) ------
;;;
;;; THE BUG THIS REPLACES. The environment used to open ONE pointer and ONE
;;; keyboard: FIND-MOUSE-DEVICE ranked the candidates (tablet > mouse > touchpad)
;;; and returned the best, FIND-KEYBOARD-DEVICE returned the first keyboard, and
;;; both ran once, when the environment started. On the laptop with an external
;;; mouse and keyboard that loses three ways at once:
;;;
;;;   - the external mouse and the touchpad are BOTH candidates and only one wins —
;;;     whichever ranks higher, or comes first in /proc/bus/input/devices; the other
;;;     is never read at all;
;;;   - the first keyboard is the laptop's own (i8042 probes before USB), so the
;;;     external keyboard's Ctrl / Super / Alt are never seen — typing still works,
;;;     because TEXT comes from the console, which merges every keyboard, but
;;;     Ctrl-click and every SuperL- binding on it are dead;
;;;   - a device plugged in after the environment started is never opened.
;;;
;;; THE FIX. Every keyboard and every pointer is open at once, each an INPUT-DEV with
;;; its own state; FBB-WAIT polls all of them; a watch on /dev/input (inotify, tag
;;; #26) re-scans when a node appears or changes owner; a device that disappears
;;; reports POLLHUP and is closed and forgotten.
;;;
;;; PER-DEVICE STATE, WITHOUT REWRITING THE DECODER. READ-MOUSE and PAD-MOTION keep
;;; their state in special variables (*pad-x*, *tap-live*, *button-down*, ...), which
;;; was right for one device and is wrong for two: a touchpad's remembered finger
;;; position must not be differenced against a mouse's packet. Rather than thread a
;;; struct through every line of the decoder, WITH-DEVICE-STATE rebinds those
;;; specials (PROGV) to the device's own saved values for the length of one drain and
;;; saves them back after. The decoder does not know there is more than one device,
;;; which is also why a file of replayed records still goes through it unchanged.
;;;
;;; WHAT IS SHARED, AND WHY. There is ONE cursor (*cursor-x/y*): every pointer moves
;;; the same arrow, as in X. Held buttons and modifiers are each device's own, and
;;; the globals the browser reads are their OR: Ctrl held on the external keyboard
;;; makes a click on the touchpad a Ctrl-click; a press held on the pad while the
;;; mouse moves is a drag. That is not a choice we had freely — the console's own
;;; keyboard handler keeps ONE shift_state for all keyboards (drivers/tty/vt/
;;; keyboard.c), so Ctrl on one keyboard and 'a' on another already reach the tty as
;;; C-a, and anything else here would disagree with the text we receive. What
;;; per-device state buys is RELEASE: a device that goes away takes its held keys
;;; with it, so unplugging a keyboard with Ctrl down cannot leave Ctrl stuck.

(defparameter +input-dev-vars+
  '(*pointer-abs-p* *pointer-pad-p* *abs-x-min* *abs-x-max* *abs-y-min* *abs-y-max*
    *pad-x* *pad-y* *pad-fingers* *pad-rx* *pad-ry* *pad-scroll*
    *tap-down-us* *tap-x* *tap-y* *tap-live* *tap-peak-z*
    *button-down* *mouse-rx* *mouse-ry*
    *ctrl-down* *shift-down* *super-down* *alt-down*)
  "The special variables that are really ONE DEVICE's state. WITH-DEVICE-STATE binds
   them to a device's saved values while that device is read. The tuning knobs
   (*pad-speed*, *tap-min-pressure*, *mouse-speed*) and *last-touch* are NOT here:
   they are settings and a diagnostic, the same for every device.")

(defstruct (input-dev (:constructor %make-input-dev))
  "One open evdev node: what it is, its fd, and its own copy of +INPUT-DEV-VARS+."
  node            ; "event17"
  sysfs           ; the kernel's unique path for it — "eventN" alone can be reused
  name            ; "PixArt Microsoft USB Optical Mouse"
  phys            ; where it hangs: "usb-0000:00:14.0-8.3/input0", "isa0060/serio1/input0"
  kind            ; :mouse :pad :tablet :keyboard (INPUT-DEVICE-KIND)
  fd
  state           ; values of +INPUT-DEV-VARS+, in that order
  ready           ; did the last poll say it has records waiting?
  (records 0)     ; input_event records read from it — the diagnostic view shows these
  (clicks 0))     ; left clicks it produced (a press, or a tap on a pad)

(defvar *input-devices* '() "Every input device the environment has open (INPUT-DEV).")
(defvar *input-watch-fd* nil "The inotify fd watching /dev/input, or NIL (then we rescan on a timer).")
(defvar *input-rescan-due* nil "Internal-real-time of the next scheduled rescan, or NIL.")
(defvar *input-open-failures* '()
  "Devices we wanted and could not open, as (NODE NAME ERRNO) — shown in the
   diagnostic view, because 'my mouse does nothing' and 'my mouse is not mine
   (EACCES)' look identical from the chair.")
(defvar *input-log* '() "Recent hotplug events, newest first: (SECONDS-SINCE-BOOT TEXT).")
(defvar *input-wakeups* 0
  "Times FBB-WAIT's poll(2) returned. check-input.lisp samples it twice across an idle
   stretch to prove a dropped device does not leave the loop spinning.")

(defun pointer-kind-p (kind) (member kind '(:mouse :pad :tablet)))

(defun input-log (fmt &rest args)
  (push (list (round (get-internal-real-time) internal-time-units-per-second)
              (apply #'format nil fmt args))
        *input-log*)
  (when (> (length *input-log*) 20) (setf *input-log* (subseq *input-log* 0 20))))

(defmacro with-device-state ((dev) &body body)
  "Run BODY with +INPUT-DEV-VARS+ bound to DEV's own values, and save them back."
  (let ((d (gensym "DEV")))
    `(let ((,d ,dev))
       (progv +input-dev-vars+ (input-dev-state ,d)
         (unwind-protect (progn ,@body)
           (setf (input-dev-state ,d) (mapcar #'symbol-value +input-dev-vars+)))))))

(defun device-var (dev symbol)
  "DEV's saved value of SYMBOL, one of +INPUT-DEV-VARS+."
  (nth (position symbol +input-dev-vars+) (input-dev-state dev)))

(defun fresh-device-state (fd kind)
  "The starting values of +INPUT-DEV-VARS+ for a device of KIND open on FD: nothing
   held, no finger remembered, and — for an absolute device — the axis ranges the
   device itself reports (EVDEV-ABS-RANGE), which is what OPEN-MOUSE used to do."
  (progv +input-dev-vars+ (make-list (length +input-dev-vars+))
    (setf *pointer-abs-p* (and (member kind '(:pad :tablet)) t)
          *pointer-pad-p* (eq kind :pad)
          *abs-x-min* 0 *abs-x-max* 32767 *abs-y-min* 0 *abs-y-max* 32767
          *pad-fingers* 0 *pad-rx* 0 *pad-ry* 0 *pad-scroll* 0
          *mouse-rx* 0 *mouse-ry* 0)
    (when *pointer-abs-p*
      (multiple-value-setq (*abs-x-min* *abs-x-max*) (evdev-abs-range fd 0))
      (multiple-value-setq (*abs-y-min* *abs-y-max*) (evdev-abs-range fd 1)))
    (mapcar #'symbol-value +input-dev-vars+)))

(defun sync-input-globals ()
  "Recompute what the browser reads from what the devices hold: each held modifier
   and *BUTTON-DOWN* is the OR over every open device (see the section header), and
   *FB-MOUSE* / *FB-KBD* say whether any pointer / keyboard is open at all."
  (flet ((any (sym) (and (some (lambda (d) (device-var d sym)) *input-devices*) t)))
    (setf *ctrl-down*   (any '*ctrl-down*)
          *shift-down*  (any '*shift-down*)
          *super-down*  (any '*super-down*)
          *alt-down*    (any '*alt-down*)
          *button-down* (any '*button-down*)
          *fb-mouse* (and (some (lambda (d) (pointer-kind-p (input-dev-kind d))) *input-devices*)
                          :pointers)
          *fb-kbd*   (and (some (lambda (d) (eq :keyboard (input-dev-kind d))) *input-devices*)
                          :keyboards))))

(defun drop-input-dev (dev why)
  "Close DEV and forget it — it was unplugged (POLLHUP) or vanished from the list.
   Its held buttons and modifiers go with it (SYNC-INPUT-GLOBALS)."
  (ignore-errors (%fbb-close (input-dev-fd dev)))
  (setf *input-devices* (remove dev *input-devices*))
  (input-log "~(~a~) ~a: ~a (~a)" (input-dev-kind dev) why (input-dev-name dev) (input-dev-node dev))
  (sync-input-globals))

(defun rescan-input-devices ()
  "Make the open set match /proc/bus/input/devices: open every keyboard and pointer
   not yet open, close any open one the kernel no longer lists. Cheap — one small
   proc file — so it runs on every hotplug event and costs nothing to repeat.
   Returns the list of open devices."
  (let ((seen (scan-input-devices)))
    ;; gone from the list: closed first, so a reused eventN cannot shadow a new device
    (dolist (d *input-devices*)
      (unless (find-if (lambda (b) (and (equal (getf b :node) (input-dev-node d))
                                        (equal (getf b :sysfs) (input-dev-sysfs d))))
                       seen)
        (drop-input-dev d "removed")))
    (setf *input-open-failures* '())
    (dolist (b seen)
      (let ((kind (input-device-kind b)))
        (when (and kind
                   (not (find-if (lambda (d) (and (equal (getf b :node) (input-dev-node d))
                                                  (equal (getf b :sysfs) (input-dev-sysfs d))))
                                 *input-devices*)))
          (multiple-value-bind (fd errno)
              (open-evdev (format nil "/dev/input/~a" (getf b :node)))
            (if fd
                (let ((dev (%make-input-dev :node (getf b :node) :sysfs (getf b :sysfs)
                                            :name (getf b :name) :phys (getf b :phys)
                                            :kind kind :fd fd)))
                  (setf (input-dev-state dev) (fresh-device-state fd kind))
                  (setf *input-devices* (append *input-devices* (list dev)))
                  (input-log "~(~a~) added: ~a (~a)" kind (getf b :name) (getf b :node)))
                (push (list (getf b :node) (getf b :name) errno) *input-open-failures*))))))
    (sync-input-globals)
    *input-devices*))

;;; inotify(7): the kernel tells us when /dev/input changes. Three events matter —
;;; IN_CREATE (devtmpfs made a node: a device arrived), IN_DELETE (one went), and
;;; IN_ATTRIB (lol-rootd chowned a node to the user, so one we could not open a
;;; moment ago now opens: as the user, CREATE alone finds a root-owned 0600 node).
(sb-alien:define-alien-routine ("inotify_init1" %inotify-init1) sb-alien:int (flags sb-alien:int))
(sb-alien:define-alien-routine ("inotify_add_watch" %inotify-add-watch) sb-alien:int
  (fd sb-alien:int) (path sb-alien:c-string) (mask sb-alien:unsigned-int))

(defconstant +in-attrib+ #x4)    ; linux/include/uapi/linux/inotify.h
(defconstant +in-create+ #x100)
(defconstant +in-delete+ #x200)
(defconstant +in-nonblock+ #x800) ; = O_NONBLOCK
(defparameter +input-rescan-retry-ms+ 400
  "After a hotplug event, scan again this much later: the node, its /proc line and
   its owner change in quick succession, and one of them may not have landed yet.")
(defparameter +input-rescan-poll-ms+ 2000
  "Without inotify, how often to rescan — the fallback, so hotplug still works.")

(defun ms->itu (ms) (round (* ms internal-time-units-per-second) 1000))

(defun open-input-devices ()
  "Open every input device and start watching /dev/input for more."
  (unless *input-watch-fd*
    (let ((fd (%inotify-init1 +in-nonblock+)))
      (when (>= fd 0)
        (if (>= (%inotify-add-watch fd "/dev/input" (logior +in-create+ +in-delete+ +in-attrib+)) 0)
            (setf *input-watch-fd* fd)
            (%fbb-close fd)))))
  (setf *input-rescan-due* (and (null *input-watch-fd*)
                                (+ (get-internal-real-time) (ms->itu +input-rescan-poll-ms+))))
  (rescan-input-devices))

(defun close-input-devices ()
  (dolist (d *input-devices*) (ignore-errors (%fbb-close (input-dev-fd d))))
  (setf *input-devices* '())
  (when *input-watch-fd* (ignore-errors (%fbb-close *input-watch-fd*)) (setf *input-watch-fd* nil))
  (sync-input-globals))

(defun drain-input-watch ()
  "Read and discard every pending inotify event. We never look inside: any change to
   /dev/input means 'rescan', and the rescan is the source of truth."
  (sb-alien:with-alien ((buf (sb-alien:array sb-alien:unsigned-char 4096)))
    (loop while (plusp (%fbb-read *input-watch-fd*
                                  (sb-alien:cast buf (sb-alien:* sb-alien:unsigned-char)) 4096)))))

(defun input-rescan-timeout ()
  "Ms until a scheduled rescan is due (0 if overdue), or -1 when none is."
  (if *input-rescan-due*
      (max 0 (round (* 1000 (- *input-rescan-due* (get-internal-real-time)))
                    internal-time-units-per-second))
      -1))

(defun maybe-rescan-input ()
  "Run the scheduled rescan if it is due, and schedule the next one when there is no
   inotify to tell us."
  (when (and *input-rescan-due* (>= (get-internal-real-time) *input-rescan-due*))
    (setf *input-rescan-due* (and (null *input-watch-fd*)
                                  (+ (get-internal-real-time) (ms->itu +input-rescan-poll-ms+))))
    (rescan-input-devices)))

(defconstant +pollerr+  #x8)    ; struct pollfd revents: an error on the fd
(defconstant +pollhup+  #x10)   ; ... the other end hung up — evdev's "device is gone"
(defconstant +pollnval+ #x20)   ; ... not an open fd at all

(defparameter +max-poll-slots+ 40)

(defun poll-input-once (kb-fd timeout-ms)
  "ONE poll(2) over the console tty (KB-FD), the /dev/input watch and every open
   device, then the housekeeping it implies. Returns (values KB POINTER KEYBOARD
   HOUSEKEEPING): which of the three the caller cares about have records, and
   whether something else happened (a hotplug, a device gone) that it does not.

   The layout is the one FBB-WAIT always had, just longer: slot 0 the tty, slot 1 the
   watch, then one slot per device; an unused slot holds fd -1, which poll ignores.

   A device that was unplugged does not just go quiet. evdev_poll answers EPOLLHUP |
   EPOLLERR for it (drivers/input/evdev.c) — on EVERY call, at once — and read(2)
   says ENODEV. Ignore that and the loop spins at 100% CPU on a corpse; so HUP / ERR /
   NVAL closes the fd and forgets the device, here, before anyone tries to read it."
  (sb-alien:with-alien ((fds (sb-alien:array sb-alien:unsigned-char 320)))   ; 40 slots
    (let* ((devs (subseq *input-devices* 0 (min (length *input-devices*) (- +max-poll-slots+ 2))))
           ;; the services' fds take whatever slots the devices left
           (extra (subseq *poll-extra-fds* 0 (min (length *poll-extra-fds*)
                                                   (- +max-poll-slots+ 2 (length devs)))))
           (n (+ 2 (length devs) (length extra))))
      (dotimes (i (* n 8)) (setf (sb-alien:deref fds i) 0))
      (flet ((put-slot (i fd) (if fd (%put-pollfd fds i fd) (%put-pollfd fds i -1)))
             (revents (i) (sb-alien:deref fds (+ (* i 8) 6))))
        (put-slot 0 kb-fd)
        (put-slot 1 *input-watch-fd*)
        (loop for d in devs for i from 2 do (put-slot i (input-dev-fd d)))
        (loop for fd in extra for i from (+ 2 (length devs)) do (put-slot i fd))
        (%fbb-poll (sb-alien:cast fds (sb-alien:* sb-alien:unsigned-char)) n timeout-ms)
        (incf *input-wakeups*)
        (setf *poll-extra-ready*
              (loop for fd in extra for i from (+ 2 (length devs))
                    unless (zerop (revents i)) collect fd))
        (let ((kb (and kb-fd (logbitp 0 (revents 0)))) (ptr nil) (key nil) (house nil))
          (when (and *input-watch-fd* (plusp (revents 1)))
            (drain-input-watch)
            (rescan-input-devices)
            ;; and once more shortly: the /proc line or the chown may still be in flight
            (setf *input-rescan-due* (+ (get-internal-real-time) (ms->itu +input-rescan-retry-ms+))
                  house t))
          (loop for d in devs for i from 2
                for r = (revents i)
                do (cond ((plusp (logand r (logior +pollerr+ +pollhup+ +pollnval+)))
                          (when (member d *input-devices*) (drop-input-dev d "unplugged"))
                          (setf house t))
                         ((logbitp 0 r)
                          (setf (input-dev-ready d) t)
                          (if (eq :keyboard (input-dev-kind d)) (setf key t) (setf ptr t)))))
          (when (/= -1 (input-rescan-timeout))
            (when (zerop (input-rescan-timeout)) (setf house t))
            (maybe-rescan-input))
          (values kb ptr key house))))))

(defun read-pointers (xres yres)
  "Drain every pointer the last poll found ready, each against its own state, and
   combine what they said: (values CLICKED WHEEL BUTTON RECORDS) as READ-MOUSE returns
   for one. Two devices pressing in the same wake is not reachable by hand; the
   first one's button wins."
  (let ((clicked nil) (wheel 0) (button nil) (records 0))
    (dolist (d *input-devices*)
      (when (and (input-dev-ready d) (pointer-kind-p (input-dev-kind d)))
        (setf (input-dev-ready d) nil)
        (multiple-value-bind (c w b n)
            (with-device-state (d) (read-mouse (input-dev-fd d) xres yres))
          (incf (input-dev-records d) n) (incf records n)
          (when c (setf clicked t) (incf (input-dev-clicks d)))
          (incf wheel w)
          (when (and b (null button)) (setf button b)))))
    (sync-input-globals)
    (values clicked wheel button records)))

(defun read-keyboards ()
  "Drain every keyboard the last poll found ready (their modifiers), then recompute
   the shared modifier state. Returns :MENU if any of them pressed the Menu key."
  (let ((menu nil))
    (dolist (d *input-devices*)
      (when (and (input-dev-ready d) (eq :keyboard (input-dev-kind d)))
        (setf (input-dev-ready d) nil)
        (multiple-value-bind (m n) (with-device-state (d) (read-keyboard-evdev (input-dev-fd d)))
          (incf (input-dev-records d) n)
          (when m (setf menu m)))))
    (sync-input-globals)
    menu))

;;; ---- the pointer is not part of the frame ---------------------------------
;;; The canvas holds the browser and NEVER the pointer. The pointer is composited on
;;; the way out — into a patch the size of the arrow — and only that patch is sent to
;;; the screen. That split is what makes a moving mouse cheap: the pixels under the
;;; old position are still sitting untouched in the canvas, so erasing the arrow is
;;; re-sending that little rectangle, and nothing has to be redrawn at all.
;;;
;;; Before this, every mouse packet repainted and re-sent the whole 1280x800 frame
;;; for a change of about 200 pixels — 6.5 ms a frame under KVM and 67 ms under
;;; QEMU's TCG (tag #24), so the pointer trailed the hand. A move now costs 12 us and
;;; 0.13 ms (doc/framebuffer.org §9.5). (Nothing the browser DRAWS depends on where
;;; the pointer is — no hover effects — which is what makes skipping the redraw correct.)

(defparameter +cursor-w+ (reduce #'max *cursor-bits* :key #'length))
(defparameter +cursor-h+ (length *cursor-bits*))

(defvar *cursor-patch* nil "Scratch canvas the pointer is composited into.")

;;; The pointer HIDES itself after a few seconds without mouse activity — the arrow
;;; parked over the text you are typing is in the way — and comes back on the next
;;; move, click or wheel notch. Typing alone never brings it back: that is exactly
;;; when you want it gone. Hiding is the same trick as a move: re-send the canvas
;;; rectangle under the arrow, and simply do not draw it again.
(defparameter +cursor-idle-ms+ 3000 "Mouse inactivity, in ms, before the pointer hides.")
(defvar *cursor-hidden* nil "Is the pointer currently hidden for inactivity?")
(defvar *pointer-active-at* 0 "Internal-real-time of the last mouse activity.")

(defun ms-since (time)
  "Milliseconds elapsed since internal-real-time TIME."
  (round (* 1000 (- (get-internal-real-time) time)) internal-time-units-per-second))

(defun put-cursor (canvas x y)
  "Show the pointer at X,Y over CANVAS's pixels, sending only the arrow's patch."
  (let* ((patch (or *cursor-patch*
                    (setf *cursor-patch* (lol.canvas:make-canvas +cursor-w+ +cursor-h+))))
         (dst (lol.canvas:canvas-pixels patch))
         (src (lol.canvas:canvas-pixels canvas))
         (cw (lol.canvas:canvas-w canvas))
         (n (max 0 (min +cursor-w+ (- cw x)))))    ; columns that are on screen
    (dotimes (row (max 0 (min +cursor-h+ (- (lol.canvas:canvas-h canvas) y))))
      (replace dst src :start1 (* row +cursor-w+) :end1 (+ (* row +cursor-w+) n)
                       :start2 (+ (* (+ y row) cw) x)))
    (draw-cursor patch 0 0)
    (lol.fb:present-fb patch :x0 x :y0 y)))

(defun show-frame (canvas)
  "Send a freshly drawn CANVAS to the screen, then the pointer over it (unless it is
   hidden for inactivity)."
  (lol.fb:present-fb canvas)
  (when (and *fb-mouse* (not *cursor-hidden*)) (put-cursor canvas *cursor-x* *cursor-y*)))

(defun move-cursor (canvas old-x old-y)
  "The pointer moved and nothing else changed: put back what was under it (straight
   from CANVAS), then draw it at its new place. Two tiny writes instead of a frame.
   A hidden pointer has nothing to put back; it just reappears."
  (unless *cursor-hidden*
    (lol.fb:present-fb-rect canvas old-x old-y +cursor-w+ +cursor-h+))
  (setf *cursor-hidden* nil)
  (put-cursor canvas *cursor-x* *cursor-y*))

(defun pointer-activity ()
  "The mouse did something: restart the idle clock. Returns T when the pointer was
   hidden, so the caller knows it must be drawn again even if it did not move."
  (setf *pointer-active-at* (get-internal-real-time))
  (shiftf *cursor-hidden* nil))

(defun hide-cursor (canvas)
  "Take the pointer off the screen: re-send the canvas pixels under it."
  (unless *cursor-hidden*
    (lol.fb:present-fb-rect canvas *cursor-x* *cursor-y* +cursor-w+ +cursor-h+)
    (setf *cursor-hidden* t)))

(defun cursor-idle-timeout ()
  "Ms until the visible pointer is due to hide, or -1 when it is hidden or absent."
  (if (or *cursor-hidden* (null *fb-mouse*))
      -1
      (max 0 (- +cursor-idle-ms+ (ms-since *pointer-active-at*)))))

;;; The systray's clock is the second thing on screen that changes without input
;;; (the stage is the first). It does NOT repaint the frame: DRAW-TRAY repaints the
;;; tray's own strip of the canvas, and only that rectangle is sent — the same
;;; economy as the pointer.
(defun tray-timeout ()
  "Ms until the clock's next tick: the next wall-clock second, or minute when the
   tray shows no seconds (*CLOCK-SECONDS*), plus a few ms so we land just past it."
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (+ 5 (- (if *clock-seconds* 1000 (* 1000 (- 60 (mod sec 60))))
            (floor usec 1000)))))

(defun tick-tray (canvas w)
  "Repaint the tray strip and send just that rectangle; put the pointer back on top
   if it was over it."
  (draw-tray canvas w)
  (when *tray-rect*
    (destructuring-bind (x y tw th) *tray-rect*
      (lol.fb:present-fb-rect canvas x y tw th)
      (when (and *fb-mouse* (not *cursor-hidden*)
                 (< (- x +cursor-w+) *cursor-x* (+ x tw))
                 (< *cursor-y* (+ y th)))
        (put-cursor canvas *cursor-x* *cursor-y*)))))

(defun earliest-timeout (&rest timeouts)
  "The soonest of several poll timeouts in ms, where -1 means 'no deadline'."
  (let ((real (remove -1 timeouts)))
    (if real (reduce #'min real) -1)))

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
   reaches RUN-DEBUGGER-FB through the HANDLER-BIND below exactly as one from the
   Workspace does (WORKSPACE-DEBUG-IT), with the stack still standing and the stage's own restarts on the
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


;;; ---- services: other things that can wake the loop -------------------------
;;; Input and a stage's clock were, for a long time, the only reasons the loop woke.
;;; A web pane adds a third: Chrome writes a frame down a pipe whenever the page
;;; changes, and the loop must wake for THAT, without polling on a timer. A SERVICE
;;; is the general form — a plist of functions the loop calls by name, so this file
;;; needs no knowledge of what is behind them (web.lisp registers the first one):
;;;
;;;   :FD      () -> an fd to add to the poll, or NIL
;;;   :TIMEOUT () -> ms until the service must run even with nothing to read, or -1
;;;   :RUN     (canvas dirty) -> :REDRAW to repaint the browser; anything else, the
;;;            service has done (or does not need) its own drawing. DIRTY says a
;;;            full redraw is coming anyway, so a partial one would be wasted.
;;;   :MOTION  (x y) -> a bare pointer move, which otherwise redraws nothing

(defvar *fb-services* '() "Registered services, see above. FB-REGISTER-SERVICE adds one.")

(defun fb-register-service (name &rest functions &key fd timeout run motion)
  "Register (or replace) the service NAME. FUNCTIONS are symbols, called by name, so
   redefining one at the REPL takes effect at once."
  (declare (ignore fd timeout run motion))
  (setf *fb-services* (cons (list* :name name functions)
                            (remove name *fb-services* :key (lambda (s) (getf s :name))))))

(defun fb-service-call (key &rest args)
  "Call every service's KEY function with ARGS; -> the list of their non-NIL values.
   A service that signals is reported in the status line, not allowed to kill PID 1."
  (loop for s in *fb-services*
        for fn = (getf s key)
        for v = (and fn (fboundp fn)
                     (handler-case (apply fn args)
                       (serious-condition (c)
                         (setf *browser-status* (format nil "~(~a~): ~a" (getf s :name) c))
                         nil)))
        when v collect v))

(defun run-browser-fb (&optional root)
  "Launch the browser full-screen on /dev/fb0, rooted at ROOT. The supervisor calls
   it once at boot, and again from its menu ('b', 'p', 'c', 'l'). Blocks until you
   leave it (SuperL-q, which asks: the supervisor's console or power off; or q on a
   list pane), then restores the text console and returns to the supervisor menu,
   or powers off. Any error is caught and printed, so a dying
   environment drops you at the menu instead of crashing PID 1.

   ROOT defaults to the WORKSPACE alone. It used to be the module index with a
   Workspace tab beside it; but the environment is something you WORK in, and a
   table of contents you did not ask for was the first thing in the way. The index
   is one key away (SuperL-m; it is still the only view that shows the whole system,
   lol.* toolkit included), the flat CL-USER package is SuperL-l, and anything at all
   is SuperL-Space."
  (handler-case
      (progn
        (unless *bfont* (set-browser-font (lol.canvas:load-font "/cozette.lolf")))
        ;; installed by NAME, not by #', because run-browser-fb is compiled before
        ;; fb-prompt-value is defined further down this same file
        (setf *value-prompt* 'fb-prompt-value)    ; the shell asks; this backend reads
        (setf *chooser* 'run-chooser-fb)          ; ... and shows its menus and halos
        (setf *fm-interrupt-fn* 'fb-stop-requested-p) ; ... and lets Esc stop a long find
        (open-environment (or root (subj-workspace)))
        (multiple-value-bind (xres yres) (lol.fb:fb-geometry)
          (let ((canvas (lol.canvas:make-canvas xres yres)))
            ;; every keyboard and pointer, plus a watch for the ones still to come;
            ;; sets *FB-MOUSE* / *FB-KBD* and clears every held modifier (§4¾)
            (open-input-devices)
            (setf *cursor-x* (floor xres 2) *cursor-y* (floor yres 2)
                  *fb-canvas* canvas *fb-xres* xres *fb-yres* yres
                  *cursor-hidden* nil *pointer-active-at* (get-internal-real-time))
            (unwind-protect
                 (lol.fb:with-graphics-console         ; KD_GRAPHICS: fbcon stops drawing
                   ;; signals OFF so Ctrl-C arrives as a byte (the C-c C-c Accept chord)
                   (with-raw-mode (0 nil)
                     ;; DIRTY = the browser's state changed since the last frame. Only
                     ;; a key, a click, the wheel or a stage tick can change it; a held
                     ;; modifier or a bare pointer move cannot, so neither redraws.
                     (loop with dirty = t do
                       (when dirty
                         (draw-browser canvas xres yres)
                         (show-frame canvas)
                         (setf dirty nil))
                       ;; A running stage (§3b) is the one thing on screen that changes
                       ;; without being asked, so it — and only it — makes the loop
                       ;; wake on a deadline rather than sleeping until input.
                       (multiple-value-bind (kb ms kev xs)
                           (fbb-wait 0 *fb-mouse* *fb-kbd*
                                     (apply #'earliest-timeout
                                            (live-stage-timeout)
                                            (cursor-idle-timeout)
                                            (tray-timeout)
                                            ;; a live run pane (runs.lisp)
                                            (runs-timeout)
                                            (fb-service-call :timeout))
                                     (fb-service-call :fd))
                         ;; refresh Ctrl/Shift/Super first; the Menu key opens the
                         ;; context menu where the caret (or selected row) is
                         (when (and kev (eq :menu (read-keyboard-evdev *fb-kbd*)))
                           (keyboard-context-menu)
                           (setf dirty t))
                         (when kb
                           (setf dirty t)
                           (multiple-value-bind (key state) (fb-read-key 0)
                             ;; Ctrl-/ reaches the tty as DEL, Backspace's own byte
                             ;; (defkeymap: ctrl_map[53] = 0x7f), so only the held Ctrl
                             ;; tells them apart: undo, as C-/ is in Emacs
                             (when (and (eq key :backspace) (zerop state) *ctrl-down*)
                               (setf key #\/ state 4))
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
                         (when ms
                           (let ((ox *cursor-x*) (oy *cursor-y*)
                                 (was-down *button-down*)
                                 (was-hidden (pointer-activity)))
                             (multiple-value-bind (clicked wheel button)
                                 (read-mouse *fb-mouse* xres yres)   ; every ready pointer
                               ;; wheel first: a notch scrolls the current pane a few
                               ;; lines (up = toward the start), point untouched.
                               (unless (zerop wheel)
                                 (browser-scroll (* (- wheel) +wheel-lines+))
                                 (setf dirty t))
                               ;; Alt-click is the middle button, for a pointer that
                               ;; has none (doc/input.org, "Three buttons")
                               (when (and clicked *alt-down*)
                                 (setf clicked nil button :middle *button-down* nil))
                               (when clicked
                                 (browser-click *cursor-x* *cursor-y* *ctrl-down* *super-down*)
                                 (setf dirty t))
                               (case button
                                 (:right  (browser-context-menu *cursor-x* *cursor-y*)
                                          (setf dirty t))
                                 (:middle (browser-middle-click *cursor-x* *cursor-y* *super-down*)
                                          (setf dirty t)))
                               ;; moved with the button still held: a drag, which
                               ;; extends the text selection the press started
                               (when (and *button-down* (not clicked)
                                          (or (/= ox *cursor-x*) (/= oy *cursor-y*))
                                          (browser-drag *cursor-x* *cursor-y*))
                                 (setf dirty t))
                               ;; the button came up: a drag that selected something
                               ;; hands it to PRIMARY and the clipboard (X's rule)
                               (when (and was-down (not *button-down*))
                                 (browser-drag-end))
                               ;; a bare move: the frame is still right, only the arrow
                               ;; is in the wrong place (a redraw would move it anyway)
                               (when (and (not dirty)
                                          (or was-hidden (/= ox *cursor-x*) (/= oy *cursor-y*)))
                                 (move-cursor canvas ox oy)
                                 (fb-service-call :motion *cursor-x* *cursor-y*)))))
                         ;; a run's reader thread may have collected output: copy it
                         ;; into its pane (cheap when nothing changed — a compare)
                         (when (tick-runs) (setf dirty t))
                         ;; the services (a web page's frames, …): every turn, since
                         ;; they also react to what input just did (a click's page)
                         (when (member :redraw (fb-service-call :run canvas dirty))
                           (setf dirty t))
                         ;; Nothing was ready: a deadline fired. Which one? A running
                         ;; stage advances the world; an idle pointer hides.
                         (unless (or kb ms kev xs)
                           (when (/= -1 (live-stage-timeout))
                             (tick-live-stage) (setf dirty t))
                           (when (zerop (cursor-idle-timeout))
                             (hide-cursor canvas))
                           ;; the clock: cheap, so simply on every timeout — a
                           ;; stage frame or a hide repaints an unchanged tray
                           (unless dirty (tick-tray canvas xres)))))))
              (close-input-devices)))))
    (serious-condition (c)
      (format t "~&code browser: ~a~%" c) (finish-output)))
  ;; SuperL-q chose to power off: only now, with the text console back, so the
  ;; kernel's last words (and POWER-OFF's own) are readable if it is refused
  (when (eq (shiftf *leave-action* nil) :power-off)
    (power-off)))

(defun fb-stop-requested-p ()
  "True when a key is waiting and it is Esc or C-g — the file manager's recursive find
   polls this between folders (*FM-INTERRUPT-FN*), so a search that is taking too long
   can be stopped from the keyboard. Any OTHER key read here is lost: typing ahead
   during a find is not supported, and the find takes three seconds at most anyway."
  (and (fbb-ready-p 0 0)
       (multiple-value-bind (key state) (fb-read-key 0)
         (or (eq key :escape) (and (logbitp 2 state) (member key '(#\g #\G)))))))

;;; ---- the debugger: a condition made interactive (§3) ---------------------
;;; Entered from a HANDLER-BIND handler (WORKSPACE-DEBUG-IT, TICK-LIVE-STAGE) — not
;;; from *DEBUGGER-HOOK*, which nothing sets — with the stack STILL STANDING, so the restarts are
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
    (loop with dirty = t do                 ; redraw only on a change (see run-browser-fb)
      (when dirty
        (lol.canvas:fill-rect canvas 0 0 w h *ui-bg*)
        (lol.canvas:fill-rect canvas 0 0 w 24 *ui-hi*)
        (lol.canvas:draw-string canvas *bfont*
          (format nil "source of ~(~a~)   —   scroll;  Esc / q / Enter  back to the debugger" name)
          12 5 *ui-accent*)
        (lol.textview:tv-draw canvas tv)
        (show-frame canvas)
        (setf dirty nil))
      (multiple-value-bind (kb ms kev) (fbb-wait 0 *fb-mouse* *fb-kbd*)
        (when kev (read-keyboard-evdev *fb-kbd*))
        (when kb
          (setf dirty t)
          (multiple-value-bind (key state) (fb-read-key 0)
            (declare (ignore state))
            (case key
              (:up        (lol.textview:tv-wheel tv -1))
              (:down      (lol.textview:tv-wheel tv 1))
              (:page-up   (lol.textview:tv-wheel tv (- +src-page+)))
              (:page-down (lol.textview:tv-wheel tv +src-page+))
              ((:escape #\q #\Q :return #\Return) (return)))))
        (when (and *fb-mouse* ms)
          (let ((ox *cursor-x*) (oy *cursor-y*))
            (multiple-value-bind (clicked wheel) (read-mouse *fb-mouse* w h)
              (declare (ignore clicked))
              (cond ((/= wheel 0)
                     (lol.textview:tv-wheel tv (* (- wheel) +wheel-lines+))
                     (setf dirty t))
                    ((and (not dirty) (or (/= ox *cursor-x*) (/= oy *cursor-y*)))
                     (move-cursor canvas ox oy))))))))))

(defun dbg-prompt-value (name)
  "A modal input line over the debugger for a restart that needs a value: read the Lisp
   expression the user types (Enter accepts, Esc cancels). Returns the typed STRING, or
   NIL if cancelled. Draws its bar over the current debugger frame (already on canvas)."
  (let ((buf "") (canvas *fb-canvas*) (w *fb-xres*) (h *fb-yres*))
    (loop with dirty = t do
      (when dirty
        (lol.canvas:fill-rect canvas 0 (- h 44) w 44 *ui-hi*)
        (lol.canvas:fill-rect canvas 0 (- h 44) w 2 *ui-amber*)
        (lol.canvas:draw-string canvas *bfont*
          ;; "accepts", not "invokes": this line is shared now — a restart INVOKES with
          ;; the value, the inspector SETS a field to it (shell.lisp's set-field)
          (format nil "value for [~(~a~)] — an expression, Enter accepts, Esc cancels:" name)
          12 (- h 38) *ui-amber*)
        (lol.canvas:draw-string canvas *bfont* (format nil "~a_" buf) 12 (- h 18) *ui-text*)
        (show-frame canvas)
        (setf dirty nil))
      (multiple-value-bind (kb ms kev) (fbb-wait 0 *fb-mouse* *fb-kbd*)
        (when kev (read-keyboard-evdev *fb-kbd*))
        ;; The mouse must be READ even though a click means nothing here: an unread
        ;; mouse fd stays readable, and poll would then return at once forever — this
        ;; loop used to spin, repainting, from the first twitch of the pointer on.
        (when (and *fb-mouse* ms)
          (let ((ox *cursor-x*) (oy *cursor-y*))
            (read-mouse *fb-mouse* w h)
            (when (or (/= ox *cursor-x*) (/= oy *cursor-y*))
              (move-cursor canvas ox oy))))
        (when kb
          (setf dirty t)
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
    (loop with dirty = t do                 ; redraw only on a change (see run-browser-fb)
      (when dirty
        ;; locals read fresh on each redraw — cheap, and a redraw only follows input
        (let ((locals (ignore-errors (frame-locals (nth fsel frame-objs)))))
          (draw-debugger canvas w h condition restarts backtrace locals rsel fsel focus))
        (show-frame canvas)
        (setf dirty nil))
      (multiple-value-bind (kb ms kev) (fbb-wait 0 *fb-mouse* *fb-kbd*)
        (when kev (read-keyboard-evdev *fb-kbd*))
        (when kb
          (setf dirty t)
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
        (when (and *fb-mouse* ms)
          (let ((ox *cursor-x*) (oy *cursor-y*))
            (if (read-mouse *fb-mouse* w h)
                (let ((ri (dbg-restart-at *cursor-x* *cursor-y* (length restarts)))
                      (fi (dbg-frame-at *cursor-x* *cursor-y* (length frame-objs))))
                  (setf dirty t)
                  (cond (ri (dbg-enter restarts ri))  ; a restart click invokes (asks if needed)
                        (fi (setf focus :backtrace fsel fi))))  ; a frame click selects it
                (when (and (not dirty) (or (/= ox *cursor-x*) (/= oy *cursor-y*)))
                  (move-cursor canvas ox oy)))))))))

;;; ---- the SuperL-? cheatsheet: every binding, on demand (§4¾) --------------
;;; The keys are deliberately NOT shown as permanent chrome — SuperL-? summons this
;;; overlay instead. Super reaches us only through the keyboard's own evdev (the
;;; cooked tty can't report it), same as Ctrl/Shift; the main loop folds *super-down*
;;; into the key state, so browser-key sees (super-p state) and calls in here.

(defparameter *cheatsheet*
  '(("Navigate"
     ("up / down"          "move the selection / caret")
     ("Enter"              "drill into the selection")
     ("S-Enter"            "open it BESIDE the list (editable)")
     ("right"              "focus the right column")
     ("Backspace / C-g"    "back (C-g also leaves the right column)")
     ("PageUp / PageDown"  "page the list / source")
     ("mouse wheel"        "scroll; two fingers on the pad")
     ("Esc"                "Meta prefix: Esc . = M-.")
     ("q"                  "back, like Backspace (never leaves)"))
    ("Places  (Super)"
     ("Super-Space"        "the Spotter (C-n / C-p move)")
     ("Super-m"            "the module index: every definition")
     ("Super-l"            "the CL-USER package, flat")
     ("Super-h"            "the clipboard history")
     ("Super-f"            "the file manager (home)")
     ("Super-b"            "a web page (Chrome, doc/web-browser)")
     ("Super-?"            "this cheatsheet")
     ("Super-q"            "leave: console or shut down"))
    ("Tabs  (Super)"
     ("Super-1..9"         "jump to tab N")
     ("Super-S-1..9"       "move this tab to slot N")
     ("Super-Tab"          "cycle tabs (Shift: backwards)")
     ("Super-Enter/click"  "open the selection in a NEW tab")
     ("Super-w"            "close the current tab")
     ("Super-o"            "focus list <-> side pane")
     ("Super-Left"         "back in the trail"))
    ("Workspace & source"
     ("Tab"                "complete the symbol (else indent)")
     ("C-c C-c"            "Accept / define the form")
     ("C-x C-e"            "Do it: value to the status line")
     ("C-c C-p"            "Print it: result woven inline")
     ("C-c C-i"            "Inspect it: open the value")
     ("C-c C-d"            "Debug it: debugger armed"))
    ("Web page"
     ("C-l / M-Left,Right" "address / back, forward")
     ("C-r  C-f  C-+ C--"  "reload, find, zoom")
     ("C-c / C-v"          "copy / paste via OUR clipboard"))
    ("Inspector"
     ("C-c C-c"            "set the selected field")
     ("C-c C-e"            "evaluate in the object (SELF)")
     ("C-c C-a"            "apply this function"))
    ("Jump"
     ("M-. / Ctrl-click"   "the definition at point / clicked")
     ("M-,"                "senders of the symbol at point"))
    ("Motion & selection"
     ("C-Left / C-Right"   "by word  (also M-b / M-f)")
     ("C-a / C-e"          "line start / end")
     ("C-Home / C-End"     "buffer start / end")
     ("Shift + motion"     "extend the selection")
     ("C-Space"            "set the mark (motion extends)")
     ("C-x Space"          "rectangle mark (column block)")
     ("C-x h"              "select all")
     ("C-_ / C-/ / C-x u"  "undo (again: further; Emacs redo)"))
    ("Clipboard  (Emacs + PC)"
     ("C-w / S-Delete"     "cut the region")
     ("M-w / C-Insert"     "copy the region")
     ("C-y / S-Insert"     "paste the newest copy")
     ("M-y"                "after a paste: the one before")
     ("C-k"                "kill the line (repeat: append)")
     ("M-d / M-Backspace"  "kill a word forward / back")
     ("mouse select"       "copies to PRIMARY + history")
     ("middle click"       "pastes PRIMARY into text"))
    ("Rectangles  (C-x r)"
     ("C-x r k / M-w"      "kill / copy the rectangle")
     ("C-x r y"            "yank the last rectangle")
     ("C-x r d / c"        "delete / blank it out")
     ("C-x r o"            "open: shift text right")
     ("C-x r t"            "replace each line's span"))
    ("Mouse & pad"
     ("click / tap"        "select, drill, place caret")
     ("press + drag"       "select text")
     ("double / triple"    "select the word / the line")
     ("right / Menu key"   "context menu  (S-F10 too)")
     ("pad corner press"   "right button (bottom right)")
     ("middle"             "halo: source, inspect, code")
     ("  again / Up / Tab" "halo: the thing around it")
     ("2-finger press"     "middle button  (Alt-click too)")
     ("Super-middle"       "halo even in editable text"))
    ("Files  (in a folder)"
     ("/  f"               "filter here / find below")
     ("s  S  ."            "sort, reverse, dot files")
     ("+  n"               "new folder / new file")
     ("r  c  m  d  Del"    "rename copy move delete")
     ("l  L  x"            "symlink, hard link, chmod +x")
     ("Space  U"           "mark (d/c/m act on all) / clear")
     ("^  ~  i  y"         "up, home, info, copy path")
     ("j  g"               "follow a link / refresh")
     ("Esc (during f)"     "stop a running find")
     ("C-x C-s"            "save a file you are editing")
     ("Spotter ~/ or /"    "type a path; Tab completes"))
    ("Debugger"
     ("0-9 / up-down"      "select a restart")
     ("Tab"                "focus the backtrace, walk frames")
     ("M-."                "the selected frame's source")
     ("Enter / click"      "invoke the selected restart")
     ("C-g"                "abort"))
    ("Stage  (§3b)"
     ("Space / n"          "run-pause / one tick")
     ("s / a / m / t"      "spawn ship/asteroid/missile/station")
     ("Tab / click"        "select a sprite;  Enter inspects")
     ("w / r"              "the Workspace beside / reset")))
  "The full key map, grouped, for the SuperL-? overlay. Data only — DRAW-CHEATSHEET
   lays it out in columns.")

(defun draw-cheatsheet (canvas w h)
  "Render *cheatsheet* as a full-screen overlay: as many columns as the width
   allows (three at 1280), the groups poured into them in order, each column
   filled before the next starts. Group titles in amber, the keys in accent, the
   descriptions in text."
  (lol.canvas:fill-rect canvas 0 0 w h *ui-bg*)
  (lol.canvas:fill-rect canvas 0 0 w 22 *ui-hi*)
  (lol.canvas:draw-string canvas *bfont*
    "Keys  —  press any key or click to dismiss" 12 4 *ui-accent*)
  (let* ((ncols (max 1 (min 4 (floor (- w 40) 410))))
         (colw (floor (- w 40) ncols))
         (keyw 150)
         (maxc (max 8 (floor (- colw keyw 24) (lol.canvas:font-cw *bfont*))))
         (rows (+ (* 2 (length *cheatsheet*))           ; title + gap per group
                  (loop for g in *cheatsheet* sum (length (cdr g)))))
         (per-col (ceiling rows ncols))
         (c 0) (y 34) (used 0))
    (dolist (g *cheatsheet*)
      ;; start a new column when this group would overflow the share of this one
      (when (and (plusp used) (< c (1- ncols))
                 (> (+ used (length (cdr g)) 1) per-col))
        (incf c) (setf y 34 used 0))
      (let ((x (+ 20 (* c colw))))
        (lol.canvas:draw-string canvas *bfont* (car g) x y *ui-amber*)
        (incf y 18)
        (dolist (row (cdr g))
          (lol.canvas:draw-string canvas *bfont* (first row) (+ x 10) y *ui-accent*)
          (lol.canvas:draw-string canvas *bfont* (clip-text (second row) maxc)
                                  (+ x 10 keyw) y *ui-text*)
          (incf y 15))
        (incf y 9)
        (incf used (+ 2 (length (cdr g))))))))

(defun run-cheatsheet-fb ()
  "Draw the cheatsheet over the browser's live canvas and block until any key or
   click dismisses it. Reuses the *fb-* I/O the browser owns (like the debugger)."
  (let ((canvas *fb-canvas*) (w *fb-xres*) (h *fb-yres*))
    (when canvas
      (draw-cheatsheet canvas w h)
      (show-frame canvas)
      (loop
        (multiple-value-bind (kb ms kev) (fbb-wait 0 *fb-mouse* *fb-kbd*)
          (when kev (read-keyboard-evdev *fb-kbd*))
          (when kb (fb-read-key 0) (return))            ; any key dismisses
          (when (and *fb-mouse* ms)
            (let ((ox *cursor-x*) (oy *cursor-y*))
              (when (read-mouse *fb-mouse* w h) (return))   ; a click dismisses
              (when (or (/= ox *cursor-x*) (/= oy *cursor-y*))
                (move-cursor canvas ox oy)))))))))

;;; ---- menus and halos: one modal chooser -----------------------------------
;;; The right-button menu and the middle-button halo are the same interaction — a
;;; short list of things you can do, pick one or leave — drawn two ways. The shell
;;; decides WHAT is offered (context-menu-entries, halo-entries) and how each looks
;;; (draw-context-menu, draw-halo); this loop only reads input, the same way the
;;; Spotter's does. It is installed as the shell's *CHOOSER* hook.

(defun run-chooser-fb (kind entries x y &optional layers)
  "Show ENTRIES as a :MENU card at X,Y or a :HALO round it, and return the entry
   picked, or NIL. Keys: up/down move (left/right for the ring), Return picks, 1-9
   pick directly, Esc leaves. A click picks what is under it; a click anywhere else
   leaves. A press of the button that opened it, on the same spot, does not count —
   only a new click does.

   A halo is about one of LAYERS (shell.lisp's HALO-LAYERS: the word, the element
   holding it, the pane, the screen). Up, Tab and the middle button walk OUT to the
   thing around the current one, Down walks back IN — Morphic's repeated middle-click
   — and each step rebuilds the handles for the new layer."
  (let ((canvas *fb-canvas*) (w *fb-xres*) (h *fb-yres*) (sel 0) (n (length entries))
        (depth 0))
    (when (and canvas entries)
      (labels ((halo-p () (eq kind :halo))
               (draw ()
                 (if (halo-p)
                     (draw-halo canvas w h layers depth entries sel x y)
                     (draw-context-menu canvas w h entries sel x y)))
               (hit (px py)
                 (if (halo-p) (halo-handle-at px py) (menu-entry-at px py)))
               (walk (delta)                 ; to another layer: new handles, first one lit
                 (setf depth (mod (+ depth delta) (length layers))
                       entries (halo-entries (nth depth layers))
                       n (length entries) sel 0)))
        (loop with dirty = t do
          (when dirty (draw) (show-frame canvas) (setf dirty nil))
          (multiple-value-bind (kb ms kev) (fbb-wait 0 *fb-mouse* *fb-kbd*)
            (when kev (read-keyboard-evdev *fb-kbd*))
            (when kb
              (setf dirty t)
              (multiple-value-bind (key state) (fb-read-key 0)
                ;; C-p / C-n move the selection; in a halo they go round the
                ;; handles, since Up and Down there walk the layers
                (let ((k (emacs-arrow key state)))
                  (unless (eq k key)
                    (setf key (if (halo-p) (if (eq k :up) :left :right) k))))
                (cond
                  ((member key '(:escape :eof)) (return nil))
                  ((member key '(:return #\Return)) (return (nth sel entries)))
                  ((and (halo-p) (eq key :tab)) (walk (if (logbitp 0 state) -1 1)))
                  ((and (halo-p) (eq key :up)) (walk 1))
                  ((and (halo-p) (eq key :down)) (walk -1))
                  ((member key '(:up :left))    (setf sel (mod (1- sel) n)))
                  ((member key '(:down :right :tab)) (setf sel (mod (1+ sel) n)))
                  ((and (characterp key) (digit-char-p key) (<= 1 (digit-char-p key) n))
                   (return (nth (1- (digit-char-p key)) entries))))))
            (when (and *fb-mouse* ms)
              (let ((ox *cursor-x*) (oy *cursor-y*))
                (multiple-value-bind (clicked wheel button) (read-mouse *fb-mouse* w h)
                  (declare (ignore wheel))
                  ;; Alt-click is the touchpad's middle button, as in the main loop
                  (when (and clicked *alt-down*) (setf clicked nil button :middle *button-down* nil))
                  (cond
                    ((and (halo-p) (eq button :middle))
                     (walk 1) (setf dirty t))
                    ((or clicked button)
                     (let ((i (hit *cursor-x* *cursor-y*)))
                       (cond ((eq i :outside) (return nil))
                             ((integerp i) (return (nth i entries))))))
                    ((or (/= ox *cursor-x*) (/= oy *cursor-y*))
                     ;; hovering a menu row selects it, the way every menu behaves
                     (let ((i (hit *cursor-x* *cursor-y*)))
                       (if (and (integerp i) (/= i sel))
                           (setf sel i dirty t)
                           (move-cursor canvas ox oy))))))))))))))

;;; ---- the clipboard manager: SuperL-h (doc/clipboard.org) -------------------
;;; Parcellite's history window as a palette, the Spotter's shape: type to filter,
;;; arrows to move, Enter makes the entry current and pastes it where the caret was,
;;; Delete forgets it, Esc leaves. Drawing: draw-clipboard-card in shell.lisp.

(defun run-clipboard-fb ()
  "The clipboard history, modal over the browser."
  (let ((canvas *fb-canvas*) (w *fb-xres*) (h *fb-yres*) (query "") (sel 0))
    (when canvas
      (loop with dirty = t
            for results = (lol.clipboard:clip-filter query) do
        (setf sel (max 0 (min sel (1- (length results)))))
        (when dirty
          (draw-clipboard-card canvas w h query results sel)
          (show-frame canvas)
          (setf dirty nil))
        (multiple-value-bind (kb ms kev) (fbb-wait 0 *fb-mouse* *fb-kbd*)
          (when kev (read-keyboard-evdev *fb-kbd*))
          (when kb
            (setf dirty t)
            (multiple-value-bind (key state) (fb-read-key 0)
              (let ((ctrl (logbitp 2 state)))
                (cond
                  ((or (member key '(:escape :eof)) (and ctrl (eql key #\g))) (return))
                  ((member key '(:return #\Return))
                   (when results (clipboard-choose (car (nth sel results))))
                   (return))
                  ((or (eq key :up) (and ctrl (eql key #\p))) (decf sel))
                  ((or (eq key :down) (and ctrl (eql key #\n))) (incf sel))
                  ((eq key :delete)
                   (when results (lol.clipboard:clip-delete (car (nth sel results)))))
                  ((eq key :backspace)
                   (when (plusp (length query))
                     (setf query (subseq query 0 (1- (length query))) sel 0)))
                  ((and (characterp key) (graphic-char-p key) (not ctrl))
                   (setf query (concatenate 'string query (string key)) sel 0))))))
          (when (and *fb-mouse* ms)
            (let ((ox *cursor-x*) (oy *cursor-y*))
              (if (read-mouse *fb-mouse* w h)
                  (let ((i (clipboard-row-at *cursor-x* *cursor-y*)))
                    (cond ((eq i :outside) (return))
                          ((and (integerp i) (< i (length results)))
                           (clipboard-choose (car (nth i results))) (return))))
                  (when (and (not dirty) (or (/= ox *cursor-x*) (/= oy *cursor-y*)))
                    (move-cursor canvas ox oy))))))))))

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

(defun emacs-arrow (key state)
  "C-p / C-n as :UP / :DOWN, any other KEY as itself — for the modal lists (the
   Spotter, the menus), where nothing is edited so the Emacs line keys are free. A
   control byte reaches us as its letter plus the Ctrl bit (FB-READ-KEY), so without
   this C-n would type an n into the Spotter's query."
  (cond ((not (logbitp 2 state)) key)
        ((member key '(#\p #\P)) :up)
        ((member key '(#\n #\N)) :down)
        (t key)))

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
      (loop with dirty = t do               ; redraw only on a change (see run-browser-fb)
        (when dirty
          (draw-spotter-card canvas w h query results sel)
          (show-frame canvas)
          (setf dirty nil))
        (flet ((refilter () (setf results (ignore-errors (spotter-results query cands +spotter-cap+))
                                  sel 0)))
          (multiple-value-bind (kb ms kev) (fbb-wait 0 *fb-mouse* *fb-kbd*)
            (when kev (read-keyboard-evdev *fb-kbd*))
            (when kb
              (setf dirty t)
              (multiple-value-bind (key state) (fb-read-key 0)
                (setf key (emacs-arrow key state))
                (cond
                  ((eq key :escape) (return))
                  ((member key '(:return #\Return))
                   (let ((hit (nth sel results)))
                     (when (and hit (getf hit :subject)) (new-trail (getf hit :subject))))
                   (return))
                  ;; Tab completes a PATH query the way a shell does (common prefix;
                  ;; a single folder gets its slash) — SPOTTER-COMPLETE, shell.lisp
                  ((eq key :tab)
                   (let ((new (spotter-complete query)))
                     (unless (string= new query) (setf query new) (refilter))))
                  ((eq key :up)   (setf sel (max 0 (1- sel))))
                  ((eq key :down) (setf sel (min (max 0 (1- (length results))) (1+ sel))))
                  ((eq key :backspace)
                   (when (plusp (length query))
                     (setf query (subseq query 0 (1- (length query)))) (refilter)))
                  ((and (characterp key) (graphic-char-p key) (not (logbitp 2 state)))
                   (setf query (concatenate 'string query (string key))) (refilter)))))
            (when (and *fb-mouse* ms)
              (let ((ox *cursor-x*) (oy *cursor-y*))
                (if (read-mouse *fb-mouse* w h)
                    (let ((i (spotter-row-at* *cursor-x* *cursor-y*)))
                      (cond ((and i (< i (length results)))
                             (new-trail (getf (nth i results) :subject)) (return))
                            ((spotter-outside-p* *cursor-x* *cursor-y*) (return)))) ; click-away dismisses
                    (when (and (not dirty) (or (/= ox *cursor-x*) (/= oy *cursor-y*)))
                      (move-cursor canvas ox oy)))))))))))

;;; ---- what the environment sees: the input devices as a subject (§4¾) -------
;;; "My mouse does nothing" has four different answers — the kernel has no driver
;;; for it, it is not a pointer by our rules, it is not ours to open, or it is open
;;; and fine and the trouble is elsewhere — and from the chair they all look the
;;; same. This view tells them apart. SuperL-Space, "input devices"; or
;;; (show-input-view) at a REPL. (Not SHOW-INPUT-DEVICES: that is the supervisor's
;;; boot-time list of what the kernel sees, a different question.)

(defstruct (subj-input-devices (:constructor subj-input-devices ())))

(defun input-errno-name (errno)
  (case errno (2 "ENOENT, no such node") (13 "EACCES, not yours yet")
              (19 "ENODEV, gone") (t (format nil "errno ~a" errno))))

(defun input-dev-held (dev)
  "The modifiers and button DEV is holding right now, as a short string, or NIL."
  (let ((held (loop for (sym name) in '((*ctrl-down* "Ctrl") (*shift-down* "Shift")
                                        (*super-down* "Super") (*alt-down* "Alt")
                                        (*button-down* "button"))
                    when (device-var dev sym) collect name)))
    (and held (format nil "~{~a~^+~}" held))))

(defmethod present ((s subj-input-devices))
  (let* ((open *input-devices*)
         (seen (scan-input-devices))
         (unused (remove-if (lambda (b)
                              (or (find (getf b :node) open :key #'input-dev-node :test #'equal)
                                  (find (getf b :node) *input-open-failures* :key #'first
                                                                             :test #'equal)))
                            seen)))
    (make-view
     :title "input devices - what the environment reads"
     :kind :list :subject s
     :items
     (append
      (if open
          (mapcar (lambda (d)
                    (make-item
                     :label (format nil "~8a ~9a ~a" (input-dev-node d)
                                    (string-downcase (input-dev-kind d)) (input-dev-name d))
                     :detail (format nil "~d records~:[~*~; · ~d clicks~]~@[ · holding ~a~] · ~a"
                                     (input-dev-records d)
                                     (pointer-kind-p (input-dev-kind d)) (input-dev-clicks d)
                                     (input-dev-held d) (input-dev-phys d))
                     :kind (if (eq (input-dev-kind d) :keyboard) :class :variable)
                     :group "open - every one of these is read"
                     :subject d))
                  open)
          (list (make-item :label "(nothing open - is the environment running?)"
                           :group "open - every one of these is read")))
      (mapcar (lambda (f)
                (make-item :label (format nil "~8a ~a" (first f) (second f))
                           :detail (format nil "wanted, could not open: ~a"
                                           (input-errno-name (third f)))
                           :kind :condition
                           :group "could not open"))
              *input-open-failures*)
      (mapcar (lambda (b)
                (make-item :label (format nil "~8a ~a" (getf b :node) (getf b :name))
                           :detail (format nil "not a keyboard or pointer: EV=~x REL=~x ABS=~x"
                                           (getf b :ev) (getf b :rel) (getf b :abs))
                           :group "seen, not read"))
              unused)
      (list (make-item :label (if *input-watch-fd*
                                  "hotplug: inotify on /dev/input"
                                  (format nil "hotplug: rescanning every ~ds (no inotify)"
                                          (round +input-rescan-poll-ms+ 1000)))
                       :detail (format nil "held now: ~:[~;Ctrl ~]~:[~;Shift ~]~:[~;Super ~]~:[~;Alt ~]~:[~;button~]"
                                       *ctrl-down* *shift-down* *super-down* *alt-down* *button-down*)
                       :group "how"))
      (mapcar (lambda (e)
                (make-item :label (second e)
                           :detail (format nil "~ds after boot" (first e))
                           :group "recent changes"))
              *input-log*)))))

(defmethod commands-for ((s subj-input-devices))
  (list (make-command :label "Refresh" :target s :disposition :replace)))

(defun show-input-view ()
  "REPL entry point: the input devices view, printed."
  (show-view (subj-input-devices)))

(defun input-devices-spotter-candidates ()
  (list (list :label "input devices"
              :detail "surface · keyboards, mice, touchpads the environment reads"
              :group "surfaces" :subject (subj-input-devices))))

(pushnew 'input-devices-spotter-candidates *spotter-sources*)
