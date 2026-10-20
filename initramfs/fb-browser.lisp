;;;; fb-browser.lisp — run the code browser on /dev/fb0 (the guest; no X).
;;;;
;;;; The same shell (poc/shell.lisp) that drives the browser in an X11 window on
;;;; the dev host runs here on the bare framebuffer, launched from the supervisor
;;;; menu. Output is lol.fb:present-fb into /dev/fb0 (under KD_GRAPHICS so fbcon
;;;; stops scribbling); input is BOTH the raw console keyboard AND the evdev mouse,
;;;; multiplexed with poll(2), decoded into the world's events (poc/morph.lisp) and
;;;; routed there; one turn of that loop is FB-CYCLE, which a waiting overlay turns
;;;; too (doc/morphic.org). It returns to the menu when you leave (SuperL-q). Esc is
;;;; the Meta prefix, so it does not quit.
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
    ;; ESC [ [ A..E: the Linux console's F1..F5 (its keymap's func_buf, not xterm's)
    (when (eql b 91)
      (let ((c (fbb-read-byte fd)))
        (return-from fb-read-csi
          (values (if (and c (<= 65 c 69)) (nth (- c 65) '(:f1 :f2 :f3 :f4 :f5)) :ignore) 0))))
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
           ;; 17..24: F6..F12 (the same numbers xterm uses; 22 is skipped by both)
           (t (values (case n1 (1 :home) (7 :home) (4 :end) (8 :end) (2 :insert)
                               (3 :delete) (5 :page-up) (6 :page-down) (21 :f10)
                               (17 :f6) (18 :f7) (19 :f8) (20 :f9) (23 :f11) (24 :f12)
                               (t :ignore))
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
               ;; ESC CR / ESC TAB = M-RET / M-TAB: the console's alt_map sends Enter and
               ;; Tab with the Meta prefix (defkeymap: alt_map[28] = 0xf80d, [15] =
               ;; 0xf809). Org's M-RET is a new heading (poc/org-mode.lisp).
               ((member b1 '(13 10)) (values :return 8))
               ((eql b1 9) (values :tab 8))
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
      ;; UTF-8: the console's keyboard is in Unicode mode (vt.c, default_utf8), so a
      ;; character past ASCII — ç from the pt-BR layer (keyboard-layout.lisp), á from a
      ;; dead key — arrives as 2 to 4 bytes: a lead byte 110xxxxx / 1110xxxx /
      ;; 11110xxx, then that many 10xxxxxx. Until 2026-10-19 each byte became a
      ;; character of its own, and "ç" (C3 A7) came out as "Ã§".
      ((<= #xC2 c #xF4)
       (let* ((n (cond ((< c #xE0) 1) ((< c #xF0) 2) (t 3)))
              (code (logand c (case n (1 #x1F) (2 #x0F) (t #x07)))))
         (dotimes (i n (values (or (ignore-errors (code-char code)) #\?) 0))
           (let ((b (and (fbb-ready-p fd 20) (fbb-read-byte fd))))
             (if (and b (= (logand b #xC0) #x80))
                 (setf code (logior (ash code 6) (logand b #x3F)))
                 (return (values #\? 0)))))))       ; a broken sequence: one ?, not junk
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
;;; It is OFF unless asked for (*TAP-TO-CLICK*): only a physical press clicks.

(defvar *tap-to-click* nil
  "Does a tap on the touchpad click? NIL (the default, the user's choice 2026-10-16):
   only a PHYSICAL press of the pad clicks, and a finger merely touching it never
   does, however firm or brief. T turns the tap rule below back on. Read from
   ~/.lol/settings (:TAP-TO-CLICK T) when the browser starts; setting it in
   the Workspace takes effect on the next contact.")
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
   both 'down' — 0 release). Returns :MENU on a Menu-key press, #\\= #\\+ #\\0 for Ctrl
   with = or 0 (the console sends no byte for those), else NIL. Shared by
   READ-KEYBOARD-EVDEV and READ-MOUSE, so a device that is both (some wireless combo
   receivers) keeps its modifiers too. While the multi-device reader is draining a
   device these specials are THAT DEVICE's own (WITH-DEVICE-STATE)."
  (cond
    ((or (= code 29) (= code 97))   (setf *ctrl-down* (plusp val)) nil)   ; KEY_LEFTCTRL / RIGHTCTRL
    ((or (= code 42) (= code 54))   (setf *shift-down* (plusp val)) nil)  ; KEY_LEFTSHIFT / RIGHTSHIFT
    ((or (= code 125) (= code 126)) (setf *super-down* (plusp val)) nil)  ; KEY_LEFTMETA / RIGHTMETA (Super)
    ((or (= code 56) (= code 100))  (setf *alt-down* (plusp val)) nil)    ; KEY_LEFTALT / RIGHTALT
    ((and (= code 127) (= val 1))   :menu)                                ; KEY_COMPOSE: the Menu key
    ;; Ctrl with 0 or = : the console keymap (drivers/tty/vt/defkeymap.map) gives keycodes
    ;; 11 and 13 no CONTROL column, so the tty sends NO byte for C-0 / C-= — found when
    ;; check-font.lisp's C-x C-= did nothing and the next plain = completed the chord.
    ;; Report them as characters for the text-size chord (doc/fonts.org). Ctrl-- needs
    ;; no help: it has one (Control_underscore, byte 31).
    ((and *ctrl-down* (plusp val) (member code '(11 13)))                  ; KEY_0 / KEY_EQUAL
     (cond ((= code 11) #\0) (*shift-down* #\+) (t #\=)))
    ;; Ctrl with , or . (so also C-< / C->): the console keymap gives keycode 51 no
    ;; control entry and binds control-52 to Compose — neither sends a byte. Report
    ;; them as < and >, the start / end of the buffer in every text (TV-KEY,
    ;; MINIBUFFER-KEY), with or without the Shift that < and > take.
    ((and *ctrl-down* (plusp val) (member code '(51 52)))                  ; KEY_COMMA / KEY_DOT
     (if (= code 51) #\< #\>))
    ;; Ctrl-; : keycode 39 has no control entry either. It comments the line or the
    ;; region (EDITOR-TOGGLE-COMMENT).
    ((and *ctrl-down* (plusp val) (= code 39)) #\;)                       ; KEY_SEMICOLON
    ;; Shift-PageUp/PageDown: the console keymap binds them to ITS OWN scrollback
    ;; (shift_map: Scroll_Backward / Scroll_Forward) and sends no byte, so this is the
    ;; only place they can be seen. A terminal pane scrolls its history with them
    ;; (doc/terminal.org); elsewhere they reach BROWSER-KEY as Shift-PageUp/Down.
    ((and *shift-down* (plusp val) (= code 104)) :shift-page-up)          ; KEY_PAGEUP
    ((and *shift-down* (plusp val) (= code 109)) :shift-page-down)        ; KEY_PAGEDOWN
    ;; The laptop's volume keys (Fn-F1..F3 on the XPS) arrive on the AT keyboard's
    ;; evdev and send no byte to the tty either (no keymap entry). The host's StumpWM
    ;; binds the same keys (XF86Audio*) to pactl; here MIXER.LISP's AUDIO-KEY writes
    ;; the card's controls itself. A held key repeats (value 2), as on the desktop.
    ((and (plusp val) (= code 115)) :volume-up)                         ; KEY_VOLUMEUP
    ((and (plusp val) (= code 114)) :volume-down)                       ; KEY_VOLUMEDOWN
    ((and (= val 1) (= code 113)) :mute)                                ; KEY_MUTE
    ((and (= val 1) (= code 248)) :mic-mute)                            ; KEY_MICMUTE
    ;; Caps Lock, when the host's layouts include br (keyboard-layout.lisp): its keymap
    ;; entry is a VoidSymbol then, so this press is the only trace — X's grp:caps_toggle
    ((and (= val 1) (= code 58)) :layout-toggle)                        ; KEY_CAPSLOCK
    (t nil)))

(defun unbind-console-ctrl-dot (&optional (fd 0))
  "Take Compose off Ctrl-. in the console's keymap. drivers/tty/vt/defkeymap.map binds
   'control keycode 52 = Compose', so Ctrl-. (and Ctrl-> — the same key) starts a
   compose sequence that swallows the next keys; the environment uses that chord for
   end-of-buffer (NOTE-MODIFIER-KEY, TV-KEY). KDSKBENT (0x4B47) sets ONE entry: table 4
   (the Ctrl map), index 52, value K_HOLE (0x0200, VoidSymbol: no byte). Returns T when
   the ioctl succeeded (NIL off a console — on the host, or a serial line)."
  (sb-alien:with-alien ((e (sb-alien:array sb-alien:unsigned-char 4)))
    (setf (sb-alien:deref e 0) 4            ; kb_table: the control map
          (sb-alien:deref e 1) 52           ; kb_index: KEY_DOT
          (sb-alien:deref e 2) #x00         ; kb_value K_HOLE = 0x0200, little-endian
          (sb-alien:deref e 3) #x02)
    (zerop (sb-alien:alien-funcall
            (sb-alien:extern-alien "ioctl" (function sb-alien:int sb-alien:int sb-alien:unsigned-long
                                                     (* (sb-alien:array sb-alien:unsigned-char 4))))
            fd #x4B47 (sb-alien:addr e)))))

(defun console-kbent (table index &optional value (fd 0))
  "Read (KDGKBENT, 0x4B46) the console keymap's entry for keycode INDEX in TABLE (0
   plain, 1 Shift, 4 Ctrl, 8 Alt — a bit per modifier), or with VALUE set it
   (KDSKBENT, 0x4B47; needs CAP_SYS_TTY_CONFIG, which PID 1 keeps). Returns the
   entry's value (a K(type,value) code, linux/keyboard.h), or NIL when the ioctl
   fails (off a console). struct kbentry: kb_table, kb_index (bytes), kb_value (u16)."
  (sb-alien:with-alien ((e (sb-alien:array sb-alien:unsigned-char 4)))
    (setf (sb-alien:deref e 0) table
          (sb-alien:deref e 1) index
          (sb-alien:deref e 2) (ldb (byte 8 0) (or value 0))
          (sb-alien:deref e 3) (ldb (byte 8 8) (or value 0)))
    (when (zerop (sb-alien:alien-funcall
                  (sb-alien:extern-alien "ioctl" (function sb-alien:int sb-alien:int sb-alien:unsigned-long
                                                           (* (sb-alien:array sb-alien:unsigned-char 4))))
                  fd (if value #x4B47 #x4B46) (sb-alien:addr e)))
      (logior (sb-alien:deref e 2) (ash (sb-alien:deref e 3) 8)))))

(defconstant +k-hold+ #x0209 "K(KT_SPEC,9): the console's Hold, fn_hold in drivers/tty/vt/keyboard.c.")
(defconstant +k-hole+ #x0200 "K(KT_SPEC,0): VoidSymbol, a key that does nothing.")

(defun unbind-console-hold (&optional (fd 0))
  "Take Hold off Scroll Lock in every table of the console keymap. Hold is the VT's
   own pause: fn_hold calls stop_tty(), which stops the console's OUTPUT until the key
   is pressed again — whatever the termios flags say, so clearing IXON (line-editor.lisp)
   does not cover it. A stopped console makes the next write to it block, and the
   environment's thread writes there (*error-output*): found 2026-10-17, after the C-s
   freeze (doc/diagnostics.org §1), by a QMP scroll_lock in QEMU. defkeymap binds it
   to keycode 70 (KEY_SCROLLLOCK) in the plain and Alt tables; all 256 tables are
   scanned, since a loaded keymap may put it elsewhere in that key's row. Returns how
   many entries were changed (0 off a console, or when none held it)."
  (loop for table below 256
        count (and (eql (console-kbent table 70 nil fd) +k-hold+)
                   (console-kbent table 70 +k-hole+ fd))))

(defparameter +console-switch-entries+ '(#x0210 #x0211)
  "K_DECRCONSOLE / K_INCRCONSOLE (K(KT_SPEC,16/17), linux/keyboard.h) as KDGKBENT
   reports them. defkeymap.c_shipped writes 0xf210 / 0xf211: the 0xf0 there is its
   U() marker, which the kernel strips — read back in the guest on 2026-10-18 they are
   0x0210 / 0x0211, the same form as +K-HOLD+.")

(defun unbind-console-alt-arrows (&optional (fd 0))
  "Take VT switching off Alt-Left / Alt-Right, so they reach the environment as
   M-<left> / M-<right> (Org's promote and demote, doc/org.org §6.2; a terminal's
   Alt-arrows). defkeymap binds alt_map[105] / [106] (KEY_LEFT / KEY_RIGHT) to the
   previous / next console; this binds every such entry, in every table with the Alt
   bit (8), to the cursor key itself — K_LEFT / K_RIGHT, K(KT_CUR,1/2), which is what
   alt_map already holds for Up and Down. Kernel 7.2's k_cur (keyboard.c) encodes the
   held modifiers xterm's way, so Alt-Left then arrives as ESC [ 1 ; 3 D, like
   Alt-Up's ESC [ 1 ; 3 A, and FB-READ-CSI turns the 3 into the Meta bit. Alt-Shift-
   arrows needed nothing: there is no Shift+Alt table, and for a cursor key without
   one keyboard.c falls back to the plain map, so they arrive as ESC [ 1 ; 4 x.
   Two things ruled out on the way (check-org.lisp, 2026-10-18): reading Alt-arrows
   from the evdev as well made Alt-Shift-Right demote TWICE, once per path; and
   K_HOLE instead of K_LEFT does NOT trigger that fallback — keyboard.c compares the
   stored entry with K_HOLE, but entries are stored as U(val) = val ^ 0xf000, so a
   hole set by KDSKBENT reads 0xf200 there and never matches; Alt-Left then sent
   nothing at all. Ctrl-Alt-F1... still switch VTs. Returns how many entries changed."
  (loop for table below 256
        when (logbitp 3 table)
          sum (loop for (code key) in '((105 #x0601) (106 #x0602))   ; K_LEFT, K_RIGHT
                    count (and (member (console-kbent table code nil fd) +console-switch-entries+)
                               (console-kbent table code key fd)))))

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
   records were read.

   MENU is instead a CHARACTER — #\\= #\\+ or #\\0 — for Ctrl held with = or 0 (see
   NOTE-MODIFIER-KEY): the text-size chord C-x C-= / C-x C-0 (doc/fonts.org)."
  (unless (integerp fd) (return-from read-keyboard-evdev (read-keyboards)))
  (sb-alien:with-alien ((e (sb-alien:array sb-alien:unsigned-char 24)))
    (loop with menu = nil and count = 0 do
      (let ((n (%fbb-read fd (sb-alien:cast e (sb-alien:* sb-alien:unsigned-char)) 24)))
        (when (< n 24) (return (values menu count)))
        (incf count)
        (let ((type (logior (sb-alien:deref e 16) (ash (sb-alien:deref e 17) 8)))
              (code (logior (sb-alien:deref e 18) (ash (sb-alien:deref e 19) 8)))
              (val  (logior (sb-alien:deref e 20) (ash (sb-alien:deref e 21) 8))))
          (when (= type 1)                               ; EV_KEY: :menu, a char, or NIL
            (let ((r (note-modifier-key code val)))
              (when r (setf menu r)))))))))

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
                               *tap-live* (and *tap-to-click* *pointer-pad-p*))
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
        ;; every loop in the environment sleeps HERE, so this is where its thread is
        ;; idle — and busy from the moment poll returns until it comes back. The
        ;; watcher (session-log.lisp) reports a STALL when busy lasts too long.
        (note-main-idle)
        (%fbb-poll (sb-alien:cast fds (sb-alien:* sb-alien:unsigned-char)) n timeout-ms)
        (note-main-busy)
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

;;; Every write of ours to /dev/fb0 is in the five functions below — PUT-CURSOR,
;;; PRESENT-FRAME, PRESENT-REPAIR, MOVE-CURSOR, HIDE-CURSOR — which is what makes
;;; these two hooks enough (doc/screencast.org §1, §8.2):
(defvar *frame-tap* nil
  "A function called with (RECTS FULL) each time a repair (its rects) or a whole frame
   (FULL) is presented:
   the screen recorder's \"the canvas changed\" (recorder.lisp). NIL: nothing to tell.")
(defvar *screen-lent* nil
  "True while another program owns /dev/fb0 — the host's MPlayer playing a video
   full screen (player.lisp). Only one process may write the screen at a time
   (doc/processes.org), so while it is lent nothing of ours is sent: the world goes
   on repairing the CANVAS, and the whole frame is presented when the screen comes
   back.")
(defvar *pointer-active-at* 0 "Internal-real-time of the last mouse activity.")

(defun ms-since (time)
  "Milliseconds elapsed since internal-real-time TIME."
  (round (* 1000 (- (get-internal-real-time) time)) internal-time-units-per-second))

;; (before PUT-CURSOR, which sets them: a variable set before its DEFVAR is a build WARNING)
(defvar *cursor-shown-x* 0 "Where PUT-CURSOR last drew the arrow: what a repair that
   moved it must put back (PRESENT-REPAIR).")
(defvar *cursor-shown-y* 0)

(defun put-cursor (canvas x y)
  "Show the pointer at X,Y over CANVAS's pixels, sending only the arrow's patch."
  (when *screen-lent* (return-from put-cursor nil))
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
    (lol.fb:present-fb patch :x0 x :y0 y)
    (setf *cursor-shown-x* x *cursor-shown-y* y)))

(defun present-frame (canvas)
  "Send a freshly drawn CANVAS to the screen, then the pointer over it (unless it is
   hidden for inactivity)."
  (when *frame-tap* (funcall *frame-tap* nil t))
  (when *screen-lent* (return-from present-frame nil))
  (lol.fb:present-fb canvas)
  (when (and *fb-mouse* (not *cursor-hidden*)) (put-cursor canvas *cursor-x* *cursor-y*)))

(defun cursor-over-rect-p (r)
  "Does the arrow, drawn at the cursor, overlap rect R (x y w h)?"
  (destructuring-bind (x y w h) r
    (and (< (- x +cursor-w+) *cursor-x* (+ x w))
         (< (- y +cursor-h+) *cursor-y* (+ y h)))))

(defun present-repair (canvas &optional rects full)
  "Send what LOL.MORPH:WORLD-REPAIR just redrew: the whole frame when FULL (then
   exactly what the loop always did), else each rect alone. The pointer goes back on
   top if a rect ran under it, and is moved if it moved since it was last drawn."
  (when (and *frame-tap* (or full rects)) (funcall *frame-tap* rects full))
  (when *screen-lent* (return-from present-repair nil))
  (cond
    (full (present-frame canvas))
    (rects
     (dolist (r rects) (apply #'lol.fb:present-fb-rect canvas r))
     (when (and *fb-mouse* (not *cursor-hidden*))
       (if (or (/= *cursor-shown-x* *cursor-x*) (/= *cursor-shown-y* *cursor-y*))
           (move-cursor canvas *cursor-shown-x* *cursor-shown-y*)
           (when (some #'cursor-over-rect-p rects)
             (put-cursor canvas *cursor-x* *cursor-y*)))))))

(defun move-cursor (canvas old-x old-y)
  "The pointer moved and nothing else changed: put back what was under it (straight
   from CANVAS), then draw it at its new place. Two tiny writes instead of a frame.
   A hidden pointer has nothing to put back; it just reappears."
  (when *screen-lent* (return-from move-cursor nil))
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
  (when *screen-lent* (return-from hide-cursor nil))
  (unless *cursor-hidden*
    (lol.fb:present-fb-rect canvas *cursor-x* *cursor-y* +cursor-w+ +cursor-h+)
    (setf *cursor-hidden* t)))

(defun cursor-idle-timeout ()
  "Ms until the visible pointer is due to hide, or -1 when it is hidden or absent."
  (if (or *cursor-hidden* (null *fb-mouse*))
      -1
      (max 0 (- +cursor-idle-ms+ (ms-since *pointer-active-at*)))))

;;; The systray's clock is the second thing on screen that changes without input
;;; (the stage is the first). It does NOT repaint the frame: the TRAY morph repaints
;;; its own strip of the canvas (TICK-TRAY, poc/morphic.lisp), and only that rectangle
;;; is sent — the same economy as the pointer.
(defun tray-timeout ()
  "Ms until the clock's next tick: the next wall-clock second, or minute when the
   tray shows no seconds (*CLOCK-SECONDS*), plus a few ms so we land just past it."
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (+ 5 (- (if *clock-seconds* 1000 (* 1000 (- 60 (mod sec 60))))
            (floor usec 1000)))))

(defun earliest-timeout (&rest timeouts)
  "The soonest of several poll timeouts in ms, where -1 means 'no deadline'."
  (let ((real (remove -1 timeouts)))
    (if real (reduce #'min real) -1)))

;;; ---- the stage's heartbeat (§3b) -----------------------------------------
;;; The browser has always been a pure function of state that only input changes, and
;;; the poll that sleeps forever is how it stays at zero CPU on an idle console. A
;;; running stage breaks that, so it is allowed to — but ONLY while one is actually on
;;; screen and running. It is a STEP now (doc/morphic.org step 5): the morph that shows
;;; the stage asks to be stepped every +STAGE-FRAME-MS+ (poc/morphic.lisp,
;;; STEP-STAGE-PANE), the poll's deadline is the world's next step
;;; (LOL.MORPH:WORLD-STEP-TIMEOUT), and the moment nothing on screen steps that
;;; deadline is -1 again: sleep until a key.

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

(defvar *services-running* '()
  "The services whose :RUN is on the stack right now. A :RUN can open an overlay that
   waits (a web page's <select> becomes our menu) and so turn the loop again from
   inside itself; that inner turn must not run the same service again, nor poll its
   fd — it would read the pipe the outer call is in the middle of, and an fd nobody
   reads would wake the poll at once, forever. So a running service sits the inner
   turns out, exactly as it did when the menu was a loop of its own.")

(defun fb-service-call (key &rest args)
  "Call every service's KEY function with ARGS; -> the list of their non-NIL values.
   A service that signals is reported in the status line, not allowed to kill PID 1.
   A service already inside its :RUN is skipped (*SERVICES-RUNNING*)."
  (loop for s in *fb-services*
        for fn = (getf s key)
        for v = (and fn (fboundp fn)
                     (not (member (getf s :name) *services-running*))
                     (handler-case
                         (if (eq key :run)
                             (let ((*services-running* (cons (getf s :name) *services-running*)))
                               (apply fn args))
                             (apply fn args))
                       (serious-condition (c)
                         (setf *browser-status* (format nil "~(~a~): ~a" (getf s :name) c))
                         nil)))
        when v collect v))

(defparameter +guest-font+ "/usr/share/lol/mono.ttf"
  "Where the image keeps its outline font: build.sh copies the build host's monospace
   TrueType file there ($LOL_FONT, else DejaVu Sans Mono — doc/fonts.org §6).")
(defun fb-service-fds ()
  "Every service's fds, flattened: a service's :FD may return one fd or a LIST of
   them — the terminals are one service with a pty master per open terminal."
  (loop for v in (fb-service-call :fd)
        if (listp v) append v else collect v))

;;; The terminals (doc/terminal.org) are a service: their pty masters wake the poll,
;;; and :RUN drains every live one (non-blocking, so cheap when nothing came) and
;;; repaints only the ROWS their programs changed — a shell echoing a key costs one
;;; row, not a frame — the tiled terminal's straight onto the screen, a window's into
;;; its backing store (TERMINAL-ROWS-TO-WORLD, poc/morphic.lisp).
(defun terminal-service-run (canvas dirty)
  (declare (ignore dirty))
  (let ((r (terminal-service (terminal-fds))))
    (cond ((eq r :full) :redraw)
          ((eq r :rows) (terminal-rows-to-world canvas) nil))))

(fb-register-service :terminals :fd 'terminal-fds :run 'terminal-service-run)

;;; ---- what happened, as events ------------------------------------------------
;;; The loop used to act on each input as it decoded it. Now it only DECODES — into
;;; LOL.MORPH events (poc/morph.lisp) — and the world decides who acts: the focused
;;; morph for a key, the morph under the pointer for a click. The decoding itself is
;;; the old loop's, step for step and in the same order (the keyboard's evdev, then
;;; the console, then the pointers), so nothing about input changed meaning.

(defun input-events (kb ms kev xres yres)
  "The events the last poll's readable fds hold, in the order the old loop handled
   them; :EOF in the list means the console is gone and the environment must end.
   KB, MS, KEV say which of the console, the pointers and the keyboards' evdev were
   ready (FBB-WAIT's values)."
  (let ((events '()))
    (flet ((emit (kind &rest args)
             (push (apply #'lol.morph:make-event :kind kind
                          :x *cursor-x* :y *cursor-y* args)
                   events)))
      ;; refresh Ctrl/Shift/Super first; the Menu key opens the context menu where
      ;; the caret (or selected row) is
      (let ((ev (and kev (read-keyboard-evdev *fb-kbd*))))
        ;; (:EVDEV marks a key the console never saw: the modal overlays ignore those,
        ;; as the loops they replaced never read them — poc/overlays.lisp)
        (cond ((eq ev :menu) (emit :menu))
              ;; seen only on the evdev: the console keeps them for its own
              ;; scrollback (a terminal pane's history)
              ((eq ev :shift-page-up) (emit :key :key :page-up :state 1 :data '(:evdev t)))
              ((eq ev :shift-page-down) (emit :key :key :page-down :state 1 :data '(:evdev t)))

              ;; the volume keys act at once, whatever has the focus (mixer.lisp);
              ;; the status line says the new level, the tray follows
              ;; Caps Lock switches the layout, as on the host (keyboard-layout.lisp)
              ((eq ev :layout-toggle)
               (when (and (boundp '*layouts-enabled*) (symbol-value '*layouts-enabled*))
                 (setf *browser-status* (funcall 'toggle-layout))))
              ((member ev '(:volume-up :volume-down :mute :mic-mute))
               (when (fboundp 'audio-key)
                 (setf *browser-status* (ignore-errors (funcall 'audio-key ev)))
                 (funcall 'forget-sound-tray)))
              ;; a Ctrl chord the console sends no byte for
              ((characterp ev)
               (emit :key :key ev :state (logior 4 (if *super-down* 64 0)) :data '(:evdev t)))))
      (when kb
        (multiple-value-bind (key state) (fb-read-key 0)
          ;; Ctrl-/ reaches the tty as DEL, Backspace's own byte (defkeymap:
          ;; ctrl_map[53] = 0x7f), so only the held Ctrl tells them apart: undo, as
          ;; C-/ is in Emacs
          (when (and (eq key :backspace) (zerop state) *ctrl-down*)
            (setf key #\/ state 4))
          (if (eq key :eof)
              (push :eof events)
              ;; The cooked tty sends a bare ESC[A for EVERY arrow and no byte for a
              ;; held modifier — it lives only in the evdev state, so fold it in
              ;; (Ctrl-arrow = word, Shift = select, Super = the spatial layer).
              (emit :key :key key
                         :state (logior state (if *ctrl-down* 4 0) (if *shift-down* 1 0)
                                        (if *super-down* 64 0))
                         :data '(:tty t)))))
      (when ms
        (let ((ox *cursor-x*) (oy *cursor-y*) (was-down *button-down*))
          (multiple-value-bind (clicked wheel button)
              (read-mouse *fb-mouse* xres yres)        ; every ready pointer
            ;; a bare move (no button held before or now): it changes nothing on
            ;; screen, but a menu's hover follows it and a carried window moves
            (when (and (not was-down) (not *button-down*)
                       (or (/= ox *cursor-x*) (/= oy *cursor-y*)))
              (emit :move))
            ;; wheel first: a notch scrolls the pane under focus a few lines
            (unless (zerop wheel) (emit :wheel :notches wheel))
            ;; Alt-click is the middle button, for a pointer that has none
            ;; (doc/input.org, "Three buttons")
            (when (and clicked *alt-down*)
              (setf clicked nil button :middle *button-down* nil))
            ;; a TAP is a click with no button held, and no release to come: marked,
            ;; so what waits for a release (a row, which a press that moves would pick
            ;; up instead — doc/morphic.org §13) acts at once
            (when clicked
              (emit :down :state (logior (if *ctrl-down* 4 0) (if *super-down* 64 0))
                          :data (unless *button-down* '(:tap t))))
            (case button
              (:right (emit :right))
              (:middle (emit :middle :state (if *super-down* 64 0))))
            ;; moved with the button still held: a drag
            (when (and *button-down* (not clicked)
                       (or (/= ox *cursor-x*) (/= oy *cursor-y*)))
              (emit :drag))
            ;; the button came up
            (when (and was-down (not *button-down*))
              (emit :up))))))
    (nreverse events)))

;;; ---- the cycle ------------------------------------------------------------------
;;; One turn of the environment's loop. RUN-BROWSER-FB turns it until you leave; a
;;; blocking overlay turns it too while it waits for its answer (RUN-OVERLAY), so
;;; everything below goes on behind a menu or the debugger — except the steps and the
;;; agent's tick, which wait with it (poc/morphic.lisp says why).

(defun fb-cycle ()
  "Repair what is damaged and send it; sleep in poll until something happens — input,
   a service's fd, the earliest deadline (a morph's step, the pointer's idle hide, the
   clock, a service); turn what happened into events and let the world route them;
   then the things that change the screen without an event (the services, the steps,
   the clock). -> :QUIT when the environment must end. A held modifier or a bare
   pointer move damages nothing, so neither redraws."
  (let* ((canvas *fb-canvas*) (xres *fb-xres*) (yres *fb-yres*)
         (waiting (plusp *blocking-overlays*)))
    ;; the agent (agent-ui.lisp): stream a reply into its panes and run the tool calls
    ;; its turn posted — on THIS thread, so not while something below is waiting. What
    ;; it changed (a conversation's text, a tab's dot, the tray's cost) the repair's
    ;; own look at the morphs' signatures finds (DESK-SYNC, poc/morphic.lisp).
    (when (and (not waiting) (fboundp 'agent-ui-tick))
      (funcall 'agent-ui-tick))
    (multiple-value-call #'present-repair canvas (lol.morph:world-repair *world* canvas))
    (multiple-value-bind (kb ms kev xs)
        (fbb-wait 0 *fb-mouse* *fb-kbd*
                  (apply #'earliest-timeout
                         ;; a running stage, a live run pane: the morph that shows it
                         ;; steps; nothing stepping is -1, sleep until input
                         (lol.morph:world-step-timeout *world*)
                         (cursor-idle-timeout)
                         (tray-timeout)
                         (runs-timeout)              ; a stop waiting for its SIGKILL
                         ;; a conversation streaming (agent-ui.lisp)
                         (if (and (not waiting) (fboundp 'agent-ui-timeout))
                             (funcall 'agent-ui-timeout) -1)
                         (fb-service-call :timeout))
                  (fb-service-fds))
      (let ((ox *cursor-x*) (oy *cursor-y*)
            (was-hidden (and ms (pointer-activity))))
        ;; what happened, as events, routed by the world: keys to the focus (or the
        ;; overlay on top), the pointer to the morph under it
        (dolist (e (input-events kb ms kev xres yres))
          (when (or (eq e :eof)
                    ;; the screen lent to the player (player.lisp): its keys are the
                    ;; player's, turned into MPlayer's commands; the world gets none
                    (if (and *screen-lent* (fboundp 'player-event))
                        (progn (funcall 'player-event e) nil)
                        (eq :quit (lol.morph:world-dispatch *world* e)))
                    *world-quit*)
            (return-from fb-cycle :quit)))
        ;; a bare move: the frame is still right, only the arrow is in the wrong place
        ;; (a repair would move it anyway)
        (when (and ms (not (lol.morph:world-damaged-p *world*))
                   (or was-hidden (/= ox *cursor-x*) (/= oy *cursor-y*)))
          (move-cursor canvas ox oy)
          ;; the screen point: a page finds itself under it, in a window too (a page
          ;; under an overlay gets no hover — the overlay has the pointer)
          (unless (top-overlay)
            (fb-service-call :motion *cursor-x* *cursor-y*))))
      ;; a run asked to stop that has not: SIGKILL after the grace
      (escalate-run-stops)
      ;; a closed tab or a popped pane may have taken a terminal with it: hang its
      ;; program up (SIGHUP, as closing a window)
      (when (or kb ms kev) (terminal-sweep))
      ;; the services (a web page's frames, the terminals, the live workers): every
      ;; turn, since they also react to what input just did. A frame, a terminal's rows
      ;; are painted where their pane is (PAINT-INTO); what else a service changed the
      ;; desk finds by the morphs' signatures (ENVIRONMENT-CHANGED)
      (when (member :redraw (fb-service-call :run canvas (lol.morph:world-damaged-p *world*)))
        (environment-changed))
      ;; the morphs that change on their own: each damages only what it changed
      (lol.morph:world-step *world*)
      ;; nothing was ready: a deadline fired. An idle pointer hides; the clock is cheap,
      ;; so it simply ticks on every timeout (a step's frame repaints an unchanged tray)
      (unless (or kb ms kev xs)
        (when (zerop (cursor-idle-timeout))
          (hide-cursor canvas))
        (tick-tray canvas)))
    nil))

;;; ---- one turn's error is not the environment's death -------------------------
;;; An error in a turn (a key's command, a morph's drawing, a service) used to unwind
;;; out of the loop: the environment ended, its tabs were dropped to be safe, and the
;;; next 'b' started bare — unsaved Workspace text and every open tab gone for one bad
;;; keystroke (review of 2026-10-16). Now a turn that fails is logged with its
;;; backtrace, said on the status line, and the loop goes on, with the whole screen
;;; marked for repair (the failed turn may have stopped halfway through drawing).
;;; Only a run of failures with no good turn between them, which means the error is
;;; in the drawing itself and will come back every turn, still ends the environment.

(defparameter +cycle-failure-limit+ 3
  "Failed turns in a row (no good turn between them) that end the environment.")

(defvar *cycle-failures* '() "Conditions of the failed turns since the last good one.")

(defun guarded-fb-cycle ()
  "FB-CYCLE, surviving an error in it (see above). -> what FB-CYCLE returned, or NIL."
  (block turn
    (handler-bind
        ((error
           (lambda (c)
             ;; logged HERE, before unwinding, so the backtrace is the error's own
             (ignore-errors
               (session-log-block (format nil "a turn of the environment failed: ~s ~a" (type-of c)
                                          (or (ignore-errors (clip-one-line c 300)) "?"))
                                  (backtrace-string :count 30)))
             (push c *cycle-failures*)
             (when (>= (length *cycle-failures*) +cycle-failure-limit+)
               (session-log "~d turns failed in a row: leaving the environment" (length *cycle-failures*))
               (return-from turn (error c)))       ; RUN-BROWSER-FB's handler ends it, as before
             (setf *browser-status*
                   (format nil "error: ~a — logged (~~/.lol/log/session.log); carrying on"
                           (or (ignore-errors (clip-one-line c 120)) (type-of c))))
             (ignore-errors (repair-everything))
             (return-from turn nil))))
      (prog1 (fb-cycle)
        (setf *cycle-failures* '())))))

(defun repair-everything ()
  "Mark the whole screen, and every window's backing store, as needing a repaint."
  (when *world*
    (dolist (m (lol.morph:morph-descendants *world*))
      (when (typep m 'lol.morph:surface) (setf (lol.morph:surface-damage m) :all)))
    (lol.morph:invalidate *world*)))

(defun run-browser-fb (&optional root)
  "Launch the browser full-screen on /dev/fb0, rooted at ROOT. The supervisor calls
   it once at boot, and again from its menu ('b', 'p', 'c', 'l'). Blocks until you
   leave it (SuperL-q, which asks: the supervisor's console or power off; or q on a
   list pane), then restores the text console and returns to the supervisor menu,
   or powers off. An error in one turn is logged and the loop goes on
   (GUARDED-FB-CYCLE); only errors turn after turn end it, and those are caught
   and printed, so a dying environment drops you at the menu instead of crashing
   PID 1.

   ROOT defaults to the WORKSPACE alone. It used to be the module index with a
   Workspace tab beside it; but the environment is something you WORK in, and a
   table of contents you did not ask for was the first thing in the way. The index
   is one key away (SuperL-m; it is still the only view that shows the whole system,
   lol.* toolkit included), the flat CL-USER package is SuperL-l, and anything at all
   is SuperL-Space."
  (handler-case
      (progn
        ;; the font: the TrueType file build.sh copied in, at the size the user last
        ;; chose (~/.lol/settings); the bitmap blob if that file is missing or
        ;; empty. Loaded ONCE — SuperL-= resizes this same object (doc/fonts.org).
        (unless *bfont*
          (set-browser-font
           (open-browser-font (list +guest-font+) "/cozette.lolf"
                              :size (let ((s (setting :font-size))) (and (integerp s) s)))))
        ;; installed by NAME, not by #', so that redefining one at the REPL takes
        ;; effect at once (both are overlays, poc/overlays.lisp)
        (setf *value-prompt* 'run-value-prompt)   ; the shell asks; an overlay reads
        (setf *chooser* 'run-chooser)             ; ... and shows its menus and halos
        (ignore-errors (unbind-console-ctrl-dot)) ; Ctrl-. is end-of-buffer, not Compose
        (ignore-errors (unbind-console-hold))     ; Scroll Lock must not stop the console
        (when (fboundp 'setup-keyboard-layouts)    ; pt-BR under Right Alt, Caps Lock
          (ignore-errors (funcall 'setup-keyboard-layouts)))   ; toggles (the host's X)
        (ignore-errors (unbind-console-alt-arrows)) ; Alt-Left/Right are keys, not VT switching
        ;; the touchpad: does a tap click? Off unless the settings say so.
        (setf *tap-to-click* (eq (setting :tap-to-click) t))
        (setf *fm-interrupt-fn* 'fb-stop-requested-p) ; ... and lets Esc stop a long find
        (session-log "entering the environment")
        (open-environment root)        ; NIL: the desk as it was (shell.lisp)
        (multiple-value-bind (xres yres) (lol.fb:fb-geometry)
          (let ((canvas (lol.canvas:make-canvas xres yres)))
            ;; every keyboard and pointer, plus a watch for the ones still to come;
            ;; sets *FB-MOUSE* / *FB-KBD* and clears every held modifier (§4¾)
            (open-input-devices)
            (setf *cursor-x* (floor xres 2) *cursor-y* (floor yres 2)
                  *fb-canvas* canvas *fb-xres* xres *fb-yres* yres
                  *cursor-hidden* nil *pointer-active-at* (get-internal-real-time)
                  ;; the windows of the last time round (the console and back), unless
                  ;; the desk file just gave some (desk.lisp, at boot)
                  *windows-to-restore* (or *windows-to-restore*
                                           (and *world* (desk-window-layout)))
                  ;; the world: the tiled shell as one full-screen morph, all damaged
                  *world* (make-desk xres yres))
            (unwind-protect
                 (lol.fb:with-graphics-console         ; KD_GRAPHICS: fbcon stops drawing
                   ;; signals OFF so Ctrl-C arrives as a byte (the C-c C-c Accept chord)
                   (with-raw-mode (0 nil)
                     ;; THE CYCLE (doc/morphic.org §4.2), one turn at a time: FB-CYCLE.
                     ;; The world starts wholly damaged, so the first turn draws the
                     ;; frame. A blocking overlay (a menu, the debugger) turns the same
                     ;; function while it waits (RUN-OVERLAY, poc/morphic.lisp) —
                     ;; the GUARDED one, so a failing turn under a menu survives too.
                     (unwind-protect
                          (progn
                            (setf *world-cycle* 'guarded-fb-cycle *world-quit* nil
                                  *cycle-failures* '())
                            (loop until (or (eq :quit (guarded-fb-cycle)) *world-quit*)))
                       (setf *world-cycle* nil))))
              (close-input-devices)))))
    (serious-condition (c)
      (log-condition "the environment died" c)
      ;; start the next 'b' afresh: the trails it died with may be why it died
      (setf *trails* nil)
      (format t "~&code browser: ~a~%" c) (finish-output)))
  (note-main-away)                       ; the text console now: not watched for stalls
  (session-log "left the environment~@[ (~(~a~))~]" *leave-action*)
  ;; SuperL-q chose to power off: only now, with the text console back, so the
  ;; kernel's last words (and POWER-OFF's own) are readable if it is refused
  (case (shiftf *leave-action* nil)
    (:power-off (funcall 'leave-without-saving) (power-off))   ; both in desk.lisp, loaded later
    ;; the desk is written HERE, before POWER-OFF's release-disk makes /home read-only
    (:save-power-off (funcall 'save-desk) (power-off))
    ;; restarts (process.lisp): kexec into /boot/lisp's image when the kernel can,
    ;; else the firmware. :UPDATE-RESTART installed the new build already, while the
    ;; environment was still on screen (BUILD-INSTALLED-P, poc/shell.lisp)
    (:restart (funcall 'leave-without-saving) (restart-machine))
    ((:save-restart :update-restart)
     (if (funcall 'persistence-available-p)
         (funcall 'save-desk)
         (funcall 'leave-without-saving))
     (restart-machine))))

(defun fb-stop-requested-p ()
  "True when a key is waiting and it is Esc or C-g — the file manager's recursive find
   polls this between folders (*FM-INTERRUPT-FN*), so a search that is taking too long
   can be stopped from the keyboard. Any OTHER key read here is lost: typing ahead
   during a find is not supported, and the find takes three seconds at most anyway."
  (and (fbb-ready-p 0 0)
       (multiple-value-bind (key state) (fb-read-key 0)
         (or (eq key :escape) (and (logbitp 2 state) (member key '(#\g #\G)))))))

;;; ---- the debugger: a condition made interactive (§3) ---------------------
;;; Entered from a HANDLER-BIND handler (WORKSPACE-DEBUG-IT, STEP-STAGE-PANE) — not
;;; from *DEBUGGER-HOOK*, which nothing sets — with the stack STILL STANDING, so the
;;; restarts are live and invoking one actually transfers control. It is an OVERLAY
;;; (poc/morphic.lisp) that its caller waits in, turning the environment's own loop —
;;; so a terminal behind it keeps printing — and it is left only by invoking a restart
;;; (there is no standard debugger to fall through to as PID 1).

;;; What the debugger drew where — the first restart row, the frame column, the
;;; locals strip — DRAW-DEBUGGER returns as a plist, its GEOMETRY, which the
;;; debugger's overlay keeps and its clicks read back (DBG-RESTART-AT & co.): like
;;; every other hit map in the environment, it belongs to the morph that drew it.
;;; The debugger's row metrics follow the CURRENT font (doc/fonts.org §5). They were
;;; constants (17 and 15 px, for the 6x15 font); kept under their old names as symbol
;;; macros — like lol.widgets' +ROW-H+ — so every use below reads unchanged.
(defun dbg-row-h () "A restart row: the cell plus 2." (+ (line-h) 2))
(defun dbg-band-h () "The title band: the cell plus 9 (24 at 6x15)." (+ (line-h) 9))
(define-symbol-macro +dbg-row+ (dbg-row-h))
(define-symbol-macro +dbg-frame-row+ (line-h))

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

(defun draw-dbg-locals (canvas x0 y0 w h locals &optional lsel)
  "Render LOCALS (FRAME-LOCAL-PLACES plists: :name :value :set :why) as one 'name =
   value' line each, starting at X0,Y0 and stopping before the screen bottom. A local
   that can be SET (it has a :set function) has its name in teal; one that cannot is
   dim all through — that is the honest version of the feature (doc/inspector-history
   §5.4): only what sb-di vouches for looks editable. LSEL, when non-NIL, is the
   selected row (the locals have the focus): it is highlighted and the strip scrolls
   to keep it in view. Each line is clipped to the panel's right edge (W).
   Placeholder when the frame has no debug vars. Returns where the rows went, as
   (:locals-top Y :locals-first I :locals-rows N), for clicks."
  (let ((rows (max 1 (floor (- h 8 y0) (line-h)))) (first 0))
    (if (null locals)
        (lol.canvas:draw-string canvas *bfont* "(no locals — no debug vars here)"
                                x0 y0 *ui-dim*)
        (let* ((y y0)
               (maxc (max 1 (floor (- w x0 8) (lol.canvas:font-cw *bfont*)))))  ; chars that fit
          (setf first (if lsel (max 0 (- lsel (1- rows))) 0))
          (loop for v in (nthcdr first locals) for i from first do
            (when (< y (- h 8))
              (let* ((val     (getf v :value))
                     (unavail (eq val :unavailable))
                     (fixed   (or unavail (null (getf v :set))))   ; shown, but not settable
                     (name    (princ-to-string (getf v :name)))
                     (vstr    (if unavail "#<unavailable>" (princ-to-string val)))
                     (full    (format nil "~a = ~a" name vstr)))
                (when (eql i lsel)
                  (lol.canvas:fill-rect canvas (- x0 6) (- y 1) (- w x0) (line-h) *ui-sel*)
                  (lol.canvas:fill-rect canvas (- x0 6) (- y 1) 2 (line-h) *ui-accent*))
                ;; name in teal, value in text (whole line dim when it cannot be set); a
                ;; line that overruns is clipped with ".." (the … glyph would blit as a stray &).
                (if (> (length full) maxc)
                    (lol.canvas:draw-string canvas *bfont*
                      (concatenate 'string (subseq full 0 (max 1 (- maxc 2))) "..")
                      x0 y (if fixed *ui-dim* *ui-text*))
                    (let ((pen (lol.canvas:draw-string canvas *bfont* name x0 y
                                                       (if fixed *ui-dim* *ui-accent*))))
                      (lol.canvas:draw-string canvas *bfont* (format nil " = ~a" vstr) pen y
                                              (if unavail *ui-dim* *ui-text*))))
                (incf y (line-h)))))))
    (list :locals-top y0 :locals-first first :locals-rows rows)))

(defun dbg-local-at (geometry x y n)
  "The locals-row index at screen X,Y (accounting for the strip's scroll), by the
   GEOMETRY DRAW-DEBUGGER returned, or NIL."
  (declare (ignore x))
  (let ((top (getf geometry :locals-top 0)) (first (getf geometry :locals-first 0))
        (rows (getf geometry :locals-rows 0)))
    (when (>= y top)
      (let ((i (+ first (floor (- y top) (line-h)))))
        (when (and (< i n) (< (- i first) rows)) i)))))

;;; ---- the two-sided spine (§3): the signal rose, a restart unwinds -----------
(defparameter *dbg-red*      !#FF6B6B "the condition itself — the concept :condition colour.")
(defparameter *dbg-title-bg* !#5A1C1C "the alarm-red title band.")
(defparameter *dbg-title-fg* !#FFD0C0 "text on the title band.")
(define-symbol-macro +dbg-locals-h+ (+ 28 (* 7 (line-h))))   ; the locals strip: 132 at 6x15

(defun dbg-fit (s px)
  "Trim S to fit PX pixels of the browser font (…-elided)."
  (let ((maxc (max 1 (floor px (lol.canvas:font-cw *bfont*)))))
    (if (> (length s) maxc) (concatenate 'string (subseq s 0 (max 1 (- maxc 2))) "..") s)))

(defun draw-dbg-rail (canvas x y0 y1 color)
  "A SOLID vertical rail from Y0 to Y1 — a stack side that stands, drawn whole (not dashed)."
  (lol.canvas:fill-rect canvas x y0 2 (max 0 (- y1 y0)) color))

(defun draw-debugger (canvas w h condition restarts backtrace locals rsel fsel focus
                      &optional (lsel 0) note)
  "Paint the debugger as the two-sided spine of §3: the CONDITION is the header; the
   RESTARTS stand in an amber rail down the LEFT (they are the way out — the first
   thing to read, and invoking one is the unwind); the CALL STACK runs down the RIGHT
   with a teal rail hugging its left (the signal rose here and NOTHING unwound — every
   frame still stands) and the innermost frame marked !.
   The selected frame's locals fill a strip along the bottom. FOCUS (:restarts /
   :backtrace / :locals) says which list the arrows drive; LSEL is the selected local
   and NOTE, when non-NIL, the outcome of the last set or restart (shown in the strip's
   header, where the eye already is). Returns its GEOMETRY — where the restart rail,
   the frame column and the locals rows went — for the overlay's clicks."
  (lol.canvas:fill-rect canvas 0 0 w h *ui-bg*)
  (lol.canvas:fill-rect canvas 0 0 w (dbg-band-h) *dbg-title-bg*)
  (lol.canvas:draw-string canvas *bfont* "! DEBUGGER — an unhandled condition"
                          10 (text-dy (dbg-band-h)) *dbg-title-fg*)
  (let ((y (+ (dbg-band-h) 12)))
    ;; --- the header: what was signalled (the condition class is the headline) ---
    (lol.canvas:draw-string canvas *bfont* (format nil "~(~a~)" (type-of condition))
                            12 y *dbg-red* :scale 2)
    (incf y (+ 2 (* 2 (line-h))))       ; the line above was drawn at :scale 2
    ;; the report is often multi-line (draw-string has no newline handling): split it.
    (dolist (line (split-lines (princ-to-string condition)))
      (lol.canvas:draw-string canvas *bfont* (dbg-fit line (- w 24)) 12 y *ui-text*)
      (incf y (1+ (line-h))))
    (incf y 2)
    (lol.canvas:draw-string canvas *bfont*
      "no handler claimed it — caught here with the whole stack still standing."
      12 y *ui-dim*)
    (incf y (+ (line-h) 11))
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
        (lol.canvas:draw-string canvas *bfont* "  ·  Tab walks · R restarts · M-. source" pen top *ui-dim*))
      (incf top (+ (line-h) 5))
      ;; a rule divides the two sides; a dim-amber restart rail (the unwind) faces a
      ;; mirrored SOLID teal signal rail (the stack stands) — §3's two forces, left and right.
      (lol.canvas:fill-rect canvas (- split 8) top 1 (- bottom top) *ui-rule*)
      (draw-dbg-rail canvas rail-x top (min bottom (+ top (* +dbg-row+ (length restarts))))
                     !#8A5A28)
      (draw-dbg-rail canvas split top (min bottom (+ top (* +dbg-frame-row+ (length backtrace))))
                     *ui-accent*)
      ;; --- left: the restart rail (amber — invoking is the unwind) ---
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
        (let ((pen (lol.canvas:draw-string canvas *bfont*
                     (dbg-fit (format nil "LOCALS of frame ~d  ·  ~a" fsel (nth fsel backtrace))
                              (floor (- w 24) 2))
                     12 (+ ly 7) (if (eq focus :locals) *ui-accent* *ui-dim*))))
          ;; the right half of the header: the outcome of the last action, else the keys
          (lol.canvas:draw-string canvas *bfont*
            (dbg-fit (if note
                         (format nil "   ~a" note)
                         "   ·  Tab here · Enter / C-c C-c sets · R restarts the frame")
                     (- w pen 12))
            pen (+ ly 7) (if note *ui-amber* *ui-dim*)))
        (list* :restart-top top :restart-x0 8 :restart-x1 (- split 12)
               :frame-top top :frame-x0 (- frame-x 6) :frame-x1 (- w 8)
               (draw-dbg-locals canvas 12 (+ ly (line-h) 13) w h locals
                                (and (eq focus :locals) lsel)))))))

(defun dbg-restart-at (geometry x y n)
  "The restart-row index at screen X,Y — only within the restart rail (its left
   column), so a click in the stack never invokes a restart. NIL otherwise. GEOMETRY
   is what DRAW-DEBUGGER returned."
  (let ((top (getf geometry :restart-top 0)))
    (when (and (>= y top) (<= (getf geometry :restart-x0 8) x (getf geometry :restart-x1 0)))
      (let ((i (floor (- y top) +dbg-row+)))
        (when (< i n) i)))))

(defun dbg-frame-at (geometry x y n)
  "The backtrace-row index at screen X,Y — only within the frame column (its right
   half), so a click in the restart rail never selects a frame. NIL otherwise."
  (let ((top (getf geometry :frame-top 0)))
    (when (and (>= y top) (<= (getf geometry :frame-x0 0) x (getf geometry :frame-x1 0)))
      (let ((i (floor (- y top) +dbg-frame-row+)))
        (when (< i n) i)))))

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

;;; M-. from the debugger: NAME's definition source in a read-only overlay OVER the
;;; debugger — the erroring stack stays standing, so you look and come back. Nobody
;;; waits for it: it is opened and the debugger's own wait goes on underneath.

(defclass source-overlay (overlay)
  ((name :initarg :name :reader source-name)
   (tv   :initarg :tv :reader source-tv))
  (:default-initargs :opaque t)
  (:documentation "A definition's source, read-only, over the debugger."))

(defun open-frame-source (name)
  "Show NAME's definition source over the debugger. Scroll with up/down, PageUp/Down,
   or the wheel; Esc / q / Enter returns to the debugger. Reuses PRESENT (subj-defn
   NAME), so a recorded definition shows its source and a built-in shows whatever
   PRESENT falls back to (or a plain 'no source' note)."
  (let* ((w (third (lol.morph:morph-bounds *world*))) (h (fourth (lol.morph:morph-bounds *world*)))
         (view (ignore-errors (present (subj-defn name))))
         (text (or (and view (view-text view))
                   (format nil "(no source recorded for ~(~a~) — a built-in or anonymous frame)"
                           name)))
         (tv (lol.textview:make-textview :font *bfont* :focus nil)))
    (lol.textview:tv-set-text tv text)
    (lol.textview:tv-geometry tv 12 (+ (dbg-band-h) 8) (- w 24) (- h (dbg-band-h) 20))
    (open-overlay (make-instance 'source-overlay :name name :tv tv))))

(defmethod overlay-draw ((o source-overlay) canvas w h)
  (lol.canvas:fill-rect canvas 0 0 w h *ui-bg*)
  (lol.canvas:fill-rect canvas 0 0 w (dbg-band-h) *ui-hi*)
  (lol.canvas:draw-string canvas *bfont*
    (format nil "source of ~(~a~)   —   scroll;  Esc / q / Enter  back to the debugger" (source-name o))
    12 (text-dy (dbg-band-h)) *ui-accent*)
  (lol.textview:tv-draw canvas (source-tv o)))

(defmethod overlay-event ((o source-overlay) e)
  (let ((tv (source-tv o)) (key (overlay-key e)))
    (cond
      (key
       (case key
         (:up        (lol.textview:tv-wheel tv -1))
         (:down      (lol.textview:tv-wheel tv 1))
         (:page-up   (lol.textview:tv-wheel tv (- +src-page+)))
         (:page-down (lol.textview:tv-wheel tv +src-page+))
         ((:escape #\q #\Q :return #\Return) (close-overlay o)))
       (overlay-changed o))
      ((eq (lol.morph:event-kind e) :wheel)
       (lol.textview:tv-wheel tv (* (- (lol.morph:event-notches e)) +wheel-lines+))
       (overlay-changed o)))))

(defun dbg-invoke-with-value (restarts i)
  "Prompt for a value and invoke restart I with it — the way to answer a restart that
   takes an argument (USE-VALUE, our USE-HEADING, …). The typed text is READ then EVAL'd,
   so both a literal (=120=) and an expression (=(* 2 60)=) work; a read/eval error just
   cancels (the debugger must not error while reporting one). A non-local exit on success."
  (let* ((r (nth i restarts))
         (line (and r (run-value-prompt (getf r :name)))))
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

(defvar *dbg-state* nil
  "While the framebuffer debugger is up: what it shows, as a plist (:condition :restarts
   :frames :fsel :focus :locals :lsel :note) — names and strings only, refreshed on
   every redraw, NIL once it has gone. For reading over the net REPL (check-debugger.lisp
   drives the debugger with real keys and reads its answers here); nothing in the image
   reads it back. A global rather than a binding, because the REPL is another thread.")

(defun dbg-set-local (frame place)
  "Ask for a new value for the frame local PLACE (a FRAME-LOCAL-PLACES plist) and write
   it into the standing FRAME — the debugger's twin of the inspector's SET-FIELD
   (shell.lisp), and the gesture of doc/inspector-history.org §5.4. The typed text is
   READ and evaluated IN THE FRAME (EVAL-IN-FRAME-CONTEXT), so the frame's other locals
   are in scope: (* x 3) means this frame's X. Returns the note for the strip's header:
   the value READ BACK from the frame, or why nothing was written. Never signals —
   this runs while an error is already being shown."
  (let ((name (getf place :name)))
    (cond
      ((null place) "no local selected")
      ((null (getf place :set))
       (format nil "~a cannot be set: ~a" name (or (getf place :why) "no setter")))
      (t
       (let ((text (run-value-prompt name)))
         (if (or (null text) (zerop (length (string-trim '(#\Space #\Tab) text))))
             nil                                   ; cancelled: clear the note
             (handler-case
                 (let* ((new (let ((*standard-output* (make-broadcast-stream))
                                   (*error-output* (make-broadcast-stream)))
                               (eval-in-frame-context text frame)))
                        (back (funcall (getf place :set) new)))
                   (let ((*print-length* 8) (*print-level* 3))
                     (format nil "~a = ~s  (read back from the frame)" name back)))
               (serious-condition (c)
                 (compact-value (format nil "set failed: ~a" c))))))))))

(defun dbg-restart-frame (frame)
  "R: unwind to FRAME and call its function again with the arguments it holds now
   (model.lisp's RESTART-FRAME) — after setting an argument, the way to see the call
   run again with it. A non-local exit on success; otherwise the note saying why not."
  (let ((why (and frame (frame-restart-refusal frame))))
    (cond ((null frame) "no frame selected")
          (why (format nil "can't restart this frame: ~a" why))
          (t (restart-frame frame)))))

(defun run-debugger-fb (condition)
  "The debugger, with one more restart when the agent is in the image: ASK — explain
   this, and propose a fix (doc/agent.org §8.8, initramfs/agent-ui.lisp). It is a real
   restart, established here around the rail, so it is listed and chosen like the
   condition's own; choosing it shows the agent's answer and comes back here, with the
   stack still standing."
  ;; the log keeps what the screen shows, in case the screen is the last thing seen
  (ignore-errors
    (session-log-block (format nil "debugger: ~s ~a" (type-of condition) (clip-one-line condition 300))
                       (backtrace-string :count 25) :max-lines 25))
  (restart-case (%run-debugger-fb condition)
    (ask-the-agent ()
      :report "ASK - explain this, and propose a fix"
      :test (lambda (c) (declare (ignore c)) (fboundp 'agent-ask-about-condition))
      (ignore-errors (funcall 'agent-ask-about-condition condition))
      (run-debugger-fb condition))))

(defclass debugger-overlay (overlay)
  ((condition :initarg :condition :reader dbg-condition)
   (restarts  :initarg :restarts :reader dbg-restarts)
   (frames    :initarg :frames :reader dbg-frames)       ; sb-di frame objects
   (backtrace :initarg :backtrace :reader dbg-backtrace) ; their labels
   (rsel :initform 0 :accessor dbg-rsel)
   (fsel :initform 0 :accessor dbg-fsel)
   (lsel :initform 0 :accessor dbg-lsel)
   (focus :initform :restarts :accessor dbg-focus)
   (pending-meta :initform nil :accessor dbg-pending-meta)
   (pending-cc :initform nil :accessor dbg-pending-cc)
   (note :initform nil :accessor dbg-note)
   (places :initform nil :accessor dbg-places)
   (geometry :initform '() :accessor dbg-geometry
             :documentation "Where it drew its rows (DRAW-DEBUGGER's value), for clicks."))
  (:default-initargs :opaque t)
  (:documentation "The debugger as an overlay: CONDITION, its restarts, the standing
   stack and the selected frame's locals. Opened by %RUN-DEBUGGER-FB, which WAITS in
   it — the erroring stack must stay standing under it — and is left only by a restart
   (a non-local exit straight through the wait)."))

(defun dbg-refresh (o)
  "Read the selected frame's locals afresh — as PLACES: the value and, for a settable
   local, how to write it — and publish what is shown in *DBG-STATE*. After every
   event: cheap, and only input changes it."
  (setf (dbg-places o) (frame-local-places (nth (dbg-fsel o) (dbg-frames o)))
        (dbg-lsel o) (min (dbg-lsel o) (max 0 (1- (length (dbg-places o))))))
  (setf *dbg-state*
        (list :condition (ignore-errors (princ-to-string (dbg-condition o)))
              :restarts (mapcar (lambda (r) (getf r :name)) (dbg-restarts o))
              :frames (dbg-backtrace o) :fsel (dbg-fsel o) :focus (dbg-focus o)
              :locals (mapcar (lambda (p) (list (getf p :name) (getf p :value)
                                                (and (getf p :set) t)))
                              (dbg-places o))
              :lsel (dbg-lsel o) :note (dbg-note o)))
  (overlay-changed o))

(defmethod overlay-draw ((o debugger-overlay) canvas w h)
  (setf (dbg-geometry o)
        (draw-debugger canvas w h (dbg-condition o) (dbg-restarts o) (dbg-backtrace o) (dbg-places o)
                       (dbg-rsel o) (dbg-fsel o) (dbg-focus o) (dbg-lsel o) (dbg-note o))))

(defun %run-debugger-fb (condition)
  "Show CONDITION, its restarts and backtrace, and let the user pick a restart to
   invoke — and walk the backtrace to inspect any frame's locals. Returns only by
   invoking a restart (a non-local exit out of here). Tab moves focus round the
   restart column (Enter/0-9/click invoke), the backtrace (up-dn/click walk frames)
   and the selected frame's LOCALS, always shown along the bottom: there up-dn/click
   pick one and Enter or C-c C-c sets it in the standing frame (DBG-SET-LOCAL). R
   re-enters the selected frame with the arguments it now holds (DBG-RESTART-FRAME);
   so set, then CONTINUE or R, is how a fix is tried without leaving the error. M-.
   opens the selected frame's definition source over it without unwinding the stack."
  (let* ((restarts (let ((rs (restart-list condition)))   ; ASK last: Enter keeps meaning
                     (flet ((ask-p (r) (eq (getf r :name) 'ask-the-agent)))   ; the condition's own first
                       (append (remove-if #'ask-p rs) (remove-if-not #'ask-p rs)))))
         (frame-objs (backtrace-frame-objects 40 4)))   ; skip our own hook machinery
    (when (null restarts) (return-from %run-debugger-fb))  ; nothing to do; let it pass
    (let ((o (make-instance 'debugger-overlay :condition condition :restarts restarts
                                              :frames frame-objs
                                              :backtrace (mapcar #'frame-label frame-objs))))
      (dbg-refresh o)
      (unwind-protect (run-overlay o)          ; however we leave, *DBG-STATE* goes too
        (setf *dbg-state* nil)))))

(defmethod overlay-event ((o debugger-overlay) e)
  (let* ((restarts (dbg-restarts o)) (frame-objs (dbg-frames o))
         (places (dbg-places o)) (fsel (dbg-fsel o)) (lsel (dbg-lsel o)))
    (multiple-value-bind (key state) (overlay-key e)
      (cond
        (key
         (let ((meta (or (lol.canvas:meta-p state) (dbg-pending-meta o)))
               (ctrl (lol.canvas:ctrl-p state))
               (cc (dbg-pending-cc o)))
           (setf (dbg-pending-meta o) nil (dbg-pending-cc o) nil (dbg-note o) nil)
           (cond
             ;; C-c C-c — set the selected local (as on an inspector field); the first
             ;; C-c only arms the chord
             ((and ctrl (member key '(#\c #\C)))
              (if cc
                  (setf (dbg-focus o) :locals
                        (dbg-note o) (dbg-set-local (nth fsel frame-objs) (nth lsel places)))
                  (setf (dbg-pending-cc o) t)))
             ;; R — restart the selected frame with the arguments it holds now
             ((and (not ctrl) (not meta) (member key '(#\r #\R)))
              (setf (dbg-note o) (dbg-restart-frame (nth fsel frame-objs))))
             ;; M-. — view the selected frame's definition source (stack stays up)
             ((and meta (eql key #\.))
              (let ((name (frame-defn-name (nth fsel frame-objs))))
                (when name (open-frame-source name))))
             ;; Esc = the Meta prefix (sticky); it must NOT quit the debugger
             ((eq key :escape) (setf (dbg-pending-meta o) t))
             ((and ctrl (member key '(#\g #\G))) (dbg-abort restarts))
             ((eq key :tab)                      ; restarts -> stack -> locals -> ...
              (setf (dbg-focus o) (case (dbg-focus o) (:restarts :backtrace) (:backtrace :locals)
                                    (t :restarts))))
             ((eq key :up)
              (case (dbg-focus o)
                (:backtrace (setf (dbg-fsel o) (max 0 (1- fsel)) (dbg-lsel o) 0))
                (:locals    (setf (dbg-lsel o) (max 0 (1- lsel))))
                (t          (setf (dbg-rsel o) (max 0 (1- (dbg-rsel o)))))))
             ((eq key :down)
              (case (dbg-focus o)
                (:backtrace (setf (dbg-fsel o) (min (max 0 (1- (length frame-objs))) (1+ fsel))
                                  (dbg-lsel o) 0))
                (:locals    (setf (dbg-lsel o) (min (max 0 (1- (length places))) (1+ lsel))))
                (t          (setf (dbg-rsel o) (min (1- (length restarts)) (1+ (dbg-rsel o)))))))
             ;; Enter on a local sets it; elsewhere it still invokes the restart
             ((and (eq (dbg-focus o) :locals) (member key '(:return #\Return)))
              (setf (dbg-note o) (dbg-set-local (nth fsel frame-objs) (nth lsel places))))
             ;; type a restart's number to select it (0-9) — the count is small
             ((and (characterp key) (char<= #\0 key #\9))
              (let ((i (- (char-code key) (char-code #\0))))
                (when (< i (length restarts)) (setf (dbg-rsel o) i))))
             ;; v — force a value even when Enter wouldn't ask (override an :interactive
             ;; default, or supply args to any restart)
             ((and (characterp key) (char-equal key #\v)) (dbg-invoke-with-value restarts (dbg-rsel o)))
             ((member key '(:return #\Return)) (dbg-enter restarts (dbg-rsel o)))))
         (dbg-refresh o))
        ((eq (lol.morph:event-kind e) :down)
         (let* ((x (lol.morph:event-x e)) (y (lol.morph:event-y e))
                (g (dbg-geometry o))
                (ri (dbg-restart-at g x y (length restarts)))
                (fi (dbg-frame-at g x y (length frame-objs)))
                (li (dbg-local-at g x y (length places))))
           (cond (ri (dbg-enter restarts ri))  ; a restart click invokes (asks if needed)
                 (fi (setf (dbg-focus o) :backtrace (dbg-fsel o) fi (dbg-lsel o) 0))  ; a frame click selects it
                 (li (setf (dbg-focus o) :locals (dbg-lsel o) li)))  ; a local click selects it
           (dbg-refresh o)))))))

;;; ---- the ASK restart's view: the agent's answer, over the debugger -------------
;;; (doc/agent.org §8.8.) Opened by AGENT-ASK-ABOUT-CONDITION (agent-ui.lisp, through
;;; RUN-AGENT-DEBUGGER-VIEW), which waits in it; Esc goes back to the restarts. It fills
;;; in as the answer streams: it STEPS while the turn runs (a frame period), so the wait
;;; wakes for it without a key.

(defclass agent-view-overlay (overlay)
  ((conv   :initarg :conv :reader agent-view-conv)
   (tv     :initarg :tv :reader agent-view-tv)
   (seen   :initform -1 :accessor agent-view-seen)
   (busy   :initform nil :accessor agent-view-busy)
   (prefix :initform nil :accessor agent-view-prefix)
   (note   :initform nil :accessor agent-view-note))
  (:default-initargs :opaque t))

(defun agent-view-band () (+ (line-h) 9))   ; 24 px under the old 6x15 font

(defun run-agent-view (conv)
  "A modal view of CONV over the debugger: it fills in as the answer streams, arrows
   and PageUp/Down move the caret, C-c C-c accepts the form before it, C-g stops the
   turn, Esc returns to the rail."
  (let* ((w (third (lol.morph:morph-bounds *world*))) (h (fourth (lol.morph:morph-bounds *world*)))
         (band (agent-view-band))
         (tv (lol.textview:make-textview :font *bfont* :focus t :tint #'conversation-tint))
         (o (make-instance 'agent-view-overlay :conv conv :tv tv)))
    (lol.textview:tv-geometry tv 12 (+ band 8) (- w 24) (- h band 8 (line-h) 9))
    (agent-view-sync o)
    (run-overlay o)))

(defun agent-view-sync (o)
  "Bring the view up to the conversation's latest revision (and its busy flag)."
  (let* ((conv (agent-view-conv o)) (rev (conversation-revision conv))
         (busy (conversation-busy-p conv)))
    (when (or (/= rev (agent-view-seen o)) (not (eq busy (agent-view-busy o))))
      (setf (agent-view-seen o) rev (agent-view-busy o) busy)
      (let ((lines (coerce (conversation-lines conv) 'simple-vector)))
        (lol.textview:tv-replace (agent-view-tv o) lines (1- (length lines)) 0))
      (overlay-changed o))))

(defmethod lol.morph:morph-step-ms ((o agent-view-overlay))
  ;; a frame period while the answer streams; idle, still catch the last touch
  (if (conversation-busy-p (agent-view-conv o)) +agent-frame-ms+ 500))

(defmethod lol.morph:step-morph ((o agent-view-overlay) now)
  (declare (ignore now))
  (agent-view-sync o))

(defmethod overlay-draw ((o agent-view-overlay) canvas w h)
  (let ((band (agent-view-band)) (note (agent-view-note o)))
    (lol.canvas:fill-rect canvas 0 0 w h *ui-bg*)
    (lol.canvas:fill-rect canvas 0 0 w band *ui-hi*)
    (lol.canvas:draw-string canvas *bfont*
      (format nil "ASK — ~a   ·   C-c C-c accept the form before the caret · C-g stop · Esc back to the restarts"
              (if (agent-view-busy o) "answering..." "done"))
      12 (text-dy band) *ui-accent*)
    (lol.textview:tv-draw canvas (agent-view-tv o))
    (when note (lol.canvas:draw-string canvas *bfont* note 12 (- h (line-h) 5) *ui-amber*))))

(defmethod overlay-event ((o agent-view-overlay) e)
  (multiple-value-bind (key state) (overlay-key e)
    (when key
      (let ((ctrl (or (lol.canvas:ctrl-p state) *ctrl-down*))
            (conv (agent-view-conv o)) (tv (agent-view-tv o)))
        (cond
          ((eq key :escape) (close-overlay o))
          ((and ctrl (member key '(#\g #\G)))
           (cancel-conversation conv) (setf (agent-view-note o) "stopping..."))
          ((and (agent-view-prefix o) ctrl (member key '(#\c #\C)))
           (setf (agent-view-prefix o) nil)
           (let ((text (form-text-before-point tv)))
             (setf (agent-view-note o)
                   (if (null text) "no form before the caret"
                       (handler-case (format nil "Accepted ~(~a~) — Esc, then a restart to retry"
                                             (let ((*read-eval* nil)) (accept-source text)))
                         (serious-condition (c) (format nil "Accept failed: ~a" c)))))))
          ((and ctrl (member key '(#\c #\C))) (setf (agent-view-prefix o) t))
          ((member key '(:up :down :left :right :page-up :page-down :home :end))
           (setf (agent-view-prefix o) nil)
           (lol.textview:tv-key tv key 0))
          (t (setf (agent-view-prefix o) nil)))
        (overlay-changed o)))))

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
     ("C-wheel"            "bigger / smaller text, everywhere")
     ("Esc"                "Meta prefix: Esc . = M-.")
     ("q"                  "back, like Backspace (never leaves)"))
    ("Places  (Super)"
     ("Super-Space"        "the Spotter (C-n / C-p move)")
     ("Super-e"            "a Workspace (S-e: a new one)")
     ("Super-m"            "the module index: every definition")
     ("Super-l"            "the CL-USER package, flat")
     ("Super-h"            "the clipboard history")
     ("Super-f"            "the file manager (home)")
     ("Super-b"            "a web page (Chrome, doc/web-browser)")
     ("Super-n"            "the network: links, DNS, Wi-Fi")
     ("Super-Enter"        "a terminal (a shell)")
     ("Super-?"            "this cheatsheet")
     ("Super-q"            "leave: console or shut down"))
    ("Tabs  (Super)"
     ("Super-1..9"         "jump to tab N")
     ("Super-S-1..9"       "move this tab to slot N")
     ("Super-Tab"          "cycle tabs (Shift: backwards)")
     ("Super-S-Enter/click" "open the selection in a NEW tab")
     ("Super-w"            "close the current tab")
     ("Super-o"            "focus list <-> side pane")
     ("Super-Left"         "back in the trail"))
    ("Windows  (doc/morphic.org)"
     ("Super-g"            "tear this tab off into a window;")
     (""                   "  on a window: grab it (keys below)")
     ("  arrows / S-arrows" "move it a cell / eight cells")
     ("  C-arrows"         "resize it")
     ("  t / Super-g"      "re-tile: back into the tab strip")
     ("  Enter / Esc"      "done grabbing")
     ("drag a tab down"    "tear it off, carried by the pointer")
     ("drag the title bar" "move;  drop it on the tabs: re-tile")
     ("drag the corner"    "resize")
     ("click x / Super-w"  "close the window's trail")
     ("Super-Tab / 1..9"   "reach windows too (raises them)")
     ("halo, Up to window" "Move Resize Raise Close Re-tile")
     ("  Move / Resize"    "the pointer carries it; click: down")
     ("  Inspect, bounds"  "C-c C-c a (x y w h): it moves"))
    ("Drag and drop  (doc/morphic.org)"
     ("drag a row / sprite" "pick up what it shows; let go: drop")
     ("  on editable text" "insert a reference: 'f #'g \"path\"")
     ("  on a field"       "set it (if settable) to the value")
     ("  on the tabs"      "open it as a new tab")
     ("  on a pane / title" "drill it there (a window's too)")
     ("Super-p"            "pick up the selection / the name")
     ("Super-d / Super-S-d" "drop at the focus / as a new tab")
     ("Esc"                "put back what the hand carries"))
    ("Text size  (doc/fonts.org)"
     ("Super-= / Super-+"  "bigger text, everywhere")
     ("Super--"            "smaller text")
     ("Super-0"            "back to the default size")
     ("C-x C-+ / C-- / C-0" "the same, Emacs-style;")
     ("  then + - 0"       "  ... repeat without C-x"))
    ("Sound  (doc/screencast.org)"
     ("Vol+ / Vol- keys"   "volume up / down 5% (held: repeats)")
     ("Mute key"           "mute / unmute (the tray says VOL)")
     ("Mic-mute key"       "microphone off / on"))
    ("Keyboard  (doc/input.org)"
     ("Right Alt (held)"   "type pt-BR: ; = ç, [ = ´, ' = ~")
     ("Caps Lock"          "switch layout US <-> pt-BR (tray: BR)")
     ("  ´ ~ ^ then a"     "á ã â — dead keys, as in X"))
    ("Recording  (doc/screencast.org)"
     ("Super-r"            "record the screen + the mic;")
     (""                   "  again: pause / resume")
     ("Super-S-r"          "stop, save, show the result")
     ("Spotter \"record\""  "the recorder: level, recordings")
     ("  p / w on one"     "play it full screen / in a window")
     ("  playing"          "SPC pause, arrows seek, q back"))
    ("Workspace & source"
     ("Tab"                "complete the symbol (else indent)")
     ("C-< / C->  M-< M->" "start / end of the buffer")
     ("C-c C-c"            "Accept / define the form")
     ("C-x C-e"            "Do it: the form before the caret,")
     (""                   "  or the selection; value to status")
     ("C-c C-e"            "Do it: the top-level form around")
     (""                   "  the caret (code in a conversation)")
     ("C-;"                "comment / uncomment line or region")
     ("C-c C-p"            "Print it: result woven inline")
     ("C-c C-i"            "Inspect it: open the value")
     ("C-c C-d"            "Debug it: debugger armed"))
    ("Web page"
     ("C-l / Super-Left,Rt" "address / back, forward")
     ("in the address"     "C-y / C-v paste, C-u or the ✕ clear")
     ("C-r  C-f  C-+ C--"  "reload, find, zoom")
     ("C-c / C-v"          "copy / paste via OUR clipboard"))
    ("The agent  (doc/agent.org)"
     ("C-c C-a / C-c ?"    "Ask it: selection, form or subject")
     ("Super-a"            "the conversation (its tab)")
     ("C-c C-a in it"      "send the question typed at the end")
     ("C-c C-o"            "open what a ;; #N tool call read")
     ("C-g"                "stop the turn that is answering"))
    ("Inspector"
     ("C-c C-c"            "set the selected field")
     ("C-c C-e"            "evaluate in the object (SELF)")
     ("C-c C-a"            "apply this function (else: Ask)"))
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
    ("Search  (doc/clipboard.org)"
     ("C-s / C-r"          "search forward / back as you type")
     ("  C-s / C-r again"  "the next match; then wrap round")
     ("  C-s C-s"          "the last string again")
     ("  DEL / C-g"        "back a step / cancel (C-g: drop")
     (""                   "  the part that fails first)")
     ("  C-w / C-y"        "add the next word / the clipboard")
     ("  RET / any key"    "stop at the match (the key acts)")
     ("C-x C-x"            "back to where the search began"))
    ("Org files  (doc/org.org)"
     ("TAB on a headline"  "fold: folded / children / all")
     ("S-TAB"              "overview / contents / show all")
     ("C-c C-n / C-c C-p"  "next / previous heading")
     ("C-c C-f / C-c C-b"  "next / previous same level")
     ("C-c C-u"            "up to the parent heading")
     ("C-c C-j"            "go to a heading by name")
     ("M-RET"              "new heading (or list item)")
     ("M-Up / M-Down"      "move the subtree")
     ("M-Left / M-Right"   "promote / demote (M-S-: subtree)")
     ("C-c C-t"            "TODO -> DONE -> none")
     ("C-c C-o / C-c C-l"  "follow / insert a link"))
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
     ("^  ~  i  y  Y"      "up, home, info, path; Y for a shell")
     ("j  g"               "follow a link / refresh")
     ("Esc (during f)"     "stop a running find")
     ("C-x C-s"            "save a file you are editing")
     ("p  w (a video)"     "play full screen / in a window")
     ("Spotter ~/ or /"    "type a path; Tab completes"))
    ("Terminal"
     ("any key"            "to the program (not Super)")
     ("S-PgUp / wheel"     "scroll back the history")
     ("drag / S-drag"      "select + copy (S- if it has the mouse)")
     ("S-Insert / middle"  "paste clipboard / selection")
     ("ret / r  (ended)"   "close / run again"))
    ("Debugger"
     ("0-9 / up-down"      "select a restart")
     ("Tab"                "restarts -> frames -> the frame's locals")
     ("Enter / C-c C-c"    "on a local: set it in the frame")
     ("R"                  "restart the frame (args as they are now)")
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

(defun cheatsheet-col-w ()
  "One cheatsheet column: a 21-character key, about 28 of description, and air — 334 px
   at 6x15 (three columns at 1280), 584 at 18 px (two). A longer description is
   clipped with '..' rather than costing a column."
  (+ 34 (* 50 (char-w))))

(defun cheatsheet-columns (w h)
  "*cheatsheet*'s groups poured into columns for a screen W x H -> a list of columns,
   each a list of groups. A group is never split. When everything fits on one screen the
   columns are BALANCED (each holds about the same number of lines); when it does not —
   a large font on a small screen — each column is filled to the bottom and the overflow
   goes onto further PAGES (see DRAW-CHEATSHEET)."
  (let* ((lines (+ (* 2 (length *cheatsheet*))           ; title + gap per group
                   (loop for g in *cheatsheet* sum (length (cdr g)))))
         (fit (max 4 (floor (- h (* 2 (line-h)) 30) (line-h))))  ; lines one column holds
         (ncols (max 1 (floor (- w 40) (cheatsheet-col-w))))
         (per-col (min fit (max 4 (ceiling lines ncols))))
         (cols '()) (col '()) (used 0))
    (dolist (g *cheatsheet*)
      (let ((need (+ 2 (length (cdr g)))))
        (when (and col (> (+ used need) per-col))
          (push (nreverse col) cols) (setf col '() used 0))
        (push g col) (incf used need)))
    (when col (push (nreverse col) cols))
    (nreverse cols)))

(defun draw-cheatsheet (canvas w h &optional (page 0))
  "Render *cheatsheet* as a full-screen overlay: as many columns as the width allows
   (three at 1280 with the 6x15 font), the groups poured into them in order. Group
   titles in amber, the keys in accent, the descriptions in text. Every distance is a
   multiple of the current font's cell, so it scales with SuperL-= (doc/fonts.org);
   what does not fit goes onto further pages. Returns the number of pages."
  (let* ((cols (cheatsheet-columns w h))
         (ncols (max 1 (floor (- w 40) (cheatsheet-col-w))))
         (pages (max 1 (ceiling (length cols) ncols)))
         (page (mod page pages))
         (band (+ (line-h) 7))
         (top (+ band 12))
         (colw (floor (- w 40) ncols))
         (keyw (+ 12 (* 21 (char-w))))                       ; 138 at 6x15
         (maxc (max 8 (floor (- colw keyw 24) (char-w)))))
    (lol.canvas:fill-rect canvas 0 0 w h *ui-bg*)
    (lol.canvas:fill-rect canvas 0 0 w band *ui-hi*)
    (lol.canvas:draw-string canvas *bfont*
      (format nil "Keys  —  ~:[press any key or click to dismiss~;~:*page ~d of ~d: ~
                   PageDown / Space next, PageUp back, any other key dismisses~]"
              (and (> pages 1) (1+ page)) pages)
      12 (text-dy band) *ui-accent*)
    (loop for col in (subseq cols (* page ncols) (min (length cols) (* (1+ page) ncols)))
          for c from 0
          do (let ((x (+ 20 (* c colw))) (y top))
               (dolist (g col)
                 (lol.canvas:draw-string canvas *bfont* (car g) x y *ui-amber*)
                 (incf y (+ (line-h) 3))
                 (dolist (row (cdr g))
                   (lol.canvas:draw-string canvas *bfont* (first row) (+ x 10) y *ui-accent*)
                   (lol.canvas:draw-string canvas *bfont* (clip-text (second row) maxc)
                                           (+ x 10 keyw) y *ui-text*)
                   (incf y (line-h)))
                 (incf y (- (line-h) 6)))))
    pages))

(defclass cheatsheet-overlay (overlay)
  ((page  :initform 0 :accessor sheet-page)
   (pages :initform 1 :accessor sheet-pages))
  (:default-initargs :opaque t)
  (:documentation "The SuperL-? cheatsheet, over everything until a key dismisses it."))

(defun open-cheatsheet ()
  "Put the cheatsheet up over the screen and return. Any key or click dismisses it —
   or, when it runs to several pages, PageDown / Space / Right turn the page and
   PageUp / Left turn it back."
  (when *world*                     ; the screen is the world's, on either backend
    (open-overlay (make-instance 'cheatsheet-overlay))))

(defmethod overlay-draw ((o cheatsheet-overlay) canvas w h)
  (setf (sheet-pages o) (draw-cheatsheet canvas w h (sheet-page o))))

(defmethod overlay-event ((o cheatsheet-overlay) e)
  (let ((key (overlay-key e)) (pages (sheet-pages o)))
    (cond
      (key
       (cond ((and (> pages 1) (member key '(:page-down :right #\Space)))
              (incf (sheet-page o)) (overlay-changed o))
             ((and (> pages 1) (member key '(:page-up :left)))
              (decf (sheet-page o)) (overlay-changed o))
             (t (close-overlay o))))                    ; any other key dismisses
      ((eq (lol.morph:event-kind e) :down) (close-overlay o)))))   ; a click dismisses

;;; The menus and halos (RUN-CHOOSER), the clipboard manager (RUN-CLIPBOARD-MANAGER),
;;; the Spotter (OPEN-SPOTTER) and the value prompt (RUN-VALUE-PROMPT) are overlays
;;; that need nothing of this backend: poc/overlays.lisp. They used to be loops here
;;; (RUN-CHOOSER-FB, RUN-CLIPBOARD-FB, RUN-SPOTTER-FB, DBG-PROMPT-VALUE).

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
