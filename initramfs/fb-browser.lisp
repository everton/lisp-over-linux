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
(defconstant +wheel-lines+ 3 "Lines the current pane scrolls per mouse-wheel notch.")

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

(defun read-mouse (fd xres yres)
  "Drain pending 24-byte input_event records; move *cursor-x/y*. Returns
   (values CLICKED WHEEL): CLICKED is T on a left-button press; WHEEL is the summed
   scroll notches this drain (REL_WHEEL, + = up / away from the user, - = down), 0
   when there was none. Callers that want only the click keep working on the first
   value. Handles EV_ABS (usb-tablet, 0..32767) and EV_REL (PS/2 + wheel)."
  (let ((clicked nil) (wheel 0))
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
                 (1 (setf *cursor-y* (max 0 (min (1- yres) (+ *cursor-y* val)))))
                 (8 (incf wheel val))))          ; REL_WHEEL: +1 up / -1 down
              ((and (= type 1) (= code #x110) (= val 1))    ; EV_KEY BTN_LEFT press
               (setf clicked t)))))))
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
                       (multiple-value-bind (kb ms kev) (fbb-wait 0 mouse kbd)
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
                               (browser-click *cursor-x* *cursor-y* *ctrl-down* *super-down*))))))))
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
(defvar *dbg-frame-x0* 8 "left edge of the clickable backtrace column.")
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
                              x0 y0 *sh-dim*)
      (let ((y y0)
            (maxc (max 1 (floor (- w x0 8) (lol.canvas:font-cw *bfont*)))))  ; chars that fit
        (dolist (v locals)
          (when (< y (- h 8))
            (let* ((val     (getf v :value))
                   (unavail (eq val :unavailable))
                   (color   (if unavail *sh-dim* *sh-text*))
                   (text    (format nil "~a = ~a" (getf v :name)
                                    (if unavail "#<unavailable>" val))))
              (dolist (piece (split-lines text))
                (when (< y (- h 8))
                  (let ((piece (if (> (length piece) maxc)
                                   (concatenate 'string (subseq piece 0 (1- maxc)) "…")
                                   piece)))
                    (lol.canvas:draw-string canvas *bfont* piece x0 y color))
                  (incf y 15)))))))))

(defun draw-debugger (canvas w h condition restarts backtrace locals rsel fsel focus)
  "Paint the debugger: the condition, the restart column (RSEL selected), and — below
   — a two-column split of the backtrace spine (FSEL selected) beside the locals of
   that frame. FOCUS (:restarts or :backtrace) says which list the arrows drive; it
   only changes which column reads as active."
  (lol.canvas:fill-rect canvas 0 0 w h *sh-bg*)
  (lol.canvas:fill-rect canvas 0 0 w 20 (lol.canvas:rgb #x5a #x1c #x1c))   ; red title
  (lol.canvas:draw-string canvas *bfont* "DEBUGGER — an unhandled condition"
                          10 3 (lol.canvas:rgb #xff #xd0 #xc0))
  (let ((y 30))
    ;; The condition's report is often multi-line (e.g. DIVISION-BY-ZERO prints the
    ;; operation on a second line) — draw-string has no newline handling, so split
    ;; and draw each line, or an embedded #\Newline blits as a stray glyph.
    (dolist (line (split-lines (princ-to-string condition)))
      (lol.canvas:draw-string canvas *bfont* line 12 y *sh-text*)
      (incf y 17))
    (incf y 3)
    (lol.canvas:draw-string canvas *bfont* (format nil "type: ~(~a~)" (type-of condition))
                            12 y *sh-dim*)
    (incf y 30)
    ;; --- the restart column (the primary action; Enter/click invokes) ---
    (lol.canvas:draw-string canvas *bfont*
      "restarts  —  0-9 / up-dn select, Enter or click invoke, C-g abort:"
      12 y (if (eq focus :restarts) *sh-accent* *sh-dim*))
    (incf y 22)
    (setf *dbg-restart-top* y)
    (loop for r in restarts for i from 0 do
      (when (= i rsel)
        (lol.canvas:fill-rect canvas 8 (- y 2) (- w 16) +dbg-row+ *sh-sel*)
        (lol.canvas:fill-rect canvas 8 (- y 2) 2 +dbg-row+ *sh-accent*))
      (lol.canvas:draw-string canvas *bfont*
        (format nil "~d  ~@[[~a]  ~]~a" i (getf r :name) (getf r :report))
        16 y (if (= i rsel) *sh-text* *sh-dim*))
      (incf y +dbg-row+))
    (incf y 16)
    ;; --- the lower split: backtrace (left) | locals of the selected frame (right) ---
    (let ((split (floor w 2)))
      (lol.canvas:draw-string canvas *bfont*
        "backtrace  —  Tab focuses, up-dn walks frames:"
        12 y (if (eq focus :backtrace) *sh-accent* *sh-dim*))
      (lol.canvas:draw-string canvas *bfont* (format nil "locals of frame ~d:" fsel)
                              (+ split 8) y *sh-accent*)
      (incf y 20)
      (setf *dbg-frame-top* y *dbg-frame-x0* 8 *dbg-frame-x1* (- split 8))
      ;; the frame column — selectable; the selected row is brighter when it has focus
      (let ((fy y))
        (loop for fr in backtrace for i from 0
              while (< fy (- h 8)) do
          (when (= i fsel)
            (lol.canvas:fill-rect canvas 8 (- fy 2) (- split 16) +dbg-frame-row+
                                  (if (eq focus :backtrace) *sh-sel* *sh-hi*))
            (when (eq focus :backtrace)
              (lol.canvas:fill-rect canvas 8 (- fy 2) 2 +dbg-frame-row+ *sh-accent*)))
          (lol.canvas:draw-string canvas *bfont* (format nil "~2d  ~a" i fr)
                                  16 fy (if (= i fsel) *sh-text* *sh-dim*))
          (incf fy +dbg-frame-row+)))
      ;; the locals of the selected frame, in the right column
      (draw-dbg-locals canvas (+ split 8) y w h locals))))

(defun dbg-restart-at (y n)
  "The restart-row index at screen-Y, or NIL. N is how many restarts there are."
  (when (>= y *dbg-restart-top*)
    (let ((i (floor (- y *dbg-restart-top*) +dbg-row+)))
      (when (< i n) i))))

(defun dbg-frame-at (x y n)
  "The backtrace-row index at screen X,Y — only within the frame column (its left
   half), so a click in the locals column never selects a frame. NIL otherwise."
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
      (lol.canvas:fill-rect canvas 0 0 w h *sh-bg*)
      (lol.canvas:fill-rect canvas 0 0 w 24 *sh-hi*)
      (lol.canvas:draw-string canvas *bfont*
        (format nil "source of ~(~a~)   —   scroll;  Esc / q / Enter  back to the debugger" name)
        12 5 *sh-accent*)
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
              ((member key '(:return #\Return)) (dbg-invoke restarts rsel))))))
        (when (and *fb-mouse* ms (read-mouse *fb-mouse* w h))
          (let ((ri (dbg-restart-at *cursor-y* (length restarts)))
                (fi (dbg-frame-at *cursor-x* *cursor-y* (length frame-objs))))
            (cond (ri (dbg-invoke restarts ri))          ; a restart click invokes
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
     ("Backspace / C-g"    "back  (C-g at the root leaves)")
     ("PageUp / PageDown"  "page the list / source")
     ("mouse wheel"        "scroll the pane")
     ("q"                  "quit  (list & reference panes)"))
    ("Spatial  (Super)"
     ("Super-?"            "this cheatsheet")
     ("Super-Left"         "back in the trail")
     ("Super-q"            "quit the browser"))
    ("Edit  (source / workspace)"
     ("Tab"                "complete the symbol  (else indent)")
     ("C-c C-c"            "Accept — compile into the image")
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
     ("C-g"                "abort")))
  "The full key map, grouped, for the SuperL-? overlay. Data only — DRAW-CHEATSHEET
   lays it out in two columns.")

(defun draw-cheatsheet (canvas w h)
  "Render *cheatsheet* as a full-screen two-column overlay. Group titles in amber,
   the keys in accent, the descriptions in text."
  (lol.canvas:fill-rect canvas 0 0 w h *sh-bg*)
  (lol.canvas:fill-rect canvas 0 0 w 22 *sh-hi*)
  (lol.canvas:draw-string canvas *bfont*
    "Keys  —  press any key or click to dismiss" 12 4 *sh-accent*)
  (let ((col-x (vector 40 (+ (floor w 2) 20)))
        (col-y (vector 44 44))
        (half  (ceiling (length *cheatsheet*) 2)))
    (loop for g in *cheatsheet* for i from 0
          for c = (if (< i half) 0 1)
          do (let ((x (aref col-x c)) (y (aref col-y c)))
               (lol.canvas:draw-string canvas *bfont* (car g) x y *sh-amber*)
               (incf y 20)
               (dolist (row (cdr g))
                 (lol.canvas:draw-string canvas *bfont* (first row)  (+ x 12)  y *sh-accent*)
                 (lol.canvas:draw-string canvas *bfont* (second row) (+ x 168) y *sh-text*)
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
