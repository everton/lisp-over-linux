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
        (open-environment (or root (find-package :cl-user)))
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
                   (name    (princ-to-string (getf v :name)))
                   (vstr    (if unavail "#<unavailable>" (princ-to-string val)))
                   (full    (format nil "~a = ~a" name vstr)))
              ;; name in teal, value in text (whole line dim when unavailable); a line that
              ;; overruns is clipped with ".." (the … glyph would blit as a stray &).
              (if (> (length full) maxc)
                  (lol.canvas:draw-string canvas *bfont*
                    (concatenate 'string (subseq full 0 (max 1 (- maxc 2))) "..")
                    x0 y (if unavail *sh-dim* *sh-text*))
                  (let ((pen (lol.canvas:draw-string canvas *bfont* name x0 y
                                                     (if unavail *sh-dim* *sh-accent*))))
                    (lol.canvas:draw-string canvas *bfont* (format nil " = ~a" vstr) pen y
                                            (if unavail *sh-dim* *sh-text*))))
              (incf y 15)))))))

;;; ---- the two-sided spine (§3): the signal rose, a restart unwinds -----------
(defparameter *dbg-red*      !#FF6B6B "the condition itself — the concept :condition colour.")
(defparameter *dbg-title-bg* !#5A1C1C "the alarm-red title band.")
(defparameter *dbg-title-fg* !#FFD0C0 "text on the title band.")
(defparameter +dbg-locals-h+ 132 "height of the bottom locals strip.")
(defvar *dbg-restart-x0* 0 "left edge of the clickable restart rail (set at draw).")
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
   CALL STACK runs down the centre with a teal rail hugging its left (the signal rose
   here and NOTHING unwound — every frame still stands) and the innermost frame marked
   !; the RESTARTS stand in an amber rail on the right (invoking one is the unwind).
   The selected frame's locals fill a strip along the bottom. FOCUS (:restarts /
   :backtrace) says which rail the arrows drive."
  (lol.canvas:fill-rect canvas 0 0 w h *sh-bg*)
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
      (lol.canvas:draw-string canvas *bfont* (dbg-fit line (- w 24)) 12 y *sh-text*)
      (incf y 16))
    (incf y 2)
    (lol.canvas:draw-string canvas *bfont*
      "no handler claimed it — caught here with the whole stack still standing."
      12 y *sh-dim*)
    (incf y 26)
    ;; --- the triptych ---
    (let* ((split   (floor (* w 52) 100))
           (rail-x  28)
           (frame-x 46)
           (top     y)
           (bottom  (- h +dbg-locals-h+ 6)))
      ;; column headers — the section title bright in its side colour, the key-hint dim
      (let ((pen (lol.canvas:draw-string canvas *bfont* "CALL STACK" frame-x top
                                         (if (eq focus :backtrace) *sh-accent* *sh-dim*))))
        (lol.canvas:draw-string canvas *bfont* "  ·  Tab walks · M-. source" pen top *sh-dim*))
      (let ((pen (lol.canvas:draw-string canvas *bfont* "RESTARTS" (+ split 8) top
                                         (if (eq focus :restarts) *sh-amber* *sh-dim*))))
        (lol.canvas:draw-string canvas *bfont* "  ·  0-9 · Enter (asks for a value) · unwinds"
                                pen top *sh-dim*))
      (incf top 20)
      ;; a rule divides the two sides; a SOLID teal signal rail (the stack stands) faces a
      ;; mirrored dim-amber restart rail (the unwind) — §3's two forces, left and right.
      (lol.canvas:fill-rect canvas (- split 8) top 1 (- bottom top) *sh-rule*)
      (draw-dbg-rail canvas rail-x top (min bottom (+ top (* +dbg-frame-row+ (length backtrace))))
                     *sh-accent*)
      (draw-dbg-rail canvas split top (min bottom (+ top (* +dbg-row+ (length restarts))))
                     !#8A5A28)
      ;; --- centre: the call stack; frame 0 (innermost) is where it was signalled ---
      (setf *dbg-frame-top* top *dbg-frame-x0* (- frame-x 6) *dbg-frame-x1* (- split 12))
      (let ((fy top))
        (loop for fr in backtrace for i from 0 while (< fy bottom) do
          (when (= i fsel)
            (lol.canvas:fill-rect canvas (- frame-x 6) (- fy 2) (- split frame-x 6) +dbg-frame-row+
                                  (if (eq focus :backtrace) *sh-sel* *sh-hi*))
            (when (eq focus :backtrace)
              (lol.canvas:fill-rect canvas (- frame-x 6) (- fy 2) 2 +dbg-frame-row+ *sh-accent*)))
          (when (zerop i)                                    ; the signal frame
            (lol.canvas:draw-string canvas *bfont* "!" (- frame-x 16) fy *sh-amber*))
          (lol.canvas:draw-string canvas *bfont*
            (dbg-fit (format nil "~2d ~a" i fr) (- split frame-x 12))
            frame-x fy (if (= i fsel) *sh-text* *sh-dim*))
          (incf fy +dbg-frame-row+)))
      ;; --- right: the restart rail (amber — invoking is the unwind) ---
      (setf *dbg-restart-top* top *dbg-restart-x0* split *dbg-restart-x1* (- w 8))
      (let ((ry top))
        (loop for r in restarts for i from 0 while (< ry bottom) do
          (when (= i rsel)
            (lol.canvas:fill-rect canvas split (- ry 2) (- w split 8) +dbg-row+ *sh-sel*)
            (lol.canvas:fill-rect canvas split (- ry 2) 2 +dbg-row+ *sh-amber*))
          (lol.canvas:draw-string canvas *bfont*
            (dbg-fit (format nil "~d  ~@[[~(~a~)]  ~]~a" i (getf r :name) (getf r :report))
                     (- w split 16))
            (+ split 10) ry (if (= i rsel) *sh-text* *sh-dim*))
          (incf ry +dbg-row+)))
      ;; --- bottom strip: the locals of the selected frame ---
      (let ((ly (- h +dbg-locals-h+)))
        (lol.canvas:fill-rect canvas 0 ly w 1 *sh-rule*)
        (lol.canvas:draw-string canvas *bfont*
          (dbg-fit (format nil "locals of frame ~d  ·  ~a" fsel (nth fsel backtrace)) (- w 24))
          12 (+ ly 7) *sh-accent*)
        (draw-dbg-locals canvas 12 (+ ly 28) w h locals)))))

(defun dbg-restart-at (x y n)
  "The restart-row index at screen X,Y — only within the restart rail (its right
   column), so a click in the stack never invokes a restart. NIL otherwise."
  (when (and (>= y *dbg-restart-top*) (<= *dbg-restart-x0* x *dbg-restart-x1*))
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

(defun dbg-prompt-value (name)
  "A modal input line over the debugger for a restart that needs a value: read the Lisp
   expression the user types (Enter accepts, Esc cancels). Returns the typed STRING, or
   NIL if cancelled. Draws its bar over the current debugger frame (already on canvas)."
  (let ((buf "") (canvas *fb-canvas*) (w *fb-xres*) (h *fb-yres*))
    (loop
      (lol.canvas:fill-rect canvas 0 (- h 44) w 44 *sh-hi*)
      (lol.canvas:fill-rect canvas 0 (- h 44) w 2 *sh-amber*)
      (lol.canvas:draw-string canvas *bfont*
        (format nil "value for [~(~a~)] — an expression, Enter invokes, Esc cancels:" name)
        12 (- h 38) *sh-amber*)
      (lol.canvas:draw-string canvas *bfont* (format nil "~a_" buf) 12 (- h 18) *sh-text*)
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
     ("Backspace / C-g"    "back  (C-g at the root leaves)")
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

;;; ---- the Spotter: SuperL-Space fuzzy finder (§4¾) -------------------------
;;; A modal overlay over the browser's live canvas + input, like the debugger and
;;; the cheatsheet, but LIVE: it accumulates a query string and re-filters the
;;; candidate set (spotter-candidates / spotter-filter, present.lisp) on every
;;; keystroke. Enter opens the selected hit in a NEW trail (new-trail, shell.lisp).

(defvar *spotter-top* 0 "screen-y of the first Spotter result row, for click hit-testing.")
(defvar *spotter-rows* 0 "how many result rows the card is currently showing.")
(defvar *spotter-card* nil "the Spotter card rect (x0 y0 x1 y1) for click routing, or NIL.")
(defparameter +spotter-row+ 17)
(defparameter +spotter-cap+ 60 "Max results to rank/show — a few screenfuls.")

(defun dim-canvas (canvas factor)
  "Darken every pixel of CANVAS to FACTOR/256 of its brightness — a cheap modal scrim
   (the framebuffer has no alpha), so a floating panel reads as ABOVE the browser behind
   it. Only ever called on a Spotter keystroke, so the full-buffer pass is fine."
  (declare (type (integer 0 256) factor))
  (let ((px (lol.canvas:canvas-pixels canvas)))
    (declare (type (simple-array (unsigned-byte 32) (*)) px)
             (optimize (speed 3) (safety 0)))
    (dotimes (i (length px))
      (let ((p (aref px i)))
        (setf (aref px i)
              (logior (ash (ash (* (ldb (byte 8 16) p) factor) -8) 16)
                      (ash (ash (* (ldb (byte 8 8) p) factor) -8) 8)
                      (ash (* (ldb (byte 8 0) p) factor) -8)))))))

(defun draw-spotter (canvas w h query results sel)
  "The Spotter as a floating command palette OVER the (dimmed) browser — the tabs and
   panes stay visible behind it. A header hint, the live query line, and the ranked
   result rows (label + dim detail) with the selection highlighted. Records *spotter-card*
   / *spotter-top* / *spotter-rows* so a click routes to the right row or dismisses."
  (draw-browser canvas w h)                         ; the browser, as backdrop …
  (dim-canvas canvas 105)                            ; … dimmed, so the card floats above
  (let* ((cw   (min 780 (- w 80)))
         (cx   (floor (- w cw) 2))
         (cy   (+ +bar-h+ 14))                       ; just below the tab strip
         (maxr (max 3 (min 16 (floor (- h cy 70) +spotter-row+))))
         (rows (max 1 (min (length results) maxr)))
         (ch   (+ 56 (* rows +spotter-row+) 12)))
    (setf *spotter-card* (list cx cy (+ cx cw) (+ cy ch)) *spotter-rows* rows)
    (lol.canvas:fill-rect canvas (+ cx 5) (+ cy 6) cw ch !#080A0C)   ; drop shadow
    (lol.canvas:fill-rect canvas cx cy cw ch *sh-panel*)             ; the card
    (lol.canvas:fill-rect canvas cx cy cw 2 *sh-accent*)             ; accent top rule
    (lol.canvas:draw-rect canvas cx cy cw ch *sh-rule*)
    (lol.canvas:draw-string canvas *bfont*
      "Spotter — type to find · Enter opens a new tab · Esc closes"
      (+ cx 12) (+ cy 8) *sh-dim*)
    (let ((qy (+ cy 26)))                            ; the query line
      (lol.canvas:fill-rect canvas (+ cx 8) qy (- cw 16) 20 *sh-hi*)
      (lol.canvas:draw-rect canvas (+ cx 8) qy (- cw 16) 20 *sh-rule*)
      (lol.canvas:draw-string canvas *bfont* (format nil "> ~a_" query)
                              (+ cx 14) (+ qy 3) *sh-accent*)
      (let ((y (+ qy 30)))                           ; the results
        (setf *spotter-top* y)
        (if (null results)
            (lol.canvas:draw-string canvas *bfont* "(no matches)" (+ cx 16) y *sh-dim*)
            (loop for r in results for i from 0 while (< i rows)
                  do (when (= i sel)
                       (lol.canvas:fill-rect canvas (+ cx 6) (- y 2) (- cw 12) +spotter-row+ *sh-sel*)
                       (lol.canvas:fill-rect canvas (+ cx 6) (- y 2) 2 +spotter-row+ *sh-accent*))
                     (let ((pen (lol.canvas:draw-string canvas *bfont* (getf r :label) (+ cx 14) y
                                                        (if (= i sel) *sh-text* *sh-dim*))))
                       (lol.canvas:draw-string canvas *bfont* (format nil "  ~a" (getf r :detail))
                                               pen y *sh-dim*))
                     (incf y +spotter-row+)))
        (when (> (length results) rows)              ; a count of what didn't fit
          (lol.canvas:draw-string canvas *bfont* (format nil ".. +~d more" (- (length results) rows))
                                  (+ cx 14) y *sh-dim*))))))

(defun spotter-row-at (x y n)
  "The VISIBLE result-row index at screen X,Y within the card, or NIL. N = result count."
  (when (and *spotter-card* (>= y *spotter-top*)
             (<= (first *spotter-card*) x (third *spotter-card*)))
    (let ((i (floor (- y *spotter-top*) +spotter-row+)))
      (when (< i (min n *spotter-rows*)) i))))

(defun spotter-outside-p (x y)
  "True when X,Y is outside the Spotter card — a click there dismisses the palette."
  (or (null *spotter-card*)
      (destructuring-bind (x0 y0 x1 y1) *spotter-card*
        (not (and (<= x0 x x1) (<= y0 y y1))))))

(defun run-spotter-fb ()
  "The Spotter (SuperL-Space): fuzzy-find any definition, package or surface and open
   it in a new trail. Modal over the browser's canvas + input; live-filters on each
   keystroke. Returns after Enter (opens the hit) or Esc (dismiss)."
  (let* ((canvas *fb-canvas*) (w *fb-xres*) (h *fb-yres*)
         (cands (ignore-errors (spotter-candidates)))
         (query "")
         (results (ignore-errors (spotter-filter query cands +spotter-cap+)))
         (sel 0))
    (when canvas
      (loop
        (draw-spotter canvas w h query results sel)
        (when *fb-mouse* (draw-cursor canvas *cursor-x* *cursor-y*))
        (lol.fb:present-fb canvas)
        (flet ((refilter () (setf results (ignore-errors (spotter-filter query cands +spotter-cap+))
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
              (let ((i (spotter-row-at *cursor-x* *cursor-y* (length results))))
                (cond (i (new-trail (getf (nth i results) :subject)) (return))
                      ((spotter-outside-p *cursor-x* *cursor-y*) (return)))))))))))  ; click-away dismisses
