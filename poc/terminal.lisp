;;;; terminal.lisp — a VT engine: bytes from a program in, a grid of cells out.
;;;;
;;;; A terminal emulator is two translators glued to a pseudo-terminal:
;;;;
;;;;   program --bytes--> [ PARSER  -> GRID ] --pixels--> screen
;;;;   program <--bytes-- [ KEY ENCODER     ] <--keys---- keyboard
;;;;
;;;; This file is both translators and nothing else. It knows no pixels, no file
;;;; descriptors and no fonts: TERM-FEED takes octets and updates a grid of cells
;;;; (character + colours + attributes); ENCODE-KEY / ENCODE-MOUSE / ENCODE-PASTE turn
;;;; the environment's input events into the octets a program expects. Everything
;;;; here is pure data, so the whole engine is tested on the host with a stock SBCL
;;;; (poc/terminal-test.lisp) against output recorded from real programs. The pty
;;;; plumbing is initramfs/pty.lisp; the pane that draws a TERM is
;;;; poc/terminal-pane.lisp. The didactic tour: doc/terminal.org.
;;;;
;;;; WHICH terminal this is. Programs do not ask what we are; they read $TERM and look
;;;; the answer up in terminfo. We say TERM=xterm-256color, the most widely
;;;; understood entry, and then implement the subset of xterm that entry (and
;;;; readline, vim, less, htop, psql) actually uses — checked against
;;;; `infocmp -x xterm-256color` on the build host. Anything outside it is parsed
;;;; (so it can never leak onto the screen as garbage) and then ignored.
;;;;
;;;; THE PARSER is Paul Williams' DEC-compatible state machine
;;;; (https://vt100.net/emu/dec_ansi_parser): a handful of states (ground, escape,
;;;; CSI entry/param/intermediate/ignore, OSC string, DCS/SOS/PM/APC strings) and,
;;;; for each byte class, an action and a next state. It is the reason a terminal
;;;; never prints half a sequence: every byte belongs to exactly one state, and the
;;;; "anywhere" transitions (CAN, SUB, ESC) can always pull it back to safety.
;;;; We run it over CODE POINTS rather than bytes: the input is UTF-8, decoded first,
;;;; which is what xterm does in UTF-8 mode too.

(defpackage :lol.terminal
  (:use :cl)
  (:export
   ;; the engine
   #:term #:make-term #:term-feed #:term-feed-string #:term-resize #:term-reset
   #:term-cols #:term-rows #:term-cx #:term-cy #:term-cursor-visible-p #:term-cursor-style
   #:term-title #:term-cwd #:term-bell-count #:term-alt-screen-p
   #:term-take-copies
   #:term-app-cursor-p #:term-app-keypad-p #:term-bracketed-paste-p
   #:term-mouse-mode #:term-mouse-sgr-p #:term-take-output
   ;; looking at the grid
   #:tline #:tline-chars #:tline-attrs #:tline-wrapped
   #:term-line #:term-view-line #:term-row-text #:term-screen-text
   #:term-scrollback-count #:term-view-offset #:term-scroll-view #:term-view-top
   #:term-dirty #:term-dirty-p #:term-mark-all-dirty #:term-clear-dirty
   #:term-text-range #:+wide-tail+
   ;; cells' attributes
   #:attr-fg #:attr-bg #:attr-flags
   #:+bold+ #:+faint+ #:+italic+ #:+underline+ #:+blink+ #:+inverse+ #:+invisible+ #:+strike+
   #:color-rgb #:*palette*
   ;; input: keys, the mouse, a paste
   #:encode-key #:encode-mouse #:encode-paste
   ;; utilities shared with the pane
   #:utf8-encode #:char-cells))

(in-package :lol.terminal)

;;; ===========================================================================
;;; Cells: a character and one fixnum of attributes
;;; ===========================================================================
;;; A screen of 200x50 cells is 10,000 cells, and a busy program rewrites most of it
;;; many times a second, so a cell must not be an object. A LINE is two parallel
;;; arrays: the characters, and one fixnum per cell packing everything else:
;;;
;;;    bits 0-7    flags (bold, faint, italic, underline, blink, inverse, …)
;;;    bits 8-32   foreground colour (25 bits, below)
;;;    bits 33-57  background colour
;;;
;;; 58 bits: a fixnum on 64-bit SBCL (62 bits) holds it with room to spare, so the
;;; array is a specialised (SIMPLE-ARRAY FIXNUM) and nothing is boxed.
;;;
;;; A colour is 25 bits:  0 = "the default" (whatever the pane's theme says),
;;; 1..256 = palette index + 1 (the 16 ANSI colours and the xterm 256-colour cube),
;;; bit 24 set = a 24-bit truecolor RGB in the low 24 bits.

(defconstant +bold+      1)
(defconstant +faint+     2)
(defconstant +italic+    4)
(defconstant +underline+ 8)
(defconstant +blink+     16)
(defconstant +inverse+   32)
(defconstant +invisible+ 64)
(defconstant +strike+    128)

(defconstant +truecolor+ (ash 1 24))

(declaim (inline make-attr attr-fg attr-bg attr-flags))
(defun make-attr (flags fg bg)
  (logior flags (ash fg 8) (ash bg 33)))
(defun attr-flags (a) (ldb (byte 8 0) a))
(defun attr-fg (a)    (ldb (byte 25 8) a))
(defun attr-bg (a)    (ldb (byte 25 33) a))

(defconstant +wide-tail+ (code-char 0)
  "The character stored in the RIGHT half of a double-width character. It draws as
   nothing: the glyph in the left cell covers both.")

(defstruct (tline (:constructor %make-tline (chars attrs)))
  "One row of cells. WRAPPED is true when the text ran off the end of this row and
   continued on the next (autowrap) rather than ending in a newline — the difference
   matters when you copy a long command line: it should come back as one line."
  (chars "" :type simple-string)
  (attrs (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (wrapped nil))

(defun make-tline (cols &optional (attr 0))
  (%make-tline (make-string cols :initial-element #\Space)
               (make-array cols :element-type 'fixnum :initial-element attr)))

(defun tline-resize (line cols)
  "LINE at width COLS: truncated, or padded with blank cells."
  (let ((old (length (tline-chars line))))
    (if (= old cols)
        line
        (let ((new (make-tline cols)))
          (replace (tline-chars new) (tline-chars line))
          (replace (tline-attrs new) (tline-attrs line))
          (setf (tline-wrapped new) (and (>= cols old) (tline-wrapped line)))
          new))))

;;; ===========================================================================
;;; Colours
;;; ===========================================================================
;;; The 16 ANSI colours are a matter of taste (every terminal ships its own); these
;;; are tuned to sit on the browser's dark panel. 16..231 are xterm's 6x6x6 cube,
;;; 232..255 its grey ramp — those two are fixed by convention, and programs that
;;; pick "colour 208" expect xterm's orange.

(defparameter *palette*
  (let ((p (make-array 256 :element-type '(unsigned-byte 32))))
    (replace p '(#x1B1E24 #xE06C75 #x98C379 #xE5C07B #x61AFEF #xC678DD #x56B6C2 #xC8C6BE
                 #x5C6370 #xFF7B86 #xB5E890 #xFFD787 #x82C4FF #xE09BF0 #x7FDBCA #xFFFFFF))
    (let ((steps #(0 95 135 175 215 255)))
      (dotimes (i 216)
        (setf (aref p (+ 16 i))
              (logior (ash (aref steps (floor i 36)) 16)
                      (ash (aref steps (mod (floor i 6) 6)) 8)
                      (aref steps (mod i 6))))))
    (dotimes (i 24)
      (let ((v (+ 8 (* 10 i)))) (setf (aref p (+ 232 i)) (logior (ash v 16) (ash v 8) v))))
    p)
  "Index -> #xRRGGBB for the 256 indexed colours.")

(defun color-rgb (color default)
  "The #xRRGGBB a 25-bit COLOR stands for; DEFAULT when it is 0 (the theme's)."
  (cond ((zerop color) default)
        ((logtest color +truecolor+) (logand color #xFFFFFF))
        (t (aref *palette* (min 255 (1- color))))))

;;; ===========================================================================
;;; The terminal's state
;;; ===========================================================================

(defstruct (screen (:constructor %make-screen))
  "One of the two screens. The PRIMARY screen is where a shell prints, and lines that
   scroll off its top go to the scrollback. The ALTERNATE screen is a blank page a
   full-screen program (vim, less, htop) asks for so that, when it exits, the shell's
   screen comes back exactly as it was — that is the whole trick of mode 1049."
  (lines #() :type simple-vector)
  (saved nil))                          ; DECSC's saved cursor, per screen as in xterm

(defun make-screen (cols rows)
  (let ((v (make-array rows)))
    (dotimes (i rows) (setf (svref v i) (make-tline cols)))
    (%make-screen :lines v)))

(defstruct (term (:constructor %make-term))
  (cols 80 :type fixnum)
  (rows 24 :type fixnum)
  primary alt screen                    ; SCREEN is whichever of the two is showing
  ;; the cursor, and the "pending wrap" flag: writing into the LAST column leaves the
  ;; cursor ON that column with this flag set, and only the NEXT printable character
  ;; wraps. That is DEC's rule (and xterm's), and programs depend on it: a status
  ;; line drawn into the bottom-right cell must not scroll the screen.
  (cx 0 :type fixnum) (cy 0 :type fixnum) (pending-wrap nil)
  (pen 0 :type fixnum)                  ; the attributes new characters get (SGR)
  (top 0 :type fixnum) (bottom 23 :type fixnum) ; the scroll region (DECSTBM), inclusive
  ;; modes
  (autowrap t) (origin nil) (insert nil) (app-cursor nil) (app-keypad nil)
  (cursor-visible t) (cursor-style 0) (bracketed-paste nil) (newline-mode nil)
  (mouse-mode nil)                      ; NIL, :x10, :press-release, :button-motion, :any-motion
  (mouse-sgr nil) (focus-events nil)
  ;; character sets: G0 and G1, and which one is "shifted in" (SI / SO)
  (g0 :ascii) (g1 :ascii) (gl 0)
  (tabs (make-array 80 :element-type 'bit) :type simple-bit-vector)
  ;; the scrollback: a ring of lines that left the top of the primary screen
  (sb-lines (make-array 2000) :type simple-vector)
  (sb-start 0 :type fixnum) (sb-count 0 :type fixnum)
  (view-offset 0 :type fixnum)          ; how far the user has scrolled back (0 = live)
  ;; what the program told us about itself
  (title "") (cwd nil) (bell-count 0 :type fixnum)
  ;; texts the program asked to put on the clipboard (OSC 52), oldest first: (TARGETS . STRING)
  (copies '())
  ;; bytes the TERMINAL must send back to the program (answers to DSR, DA, …)
  (output (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  ;; rows that changed since the pane last drew (so only those are repainted)
  (dirty (make-array 24 :element-type 'bit :initial-element 1) :type simple-bit-vector)
  ;; --- the parser's state (Williams) ---
  (state :ground)
  (params '())                          ; finished parameter groups, newest first
  (param nil)                           ; the number being read, or NIL
  (subparams '())                       ; colon-separated parts of the current group
  (prefix nil)                          ; a private marker: ? > < =
  (intermediates '())                   ; 0x20-0x2F bytes, e.g. the space in "CSI 2 q"
  (string-buf (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
  (string-esc nil)                      ; saw ESC inside a string: is it ST (ESC \)?
  ;; --- the UTF-8 decoder's state ---
  (u8-need 0 :type fixnum) (u8-code 0 :type fixnum))

(defun make-term (cols rows &key (scrollback 2000))
  "A blank COLS x ROWS terminal with SCROLLBACK lines of history."
  (let ((term (%make-term :cols cols :rows rows :bottom (1- rows)
                          :sb-lines (make-array (max 1 scrollback) :initial-element nil))))
    (setf (term-primary term) (make-screen cols rows)
          (term-alt term) (make-screen cols rows)
          (term-screen term) (term-primary term))
    (reset-tabs term)
    (setf (term-dirty term) (make-array rows :element-type 'bit :initial-element 1))
    term))

(defun reset-tabs (term)
  (let ((v (make-array (term-cols term) :element-type 'bit :initial-element 0)))
    (loop for i from 8 below (term-cols term) by 8 do (setf (sbit v i) 1))
    (setf (term-tabs term) v)))

(defun term-reset (term)
  "RIS (ESC c): everything back to power-on, the scrollback kept."
  (let ((cols (term-cols term)) (rows (term-rows term)))
    (setf (term-primary term) (make-screen cols rows)
          (term-alt term) (make-screen cols rows)
          (term-screen term) (term-primary term)
          (term-cx term) 0 (term-cy term) 0 (term-pending-wrap term) nil
          (term-pen term) 0 (term-top term) 0 (term-bottom term) (1- rows)
          (term-autowrap term) t (term-origin term) nil (term-insert term) nil
          (term-app-cursor term) nil (term-app-keypad term) nil
          (term-cursor-visible term) t (term-cursor-style term) 0
          (term-bracketed-paste term) nil (term-newline-mode term) nil
          (term-mouse-mode term) nil (term-mouse-sgr term) nil
          (term-g0 term) :ascii (term-g1 term) :ascii (term-gl term) 0
          (term-view-offset term) 0)
    (reset-tabs term)
    (term-mark-all-dirty term)))

;;; Small readers the pane uses (with names that say what they mean)
(defun term-cursor-visible-p (term) (term-cursor-visible term))
(defun term-alt-screen-p (term) (eq (term-screen term) (term-alt term)))
(defun term-app-cursor-p (term) (term-app-cursor term))
(defun term-app-keypad-p (term) (term-app-keypad term))
(defun term-bracketed-paste-p (term) (term-bracketed-paste term))
(defun term-mouse-sgr-p (term) (term-mouse-sgr term))

(defun term-take-output (term)
  "The bytes the terminal owes the program (answers to queries), or NIL — and forget
   them: the caller is about to write them to the pty."
  (let ((out (term-output term)))
    (when (plusp (fill-pointer out))
      (prog1 (coerce out '(simple-array (unsigned-byte 8) (*)))
        (setf (fill-pointer out) 0)))))

(defun reply (term string)
  "Queue STRING (ASCII) as an answer to the program."
  (loop for c across string do (vector-push-extend (char-code c) (term-output term))))

;;; ---- dirty rows ----------------------------------------------------------

(declaim (inline mark-dirty))
(defun mark-dirty (term y)
  (when (< -1 y (length (term-dirty term))) (setf (sbit (term-dirty term) y) 1)))

(defun term-mark-all-dirty (term) (fill (term-dirty term) 1))
(defun term-clear-dirty (term) (fill (term-dirty term) 0))
(defun term-dirty-p (term) (find 1 (term-dirty term)))

;;; ---- lines -----------------------------------------------------------------

(declaim (inline cur-lines term-line))
(defun cur-lines (term) (screen-lines (term-screen term)))
(defun term-line (term y)
  "Screen row Y of the screen now showing (no scrollback)."
  (svref (cur-lines term) y))

(defun blank-attr (term)
  "What an erased cell looks like. xterm-256color declares `bce' (background colour
   erase): erasing paints with the CURRENT background, not the default one — so a
   program can clear a region to blue with one SGR and one EL."
  (make-attr 0 0 (attr-bg (term-pen term))))

(defun blank-line (term)
  (make-tline (term-cols term) (blank-attr term)))

(defun clear-cells (term line from to)
  "Erase cells [FROM, TO) of LINE."
  (let ((a (blank-attr term)) (to (min to (length (tline-chars line)))))
    (when (< from to)
      (fill (tline-chars line) #\Space :start from :end to)
      (fill (tline-attrs line) a :start from :end to))))

;;; ===========================================================================
;;; The scrollback
;;; ===========================================================================
;;; A ring buffer: once full, the oldest line is overwritten. Only lines leaving the
;;; top of the PRIMARY screen, with the scroll region at the top, go there: a
;;; full-screen program on the alternate screen, or one scrolling a region in the
;;; middle (a status bar app), is redrawing, not printing — its lines are not history.

(defun term-scrollback-count (term) (term-sb-count term))

(defun sb-push (term line)
  (let* ((v (term-sb-lines term)) (cap (length v)))
    (if (< (term-sb-count term) cap)
        (progn (setf (svref v (mod (+ (term-sb-start term) (term-sb-count term)) cap)) line)
               (incf (term-sb-count term)))
        (progn (setf (svref v (term-sb-start term)) line)
               (setf (term-sb-start term) (mod (1+ (term-sb-start term)) cap))))
    ;; somebody reading the history keeps their place while new lines arrive
    (when (plusp (term-view-offset term))
      (setf (term-view-offset term) (min (term-sb-count term) (1+ (term-view-offset term)))))))

(defun sb-pop (term)
  "Take the NEWEST line back out of the scrollback (a window growing taller pulls the
   history back onto the screen, as xterm does), or NIL."
  (when (plusp (term-sb-count term))
    (decf (term-sb-count term))
    (let* ((v (term-sb-lines term))
           (i (mod (+ (term-sb-start term) (term-sb-count term)) (length v))))
      (prog1 (svref v i) (setf (svref v i) nil)))))

(defun sb-line (term i)
  "Scrollback line I, 0 = the oldest kept."
  (let ((v (term-sb-lines term)))
    (svref v (mod (+ (term-sb-start term) i) (length v)))))

(defun term-view-top (term)
  "The absolute row index shown at the top of the view. Absolute rows count the
   scrollback first (0 = oldest kept line) and then the screen."
  (- (term-sb-count term) (term-view-offset term)))

(defun term-abs-line (term r)
  "Absolute row R: scrollback then screen."
  (let ((n (term-sb-count term)))
    (if (< r n) (sb-line term r) (term-line term (- r n)))))

(defun term-view-line (term y)
  "The line drawn at view row Y — from the scrollback while scrolled back."
  (if (or (zerop (term-view-offset term)) (term-alt-screen-p term))
      (term-line term y)
      (term-abs-line term (+ (term-view-top term) y))))

(defun term-scroll-view (term n)
  "Scroll the VIEW N lines back into history (negative: towards the present).
   Returns T when the view moved. The alternate screen has no history to scroll."
  (let* ((old (term-view-offset term))
         (new (if (term-alt-screen-p term) 0
                  (max 0 (min (term-sb-count term) (+ old n))))))
    (unless (= new old)
      (setf (term-view-offset term) new)
      (term-mark-all-dirty term)
      t)))

;;; ===========================================================================
;;; Scrolling the screen, and moving the cursor
;;; ===========================================================================

(defun scroll-up (term n &optional (top (term-top term)) (bottom (term-bottom term)))
  "Move rows TOP..BOTTOM up by N; blank rows appear at the bottom. Lines leaving the
   very top of the primary screen go to the scrollback."
  (let* ((lines (cur-lines term))
         (n (min n (1+ (- bottom top)))))
    (dotimes (k n)
      (let ((gone (svref lines top)))
        (when (and (zerop top) (not (term-alt-screen-p term)))
          (sb-push term gone))
        (replace lines lines :start1 top :start2 (1+ top) :end2 (1+ bottom))
        (setf (svref lines bottom) (blank-line term))))
    (loop for y from top to bottom do (mark-dirty term y))))

(defun scroll-down (term n &optional (top (term-top term)) (bottom (term-bottom term)))
  "Move rows TOP..BOTTOM down by N; blank rows appear at the top."
  (let* ((lines (cur-lines term))
         (n (min n (1+ (- bottom top)))))
    (dotimes (k n)
      (replace lines lines :start1 (1+ top) :end1 (1+ bottom) :start2 top)
      (setf (svref lines top) (blank-line term)))
    (loop for y from top to bottom do (mark-dirty term y))))

(defun set-cursor (term x y)
  "Put the cursor at X,Y (clamped to the screen) and clear the pending wrap. Both the
   row it leaves and the row it lands on are marked dirty, because the cursor is drawn."
  (mark-dirty term (term-cy term))
  (setf (term-cx term) (max 0 (min x (1- (term-cols term))))
        (term-cy term) (max 0 (min y (1- (term-rows term))))
        (term-pending-wrap term) nil)
  (mark-dirty term (term-cy term)))

(defun linefeed (term)
  "LF: down a row; at the bottom of the scroll region, scroll it instead."
  (if (= (term-cy term) (term-bottom term))
      (progn (scroll-up term 1) (setf (term-pending-wrap term) nil))
      (set-cursor term (term-cx term) (1+ (term-cy term)))))

(defun reverse-index (term)
  "RI (ESC M): up a row; at the top of the region, scroll it down."
  (if (= (term-cy term) (term-top term))
      (scroll-down term 1)
      (set-cursor term (term-cx term) (1- (term-cy term)))))

;;; ===========================================================================
;;; Printing a character
;;; ===========================================================================

(defun char-cells (code)
  "How many columns the character CODE occupies: 0 (a combining mark, drawn over the
   previous cell — we simply drop it), 1, or 2 (East Asian wide, and most emoji).
   A terminal and the program must AGREE on this or every cursor movement after the
   character is off by one; this is a compact version of the table glibc's wcwidth
   uses, which is what the programs on the other side consult."
  (cond ((< code #x300) 1)
        ((or (<= #x300 code #x36F) (<= #x1AB0 code #x1AFF) (<= #x1DC0 code #x1DFF)
             (<= #x20D0 code #x20FF) (<= #xFE20 code #xFE2F)
             (= code #x200B) (<= #x200C code #x200F) (= code #xFEFF))
         0)
        ((or (<= #x1100 code #x115F) (<= #x2E80 code #x303E) (<= #x3041 code #x33FF)
             (<= #x3400 code #x4DBF) (<= #x4E00 code #x9FFF) (<= #xA000 code #xA4CF)
             (<= #xAC00 code #xD7A3) (<= #xF900 code #xFAFF) (<= #xFE30 code #xFE4F)
             (<= #xFF00 code #xFF60) (<= #xFFE0 code #xFFE6)
             (<= #x1F300 code #x1F64F) (<= #x1F900 code #x1F9FF)
             (<= #x20000 code #x3FFFD))
         2)
        (t 1)))

;;; DEC Special Graphics: the character set behind ESC ( 0. Before Unicode, this was
;;; how a terminal drew boxes — the program switches G0 to it and sends plain ASCII
;;; letters, and "q" comes out as a horizontal line. ncurses still uses it whenever
;;; the locale is not UTF-8 (the `acsc', `smacs', `rmacs' capabilities), which is
;;; exactly the situation inside a fresh container with no LANG set.
(defparameter +dec-graphics+
  (let ((h (make-hash-table)))
    (loop for (from . to) in '((#\` . #\U+25C6) (#\a . #\U+2592) (#\f . #\U+00B0)
                               (#\g . #\U+00B1) (#\j . #\U+2518) (#\k . #\U+2510)
                               (#\l . #\U+250C) (#\m . #\U+2514) (#\n . #\U+253C)
                               (#\o . #\U+23BA) (#\p . #\U+23BB) (#\q . #\U+2500)
                               (#\r . #\U+23BC) (#\s . #\U+23BD) (#\t . #\U+251C)
                               (#\u . #\U+2524) (#\v . #\U+2534) (#\w . #\U+252C)
                               (#\x . #\U+2502) (#\y . #\U+2264) (#\z . #\U+2265)
                               (#\{ . #\U+03C0) (#\| . #\U+2260) (#\} . #\U+00A3)
                               (#\~ . #\U+00B7) (#\_ . #\Space))
          do (setf (gethash from h) to))
    h))

(defun put-char (term code)
  "Print the character CODE at the cursor, the way a VT does."
  (let* ((charset (if (zerop (term-gl term)) (term-g0 term) (term-g1 term)))
         (ch (or (and (eq charset :dec-graphics) (< code 128)
                      (gethash (code-char code) +dec-graphics+))
                 (code-char code)))
         (w (char-cells (char-code ch)))
         (cols (term-cols term)))
    (when (zerop w) (return-from put-char))
    ;; the deferred wrap from the last character
    (when (and (term-pending-wrap term) (term-autowrap term))
      (setf (tline-wrapped (term-line term (term-cy term))) t)
      (setf (term-pending-wrap term) nil)
      (linefeed term)
      (set-cursor term 0 (term-cy term)))
    ;; a wide character that does not fit in the last column wraps whole
    (when (and (= w 2) (= (term-cx term) (1- cols)))
      (if (term-autowrap term)
          (progn (clear-cells term (term-line term (term-cy term)) (term-cx term) cols)
                 (setf (tline-wrapped (term-line term (term-cy term))) t)
                 (linefeed term)
                 (set-cursor term 0 (term-cy term)))
          (setf w 1 ch #\?)))
    (let* ((line (term-line term (term-cy term)))
           (x (term-cx term))
           (chars (tline-chars line)) (attrs (tline-attrs line)))
      ;; IRM (insert mode): shift the rest of the line right first
      (when (term-insert term)
        (replace chars chars :start1 (+ x w) :start2 x)
        (replace attrs attrs :start1 (+ x w) :start2 x))
      (setf (schar chars x) ch (aref attrs x) (term-pen term))
      (when (= w 2)
        (setf (schar chars (1+ x)) +wide-tail+ (aref attrs (1+ x)) (term-pen term)))
      (mark-dirty term (term-cy term))
      (if (>= (+ x w) cols)
          (if (term-autowrap term)
              (setf (term-cx term) (- cols 1) (term-pending-wrap term) t)
              (setf (term-cx term) (- cols 1)))
          (setf (term-cx term) (+ x w))))))

;;; ===========================================================================
;;; Control characters (C0)
;;; ===========================================================================

(defun next-tab (term x)
  (loop for i from (1+ x) below (term-cols term)
        when (= 1 (sbit (term-tabs term) i)) do (return i)
        finally (return (1- (term-cols term)))))

(defun prev-tab (term x)
  (loop for i from (1- x) downto 0
        when (= 1 (sbit (term-tabs term) i)) do (return i)
        finally (return 0)))

(defun execute-c0 (term code)
  (case code
    (7  (incf (term-bell-count term)))                       ; BEL
    (8  (when (plusp (term-cx term))                         ; BS
          (set-cursor term (1- (term-cx term)) (term-cy term))))
    (9  (set-cursor term (next-tab term (term-cx term)) (term-cy term))) ; HT
    ((10 11 12)                                              ; LF VT FF
     (linefeed term)
     (when (term-newline-mode term) (set-cursor term 0 (term-cy term))))
    (13 (set-cursor term 0 (term-cy term)))                  ; CR
    (14 (setf (term-gl term) 1))                             ; SO: G1 into GL
    (15 (setf (term-gl term) 0))))                           ; SI: G0 back

;;; ===========================================================================
;;; The parser (Williams' state machine)
;;; ===========================================================================
;;; One function per byte class would be faithful to the diagram; one CASE on the
;;; state with the classes inside reads better in Lisp and is the same machine.
;;;
;;;   ground ──ESC──> escape ──[──> csi-entry ──digit/;──> csi-param ──final──> ground
;;;                     │                └──?>=<──┘ (private marker)     (dispatch)
;;;                     ├──]──> osc-string ──BEL or ESC \──> ground (dispatch title …)
;;;                     ├──P──> dcs-string  ──ESC \──> ground (ignored)
;;;                     ├──X ^ _──> sos/pm/apc string (ignored)
;;;                     ├──( ) # space…──> escape-intermediate ──final──> ground
;;;                     └──final──> ground (ESC 7, ESC M, ESC c, …)
;;;
;;; plus the transitions from ANYWHERE: CAN/SUB abort a sequence, ESC starts a new
;;; one, and the C0 controls (CR, LF, BS, …) execute even in the middle of a CSI
;;; sequence — a VT100 really did that, and so programs may rely on it.

(defun clear-sequence (term)
  (setf (term-params term) '() (term-param term) nil (term-subparams term) '()
        (term-prefix term) nil (term-intermediates term) '()))

(defun finish-param (term)
  "Close the current parameter group (a ; was seen, or the final byte)."
  (let ((group (reverse (cons (term-param term) (term-subparams term)))))
    (push group (term-params term))
    (setf (term-param term) nil (term-subparams term) '())))

(defun term-feed-string (term string)
  "Feed a Lisp STRING (already characters) — for tests and for the Lisp REPL pane."
  (loop for c across string do (parse-code term (char-code c))))

(defun term-feed (term octets &key (start 0) (end (length octets)))
  "Feed raw OCTETS from the program: decode UTF-8, then run each code point through
   the parser. A multi-byte character split between two reads is completed by the
   next call — the decoder's state lives in TERM, not here."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets) (fixnum start end))
  (let ((i start))
    (declare (fixnum i))
    (loop while (< i end) do
      (let ((b (aref octets i)))
        (incf i)
        (cond
          ;; inside a multi-byte sequence: a continuation byte 10xxxxxx
          ((plusp (term-u8-need term))
           (if (= (logand b #xC0) #x80)
               (progn
                 (setf (term-u8-code term) (logior (ash (term-u8-code term) 6) (logand b #x3F)))
                 (when (zerop (decf (term-u8-need term)))
                   (parse-code term (let ((c (term-u8-code term)))
                                      (if (or (> c #x10FFFF) (<= #xD800 c #xDFFF)) #xFFFD c)))))
               ;; a broken sequence: show the replacement character, and read this
               ;; byte again from scratch — it may well be a perfectly good ESC
               (progn (setf (term-u8-need term) 0)
                      (parse-code term #xFFFD)
                      (decf i))))
          ((< b #x80) (parse-code term b))
          ((= (logand b #xE0) #xC0) (setf (term-u8-need term) 1 (term-u8-code term) (logand b #x1F)))
          ((= (logand b #xF0) #xE0) (setf (term-u8-need term) 2 (term-u8-code term) (logand b #x0F)))
          ((= (logand b #xF8) #xF0) (setf (term-u8-need term) 3 (term-u8-code term) (logand b #x07)))
          (t (parse-code term #xFFFD)))))))

(defun parse-code (term c)
  "One code point through the state machine."
  (declare (fixnum c))
  (let ((state (term-state term)))
    ;; ---- the string states swallow everything up to their terminator ----
    (when (member state '(:osc-string :dcs-string :ignore-string))
      (cond
        ((term-string-esc term)                ; the byte after an ESC in a string
         (setf (term-string-esc term) nil)
         (end-string term)
         (unless (= c 92)                      ; ESC \ is ST: the string is done
           ;; anything else: that ESC began a new sequence
           (setf (term-state term) :escape)
           (clear-sequence term)
           (parse-code term c)))
        ((= c 27) (setf (term-string-esc term) t))
        ((and (= c 7) (eq state :osc-string)) (end-string term))  ; BEL ends an OSC too
        ((member c '(24 26)) (setf (term-state term) :ground))
        ((eq state :osc-string)
         (when (and (>= c 32) (< (fill-pointer (term-string-buf term)) (osc-limit term)))
           (vector-push-extend (code-char c) (term-string-buf term)))))
      (return-from parse-code))
    ;; ---- anywhere ----
    (cond
      ((= c 27) (setf (term-state term) :escape) (clear-sequence term)
       (return-from parse-code))
      ((member c '(24 26)) (setf (term-state term) :ground) (return-from parse-code))
      ((or (< c 32) (= c 127))
       (unless (= c 127) (execute-c0 term c))   ; DEL is ignored everywhere
       (return-from parse-code)))
    (ecase state
      (:ground
       (if (<= #x80 c #x9F) nil (put-char term c)))   ; raw C1 code points: ignored
      (:escape
       (cond
         ((= c 91) (setf (term-state term) :csi-entry))                ; [
         ((= c 93) (setf (term-state term) :osc-string)                ; ]
          (setf (fill-pointer (term-string-buf term)) 0))
         ((= c 80) (setf (term-state term) :dcs-string))               ; P
         ((member c '(88 94 95)) (setf (term-state term) :ignore-string)) ; X ^ _
         ((<= 32 c 47) (push c (term-intermediates term))
          (setf (term-state term) :escape-intermediate))
         (t (esc-dispatch term c) (setf (term-state term) :ground))))
      (:escape-intermediate
       (if (<= 32 c 47)
           (push c (term-intermediates term))
           (progn (esc-dispatch term c) (setf (term-state term) :ground))))
      ((:csi-entry :csi-param)
       (cond
         ((<= 48 c 57)                                                 ; a digit
          (setf (term-param term) (min 65535 (+ (* 10 (or (term-param term) 0)) (- c 48)))
                (term-state term) :csi-param))
         ((= c 59) (finish-param term) (setf (term-state term) :csi-param))   ; ;
         ((= c 58) (push (term-param term) (term-subparams term))             ; : sub-parameter
          (setf (term-param term) nil (term-state term) :csi-param))
         ((and (<= 60 c 63) (eq state :csi-entry))                      ; ? > = < private marker
          (setf (term-prefix term) (code-char c)))
         ((<= 60 c 63) (setf (term-state term) :csi-ignore))           ; a marker too late: bad
         ((<= 32 c 47) (push c (term-intermediates term))
          (setf (term-state term) :csi-intermediate))
         ((<= 64 c 126) (csi-finish term c))
         (t (setf (term-state term) :csi-ignore))))
      (:csi-intermediate
       (cond ((<= 32 c 47) (push c (term-intermediates term)))
             ((<= 64 c 126) (csi-finish term c))
             (t (setf (term-state term) :csi-ignore))))
      (:csi-ignore
       (when (<= 64 c 126) (setf (term-state term) :ground))))))

(defun csi-finish (term final)
  (when (or (term-param term) (term-subparams term) (term-params term))
    (finish-param term))
  (setf (term-state term) :ground)
  (csi-dispatch term final (term-prefix term) (reverse (term-intermediates term))
                (reverse (term-params term))))

(defun end-string (term)
  "An OSC/DCS/… string ended. Only OSC carries anything we use."
  (when (eq (term-state term) :osc-string)
    (osc-dispatch term (coerce (term-string-buf term) 'simple-string)))
  (setf (term-state term) :ground))

;;; ===========================================================================
;;; ESC sequences
;;; ===========================================================================

(defun save-cursor (term)
  (setf (screen-saved (term-screen term))
        (list (term-cx term) (term-cy term) (term-pen term) (term-origin term)
              (term-g0 term) (term-g1 term) (term-gl term) (term-pending-wrap term))))

(defun restore-cursor (term)
  (destructuring-bind (&optional (x 0) (y 0) (pen 0) origin (g0 :ascii) (g1 :ascii) (gl 0) wrap)
      (screen-saved (term-screen term))
    (set-cursor term x y)
    (setf (term-pen term) pen (term-origin term) origin
          (term-g0 term) g0 (term-g1 term) g1 (term-gl term) gl
          (term-pending-wrap term) wrap)))

(defun esc-dispatch (term c)
  (let ((inter (first (term-intermediates term))))
    (cond
      ;; ESC ( X / ESC ) X: designate a character set into G0 / G1
      ((and inter (member inter '(40 41)))
       (let ((set (if (= c 48) :dec-graphics :ascii)))     ; "0" = DEC graphics
         (if (= inter 40) (setf (term-g0 term) set) (setf (term-g1 term) set))))
      ;; ESC # 8: DECALN, the screen-alignment test — fill with E (vttest uses it)
      ((and (eql inter 35) (= c 56))
       (dotimes (y (term-rows term))
         (fill (tline-chars (term-line term y)) #\E)
         (mark-dirty term y)))
      (inter nil)                                         ; anything else: ignored
      (t
       (case (code-char c)
         (#\7 (save-cursor term))                          ; DECSC
         (#\8 (restore-cursor term))                       ; DECRC
         (#\D (linefeed term))                             ; IND
         (#\E (linefeed term) (set-cursor term 0 (term-cy term))) ; NEL
         (#\H (setf (sbit (term-tabs term) (term-cx term)) 1))    ; HTS
         (#\M (reverse-index term))                        ; RI
         (#\c (term-reset term))                           ; RIS
         (#\= (setf (term-app-keypad term) t))             ; DECKPAM
         (#\> (setf (term-app-keypad term) nil)))))))      ; DECKPNM

;;; ===========================================================================
;;; CSI sequences
;;; ===========================================================================

(defun param (params i &optional (default 0))
  "Parameter I of PARAMS (each a group of colon parts), or DEFAULT when absent or 0
   where 0 means 'default' — which is how every movement parameter works."
  (let ((v (first (nth i params))))
    (if (or (null v) (zerop v)) default v)))

(defun param0 (params i)
  "Parameter I where 0 is a real value (ED, EL, TBC, SGR …)."
  (or (first (nth i params)) 0))

(defun region-top (term) (if (term-origin term) (term-top term) 0))

(defun erase-display (term mode)
  (let ((cy (term-cy term)) (cx (term-cx term)))
    (case mode
      (0 (clear-cells term (term-line term cy) cx (term-cols term))
         (loop for y from (1+ cy) below (term-rows term)
               do (clear-cells term (term-line term y) 0 (term-cols term))))
      (1 (loop for y from 0 below cy
               do (clear-cells term (term-line term y) 0 (term-cols term)))
         (clear-cells term (term-line term cy) 0 (1+ cx)))
      (2 (dotimes (y (term-rows term))
           (clear-cells term (term-line term y) 0 (term-cols term))))
      (3 (setf (term-sb-count term) 0 (term-sb-start term) 0 (term-view-offset term) 0)
         (fill (term-sb-lines term) nil)))
    (dotimes (y (term-rows term))
      (setf (tline-wrapped (term-line term y)) (and (/= mode 2) (tline-wrapped (term-line term y)))))
    (term-mark-all-dirty term)))

(defun erase-line (term mode)
  (let ((line (term-line term (term-cy term))) (cx (term-cx term)))
    (case mode
      (0 (clear-cells term line cx (term-cols term)) (setf (tline-wrapped line) nil))
      (1 (clear-cells term line 0 (1+ cx)))
      (2 (clear-cells term line 0 (term-cols term)) (setf (tline-wrapped line) nil)))
    (mark-dirty term (term-cy term))))

(defun insert-lines (term n)
  "IL: N blank lines at the cursor, inside the scroll region only."
  (when (<= (term-top term) (term-cy term) (term-bottom term))
    (scroll-down term n (term-cy term) (term-bottom term))
    (set-cursor term 0 (term-cy term))))

(defun delete-lines (term n)
  "DL: remove N lines at the cursor; the region below moves up."
  (when (<= (term-top term) (term-cy term) (term-bottom term))
    ;; deleting is not scrolling off the top of the screen: nothing goes to history
    (let* ((lines (cur-lines term)) (top (term-cy term)) (bottom (term-bottom term))
           (n (min n (1+ (- bottom top)))))
      (dotimes (k n)
        (replace lines lines :start1 top :start2 (1+ top) :end2 (1+ bottom))
        (setf (svref lines bottom) (blank-line term)))
      (loop for y from top to bottom do (mark-dirty term y)))
    (set-cursor term 0 (term-cy term))))

(defun shift-cells (term n insert)
  "ICH (INSERT true) / DCH: open or close N cells at the cursor."
  (let* ((line (term-line term (term-cy term)))
         (chars (tline-chars line)) (attrs (tline-attrs line))
         (x (term-cx term)) (cols (term-cols term)) (n (min n (- cols x))))
    (if insert
        (progn (replace chars chars :start1 (+ x n) :start2 x)
               (replace attrs attrs :start1 (+ x n) :start2 x)
               (clear-cells term line x (+ x n)))
        (progn (replace chars chars :start1 x :start2 (+ x n))
               (replace attrs attrs :start1 x :start2 (+ x n))
               (clear-cells term line (- cols n) cols)))
    (setf (term-pending-wrap term) nil)
    (mark-dirty term (term-cy term))))

(defun switch-screen (term alt clear)
  "Show the alternate screen (ALT true) or the primary. CLEAR blanks the alternate
   screen on the way in — what 1047/1049 do, so a program never finds the last one's
   leftovers there."
  (let ((target (if alt (term-alt term) (term-primary term))))
    (unless (eq target (term-screen term))
      (setf (term-screen term) target (term-view-offset term) 0)
      (when (and alt clear)
        (let ((pen (term-pen term)))
          (setf (term-pen term) 0)
          (dotimes (y (term-rows term))
            (setf (svref (screen-lines target) y) (blank-line term)))
          (setf (term-pen term) pen)))
      (term-mark-all-dirty term))))

(defun set-dec-mode (term mode on)
  "DECSET (CSI ? n h) / DECRST (CSI ? n l)."
  (case mode
    (1 (setf (term-app-cursor term) on))                  ; DECCKM: arrows send ESC O A
    (6 (setf (term-origin term) on) (set-cursor term 0 (region-top term))) ; DECOM
    (7 (setf (term-autowrap term) on))                    ; DECAWM
    (9 (setf (term-mouse-mode term) (and on :x10)))       ; X10 mouse: presses only
    (25 (setf (term-cursor-visible term) on) (mark-dirty term (term-cy term))) ; DECTCEM
    (47 (switch-screen term on nil))
    (1047 (if on (switch-screen term t t) (switch-screen term nil nil)))
    (1048 (if on (save-cursor term) (restore-cursor term)))
    (1049 (if on
              (progn (save-cursor term) (switch-screen term t t))
              (progn (switch-screen term nil nil) (restore-cursor term))))
    (1000 (setf (term-mouse-mode term) (and on :press-release)))
    (1002 (setf (term-mouse-mode term) (and on :button-motion)))
    (1003 (setf (term-mouse-mode term) (and on :any-motion)))
    (1004 (setf (term-focus-events term) on))
    (1006 (setf (term-mouse-sgr term) on))
    (2004 (setf (term-bracketed-paste term) on))))

(defun dec-mode-value (term mode)
  "What DECRQM reports for MODE: 1 set, 2 reset, 0 not recognised."
  (flet ((b (x) (if x 1 2)))
    (case mode
      (1 (b (term-app-cursor term))) (6 (b (term-origin term))) (7 (b (term-autowrap term)))
      (25 (b (term-cursor-visible term)))
      ((47 1047 1049) (b (term-alt-screen-p term)))
      (1000 (b (eq (term-mouse-mode term) :press-release)))
      (1002 (b (eq (term-mouse-mode term) :button-motion)))
      (1003 (b (eq (term-mouse-mode term) :any-motion)))
      (1004 (b (term-focus-events term)))
      (1006 (b (term-mouse-sgr term)))
      (2004 (b (term-bracketed-paste term)))
      (t 0))))

(defun csi-dispatch (term final prefix inter params)
  (let ((f (code-char final)) (cx (term-cx term)) (cy (term-cy term)))
    (cond
      ;; ---- private (CSI ? …) ----
      ((eql prefix #\?)
       (case f
         (#\h (dolist (g params) (set-dec-mode term (or (first g) 0) t)))
         (#\l (dolist (g params) (set-dec-mode term (or (first g) 0) nil)))
         (#\p (when (equal inter '(36))                   ; DECRQM: CSI ? n $ p
                (let ((m (param0 params 0)))
                  (reply term (format nil "~c[?~d;~d$y" #\Esc m (dec-mode-value term m))))))
         (#\n (when (= (param0 params 0) 6)               ; DECXCPR: CSI ? 6 n
                (reply term (format nil "~c[?~d;~dR" #\Esc (1+ cy) (1+ cx)))))))
      ;; ---- CSI > … : secondary DA, and xterm's key-modifier options (ignored) ----
      ((eql prefix #\>)
       (when (and (char= f #\c) (zerop (param0 params 0)))
         ;; "which terminal, which version?" — answered as a VT220-class terminal
         ;; (1) at a modest version, so programs do not assume features of a recent
         ;; xterm that we do not have
         (reply term (format nil "~c[>1;10;0c" #\Esc))))
      (prefix nil)                                       ; CSI = / CSI < …: ignored
      ;; ---- with an intermediate ----
      (inter
       (cond
         ((and (char= f #\q) (equal inter '(32)))         ; DECSCUSR: CSI n SP q
          (setf (term-cursor-style term) (param0 params 0)))
         ((and (char= f #\p) (equal inter '(33)))         ; DECSTR: CSI ! p (soft reset)
          (setf (term-pen term) 0 (term-insert term) nil (term-origin term) nil
                (term-autowrap term) t (term-app-cursor term) nil (term-app-keypad term) nil
                (term-cursor-visible term) t (term-top term) 0
                (term-bottom term) (1- (term-rows term))
                (term-g0 term) :ascii (term-g1 term) :ascii (term-gl term) 0))
         ((and (char= f #\p) (equal inter '(36)))         ; DECRQM, ANSI modes
          (let ((m (param0 params 0)))
            (reply term (format nil "~c[~d;~d$y" #\Esc m
                                (case m (4 (if (term-insert term) 1 2))
                                        (20 (if (term-newline-mode term) 1 2)) (t 0))))))))
      (t
       (case f
         (#\@ (shift-cells term (param params 0 1) t))                 ; ICH
         (#\A (set-cursor term cx (max (if (>= cy (term-top term)) (term-top term) 0)
                                       (- cy (param params 0 1)))))     ; CUU
         (#\B (set-cursor term cx (min (if (<= cy (term-bottom term)) (term-bottom term)
                                           (1- (term-rows term)))
                                       (+ cy (param params 0 1)))))     ; CUD
         (#\C (set-cursor term (+ cx (param params 0 1)) cy))           ; CUF
         (#\D (set-cursor term (- (min cx (1- (term-cols term))) (param params 0 1)) cy)) ; CUB
         (#\E (set-cursor term 0 (+ cy (param params 0 1))))           ; CNL
         (#\F (set-cursor term 0 (- cy (param params 0 1))))           ; CPL
         ((#\G #\`) (set-cursor term (1- (param params 0 1)) cy))      ; CHA / HPA
         (#\a (set-cursor term (+ cx (param params 0 1)) cy))           ; HPR
         ((#\H #\f)                                                     ; CUP / HVP
          (let ((y (+ (region-top term) (1- (param params 0 1)))))
            (set-cursor term (1- (param params 1 1))
                        (if (term-origin term) (min y (term-bottom term)) y))))
         (#\d (set-cursor term cx (+ (region-top term) (1- (param params 0 1))))) ; VPA
         (#\e (set-cursor term cx (+ cy (param params 0 1))))           ; VPR
         (#\I (dotimes (i (param params 0 1))                          ; CHT
                (set-cursor term (next-tab term (term-cx term)) cy)))
         (#\Z (dotimes (i (param params 0 1))                          ; CBT
                (set-cursor term (prev-tab term (term-cx term)) cy)))
         (#\J (erase-display term (param0 params 0)))                  ; ED
         (#\K (erase-line term (param0 params 0)))                     ; EL
         (#\L (insert-lines term (param params 0 1)))                  ; IL
         (#\M (delete-lines term (param params 0 1)))                  ; DL
         (#\P (shift-cells term (param params 0 1) nil))               ; DCH
         (#\S (scroll-up term (param params 0 1)))                     ; SU
         (#\T (when (<= (length params) 1)                             ; SD (not the old
                (scroll-down term (param params 0 1))))                ;  mouse-tracking T)
         (#\X (clear-cells term (term-line term cy) cx (+ cx (param params 0 1))) ; ECH
          (mark-dirty term cy))
         (#\b (let ((prev (and (plusp cx)                              ; REP
                               (schar (tline-chars (term-line term cy)) (1- cx)))))
                (when prev (dotimes (i (min (param params 0 1) 65535)) (put-char term (char-code prev))))))
         (#\c (when (zerop (param0 params 0))                          ; DA: who are you?
                ;; VT220 (62) with ANSI colour (22): enough for readline, vim, less
                (reply term (format nil "~c[?62;22c" #\Esc))))
         (#\g (case (param0 params 0)                                  ; TBC
                (0 (setf (sbit (term-tabs term) cx) 0))
                (3 (fill (term-tabs term) 0))))
         (#\h (dolist (g params)                                        ; SM (ANSI modes)
                (case (first g) (4 (setf (term-insert term) t)) (20 (setf (term-newline-mode term) t)))))
         (#\l (dolist (g params)                                        ; RM
                (case (first g) (4 (setf (term-insert term) nil)) (20 (setf (term-newline-mode term) nil)))))
         (#\m (sgr term params))
         (#\n (case (param0 params 0)                                  ; DSR
                (5 (reply term (format nil "~c[0n" #\Esc)))            ;   "are you OK?"  yes
                (6 (reply term (format nil "~c[~d;~dR" #\Esc           ;   "where is the cursor?"
                                       (1+ (- cy (region-top term))) (1+ cx))))))
         (#\r (let ((top (1- (param params 0 1)))                      ; DECSTBM
                    (bottom (1- (param params 1 (term-rows term)))))
                (setf bottom (min bottom (1- (term-rows term))))
                (when (< top bottom)
                  (setf (term-top term) top (term-bottom term) bottom)
                  (set-cursor term 0 (region-top term)))))
         (#\s (save-cursor term))                                       ; SCOSC
         (#\u (restore-cursor term))                                    ; SCORC
         (#\t (case (param0 params 0)                                  ; window ops
                (18 (reply term (format nil "~c[8;~d;~dt" #\Esc (term-rows term) (term-cols term))))
                (t nil))))))))                                         ; title stack etc: ignored

;;; ---- SGR: colours and attributes ------------------------------------------

(defun sgr-color (params i)
  "Read an extended colour starting at group I (the one holding 38/48). Returns
   (values COLOR GROUPS-CONSUMED). Both spellings are accepted: the colon form a
   modern program sends (38:2::R:G:B, 38:5:N), with everything in ONE group, and the
   semicolon form older ones send (38;2;R;G;B, 38;5;N), spread over several."
  (let ((g (nth i params)))
    (flet ((rgb (r g b) (logior +truecolor+ (ash (min 255 (or r 0)) 16)
                                (ash (min 255 (or g 0)) 8) (min 255 (or b 0)))))
      (if (cdr g)
          ;; colon form, inside the group: (38 2 [cs] r g b) or (38 5 n)
          (case (second g)
            (5 (values (1+ (min 255 (or (third g) 0))) 1))
            (2 (let ((rest (cddr g)))
                 (when (= (length rest) 4) (pop rest))      ; skip the colour-space id
                 (values (rgb (first rest) (second rest) (third rest)) 1)))
            (t (values nil 1)))
          ;; semicolon form: the next groups carry the parts
          (let ((kind (first (nth (1+ i) params))))
            (case kind
              (5 (values (1+ (min 255 (or (first (nth (+ i 2) params)) 0))) 3))
              (2 (values (rgb (first (nth (+ i 2) params)) (first (nth (+ i 3) params))
                              (first (nth (+ i 4) params)))
                         5))
              (t (values nil 1))))))))

(defun sgr (term params)
  (let ((flags (attr-flags (term-pen term)))
        (fg (attr-fg (term-pen term)))
        (bg (attr-bg (term-pen term)))
        (i 0)
        (n (max 1 (length params))))
    (loop while (< i n) do
      (let ((p (or (first (nth i params)) 0)) (step 1))
        (cond
          ((= p 0) (setf flags 0 fg 0 bg 0))
          ((= p 1) (setf flags (logior flags +bold+)))
          ((= p 2) (setf flags (logior flags +faint+)))
          ((= p 3) (setf flags (logior flags +italic+)))
          ((= p 4) (setf flags (if (eql (second (nth i params)) 0)   ; 4:0 = no underline
                                   (logandc2 flags +underline+)
                                   (logior flags +underline+))))
          ((member p '(5 6)) (setf flags (logior flags +blink+)))
          ((= p 7) (setf flags (logior flags +inverse+)))
          ((= p 8) (setf flags (logior flags +invisible+)))
          ((= p 9) (setf flags (logior flags +strike+)))
          ((= p 21) (setf flags (logior flags +underline+)))
          ((= p 22) (setf flags (logandc2 flags (logior +bold+ +faint+))))
          ((= p 23) (setf flags (logandc2 flags +italic+)))
          ((= p 24) (setf flags (logandc2 flags +underline+)))
          ((= p 25) (setf flags (logandc2 flags +blink+)))
          ((= p 27) (setf flags (logandc2 flags +inverse+)))
          ((= p 28) (setf flags (logandc2 flags +invisible+)))
          ((= p 29) (setf flags (logandc2 flags +strike+)))
          ((<= 30 p 37) (setf fg (1+ (- p 30))))
          ((= p 38) (multiple-value-bind (c used) (sgr-color params i)
                      (when c (setf fg c)) (setf step used)))
          ((= p 39) (setf fg 0))
          ((<= 40 p 47) (setf bg (1+ (- p 40))))
          ((= p 48) (multiple-value-bind (c used) (sgr-color params i)
                      (when c (setf bg c)) (setf step used)))
          ((= p 49) (setf bg 0))
          ((<= 90 p 97) (setf fg (1+ (+ 8 (- p 90)))))
          ((<= 100 p 107) (setf bg (1+ (+ 8 (- p 100))))))
        (incf i step)))
    (setf (term-pen term) (make-attr flags fg bg))))

;;; ---- OSC: strings the program sends us -------------------------------------

(defparameter +osc-52-limit+ (* 1024 1024)
  "Characters of base64 an OSC 52 may carry (about 768 KB of text). Every other OSC
   is a title or a path, and is cut at 4096.")

(defun osc-limit (term)
  "How long the OSC being read may grow: long only once it is known to be a copy."
  (let ((buf (term-string-buf term)))
    (if (and (>= (fill-pointer buf) 3) (string= "52;" buf :end2 3)) +osc-52-limit+ 4096)))

(defun base64-octets (s)
  "The octets of base64 text S. Anything outside the alphabet (line breaks, the
   padding) is skipped rather than refused, as xterm does."
  (let ((out (make-array (floor (* 3 (length s)) 4) :element-type '(unsigned-byte 8)
                                                     :fill-pointer 0))
        (acc 0) (bits 0))
    (loop for ch across s
          for v = (position ch "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
          when v
            do (setf acc (logior (ash (logand acc #xFFFF) 6) v)) (incf bits 6)
               (when (>= bits 8)
                 (decf bits 8)
                 (vector-push (ldb (byte 8 bits) acc) out)))
    out))

(defun osc-52 (term arg)
  "OSC 52 ; TARGETS ; BASE64 — the program puts text on the clipboard. It is how a
   program that took the mouse (Claude Code's full screen, vim with mouse=a, tmux)
   copies what it selected: our own selection never sees those drags. TARGETS are
   xterm's letters: c the clipboard, p PRIMARY, s 'the selection' (empty means s 0).
   The text waits in TERM-COPIES for the pane to take. A QUERY (BASE64 = ?) is not
   answered: it would let any program read whatever you last copied — xterm refuses
   it by default too."
  (let* ((semi (position #\; arg))
         (targets (if semi (subseq arg 0 semi) ""))
         (data (if semi (subseq arg (1+ semi)) "")))
    (unless (string= data "?")
      (let ((text (sb-ext:octets-to-string (coerce (base64-octets data)
                                                   '(simple-array (unsigned-byte 8) (*)))
                                           :external-format '(:utf-8 :replacement #\?))))
        (setf (term-copies term)
              (append (term-copies term) (list (cons (if (string= targets "") "s0" targets)
                                                     text))))))))

(defun term-take-copies (term)
  "The texts the program asked to copy since the last call, oldest first, as
   (TARGETS . STRING) — and forget them."
  (shiftf (term-copies term) '()))

(defun osc-dispatch (term s)
  (let* ((semi (position #\; s))
         (code (ignore-errors (parse-integer s :end (or semi (length s)))))
         (arg (if semi (subseq s (1+ semi)) "")))
    (case code
      ((0 2) (setf (term-title term) (subseq arg 0 (min 200 (length arg)))))   ; window title
      (7 (setf (term-cwd term) arg))                     ; the shell's cwd, as a file:// URL
      (52 (osc-52 term arg))                             ; the program copies
      ;; "what are your colours?" — vim asks for the background (to choose
      ;; 'background=dark') and waits briefly for the answer; giving one is cheap
      ((10 11) (when (string= arg "?")
                 (let ((rgb (if (= code 10) #xD6D3C8 #x1B1E24)))
                   (reply term (format nil "~c]~d;rgb:~(~2,'0x~:*~2,'0x/~2,'0x~:*~2,'0x/~2,'0x~:*~2,'0x~)~c\\"
                                       #\Esc code (ldb (byte 8 16) rgb) (ldb (byte 8 8) rgb)
                                       (ldb (byte 8 0) rgb) #\Esc))))))))

;;; ===========================================================================
;;; Resizing
;;; ===========================================================================

(defun resize-screen (term screen cols rows primary)
  "SCREEN at COLS x ROWS. A primary screen getting SHORTER pushes its top lines into
   the scrollback (so the cursor's line survives), and one getting TALLER pulls them
   back — xterm's behaviour, and the reason a shell's prompt does not vanish when you
   shrink the window."
  (let* ((old (screen-lines screen)) (old-rows (length old))
         (lines (make-array rows)) (shift 0))
    (when (and primary (> old-rows rows) (eq screen (term-screen term)))
      ;; lines above the cursor go to history, as many as needed for the cursor to fit
      (setf shift (max 0 (- (1+ (term-cy term)) rows)))
      (dotimes (i shift) (sb-push term (svref old i))))
    (let ((pulled '()))
      (when (and primary (< old-rows rows) (eq screen (term-screen term)))
        (loop repeat (- rows old-rows)
              for l = (sb-pop term) while l do (push l pulled)))
      (let ((k (length pulled)))
        (loop for l in pulled for i from 0 do (setf (svref lines i) (tline-resize l cols)))
        (dotimes (i (- rows k))
          (let ((src (+ i shift)))
            (setf (svref lines (+ i k))
                  (if (< src old-rows) (tline-resize (svref old src) cols) (make-tline cols)))))
        (when (eq screen (term-screen term))
          (setf (term-cy term) (+ (term-cy term) k (- shift))))))
    (setf (screen-lines screen) lines)))

(defun term-resize (term cols rows)
  "Change the grid to COLS x ROWS. The pane calls this when its size in CELLS changes —
   because the window did, or because the font did — and then tells the program with
   TIOCSWINSZ, which makes the kernel send it SIGWINCH."
  (let ((cols (max 2 cols)) (rows (max 2 rows)))
    (unless (and (= cols (term-cols term)) (= rows (term-rows term)))
      (resize-screen term (term-primary term) cols rows t)
      (resize-screen term (term-alt term) cols rows nil)
      (setf (term-cols term) cols (term-rows term) rows
            (term-top term) 0 (term-bottom term) (1- rows)
            (term-cx term) (min (term-cx term) (1- cols))
            (term-cy term) (max 0 (min (term-cy term) (1- rows)))
            (term-pending-wrap term) nil
            (term-view-offset term) (min (term-view-offset term) (term-sb-count term))
            (term-dirty term) (make-array rows :element-type 'bit :initial-element 1))
      (let ((old (term-tabs term)))
        (reset-tabs term)
        (replace (term-tabs term) old))
      t)))

;;; ===========================================================================
;;; Reading the grid as text (tests, copying a selection)
;;; ===========================================================================

(defun line-text (line &optional (start 0) end)
  "LINE's characters [START, END) as a string, wide tails dropped, trailing blanks
   trimmed."
  (let* ((chars (tline-chars line))
         (end (min (or end (length chars)) (length chars)))
         (s (remove +wide-tail+ (subseq chars (min start end) end))))
    (string-right-trim " " s)))

(defun term-row-text (term y)
  "Screen row Y as a string (trailing blanks trimmed)."
  (line-text (term-line term y)))

(defun term-screen-text (term)
  "The whole screen, one string per row — what the tests compare."
  (loop for y below (term-rows term) collect (term-row-text term y)))

(defun term-text-range (term r0 c0 r1 c1)
  "The text between absolute positions (R0,C0) and (R1,C1), inclusive of the end cell,
   in either order. Rows that WRAPPED join without a newline, so a long command line
   copies back as the one line it was."
  (when (or (> r0 r1) (and (= r0 r1) (> c0 c1)))
    (rotatef r0 r1) (rotatef c0 c1))
  (with-output-to-string (out)
    (loop for r from r0 to r1 do
      (let* ((line (if (term-alt-screen-p term)
                       (term-line term (max 0 (min (1- (term-rows term))
                                                   (- r (term-sb-count term)))))
                       (term-abs-line term r)))
             (from (if (= r r0) c0 0))
             (to (if (= r r1) (1+ c1) (length (tline-chars line)))))
        (write-string (line-text line from to) out)
        (when (and (< r r1) (not (tline-wrapped line)))
          (terpri out))))))

;;; ===========================================================================
;;; Input: keys, the mouse and a paste, encoded as the program expects
;;; ===========================================================================
;;; The browser hands us a key the way it hands one to every pane: a CHARACTER or a
;;; keyword (:up, :page-down, :f5 …) plus a STATE bitmask in the X11 convention
;;; (Shift 1, Ctrl 4, Meta/Alt 8). We turn it back into the bytes xterm would send —
;;; the same ones the terminfo entry above promises the program (kcuu1, kf5, kDC …).

(defun xterm-modifier (state)
  "xterm's modifier parameter: 1 + Shift(1) + Alt(2) + Ctrl(4), or NIL for none."
  (let ((m (logior (if (logbitp 0 state) 1 0) (if (logbitp 3 state) 2 0)
                   (if (logbitp 2 state) 4 0))))
    (and (plusp m) (1+ m))))

(defun ctrl-char (ch)
  "The control byte Ctrl+CH sends, or NIL: Ctrl clears bits 5 and 6 of a key in the
   @..._ column (so C-a is 1 and C-[ is ESC) — the keyboard's oldest arithmetic."
  (let ((c (char-upcase ch)))
    (cond ((char<= #\@ c #\_) (code-char (logand (char-code c) #x1F)))
          ((char= c #\Space) (code-char 0))
          ((char= c #\?) (code-char 127))
          ((char= c #\/) (code-char 31))
          ((char<= #\2 c #\8) (code-char (aref #(0 27 28 29 30 31 127) (- (char-code c) 50))))
          (t nil))))

(defun encode-key (term key state)
  "The string KEY (with modifier STATE) should send to the program, or NIL when it
   sends nothing. Characters are returned as Lisp characters; the pane encodes the
   string as UTF-8 on its way to the pty."
  (let* ((esc (string #\Esc))
         (ctrl (logbitp 2 state)) (meta (logbitp 3 state))
         (mod (xterm-modifier state))
         (meta-prefix (if meta esc "")))
    (flet ((csi (s) (concatenate 'string esc "[" s))
           (ss3 (s) (concatenate 'string esc "O" s)))
      (cond
        ((characterp key)
         (let ((c (if ctrl (or (ctrl-char key) key) key)))
           (concatenate 'string meta-prefix (string c))))
        (t
         (case key
           (:return (concatenate 'string meta-prefix
                                 (if (term-newline-mode term) (format nil "~c~c" #\Return #\Newline)
                                     (string #\Return))))
           (:backspace (concatenate 'string meta-prefix
                                    (string (code-char (if ctrl 8 127)))))  ; kbs=^?
           (:tab (if (logbitp 0 state) (csi "Z") (concatenate 'string meta-prefix (string #\Tab))))
           (:escape esc)
           ((:up :down :right :left :home :end)
            (let ((letter (ecase key (:up "A") (:down "B") (:right "C") (:left "D")
                            (:home "H") (:end "F"))))
              (cond (mod (csi (format nil "1;~d~a" mod letter)))     ; kUP5 = ESC[1;5A
                    ((term-app-cursor term) (ss3 letter))           ; smkx: ESC O A
                    (t (csi letter)))))
           ((:insert :delete :page-up :page-down)
            (let ((n (ecase key (:insert 2) (:delete 3) (:page-up 5) (:page-down 6))))
              (csi (if mod (format nil "~d;~d~~" n mod) (format nil "~d~~" n)))))
           ((:f1 :f2 :f3 :f4)
            (let ((letter (ecase key (:f1 "P") (:f2 "Q") (:f3 "R") (:f4 "S"))))
              (if mod (csi (format nil "1;~d~a" mod letter)) (ss3 letter))))
           ((:f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12)
            (let ((n (ecase key (:f5 15) (:f6 17) (:f7 18) (:f8 19) (:f9 20) (:f10 21)
                       (:f11 23) (:f12 24))))
              (csi (if mod (format nil "~d;~d~~" n mod) (format nil "~d~~" n)))))
           (t nil)))))))

(defun encode-mouse (term event button col row state)
  "The report for a mouse EVENT (:press :release :motion) of BUTTON (0 left, 1 middle,
   2 right, :wheel-up, :wheel-down, or NIL for motion with nothing held) at cell
   COL,ROW (0-based), or NIL when the program did not ask for this kind of event.

   Programs opt in by mode: 9/1000 presses (and releases), 1002 also motion while a
   button is held, 1003 all motion. 1006 changes only the ENCODING, to
   ESC [ < b ; x ; y M/m — decimal numbers, so it works past column 223 and says which
   button was released, two things the original one-byte-per-number form cannot."
  (let ((mode (term-mouse-mode term)))
    (when (or (null mode)
              (and (eq event :release) (eq mode :x10))
              (and (eq event :motion)
                   (not (or (eq mode :any-motion)
                            (and (eq mode :button-motion) (integerp button))))))
      (return-from encode-mouse nil))
    (let* ((b (case button
                (:wheel-up 64) (:wheel-down 65)
                ((0 1 2) button)
                (t 3)))                                   ; motion with no button
           (b (+ b (if (eq event :motion) 32 0)
                 (if (logbitp 0 state) 4 0) (if (logbitp 3 state) 8 0)
                 (if (logbitp 2 state) 16 0)))
           (x (1+ col)) (y (1+ row)))
      (if (term-mouse-sgr term)
          (format nil "~c[<~d;~d;~d~c" #\Esc b x y (if (eq event :release) #\m #\M))
          (let ((b (if (eq event :release) (+ 3 (logand b (lognot 3))) b)))
            (when (and (<= x 223) (<= y 223))
              (format nil "~c[M~c~c~c" #\Esc (code-char (+ 32 b))
                      (code-char (+ 32 x)) (code-char (+ 32 y)))))))))

(defun encode-paste (term text)
  "TEXT as a paste: newlines become CR (what the Return key sends), and with bracketed
   paste (mode 2004) the whole is wrapped in ESC[200~ … ESC[201~ so the program knows
   it was pasted, not typed — a shell then will not run each line as it arrives.
   Any ESC inside is dropped, or a crafted clipboard could close the bracket early."
  (let ((body (remove #\Esc (substitute #\Return #\Newline
                                         (remove #\Return text)))))
    (if (term-bracketed-paste term)
        (format nil "~c[200~~~a~c[201~~" #\Esc body #\Esc)
        body)))

(defun utf8-encode (string)
  "STRING as UTF-8 octets (SB-EXT has one, but this file stays portable CL)."
  (let ((out (make-array (length string) :element-type '(unsigned-byte 8)
                                         :adjustable t :fill-pointer 0)))
    (loop for ch across string
          for c = (char-code ch) do
            (cond ((< c #x80) (vector-push-extend c out))
                  ((< c #x800) (vector-push-extend (logior #xC0 (ash c -6)) out)
                   (vector-push-extend (logior #x80 (logand c #x3F)) out))
                  ((< c #x10000) (vector-push-extend (logior #xE0 (ash c -12)) out)
                   (vector-push-extend (logior #x80 (logand (ash c -6) #x3F)) out)
                   (vector-push-extend (logior #x80 (logand c #x3F)) out))
                  (t (vector-push-extend (logior #xF0 (ash c -18)) out)
                     (vector-push-extend (logior #x80 (logand (ash c -12) #x3F)) out)
                     (vector-push-extend (logior #x80 (logand (ash c -6) #x3F)) out)
                     (vector-push-extend (logior #x80 (logand c #x3F)) out))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))
