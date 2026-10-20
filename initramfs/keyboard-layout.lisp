;;;; keyboard-layout.lisp — Portuguese (Brazil) on the console, the way the host's X does it.
;;;;
;;;; On the laptop's Debian, X has two layouts and two ways to reach the second
;;;; (/etc/default/keyboard, read 2026-10-19):
;;;;
;;;;   XKBLAYOUT="us,br"
;;;;   XKBOPTIONS="…,grp:switch,grp:caps_toggle,grp_led:caps"
;;;;
;;;; grp:switch: Right Alt held = the second layout while it is down; grp:caps_toggle:
;;;; Caps Lock switches to it and back (and is no longer a caps lock); grp_led:caps: the
;;;; Caps Lock light says which. The environment's text comes from the Linux console,
;;;; whose keyboard is translated by the KERNEL's keymap — a table per modifier
;;;; combination, keycode -> keysym (drivers/tty/vt/keyboard.c), set entry by entry with
;;;; the KDSKBENT ioctl (CONSOLE-KBENT, fb-browser.lisp; PID 1 keeps the
;;;; CAP_SYS_TTY_CONFIG it needs). That keymap already has what X's options need:
;;;;
;;;;   - Right Alt is the console's AltGr (defkeymap: keycode 100 = AltGr), a modifier
;;;;     with tables of its own (2 = AltGr, 3 = AltGr+Shift): fill those with pt-BR and
;;;;     holding Right Alt types pt-BR — grp:switch;
;;;;   - dead keys (KT_DEAD: ´ ` ~ ^ ¨) and a composition table that already holds
;;;;     Portuguese (defkeymap's accent_table: ' a -> á, ~ a -> ã, , c -> ç, …);
;;;;   - Unicode mode (vt.c's default_utf8): ç reaches us as UTF-8 (FB-READ-KEY).
;;;;
;;;; Caps Lock is done here rather than by the kernel's AltGr_Lock, because the kernel
;;;; has no ioctl that READS a lock's state, and the light and the tray must say the
;;;; truth: the key is made a VoidSymbol in the keymap, its press is seen on the
;;;; keyboard's evdev (NOTE-MODIFIER-KEY, :LAYOUT-TOGGLE), and TOGGLE-LAYOUT swaps the
;;;; plain tables to pt-BR and the AltGr tables to US — so Right Alt held while pt-BR
;;;; is on gives US back, as X does.
;;;;
;;;; The layout is the host's XKB "br" (= "abnt2"), read from
;;;; /usr/share/X11/xkb/symbols/br on 2026-10-19: "latin" plus the overrides below, on
;;;; the keys of the laptop's US keyboard (evdev keycode = XKB keycode - 8). Only levels
;;;; 1 and 2: grp:switch makes Right Alt a group switch, so br's levels 3-4 are out of
;;;; reach in the host's X as well. Nothing here runs unless the host's
;;;; /etc/default/keyboard lists "br" among its layouts.
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

(defparameter *br-overrides*
  ;; keycode  XKB key   plain              shifted             (US key cap)
  '((41  "TLDE" #\'                #\")                ; `  ~
    (7   "AE06" #\6                :dead-diaeresis)    ; 6  ^
    (26  "AD11" :dead-acute        :dead-grave)        ; [  {
    (27  "AD12" #\[                #\{)                ; ]  }
    (43  "BKSL" #\]                #\})                ; \  |
    (39  "AC10" #\LATIN_SMALL_LETTER_C_WITH_CEDILLA #\LATIN_CAPITAL_LETTER_C_WITH_CEDILLA) ; ;  :
    (40  "AC11" :dead-tilde        :dead-circumflex)   ; '  "
    (53  "AB10" #\;                #\:)                ; /  ?
    (86  "LSGT" #\\                #\|)                ; the 102nd key (ISO/ABNT keyboards)
    (89  "AB11" #\/                #\?))               ; the ABNT2 key beside Right Shift
  "pt-BR's keys that differ from US: (keycode xkb-name plain shifted).")

(defparameter +dead-keys+
  '(:dead-grave 0 :dead-acute 1 :dead-circumflex 2 :dead-tilde 3 :dead-diaeresis 4)
  "linux/keyboard.h: K_DGRAVE = K(KT_DEAD,0) … K_DDIERE = K(KT_DEAD,4).")

(defun keysym-value (sym)
  "SYM as the value KDSKBENT takes. A dead key is K(KT_DEAD, n) = #x400 + n. A
   character past ASCII is Unicode, and the kernel stores entries XORed with #xF000
   (keyboard.c's U()), keeping as Unicode whatever then has a type byte under #xF0 — so
   we pass the code point XOR #xF000 (ç, U+00E7 -> #xF0E7). An ASCII letter is
   K(KT_LETTER, c), so Caps Lock could act on it; other ASCII is K(KT_LATIN, c) = c."
  (cond ((keywordp sym) (+ #x400 (getf +dead-keys+ sym)))
        ((> (char-code sym) 127) (logxor (char-code sym) #xF000))
        ((alpha-char-p sym) (+ #xB00 (char-code sym)))
        (t (char-code sym))))

(defconstant +caps-lock-keycode+ 58)
(defconstant +kdsetled+ #x4B32 "KDSETLED: set the lights (an int: scroll 1, num 2, caps 4).")

(defvar *us-tables* nil
  "The console's own plain and Shift tables, read once per boot before we change them:
   (plain-vector shift-vector), keycode -> value. PID 1 lives across the environment's
   restarts, so a second start must not read our pt-BR back as the original.")
(defvar *layout* :us "The layout the plain keys type now: :US or :BR.")
(defvar *layouts-enabled* nil "Did the host's keyboard settings ask for br (and did we set it up)?")

(defun host-keyboard-settings ()
  "XKBLAYOUT and XKBOPTIONS from the host's /etc/default/keyboard, as (layouts options),
   each a list of strings; NIL when there is no such file."
  (with-open-file (s (host-path "/etc/default/keyboard") :if-does-not-exist nil)
    (when s
      (let (layouts options)
        (flet ((value (line key)
                 (let ((p (search key line)))
                   (when (eql p 0)
                     (string-trim "\"' " (subseq line (length key)))))))
          (loop for line = (read-line s nil) while line
                do (let ((l (value line "XKBLAYOUT=")) (o (value line "XKBOPTIONS=")))
                     (when l (setf layouts (split-commas l)))
                     (when o (setf options (split-commas o))))))
        (list layouts options)))))

(defun split-commas (s)
  (loop for start = 0 then (1+ end)
        for end = (or (position #\, s :start start) (length s))
        for part = (string-trim " " (subseq s start end))
        when (plusp (length part)) collect part
        while (< end (length s))))

(defun read-us-tables ()
  (or *us-tables*
      (setf *us-tables*
            (loop for table in '(0 1)
                  collect (let ((v (make-array 128 :initial-element nil)))
                            (dotimes (k 128 v) (setf (aref v k) (console-kbent table k))))))))

(defun br-value (keycode shifted)
  "pt-BR's value for KEYCODE: the override, else what US has there."
  (let ((o (assoc keycode *br-overrides*)))
    (if o
        (keysym-value (if shifted (fourth o) (third o)))
        (aref (nth (if shifted 1 0) (read-us-tables)) keycode))))

(defun us-value (keycode shifted) (aref (nth (if shifted 1 0) (read-us-tables)) keycode))

(defun write-layer (tables value-fn)
  "Write (VALUE-FN keycode shifted) into TABLES (plain . shifted), every keycode."
  (dotimes (k 128)
    (let ((p (funcall value-fn k nil)) (s (funcall value-fn k t)))
      (when p (console-kbent (car tables) k p))
      (when s (console-kbent (cdr tables) k s)))))

(defun set-caps-led (on)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "ioctl" (function sb-alien:int sb-alien:int sb-alien:unsigned-long sb-alien:unsigned-long))
   0 +kdsetled+ (if on 4 0)))

(defun apply-layout (layout)
  "Make the plain keys type LAYOUT and Right Alt (AltGr) type the other one. Only the
   keys that differ are rewritten in the plain tables; the AltGr tables are written
   whole (the first time they hold nothing of ours)."
  (let ((plain (if (eq layout :br) #'br-value #'us-value))
        (other (if (eq layout :br) #'us-value #'br-value)))
    (dolist (o *br-overrides*)
      (let ((k (first o)))
        (console-kbent 0 k (funcall plain k nil))
        (console-kbent 1 k (funcall plain k t))))
    (write-layer '(2 . 3) other)
    ;; Caps Lock: nothing of the kernel's (no caps lock, no AltGr lock) in any table —
    ;; its press is ours, on the evdev
    (dolist (table '(0 1 2 3)) (console-kbent table +caps-lock-keycode+ +k-hole+))
    (setf *layout* layout)
    (ignore-errors (set-caps-led (eq layout :br)))
    layout))

(defun setup-keyboard-layouts ()
  "At the environment's start: if the host's keyboard settings list br, take the US
   tables as they are, put pt-BR under Right Alt and Caps Lock in our hands.
   -> T when set up. Logged."
  (let* ((settings (ignore-errors (host-keyboard-settings)))
         (layouts (first settings)) (options (second settings)))
    (when (and (member "br" layouts :test #'string=) (read-us-tables)
               (aref (first *us-tables*) 30))      ; KDGKBENT answered: we are on a console
      (apply-layout *layout*)
      (setf *layouts-enabled* t)
      (when (fboundp 'session-log)
        (funcall 'session-log "keyboard: layouts ~{~a~^,~} (options ~{~a~^,~}) — Right Alt held = the other, Caps Lock toggles"
                 layouts options))
      t)))

(defun toggle-layout ()
  "Caps Lock: switch the plain keys between US and pt-BR. -> a message."
  (if (not *layouts-enabled*)
      "Caps Lock"
      (progn (apply-layout (if (eq *layout* :br) :us :br))
             (forget-layout-tray)
             (format nil "keyboard: ~a (Caps Lock switches; Right Alt held types the other)"
                     (if (eq *layout* :br) "pt-BR" "US")))))

(defvar *layout-tray-cache* nil)
(defun forget-layout-tray () (setf *layout-tray-cache* nil))
(defun layout-tray-item ()
  "The tray's BR while pt-BR is the plain layout (X's Caps Lock light, said in words)."
  (when (and *layouts-enabled* (eq *layout* :br))
    (list :kind :layout :text "BR")))
