;;;; check-keyboard.lisp — the host's layouts (us,br) on the console, in the booted guest.
;;;;
;;;;     LOL_GUEST_HOSTROOT=1 LOL_PORT=4022 sbcl --script check-keyboard.lisp
;;;;
;;;; keyboard-layout.lisp reads the HOST's /etc/default/keyboard (LOL_GUEST_HOSTROOT=1
;;;; shares the build host's / — the laptop's own Debian, XKBLAYOUT="us,br"), puts pt-BR
;;;; in the console keymap's AltGr tables (Right Alt held) and lets Caps Lock swap the
;;;; layouts. Typed by QMP, read back from the Workspace's text and a terminal's grid:
;;;;
;;;;   - set up: enabled, Right Alt is the console's AltGr (keycode 100 = K_ALTGR);
;;;;   - Right Alt + ;  -> ç  (a character past ASCII: UTF-8 from the console, decoded);
;;;;   - Right Alt + '  then a -> ã   (dead tilde; the kernel's composition table)
;;;;   - Right Alt + [  then e -> é   (dead acute)
;;;;   - Right Alt + Shift + ' then o -> ô   (dead circumflex)
;;;;   - Caps Lock: pt-BR on the plain keys — ; -> ç, the tray says BR, the Caps light is
;;;;     on, and Right Alt held gives US back (; -> ;); Caps Lock again: US, ; -> ;
;;;;   - a terminal: ç typed reaches the program (echo prints it back).
;;;;
;;;; The net REPL's answers are read as bytes, so text is compared by CHAR-CODE.
;;;; All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(in-package :lol.guest)

(defun row (label value) (format t "~&  ~44a ~a~%" label value) (finish-output) value)

(defun ralt (qcode) (qmp-hold "alt_r" qcode) (sleep 0.3))

(defun ws-tail (n)
  "The char codes of the Workspace's last N characters."
  (probe (format nil "(let ((s (lol.textview:tv-text (pane-tv (current-pane))))) (map 'list #'char-code (subseq s (max 0 (- (length s) ~d)))))" n)))

(defun fresh-line-in-workspace ()
  (probe "(let ((tv (pane-tv (current-pane)))) (lol.textview:tv-key tv :end 4) (ws-insert tv (string #\\Newline)) t)"))

(defun checks ()
  (wait-for-browser)
  (quiet-kernel-console)
  (format t "~&~%== set up from the host's /etc/default/keyboard ==~%")
  (check "the host lists br, so the layouts are on" (string= "T" (row "*layouts-enabled*" (probe "*layouts-enabled*"))))
  (row "the host's settings" (probe "(host-keyboard-settings)"))
  (check "Right Alt (keycode 100) is the console's AltGr" (string= "1793" (row "plain[100]" (probe "(console-kbent 0 100)"))))
  (format t "~&~%== Right Alt held: pt-BR ==~%")
  (fresh-line-in-workspace)
  (ralt "semicolon")
  (check "Right Alt + ; types ç (U+00E7)" (string= "(231)" (row "last char" (ws-tail 1))))
  (ralt "apostrophe") (qmp-key "a") (sleep 0.3)
  (check "Right Alt + ' then a: ã (U+00E3), the dead tilde" (string= "(227)" (row "last char" (ws-tail 1))))
  (ralt "bracket_left") (qmp-key "e") (sleep 0.3)
  (check "Right Alt + [ then e: é (U+00E9), the dead acute" (string= "(233)" (row "last char" (ws-tail 1))))
  (qmp-events (key-event t "alt_r")) (sleep 0.15) (qmp-events (key-event t "shift")) (sleep 0.15)
  (qmp-key "apostrophe") (sleep 0.15)
  (qmp-events (key-event nil "shift")) (qmp-events (key-event nil "alt_r")) (sleep 0.2)
  (qmp-key "o") (sleep 0.3)
  (check "Right Alt + Shift + ' then o: ô (U+00F4), the dead circumflex" (string= "(244)" (row "last char" (ws-tail 1))))
  (qmp-key "semicolon") (sleep 0.3)
  (check "without Right Alt, ; is ;" (string= "(59)" (row "last char" (ws-tail 1))))
  (format t "~&~%== Caps Lock: pt-BR on the plain keys ==~%")
  (qmp-key "caps_lock") (sleep 0.6)
  (check "Caps Lock switches to pt-BR" (string= ":BR" (row "*layout*" (probe "*layout*"))))
  (check "the tray says BR" (string= "\"BR\"" (row "tray" (probe "(getf (layout-tray-item) :text)"))))
  (check "the Caps light is on (KDGETLED = 4)"
         (string= "4" (row "leds" (probe "(sb-alien:with-alien ((c sb-alien:unsigned-char 0)) (sb-alien:alien-funcall (sb-alien:extern-alien \"ioctl\" (function sb-alien:int sb-alien:int sb-alien:unsigned-long (* sb-alien:unsigned-char))) 0 #x4B31 (sb-alien:addr c)) c)"))))
  (qmp-key "semicolon") (sleep 0.3)
  (check "now ; types ç" (string= "(231)" (row "last char" (ws-tail 1))))
  (qmp-key "apostrophe") (qmp-key "a") (sleep 0.3)
  (check "and ' then a types ã" (string= "(227)" (row "last char" (ws-tail 1))))
  (ralt "semicolon")
  (check "Right Alt held gives US back: ;" (string= "(59)" (row "last char" (ws-tail 1))))
  (qmp-key "caps_lock") (sleep 0.6)
  (check "Caps Lock again: US" (string= ":US" (row "*layout*" (probe "*layout*"))))
  (qmp-key "semicolon") (sleep 0.3)
  (check "and ; is ; again" (string= "(59)" (row "last char" (ws-tail 1))))
  (check "the tray says nothing" (string= "NIL" (probe "(layout-tray-item)")))
  (format t "~&~%== a terminal gets ç ==~%")
  (probe "(progn (open-terminal (append (remove \"-l\" (default-terminal-command) :test #'string=) (list \"--norc\" \"--noprofile\")) :title \"kb\") t)")
  (sleep 4)
  (qmp-type "echo x") (ralt "semicolon") (qmp-type "x") (qmp-key "ret") (sleep 1.5)
  (check "echo prints xçx back (the pty got UTF-8)"
         (string= "T" (row "a row reads x ç x"
                           (probe "(and (find (coerce (list #\\x (code-char 231) #\\x) 'string) (lol.terminal:term-screen-text (term-session-term (pane-session (current-pane)))) :test #'string=) t)")))))

(run-guest-checks "keyboard" #'checks)
