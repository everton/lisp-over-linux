;;;; check-org.lisp — Org files in the booted guest: the keys, from a real keyboard.
;;;;
;;;;     sbcl --script check-org.lisp        ; exits 0 if all assertions pass
;;;;
;;;; What Org DOES is host-tested against Emacs (poc/org-test.lisp, the pure part;
;;;; poc/org-mode-test.lisp, the shell). What only the guest can show is how its KEYS
;;;; arrive on the bare console, and three of them needed work (doc/org.org §6.2):
;;;;
;;;;   - S-TAB: the console sends a plain Tab; the Shift comes from the keyboard's evdev
;;;;   - M-RET: alt_map sends ESC CR, which the tty decoder used to drop
;;;;   - M-arrows: kernel 7.2's console sends a cursor key with its modifiers xterm's
;;;;     way (ESC [ 1 ; 3 A) — but its keymap SWITCHES VTs on Alt-Left / Alt-Right;
;;;;     UNBIND-CONSOLE-ALT-ARROWS binds them to the cursor keys, and then they arrive
;;;;     like the rest
;;;;
;;;; Driven by QMP key events (a held modifier is pressed for real, so the evdev sees
;;;; it — qmp-hold); the guest's state is read back over the net REPL. The Org file is
;;;; written by the net REPL into /tmp, so no disk is needed.
;;;;
;;;;   A. the file opens as Org; S-TAB folds to OVERVIEW and back
;;;;   B. TAB on a headline cycles; C-c C-n moves by heading
;;;;   C. M-RET makes a heading; M-<down> / M-<up> move a subtree; M-S-<right> demotes it
;;;;   D. Alt-Left / Alt-Right promote and demote — and no longer switch the VT
;;;;   E. C-c C-o follows a link; the outline subject opens and drills

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))

(in-package :lol.guest)

(defun qmp-chord (modifier qcode)
  (qmp (format nil "{\"execute\":\"send-key\",\"arguments\":{\"keys\":[~
                    {\"type\":\"qcode\",\"data\":\"~a\"},{\"type\":\"qcode\",\"data\":\"~a\"}]}}"
               modifier qcode))
  (sleep 0.6))

(defun qmp-hold2 (m1 m2 qcode)
  "QCODE with two modifiers genuinely held (Alt-Shift-Right)."
  (qmp-events (key-event t m1)) (sleep 0.1)
  (qmp-events (key-event t m2)) (sleep 0.2)
  (qmp-events (key-event t qcode)) (sleep 0.1)
  (qmp-events (key-event nil qcode)) (sleep 0.2)
  (qmp-events (key-event nil m2)) (sleep 0.1)
  (qmp-events (key-event nil m1)) (sleep 0.6))

(defun held (modifier qcode) (qmp-hold modifier qcode) (sleep 0.6))

(defun yes (form) (string= (probe form) "T"))
(defun lines-are (from &rest want)
  (yes (format nil "(equal (coerce (subseq (lol.textview:tv-lines (pane-tv (current-pane))) ~d ~d) 'list) '~s)"
               from (+ from (length want)) want)))
(defun hidden () (probe "(let ((b (lol.textview:tv-hidden (pane-tv (current-pane))))) (and b (loop for i below (length b) when (= 1 (sbit b i)) collect i)))"))
(defun point-line () (probe-int "(lol.textview:tv-point-line (pane-tv (current-pane)))"))
(defun goto (line) (probe (format nil "(progn (lol.textview:tv-move-point (pane-tv (current-pane)) ~d 0) t)" line)) (sleep 0.3))
(defun status () (probe "*browser-status*"))

(defparameter +fixture+
  ;; poc/org-test.lisp's folding fixture (Emacs's answers apply) without the blank
  ;; lines before C — they would bring Org's blank-line rules into the M-RET checks,
  ;; which are host-tested — and with a link to C
  '("#+TITLE: fx" "preamble" "* A" "a body" "** A1" "a1 body" "" "** A2" "a2 body"
    "#+begin_src lisp" "* not-a-heading?" "#+end_src" "* B :tag:" "b body [[*C][to C]]" "* C"))

(defun checks ()
  (quiet-kernel-console)
  (wait-for-browser)
  (probe (format nil "(with-open-file (s \"/tmp/fx.org\" :direction :output :if-exists :supersede) (dolist (l '~s) (write-line l s)) t)"
                 +fixture+))
  (probe "(progn (new-trail (subj-file \"/tmp/fx.org\")) t)")
  (sleep 1.5)

  (format t "~&~%== A. the file, S-TAB ==~%")
  (check "the file opens as an Org pane" (yes "(and (org-pane-p (current-pane)) t)"))
  (held "shift" "tab")
  (check "S-TAB (Tab + the evdev's Shift): OVERVIEW, the lines Emacs hides"
         (string= (hidden) "(3 4 5 6 7 8 9 11 13)"))
  (check "the status line says OVERVIEW" (string= (status) "\"OVERVIEW\""))
  (held "shift" "tab") (held "shift" "tab")
  (check "twice more: CONTENTS, then SHOW ALL" (string= (hidden) "NIL"))

  (format t "~&~%== B. TAB, heading motion ==~%")
  (goto 2)
  (qmp-key "tab") (sleep 0.6)
  (check "TAB on A: FOLDED" (string= (hidden) "(3 4 5 6 7 8 9)"))
  (qmp-key "tab") (sleep 0.6)
  (check "TAB again: CHILDREN" (string= (hidden) "(5 8 9)"))
  (qmp-key "tab") (sleep 0.6)
  (check "TAB again: SUBTREE" (string= (hidden) "NIL"))
  (goto 3)
  (qmp-chord "ctrl" "c") (qmp-chord "ctrl" "n")
  (check "C-c C-n from a body line: the next heading (Emacs 4 -> 5)" (eql (point-line) 4))
  (qmp-chord "ctrl" "c") (qmp-chord "ctrl" "u")
  (check "C-c C-u: its parent" (eql (point-line) 2))

  (format t "~&~%== C. M-RET and the M-arrows ==~%")
  (goto 14) (qmp-chord "ctrl" "e")
  (held "alt" "ret")
  (check "M-RET (ESC CR from the console) at the end of C: a new heading"
         (and (lines-are 14 "* C" "* ") (eql (point-line) 15)))
  (qmp-type "d") (sleep 0.4)
  (check "typed into its title" (lines-are 15 "* d"))
  ;; d's subtree carries the file's last (empty) line with it, as Emacs moves a
  ;; subtree with its trailing blank lines
  (held "alt" "up")
  (check "M-<up> (ESC [1;3A): d moved above C"
         (and (lines-are 14 "* d" "" "* C") (eql (point-line) 14)))
  (held "alt" "down")
  (check "M-<down>: back below C" (lines-are 14 "* C" "* d"))
  (qmp-hold2 "alt" "shift" "right")
  (check "M-S-<right> (no Shift+Alt table: the kernel falls back, ESC [1;4C): d demoted ONCE"
         (lines-are 15 "** d"))

  (format t "~&~%== D. Alt-Left / Alt-Right ==~%")
  (let ((vt (probe "(with-open-file (s \"/sys/class/tty/tty0/active\") (read-line s))")))
    (format t "~&  active VT before: ~a~%" vt)
    (held "alt" "left")
    (check "Alt-Left promotes d (it used to be the console's previous-VT key)" (lines-are 15 "* d"))
    (check "and the VT did not switch"
           (string= (probe "(with-open-file (s \"/sys/class/tty/tty0/active\") (read-line s))") vt))
    (held "alt" "right")
    (check "Alt-Right demotes it again" (lines-are 15 "** d"))
    (check "the VT is still the environment's"
           (string= (probe "(with-open-file (s \"/sys/class/tty/tty0/active\") (read-line s))") vt)))
  (check "the console's Alt-Left / Alt-Right are the cursor keys now, K_LEFT / K_RIGHT (KDGKBENT)"
         (yes "(equal (list (console-kbent 8 105) (console-kbent 8 106)) '(#x0601 #x0602))"))

  (format t "~&~%== E. links and the outline ==~%")
  (goto 13) (qmp-chord "ctrl" "e") (qmp-key "left") (qmp-key "left") (sleep 0.3)
  (qmp-chord "ctrl" "c") (qmp-chord "ctrl" "o")
  (check "C-c C-o on [[*C][to C]]: the caret on C" (eql (point-line) 14))
  (probe "(progn (new-trail (subj-org \"/tmp/fx.org\")) t)")
  (sleep 1.0)
  ;; the outline is of the file ON DISK, which these edits never saved
  (check "the outline subject lists the top headings (of the file on disk)"
         (yes "(equal (mapcar (function item-label) (view-items (pane-view (current-pane)))) '(\"Edit the file\" \"A\" \"not-a-heading?\" \"B\" \"C\"))"))
  (qmp-key "down") (qmp-key "ret") (sleep 1.0)
  (check "a heading drills: Edit here, then its children"
         (yes "(equal (mapcar (function item-label) (view-items (pane-view (current-pane)))) '(\"Edit here\" \"A1\" \"A2\"))"))
  t)

(run-guest-checks "check-org" #'checks)
