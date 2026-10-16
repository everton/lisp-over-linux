;;;; check-terminal.lisp — assert the TERMINAL pane in the booted guest.
;;;;
;;;;     LOL_GUEST_HOSTROOT=1 LOL_PORT=4017 sbcl --script check-terminal.lisp
;;;;
;;;; LOL_GUEST_HOSTROOT=1 shares the build host's / read-only at /mnt/host, so the
;;;; terminal has a real bash (and less) to run. Without it the pane runs the image's
;;;; own Lisp REPL instead, and the shell-specific checks are reported as skipped
;;;; failures. Checks (doc/terminal.org):
;;;;
;;;;   - SuperL-Return (a real held Super over QMP) opens a terminal, its program live;
;;;;   - `echo hi' typed over QMP comes back on the grid as a row "hi";
;;;;   - typing into it repaints ROWS, not frames (DRAW-BROWSER counted while typing);
;;;;   - output past the screen goes to the scrollback; Shift-PageUp (seen only on the
;;;;     keyboard's evdev — the console keymap swallows it) scrolls the view back;
;;;;   - `less' takes the alternate screen and `q' gives it back;
;;;;   - a smaller pane reaches the program: `stty size' after a crumb is added above;
;;;;   - so does a bigger font (SuperL-=, doc/fonts.org): fewer columns, `stty size';
;;;;   - `exit' ends the program and the pane says how.
;;;;
;;;; LOL_SHOTS=DIR keeps screendumps of the shell and of less.
;;;; All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(use-package :lol.guest)

(defun row (label value)
  (format t "~&  ~44a ~a~%" label value)
  (finish-output)
  value)

(defun shot (name)
  (let ((dir (sb-posix:getenv "LOL_SHOTS")))
    (when (and dir (plusp (length dir)))
      (qmp (format nil "{\"execute\":\"screendump\",\"arguments\":{\"filename\":\"~a/~a.ppm\"}}"
                   (string-right-trim "/" dir) name)))))

(defparameter +term+ "(term-session-term (pane-session (current-pane)))"
  "A guest form for the current terminal's engine.")

(defun term-form (fmt) (format nil fmt +term+))

(defun screen-has (text)
  "Does some row of the current terminal read exactly TEXT (trailing blanks trimmed)?"
  (string= "T" (probe (format nil "(and (find ~s (lol.terminal:term-screen-text ~a) :test #'string=) t)"
                              text +term+))))

(defun screen-find (prefix)
  "The first row of the current terminal that starts with PREFIX, or \"?\"."
  (probe (format nil "(find-if (lambda (l) (eql 0 (search ~s l))) (lol.terminal:term-screen-text ~a))"
                 prefix +term+)))

(defun wait-until (form-string &optional (seconds 10))
  "Poll FORM-STRING in the guest until it prints T."
  (loop repeat (* 2 seconds)
        when (string= "T" (probe form-string)) do (return t)
        do (sleep 0.5)))

(defun checks ()
  (wait-for-browser)
  (quiet-kernel-console)
  (let ((host (row "host bash at /mnt/host"
                   (probe "(and (probe-file \"/mnt/host/usr/bin/bash\") t)"))))
    (unless (string= host "T")
      (format t "~&  (no host root: LOL_GUEST_HOSTROOT=1 is needed for the shell checks)~%"))
    (format t "~&~%== SuperL-Return opens a terminal ==~%")
    (qmp-hold "meta_l" "ret") (sleep 3)
    (check "SuperL-Return opens a terminal pane"
           (string= "T" (row "current pane is a terminal" (probe "(terminal-pane-p (current-pane))"))))
    (check "its program is running"
           (string= "T" (row "session live" (probe "(and (session-live-p (pane-session (current-pane))) t)"))))
    (row "what it runs" (probe "(subj-terminal-title (pane-subject (current-pane)))"))
    (row "its size" (probe (term-form "(list (lol.terminal:term-cols ~a) (lol.terminal:term-rows ~:*~a))")))
    (check "the login shell wrote to the grid"
           (wait-until (term-form "(plusp (length (lol.terminal:term-row-text ~a 0)))")))
    (row "its first line" (probe (term-form "(lol.terminal:term-row-text ~a 0)")))
    (check "/dev/fd exists (bash's <(...) needs it; preinit makes it)"
           (string= "\"/proc/self/fd\"" (row "readlink /dev/fd" (probe "(sb-unix:unix-readlink \"/dev/fd\")"))))
    ;; A LOGIN shell runs the user's dotfiles, and those may stop to ask something (an
    ;; ssh-agent passphrase, on the build host). So the typing checks below use a
    ;; shell started the same way but without dotfiles; this one is closed with
    ;; SuperL-w, which must hang its program up.
    (let ((before (probe-int "(length *terminals*)")))
      (qmp-hold "meta_l" "w") (sleep 2)
      (check "SuperL-w closes the tab and hangs the program up"
             (eql (row "live terminals after SuperL-w" (probe-int "(length *terminals*)"))
                  (and before (1- before)))))
    (probe "(progn (open-terminal (append (remove \"-l\" (default-terminal-command) :test #'string=) (list \"--norc\" \"--noprofile\")) :title \"check\") t)")
    (sleep 3)
    (wait-until (term-form "(plusp (length (lol.terminal:term-row-text ~a 0)))"))

    (format t "~&~%== typing: echo hi ==~%")
    ;; count whole draws of the terminal's pane while typing: a key into a terminal
    ;; should repaint rows only (painted straight in, not drawn by the pane morph)
    (probe "(progn (defvar *frames* 0) (setf *frames* (lol.morph:morph-draws (current-pane))) t)")
    (qmp-type "echo hi") (qmp-key "ret") (sleep 2)
    (let ((frames (row "whole draws of the terminal while typing 8 keys"
                       (probe-int "(- (lol.morph:morph-draws (current-pane)) *frames*)"))))
      (check "typing repaints rows, not frames (fewer than 3 frames for 8 keys)"
             (and frames (< frames 3))))
    (check "the grid shows the row \"hi\"" (row "a row reads hi" (screen-has "hi")))
    (shot "terminal-shell")

    (format t "~&~%== the scrollback, and Shift-PageUp from the evdev ==~%")
    (qmp-type "seq 1 200") (qmp-key "ret") (sleep 3)
    (let ((n (row "scrollback lines" (probe-int (term-form "(lol.terminal:term-scrollback-count ~a)")))))
      (check "200 lines of output reached the scrollback" (and n (> n 100))))
    (qmp-hold "shift" "pgup") (sleep 1.5)
    (let ((off (row "view offset after S-PgUp" (probe-int (term-form "(lol.terminal:term-view-offset ~a)")))))
      (check "Shift-PageUp scrolls back the history" (and off (plusp off))))
    (qmp-key "spc") (qmp-key "backspace") (sleep 1)      ; a key returns to the live view
    (check "typing returns to the live view"
           (eql 0 (row "view offset after a key" (probe-int (term-form "(lol.terminal:term-view-offset ~a)")))))

    (format t "~&~%== a full-screen program: less ==~%")
    (qmp-type "less /proc/cpuinfo") (qmp-key "ret") (sleep 3)
    (check "less takes the alternate screen"
           (string= "T" (row "alternate screen" (probe (term-form "(lol.terminal:term-alt-screen-p ~a)")))))
    (row "less shows" (screen-find "processor"))
    (shot "terminal-less")
    (qmp-key "q") (sleep 2)
    (check "q gives the primary screen back"
           (string= "NIL" (row "alternate screen after q" (probe (term-form "(lol.terminal:term-alt-screen-p ~a)")))))

    (format t "~&~%== a smaller pane reaches the program (TIOCSWINSZ) ==~%")
    (let ((before (row "grid rows before" (probe-int (term-form "(lol.terminal:term-rows ~a)")))))
      ;; a breadcrumb above the terminal takes a bar's height from it: the same path a
      ;; window or font change takes (TERMINAL-FIT, at the next full frame)
      (probe "(progn (set-current-trail (cons (open-pane (subj-workspace)) (current-trail))) t)")
      (qmp-hold "meta_l" "tab") (sleep 1) (qmp-hold "meta_l" "tab") (sleep 1.5) ; two full frames
      (let ((after (row "grid rows after" (probe-int (term-form "(lol.terminal:term-rows ~a)"))))
            (cols (probe-int (term-form "(lol.terminal:term-cols ~a)"))))
        (check "the grid lost a row or more" (and before after (< after before)))
        (qmp-type "stty size") (qmp-key "ret") (sleep 2)
        (check "stty size in the shell agrees with the grid"
               (row "stty says" (screen-has (format nil "~d ~d" after cols))))))

    (format t "~&~%== a bigger font reaches the program (SuperL-=) ==~%")
    (let ((cols0 (row "grid cols before" (probe-int (term-form "(lol.terminal:term-cols ~a)")))))
      (qmp-hold "meta_l" "equal") (sleep 2)
      (let ((cols1 (row "grid cols after SuperL-=" (probe-int (term-form "(lol.terminal:term-cols ~a)"))))
            (rows1 (probe-int (term-form "(lol.terminal:term-rows ~a)"))))
        (check "a bigger font means fewer columns" (and cols0 cols1 (< cols1 cols0)))
        (qmp-type "stty size") (qmp-key "ret") (sleep 2)
        (check "stty size follows the font (TIOCSWINSZ)"
               (row "stty says" (screen-has (format nil "~d ~d" rows1 cols1)))))
      (qmp-hold "meta_l" "0") (sleep 1.5))
    (format t "~&~%== exit ==~%")
    (qmp-type "exit") (qmp-key "ret") (sleep 2)
    (check "exit ends the program, and the pane says so"
           (search "exited" (row "message" (probe "(term-session-message (pane-session (current-pane)))"))))
    (qmp-key "ret") (sleep 1.5)
    (check "Return on an ended terminal closes it"
           (string= "NIL" (row "still a terminal?" (probe "(terminal-pane-p (current-pane))"))))))

(run-guest-checks "terminal" #'checks)
