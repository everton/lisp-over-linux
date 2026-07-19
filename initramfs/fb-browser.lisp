;;;; fb-browser.lisp — run the code browser on /dev/fb0 (the guest; no X).
;;;;
;;;; The same shell (poc/shell.lisp) that drives the browser in an X11 window on
;;;; the dev host runs here on the bare framebuffer, launched from the supervisor
;;;; menu. Output is lol.fb:present-fb into /dev/fb0 (under KD_GRAPHICS so fbcon
;;;; stops scribbling); input is the raw console keyboard — the same with-raw-mode
;;;; termios + read-escape the line editor already uses — mapped to the shell's
;;;; keys. It returns to the menu when you leave (C-g at the root, or Esc).
;;;;
;;;; No mouse yet (evdev is not read here), and no Super over the console — so
;;;; navigation is arrows / Enter / Backspace, C-g = back, C-c C-c = Accept.
;;;; CL-USER, loaded after the toolkit (canvas/textview/shell/fb) and line-editor.

(in-package :cl-user)

(defun fb-read-key (in)
  "Read one keystroke from raw-mode IN and return (values KEY STATE) shaped for
   BROWSER-KEY. printable -> the char, state 0; a control byte -> its letter with
   the Ctrl bit set (so C-c C-c, C-g etc. work); ESC[… -> an arrow/navigation
   keyword via READ-ESCAPE; a lone ESC -> :escape (detected with LISTEN so it does
   not block waiting for a sequence that never comes)."
  (let ((c (read-char in nil :eof)))
    (if (eq c :eof)
        (values :eof 0)
        (let ((code (char-code c)))
          (cond
            ((= code 27) (if (listen in)
                             (values (read-escape in) 0)
                             (values :escape 0)))
            ((member code '(13 10)) (values :return 0))
            ((member code '(8 127)) (values :backspace 0))
            ((= code 9)             (values :tab 0))
            ((<= 1 code 26)         (values (code-char (+ code 96)) 4))  ; C-a..C-z (bit 2 = Ctrl)
            (t                      (values c 0)))))))

(defun run-browser-fb ()
  "Launch the code browser full-screen on /dev/fb0, rooted at CL-USER. Blocks until
   you leave it (C-g at the root, or Esc), then restores the text console and
   returns to the supervisor menu. Any error is caught so it cannot crash PID 1."
  (handler-case
      (progn
        (unless *bfont* (setf *bfont* (lol.canvas:load-font "/cozette.lolf")))
        (trail-root (find-package :cl-user))
        (multiple-value-bind (xres yres) (lol.fb:fb-geometry)
          (let ((canvas (lol.canvas:make-canvas xres yres)))
            (lol.fb:with-graphics-console            ; KD_GRAPHICS: fbcon stops drawing
              (with-raw-mode (0)                     ; console keys, unbuffered, no echo
                (loop
                  (draw-browser canvas xres yres)
                  (lol.fb:present-fb canvas)
                  (multiple-value-bind (key state) (fb-read-key *standard-input*)
                    (when (or (eq key :eof) (eq :quit (browser-key key state)))
                      (return)))))))))
    (serious-condition (c)
      (format t "~&code browser: ~a~%" c) (finish-output))))
