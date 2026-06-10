;;;; supervisor.lisp — a tiny, didactic init/supervisor in Common Lisp.
;;;;
;;;; Roles chosen by argv:
;;;;   (no args)   => interactive SUPERVISOR menu  (this is PID 1)
;;;;   "worker" N  => a short-lived WORKER process
;;;;
;;;; The supervisor menu lets you run a Lisp REPL, spawn a worker, or power
;;;; the machine off. PID 1 must NEVER return, so every path either loops or
;;;; powers off, and errors are caught so a typo can't crash init.

(defun worker-main (id)
  "A throwaway child: prints who it is, does a little Lisp, then exits."
  (format t "    [worker ~a] hello — my pid is ~a~%" id (sb-unix:unix-getpid))
  (finish-output)
  (let ((sum (loop for i from 1 to 1000000 summing i)))
    (format t "    [worker ~a] computed (sum 1..1,000,000) = ~:d~%" id sum))
  (finish-output)
  (sb-ext:exit :code (mod (parse-integer id) 256)))

(defun spawn-worker (id)
  "Fork+exec another copy of ourselves as a worker (run-program reaps it)."
  (sb-ext:run-program "/sbin/lisp" (list "worker" (princ-to-string id))
                      :wait nil :input t :output t :error t))

(defun power-off ()
  "Sync filesystems and ask the kernel to power off via reboot(2).
   Needs PID-1/root privileges; returns to the caller if it is refused."
  (format t "~&Syncing disks and powering off...~%")
  (finish-output)
  (sb-alien:alien-funcall (sb-alien:extern-alien "sync" (function sb-alien:void)))
  (sleep 1)
  ;; glibc reboot(howto); #x4321fedc = RB_POWER_OFF (ACPI power off).
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "reboot" (function sb-alien:int sb-alien:int))
   #x4321fedc)
  ;; only reached if the power-off was refused
  (format t "~&Power-off failed (need privileges?). Back to the menu.~%")
  (finish-output))

;;; ---- a "poor man's readline": in-line editing for the REPL --------------
;;;
;;; SBCL ships no readline, and we refuse to add a native library. But rich
;;; line editing only needs three things we already have:
;;;
;;;   1. RAW MODE — tell the kernel TTY to stop line-buffering and stop echoing,
;;;      so we see every keystroke (arrows included) the instant it is pressed.
;;;      This is the ONE syscall we can't do in portable Lisp; it is FFI into
;;;      tcgetattr/tcsetattr in the libc the image is already linked against
;;;      (same trick power-off uses for reboot(2)). No new file, no new lib.
;;;   2. The framebuffer console is a VT/ANSI terminal emulator, so we redraw
;;;      the line by EMITTING escape sequences (clear-to-EOL, absolute column).
;;;   3. Everything else — buffer, cursor, history, key handling — is plain Lisp.
;;;
;;; See line-editing.org (Path 2) for the full rationale.

;; struct termios on x86-64 Linux/glibc is 60 bytes; we treat it as a raw byte
;; block and patch only the fields we need (no field-by-field alien struct):
;;   offset  0  c_iflag   (4)   8  c_cflag (4)
;;           4  c_oflag   (4)  12  c_lflag (4)  <- ICANON|ECHO live in its low byte
;;          16  c_line    (1)  17  c_cc[32]     <- VTIME=c_cc[5], VMIN=c_cc[6]
;;          52  c_ispeed  (4)  56  c_ospeed (4)
(defconstant +termios-size+ 60)
(defconstant +lflag-off+    12)            ; byte offset of c_lflag
(defconstant +vtime-off+    (+ 17 5))      ; c_cc[VTIME]
(defconstant +vmin-off+     (+ 17 6))      ; c_cc[VMIN]
(defconstant +icanon\|echo+ #x0A)          ; ICANON(0x02) | ECHO(0x08), both in c_lflag's low byte

(sb-alien:define-alien-routine ("tcgetattr" %tcgetattr) sb-alien:int
  (fd sb-alien:int) (tio (sb-alien:* sb-alien:unsigned-char)))
(sb-alien:define-alien-routine ("tcsetattr" %tcsetattr) sb-alien:int
  (fd sb-alien:int) (action sb-alien:int) (tio (sb-alien:* sb-alien:unsigned-char)))

(defun tty-raw-capable-p (fd)
  "True if FD is a real terminal (tcgetattr succeeds). False for a pipe/file,
   where we fall back to ordinary line-buffered input."
  (let ((tio (sb-alien:make-alien sb-alien:unsigned-char +termios-size+)))
    (unwind-protect (zerop (%tcgetattr fd tio))
      (sb-alien:free-alien tio))))

(defmacro with-raw-mode ((&optional (fd 0)) &body body)
  "Run BODY with FD's TTY in raw-ish mode (no canonical line buffering, no echo;
   read() returns after each keystroke). Restores the saved settings on ANY exit
   — return, error, or non-local — via unwind-protect. We keep ISIG enabled, so
   Ctrl-C still interrupts a runaway eval the normal way."
  (let ((g-fd (gensym)) (saved (gensym)) (work (gensym)) (i (gensym)))
    `(let* ((,g-fd  ,fd)
            (,saved (sb-alien:make-alien sb-alien:unsigned-char +termios-size+))
            (,work  (sb-alien:make-alien sb-alien:unsigned-char +termios-size+)))
       (unwind-protect
            (progn
              (%tcgetattr ,g-fd ,saved)
              (dotimes (,i +termios-size+)         ; start from a copy of the live settings
                (setf (sb-alien:deref ,work ,i) (sb-alien:deref ,saved ,i)))
              (setf (sb-alien:deref ,work +lflag-off+)            ; clear ICANON|ECHO
                    (logand (sb-alien:deref ,work +lflag-off+) (lognot +icanon\|echo+)))
              (setf (sb-alien:deref ,work +vtime-off+) 0          ; VTIME=0, VMIN=1:
                    (sb-alien:deref ,work +vmin-off+)  1)         ; block for one keystroke
              (%tcsetattr ,g-fd 0 ,work)           ; 0 = TCSANOW
              (unwind-protect (progn ,@body)
                (%tcsetattr ,g-fd 0 ,saved)))      ; ALWAYS restore cooked mode
         (sb-alien:free-alien ,saved)
         (sb-alien:free-alien ,work)))))

(defvar *repl-history* '()
  "Previously entered REPL lines, most recent first; recalled with Up/Down.")

(defun refresh-line (out prompt line point)
  "Redraw the single input line in place: carriage-return to column 0, repaint
   PROMPT+LINE, clear any leftover tail, then park the cursor at POINT. One
   uniform redraw covers every edit (insert, delete, cursor move, history)."
  (write-char #\Return out)                          ; column 0
  (write-string prompt out)
  (write-string line out)
  (format out "~C[K" #\Escape)                       ; clear to end of line
  (format out "~C[~dG" #\Escape (+ (length prompt) point 1))  ; absolute column (1-based)
  (finish-output out))

(defun read-escape (in)
  "Parse the tail of an ESC sequence already begun by ESC; return a key keyword
   (:up :down :left :right :home :end :delete) or :ignore. Covers CSI (ESC [ …)
   including the numeric ESC [ n ~ forms, and the ESC O H/F some terminals send."
  (let ((c1 (read-char in nil nil)))
    (cond
      ((eql c1 #\[)
       (let ((c2 (read-char in nil nil)))
         (cond
           ((null c2) :ignore)
           ((char= c2 #\A) :up)    ((char= c2 #\B) :down)
           ((char= c2 #\C) :right) ((char= c2 #\D) :left)
           ((char= c2 #\H) :home)  ((char= c2 #\F) :end)
           ((digit-char-p c2)                          ; ESC [ <n> ~
            (let ((n (digit-char-p c2)))
              (loop for c = (read-char in nil nil)     ; consume digits up to the ~
                    while (and c (digit-char-p c))
                    do (setf n (+ (* n 10) (digit-char-p c))))
              (case n ((1 7) :home) ((4 8) :end) (3 :delete) (t :ignore))))
           (t :ignore))))
      ((eql c1 #\O)                                    ; ESC O H / ESC O F
       (let ((c2 (read-char in nil nil)))
         (cond ((eql c2 #\H) :home) ((eql c2 #\F) :end) (t :ignore))))
      (t :ignore))))

(defun read-line-edited (in out prompt)
  "Read one line from IN with in-line editing, rendering to OUT. Returns the
   finished string, or :eof (Ctrl-D on an empty line, or stream end). Lines are
   short here, so the buffer is just a string we rebuild on each edit — clearer
   than juggling a gap buffer, and plenty fast for a console."
  (let ((line "") (point 0) (hpos -1) (stash ""))
    (labels ((redraw () (refresh-line out prompt line point))
             (ins (c) (setf line (concatenate 'string (subseq line 0 point)
                                              (string c) (subseq line point)))
                  (incf point))
             (del-back () (when (> point 0)
                            (setf line (concatenate 'string (subseq line 0 (1- point))
                                                    (subseq line point)))
                            (decf point)))
             (del-fwd () (when (< point (length line))
                           (setf line (concatenate 'string (subseq line 0 point)
                                                   (subseq line (1+ point))))))
             (kill-word ()
               (let ((i point))
                 (loop while (and (> i 0) (char= (char line (1- i)) #\Space)) do (decf i))
                 (loop while (and (> i 0) (char/= (char line (1- i)) #\Space)) do (decf i))
                 (setf line (concatenate 'string (subseq line 0 i) (subseq line point))
                       point i)))
             (hist-prev ()                       ; Up / Ctrl-P: older entry
               (when (and *repl-history* (< hpos (1- (length *repl-history*))))
                 (when (= hpos -1) (setf stash line))   ; remember the live line once
                 (incf hpos)
                 (setf line (nth hpos *repl-history*) point (length line))))
             (hist-next ()                       ; Down / Ctrl-N: newer entry / back to live
               (cond ((> hpos 0) (decf hpos)
                      (setf line (nth hpos *repl-history*) point (length line)))
                     ((= hpos 0) (setf hpos -1 line stash point (length line))))))
      (redraw)
      (loop
        (let ((c (read-char in nil :eof)))
          (when (eq c :eof) (terpri out) (return :eof))
          (case (char-code c)
            ((13 10) (terpri out) (finish-output out) (return line))  ; Enter
            (4 (if (string= line "")                                  ; Ctrl-D
                   (progn (terpri out) (return :eof))
                   (progn (del-fwd) (redraw))))
            ((8 127) (del-back) (redraw))                             ; Backspace / Rubout
            (1 (setf point 0) (redraw))                              ; Ctrl-A: line start
            (5 (setf point (length line)) (redraw))                  ; Ctrl-E: line end
            (2 (when (> point 0) (decf point) (redraw)))             ; Ctrl-B: left
            (6 (when (< point (length line)) (incf point) (redraw))) ; Ctrl-F: right
            (11 (setf line (subseq line 0 point)) (redraw))          ; Ctrl-K: kill to EOL
            (21 (setf line (subseq line point) point 0) (redraw))    ; Ctrl-U: kill to start
            (23 (kill-word) (redraw))                                ; Ctrl-W: kill word back
            (12 (format out "~C[2J~C[H" #\Escape #\Escape) (redraw)) ; Ctrl-L: clear screen
            (16 (hist-prev) (redraw))                                ; Ctrl-P
            (14 (hist-next) (redraw))                                ; Ctrl-N
            (27 (case (read-escape in)                               ; ESC sequence
                  (:left  (when (> point 0) (decf point) (redraw)))
                  (:right (when (< point (length line)) (incf point) (redraw)))
                  (:home  (setf point 0) (redraw))
                  (:end   (setf point (length line)) (redraw))
                  (:up    (hist-prev) (redraw))
                  (:down  (hist-next) (redraw))
                  (:delete (del-fwd) (redraw))
                  (t nil)))
            (t (when (>= (char-code c) 32) (ins c) (redraw)))))))))  ; printable

(defun complete-form (text)
  "Try to READ one form from TEXT. Returns a second value telling the caller what
   to do: :ok (with the form), :incomplete (unbalanced — read another line),
   :empty (only whitespace), or :error (with the condition)."
  (if (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) text)
      (values nil :empty)
      (handler-case (values (read-from-string text) :ok)
        (end-of-file ()        (values nil :incomplete))
        (serious-condition (c) (values c :error)))))

(defun push-history (text)
  "Record TEXT as a history entry (newlines flattened to keep recall single-line),
   skipping blanks and immediate duplicates."
  (let ((entry (substitute #\Space #\Newline
                           (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (when (and (plusp (length entry))
               (or (null *repl-history*) (string/= entry (first *repl-history*))))
      (push entry *repl-history*))))

(defun editing-repl (in out)
  "The editing REPL: read a line, and keep reading continuation lines until a
   whole form is available (so multi-line forms work), then eval and print."
  (loop
    (let ((text "") (prompt "lisp> "))
      (block one-form
        (loop
          (let ((line (read-line-edited in out prompt)))
            (when (eq line :eof)
              (if (string= text "")
                  (return-from editing-repl)   ; EOF at a fresh prompt: leave the REPL
                  (return-from one-form)))      ; EOF mid-form: drop it, start fresh
            (setf text (if (string= text "") line
                           (concatenate 'string text (string #\Newline) line)))
            (multiple-value-bind (form status) (complete-form text)
              (ecase status
                (:empty      (return-from one-form))             ; blank line: re-prompt
                (:incomplete (setf prompt "  ...> "))            ; unbalanced: keep reading
                (:error      (format out "~&read error: ~a~%" form)
                             (finish-output out) (return-from one-form))
                (:ok
                 (push-history text)
                 (when (eq form :quit) (return-from editing-repl))
                 (handler-case (format out "~&=> ~s~%" (eval form))
                   (serious-condition (c) (format out "~&error: ~a~%" c)))
                 (finish-output out)
                 (return-from one-form))))))))))

(defun plain-repl (in out)
  "Fallback REPL when stdin is NOT a TTY (a pipe/file): ordinary line-buffered
   READ, no editing — the kernel still gives us backspace + line buffering."
  (let ((eof (list :eof)))
    (loop
      (format out "~&lisp> ") (finish-output out)
      (let ((form (handler-case (read in nil eof)
                    (serious-condition (c) (format out "~&read error: ~a~%" c) nil))))
        (cond
          ((or (eq form eof) (eq form :quit)) (return))
          ((null form))
          (t (handler-case (format out "~&=> ~s~%" (eval form))
               (serious-condition (c) (format out "~&error: ~a~%" c)))))))))

(defun run-repl ()
  "A read-eval-print loop with in-line editing. :quit (or Ctrl-D on an empty
   line) returns to the menu; every read/eval is guarded so a bad form cannot
   kill PID 1. Uses the editor on a real TTY; falls back to plain READ otherwise."
  (let ((in *standard-input*) (out *standard-output*))
    (format out "~&~%Lisp REPL — edit with arrows / Ctrl-A E B F K U W, Up/Down for~%")
    (format out "history, :quit (or Ctrl-D on an empty line) to return to the menu.~%")
    (finish-output out)
    (if (tty-raw-capable-p 0)
        (with-raw-mode (0) (editing-repl in out))
        (plain-repl in out))))

(defun show-input-devices ()
  "Diagnostic: list the input devices the kernel currently sees (from
   /proc/bus/input/devices). Tells us whether a keyboard driver bound."
  (format t "~&Input devices detected by the kernel:~%")
  (let ((any nil))
    (ignore-errors
      (with-open-file (s "/proc/bus/input/devices" :if-does-not-exist nil)
        (when s
          (loop for line = (read-line s nil nil) while line do
            (when (and (> (length line) 2) (string= (subseq line 0 2) "N:"))
              (setf any t)
              (format t "   ~a~%" line))))))
    (unless any
      (format t "   (none found — no keyboard driver bound)~%")))
  (finish-output))

;;; ---- framebuffer graphics: blit the Land-of-Lisp alien ------------------
;;;
;;; The framebuffer is just memory: /dev/fb0 maps pixel (x,y) to byte offset
;;; y*stride + x*4 (we assume 32 bpp). We read the live geometry from sysfs,
;;; then copy our sprite into the top-right corner. Transparency is honoured
;;; by reading each background row first and only overwriting opaque pixels.

(defparameter +fb-dev+      "/dev/fb0")
(defparameter +alien-path+  "/alien.rgba")   ; raw RGBA, top-to-bottom
(defparameter +alien-width+  256)
(defparameter +alien-height+ 150)

(defun read-fb-geometry ()
  "Return (values xres yres stride-bytes bpp) from /sys/class/graphics/fb0.
   virtual_size is 'W,H'; stride and bits_per_pixel are plain integers."
  (flet ((line (path) (with-open-file (s path) (read-line s))))
    (let* ((vs    (line "/sys/class/graphics/fb0/virtual_size"))
           (comma (position #\, vs)))
      (values (parse-integer vs :end comma)
              (parse-integer vs :start (1+ comma) :junk-allowed t)
              (parse-integer (line "/sys/class/graphics/fb0/stride")        :junk-allowed t)
              (parse-integer (line "/sys/class/graphics/fb0/bits_per_pixel") :junk-allowed t)))))

(defun draw-alien (&key (margin 16) announce)
  "Blit the alien sprite into the top-right corner of /dev/fb0.
   Pure userland: the framebuffer is memory we seek into and write.
   :announce t prints a status line (we stay silent on routine menu redraws)."
  (handler-case
      (multiple-value-bind (xres yres stride bpp) (read-fb-geometry)
        (unless (= bpp 32)
          (format t "~&framebuffer is ~a bpp; this demo assumes 32 — skipping.~%" bpp)
          (return-from draw-alien))
        (let* ((bypp 4)
               (x0   (max 0 (- xres +alien-width+ margin)))
               (y0   margin)
               (rows (min +alien-height+ (- yres y0)))   ; clamp to screen height
               (rgba (make-array (* +alien-width+ +alien-height+ bypp)
                                 :element-type '(unsigned-byte 8)))
               (row  (make-array (* +alien-width+ bypp)
                                 :element-type '(unsigned-byte 8))))
          (with-open-file (a +alien-path+ :element-type '(unsigned-byte 8))
            (read-sequence rgba a))
          (with-open-file (fb +fb-dev+ :direction :io
                                       :element-type '(unsigned-byte 8)
                                       :if-exists :overwrite)
            (dotimes (y rows)
              (let ((dst (+ (* (+ y0 y) stride) (* x0 bypp))))
                (file-position fb dst)
                (read-sequence row fb)               ; keep the existing background
                (dotimes (x +alien-width+)
                  (let* ((si (* (+ (* y +alien-width+) x) 4))
                         (alpha (aref rgba (+ si 3))))
                    (when (>= alpha 128)             ; opaque enough -> paint it
                      (let ((di (* x bypp)))
                        ;; efifb is commonly BGRX: bytes are Blue,Green,Red,pad.
                        ;; If colours look swapped, exchange these b/r lines.
                        (setf (aref row di)       (aref rgba (+ si 2))   ; B
                              (aref row (+ di 1)) (aref rgba (+ si 1))   ; G
                              (aref row (+ di 2)) (aref rgba si))))))    ; R
                (file-position fb dst)
                (write-sequence row fb)))
            (finish-output fb))
          (when announce
            (format t "~&alien drawn at (~a,~a) on a ~ax~a framebuffer.~%"
                    x0 y0 xres yres)
            (finish-output))))
    (serious-condition (c)
      (format t "~&couldn't draw the alien: ~a~%" c) (finish-output))))

(defun print-menu ()
  (format t "~&~%======== lisp-over-linux supervisor ========~%")
  (format t "  I am PID ~a · SBCL ~a~%~%"
          (sb-unix:unix-getpid) (lisp-implementation-version))
  (format t "  r) run a Lisp REPL~%")
  (format t "  w) spawn a worker process~%")
  (format t "  a) draw the Land-of-Lisp alien~%")
  (format t "  s) shut down (power off)~%")
  (format t "============================================~%")
  (format t "choice> ") (finish-output)
  (draw-alien))                  ; (re)paint the alien LAST so it stays on screen

(defun supervisor-main ()
  "PID 1: an interactive menu loop. Never returns."
  (format t "~%Supervisor up — I am PID ~a, SBCL ~a.~%"
          (sb-unix:unix-getpid) (lisp-implementation-version))
  (sleep 2)                       ; give USB a moment to enumerate
  (show-input-devices)            ; diagnostic: which keyboard(s) bound?
  (sleep 1)                       ; let the boot diagnostics be read first
  (format t "~C[2J~C[H" #\Escape #\Escape)  ; clear screen for a clean menu + alien
  (let ((worker-id 0))
    (loop
      (handler-case
          (progn
            (print-menu)
            (let* ((line   (read-line *standard-input* nil :eof))
                   (choice (and (stringp line)
                                (string-trim '(#\Space #\Tab #\Return) line))))
              (cond
                ((eq line :eof) (sleep 1))            ; no input source: don't spin
                ((string-equal choice "r") (run-repl))
                ((string-equal choice "w")
                 (incf worker-id)
                 (format t "~&spawning worker ~a~%" worker-id)
                 (spawn-worker worker-id))
                ((string-equal choice "a") (draw-alien :announce t))
                ((string-equal choice "s") (power-off))
                ((string= choice ""))                 ; bare Enter: just redraw
                (t (format t "~&unknown choice: ~a~%" choice)))))
        (serious-condition (c)
          (format t "~&menu error: ~a~%" c) (finish-output))))))

(defun init-toplevel ()
  (let ((args (rest sb-ext:*posix-argv*)))
    (if (and args (string= (first args) "worker"))
        (worker-main (second args))
        (supervisor-main))))
