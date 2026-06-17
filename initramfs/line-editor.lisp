;;;; line-editor.lisp — a "poor man's readline": in-line editing in pure Lisp.
;;;;
;;;; SBCL ships no readline, and we refuse to add a native library. But rich
;;;; line editing only needs three things we already have:
;;;;
;;;;   1. RAW MODE — tell the kernel TTY to stop line-buffering and stop echoing,
;;;;      so we see every keystroke (arrows included) the instant it is pressed.
;;;;      This is the ONE syscall we can't do in portable Lisp; it is FFI into
;;;;      tcgetattr/tcsetattr in the libc the image is already linked against
;;;;      (same trick power-off uses for reboot(2)). No new file, no new lib.
;;;;   2. The framebuffer console is a VT/ANSI terminal emulator, so we redraw
;;;;      the line by EMITTING escape sequences (clear-to-EOL, absolute column).
;;;;   3. Everything else — buffer, cursor, history, key handling — is plain Lisp.
;;;;
;;;; This file is the reusable line-reading toolkit; repl.lisp drives it.
;;;; See line-editing.org (Path 2) for the full rationale.

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

(defvar *completer* nil
  "Function of one arg (the partial token before point) returning a list of
   completion strings, or NIL to disable <Tab> completion. repl.lisp sets it to
   complete-symbol. Kept as a hook so this editor stays Lisp-agnostic.")

(defun token-start (line point)
  "Index where the token ending at POINT begins — scan back over symbol
   constituents (anything that is not whitespace or a usual Lisp delimiter)."
  (let ((i point))
    (loop while (and (> i 0)
                     (not (member (char line (1- i))
                                  '(#\Space #\Tab #\Newline #\( #\) #\' #\" #\; #\` #\,))))
          do (decf i))
    i))

(defun longest-common-prefix (strings)
  "The longest string that every member of STRINGS starts with."
  (if (null strings) ""
      (reduce (lambda (a b) (subseq a 0 (or (mismatch a b) (length a)))) strings)))

(defun list-candidates (out candidates &optional (limit 80))
  "Print CANDIDATES space-separated on their own line (capped at LIMIT)."
  (terpri out)
  (let ((shown (if (> (length candidates) limit) (subseq candidates 0 limit) candidates)))
    (format out "~{~a~^  ~}" shown)
    (when (> (length candidates) limit)
      (format out "  … (~d total)" (length candidates))))
  (terpri out)
  (finish-output out))

(defun read-line-edited (in out prompt)
  "Read one line from IN with in-line editing, rendering to OUT. Returns the
   finished string, :eof (Ctrl-D on an empty line, or stream end), or :cancel
   (Ctrl-C — abandon this line). Lines are short here, so the buffer is just a
   string we rebuild on each edit — clearer than a gap buffer, and plenty fast.

   Note on Ctrl-C: on the LOCAL console the editor runs under with-raw-mode, which
   keeps ISIG enabled, so Ctrl-C raises SIGINT there and never reaches this code
   as a byte. The byte-3 case below only fires over the NETWORK REPL, where the
   host client forwards keystrokes raw (no signals) — there Ctrl-C aborts the line."
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
            (3 (write-string "^C" out) (terpri out) (finish-output out)
               (return :cancel))                                     ; Ctrl-C: abort line
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
            (9 (when *completer*                                     ; Tab: completion
                 (let* ((start (token-start line point))
                        (token (subseq line start point))
                        (cands (and (plusp (length token)) (funcall *completer* token))))
                   (cond
                     ((null cands))                                  ; no match: do nothing
                     ((null (rest cands))                            ; one match: insert it
                      (setf line (concatenate 'string (subseq line 0 start)
                                              (first cands) (subseq line point))
                            point (+ start (length (first cands))))
                      (redraw))
                     (t (let ((cp (longest-common-prefix cands)))
                          (if (> (length cp) (length token))         ; many: extend prefix
                              (progn
                                (setf line (concatenate 'string (subseq line 0 start)
                                                        cp (subseq line point))
                                      point (+ start (length cp)))
                                (redraw))
                              (progn (list-candidates out cands)     ; can't extend: list
                                     (redraw)))))))))
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

(defun push-history (text)
  "Record TEXT as a history entry (newlines flattened to keep recall single-line),
   skipping blanks and immediate duplicates."
  (let ((entry (substitute #\Space #\Newline
                           (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (when (and (plusp (length entry))
               (or (null *repl-history*) (string/= entry (first *repl-history*))))
      (push entry *repl-history*))))
