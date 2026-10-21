;;;; terminal-test.lisp — host test for poc/terminal.lisp (the VT engine).
;;;;
;;;;   sbcl --script poc/terminal-test.lisp
;;;;
;;;; Prints PASS/FAIL per assertion and exits non-zero if any failed. Needs nothing
;;;; but a stock SBCL: the engine is pure data. Two kinds of case:
;;;;
;;;;  - hand-written sequences, one feature each (wrap, scroll regions, SGR, …), so a
;;;;    failure names the feature;
;;;;  - RECORDINGS of real programs (poc/terminal-data/*.typescript), captured on the
;;;;    build host with script(1) at 80x24 and TERM=xterm-256color — what less, vi and
;;;;    an interactive bash really send, not what the manual says they send. The
;;;;    recipe is in doc/terminal.org, "Testing against real programs".

(load (merge-pathnames "terminal.lisp" (or *load-truename* *default-pathname-defaults*)))

(defpackage :lol.terminal-test (:use :cl :lol.terminal))
(in-package :lol.terminal-test)

(defvar *failures* 0)
(defvar *count* 0)

(defun check (name got want)
  (incf *count*)
  (if (equalp got want)
      (format t "PASS  ~a~%" name)
      (progn (incf *failures*)
             (format t "FAIL  ~a~%      got:  ~s~%      want: ~s~%" name got want))))

(defun esc (fmt &rest args)
  "FMT (through FORMAT only when ARGS are given) with C-style escapes: \\e ESC,
   \\r CR, \\n LF, \\t TAB, \\\\ one backslash. Lisp strings have no such escapes,
   and terminal sequences are unreadable without them."
  (let ((s (if args (apply #'format nil fmt args) fmt)))
    (with-output-to-string (o)
      (loop with i = 0 while (< i (length s)) do
        (let ((c (char s i)) (n (and (< (1+ i) (length s)) (char s (1+ i)))))
          (if (and (char= c #\\) n (find n "ernt\\"))
              (progn (write-char (ecase n (#\e #\Esc) (#\r #\Return) (#\n #\Newline)
                                   (#\t #\Tab) (#\\ #\\))
                                 o)
                     (incf i 2))
              (progn (write-char c o) (incf i))))))))

(defun feed (term fmt &rest args)
  (term-feed term (utf8-encode (apply #'esc fmt args))))

(defun row (term y) (term-row-text term y))

(defun bytes-of (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defun data (name)
  (merge-pathnames (concatenate 'string "terminal-data/" name)
                   (or *load-truename* *default-pathname-defaults*)))

(defun take-reply (term)
  (let ((o (term-take-output term))) (and o (map 'string #'code-char o))))

;;; ---- printing, wrapping -------------------------------------------------------

(let ((term (make-term 10 4)))
  (feed term "hello~c~cworld" #\Return #\Newline)
  (check "print + CRLF" (list (row term 0) (row term 1)) '("hello" "world"))
  (check "cursor after" (list (term-cx term) (term-cy term)) '(5 1)))

(let ((term (make-term 10 4)))
  (feed term "0123456789abcde")
  (check "autowrap: the 11th char starts row 1" (list (row term 0) (row term 1)) '("0123456789" "abcde"))
  (check "a wrapped row copies back as ONE line"
         (term-text-range term (term-view-top term) 0 (1+ (term-view-top term)) 4) "0123456789abcde"))

(let ((term (make-term 10 3)))
  (feed term "\\e[3;1H0123456789")
  (check "pending wrap: filling the bottom-right cell does not scroll"
         (list (row term 2) (term-scrollback-count term) (term-cx term)) '("0123456789" 0 9))
  (feed term "X")
  (check "...the NEXT character does" (list (row term 1) (row term 2) (term-scrollback-count term))
         '("0123456789" "X" 1)))

(let ((term (make-term 10 3)))
  (feed term "\\e[?7l0123456789XY")
  (check "DECAWM off: the last column is overwritten" (list (row term 0) (row term 1)) '("012345678Y" "")))

;;; ---- cursor movement and erasing ----------------------------------------------

(let ((term (make-term 20 5)))
  (feed term "abcdefghij\\e[3;5HX\\e[2AY\\e[10CZ\\e[3DW")
  (check "CUP / CUU / CUF / CUB" (list (row term 0) (row term 2)) '("abcdeYghij    W Z" "    X"))
  (feed term "\\e[1;3H\\e[K")
  (check "EL 0 erases to the end" (row term 0) "ab")
  (feed term "\\e[3;3H\\e[1K")
  (check "EL 1 erases to the cursor" (row term 2) "    X")
  (feed term "\\e[2J")
  (check "ED 2 clears the screen" (term-screen-text term) '("" "" "" "" "")))

(let ((term (make-term 10 3)))
  (feed term "abcdef\\e[1;3H\\e[2@")
  (check "ICH opens cells" (row term 0) "ab  cdef")
  (feed term "\\e[3P")
  (check "DCH closes them" (row term 0) "abdef")
  (feed term "\\e[1;2H\\e[2X")
  (check "ECH blanks without moving the rest" (row term 0) "a  ef"))

(let ((term (make-term 10 4)))
  (feed term "one\\r\\ntwo\\r\\nthree\\r\\nfour\\e[2;1H\\e[L")
  (check "IL pushes lines down inside the region"
         (term-screen-text term) '("one" "" "two" "three"))
  (feed term "\\e[M\\e[M")
  (check "DL pulls them up" (term-screen-text term) '("one" "three" "" "")))

(let ((term (make-term 20 3)))
  (feed term "a\\tb\\tc")
  (check "tabs every 8 columns" (row term 0) "a       b       c")
  (feed term "\\e[Z\\e[ZX")
  (check "CBT goes back a stop" (row term 0) "a       X       c"))

;;; ---- scroll regions and the scrollback ------------------------------------------

(let ((term (make-term 10 5)))
  (feed term "top\\e[5;1Hbottom\\e[2;4r\\e[4;1Ha\\r\\nb\\r\\nc")
  (check "DECSTBM: scrolling stays inside the region"
         (term-screen-text term) '("top" "a" "b" "c" "bottom"))
  (check "...and a region scroll is not history" (term-scrollback-count term) 0)
  (feed term "\\e[r\\e[2;1H\\e[S")
  (check "SU scrolls the whole screen" (row term 0) "a")
  (feed term "\\e[T")
  (check "SD scrolls it back down" (list (row term 0) (row term 1)) (quote ("" "a"))))

(let ((term (make-term 10 3)))
  (dotimes (i 8) (feed term "line~d\\r\\n" i))
  (check "lines leaving the top go to the scrollback" (term-scrollback-count term) 6)
  (check "the screen shows the newest" (term-screen-text term) '("line6" "line7" ""))
  (term-scroll-view term 2)
  (check "scrolling the view back shows history"
         (loop for y below 3 collect (lol.terminal::line-text (term-view-line term y)))
         '("line4" "line5" "line6"))
  (feed term "more\\r\\n")
  (check "new output keeps the reader's place" (term-view-offset term) 3)
  (term-scroll-view term -100)
  (check "back to live" (term-view-offset term) 0))

;;; ---- the alternate screen --------------------------------------------------------

(let ((term (make-term 10 3)))
  (feed term "shell$ ls")
  (feed term "\\e[?1049h\\e[H\\e[2Jfull screen")
  (check "1049 enters a blank alternate screen" (list (term-alt-screen-p term) (row term 0))
         '(t "full scree"))
  (feed term "\\e[?1049l")
  (check "...and leaving restores the shell's screen and cursor"
         (list (term-alt-screen-p term) (row term 0) (term-cx term) (term-cy term))
         '(nil "shell$ ls" 9 0)))

;;; ---- SGR -------------------------------------------------------------------------

(flet ((pen-after (seq)
         (let ((term (make-term 10 2)))
           (feed term seq)
           (feed term "x")
           (let ((a (aref (tline-attrs (term-line term 0)) 0)))
             (list (attr-flags a) (attr-fg a) (attr-bg a))))))
  (check "SGR 1;31" (pen-after "\\e[1;31m") (list +bold+ 2 0))
  (check "SGR 7 + 44" (pen-after "\\e[7;44m") (list +inverse+ 0 5))
  (check "SGR 38;5;208 (256 colours)" (pen-after "\\e[38;5;208m") '(0 209 0))
  (check "SGR 38;2;1;2;3 (truecolor, semicolons)" (pen-after "\\e[38;2;1;2;3m")
         (list 0 (logior (ash 1 24) #x010203) 0))
  (check "SGR 48:2::10:20:30 (truecolor, colons)" (pen-after "\\e[48:2::10:20:30m")
         (list 0 0 (logior (ash 1 24) #x0A141E)))
  (check "SGR 4:0 is NOT underline" (pen-after "\\e[4:0m") '(0 0 0))
  (check "SGR 92 bright green" (pen-after "\\e[92m") '(0 11 0))
  (check "SGR 1;4;31 then 0 resets" (pen-after "\\e[1;4;31m\\e[0m") '(0 0 0))
  (check "SGR 1 then 22 clears bold" (pen-after "\\e[1m\\e[22m") '(0 0 0)))

(let ((term (make-term 10 2)))
  (feed term "\\e[44m\\e[2J")
  (check "bce: erasing paints the current background"
         (attr-bg (aref (tline-attrs (term-line term 1)) 5)) 5))

;;; ---- answers to the program -------------------------------------------------------

(let ((term (make-term 80 24)))
  (feed term "\\e[5;10H\\e[6n")
  (check "DSR 6: the cursor position report" (take-reply term) (esc "\\e[5;10R"))
  (feed term "\\e[c")
  (check "DA: a VT220 with colour" (take-reply term) (esc "\\e[?62;22c"))
  (feed term "\\e[>c")
  (check "DA2" (take-reply term) (esc "\\e[>1;10;0c"))
  (feed term "\\e[?2004h\\e[?2004$p")
  (check "DECRQM on a mode we know" (take-reply term) (esc "\\e[?2004;1$y"))
  (feed term "\\e[?2026$p")
  (check "DECRQM on one we don't: 0" (take-reply term) (esc "\\e[?2026;0$y"))
  (feed term "\\e[18t")
  (check "size in characters" (take-reply term) (esc "\\e[8;24;80t"))
  (feed term "\\e]11;?\\e\\\\")
  (check "OSC 11: the background colour" (take-reply term) (esc "\\e]11;rgb:1b1b/1e1e/2424\\e\\\\"))
  (check "nothing else owed" (take-reply term) nil))

(let ((term (make-term 20 2)))
  (feed term "\\e]0;my title~c" (code-char 7))
  (check "OSC 0 with BEL" (term-title term) "my title")
  (feed term "\\e]2;other\\e\\\\after")
  (check "OSC 2 with ST, and printing resumes" (list (term-title term) (row term 0)) '("other" "after")))

;; OSC 52: the program copies (base64 of UTF-8; "aé日" is YcOp5pel)
(let ((term (make-term 20 2)))
  (feed term "\\e]52;c;YcOp5pel~c" (code-char 7))
  (feed term "\\e]52;;aGk=\\e\\\\")
  (feed term "\\e]52;c;?\\e\\\\")
  (check "OSC 52: copies kept in order, empty targets = s0, a query not answered"
         (list (term-take-copies term) (take-reply term))
         '((("c" . "aé日") ("s0" . "hi")) nil))
  (check "OSC 52: taken once" (term-take-copies term) nil)
  (let ((long (make-string 9000 :initial-element #\A)))      ; 6750 octets of zeros
    (feed term (format nil "\\e]52;c;~a\\e\\\\" long))
    (check "OSC 52: longer than any other OSC may be"
           (length (cdr (first (term-take-copies term)))) 6750)))

;;; ---- UTF-8, wide characters, DEC graphics -------------------------------------------

(let* ((term (make-term 10 2))
       (bytes (utf8-encode "aé日b")))
  (term-feed term (subseq bytes 0 2))       ; a, and the FIRST byte of é
  (term-feed term (subseq bytes 2))         ; the rest: a character split across reads
  (check "UTF-8 across two reads, a wide char in two cells"
         (list (row term 0) (term-cx term)) (list "aé日b" 5))
  (check "the wide char's right half is a tail cell"
         (char (tline-chars (term-line term 0)) 3) +wide-tail+))

(let ((term (make-term 10 2)))
  (feed term "\\e(0lqqk\\e(Bq")
  (check "ESC ( 0: DEC special graphics draw boxes"
         (row term 0) (coerce (list (code-char #x250C) (code-char #x2500) (code-char #x2500)
                                    (code-char #x2510) #\q) 'string)))

(let ((term (make-term 10 2)))
  (feed term "ab\\e[31;1~cd~c~cef" (code-char 24) (code-char 7) (code-char 7))
  (check "a sequence aborted by CAN prints nothing of itself" (row term 0) "abdef")
  (check "BEL counted" (term-bell-count term) 2))

(let ((term (make-term 30 5)) (state (make-random-state nil)))
  (check "64 KB of random bytes never signal"
         (handler-case
             (progn (dotimes (i 64)
                      (let ((v (make-array 1024 :element-type '(unsigned-byte 8))))
                        (dotimes (j 1024) (setf (aref v j) (random 256 state)))
                        (term-feed term v)))
                    :ok)
           (error (e) (princ-to-string e)))
         :ok))

;;; ---- resizing --------------------------------------------------------------------

(let ((term (make-term 10 4)))
  (feed term "a\\r\\nb\\r\\nc\\r\\nd")
  (term-resize term 10 2)
  (check "shrinking keeps the cursor's line, pushing the top to history"
         (list (term-screen-text term) (term-scrollback-count term) (term-cy term))
         '(("c" "d") 2 1))
  (term-resize term 10 4)
  (check "growing pulls history back" (list (term-screen-text term) (term-cy term))
         '(("a" "b" "c" "d") 3))
  (term-resize term 4 4)
  (check "narrowing truncates" (row term 0) "a"))

;;; ---- keys, mouse, paste --------------------------------------------------------------

(let ((term (make-term 80 24)))
  (check "up, normal" (encode-key term :up 0) (esc "\\e[A"))
  (feed term "\\e[?1h")
  (check "up, application cursor (DECCKM)" (encode-key term :up 0) (esc "\\eOA"))
  (check "ctrl-up has a modifier" (encode-key term :up 4) (esc "\\e[1;5A"))
  (check "shift-end" (encode-key term :end 1) (esc "\\e[1;2F"))
  (check "C-a" (encode-key term #\a 4) (string (code-char 1)))
  (check "C-Space is NUL" (encode-key term #\Space 4) (string (code-char 0)))
  (check "M-x" (encode-key term #\x 8) (esc "\\ex"))
  (check "Return" (encode-key term :return 0) (string #\Return))
  (check "Shift-Return is ESC CR (Claude Code's new line)" (encode-key term :return 1) (esc "\\e\\r"))
  (check "Backspace is DEL (kbs=^?)" (encode-key term :backspace 0) (string (code-char 127)))
  (check "shift-tab (kcbt)" (encode-key term :tab 1) (esc "\\e[Z"))
  (check "Delete" (encode-key term :delete 0) (esc "\\e[3~"))
  (check "ctrl-PageDown" (encode-key term :page-down 4) (esc "\\e[6;5~"))
  (check "F1" (encode-key term :f1 0) (esc "\\eOP"))
  (check "F5" (encode-key term :f5 0) (esc "\\e[15~"))
  (check "F12" (encode-key term :f12 0) (esc "\\e[24~")))

(let ((term (make-term 80 24)))
  (check "no mouse mode: no report" (encode-mouse term :press 0 4 2 0) nil)
  (feed term "\\e[?1000h")
  (check "X10 encoding" (encode-mouse term :press 0 4 2 0)
         (format nil "~c[M~c~c~c" #\Esc (code-char 32) (code-char 37) (code-char 35)))
  (check "1000 ignores motion" (encode-mouse term :motion 0 5 2 0) nil)
  (feed term "\\e[?1002h\\e[?1006h")
  (check "SGR press" (encode-mouse term :press 0 4 2 0) (esc "\\e[<0;5;3M"))
  (check "SGR drag (1002)" (encode-mouse term :motion 0 5 2 0) (esc "\\e[<32;6;3M"))
  (check "SGR release says which button" (encode-mouse term :release 2 5 2 0) (esc "\\e[<2;6;3m"))
  (check "SGR wheel with ctrl" (encode-mouse term :press :wheel-down 0 0 4) (esc "\\e[<81;1;1M")))

(let ((term (make-term 80 24)))
  (check "plain paste: newlines become CR" (encode-paste term (format nil "a~%b"))
         (format nil "a~cb" #\Return))
  (feed term "\\e[?2004h")
  (check "bracketed paste, an ESC inside dropped"
         (encode-paste term (format nil "x~c[201~~y" #\Esc)) (esc "\\e[200~x[201~y\\e[201~")))

;;; ---- recordings of real programs ----------------------------------------------------

(defun feed-until (term bytes marker)
  "Feed BYTES up to (not including) the first MARKER; return the index there."
  (let ((i (search (utf8-encode (esc marker)) bytes)))
    (term-feed term bytes :end i)
    i))

(let* ((bytes (bytes-of (data "less.typescript")))
       (term (make-term 80 24))
       (i (feed-until term bytes "\\r\\e[K\\e[?1l")))
  (check "less: on the alternate screen" (term-alt-screen-p term) t)
  (check "less: first and last lines of the page" (list (row term 0) (row term 22))
         '("line 1 of the file" "line 23 of the file"))
  (check "less: the prompt line, in inverse video"
         (list (row term 23) (logtest +inverse+ (attr-flags (aref (tline-attrs (term-line term 23)) 0))))
         '("numbers.txt" t))
  (check "less: smkx put the arrows in application mode" (term-app-cursor-p term) t)
  (term-feed term bytes :start i)
  (check "less: quitting restores the primary screen, normal arrows"
         (list (term-alt-screen-p term) (row term 0) (term-app-cursor-p term))
         '(nil "" nil)))

(let* ((bytes (bytes-of (data "vi.typescript")))
       (term (make-term 80 24))
       (i (feed-until term bytes "\\e[?2004l")))
  (check "vi: the file" (list (row term 0) (row term 1) (row term 2)) '("alpha" "beta" "gamma"))
  (check "vi: tildes past the end" (row term 3) "~")
  (check "vi: in a bright blue" (attr-fg (aref (tline-attrs (term-line term 3)) 0)) 13)
  (check "vi: the command line it typed" (row term 23) ":q")
  (term-feed term bytes :start i)
  (check "vi: gone, the cursor visible again"
         (list (term-alt-screen-p term) (term-cursor-visible-p term) (term-bracketed-paste-p term))
         '(nil t nil)))

(let* ((bytes (bytes-of (data "bash.typescript")))
       (term (make-term 80 24)))
  (term-feed term bytes)
  (check "bash: prompt and output" (list (row term 0) (row term 1) (row term 3))
         '("$" "hi" "red plain"))
  (check "bash: printf's colour reached the cell"
         (let ((a (aref (tline-attrs (term-line term 3)) 0))) (list (attr-flags a) (attr-fg a)))
         (list +bold+ 2))
  (check "bash: bracketed paste off after each prompt" (term-bracketed-paste-p term) nil))

(format t "~%~d/~d passed~%" (- *count* *failures*) *count*)
(sb-ext:exit :code (if (zerop *failures*) 0 1))
