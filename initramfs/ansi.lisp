;;;; ansi.lisp — ANSI SGR color for the REPL and line editor.
;;;;
;;;; The framebuffer console (fbcon) is a VT/ANSI emulator, but the mainline
;;;; kernel VT only renders the SIXTEEN classic colors — the 8 normal (SGR
;;;; 30-37) plus the 8 bright/aixterm ones (90-97), optionally bold. It does
;;;; NOT honour 256-color (38;5;n) or 24-bit truecolor (38;2;r;g;b); those get
;;;; approximated to the nearest of the 16 or dropped. So every color here is
;;;; one of those 16 — which also renders faithfully on the network REPL's host
;;;; terminal (it supports more, but the 16-color subset is universal).
;;;;
;;;; Everything routes through SGR — "Select Graphic Rendition", ESC [ … m.
;;;; An SGR sequence has ZERO display width, so inserting color never disturbs
;;;; the line editor's cursor-column arithmetic (see line-editor.lisp).
;;;;
;;;; Loaded BEFORE line-editor.lisp (which colorizes typed input) and repl.lisp
;;;; (which colorizes prompts and results). No package: like the rest of the
;;;; userland this lives in CL-USER.

(defvar *ansi* t
  "When true, the REPL and line editor emit ANSI color. Bind or SETF to NIL to
   get plain monochrome output (e.g. when logging to a file) — every helper
   below then returns the empty string, so nothing else has to change.")

(defun sgr (&rest codes)
  "The ANSI Select-Graphic-Rendition escape for CODES, e.g. (sgr 1 92) => the
   string ESC[1;92m (bold bright-green). Returns \"\" when *ansi* is off. The
   result has zero display width, so it is safe to splice anywhere in a line
   without perturbing column counts."
  (if *ansi*
      (format nil "~C[~{~A~^;~}m" #\Escape codes)
      ""))

;;; Semantic colors — named once here so the scheme lives in a single place.
;;; Functions (not constants) so they honour *ansi* at call time.
(defun col-reset   () (sgr 0))            ; back to the terminal default
(defun col-prompt  () (sgr 1 96))         ; bold bright-cyan   — the live prompt
(defun col-cont    () (sgr 90))           ; dim gray           — continuation prompt
(defun col-result  () (sgr 1 92))         ; bold bright-green  — the => marker
(defun col-error   () (sgr 1 91))         ; bold bright-red    — errors
(defun col-string  () (sgr 32))           ; green              — "strings"
(defun col-comment () (sgr 90))           ; dim gray           — ; comments
(defun col-keyword () (sgr 95))           ; bright-magenta     — :keywords
(defun col-char    () (sgr 36))           ; cyan               — #\char literals

(defparameter *paren-cycle* #(93 95 96 92)
  "SGR codes cycled by nesting depth for rainbow parentheses: bright yellow,
   magenta, cyan, green. Four levels keeps adjacent depths visually distinct.")

(defun col-paren (depth)
  "The paren color for nesting DEPTH (0-based), cycling through *paren-cycle*."
  (sgr (aref *paren-cycle* (mod depth (length *paren-cycle*)))))

;;; ===========================================================================
;;; The other direction: READING SGR out of a program's output.
;;; ===========================================================================
;;;
;;; Everything above WRITES colour for a terminal to draw. A run pane (runs.lisp)
;;; is the opposite situation: a program such as git, rails or ruby writes ANSI
;;; colour because it believes it is on a terminal (we give it a pty, so it is),
;;; and WE are the terminal that has to draw it. We do not draw onto the kernel VT
;;; here but onto our own canvas, so we are not held to its sixteen colours: the
;;; 256-colour cube and 24-bit colour come out exactly.
;;;
;;; This is deliberately NOT a terminal emulator — a grid, cursor addressing and an
;;; alternate screen are the terminal pane's job (doc/containers.org §5.2 step 2).
;;; It is a LINE reader: text, newlines, colours, and just enough of the rest that
;;; common output reads right:
;;;   \r without \n   restart the line (progress bars overwrite themselves)
;;;   \b              erase the previous character
;;;   \t              to the next multiple of 8
;;;   ESC [ … m       SGR: colour and bold, which is all a log ever uses
;;;   any other ESC [ … X, ESC ] … BEL (window titles), ESC x   swallowed whole
;;;
;;; It is a STATE MACHINE fed in chunks, because a read(2) from a pipe can stop in
;;; the middle of an escape sequence: "ESC [ 3" now, "2 m" in the next chunk.
;;;
;;; A colour here is an integer #xRRGGBB, or NIL for "the pane's default text
;;; colour" — the reader does not know the palette it will be drawn on. A finished
;;; line is (STRING . RUNS), RUNS a list of (START END . COLOUR): the very shape
;;; textview.lisp's lexer produces, so the text view draws it unchanged.

(defparameter *ansi-palette*
  #(#x3B4048 #xE06C75 #x98C379 #xE5C07B #x61AFEF #xC678DD #x56B6C2 #xABB2BF   ; 0-7
    #x7F848E #xFF7B86 #xB5E890 #xFFD68A #x82C4FF #xDD9CF0 #x7FD4DF #xF2F2F2)  ; 8-15
  "The sixteen classic colours as RGB, tuned to read on the browser's dark text
   background. Black is lifted to a dark grey (black text on our near-black would
   vanish) — the same liberty every terminal theme takes.")

(defun ansi-256-rgb (n)
  "Colour N of the xterm 256-colour table: 0-15 the palette, 16-231 a 6x6x6 cube,
   232-255 a grey ramp. (The cube's steps are 0,95,135,175,215,255 — xterm's own.)"
  (cond ((< n 16) (aref *ansi-palette* n))
        ((< n 232) (let* ((i (- n 16))
                          (lv (lambda (c) (if (zerop c) 0 (+ 55 (* 40 c))))))
                     (logior (ash (funcall lv (floor i 36)) 16)
                             (ash (funcall lv (mod (floor i 6) 6)) 8)
                             (funcall lv (mod i 6)))))
        (t (let ((g (+ 8 (* 10 (- n 232))))) (logior (ash g 16) (ash g 8) g)))))

(defstruct (ansi-reader (:constructor make-ansi-reader ()))
  (line (make-array 80 :element-type 'character :adjustable t :fill-pointer 0))
  (runs '())          ; finished runs of the current line, newest first
  (run-start 0)       ; where the open run began
  (fg nil)            ; current foreground: an index 0-15, an RGB in a cons (rgb), or NIL
  (bold nil)
  (state :text)       ; :text :esc :csi :osc :osc-esc
  (params (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
  (cr nil))           ; saw \r: the next printable character restarts the line

(defun ansi-reader-colour (r)
  "The RGB the current SGR state draws in, or NIL for the default. Bold brightens
   colours 0-7 into 8-15, which is what terminals did before 'bright' had its own
   codes and what ls and git still rely on."
  (let ((fg (ansi-reader-fg r)))
    (cond ((null fg) (if (ansi-reader-bold r) (aref *ansi-palette* 15) nil))
          ((consp fg) (car fg))
          ((and (ansi-reader-bold r) (< fg 8)) (aref *ansi-palette* (+ fg 8)))
          (t (aref *ansi-palette* fg)))))

(defun ansi-close-run (r)
  "End the open run at the current length: a colour change or the end of a line."
  (let ((end (length (ansi-reader-line r))))
    (when (< (ansi-reader-run-start r) end)
      (push (list* (ansi-reader-run-start r) end (ansi-reader-colour r)) (ansi-reader-runs r)))
    (setf (ansi-reader-run-start r) end)))

(defun ansi-apply-sgr (r param-string)
  "Apply one SGR parameter list (\"1;32\", \"38;5;208\", \"\") to the reader's state."
  (let ((ps (let ((out '()) (start 0))
              (loop for semi = (position #\; param-string :start start)
                    do (push (or (parse-integer param-string :start start :end semi
                                                :junk-allowed t) 0)
                             out)
                       (if semi (setf start (1+ semi)) (return)))
              (nreverse out))))
    (ansi-close-run r)
    (loop while ps do
      (let ((p (pop ps)))
        (cond ((= p 0) (setf (ansi-reader-fg r) nil (ansi-reader-bold r) nil))
              ((= p 1) (setf (ansi-reader-bold r) t))
              ((= p 22) (setf (ansi-reader-bold r) nil))
              ((<= 30 p 37) (setf (ansi-reader-fg r) (- p 30)))
              ((= p 39) (setf (ansi-reader-fg r) nil))
              ((<= 90 p 97) (setf (ansi-reader-fg r) (+ 8 (- p 90))))
              ;; extended colours: 38 sets the foreground, 48 the background (which
              ;; we do not paint, but its arguments must still be consumed)
              ((member p '(38 48))
               (let ((mode (pop ps)))
                 (cond ((eql mode 5)
                        (let ((n (pop ps)))
                          (when (and (= p 38) n) (setf (ansi-reader-fg r) (list (ansi-256-rgb (min n 255)))))))
                       ((eql mode 2)
                        (let ((red (pop ps)) (green (pop ps)) (blue (pop ps)))
                          (when (and (= p 38) blue)
                            (setf (ansi-reader-fg r)
                                  (list (logior (ash (min red 255) 16) (ash (min green 255) 8)
                                                (min blue 255))))))))))
              (t nil))))))          ; underline, background, blink …: not drawn

(defun ansi-take-line (r)
  "The finished current line as (STRING . RUNS), and a fresh one started. The SGR
   state carries over — a colour set on one line is still on at the next."
  (ansi-close-run r)
  (prog1 (cons (coerce (ansi-reader-line r) 'simple-string) (reverse (ansi-reader-runs r)))
    (setf (fill-pointer (ansi-reader-line r)) 0
          (ansi-reader-runs r) '()
          (ansi-reader-run-start r) 0)))

(defun ansi-reader-partial (r)
  "The line still being written, as (STRING . RUNS), without finishing it — for
   showing a prompt or a progress bar before its newline arrives."
  (let ((line (ansi-reader-line r)))
    (cons (coerce line 'simple-string)
          (reverse (if (< (ansi-reader-run-start r) (length line))
                       (cons (list* (ansi-reader-run-start r) (length line) (ansi-reader-colour r))
                             (ansi-reader-runs r))
                       (ansi-reader-runs r))))))

(defun ansi-feed (r string emit)
  "Feed STRING (any chunk of output) through the reader; call EMIT with each line
   it finishes, as (STRING . RUNS)."
  (flet ((put (ch)
           (when (ansi-reader-cr r)              ; \r then text: overwrite the line
             (setf (ansi-reader-cr r) nil)
             (setf (fill-pointer (ansi-reader-line r)) 0
                   (ansi-reader-runs r) '() (ansi-reader-run-start r) 0))
           (vector-push-extend ch (ansi-reader-line r))))
    (loop for ch across string do
      (ecase (ansi-reader-state r)
        (:text
         (case ch
           (#\Escape (setf (ansi-reader-state r) :esc))
           (#\Newline (setf (ansi-reader-cr r) nil) (funcall emit (ansi-take-line r)))
           (#\Return (setf (ansi-reader-cr r) t))
           (#\Backspace (let ((line (ansi-reader-line r)))
                          (when (> (length line) (ansi-reader-run-start r))
                            (decf (fill-pointer line)))))
           (#\Tab (loop do (put #\Space)
                        until (zerop (mod (length (ansi-reader-line r)) 8))))
           (t (when (>= (char-code ch) 32) (put ch)))))
        (:esc
         (setf (ansi-reader-state r)
               (case ch (#\[ (setf (fill-pointer (ansi-reader-params r)) 0) :csi)
                        (#\] :osc)
                        (t :text))))                    ; ESC x: a two-byte sequence, dropped
        (:csi
         ;; parameters and intermediates are 0x20-0x3F; the final byte is 0x40-0x7E
         (if (<= #x40 (char-code ch) #x7E)
             (progn (when (char= ch #\m)
                      (ansi-apply-sgr r (coerce (ansi-reader-params r) 'simple-string)))
                    (setf (ansi-reader-state r) :text))
             (vector-push-extend ch (ansi-reader-params r))))
        (:osc                                           ; ESC ] … ends at BEL or ESC \
         (case ch (#\Bel (setf (ansi-reader-state r) :text))
                  (#\Escape (setf (ansi-reader-state r) :osc-esc))))
        (:osc-esc (setf (ansi-reader-state r) :text))))))
