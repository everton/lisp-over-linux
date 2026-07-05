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
