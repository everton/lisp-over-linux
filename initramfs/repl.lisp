;;;; repl.lisp — the Lisp read-eval-print loop, driven by the line editor.
;;;;
;;;; This is the "r) run a Lisp REPL" menu action, NOT the supervisor loop.
;;;; It uses read-line-edited (line-editor.lisp) for input on a real TTY, and
;;;; falls back to plain line-buffered READ when stdin is a pipe/file. Every
;;;; read and eval is guarded so a bad form cannot kill PID 1.

;;; ---- <Tab> symbol completion (the *completer* hook for line-editor.lisp) ----

(defun prefix-ci-p (prefix string)
  "True if STRING begins with PREFIX, case-insensitively."
  (and (<= (length prefix) (length string))
       (string-equal prefix string :end2 (length prefix))))

(defun complete-symbol (token)
  "Completions for the partial symbol TOKEN: symbols accessible in *package*, or
   the symbols of a named package given a 'pkg:' (external) / 'pkg::' (all) prefix,
   or keywords for a leading ':'. Case-insensitive; results are lowercased so they
   read back correctly under the default upcasing readtable."
  (when (plusp (length token))
    (let ((colon (position #\: token)) (names '()))
      (cond
        ((and colon (zerop colon))                       ; :keyword
         (let ((part (string-left-trim ":" token)))
           (do-symbols (s (find-package :keyword))
             (when (prefix-ci-p part (symbol-name s))
               (push (format nil ":~(~a~)" (symbol-name s)) names)))))
        (colon                                           ; pkg:sym / pkg::sym
         (let* ((dbl (and (< (1+ colon) (length token)) (char= (char token (1+ colon)) #\:)))
                (pname (subseq token 0 colon))
                (spart (subseq token (if dbl (+ colon 2) (1+ colon))))
                (pkg   (find-package (string-upcase pname))))
           (when pkg
             (if dbl
                 (do-symbols (s pkg)
                   (when (and (eq (symbol-package s) pkg) (prefix-ci-p spart (symbol-name s)))
                     (push (format nil "~(~a~)::~(~a~)" pname (symbol-name s)) names)))
                 (do-external-symbols (s pkg)
                   (when (prefix-ci-p spart (symbol-name s))
                     (push (format nil "~(~a~):~(~a~)" pname (symbol-name s)) names)))))))
        (t                                               ; accessible in *package*
         (do-symbols (s *package*)
           (when (prefix-ci-p token (symbol-name s))
             (push (string-downcase (symbol-name s)) names)))))
      (sort (delete-duplicates names :test #'string=) #'string<))))

(setf *completer* #'complete-symbol)     ; enable <Tab> completion in the editor

(defun complete-form (text)
  "Try to READ one form from TEXT. Returns a second value telling the caller what
   to do: :ok (with the form), :incomplete (unbalanced — read another line),
   :empty (only whitespace), or :error (with the condition)."
  (if (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) text)
      (values nil :empty)
      (handler-case (values (read-from-string text) :ok)
        (end-of-file ()        (values nil :incomplete))
        (serious-condition (c) (values c :error)))))

(defun editing-repl (in out)
  "The editing REPL: read a line, and keep reading continuation lines until a
   whole form is available (so multi-line forms work), then eval and print.
   Used both locally (under with-raw-mode, via run-repl) and over the network
   (net.lisp drives it on a socket stream — the host client supplies raw mode)."
  (loop
    (let ((text "") (prompt "lisp> ") (prompt-sgr (col-prompt)))
      (block one-form
        (loop
          (let ((line (read-line-edited in out prompt prompt-sgr)))
            (when (eq line :cancel)            ; Ctrl-C: abandon the in-progress form
              (return-from one-form))
            (when (eq line :eof)
              (if (string= text "")
                  (return-from editing-repl)   ; EOF at a fresh prompt: leave the REPL
                  (return-from one-form)))      ; EOF mid-form: drop it, start fresh
            (setf text (if (string= text "") line
                           (concatenate 'string text (string #\Newline) line)))
            (multiple-value-bind (form status) (complete-form text)
              (ecase status
                (:empty      (return-from one-form))             ; blank line: re-prompt
                (:incomplete (setf prompt "  ...> "               ; unbalanced: keep reading
                                   prompt-sgr (col-cont)))        ;   dim continuation prompt
                (:error      (format out "~&~aread error:~a ~a~%" (col-error) (col-reset) form)
                             (finish-output out) (return-from one-form))
                (:ok
                 (push-history text)
                 (when (eq form :quit) (return-from editing-repl))
                 (handler-case
                     (format out "~&~a=>~a ~s~%" (col-result) (col-reset) (eval form))
                   (serious-condition (c)
                     (format out "~&~aerror:~a ~a~%" (col-error) (col-reset) c)))
                 (finish-output out)
                 (return-from one-form))))))))))

(defun plain-repl (in out)
  "Fallback REPL when stdin is NOT a TTY (a pipe/file): ordinary line-buffered
   READ, no editing — the kernel still gives us backspace + line buffering."
  (let ((eof (list :eof)))
    (loop
      (format out "~&~alisp>~a " (col-prompt) (col-reset)) (finish-output out)
      (let ((form (handler-case (read in nil eof)
                    (serious-condition (c)
                      (format out "~&~aread error:~a ~a~%" (col-error) (col-reset) c) nil))))
        (cond
          ((or (eq form eof) (eq form :quit)) (return))
          ((null form))
          (t (handler-case
                 (format out "~&~a=>~a ~s~%" (col-result) (col-reset) (eval form))
               (serious-condition (c)
                 (format out "~&~aerror:~a ~a~%" (col-error) (col-reset) c)))))))))

(defun run-repl ()
  "A read-eval-print loop with in-line editing. :quit (or Ctrl-D on an empty
   line) returns to the menu; every read/eval is guarded so a bad form cannot
   kill PID 1. Uses the editor on a real TTY; falls back to plain READ otherwise."
  (let ((in *standard-input*) (out *standard-output*))
    (format out "~&~%Lisp REPL — arrows / Ctrl-A E B F K U W, Tab completes, Up/Down~%")
    (format out "history, :quit (or Ctrl-D on an empty line) to return to the menu.~%")
    (finish-output out)
    (if (tty-raw-capable-p 0)
        (with-raw-mode (0) (editing-repl in out))
        (plain-repl in out))))
