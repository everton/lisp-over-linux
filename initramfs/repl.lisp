;;;; repl.lisp — the Lisp read-eval-print loop, driven by the line editor.
;;;;
;;;; This is the "r) run a Lisp REPL" menu action, NOT the supervisor loop.
;;;; It uses read-line-edited (line-editor.lisp) for input on a real TTY, and
;;;; falls back to plain line-buffered READ when stdin is a pipe/file. Every
;;;; read and eval is guarded so a bad form cannot kill PID 1.

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
