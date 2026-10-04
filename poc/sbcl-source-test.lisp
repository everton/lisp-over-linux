;;;; sbcl-source-test.lisp — host test for initramfs/sbcl-source.lisp.
;;;;
;;;;   sbcl --script poc/sbcl-source-test.lisp       ; exits 0 if all pass
;;;;
;;;; Needs the ./sbcl tree's sources on disk (they are, on the build host: the
;;;; core's SYS host points at it). Two halves: the textual scanner on made-up
;;;; text (the reader's corner cases), then real lookups into SBCL's source.

(require :sb-introspect)
(load (merge-pathnames "../initramfs/sbcl-source.lisp" *load-truename*))

(defvar *fails* 0)
(defvar *count* 0)
(defun check (label ok)
  (incf *count*)
  (unless ok (incf *fails*))
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok label))

(format t "~&== the scanner ==~%")
(flet ((cut (text) (values (form-at text 0))))
  (check "a plain list" (string= (cut "  (a (b c) d) (e)") "(a (b c) d)"))
  (check "a ) inside a string" (string= (cut "(f \")\" x) y") "(f \")\" x)"))
  (check "an escaped quote inside a string" (string= (cut "(f \"a\\\"b)\") y") "(f \"a\\\"b)\")"))
  (check "#\\( and #\\) characters" (string= (cut "(list #\\( #\\)) z") "(list #\\( #\\))"))
  (check "a ; comment holding a )" (string= (cut (format nil "(a ; )~% b) c")) (format nil "(a ; )~% b)")))
  (check "a nested #| |# comment before" (string= (cut "#| x #| y |# ) |# (k) (l)") "(k)"))
  (check "#+feature form is one form" (string= (cut "#+sbcl (a) (b)") "#+sbcl (a)"))
  (check "#-(or a b) form is one form" (string= (cut "#-(or a b) (a) (b)") "#-(or a b) (a)"))
  (check "|sym|bols with ( in them" (string= (cut "(f |a(b| c) d") "(f |a(b| c)"))
  (check "quote and backquote prefixes" (string= (cut "'(a ,@b `(c)) x") "'(a ,@b `(c))"))
  (check "an unbalanced form is refused" (null (cut "(a (b c)")))
  (check "the Nth top-level form" (= (nth-form-start (format nil "(a) ; c~%(b) #+x (c) (d)") 3) 20)))

(format t "~&== SBCL's own source (root ~a) ==~%" *sbcl-source-root*)
(flet ((starts (name prefix &optional kind)
         (multiple-value-bind (text path line) (sbcl-definition-source name kind)
           (check (format nil "~(~s~) -> ~a:~a" name (and path (file-namestring path)) line)
                  (and text (eql 0 (search prefix text)))))))
  (starts 'car "(defun car (list)")
  (starts 'mapcar "(macrolet ((define-list-map")      ; defined by a local macro
  (starts 'hash-table "(sb-xc:defstruct (hash-table" :structure)
  (starts 'stream "(defclass stream (t)" :class)
  (starts '*print-base* "(defvar *print-base*" :variable)
  (starts 'loop "(sb-xc:defmacro loop")
  (starts 'cell-error-name "(define-condition cell-error") ; a reader, inside its condition
  (starts 'sb-introspect:who-calls "(defun who-calls")) ; a contrib: SYS:CONTRIB;…

(multiple-value-bind (text why) (sbcl-definition-source 'if)
  (check (format nil "a special operator has no source, and says why: ~a" why)
         (and (null text) (search "no source location" why))))
(let ((*sbcl-source-root* "/nonexistent/"))
  (multiple-value-bind (text why) (sbcl-definition-source 'car)
    (check (format nil "a missing tree is reported, not signalled: ~a" why)
           (and (null text) (search "not on this disk" why)))))

(let ((found 0) (total 0))
  (do-external-symbols (s :cl)
    (when (or (fboundp s) (find-class s nil) (boundp s))
      (incf total)
      (when (sbcl-definition-source s) (incf found))))
  (format t "~&  every external CL symbol: ~a of ~a found~%" found total)
  ;; 893 of 935 against 2.6.8: the misses are special operators and built-in types
  ;; (no location recorded) and three names DEFSTRUCT generates (never in the text)
  (check "at least 890 external CL symbols resolve" (>= found 890)))

(format t "~&~a/~a passed~%" (- *count* *fails*) *count*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
