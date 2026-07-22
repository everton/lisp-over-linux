;;;; registry.lisp — the definition registry: the source lives IN the image.
;;;;
;;;; This is Phase 0 of the code browser (doc/code-browser.org §2). The problem
;;;; it solves: build.sh bakes the .lisp files into the frozen lisp-init and
;;;; ships only the binary — the sources are NOT in the running system, and
;;;; sb-introspect's source locations point at the build host's paths, which do
;;;; not exist in the guest. So a browser that wanted to show draw-alien's code
;;;; would have nothing to read.
;;;;
;;;; The fix: at BUILD time, load each module with LOAD-RECORDING instead of LOAD.
;;;; It loads the file exactly as before (so behaviour, compilation and xref are
;;;; unchanged) and, as a separate non-evaluating read pass, captures the verbatim
;;;; source text of every top-level form into *REGISTRY*, keyed by definition
;;;; name. save-lisp-and-die then freezes that table into the heap. At RUNTIME the
;;;; REPL can ask the image to show its own source:  (show-source 'draw-alien).
;;;;
;;;; Loaded (with plain LOAD) FIRST in build.sh, before the recorded modules,
;;;; because it defines LOAD-RECORDING. Needs sb-introspect required before it
;;;; (SENDERS wraps sb-introspect:who-calls). Like the rest of the userland it
;;;; lives in CL-USER.

(defstruct defn
  "One captured top-level definition."
  name          ; a symbol, or (SETF sym)
  kind          ; :function :macro :generic-function :method :variable :constant
                ; :class :condition :struct :type :package :compiler-macro
  source        ; the verbatim text, comments and formatting intact
  file          ; build-time provenance (the host path — for reference only)
  label         ; for methods: qualifiers + specializers, to tell them apart
  versions)     ; previously-accepted source strings, most-recent first (Smalltalk Versions)

(defvar *registry* (make-hash-table :test 'equal)
  "Maps a definition NAME to a list of DEFN records (most-recent first). One name
   can carry several — a generic function plus each of its methods, say. EQUAL
   test so (SETF FOO) list-names key correctly alongside plain symbols.")

(defun read-file-into-string (path)
  "Slurp PATH into a string. (No alexandria in the image; this is the one use.)"
  (with-open-file (s path :element-type 'character)
    (let ((buf (make-string (file-length s))))       ; file-length is a byte upper
      (subseq buf 0 (read-sequence buf s)))))          ; bound; trim to chars read

(defun defn-name-and-kind (form)
  "For a top-level FORM, return (values NAME KIND LABEL), or NIL if it is not a
   definition we track (it still loads — it just is not in the registry)."
  (when (and (consp form) (symbolp (first form)))
    (flet ((specializers (lambda-list)
             (let ((req (loop for p in lambda-list
                              until (member p lambda-list-keywords) collect p)))
               (format nil "(~{~a~^ ~})"
                       (mapcar (lambda (p) (if (consp p) (second p) t)) req)))))
      (case (first form)
        ((defun)                (values (second form) :function nil))
        ((defmacro)             (values (second form) :macro nil))
        ((defgeneric)           (values (second form) :generic-function nil))
        ((define-compiler-macro)(values (second form) :compiler-macro nil))
        ((defmethod)
         ;; (defmethod NAME qualifier* (lambda-list) . body)
         (let* ((rest  (cddr form))
                (quals (loop while (and rest (not (listp (car rest))))
                             collect (pop rest)))
                (ll    (car rest)))
           (values (second form) :method
                   (string-trim " " (format nil "~{~a ~}~a" quals (specializers ll))))))
        ((defvar defparameter)  (values (second form) :variable nil))
        ((defconstant)          (values (second form) :constant nil))
        ((defclass)             (values (second form) :class nil))
        ((define-condition)     (values (second form) :condition nil))
        ((defstruct)            (values (if (consp (second form))
                                            (first (second form)) (second form))
                                        :struct nil))
        ((deftype)              (values (second form) :type nil))
        ((defpackage)           (values (second form) :package nil))
        (t nil)))))

(defun register-source (form text file)
  "Record FORM's verbatim TEXT under its definition name, if it is one we track."
  (multiple-value-bind (name kind label) (defn-name-and-kind form)
    (when name
      (push (make-defn :name name :kind kind :file file :label label
                       :source (string-trim '(#\Newline #\Space #\Tab) text))
            (gethash name *registry*)))))

(defun load-recording (path)
  "LOAD PATH, and additionally capture each top-level form's source into
   *REGISTRY*. LOAD runs FIRST so every package the file defines already exists;
   then a SEPARATE read pass (with *READ-EVAL* off, so it runs no code) captures the
   verbatim source. That pass tracks IN-PACKAGE — binding *PACKAGE* locally and
   switching it on each (in-package …) — so every definition is filed under its REAL
   package, not CL-USER (without this, an in-package file's symbols all land in
   CL-USER and its own package browses empty). Any capture failure degrades to 'that
   form is not in the registry' — never to a broken build.

   Note: the span from one form's end to the next form's end includes the blank
   lines and comments between them, so a doc-comment written above a DEFUN is
   captured WITH it — exactly where it belongs (doc/code-browser.org §2)."
  (load path)                                    ; execute first: packages now exist
  (let ((text (ignore-errors (read-file-into-string path))))
    (when text
      (ignore-errors
        (with-input-from-string (s text)
          (let ((*read-eval* nil) (*package* (find-package :cl-user)))
            (loop for start = (file-position s)
                  for form = (handler-case (read s nil :eof)
                               (serious-condition () :eof))   ; stop cleanly on a form we can't read
                  until (eq form :eof)
                  do (register-source form (subseq text start (file-position s)) path)
                     ;; follow IN-PACKAGE so the NEXT forms intern in the right package
                     (when (and (consp form) (symbolp (first form))
                                (string= (symbol-name (first form)) "IN-PACKAGE"))
                       (let ((p (ignore-errors (find-package (second form)))))
                         (when p (setf *package* p))))))))))
  (values))

;;; ---- Accept: compile edited source into the live image + update the registry --

(defun accept-source (text &optional name kind label)
  "Compile TEXT (an edited definition) into the RUNNING image and update the
   registry: eval the form (redefining it live — the Smalltalk 'accept'), and set
   its recorded source, pushing the previous source onto the version history. NAME,
   KIND and LABEL (from the browser pane's subject) are fallbacks — the form is
   authoritative when it names a definition; LABEL (a method's specializer label)
   keeps Accept on the right method when the form's own head isn't enough. Returns the
   name compiled. Signals on a read/compile error (the caller reports it)."
  (let* ((form   (read-from-string text))
         (result (eval form)))                  ; compile/redefine in the live image
    (multiple-value-bind (fname fkind flabel) (defn-name-and-kind form)
      (let* ((the-name  (or fname name))
             (the-kind  (or fkind kind))
             (the-label (or flabel label))      ; a method's specializer label, else NIL
             (trimmed   (string-trim '(#\Newline #\Space #\Tab) text)))
        (when the-name
          (let* ((defns    (gethash the-name *registry*))
                 ;; a name can hold MANY methods, so a :method must match by its
                 ;; specializer label — otherwise Accept would overwrite the source of
                 ;; some other method of the same generic function.
                 (existing (if (eq the-kind :method)
                               (find-if (lambda (d)
                                          (and (eq (defn-kind d) :method)
                                               (equal (defn-label d) the-label)))
                                        defns)
                               (find the-kind defns :key #'defn-kind))))
            (if existing
                (progn
                  (push (defn-source existing) (defn-versions existing))
                  (setf (defn-source existing) trimmed))
                (push (make-defn :name the-name :kind the-kind :label the-label :source trimmed)
                      (gethash the-name *registry*)))))
        (or the-name result)))))

;;; ---- runtime queries (what the REPL / browser ask) -----------------------

(defun definitions-of (name &optional kind)
  "All DEFN records for NAME, optionally filtered to KIND."
  (let ((defns (reverse (gethash name *registry*))))    ; source order
    (if kind (remove kind defns :key #'defn-kind :test-not #'eq) defns)))

(defun source-of (name &optional kind)
  "The captured source TEXT of NAME. A single string when there is one definition;
   a list of strings when several share the name (e.g. a GF and its methods).
   NIL if NAME was never recorded."
  (let ((defns (definitions-of name kind)))
    (cond ((null defns) nil)
          ((null (cdr defns)) (defn-source (first defns)))
          (t (mapcar #'defn-source defns)))))

(defun where-defined (name kind)
  "The source pathname SBCL records for NAME — a location it knows but that we do
   NOT ship (the core is compiled; its src tree is not in the image). NIL if none."
  (ignore-errors
    (let ((srcs (sb-introspect:find-definition-sources-by-name name kind)))
      (and srcs (sb-introspect:definition-source-pathname (first srcs))))))

(defun show-reference (name)
  "The fallback for a symbol NOT in our registry — a CL/SBCL builtin, or anything
   compiled into the core. There is no source TEXT to show (the core is machine
   code), so print what the image CAN tell us: kind, lambda list, docstring, and
   where SBCL says it lives. Reference-only — unlike your own code there is nothing
   to edit-and-Accept, though the notes say how you *could* experiment. Keeps the
   browser from dead-ending on a builtin (doc/code-browser.org: 'no dead ends')."
  (let ((k (cond ((special-operator-p name)                                  :special-operator)
                 ((macro-function name)                                      :macro)
                 ((and (fboundp name)
                       (typep (fdefinition name) 'generic-function))         :generic-function)
                 ((fboundp name)                                             :function)
                 ((find-class name nil)                                      :class)
                 ((boundp name)                                              :variable)
                 (t nil))))
    (if (null k)
        (format t "~&; ~s is not defined in this image.~%" name)
        (progn
          (format t "~&; ~(~a~) ~s   [reference — compiled into the core, no source shipped]~%" k name)
          (let ((ll (ignore-errors (sb-introspect:function-lambda-list name))))
            (when ll (format t ";   lambda list: ~a~%" ll)))
          (let ((doc (or (documentation name 'function) (documentation name 'variable))))
            (when doc (format t ";   doc: ~a~%" doc)))
          (let ((where (where-defined name (if (eq k :variable) :variable :function))))
            (when where (format t ";   SBCL defines it at: ~a  (not on disk here)~%" where)))
          (case k
            (:special-operator
             (format t ";   a special operator — the compiler owns it; genuinely not redefinable.~%"))
            ((:function :generic-function :macro)
             (format t ";   disassemble: (disassemble #'~(~a~))~%" name)
             (format t ";   experimental redefine: (sb-ext:without-package-locks (defun ~(~a~) …));~%" name)
             (format t ";     but inlined call sites won't see it, and breaking it panics PID 1.~%")))))
    (finish-output)
    name))

(defun show-source (name &optional kind)
  "Print the source of NAME to *standard-output* (the readable REPL demo — unlike
   SOURCE-OF, which returns the raw string). For our own recorded code this is the
   captured text; for anything else (a CL/SBCL builtin) it falls back to
   SHOW-REFERENCE, so a builtin is browsable, not a dead end. Returns NAME."
  (let ((defns (definitions-of name kind)))
    (if (null defns)
        (show-reference name)
        (progn
          (dolist (d defns)
            (format t "~&; ~(~a~) ~s~@[  ~a~]  [~a]~%~a~%~%"
                    (defn-kind d) (defn-name d) (defn-label d)
                    (file-namestring (defn-file d)) (defn-source d)))
          (finish-output))))
  name)

(defun all-definitions ()
  "Every recorded (name . kind), for building the browser's lists later."
  (let ((out '()))
    (maphash (lambda (name defns)
               (dolist (d defns) (push (cons name (defn-kind d)) out)))
             *registry*)
    (sort out #'string-lessp :key (lambda (nc) (princ-to-string (car nc))))))

;;; ---- cross-reference wrappers (sb-introspect) ----------------------------
;;; These cover our own code (xref is recorded when build.sh loads it); they will
;;; disappoint on built-in CL functions, which is expected.

(defun changed-definitions ()
  "The session's CHANGE SET (Smalltalk's Changes): every name whose source has been
   Accepted — a live edit that pushed the old text onto its VERSIONS. At boot every
   defn carries only its build-time source (no versions), so a non-empty VERSIONS is
   exactly 'edited since boot'. Returns (NAME . VERSION-COUNT) pairs."
  (let (out)
    (maphash (lambda (name defns)
               (let ((vers (reduce #'+ defns :key (lambda (d) (length (defn-versions d))))))
                 (when (plusp vers) (push (cons name vers) out))))
             *registry*)
    (sort out #'string-lessp :key (lambda (p) (princ-to-string (car p))))))

(defun senders (name)
  "Names of functions that CALL NAME — Smalltalk's 'senders'."
  (mapcar #'car (sb-introspect:who-calls name)))

(defun referrers (name)
  "Names of functions that reference the VARIABLE NAME."
  (mapcar #'car (sb-introspect:who-references name)))

(defun specializers-of (class-name)
  "Methods that specialize on CLASS-NAME (the class side of dispatch)."
  (mapcar #'car (sb-introspect:who-specializes-directly (find-class class-name))))
