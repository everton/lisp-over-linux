;;;; regen.lisp — re-derive the guide's facts from the code, and check its links.
;;;;
;;;;   sbcl --script doc/guide/regen.lisp          (from anywhere; it finds the repo)
;;;;
;;;; The HTML guide in doc/guide/ makes two kinds of claim that rot silently when
;;;; the code moves: "function F lives at file:line", and "module A uses module B".
;;;; Neither is written by hand. This script derives both, from the source text:
;;;;
;;;;   1. A DEFINITION INDEX. Every top-level (def… NAME …) form in every Lisp file,
;;;;      keyed by (package . name), with its line. Plain line scanning, no READ:
;;;;      reading our files needs their packages to exist, and SBCL-specific reader
;;;;      syntax (!#RRGGBB colours, canvas.lisp) would need the real readtable. What
;;;;      a definition IS at the top level is regular enough for a scanner.
;;;;
;;;;   2. A MODULE DEPENDENCY GRAPH (deps-data.js, read by modules.html). For each
;;;;      file, every symbol it MENTIONS is looked up in the index; a mention of a
;;;;      name defined in another file is an edge. "Mentions" is deliberately
;;;;      conservative, so the graph errs towards missing an edge rather than
;;;;      inventing one:
;;;;        - a function/macro/accessor counts only in CALL position "(name", or
;;;;          quoted "#'name" / "'name" (FUNCALL 'NAME is how a module reaches one
;;;;          loaded after it);
;;;;        - a variable (defvar/defparameter/defconstant) counts anywhere;
;;;;        - a type (defstruct/defclass/define-condition) counts quoted, or as a
;;;;          bare word only if hyphenated — `canvas`, `item`, `pane` are also
;;;;          everybody's local variable names, `subj-defn` is not;
;;;;        - pkg:name counts always (and makes an edge to the file that defines
;;;;          the package even when the name itself was not found).
;;;;      Comments, strings and #| |# are stripped first, so a docstring that
;;;;      MENTIONS a function is not a dependency on it.
;;;;      A name defined in several files of the SAME package is ambiguous and is
;;;;      skipped — except a generic function, which belongs to the file holding
;;;;      its DEFGENERIC (methods elsewhere are users of it).
;;;;
;;;;   3. LINK CHECKING + REWRITING. Every <a class="ref" data-ref="FILE#WHAT"> in
;;;;      doc/guide/*.html is regenerated: href made relative, text = name plus
;;;;      FILE:LINE. WHAT is NAME (a definition), NAME@SPEC (a method, by its first
;;;;      specializer), or "LITERAL" (the first line containing that text — for C,
;;;;      shell, org). A ref that does not resolve is an ERROR and the script exits
;;;;      non-zero, so a renamed function breaks the build of the guide loudly
;;;;      instead of leaving it quietly wrong. That is the guide's "verify, don't
;;;;      assert": every file:line in it was put there by this script.
;;;;
;;;; It also flags LATE-BOUND edges: A uses B but B is loaded AFTER A in build.sh's
;;;; LISP_SOURCES. Legal for a function called at run time (the call is resolved
;;;; when it happens), fatal for a macro (expanded when A is compiled). The guide's
;;;; module page shows them dashed. Host-only tooling; never loaded into the image.

(defpackage :guide-regen (:use :cl))
(in-package :guide-regen)

(defparameter *guide-dir* (make-pathname :name nil :type nil :defaults *load-truename*))
(defparameter *root* (merge-pathnames "../../" *guide-dir*))

;;; ---- small text helpers -----------------------------------------------------

(defun slurp (path)
  (with-open-file (s path :external-format :utf-8)
    (let* ((buf (make-string (file-length s))) (n (read-sequence buf s)))
      (subseq buf 0 n))))

(defun lines-of (text)
  (let ((out '()) (start 0))
    (loop for nl = (position #\Newline text :start start)
          do (push (subseq text start (or nl (length text))) out)
             (if nl (setf start (1+ nl)) (return)))
    (coerce (nreverse out) 'vector)))

(defun rel (path)
  "PATH as a string relative to the repository root."
  (enough-namestring (truename path) (truename *root*)))

(defun starts-with (prefix s)
  (and (>= (length s) (length prefix)) (string= prefix s :end2 (length prefix))))

(defun ends-with (suffix s)
  (and (>= (length s) (length suffix)) (string= suffix s :start2 (- (length s) (length suffix)))))

(defun delim-p (c)
  (member c '(#\Space #\Tab #\Newline #\Return #\( #\) #\' #\` #\, #\" #\;)))

(defun token-at (s i)
  "The token in S starting at I (up to a delimiter), and the index after it."
  (let ((j (or (position-if #'delim-p s :start i) (length s))))
    (values (subseq s i j) j)))

(defun skip-ws (s i)
  (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) s :start i) (length s)))

(defun norm-package (name)
  "\":lol.canvas\" / \"#:lol.canvas\" / \"LOL.CANVAS\" -> \"lol.canvas\"."
  (string-downcase (string-left-trim "#:" name)))

;;; ---- 1. the definition index ------------------------------------------------

(defstruct def file line pkg name kind spec)

(defparameter +fn-kinds+
  '("defun" "defmacro" "defgeneric" "defmethod" "define-alien-routine"
    "define-compiler-macro" "accessor" "constructor"))
(defparameter +var-kinds+ '("defvar" "defparameter" "defconstant" "define-symbol-macro"))
(defparameter +type-kinds+ '("defstruct" "defclass" "define-condition" "deftype"))

(defun parse-def-line (line)
  "If LINE opens a top-level definition, (values KIND NAME SPEC), else NIL.
   KIND is the def operator without its package prefix, lowercased."
  (when (and (plusp (length line)) (char= (char line 0) #\())
    (multiple-value-bind (head i) (token-at line 1)
      (let* ((head (string-downcase head))
             (head (let ((c (position #\: head :from-end t))) (if c (subseq head (1+ c)) head))))
        (when (and (or (starts-with "def" head) (string= head "define-alien-routine"))
                   (not (string= head "defpackage")))
          (let ((i (skip-ws line i)) name)
            (when (>= i (length line)) (return-from parse-def-line nil))
            (cond
              ((char= (char line i) #\()
               (let ((k (skip-ws line (1+ i))))
                 (cond ((and (< k (length line)) (char= (char line k) #\"))   ; ("open" %open)
                        (let ((e (position #\" line :start (1+ k))))
                          (setf name (token-at line (skip-ws line (1+ e))))))
                       ((starts-with "setf " (string-downcase (subseq line k)))
                        (setf name (format nil "(setf ~a)" (token-at line (skip-ws line (+ k 5))))))
                       (t (setf name (token-at line k))))))
              (t (multiple-value-bind (tok j) (token-at line i)
                   (setf name tok i j))))
            (let ((spec nil))
              (when (string= head "defmethod")
                ;; skip NAME, then qualifiers, to the lambda list; SPEC = the first
                ;; required parameter's specializer, or "t"
                (let* ((j (nth-value 1 (token-at line (skip-ws line i))))
                       (j (loop for k = (skip-ws line j)
                                while (and (< k (length line)) (char/= (char line k) #\())
                                do (setf j (nth-value 1 (token-at line k)))
                                   (when (= j k) (return k))
                                finally (return k))))
                  (when (and (< j (length line)) (char= (char line j) #\())
                    (let ((k (skip-ws line (1+ j))))
                      (setf spec
                            (if (and (< k (length line)) (char= (char line k) #\())
                                (let* ((v (nth-value 1 (token-at line (1+ k))))
                                       (v (skip-ws line v)))
                                  (if (and (< v (length line)) (char= (char line v) #\())
                                      "eql"
                                      (token-at line v)))
                                "t"))))))
              (values head (string-downcase name) (and spec (string-downcase spec))))))))))

(defvar *defs* '() "Every DEF found.")
(defvar *package-file* (make-hash-table :test 'equal) "package name -> defining file.")
(defvar *package-uses* (make-hash-table :test 'equal) "package -> list of used packages.")
(defvar *struct-prefixes* '() "(prefix pkg file) for defstruct accessors.")

(defun scan-defpackage (text file)
  "Record every DEFPACKAGE in TEXT: its name -> FILE, and its :USE list."
  (let ((start 0))
    (loop for p = (search "(defpackage" text :start2 start)
          while p do
            (multiple-value-bind (name j) (token-at text (skip-ws text (+ p 11)))
              (let* ((pkg (norm-package name))
                     (end (or (search "(in-package" text :start2 j) (length text)))
                     (u (search "(:use" text :start2 j :end2 end)))
                (setf (gethash pkg *package-file*) file)
                (when u
                  (let ((close (position #\) text :start u)))
                    (setf (gethash pkg *package-uses*)
                          (loop with k = (+ u 5)
                                for s = (position-if-not (lambda (c) (member c '(#\Space #\Newline #\Tab))) text :start k)
                                while (and s (< s close))
                                collect (multiple-value-bind (tok e) (token-at text s)
                                          (setf k e) (norm-package tok)))))))
              (setf start j)))))

(defun scan-file-defs (path)
  (let* ((file (rel path)) (text (slurp path)) (lines (lines-of text))
         (pkg "cl-user") (in-class nil))
    (scan-defpackage text file)
    (loop for line across lines for n from 1 do
      ;; follow (in-package …) so each definition is keyed by its real package
      (when (starts-with "(in-package" line)
        (setf pkg (norm-package (string-right-trim ")" (token-at line (skip-ws line 11))))))
      (when (and (plusp (length line)) (char= (char line 0) #\())
        (setf in-class nil))
      (multiple-value-bind (kind name spec) (parse-def-line line)
        (when kind
          (push (make-def :file file :line n :pkg pkg :name name :kind kind :spec spec) *defs*)
          (when (member kind '("defclass" "define-condition") :test #'string=)
            (setf in-class t))
          (when (string= kind "defstruct")
            ;; accessors: the conc-name prefix; constructors: :constructor or make-NAME
            (let* ((head (with-output-to-string (o)
                           (loop for k from (1- n) below (min (length lines) (+ n 3))
                                 do (write-line (aref lines k) o))))
                   (cn (search ":conc-name" head))
                   (prefix (if cn (string-downcase (token-at head (skip-ws head (+ cn 10))))
                               (format nil "~a-" name)))
                   (ctor (search ":constructor" head)))
              (unless (string= prefix "nil") (push (list prefix pkg file) *struct-prefixes*))
              (dolist (c (list (format nil "make-~a" name)
                               (and ctor (string-downcase (token-at head (skip-ws head (+ ctor 12)))))
                               (format nil "~a-p" name) (format nil "copy-~a" name)))
                (when (and c (plusp (length c)))
                  (push (make-def :file file :line n :pkg pkg :name c :kind "constructor") *defs*)))))))
      ;; CLOS slot accessors inside a defclass / define-condition
      (when in-class
        (dolist (key '(":accessor" ":reader" ":writer"))
          (let ((p (search key line)))
            (when p
              (push (make-def :file file :line n :pkg pkg :kind "accessor"
                              :name (string-downcase (token-at line (skip-ws line (+ p (length key))))))
                    *defs*))))))))

;;; ---- 2. mentions: strip comments/strings, tokenize with a context -------------

;;; "(name" is a call — except where a list is a BINDING rather than a form:
;;; (let ((present 0)) …) binds a variable called PRESENT, and (dolist (item …))
;;; one called ITEM. The first version counted those as calls and drew an edge from
;;; bdf2font.lisp to present.lisp. So the tokenizer keeps a stack of open lists,
;;; each knowing its head and how many elements it has seen, and classifies a new
;;; list by where it opens:
;;;   :bindings     the first argument of LET-like forms: ((a 1) (b 2))
;;;   :binding      one entry of that: (a 1) — A is a name, not a call
;;;   :lambda-list  the first argument of a variable binder, or a def form's
;;;                 parameter list — every name in it is a variable
;;;   :quoted       '(…) — data
;;; and only a head in a :normal list counts as a call.
(defparameter +let-like+
  '("let" "let*" "flet" "labels" "macrolet" "symbol-macrolet" "sb-alien:with-alien"
    "with-alien" "handler-bind" "restart-bind"))
(defparameter +var-binders+
  '("lambda" "dolist" "dotimes" "multiple-value-bind" "destructuring-bind"
    "with-open-file" "with-input-from-string" "with-output-to-string"
    "with-open-stream" "do-symbols" "do-external-symbols" "with-slots" "with-accessors"
    "handler-case" "restart-case" "multiple-value-setq" "with-raw-mode"))
(defparameter +def-like+
  '("defun" "defmacro" "defmethod" "defgeneric" "define-compiler-macro" "deftype"
    "defstruct" "defclass" "define-condition"))

(defstruct frame head (n 0) (kind :normal) has-ll)

(defun mentions (text)
  "List of (TOKEN CONTEXT) for TEXT with comments, strings and character literals
   removed. CONTEXT is :head for the operator of a form (a call), :quote after ' or
   #', else :plain. See the binding rules above."
  (let ((out '()) (i 0) (n (length text)) (prev nil)
        (stack (list (make-frame :kind :top))))
    (labels ((open-list (quoted)
               (let* ((parent (first stack))
                      (ph (frame-head parent))
                      (k (frame-n parent))
                      (kind (cond
                              (quoted :quoted)
                              ((member (frame-kind parent) '(:quoted)) :quoted)
                              ((eq (frame-kind parent) :bindings) :binding)
                              ;; (x 1) / (s subj-defn) inside a lambda list: X is
                              ;; bound, the rest (a default, a specializer) is not
                              ((eq (frame-kind parent) :lambda-list) :binding)
                              ;; the VALUE of a binding is an ordinary form again
                              ((eq (frame-kind parent) :binding) :normal)
                              ((and (= k 1) (member ph +let-like+ :test #'string=)) :bindings)
                              ((and (= k 1) (member ph +var-binders+ :test #'string=)) :lambda-list)
                              ((and (>= k 2) (not (frame-has-ll parent))
                                    (member ph +def-like+ :test #'string=))
                               (setf (frame-has-ll parent) t)
                               :lambda-list)
                              (t :normal))))
                 (incf (frame-n parent))
                 (push (make-frame :kind kind) stack))))
      (loop while (< i n) do
        (let ((c (char text i)))
          (cond
            ((char= c #\;) (setf i (or (position #\Newline text :start i) n)))
            ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
             (let ((depth 1) (k (+ i 2)))
               (loop while (and (< k (1- n)) (plusp depth)) do
                 (cond ((and (char= (char text k) #\|) (char= (char text (1+ k)) #\#)) (decf depth) (incf k 2))
                       ((and (char= (char text k) #\#) (char= (char text (1+ k)) #\|)) (incf depth) (incf k 2))
                       (t (incf k))))
               (setf i k)))
            ((char= c #\")
             (let ((k (1+ i)))
               (loop while (and (< k n) (char/= (char text k) #\"))
                     do (incf k (if (char= (char text k) #\\) 2 1)))
               (incf (frame-n (first stack)))
               (setf i (1+ k) prev nil)))
            ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\\))
             ;; #\x or #\Name: skip the char and any name letters after it
             (setf i (+ i 3))
             (loop while (and (< i n) (alphanumericp (char text i))) do (incf i))
             (incf (frame-n (first stack)))
             (setf prev nil))
            ((and (char= c #\#) (< (1+ i) n) (char= (char text (1+ i)) #\'))
             (setf prev :quote i (+ i 2)))
            ((char= c #\') (setf prev :quote i (1+ i)))
            ((char= c #\()
             (open-list (eq prev :quote))
             (setf prev :head i (1+ i)))
            ((char= c #\))
             (when (rest stack) (pop stack))
             (setf prev nil i (1+ i)))
            ((delim-p c) (setf prev nil i (1+ i)))
            (t (multiple-value-bind (tok j) (token-at text i)
                 (let* ((f (first stack))
                        (tok (string-downcase tok))
                        (first-p (zerop (frame-n f)))
                        (ctx (cond ((eq prev :quote) :quote)
                                   ((and first-p (eq prev :head) (eq (frame-kind f) :normal)) :head)
                                   ((and first-p (eq (frame-kind f) :binding)) :bound)
                                   ((eq (frame-kind f) :lambda-list) :bound)
                                   ((eq (frame-kind f) :quoted) :plain)
                                   (t :plain))))
                   (when first-p (setf (frame-head f) tok))
                   (incf (frame-n f))
                   ;; a name in a binding position is a fresh variable: not a mention
                   (unless (and (eq ctx :bound) (not (find #\: tok)))
                     (push (list tok (if (eq ctx :bound) :plain ctx)) out)))
                 (setf prev nil i j)))))))
    (nreverse out)))

;;; ---- resolving a mention to a defining file ---------------------------------

(defvar *index* (make-hash-table :test 'equal) "\"pkg|name\" -> list of DEFs.")

(defun index-defs ()
  (dolist (d *defs*) (push d (gethash (format nil "~a|~a" (def-pkg d) (def-name d)) *index*))))

(defun owner (defs)
  "The one file a name belongs to, or NIL if ambiguous. A DEFGENERIC owns its name."
  (let ((files (remove-duplicates (mapcar #'def-file defs) :test #'string=)))
    (cond ((null (rest files)) (first files))
          (t (let ((g (find "defgeneric" defs :key #'def-kind :test #'string=)))
               (and g (def-file g)))))))

(defun kind-class (kind)
  (cond ((member kind +fn-kinds+ :test #'string=) :fn)
        ((member kind +var-kinds+ :test #'string=) :var)
        ((member kind +type-kinds+ :test #'string=) :type)
        (t :fn)))

(defun counts-p (defs name context qualified)
  "Does a mention in CONTEXT count as a use of DEFS (see the header)?"
  (or qualified
      (some (lambda (d)
              (ecase (kind-class (def-kind d))
                (:var t)
                (:fn (member context '(:head :quote)))
                (:type (or (eq context :quote) (find #\- name)))))
            defs)))

(defun resolve (token context pkg)
  "TOKEN mentioned in package PKG -> (values FILE LABEL KIND) or NIL."
  (let* ((c (position #\: token))
         (qpkg (and c (plusp c) (subseq token 0 c)))
         (name (if c (string-left-trim ":" (subseq token c)) token)))
    (when (or (zerop (length name)) (and c (zerop c))) (return-from resolve nil)) ; keyword
    (when (and qpkg (or (member qpkg '("cl" "common-lisp" "keyword") :test #'string=)
                        (starts-with "sb-" qpkg)))
      (return-from resolve nil))
    (flet ((try (p)
             (let ((defs (gethash (format nil "~a|~a" p name) *index*)))
               (when (and defs (counts-p defs name context qpkg))
                 (let ((f (owner defs)))
                   (when f (return-from resolve
                             (values f (if qpkg token name) (def-kind (first defs))))))))))
      (if qpkg
          (progn (try qpkg)
                 (let ((f (gethash qpkg *package-file*)))
                   (when f (return-from resolve (values f token "package")))))
          (progn (try pkg)
                 (dolist (u (gethash pkg *package-uses*)) (try u))
                 ;; a defstruct accessor, by its conc-name prefix
                 (when (eq context :head)
                   (dolist (sp *struct-prefixes*)
                     (destructuring-bind (prefix spkg file) sp
                       (when (and (string= spkg pkg) (> (length name) (length prefix))
                                  (starts-with prefix name))
                         (return-from resolve (values file name "accessor")))))))))
    nil))

;;; ---- the module table -------------------------------------------------------

(defparameter +layers+
  '(("kernel interface" "the image talking to Linux: syscalls via sb-alien, ioctls, sysfs, /dev"
     "initramfs/logcount-fix.lisp" "poc/fb.lisp" "initramfs/framebuffer.lisp" "initramfs/files.lisp"
     "initramfs/sysinfo.lisp" "initramfs/meminfo.lisp" "initramfs/process.lisp"
     "initramfs/net.lisp" "initramfs/dhcp.lisp" "initramfs/dns.lisp" "initramfs/sntp.lisp"
     "initramfs/tls.lisp" "initramfs/http.lisp" "initramfs/wifi.lisp")
    ("data" "pure formats and algorithms: bytes in, values out, no I/O of their own"
     "poc/inflate.lisp" "poc/image.lisp" "poc/clipboard.lisp" "initramfs/ansi.lisp")
    ("model" "the image describing itself: captured source, CLOS via the MOP, SBCL's own source"
     "initramfs/registry.lisp" "initramfs/model.lisp" "initramfs/sbcl-source.lisp")
    ("present protocol" "subjects -> views of items + commands; pure, headless"
     "initramfs/present.lisp" "initramfs/tour.lisp" "initramfs/file-views.lisp"
     "initramfs/worker.lisp" "initramfs/examples.lisp" "initramfs/stage.lisp"
     "initramfs/containers.lisp" "initramfs/runs.lisp" "initramfs/net-views.lisp")
    ("UI toolkit" "pixels and text: the canvas, fonts, widgets, the text editor, the TTY line editor"
     "poc/canvas.lisp" "poc/textview.lisp" "poc/widgets.lisp" "initramfs/line-editor.lisp")
    ("shell" "the one stateful layer: trails, panes, keys, menus, halo, Spotter"
     "poc/shell.lisp" "poc/file-manager.lisp")
    ("framebuffer runtime" "PID 1's loops: the poll loop on /dev/fb0 + evdev, the REPL, the supervisor"
     "initramfs/fb-browser.lisp" "initramfs/repl.lisp" "initramfs/supervisor.lisp")
    ("host tools" "never in the image: the X11 backend, runners, the frame renderer, converters"
     "poc/x11.lisp" "poc/run-browser.lisp" "poc/run-editor.lisp" "poc/run-host.lisp"
     "poc/run-guest.lisp" "poc/scene.lisp" "poc/render-frame.lisp" "poc/bdf2font.lisp")))

(defun layer-of (file)
  (loop for (name nil . files) in +layers+ for i from 0
        when (member file files :test #'string=) return (values name i)))

(defun build-order ()
  "build.sh's LISP_SOURCES as repo-relative paths, registry.lisp first (it is LOADed
   before the list), in load order."
  (let ((out (list "initramfs/registry.lisp")) (in nil))
    (loop for line across (lines-of (slurp (merge-pathnames "build.sh" *root*))) do
      (cond ((starts-with "LISP_SOURCES=(" line) (setf in t))
            ((and in (starts-with ")" line)) (setf in nil))
            (in (let* ((q (position #\" line)) (e (and q (position #\" line :start (1+ q)))))
                  (when q
                    (let ((s (subseq line (1+ q) e)))
                      (push (cond ((starts-with "$INITRAMFS_DIR/" s) (concatenate 'string "initramfs/" (subseq s 15)))
                                  ((starts-with "$POC_DIR/" s) (concatenate 'string "poc/" (subseq s 9)))
                                  (t s))
                            out)))))))
    (nreverse out)))

(defun role-of (lines)
  "The one-line role from a ';;;; name.lisp — role' header."
  (let* ((first (if (plusp (length lines)) (aref lines 0) ""))
         (p (search " — " first)))
    (if p (string-trim " ." (subseq first (+ p 3))) "")))

;;; ---- 3. refs in the guide's HTML ----------------------------------------------

(defun find-ref-line (file what)
  "Line number of WHAT in FILE (see the header), or NIL."
  (cond
    ((null what) 1)
    ((and (> (length what) 1) (char= (char what 0) #\"))
     (let ((lit (string-trim "\"" what))
           (lines (lines-of (slurp (merge-pathnames file *root*)))))
       (loop for l across lines for n from 1 when (search lit l) return n)))
    (t (let* ((at (position #\@ what))
              (name (string-downcase (if at (subseq what 0 at) what)))
              (spec (and at (string-downcase (subseq what (1+ at)))))
              (hits (remove-if-not (lambda (d) (and (string= (def-file d) file)
                                                    (string= (def-name d) name)
                                                    (or (null spec) (equal (def-spec d) spec))
                                                    (string/= (def-kind d) "constructor")
                                                    (string/= (def-kind d) "accessor")))
                                   *defs*)))
         (and hits (reduce #'min hits :key #'def-line))))))

(defun html-escape (s)
  (with-output-to-string (o)
    (loop for c across s do
      (case c (#\< (write-string "&lt;" o)) (#\> (write-string "&gt;" o))
              (#\& (write-string "&amp;" o)) (#\" (write-string "&quot;" o))
              (t (write-char c o))))))

(defun attr (tag name)
  "The value of attribute NAME in the opening TAG text, or NIL."
  (let ((p (search (format nil " ~a=\"" name) tag)))
    (when p
      (let* ((s (+ p (length name) 3)) (e (position #\" tag :start s)))
        (subseq tag s e)))))

(defun unescape (s)
  (let ((out s))
    (loop for (from to) in '(("&quot;" "\"") ("&lt;" "<") ("&gt;" ">") ("&amp;" "&"))
          do (loop for p = (search from out) while p
                   do (setf out (concatenate 'string (subseq out 0 p) to (subseq out (+ p (length from)))))))
    out))

(defvar *errors* 0)
(defvar *refs* 0)
(defvar *unresolved* 0)

(defun rewrite-refs (path)
  (let* ((text (slurp path)) (out (make-string-output-stream)) (pos 0) (page (file-namestring path)))
    (loop for p = (search "<a class=\"ref\"" text :start2 pos)
          while p do
            (let* ((gt (position #\> text :start p))
                   (close (search "</a>" text :start2 gt))
                   (tag (subseq text p (1+ gt)))
                   (ref (unescape (attr tag "data-ref")))
                   (label (attr tag "data-label"))
                   (hash (position #\# ref))
                   (file (if hash (subseq ref 0 hash) ref))
                   (what (and hash (subseq ref (1+ hash))))
                   (exists (probe-file (merge-pathnames file *root*)))
                   (line (and exists (find-ref-line file what))))
              (incf *refs*)
              (write-string (subseq text pos p) out)
              (cond
                ((null line)
                 (incf *errors*) (incf *unresolved*)
                 (format *error-output* "~&UNRESOLVED ~a: ~a~a~%" page ref
                         (if exists "" "  (no such file)"))
                 (write-string (subseq text p (+ close 4)) out))
                (t
                 (let ((shown (or label
                                  (cond ((null what) (file-namestring file))
                                        ((char= (char what 0) #\") (string-trim "\"" what))
                                        (t (let ((at (position #\@ what)))
                                             (if at (format nil "(~a ~a)" (subseq what 0 at) (subseq what (1+ at)))
                                                 what)))))))
                   (format out "<a class=\"ref\" data-ref=\"~a\"~@[ data-label=\"~a\"~] href=\"../../~a\"><code>~a</code><span class=\"loc\">~a~@[:~a~]</span></a>"
                           (html-escape ref) (and label label) (html-escape file)
                           (html-escape (unescape shown)) (html-escape file)
                           (and what line)))))
              (setf pos (+ close 4))))
    (write-string (subseq text pos) out)
    (let ((new (get-output-stream-string out)))
      (unless (string= new text)
        (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
          (write-string new s))))))

;;; ---- 4. the sidebar, stamped into every page ----------------------------------
;;; Twelve pages sharing one navigation list is twelve chances to drift, so the
;;; list lives HERE and is written between <!-- nav:begin --> and <!-- nav:end -->
;;; in each page. A new section is one line in +PAGES+ and a re-run.

(defparameter +pages+
  '(("index.html"       "Start here: the big picture")
    ("build.html"       "How the image is built")
    ("modules.html"     "The module map")
    ("present.html"     "present and the shell")
    ("pixels.html"      "Pixels and input")
    ("live.html"        "Live editing, the debugger")
    ("system.html"      "Storage, privilege, network")
    ("verify.html"      "How to verify things")
    ("paths.html"       "Reading paths")
    ("glossary.html"    "Glossary")
    ("exercises.html"   "Exercises")
    ("in-progress.html" "Subsystems in progress")))

(defun nav-html (current)
  (with-output-to-string (o)
    (format o "<!-- nav:begin -->~%<nav class=\"side\" aria-label=\"Guide pages\">~%")
    (format o "  <div class=\"brand\"><a href=\"index.html\">lisp-over-linux</a></div>~%")
    (format o "  <div class=\"sub\">a guide to the code · <a href=\"../tour.org\">tour.org</a></div>~%  <ol>~%")
    (loop for (file title) in +pages+
          do (format o "    <li><a href=\"~a\"~:[~; aria-current=\"page\"~]>~a</a></li>~%"
                     file (string= file current) title))
    (format o "  </ol>~%  <div class=\"theme\">theme: <button type=\"button\" data-theme-toggle>auto</button></div>~%</nav>~%<!-- nav:end -->")))

(defun stamp-nav (path)
  (let* ((text (slurp path))
         (b (search "<!-- nav:begin -->" text))
         (e (search "<!-- nav:end -->" text)))
    (when (and b e)
      (let ((new (concatenate 'string (subseq text 0 b) (nav-html (file-namestring path))
                              (subseq text (+ e (length "<!-- nav:end -->"))))))
        (unless (string= new text)
          (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
            (write-string new s)))))
    (unless (find (file-namestring path) +pages+ :key #'first :test #'string=)
      (format *error-output* "~&NOTE: ~a is not in +PAGES+ (no sidebar entry)~%" (file-namestring path)))))

;;; ---- main -------------------------------------------------------------------

(defun lisp-files ()
  (sort (mapcar #'rel (append (directory (merge-pathnames "initramfs/*.lisp" *root*))
                              (directory (merge-pathnames "poc/*.lisp" *root*))))
        #'string<))

(defun js-string (s)
  (with-output-to-string (o)
    (write-char #\" o)
    (loop for c across s do
      (case c (#\" (write-string "\\\"" o)) (#\\ (write-string "\\\\" o))
              (#\Newline (write-string "\\n" o)) (t (write-char c o))))
    (write-char #\" o)))

(defun main ()
  (let ((files (lisp-files)))
    ;; index EVERY Lisp file (tests and host tools too), so refs into them resolve
    (dolist (f (append files
                       (mapcar #'rel (directory (merge-pathnames "*.lisp" *root*)))
                       (mapcar #'rel (directory (merge-pathnames "host-client/*.lisp" *root*)))))
      (scan-file-defs (merge-pathnames f *root*)))
    (index-defs)
    (let* ((modules (remove-if (lambda (f) (ends-with "-test.lisp" f)) files))
           (order (build-order))
           (edges (make-hash-table :test 'equal)))
      ;; edges
      (dolist (f modules)
        (let ((pkg "cl-user"))
          (dolist (m (mentions (slurp (merge-pathnames f *root*))))
            (destructuring-bind (tok ctx) m
              (cond
                ((and (eq ctx :head) (string= tok "in-package")) (setf pkg :next))
                ((eq pkg :next) (setf pkg (norm-package tok)))
                (t (multiple-value-bind (to label) (resolve tok ctx pkg)
                     (cond ((null to) nil)
                           ((string= to f) nil)
                           ((not (member to modules :test #'string=)) nil)
                           (t (pushnew label (gethash (cons f to) edges) :test #'string=)))))))))
        ;; BY-NAME uses: a package named in a STRING — (optional-fn "LOL.IMAGE" …),
        ;; (find-package "LOL.IMAGE"). The module reaches the other at run time, by
        ;; name, precisely so it still loads when that one is absent. The strings
        ;; were stripped above, so look for them in the raw text.
        (let ((text (slurp (merge-pathnames f *root*))) (start 0))
          (loop for p = (search "\"LOL." text :start2 start)
                while p do
                  (let* ((e (position #\" text :start (1+ p)))
                         (pkg (string-downcase (subseq text (1+ p) e)))
                         (to (gethash pkg *package-file*)))
                    (when (and to (string/= to f))
                      (pushnew (format nil "~a (by name, at run time)" pkg)
                               (gethash (cons f to) edges) :test #'string=))
                    (setf start (1+ e))))))
      ;; deps-data.js
      (let ((js (merge-pathnames "deps-data.js" *guide-dir*)))
        (with-open-file (o js :direction :output :if-exists :supersede :external-format :utf-8)
          (format o "// GENERATED by doc/guide/regen.lisp — do not edit; re-run the script.~%")
          (format o "window.LOL_DEPS = {~%  layers: [~%")
          (loop for (name blurb) in +layers+
                do (format o "    {name: ~a, blurb: ~a},~%" (js-string name) (js-string blurb)))
          (format o "  ],~%  modules: [~%")
          (dolist (f modules)
            (let* ((lines (lines-of (slurp (merge-pathnames f *root*))))
                   (ndefs (count f *defs* :key #'def-file :test #'string=))
                   (pkgs (let (ps)
                           (maphash (lambda (p pf) (when (string= pf f) (push p ps))) *package-file*)
                           ps)))
              (multiple-value-bind (layer li) (layer-of f)
                (unless layer
                  (format *error-output* "~&NOTE: ~a has no layer in +LAYERS+ (shown under 'other')~%" f))
                (format o "    {file: ~a, layer: ~a, role: ~a, lines: ~d, defs: ~d, packages: [~{~a~^, ~}], load: ~a},~%"
                        (js-string f) (or li -1) (js-string (role-of lines)) (length lines) ndefs
                        (mapcar #'js-string pkgs)
                        (let ((i (position f order :test #'string=))) (if i i "null"))))))
          (format o "  ],~%  edges: [~%")
          (let ((list '()))
            (maphash (lambda (k v) (push (cons k v) list)) edges)
            (setf list (sort list #'string< :key (lambda (e) (format nil "~a ~a" (caar e) (cdar e)))))
            (dolist (e list)
              (destructuring-bind ((from . to) . names) e
                (let* ((fi (position from order :test #'string=))
                       (ti (position to order :test #'string=))
                       (late (and fi ti (> ti fi))))
                  (format o "    {from: ~a, to: ~a, late: ~:[false~;true~], names: [~{~a~^, ~}]},~%"
                          (js-string from) (js-string to) late
                          (mapcar #'js-string (sort (copy-list names) #'string<))))))
            (format o "  ]~%};~%")
            (format t "~&deps-data.js: ~d modules, ~d edges (~d late-bound)~%"
                    (length modules) (length list)
                    (count-if (lambda (e) (let ((fi (position (caar e) order :test #'string=))
                                                (ti (position (cdar e) order :test #'string=)))
                                            (and fi ti (> ti fi))))
                              list))))))
    ;; refs
    (dolist (page (directory (merge-pathnames "*.html" *guide-dir*)))
      (stamp-nav page)
      (rewrite-refs page))
    (dolist (p +pages+)
      (unless (probe-file (merge-pathnames (first p) *guide-dir*))
        (incf *errors*)
        (format *error-output* "~&MISSING PAGE ~a (listed in +PAGES+)~%" (first p))))
    (format t "~&refs: ~d checked, ~d unresolved; ~d error~:p in all~%" *refs* *unresolved* *errors*)
    (sb-ext:exit :code (if (zerop *errors*) 0 1))))

(main)
