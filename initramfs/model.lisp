;;;; model.lisp — the code browser's MODEL layer (Phase 1, doc/code-browser.org).
;;;;
;;;; Phase 0 (registry.lisp) put our own SOURCE in the image. This layer adds the
;;;; STRUCTURE the browser's views will render: the class DAG, the generic-function
;;;; / method graph, categories, an object inspector, and the rest of the xref
;;;; wrappers. It is all pure introspection — sb-mop (the Metaobject Protocol,
;;;; built into SBCL, no require) plus sb-introspect — and it is deliberately
;;;; HEADLESS: every function here is callable and testable from the REPL, long
;;;; before any pixels. The eventual (present ...) methods (§4½) are thin renderers
;;;; over exactly these accessors.
;;;;
;;;; Convention, as in registry.lisp: FOO-INFO / *-OF return DATA (plists/lists the
;;;; UI will consume); SHOW-* print a readable REPL view. Loaded via load-recording
;;;; after registry.lisp.

;;; ======================================================================
;;; Classes — the DAG (not a tree: CLOS has multiple inheritance)
;;; ======================================================================

(defun as-class (x)
  "Coerce a class designator (a class, or its name) to a finalized class, or NIL."
  (let ((c (etypecase x
             (class x)
             (symbol (find-class x nil)))))
    (when c
      (unless (sb-mop:class-finalized-p c)
        (ignore-errors (sb-mop:finalize-inheritance c)))
      c)))

(defun class-kind (class)
  "Which flavour of class: :struct, :condition, :built-in, or :class (standard)."
  (typecase class
    (structure-class :struct)
    (built-in-class  :built-in)
    (t (if (subtypep (class-name class) 'condition) :condition :class))))

(defun superclasses (name)
  "Direct superclasses of NAME (names). The 'up' edges of the DAG."
  (let ((c (as-class name)))
    (and c (mapcar #'class-name (sb-mop:class-direct-superclasses c)))))

(defun subclasses (name)
  "Direct subclasses of NAME (names). The 'down' edges of the DAG."
  (let ((c (as-class name)))
    (and c (mapcar #'class-name (sb-mop:class-direct-subclasses c)))))

(defun precedence-list (name)
  "The class precedence list (linearised CPL) of NAME — how method dispatch reads
   the inheritance DAG, most-specific first, always ending at T."
  (let ((c (as-class name)))
    (and c (mapcar #'class-name (sb-mop:class-precedence-list c)))))

(defun slot-info (slotd)
  "A plist describing one slot definition."
  (list :name       (sb-mop:slot-definition-name slotd)
        :initargs    (sb-mop:slot-definition-initargs slotd)
        :allocation  (sb-mop:slot-definition-allocation slotd)
        :type        (sb-mop:slot-definition-type slotd)
        ;; readers/writers are recorded only on DIRECT slotds
        :readers     (ignore-errors (sb-mop:slot-definition-readers slotd))
        :writers     (ignore-errors (sb-mop:slot-definition-writers slotd))))

(defun class-info (name)
  "Everything the class view needs about NAME, as a plist."
  (let ((c (as-class name)))
    (when c
      (list :name        (class-name c)
            :kind        (class-kind c)
            :doc         (documentation c 'type)
            :supers      (mapcar #'class-name (sb-mop:class-direct-superclasses c))
            :subs        (mapcar #'class-name (sb-mop:class-direct-subclasses c))
            :cpl         (mapcar #'class-name (sb-mop:class-precedence-list c))
            :direct-slots(mapcar #'slot-info (sb-mop:class-direct-slots c))
            :all-slots   (mapcar #'slot-info (sb-mop:class-slots c))))))

(defun show-class (name)
  "Print a readable class view (the class DAG + slots), for the REPL."
  (let ((i (class-info name)))
    (if (null i)
        (format t "~&; ~s is not a class.~%" name)
        (progn
          (format t "~&; ~(~a~) ~s~@[  — ~a~]~%" (getf i :kind) (getf i :name) (getf i :doc))
          (format t ";   direct superclasses: ~a~%" (or (getf i :supers) '(none)))
          (format t ";   direct subclasses  : ~a~%" (or (getf i :subs) '(none)))
          (format t ";   precedence list    : ~a~%" (getf i :cpl))
          (format t ";   direct slots:~%")
          (dolist (s (getf i :direct-slots))
            (format t ";     ~a~@[ :type ~a~]~@[ :readers ~a~]~%"
                    (getf s :name)
                    (unless (eq (getf s :type) t) (getf s :type))
                    (getf s :readers)))))
    (finish-output)
    name))

;;; ======================================================================
;;; Generic functions & methods — the graph CLOS actually has
;;; ======================================================================

(defun generic-function-p (name)
  (and (fboundp name) (typep (fdefinition name) 'generic-function)))

(defun specializer-name (s)
  "A method specializer as a readable name: a class -> its name; an eql-specializer
   -> (EQL <object>)."
  (typecase s
    (class s)                                      ; caller maps class-name
    (sb-mop:eql-specializer (list 'eql (sb-mop:eql-specializer-object s)))
    (t s)))

(defun method-source (name quals specs)
  "Try to find a method's captured source in the registry (our own methods only).
   Matches a :method DEFN whose label mentions the qualifiers + specializers."
  (let ((label (string-trim " " (format nil "~{~a ~}(~{~a~^ ~})" quals specs))))
    (dolist (d (definitions-of name :method))
      (when (and (defn-label d) (string-equal (defn-label d) label))
        (return (defn-source d))))))

(defun method-info (method)
  "A plist describing one method: qualifiers, its specializer TUPLE (as names), and
   — best effort — the captured source text, matched out of the registry by name
   and specializer label."
  (let* ((gf    (sb-mop:method-generic-function method))
         (name  (and gf (sb-mop:generic-function-name gf)))
         (specs (mapcar (lambda (s) (let ((n (specializer-name s)))
                                      (if (typep n 'class) (class-name n) n)))
                        (sb-mop:method-specializers method)))
         (quals (sb-mop:method-qualifiers method)))
    (list :name name :qualifiers quals :specializers specs
          :source (method-source name quals specs))))

(defun methods-of (gf-name)
  "All methods of the generic function GF-NAME, as method-info plists, ordered
   most-specific-first where CLOS defines an order."
  (when (generic-function-p gf-name)
    (mapcar #'method-info (sb-mop:generic-function-methods (fdefinition gf-name)))))

(defun gf-info (name)
  "Everything the generics view needs about GF NAME."
  (when (generic-function-p name)
    (let ((gf (fdefinition name)))
      (list :name name
            :lambda-list (sb-mop:generic-function-lambda-list gf)
            :doc         (documentation name 'function)
            :methods     (methods-of name)))))

(defun methods-on-class (class-name)
  "The CLASS SIDE of dispatch: every method with CLASS-NAME anywhere in its
   specializer tuple (sb-mop:specializer-direct-methods). Each as method-info — so
   a multi-method appears under EVERY class it dispatches on. That is multiple
   dispatch, shown honestly (doc/code-browser.org §3)."
  (let ((c (as-class class-name)))
    (and c (mapcar #'method-info (sb-mop:specializer-direct-methods c)))))

(defun applicable-methods (gf-name &rest class-names)
  "The effective-method PREVIEW: which methods apply, and in what order, for a call
   dispatching on CLASS-NAMES. Returns (values method-infos definitive-p) — the
   second value is NIL when class information alone cannot decide (an eql method
   might apply), exactly as compute-applicable-methods-using-classes reports."
  (when (generic-function-p gf-name)
    (multiple-value-bind (methods ok)
        (sb-mop:compute-applicable-methods-using-classes
         (fdefinition gf-name) (mapcar #'as-class class-names))
      (values (mapcar #'method-info methods) ok))))

;;; --- Coverage: the dispatch matrix (where methods live across the classes) ---

(defun specializer-tuple (method)
  "METHOD's specializers as names, one per required parameter: a class -> its name,
   an eql-specializer -> (EQL object)."
  (mapcar (lambda (s) (let ((n (specializer-name s)))
                        (if (typep n 'class) (class-name n) n)))
          (sb-mop:method-specializers method)))

(defun gf-required-arity (gf-name)
  "How many required parameters GF-NAME dispatches over (the specializer-tuple
   length; every method of a GF has the same)."
  (let ((ms (and (generic-function-p gf-name)
                 (sb-mop:generic-function-methods (fdefinition gf-name)))))
    (if ms (length (sb-mop:method-specializers (first ms))) 0)))

(defun dispatch-axes (gf-name)
  "Per required position, the distinct specializers that appear there across all
   methods (names), with T sorted last. A position no method specializes lists
   just (T)."
  (let* ((arity (gf-required-arity gf-name))
         (tuples (mapcar #'specializer-tuple
                         (and (generic-function-p gf-name)
                              (sb-mop:generic-function-methods (fdefinition gf-name))))))
    (loop for i below arity collect
      (let ((specs (remove-duplicates (mapcar (lambda (tp) (nth i tp)) tuples)
                                      :test #'equal)))
        (append (sort (remove t specs) #'string-lessp :key #'princ-to-string)
                (when (member t specs) '(t)))))))

(defun active-axes (gf-name)
  "The parameter positions some method actually specializes (a non-T specializer).
   These are the axes worth drawing; the rest are always-T and carry no coverage."
  (loop for ax in (dispatch-axes gf-name) for i from 0
        when (remove t ax) collect i))

(defun dispatch-matrix (gf-name)
  "Coverage of GF-NAME across the class space (doc/code-browser.org §3). A plist:
     :mode  :matrix (exactly 2 active axes — the grid) | :list (0 or 1) | :multi (3+)
     :name  the GF name
   For :matrix — :row-pos/:col-pos the two positions, :rows/:cols their specializer
   names (T last), and :cells a row-major list of lists, each cell the method-infos
   whose tuple matches that (row,col) exactly. For :list/:multi — :active the active
   positions and :methods the method-infos (degrade honestly rather than fake a grid)."
  (let ((active (active-axes gf-name)))
    (if (= (length active) 2)
        (destructuring-bind (ri ci) active
          (let* ((axes (dispatch-axes gf-name))
                 (rows (nth ri axes)) (cols (nth ci axes))
                 (methods (methods-of gf-name)))
            (list :mode :matrix :name gf-name :row-pos ri :col-pos ci
                  :arity (gf-required-arity gf-name)
                  :rows rows :cols cols
                  ;; cells count PRIMARY methods: a 0 (a gap) means no behaviour for
                  ;; that tuple — the real signal. Cross-cutting :around/:before/
                  ;; :after methods are read in the onion, not here.
                  :cells (loop for r in rows collect
                           (loop for c in cols collect
                             (remove-if-not
                              (lambda (m) (and (null (getf m :qualifiers))
                                               (let ((tp (getf m :specializers)))
                                                 (and (equal (nth ri tp) r)
                                                      (equal (nth ci tp) c)))))
                              methods))))))
        (list :mode (if (>= (length active) 3) :multi :list)
              :name gf-name :active active :methods (methods-of gf-name)))))

;;; --- Combination: the effective-method onion (what actually runs) ---------

(defun spec->class (x)
  "Resolve one specializer designator to a class for the effective-method preview:
   a class or class-name via as-class; an (EQL object) form -> the object's class
   (a representative — classes-only dispatch can't honour the eql itself, but this
   shows what would run for an object of that kind)."
  (if (and (consp x) (eq (car x) 'eql))
      (class-of (second x))
      (as-class x)))

(defun effective-method-onion (gf-name &rest class-names)
  "The standard-method-combination structure of a call to GF-NAME dispatching on
   CLASS-NAMES, as a plist the onion view renders (doc/code-browser.org §3):
     :around   most-specific-first, each wraps the next (outer -> inner)
     :before   most-specific-first (all run, before the primary chain)
     :primary  most-specific-first (the call-next-method chain, threaded inward)
     :after    LEAST-specific-first (all run, after the primary chain)
     :definitive-p  NIL when classes alone can't decide (an EQL method might apply)
   Empty layers stay empty. This is exactly what CLOS weaves; the renderer's
   indentation IS the call stack."
  (when (generic-function-p gf-name)
    (multiple-value-bind (methods ok)
        (sb-mop:compute-applicable-methods-using-classes
         (fdefinition gf-name) (mapcar #'spec->class class-names))
      (flet ((qual= (m q) (equal (sb-mop:method-qualifiers m) (list q))))
        (list :name gf-name :on (copy-list class-names)
              :around  (mapcar #'method-info (remove-if-not (lambda (m) (qual= m :around)) methods))
              :before  (mapcar #'method-info (remove-if-not (lambda (m) (qual= m :before)) methods))
              :primary (mapcar #'method-info (remove-if #'sb-mop:method-qualifiers methods))
              :after   (mapcar #'method-info (reverse (remove-if-not (lambda (m) (qual= m :after)) methods)))
              :definitive-p ok)))))

(defun show-gf (name)
  "Print a readable generic-function view (methods + specializer tuples)."
  (let ((i (gf-info name)))
    (if (null i)
        (format t "~&; ~s is not a generic function.~%" name)
        (progn
          (format t "~&; generic-function ~s ~a~@[  — ~a~]~%"
                  (getf i :name) (getf i :lambda-list) (getf i :doc))
          (format t ";   ~a method~:p:~%" (length (getf i :methods)))
          (dolist (m (getf i :methods))
            (format t ";     ~@[~{~a ~}~](~{~a~^ ~})~@[   [has source]~]~%"
                    (getf m :qualifiers) (getf m :specializers)
                    (and (getf m :source) t)))))
    (finish-output)
    name))

;;; ======================================================================
;;; Categories — the "protocol" metadata Lisp lacks, reintroduced (§3)
;;; ======================================================================

(defun category-of (defn &optional (scheme :file))
  "The category of a DEFN record under SCHEME:
     :kind   — Functions / Macros / Classes / …  (safe default)
     :file   — the module it came from (our files ARE our categories)
     :export — external vs internal ≈ public vs private."
  (ecase scheme
    (:kind   (defn-kind defn))
    (:file   (and (defn-file defn)
                  (intern (string-upcase (pathname-name (defn-file defn))) :keyword)))
    (:export (multiple-value-bind (sym status)
                 (when (symbolp (defn-name defn))
                   (find-symbol (symbol-name (defn-name defn))
                                (symbol-package (defn-name defn))))
               (declare (ignore sym))
               (if (eq status :external) :external :internal)))))

(defun categorize (&optional (scheme :file))
  "Group ALL recorded definitions by category under SCHEME. Returns an alist of
   (category . (names…)), sorted — the data behind the browser's category pane."
  (let ((groups (make-hash-table :test 'equal)))
    (maphash (lambda (name defns)
               (dolist (d defns)
                 (push name (gethash (category-of d scheme) groups))))
             *registry*)
    (let ((out '()))
      (maphash (lambda (cat names)
                 (push (cons cat (sort (delete-duplicates names :test #'equal)
                                       #'string-lessp :key #'princ-to-string))
                       out))
               groups)
      (sort out #'string-lessp :key (lambda (g) (princ-to-string (car g)))))))

;;; ======================================================================
;;; The Inspector — every value browsable (the (present (x t)) fallback)
;;; ======================================================================

(defun inspect-fields (obj)
  "The 'fields' of any OBJ, as a list of (LABEL . VALUE) — the model an inspector
   pane renders, and what makes 'everything browsable from everywhere' mechanical:
   each VALUE is itself inspectable, so drilling never dead-ends."
  ;; Order matters: hash-table, package (and others) ARE structure-objects in
  ;; SBCL, so their specific clauses must precede the general object clause or
  ;; they'd show SBCL's internal slots instead of their real contents.
  (typecase obj
    (hash-table
     (let (fields) (maphash (lambda (k v) (push (cons k v) fields)) obj) (nreverse fields)))
    (package
     (list (cons :name (package-name obj))
           (cons :nicknames (package-nicknames obj))
           (cons :use-list (mapcar #'package-name (package-use-list obj)))))
    (symbol                                      ; a symbol's cells — deeply Lispy
     (list (cons :name (symbol-name obj))
           (cons :package (symbol-package obj))
           (cons :value (if (boundp obj) (symbol-value obj) :unbound))
           (cons :function (if (fboundp obj) (fdefinition obj) :unbound))
           (cons :plist (symbol-plist obj))))
    (cons                                        ; a cons: its two halves (drill the cdr to walk)
     (list (cons :car (car obj)) (cons :cdr (cdr obj))))
    ((and vector (not string))
     (loop for i below (length obj) collect (cons i (aref obj i))))
    ((or standard-object structure-object)       ; general: slots by name (LAST)
     (mapcar (lambda (s)
               (let ((n (sb-mop:slot-definition-name s)))
                 (cons n (if (slot-boundp obj n) (slot-value obj n) :unbound))))
             (sb-mop:class-slots (class-of obj))))
    (t (list (cons :value obj) (cons :type (type-of obj))))))

(defun show-inspect (obj)
  "Print OBJ's fields (the REPL inspector)."
  (format t "~&; inspect ~s   [~a]~%" obj (type-of obj))
  (dolist (f (inspect-fields obj))
    (format t ";   ~s = ~s~%" (car f) (cdr f)))
  (finish-output)
  obj)

;;; ======================================================================
;;; The condition system — the live debugger's two stacks (§3)
;;; ======================================================================
;;; A signal rises the HANDLER side with the stack intact; invoking a RESTART drops
;;; the RESTART side and unwinds. compute-restarts gives the restart column (already
;;; innermost-first, in stack order); sb-di walks the backtrace spine. This is the
;;; data the framebuffer debugger (fb-browser.lisp) renders and acts on.

(defun restart-list (&optional condition)
  "The active restarts (innermost first — stack order), each a plist
   (:name :report :restart). With CONDITION, only restarts associated with it (plus
   the un-associated ones), exactly what would be offered were it unhandled."
  (mapcar (lambda (r)
            (list :name (restart-name r)
                  :report (princ-to-string r)     ; the restart's report string
                  :restart r))
          (compute-restarts condition)))

(defun backtrace-frame-objects (&optional (count 40) (skip 0))
  "The live sb-di frame OBJECTS, innermost first — the same walk as BACKTRACE-FRAMES,
   but returning the frames themselves so the debugger can read each one's locals.
   SKIP drops the innermost frames (the debugger's own machinery); COUNT caps depth."
  (let ((frames '()))
    (do ((f (sb-di:top-frame) (sb-di:frame-down f))
         (i 0 (1+ i)))
        ((or (null f) (>= (length frames) count)) (nreverse frames))
      (when (>= i skip) (push f frames)))))

(defun frame-label (frame)
  "A best-effort name string for FRAME — some internal frames have no tidy name."
  (or (ignore-errors
        (let ((*print-length* 4) (*print-level* 3))
          (princ-to-string (sb-di:debug-fun-name (sb-di:frame-debug-fun frame)))))
      "<anonymous frame>"))

(defun frame-defn-name (frame)
  "The definition name to OPEN for FRAME's function — the debugger's M-. target: a
   plain symbol, or (SETF x), that we can look up; NIL for a frame with nothing to
   jump to. A structured name — a method (…FAST-METHOD gf …), a nested (FLET f :IN g)
   — yields its first bound, non-keyword symbol, so a method opens its generic
   function and an flet opens the function it lives in."
  (ignore-errors
    (let ((name (sb-di:debug-fun-name (sb-di:frame-debug-fun frame))))
      (cond
        ((and name (symbolp name)) name)
        ((and (consp name) (eq (car name) 'setf)) name)
        ((consp name)
         (some (lambda (x) (and (symbolp x) (not (keywordp x)) (fboundp x) x))
               (cdr name)))
        (t nil)))))

(defun backtrace-frames (&optional (count 40) (skip 0))
  "The live call stack as a list of name strings, innermost first, via sb-di (what
   SBCL's own debugger walks). SKIP drops the innermost frames (the debugger's own
   machinery); COUNT caps depth. Each frame's name is best-effort — some internal
   frames have no tidy name."
  (mapcar #'frame-label (backtrace-frame-objects count skip)))

(defun frame-var-name (var)
  "VAR's source name, lower-cased; a non-zero id (a shadowing duplicate) is appended
   as =name#n=, exactly as SBCL's own debugger disambiguates them."
  (or (ignore-errors
        (let ((name (string-downcase (symbol-name (sb-di:debug-var-symbol var))))
              (id   (sb-di:debug-var-id var)))
          (if (zerop id) name (format nil "~a#~d" name id))))
      "?"))

(defun compact-value (s)
  "One tidy line for the debugger's locals column: newlines/tabs -> spaces (a slot
   holding a banner string must not blow the panel open), capped with an ellipsis."
  (let ((s (substitute #\Space #\Newline (substitute #\Space #\Tab s))))
    (if (> (length s) 120) (concatenate 'string (subseq s 0 117) "…") s)))

(defun frame-var-value (var loc frame)
  "VAR's value printed short and on one line, or :UNAVAILABLE when it is not live at
   LOC (a temp, or not yet bound at this program point). Never signals — the debugger
   cannot afford a second error while showing the first. Kept shallow (level 2, no
   pretty-print) so a nested struct collapses to =#= instead of unrolling its slots."
  (if (eq (ignore-errors (sb-di:debug-var-validity var loc)) :valid)
      (or (ignore-errors
            (let ((*print-length* 4) (*print-level* 2)
                  (*print-circle* t) (*print-pretty* nil))
              (compact-value (princ-to-string (sb-di:debug-var-value var frame)))))
          "#<unprintable>")
      :unavailable))

(defun frame-locals (frame)
  "The local variables live in FRAME, as a list of (:name STRING :value STRING-or-
   :UNAVAILABLE) — what SBCL's own debugger lists under 'l'. NIL when the frame carries
   no debug variables (interpreted code, or compiled with too little debug info). Every
   sb-di read is guarded: this feeds PID 1's last-resort debugger, which must not error
   while reporting an error. (=debug-fun-debug-vars= is internal to SB-DI.)"
  (ignore-errors
    (let* ((fun  (sb-di:frame-debug-fun frame))
           (loc  (sb-di:frame-code-location frame))
           (vars (sb-di::debug-fun-debug-vars fun)))
      (when (and vars (plusp (length vars)))
        (loop for v across vars
              collect (list :name (frame-var-name v)
                            :value (frame-var-value v loc frame)))))))

;;; ======================================================================
;;; The rest of the xref wrappers (registry.lisp has senders/referrers/…)
;;; ======================================================================

(defun binders   (name) "Functions that BIND the special variable NAME."
  (mapcar #'car (sb-introspect:who-binds name)))
(defun setters   (name) "Functions that SETQ the special variable NAME."
  (mapcar #'car (sb-introspect:who-sets name)))
(defun macro-users (name) "Functions that expand the macro NAME."
  (mapcar #'car (sb-introspect:who-macroexpands name)))
