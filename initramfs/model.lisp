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

(defun method-source-label (quals specs)
  "The registry label for a method — its qualifiers then its specializer tuple, e.g.
   \"(SUBJ-DEFN)\" or \":AROUND (WIDGET T)\". Kept identical to the label DEFN-NAME-AND-KIND
   records so method lookup AND Accept agree on which method a defmethod names.

   Identical by CONSTRUCTION now rather than by two format strings happening to
   match: both sides call METHOD-LABEL in registry.lisp, which is why that printer
   lives in the first module loaded rather than here."
  (method-label quals specs))

(defun method-source (name quals specs)
  "Try to find a method's captured source in the registry (our own methods only).
   Matches a :method DEFN whose label mentions the qualifiers + specializers."
  (let ((label (method-source-label quals specs)))
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
            (format t ";     ~a~@[   [has source]~]~%"
                    (method-source-label (getf m :qualifiers) (getf m :specializers))
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
    ;; Key on DIRECTORY/NAME, not the bare name. We load from two trees —
    ;; initramfs/ (guest-only) and poc/ (shared with the host dev loop) — so a
    ;; bare basename would silently merge an initramfs/foo.lisp with a
    ;; poc/foo.lisp into one bucket. Qualifying also tells the reader which tree
    ;; a module came from, which is real information here.
    (:file   (let ((f (defn-file defn)))
               (when f
                 (let ((dir (car (last (pathname-directory f)))))
                   (intern (string-upcase
                            (if (stringp dir)
                                (format nil "~a/~a" dir (pathname-name f))
                                (pathname-name f)))
                           :keyword)))))
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
;;; Functions as objects — a closure's captures ARE its slots
;;; ======================================================================
;;; Most tooling shows a function as a leaf: #<FUNCTION FOO> and nothing to open.
;;; That is wrong, and Lisp makes it obviously wrong — a closure is a RECORD whose
;;; fields are the variables it closed over. Opening one should show them.
;;;
;;; The detail that makes this fit INSPECT-PLACES' thesis exactly: SBCL boxes a
;;; captured variable in a VALUE-CELL *only when something assigns to it*. A
;;; capture that is merely read is stored inline in the closure and there is no
;;; cell to write through — (SETF SB-KERNEL:%CLOSURE-INDEX-REF) does not exist.
;;; So some captures are places and some are not, which is the one thing the
;;; (:LABEL :VALUE :SET) plist can say and a uniformly-settable design could not.

(defun value-cell-p (x)
  "True when X is a VALUE-CELL — the box SBCL puts around a closed-over variable
   that is ASSIGNED somewhere. Tested by widetag rather than by calling
   SB-DI::INDIRECT-VALUE-CELL-P, which is internal to SB-DI and not exported."
  (and (sb-kernel:pointerp x)
       (eql (sb-kernel:widetag-of x) sb-vm:value-cell-widetag)))

(defun closure-captures (fn)
  "FN's captured variables as PLACES: a list of plists (:label L :value V :set FN-or-NIL),
   the same shape INSPECT-PLACES returns, one per closure slot.

   Labels are POSITIONAL (\"capture 0\"), deliberately. SBCL's debug info does know the
   names — SB-DI:DO-DEBUG-FUN-VARS yields them — but it yields them SORTED BY NAME, not
   in capture order, and it mixes the function's own parameters in with the captures.
   Ordering the debug vars by their storage offset recovers capture order when every
   capture is read-only, and breaks as soon as one is boxed (the cell lands in a
   different storage class). There is no honest mapping short of decoding SBCL's
   storage-class encoding, so we do not guess: the row says which SLOT it is, and
   FUNCTION-CLOSES-OVER reports the names as an unordered set alongside. Naming a
   capture wrongly would be worse than not naming it, because the browser would let
   you edit \"step\" and silently change `start'."
  (when (sb-kernel:closurep fn)
    (loop for v in (sb-kernel:%closure-values fn)
          for i from 0
          collect (if (value-cell-p v)
                      ;; assigned somewhere, so it lives in a cell — a real place
                      (let ((cell v))
                        (list :label (format nil "capture ~d" i)
                              :value (sb-kernel:value-cell-ref cell)
                              :set (lambda (new) (sb-kernel:value-cell-set cell new))))
                      ;; read-only capture: stored inline, no cell, nothing to write to
                      (list :label (format nil "capture ~d" i) :value v :set nil)))))

(defun flatten-tree (tree)
  "Every atom in TREE, as a flat list — enough to pull the variable names out of a
   lambda list without a real parser. We ship no utility library (a saved image is a
   frozen heap), so the four lines live here."
  (let ((out '()))
    (labels ((walk (x) (cond ((null x))
                             ((consp x) (walk (car x)) (walk (cdr x)))
                             (t (push x out)))))
      (walk tree))
    (nreverse out)))

(defun function-closes-over (fn)
  "The NAMES of FN's closed-over variables, as an unordered set, or NIL when we cannot
   say for certain. Computed as (debug vars of FN) minus (FN's own parameters), because
   DO-DEBUG-FUN-VARS yields both.

   Three guards, each of which was earning its keep within a minute of the first render:

   - FN must actually BE a closure. A plain DEFUN has debug vars too — they are its
     locals — and reporting those as `closes over' is simply false. DRAW-ALIEN showed
     (A BPP DST FB RGBA …) before this guard existed.
   - Uninterned symbols are dropped. &KEY defaulting introduces gensyms such as
     #:.DEFAULTING-TEMP., which are never variables anybody wrote.
   - The count must MATCH the number of capture slots. Debug vars include the inner
     lambda's own locals, and there is no way to tell which of them are captures; when
     the counts disagree we return NIL rather than name the wrong set. Unordered even
     when we do answer — see CLOSURE-CAPTURES for why the order cannot be recovered."
  (when (sb-kernel:closurep fn)
    (ignore-errors
      (let* ((df (sb-di:fun-debug-fun fn))
             (params (remove-if-not #'symbolp
                                    (flatten-tree
                                     (ignore-errors (sb-kernel:%fun-lambda-list fn)))))
             (vars '()))
        (sb-di:do-debug-fun-vars (v df) (push (sb-di:debug-var-symbol v) vars))
        (let ((names (sort (remove-if (lambda (sym)
                                        (or (null (symbol-package sym))   ; a gensym
                                            (member sym params :test #'string=)
                                            (member sym lambda-list-keywords)))
                                      (nreverse vars))
                           #'string< :key #'symbol-name)))
          (when (= (length names) (length (sb-kernel:%closure-values fn)))
            names))))))

(defun abbreviate-fun-name (name)
  "A function NAME fit to put on screen: any absolute source path inside it shortened
   to DIRECTORY/NAME.TYPE.

   SBCL names a top-level anonymous lambda after the file it was compiled from, and that
   name is recorded with the ABSOLUTE path of the build machine — so an unedited title
   reads (LAMBDA (NAME) :IN \"/home/someone/lisp-over-linux/initramfs/stage.lisp\") and
   the browser cheerfully shows a stranger's home directory on a laptop that has no such
   directory. The repo is deliberately relocatable (README.org; the cpio paths and
   build.sh are all relative), and the UI should not be the one place that is not.

   DIRECTORY/NAME is the same key CATEGORIZE :file uses for the module index, so an
   abbreviated name reads exactly like the module it came from."
  (labels ((short (x)
             (if (and (stringp x) (find #\/ x))
                 (let* ((path (pathname x))
                        (dir  (car (last (pathname-directory path))))
                        (file (format nil "~a~@[.~a~]" (pathname-name path)
                                      (pathname-type path))))
                   (if (stringp dir) (format nil "~a/~a" dir file) file))
                 x)))
    (if (consp name) (mapcar #'short name) (short name))))

(defun function-source-name (fn)
  "The NAME to look FN's source up under: its own name when it has one, or the name of
   the function it was defined inside when it is an anonymous closure — SBCL records
   that as (LAMBDA (…) :IN MAKE-COUNTER), so the enclosing definition is recoverable
   and `Source' on a closure can open the DEFUN that built it."
  (let ((n (ignore-errors (sb-kernel:%fun-name fn))))
    (cond ((symbolp n) n)
          ;; (SETF FOO) and friends are legal function names — keep them whole
          ((and (consp n) (eq (first n) 'setf)) n)
          ;; (LAMBDA (…) :IN ENCLOSING) / (FLET FOO :IN ENCLOSING)
          ((and (consp n) (member :in n))
           (let ((tail (cdr (member :in n))))
             (and (consp tail) (symbolp (first tail)) (first tail))))
          (t nil))))

(defun function-places (fn)
  "FN's fields as places — the FUNCTION clause of INSPECT-PLACES.

   Name, lambda list and documentation, then one row per capture. DOCUMENTATION is the
   only non-capture row that is settable, because it is the only one with a defined
   writer: a function's name and arity are properties of the compiled code object, and
   changing what a function DOES is `C-c C-c' on its source, not a one-line prompt —
   the same argument INSPECT-PLACES already makes about a symbol's :FUNCTION cell."
  (let* ((name   (ignore-errors (sb-kernel:%fun-name fn)))
         (ll     (ignore-errors (sb-kernel:%fun-lambda-list fn)))
         (closes (function-closes-over fn))
         (expr   (ignore-errors (function-lambda-expression fn))))
    (append
     (list (list :label :name :value name :set nil)
           (list :label :lambda-list :value ll :set nil)
           (list :label :documentation :value (or (documentation fn t) :none)
                 :set (lambda (new) (setf (documentation fn t) new))))
     (when closes
       (list (list :label :closes-over :value closes :set nil)))
     (closure-captures fn)
     ;; FUNCTION-LAMBDA-EXPRESSION returns NIL for most COMPILED functions, so this row
     ;; is present only when the form really was retained (it is, for anything the image
     ;; loaded as source). Source recovery proper goes through the registry by name —
     ;; see FUNCTION-SOURCE-NAME — which works for compiled definitions too.
     (when expr
       (list (list :label :source-form :value expr :set nil))))))

;;; ======================================================================
;;; The Inspector — every value browsable (the (present (x t)) fallback)
;;; ======================================================================

(defgeneric inspect-slot-setter (object slot-name)
  (:documentation "How the inspector writes slot SLOT-NAME of OBJECT: a function of the
   new value, or NIL when the field must not be set from the inspector. The default
   is plain (SETF SLOT-VALUE). A class whose slot means more than its value says so
   with a method: a morph's BOUNDS is set by SET-MORPH-BOUNDS, which tells the screen
   (poc/morphic.lisp) — writing the slot alone would move nothing.")
  (:method (object slot-name)
    (lambda (new) (setf (slot-value object slot-name) new))))

(defun inspect-places (obj)
  "The 'fields' of any OBJ as PLACES, not just values: a list of plists
   (:label L :value V :set FN), where FN writes a new value back, or NIL when the
   field is not settable.

   The distinction is the whole reason this function exists. A VALUE has no identity
   as a location — you cannot write to \"the string it happens to hold\", only to the
   CELL that holds it — so an inspector that means to edit has to carry the cell
   along, and a closure is the smallest honest way to say \"here is how to put
   something back\".

   Order matters below: hash-table, package (and others) ARE structure-objects in
   SBCL, so their specific clauses must precede the general object clause or they
   would show SBCL's internal slots instead of their real contents."
  (flet ((ro (label value) (list :label label :value value :set nil)))
    (typecase obj
      (hash-table
       (let (fields)
         (maphash (lambda (k v)
                    (push (list :label k :value v
                                :set (lambda (new) (setf (gethash k obj) new)))
                          fields))
                  obj)
         (nreverse fields)))
      (package
       (list (ro :name (package-name obj))
             (ro :nicknames (package-nicknames obj))
             (ro :use-list (mapcar #'package-name (package-use-list obj)))))
      (symbol                                    ; a symbol's cells — deeply Lispy
       (list (ro :name (symbol-name obj))
             (ro :package (symbol-package obj))
             (list :label :value :value (if (boundp obj) (symbol-value obj) :unbound)
                   :set (lambda (new) (setf (symbol-value obj) new)))
             ;; :function is deliberately NOT settable here. It is writable —
             ;; (setf (fdefinition sym) …) works — but redefining a function by
             ;; typing an expression into a one-line prompt is a worse version of
             ;; something the browser already does properly: open its source and
             ;; C-c C-c, which also records the old text in the change set.
             (ro :function (if (fboundp obj) (fdefinition obj) :unbound))
             (list :label :plist :value (symbol-plist obj)
                   :set (lambda (new) (setf (symbol-plist obj) new)))))
      (cons                                      ; a cons: its two halves
       (list (list :label :car :value (car obj) :set (lambda (new) (setf (car obj) new)))
             (list :label :cdr :value (cdr obj) :set (lambda (new) (setf (cdr obj) new)))))
      ((and vector (not string))
       (loop for i below (length obj)
             collect (let ((i i))
                       (list :label i :value (aref obj i)
                             :set (lambda (new) (setf (aref obj i) new))))))
      ;; A function is not a leaf — see the section above. This MUST precede the
      ;; standard-object clause: a generic function is a funcallable instance and
      ;; therefore also a STANDARD-OBJECT, and the function reading is the useful one.
      (function (function-places obj))
      ((or standard-object structure-object)     ; general: slots by name (LAST)
       (mapcar (lambda (s)
                 (let ((n (sb-mop:slot-definition-name s)))
                   (list :label n
                         :value (if (slot-boundp obj n) (slot-value obj n) :unbound)
                         :set (inspect-slot-setter obj n))))
               (sb-mop:class-slots (class-of obj))))
      ;; The fallback shows what a thing IS, and neither field is a place: we were
      ;; handed the value, not the cell it came from. A string lands here — which is
      ;; the right answer, because the way to change a string is to replace the cell
      ;; holding it (inspect the SYMBOL and set its :value), not to mutate the string.
      ;; Mutating one in place is undefined behaviour that happens to work: SBCL may
      ;; coalesce equal literals in compiled code, so editing a string could silently
      ;; change an unrelated function that shares it.
      (t (list (ro :value obj) (ro :type (type-of obj)))))))

(defun inspect-fields (obj)
  "The 'fields' of any OBJ, as a list of (LABEL . VALUE) — the simple view, over
   INSPECT-PLACES. Each VALUE is itself inspectable, so drilling never dead-ends."
  (mapcar (lambda (p) (cons (getf p :label) (getf p :value))) (inspect-places obj)))

(defun show-inspect (obj)
  "Print OBJ's fields (the REPL inspector)."
  (format t "~&; inspect ~s   [~a]~%" obj (type-of obj))
  (dolist (f (inspect-fields obj))
    (format t ";   ~s = ~s~%" (car f) (cdr f)))
  (finish-output)
  obj)

;;; ======================================================================
;;; Inspector POWERS — the verbs, not the fields (doc/inspector-history.org)
;;; ======================================================================
;;; Everything above answers "what is in this object". These answer the questions an
;;; inspector on a Lisp Machine could answer and a modern object viewer cannot:
;;; run code inside it, find every instance of its class, and ask the garbage
;;; collector why it is still alive. All three are pure model — they compute and
;;; return; present.lisp renders and shell.lisp supplies the gesture.

(defun eval-in-context (obj text)
  "READ and EVAL the expression in TEXT *inside* OBJ: SELF is bound to the object, and
   every slot name is bound as a PLACE. Returns the value.

   This is Smalltalk's inspector code pane — the power that turns a fixed set of
   affordances into an open one, because anything we did not build a button for you
   can simply type. The CLOS version goes one better than Smalltalk's `self': WITH-SLOTS
   makes the slots themselves lexically visible, so (list name hp) reads two slots and
   (incf hp 100) writes one, with no accessors required and no SLOT-VALUE boilerplate.

   Two things here are not obvious and both were found the hard way:

   1. The object is bound as a LAMBDA PARAMETER, never spliced into the form. The
      obvious `(let ((self ,obj)) …) breaks the moment the object is a list — the form
      becomes (LET ((SELF (1 2 3))) …) and (1 2 3) reads as a function call.

   2. *PACKAGE* is bound around the READ, not just the EVAL. Slot names are symbols in
      some package; typing `hp' while *PACKAGE* is CL-USER when the class lives in
      LOL.SHELL interns a DIFFERENT symbol and WITH-SLOTS silently never matches it.
      Binding it to the class name's home package is what makes bare slot names work,
      which is the whole ergonomic point.

   Objects with no slots at all (a cons, a hash-table, a function) degrade to a bare
   SELF binding rather than erroring, so the verb is available on every subject."
  (let* ((names (ignore-errors
                  (mapcar #'sb-mop:slot-definition-name
                          (sb-mop:class-slots (class-of obj)))))
         (names (remove-if-not #'symbolp names))
         (*package* (or (ignore-errors
                          (symbol-package (class-name (class-of obj))))
                        *package*))
         (form (read-from-string text))
         (self (intern "SELF" *package*)))
    (funcall (eval (if names
                       `(lambda (,self) (declare (ignorable ,self))
                          (with-slots ,names ,self ,form))
                       `(lambda (,self) (declare (ignorable ,self)) ,form)))
             obj)))

(defun all-instances (class &key (limit 500))
  "Every live instance of CLASS in dynamic space — Smalltalk's allInstances, which CLOS
   has no operator for because instances are not registered anywhere. A heap walk is the
   only way to ask, and SBCL ships one.

   Two traps, both hit while building this:

   1. The :TEST MUST NOT CONS. room.lisp says so in its own source: a consing predicate
      makes the result `very arbitrary - including possible duplication of objects if we
      visit something and then see it again after GC transports it higher'. TYPEP against
      a class is a layout check and is safe. LIST-ALLOCATED-OBJECTS is the right entry
      point because it does the two-pass count-then-fill that keeps its own allocation
      out of the walk.

   2. PCL's CLASS PROTOTYPE is a live instance of the class and it has UNBOUND SLOTS.
      The object system makes one for its own use, so a heap walk finds it, and any
      caller that reasonably assumes `every SHIP has a name' then breaks on a ghost.
      Filtering it is not optional."
  (let* ((c (if (symbolp class) (find-class class nil) class))
         (proto (and c (ignore-errors (sb-mop:class-prototype c)))))
    (when c
      (let ((found (sb-vm:list-allocated-objects
                    :dynamic :test (lambda (x) (typep x c)) :count limit)))
        (remove proto found)))))

(defun retention-path (obj)
  "Why is OBJ still alive? A list of HOPS from a GC root down to it, outermost first.
   Each hop is a plist (:label L :object O :index I) — O is the holder, so every hop is
   itself something the browser can open, and I is the slot within it.

   This asks the GARBAGE COLLECTOR, which already knows the object graph, rather than
   scanning the heap by hand — and it answers the sharper question. `Who points at this'
   is rarely what you want to know; `what is the chain from something definitionally
   live' is. SB-EXT:SEARCH-ROOTS is one of the very few powers on the historical list
   that modern SBCL does BETTER than a Lisp Machine did.

   The argument must be a WEAK POINTER — a reference the collector is allowed to ignore —
   so that asking cannot itself become the reason the object is alive. Note the honest
   limit that follows from this: the browser is holding the subject while you ask, so a
   path ending at a thread stack may be reporting OUR OWN frames. Such a hop is labelled
   as a stack root rather than dressed up as a data structure.

   Hops come back in three shapes and all three are normalised here: the keyword :STATIC
   (the root space itself), (\"main thread\" . SAP) for a stack root, and (HOLDER . SLOT)
   for the ordinary heap case. Labels are printed under tight *PRINT-LENGTH*/:LEVEL
   because a hop is routinely an internal vector of several hundred symbols, and a view
   row that tried to show it whole would be unreadable and slow."
  (ignore-errors
    (let* ((results (sb-ext:search-roots (sb-ext:make-weak-pointer obj)
                                         :criterion :oldest :print nil))
           ;; one weak pointer in, so at most one (TARGET . PATH) out
           (path (cdr (first results))))
      (loop for hop in path
            collect (cond
                      ((keywordp hop)
                       (list :label (format nil "~(~a~) space (a GC root)" hop)
                             :object nil :index nil))
                      ((and (consp hop) (stringp (car hop)))
                       (list :label (format nil "~a's stack -- possibly this browser's own frames"
                                            (car hop))
                             :object nil :index nil))
                      ((consp hop)
                       (list :label (let ((*print-length* 3) (*print-level* 2)
                                          (*print-lines* 1))
                                      (format nil "~s" (car hop)))
                             :object (car hop) :index (cdr hop)))
                      (t (list :label (format nil "~s" hop) :object nil :index nil)))))))

(defun apply-function (fn args-text)
  "Call FN with the arguments in ARGS-TEXT (a bare argument list, without the surrounding
   parens: \"1 2\"). Returns the value.

   For a FUNCTION, `evaluate in the object's context' collapses to this: the object IS
   the verb, so the useful thing to type is its arguments. Reading a wrapped list rather
   than EVALing a whole call form keeps the function identity fixed — you apply the
   closure you are looking at, not whatever its name happens to be bound to now, which
   for an anonymous closure is nothing at all."
  (apply fn (read-from-string (concatenate 'string "(" args-text ")"))))

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

;;; ---- frame locals as PLACES: setting a variable in a standing frame ------
;;; (doc/inspector-history.org §5.4.) INSPECT-PLACES turned an object's fields from
;;; values into places; this does the same for a stack frame's locals, so the
;;; debugger can write a new value into a frame that is still waiting for ERROR (or
;;; CERROR) to return — and then CONTINUE, or re-enter the frame, and watch the
;;; function carry on with the value you gave it.
;;;
;;; Where a local LIVES. The compiler told sb-di, for every debug variable, a
;;; "storage class + offset" (SC+OFFSET): which kind of storage (a register, a stack
;;; slot, an unboxed float slot, ...) and which one. A frame that is NOT the innermost
;;; one is stopped at a call, and SBCL on x86-64 has no callee-saved registers, so
;;; across that call everything still live has been SAVED to the frame's stack — the
;;; variable's SAVE-SC+OFFSET. Writing there is writing what the frame will read when
;;; the call returns. The innermost frame of an internal error (CAR of a number, a
;;; type check) is different: it was INTERRUPTED, so its registers sit in a signal
;;; context ("escaped"), and sb-di writes into that context instead.
;;;
;;; Measured against ./sbcl 2.6.8 first (the numbers and the cases are in the doc):
;;;   * at (debug 0) a frame's locals are ARG-0, ARG-1 ... and :UNKNOWN — not settable;
;;;   * at (debug 1), our image's default, only the ARGUMENTS survive as debug vars —
;;;     a LET-bound temporary is simply not there to set;
;;;   * at (debug 2)+ every local is listed, valid, and a write is seen on CONTINUE.
;;; And three traps, each guarded below rather than left to bite:
;;;   1. A variable the compiler keeps as a FIXNUM (storage class ANY-REG) will take a
;;;      string without complaint — and the frame then computes with the string's
;;;      address as if it were a number ((+ n 1) => 38663474160). So the storage class
;;;      decides what may be written, not the old value.
;;;   2. An "indirect" variable — one a dynamic-extent local function reads out of its
;;;      PARENT's frame — makes SBCL's own (SETF DEBUG-VAR-VALUE) overwrite the slot
;;;      that holds the parent's frame POINTER. Measured: the next read is a memory
;;;      fault, and in PID 1 a memory fault is a kernel panic. We write through the
;;;      pointer instead (SET-INDIRECT-FRAME-VAR).
;;;   3. A variable CLOSED OVER but never SETQ'd is COPIED into each closure made from
;;;      it. Setting the frame's copy does not change a closure already made — which is
;;;      Lisp's semantics, not a bug, but it reads like one, so the doc says so.

(defun sc-number (name)
  "The value of SB-VM's NAME-SC-NUMBER constant, or NIL — looked up by name so that an
   SBCL without (say) a separate CHARACTER-STACK simply contributes nothing."
  (let ((s (find-symbol name :sb-vm)))
    (and s (boundp s) (symbol-value s))))

(defun sc-value-type (scn)
  "The Lisp type a variable stored in storage class SCN can hold, or T for a boxed
   (descriptor) slot, which can hold any object. The unboxed classes are raw machine
   words: what goes in must already BE that kind of number, character or pointer."
  (flet ((in (&rest names) (member scn (mapcar #'sc-number names))))
    (cond ((in "ANY-REG-SC-NUMBER")                              'fixnum)
          ((in "SIGNED-REG-SC-NUMBER" "SIGNED-STACK-SC-NUMBER")    '(signed-byte 64))
          ((in "UNSIGNED-REG-SC-NUMBER" "UNSIGNED-STACK-SC-NUMBER") '(unsigned-byte 64))
          ((in "CHARACTER-REG-SC-NUMBER" "CHARACTER-STACK-SC-NUMBER") 'character)
          ((in "SAP-REG-SC-NUMBER" "SAP-STACK-SC-NUMBER")          'system-area-pointer)
          ((in "SINGLE-REG-SC-NUMBER" "SINGLE-STACK-SC-NUMBER")    'single-float)
          ((in "DOUBLE-REG-SC-NUMBER" "DOUBLE-STACK-SC-NUMBER")    'double-float)
          (t t))))

(defun frame-var-value-type (var)
  "What VAR's storage can hold (see SC-VALUE-TYPE). For an indirect variable the slot
   that matters is the one in the parent frame, not the pointer to it."
  (sc-value-type
   (sb-c:sc+offset-scn (or (sb-di::compiled-debug-var-indirect-sc+offset var)
                           (sb-di::compiled-debug-var-sc+offset var)))))

(defun frame-var-unsettable-reason (var loc frame)
  "NIL when VAR can be written in FRAME at code location LOC; otherwise a short phrase
   saying why not, for the debugger to show. Never signals."
  (handler-case
      (cond ((not (typep var 'sb-di::compiled-debug-var)) "not a compiled variable")
            ((not (typep frame 'sb-di::compiled-frame))   "not a compiled frame")
            ;; :INVALID = dead here (not yet bound, or no longer used); :UNKNOWN =
            ;; sb-di cannot tell, which is what (debug 0) code gives for everything
            ((eq (sb-di:debug-var-validity var loc) :invalid)
             "not live at this point in the function")
            ((not (eq (sb-di:debug-var-validity var loc) :valid))
             "location unknown here (compiled at low debug)")
            ;; sb-di's own marker for a value in a register this frame has no saved
            ;; copy of: there is nowhere to write that the frame would read back.
            ((eq (sb-di::access-compiled-debug-var-slot var frame)
                 :invalid-value-for-unescaped-register-storage)
             "held only in a register here")
            (t nil))
    (serious-condition () "sb-di cannot locate it")))

(defun set-indirect-frame-var (var frame new)
  "Write NEW into an INDIRECT variable: one stored in a PARENT frame and reached from
   FRAME through a saved copy of the parent's frame pointer (a dynamic-extent FLET
   reading its enclosing function's LET). Mirrors the read path in SBCL's
   ACCESS-COMPILED-DEBUG-VAR-SLOT step for step: fetch the parent's frame pointer from
   VAR's own slot, then write at the indirect SC+OFFSET relative to it. SBCL's (SETF
   DEBUG-VAR-VALUE) skips the first step and overwrites the pointer — see trap 2."
  (let* ((escaped (sb-di::compiled-frame-escaped frame))
         (parent  (sb-di::sub-access-debug-var-slot
                   (sb-di::frame-pointer frame)
                   (if escaped
                       (sb-di::compiled-debug-var-sc+offset var)
                       (or (sb-di::compiled-debug-var-save-sc+offset var)
                           (sb-di::compiled-debug-var-sc+offset var)))
                   escaped))
         (parent-fp (sb-sys:int-sap (sb-kernel:get-lisp-obj-address parent)))
         (indirect  (sb-di::compiled-debug-var-indirect-sc+offset var))
         (old (sb-di::sub-access-debug-var-slot parent-fp indirect escaped)))
    ;; closed over AND set: the slot holds a value cell, and the cell is the variable
    (if (sb-di::indirect-value-cell-p old)
        (sb-kernel:value-cell-set old new)
        (sb-di::sub-set-debug-var-slot parent-fp indirect new escaped))
    new))

(defun set-frame-var (var frame new)
  "Write NEW into the local VAR of the standing FRAME, and return the value READ BACK
   from the frame afterwards (not NEW: the read-back is the evidence the write landed).
   Signals a plain ERROR, with a reason, when the variable cannot be written or NEW
   cannot live in its storage — the caller (the debugger) shows the message."
  (let ((loc (sb-di:frame-code-location frame)))
    (let ((why (frame-var-unsettable-reason var loc frame)))
      (when why (error "~a cannot be set: ~a" (frame-var-name var) why)))
    (let ((type (frame-var-value-type var)))
      (unless (typep new type)
        (error "~a is stored as a raw ~(~a~) in this frame; ~s is not one"
               (frame-var-name var) type new)))
    (if (sb-di::compiled-debug-var-indirect-sc+offset var)
        (set-indirect-frame-var var frame new)
        ;; SBCL's own setter handles the plain and the value-cell cases correctly
        (setf (sb-di:debug-var-value var frame) new))
    (sb-di:debug-var-value var frame)))

(defun frame-local-places (frame)
  "FRAME's locals as PLACES — FRAME-LOCALS plus, for each, how to write it: a list of
   (:name STRING :value STRING-or-:UNAVAILABLE :set FN-or-NIL :why STRING-or-NIL).
   FN takes the new value and returns what the frame holds afterwards (SET-FRAME-VAR);
   :WHY says why a local has no FN. The same shape INSPECT-PLACES gives an object's
   fields, applied to a second data source. Never signals."
  (ignore-errors
    (let* ((fun  (sb-di:frame-debug-fun frame))
           (loc  (sb-di:frame-code-location frame))
           (vars (sb-di::debug-fun-debug-vars fun)))
      (when (and vars (plusp (length vars)))
        (loop for v across vars
              for why = (frame-var-unsettable-reason v loc frame)
              collect (list :name (frame-var-name v)
                            :value (frame-var-value v loc frame)
                            :set (unless why
                                   (let ((v v)) (lambda (new) (set-frame-var v frame new))))
                            :why why))))))

(defun eval-in-frame-context (text frame)
  "READ TEXT and evaluate it with FRAME's live locals in scope (sb-di's EVAL-IN-FRAME,
   which binds each valid local as a symbol macro), so the value typed for a local may
   be computed from its neighbours: (* x 3). READ in the home package of the frame's
   function, for the same reason EVAL-IN-CONTEXT reads in a class's package: a local
   named X in LOL.EXAMPLES is not CL-USER::X. Falls back to a plain EVAL when the frame
   has no debug variables to scope over."
  (let* ((name (frame-defn-name frame))
         (sym  (if (consp name) (second name) name))
         (*package* (or (and (symbolp sym) sym (symbol-package sym)) *package*))
         (form (read-from-string text)))
    (handler-case (sb-di:eval-in-frame frame form)
      (sb-di::no-debug-vars () (eval form)))))

;;; ---- re-entering a frame: RESTART-FRAME --------------------------------------
;;; With a local set, there are two ways to see the effect: CONTINUE (when the error
;;; offered that restart — CERROR does) lets the frame run on from where it stopped;
;;; RESTART-FRAME unwinds TO the frame and calls its function again, from the top,
;;; with the arguments the frame holds NOW — so an argument you just set is the
;;; argument of the new call. Both pieces are SBCL's own debugger's (sb-debug's
;;; RESTART-FRAME command): FRAME-CALL reads the call back out of the frame, and
;;; UNWIND-TO-FRAME-AND-CALL unwinds (running every UNWIND-PROTECT cleanup on the way)
;;; and calls a thunk in the frame's place. Internal to SB-DEBUG, but in the core.

(defun frame-restart-refusal (frame)
  "NIL when FRAME can be restarted; otherwise why not, as a short phrase."
  (handler-case
      (multiple-value-bind (name args) (sb-debug::frame-call frame)
        (cond ((not (sb-debug::frame-has-debug-tag-p frame))
               "no unwind tag — compiled at (debug 0)")
              ((not (and (sb-int:legal-fun-name-p name) (fboundp name)))
               "no global function to call again")
              ((some (lambda (a) (typep a 'sb-debug::unprintable-object)) args)
               "an argument is unavailable")
              (t nil)))
    (serious-condition () "sb-debug cannot read the call")))

(defun frame-call-form (frame)
  "FRAME's call as a list (NAME ARG...), with the values its arguments hold now."
  (multiple-value-bind (name args) (sb-debug::frame-call frame)
    (cons name args)))

(defun restart-frame (frame)
  "Unwind to FRAME and call its function again with the arguments the frame holds now
   (the global definition, so a definition re-Accepted meanwhile is the one called).
   Does not return. Signals an ERROR, with the reason, when FRAME cannot be restarted."
  (let ((why (frame-restart-refusal frame)))
    (when why (error "can't restart this frame: ~a" why)))
  (destructuring-bind (name &rest args) (frame-call-form frame)
    (sb-debug::unwind-to-frame-and-call
     frame (lambda () (apply (fdefinition name) args)))))

;;; ======================================================================
;;; The condition system — the STATIC face (§3, phase 4a)
;;; ======================================================================
;;; The live half above is the debugger: a real stack, real restarts. This half is
;;; what the BROWSER can say about a condition type when nothing is signalling —
;;; and it is deliberately the same shape as Senders/Implementors:
;;;
;;;   Signallers  — who ERRORs/SIGNALs/WARNs this type   (the analogue of Senders)
;;;   Handlers    — who HANDLER-BINDs/HANDLER-CASEs it   (the analogue of Implementors)
;;;   Restarts    — what RESTART-CASEs stand around those signalling sites
;;;
;;; None of that is derivable from CLOS: control flow is invisible in the class.
;;; It IS derivable from source, and we have every definition's source in the
;;; registry — so these three are a walk over *REGISTRY*.
;;;
;;; HONEST LIMIT, stated once and surfaced in the view: this is a STATIC
;;; approximation. It sees the lexically apparent forms in the code we recorded. It
;;; cannot see a handler established by a function it calls, a type computed at
;;; runtime, or anything inside SBCL itself (whose source we do not ship — §0.1).
;;; The live debugger remains the authority on what is actually established.

(defun condition-class-p (name)
  "True when NAME designates a condition class."
  (let ((c (as-class name)))
    (and c (subtypep (class-name c) 'condition) t)))

(defun defn-read-package (d)
  "The package to READ D's captured source in — the home package of the name it
   defines, which is the package the file was loaded in. Reading in the right
   package matters for more than tidiness: read `(defmethod collide ((a asteroid)))`
   in the wrong one and ASTEROID is interned somewhere it does not belong. Because
   we read source that was already read once at load time, every symbol in it
   already exists, so a correct *PACKAGE* interns nothing at all."
  (let ((n (defn-name d)))
    (or (and (symbolp n) (symbol-package n))
        (and (consp n) (symbolp (second n)) (symbol-package (second n)))
        *package*)))

(defun defn-form (d)
  "D's captured source READ back into a form, or NIL if it will not read. Reading
   beats matching text: `(handler-case … (division-by-zero …))` is a shape, and a
   shape survives reformatting, line breaks and a symbol appearing in a comment."
  (let ((src (defn-source d)))
    (when (stringp src)
      (let ((*package* (defn-read-package d))
            (*read-eval* nil))                  ; captured source is data, never #.
        (ignore-errors (read-from-string src))))))

(defun walk-form (form fn)
  "Call FN on FORM and every sub-form of it (pre-order, conses only)."
  (funcall fn form)
  (when (consp form)
    (loop for tail = form then (cdr tail)
          while (consp tail)
          do (walk-form (car tail) fn))))

(defun form-arg (form n)
  "Element N of FORM, or NIL if FORM is shorter than that — or IMPROPER. The second
   guard is the one that matters: captured source is full of dotted pairs (an alist
   literal like '(error . simple-error) is one), and SECOND on a dotted pair is a
   type error rather than NIL. A walker over arbitrary recorded source meets them."
  (let ((tail form))
    (dotimes (i n)
      (unless (consp tail) (return-from form-arg nil))
      (setf tail (cdr tail)))
    (and (consp tail) (car tail))))

(defun form-tail (form n)
  "The Nth cdr of FORM as a LIST, or NIL when FORM is improper or too short."
  (let ((tail form))
    (dotimes (i n)
      (unless (consp tail) (return-from form-tail nil))
      (setf tail (cdr tail)))
    (and (listp tail) tail)))

;;; A condition type is written one way at a SIGNALLING site and the opposite way in
;;; a HANDLER clause, so reading them takes two functions rather than one lenient
;;; one. It used to be one, and the cost was a phantom: COMMANDS-FOR contains
;;; (signal type), where TYPE is a parameter, and the sweep duly reported a
;;; condition called TYPE that has never existed. Same for (make-condition type) in
;;; CONDITION-REPORT. Guessing that a variable's NAME is a type name is worse than
;;; declining to answer, because the browser then offers you a view of nothing.

(defun quoted-symbol (x)
  "X if it is (QUOTE symbol), else NIL. A condition type at a SIGNALLING site is
   always quoted — (error 'end-of-file), (make-condition 'off-course :by 3) — so a
   BARE symbol there is a variable holding a type, whose value we cannot know
   statically and must not invent. Resignalling an existing condition object,
   (error c), is likewise not a type and correctly yields NIL."
  (and (consp x) (eq (first x) 'quote) (symbolp (second x)) (second x)))

(defun clause-type-symbol (x)
  "X if it is a bare symbol, or (QUOTE symbol), else NIL. The opposite convention:
   a type in a HANDLER-CASE clause, a HANDLER-BIND binding or a RESTART-CASE spec is
   written bare — (handler-case … (end-of-file (e) …)) — and there it really is a
   literal name, because the macro does not evaluate it."
  (cond ((and (symbolp x) x (not (keywordp x))) x)
        ((and (consp x) (eq (first x) 'quote) (symbolp (second x))) (second x))))

(defparameter *signal-operators*
  '((error . simple-error) (cerror . simple-error) (signal . simple-condition)
    (warn . simple-warning))
  "The signalling operators, each with the type it signals when handed a FORMAT
   STRING instead of a condition type — `(error \"boom\")` really does signal a
   SIMPLE-ERROR, so SIMPLE-ERROR's Signallers list should contain it.")

(defun form-signalled-type (form)
  "The condition type FORM signals, if FORM is a signalling call; else NIL.
   Handles (error 'foo …), (make-condition 'foo …), (signal 'foo), and the
   format-string spellings that mean SIMPLE-ERROR / SIMPLE-WARNING."
  (when (and (consp form) (symbolp (first form)) (first form))
    (let* ((op (first form))
           (arg (form-arg form 1))
           ;; CERROR's first argument is the continue-report string; the type is next.
           (arg (if (eq op 'cerror) (form-arg form 2) arg))
           (entry (and (symbolp op) (assoc op *signal-operators*))))
      (cond
        ((eq op 'make-condition) (quoted-symbol arg))
        ((null entry) nil)
        ((stringp arg) (cdr entry))                ; (error "boom") -> simple-error
        (t (quoted-symbol arg))))))

(defun form-handled-types (form)
  "The condition types FORM catches, if FORM is a handler form; else NIL.
   HANDLER-BIND binds ((type fn) …); HANDLER-CASE clauses are (type (var) …) after
   the protected form; IGNORE-ERRORS is HANDLER-CASE on ERROR wearing a hat."
  (when (consp form)
    (case (and (symbolp (first form)) (first form))
      (handler-bind
       (let ((bindings (form-arg form 1)))
         (remove nil (mapcar (lambda (b) (and (consp b) (clause-type-symbol (first b))))
                             (and (listp bindings) bindings)))))
      (handler-case
       (remove nil (mapcar (lambda (c) (and (consp c) (clause-type-symbol (first c))))
                           (form-tail form 2))))
      (ignore-errors (list 'error))
      (t nil))))

(defun form-restart-names (form)
  "The restart names FORM establishes: RESTART-CASE clauses and WITH-SIMPLE-RESTART."
  (when (consp form)
    (case (and (symbolp (first form)) (first form))
      (restart-case
       (remove nil (mapcar (lambda (c) (and (consp c) (clause-type-symbol (first c))))
                           (form-tail form 2))))
      (with-simple-restart
       (let* ((spec (form-arg form 1))
              (name (and (consp spec) (clause-type-symbol (first spec)))))
         (and name (list name))))
      (t nil))))

(defun map-recorded-forms (fn)
  "Call FN with (DEFN FORM) for every definition whose source reads back. One pass
   over the registry; the three scanners below each do their own, which is fine at
   our scale (a few hundred definitions) and keeps them independently readable."
  (maphash (lambda (name defns)
             (declare (ignore name))
             (dolist (d defns)
               (let ((form (defn-form d)))
                 (when form (funcall fn d form)))))
           *registry*))

(defun subtype-names-of (type)
  "TYPE and every recorded subtype of it — signalling a subtype signals TYPE too,
   so a Signallers list that ignored them would lie by omission."
  (let ((c (as-class type)))
    (if (null c)
        (list type)
        (cons (class-name c)
              (labels ((down (class)
                         (loop for s in (sb-mop:class-direct-subclasses class)
                               append (cons (class-name s) (down s)))))
                (remove-duplicates (down c)))))))

(defun condition-signallers (type)
  "Every recorded definition that signals TYPE (or a subtype), as plists
   (:name :kind :type). The condition analogue of SENDERS."
  (let ((wanted (subtype-names-of type)) (out '()))
    (map-recorded-forms
     (lambda (d form)
       (let ((hits '()))
         (walk-form form (lambda (f)
                           (let ((ty (form-signalled-type f)))
                             (when (member ty wanted) (pushnew ty hits)))))
         (dolist (ty hits)
           (push (list :name (defn-name d) :kind (defn-kind d)
                       :label (defn-label d) :type ty) out)))))
    (sort out #'string-lessp :key (lambda (e) (princ-to-string (getf e :name))))))

(defun condition-handlers (type)
  "Every recorded definition that HANDLER-BINDs / HANDLER-CASEs TYPE *or a
   supertype of it* — a handler on ERROR does catch a DIVISION-BY-ZERO, and a view
   that hid that would answer the wrong question. Plists (:name :kind :type)."
  (let* ((c (as-class type))
         (wanted (if c (mapcar #'class-name (sb-mop:class-precedence-list c)) (list type)))
         (out '()))
    (map-recorded-forms
     (lambda (d form)
       (let ((hits '()))
         (walk-form form (lambda (f)
                           (dolist (ty (form-handled-types f))
                             (when (member ty wanted) (pushnew ty hits)))))
         (dolist (ty hits)
           (push (list :name (defn-name d) :kind (defn-kind d)
                       :label (defn-label d) :type ty) out)))))
    (sort out #'string-lessp :key (lambda (e) (princ-to-string (getf e :name))))))

(defun condition-restarts (type)
  "The restarts a signaller of TYPE stands inside: for each definition that signals
   TYPE, the RESTART-CASE / WITH-SIMPLE-RESTART names established in that same
   definition. Plists (:name :in). This is the shakiest of the three — a restart is
   established at runtime by whoever is on the stack, and the same-definition rule
   is only the part that is lexically visible — so the view labels it as such."
  (let ((signallers (mapcar (lambda (e) (getf e :name)) (condition-signallers type)))
        (out '()))
    (when signallers
      (map-recorded-forms
       (lambda (d form)
         (when (member (defn-name d) signallers :test #'equal)
           (let ((hits '()))
             (walk-form form (lambda (f)
                               (dolist (r (form-restart-names f)) (pushnew r hits))))
             (dolist (r hits) (push (list :name r :in (defn-name d)) out)))))))
    (sort out #'string-lessp :key (lambda (e) (princ-to-string (getf e :name))))))

(defun condition-report (type)
  "TYPE's report string as it would print, or NIL. Built by MAKE-CONDITION on a
   bare instance — many condition types need initargs and simply refuse, which is
   not an error here, just an absent answer."
  (ignore-errors
    (let ((c (make-condition type)))
      (let ((s (princ-to-string c)))
        (and (plusp (length s)) s)))))

(defun condition-info (type)
  "Everything the static condition view needs, as a plist. Reuses CLASS-INFO for the
   anatomy — conditions ARE CLOS classes, so the DAG and the slots come free."
  (let ((i (class-info type)))
    (when i
      (append i (list :report      (condition-report type)
                      :signallers  (condition-signallers type)
                      :handlers    (condition-handlers type)
                      :restarts    (condition-restarts type))))))

(defun show-condition (type)
  "Print the static condition view — the REPL twin of (present (subj-condition …))."
  (let ((i (condition-info type)))
    (if (null i)
        (format t "~&; ~s is not a class.~%" type)
        (progn
          (format t "~&; condition ~s~@[  — ~a~]~%" (getf i :name) (getf i :doc))
          (format t ";   supertypes : ~a~%" (or (getf i :supers) '(none)))
          (format t ";   subtypes   : ~a~%" (or (getf i :subs) '(none)))
          (format t ";   report     : ~@[~a~]~%" (getf i :report))
          (dolist (key '(:signallers :handlers))
            (format t ";   ~(~a~):~%" key)
            (dolist (e (getf i key))
              (format t ";     ~(~a~) ~(~a~)~%" (getf e :name) (getf e :type))))
          (format t ";   restarts (lexical, near signallers):~%")
          (dolist (e (getf i :restarts))
            (format t ";     ~(~a~) in ~(~a~)~%" (getf e :name) (getf e :in)))))
    (finish-output)
    type))

;;; ======================================================================
;;; The rest of the xref wrappers (registry.lisp has senders/referrers/…)
;;; ======================================================================

(defun binders   (name) "Functions that BIND the special variable NAME."
  (mapcar #'car (sb-introspect:who-binds name)))
(defun setters   (name) "Functions that SETQ the special variable NAME."
  (mapcar #'car (sb-introspect:who-sets name)))
(defun macro-users (name) "Functions that expand the macro NAME."
  (mapcar #'car (sb-introspect:who-macroexpands name)))
