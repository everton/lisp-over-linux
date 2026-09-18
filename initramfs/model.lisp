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
;;; The Inspector — every value browsable (the (present (x t)) fallback)
;;; ======================================================================

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
      ((or standard-object structure-object)     ; general: slots by name (LAST)
       (mapcar (lambda (s)
                 (let ((n (sb-mop:slot-definition-name s)))
                   (list :label n
                         :value (if (slot-boundp obj n) (slot-value obj n) :unbound)
                         :set (lambda (new) (setf (slot-value obj n) new)))))
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

(defun quoted-symbol (x)
  "X if it is a bare symbol or (QUOTE symbol), else NIL — how a condition type is
   written at a signalling site: (error 'end-of-file) or (error 'end-of-file :s …)."
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
         (remove nil (mapcar (lambda (b) (and (consp b) (quoted-symbol (first b))))
                             (and (listp bindings) bindings)))))
      (handler-case
       (remove nil (mapcar (lambda (c) (and (consp c) (quoted-symbol (first c))))
                           (form-tail form 2))))
      (ignore-errors (list 'error))
      (t nil))))

(defun form-restart-names (form)
  "The restart names FORM establishes: RESTART-CASE clauses and WITH-SIMPLE-RESTART."
  (when (consp form)
    (case (and (symbolp (first form)) (first form))
      (restart-case
       (remove nil (mapcar (lambda (c) (and (consp c) (quoted-symbol (first c))))
                           (form-tail form 2))))
      (with-simple-restart
       (let* ((spec (form-arg form 1))
              (name (and (consp spec) (quoted-symbol (first spec)))))
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
