;;;; present.lisp — the drill-down PRESENT protocol (code browser §4½ / §4½ affordances).
;;;;
;;;; This is the browser's SPINE, built headless first (like registry.lisp and
;;;; model.lisp): one generic function turns any subject into a VIEW of items, and
;;;; a second turns it into the COMMANDs it affords. Both are PURE — subject in,
;;;; data out, no side effects — so they are fully REPL-testable long before any
;;;; pixels. The eventual UI is a thin renderer over these; the shell (the one
;;;; stateful layer) is what interprets the affordances and mutates trails/menus.
;;;;
;;;;   (present SUBJECT)      -> a VIEW  (title, kind, items[, text])
;;;;   (commands-for SUBJECT) -> list of COMMAND
;;;;
;;;; An ITEM that carries a SUBJECT is a drill: selecting it opens (present that).
;;;; Content never opens anything itself — it only DECLARES (a target, a
;;;; disposition, an action, commands); the shell disposes. See doc/code-browser.org
;;;; §4½ "Content that talks back". Loaded via load-recording after model.lisp.

;;; ---- the data the shell renders -----------------------------------------

(defstruct item
  "One selectable line in a view."
  label                  ; primary text
  detail                 ; secondary/dim text, or NIL
  kind                   ; a Lisp-concept keyword (:function :class :macro …) — the
                         ; shell tints the row by it (concept-color); NIL = plain text
  subject                ; what drilling opens — (present subject) — or NIL
  (disposition :in-place); :in-place | :new-trail | :beside | :replace (a HINT)
  action)                ; a thunk to run instead of drilling, or NIL

(defstruct view
  "What (present subject) returns — a description, never pixels."
  title
  (kind :list)           ; :list :source :reference :class :generic :inspector :matrix
  (items '())            ; list of ITEM
  text                   ; body text for :source / :reference views
  data                   ; structured payload for a non-text render (a :matrix grid)
  (sel 0)                ; the row a list view opens focused on (default: the first)
  subject)               ; back-reference to the presented subject

(defstruct command
  "A type-level affordance. One command surfaces in whatever the shell offers —
   menu, halo, palette, key — per its SURFACES. Declarative: DO is data."
  label
  target                 ; a subject to open, or NIL
  (disposition :new-trail)
  action                 ; a thunk, or NIL
  (surfaces '(:menu))    ; any of :menu :halo :palette :key
  key)                   ; accelerator string, or NIL

;;; Subject wrappers for things that are not first-class CL objects: a named
;;; definition (source), and an xref query (senders/referrers). Presenting a raw
;;; symbol inspects its cells; presenting a SUBJ-DEFN shows its source — the two
;;; readings a symbol has, disambiguated by which subject you hand to PRESENT.
(defstruct (subj-defn (:constructor subj-defn (name &optional kind))) name kind)
(defstruct (subj-xref (:constructor subj-xref (relation name))) relation name)
;; The two generic-function views (§3): coverage (the dispatch matrix) and
;; combination (the effective-method onion for one argument tuple). Both render as
;; text — no grid widget yet — so they ride the existing :reference text pane.
(defstruct (subj-matrix (:constructor subj-matrix (name))) name)
(defstruct (subj-onion  (:constructor subj-onion  (name tuple))) name tuple)
;; One method of a generic function — carries enough (name + qualifiers + specializer
;; tuple) to find its own captured source, so a GF's method list drills to the exact
;; defmethod for each specialised case, not to the combination.
(defstruct (subj-method (:constructor subj-method (name qualifiers specializers)))
  name qualifiers specializers)
;; The Workspace (§3a): a scratch buffer whose verbs eval — Do it / Print it /
;; Inspect it — rather than Accept a whole definition. A :workspace view is an
;; editable text pane the shell drives; the shell owns the eval, staying pure here.
(defstruct (subj-workspace (:constructor subj-workspace ())))
;; The change set (§6): the definitions Accepted (edited live) this session — a
;; browsable list, Smalltalk's Changes. Data comes from CHANGED-DEFINITIONS.
(defstruct (subj-change-set (:constructor subj-change-set ())))

;;; ---- the Spotter: fuzzy-find anything, open it in a trail (§4¾) -----------
;;; Pure data + ranking; the shell owns the overlay and the open-in-new-trail. A
;;; candidate is a plist (:label :detail :subject) — the same shape a list item
;;; carries, so opening one is just (present (getf c :subject)).

(defun subsequence-match-p (query label)
  "True when every character of QUERY appears in LABEL in order (case-insensitive) —
   the fuzzy-finder rule. An empty QUERY matches anything."
  (let ((qi 0) (ql (length query)))
    (when (zerop ql) (return-from subsequence-match-p t))
    (loop for ch across label do
      (when (char-equal ch (char query qi))
        (incf qi)
        (when (= qi ql) (return-from subsequence-match-p t))))
    nil))

(defun word-boundary-char-p (label i)
  "True when position I in LABEL begins a word — index 0, or just past a delimiter.
   (=make-TextView=, =lol.CANVAS=: the capitals here are word starts.)"
  (or (= i 0)
      (member (char label (1- i)) '(#\- #\: #\/ #\. #\Space #\_ #\*))))

(defun word-boundary-match-p (query label)
  "A subsequence match whose FIRST character lands on a word boundary — a stronger
   fuzzy hit than a mid-word one (e.g. =tv= -> make-TextView). Case-insensitive."
  (when (zerop (length query)) (return-from word-boundary-match-p t))
  (loop for i from 0 below (length label)
        when (and (char-equal (char label i) (char query 0))
                  (word-boundary-char-p label i)
                  (subsequence-match-p (subseq query 1) (subseq label (1+ i))))
          do (return-from word-boundary-match-p t))
  nil)

(defun spotter-score (query label)
  "A rank key for LABEL vs QUERY, lower = better, or NIL when there is no match:
   0 exact, 1 prefix, 2 word-boundary subsequence, 3 plain subsequence."
  (let ((q (string-downcase query)) (l (string-downcase label)))
    (cond
      ((string= q l) 0)
      ((and (<= (length q) (length l)) (string= q (subseq l 0 (length q)))) 1)
      ((word-boundary-match-p query label) 2)
      ((subsequence-match-p query label) 3)
      (t nil))))

(defun spotter-candidates ()
  "A fresh list of everything the Spotter can jump to — each a plist
   (:label STRING :detail STRING :subject SUBJECT): the named surfaces (Workspace,
   change set), every package, and every recorded definition (deduped by name, its
   newest kind as the detail). Every row is guarded, since this feeds a live finder
   in PID 1 and one malformed entry must not break the list."
  (let ((out '()))
    (ignore-errors
      (push (list :label "workspace"  :detail "surface" :subject (subj-workspace))  out)
      (push (list :label "change set" :detail "surface" :subject (subj-change-set)) out))
    (dolist (p (ignore-errors (list-all-packages)))
      (ignore-errors
        (push (list :label (string-downcase (package-name p))
                    :detail "package" :subject p)
              out)))
    (ignore-errors
      (maphash (lambda (name defns)
                 (ignore-errors
                   (let* ((kind (and defns (defn-kind (car defns))))
                          ;; a live generic function jumps to its GF VIEW (matrix +
                          ;; methods), not to a source blob of all its methods.
                          (gf   (and (ignore-errors (fboundp name))
                                     (typep (fdefinition name) 'generic-function))))
                     ;; skip a defpackage's :package defn — the package OBJECT is
                     ;; already a candidate (above), so this would just duplicate it.
                     (unless (eq kind :package)
                       (push (list :label (string-downcase (princ-to-string name))
                                   :detail (cond (gf "generic function")
                                                 (kind (string-downcase (symbol-name kind)))
                                                 (t "definition"))
                                   :subject (if gf (fdefinition name) (subj-defn name)))
                             out)))))
               *registry*))
    (nreverse out)))

(defun spotter-filter (query candidates &optional (limit 200))
  "The CANDIDATES matching QUERY, best-first, capped at LIMIT. Match is a
   case-insensitive subsequence; rank is exact > prefix > word-boundary > plain, ties
   broken by shorter label then alphabetically. An empty QUERY returns all, sorted
   alphabetically (also capped)."
  (if (zerop (length query))
      (let ((sorted (sort (copy-list candidates) #'string-lessp
                          :key (lambda (c) (getf c :label)))))
        (if (> (length sorted) limit) (subseq sorted 0 limit) sorted))
      (let ((scored '()))
        (dolist (c candidates)
          (let ((s (spotter-score query (getf c :label))))
            (when s (push (cons s c) scored))))
        (let* ((ranked (sort scored
                             (lambda (a b)
                               (let ((sa (car a)) (sb (car b))
                                     (la (getf (cdr a) :label)) (lb (getf (cdr b) :label)))
                                 (cond ((/= sa sb) (< sa sb))
                                       ((/= (length la) (length lb)) (< (length la) (length lb)))
                                       (t (string-lessp la lb)))))))
               (result (mapcar #'cdr ranked)))
          (if (> (length result) limit) (subseq result 0 limit) result)))))

;;; ---- the two generic functions ------------------------------------------

(defgeneric present (subject)
  (:documentation "A VIEW of SUBJECT. Pure — no side effects. Drillable items
    carry the subject to (present ...) next. The (T) method is the inspector, so
    nothing is ever a dead end."))

(defgeneric commands-for (subject)
  (:documentation "The COMMANDs SUBJECT affords, by type. Pure."))

;;; ---- packages: their definitions ----------------------------------------

(defmethod present ((p package))
  (let ((items '()) (seen-gfs '()))
    (maphash (lambda (name defns)
               (when (and (symbolp name) (eq (symbol-package name) p))
                 (if (and (fboundp name) (typep (fdefinition name) 'generic-function))
                     ;; a generic function is ONE entry that drills into its own view
                     ;; (matrix + methods + source); its methods don't clutter the
                     ;; package list. Its subject is the GF object, not a subj-defn.
                     (unless (member name seen-gfs)
                       (push name seen-gfs)
                       (push (make-item :label (string-downcase (princ-to-string name))
                                        :detail "generic function" :kind :generic-function
                                        :subject (fdefinition name))
                             items))
                     (dolist (d defns)
                       (push (make-item :label  (string-downcase (princ-to-string name))
                                        :detail (string-downcase (princ-to-string (defn-kind d)))
                                        :kind (defn-kind d)
                                        :subject (subj-defn name (defn-kind d)))
                             items)))))
             *registry*)
    (make-view :title (format nil "package ~a" (package-name p))
               :kind :list :subject p
               :items (sort items #'string< :key #'item-label))))

;;; ---- a definition: its source (or reference, for a builtin) --------------

(defmethod present ((d subj-defn))
  (let ((src (source-of (subj-defn-name d) (subj-defn-kind d))))
    (if src
        (make-view :title (format nil "~(~a~) ~(~a~)"
                                  (or (subj-defn-kind d) "definition") (subj-defn-name d))
                   :kind :source :subject d
                   :text (if (listp src) (format nil "~{~a~^~%~%~}" src) src))
        (make-view :title (format nil "~a" (subj-defn-name d))
                   :kind :reference :subject d
                   :text (with-output-to-string (*standard-output*)
                           (show-reference (subj-defn-name d)))))))

(defmethod commands-for ((d subj-defn))
  (let ((name (subj-defn-name d)))
    (remove nil
      (list
       (when (fboundp name)
         (make-command :label "Senders" :key "M-." :surfaces '(:menu :key :halo)
                       :target (subj-xref :senders name)))
       (when (fboundp name)
         (make-command :label "Disassemble" :surfaces '(:menu)
                       :action (lambda () (disassemble name))))
       (make-command :label "Inspect" :key "C-c C-i" :surfaces '(:menu :halo)
                     :target name)))))                ; the symbol -> its cells

;;; ---- xref results: a list you can drill back into definitions ------------

(defmethod present ((x subj-xref))
  (let ((names (ecase (subj-xref-relation x)
                 (:senders   (senders   (subj-xref-name x)))
                 (:referrers (referrers (subj-xref-name x))))))
    (make-view :title (format nil "~(~a~) of ~(~a~)" (subj-xref-relation x) (subj-xref-name x))
               :kind :list :subject x
               :items (mapcar (lambda (n)
                                (make-item :label (string-downcase (princ-to-string n))
                                           :subject (subj-defn n)))
                              names))))

;;; ---- classes: the DAG + slots -------------------------------------------

(defmethod present ((c class))
  (let ((i (class-info (class-name c))))
    (make-view
     :title (format nil "~(~a~) ~(~a~)" (getf i :kind) (getf i :name))
     :kind :class :subject c
     ;; labels stay within the font's coverage (§6.2): the detail distinguishes
     ;; super- from sub-class, so no >255 arrow glyphs in the text.
     :items (append
             (mapcar (lambda (s) (make-item :label (string-downcase (princ-to-string s))
                                            :detail "superclass" :kind :class :subject (find-class s)))
                     (getf i :supers))
             (mapcar (lambda (s) (make-item :label (string-downcase (princ-to-string s))
                                            :detail "subclass" :kind :class :subject (find-class s)))
                     (getf i :subs))
             (mapcar (lambda (sl) (make-item :label (format nil "slot ~(~a~)" (getf sl :name))
                                             :detail (and (getf sl :readers) "has readers")
                                             :kind :slot))
                     (getf i :direct-slots))))))

;;; ---- generic functions: methods, coverage, combination ------------------

(defun method-tuple-label (m)
  "A method as its qualifiers + specializer tuple, e.g. \":around (button surface)\"."
  (format nil "~@[~{~(~a~) ~}~](~{~(~a~)~^ ~})"
          (getf m :qualifiers) (getf m :specializers)))

(defun format-matrix (m)
  "Render dispatch-matrix data (model.lisp) as a monospace grid / degraded list."
  (with-output-to-string (s)
    (ecase (getf m :mode)
      (:matrix
       (let* ((rows (getf m :rows)) (cols (getf m :cols)) (cells (getf m :cells))
              (rowlab (mapcar (lambda (r) (string-downcase (princ-to-string r))) rows))
              (collab (mapcar (lambda (c) (string-downcase (princ-to-string c))) cols))
              (rw (reduce #'max rowlab :key #'length :initial-value 6))
              (cw (max 3 (reduce #'max collab :key #'length :initial-value 3))))
         (format s "dispatch matrix — ~(~a~)~%arg ~d (rows) x arg ~d (cols)~%~%"
                 (getf m :name) (getf m :row-pos) (getf m :col-pos))
         (format s "~va | ~{~va~^ ~}~%" rw "" (mapcan (lambda (c) (list cw c)) collab))
         (format s "~a-+-~a~%" (make-string rw :initial-element #\-)
                 (make-string (+ (* cw (length cols)) (max 0 (1- (length cols))))
                              :initial-element #\-))
         (loop for rl in rowlab for rowcells in cells do
           (format s "~va | ~{~va~^ ~}~%" rw rl
                   (mapcan (lambda (cell)
                             (list cw (if cell (princ-to-string (length cell)) ".")))
                           rowcells)))
         (format s "~%counts are primary methods; . is a gap (no method for that~%")
         (format s "tuple). :around / :before / :after show in the effective method.~%")))
      (:list
       (format s "~(~a~) dispatches on one argument — the ordered method list:~%~%"
               (getf m :name))
       (dolist (mi (getf m :methods)) (format s "  ~a~%" (method-tuple-label mi))))
      (:multi
       (format s "~(~a~) dispatches on ~d arguments — full specializer tuples:~%~%"
               (getf m :name) (length (getf m :active)))
       (dolist (mi (getf m :methods)) (format s "  ~a~%" (method-tuple-label mi)))))))

(defun format-onion (o)
  "Render an effective-method-onion (model.lisp) as text; indentation is the call
   stack — :around wraps the core, which runs :before, the primary call-next-method
   chain, then :after."
  (with-output-to-string (s)
    (format s "effective method — ~(~a~) (~{~(~a~)~^ ~})~%" (getf o :name) (getf o :on))
    (format s "~a~%~%"
            (if (getf o :definitive-p)
                "classes fully decide this call."
                "an eql method might also apply — classes alone can't decide."))
    (flet ((emit (indent list &optional arrows)
             (loop for m in list for i from 0 do
               (format s "~a~a~a~%" (make-string indent :initial-element #\Space)
                       (if (and arrows (plusp i)) "-> " "") (method-tuple-label m)))))
      (let ((around (getf o :around)) (before (getf o :before))
            (primary (getf o :primary)) (after (getf o :after)))
        (if around
            (progn (format s ":around   outermost first, wraps everything~%")
                   (emit 6 around)
                   (format s "  |  call-next-method reaches the core~%"))
            (format s ":around   (none)~%"))
        (format s "core~%")
        (format s "  :before   most-specific first, all run~%")
        (if before (emit 6 before) (format s "      (none)~%"))
        (format s "  primary   the call-next-method chain (inward)~%")
        (if primary (emit 6 primary t) (format s "      (NO applicable primary method)~%"))
        (format s "  :after    least-specific first, all run~%")
        (if after (emit 6 after) (format s "      (none)~%"))))))

(defmethod present ((s subj-matrix))
  (let ((m (dispatch-matrix (subj-matrix-name s))))
    ;; A true 2-axis grid becomes a CLICKABLE :matrix view — the shell renders the
    ;; cells and a click drills the effective-method onion for that (row,col) class
    ;; pair. A degraded (0/1/3+ axis) matrix has no grid to click, so it stays text.
    (if (eq (getf m :mode) :matrix)
        (make-view :title (format nil "matrix ~(~a~)" (subj-matrix-name s))
                   :kind :matrix :subject s :data m
                   :text (format-matrix m))          ; text kept as the fallback body
        (make-view :title (format nil "matrix ~(~a~)" (subj-matrix-name s))
                   :kind :reference :subject s
                   :text (format-matrix m)))))

(defun onion-role (m)
  "A one-line note on WHEN a method in the combination runs, from its qualifier."
  (case (first (getf m :qualifiers))
    (:around "around · wraps the call")
    (:before "before · runs first")
    (:after  "after · runs last")
    (t       "primary · the call-next-method chain")))

(defmethod present ((s subj-onion))
  "The effective method for one argument tuple, as a BROWSABLE LIST in execution order
   (:around → :before → primary → :after). Each row drills to that method's own source
   (subj-method — editable when it is ours), and the nav split previews it on the right,
   so the combination and each participant's code are visible together."
  (let* ((o    (apply #'effective-method-onion (subj-onion-name s) (subj-onion-tuple s)))
         (name (getf o :name))
         (items '()))
    (flet ((add (m)
             (push (make-item :label (method-tuple-label m)
                              :detail (onion-role m)
                              :kind :method
                              :subject (subj-method name (getf m :qualifiers)
                                                    (getf m :specializers)))
                   items)))
      (dolist (m (getf o :around))  (add m))
      (dolist (m (getf o :before))  (add m))
      (dolist (m (getf o :primary)) (add m))
      (dolist (m (getf o :after))   (add m)))
    (make-view
     :title (format nil "effective ~(~a~) (~{~(~a~)~^ ~})" name (subj-onion-tuple s))
     :kind :list :subject s
     ;; open focused on the PRIMARY (what actually runs) — it sits after the :around and
     ;; :before layers, so its row index is however many of those there are.
     :sel (+ (length (getf o :around)) (length (getf o :before)))
     :items (or (nreverse items)
                (list (make-item :label "(no applicable primary method)"
                                 :detail (if (getf o :definitive-p) "classes decide"
                                             "an eql method might still apply")))))))

(defmethod present ((m subj-method))
  "One method of a generic function, as its OWN source — the exact defmethod for this
   specialised case. When we captured that source it is an EDITABLE :source pane (like
   any other definition — C-c C-c Accept recompiles just this method); when we did not
   (a builtin or inherited method) it falls back to the read-only effective method for
   its tuple, so a method row is never a dead end."
  (let* ((name  (subj-method-name m))
         (specs (subj-method-specializers m))
         (quals (subj-method-qualifiers m))
         (src   (method-source name quals specs))
         (title (format nil "method ~(~a~) ~@[~(~a~) ~](~{~(~a~)~^ ~})"
                        name (first quals) specs)))
    (if src
        (make-view :title title :kind :source :subject m :text src)
        (make-view :title title :kind :reference :subject m
                   :text (or (ignore-errors
                              (format-onion (apply #'effective-method-onion name specs)))
                             (format nil "~a~%~%(no captured source for this method)" title))))))

(defun accept-target (subject)
  "The (values NAME KIND LABEL) that ACCEPT needs to file a pane's edited source back
   into the registry — for a subj-defn or a subj-method. The edited form is still
   authoritative (accept-source re-derives these from it); these are the fallbacks and,
   for a method, the specializer LABEL that keeps Accept on the right method."
  (typecase subject
    (subj-method (values (subj-method-name subject) :method
                         (method-source-label (subj-method-qualifiers subject)
                                              (subj-method-specializers subject))))
    (subj-defn   (values (subj-defn-name subject) (subj-defn-kind subject) nil))
    (t           (values nil nil nil))))

(defmethod present ((gf generic-function))
  (let* ((name (sb-mop:generic-function-name gf))
         (methods (methods-of name)))
    (make-view
     :title (format nil "generic-function ~(~a~)" name)
     :kind :generic :subject gf
     ;; the dispatch matrix (coverage) on top; then each method — drilling one opens
     ;; that method's OWN source (the defmethod for its specialised case).
     :items (list*
             (make-item :label "dispatch matrix"
                        :detail (format nil "~d method~:p" (length methods))
                        :disposition :new-trail
                        :subject (subj-matrix name))
             (make-item :label "definition source"
                        :detail "the defgeneric form"
                        :subject (subj-defn name :generic))
             (mapcar (lambda (m)
                       (make-item
                        :label (method-tuple-label m)
                        :detail (if (getf m :source) "specialised method" "method (no source)")
                        :kind :method
                        :subject (subj-method name (getf m :qualifiers) (getf m :specializers))))
                     methods)))))

;;; ---- the Workspace: a scratch buffer with the editor's hands (§3a) --------

(defmethod present ((s subj-workspace))
  (make-view
   :title "workspace" :kind :workspace :subject s
   :text ";; Workspace — a Lisp scratch buffer (not the REPL; this is the notebook).
;;   C-x C-e   Do it       eval the form before point (result in the status line)
;;   C-c C-p   Print it    eval and weave the result inline, after the form
;;   C-c C-i   Inspect it   eval and open the value in the inspector
;;   C-c C-d   Debug it     eval with the debugger armed (try a form that errors)
;;   C-g       leave
;; Editing is the text view's: caret, selection, syntax, auto-indent.

(+ 1 2)
"))

(defmethod present ((s subj-change-set))
  "The change set: every definition Accepted this session, each drilling to its
   (now current) source. Empty until you edit — C-c C-c in a source pane Accepts."
  (let ((changes (changed-definitions)))
    (make-view
     :title "change set — definitions edited this session"
     :kind :list :subject s
     :items (if changes
                (mapcar (lambda (pair)
                          (destructuring-bind (name . n) pair
                            (make-item :label (string-downcase (princ-to-string name))
                                       :detail (format nil "~d prior version~:p" n)
                                       :subject (subj-defn name))))
                        changes)
                (list (make-item
                       :label "(nothing edited yet)"
                       :detail "C-c C-c in a source pane Accepts an edit"))))))

;;; ---- the T fallback: the inspector. NOTHING is a dead end ----------------

(defmethod present ((x t))
  (make-view :title (format nil "~s" x) :kind :inspector :subject x
             :items (mapcar (lambda (f)
                              (make-item :label  (format nil "~s" (car f))
                                         :detail (let ((*print-length* 6) (*print-level* 3))
                                                   (format nil "~s" (cdr f)))
                                         :subject (cdr f)))     ; drill into the value
                            (inspect-fields x))))

(defmethod commands-for ((x t))
  (list (make-command :label "Inspect" :key "C-c C-i" :surfaces '(:menu :halo) :target x)))

;;; ---- REPL demo: render a view (and its commands) as text -----------------

(defun show-view (subject)
  "Print (present subject) — the headless proof that the protocol works: a title,
   the drillable items, and the commands the subject affords."
  (let ((v (present subject)) (cmds (commands-for subject)))
    (format t "~&== ~a   [~(~a~)]~%" (view-title v) (view-kind v))
    (when (view-text v)
      (let ((tx (view-text v)))
        (format t "~a~@[…~]~%" (subseq tx 0 (min 240 (length tx))) (> (length tx) 240))))
    (dolist (it (view-items v))
      (format t "   • ~a~@[  ~a~]~@[   → ~a~]~%"
              (item-label it) (item-detail it)
              (and (item-subject it)
                   (if (eq (item-disposition it) :in-place) "drill"
                       (format nil "open ~(~a~)" (item-disposition it))))))
    (when cmds
      (format t "   commands: ~{~a~^  ~}~%"
              (mapcar (lambda (c) (format nil "[~a~@[ ~a~] →~(~a~)]"
                                          (command-label c) (command-key c)
                                          (command-disposition c)))
                      cmds)))
    (finish-output)
    v))
