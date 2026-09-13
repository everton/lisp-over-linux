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
  group                  ; a section label (a string), or NIL. Consecutive items sharing
                         ; a GROUP are one visual block; the shell rules between blocks
                         ; and heads each one. Declarative like everything else here —
                         ; content says "these belong together", the shell decides how
                         ; that looks. Views that need no sections simply leave it NIL.
  subject                ; what drilling opens — (present subject) — or NIL
  (disposition :in-place); :in-place | :new-trail | :beside | :replace (a HINT)
  action)                ; a thunk to run instead of drilling, or NIL

(defstruct view
  "What (present subject) returns — a description, never pixels."
  title
  (kind :list)           ; :list :source :reference :class :generic :inspector :matrix :stage
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
(defvar *stage-subject-fn* nil
  "Set by initramfs/stage.lisp (§3b): (funcall it GF-NAME) -> a stage subject, or NIL.

   present.lisp loads before stage.lisp and must not mention it, but the
   generic-function view is where a stage has to be offered from — so the dependency
   is inverted into this one hook rather than reordering the build for a demo.")

(defstruct (subj-workspace (:constructor subj-workspace ())))
;; The change set (§6): the definitions Accepted (edited live) this session — a
;; browsable list, Smalltalk's Changes. Data comes from CHANGED-DEFINITIONS.
(defstruct (subj-change-set (:constructor subj-change-set ())))
;; The module index: OUR FILES ARE OUR CATEGORIES (model.lisp's CATEGORIZE :file).
;; A package view is one flat list of every definition in the image; that is the
;; wrong granularity for finding your way around, because it throws away the one
;; grouping the source already has — which module a definition came from.
;; SUBJ-MODULES is the index (one row per file); SUBJ-MODULE is one file's
;; definitions. Provenance comes from DEFN-FILE, captured by LOAD-RECORDING.
(defstruct (subj-modules (:constructor subj-modules ())))
(defstruct (subj-module  (:constructor subj-module (category))) category)
;; A condition type's STATIC face (§3, phase 4a). The LIVE face is the debugger —
;; a real stack with real restarts. This is what the browser can say when nothing
;; is signalling, and it deliberately mirrors Senders/Implementors: anatomy (the
;; class DAG, slots, the report string), Signallers, Handlers, and the restarts
;; standing around the signalling sites. Presenting the CLASS shows the DAG;
;; presenting a SUBJ-CONDITION shows the control flow — the same "two readings,
;; disambiguated by which subject you hand to PRESENT" trick as symbol vs subj-defn.
(defstruct (subj-condition (:constructor subj-condition (name))) name)

(defun module-index ()
  "Alist of (CATEGORY . COUNT) over every recorded definition, keyed by the module
   it was loaded from (CATEGORY-OF … :file), alphabetical so the list reads the
   same every time. A definition with no recorded file is skipped rather than
   bucketed under NIL — that only happens for things we did not load-record.
   Defined up here because the Spotter (below) offers every module as a
   destination, and it is nicer than a forward reference."
  (let ((counts (make-hash-table :test 'eql)) (out '()))
    (maphash (lambda (name defns)
               (declare (ignore name))
               (dolist (d defns)
                 (let ((c (ignore-errors (category-of d :file))))
                   (when c (incf (gethash c counts 0))))))
             *registry*)
    (maphash (lambda (c n) (push (cons c n) out)) counts)
    (sort out #'string-lessp :key (lambda (e) (princ-to-string (car e))))))

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
   (:label STRING :detail STRING :group STRING :subject SUBJECT): the named surfaces
   (Workspace, change set), every package, and every recorded definition (deduped by
   name, its newest kind as the detail). Every row is guarded, since this feeds a live
   finder in PID 1 and one malformed entry must not break the list.

   GROUP is the design mock-up's banding (media/browser-views-design.html §1):
   \"definitions\", \"packages & files\", \"surfaces\" — nine matches spread over three
   kinds of thing read very differently from nine matches in a heap."
  (let ((out '()))
    (ignore-errors
      (push (list :label "workspace"  :detail "surface" :group "surfaces"
                  :subject (subj-workspace))  out)
      (push (list :label "change set" :detail "surface" :group "surfaces"
                  :subject (subj-change-set)) out)
      (push (list :label "modules"    :detail "surface" :group "surfaces"
                  :subject (subj-modules))    out))
    ;; each module by name, so typing "net" finds net.lisp itself and not only the
    ;; definitions inside it — the index is a destination, not just a waypoint.
    (dolist (e (ignore-errors (module-index)))
      (ignore-errors
        (push (list :label (string-downcase (symbol-name (car e)))
                    :detail (format nil "module · ~d definition~:p" (cdr e))
                    :group "packages & files"
                    :subject (subj-module (car e)))
              out)))
    (dolist (p (ignore-errors (list-all-packages)))
      (ignore-errors
        (push (list :label (string-downcase (package-name p))
                    :detail "package" :group "packages & files" :subject p)
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
                                   :group "definitions"
                                   :subject (if gf (fdefinition name) (subj-defn name)))
                             out)))))
               *registry*))
    (nreverse out)))

(defvar *spotter-recents* '()
  "The subjects most recently opened, newest first, as Spotter candidate plists. The
   design mock-up (§1) says the thing you were just reading is always one click away
   \"as a tab, a breadcrumb crumb, or in the Spotter's recents\" — this is that third
   answer. Filled by the shell as it opens panes; capped, because a finder's job is to
   be short.")

(defparameter +recents-kept+ 8)

(defun note-recent (subject label &optional detail kind)
  "Record that SUBJECT was just opened, for the Spotter's recents. Deduped by LABEL —
   revisiting something should move it up the list, not add a second copy. KIND is the
   Lisp concept it is (:class, :function, …) so the row can be tinted like every other
   row; \"recent\" is a provenance, not a kind, and says nothing about what the thing is."
  (ignore-errors
    (setf *spotter-recents*
          (cons (list :label (string-downcase (princ-to-string label))
                      :detail (or detail "recent") :group "recent"
                      :kind kind :subject subject)
                (remove (string-downcase (princ-to-string label)) *spotter-recents*
                        :key (lambda (c) (getf c :label)) :test #'string=)))
    (when (> (length *spotter-recents*) +recents-kept+)
      (setf *spotter-recents* (subseq *spotter-recents* 0 +recents-kept+)))))

(defun spotter-filter (query candidates &optional (limit 200))
  "The CANDIDATES matching QUERY, best-first, capped at LIMIT. Match is a
   case-insensitive subsequence; rank is exact > prefix > word-boundary > plain, ties
   broken by shorter label then alphabetically. An empty QUERY returns all, sorted
   alphabetically (also capped)."
  (if (zerop (length query))
      ;; A fresh Spotter opens on your RECENTS, then everything else alphabetically.
      ;; With no query there is nothing to rank by, and "where was I" is the only
      ;; question an empty finder can usefully answer.
      (let* ((recent-labels (mapcar (lambda (c) (getf c :label)) *spotter-recents*))
             (rest (sort (remove-if (lambda (c) (member (getf c :label) recent-labels
                                                        :test #'string=))
                                    (copy-list candidates))
                         #'string-lessp :key (lambda (c) (getf c :label))))
             (sorted (append *spotter-recents* rest)))
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

;;; ---- packages and modules: lists of definitions --------------------------

(defun definition-items (keep-p)
  "Items for every recorded definition KEEP-P accepts — it is called with
   (NAME DEFN) and returns true to include that record. A generic function
   collapses to ONE row that drills into its own view (matrix + methods), so its
   methods do not clutter the list; everything else gets a row per DEFN record.
   Shared by the package view and the module view so the two look, sort and tint
   identically — the only difference between them is this predicate. Every probe
   is guarded: this runs in PID 1, and one malformed entry must not break a list."
  (let ((items '()) (seen-gfs '()))
    (maphash
     (lambda (name defns)
       (dolist (d defns)
         (when (ignore-errors (funcall keep-p name d))
           (if (and (ignore-errors (fboundp name))
                    (ignore-errors (typep (fdefinition name) 'generic-function)))
               (unless (member name seen-gfs :test #'equal)
                 (push name seen-gfs)
                 (push (make-item :label (string-downcase (princ-to-string name))
                                  :detail "generic function" :kind :generic-function
                                  :subject (fdefinition name))
                       items))
               (push (make-item :label  (string-downcase (princ-to-string name))
                                :detail (string-downcase (princ-to-string (defn-kind d)))
                                :kind (defn-kind d)
                                :subject (subj-defn name (defn-kind d)))
                     items)))))
     *registry*)
    (sort items #'string< :key #'item-label)))

(defmethod present ((p package))
  (make-view :title (format nil "package ~a" (package-name p))
             :kind :list :subject p
             :items (definition-items
                        (lambda (name d)
                          (declare (ignore d))
                          (and (symbolp name) (eq (symbol-package name) p))))))

;;; ---- modules: the browser's table of contents ----------------------------
;;; A flat package list answers "what exists"; it does not answer "where does
;;; this live". The registry already knows — LOAD-RECORDING stamps each DEFN
;;; with the file it came from — so grouping by module costs nothing but a
;;; predicate, and gives back the structure build.sh's LISP_SOURCES describes.

(defmethod present ((s subj-modules))
  "The module index — one row per source file, drilling into its definitions."
  (let ((mods (module-index)))
    (make-view
     :title "modules — every definition, by the file it came from"
     :kind :list :subject s
     :items (if mods
                (mapcar (lambda (e)
                          (destructuring-bind (cat . n) e
                            (make-item :label (string-downcase (symbol-name cat))
                                       :detail (format nil "~d definition~:p" n)
                                       :kind :package   ; a container, tinted like one
                                       :subject (subj-module cat))))
                        mods)
                (list (make-item :label "(no recorded modules)"
                                 :detail "nothing was loaded with load-recording"))))))

(defmethod present ((m subj-module))
  (let ((cat (subj-module-category m)))
    (make-view :title (format nil "module ~(~a~).lisp" cat)
               :kind :list :subject m
               :items (definition-items
                          (lambda (name d)
                            (declare (ignore name))
                            (eql (ignore-errors (category-of d :file)) cat))))))

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
       ;; M-, not M-. — M-. is jump-to-definition (SLIME's meaning, and what the
       ;; shell actually binds). This command advertised M-. for Senders while the
       ;; shell used it for the jump, so the one key the status line named was the
       ;; one key that did something else.
       (when (fboundp name)
         (make-command :label "Senders" :key "M-," :surfaces '(:menu :key :halo)
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
             ;; A condition IS a class, so it lands here — but the interesting half
             ;; of a condition is not its shape, it is who signals and who handles
             ;; it, and that is a different subject (§3). Offer the door.
             (when (condition-class-p (class-name c))
               (list (make-item :label "signallers, handlers & restarts"
                                :detail "the condition's control flow"
                                :kind :condition :group "control flow"
                                :disposition :new-trail
                                :subject (subj-condition (class-name c)))))
             (mapcar (lambda (s) (make-item :label (string-downcase (princ-to-string s))
                                            :detail "superclass" :kind :class :group "superclasses"
                                            :subject (find-class s)))
                     (getf i :supers))
             (mapcar (lambda (s) (make-item :label (string-downcase (princ-to-string s))
                                            :detail "subclass" :kind :subclass :group "subclasses"
                                            :subject (find-class s)))
                     (getf i :subs))
             (mapcar (lambda (sl) (make-item :label (format nil "slot ~(~a~)" (getf sl :name))
                                             :detail (and (getf sl :readers) "has readers")
                                             :kind :slot :group "slots"))
                     (getf i :direct-slots))))))

;;; ---- conditions: the static face of the debugger (§3, phase 4a) ----------
;;; The generic-function views answer "where do methods live" and "what actually
;;; runs". The condition view answers the two questions CODE CANNOT SHOW, because
;;; the condition system is control flow and control flow is invisible in a class:
;;; who SIGNALS this, and who HANDLES it. They are the exact analogues of Senders
;;; and Implementors, and model.lisp computes both by walking the registry's
;;; captured source. Everything here is a thin renderer over CONDITION-INFO.

(defun condition-group-items (entries group kind detail-fn)
  "ENTRIES (plists from CONDITION-SIGNALLERS / CONDITION-HANDLERS) as items in
   GROUP, each drilling to the definition it names."
  (mapcar (lambda (e)
            (make-item :label (format nil "~(~a~)~@[ ~a~]"
                                      (princ-to-string (getf e :name))
                                      ;; a method's specializer label, so two methods
                                      ;; of one generic function are told apart
                                      (getf e :label))
                       :detail (funcall detail-fn e)
                       :kind kind :group group
                       :subject (subj-defn (getf e :name) (getf e :kind))))
          entries))

(defmethod present ((s subj-condition))
  (let* ((type (subj-condition-name s))
         (i (condition-info type)))
    (if (null i)
        (make-view :title (format nil "~(~a~) ~c not a condition class"
                                  type (code-char #x2014))
                   :kind :list :subject s)
        (make-view
         ;; NOT "condition <name>" — that is exactly what (present (c class)) titles
         ;; ITS view, since CLASS-KIND reports a condition class as :condition. Two
         ;; different views of the same type with the same header is a way to get lost
         ;; one drill in. The " — " gloss also shortens cleanly for the tab strip.
         :title (format nil "~(~a~) ~c signallers, handlers & restarts"
                        (getf i :name) (code-char #x2014))
         :kind :list :subject s
         :items
         (append
          ;; --- anatomy: it IS a class, so say so and offer the class view ------
          (list (make-item :label (string-downcase (princ-to-string (getf i :name)))
                           :detail (or (getf i :report) "the condition class + its DAG")
                           :kind :condition :group "anatomy"
                           :subject (find-class (getf i :name) nil)))
          (mapcar (lambda (n)
                    (make-item :label (string-downcase (princ-to-string n))
                               :detail "supertype" :kind :condition :group "anatomy"
                               :subject (find-class n nil)))
                  (getf i :supers))
          (mapcar (lambda (n)
                    (make-item :label (string-downcase (princ-to-string n))
                               :detail "subtype" :kind :condition :group "anatomy"
                               :subject (find-class n nil)))
                  (getf i :subs))
          (mapcar (lambda (sl)
                    (make-item :label (format nil "slot ~(~a~)" (getf sl :name))
                               :detail (let ((r (getf sl :readers)))
                                         (and r (format nil "reader ~(~a~)" (first r))))
                               :kind :slot :group "anatomy"))
                  (getf i :direct-slots))
          ;; --- signallers: the analogue of Senders -----------------------------
          (or (condition-group-items
               (getf i :signallers) "signallers" :function
               (lambda (e) (format nil "signals ~(~a~)" (getf e :type))))
              (list (make-item :label "(nobody in this image signals it)"
                               :group "signallers")))
          ;; --- handlers: the analogue of Implementors --------------------------
          (or (condition-group-items
               (getf i :handlers) "handlers" :function
               (lambda (e) (format nil "handles ~(~a~)" (getf e :type))))
              (list (make-item :label "(nobody in this image handles it)"
                               :group "handlers")))
          ;; --- restarts: what a signaller stands inside ------------------------
          (or (mapcar (lambda (e)
                        (make-item :label (string-downcase (princ-to-string (getf e :name)))
                                   :detail (format nil "in ~(~a~)" (getf e :in))
                                   :kind :restart :group "restarts"
                                   :subject (subj-defn (getf e :in))))
                      (getf i :restarts))
              (list (make-item :label "(no restart-case around a signalling site)"
                               :group "restarts")))
          ;; --- and the honest footnote ----------------------------------------
          (list (make-item
                 :label "static approximation — the live debugger is the authority"
                 :detail "lexical scan of recorded source; SBCL's own source is not shipped"
                 :group "note")))))))

(defmethod commands-for ((s subj-condition))
  (let ((type (subj-condition-name s)))
    (remove nil
      (list (make-command :label "Class view" :surfaces '(:menu :halo)
                          :target (find-class type nil))
            (make-command :label "Signal it" :surfaces '(:menu)
                          :action (lambda () (signal type)))))))

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

(defun onion-phase (m)
  "Which phase of the combination M belongs to: \"around\" / \"before\" / \"primary\"
   / \"after\". Used as the row's GROUP, so the onion renders as four ruled bands in
   execution order — the waterfall the view is trying to show."
  (case (first (getf m :qualifiers))
    (:around "around")
    (:before "before")
    (:after  "after")
    (t       "primary")))

(defun onion-role (m &optional invoked)
  "A one-line note on WHEN M runs. INVOKED marks the ONE primary the call actually
   enters — the others in the chain run only if it calls CALL-NEXT-METHOD, which is a
   choice, not a certainty. Saying so is the difference between a list of candidates
   and a picture of what happens."
  (case (first (getf m :qualifiers))
    (:around "wraps the call")
    (:before "runs first")
    (:after  "runs last")
    (t (if invoked
           "THE call — the most specific primary"
           "next in the chain, only if call-next-method"))))

(defmethod present ((s subj-onion))
  "The effective method for one argument tuple, as a BROWSABLE LIST in execution order
   (:around → :before → primary → :after). Each row drills to that method's own source
   (subj-method — editable when it is ours), and the nav split previews it on the right,
   so the combination and each participant's code are visible together."
  (let* ((o    (apply #'effective-method-onion (subj-onion-name s) (subj-onion-tuple s)))
         (name (getf o :name))
         (items '()))
    (flet ((add (m &optional invoked)
             (push (make-item :label (method-tuple-label m)
                              :detail (onion-role m invoked)
                              :kind :method
                              :group (onion-phase m)
                              :subject (subj-method name (getf m :qualifiers)
                                                    (getf m :specializers)))
                   items)))
      (dolist (m (getf o :around))  (add m))
      (dolist (m (getf o :before))  (add m))
      ;; the primaries come most-specific-first, so the FIRST one is the method the
      ;; call actually enters; the rest are reachable only through call-next-method
      (loop for m in (getf o :primary) for first = t then nil do (add m first))
      (dolist (m (getf o :after))   (add m))
      ;; The RETURN leg. An :around method is the only layer that runs TWICE — once on
      ;; the way in and once on the way out, around its own call-next-method — and a
      ;; list that shows it once cannot say so. The design mock-up (§2) draws the same
      ;; closing row. Outermost-last on the way out, so the order really is a loop.
      (dolist (m (reverse (getf o :around)))
        (push (make-item :label (method-tuple-label m)
                         :detail "resumes here, after everything inside it returned"
                         :kind :method
                         :group "unwind"
                         :subject (subj-method name (getf m :qualifiers)
                                               (getf m :specializers)))
              items)))
    (make-view
     :title (format nil "effective ~(~a~) (~{~(~a~)~^ ~})~@[ ~c ~d combine~]"
                    name (subj-onion-tuple s) (code-char #xB7)
                    (let ((n (+ (length (getf o :around)) (length (getf o :before))
                                (length (getf o :primary)) (length (getf o :after)))))
                      (and (plusp n) n)))
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
             (append
              ;; a GF with a stage (§3b) offers it right under the matrix: the matrix
              ;; says which pairs HAVE a rule, the stage shows the rules running.
              (let ((stage (and *stage-subject-fn* (funcall *stage-subject-fn* name))))
                (when stage
                  (list (make-item :label "stage"
                                   :detail "the instances, moving — dispatch made visible"
                                   :disposition :new-trail
                                   :subject stage))))
              (list (make-item :label "definition source"
                               :detail "the defgeneric form"
                               :subject (subj-defn name :generic)))
              (mapcar (lambda (m)
                        (make-item
                         :label (method-tuple-label m)
                         :detail (if (getf m :source) "specialised method" "method (no source)")
                         :kind :method
                         :subject (subj-method name (getf m :qualifiers) (getf m :specializers))))
                      methods))))))

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
