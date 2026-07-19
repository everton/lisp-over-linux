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
  subject                ; what drilling opens — (present subject) — or NIL
  (disposition :in-place); :in-place | :new-trail | :beside | :replace (a HINT)
  action)                ; a thunk to run instead of drilling, or NIL

(defstruct view
  "What (present subject) returns — a description, never pixels."
  title
  (kind :list)           ; :list :source :reference :class :generic :inspector
  (items '())            ; list of ITEM
  text                   ; body text for :source / :reference views
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

;;; ---- the two generic functions ------------------------------------------

(defgeneric present (subject)
  (:documentation "A VIEW of SUBJECT. Pure — no side effects. Drillable items
    carry the subject to (present ...) next. The (T) method is the inspector, so
    nothing is ever a dead end."))

(defgeneric commands-for (subject)
  (:documentation "The COMMANDs SUBJECT affords, by type. Pure."))

;;; ---- packages: their definitions ----------------------------------------

(defmethod present ((p package))
  (let ((items '()))
    (maphash (lambda (name defns)
               (when (and (symbolp name) (eq (symbol-package name) p))
                 (dolist (d defns)
                   (push (make-item :label  (string-downcase (princ-to-string name))
                                    :detail (string-downcase (princ-to-string (defn-kind d)))
                                    :subject (subj-defn name (defn-kind d)))
                         items))))
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
     :items (append
             (mapcar (lambda (s) (make-item :label (format nil "▲ ~(~a~)" s)
                                            :detail "superclass" :subject (find-class s)))
                     (getf i :supers))
             (mapcar (lambda (s) (make-item :label (format nil "▼ ~(~a~)" s)
                                            :detail "subclass" :subject (find-class s)))
                     (getf i :subs))
             (mapcar (lambda (sl) (make-item :label (format nil "slot ~(~a~)" (getf sl :name))
                                             :detail (and (getf sl :readers) "has readers")))
                     (getf i :direct-slots))))))

;;; ---- generic functions: methods -----------------------------------------

(defmethod present ((gf generic-function))
  (let ((name (sb-mop:generic-function-name gf)))
    (make-view :title (format nil "generic-function ~(~a~)" name)
               :kind :generic :subject gf
               :items (mapcar (lambda (m)
                                (make-item
                                 :label (format nil "~@[~{~(~a~) ~}~](~{~(~a~)~^ ~})"
                                                (getf m :qualifiers) (getf m :specializers))
                                 :detail (and (getf m :source) "source")))
                              (methods-of name)))))

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
