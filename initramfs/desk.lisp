;;;; desk.lisp — the desk: which tabs were open, and what the Workspaces said.
;;;;
;;;;   ~/.lol/desk/desk.lisp                   written by "Save and shut down"
;;;;   ~/.lol/desk/desk-autosave.lisp          written every few minutes, when it changed
;;;;
;;;; The journal (journal.lisp) keeps DEFINITIONS. Everything else a session builds up
;;;; lives in the heap — the tabs, the trail of panes in each, the Workspace you were
;;;; typing in — and is gone at power-off. Most of it is not code but it is small and
;;;; DESCRIBABLE: a tab is a list of subjects, and most subjects are little structs
;;;; naming something by name or path. So the desk is one printed plist, read back
;;;; with *READ-EVAL* off (doc/session-persistence.org §2c):
;;;;
;;;;   (:version 1 :build "…" :saved 3970000000
;;;;    :trails (((:ref (:workspace) :text ";; my scratch…" :point (12 4)))
;;;;             ((:ref (:modules) :sel 3) (:ref (:module "poc/textview")) …))
;;;;    :active 0 :windows ((1 200 120 768 500)) :recents (…)
;;;;    :settings (:file-sort :name …) :clipboard nil)
;;;;
;;;; A SUBJECT IS WRITTEN AS A REFERENCE, never as itself. DESK-REF turns a subject
;;;; into a small list of strings and keywords — (:dir "/home/…"), (:defn ("LOL.FB"
;;;; "FB-GEOMETRY") :function) — and DESK-SUBJECT, a WHITELIST, turns one back by
;;;; calling the constructor. A symbol is found with FIND-SYMBOL, never interned, and
;;;; a path must still exist. Anything with no reference — an inspected instance, a
;;;; closure, a terminal's process, a web page — belongs to the "opaque heap" no file
;;;; describes honestly; the trail is CUT at the first such pane, and what came before
;;;; it is kept.
;;;;
;;;; Two files, so "Shut down without saving" means what it says. An explicit save
;;;; writes desk.lisp and removes the autosave; the timer writes only the autosave;
;;;; shutting down without saving removes the autosave. So at boot an autosave exists
;;;; only when the last session ENDED WITHOUT A SHUTDOWN (a crash, a held power
;;;; button), and then it is the newer truth and wins.
;;;;
;;;; Not kept, on purpose: a file buffer you did not save (the leave menu asks about
;;;; those first — a secret second copy of an unsaved file is how editors earn
;;;; distrust), and the clipboard unless you ask for it (a pasted password lives
;;;; there): the setting :DESK-CLIPBOARD N keeps the newest N entries.
;;;;
;;;; Loaded after every module whose subjects it can name, before supervisor.lisp.

(in-package :cl-user)

(defparameter +desk-version+ 1)
(defparameter +desk-autosave-seconds+ 300 "How often the autosave looks for a change.")

(defun desk-path () (persist-path :desk "desk.lisp"))
(defun desk-autosave-path () (persist-path :desk "desk-autosave.lisp"))

;;; ---- subjects <-> references ---------------------------------------------------------

(defun symbol-ref (sym)
  "A symbol (or a (SETF sym) name) as (\"PACKAGE\" \"NAME\") / (:SETF \"PACKAGE\" \"NAME\"),
   or NIL for an uninterned one, which no later boot could find."
  (cond ((and (consp sym) (eq (first sym) 'setf) (symbolp (second sym)))
         (let ((r (symbol-ref (second sym)))) (and r (cons :setf r))))
        ((and (symbolp sym) (symbol-package sym))
         (list (package-name (symbol-package sym)) (symbol-name sym)))))

(defun ref-symbol (ref)
  "The symbol (or SETF name) REF names, by FIND-SYMBOL — NIL if it no longer exists.
   Never interns: a desk naming a symbol this build lacks must not create it."
  (cond ((and (consp ref) (eq (first ref) :setf))
         (let ((s (ref-symbol (rest ref)))) (and s (list 'setf s))))
        ((and (consp ref) (stringp (first ref)) (stringp (second ref)))
         (let ((p (find-package (first ref))))
           (and p (multiple-value-bind (s status) (find-symbol (second ref) p)
                    (and status s)))))))

(defgeneric desk-ref (subject)
  (:documentation "SUBJECT as a printable REFERENCE that DESK-SUBJECT can rebuild it
    from, or NIL when it has none (the opaque heap: a trail is cut there).")
  (:method (subject) (declare (ignore subject)) nil))

(defmacro define-singleton-refs (&rest pairs)
  "(define-singleton-refs subj-workspace :workspace …): subjects with no fields."
  `(progn ,@(loop for (type key) on pairs by #'cddr
                  collect `(defmethod desk-ref ((s ,type)) (declare (ignore s)) (list ,key)))))

(define-singleton-refs
  subj-workspace :workspace   subj-modules :modules   subj-change-set :change-set
  subj-tour :tour             subj-runs :runs         subj-containers :containers
  subj-network :network       subj-journal :journal)

(defmethod desk-ref ((s subj-module))
  (and (stringp (subj-module-category s)) (list :module (subj-module-category s))))
(defmethod desk-ref ((s subj-defn))
  (let ((r (symbol-ref (subj-defn-name s))))
    (and r (list :defn r (and (keywordp (subj-defn-kind s)) (subj-defn-kind s))))))
(defmethod desk-ref ((s subj-xref))
  (let ((r (symbol-ref (subj-xref-name s))))
    (and r (keywordp (subj-xref-relation s)) (list :xref (subj-xref-relation s) r))))
(defmethod desk-ref ((s subj-matrix))
  (let ((r (symbol-ref (subj-matrix-name s)))) (and r (list :matrix r))))
(defmethod desk-ref ((s subj-condition))
  (let ((r (symbol-ref (subj-condition-name s)))) (and r (list :condition r))))
(defmethod desk-ref ((s subj-dir)) (list :dir (subj-dir-path s)))
(defmethod desk-ref ((s subj-file)) (list :file (subj-file-path s)))
;; an Org outline, or one heading of it by its chain of titles (poc/org-mode.lisp):
;; the titles, not a line number, so the tab finds its heading after edits above it
(defmethod desk-ref ((s subj-org))
  (and (stringp (subj-org-path s)) (every #'stringp (subj-org-heading s))
       (list* :org (subj-org-path s) (and (subj-org-heading s) (list (subj-org-heading s))))))
(defmethod desk-ref ((s subj-file-info)) (list :file-info (subj-file-info-path s)))
(defmethod desk-ref ((s subj-container))
  (and (stringp (subj-container-name s)) (list :container (subj-container-name s))))
(defmethod desk-ref ((p package)) (list :package (package-name p)))
(defmethod desk-ref ((c class))
  (let ((r (symbol-ref (class-name c))))
    (and r (eq (find-class (class-name c) nil) c) (list :class r))))
(defmethod desk-ref ((g generic-function))
  (let* ((name (sb-mop:generic-function-name g)) (r (symbol-ref name)))
    (and r (ignore-errors (eq (fdefinition name) g)) (list :gf r))))

(defun desk-subject (ref)
  "The subject REF names, rebuilt through its constructor, or NIL — the WHITELIST:
   only these shapes are ever turned back into objects."
  (ignore-errors
    (flet ((path-ok (p) (and (stringp p) (path-exists-p p) p))
           (sym (r) (ref-symbol r)))
      (case (and (consp ref) (first ref))
        (:workspace (subj-workspace))  (:modules (subj-modules))  (:change-set (subj-change-set))
        (:tour (subj-tour))  (:runs (subj-runs))  (:containers (subj-containers))
        (:network (subj-network))  (:journal (subj-journal))
        (:module (and (stringp (second ref)) (subj-module (second ref))))
        (:defn (let ((s (sym (second ref))))
                 (and s (if (keywordp (third ref)) (subj-defn s (third ref)) (subj-defn s)))))
        (:xref (let ((s (sym (third ref))))
                 (and s (member (second ref) '(:senders :referrers)) (subj-xref (second ref) s))))
        (:matrix (let ((s (sym (second ref)))) (and s (subj-matrix s))))
        (:condition (let ((s (sym (second ref)))) (and s (subj-condition s))))
        (:dir (let ((p (path-ok (second ref)))) (and p (eq (followed-type p) :directory) (subj-dir p))))
        (:file (let ((p (path-ok (second ref)))) (and p (subj-file p))))
        (:org (let ((p (path-ok (second ref))) (titles (third ref)))
                (and p (listp titles) (every #'stringp titles) (subj-org p titles))))
        (:file-info (let ((p (path-ok (second ref)))) (and p (subj-file-info p))))
        (:container (and (stringp (second ref)) (subj-container (second ref))))
        (:package (and (stringp (second ref)) (find-package (second ref))))
        (:class (let ((s (sym (second ref)))) (and s (symbolp s) (find-class s nil))))
        (:gf (let ((s (sym (second ref))))
               (and s (fboundp s) (typep (fdefinition s) 'generic-function) (fdefinition s))))))))

;;; ---- the desk as data ------------------------------------------------------------------

(defun pane-record (pane)
  "PANE as a desk entry, or NIL when its subject has no reference."
  (let ((ref (desk-ref (pane-subject pane))))
    (when ref
      (append (list :ref ref :sel (pane-sel pane))
              (when (workspace-pane-p pane)
                (let ((tv (pane-tv pane)))
                  (list :text (lol.textview:tv-text tv)
                        :point (list (lol.textview:tv-point-line tv) (lol.textview:tv-point-col tv)))))))))

(defun trail-record (trail)
  "TRAIL's panes as desk entries, root first, CUT at the first that has no reference."
  (loop for p in trail for r = (ignore-errors (pane-record p)) while r collect r))

(defun clipboard-kept-count ()
  (let ((n (setting :desk-clipboard))) (if (and (integerp n) (plusp n)) (min n 50) 0)))

(defun desk-data ()
  "The whole desk, as the plist that is printed (without :SAVED, so two desks can
   be compared with EQUAL to see whether anything changed)."
  (let* ((records (mapcar #'trail-record *trails*))
         ;; a trail cut to nothing is dropped, and the active index follows it
         (active (count-if #'identity (subseq records 0 (min *active* (length records)))))
         (kept (remove nil records)))
    (list :version +desk-version+ :build *build-stamp*
          :trails kept
          :active (min active (max 0 (1- (length kept))))
          ;; the windows (doc/morphic.org): ((TRAIL-INDEX x y w h) …), back to front,
          ;; the index counting the KEPT trails; a window whose trail was cut away goes
          :windows (and (boundp '*world*) *world*
                        (loop for (root . bounds) in (desk-window-layout)
                              for i = (position root *trails* :key #'first)
                              when (and i (nth i records))
                                collect (cons (count-if #'identity (subseq records 0 i)) bounds)))
          :recents (loop for c in *spotter-recents*
                         for ref = (ignore-errors (desk-ref (getf c :subject)))
                         when ref collect (list :label (getf c :label) :detail (getf c :detail)
                                                :kind (getf c :kind) :ref ref))
          :settings (list :file-sort *file-sort* :file-sort-reverse (and *file-sort-reverse* t)
                          :show-hidden (and *show-hidden* t))
          :clipboard (let ((n (clipboard-kept-count)))
                       (and (plusp n)
                            (let ((h (lol.clipboard:clip-history)))
                              (subseq h 0 (min n (length h)))))))))

(defvar *desk-last-written* nil "The DESK-DATA last written (either file), to skip no-op saves.")

(defun write-desk-file (path data)
  (ensure-directories-under-home path)
  (unless (path-exists-p path) (append-synced path ""))      ; born 0600: it holds your text
  (write-file-atomically
   path (sb-ext:string-to-octets
         (with-standard-io-syntax
           (let ((*print-readably* nil) (*package* (find-package :keyword)))
             (format nil ";; LoL desk — written by the environment (desk.lisp); read with *read-eval* off.~%~s~%"
                     (append data (list :saved (get-universal-time))))))
         :external-format :utf-8)))

(defun save-desk ()
  "\"Save and shut down\": write desk.lisp, remove the autosave. Never signals —
   it runs on the way to power-off. Returns T when written."
  (let ((path (desk-path)))
    (when path
      (handler-case
          (let ((data (desk-data)))
            (write-desk-file path data)
            (setf *desk-last-written* data)
            (let ((a (desk-autosave-path))) (when (path-exists-p a) (delete-path a)))
            (session-log "desk saved: ~d tab~:p" (length (getf data :trails)))
            t)
        (error (c) (session-log "desk NOT saved: ~a" c) nil)))))

(defun leave-without-saving ()
  "\"Shut down without saving\": the last explicit save stands, the autosave goes."
  (ignore-errors
    (let ((a (desk-autosave-path))) (when (and a (path-exists-p a)) (delete-path a)))))

(defun autosave-desk ()
  "Write desk-autosave.lisp when the desk changed since the last write. Never signals."
  (let ((path (desk-autosave-path)))
    (when path
      (ignore-errors
        (let ((data (desk-data)))
          (unless (equal data *desk-last-written*)
            (write-desk-file path data)
            (setf *desk-last-written* data)
            t))))))

;;; ---- reading it back --------------------------------------------------------------------

(defun read-desk-file (path)
  "The desk plist in PATH, or NIL. Never signals; *READ-EVAL* off; bare words read as
   keywords, so nothing can be interned anywhere that matters."
  (when (and path (path-exists-p path))
    (ignore-errors
      (with-open-file (s (native-path path) :external-format :utf-8)
        (let* ((*read-eval* nil) (*package* (find-package :keyword))
               (form (read s nil nil)))
          (and (listp form) (eql (getf form :version) +desk-version+) form))))))

(defun restore-pane (record)
  "One desk entry back into a PANE, or NIL."
  (let ((subject (desk-subject (getf record :ref))))
    (when subject
      (let ((pane (ignore-errors (open-pane subject))))
        (when pane
          (let ((sel (getf record :sel)))
            (when (integerp sel) (set-pane-sel pane sel)))
          (let ((text (getf record :text)) (tv (pane-tv pane)))
            (when (and (stringp text) tv (workspace-pane-p pane))
              (lol.textview:tv-set-text tv text)
              (let ((pt (getf record :point)))
                (when (and (consp pt) (integerp (first pt)) (integerp (second pt)))
                  (lol.textview:tv-move-point tv (first pt) (second pt))))))
          pane)))))

(defun restore-settings (plist)
  (let ((sort (getf plist :file-sort)))
    (when (member sort +sort-keys+) (setf *file-sort* sort)))
  (setf *file-sort-reverse* (and (getf plist :file-sort-reverse) t)
        *show-hidden* (and (getf plist :show-hidden) t)))

(defvar *desk-restored-from* nil "Which desk file this boot restored, or NIL.")
(defvar *desk-tried* nil "True once this boot has tried: the desk is restored at most once.")

(defun restore-desk ()
  "*RESTORE-DESK-FN*: rebuild *TRAILS* from the autosave (the last session did not
   shut down) or else desk.lisp. True when at least one tab came back. Once per boot
   only — if the environment dies, the next 'b' starts fresh rather than restoring
   the desk that may have killed it. Opens the journal beside it when replay needs
   you (JOURNAL-NEEDS-ATTENTION-P)."
  (unless *desk-tried*
    (setf *desk-tried* t)
    (unless (or (not (persistence-available-p)) (replay-disabled-p))
      (let* ((auto (read-desk-file (desk-autosave-path)))
             (desk (or auto (read-desk-file (desk-path))))
             (trails (and desk (remove nil (mapcar (lambda (tr)
                                                     (loop for r in tr for p = (restore-pane r)
                                                           while p collect p))
                                                   (getf desk :trails))))))
        (when desk
          (ignore-errors (restore-settings (getf desk :settings)))
          (ignore-errors
            (dolist (c (reverse (getf desk :recents)))
              (let ((s (desk-subject (getf c :ref))))
                (when (and s (stringp (getf c :label)))
                  (note-recent s (getf c :label) (getf c :detail) (getf c :kind))))))
          (ignore-errors
            (dolist (s (reverse (getf desk :clipboard)))
              (when (stringp s) (lol.clipboard:clip-copy s)))))
        (when trails
          (setf *trails* trails
                *active* (let ((a (getf desk :active)))
                           (if (and (integerp a) (< -1 a (length trails))) a 0))
                *desk-restored-from* (if auto (desk-autosave-path) (desk-path)))
          ;; the windows: torn off again when the environment makes its world
          (setf *windows-to-restore*
                (loop for w in (getf desk :windows)
                      when (and (consp w) (integerp (first w)) (< -1 (first w) (length trails)))
                        collect (cons (first (nth (first w) trails)) (rest w))))
          (session-log "desk restored from ~a: ~d tab~:p" *desk-restored-from* (length trails))))))
  ;; replay left something for you (a failure, a pending entry, a torn tail): put the
  ;; journal in a tab beside whatever came back, focus unchanged
  (when (journal-needs-attention-p)
    (unless *trails* (trail-root (subj-workspace)))
    (unless (find-if (lambda (trail) (subj-journal-p (pane-subject (first trail)))) *trails*)
      (let ((keep *active*))
        (new-trail (subj-journal))
        (setf *active* keep))))
  (and *trails* t))

(setf *restore-desk-fn* 'restore-desk)

;;; ---- autosave, as a service of the environment's poll loop -----------------------------
;;; fb-browser.lisp's loop asks every registered service how long it may sleep
;;; (:TIMEOUT, ms, -1 = forever) and runs each one's :RUN every turn. The desk asks to
;;; be woken every +DESK-AUTOSAVE-SECONDS+, and writes only when something changed.

(defvar *desk-next-autosave* nil "Internal real time of the next autosave check.")

(defun desk-autosave-timeout ()
  (if (persistence-available-p)
      (let ((now (get-internal-real-time)))
        (unless *desk-next-autosave*
          (setf *desk-next-autosave* (+ now (* +desk-autosave-seconds+ internal-time-units-per-second))))
        (max 0 (round (* 1000 (- *desk-next-autosave* now)) internal-time-units-per-second)))
      -1))

(defun desk-autosave-run (canvas dirty)
  (declare (ignore canvas dirty))
  (when (and *desk-next-autosave* (>= (get-internal-real-time) *desk-next-autosave*))
    (setf *desk-next-autosave* nil)
    (autosave-desk))
  nil)

(fb-register-service :desk :timeout 'desk-autosave-timeout :run 'desk-autosave-run)

;;; ---- the supervisor's 's' -----------------------------------------------------------------

(defun console-shutdown (&key (verb "shut down") (then #'power-off))
  "The supervisor menu's 's': the leave menu's question, asked in text, before THEN
   (POWER-OFF; CONSOLE-RESTART passes a restart). Unsaved file buffers are named
   first, since nothing here can save them."
  (let ((dirty (loop for trail in *trails* append (remove-if-not #'file-pane-dirty-p trail))))
    (when dirty
      (format t "~&~d file~:p with unsaved changes (lost at power-off; 'b' to go back and save):~%"
              (length dirty))
      (dolist (p dirty) (format t "   ~a~%" (getf (view-data (pane-view p)) :path))))
    (if (persistence-available-p)
        (progn
          (format t "~&Save the desk first?  y) save and ~a  n) ~a without saving  c) cancel~%> "
                  verb verb)
          (finish-output)
          (let ((a (string-trim " " (or (read-line *standard-input* nil "c") "c"))))
            (cond ((string-equal a "y") (save-desk) (funcall then))
                  ((string-equal a "n") (leave-without-saving) (funcall then))
                  (t (format t "~&not going to ~a.~%" verb)))))
        (progn
          (format t "~&No disk - nothing is kept. ~@(~a~)?  y/n~%> " verb) (finish-output)
          (if (string-equal (string-trim " " (or (read-line *standard-input* nil "n") "n")) "y")
              (funcall then)
              (format t "~&not going to ~a.~%" verb))))))

(defun console-restart (&key install)
  "The supervisor menu's 'e' (restart) and, with INSTALL, 'u': install the checkout's
   new build into /boot/lisp first (update.lisp) — refused, nothing else happens."
  (if (not install)
      (console-shutdown :verb "restart" :then #'restart-machine)
      (handler-case
          (let ((info (progn (format t "~&Installing the new build into /boot/lisp...~%")
                             (finish-output)
                             (install-pending-build))))
            (format t "~&Installed ~a.~%" (build-label info))
            (console-shutdown :verb "restart" :then #'restart-machine))
        (error (c) (format t "~&Not installed: ~a~%" c)))))
