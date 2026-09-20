;;;; worker.lisp — a second process that contributes a VIEW, not pixels.
;;;;
;;;; The smallest honest demonstration of doc/processes.org §2: a peer process
;;;; puts something in this environment's UI without ever touching /dev/fb0 and
;;;; without anything resembling X.
;;;;
;;;; The whole trick is that `present` was never a pixel protocol. Its own view
;;;; struct says so — "a description, never pixels" — so what crosses the process
;;;; boundary is a list of labels, kinds and subject names, and OUR toolkit draws
;;;; it. A worker's rows are tinted by the same concept palette, scroll with the
;;;; same widget and drill with the same key as anything native, because by the
;;;; time the shell sees them they ARE native.
;;;;
;;;; Three decisions worth stating, because each one is a choice:
;;;;
;;;;   1. THE CHILD IS THE SAME IMAGE. `run-program "/sbin/lisp" ("job" ...)` is
;;;;      the mechanism spawn-worker already used; init-toplevel dispatches on
;;;;      argv. So the child arrives owning present.lisp, model.lisp and the whole
;;;;      registry. We are not defining a foreign protocol — we are deciding how a
;;;;      view crosses a boundary. That is why this file is ~150 lines and not
;;;;      ~350: there is no client library to write, because the client is us.
;;;;
;;;;   2. A PIPE, NOT A SOCKET. run-program gives us :output :stream for free,
;;;;      which is the cheapest transport that exists here and needs no AF_UNIX,
;;;;      no fourth pollfd, no accept loop. The cost is that this is SYNCHRONOUS:
;;;;      the parent waits. Liveness — a child pushing (:invalidate) as results
;;;;      arrive — is the part §2 still owes, and it is what wants the socket.
;;;;      Doing the easy half first keeps the claim honest.
;;;;
;;;;   3. SUBJECTS DO NOT TRAVEL; NAMES DO. The child sends symbols, and the
;;;;      parent rebuilds real subjects around them. So a row computed in another
;;;;      process still drills into this image's own condition view. That is the
;;;;      handle idea of §2 in its simplest possible form — good enough while the
;;;;      only subjects are named ones, and not good enough the moment a worker
;;;;      wants to hand back an arbitrary object.
;;;;
;;;; Runs on the host too: with no /sbin/lisp to exec, CALL-WORKER does the job
;;;; in-process and says so, so poc/run-browser.lisp and render-frame.lisp keep
;;;; working. The fallback is visible in the view, never silent.
;;;;
;;;; See doc/processes.org §2 and §7. All original code, MIT.

(in-package :cl-user)

;;; ---- the jobs themselves (these run IN THE CHILD) -------------------------
;;; A job returns plain data: a plist of (:title :rows), where each row is itself
;;; a plist of (:label :detail :kind :name). Nothing here may be a live object —
;;; the whole point is that it survives PRIN1 and READ.

(defun job-signallers ()
  "Every condition type signalled anywhere in the image, and by how many sites.

   The whole-image sweep doc/processes.org §7 argues for: it reads EVERY recorded
   definition's captured source back out of the registry and walks every subform
   looking for a signalling call. CLOS cannot answer this — there is no generic
   function to introspect — so it is derived from source, which is why it is both
   genuinely useful and genuinely expensive. Exactly the shape of work you want
   off the UI's thread of control."
  (let ((tally (make-hash-table :test 'eq)))
    (map-recorded-forms
     (lambda (d form)
       (walk-form form
                  (lambda (sub)
                    (let ((type (form-signalled-type sub)))
                      (when (and type (symbolp type))
                        (pushnew (defn-name d) (gethash type tally))))))))
    (let ((rows '()))
      (maphash (lambda (type sites)
                 (push (list :label (string-downcase (symbol-name type))
                             :detail (format nil "~d site~:p" (length sites))
                             :kind :condition
                             :name type)
                       rows))
               tally)
      (list :title "conditions signalled, swept from source"
            :rows (sort rows #'string< :key (lambda (r) (getf r :label)))))))

(defun job-self ()
  "What this process IS: the smoke test that the work really happened elsewhere.

   The PID is the load-bearing row. If it differs from the supervisor's, a second
   process ran; if it does not, you are looking at the in-process fallback."
  (list :title "the process that answered"
        :rows (list (list :label "pid" :detail (princ-to-string (sb-unix:unix-getpid))
                          :kind :value)
                    (list :label "heap" :detail (format nil "~:d bytes live"
                                                        (sb-kernel:dynamic-usage))
                          :kind :value)
                    (list :label "definitions" :detail (format nil "~:d recorded"
                                                               (hash-table-count *registry*))
                          :kind :value))))

(defparameter *worker-jobs*
  '((:signallers . job-signallers)
    (:self       . job-self))
  "Job name -> the function the child calls. Add a pair and it is runnable from
   both sides; there is deliberately no registration protocol.")

(defun run-worker-job (name)
  "CHILD ENTRY POINT. Run job NAME and print its result as a readable form.

   WITH-STANDARD-IO-SYNTAX because this is a wire format now: the parent's reader
   must not be at the mercy of whatever *print-length* or *package* the image
   happened to be in. Anything that goes wrong is reported as data too, so the
   parent always has something to present instead of an empty pane."
  (let* ((key (intern (string-upcase name) :keyword))
         (fn  (cdr (assoc key *worker-jobs*)))
         (result (handler-case
                     (if fn
                         (funcall fn)
                         (list :title (format nil "no such job: ~a" name) :rows '()))
                   (serious-condition (c)
                     (list :title "the job failed"
                           :rows (list (list :label (princ-to-string c) :kind :condition)))))))
    (with-standard-io-syntax
      ;; ...but *print-readably* off. WITH-STANDARD-IO-SYNTAX turns it ON, and a
      ;; readably-printed string is #A((6) BASE-CHAR . "182887") — which the reader
      ;; does accept, and which is pure noise on a wire carrying mostly strings.
      ;; *PRINT-ESCAPE* is still T, so "foo" round-trips as "foo".
      (let ((*print-readably* nil))
        (prin1 (append result (list :pid (sb-unix:unix-getpid))) *standard-output*)))
    (finish-output)))

;;; ---- the parent side -----------------------------------------------------

(defparameter +worker-binary+ "/sbin/lisp"
  "The image re-execs itself. There is no other executable to run (the initramfs
   ships /init and this, and nothing else), which is exactly why the child comes
   with the registry already loaded.")

(defun read-worker-form (stream)
  "Read one form from a child. *READ-EVAL* nil because a pipe is untrusted input
   even when we wrote the other end — the net REPL is already remote eval and one
   of those is enough (doc/processes.org §2)."
  (with-standard-io-syntax
    (let ((*read-eval* nil)
          (*package* (find-package :cl-user)))
      (handler-case (read stream nil nil)
        (serious-condition () nil)))))

(defun call-worker (job)
  "Run JOB in a SEPARATE PROCESS and return its plist, or run it here if we cannot.

   Returns a second value: T when a real child did the work, NIL when this was the
   in-process fallback. The caller shows that difference rather than hiding it."
  (if (probe-file +worker-binary+)
      (let* ((p (sb-ext:run-program +worker-binary+
                                    (list "job" (string-downcase (symbol-name job)))
                                    :wait nil :output :stream :error nil))
             (form (read-worker-form (sb-ext:process-output p))))
        (sb-ext:process-wait p)
        (if form
            (values form t)
            (values (list :title "the worker said nothing"
                          :rows (list (list :label "no readable form on the pipe"
                                            :kind :condition)))
                    t)))
      (let ((fn (cdr (assoc job *worker-jobs*))))
        (values (if fn (funcall fn) (list :title "no such job" :rows '())) nil))))

;;; ---- the subject ---------------------------------------------------------
;;; One struct and one PRESENT method, which is the whole extension surface the
;;; drill-down protocol promised (code-browser.org §4½).

(defstruct (subj-worker (:constructor subj-worker (job))) job)

(defun worker-row->item (row)
  "One row off the wire -> one ITEM. The subject is rebuilt HERE, around the name
   the child sent, so a remotely-computed row still drills into a local view."
  (let ((name (getf row :name))
        (kind (getf row :kind)))
    (make-item :label (getf row :label)
               :detail (getf row :detail)
               :kind kind
               :subject (cond ((null name) nil)
                              ((eq kind :condition) (subj-condition name))
                              (t (subj-defn name))))))

(defmethod present ((s subj-worker))
  (multiple-value-bind (result forked) (call-worker (subj-worker-job s))
    (make-view
     :title (format nil "worker ~(~a~) — ~a"
                    (subj-worker-job s) (getf result :title "?"))
     :kind :list :subject s
     :items (cons (make-item :label (if forked
                                        (format nil "computed in pid ~a"
                                                (getf result :pid "?"))
                                        "computed in THIS process (no /sbin/lisp — host fallback)")
                             :detail (if forked "a second process sent this view" "fallback")
                             :kind (if forked :package :condition)
                             :group "provenance")
                  (mapcar (lambda (r)
                            (let ((it (worker-row->item r)))
                              (setf (item-group it) "results")
                              it))
                          (getf result :rows))))))

(defmethod commands-for ((s subj-worker))
  (list (make-command :label "Run again" :target s :disposition :replace)))

(defun show-worker (&optional (job :signallers))
  "REPL entry point: run JOB in a child and print the view it sent back.
   (show-worker :self) is the one that proves a different process answered."
  (show-view (subj-worker job)))
