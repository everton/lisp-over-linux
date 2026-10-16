;;;; sandbox.lisp — Accept tries a definition in a CHILD before PID 1 runs it.
;;;;
;;;; C-c C-c compiles a definition into the running image, and the running image is
;;;; PID 1. Most mistakes are harmless there: a compile error, an unbound variable,
;;;; a type error are CONDITIONS, accept-pane's HANDLER-CASE catches them and the
;;;; status line says "Accept failed". What a handler cannot catch is the definition
;;;; that never returns, or the one that takes the process down with it:
;;;;
;;;;     (defparameter *table* (loop))            ; PID 1 frozen: no key ever answered
;;;;     (defvar *x* (sb-ext:exit :abort t))      ; PID 1 exits: the kernel panics
;;;;
;;;; Both evaluate at LOAD time, which is exactly when Accept runs them. So before
;;;; PID 1 evaluates anything, it re-execs itself as a child (argv "try"), sends the
;;;; text down a pipe and waits. The child Accepts it in ITS heap and answers. Then:
;;;;
;;;;   the child answered :ACCEPTED  → PID 1 accepts it too.
;;;;   the child answered :ERROR     → PID 1 accepts it ANYWAY. An error is the case
;;;;                                   PID 1 already survives, and the child may have
;;;;                                   failed only for lack of something PID 1 has (a
;;;;                                   class you defined with Do it an hour ago).
;;;;                                   Refusing would be a false alarm; trying again
;;;;                                   here gives the real error, caught as before.
;;;;   the child DIED (pipe closed)  → REFUSED. PID 1 never evaluates it.
;;;;   the child did not answer in   → REFUSED, and the child is killed.
;;;;     *SANDBOX-SECONDS*
;;;;
;;;; So the sandbox refuses only what PID 1 could not have survived, and nothing
;;;; else. That rule is why the child does NOT replay this session's earlier Accepts
;;;; first, which would make it a closer copy of PID 1: a missing definition can only
;;;; turn into an error (allowed through), and replaying would run every earlier
;;;; Accept's side effects again in every trial.
;;;;
;;;; What it does not catch, said plainly: a definition that is fine to LOAD and
;;;; wrong to CALL. Redefine DRAW-PANE-EXPANDED with an infinite loop in its body and
;;;; the trial passes — nothing calls it in the child — and the next repair hangs PID 1.
;;;; The child has no screen, no input and no event loop, so it cannot exercise what
;;;; the definition will be used for. Catching that needs the child to run the CALLER
;;;; (a frame, headless, as render-frame.lisp does on the host) — not attempted.
;;;;
;;;; It costs a process start per Accept — the time is in *LAST-SANDBOX*, and the
;;;; status line shows the trial's pid. (setf *accept-sandbox* nil) turns it off, for
;;;; a definition the sandbox refuses that you mean anyway. On the host there is no
;;;; /sbin/lisp: Accept is unsandboxed there and *LAST-SANDBOX* says so.
;;;;
;;;; See doc/processes.org §7¾. All original code, MIT.

(in-package :cl-user)

(defparameter *sandbox-seconds* 10
  "How long a trial may take before it counts as a hang. Generous: the child has to
   start (a fresh image) as well as compile.")

(defvar *last-sandbox* nil
  "How the last trial went: (:outcome O :pid P :seconds S [:note N]), O one of
   :accepted :error :died :hung :unsandboxed.")

(define-condition accept-refused (error)
  ((reason :initarg :reason :reader accept-refused-reason))
  (:report (lambda (c s) (write-string (accept-refused-reason c) s)))
  (:documentation "The sandbox would not let a definition into PID 1."))

(defun note-sandbox (outcome pid t0 &optional note)
  (setf *last-sandbox*
        (list* :outcome outcome :pid pid
               :seconds (/ (round (* 100 (- (get-internal-real-time) t0))
                                  internal-time-units-per-second)
                           100.0)
               (and note (list :note note)))))

(defun sandbox-try-accept (text package-name)
  "Try TEXT in a child; return normally to let PID 1 accept it, or signal
   ACCEPT-REFUSED. This is the value of *ACCEPT-SANDBOX* (registry.lisp)."
  (let ((t0 (get-internal-real-time)))
    (unless (probe-file +worker-binary+)
      (note-sandbox :unsandboxed nil t0 "no /sbin/lisp: the host")
      (return-from sandbox-try-accept :unsandboxed))
    (let* ((p (sb-ext:run-program +worker-binary+ '("try")
                                  :wait nil :input :stream :output :stream :error nil))
           (pid (sb-ext:process-pid p))
           (w (make-wire (sb-sys:fd-stream-fd (sb-ext:process-output p))
                         (sb-sys:fd-stream-fd (sb-ext:process-input p))))
           (deadline (+ t0 (* *sandbox-seconds* internal-time-units-per-second)))
           (reply nil))
      (set-nonblocking (wire-in-fd w))
      (wire-send w (list :accept text package-name))
      (close (sb-ext:process-input p))        ; the child's stdin: EOF after the request
      (unwind-protect
           (loop until (or reply (wire-eof w)
                           (>= (get-internal-real-time) deadline))
                 do (wire-wait w (/ (- deadline (get-internal-real-time))
                                    internal-time-units-per-second))
                    (wire-pump w)
                    (setf reply (find-if (lambda (m) (member (car m) '(:accepted :error)))
                                         (wire-take w))))
        (unless (or reply (wire-eof w))
          (ignore-errors (sb-ext:process-kill p 9)))     ; hung: it goes, whatever it was
        (sb-ext:process-wait p)
        (ignore-errors (sb-ext:process-close p)))
      (cond
        (reply
         (note-sandbox (car reply) pid t0 (second reply))
         (car reply))
        ((wire-eof w)
         (note-sandbox :died pid t0)
         (error 'accept-refused
                :reason (format nil "refused: trying it KILLED a sandbox process (pid ~a, ~(~a~) ~a) — ~
                                     PID 1 untouched. (setf *accept-sandbox* nil) to accept anyway"
                                pid (sb-ext:process-status p) (sb-ext:process-exit-code p))))
        (t
         (note-sandbox :hung pid t0)
         (error 'accept-refused
                :reason (format nil "refused: it had not finished loading after ~d s in a sandbox ~
                                     process (pid ~a, killed) — PID 1 untouched. ~
                                     (setf *accept-sandbox* nil) to accept anyway"
                                *sandbox-seconds* pid)))))))

(defun run-accept-trial ()
  "CHILD ENTRY POINT (argv: try). Read one (:accept TEXT PACKAGE) from stdin, Accept
   it in this throwaway heap, answer (:accepted NAME) or (:error MESSAGE), exit. If
   the definition kills or hangs this process, that IS the answer — PID 1 reads it
   off the pipe closing, or off the clock."
  (let ((w (make-wire 0 1)) (request nil))
    (set-nonblocking 0)
    (loop until (or request (wire-eof w))
          do (wire-wait w nil)
             (wire-pump w)
             (setf request (find :accept (wire-take w) :key #'car)))
    (when request
      (destructuring-bind (text package-name) (rest request)
        (wire-send w
                   (handler-case
                       (let ((*accept-sandbox* nil)        ; the trial must not trial itself
                             ;; ...nor KEEP itself: the hooks are PID 1's (the journal,
                             ;; journal.lisp), and a trial that ran them appended every
                             ;; Accept to the journal twice — once here, once for real
                             (*accept-hooks* '())
                             (*package* (or (find-package package-name) (find-package :cl-user)))
                             (*standard-output* (make-broadcast-stream))
                             (*error-output* (make-broadcast-stream)))
                         (list :accepted (prin1-to-string (accept-source text))))
                     (serious-condition (c)
                       (list :error (handler-case (princ-to-string c)
                                      (error () "an unprintable condition"))))))))))

(setf *accept-sandbox* 'sandbox-try-accept)
