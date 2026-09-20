;;;; check-worker.lisp — assert that a SECOND PROCESS really contributes the view.
;;;;
;;;;     sbcl --script check-worker.lisp        ; exits 0 if all assertions pass
;;;;
;;;; initramfs/worker.lisp claims something that is easy to fake and easy to break
;;;; silently: that the view on screen was computed somewhere else. On the host
;;;; there is no /sbin/lisp to exec, so CALL-WORKER falls back to running the job
;;;; in-process — which is the right behaviour, and which means *every host test of
;;;; this feature passes without ever forking*. Only a booted guest can tell the
;;;; two apart, so this is a guest assertion or it is nothing.
;;;;
;;;; What it pins down, in order of what would hurt most if it rotted:
;;;;
;;;;   THE FORK IS REAL. The environment is PID 1 and the view comes back naming a
;;;;   different pid. If worker.lisp ever silently fell back in the guest — a
;;;;   renamed binary, a broken argv role, a run-program that failed — the feature
;;;;   would still "work" and mean nothing. That is assertion 1, and it is the point.
;;;;
;;;;   THE WIRE ROUND-TRIPS. The child prin1s a plist and the parent reads it back
;;;;   under WITH-STANDARD-IO-SYNTAX with *READ-EVAL* nil. Printer settings drift
;;;;   (this already bit once: *print-readably* turned every string into
;;;;   #A((6) BASE-CHAR . "...")), so the count and shape of the rows are checked.
;;;;
;;;;   NAMES TRAVEL, SUBJECTS ARE REBUILT. A row computed in another process must
;;;;   still drill into THIS image's condition view. That is the whole handle idea
;;;;   of doc/processes.org §2 in its simplest form, and the assertion that a
;;;;   SUBJ-CONDITION comes back — and presents — is what keeps it honest.
;;;;
;;;;   THE READER STAYS GUARDED. A pipe is untrusted input even when we wrote the
;;;;   other end. Feeding READ-WORKER-FORM a form containing #. must yield NIL, not
;;;;   an evaluation. The net REPL is already remote eval; one of those is enough.
;;;;
;;;; No synthetic input is needed — every question is asked over the net REPL — so
;;;; this runs with the harness's QMP channel switched off and boots faster for it.
;;;; See host-client/guest-harness.lisp, and check-scroll.lisp for the input half.
;;;;
;;;; All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(use-package :lol.guest)

;;; The sweep reads every recorded definition's source back and walks it, which is
;;; the point of the job and is not instant on one emulated CPU. Give it room; a
;;; timeout here would look exactly like a failure.
(defparameter +sweep-seconds+ 25.0)

(defun report-row (label value)
  (format t "~&  ~34a ~a~%" label value)
  (finish-output)
  value)

(defun checks ()
  (format t "~&~%== A. is the work really happening elsewhere? ==~%")
  (let* ((me    (report-row "the environment's own pid"
                            (probe-int "(sb-unix:unix-getpid)")))
         ;; the pid row of (subj-worker :self) — the child's own view of itself
         (child (report-row "pid the child reported"
                            (probe-int
                             "(parse-integer (item-detail (second (view-items (present (subj-worker :self))))))"
                             15.0)))
         ;; 1 when the provenance row says a second process answered, 0 on fallback
         (forked (report-row "provenance says forked (1=yes)"
                             (probe-int
                              "(if (search \"THIS process\" (item-label (first (view-items (present (subj-worker :self)))))) 0 1)"
                              15.0)))
         (defs  (report-row "definitions the child could see"
                            (probe-int
                             "(parse-integer (item-detail (fourth (view-items (present (subj-worker :self))))) :junk-allowed t)"
                             15.0))))
    (check "the environment is PID 1" (eql me 1))
    (check "a DIFFERENT process answered" (and child (integerp child) (/= child 1)))
    (check "the provenance row says forked, not fallback" (eql forked 1))
    (check "the child had the whole registry loaded" (and defs (> defs 100))))

  (format t "~&~%== B. does the wire round-trip? ==~%")
  (let ((self-rows (report-row "rows in the :self view"
                               (probe-int "(length (view-items (present (subj-worker :self))))" 15.0)))
        (sweep-rows (report-row "rows in the :signallers view"
                                (probe-int "(length (view-items (present (subj-worker :signallers))))"
                                           +sweep-seconds+)))
        (unknown (report-row "an unknown job still yields a view"
                             (probe-int "(if (view-p (present (subj-worker :no-such-job))) 1 0)" 15.0))))
    ;; one provenance row + pid/heap/definitions
    (check "the :self view has provenance + 3 result rows" (eql self-rows 4))
    (check "the sweep found at least one signalled type"
           (and sweep-rows (> sweep-rows 1)))
    (check "an unknown job degrades to a view, not an error" (eql unknown 1)))

  (format t "~&~%== C. names travel, subjects are rebuilt here ==~%")
  (let ((is-cond (report-row "a swept row's subject is a condition"
                             (probe-int
                              "(let ((r (find :condition (rest (view-items (present (subj-worker :signallers)))) :key (function item-kind)))) (if (and r (subj-condition-p (item-subject r))) 1 0))"
                              +sweep-seconds+)))
        (drills (report-row "and presenting that subject works"
                            (probe-int
                             "(let ((r (find :condition (rest (view-items (present (subj-worker :signallers)))) :key (function item-kind)))) (if (and r (view-p (present (item-subject r)))) 1 0))"
                             +sweep-seconds+))))
    (check "a remotely-computed row carries a local SUBJ-CONDITION" (eql is-cond 1))
    (check "that subject drills into a real view" (eql drills 1)))

  (format t "~&~%== D. the reader stays guarded ==~%")
  ;; #. must NOT evaluate. READ-WORKER-FORM binds *read-eval* nil and catches the
  ;; reader error, so a guarded read returns NIL and an unguarded one would not.
  (let ((guarded (report-row "#. in a worker form is refused"
                             (probe-int
                              "(if (read-worker-form (make-string-input-stream \"(:title #.(quote pwned))\")) 0 1)"
                              10.0)))
        (plain (report-row "an ordinary form still reads"
                           (probe-int
                            "(if (read-worker-form (make-string-input-stream \"(:title 7)\")) 1 0)"
                            10.0))))
    (check "a form containing #. is refused, not evaluated" (eql guarded 1))
    (check "an ordinary form still reads back" (eql plain 1)))
  t)

(run-guest-checks "check-worker" #'checks :qmp nil)
