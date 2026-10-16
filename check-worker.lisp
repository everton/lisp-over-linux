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
;;;;   IT STAYS LIVE (worker-live.lisp). A child that keeps running sends views by
;;;;   HANDLE and pushes (:invalidate h); the pane follows it with NO KEY PRESSED —
;;;;   ticks advance and DRAW-BROWSER runs — and costs nothing while its tab is
;;;;   hidden; drilling reaches an object of the child's that has no name; a killed
;;;;   child leaves the poll; a child nobody shows is told to quit and reaped.
;;;;
;;;;   ACCEPT IS TRIED FIRST (sandbox.lisp). A definition that hangs, or exits its
;;;;   process, is refused and PID 1 never runs it; an ordinary error is let through.
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
                             ;; REMOVE the commas: job-self prints ~:d, and once the
                             ;; registry passed 999 this read "2,447" as 2
                             "(parse-integer (remove #\\, (item-detail (fourth (view-items (present (subj-worker :self)))))) :junk-allowed t)"
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
  (live-checks)
  (sandbox-checks)
  t)

;;; ---- the LIVE worker (initramfs/worker-live.lisp) ----------------------------
;;; Everything below is asked over the net REPL and NO KEY IS EVER SENT — this
;;; script does not even open the QMP channel. A pane opened from the REPL thread
;;; joins the loop's poll at its next wake (the systray's clock wakes it once a
;;; second); after that, the only thing that can make the world repair anything is
;;; the child writing down its pipe. Counting the repairs that drew (wrapping
;;; LOL.MORPH:WORLD-REPAIR with sb-int:encapsulate, as check-pointer.lisp does — the
;;; clock's tick paints without one) is how "the screen changed" is measured.

(defun guest-helpers ()
  (probe "(progn (defvar *zz-frames* 0) (setf *zz-frames* 0) (sb-int:encapsulate 'lol.morph:world-repair 'zz-count (lambda (f &rest a) (let ((n (lol.morph:world-repairs *world*))) (multiple-value-prog1 (apply f a) (unless (= n (lol.morph:world-repairs *world*)) (incf *zz-frames*)))))) t)")
  (probe "(progn (defun zz-row (label) (let ((it (find label (view-items (pane-view (current-pane))) :key (function item-label) :test (function equal)))) (and it (item-detail it)))) t)")
  (probe "(progn (defun zz-conn () (let ((s (pane-subject (current-pane)))) (and (subj-remote-p s) (subj-remote-conn s)))) t)")
  (probe "(progn (defun zz-group (g) (find g (view-items (pane-view (current-pane))) :key (function item-group) :test (function equal))) t)")
  (probe "(progn (defun zz-ticks-field () (let ((it (find-if (lambda (i) (search \"TICKS\" (string-upcase (item-label i)))) (view-items (pane-view (current-pane)))))) (and it (format nil \"~a ~a\" (item-label it) (item-detail it))))) t)"))

(defun frames () (probe-int "*zz-frames*"))

(defun live-checks ()
  (format t "~&~%== E. a live view: the child changes, the screen follows, no key ==~%")
  (report-row "environment up" (wait-for-browser))
  (guest-helpers)
  (report-row "open (subj-live :ticker) in a new tab"
              (probe "(progn (new-trail (subj-live :ticker)) (defvar *zz-pane* nil) (setf *zz-pane* (current-pane)) (defvar *zz-tab* 0) (setf *zz-tab* *active*) t)" 20.0))
  (let ((pid (report-row "the ticker's pid" (probe-int "(worker-conn-pid (zz-conn))")))
        (real (report-row "a real process (1=yes)" (probe-int "(if (worker-conn-process (zz-conn)) 1 0)")))
        (polled (report-row "its pipe is in the poll (1=yes)"
                            (probe-int "(if (member (wire-in-fd (worker-conn-wire (zz-conn))) (fb-service-fds)) 1 0)"))))
    (check "the ticker runs in a different process" (and pid (/= pid 1)))
    (check "... a real child, not the host's thread fallback" (eql real 1))
    (check "its pipe is one of the poll's fds (the :workers service)" (eql polled 1)))
  (sleep 1.5)                                        ; let the loop pick the fd up
  (let ((t0 (probe-int "(parse-integer (zz-row \"ticks\"))"))
        (f0 (frames)))
    (sleep 4.0)                                      ; NO input of any kind
    (let ((t1 (report-row "ticks after 4 s, no key" (probe-int "(parse-integer (zz-row \"ticks\"))")))
          (f1 (frames)))
      (report-row "ticks before" t0)
      (report-row "repairs in those 4 s" (and f0 f1 (- f1 f0)))
      (check "the view changed with no key pressed (ticks advanced)" (and t0 t1 (>= (- t1 t0) 2)))
      (check "... and the browser redrew for it (>= 2 frames)" (and f0 f1 (>= (- f1 f0) 2)))))
  (check "the pane is the same pane, its subject the interned one"
         (string= "T" (probe "(and (eq *zz-pane* (current-pane)) (eq (pane-subject (current-pane)) (remote-subject (zz-conn) 0)))")))

  (format t "~&~%== F. handles: a row that leads to an object with no name ==~%")
  (report-row "drill into 'this job, as an object'"
              (probe "(progn (trail-push (item-subject (zz-group \"drill\"))) t)" 15.0))
  (let ((remote (report-row "the drilled pane is remote (1=yes)"
                            (probe-int "(if (subj-remote-p (pane-subject (current-pane))) 1 0)")))
        (handle (report-row "its handle" (probe-int "(subj-remote-handle (pane-subject (current-pane)))")))
        (a (report-row "its TICKS field" (probe "(zz-ticks-field)"))))
    (sleep 3.0)
    (let ((b (report-row "... 3 s later" (probe "(zz-ticks-field)"))))
      (check "drilling crossed by HANDLE into the child's own struct"
             (and (eql remote 1) handle (plusp handle) (search "TICKS" (string-upcase a))))
      (check "the child's inspector of that object is live too" (string/= a b))))

  (format t "~&~%== G. hidden, it costs nothing; shown again, it catches up ==~%")
  (probe "(progn (new-trail (subj-defn 'subj-live)) t)" 10.0)
  (sleep 2.0)
  (let ((f0 (frames)) (u0 (probe-int "(worker-conn-updates (subj-remote-conn (pane-subject *zz-pane*)))")))
    (sleep 4.0)
    (let ((f1 (frames)) (u1 (probe-int "(worker-conn-updates (subj-remote-conn (pane-subject *zz-pane*)))")))
      (report-row "frames while its tab is hidden" (and f0 f1 (- f1 f0)))
      (report-row "views fetched meanwhile" (and u0 u1 (- u1 u0)))
      (check "a hidden live tab redraws nothing" (and f0 f1 (= f0 f1)))
      (check "... and asks its child for nothing" (and u0 u1 (= u0 u1)))))
  (probe "(progn (activate-trail *zz-tab*) t)")
  (let ((f0 (frames)))
    (sleep 3.0)
    (check "shown again, it catches up and keeps going"
           (let ((f1 (frames))) (and f0 f1 (>= (- f1 f0) 2)))))

  (format t "~&~%== H. the child dies ==~%")
  (probe "(progn (setf *trails* (list (list *zz-pane*)) *active* 0) t)")
  (probe "(progn (sb-ext:process-kill (worker-conn-process (zz-conn)) 9) t)")
  (sleep 2.0)
  (let ((state (report-row "connection state" (probe "(worker-conn-state (zz-conn))")))
        (row (report-row "provenance row" (probe "(item-label (first (view-items (pane-view (current-pane)))))")))
        (polled (report-row "its fd still polled (0=no)"
                            (probe-int "(if (member (wire-in-fd (worker-conn-wire (zz-conn))) (fb-service-fds)) 1 0)")))
        (f0 (frames)))
    (sleep 3.0)
    (let ((f1 (frames)))
      (check "a killed child reads as :EXITED" (search "EXITED" state))
      (check "... the pane says so and keeps the last view" (search "exited" row))
      (check "... its fd left the poll (no busy loop on a hung-up pipe)" (eql polled 0))
      (check "... and nothing redraws after that" (and f0 f1 (= f0 f1)))))

  (format t "~&~%== I. the live sweep: results as they arrive ==~%")
  (probe "(progn (new-trail (subj-live :sweep)) t)" 20.0)
  (let ((p0 (report-row "progress at once" (probe "(item-label (zz-group \"progress\"))"))))
    (sleep 1.5)
    (let ((p1 (report-row "progress 1.5 s later" (probe "(item-label (zz-group \"progress\"))"))))
      (check "the sweep's progress row moved on its own" (string/= p0 p1))))
  (loop repeat 60
        until (string= "T" (probe "(and (search \"done\" (view-title (pane-view (current-pane)))) t)"))
        do (sleep 1.0))
  (let ((live (report-row "rows the live sweep found"
                          (probe-int "(count \"results\" (view-items (pane-view (current-pane))) :key (function item-group) :test (function equal))")))
        (sync (report-row "rows the synchronous sweep finds"
                          (probe-int "(length (getf (job-signallers) :rows))" +sweep-seconds+))))
    (check "the live sweep finished with what the one-shot sweep finds"
           (and live sync (plusp live) (= live sync))))

  (format t "~&~%== J. letting go ==~%")
  (probe "(progn (setf *trails* (list (list (open-pane (subj-defn 'subj-live)))) *active* 0) t)")
  (sleep 9.0)                                        ; +worker-idle-seconds+ and a margin
  (let ((live (report-row "live connections left" (probe-int "(count-if (function conn-live-p) *worker-conns*)")))
        (zombies (report-row "children not yet reaped" (probe-int "(length *worker-reap*)"))))
    (check "a worker no pane shows is told to quit" (eql live 0))
    (check "... and reaped (no zombie)" (eql zombies 0))))

;;; ---- the Accept sandbox (initramfs/sandbox.lisp) ------------------------------
;;; The dangerous probes here are dangerous for real: if the sandbox failed, the
;;; (sb-ext:exit) one would end PID 1 and panic the kernel, and every probe after it
;;; would come back "?". That is the point — the assertion is that it does not.

(defun sandbox-checks ()
  (format t "~&~%== K. Accept tries a definition in a child first ==~%")
  (probe "(setf *sandbox-seconds* 4)")
  (let ((ok (report-row "accept (defun zz-sand ...) then call it"
                        (probe-int "(progn (accept-source \"(defun zz-sand (x) (* x 3))\") (zz-sand 2))" 15.0)))
        (pid (report-row "the trial ran in pid" (probe-int "(getf *last-sandbox* :pid)"))))
    (report-row "a trial costs (s)" (probe "(getf *last-sandbox* :seconds)"))
    (check "a good definition is accepted, after a trial" (eql ok 6))
    (check "... and the trial was another process" (and pid (/= pid 1))))
  (let ((hang (report-row "accept (defparameter *zz-hang* (loop))"
                          (probe-int "(handler-case (progn (accept-source \"(defparameter *zz-hang* (loop))\") 0) (accept-refused () 1))" 20.0)))
        (bound (report-row "*zz-hang* bound in PID 1 (0=no)" (probe-int "(if (boundp '*zz-hang*) 1 0)"))))
    (check "a definition that never finishes loading is refused" (eql hang 1))
    (check "... and PID 1 never ran it" (eql bound 0)))
  (let ((die (report-row "accept (defvar *zz-die* (sb-ext:exit ...))"
                         (probe-int "(handler-case (progn (accept-source \"(defvar *zz-die* (sb-ext:exit :abort t))\") 0) (accept-refused () 1))" 20.0)))
        (alive (report-row "PID 1 still answers, as pid" (probe-int "(sb-unix:unix-getpid)")))
        (outcome (report-row "the trial's outcome" (probe "(getf *last-sandbox* :outcome)"))))
    (check "a definition that kills its process is refused" (eql die 1))
    (check "... PID 1 is alive" (eql alive 1))
    (check "... and the trial records why" (search "DIED" outcome)))
  (let ((err (report-row "accept a read error"
                         (probe-int "(handler-case (progn (accept-source \"(defun zz-err (\") 0) (accept-refused () 2) (error () 1))" 15.0)))
        (outcome (report-row "the trial's outcome" (probe "(getf *last-sandbox* :outcome)"))))
    (check "an ordinary error is NOT refused: PID 1 tries, and catches it" (eql err 1))
    (check "... after the child reported it as an :ERROR" (search "ERROR" outcome))))

(run-guest-checks "check-worker" #'checks :qmp nil)
