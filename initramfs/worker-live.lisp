;;;; worker-live.lisp — a second process whose view stays LIVE.
;;;;
;;;; worker.lisp proved the cheap half of doc/processes.org §2: a child computes a
;;;; view, prints it down a pipe, exits, and our toolkit draws it. That child is a
;;;; function call that happens to run elsewhere — synchronous, one answer, and the
;;;; answer can only name things (symbols), because the child is gone by the time
;;;; you drill.
;;;;
;;;; This file is the other half, the one §2 said would "actually need designing".
;;;; The child stays alive, keeps its own objects, and the screen follows it:
;;;;
;;;;   1. HANDLES, NOT SUBJECTS. Every subject the child shows us is filed in a
;;;;      table on ITS side, and we get an integer. Our side wraps (connection,
;;;;      handle) in a SUBJ-REMOTE, and presenting one asks the child to present
;;;;      the real thing. So a row can lead to an object that has no name at all —
;;;;      a struct, a closure, whatever the child's inspector shows — because it
;;;;      never has to cross: only its number does.
;;;;
;;;;   2. LIVENESS IS A PUSH. When the child's state changes it sends
;;;;      (:invalidate handle). It does NOT send the new view: the parent decides
;;;;      whether anything showing that handle is on screen, and only then asks
;;;;      (:present handle). A hidden pane costs one message and then silence —
;;;;      the child sends at most ONE invalidate per handle until the parent asks
;;;;      again, so it can never flood a parent that is not looking.
;;;;
;;;;   3. THE PIPE IS A POLL SLOT. The child's output joins fb-browser.lisp's
;;;;      poll(2) through the *FB-SERVICES* hook web.lisp already uses (a service is
;;;;      :fd + :run). The browser sleeps until a key, a click — or the child
;;;;      writing. Idle stays idle: no timer, no polling, zero CPU while nothing
;;;;      changes. That is the property §2 claimed this design keeps, and it is the
;;;;      reason this is not runs.lisp's shape (a reader thread + a 100 ms tick).
;;;;
;;;;   4. INTERNING, OR EQ BREAKS. The shell compares subjects with EQ — the
;;;;      nav-split preview cache, the beside-toggle. A freshly built SUBJ-REMOTE
;;;;      per refresh would never be EQ to the last one, so the preview would rebuild
;;;;      on every update and ⇧↵ would never toggle closed. REMOTE-SUBJECT therefore
;;;;      hands out ONE struct per (connection, handle), forever. The child does the
;;;;      mirror image: the same subject gets the same handle (§2, point 3).
;;;;
;;;; Why still a PIPE PAIR and not the AF_UNIX socket §2 sketched: the child is
;;;; ours. run-program gives us both directions as pipes, no listening socket, no
;;;; accept loop, no file in /tmp for someone else to connect to. A socket earns its
;;;; place when the peer is a process we did NOT start; that is still nobody.
;;;;
;;;; The wire is NUL-framed READable forms — the same framing web-cdp.lisp uses for
;;;; Chrome's DevTools pipe, with s-expressions where Chrome has JSON. Read with
;;;; *READ-EVAL* nil (READ-WORKER-FORM) and only keyword heads from a fixed list are
;;;; accepted: a presentation channel that cannot eval (§2, "Security").
;;;;
;;;; Not here: the closures. ITEM-EDIT (an inspector row's setter) and ITEM-ACTION
;;;; are functions in the child's heap and are dropped at the boundary, so a remote
;;;; inspector is read-only. Making them travel means naming them — a declarative
;;;; tag the child resolves — which is §2's point 2 and still owed.
;;;;
;;;; On the host there is no /sbin/lisp to exec, so the "child" is a THREAD of this
;;;; process speaking the same protocol over two real pipes — and its views say so.
;;;; poc/worker-live-test.lisp tests the protocol that way; check-worker.lisp boots
;;;; the image and proves the real thing.
;;;;
;;;; See doc/processes.org §7½. All original code, MIT.

(in-package :cl-user)

;;; ============================================================================
;;; 1. The wire: NUL-framed s-expressions over a pair of fds
;;; ============================================================================

(defparameter +wire-heads+
  '(:hello :view :invalidate :error          ; child -> parent
    :present :quit                           ; parent -> child
    :accept :accepted)                       ; the Accept sandbox (sandbox.lisp)
  "Every message head either side will act on. Anything else that arrives is
   dropped unread — the reader already refused #. ; this refuses the vocabulary.")

(defstruct (wire (:constructor make-wire (in-fd out-fd)))
  "One end of a conversation: the fd we read, the fd we write, and the bytes read
   so far that do not yet make a whole frame."
  in-fd out-fd
  (buf (make-array 4096 :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))
  (eof nil))                   ; the other end closed: a read returned 0

(defun wire-encode (form)
  "FORM as the octets of one frame: printed, then a NUL. WITH-STANDARD-IO-SYNTAX so
   the reader on the far side is not at the mercy of this side's *PRINT-LENGTH* or
   *PACKAGE* — but with *PRINT-READABLY* off (strings stay \"…\", not #A((6)…)) and
   *PRINT-PRETTY* off (no line breaks we did not put there). A NUL inside a string
   would end the frame early, so it is printed as U+2400, the picture of a NUL."
  (let* ((text (with-standard-io-syntax
                 (let ((*print-readably* nil) (*print-pretty* nil))
                   (prin1-to-string form))))
         ;; COERCE first: PRIN1-TO-STRING may hand back a BASE-STRING, which cannot
         ;; hold the U+2400 we substitute in (the host test caught that)
         (octets (sb-ext:string-to-octets (substitute (code-char #x2400) (code-char 0)
                                                      (coerce text '(simple-array character (*))))
                                          :external-format :utf-8))
         (frame (make-array (1+ (length octets)) :element-type '(unsigned-byte 8)
                                                 :initial-element 0)))
    (replace frame octets)                     ; ... the last byte stays 0: the frame end
    frame))

(defun wire-send (w form)
  "Write FORM as one frame. -> T, or NIL when the other end has gone. SBCL ignores
   SIGPIPE, so writing to a pipe nobody reads is an EPIPE return here, not the death
   of the writer — which matters when the writer is PID 1."
  (let ((octets (wire-encode form)))
    (sb-sys:with-pinned-objects (octets)
      (let ((sap (sb-sys:vector-sap octets)) (n (length octets)) (off 0))
        (loop while (< off n) do
          (multiple-value-bind (k err) (sb-unix:unix-write (wire-out-fd w) sap off (- n off))
            (cond (k (incf off k))
                  ((eql err sb-unix:eintr))
                  ((eql err sb-unix:eagain)
                   (sb-sys:wait-until-fd-usable (wire-out-fd w) :output 1 nil))
                  (t (return-from wire-send nil)))))
        t))))

(defun wire-pump (w)
  "Append whatever has arrived to W's buffer, WITHOUT BLOCKING — the read end is
   O_NONBLOCK (SET-NONBLOCKING, web-cdp.lisp), so an empty pipe is EAGAIN, not a
   sleep. -> T when any bytes came. A read of 0 means the writer closed: EOF."
  (let ((chunk (make-array 8192 :element-type '(unsigned-byte 8))) (any nil))
    (loop
      (multiple-value-bind (n err)
          (sb-sys:with-pinned-objects (chunk)
            (sb-unix:unix-read (wire-in-fd w) (sb-sys:vector-sap chunk) (length chunk)))
        (cond ((and n (plusp n))
               (setf any t)
               (let* ((buf (wire-buf w)) (fp (fill-pointer buf)))
                 (when (> (+ fp n) (array-dimension buf 0))
                   (setf buf (adjust-array buf (max (* 2 (array-dimension buf 0)) (+ fp n)))
                         (wire-buf w) buf))
                 (setf (fill-pointer buf) (+ fp n))
                 (replace buf chunk :start1 fp :end2 n)))
              ((and n (zerop n)) (setf (wire-eof w) t) (return any))
              ((eql err sb-unix:eintr))
              (t (return any)))))))                     ; EAGAIN: drained

(defun wire-take (w)
  "Cut every complete frame out of W's buffer. -> the messages, oldest first. A frame
   that does not read, or whose head is not in +WIRE-HEADS+, is dropped; an
   incomplete tail stays for the next pump."
  (let* ((buf (wire-buf w)) (n (fill-pointer buf)) (start 0) (out '()))
    (loop for nul = (position 0 buf :start start :end n)
          while nul
          do (let* ((text (sb-ext:octets-to-string buf :start start :end nul
                                                       :external-format '(:utf-8 :replacement #\?)))
                    (form (read-worker-form (make-string-input-stream text))))
               (when (and (consp form) (member (car form) +wire-heads+))
                 (push form out))
               (setf start (1+ nul))))
    (when (plusp start)
      (replace buf buf :start2 start :end2 n)
      (setf (fill-pointer buf) (- n start)))
    (nreverse out)))

(defun wire-wait (w seconds)
  "Sleep until W's read end has something (or has hung up), or SECONDS pass (NIL =
   forever). -> true when the fd woke us. SERVE-EVENTS nil: we want this one fd, not
   SBCL's other handlers run behind our back."
  (sb-sys:wait-until-fd-usable (wire-in-fd w) :input seconds nil))

;;; ============================================================================
;;; 2. The CHILD: one job, its objects, and a loop that answers and steps
;;; ============================================================================

(defgeneric live-step (subject)
  (:documentation "Advance a live job's ROOT subject by one step, IN THE CHILD.
    -> (values DELAY CHANGED): DELAY is seconds until the next step (0 = at once), or
    NIL when the job has nothing more to do — the child then only answers requests
    until the parent hangs up. CHANGED is :ALL when anything the parent was shown may
    be stale, else NIL for 'just the root'.")
  (:method (subject) (declare (ignore subject)) nil))

(defstruct (worker-server (:constructor make-worker-server (wire)))
  "The child's side: every subject it has handed out, and who was told what."
  wire
  (subjects (make-array 16 :adjustable t :fill-pointer 0)) ; handle -> subject
  (by-eq (make-hash-table :test 'eq))         ; live objects: identity IS the object
  (by-name (make-hash-table :test 'equal))    ; named wrappers: identity is the name
  (served (make-hash-table))                  ; handles whose view the parent has seen
  (notified (make-hash-table)))               ; ... and been told are stale since

(defun plain-datum-p (x)
  "Is X made only of names and numbers — the stuff a SUBJ-CONDITION or SUBJ-DEFN
   holds? Such a wrapper is rebuilt fresh by every PRESENT, so EQ would give it a
   new handle each time; its slots ARE its identity."
  (typecase x
    ((or symbol number character string) t)
    (cons (and (plain-datum-p (car x)) (plain-datum-p (cdr x))))
    (t nil)))

(defun subject-name-key (subject)
  "For a structure whose every slot is plain data: (TYPE . SLOT-VALUES), an EQUAL
   key. NIL for anything else, which is then identified by EQ."
  (when (typep subject 'structure-object)
    (let ((values (mapcar (lambda (s) (slot-value subject (sb-mop:slot-definition-name s)))
                          (sb-mop:class-slots (class-of subject)))))
      (when (every #'plain-datum-p values)
        (cons (type-of subject) values)))))

(defun server-handle (srv subject)
  "The integer the parent will use for SUBJECT — the same one every time. A named
   wrapper is found by its name, anything else by EQ: so a present method that wants
   a STABLE handle for a live object must hand back the same object each time
   (live-ticker's SELF slot does exactly that)."
  (let* ((key (subject-name-key subject))
         (table (if key (worker-server-by-name srv) (worker-server-by-eq srv)))
         (k (or key subject)))
    (or (gethash k table)
        (setf (gethash k table)
              (vector-push-extend subject (worker-server-subjects srv))))))

(defparameter +wire-list-kinds+ '(:list :inspector)
  "View kinds that are rows, and so cross as rows. Everything else is text, or is
   :DATA the renderer reads — a matrix grid, the stage's live structs — and that
   last kind does not cross at all (doc/processes.org §7, 'Why the stage was the
   wrong candidate').")

(defun wire-string (x)
  (cond ((null x) nil) ((stringp x) x) (t (princ-to-string x))))

(defun view->wire (srv view)
  "VIEW as plain data, every subject replaced by its handle."
  (let* ((kind (view-kind view))
         (rows-p (member kind +wire-list-kinds+)))
    (list :title (wire-string (view-title view))
          :kind (if rows-p kind :reference)
          :sel (view-sel view)
          :text (cond (rows-p nil)
                      ((view-text view) (wire-string (view-text view)))
                      (t (format nil ";; a ~(~a~) view: its data is live objects in the~%~
                                     ;; worker's heap, and it does not cross a process boundary."
                                 kind)))
          :items (when rows-p
                   (mapcar (lambda (it)
                             (list :label (wire-string (item-label it))
                                   :detail (wire-string (item-detail it))
                                   :kind (and (symbolp (item-kind it)) (item-kind it))
                                   :group (wire-string (item-group it))
                                   :disposition (item-disposition it)
                                   :handle (and (item-subject it)
                                                (server-handle srv (item-subject it)))))
                           (view-items view))))))

(defun serve-present (srv handle)
  "Answer (:present HANDLE): present the child's own subject, ship it as data."
  (let* ((subjects (worker-server-subjects srv))
         (subject (and (typep handle '(integer 0)) (< handle (length subjects))
                       (aref subjects handle)))
         (view (if subject
                   (handler-case (present subject)
                     (serious-condition (c)
                       (make-view :title "the worker could not present this"
                                  :items (list (make-item :label (princ-to-string c)
                                                          :kind :condition)))))
                   (make-view :title (format nil "no such handle: ~s" handle)))))
    (setf (gethash handle (worker-server-served srv)) t)
    (remhash handle (worker-server-notified srv))
    (wire-send (worker-server-wire srv) (list :view handle (view->wire srv view)))))

(defun notify-stale (srv changed)
  "Tell the parent what may now be out of date: the root, or (:ALL) everything it
   was shown — but at most ONCE per handle until it asks again."
  (let ((w (worker-server-wire srv)))
    (dolist (h (if (eq changed :all)
                   (loop for h being the hash-keys of (worker-server-served srv) collect h)
                   (list 0)))
      (when (and (gethash h (worker-server-served srv))
                 (not (gethash h (worker-server-notified srv))))
        (setf (gethash h (worker-server-notified srv)) t)
        (wire-send w (list :invalidate h))))))

(defun serve-job (srv root)
  "The child's loop: answer every request that is waiting, and when a step is due,
   take it and say what went stale. Between the two it SLEEPS in WIRE-WAIT — on the
   request pipe, with the step's deadline as the timeout — so a child with nothing
   to do costs nothing either. Ends when the parent sends :QUIT or hangs up."
  (let ((w (worker-server-wire srv))
        (due (get-internal-real-time)))           ; the first step: at once
    (server-handle srv root)                      ; => handle 0, always
    (wire-send w (list :hello :pid (sb-unix:unix-getpid)))
    (loop
      (wire-pump w)
      (dolist (msg (wire-take w))
        (case (first msg)
          (:present (serve-present srv (second msg)))
          (:quit (return-from serve-job :quit))))
      (when (wire-eof w) (return :eof))
      (let ((now (get-internal-real-time)))
        (if (and due (<= due now))
            (multiple-value-bind (delay changed)
                (handler-case (live-step root)
                  (serious-condition (c)
                    (wire-send w (list :error (format nil "the job's step failed: ~a" c)))
                    nil))
              (setf due (and delay (+ now (round (* delay internal-time-units-per-second)))))
              (notify-stale srv changed))
            (wire-wait w (and due (/ (- due now) internal-time-units-per-second))))))))

;;; ---- the live jobs (these run IN THE CHILD) --------------------------------

(defstruct (live-ticker (:constructor make-live-ticker ()))
  "The smallest live thing: a child that changes once a second on its own."
  (ticks 0)
  (started (get-internal-real-time))
  self)                              ; (subj-object THIS), made once: a stable handle

(defmethod present ((s live-ticker))
  (flet ((row (label detail) (make-item :label label :detail detail :kind :value
                                        :group "this process")))
    (make-view
     :title "a child that ticks"
     :kind :list :subject s
     :items (list (row "pid" (princ-to-string (sb-unix:unix-getpid)))
                  (row "ticks" (princ-to-string (live-ticker-ticks s)))
                  (row "uptime" (format nil "~,1f s" (/ (- (get-internal-real-time)
                                                           (live-ticker-started s))
                                                        internal-time-units-per-second)))
                  (row "heap" (format nil "~:d bytes live" (sb-kernel:dynamic-usage)))
                  (make-item :label "this job, as an object"
                             :detail "it has no name: only a handle reaches it"
                             :kind :variable :group "drill"
                             :subject (or (live-ticker-self s)
                                          (setf (live-ticker-self s) (subj-object s))))))))

(defmethod live-step ((s live-ticker))
  (incf (live-ticker-ticks s))
  (values 1 :all))                   ; the object view shows TICKS too: :ALL

(defparameter +sweep-batch+ 40
  "Definitions the live sweep reads per step.")
(defparameter +sweep-pace+ 0.05
  "Seconds between the live sweep's steps. A pace, not a necessity: at 0 it runs flat
   out and the parent still keeps up (it asks for one view at a time). It is here so
   a person can WATCH results arrive, which is the point of the demonstration.")

(defstruct (live-sweep (:constructor make-live-sweep
                           (&aux (todo (loop for defns being the hash-values of *registry*
                                             append defns))
                                 (total (length todo)))))
  "job-signallers (worker.lisp), done a batch at a time with a view after each."
  todo total (done 0) (tally (make-hash-table :test 'eq)))

(defmethod present ((s live-sweep))
  (let ((finished (null (live-sweep-todo s))))
    (make-view
     :title (format nil "conditions signalled, swept from source~:[ — sweeping~; — done~]"
                    finished)
     :kind :list :subject s
     :items (cons (make-item :label (format nil "read ~:d of ~:d definitions"
                                            (live-sweep-done s) (live-sweep-total s))
                             :detail (if finished "done" "sweeping…")
                             :kind (if finished :value :restart) :group "progress")
                  (mapcar (lambda (r)
                            (make-item :label (getf r :label) :detail (getf r :detail)
                                       :kind :condition :group "results"
                                       :subject (subj-condition (getf r :name))))
                          (signal-tally-rows (live-sweep-tally s)))))))

(defmethod live-step ((s live-sweep))
  (loop repeat +sweep-batch+
        while (live-sweep-todo s)
        do (let* ((d (pop (live-sweep-todo s)))
                  (form (defn-form d)))
             (incf (live-sweep-done s))
             (when form (tally-signals d form (live-sweep-tally s)))))
  (values (and (live-sweep-todo s) +sweep-pace+) nil))

(defparameter *live-jobs*
  '((:ticker . make-live-ticker)
    (:sweep  . make-live-sweep))
  "Live job name -> the function that builds its root subject, in the child.")

(defun run-worker-server (name &key (in-fd 0) (out-fd 1))
  "CHILD ENTRY POINT (argv: serve NAME). Speak the protocol on IN-FD/OUT-FD — stdin
   and stdout in a real child. *STANDARD-OUTPUT* goes nowhere while we serve: fd 1
   IS the wire, and one stray PRINT in a present method would corrupt it."
  (let* ((ctor (cdr (assoc (intern (string-upcase name) :keyword) *live-jobs*)))
         (w (make-wire in-fd out-fd))
         (*standard-output* (make-broadcast-stream)))
    (set-nonblocking in-fd)
    (if ctor
        (serve-job (make-worker-server w) (funcall ctor))
        (wire-send w (list :error (format nil "no such live job: ~a" name))))))

;;; ============================================================================
;;; 3. The PARENT: connections, remote subjects, and the poll-loop service
;;; ============================================================================

(defstruct (worker-conn (:constructor %make-worker-conn))
  "One live child, from PID 1's side."
  job wire
  process                       ; the SB-EXT:PROCESS, or NIL on the host fallback
  thread                        ; ... the thread playing the child there
  pid
  (state :starting)             ; :starting | :live | :exited | :failed
  note                          ; why it is not live, in words
  ;; Two threads touch a connection: the browser's loop (WORKER-LIVE-RUN) and
  ;; whoever PRESENTs a remote subject — usually the same thread, but the net REPL
  ;; is another. So the tables are synchronized and the read side is under LOCK:
  ;; two readers of one pipe would each get half the frames.
  (lock (sb-thread:make-mutex :name "worker conn"))
  (views (make-hash-table :synchronized t))     ; handle -> the last view's wire data
  (stale (make-hash-table :synchronized t))     ; handles the child said changed
  (pending (make-hash-table :synchronized t))   ; handles asked for, no answer yet
  (subjects (make-hash-table :synchronized t))  ; handle -> THE subj-remote (interned)
  (updates 0)                   ; views received: the proof that it is live
  idle-since)                   ; when no pane last showed it (see WORKER-SWEEP)

(defmethod print-object ((c worker-conn) stream)
  (print-unreadable-object (c stream)
    (format stream "worker ~(~a~) pid ~a ~(~a~)" (worker-conn-job c) (worker-conn-pid c)
            (worker-conn-state c))))

(defstruct (subj-remote (:constructor %subj-remote (conn handle)))
  "A subject that lives in another process: which one, and its number there."
  conn handle)

(defmethod print-object ((s subj-remote) stream)
  (print-unreadable-object (s stream)
    (format stream "remote ~a of ~a" (subj-remote-handle s) (subj-remote-conn s))))

(defvar *worker-conns* '() "Every live-worker connection still worth showing.")
(defvar *worker-reap* '() "Processes we let go of and have not yet waited for.")

(defparameter +remote-wait-seconds+ 10
  "How long PRESENT waits for a child's first answer before showing 'no answer'.")
(defparameter +worker-idle-seconds+ 5
  "A child no pane shows any more is told to quit after this long — long enough that
   drilling out of it and straight back in does not cost a new process.")

(defun conn-live-p (c) (member (worker-conn-state c) '(:starting :live)))

(defun remote-subject (conn handle)
  "THE subject for HANDLE on CONN — one struct per pair, forever, so the shell's EQ
   tests (preview cache, beside-toggle) keep working across live updates."
  (or (gethash handle (worker-conn-subjects conn))
      (setf (gethash handle (worker-conn-subjects conn)) (%subj-remote conn handle))))

(defun spawn-live-worker (job)
  "Start JOB in a child and return its connection. The child is this image again,
   argv (\"serve\" JOB), stdin and stdout both pipes; we read its stdout O_NONBLOCK
   so the poll loop can drain it without ever stalling on it. With no /sbin/lisp (the
   host), the child is a THREAD over two pipes — same bytes, same code, said so."
  (if (probe-file +worker-binary+)
      (let* ((p (sb-ext:run-program +worker-binary+
                                    (list "serve" (string-downcase (string job)))
                                    :wait nil :input :stream :output :stream :error nil))
             (w (make-wire (sb-sys:fd-stream-fd (sb-ext:process-output p))
                           (sb-sys:fd-stream-fd (sb-ext:process-input p)))))
        (set-nonblocking (wire-in-fd w))
        (%make-worker-conn :job job :wire w :process p))
      (multiple-value-bind (down-r down-w) (sb-unix:unix-pipe)      ; parent -> child
        (multiple-value-bind (up-r up-w) (sb-unix:unix-pipe)        ; child -> parent
          (set-nonblocking up-r)
          (%make-worker-conn
           :job job :wire (make-wire up-r down-w)
           :thread (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect (run-worker-server (string job) :in-fd down-r :out-fd up-w)
                        (sb-unix:unix-close down-r)
                        (sb-unix:unix-close up-w)))
                    :name (format nil "live worker ~(~a~)" job)))))))

(defun subj-live (job)
  "Start live JOB (:ticker, :sweep — see *LIVE-JOBS*) in a new child process and
   return the subject for its root view. Inspect it (C-c C-i) and the pane follows
   the child with no key pressed."
  (let ((conn (spawn-live-worker job)))
    ;; the first view BEFORE the loop can see the connection, so nobody else is
    ;; reading its pipe while we wait for the hello and the root
    (conn-request conn 0)
    (conn-await conn 0 +remote-wait-seconds+)
    (push conn *worker-conns*)
    (remote-subject conn 0)))

(defun conn-handle-message (conn msg)
  "Act on one message from CONN's child. -> the handle whose view arrived, or NIL."
  (handler-case
      (destructuring-bind (head &rest args) msg
        (ecase head
          (:hello (setf (worker-conn-pid conn) (getf args :pid)
                        (worker-conn-state conn) :live)
                  nil)
          (:view (destructuring-bind (h data) args
                   (when (and (typep h '(integer 0)) (listp data))
                     (setf (gethash h (worker-conn-views conn)) data)
                     (remhash h (worker-conn-pending conn))
                     (remhash h (worker-conn-stale conn))
                     (incf (worker-conn-updates conn))
                     h)))
          (:invalidate (let ((h (first args)))
                         (when (typep h '(integer 0))
                           (setf (gethash h (worker-conn-stale conn)) t))
                         nil))
          (:error (setf (worker-conn-note conn) (wire-string (first args)))
                  (when (eq (worker-conn-state conn) :starting)
                    (setf (worker-conn-state conn) :failed))
                  nil)))
    (error () nil)))                              ; a malformed message: ignore it

(defun conn-pump (conn)
  "Drain CONN without blocking and act on what came. -> (values ARRIVED CHANGED):
   the handles whose views arrived, and T when the connection's state changed (the
   child exited) — both are reasons to refresh panes."
  (sb-thread:with-recursive-lock ((worker-conn-lock conn))
   (if (not (conn-live-p conn))
      (values nil nil)
      (let ((w (worker-conn-wire conn)) (arrived '()))
        (wire-pump w)
        (dolist (m (wire-take w))
          (let ((h (conn-handle-message conn m))) (when h (pushnew h arrived))))
        (cond ((wire-eof w)
               (worker-close conn :exited "the worker exited — this is its last view")
               (values arrived t))
              (t (values arrived nil)))))))

(defun conn-request (conn h)
  "Ask CONN's child for H's view, unless we already have."
  (sb-thread:with-recursive-lock ((worker-conn-lock conn))
    (when (and (conn-live-p conn) (not (gethash h (worker-conn-pending conn))))
      (if (wire-send (worker-conn-wire conn) (list :present h))
          (setf (gethash h (worker-conn-pending conn)) t)
          (worker-close conn :exited "the worker's pipe is closed")))))

(defun conn-await (conn h seconds)
  "Block until H's view arrives, the child dies, or SECONDS pass. Only PRESENT does
   this, and only for a view it has never had: opening a pane needs SOMETHING to
   draw now. Every later update is asynchronous (WORKER-LIVE-RUN)."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* seconds internal-time-units-per-second)))))
    (loop
      (conn-pump conn)
      (when (or (not (conn-live-p conn))
                (not (gethash h (worker-conn-pending conn))))
        (return))
      (let ((left (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
        (when (<= left 0)
          (setf (worker-conn-note conn) (format nil "no answer in ~d s" seconds))
          (return))
        ;; in slices: another thread's pump may take the frame we wait for (it
        ;; files it in the same tables), and would leave our wait none the wiser
        (wire-wait (worker-conn-wire conn) (min left 0.1))))))

(defun worker-close (conn state note)
  "Let go of CONN: tell the child to quit, close our ends, and leave the process to
   be reaped (WORKER-REAP). Its views stay, so its panes still show the last one."
  (sb-thread:with-recursive-lock ((worker-conn-lock conn))
   (when (conn-live-p conn)
    (let ((w (worker-conn-wire conn)) (p (worker-conn-process conn)))
      (ignore-errors (wire-send w '(:quit)))
      (setf (worker-conn-state conn) state (worker-conn-note conn) note)
      (cond (p (ignore-errors (close (sb-ext:process-input p)))      ; the child sees EOF
               (push p *worker-reap*))
            (t (sb-unix:unix-close (wire-out-fd w))))
      (if p
          (ignore-errors (close (sb-ext:process-output p)))          ; owns the read fd
          (sb-unix:unix-close (wire-in-fd w)))))))

(defun worker-reap ()
  "Wait for the children that are done (non-blocking: PROCESS-ALIVE-P asks waitpid
   with WNOHANG), so none lingers as a zombie."
  (setf *worker-reap*
        (remove-if (lambda (p)
                     (unless (sb-ext:process-alive-p p)
                       (ignore-errors (sb-ext:process-close p))
                       t))
                   *worker-reap*)))

;;; ---- presenting a remote subject -------------------------------------------

(defun provenance-item (conn)
  "The row that says where a view came from and whether it is still alive."
  (let ((pid (worker-conn-pid conn)))
    (make-item :label (cond ((not (worker-conn-process conn))
                             "a THREAD of this process (host fallback: no /sbin/lisp)")
                            ((conn-live-p conn) (format nil "live from pid ~a" pid))
                            (t (format nil "pid ~a: ~a" pid (or (worker-conn-note conn) "gone"))))
               :detail (format nil "~d update~:p" (worker-conn-updates conn))
               :kind (if (conn-live-p conn) :package :condition)
               :group "provenance")))

(defun wire->item (conn row)
  "One row off the wire -> one ITEM, its handle turned back into THE subject."
  (let ((h (getf row :handle)))
    (make-item :label (or (getf row :label) "")
               :detail (getf row :detail)
               :kind (getf row :kind)
               :group (getf row :group)
               :disposition (if (member (getf row :disposition) '(:in-place :new-trail :beside :replace))
                                (getf row :disposition)
                                :in-place)
               :subject (and (typep h '(integer 0)) (remote-subject conn h)))))

(defun remote-view (s)
  "The VIEW for remote subject S, built from the last data its child sent."
  (let* ((conn (subj-remote-conn s))
         (data (gethash (subj-remote-handle s) (worker-conn-views conn)))
         (job (string-downcase (string (worker-conn-job conn)))))
    (cond
      ((null data)
       (make-view :title (format nil "worker ~a — ~a" job (or (worker-conn-note conn) "no answer"))
                  :kind :list :subject s :items (list (provenance-item conn))))
      ((eq (getf data :kind) :reference)
       (make-view :title (format nil "~a   [pid ~a]" (getf data :title) (worker-conn-pid conn))
                  :kind :reference :subject s :text (or (getf data :text) "")))
      (t
       (make-view :title (format nil "worker ~a · ~a" job (getf data :title))
                  :kind (if (eq (getf data :kind) :inspector) :inspector :list)
                  :subject s
                  :sel (1+ (or (getf data :sel) 0))       ; past the provenance row
                  :items (cons (provenance-item conn)
                               (mapcar (lambda (r) (wire->item conn r)) (getf data :items))))))))

(defmethod present ((s subj-remote))
  (let ((conn (subj-remote-conn s)) (h (subj-remote-handle s)))
    (when (and (conn-live-p conn)
               (or (null (gethash h (worker-conn-views conn)))
                   (gethash h (worker-conn-stale conn))))
      (conn-request conn h)
      (conn-await conn h +remote-wait-seconds+))
    (remote-view s)))

(defmethod commands-for ((s subj-remote))
  (let ((conn (subj-remote-conn s)))
    (list (make-command :label "Stop this worker" :surfaces '(:menu)
                        :action (lambda () (worker-close conn :exited "stopped by you"))))))

;;; ---- the poll-loop service ---------------------------------------------------

(defun browser-panes (&key visible)
  "Every pane the shell holds — each trail's panes, their side panes and their
   nav-split previews — or, with VISIBLE, only those on screen: the tiled trail's and
   every window's (poc/morphic.lisp, VISIBLE-TRAILS)."
  (let ((out '()))
    (dolist (trail (if visible
                       (if (fboundp 'visible-trails) (funcall 'visible-trails) (list (current-trail)))
                       *trails*))
      (dolist (p trail)
        (push p out)
        (when (pane-side p) (push (pane-side p) out))
        ;; A preview is a CACHE: it keeps the last selection's pane after the
        ;; selection moves on, so it is "on screen" only while its list is the
        ;; trail's expanded pane and it previews what that list has selected now.
        ;; (Counting it always made a hidden ticker redraw the screen every second —
        ;; the guest test caught that, the host test could not: nothing draws there,
        ;; so nothing fills the cache.)
        (let ((pv (pane-preview p)))
          (when (and pv
                     (or (not visible)
                         (and (eq p (car (last trail)))
                              (let ((it (and (pane-view p) (selected-item p))))
                                (and it (eq (item-subject it) (pane-subject pv)))))))
            (push pv out)))))
    out))

(defun pane-remote (p)
  (let ((s (and p (pane-subject p)))) (and (subj-remote-p s) s)))

(defun refresh-remote-panes (conn handles)
  "Give every pane showing one of HANDLES on CONN (all of them when HANDLES is :ALL)
   the view that just arrived. The pane keeps its place: same selection index
   (clamped), same scroll. -> the refreshed panes that are ON SCREEN."
  (let ((visible (browser-panes :visible t)) (seen '()))
    (dolist (p (browser-panes))
      (let ((s (pane-remote p)))
        (when (and s (eq (subj-remote-conn s) conn)
                   (or (eq handles :all) (member (subj-remote-handle s) handles)))
          (let ((v (remote-view s)))
            (setf (pane-view p) v
                  (pane-sel p) (max 0 (min (pane-sel p) (1- (length (view-items v))))))
            (when (and (pane-tv p) (view-text v))
              (lol.textview:tv-set-text (pane-tv p) (view-text v)))
            (when (member p visible) (push p seen))))))
    seen))

(defun conn-shown-p (conn &key visible)
  (some (lambda (p) (let ((s (pane-remote p))) (and s (eq (subj-remote-conn s) conn))))
        (browser-panes :visible visible)))

(defun worker-sweep (conn)
  "Quit CONN's child once no pane has shown it for +WORKER-IDLE-SECONDS+, and forget
   the connection. Closing a tab is how you let a worker go."
  (cond ((conn-shown-p conn) (setf (worker-conn-idle-since conn) nil))
        ((null (worker-conn-idle-since conn))
         (setf (worker-conn-idle-since conn) (get-internal-real-time)))
        ((> (- (get-internal-real-time) (worker-conn-idle-since conn))
            (* +worker-idle-seconds+ internal-time-units-per-second))
         (worker-close conn :exited "no pane showed it")
         (setf *worker-conns* (remove conn *worker-conns*)))))

(defun worker-live-fds ()
  "The service's :FD — every live child's output pipe, for the browser's poll."
  (loop for c in *worker-conns*
        when (conn-live-p c) collect (wire-in-fd (worker-conn-wire c))))

(defun worker-live-timeout ()
  "The service's :TIMEOUT. Never (-1) while every child is on screen or gone: the
   pipe wakes us. Only a child waiting out its idle grace needs a second look."
  (if (some (lambda (c) (and (conn-live-p c) (worker-conn-idle-since c))) *worker-conns*)
      1000 -1))

(defun worker-live-run (canvas dirty)
  "The service's :RUN, every turn of the loop. Drain each child; refresh the panes
   whose views arrived; ask again for whatever went stale AND is on screen (a stale
   handle nobody is looking at is just remembered — it is asked for when its tab
   comes back); let go of children nobody shows. Each refreshed pane on screen
   damages just the morph that shows it (DAMAGE-PANE, poc/morphic.lisp — the shell, or
   one window); with no world, -> :REDRAW when the screen changed."
  (declare (ignore canvas dirty))
  (let ((redraw nil))
    (dolist (c (copy-list *worker-conns*))
      (multiple-value-bind (arrived changed) (conn-pump c)
        (when (or arrived changed)
          (let ((shown (refresh-remote-panes c (if changed :all arrived))))
            (cond ((null shown))
                  ((and (fboundp 'damage-pane) (boundp '*world*) (symbol-value '*world*))
                   (mapc (symbol-function 'damage-pane) shown))
                  (t (setf redraw t))))))
      (when (conn-live-p c)
        (let ((on-screen (loop for p in (browser-panes :visible t)
                               for s = (pane-remote p)
                               when (and s (eq (subj-remote-conn s) c))
                                 collect (subj-remote-handle s))))
          (maphash (lambda (h stale)
                     (declare (ignore stale))
                     (when (member h on-screen) (conn-request c h)))
                   (worker-conn-stale c))))
      (worker-sweep c))
    (worker-reap)
    (and redraw :redraw)))

(when (fboundp 'fb-register-service)
  (funcall 'fb-register-service :workers
           :fd 'worker-live-fds :timeout 'worker-live-timeout :run 'worker-live-run))
