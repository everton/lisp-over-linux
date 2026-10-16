;;;; session-log.lisp — what the machine was doing when it stopped.
;;;;
;;;; A session froze on the laptop (2026-10-15) and left nothing behind but
;;;; network.log, which stopped three minutes after boot. The screen keeps the last
;;;; frame whatever happened underneath, so "frozen" could have been any of:
;;;;
;;;;   - the environment's thread STUCK in a loop or a wait (an agent's lisp-eval runs
;;;;     there, by design — agent.lisp, POST-JOB-AND-WAIT);
;;;;   - two threads in a DEADLOCK;
;;;;   - PID 1 DEAD. Two ways were found by reading, not guessed:
;;;;       * an unhandled error on ANY background thread quits the whole process. The
;;;;         image is saved from a --non-interactive build, so its debugger hook is
;;;;         SB-DEBUG::DEBUGGER-DISABLED-HOOK, which prints "unhandled condition in
;;;;         --disable-debugger mode, quitting" and exits. PID 1 exiting is a kernel
;;;;         panic, drawn on a console that KD_GRAPHICS has switched off;
;;;;       * a fatal runtime error — "Heap exhausted, game over", a GC invariant —
;;;;         calls the C runtime's lose(), which unless --disable-ldb was given enters
;;;;         LDB, SBCL's low-level debugger, waiting for a command on the console.
;;;;         Same picture: nothing moves. (sbcl/src/runtime/interr.c, lose and
;;;;         enable_lossage_handler; runtime.c turns LDB on by default.)
;;;;   - the KERNEL stuck (no Lisp code can see that; doc/diagnostics.org §6).
;;;;
;;;; So this file makes the next freeze tell us which. Everything it writes goes to
;;;; ~/.lol/log/, on the laptop's /home, so Debian can read it after a hard reset:
;;;;
;;;;   session.log  one line per event, each fsync'd: the boot, every agent turn and
;;;;                tool call, errors, a PULSE every few seconds (is the environment's
;;;;                thread idle or busy, heap, GCs, threads, free memory), and STALL
;;;;                reports with every thread's backtrace and who holds which lock
;;;;   stderr.log   the process's file descriptor 2, where the C runtime writes its
;;;;                last words (lose(), heap exhaustion) and where child processes
;;;;                (Chrome, wpa_supplicant) complain
;;;;   network.log  dhcp.lisp's, unchanged in content
;;;;
;;;; On every boot the previous session's logs are renamed with the time they were
;;;; last written, gzipped into ~/.lol/log/archive/ by a background thread (pure
;;;; Lisp, poc/deflate.lisp) and pruned to the newest +ARCHIVE-KEEP+ of each kind, so
;;;; the directory never grows unattended. A diskless boot (HOME is /) logs to
;;;; /tmp/lol-log/ instead: lost at power-off, but still readable over the net REPL.
;;;;
;;;; Everything here NEVER signals: a logger that can take the machine down is worse
;;;; than none. Doc: doc/diagnostics.org. Tests: poc/session-log-test.lisp (host),
;;;; check-diagnostics.lisp (guest).
;;;;
;;;; All original code, under this project's MIT licence.

;;; ===========================================================================
;;; Where, and how a line is written
;;; ===========================================================================

(defparameter +log-cap+ (* 16 1024 1024)
  "A log file larger than this is cut mid-session: renamed aside like a finished
   session's (it is archived at the next boot) and started afresh.")
(defparameter +archive-keep+ 30 "How many archived sessions of each log to keep.")

(defvar *log-dir* nil "When set, the log directory, whatever HOME says (the host test).")

(defun log-dir ()
  "$HOME/.lol/log/ on a boot with a real home (the laptop's /home survives a reset);
   /tmp/lol-log/ on a diskless boot, where HOME is /."
  (or *log-dir*
      (let ((home (sb-ext:posix-getenv "HOME")))
        (if (and home (string/= home "") (string/= home "/"))
            (format nil "~a/.lol/log/" (string-right-trim "/" home))
            "/tmp/lol-log/"))))

(defun log-path (base) (format nil "~a~a.log" (log-dir) base))

(defun utc-stamp (&optional (ut (get-universal-time)))
  "2026-10-16 02:14:50Z — the format network.log always had."
  (multiple-value-bind (s m h d mo y) (decode-universal-time ut 0)
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0dZ" y mo d h m s)))

(defun file-stamp (ut)
  "20261016T021450Z — a UTC time that sorts as text and is safe in a file name."
  (multiple-value-bind (s m h d mo y) (decode-universal-time ut 0)
    (format nil "~d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ" y mo d h m s)))

(defvar *log-lock* (sb-thread:make-mutex :name "session log")
  "Several threads log; one line must never interleave with another.")

(defun fsync-fd (fd)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "fsync" (function sb-alien:int sb-alien:int)) fd))

(defun chmod-path (path mode)
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "chmod" (function sb-alien:int sb-alien:c-string sb-alien:unsigned-int))
   (namestring path) mode))

(defun ensure-private-file (path)
  "Create PATH 0600 if it does not exist yet: the logs quote what the agent ran and
   what the network did — readable by you, not by every account on the host."
  (unless (probe-file path)
    (let ((fd (sb-unix:unix-open (namestring path)
                                 (logior sb-unix:o_wronly sb-unix:o_creat sb-unix:o_append) #o600)))
      (when fd (sb-unix:unix-close fd)))))

(defun set-aside-name (path ut)
  "session.log -> session.20261016T021450Z.log: a finished (or cut) log waiting to
   be archived. UT is when it was last written."
  (let ((dot (position #\. path :from-end t)))
    (format nil "~a.~a~a" (subseq path 0 dot) (file-stamp ut) (subseq path dot))))

(defvar *logs-closed* nil
  "True after SESSION-LOG-CLOSE: the shutdown line must stay the LAST line, or the
   next boot would read a pulse written after it as a session that never ended.")

(defun append-log-line (path text)
  "Append TEXT to PATH as one time-stamped line, and push it to the disk (fsync) —
   a line still in the page cache is lost to a power-button reset, and a reset is
   exactly when it is wanted. Over +LOG-CAP+ the file is set aside first. Never
   signals; returns T when the line was written."
  (when *logs-closed* (return-from append-log-line nil))
  (ignore-errors
    (sb-thread:with-recursive-lock (*log-lock*)
      (ensure-directories-exist path)
      (ensure-private-file path)
      (with-open-file (o path :direction :output :if-exists :append
                              :if-does-not-exist :create :external-format :utf-8)
        (when (> (file-length o) +log-cap+)
          (close o)
          (rename-file path (set-aside-name path (get-universal-time)))
          (return-from append-log-line (append-log-line path text)))
        (format o "~a ~a~%" (utc-stamp) text)
        (finish-output o)
        (fsync-fd (sb-sys:fd-stream-fd o))
        t))))

(defun thread-tag ()
  "The current thread, short: [main], [agent turn 3], [netd] …"
  (let ((th sb-thread:*current-thread*))
    (if (eq th (sb-thread:main-thread))
        "main"
        (or (sb-thread:thread-name th) "?"))))

(defun session-log (fmt &rest args)
  "Log one event to session.log, tagged with the calling thread. FORMAT-style."
  (append-log-line (log-path "session")
                   (format nil "[~a] ~a" (thread-tag)
                           (or (ignore-errors (apply #'format nil fmt args)) fmt))))

(defun log-lines (text)
  (loop with start = 0 for nl = (position #\Newline text :start start)
        collect (subseq text start (or nl (length text)))
        while nl do (setf start (1+ nl))))

(defun clip-line (s n)
  (if (> (length s) n) (concatenate 'string (subseq s 0 (- n 1)) "…") s))

(defun clip-one-line (s &optional (n 160))
  "S on one line (newlines shown as ⏎), clipped to N characters — for quoting an
   agent's question or a tool's input in a log line."
  (clip-line (substitute #\⏎ #\Newline (princ-to-string s)) n))

(defun session-log-close (fmt &rest args)
  "Write the session's LAST line — \"shutdown: …\", which tells the next boot this one
   ended on purpose — then refuse every later line. SESSION-LOG-REOPEN undoes it,
   for a power-off the kernel refused."
  (prog1 (session-log "shutdown: ~?" fmt args)
    (setf *logs-closed* t)))

(defun session-log-reopen (why)
  (setf *logs-closed* nil)
  (session-log "the shutdown did not happen: ~a" why))

(defun session-log-block (title text &key (max-lines 80))
  "Log TITLE and then TEXT (a backtrace, a report) indented under it, as ONE write
   so another thread's line cannot land in the middle."
  (let* ((lines (log-lines (string-right-trim '(#\Newline #\Space) text)))
         (shown (subseq lines 0 (min max-lines (length lines)))))
    (append-log-line
     (log-path "session")
     (with-output-to-string (s)
       (format s "[~a] ~a" (thread-tag) title)
       (dolist (l shown) (format s "~%    ~a" (clip-line l 300)))
       (when (> (length lines) max-lines)
         (format s "~%    … ~d more lines" (- (length lines) max-lines)))))))

;;; ===========================================================================
;;; The environment's thread: idle, busy, or away
;;; ===========================================================================
;;;
;;; The environment (fb-browser.lisp) is a loop that sleeps in poll(2) until a key, a
;;; click or a deadline, does the work, and sleeps again. Every one of its modal loops
;;; (the Spotter, the debugger, menus, prompts) sleeps in the SAME function,
;;; POLL-INPUT-ONCE, which marks the thread :IDLE going in and :BUSY coming out. So
;;; "busy for ten seconds" means one piece of work has run ten seconds without coming
;;; back to wait for input — the definition of a frozen screen. :AWAY is the text
;;; console (the supervisor's menu, the REPL), which this does not watch.
;;;
;;; Plain global SETFs, not bindings: a LET would be private to the thread that made
;;; it, and the whole point is that ANOTHER thread reads these.

(defvar *main-state* :away "The environment's thread: :IDLE (in poll), :BUSY or :AWAY.")
(defvar *main-since* 0 "GET-INTERNAL-REAL-TIME when *MAIN-STATE* last changed.")
(defvar *main-activity* nil
  "What the environment's thread is doing, when something says: \"agent tool
   lisp-eval\". Shown in a STALL report.")

(declaim (inline note-main-idle note-main-busy))
(defun note-main-idle () (setf *main-state* :idle *main-since* (get-internal-real-time)))
(defun note-main-busy () (setf *main-state* :busy *main-since* (get-internal-real-time)))
(defun note-main-away () (setf *main-state* :away *main-since* (get-internal-real-time)))

(defmacro with-main-activity ((fmt &rest args) &body body)
  "Run BODY with *MAIN-ACTIVITY* saying what it is, for a STALL report to quote."
  (let ((old (gensym)))
    `(let ((,old *main-activity*))
       (setf *main-activity* (ignore-errors (format nil ,fmt ,@args)))
       (unwind-protect (progn ,@body)
         (setf *main-activity* ,old)))))

(defun seconds-since (itu)
  (/ (- (get-internal-real-time) itu) internal-time-units-per-second))

;;; ===========================================================================
;;; Looking inside another thread
;;; ===========================================================================

(defvar *lock-sources* '()
  "Functions of no arguments, each returning a list of mutexes worth naming in a
   STALL report (who holds them). agent.lisp registers its conversations' locks,
   tls.lisp its OpenSSL setup lock.")

(defun watch-locks (fn) (pushnew fn *lock-sources*))

(defun backtrace-string (&key (count 40) (from :debugger-frame))
  "The calling thread's backtrace as a string, printed SHORT: a frame's arguments
   may be a whole conversation, and a megabyte per frame is not a log line."
  (let ((sb-debug:*debug-print-variable-alist*
          (list* '(*print-length* . 6) '(*print-level* . 3) '(*print-lines* . 2)
                 sb-debug:*debug-print-variable-alist*)))
    (with-output-to-string (s)
      (sb-debug:print-backtrace :stream s :count count :from from :print-thread nil))))

(defun thread-backtrace (thread &key (timeout 2.0))
  "THREAD's backtrace as a string. Another thread's stack can only be walked from
   INSIDE it, so we INTERRUPT-THREAD it with a function that prints the backtrace
   into a box and returns, and wait up to TIMEOUT seconds for the box to fill.

   A thread may not answer: SBCL delays an interruption while the thread has
   interrupts disabled (inside WITHOUT-INTERRUPTS, which includes parts of the GC
   and of the thread machinery). The answer then says so — that is a finding too.
   The interruption is the same signal SBCL sends every thread for each GC, so a
   blocking system call it breaks is one that every GC breaks already."
  (cond
    ((eq thread sb-thread:*current-thread*) (backtrace-string))
    ((not (sb-thread:thread-alive-p thread)) "(thread already finished)")
    (t
     (let ((box (list nil)))
       (handler-case
           (sb-thread:interrupt-thread
            thread (lambda ()
                     (setf (car box)
                           (or (ignore-errors (backtrace-string :from :interrupted-frame))
                               "(the backtrace itself failed)"))))
         (error (c) (return-from thread-backtrace (format nil "(could not interrupt it: ~a)" c))))
       (loop with end = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
             until (or (car box) (> (get-internal-real-time) end))
             do (sleep 0.05))
       (or (car box)
           (format nil "(no answer in ~,1fs: interrupts disabled — in the GC, or WITHOUT-INTERRUPTS)"
                   timeout))))))

(defun lock-report ()
  "One line per watched mutex that someone holds: \"conversation\" held by [agent turn 3]."
  (let ((lines '()))
    (dolist (src *lock-sources*)
      (dolist (m (ignore-errors (funcall src)))
        (let ((owner (ignore-errors (sb-thread:mutex-owner m))))
          (when owner
            (push (format nil "lock ~s held by [~a]" (sb-thread:mutex-name m)
                          (if (eq owner (sb-thread:main-thread)) "main"
                              (sb-thread:thread-name owner)))
                  lines)))))
    (nreverse lines)))

(defun report-threads (title)
  "Log TITLE, then every thread's backtrace (but the watcher's own) and the lock
   report — the whole picture of a stall, in one block per thread."
  (session-log "~a" title)
  (dolist (l (lock-report)) (session-log "  ~a" l))
  (dolist (th (sb-thread:list-all-threads))
    (unless (or (eq th sb-thread:*current-thread*)
                (equal (sb-thread:thread-name th) "finalizer"))
      (session-log-block (format nil "  thread [~a] backtrace:"
                                 (if (eq th (sb-thread:main-thread)) "main" (sb-thread:thread-name th)))
                         (thread-backtrace th)))))

;;; ===========================================================================
;;; The watcher: pulse + stall detection, one thread
;;; ===========================================================================

(defparameter +pulse-seconds+ 5
  "Seconds between PULSE lines. The gap after the last one says when the machine
   stopped; what the last few say (heap, threads) says what it looked like.")
(defparameter +stall-thresholds+ '(10 60 300)
  "Seconds of :BUSY after which a STALL is reported — once at each, per stall.")
(defparameter +heap-warn+ 0.80 "Post-GC heap fraction that is logged as HEAP HIGH.")

(defvar *watch-thread* nil)
(defvar *gc-count* 0 "GCs since boot, counted by an *AFTER-GC-HOOKS* entry.")
(defvar *post-gc-usage* 0 "Dynamic-space bytes in use right after the last GC.")
(defvar *heap-high* nil "True while the post-GC heap is over +HEAP-WARN+ (logged once).")

(defun note-gc ()
  "On *AFTER-GC-HOOKS*: count, and remember what the GC left. Tiny on purpose — it
   runs right after every collection."
  (incf *gc-count*)
  (setf *post-gc-usage* (sb-kernel:dynamic-usage)))

(defun mem-available-kb ()
  (ignore-errors
    (with-open-file (s "/proc/meminfo")
      (loop for line = (read-line s nil) while line
            when (and (> (length line) 13) (string= "MemAvailable:" line :end2 13))
              return (parse-integer line :start 13 :junk-allowed t)))))

(defun mb (bytes) (round bytes (* 1024 1024)))

(defun pulse-line ()
  (format nil "pulse  main ~(~a~)~@[ ~ds~]~@[ (~a)~] | heap ~d/~d MB, after GC ~d | ~d GCs | ~d threads | ~@[~,1f GB free~]"
          *main-state*
          (and (eq *main-state* :busy) (floor (seconds-since *main-since*)))
          (and (eq *main-state* :busy) *main-activity*)
          (mb (sb-kernel:dynamic-usage)) (mb (sb-ext:dynamic-space-size)) (mb *post-gc-usage*)
          *gc-count* (length (sb-thread:list-all-threads))
          (let ((kb (mem-available-kb))) (and kb (/ kb 1048576.0)))))

(defun check-heap ()
  (let ((frac (/ *post-gc-usage* (max 1 (sb-ext:dynamic-space-size)))))
    (cond ((and (not *heap-high*) (> frac +heap-warn+))
           (setf *heap-high* t)
           (session-log "HEAP HIGH: ~d of ~d MB still in use after a GC"
                        (mb *post-gc-usage*) (mb (sb-ext:dynamic-space-size))))
          ((and *heap-high* (< frac (- +heap-warn+ 0.1)))
           (setf *heap-high* nil)
           (session-log "heap back to ~d MB after a GC" (mb *post-gc-usage*))))))

(defun check-stderr-size ()
  "stderr.log is written by C and by children, around our cap. Keep it bounded:
   past the cap, truncate it (fd 2 is O_APPEND, so writers carry on at the new end)."
  (let ((path (log-path "stderr")))
    (ignore-errors
      (with-open-file (s path :if-does-not-exist nil)
        (when (and s (> (file-length s) +log-cap+))
          (sb-alien:alien-funcall
           (sb-alien:extern-alien "truncate" (function sb-alien:int sb-alien:c-string sb-alien:long))
           path 0)
          (session-log "stderr.log passed ~d MB and was truncated" (mb +log-cap+)))))))

(defun watch-step (state)
  "One tick of the watcher. STATE is a plist it keeps between ticks: the stall it is
   following (:since, the *MAIN-SINCE* that began it) and the thresholds already
   reported (:told). Returns the new STATE. Separate from the loop so a test can
   drive it."
  (let ((since (getf state :since)))
    ;; a stall that ended: the thread came back to poll
    (when (and since (not (and (eq *main-state* :busy) (eql *main-since* since))))
      (session-log "the environment's thread is responsive again (it was busy ~,1fs)"
                   (/ (- *main-since* since) internal-time-units-per-second))
      (setf state '()))
    (when (eq *main-state* :busy)
      (let ((busy (seconds-since *main-since*))
            (told (getf state :told)))
        (dolist (limit +stall-thresholds+)
          (when (and (>= busy limit) (not (member limit told)))
            (report-threads
             (format nil "STALL: the environment's thread has been busy ~ds~@[ — ~a~]"
                     (floor busy) *main-activity*))
            (setf state (list :since *main-since* :told (cons limit told))
                  told (cons limit told))))))
    state))

;;; ---- other threads that run too long ----------------------------------------
;;; A STALL is about the environment's thread, because that is what freezes the
;;; screen. An agent turn stuck in a read that never returns freezes nothing — the
;;; screen still works — but it is the next thing you would ask about, so a module
;;; can name threads that are BUSY and should not stay so: agent.lisp names every
;;; conversation's running turn. Past each of +BUSY-THRESHOLDS+ the thread's
;;; backtrace is logged, once.

(defparameter +busy-thresholds+ '(300 900)
  "Seconds a watched thread may stay busy before its backtrace is logged.")

(defvar *busy-sources* '()
  "Functions of no arguments, each returning a list of (LABEL . THREAD) for threads
   that are busy right now.")

(defun watch-busy-threads (fn) (pushnew fn *busy-sources*))

(defun watch-busy-step (seen)
  "One tick: SEEN is a hash table THREAD -> (FIRST-SEEN-ITU . THRESHOLDS-TOLD). A
   thread no source names any more is forgotten. Returns SEEN."
  (let ((now-busy '()))
    (dolist (src *busy-sources*)
      (dolist (entry (ignore-errors (funcall src)))
        (destructuring-bind (label . th) entry
          (when (and th (sb-thread:thread-alive-p th))
            (push th now-busy)
            (let ((rec (or (gethash th seen)
                           (setf (gethash th seen) (cons (get-internal-real-time) '())))))
              (dolist (limit +busy-thresholds+)
                (when (and (>= (seconds-since (car rec)) limit) (not (member limit (cdr rec))))
                  (push limit (cdr rec))
                  (session-log-block
                   (format nil "LONG: ~a has been busy ~ds — its backtrace:" label (floor (seconds-since (car rec))))
                   (thread-backtrace th)))))))))
    (loop for th being the hash-keys of seen
          unless (member th now-busy) do (remhash th seen))
    seen))

;;; ---- the kernel's log ---------------------------------------------------------
;;; The kernel keeps its messages in a ring buffer that /dev/kmsg hands out one
;;; record per read: "PRIORITY,SEQUENCE,MICROSECONDS,FLAGS;message". Copying it to
;;; kernel.log means a driver's complaint, an OOM kill or a hung-task report outlives
;;; the session. The file can be read by the user (CONFIG_SECURITY_DMESG_RESTRICT is
;;; off; the node is 0644). Records at "err" or worse (priority & 7 <= 3) are also
;;; said in session.log, where the rest of the story is.
;;;
;;; Drained every 2 s, one fsync per batch: the boot alone is a thousand records.
;;; So the last two seconds before a hard freeze can be missing — a kernel that is
;;; still alive keeps them in its buffer, and the next kernel tag's pstore (§6 of
;;; doc/diagnostics.org) is what catches a dying one.

(defvar *kmsg-thread* nil)

(defun append-log-lines (path texts)
  "APPEND-LOG-LINE for many lines: one open, one write, one fsync."
  (when (and texts (not *logs-closed*))
    (ignore-errors
      (sb-thread:with-recursive-lock (*log-lock*)
        (ensure-directories-exist path)
        (ensure-private-file path)
        (with-open-file (o path :direction :output :if-exists :append
                                :if-does-not-exist :create :external-format :utf-8)
          (let ((stamp (utc-stamp)))
            (dolist (text texts) (format o "~a ~a~%" stamp text)))
          (finish-output o)
          (fsync-fd (sb-sys:fd-stream-fd o))
          t)))))

(defun kmsg-record (rec)
  "\"6,1234,5678901,-;usb 1-1: new device\" -> (values LEVEL \"[    5.678901] usb …\"),
   or NIL for a record that does not parse."
  (let* ((semi (position #\; rec))
         (c1 (and semi (position #\, rec :end semi)))
         (c2 (and c1 (position #\, rec :start (1+ c1) :end semi)))
         (c3 (and c2 (position #\, rec :start (1+ c2) :end semi))))
    (when c2
      (let ((pri (parse-integer rec :end c1 :junk-allowed t))
            (us (parse-integer rec :start (1+ c2) :end (or c3 semi) :junk-allowed t))
            (msg (string-right-trim '(#\Newline) (subseq rec (1+ semi)))))
        (when (and pri us)
          ;; continuation lines (" SUBSYSTEM=…") follow the newline: keep the first
          (values (logand pri 7)
                  (format nil "[~5d.~6,'0d] ~a" (floor us 1000000) (mod us 1000000)
                          (subseq msg 0 (or (position #\Newline msg) (length msg))))))))))

(defun kmsg-loop ()
  (multiple-value-bind (fd err)
      ;; O_NONBLOCK is #o4000 on x86-64; sb-unix exports no name for it (wifi.lisp too)
      (sb-unix:unix-open "/dev/kmsg" (logior sb-unix:o_rdonly #o4000) 0)
    (unless fd
      (session-log "kernel log: cannot open /dev/kmsg: ~a" (sb-int:strerror err))
      (return-from kmsg-loop))
    (let ((buf (make-array 8192 :element-type '(unsigned-byte 8))))
      (loop
        (let ((all '()) (bad '()))
          (loop
            (multiple-value-bind (n errno)
                (sb-sys:with-pinned-objects (buf)
                  (sb-unix:unix-read fd (sb-sys:vector-sap buf) (length buf)))
              (cond ((and n (plusp n))
                     (multiple-value-bind (level line)
                         (kmsg-record (sb-ext:octets-to-string
                                       buf :end n :external-format '(:utf-8 :replacement #\?)))
                       (when line
                         (push line all)
                         (when (<= level 3) (push line bad)))))
                    ;; EPIPE: records were overwritten before we read them; go on
                    ((and (null n) (eql errno sb-unix:epipe)))
                    ((and (null n) (eql errno sb-unix:eintr)))
                    (t (return)))))       ; EAGAIN (nothing more), EOF, or an error
          (append-log-lines (log-path "kernel") (nreverse all))
          (dolist (line (nreverse bad)) (session-log "kernel: ~a" line)))
        (sleep 2)))))

(defun start-kmsg ()
  (unless (and *kmsg-thread* (sb-thread:thread-alive-p *kmsg-thread*))
    (setf *kmsg-thread* (sb-thread:make-thread (lambda () (ignore-errors (kmsg-loop)))
                                               :name "kmsg"))))

(defun watch-loop ()
  (let ((state '()) (next-pulse 0) (seen (make-hash-table :test #'eq)))
    (loop
      (ignore-errors
        (setf state (watch-step state))
        (setf seen (watch-busy-step seen))
        (when (>= (get-universal-time) next-pulse)
          (setf next-pulse (+ (get-universal-time) +pulse-seconds+))
          (session-log "~a" (pulse-line))
          (check-heap)
          (check-stderr-size)))
      (sleep 1))))

(defun start-watch ()
  (unless (and *watch-thread* (sb-thread:thread-alive-p *watch-thread*))
    (pushnew 'note-gc sb-ext:*after-gc-hooks*)
    (setf *watch-thread* (sb-thread:make-thread #'watch-loop :name "lol-watch"))))

;;; ===========================================================================
;;; Dying loudly instead of silently
;;; ===========================================================================

(defvar *previous-invoke-debugger-hook* nil)

(defun log-unhandled (condition hook)
  "*INVOKE-DEBUGGER-HOOK*: a condition reached the debugger with nothing to handle it.
   Log it with the backtrace. On a background thread, end THAT thread only
   (ABORT-THREAD runs its UNWIND-PROTECT cleanups) — the default would quit the
   whole process, which for PID 1 is a kernel panic. On the main thread, log and
   pass it on: there is no one else to hand it to."
  (ignore-errors
    (session-log-block (format nil "UNHANDLED ~s: ~a" (type-of condition)
                               (or (ignore-errors (clip-one-line condition 300)) "?"))
                       (backtrace-string :count 30)))
  (cond ((not (eq sb-thread:*current-thread* (sb-thread:main-thread)))
         (ignore-errors (session-log "thread [~a] ended by that error; the rest go on" (thread-tag)))
         (sb-thread:abort-thread))
        (*previous-invoke-debugger-hook*
         (funcall *previous-invoke-debugger-hook* condition hook))))

(defun log-condition (where condition)
  "For handlers that CATCH a serious condition and carry on: say so in the log."
  (session-log "~a: ~s ~a" where (type-of condition)
               (or (ignore-errors (clip-one-line condition 300)) "?")))

(defun disable-ldb ()
  "Make the C runtime's fatal errors END the process instead of waiting in LDB on a
   console nobody can see. disable_lossage_handler restores the runtime's default
   handler: print a backtrace to stderr (our stderr.log) and exit(1). As PID 1 that
   exit is a kernel panic, which preinit turns into a reboot (kernel.panic=10)."
  (ignore-errors
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "disable_lossage_handler" (function sb-alien:void)))
    t))

(defvar *console-stderr* nil "The console, on the fd that 2 used to be, for Lisp's *ERROR-OUTPUT*.")

(defun capture-stderr ()
  "Point file descriptor 2 at stderr.log, so what the C runtime and child processes
   write there survives the session — keeping Lisp's own *ERROR-OUTPUT* on the
   console, where the supervisor's menu and the REPL show it.

   Lisp's *ERROR-OUTPUT* is a synonym for SB-SYS:*STDERR*, a stream on fd 2. So:
   dup(2) keeps the console on a new fd; dup2(log, 2) puts the log at 2; and
   *STDERR* becomes a fresh stream on the kept console fd."
  (ignore-errors
    (let* ((path (log-path "stderr"))
           (fd (progn (ensure-directories-exist path)
                      ;; O_DSYNC (1 << 12, linux/include/uapi/asm-generic/fcntl.h): each write
                      ;; reaches the disk before it returns. Without it the runtime's
                      ;; dying words sit in the page cache, and a panic reboots in 10 s,
                      ;; before the 30 s writeback — found by check-crash.lisp, 2026-10-16.
                      (sb-unix:unix-open path (logior sb-unix:o_wronly sb-unix:o_creat sb-unix:o_append
                                                      #o10000)
                                         #o600))))
      (when fd
        (let ((console (sb-unix:unix-dup 2)))
          (when console
            (sb-alien:alien-funcall
             (sb-alien:extern-alien "dup2" (function sb-alien:int sb-alien:int sb-alien:int)) fd 2)
            (sb-unix:unix-close fd)
            (setf *console-stderr*
                  (sb-sys:make-fd-stream console :output t :buffering :line
                                                 :external-format :utf-8 :name "console"))
            (setf sb-sys:*stderr* *console-stderr*)
            t))))))

;;; ===========================================================================
;;; Boot: set the last session aside, start this one
;;; ===========================================================================

(defparameter +logs+ '("session" "network" "stderr" "kernel")
  "The live logs, by base name: <dir>/<base>.log.")

(defun file-mtime (path)
  (ignore-errors (nth-value 10 (sb-unix:unix-stat path))))

(defun unix->universal (unix) (+ unix (encode-universal-time 0 0 0 1 1 1970 0)))

(defun last-line (path)
  "The last non-empty line of PATH, reading only its tail."
  (ignore-errors
    (with-open-file (s path :external-format :utf-8 :element-type 'character)
      (let ((len (file-length s)))
        (file-position s (max 0 (- len 4096)))
        (let ((last nil))
          (loop for l = (read-line s nil) while l
                when (plusp (length (string-trim " " l))) do (setf last l))
          last)))))

(defvar *previous-session* nil
  "How the last session ended, from its session.log: (:clean T/NIL :last LINE :file PATH),
   or NIL when there was none.")

(defun set-aside-live-logs (&optional (dir (log-dir)))
  "Rename every non-empty live log to <base>.<last-written>.log, for ARCHIVE-PENDING.
   Records in *PREVIOUS-SESSION* whether the last session.log ended with the
   shutdown line POWER-OFF writes — no such line means it did not end on purpose."
  (setf *previous-session* nil)
  (dolist (base +logs+)
    (let ((path (format nil "~a~a.log" dir base)))
      (with-open-file (s path :if-does-not-exist nil)
        (when (and s (plusp (file-length s)))
          (close s)
          (let ((dest (set-aside-name path (unix->universal (or (file-mtime path) 0)))))
            (when (string= base "session")
              (let ((last (last-line path)))
                (setf *previous-session*
                      (list :clean (and last (search "] shutdown:" last) t)
                            :last last :file dest))))
            (ignore-errors (rename-file path dest))))))))

(defun pending-logs (&optional (dir (log-dir)))
  "Set-aside logs not archived yet: <base>.<stamp>.log in DIR."
  (remove-if-not (lambda (p)
                   (let ((name (pathname-name p)))
                     (and (equal (pathname-type p) "log") (find #\. name))))
                 (directory (concatenate 'string dir "*.log"))))

(defun prune-archive (archive)
  (let ((by-base (make-hash-table :test #'equal)))
    (dolist (p (directory (concatenate 'string archive "*.log.gz")))
      (let* ((name (file-namestring p))
             (base (subseq name 0 (position #\. name))))
        (push p (gethash base by-base))))
    (maphash (lambda (base files)
               (declare (ignore base))
               ;; the stamp sorts as text, so newest first is a string sort
               (let ((sorted (sort files #'string> :key #'file-namestring)))
                 (dolist (old (nthcdr +archive-keep+ sorted))
                   (ignore-errors (delete-file old)))
                 ;; archives from before the logs were private (2026-10-16) too
                 (dolist (keep (subseq sorted 0 (min +archive-keep+ (length sorted))))
                   (ignore-errors (chmod-path keep #o600)))))
             by-base)))

(defun archive-pending (&optional (dir (log-dir)))
  "gzip each set-aside log into DIR/archive/ and delete it; then keep only the newest
   +ARCHIVE-KEEP+ of each base. A crash midway leaves the plain file, and the next
   boot finishes the job (gzip-file writes .tmp and renames)."
  (let ((archive (concatenate 'string dir "archive/")))
    (ensure-directories-exist archive)
    (chmod-path dir #o700)
    (chmod-path archive #o700)
    (dolist (p (pending-logs dir))
      (ignore-errors
        (let ((out (format nil "~a~a.log.gz" archive (pathname-name p))))
          (lol.deflate:gzip-file p out :mtime (or (file-mtime (namestring p)) 0))
          (chmod-path out #o600)
          (delete-file p))))
    (prune-archive archive)))

(defun session-boot ()
  "Called first thing by the supervisor (PID 1). In order: set last session's logs
   aside (before anything writes a new line), take fd 2, log the boot and how the
   last session ended, catch unhandled errors and fatal runtime errors, start the
   watcher, and archive in the background."
  (set-aside-live-logs)
  (capture-stderr)
  (disable-ldb)
  (session-log "boot: pid ~d uid ~d, SBCL ~a, heap ~d MB, kernel ~a, log ~a"
               (sb-unix:unix-getpid) (sb-unix:unix-getuid) (lisp-implementation-version)
               (mb (sb-ext:dynamic-space-size)) (software-version) (log-dir))
  (let ((prev *previous-session*))
    (cond ((null prev) (session-log "previous session: no log"))
          ((getf prev :clean) (session-log "previous session: shut down cleanly"))
          (t (session-log "previous session: ENDED WITHOUT A SHUTDOWN — its last line was: ~a"
                          (clip-one-line (or (getf prev :last) "(empty)") 300))
             (session-log "  read it in ~aarchive/~a.gz" (log-dir) (file-namestring (getf prev :file))))))
  (unless (eq sb-ext:*invoke-debugger-hook* 'log-unhandled)
    (setf *previous-invoke-debugger-hook* sb-ext:*invoke-debugger-hook*
          sb-ext:*invoke-debugger-hook* 'log-unhandled))
  (start-watch)
  (start-kmsg)
  (sb-thread:make-thread (lambda () (ignore-errors (archive-pending))) :name "log-archive"))

(defun previous-session-unclean-p ()
  (and *previous-session* (not (getf *previous-session* :clean))))
