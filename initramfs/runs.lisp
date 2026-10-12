;;;; runs.lisp — run a program and STREAM its output into a read-only pane, live.
;;;;
;;;; doc/containers.org §5.2 step 1, "run and stream": the cheap half of a terminal.
;;;; A command runs; its output appears in a pane as it arrives, in its own colours;
;;;; the pane's title says whether it is still running and how it ended; k stops it.
;;;; That covers `git status`, `bundle install`, `rails test`, a server's log — and it
;;;; asks nothing of a terminal emulator, which is the separate, larger piece.
;;;;
;;;; Four decisions, each a choice:
;;;;
;;;;   1. A PTY, NOT A PIPE, for the program's output. Behind a pipe most programs
;;;;      switch colour off AND buffer their output in 4-8 KB blocks, so a slow test
;;;;      run would show nothing for a minute and then everything at once — the
;;;;      opposite of live. Behind a pty (SB-EXT:RUN-PROGRAM's :PTY, which opens
;;;;      /dev/ptmx — preinit mounts devpts for it) they believe a person is
;;;;      watching: colours on, a flush at every line. Its stdin is /dev/null all
;;;;      the same (lol-ct -N): a read-only pane has nothing to type, and a program
;;;;      that waited for input would wait forever.
;;;;
;;;;   2. A READER THREAD per run, and the UI only ever looks at a snapshot. The
;;;;      thread blocks in read(2) on the pty and feeds what comes through the ANSI
;;;;      line reader (ansi.lisp), under a mutex. The browser's poll loop never
;;;;      touches the fd: while a run is live it wakes every +RUN-TICK-MS+ (the way a
;;;;      running stage does, fb-browser.lisp), and TICK-RUNS copies the new lines
;;;;      into whichever pane shows them. So output keeps being collected while you
;;;;      look at something else — a pipe nobody drains fills at 64 KB and stalls
;;;;      the program, which is exactly the bug this shape cannot have.
;;;;
;;;;   3. THE PANE IS AN ORDINARY TEXT PANE. The view is :reference (read-only); its
;;;;      body is the lines; its colours come through the text view's SYNTAX slot as
;;;;      a function of the line index (textview.lisp). Scrolling, selection and
;;;;      copying are the text view's, unchanged. New output follows the end only if
;;;;      you were AT the end — scroll up to read and it stops chasing you.
;;;;
;;;;   4. STOPPING IS POLITE, THEN NOT. k sends SIGTERM; a run still alive
;;;;      +RUN-KILL-GRACE+ seconds later gets SIGKILL. For a container run the
;;;;      process we signal is lol-ct, which forwards the TERM inside and, if it is
;;;;      killed, takes the whole container down with it (lol-ct.c, "signals").
;;;;
;;;; Not here: input. A program that asks a question gets end-of-file. When it must
;;;; be answered, that is what the terminal pane is for.
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

(defparameter +run-tick-ms+ 100
  "How often the browser looks for new output while a run is live. Ten a second is
   faster than anyone reads a log and cheap: a tick with nothing new draws nothing.")

(defparameter +run-max-lines+ 20000
  "Lines a run keeps. A server's log grows without end; past this the oldest go, and
   the pane's first line says how many were dropped.")

(defparameter +run-kill-grace+ 3
  "Seconds between the polite SIGTERM and the SIGKILL.")

(defstruct (run (:constructor %make-run))
  argv                    ; what was run, as given
  title                   ; a short human name ("git status")
  container               ; the container's name, or NIL for a plain host-side run
  process                 ; the SB-EXT:PROCESS
  (lock (sb-thread:make-mutex :name "run output"))
  (lines (make-array 64 :adjustable t :fill-pointer 0))  ; finished (STRING . RUNS)
  (reader (make-ansi-reader))
  (dropped 0)             ; lines discarded from the front (+RUN-MAX-LINES+)
  (status :running)       ; :running | :exited | :signalled | :failed
  code                    ; exit code, or the signal number
  (started (get-internal-real-time))
  ended
  term-sent               ; internal-real-time of our SIGTERM, or NIL
  (generation 0)          ; bumped by the reader on every change; the UI compares
  thread)

(defstruct (subj-runs (:constructor subj-runs ())))   ; "every run", presented below

(defvar *runs* '() "Every run started in this session, newest first.")
(defvar *runs-lock* (sb-thread:make-mutex :name "runs list"))

;;; ---- the reader thread ---------------------------------------------------------

(defun utf8-complete-length (octets n)
  "How many of the first N OCTETS form complete UTF-8 characters. A read can end in
   the middle of a multi-byte character; the tail waits for the next read."
  (loop for back from 1 to (min 3 n)
        for b = (aref octets (- n back))
        do (cond ((< b #x80) (return-from utf8-complete-length n))        ; ASCII
                 ((>= b #xC0)                                             ; a lead byte
                  (let ((need (cond ((>= b #xF0) 4) ((>= b #xE0) 3) (t 2))))
                    (return-from utf8-complete-length
                      (if (> need back) (- n back) n))))))
  n)

(defun run-append-line (run line)
  "Store one finished line (called with the run's lock held)."
  (let ((lines (run-lines run)))
    (when (>= (length lines) +run-max-lines+)
      ;; drop the oldest tenth in one go, so this is not a shift per line
      (let ((cut (floor +run-max-lines+ 10)))
        (replace lines lines :start2 cut)
        (decf (fill-pointer lines) cut)
        (incf (run-dropped run) cut)))
    (vector-push-extend line lines)))

(defun run-reader-loop (run fd)
  "THE READER THREAD: read the pty until it says the program is gone, then wait for
   the exit status. read(2) on a pty master returns EIO — not 0 — once every holder
   of the slave end has closed it; either way, that is the end of the output."
  (let ((buf (make-array 8192 :element-type '(unsigned-byte 8)))
        (carry 0))
    (loop
      (let ((n (sb-sys:with-pinned-objects (buf)
                 (sb-unix:unix-read fd (sb-sys:sap+ (sb-sys:vector-sap buf) carry)
                                    (- (length buf) carry)))))
        (when (or (null n) (<= n 0)) (return))
        (let* ((have (+ carry n))
               (ok (utf8-complete-length buf have))
               (text (sb-ext:octets-to-string buf :end ok
                                                  :external-format '(:utf-8 :replacement #\?))))
          (replace buf buf :start2 ok :end2 have)
          (setf carry (- have ok))
          (sb-thread:with-mutex ((run-lock run))
            (ansi-feed (run-reader run) text (lambda (line) (run-append-line run line)))
            (incf (run-generation run)))))))
  ;; The output is over; the process may not quite be. PROCESS-STATUS reaps through
  ;; waitpid(WNOHANG), so poll it rather than block in a call that serves events.
  (let ((p (run-process run)))
    (loop while (member (ignore-errors (sb-ext:process-status p)) '(:running :stopped))
          do (sleep 0.05))
    (sb-thread:with-mutex ((run-lock run))
      ;; a last line without its newline becomes a line of its own — TAKEN from the
      ;; reader, not copied, or the snapshot would show it twice (it once did)
      (let ((last (ansi-take-line (run-reader run))))
        (when (plusp (length (car last))) (run-append-line run last)))
      (setf (run-status run) (if (eq (sb-ext:process-status p) :signaled) :signalled :exited)
            (run-code run) (sb-ext:process-exit-code p)
            (run-ended run) (get-internal-real-time))
      (incf (run-generation run)))
    (ignore-errors (sb-ext:process-close p))))

;;; ---- starting one --------------------------------------------------------------

(defun set-pty-size (fd cols rows)
  "TIOCSWINSZ: tell the pty its size, so a program that formats to the terminal width
   (ls, git's diffstat, rails' tables) formats to the pane rather than to 0 or 80."
  (sb-alien:with-alien ((ws (array (sb-alien:unsigned 16) 4)))
    (setf (sb-alien:deref ws 0) rows (sb-alien:deref ws 1) cols
          (sb-alien:deref ws 2) 0 (sb-alien:deref ws 3) 0)
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "ioctl" (function sb-alien:int sb-alien:int sb-alien:unsigned-long
                                              (* (array (sb-alien:unsigned 16) 4))))
     fd #x5414 (sb-alien:addr ws))))                      ; TIOCSWINSZ, asm-generic/ioctls.h

(defun pane-columns ()
  "Roughly how many characters a full-width pane holds, from the CURRENT font (never
   a hard-coded cell size: the font can change at run time)."
  (let ((w (or (and (boundp '*fb-xres*) (symbol-value '*fb-xres*)) 1280))
        (cw (or (ignore-errors (lol.canvas:font-cw *bfont*)) 6)))
    (max 40 (- (floor w cw) 8))))

(defun start-run (argv &key title container environment)
  "Start ARGV (program + arguments, a list of strings) on a pty and return a RUN
   whose output a reader thread collects. ENVIRONMENT, a list of \"VAR=value\",
   replaces the inherited one when given. Signals nothing: a program that cannot be
   started becomes a run that :FAILED, with the reason as its output."
  (let ((run (%make-run :argv argv :container container
                        :title (or title (format nil "~{~a~^ ~}" argv)))))
    (handler-case
        (let* ((p (sb-ext:run-program (first argv) (rest argv)
                                      :search t :wait nil :pty t :input nil
                                      :environment (or environment (sb-ext:posix-environ))))
               (fd (sb-sys:fd-stream-fd (sb-ext:process-pty p))))
          (setf (run-process run) p)
          (ignore-errors (set-pty-size fd (pane-columns) 40))
          (setf (run-thread run)
                (sb-thread:make-thread #'run-reader-loop :arguments (list run fd)
                                       :name (format nil "run ~a" (run-title run)))))
      (serious-condition (c)
        (vector-push-extend (cons (format nil "could not start ~a: ~a" (first argv) c)
                                  (list (list* 0 0 nil)))
                            (run-lines run))
        (setf (run-status run) :failed (run-ended run) (get-internal-real-time))))
    (sb-thread:with-mutex (*runs-lock*) (push run *runs*))
    run))

(defun run-live-p (run) (eq (run-status run) :running))

(defun stop-run (run)
  "SIGTERM now; TICK-RUNS sends SIGKILL if it is still alive +RUN-KILL-GRACE+ later."
  (when (and (run-live-p run) (run-process run))
    (ignore-errors (sb-ext:process-kill (run-process run) sb-unix:sigterm))
    (setf (run-term-sent run) (get-internal-real-time))
    t))

(defun run-again (run)
  "A new run of the same command, same container, same environment."
  (start-run (run-argv run) :title (run-title run) :container (run-container run)))

(defun run-seconds (run)
  (/ (- (or (run-ended run) (get-internal-real-time)) (run-started run))
     internal-time-units-per-second))

(defun run-state-string (run)
  "How the run stands, in words: running 4 s / exit 0 after 1.2 s / killed by signal 15."
  (case (run-status run)
    (:running (format nil "running ~d s~:[~; - stopping~]" (floor (run-seconds run)) (run-term-sent run)))
    (:exited (format nil "exit ~a after ~,1f s" (run-code run) (run-seconds run)))
    (:signalled (format nil "killed by signal ~a after ~,1f s" (run-code run) (run-seconds run)))
    (t "could not start")))

;;; ---- the snapshot the UI draws -------------------------------------------------

(defparameter +run-ok-colour+  #x98C379)
(defparameter +run-bad-colour+ #xE06C75)
(defparameter +run-dim-colour+ #x7F848E)

(defun run-snapshot (run)
  "Under the lock: a fresh simple-vector of (STRING . RUNS) — every finished line, the
   partial one, and when the run is over a last line saying how it ended. The UI only
   ever touches this copy, never the vector the reader thread is extending."
  (sb-thread:with-mutex ((run-lock run))
    (let* ((done (run-lines run))
           (out (make-array (+ (length done) 3) :fill-pointer 0)))
      (when (plusp (run-dropped run))
        (let ((s (format nil "[~:d earlier lines dropped]" (run-dropped run))))
          (vector-push (cons s (list (list* 0 (length s) +run-dim-colour+))) out)))
      (loop for l across done do (vector-push l out))
      (let ((partial (ansi-reader-partial (run-reader run))))
        (when (plusp (length (car partial))) (vector-push partial out)))
      (unless (run-live-p run)
        (let ((s (format nil "-- ~a --" (run-state-string run))))
          (vector-push (cons s (list (list* 0 (length s)
                                            (if (and (eq (run-status run) :exited)
                                                     (eql (run-code run) 0))
                                                +run-ok-colour+ +run-bad-colour+))))
                       out)))
      (when (zerop (length out)) (vector-push (cons "" '()) out))
      (coerce out 'simple-vector))))

(defun run-view-title (run)
  (format nil "run ~a~@[ in ~a~] - ~a" (run-title run) (run-container run) (run-state-string run)))

;;; ---- the subject ----------------------------------------------------------------
;;; The RUN struct is the subject itself: there is nothing to wrap.

(defmethod present ((run run))
  (let* ((snap (run-snapshot run))
         (box (vector snap)))                ; TICK-RUNS swaps a newer snapshot in here
    (make-view :title (run-view-title run)
               :kind :reference :subject run
               :text (format nil "~{~a~^~%~}" (map 'list #'car snap))
               :data (list :box box :generation -1)
               ;; the text view asks us for each line's colours (textview.lisp, SYNTAX)
               :syntax (lambda (li line)
                         (let ((v (svref box 0)))
                           (if (< li (length v))
                               (mapcar (lambda (r)
                                         (list* (min (first r) (length line))
                                                (min (second r) (length line))
                                                (or (cddr r) lol.textview::*c-text*)))
                                       (cdr (svref v li)))
                               (list (list* 0 (length line) lol.textview::*c-text*))))))))

(defmethod commands-for ((run run))
  (append
   (when (run-live-p run)
     (list (make-command :label "Stop (TERM, then KILL)" :key "k"
                         :action (lambda () (stop-run run)) :surfaces '(:menu :halo))))
   (list (make-command :label "Run again" :key "r" :surfaces '(:menu :halo)
                       :action (lambda () (show-run (run-again run))))
         (make-command :label "All runs" :target (subj-runs) :surfaces '(:menu)))))

(defmethod subject-key ((run run) pane key state)
  (declare (ignore state))
  (case key
    ((#\k #\K) (if (stop-run run)
                   (setf *browser-status* "sent SIGTERM - SIGKILL follows if it lingers")
                   (setf *browser-status* "not running"))
     t)
    ((#\r #\R) (trail-pop) (show-run (run-again run)) t)
    ((#\G :end) (run-pane-follow pane t) t)
    (t nil)))

(defmethod subject-hints ((run run) pane)
  (declare (ignore pane))
  (if (run-live-p run)
      '(("k" . "stop") ("G" . "follow the end") ("r" . "run again") ("q" . "back"))
      '(("r" . "run again") ("q" . "back") ("M-w" . "copy"))))

(defun show-run (run)
  "Open RUN's pane: drilled into the current trail. Returns RUN."
  (trail-push run)
  run)

;;; ---- liveness: what the browser's poll loop asks ----------------------------------

(defun run-pane-follow (pane &optional force)
  "Keep PANE's view on the newest line — only when it already was (or FORCE)."
  (let ((tv (pane-tv pane)))
    (when tv
      (let* ((n (length (lol.textview:tv-lines tv)))
             (vis (lol.textview::visible-lines tv)))
        (when (or force (>= (+ (lol.textview::tv-scroll tv) vis) (1- n)))
          (setf (lol.textview::tv-scroll tv) (max 0 (- n vis))))))))

(defun refresh-run-pane (pane)
  "Bring PANE (showing a RUN) up to the run's latest output. True if it changed."
  (let* ((run (pane-subject pane))
         (view (pane-view pane))
         (data (view-data view))
         (tv (pane-tv pane)))
    ;; (not before the pane has been drawn once: until then it has no height, and
    ;; "follow the end" would scroll the last line to the top)
    (when (and tv (listp data) (plusp (lol.textview::tv-h tv))
               (/= (getf data :generation) (run-generation run)))
      (let* ((gen (run-generation run))
             (snap (run-snapshot run))
             (n-old (length (lol.textview:tv-lines tv)))
             (vis (lol.textview::visible-lines tv))
             (at-end (or (= (getf data :generation) -1)        ; a fresh pane opens at the end
                         (>= (+ (lol.textview::tv-scroll tv) vis) (1- n-old)))))
        (setf (svref (getf data :box) 0) snap
              (lol.textview:tv-lines tv) (map 'simple-vector #'car snap)
              (getf (view-data view) :generation) gen
              (view-title view) (run-view-title run))
        ;; keep point inside the (possibly shorter, once lines are dropped) buffer
        (setf (lol.textview:tv-point-line tv)
              (min (lol.textview:tv-point-line tv) (1- (length snap))))
        (when at-end (run-pane-follow pane t))
        t))))

(defun visible-run-panes ()
  "The panes on screen right now that show a run: the current one, and its side pane."
  (let ((p (ignore-errors (current-pane))))
    (remove-if-not (lambda (x) (and x (typep (pane-subject x) 'run)))
                   (list p (and p (pane-side p))))))

(defun runs-timeout ()
  "The poll timeout runs need: +RUN-TICK-MS+ while any run is live or a visible run
   pane is behind its run, else -1 (no deadline: the loop may sleep until input)."
  (if (or (some #'run-live-p *runs*)
          (some (lambda (p) (let ((d (view-data (pane-view p))))
                              (and (listp d) (/= (getf d :generation)
                                                 (run-generation (pane-subject p))))))
                (visible-run-panes)))
      +run-tick-ms+
      -1))

(defun tick-runs ()
  "Called by the browser's poll loop when a deadline fires: escalate overdue stops,
   refresh the visible run panes. True when the frame needs redrawing."
  (dolist (run *runs*)
    (when (and (run-live-p run) (run-term-sent run)
               (> (- (get-internal-real-time) (run-term-sent run))
                  (* +run-kill-grace+ internal-time-units-per-second)))
      (ignore-errors (sb-ext:process-kill (run-process run) sb-unix:sigkill))
      (setf (run-term-sent run) (get-internal-real-time))))
  (let ((changed nil))
    (dolist (p (visible-run-panes))
      (when (refresh-run-pane p) (setf changed t)))
    ;; a live run's title carries its age in whole seconds: redraw when THAT changes,
    ;; not on every tick — a full frame ten times a second for a quiet server's pane
    ;; would be a busy loop wearing a clock
    (dolist (p (visible-run-panes))
      (let ((run (pane-subject p)) (data (view-data (pane-view p))))
        (when (and (run-live-p run) (listp data)
                   (/= (floor (run-seconds run)) (or (getf data :age) -1)))
          (setf (getf (view-data (pane-view p)) :age) (floor (run-seconds run))
                (view-title (pane-view p)) (run-view-title run)
                changed t))))
    changed))

;;; ---- every run, as a list ----------------------------------------------------------

(defmethod present ((s subj-runs))
  (make-view :title "runs" :kind :list :subject s
             :items (or (mapcar (lambda (run)
                                  (make-item :label (run-title run)
                                             :detail (format nil "~@[~a - ~]~a" (run-container run)
                                                             (run-state-string run))
                                             :kind (if (run-live-p run) :restart :function)
                                             :group (if (run-live-p run) "running" "finished")
                                             :subject run))
                                (sort (copy-list *runs*)
                                      (lambda (a b) (and (run-live-p a) (not (run-live-p b))))))
                        (list (make-item :label "nothing has run yet"
                                         :detail "a container's x runs a command")))))

(defun runs-spotter-candidates ()
  (list (list :label "runs" :detail "surface · every command started, live ones first"
              :group "surfaces" :subject (subj-runs))))

(pushnew 'runs-spotter-candidates *spotter-sources*)

(defun run-command-line (argv &key title)
  "REPL entry point: run ARGV on the host side (no container) and wait for it,
   returning its output lines and exit code — for a quick check without the browser."
  (let ((run (start-run argv :title title)))
    (loop while (run-live-p run) do (sleep 0.05))
    (values (map 'list #'car (run-snapshot run)) (run-code run))))
