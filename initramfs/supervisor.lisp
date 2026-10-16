;;;; supervisor.lisp — a tiny, didactic init/supervisor in Common Lisp.
;;;;
;;;; Roles chosen by argv:
;;;;   (no args)   => the SUPERVISOR (this is PID 1): boots into the environment,
;;;;                  then an interactive menu once you leave it
;;;;   "worker" N  => a short-lived WORKER process
;;;;
;;;; At boot it opens the environment (the code browser, fb-browser.lisp) directly;
;;;; the supervisor menu is what is left when you quit it or it dies, and lets you
;;;; reopen it, run a Lisp REPL, spawn a worker, or power the machine off. PID 1
;;;; must NEVER return, so every path either loops or powers off, and errors are
;;;; caught so a typo can't crash init.
;;;;
;;;; This file is just the loop + entry point. The pieces it drives live in
;;;; sibling files, loaded before it (see build.sh):
;;;;   process.lisp      worker-main, spawn-worker, power-off
;;;;   framebuffer.lisp  draw-alien (the Land-of-Lisp sprite)
;;;;   meminfo.lisp      report-memory (the "m" menu action)
;;;;   line-editor.lisp  the "poor man's readline" toolkit
;;;;   repl.lisp         run-repl (the "r" menu action)

(defun show-input-devices ()
  "Diagnostic: list the input devices the kernel currently sees (from
   /proc/bus/input/devices). Tells us whether a keyboard driver bound."
  (format t "~&Input devices detected by the kernel:~%")
  (let ((any nil))
    (ignore-errors
      (with-open-file (s "/proc/bus/input/devices" :if-does-not-exist nil)
        (when s
          (loop for line = (read-line s nil nil) while line do
            (when (and (> (length line) 2) (string= (subseq line 0 2) "N:"))
              (setf any t)
              (format t "   ~a~%" line))))))
    (unless any
      (format t "   (none found — no keyboard driver bound)~%")))
  (finish-output))

(defun show-net-interfaces ()
  "Diagnostic: list the network interfaces the kernel currently sees (from
   /proc/net/dev), with each one's link state from sysfs. Confirms the NIC
   driver bound and created e.g. eth0 (lo always exists once NET is on)."
  (format t "~&Network interfaces detected by the kernel:~%")
  (let ((any nil))
    (ignore-errors
      (with-open-file (s "/proc/net/dev" :if-does-not-exist nil)
        (when s
          (read-line s nil nil) (read-line s nil nil)   ; skip the two header rows
          (loop for line = (read-line s nil nil) while line do
            (let ((colon (position #\: line)))
              (when colon
                (let* ((name  (string-trim " " (subseq line 0 colon)))
                       (state (ignore-errors
                                (with-open-file
                                    (o (format nil "/sys/class/net/~a/operstate" name)
                                       :if-does-not-exist nil)
                                  (and o (read-line o nil nil))))))
                  (setf any t)
                  (format t "   ~a~@[  (~a)~]~@[  ~a~]~%"
                          name state (interface-ipv4 name)))))))))   ; address if any
    (unless any
      (format t "   (none found — is CONFIG_NET on?)~%")))
  (finish-output))

(defun print-menu ()
  (format t "~&~%======== lisp-over-linux supervisor ========~%")
  (format t "  I am PID ~a · SBCL ~a · ~a ~a~%~%"
          (sb-unix:unix-getpid) (lisp-implementation-version)
          (software-type) (software-version))
  (format t "  r) run a Lisp REPL~%")
  (format t "  b) environment — the Workspace; Super-m modules, Super-Space anything~%")
  (format t "  p) Lisp workspace (a scratch buffer: Do it / Print it / Inspect it)~%")
  (format t "  c) change set (definitions edited live this session)~%")
  (format t "  l) all definitions in CL-USER (the flat package view)~%")
  (format t "  w) spawn a worker process~%")
  (format t "  a) draw the Land-of-Lisp alien~%")
  (format t "  m) show memory usage~%")
  (format t "  t) network REPL on TCP :4005  [~a]~%"
          (if (net-repl-running-p) "RUNNING" "stopped"))
  (format t "  s) shut down (power off)~%")
  (format t "============================================~%")
  (format t "choice> ") (finish-output)
  (draw-alien))                  ; (re)paint the alien LAST so it stays on screen

(defun supervisor-main ()
  "PID 1: boot diagnostics, the environment, then an interactive menu loop.
   Never returns."
  ;; FIRST, before anything writes a log line: set the last session's logs aside,
  ;; take fd 2, start the watcher (session-log.lisp, doc/diagnostics.org)
  (session-boot)
  (when (previous-session-unclean-p)
    (setf *browser-status* "the last session ended without a shutdown — see ~/.lol/log/archive/"))
  (format t "~%Supervisor up — I am PID ~a, SBCL ~a on ~a ~a.~%"
          (sb-unix:unix-getpid) (lisp-implementation-version)
          (software-type) (software-version))
  (sleep 2)                       ; give USB a moment to enumerate
  (show-input-devices)            ; diagnostic: which keyboard(s) bound?
  (network-up)                    ; DHCP on each wired link + default route + resolv.conf,
                                  ;   then the netd thread (renewals, Wi-Fi); prints result
  ;; The network REPL starts only when the boot asked for it (QEMU's fw_cfg or
  ;; lol.netrepl=1 on the cmdline) — never on the laptop by default. net.lisp,
  ;; NET-REPL-WANTED-P. The menu's 't' can still switch it on by hand.
  (cond ((not (net-repl-wanted-p))
         (format t "~&network REPL off (not requested; 't' in the menu starts it).~%"))
        ((ignore-errors (start-net-repl))
         (format t "~&network REPL ENABLED on 0.0.0.0:4005 — connect from the HOST with~%")
         (format t "  host-client/lol-repl-client  (toggle off with 't' in the menu)~%")
         (format t "  INSECURE: remote eval = remote root — trusted wires only.~%"))
        (t (format t "~&network REPL could not start.~%")))
  (show-net-interfaces)           ; diagnostic: NIC bound + address
  (sleep 1)                       ; let the boot diagnostics be read first
  ;; the journal: every Accept of earlier sessions compiled back in, each entry
  ;; guarded on its own — a bad one is skipped and reported, never fatal
  ;; (journal.lisp, doc/session-persistence.org). The net REPL is up first, so a
  ;; boot that misbehaves here can still be looked at.
  (boot-replay-journal)
  ;; Boot straight INTO the environment: it is what the machine is for. The menu
  ;; below is the fallback — you land there when you leave the environment
  ;; (SuperL-q, or q on a list), or when it dies: RUN-BROWSER-FB catches any error,
  ;; prints it and returns, so a broken environment still leaves a working PID 1.
  ;; With no framebuffer at all, it fails at once and the menu is simply first.
  ;; The screen is cleared BEFORE, not after: a dying environment's last words are
  ;; printed on the way out, and should still be there above the menu.
  (format t "~C[2J~C[H" #\Escape #\Escape)  ; clear screen for a clean menu + alien
  (run-browser-fb)
  (let ((worker-id 0))
    (loop
      (handler-case
          (progn
            (print-menu)
            (let* ((line   (read-line *standard-input* nil :eof))
                   (choice (and (stringp line)
                                (string-trim '(#\Space #\Tab #\Return) line))))
              (cond
                ((eq line :eof) (sleep 1))            ; no input source: don't spin
                ((string-equal choice "r") (run-repl))
                ((string-equal choice "b") (run-browser-fb))
                ((string-equal choice "p") (run-browser-fb (subj-workspace)))
                ((string-equal choice "c") (run-browser-fb (subj-change-set)))
                ((string-equal choice "l") (run-browser-fb (find-package :cl-user)))
                ((string-equal choice "w")
                 (incf worker-id)
                 (format t "~&spawning worker ~a~%" worker-id)
                 (spawn-worker worker-id))
                ((string-equal choice "a") (draw-alien :announce t))
                ((string-equal choice "m") (report-memory))
                ((string-equal choice "t")
                 (ecase (toggle-net-repl)
                   (:started
                    (format t "~&network REPL ENABLED on 0.0.0.0:4005.~%")
                    (format t "  connect from the HOST:  host-client/lol-repl-client~%")
                    (format t "  (INSECURE: remote eval = remote root — trusted wires only)~%"))
                   (:stopped
                    (format t "~&network REPL disabled.~%"))))
                ((string-equal choice "s") (console-shutdown))   ; asks: save the desk? (desk.lisp)
                ((string= choice ""))                 ; bare Enter: just redraw
                (t (format t "~&unknown choice: ~a~%" choice)))))
        (serious-condition (c)
          (log-condition "supervisor menu" c)
          (format t "~&menu error: ~a~%" c) (finish-output))))))

(defun init-toplevel ()
  "Which role this process plays, decided by argv — the image re-execs itself, so
   every process here is this same binary wearing a different hat.

     worker N   the throwaway demo child (process.lisp)
     job NAME   compute NAME and print a VIEW to stdout (worker.lisp); the parent
                reads it and presents it, which is how a second process puts
                something on this screen without touching /dev/fb0 at all.
     serve NAME stay alive and SERVE live job NAME over stdin/stdout: views by
                handle, (:invalidate h) when they change (worker-live.lisp)
     try        read one definition on stdin, Accept it HERE, say how it went —
                the sandbox PID 1 tries a definition in first (sandbox.lisp)
     repl       a Lisp REPL on this process's terminal — what a terminal pane runs
                when there is no host shell to run (doc/terminal.org). A separate
                heap: nothing typed there can hurt the environment.
     (none)     the supervisor, i.e. PID 1"
  ;; every role: a fatal runtime error ends the process instead of waiting in LDB on
  ;; a console nobody can see (session-log.lisp, DISABLE-LDB)
  (disable-ldb)
  (let ((args (rest sb-ext:*posix-argv*)))
    (cond ((and args (string= (first args) "worker"))
           (worker-main (second args)))
          ((and args (string= (first args) "job"))
           (run-worker-job (second args)))
          ((and args (string= (first args) "serve"))
           (run-worker-server (second args)))
          ((and args (string= (first args) "try"))
           (run-accept-trial))
          ((and args (string= (first args) "repl"))
           (run-repl)
           (sb-ext:exit :abort t))
          (t (supervisor-main)))))
