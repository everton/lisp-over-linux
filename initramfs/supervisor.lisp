;;;; supervisor.lisp — a tiny, didactic init/supervisor in Common Lisp.
;;;;
;;;; Roles chosen by argv:
;;;;   (no args)   => interactive SUPERVISOR menu  (this is PID 1)
;;;;   "worker" N  => a short-lived WORKER process
;;;;
;;;; The supervisor menu lets you run a Lisp REPL, spawn a worker, or power
;;;; the machine off. PID 1 must NEVER return, so every path either loops or
;;;; powers off, and errors are caught so a typo can't crash init.
;;;;
;;;; This file is just the loop + entry point. The pieces it drives live in
;;;; sibling files, loaded before it (see build.sh):
;;;;   process.lisp      worker-main, spawn-worker, power-off
;;;;   framebuffer.lisp  draw-alien (the Land-of-Lisp sprite)
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
  (format t "  I am PID ~a · SBCL ~a~%~%"
          (sb-unix:unix-getpid) (lisp-implementation-version))
  (format t "  r) run a Lisp REPL~%")
  (format t "  w) spawn a worker process~%")
  (format t "  a) draw the Land-of-Lisp alien~%")
  (format t "  t) network REPL on TCP :4005  [~a]~%"
          (if (net-repl-running-p) "RUNNING" "stopped"))
  (format t "  s) shut down (power off)~%")
  (format t "============================================~%")
  (format t "choice> ") (finish-output)
  (draw-alien))                  ; (re)paint the alien LAST so it stays on screen

(defun supervisor-main ()
  "PID 1: an interactive menu loop. Never returns."
  (format t "~%Supervisor up — I am PID ~a, SBCL ~a.~%"
          (sb-unix:unix-getpid) (lisp-implementation-version))
  (sleep 2)                       ; give USB a moment to enumerate
  (show-input-devices)            ; diagnostic: which keyboard(s) bound?
  ;; Configure eth0 (QEMU SLIRP address) so the network REPL is ready to enable —
  ;; but do NOT start the server: it is an opt-in menu choice ('t'). (§2/§6.)
  (if (ignore-errors (bring-up-interface "eth0" "10.0.2.15" "255.255.255.0"))
      (format t "~&eth0 up at 10.0.2.15/24 — network REPL is OFF (enable with 't').~%")
      (format t "~&eth0 could not be configured (no NIC bound?).~%"))
  (show-net-interfaces)           ; diagnostic: NIC bound + address
  (sleep 1)                       ; let the boot diagnostics be read first
  (format t "~C[2J~C[H" #\Escape #\Escape)  ; clear screen for a clean menu + alien
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
                ((string-equal choice "w")
                 (incf worker-id)
                 (format t "~&spawning worker ~a~%" worker-id)
                 (spawn-worker worker-id))
                ((string-equal choice "a") (draw-alien :announce t))
                ((string-equal choice "t")
                 (ecase (toggle-net-repl)
                   (:started
                    (format t "~&network REPL ENABLED on 0.0.0.0:4005.~%")
                    (format t "  connect from the HOST:  host-client/lol-repl-client~%")
                    (format t "  (INSECURE: remote eval = remote root — trusted wires only)~%"))
                   (:stopped
                    (format t "~&network REPL disabled.~%"))))
                ((string-equal choice "s") (power-off))
                ((string= choice ""))                 ; bare Enter: just redraw
                (t (format t "~&unknown choice: ~a~%" choice)))))
        (serious-condition (c)
          (format t "~&menu error: ~a~%" c) (finish-output))))))

(defun init-toplevel ()
  (let ((args (rest sb-ext:*posix-argv*)))
    (if (and args (string= (first args) "worker"))
        (worker-main (second args))
        (supervisor-main))))
