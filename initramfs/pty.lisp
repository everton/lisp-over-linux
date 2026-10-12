;;;; pty.lisp — pseudo-terminals: run a program as if a person sat at a terminal.
;;;;
;;;; A program like bash, vim or psql does not write to "the screen"; it writes to
;;;; its standard output, and if that is a TERMINAL it behaves like an interactive
;;;; program (a prompt, line editing, colours, full-screen drawing). A pipe would make
;;;; it a batch program. A PSEUDO-TERMINAL is a pair of character devices the kernel
;;;; makes on demand, so a process can BE the terminal for another one:
;;;;
;;;;    terminal pane (us) ── master fd ══ kernel tty layer ══ slave /dev/pts/N ── bash
;;;;                                       (line discipline:
;;;;                                        echo, ^C -> SIGINT,
;;;;                                        the window size)
;;;;
;;;; We open /dev/ptmx and get the MASTER end; the kernel creates /dev/pts/N, the
;;;; SLAVE, which the child opens as its stdin/stdout/stderr. Bytes we write to the
;;;; master arrive at the child as typed input; what the child writes comes out of
;;;; the master for us to parse (poc/terminal.lisp). Between the two sits the line
;;;; discipline — the same code that serves a real serial line, which is why ^C
;;;; still means "interrupt" and ^Z "suspend" without our doing anything.
;;;;
;;;; The child must also make that slave its CONTROLLING TERMINAL, or bash prints
;;;; "no job control in this shell" and ^C kills nothing. That takes two steps in the
;;;; child, before exec: setsid() (start a new SESSION, of which it is the leader and
;;;; which has no terminal yet), then open the slave (the first terminal a session
;;;; leader opens becomes its controlling terminal). SBCL's RUN-PROGRAM :PTY does not
;;;; do the first step (it calls setpgrp, not setsid) and switches ECHO off, so it is
;;;; not usable here. We use posix_spawn(3) instead: glibc's POSIX_SPAWN_SETSID does
;;;; the setsid, and a file action opens the slave on fd 0 AFTER it — glibc runs the
;;;; two in exactly that order (sysdeps/unix/sysv/linux/spawni.c). No fork in Lisp,
;;;; no C helper: posix_spawn is vfork+exec done in libc, in a child that runs no
;;;; Lisp at all, which is why it is safe to call from a garbage-collected image.
;;;;
;;;; CL-USER, every call through EXTERN-ALIEN with basic types (INT, C-STRING,
;;;; SYSTEM-AREA-POINTER) — no sb-posix (doc/processes.org §6), so every definition
;;;; here can be recompiled in the guest. Host test: poc/pty-test.lisp.
;;;; The tour: doc/terminal.org.

(defmacro define-pty-libc (lisp-name c-name result &rest arg-types)
  "LISP-NAME calls libc's C-NAME (the same shape as files.lisp's DEFINE-LIBC; ours so
   this file loads on its own in the host test)."
  (let ((args (loop for i below (length arg-types) collect (gensym "A"))))
    `(defun ,lisp-name ,args
       (sb-alien:alien-funcall
        (sb-alien:extern-alien ,c-name (function ,result ,@arg-types))
        ,@args))))

(define-pty-libc %pty-open     "open"     sb-alien:int sb-alien:c-string sb-alien:int sb-alien:int)
(define-pty-libc %pty-close    "close"    sb-alien:int sb-alien:int)
(define-pty-libc %pty-read     "read"     sb-alien:long sb-alien:int sb-alien:system-area-pointer sb-alien:unsigned-long)
(define-pty-libc %pty-write    "write"    sb-alien:long sb-alien:int sb-alien:system-area-pointer sb-alien:unsigned-long)
(define-pty-libc %pty-ioctl    "ioctl"    sb-alien:int sb-alien:int sb-alien:unsigned-long sb-alien:system-area-pointer)
(define-pty-libc %grantpt      "grantpt"  sb-alien:int sb-alien:int)
(define-pty-libc %unlockpt     "unlockpt" sb-alien:int sb-alien:int)
(define-pty-libc %ptsname      "ptsname"  sb-alien:c-string sb-alien:int)
(define-pty-libc %waitpid      "waitpid"  sb-alien:int sb-alien:int sb-alien:system-area-pointer sb-alien:int)
(define-pty-libc %pty-kill     "kill"     sb-alien:int sb-alien:int sb-alien:int)
(define-pty-libc %pty-poll     "poll"     sb-alien:int sb-alien:system-area-pointer sb-alien:unsigned-long sb-alien:int)
(define-pty-libc %sigemptyset  "sigemptyset" sb-alien:int sb-alien:system-area-pointer)
(define-pty-libc %sigfillset   "sigfillset"  sb-alien:int sb-alien:system-area-pointer)
(define-pty-libc %fa-init      "posix_spawn_file_actions_init"    sb-alien:int sb-alien:system-area-pointer)
(define-pty-libc %fa-destroy   "posix_spawn_file_actions_destroy" sb-alien:int sb-alien:system-area-pointer)
(define-pty-libc %fa-addopen   "posix_spawn_file_actions_addopen" sb-alien:int sb-alien:system-area-pointer
                               sb-alien:int sb-alien:c-string sb-alien:int sb-alien:int)
(define-pty-libc %fa-adddup2   "posix_spawn_file_actions_adddup2" sb-alien:int sb-alien:system-area-pointer
                               sb-alien:int sb-alien:int)
(define-pty-libc %fa-addchdir  "posix_spawn_file_actions_addchdir_np" sb-alien:int sb-alien:system-area-pointer
                               sb-alien:c-string)
(define-pty-libc %fa-closefrom "posix_spawn_file_actions_addclosefrom_np" sb-alien:int sb-alien:system-area-pointer
                               sb-alien:int)
(define-pty-libc %attr-init    "posix_spawnattr_init"       sb-alien:int sb-alien:system-area-pointer)
(define-pty-libc %attr-destroy "posix_spawnattr_destroy"    sb-alien:int sb-alien:system-area-pointer)
(define-pty-libc %attr-flags   "posix_spawnattr_setflags"   sb-alien:int sb-alien:system-area-pointer sb-alien:short)
(define-pty-libc %attr-sigmask "posix_spawnattr_setsigmask" sb-alien:int sb-alien:system-area-pointer
                               sb-alien:system-area-pointer)
(define-pty-libc %attr-sigdef  "posix_spawnattr_setsigdefault" sb-alien:int sb-alien:system-area-pointer
                               sb-alien:system-area-pointer)
(define-pty-libc %posix-spawn  "posix_spawn" sb-alien:int sb-alien:system-area-pointer sb-alien:c-string
                               sb-alien:system-area-pointer sb-alien:system-area-pointer
                               sb-alien:system-area-pointer sb-alien:system-area-pointer)

;;; Constants, each checked against the build host's headers with a throwaway C
;;; program (sizeof / the macro's value) — x86-64 Linux, glibc 2.43.
(defconstant +pty-o-rdwr+     2)
(defconstant +pty-o-noctty+   #o400)
(defconstant +pty-o-nonblock+ #o4000)
(defconstant +pty-o-cloexec+  #o2000000)
(defconstant +tiocswinsz+     #x5414)   ; set the window size (and SIGWINCH the job)
(defconstant +tcgets+         #x5401)   ; tcgetattr is this ioctl
(defconstant +tcsets+         #x5402)   ; tcsetattr(TCSANOW) is this one
(defconstant +iutf8+          #x4000)   ; c_iflag: the line discipline knows UTF-8
(defconstant +wnohang+        1)
(defconstant +eagain+         11)
(defconstant +eintr+          4)
(defconstant +sighup+         1)
(defconstant +sigkill+        9)
(defconstant +spawn-setsigdef+  #x04)
(defconstant +spawn-setsigmask+ #x08)
(defconstant +spawn-setsid+     #x80)
(defconstant +spawn-fa-size+  80)       ; sizeof (posix_spawn_file_actions_t)
(defconstant +spawn-attr-size+ 336)     ; sizeof (posix_spawnattr_t)
(defconstant +sigset-size+    128)      ; sizeof (sigset_t)

(defstruct (pty-proc (:constructor %make-pty-proc))
  "A program running on a pseudo-terminal: its PID, our MASTER fd, the slave's name,
   and — once it has ended — how (STATUS is :running, :exited or :signaled; CODE the
   exit code or signal number)."
  pid master slave-name argv (status :running) code)

(defmacro with-foreign-bytes ((sap size) &body body)
  "Run BODY with SAP pointing at SIZE zeroed bytes of foreign memory, freed after."
  (let ((a (gensym "ALIEN")))
    `(let* ((,a (sb-alien:make-alien (sb-alien:unsigned 8) ,size))
            (,sap (sb-alien:alien-sap ,a)))
       (unwind-protect
            (progn (dotimes (i ,size) (setf (sb-sys:sap-ref-8 ,sap i) 0))
                   ,@body)
         (sb-alien:free-alien ,a)))))

(defun foreign-string-vector (strings)
  "A NULL-terminated char* array of STRINGS (UTF-8) in foreign memory — the shape of
   argv and envp. Returns (values SAP CLEANUP): call CLEANUP to free it all."
  (let* ((n (length strings))
         (vec (sb-alien:make-alien sb-alien:system-area-pointer (1+ n)))
         (sap (sb-alien:alien-sap vec))
         (owned '()))
    (loop for s in strings for i from 0 do
      (let ((cs (sb-alien:make-alien-string s :external-format :utf-8)))
        (push cs owned)
        (setf (sb-sys:sap-ref-sap sap (* 8 i)) (sb-alien:alien-sap cs))))
    (setf (sb-sys:sap-ref-sap sap (* 8 n)) (sb-sys:int-sap 0))
    (values sap (lambda () (mapc #'sb-alien:free-alien owned) (sb-alien:free-alien vec)))))

(defun pty-errno () (sb-alien:get-errno))

(defun pty-set-size (master cols rows &optional (xpixel 0) (ypixel 0))
  "Tell the kernel the terminal on MASTER is now COLS x ROWS cells (and XPIXEL x
   YPIXEL pixels). struct winsize is four unsigned shorts: rows, cols, x, y. The
   kernel stores it with the slave and, if it changed, sends SIGWINCH to the
   terminal's foreground process group — which is how vim learns to redraw. We never
   send that signal ourselves; this ioctl is the whole protocol."
  (with-foreign-bytes (ws 8)
    (setf (sb-sys:sap-ref-16 ws 0) rows (sb-sys:sap-ref-16 ws 2) cols
          (sb-sys:sap-ref-16 ws 4) (min #xFFFF xpixel) (sb-sys:sap-ref-16 ws 6) (min #xFFFF ypixel))
    (%pty-ioctl master +tiocswinsz+ ws)))

(defun pty-enable-utf8 (master)
  "Set IUTF8 in the slave's input flags: then the line discipline's own editing (a
   Backspace in `cat', in a password prompt) erases a whole UTF-8 character, not one
   byte of it. A termios ioctl on the MASTER acts on the slave — the kernel redirects
   it (tty_mode_ioctl's real_tty). struct termios is 60 bytes; c_iflag is the first
   32-bit field."
  (with-foreign-bytes (tio 64)
    (when (zerop (%pty-ioctl master +tcgets+ tio))
      (setf (sb-sys:sap-ref-32 tio 0) (logior (sb-sys:sap-ref-32 tio 0) +iutf8+))
      (%pty-ioctl master +tcsets+ tio))))

(defun pty-open ()
  "Open a new pseudo-terminal. Returns (values MASTER-FD SLAVE-NAME), or signals.
   The master is non-blocking (we multiplex it with poll(2)) and close-on-exec (so no
   child we spawn inherits OTHER terminals' masters)."
  (let ((m (%pty-open "/dev/ptmx" (logior +pty-o-rdwr+ +pty-o-noctty+ +pty-o-nonblock+
                                          +pty-o-cloexec+)
                      0)))
    (when (minusp m)
      (error "open /dev/ptmx: ~a (is devpts mounted on /dev/pts?)" (sb-int:strerror (pty-errno))))
    ;; grantpt: make the slave ours (devpts already does — a no-op on Linux, kept
    ;; because POSIX says to); unlockpt: allow it to be opened at all
    (when (or (minusp (%grantpt m)) (minusp (%unlockpt m)))
      (let ((e (pty-errno))) (%pty-close m) (error "unlockpt: ~a" (sb-int:strerror e))))
    (values m (%ptsname m))))

(defun pty-spawn (argv &key env cwd (cols 80) (rows 24))
  "Run ARGV (a list of strings; the first an absolute path) on a fresh
   pseudo-terminal, as the leader of a new session whose controlling terminal is the
   slave. ENV is the complete environment (a list of \"NAME=value\"), CWD the
   directory to start in. Returns a PTY-PROC, or signals an error saying why."
  (multiple-value-bind (master slave) (pty-open)
    (pty-set-size master cols rows)       ; before the child starts: it asks at once
    (pty-enable-utf8 master)
    (handler-bind ((error (lambda (c) (declare (ignore c)) (%pty-close master))))
      (with-foreign-bytes (fa +spawn-fa-size+)
        (with-foreign-bytes (attr +spawn-attr-size+)
          (with-foreign-bytes (sigs +sigset-size+)
            (with-foreign-bytes (pidbuf 8)
              (%fa-init fa)
              (%attr-init attr)
              (unwind-protect
                   (multiple-value-bind (argv-sap free-argv) (foreign-string-vector argv)
                     (multiple-value-bind (env-sap free-env) (foreign-string-vector env)
                       (unwind-protect
                            (progn
                              ;; the child's file descriptors, in order: the slave on
                              ;; 0 (opened AFTER setsid, so it becomes the controlling
                              ;; terminal), copied to 1 and 2, every other fd closed —
                              ;; the framebuffer, the evdev devices, sockets and other
                              ;; terminals' masters are none of the child's business
                              (%fa-addopen fa 0 slave +pty-o-rdwr+ 0)
                              (%fa-adddup2 fa 0 1)
                              (%fa-adddup2 fa 0 2)
                              (%fa-closefrom fa 3)
                              (when cwd (%fa-addchdir fa cwd))
                              ;; SBCL blocks and handles signals of its own; the child
                              ;; must start with an empty mask and every signal at its
                              ;; default, or ^C would not interrupt it
                              (%sigemptyset sigs) (%attr-sigmask attr sigs)
                              (%sigfillset sigs)  (%attr-sigdef attr sigs)
                              (%attr-flags attr (logior +spawn-setsid+ +spawn-setsigmask+
                                                        +spawn-setsigdef+))
                              (let ((rc (%posix-spawn pidbuf (first argv) fa attr argv-sap env-sap)))
                                (unless (zerop rc)
                                  (error "cannot run ~a: ~a" (first argv) (sb-int:strerror rc)))
                                (%make-pty-proc :pid (sb-sys:signed-sap-ref-32 pidbuf 0)
                                                :master master :slave-name slave :argv argv)))
                         (funcall free-env)
                         (funcall free-argv))))
                (%fa-destroy fa)
                (%attr-destroy attr)))))))))

(defun pty-read (proc buffer)
  "Read what the program has written into BUFFER (an octet vector). Returns the
   count, 0 when there is nothing right now, or :EOF when the terminal is finished —
   read(2) on a master whose slave every process has closed fails with EIO, which is
   how a terminal learns that the program (and everything it started) has gone."
  (let ((m (pty-proc-master proc)))
    (if (null m)
        :eof
        (let ((n (sb-sys:with-pinned-objects (buffer)
                   (%pty-read m (sb-sys:vector-sap buffer) (length buffer)))))
          (cond ((plusp n) n)
                ((and (minusp n) (member (pty-errno) (list +eagain+ +eintr+))) 0)
                (t :eof))))))

(defun pty-write (proc octets)
  "Write OCTETS (keys, a paste) to the program. A full pty buffer (a big paste into a
   program that is not reading) is waited out briefly with poll(2), then given up on
   rather than freezing the whole environment."
  (let ((m (pty-proc-master proc)) (off 0) (len (length octets)) (tries 0))
    (when m
      (sb-sys:with-pinned-objects (octets)
        (loop while (and (< off len) (< tries 50)) do
          (let ((n (%pty-write m (sb-sys:sap+ (sb-sys:vector-sap octets) off) (- len off))))
            (cond ((plusp n) (incf off n))
                  ((member (pty-errno) (list +eagain+ +eintr+))
                   (incf tries)
                   (with-foreign-bytes (pfd 8)        ; wait up to 20 ms for room (POLLOUT)
                     (setf (sb-sys:signed-sap-ref-32 pfd 0) m (sb-sys:sap-ref-16 pfd 4) 4)
                     (%pty-poll pfd 1 20)))
                  (t (return)))))))
    off))

(defun pty-reap (proc)
  "Ask, without waiting, whether the program has ended; record how. Returns its
   STATUS. The child is OURS (posix_spawn made it), so nobody else will reap it —
   SBCL's own SIGCHLD handler only waits for RUN-PROGRAM's children, by pid."
  (when (eq (pty-proc-status proc) :running)
    (with-foreign-bytes (st 4)
      (when (= (%waitpid (pty-proc-pid proc) st +wnohang+) (pty-proc-pid proc))
        (let ((s (sb-sys:sap-ref-32 st 0)))
          ;; the classic wait-status layout: low 7 bits = the signal (0 = exited
          ;; normally), next byte = the exit code
          (if (zerop (logand s #x7F))
              (setf (pty-proc-status proc) :exited (pty-proc-code proc) (ldb (byte 8 8) s))
              (setf (pty-proc-status proc) :signaled (pty-proc-code proc) (logand s #x7F)))))))
  (pty-proc-status proc))

(defvar *pty-orphans* '()
  "Programs whose terminal we closed while they still ran. They were sent SIGHUP and
   will die in a moment; until we wait for them they are zombies, so PTY-REAP-ORPHANS
   (called from the browser's loop) collects them.")

(defun pty-close (proc)
  "Close our end. If the program is still running this HANGS UP its terminal: the
   kernel sends SIGHUP to the session, which is what closing an xterm window does and
   why bash exits then. We send one SIGHUP of our own as well, to the leader directly,
   for a program that had already given its terminal away."
  (let ((m (pty-proc-master proc)))
    (when m
      (%pty-close m)
      (setf (pty-proc-master proc) nil)))
  (when (eq (pty-reap proc) :running)
    (%pty-kill (pty-proc-pid proc) +sighup+)
    (when (eq (pty-reap proc) :running) (pushnew proc *pty-orphans*)))
  (pty-proc-status proc))

(defun pty-reap-orphans ()
  "Wait for any hung-up program that has finished dying."
  (setf *pty-orphans* (remove-if-not (lambda (p) (eq (pty-reap p) :running)) *pty-orphans*)))

(defun pty-kill (proc &optional (signal +sigkill+))
  "Send SIGNAL (default SIGKILL: no negotiation) to the program's whole process
   group — the session leader's pid is also its group's id, hence the minus sign."
  (when (eq (pty-reap proc) :running)
    (%pty-kill (- (pty-proc-pid proc)) signal)))
