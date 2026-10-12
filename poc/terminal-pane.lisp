;;;; terminal-pane.lisp — the terminal as a pane in the browser: SUBJ-TERMINAL.
;;;;
;;;; The two halves meet here: poc/terminal.lisp turns bytes into a grid of cells,
;;;; initramfs/pty.lisp runs a program on a pseudo-terminal, and this file makes the
;;;; pair a SUBJECT like any other — opened in a trail, shown as a tab, closed with
;;;; SuperL-w — so a shell lives beside the Workspace and the file manager rather
;;;; than in a different world. What it adds is the furniture:
;;;;
;;;;  - DRAWING the grid with the browser's own font, cell size read from that font at
;;;;    draw time (so a font change re-flows the terminal: the new size goes to the
;;;;    program by TIOCSWINSZ, as when an xterm window is resized). Box-drawing and
;;;;    block characters, which our Latin-1 font lacks, are drawn as lines and rects.
;;;;  - KEYS: everything goes to the program, except the SuperL layer (tabs, the
;;;;    Spotter, leaving), which stays the environment's on every pane. Shift-PageUp/
;;;;    PageDown and the wheel scroll the history; Shift-Insert pastes.
;;;;  - THE MOUSE: press-and-drag selects (to PRIMARY and the clipboard history, as
;;;;    everywhere in the browser), middle-click pastes PRIMARY — unless the program
;;;;    asked for the mouse (vim's `mouse=a', htop), in which case it gets the clicks,
;;;;    and Shift held takes them back for selecting, as in xterm.
;;;;  - THE LOOP: TERMINAL-FDS gives fb-browser.lisp's poll(2) the masters to watch;
;;;;    TERMINAL-SERVICE drains whichever are ready; TERMINAL-PAINT-DIRTY repaints only
;;;;    the ROWS that changed and returns their rectangles, so a shell echoing one
;;;;    character sends one row to /dev/fb0, not the frame.
;;;;
;;;; CL-USER, after shell.lisp and file-manager.lisp. The tour: doc/terminal.org.

(in-package :cl-user)

;;; ===========================================================================
;;; The subject and its session
;;; ===========================================================================

(defstruct (subj-terminal (:constructor %subj-terminal))
  "A terminal: the program to run, and — once it has been opened — the live session.
   Creating one starts nothing; PRESENTING it does (OPEN-TERMINAL, the Spotter's
   \"terminal\" row). That makes this the one subject whose PRESENT has an effect,
   on purpose: a candidate in a finder must cost nothing until it is chosen."
  argv title cwd env session)

(defstruct (term-session (:constructor %make-term-session))
  "What a running terminal is: the engine (TERM), the program (PROC, a PTY-PROC, or
   NIL when it could not start), and the pane's own state."
  term proc
  (message nil)           ; why it is not running ("exited with code 0", an error)
  geometry                ; (x y cols rows cw ch) of the last full draw — for the
                          ;   dirty-row repaint and for mouse hit-tests
  selection               ; (r0 c0 r1 c1), absolute rows (see lol.terminal), or NIL
  (title "")              ; the last title shown, to notice OSC changes
  (bells 0))              ; the engine's bell count already acknowledged

(defvar *terminals* '()
  "Every SUBJ-TERMINAL whose program was started and not yet closed — the set whose
   masters the browser's loop polls.")

(defun pane-session (pane)
  (let ((s (pane-subject pane)))
    (and (subj-terminal-p s) (subj-terminal-session s))))

(defvar *terminal-chrome-dirty* nil
  "Set when a terminal key changed something OUTSIDE the grid's rows — the status
   line, the history banner, the pane itself. Otherwise a key typed into a terminal
   needs no frame at all: its echo comes back as pty output, and only the rows it
   touches are repainted (TERMINAL-KEY-WAS-CHEAP-P, used by fb-browser.lisp).")

(defun session-live-p (session)
  (and session (term-session-proc session)
       (pty-proc-master (term-session-proc session))))

;;; ===========================================================================
;;; What to run
;;; ===========================================================================
;;; SuperL-Return opens "a shell". Which one depends on what the machine has:
;;;
;;;  1. A shell IN THE DEFAULT CONTAINER (doc/containers.org) when that track's
;;;     CONTAINER-COMMAND-LINE is loaded and the host's root is mounted: the real
;;;     distro, with its own /usr, /etc and libraries — the intended answer.
;;;  2. Otherwise the host's bash, started through the HOST's dynamic loader with the
;;;     host's library directory. It runs, but our root is not the host's: a program
;;;     it execs is found through PATH under /mnt/host and loaded by OUR loader
;;;     (/lib64/ld-linux-x86-64.so.2, the build host's own copy) with
;;;     LD_LIBRARY_PATH pointing at the host's libraries. That works while the image's
;;;     glibc and the disk's are the same version — true right after a build, false
;;;     after the laptop upgrades glibc — and nothing that hard-codes /etc or /usr
;;;     paths finds them (no passwd entry: the prompt says "I have no name!"). A
;;;     stopgap, honestly labelled; the container is the real thing.
;;;  3. Otherwise (no host disk, as in a bare QEMU boot) a Lisp REPL: this same image
;;;     re-executed in its "repl" role (supervisor.lisp's INIT-TOPLEVEL) — a separate
;;;     process with its own heap, so nothing typed there can hurt the environment.

(defparameter *host-root* "/mnt/host")

(defun host-path (path) (concatenate 'string *host-root* path))

(defun host-loader ()
  "The host's own dynamic loader, or NIL. Looked up at its REAL path: /lib64/… on a
   merged-/usr Debian is a symlink, and an absolute symlink would resolve in OUR root."
  (find-if #'probe-file
           (mapcar #'host-path '("/usr/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2"
                                 "/lib/x86_64-linux-gnu/ld-linux-x86-64.so.2"
                                 "/lib64/ld-linux-x86-64.so.2"))))

(defparameter +host-lib-dirs+ '("/usr/lib/x86_64-linux-gnu" "/lib/x86_64-linux-gnu" "/usr/lib"))

(defun base-environment ()
  "What every program in a terminal starts with — the variables a login would set,
   plus TERM, which is how the program knows what this terminal understands."
  (let ((home (or (sb-ext:posix-getenv "HOME") "/"))
        (user (or (sb-ext:posix-getenv "USER") "user")))
    (list "TERM=xterm-256color" "COLORTERM=truecolor"
          (format nil "HOME=~a" home) (format nil "USER=~a" user) (format nil "LOGNAME=~a" user)
          "LANG=C.UTF-8" "SHELL=/bin/bash")))

(defun default-terminal-command ()
  "(values ARGV EXTRA-ENV LABEL) for SuperL-Return: see the comment above."
  (let ((host (probe-file (host-path "/usr/bin/bash"))))
    (cond
      ;; containers.lisp's own test: lol-ct shipped AND a host root with a /usr.
      ;; lol-ct builds the program's environment afresh (taking HOME, USER and TERM
      ;; from ours), so nothing else is passed; -C puts the shell where you were.
      ((and (fboundp 'container-command-line) (fboundp 'containers-available-p)
            (funcall 'containers-available-p))
       (values (funcall 'container-command-line (list "bash" "-l")
                        :cwd (and (fboundp 'container-cwd) (funcall 'container-cwd)))
               '()
               "shell in default"))
      ((and host (host-loader))
       (let ((libs (format nil "~{~a~^:~}" (mapcar #'host-path +host-lib-dirs+))))
         (values (list (host-loader) "--library-path" libs (host-path "/usr/bin/bash"))
                 (list (format nil "PATH=~{~a~^:~}"
                               (mapcar #'host-path '("/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin")))
                       (format nil "LD_LIBRARY_PATH=~a" libs)
                       (format nil "TERMINFO_DIRS=~a:~a" (host-path "/usr/share/terminfo")
                               (host-path "/lib/terminfo"))
                       "PS1=host-bash \\w \\$ ")
                 "host bash (limited)")))
      ((probe-file "/sbin/lisp")
       (values (list "/sbin/lisp" "repl") '("PATH=/sbin") "lisp repl"))
      ;; on the development host itself: its own shell
      (t (values (list (or (sb-ext:posix-getenv "SHELL") "/bin/bash"))
                 (list (format nil "PATH=~a" (or (sb-ext:posix-getenv "PATH") "/usr/bin:/bin")))
                 "shell")))))

;;; ===========================================================================
;;; Starting, presenting, closing
;;; ===========================================================================

(defun merge-env (base extra)
  "BASE with EXTRA's NAME=value pairs replacing any of the same NAME."
  (let ((names (mapcar (lambda (e) (subseq e 0 (or (position #\= e) (length e)))) extra)))
    (append (remove-if (lambda (e) (member (subseq e 0 (or (position #\= e) (length e))) names
                                           :test #'string=))
                       base)
            extra)))

(defun terminal-start (subject &key (cols 80) (rows 24))
  "Start SUBJECT's program on a fresh pty. A failure is not an error here: the
   session records the reason and the pane SHOWS it, which is more use than a
   debugger over a missing binary."
  (let* ((term (lol.terminal:make-term cols rows :scrollback 5000))
         (session (%make-term-session :term term)))
    (setf (subj-terminal-session subject) session)
    (handler-case
        (let ((proc (pty-spawn (subj-terminal-argv subject)
                               :env (merge-env (base-environment) (subj-terminal-env subject))
                               :cwd (subj-terminal-cwd subject)
                               :cols cols :rows rows)))
          (setf (term-session-proc session) proc)
          (pushnew subject *terminals*))
      (error (e)
        (setf (term-session-message session) (format nil "could not start: ~a" e))))
    session))

(defun terminal-view-title (subject)
  (let* ((s (subj-terminal-session subject))
         (osc (and s (lol.terminal:term-title (term-session-term s)))))
    (format nil "terminal ~a"
            (cond ((and osc (plusp (length osc))) osc)
                  ((subj-terminal-title subject))
                  (t (file-namestring (car (last (subj-terminal-argv subject)))))))))

(defmethod present ((s subj-terminal))
  (unless (subj-terminal-session s) (terminal-start s))
  (make-view :title (terminal-view-title s) :kind :terminal :data s :subject s))

(defmethod present-preview ((s subj-terminal))
  ;; a LOOK must not start a program (a recent in the Spotter, say)
  (make-view :title (terminal-view-title s) :kind :list :subject s
             :items (list (make-item :label (if (subj-terminal-session s)
                                                "a terminal — open it to use it"
                                                "a terminal, not started")))))

(defmethod subject-hints ((s subj-terminal) pane)
  (declare (ignore pane))
  (if (session-live-p (subj-terminal-session s))
      '(("keys" . "to the program") ("S-PgUp/wheel" . "history") ("S-Ins" . "paste")
        ("drag" . "copy") ("SuperL-w" . "close"))
      '(("ret / q" . "close") ("r" . "run again"))))

(defmethod commands-for ((s subj-terminal))
  (list (make-command :label "Paste the clipboard" :surfaces '(:menu) :key "S-Insert"
                      :action (lambda () (terminal-paste s (lol.clipboard:clip-yank))))
        (make-command :label "Copy the selection" :surfaces '(:menu) :key "C-Insert"
                      :action (lambda () (terminal-copy-selection s)))
        (make-command :label "Interrupt (send ^C)" :surfaces '(:menu)
                      :action (lambda () (terminal-send s (string (code-char 3)))))
        (make-command :label "Kill the program" :surfaces '(:menu :halo)
                      :action (lambda () (let ((ss (subj-terminal-session s)))
                                           (when (and ss (term-session-proc ss))
                                             (pty-kill (term-session-proc ss))))))
        (make-command :label "Run it again" :surfaces '(:menu)
                      :action (lambda () (terminal-restart s)))))

(defun terminal-close (subject)
  "Close SUBJECT's terminal: hang up the program (SIGHUP, as closing a window does)."
  (let ((s (subj-terminal-session subject)))
    (when (and s (term-session-proc s))
      (pty-close (term-session-proc s))))
  (setf *terminals* (remove subject *terminals*)))

(defun terminal-restart (subject)
  "Run SUBJECT's program again in the same pane, at the same size."
  (let* ((old (subj-terminal-session subject))
         (g (and old (term-session-geometry old))))
    (terminal-close subject)
    (terminal-start subject :cols (if g (third g) 80) :rows (if g (fourth g) 24))))

(defun open-terminal (argv &key title cwd env)
  "Open a terminal pane running ARGV (a list of strings; NIL = the default shell, see
   DEFAULT-TERMINAL-COMMAND) in a NEW tab, and return its subject. ENV is a list of
   \"NAME=value\" added to the base environment; CWD the starting directory."
  (multiple-value-bind (dargv denv label) (if argv (values argv nil nil) (default-terminal-command))
    (let ((s (%subj-terminal :argv dargv :title (or title label) :cwd cwd
                             :env (append denv env))))
      (new-trail s)
      s)))

(defun open-default-terminal ()
  "SuperL-Return: a shell, as good a one as this machine offers."
  (open-terminal nil :cwd (let ((h (sb-ext:posix-getenv "HOME")))
                            (and h (probe-file h) h))))

(defun terminal-sweep ()
  "Hang up every terminal no longer shown in any trail — the tab was closed, or the
   pane popped. One sweep after each input event covers every way a pane can go,
   without the shell having to tell us about each."
  (let ((shown '()))
    (dolist (trail *trails*)
      (dolist (p trail) (when (subj-terminal-p (pane-subject p)) (push (pane-subject p) shown))))
    (dolist (s *terminals*)
      (unless (member s shown) (terminal-close s))))
  (pty-reap-orphans))

;;; The Spotter offers "terminal" — a fresh subject each time it opens, started only
;;; if chosen.
(defun terminal-spotter-candidates ()
  (list (list :label "terminal" :detail "surface · a shell (SuperL-Return)" :group "surfaces"
              :kind :restart
              :subject (multiple-value-bind (argv env label) (default-terminal-command)
                         (%subj-terminal :argv argv :env env :title label
                                         :cwd (let ((h (sb-ext:posix-getenv "HOME")))
                                                (and h (probe-file h) h)))))))

(pushnew 'terminal-spotter-candidates *spotter-sources*)

;;; ===========================================================================
;;; The program's output: the poll loop's half
;;; ===========================================================================

(defvar *term-read-buffer* (make-array 65536 :element-type '(unsigned-byte 8)))

(defparameter +term-read-budget+ (* 512 1024)
  "Most bytes one service pass takes from one terminal. `cat' of a huge file would
   otherwise keep the loop parsing for as long as the file lasts, and the keyboard —
   ^C included — would never get a turn.")

(defun terminal-fds ()
  "The master fds the browser's poll(2) should watch."
  (loop for s in *terminals*
        for ss = (subj-terminal-session s)
        when (session-live-p ss) collect (pty-proc-master (term-session-proc ss))))

(defun terminal-send (subject string)
  "Send STRING (keys, a paste) to SUBJECT's program, and return the view to the
   present — typing while scrolled back means you want to see what you type."
  (let ((ss (subj-terminal-session subject)))
    (when (session-live-p ss)
      (when (lol.terminal:term-scroll-view (term-session-term ss)
                                           (- (lol.terminal:term-view-offset (term-session-term ss))))
        (setf *terminal-chrome-dirty* t))
      (pty-write (term-session-proc ss) (lol.terminal:utf8-encode string)))))

(defun service-one (subject)
  "Drain SUBJECT's master into its engine. Returns :ENDED when the program finished,
   :TITLE when its title changed, T when only the grid did, NIL for nothing."
  (let* ((ss (subj-terminal-session subject))
         (proc (term-session-proc ss))
         (term (term-session-term ss))
         (buf *term-read-buffer*)
         (taken 0) (result nil))
    (loop
      (let ((n (pty-read proc buf)))
        (cond
          ((eq n :eof)
           ;; EIO comes when the last slave fd closes, which is a moment BEFORE the
           ;; process is a zombie we can wait for — give it up to 200 ms, so the pane
           ;; can say "exited with code 0" rather than guess
           (loop repeat 20 while (eq (pty-reap proc) :running) do (sleep 0.01))
           (pty-close proc)
           (setf *terminals* (remove subject *terminals*))
           (setf (term-session-message ss)
                 (case (pty-proc-status proc)
                   (:exited (format nil "exited with code ~d" (pty-proc-code proc)))
                   (:signaled (format nil "killed by signal ~d" (pty-proc-code proc)))
                   (:unknown "ended (its exit status was reaped elsewhere)")
                   (t "the terminal hung up")))
           (return-from service-one :ended))
          ((zerop n) (return))
          (t (lol.terminal:term-feed term buf :end n)
             (setf result t)
             (incf taken n)
             (when (> taken +term-read-budget+) (return))))))
    ;; answers the program asked for (cursor position, device attributes …)
    (let ((reply (lol.terminal:term-take-output term)))
      (when reply (pty-write proc reply)))
    (let ((title (lol.terminal:term-title term)))
      (unless (string= title (term-session-title ss))
        (setf (term-session-title ss) title result :title)))
    result))

(defun terminal-service (ready-fds)
  "Service every terminal whose master is in READY-FDS. Returns :FULL when the frame
   must be redrawn (a program ended, a title changed — the tab shows it), :ROWS when
   only terminal rows changed, NIL when nothing did."
  (let ((need nil))
    (dolist (s (copy-list *terminals*))
      (let ((ss (subj-terminal-session s)))
        (when (and (session-live-p ss)
                   (member (pty-proc-master (term-session-proc ss)) ready-fds))
          (case (service-one s)
            ((:ended :title)
             (setf need :full)
             ;; the pane header and the tab show the program's title
             (dolist (trail *trails*)
               (dolist (p trail)
                 (when (eq (pane-subject p) s)
                   (setf (view-title (pane-view p)) (terminal-view-title s))))))
            ((nil))
            (t (unless need (setf need :rows)))))))
    need))

;;; ===========================================================================
;;; Drawing
;;; ===========================================================================

(defparameter *term-fg* !#D6D3C8 "The default foreground (the browser's text colour).")
(defparameter *term-bg* !#15171C "The default background: a shade under the panel, so
   the grid reads as a window into somewhere else.")
(defparameter +term-pad+ 4 "Pixels between the pane's edge and the first cell.")

(defun mix (a b f)
  "Colour A moved fraction F (0..1) towards B — for faint text and shade blocks."
  (flet ((ch (c s) (ldb (byte 8 s) c)))
    (logior (ash (round (+ (* (- 1 f) (ch a 16)) (* f (ch b 16)))) 16)
            (ash (round (+ (* (- 1 f) (ch a 8)) (* f (ch b 8)))) 8)
            (round (+ (* (- 1 f) (ch a 0)) (* f (ch b 0)))))))

(defun cell-colors (attr)
  "(values FG BG) as #xRRGGBB for a cell's ATTR: bold brightens the first eight
   colours (xterm's boldColors), inverse swaps, faint and invisible fade."
  (let* ((flags (lol.terminal:attr-flags attr))
         (fgc (lol.terminal:attr-fg attr))
         (fgc (if (and (logtest flags lol.terminal:+bold+) (<= 1 fgc 8)) (+ fgc 8) fgc))
         (fg (lol.terminal:color-rgb fgc *term-fg*))
         (bg (lol.terminal:color-rgb (lol.terminal:attr-bg attr) *term-bg*)))
    (when (logtest flags lol.terminal:+inverse+) (rotatef fg bg))
    (when (logtest flags lol.terminal:+faint+) (setf fg (mix fg bg 0.45)))
    (when (logtest flags lol.terminal:+invisible+) (setf fg bg))
    (values fg bg)))

;;; Box drawing (U+2500-257F) and blocks (U+2580-259F). Our font is Latin-1, so these
;;; would come out as "?" — and they are exactly what tmux, mc, htop's meters,
;;; `tree' and ncurses' line-drawing (via DEC graphics, see terminal.lisp) send. Each
;;; is drawn from its ARMS: which of up/down/left/right a line leaves the centre by.
;;; Heavy and double lines are drawn single; the shape is what carries the meaning.

(defun box-arms (code)
  "A list of :up :down :left :right for box-drawing CODE, or NIL."
  (flet ((in (lo hi) (<= lo code hi)))
    (cond ((member code '(#x2500 #x2501 #x2504 #x2505 #x2508 #x2509 #x254C #x254D #x2550 #x257C #x257E))
           '(:left :right))
          ((member code '(#x2502 #x2503 #x2506 #x2507 #x250A #x250B #x254E #x254F #x2551 #x257D #x257F))
           '(:up :down))
          ((or (in #x250C #x250F) (in #x2552 #x2554) (= code #x256D)) '(:down :right))
          ((or (in #x2510 #x2513) (in #x2555 #x2557) (= code #x256E)) '(:down :left))
          ((or (in #x2514 #x2517) (in #x2558 #x255A) (= code #x2570)) '(:up :right))
          ((or (in #x2518 #x251B) (in #x255B #x255D) (= code #x256F)) '(:up :left))
          ((or (in #x251C #x2523) (in #x255E #x2560)) '(:up :down :right))
          ((or (in #x2524 #x252B) (in #x2561 #x2563)) '(:up :down :left))
          ((or (in #x252C #x2533) (in #x2564 #x2566)) '(:down :left :right))
          ((or (in #x2534 #x253B) (in #x2567 #x2569)) '(:up :left :right))
          ((or (in #x253C #x254B) (in #x256A #x256C)) '(:up :down :left :right))
          ((member code '(#x2574 #x2578)) '(:left))
          ((member code '(#x2575 #x2579)) '(:up))
          ((member code '(#x2576 #x257A)) '(:right))
          ((member code '(#x2577 #x257B)) '(:down)))))

(defun draw-special-char (canvas code x y cw ch fg bg)
  "Draw box-drawing / block CODE in the cell at X,Y. Returns T when it was one."
  (let ((arms (box-arms code))
        (mx (+ x (floor cw 2))) (my (+ y (floor ch 2))))
    (cond
      (arms
       (dolist (a arms)
         (ecase a
           (:left  (lol.canvas:fill-rect canvas x my (1+ (- mx x)) 1 fg))
           (:right (lol.canvas:fill-rect canvas mx my (- (+ x cw) mx) 1 fg))
           (:up    (lol.canvas:fill-rect canvas mx y 1 (1+ (- my y)) fg))
           (:down  (lol.canvas:fill-rect canvas mx my 1 (- (+ y ch) my) fg))))
       t)
      ((<= #x2580 code #x259F)
       (flet ((r (fx fy fw fh &optional (c fg))      ; a rect in eighths of the cell
                (let ((x0 (+ x (floor (* fx cw) 8))) (y0 (+ y (floor (* fy ch) 8)))
                      (x1 (+ x (floor (* (+ fx fw) cw) 8))) (y1 (+ y (floor (* (+ fy fh) ch) 8))))
                  (lol.canvas:fill-rect canvas x0 y0 (max 1 (- x1 x0)) (max 1 (- y1 y0)) c))))
         (cond
           ((= code #x2580) (r 0 0 8 4))                                   ; upper half
           ((<= #x2581 code #x2588) (let ((k (- code #x2580))) (r 0 (- 8 k) 8 k))) ; lower k/8
           ((<= #x2589 code #x258F) (r 0 0 (- 8 (- code #x2588)) 8))      ; left (8-k)/8
           ((= code #x2590) (r 4 0 4 8))                                   ; right half
           ((<= #x2591 code #x2593)                                        ; shades
            (r 0 0 8 8 (mix bg fg (* 0.25 (- code #x2590)))))
           ((= code #x2594) (r 0 0 8 1))
           ((= code #x2595) (r 7 0 1 8))
           (t (let ((q (aref #(#b0010 #b0001 #b1000 #b1011 #b1001 #b1110 #b1101 #b0100
                               #b0110 #b0111)
                             (- code #x2596))))     ; bits: UL UR LL LR
                (when (logbitp 3 q) (r 0 0 4 4)) (when (logbitp 2 q) (r 4 0 4 4))
                (when (logbitp 1 q) (r 0 4 4 4)) (when (logbitp 0 q) (r 4 4 4 4))))))
       t))))

(defun selected-cell-p (sel r c)
  "Is absolute cell R,C inside the selection SEL (r0 c0 r1 c1, any order)?"
  (when sel
    (destructuring-bind (r0 c0 r1 c1) sel
      (when (or (> r0 r1) (and (= r0 r1) (> c0 c1))) (rotatef r0 r1) (rotatef c0 c1))
      (and (<= r0 r r1)
           (or (/= r r0) (>= c c0))
           (or (/= r r1) (<= c c1))))))

(defun draw-term-row (canvas session vy &optional focused)
  "Paint view row VY of SESSION's grid, background first, then the text in runs of
   one colour (so a row costs a few DRAW-STRINGs, not one per cell), then the
   decorations and the cursor."
  (destructuring-bind (gx gy cols rows cw ch) (term-session-geometry session)
    (declare (ignore rows))
    (let* ((term (term-session-term session))
           (line (lol.terminal:term-view-line term vy))
           (chars (lol.terminal:tline-chars line))
           (attrs (lol.terminal:tline-attrs line))
           (n (min cols (length chars)))
           (y (+ gy (* vy ch)))
           (abs-row (if (lol.terminal:term-alt-screen-p term)
                        (+ (lol.terminal:term-scrollback-count term) vy)
                        (+ (lol.terminal:term-view-top term) vy)))
           (sel (term-session-selection session))
           (font *bfont*))
      (lol.canvas:fill-rect canvas gx y (* cols cw) ch *term-bg*)
      ;; backgrounds and specials, cell by cell
      (dotimes (c n)
        (multiple-value-bind (fg bg) (cell-colors (aref attrs c))
          (when (selected-cell-p sel abs-row c) (setf bg *ui-sel* fg lol.widgets:*ui-lit*))
          (unless (= bg *term-bg*)
            (lol.canvas:fill-rect canvas (+ gx (* c cw)) y cw ch bg))
          (let ((code (char-code (schar chars c))))
            (when (>= code #x2500)
              (draw-special-char canvas code (+ gx (* c cw)) y cw ch fg bg)))))
      ;; text, in runs of equal foreground
      (let ((c 0))
        (loop while (< c n) do
          (let* ((start c)
                 (fg (multiple-value-bind (f b) (cell-colors (aref attrs c))
                       (declare (ignore b))
                       (if (selected-cell-p sel abs-row c) lol.widgets:*ui-lit* f)))
                 (run (make-string-output-stream)))
            (loop while (and (< c n)
                             (= fg (multiple-value-bind (f b) (cell-colors (aref attrs c))
                                     (declare (ignore b))
                                     (if (selected-cell-p sel abs-row c) lol.widgets:*ui-lit* f))))
                  do (let* ((chr (schar chars c)) (code (char-code chr)))
                       (write-char (cond ((eql chr lol.terminal:+wide-tail+) #\Space)
                                         ((<= #x2500 code #x259F) #\Space) ; drawn above
                                         (t chr))
                                   run))
                     (incf c))
            (let ((s (get-output-stream-string run)))
              (unless (every (lambda (x) (char= x #\Space)) s)
                (lol.canvas:draw-string canvas font s (+ gx (* start cw)) y fg))))))
      ;; underline and strike-through
      (dotimes (c n)
        (let ((flags (lol.terminal:attr-flags (aref attrs c))))
          (when (logtest flags (logior lol.terminal:+underline+ lol.terminal:+strike+))
            (let ((fg (cell-colors (aref attrs c))))
              (when (logtest flags lol.terminal:+underline+)
                (lol.canvas:fill-rect canvas (+ gx (* c cw)) (+ y ch -2) cw 1 fg))
              (when (logtest flags lol.terminal:+strike+)
                (lol.canvas:fill-rect canvas (+ gx (* c cw)) (+ y (floor ch 2)) cw 1 fg))))))
      ;; the cursor — only on the live view, only when the program shows it
      (when (and (= vy (lol.terminal:term-cy term))
                 (zerop (lol.terminal:term-view-offset term))
                 (lol.terminal:term-cursor-visible-p term)
                 (session-live-p session))
        (let ((cx (+ gx (* (lol.terminal:term-cx term) cw)))
              (style (lol.terminal:term-cursor-style term)))
          (cond
            ((not focused) (lol.canvas:draw-rect canvas cx y cw ch *ui-accent*))
            ((member style '(3 4)) (lol.canvas:fill-rect canvas cx (+ y ch -2) cw 2 *ui-accent*))
            ((member style '(5 6)) (lol.canvas:fill-rect canvas cx y 2 ch *ui-accent*))
            (t (lol.canvas:fill-rect canvas cx y cw ch *ui-accent*)
               (let ((chr (schar chars (min (1- (length chars)) (lol.terminal:term-cx term)))))
                 (unless (char= chr #\Space)
                   (lol.canvas:draw-string canvas font (string chr) cx y *term-bg*))))))))))

(defun terminal-fit (session w h)
  "Size SESSION's grid to the W x H pixels it is given, in the CURRENT font's cells.
   When the cell count changes — the window, or the font — the engine is resized and
   the program told (TIOCSWINSZ, then SIGWINCH from the kernel)."
  (let* ((cw (max 1 (lol.canvas:font-cw *bfont*))) (ch (max 1 (lol.canvas:font-ch *bfont*)))
         (cols (max 10 (floor (- w (* 2 +term-pad+)) cw)))
         (rows (max 3 (floor (- h +term-pad+) ch)))
         (term (term-session-term session)))
    (when (lol.terminal:term-resize term cols rows)
      (setf (term-session-selection session) nil)
      (when (session-live-p session)
        (pty-set-size (pty-proc-master (term-session-proc session)) cols rows
                      (* cols cw) (* rows ch))))
    (values cols rows cw ch)))

(defun draw-terminal-body (canvas pane x y w h)
  "Render a :TERMINAL view into the rect: the grid, and the program's fate when it
   has ended. Records the geometry for the dirty-row repaint and the mouse."
  (let* ((subject (pane-subject pane))
         (session (or (subj-terminal-session subject) (terminal-start subject))))
    (lol.canvas:fill-rect canvas x y w h *term-bg*)
    (multiple-value-bind (cols rows cw ch) (terminal-fit session w h)
      (setf (term-session-geometry session)
            (list (+ x +term-pad+) (+ y (floor +term-pad+ 2)) cols rows cw ch))
      (let ((focused (and (eq pane (current-pane)) (not *minibuffer*))))
        (dotimes (vy rows) (draw-term-row canvas session vy focused)))
      (lol.terminal:term-clear-dirty (term-session-term session))
      ;; scrolled back: say so, and how far
      (let ((off (lol.terminal:term-view-offset (term-session-term session))))
        (when (plusp off)
          (let ((s (format nil " history: ~d line~:p up - type or S-PgDn to return " off)))
            (lol.canvas:fill-rect canvas (- (+ x w) (lol.canvas:string-px *bfont* s) 6) (+ y 2)
                                  (+ 4 (lol.canvas:string-px *bfont* s)) (+ ch 2) *ui-amber*)
            (lol.canvas:draw-string canvas *bfont* s (- (+ x w) (lol.canvas:string-px *bfont* s) 4)
                                    (+ y 3) *ui-bg*))))
      ;; a program that ended keeps its last screen, with its fate underneath
      (let ((msg (term-session-message session)))
        (when msg
          (let ((s (format nil " [~a]  ret/q close  r run again " msg)))
            (lol.canvas:fill-rect canvas x (- (+ y h) ch 4) w (+ ch 4) *ui-hi*)
            (lol.canvas:draw-string canvas *bfont* s (+ x 6) (- (+ y h) ch 2) *ui-amber*)))))))

(defun terminal-paint-dirty (canvas)
  "If the current pane is a terminal, repaint just the rows its program changed since
   the last frame. Returns the list of rectangles (x y w h) to send to the screen, or
   NIL when there is nothing to do this way (then the caller redraws the frame)."
  (let* ((pane (ignore-errors (current-pane)))
         (session (and (terminal-pane-p pane) (pane-session pane))))
    (when (and session (term-session-geometry session) (null (pane-side pane)))
      (let ((term (term-session-term session)))
        (destructuring-bind (gx gy cols rows cw ch) (term-session-geometry session)
          ;; the grid's size no longer matches the pane (should not happen between
          ;; full frames): refuse, and let the caller redraw everything
          (unless (and (= cols (lol.terminal:term-cols term)) (= rows (lol.terminal:term-rows term)))
            (return-from terminal-paint-dirty nil))
          (let ((rects '()) (dirty (lol.terminal:term-dirty term)) (focused (not *minibuffer*)))
            (dotimes (vy (min rows (length dirty)))
              (when (= 1 (sbit dirty vy))
                (draw-term-row canvas session vy focused)
                (push (list gx (+ gy (* vy ch)) (* cols cw) ch) rects)))
            (lol.terminal:term-clear-dirty term)
            (or rects (list (list gx gy 1 1)))))))))

;;; ===========================================================================
;;; Keys
;;; ===========================================================================

(defun terminal-paste (subject text)
  "Paste TEXT into the program — bracketed when it asked (mode 2004)."
  (let ((ss (subj-terminal-session subject)))
    (cond ((null text) (setf *browser-status* "paste: the clipboard is empty"))
          ((session-live-p ss)
           (terminal-send subject (lol.terminal:encode-paste (term-session-term ss) text))))))

(defun terminal-copy-selection (subject)
  (let* ((ss (subj-terminal-session subject))
         (sel (and ss (term-session-selection ss))))
    (if sel
        (copy-to-clipboard (apply #'lol.terminal:term-text-range (term-session-term ss) sel))
        (setf *browser-status* "copy: no selection (drag across the text)"))))

(defun terminal-key-was-cheap-p (pane-before state)
  "True when the key just handled went to the program in PANE-BEFORE and nothing but
   grid rows can have changed — so the caller can skip redrawing the frame."
  (and (eq pane-before (ignore-errors (current-pane)))
       (terminal-pane-p pane-before)
       (not (lol.canvas:super-p state))
       (session-live-p (pane-session pane-before))
       (not (shiftf *terminal-chrome-dirty* nil))))

(defun terminal-key (pane key state)
  "A key for terminal PANE. Returns T when consumed. The SuperL layer never comes
   here (browser-key routes it first), so the environment keeps tabs, the Spotter and
   leaving; everything else is the program's."
  (let* ((subject (pane-subject pane))
         (ss (subj-terminal-session subject))
         (term (and ss (term-session-term ss)))
         (shift (lol.canvas:shift-p state)) (ctrl (lol.canvas:ctrl-p state)))
    (when *browser-status* (setf *browser-status* nil *terminal-chrome-dirty* t))
    (cond
      ((null ss) nil)
      ;; the program has ended: a few keys of our own
      ((not (session-live-p ss))
       (setf *terminal-chrome-dirty* t)
       (cond ((member key '(:return #\q #\Q :escape))
              (if (cdr (current-trail)) (trail-pop) (close-trail))
              (terminal-sweep))
             ((member key '(#\r #\R)) (terminal-restart subject)))
       t)
      ;; history, from the keyboard
      ((and shift (member key '(:page-up :page-down)))
       (setf *terminal-chrome-dirty* t)
       (lol.terminal:term-scroll-view term (* (if (eq key :page-up) 1 -1)
                                              (max 1 (1- (lol.terminal:term-rows term)))))
       t)
      ;; the clipboard, the PC way (Emacs's C-y / M-w belong to the program here)
      ((and shift (eq key :insert)) (terminal-paste subject (lol.clipboard:clip-yank)) t)
      ((and ctrl (eq key :insert)) (terminal-copy-selection subject) t)
      (t
       (let ((bytes (lol.terminal:encode-key term key state)))
         (when bytes
           (when (term-session-selection ss)        ; typing drops the highlight
             (setf (term-session-selection ss) nil)
             (lol.terminal:term-mark-all-dirty term))
           (terminal-send subject bytes)))
       t))))

;;; ===========================================================================
;;; The mouse
;;; ===========================================================================

;;; *TERM-DRAG* (declared in shell.lisp) is, while the button is held after a press
;;; in a terminal, either (SUBJECT . :PROGRAM) — the program took the press — or
;;; (SUBJECT ROW COL X Y), where a selection began.

(defvar *term-pointer* '(0 . 0)
  "Where the pointer was last seen over a terminal (for a release or wheel report,
   which the shell hands us without a position).")

(defun terminal-cell-at (session x y)
  "(values COL VIEW-ROW) of the cell under pixel X,Y (clamped to the grid)."
  (destructuring-bind (gx gy cols rows cw ch) (term-session-geometry session)
    (values (max 0 (min (1- cols) (floor (- x gx) cw)))
            (max 0 (min (1- rows) (floor (- y gy) ch))))))

(defun view-row->abs (term vy)
  (if (lol.terminal:term-alt-screen-p term)
      (+ (lol.terminal:term-scrollback-count term) vy)
      (+ (lol.terminal:term-view-top term) vy)))

(defun program-wants-mouse-p (ss state)
  "Does the program take the mouse? Only when it asked, and Shift is not held — Shift
   is xterm's way to take the mouse back for selecting."
  (and (session-live-p ss)
       (lol.terminal:term-mouse-mode (term-session-term ss))
       (not (lol.canvas:shift-p state))))

(defun report-mouse (subject event button x y state)
  (let ((ss (subj-terminal-session subject)))
    (multiple-value-bind (col row) (terminal-cell-at ss x y)
      (let ((s (lol.terminal:encode-mouse (term-session-term ss) event button col row state)))
        (when s (pty-write (term-session-proc ss) (lol.terminal:utf8-encode s)))))))

(defun mouse-state ()
  "The modifier bits held right now, from the keyboard's evdev (fb-browser.lisp)."
  (logior (if (and (boundp '*shift-down*) (symbol-value '*shift-down*)) 1 0)
          (if (and (boundp '*ctrl-down*) (symbol-value '*ctrl-down*)) 4 0)
          (if (and (boundp '*alt-down*) (symbol-value '*alt-down*)) 8 0)))

(defun terminal-press (pane x y)
  "A left press in terminal PANE at X,Y: to the program if it wants the mouse, else
   the start of a selection."
  (let* ((subject (pane-subject pane)) (ss (subj-terminal-session subject)))
    (setf *term-pointer* (cons x y))
    (when (and ss (term-session-geometry ss))
      (if (program-wants-mouse-p ss (mouse-state))
          (progn (report-mouse subject :press 0 x y (mouse-state))
                 (setf *term-drag* (cons subject :program)))
          (multiple-value-bind (col vy) (terminal-cell-at ss x y)
            (let ((r (view-row->abs (term-session-term ss) vy)))
              (setf (term-session-selection ss) nil
                    *term-drag* (list subject r col x y))
              (lol.terminal:term-mark-all-dirty (term-session-term ss)))))
      t)))

(defun terminal-drag (x y)
  "The left button is held and the pointer moved: extend the selection (or report
   the motion to a program that asked for it, mode 1002/1003). T when the frame changed."
  (when *term-drag*
    (setf *term-pointer* (cons x y))
    (let* ((subject (first *term-drag*)) (ss (subj-terminal-session subject)))
      (cond
        ((null (term-session-geometry ss)) nil)
        ((eq (cdr *term-drag*) :program)
         (report-mouse subject :motion 0 x y (mouse-state)) nil)
        (t (destructuring-bind (r0 c0 px py) (cdr *term-drag*)
             (when (or (> (abs (- x px)) 3) (> (abs (- y py)) 3) (term-session-selection ss))
               (multiple-value-bind (col vy) (terminal-cell-at ss x y)
                 (setf (term-session-selection ss)
                       (list r0 c0 (view-row->abs (term-session-term ss) vy) col))
                 (lol.terminal:term-mark-all-dirty (term-session-term ss))
                 t))))))))

(defun terminal-release ()
  "The button came up: a finished drag becomes PRIMARY and joins the history — once,
   now — exactly as a drag in a text view does (shell.lisp's BROWSER-DRAG-END)."
  (when *term-drag*
    (let* ((subject (first *term-drag*)) (ss (subj-terminal-session subject)))
      (if (eq (cdr *term-drag*) :program)
          (report-mouse subject :release 0 (car *term-pointer*) (cdr *term-pointer*) (mouse-state))
          (let ((sel (term-session-selection ss)))
            (when sel
              (let ((text (apply #'lol.terminal:term-text-range (term-session-term ss) sel)))
                (when (plusp (length text))
                  (lol.clipboard:clip-select text)
                  (clip-status "selected" text)))))))
    (setf *term-drag* nil)))

(defun terminal-wheel (pane lines)
  "The wheel over a terminal: history on the primary screen; on the alternate screen,
   wheel reports to a program that takes the mouse, else arrow keys (xterm's
   alternateScroll — so the wheel scrolls less and man pages)."
  (let* ((subject (pane-subject pane)) (ss (subj-terminal-session subject))
         (term (and ss (term-session-term ss))))
    (when term
      (cond
        ((and (session-live-p ss) (lol.terminal:term-mouse-mode term))
         (dotimes (i (max 1 (floor (abs lines) +wheel-lines+)))
           ;; the framebuffer backend's pointer (fb-browser.lisp), when it has one
           (let ((x (if (boundp '*cursor-x*) (symbol-value '*cursor-x*) (car *term-pointer*)))
                 (y (if (boundp '*cursor-y*) (symbol-value '*cursor-y*) (cdr *term-pointer*))))
             (report-mouse subject :press (if (minusp lines) :wheel-up :wheel-down) x y 0))))
        ((lol.terminal:term-alt-screen-p term)
         (when (session-live-p ss)
           (terminal-send subject (with-output-to-string (o)
                                    (dotimes (i (abs lines))
                                      (write-string (lol.terminal:encode-key
                                                     term (if (minusp lines) :up :down) 0)
                                                    o))))))
        (t (lol.terminal:term-scroll-view term (- lines))))
      t)))

(defun terminal-middle (pane)
  "Middle click: paste PRIMARY (what the mouse last selected) into the program."
  (terminal-paste (pane-subject pane) (primary-selection))
  t)
