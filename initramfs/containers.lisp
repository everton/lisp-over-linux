;;;; containers.lisp — the laptop's own Debian as a container, from the Lisp side.
;;;;
;;;; doc/containers.org is the design; initramfs/lol-ct.c does the namespace work.
;;;; This file is everything the environment needs on top of that helper:
;;;;
;;;;   - the PUBLIC API other parts of the system build on (the terminal pane, the
;;;;     web browser) — CONTAINER-COMMAND-LINE turns "run this" into the lol-ct
;;;;     invocation that runs it inside, and RUN-IN-CONTAINER starts it;
;;;;   - containers as DATA: a container is nothing but a state directory,
;;;;     ~/.lol/ct/<name>/, holding the overlays' upper layers (so everything written
;;;;     to /usr, /etc, /var or /opt inside persists) and the layout file lol-ct
;;;;     writes on every run. Creating one is mkdir; deleting one is rm -r; listing
;;;;     them is reading a directory;
;;;;   - containers as SUBJECTS, so they are visible in the environment: a list of
;;;;     them (Spotter: "containers"), one container's view (its runs, its state, what
;;;;     its last run was actually given), and x to run a command in one, streamed
;;;;     into a run pane (runs.lisp).
;;;;
;;;; Decisions taken with the user's design doc (§6 there, answered 2026-10-12):
;;;; inside you are YOURSELF (uid 1000, not a fake root); state lives under
;;;; ~/.lol/ct/<name>/; no uid range and no new job for lol-rootd; run-and-stream
;;;; first — a real terminal is the terminal pane's work, not this file's.
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

(defparameter *lol-ct* "/sbin/lol-ct"
  "The namespace helper, shipped in the initramfs (initramfs.sbcl.list).")

(defparameter *container-host-root* "/mnt/host"
  "The distribution a container runs: the laptop's own root, mounted read-only by
   preinit (doc/storage.org). Under QEMU's LOL_GUEST_HOSTROOT=1 it is the build
   host's root over 9p instead.")

(defparameter *container-default-name* "default")

;;; ---- where containers live ---------------------------------------------------

(defun containers-directory ()
  "~/.lol/ct — one subdirectory per container."
  (path-join (path-join (home-directory) ".lol") "ct"))

(defun valid-container-name-p (name)
  "A container name becomes a directory name and a hostname (lol-NAME): letters,
   digits, - and _ only, and not empty."
  (and (stringp name) (plusp (length name)) (<= (length name) 40)
       (every (lambda (c) (or (alphanumericp c) (member c '(#\- #\_)))) name)
       (char/= (char name 0) #\-)))

(defun container-state-dir (name)
  (path-join (containers-directory) name))

(defun containers-available-p ()
  "Can a container run here at all? It needs the helper and a host root with a /usr.
   On the build host (render-frame, poc/run-browser) neither is there."
  (and (probe-file *lol-ct*)
       (eq (ignore-errors (path-type (path-join *container-host-root* "usr"))) :directory)))

;;; ---- the public API -------------------------------------------------------------

(defun container-env-args (env)
  "ENV as lol-ct -e arguments. Each entry is \"VAR=value\" or a cons (VAR . value)."
  (loop for e in env
        append (list "-e" (if (consp e) (format nil "~a=~a" (car e) (cdr e)) e))))

(defun container-command-line (argv &key (name *container-default-name*) cwd env no-input)
  "The program and arguments which, executed, run ARGV inside container NAME: as the
   user, with the host's root plus this container's overlays as /, our network,
   our /etc/resolv.conf, /dev (with /dev/pts and /dev/shm), /tmp and /home.
   CWD is a directory INSIDE (default: $HOME). ENV adds variables (see
   CONTAINER-ENV-ARGS). NO-INPUT gives the program /dev/null as stdin — for output
   that nobody can answer (a run pane).
     (container-command-line '(\"git\" \"status\") :cwd \"/home/me/src/app\")
   => (\"/sbin/lol-ct\" \"-n\" \"default\" \"-C\" \"/home/me/src/app\" \"--\" \"git\" \"status\")"
  (unless (valid-container-name-p name) (error "bad container name ~s" name))
  (append (list *lol-ct* "-n" name)
          (when cwd (list "-C" cwd))
          (container-env-args env)
          (when no-input (list "-N"))
          (list "--")
          argv))

(defun container-environment (&optional (term "xterm-256color"))
  "The environment lol-ct is started with. lol-ct builds the program's environment
   afresh (lol-ct.c, BUILD-ENV) and takes only HOME, USER and TERM from ours."
  (cons (format nil "TERM=~a" term)
        (remove-if (lambda (s) (and (>= (length s) 5) (string= "TERM=" s :end2 5)))
                   (sb-ext:posix-environ))))

(defun run-in-container (argv &key (name *container-default-name*) cwd env pty output wait
                                   (term "xterm-256color"))
  "Start ARGV inside container NAME and return the SB-EXT:PROCESS (lol-ct's: signal
   it to stop the command — it forwards). PTY, OUTPUT and WAIT mean what they mean to
   SB-EXT:RUN-PROGRAM; with neither PTY nor OUTPUT the output is discarded. A pty
   gets the terminal type TERM."
  (sb-ext:run-program *lol-ct*
                      (rest (container-command-line argv :name name :cwd cwd :env env))
                      :wait wait :pty pty
                      :input nil
                      :output output :error (if output :output nil)
                      :environment (container-environment term)))

;;; ---- containers as data -----------------------------------------------------------

(defun list-containers ()
  "The names of the containers that exist: the subdirectories of ~/.lol/ct."
  (let ((dir (containers-directory)) (names '()))
    (when (eq (ignore-errors (path-type dir)) :directory)
      (ignore-errors
        (map-directory-names (lambda (n dtype) (declare (ignore dtype))
                               (when (and (valid-container-name-p n)
                                          (eq (path-type (path-join dir n)) :directory))
                                 (push n names)))
                             dir)))
    (sort names #'string<)))

(defun create-container (name)
  "Make container NAME's state directory. That is all a container is until it runs;
   lol-ct creates the rest (the uppers, the root mount point) on first use."
  (unless (valid-container-name-p name) (error "~s is not a container name (letters, digits, - and _)" name))
  (let ((dir (container-state-dir name)))
    (ensure-directories-exist (concatenate 'string dir "/"))
    dir))

(defun container-busy-p (name)
  "Is some lol-ct running container NAME right now? Read every process's argv from
   /proc — this sees runs started by anyone (the terminal pane too), not only ours."
  (let ((found nil))
    (ignore-errors
      (map-directory-names
       (lambda (pid dtype) (declare (ignore dtype))
         (when (every #'digit-char-p pid)
           (let ((args (ignore-errors
                        (with-open-file (s (format nil "/proc/~a/cmdline" pid)
                                           :element-type '(unsigned-byte 8))
                          (let ((v (make-array 4096 :element-type '(unsigned-byte 8))))
                            (split-nul (subseq v 0 (read-sequence v s))))))))
             (when (and args (search "lol-ct" (first args))
                        (string= name (or (second (member "-n" args :test #'string=))
                                          *container-default-name*)))
               (setf found t)))))
       "/proc"))
    found))

(defun split-nul (octets)
  "/proc/PID/cmdline: the arguments, each ended by a NUL byte."
  (let ((out '()) (start 0))
    (loop for i from 0 below (length octets)
          when (zerop (aref octets i))
            do (push (sb-ext:octets-to-string octets :start start :end i :external-format :latin-1) out)
               (setf start (1+ i)))
    (nreverse out)))

(defun make-tree-writable (path)
  "chmod u+rwx every directory under PATH. Overlayfs leaves its work/work directory
   at mode 000 — fine for the kernel, but delete-tree could not even list it."
  (when (eq (path-type path) :directory)
    (%chmod path #o755)
    (map-directory-names (lambda (n dtype) (declare (ignore dtype))
                           (make-tree-writable (path-join path n)))
                         path)))

(defun delete-container (name)
  "Delete container NAME: its state directory and so every change made inside it.
   Refused while it runs — its root is mounted on that directory in the running
   container's namespace, and removing a mount point from outside would pull the
   floor from under it."
  (unless (valid-container-name-p name) (error "bad container name ~s" name))
  (when (container-busy-p name) (error "container ~a is running - stop its runs first" name))
  (let ((dir (container-state-dir name)))
    (make-tree-writable dir)
    (delete-tree dir)
    dir))

(defun container-layout (name)
  "The lines lol-ct wrote to <state>/layout on the last run: what each directory got
   (overlay, or a fallback and why), the binds, how the root was entered."
  (ignore-errors
    (with-open-file (s (path-join (container-state-dir name) "layout"))
      (loop for l = (read-line s nil) while l collect l))))

(defun container-runs (name)
  (remove-if-not (lambda (r) (equal (run-container r) name)) *runs*))

;;; ---- running a command, streamed ----------------------------------------------------

(defparameter *container-run-shell* '("bash" "-lc")
  "How a typed command line is run: by a LOGIN bash, which reads ~/.profile — where
   rbenv, nvm and friends put themselves on the PATH — so `ruby` is the project's
   Ruby, as in a terminal on the host. The price: whatever your login dotfiles print
   (an ssh-agent banner, say) heads every run pane. '(\"bash\" \"-c\") skips them,
   and with them the PATH they set.")

(defvar *last-container-command* "git status"
  "What x offers to run, pre-filled: the last command typed.")

(defun container-cwd ()
  "Where a command starts: the folder the focused pane shows when it is under /home
   (so x in the file manager runs THERE, as a shell would), else $HOME."
  (let ((dir (ignore-errors (spotter-cwd)))
        (home (home-directory)))
    (if (and (stringp dir) (>= (length dir) 6) (string= "/home/" dir :end2 6))
        dir
        home)))

(defun run-shell-in-container (command &key (name *container-default-name*) cwd)
  "Run the shell COMMAND (a string, as typed) in container NAME, streamed into a new
   run pane. Through `bash -lc`: a LOGIN shell reads ~/.profile, which is where rbenv,
   nvm and friends put themselves on the PATH — so `ruby` is the project's Ruby, as in
   a terminal on the host. Returns the RUN."
  (let ((cwd (or cwd (container-cwd))))
    (show-run
     (start-run (container-command-line (append *container-run-shell* (list command))
                                        :name name :cwd cwd :no-input t
                                        ;; a pager would wait for a key that cannot come
                                        :env '("PAGER=cat" "GIT_PAGER=cat"))
                :title command :container name
                :environment (container-environment)))))

(defun container-run-output (command &key (name *container-default-name*) cwd (timeout 120))
  "REPL / test entry point: run the shell COMMAND in container NAME, WAIT for it (at
   most TIMEOUT seconds, then stop it), and return its output lines and exit code —
   the same RUN machinery as a pane, without the pane.
     (container-run-output \"id -u; hostname\")  =>  (\"1000\" \"lol-default\" \"-- exit 0 …\"), 0"
  (let ((run (start-run (container-command-line (append *container-run-shell* (list command))
                                                :name name :cwd (or cwd (home-directory))
                                                :no-input t :env '("PAGER=cat" "GIT_PAGER=cat"))
                        :title command :container name
                        :environment (container-environment))))
    (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
          while (run-live-p run)
          do (when (> (get-internal-real-time) deadline)
               (stop-run run)
               (sleep (+ 1 +run-kill-grace+))
               (when (run-live-p run)
                 (ignore-errors (sb-ext:process-kill (run-process run) sb-unix:sigkill))))
             (sleep 0.05))
    (values (map 'list #'car (run-snapshot run)) (run-code run))))

(defun prompt-container-command (name)
  "x: ask for a command, then run it in container NAME."
  (if (not (containers-available-p))
      (setf *browser-status* (format nil "no containers here: ~a or ~a/usr is missing"
                                     *lol-ct* *container-host-root*))
      (minibuffer-read (format nil "Run in ~a (in ~a): " name (container-cwd))
                       (lambda (text)
                         (let ((cmd (string-trim '(#\Space #\Tab) text)))
                           (when (plusp (length cmd))
                             (setf *last-container-command* cmd)
                             (run-shell-in-container cmd :name name))))
                       :initial *last-container-command*
                       :hint "bash -lc  Enter runs  Esc cancels")))

(defun open-container-shell (name)
  "A login shell in container NAME, in a TERMINAL pane — when the terminal pane exists
   (OPEN-TERMINAL, poc/terminal.lisp, a separate piece of work). Run-and-stream cannot
   host an interactive shell: it has no input."
  (if (fboundp 'open-terminal)
      (funcall 'open-terminal (container-command-line (list "bash" "-l") :name name
                                                      :cwd (container-cwd))
               :title (format nil "shell in ~a" name))
      (setf *browser-status* "no terminal pane in this image yet - x runs a command instead")))

;;; ---- subjects ------------------------------------------------------------------------

(defstruct (subj-containers (:constructor subj-containers ())))
(defstruct (subj-container (:constructor subj-container (name))) name)

(defun action-item (label detail action &key (group "do"))
  "A row that DOES something on Enter rather than opening a view. The shell drills
   only into a row's SUBJECT; our SUBJECT-KEY methods run the ACTION slot instead."
  (make-item :label label :detail detail :kind :restart :group group :action action))

(defun run-selected-action (pane)
  (let ((it (selected-item pane)))
    (when (and it (item-action it))
      (funcall (item-action it))
      t)))

(defmethod present ((s subj-containers))
  (let ((names (list-containers)))
    (make-view
     :title "containers" :kind :list :subject s
     :items
     (append
      (unless (containers-available-p)
        (list (make-item :label "containers are unavailable here"
                         :detail (format nil "need ~a and a host root at ~a" *lol-ct* *container-host-root*)
                         :kind :condition :group "note")))
      (mapcar (lambda (n)
                (make-item :label n
                           :detail (format nil "~a~@[ - ~d run~:p this session~]"
                                           (container-state-dir n)
                                           (let ((k (length (container-runs n)))) (and (plusp k) k)))
                           :kind :package :group "containers" :subject (subj-container n)))
              names)
      (list (action-item (format nil "run a command in ~a..." *container-default-name*)
                         "x - bash -lc, output streamed into a pane"
                         (lambda () (prompt-container-command *container-default-name*)))
            (action-item "new container..." "+ - a name; its state goes in ~/.lol/ct/<name>"
                         (lambda ()
                           (minibuffer-read "New container: "
                                            (lambda (text)
                                              (create-container (string-trim " " text))
                                              (setf (pane-view (current-pane))
                                                    (present (pane-subject (current-pane)))))
                                            :hint "letters digits - _"))))))))

(defmethod subject-key ((s subj-containers) pane key state)
  (declare (ignore s state))
  (let* ((it (selected-item pane))
         (name (and it (subj-container-p (item-subject it))
                    (subj-container-name (item-subject it)))))
    (case key
      (:return (run-selected-action pane))
      ((#\x #\!) (prompt-container-command (or name *container-default-name*)) t)
      ((#\+ #\n)
       (funcall (item-action (find "new container..." (view-items (pane-view pane))
                                   :test #'equal :key #'item-label)))
       t)
      (t nil))))

(defmethod subject-hints ((s subj-containers) pane)
  (declare (ignore s pane))
  '(("ret" . "open") ("x" . "run a command") ("+" . "new container") ("q" . "back")))

(defmethod present ((s subj-container))
  (let* ((name (subj-container-name s))
         (dir (container-state-dir name))
         (runs (container-runs name))
         (layout (container-layout name)))
    (make-view
     :title (format nil "container ~a" name) :kind :list :subject s
     :items
     (append
      (list (action-item "run a command..." (format nil "x - in ~a" (container-cwd))
                         (lambda () (prompt-container-command name)))
            (action-item "shell" (if (fboundp 'open-terminal)
                                     "s - a login shell in a terminal pane"
                                     "s - needs the terminal pane (not in this image yet)")
                         (lambda () (open-container-shell name))))
      (mapcar (lambda (r)
                (make-item :label (run-title r) :detail (run-state-string r)
                           :kind (if (run-live-p r) :restart :function)
                           :group "runs this session" :subject r))
              runs)
      (list (make-item :label dir :detail "state: the overlays' upper layers + layout"
                       :kind :directory :group "state"
                       :subject (and (eq (ignore-errors (path-type dir)) :directory)
                                     (subj-dir dir))))
      (if layout
          (mapcar (lambda (l) (make-item :label l :group "what the last run was given"
                                         :kind (if (or (search "FAILED" l) (search "refused" l)
                                                       (search "VANISH" l) (search "read-only bind" l))
                                                   :condition :value)))
                  layout)
          (list (make-item :label "(never run)" :group "what the last run was given")))
      (list (action-item "delete this container" "D - removes its state: every change made inside"
                         (lambda () (confirm-delete-container name))
                         :group "danger"))))))

(defun confirm-delete-container (name)
  (minibuffer-read (format nil "Delete container ~a and everything written inside it? Type yes: " name)
                   (lambda (text)
                     (when (string-equal (string-trim " " text) "yes")
                       (delete-container name)
                       (trail-pop)
                       (setf (pane-view (current-pane)) (present (pane-subject (current-pane))))
                       (setf *browser-status* (format nil "deleted container ~a" name))))
                   :hint "anything else cancels"))

(defmethod subject-key ((s subj-container) pane key state)
  (declare (ignore state))
  (let ((name (subj-container-name s)))
    (case key
      (:return (run-selected-action pane))
      ((#\x #\!) (prompt-container-command name) t)
      ((#\s #\S) (open-container-shell name) t)
      (#\D (confirm-delete-container name) t)
      (#\g (setf (pane-view pane) (present s)) t)        ; refresh
      (t nil))))

(defmethod subject-hints ((s subj-container) pane)
  (declare (ignore s pane))
  '(("x" . "run a command") ("s" . "shell") ("ret" . "open") ("g" . "refresh")
    ("D" . "delete") ("q" . "back")))

(defmethod commands-for ((s subj-container))
  (let ((name (subj-container-name s)))
    (list (make-command :label "Run a command..." :key "x" :surfaces '(:menu :halo)
                        :action (lambda () (prompt-container-command name)))
          (make-command :label "Shell (terminal pane)" :key "s" :surfaces '(:menu)
                        :action (lambda () (open-container-shell name)))
          (make-command :label "Delete container..." :key "D" :surfaces '(:menu)
                        :action (lambda () (confirm-delete-container name))))))

(defun containers-spotter-candidates ()
  (cons (list :label "containers" :detail "surface · the host's Debian, as yourself"
              :group "surfaces" :kind :package :subject (subj-containers))
        (mapcar (lambda (n)
                  (list :label (format nil "container ~a" n) :detail "container"
                        :group "surfaces" :kind :package :subject (subj-container n)))
                (or (ignore-errors (list-containers)) (list *container-default-name*)))))

(pushnew 'containers-spotter-candidates *spotter-sources*)

(defun show-containers ()
  "REPL entry point: print the containers view."
  (show-view (subj-containers)))
