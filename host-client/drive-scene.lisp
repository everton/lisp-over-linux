;;;; drive-scene.lisp — boot the guest's screenshot, by driving it from outside.
;;;;
;;;;   sbcl --script host-client/drive-scene.lisp QMP-SOCKET OUT.ppm MODE [SCENE]
;;;;
;;;; MODE is "repl", "browser" or "scene"; SCENE names one of the illustrations in
;;;; doc/code-browser.org. Called by ./refresh-screenshot.sh once QEMU is up; it does
;;;; the driving and the screendump, and the shell script does the booting and the
;;;; dated-file bookkeeping.
;;;;
;;;; This was ~270 lines of Python embedded in that shell script. Nothing about the
;;;; job wanted another language: it is a Unix socket, a TCP socket and some sleeps,
;;;; all of which SBCL has. See host-client/qmp.lisp for the QMP client and the two
;;;; timing subtleties (HOLD and CLICK) that this kind of driving keeps running into.
;;;;
;;;; The screenshots are deterministic because every list in the browser is sorted
;;;; alphabetically and a fresh pane opens on row 0 — so "down N times" always lands
;;;; in the same place. Add a definition to examples.lisp and the 'matrix' path
;;;; shifts; re-check it.

(require :sb-bsd-sockets)

(let ((here (make-pathname :directory (pathname-directory *load-truename*))))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "qmp.lisp" here))))

;;; ---- what the REPL screenshot types --------------------------------------
;;; EDIT HERE to change what the shot demonstrates. Kept stable across font versions
;;; so the shots stay comparable: green => 42, the green "SBCL" string, the Linux
;;; kernel release — the "Lisp over Linux" theme in one line — then a small
;;; multi-line loop that spells numbers out with (format ~r), chosen to show off the
;;; live highlighter (rainbow-nested parens, a green "~r" string, a dim ; comment and
;;; the dim "  ...>" continuation prompt), and finally magenta :announce. A newline
;;; inside a form drives the REPL's multi-line continuation.

(defparameter *forms*
  (list "(+ 40 2)"
        "(lisp-implementation-type)"
        "(software-version)"
        (format nil "(loop for n in '(1 42 2026)   ; spell them out~%~
                            collect (format nil \"~~r\" n))")
        "(draw-alien :announce t)"))

;;; The browser demo, as keys sent after 'b' has launched the environment. EMPTY on
;;; purpose: the DEFAULT view is already the best picture — the module index in the
;;; left pane with the selected module's definitions beside it, tabs on top, commands
;;; in the status bar. That fills the screen; drilling lands on whatever short
;;; definition happens to be second and leaves most of the canvas empty. 'esc' is
;;; deliberately never sent: we want to end INSIDE the browser.
(defparameter *browser-keys* '())

;;; ---- the named scenes ----------------------------------------------------
;;; Each is (MENU-KEY . STEPS). MENU-KEY "b" opens the module index (SuperL-m), "e"
;;; leaves the environment as it booted, anything else is a supervisor-menu letter.
;;; A step is one of:
;;;   (:key   qcode)        a single key
;;;   (:chord qcode…)       a chord, e.g. (:chord "ctrl" "c")
;;;   (:hold  mod key)      press key while mod is HELD (see lol.qmp:hold)
;;;   (:click x y)          left-click at absolute x,y (see lol.qmp:click)
;;;   (:type  "text")       type a literal string
;;;   (:wait  seconds)      extra dwell
;;;
;;; NOT a scene: the effective-method onion. A :matrix pane has no keyboard cell
;;; cursor (matrix-cell-click in poc/shell.lisp is the only way in), so it needs a
;;; scripted mouse click, and CLICK could not land one. The slam-to-corner origin
;;; works but the follow-up move never takes effect, most likely because the ~32 PS/2
;;; packets of the slam overflow QEMU's queue and later events are dropped. Try
;;; stepping the cursor in small increments if you pick this up.

(defparameter *scenes*
  '(;; THE environment, as it boots on the laptop: the stand-in disk attached, so PID 1
    ;; runs as the user with a real home — the Workspace, the tray, the outline font.
    ;; The README's first picture (2026-10-16): what you see when you turn it on.
    ;; It does one thing, so the picture shows the buffer as a REPL: define GREET
    ;; live (C-c C-c on line 10) and Print (greet "lisp") under itself (C-c C-p, line 11).
    ("desk"      "e" ((:wait 2.0)
                      (:key "down") (:key "down") (:key "down") (:key "down") (:key "down")
                      (:key "down") (:key "down") (:key "down") (:key "down") (:key "end")
                      (:chord "ctrl" "c") (:chord "ctrl" "c") (:wait 1.5)
                      (:key "down") (:key "end")
                      (:chord "ctrl" "c") (:chord "ctrl" "p") (:wait 2.0)))
    ;; the module index (§4⅔): the table of contents, first module previewed beside
    ("modules"   "b" ())
    ;; the flat CL-USER package view — the old default root, kept on 'l'
    ("package"   "l" ())
    ;; the Workspace (§3a): a scratch buffer whose verbs eval
    ("workspace" "p" ())
    ;; the dispatch matrix (§4a) for COLLIDE, the 2-axis showcase in examples.lisp:
    ;;   modules -> initramfs/examples (3rd) -> collide (3rd) -> "dispatch matrix"
    ("matrix"    "b" ((:key "down") (:key "down") (:key "ret")
                      (:key "down") (:key "down") (:key "ret")
                      (:wait 1.0) (:key "ret")))
    ;; the live debugger (phase 6): Workspace -> a form that errors -> Debug it.
    ;; (lol.examples:launch) rather than (/ 1 0): LAUNCH drifts OFF-COURSE and STEER
    ;; signals it from inside a restart-case, so the rail fills with FOUR named ways
    ;; out plus ABORT — the whole point of the condition system. (/ 1 0) offers only
    ;; ABORT, which shows a rail of one and teaches nothing.
    ;; Tab-down-down-Tab parks the frame cursor on LAUNCH and hands focus back to the
    ;; restarts, so the bottom strip shows real locals instead of frame 0's
    ;; SBCL-internal noise.
    ("debugger"  "p" ((:wait 1.0) (:type "(lol.examples:launch)") (:wait 0.8)
                      (:chord "ctrl" "c") (:chord "ctrl" "d") (:wait 2.0)
                      (:key "tab") (:key "down") (:key "down") (:key "tab")
                      (:wait 0.6)))
    ;; the stage (§3b): the same drill as 'matrix', one row further down —
    ;;   modules -> initramfs/examples (3rd) -> collide (3rd) -> "stage"
    ;; The long dwell at the end is the point. Every other scene is finished the
    ;; instant the last key lands; a stage keeps running, so the shot is of a world
    ;; several seconds old with a transcript that has had time to fill.
    ("stage"     "b" ((:key "down") (:key "down") (:key "ret")
                      (:key "down") (:key "down") (:key "ret")
                      (:wait 1.0) (:key "down") (:key "ret") (:wait 13.0)))
    ;; the stage meeting a HOLE (§3b): drill to the stage, then spawn missiles until
    ;; two of them meet. (missile missile) is one of the three total absences in
    ;; COLLIDE — nothing specialises either argument on a missile — so it signals for
    ;; real and the debugger opens on the standing stack.
    ;;
    ;; This scene exists because the path it walks was BROKEN once: the tick had no
    ;; handler-bind, so the condition reached run-browser-fb's outer handler-case,
    ;; which caught it by unwinding and dropped the user at the supervisor menu with
    ;; the error printed and the stack gone. Keep the scene; it is the regression test.
    ("stage-debugger" "b" ((:key "down") (:key "down") (:key "ret")
                           (:key "down") (:key "down") (:key "ret")
                           (:wait 1.0) (:key "down") (:key "ret") (:wait 1.0)
                           (:key "m") (:key "m") (:key "m") (:key "m") (:key "m")
                           (:wait 8.0)))
    ;; beat 4 of §3b's loop: the hole hands you a method, you Accept it, the world
    ;; changes. Drill to the stage, spawn missiles until two meet (the debugger opens),
    ;; pick restart 4 [define-method], and the template lands in the Workspace BESIDE
    ;; the stage with the specialisers already filled — then C-c C-c compiles it into
    ;; the image. The template's :?? tag is left as-is on purpose: it is a valid
    ;; keyword, so Accept succeeds without any typing and stage-enact falls to its
    ;; default. The scene proves the WIRING; filling the tag in is the human's part.
    ("stage-accept" "b" ((:key "down") (:key "down") (:key "ret")
                         (:key "down") (:key "down") (:key "ret")
                         (:wait 1.0) (:key "down") (:key "ret") (:wait 1.0)
                         (:key "m") (:key "m") (:key "m") (:key "m") (:key "m")
                         (:wait 8.0)
                         (:key "4") (:key "ret") (:wait 1.5)
                         (:chord "ctrl" "c") (:chord "ctrl" "c") (:wait 1.5)))
    ;; keyboard selection + inspect on the stage: Tab twice picks a sprite, Enter
    ;; opens the Inspector on the INSTANCE — an ordinary object view, no stage in it.
    ;; Scripted with the keyboard on purpose: a click cannot be driven reliably over
    ;; QMP (see the note above), and a stage reachable only by mouse would fail the
    ;; project's own rule that the pointer is an accelerator, not a requirement.
    ("stage-inspect" "b" ((:key "down") (:key "down") (:key "ret")
                          (:key "down") (:key "down") (:key "ret")
                          (:wait 1.0) (:key "down") (:key "ret") (:wait 2.0)
                          (:key "spc")                       ; pause, so it holds still
                          (:key "tab") (:key "tab") (:wait 0.5)
                          (:key "ret") (:wait 1.5)))
    ;; the Workspace beside the stage (§3b): 'w' opens it on a primer written for the
    ;; stage rather than the generic scratch-buffer cheatsheet. The only coverage of
    ;; the w path, and the only place the editable side pane is shown holding something
    ;; other than a defmethod template.
    ("stage-workspace" "b" ((:key "down") (:key "down") (:key "ret")
                            (:key "down") (:key "down") (:key "ret")
                            (:wait 1.0) (:key "down") (:key "ret") (:wait 2.0)
                            (:key "w") (:wait 1.5)))
    ;; the Spotter (§4¾): the floating palette over the dimmed browser. Super must be
    ;; HELD — a send-key chord releases it too fast to survive the evdev-modifier /
    ;; cooked-tty split (lol.qmp:hold explains why).
    ("spotter"   "l" ((:wait 1.0) (:hold "meta_l" "spc") (:wait 1.5) (:type "coll")))
    ;; a FUNCTION as an object (doc/inspector-history.org §5.5): the primer fits one
    ;; screen, so ONE PageDown parks point on its last line; the last form above it is
    ;; the COMPILE-d twin of MAKE-COUNTER (runnable in the guest since logcount-fix),
    ;; so Inspect it opens that closure — the same two captures.
    ;; The payload is the two capture rows — one marked (settable) because SBCL boxed
    ;; it in a VALUE-CELL when something assigned to it, one not because it is only
    ;; ever read: a decision the compiler made about your code, made visible.
    ("closure"   "p" ((:wait 1.0) (:key "pgdn")
                      (:chord "ctrl" "c") (:chord "ctrl" "i") (:wait 2.5)))
    ;; a SECOND PROCESS contributing a view (doc/processes.org §7). Typed rather than
    ;; paged to, because what is being shown is that the pid in the provenance row is
    ;; not 1.
    ("worker"    "p" ((:wait 1.0) (:type "(subj-worker :self)") (:wait 0.8)
                      (:chord "ctrl" "c") (:chord "ctrl" "i") (:wait 3.0)))
    ;; ---- the DISK scenes: refresh-screenshot.sh attaches test-disk.img (snapshot=on)
    ;; for these, so they show the stand-in home of make-test-disk.sh, never the real
    ;; one. Menu key "e" = the environment as it boots (no SuperL-m).
    ;; the file manager (doc/file-manager.org): SuperL-f opens $HOME; the live filter
    ;; ("/") narrows it to Pictures — counted rows broke once, because the symlink
    ;; notes -> Documents/notes sorts among the folders. Inside, five files sort
    ;; browser, gradient, orange, plasma, progressive: three downs park on the JPEG,
    ;; previewed beside the listing through libstbi.so (doc/previews.org).
    ("files"     "e" ((:hold "meta_l" "f") (:wait 1.5)
                      (:key "slash") (:type "Pic") (:key "ret") (:key "ret") (:wait 1.0)
                      (:key "down") (:key "down") (:key "down") (:wait 2.5)))
    ;; an archive previewed without unpacking: the live filter ("/") narrows $HOME to
    ;; Documents, and inside it to backup.tar.gz, whose preview is gzip inflated in Lisp, then the tar headers read
    ;; as a member list. Filtered rather than counted down, so a new dotfile or a
    ;; changed sort cannot move the target.
    ("files-archive" "e" ((:hold "meta_l" "f") (:wait 1.5)
                          (:key "slash") (:type "Doc") (:key "ret") (:key "ret") (:wait 1.0)
                          (:key "slash") (:type "backup") (:key "ret") (:wait 2.0)))
    ;; the Spotter reading a PATH (doc/file-manager.org §9): "~/Pic" completes to the
    ;; Pictures folder of the stand-in home.
    ("spotter-path" "e" ((:wait 1.0) (:hold "meta_l" "spc") (:wait 1.5)
                         (:type "~/Pic") (:wait 1.5)))
    ;; SBCL's OWN source, read from the ./sbcl tree on /home (sbcl-source.lisp): type
    ;; (mapcar) in the Workspace, step back inside the symbol and M-. (Esc . — Meta
    ;; as a prefix, which QMP delivers more reliably than a held Alt). MAPCAR is a
    ;; builtin, so what opens is a reference view: the facts, then list.lisp's
    ;; MACROLET that defines it, read off the disk.
    ("sbcl-source" "e" ((:wait 1.0) (:type "(mapcar)") (:key "left")
                        (:key "esc") (:key "dot") (:wait 2.0)))))

;;; ---- the net REPL: readiness, and quieting the kernel console ------------

(defun repl-drain (stream seconds)
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second)))
        (out (make-string-output-stream)))
    (loop while (< (get-internal-real-time) deadline)
          do (let ((c (read-char-no-hang stream nil nil)))
               (if c (write-char c out) (sleep 0.02))))
    (get-output-stream-string out)))

(defun bring-up-quietly (&key (timeout 120))
  "Three things must happen before we type on the console, all through the net
   REPL: confirm the guest is really EVALUATING (QEMU's hostfwd opens the HOST side
   of :4005 at t=0, so merely connecting proves nothing), and lower the kernel
   console loglevel so a late printk cannot paint over the demo on tty0, and wait
   for the boot-time environment to be up.

   CRUCIAL: the REPL's listen backlog is 1 (net.lisp: socket-listen server 1), so a
   second queued connection triggers a 'TCP: Possible SYN flooding' printk that lands
   on the console. So we make EXACTLY ONE connection and do all three on it."
  (let ((deadline (+ (get-universal-time) timeout)))
    (loop while (< (get-universal-time) deadline)
          do (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                                     :type :stream :protocol :tcp)))
               (if (not (ignore-errors
                          (sb-bsd-sockets:socket-connect
                           s #(127 0 0 1)
                           (or (ignore-errors
                                 (parse-integer (or (sb-ext:posix-getenv "LOL_REPL_PORT") "4005")))
                               4005))
                          t))
                   (sleep 1.5)                     ; refused: the REPL is not up yet
                   (let ((stream (sb-bsd-sockets:socket-make-stream
                                  s :input t :output t :buffering :none
                                  :element-type 'character :external-format :latin-1)))
                     ;; The WHOLE probe is guarded, not just the connect: QEMU's
                     ;; hostfwd accepts before the guest has a REPL, then RESETS the
                     ;; connection, and the write lands on that reset as EPIPE. It
                     ;; used to win the race; since the boot does a little more
                     ;; (session-log.lisp, 2026-10-16) it lost it every time. The
                     ;; guest harness learned this first (repl-connect).
                     (if (not (ignore-errors
                               (repl-drain stream 1.0)       ; swallow the banner
                               (write-string "(+ 1 1)" stream) (write-char #\Newline stream)
                               (force-output stream)
                               (search "2" (repl-drain stream 3.0))))
                         (progn (ignore-errors (close stream)) (sleep 1.5))
                         (progn
                           (write-string
                            (concatenate 'string
                                         "(ignore-errors (with-open-file (o \"/proc/sys/kernel/printk\""
                                         " :direction :output :if-exists :append)"
                                         " (write-string \"1\" o) (terpri o)) :quieted)")
                            stream)
                           (write-char #\Newline stream) (force-output stream)
                           (repl-drain stream 2.0)
                           ;; The image BOOTS INTO the environment; wait for it on this
                           ;; same connection, so a key we send next lands in the
                           ;; browser and not in the boot diagnostics. 1337 because the
                           ;; echoed source reads "1000 337" and no ANSI code is 1337.
                           (loop repeat 60
                                 until (search "1337"
                                               (progn
                                                 (write-string "(if *fb-canvas* (+ 1000 337) 0)" stream)
                                                 (write-char #\Newline stream) (force-output stream)
                                                 (repl-drain stream 0.5)))
                                 do (sleep 0.5))
                           (ignore-errors
                            (write-string ":quit" stream) (write-char #\Newline stream)
                            (force-output stream) (sleep 0.3))
                           (ignore-errors (close stream))
                           (return-from bring-up-quietly t)))))))
    nil))

;;; ---- driving -------------------------------------------------------------

(defun run-step (step)
  (ecase (first step)
    (:key   (lol.qmp:key (second step))            (sleep 1.2))
    (:chord (apply #'lol.qmp:keys (rest step))     (sleep 1.2))
    (:hold  (lol.qmp:hold (second step) (third step)) (sleep 1.2))
    (:click (lol.qmp:click (second step) (third step)) (sleep 1.2))
    (:type  (lol.qmp:type-string (second step))    (sleep 0.5))
    (:wait  (sleep (second step)))))

(defun main (args)
  (destructuring-bind (&optional socket ppm (mode "repl") (scene "")) args
    (unless (and socket ppm)
      (format *error-output*
              "~&usage: drive-scene.lisp QMP-SOCKET OUT.ppm MODE [SCENE]~%")
      (return-from main 2))
    (format t "~&waiting for the guest REPL to serve...~%") (finish-output)
    (unless (bring-up-quietly)
      (format *error-output* "~&ERROR: net REPL on :4005 never answered~%")
      (return-from main 1))
    (format t "~&guest REPL up, kernel console quieted~%") (finish-output)
    ;; The environment is up (bring-up-quietly waited for it); give its first
    ;; frame a moment to reach the screen.
    (sleep 2)
    (lol.qmp:with-qmp (socket)
      (format t "~&connected to QMP; driving mode=~a~@[ scene=~a~]~%"
              mode (and (string= mode "scene") scene))
      (finish-output)
      (if (string= mode "repl")
          (progn
            ;; leave the environment (SuperL-q, then 1) for the supervisor menu, where
            ;; 'r' enters the REPL (a cooked read-line, so it needs the Enter)
            (lol.qmp:hold "meta_l" "q") (sleep 1.5)
            (lol.qmp:key "1") (sleep 1.5)     ; SuperL-q asks: 1 = the console
            (lol.qmp:key "r") (sleep 0.2) (lol.qmp:key "ret") (sleep 1.5)
            ;; each Enter triggers eval + a coloured result line
            (dolist (form *forms*)
              (lol.qmp:type-string form) (sleep 0.3)
              (lol.qmp:key "ret") (sleep 1.5))
            (sleep 1.0))                      ; let the last redraw + alien settle
          ;; The image boots into the environment on its Workspace; a 'b' scene (the
          ;; module index) opens it with SuperL-m. Any other root ('l' the flat
          ;; package list, 'p' the Workspace) is a trip through the menu: SuperL-q
          ;; leaves the environment, then the menu letter. The menu read is COOKED, so it needs
          ;; the Enter; from then on the browser owns the keyboard in raw mode and
          ;; single keycodes are what it wants.
          (let* ((entry (and (string= mode "scene")
                             (or (assoc scene *scenes* :test #'string=)
                                 (error "unknown scene: ~a" scene))))
                 (menu-key (if entry (second entry) "b"))
                 (steps (if entry (third entry)
                            (mapcar (lambda (k) (list :key k)) *browser-keys*))))
            (cond
              ((string= menu-key "e"))        ; the environment as booted: nothing to open
              ((string= menu-key "b")
                ;; the environment opens on the Workspace; SuperL-m opens the module
                ;; index in a fresh tab, row 0 selected — the state 'b' used to boot to
                (lol.qmp:hold "meta_l" "m") (sleep 2.0))
              (t
                  (lol.qmp:hold "meta_l" "q") (sleep 1.5)
                  (lol.qmp:key "1") (sleep 1.5)   ; SuperL-q asks: 1 = the console
                  (lol.qmp:key menu-key) (sleep 0.2) (lol.qmp:key "ret")
                  ;; Give it time to build the view, open evdev and paint the first
                  ;; full-screen frame.
                  (sleep 8.0)))
            (mapc #'run-step steps)
            (sleep 2.0)))                     ; let the final frame settle
      (lol.qmp:screendump (namestring (merge-pathnames ppm)))
      (sleep 1.0)
      (format t "~&screendump: ~a~%done~%" ppm) (finish-output))
    0))

(sb-ext:exit :code (handler-case (main (rest sb-ext:*posix-argv*))
                     (serious-condition (c)
                       (format *error-output* "~&drive-scene: ~a~%" c)
                       1)))
