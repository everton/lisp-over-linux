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
;;; Each is (MENU-KEY . STEPS). A step is one of:
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
  '(;; the module index (§4⅔): the table of contents, first module previewed beside
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
    ;; the Spotter (§4¾): the floating palette over the dimmed browser. Super must be
    ;; HELD — a send-key chord releases it too fast to survive the evdev-modifier /
    ;; cooked-tty split (lol.qmp:hold explains why).
    ("spotter"   "l" ((:wait 1.0) (:hold "meta_l" "spc") (:wait 1.5) (:type "coll")))))

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
  "Two things must happen before we type on the console, and both go through the net
   REPL: confirm the guest is really EVALUATING (QEMU's hostfwd opens the HOST side
   of :4005 at t=0, so merely connecting proves nothing), and lower the kernel
   console loglevel so a late printk cannot paint over the demo on tty0.

   CRUCIAL: the REPL's listen backlog is 1 (net.lisp: socket-listen server 1), so a
   second queued connection triggers a 'TCP: Possible SYN flooding' printk that lands
   on the console. So we make EXACTLY ONE connection and do both jobs on it."
  (let ((deadline (+ (get-universal-time) timeout)))
    (loop while (< (get-universal-time) deadline)
          do (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                                     :type :stream :protocol :tcp)))
               (if (not (ignore-errors (sb-bsd-sockets:socket-connect s #(127 0 0 1) 4005) t))
                   (sleep 1.5)                     ; refused: the REPL is not up yet
                   (let ((stream (sb-bsd-sockets:socket-make-stream
                                  s :input t :output t :buffering :none
                                  :element-type 'character :external-format :latin-1)))
                     (repl-drain stream 1.0)       ; swallow the banner
                     (write-string "(+ 1 1)" stream) (write-char #\Newline stream)
                     (force-output stream)
                     (if (not (search "2" (repl-drain stream 3.0)))
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
    ;; The REPL comes up at start-net-repl, still BEFORE the supervisor clears the
    ;; screen and paints the menu. Wait past that so the menu key lands in the menu's
    ;; read-line, on a clean screen.
    (sleep 6)
    (lol.qmp:with-qmp (socket)
      (format t "~&connected to QMP; driving mode=~a~@[ scene=~a~]~%"
              mode (and (string= mode "scene") scene))
      (finish-output)
      (if (string= mode "repl")
          (progn
            ;; menu: 'r' enters the REPL (a cooked read-line, so it needs the Enter)
            (lol.qmp:key "r") (sleep 0.2) (lol.qmp:key "ret") (sleep 1.5)
            ;; each Enter triggers eval + a coloured result line
            (dolist (form *forms*)
              (lol.qmp:type-string form) (sleep 0.3)
              (lol.qmp:key "ret") (sleep 1.5))
            (sleep 1.0))                      ; let the last redraw + alien settle
          ;; menu: a single letter launches run-browser-fb rooted somewhere ('b' the
          ;; module index, 'l' the flat package list, 'p' the Workspace). The menu
          ;; read is still COOKED, so it needs the Enter; from then on the browser
          ;; owns the keyboard in raw mode and single keycodes are what it wants.
          (let* ((entry (and (string= mode "scene")
                             (or (assoc scene *scenes* :test #'string=)
                                 (error "unknown scene: ~a" scene))))
                 (menu-key (if entry (second entry) "b"))
                 (steps (if entry (third entry)
                            (mapcar (lambda (k) (list :key k)) *browser-keys*))))
            (lol.qmp:key menu-key) (sleep 0.2) (lol.qmp:key "ret")
            ;; Give it time to load the font, build the canvas, open evdev and paint
            ;; the first full-screen frame — much slower than a REPL prompt, because
            ;; it is a 1280x800 canvas rendered pixel by pixel in Lisp.
            (sleep 8.0)
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
