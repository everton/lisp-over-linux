;;;; render-frame.lisp — render ONE browser frame, headless, on the host.
;;;;
;;;;   sbcl --script poc/render-frame.lisp SUBJECT [-o FILE] [-s WxH] [-k "KEYS"]
;;;;
;;;; The verification harness. Every visual change to the code browser is checked
;;;; by rendering a frame and LOOKING at it — which, before this file existed, meant
;;;; booting QEMU and driving the guest over QMP for a two-pixel colour tweak.
;;;;
;;;; It loads the real stack in the real order (registry -> model -> present ->
;;;; canvas -> textview -> shell — the same list build.sh uses, minus the parts that
;;;; touch hardware), seeds the environment on a subject named on the command line,
;;;; optionally replays a script of keystrokes and clicks, has the real world repair
;;;; itself into a canvas — every morph drawing itself, as in the guest — and dumps
;;;; the pixels as a PPM.
;;;;
;;;; It is pixel-identical to /dev/fb0 by construction rather than by luck: the guest
;;;; runs THIS canvas through lol.fb:present-fb, which is a byte-for-byte copy of the
;;;; same #x00RRGGBB words into the framebuffer (poc/README.org, "one pixel format
;;;; serves both"). Nothing here draws; it only chooses the size and writes the file.
;;;;
;;;; Single-threaded, no QEMU, no X, no /dev/fb0 — plain SBCL, about a second a frame.
;;;;
;;;; Look at the result with any image viewer; PPM is deliberately dependency-free
;;;; (a 15-byte header and raw RGB), and `pnmtopng` or `convert` turns it into a PNG
;;;; if you want one.
;;;;
;;;; Examples:
;;;;   sbcl --script poc/render-frame.lisp modules
;;;;   sbcl --script poc/render-frame.lisp matrix:COLLIDE -o /tmp/m.ppm
;;;;   sbcl --script poc/render-frame.lisp class:STREAM -k "down down ret"
;;;;   sbcl --script poc/render-frame.lisp package:CL-USER -s 1280x800

(require :sb-introspect)
(require :sb-posix)
(require :sb-bsd-sockets)       ; the agent's transport (agent-wire.lisp)

;;; ---- load the real stack -------------------------------------------------
;;; The order matters and mirrors build.sh's LISP_SOURCES. registry.lisp goes first
;;; with a plain LOAD because it DEFINES load-recording; everything after is loaded
;;; through it, so the browser can browse its own source here exactly as in the
;;; guest. Omitted from build.sh's list: fb.lisp / fb-browser.lisp (want /dev/fb0),
;;; x11.lisp (wants a display), and the supervisor and network modules (nothing to
;;; draw). The shell degrades gracefully without fb-browser — it reaches the Spotter
;;; and the debugger through (fboundp …).

(defvar *halo-at* nil
  "When set by --halo X,Y[,DEPTH], render the middle-button halo opened at X,Y on
   layer DEPTH (0 = the innermost thing under the point) and list the layers.")

(defvar *spotter-query* nil
  "When set by --spotter, render the Spotter palette over the subject instead of the
   plain browser.")

(defparameter *poc-dir* (make-pathname :directory (pathname-directory *load-truename*)))
(defparameter *root-dir* (merge-pathnames "../" *poc-dir*))

(defparameter *stack*
  '("initramfs/sbcl-source.lisp"  ; SBCL's own source, read from ./sbcl on demand
    "initramfs/model.lisp"        ; CLOS/introspection model
    "initramfs/present.lisp"      ; the present / commands-for protocol
    "initramfs/tour.lisp"         ; the guided tour (subj-tour) — doc/tour.org
    "initramfs/examples.lisp"     ; COLLIDE — a real 2-axis dispatch matrix to render
    "initramfs/stage.lisp"        ; the COLLIDE stage (§3b) — bodies, dispatch, enact
    "poc/ttf.lisp"                ; lol.ttf — TrueType outlines + the rasteriser
    "poc/canvas.lisp"             ; the pixel buffer, the font, draw-string
    "poc/textview.lisp"           ; the editable, syntax-colouring text view
    "poc/widgets.lisp"            ; phase 3 toolkit: panel / list / scrollbar / grid
    "poc/morph.lisp"              ; lol.morph — morph, world, hand, damage repair
    "initramfs/ansi.lisp"         ; needed by line-editor
    "initramfs/line-editor.lisp"  ; token-start / longest-common-prefix, used by the shell
    "poc/clipboard.lisp"          ; lol.clipboard — the shell's copy / paste / rectangles
    "poc/org.lisp"                ; lol.org — Org's outline, folding and structure edits
    "initramfs/sysinfo.lisp"      ; the systray's battery + clock
    "poc/inflate.lisp"            ; lol.inflate — gzip previews
    "poc/image.lisp"              ; lol.image — image previews
    "initramfs/files.lisp"        ; the file system as data
    "poc/terminal.lisp"           ; lol.terminal — the VT engine
    "initramfs/pty.lisp"          ; pseudo-terminals (real ones: terminal:CMD runs CMD)
    "initramfs/file-views.lisp"   ; folders and files as subjects
    "poc/shell.lisp"              ; the shell: panes, tabs, the status line, input
    "poc/file-manager.lisp"       ; the file manager's keys and prompts
    "poc/org-mode.lisp"           ; Org files: colour, the outline, links, keys
    "initramfs/runs.lisp"         ; run panes: a command's output, streamed (needs shell)
    "initramfs/containers.lisp"   ; containers as subjects (no lol-ct on the host: says so)
    "initramfs/json.lisp"         ; lol.json — the agent's wire format
    "initramfs/agent-wire.lisp"   ; SSE + the plain-http transport
    "initramfs/agent.lisp"        ; the agent: conversations, tools
    "initramfs/agent-ui.lisp"     ; a conversation as a Workspace pane, the tray item
    "poc/terminal-pane.lisp"      ; the terminal pane
    "poc/morphic.lisp"            ; the desk: the shell as a morph, trail windows
    "poc/overlays.lisp"           ; the modal cards as morphs (the Spotter, menus, halo)
    "initramfs/framebuffer.lisp"  ; draw-alien — a definition worth drilling to
    "initramfs/meminfo.lisp")     ; report-memory — likewise
  "The modules to load-record, in dependency order (a subset of build.sh's list).

   Because the registry is keyed by PROVENANCE, this list IS the module index the
   rendered frame shows — so it is a strict subset of the guest's: the modules left
   out below (fb, fb-browser, x11, repl, net, dhcp, process, supervisor) do not
   appear in a `modules` frame here, and their definitions cannot be named as a
   subject. Everything the shell itself draws is present, which is what matters for
   verifying pixels.")

(let ((*load-verbose* nil) (*load-print* nil))
  (handler-bind ((warning #'muffle-warning))
    ;; the session log first: agent.lisp names its locks to the stall watcher at
    ;; load time (WATCH-LOCKS), and without session-log.lisp that is an undefined
    ;; function — this harness stopped loading on 2026-10-16 until this line
    (load (merge-pathnames "poc/host-logging.lisp" *root-dir*))
    (load (merge-pathnames "initramfs/registry.lisp" *root-dir*))
    (dolist (m *stack*)
      (funcall 'load-recording (merge-pathnames m *root-dir*)))))

(in-package :cl-user)

;;; ---- command line --------------------------------------------------------

(defparameter *usage* "
render-frame.lisp — render one code-browser frame to a PPM, headless.

  sbcl --script poc/render-frame.lisp SUBJECT [-o FILE] [-s WxH] [-k \"KEYS\"]

SUBJECT — what the environment opens on (it becomes tab 1; a Workspace is tab 2,
exactly as open-environment does in the guest):

  modules                  the module index (the browser's default root)
  stage[:TICKS]            the COLLIDE stage, stepped TICKS times (default 240)
  module:NAME              one module's definitions   e.g. module:initramfs/model.lisp
  package:NAME             a package's flat definition list      e.g. package:CL-USER
  class:NAME               the class DAG + slots                  e.g. class:STREAM
  defn:NAME[:KIND]         a definition's source            e.g. defn:draw-alien
  gf:NAME                  a generic function's view                e.g. gf:collide
  matrix:NAME              the dispatch matrix               e.g. matrix:DOCUMENTATION
  condition:NAME           a condition's static face    e.g. condition:division-by-zero
  onion:NAME:C1,C2         the effective-method onion    e.g. onion:collide:ship,asteroid
  symbol:NAME              the inspector on a symbol's cells
  function:NAME            the FUNCTION object: captures, doors, disassembly
  workspace                the Workspace scratch buffer
  containers               the containers view (on the host: unavailable, and it says so)
  run:CMD                  a run pane: sh -c CMD on the host, run to its end
  tour                     the guided tour (initramfs/tour.lisp)
  changes                  the change set
  network                  the network view: links, routes, DNS, Wi-Fi (this host's state)
  terminal:CMD             a terminal running CMD (through /bin/sh -c) on a REAL
                           pty of this host, given ~1.5 s to draw  e.g. terminal:htop
  dir:PATH                 the file manager on a folder   e.g. dir:~/lisp-over-linux
  file:PATH                a file, drilled (an editor for text)    e.g. file:README.org
  org:PATH                 an Org file's outline (doc/org.org)     e.g. org:doc/org.org
                           (PATH: absolute, ~/..., or relative to the current
                           directory; a FILE opens beside its folder's listing so
                           the frame shows the file manager's split as well)

OPTIONS

  -o FILE     output path (default poc/out/frame.ppm)
  -s WxH      canvas size (default 1280x720; the QEMU guest is 1280x800)
  -k \"KEYS\"   replay a script before drawing, so a frame can be a DRILLED state:
              tokens are up down left right ret tab bs esc space pgup pgdn,
              a bare character, click:X,Y, drag:X1,Y1,X2,Y2 (press, move held,
              release), or wheel:N (+N lines toward the end) —
              each may take C- M- S- s- prefixes
              (Ctrl / Meta / Shift / Super).  e.g. -k \"down down ret S-ret\"
  --spotter Q draw the Spotter palette, filtered by Q, over the chosen subject
  --size N    the font's pixel size (default 18; 10..40), as SuperL-= sets it
  --font PATH a TrueType file, or a .lolf for the bitmap fallback (default:
              $LOL_FONT, else DejaVu Sans Mono, else poc/cozette.lolf)
  --halo X,Y[,D] open the middle-button halo at X,Y on layer D (0 = innermost) and
              list the layers under that point
  -h          this text
")

(defun starts-with (prefix string)
  (and (>= (length string) (length prefix)) (string= prefix string :end2 (length prefix))))

(defun split-on (char string)
  "STRING split on CHAR, empty pieces dropped."
  (let ((out '()) (start 0))
    (loop for i = (position char string :start start)
          do (push (subseq string start i) out)
             (if i (setf start (1+ i)) (return)))
    (remove "" (nreverse out) :test #'string=)))

(defun find-sym (name)
  "NAME (a command-line string) -> an existing symbol, never interned. First the
   shell's own RESOLVE-SYMBOL (pkg:name, else CL-USER then CL — so what you can
   Ctrl-click you can also name here); failing that, any package that EXPORTS the
   name, which is the convenience a command line wants: `gf:collide` rather than
   `gf:lol.examples:collide`. An ambiguous name is an error naming the candidates,
   never a silent pick."
  (or (resolve-symbol name)
      (let ((hits '()) (keyword (find-package :keyword)))
        (dolist (p (list-all-packages))
          ;; Skip KEYWORD: every keyword is external there, so a name that happens to
          ;; also exist as a keyword would look "ambiguous" and refuse to resolve.
          ;; `onion:collide:ship,asteroid` started failing the moment something
          ;; interned :ASTEROID — nothing to do with the class of the same name.
          (unless (eq p keyword)
            (multiple-value-bind (sym status) (find-symbol (string-upcase name) p)
              (when (and sym (eq status :external)) (pushnew sym hits)))))
        (cond ((null (rest hits)) (first hits))
              (t (error "~a is exported by several packages: ~{~s~^, ~}" name hits))))
      (error "no such symbol: ~a (it must already exist in the image — note this ~
              harness loads only part of the guest's module list)" name)))

(defun find-class-sym (name)
  "NAME -> a symbol that actually NAMES A CLASS. FIND-SYM alone is not enough for a
   class argument: it returns the first external symbol of that name in any package,
   and once enough code is loaded a name like SHIP or ASTEROID exists in several —
   most of them not classes. So filter by what the caller actually needs."
  (let ((direct (resolve-symbol name)))
    (if (and direct (find-class direct nil))
        direct
        (let ((hits '()) (keyword (find-package :keyword)))
          (dolist (p (list-all-packages))
            (unless (eq p keyword)
              (multiple-value-bind (sym status) (find-symbol (string-upcase name) p)
                (when (and sym (eq status :external) (find-class sym nil))
                  (pushnew sym hits)))))
          (cond ((null hits) (error "no class named ~a" name))
                ((null (rest hits)) (first hits))
                (t (error "~a names a class in several packages: ~{~s~^, ~}" name hits)))))))

(defun demo-conversation ()
  "A finished conversation built by hand — no network — so the agent's pane can be
   looked at headless: a question, thinking, a tool call, a reply with live code."
  (let* ((c (make-conversation :title "why does draw-alien flip?"))
         (call (make-instance 'tool-call :id "toolu_demo" :number 0 :name "lisp-source"
                                         :input (lol.json:jobj "name" "draw-alien"))))
    (setf (call-status call) :done (call-summary call) "1 definition"
          (call-subject call) (subj-defn 'draw-alien) (call-result call) "(source)")
    (vector-push-extend call (conversation-calls c))
    (flet ((blk (&rest kv) (make-instance 'content-block :json (apply #'lol.json:jobj kv))))
      (vector-push-extend (make-turn :question "why does draw-alien flip the rows?" :start 0 :done t
                                     :usage (list :input 2140 :output 312 :cache-read 1800
                                                  :cache-write 0 :cost 0.0141d0))
                          (conversation-turns c))
      (append-message c "user" (list (blk "type" "text" "text" "why does draw-alien flip the rows?")))
      (append-message c "assistant"
                      (list (blk "type" "thinking" "thinking" "Look at the blit loop first." "signature" "s")
                            (let ((b (blk "type" "tool_use" "id" "toolu_demo" "name" "lisp-source"
                                          "input" (lol.json:jobj "name" "draw-alien"))))
                              (setf (block-call b) call) b)))
      (append-message c "user" (list (blk "type" "tool_result" "tool_use_id" "toolu_demo" "content" "(source)")))
      (append-message c "assistant"
                      (list (blk "type" "text" "text"
                                 (format nil "It does not flip them: alien.rgba is stored top-to-bottom, the same order /dev/fb0 scans, so each row is copied to y + row as it comes. If you wanted it upside down, this would do it:~%~%```lisp~%(defun alien-row-y (y0 row) (- (+ y0 149) row))~%```~%~%C-c C-c on that form compiles it into the image."))))
      (setf (conversation-cost c) 0.0141d0))
    c))

(defun parse-subject (spec)
  "The SUBJECT command-line word -> a subject object for OPEN-ENVIRONMENT."
  (let* ((colon (position #\: spec))
         (head  (string-downcase (if colon (subseq spec 0 colon) spec)))
         (rest  (and colon (subseq spec (1+ colon)))))
    (cond
      ((string= head "modules")   (subj-modules))
      ;; the stage (§3b) is the one subject with STATE: it has to be stepped before it
      ;; is worth a picture, and its RNG is seeded so N ticks render identically here
      ;; and in the guest. "stage" alone means 240 ticks (four seconds at sixty a tick).
      ((string= head "stage")
       (let ((st (reset-collide-stage))
             (n (if rest (parse-integer rest) 240)))
         (setf (stage-w st) 700 (stage-h st) 380)
         (dotimes (i n) (step-stage st))
         (subj-stage 'lol.examples:collide)))
      ((string= head "workspace") (subj-workspace))
      ;; containers / run:CMD (doc/containers.org). The host has no lol-ct, so the
      ;; containers view says so; a run is a HOST command (sh -c CMD), run to the end
      ;; first, so the frame shows its whole coloured output and its exit line.
      ((string= head "containers") (subj-containers))
      ((and (string= head "run") rest)
       (let ((run (start-run (list "sh" "-c" rest) :title rest)))
         (loop while (run-live-p run) do (sleep 0.05))
         run))
      ((string= head "tour")      (subj-tour))        ; initramfs/tour.lisp
      ;; dir:PATH / file:PATH — the file manager (doc/file-manager.org). The path is
      ;; resolved like a prompt's: ~ is $HOME, a relative path is relative to where
      ;; the harness was started.
      ((and (member head '("dir" "file" "org") :test #'string=) rest)
       (let ((path (fm-resolve rest (string-right-trim "/" (sb-ext:native-namestring
                                                               *default-pathname-defaults*)))))
         (cond ((string= head "dir") (subj-dir path))
               ((string= head "org") (subj-org path))     ; poc/org-mode.lisp
               (t (subj-file path)))))
      ((string= head "changes")   (subj-change-set))
      ((string= head "agent")     (subj-agent))
      ((string= head "conversation") (demo-conversation))
      ;; network — the network view (doc/networking.org). Its modules are loaded only
      ;; when asked, with plain LOAD, so every other frame's module index is unchanged.
      ;; It shows THIS host's interfaces and routes; Wi-Fi rows need a supplicant and
      ;; show the no-radio / not-started states.
      ((string= head "network")
       (require :sb-bsd-sockets)
       (handler-bind ((warning #'muffle-warning))
         (dolist (m '("net" "dhcp" "dns" "sntp" "tls" "http" "wifi" "net-views"))
           (load (merge-pathnames (format nil "initramfs/~a.lisp" m) *root-dir*))))
       (funcall 'subj-network))
      ;; terminal:CMD — a real program on a real pty of THIS host (doc/terminal.org)
      ((and (string= head "terminal") rest)
       (%subj-terminal :argv (list "/bin/sh" "-c" rest) :title rest
                       :env (list (format nil "PATH=~a" (or (sb-ext:posix-getenv "PATH") "/usr/bin:/bin"))
                                  (format nil "LANG=~a" (or (sb-ext:posix-getenv "LANG") "C.UTF-8")))))
      ((null rest) (error "subject ~s needs an argument — see -h" spec))
      ((string= head "package")
       (or (find-package (string-upcase rest)) (error "no such package: ~a" rest)))
      ((string= head "module")
       ;; Module keys are whatever CATEGORIZE :file produced — "directory/name", with
       ;; NO extension. Match the real index rather than guessing, accepting a typed
       ;; ".lisp" as a courtesy, and fail loudly (listing what exists) rather than
       ;; fabricating a subject that would render as an empty pane.
       (let* ((want (if (and (> (length rest) 5)
                             (string-equal ".lisp" (subseq rest (- (length rest) 5))))
                        (subseq rest 0 (- (length rest) 5))
                        rest))
              (index (module-index))
              (hit (find want index :key (lambda (e) (princ-to-string (car e)))
                                    :test #'string-equal)))
         (unless hit
           (error "no such module: ~a~%  loaded modules: ~{~a~^ ~}"
                  rest (mapcar (lambda (e) (princ-to-string (car e))) index)))
         (subj-module (car hit))))
      ((string= head "class")
       (or (find-class (find-class-sym rest) nil) (error "not a class: ~a" rest)))
      ((string= head "gf")
       (let ((f (fdefinition (find-sym rest))))
         (unless (typep f 'generic-function) (error "not a generic function: ~a" rest))
         f))
      ((string= head "matrix") (subj-matrix (find-sym rest)))
      ((string= head "condition") (subj-condition (find-sym rest)))
      ((string= head "symbol") (find-sym rest))
      ;; function:NAME — the FUNCTION object behind a name, which is a different
      ;; subject from the symbol (whose view shows its cells) and from defn:NAME
      ;; (whose view shows source). This is the one that opens the closure inspector.
      ((string= head "function")
       (let ((sym (find-sym rest)))
         (unless (fboundp sym) (error "not fbound: ~a" rest))
         (fdefinition sym)))
      ((string= head "defn")
       (let ((parts (split-on #\: rest)))
         (subj-defn (find-sym (first parts))
                    (and (second parts) (intern (string-upcase (second parts)) :keyword)))))
      ((string= head "onion")
       ;; onion:GF:C1,C2 — the tuple is class NAMES, as effective-method-onion wants.
       (let* ((parts (split-on #\: rest))
              (gf    (find-sym (first parts)))
              (tuple (mapcar #'find-class-sym (split-on #\, (or (second parts) "")))))
         (unless tuple (error "onion needs a class tuple: onion:collide:ship,asteroid"))
         (subj-onion gf tuple)))
      (t (error "unknown subject kind ~s — see -h" head)))))

;;; ---- the key script ------------------------------------------------------
;;; Enough to reach any pane a human could reach, and no more. Reusing BROWSER-KEY
;;; and BROWSER-CLICK (rather than poking *trails* directly) is the point: the frame
;;; then shows a state the real shell can actually get into.

(defparameter *key-names*
  '(("up" . :up) ("down" . :down) ("left" . :left) ("right" . :right)
    ("ret" . :return) ("enter" . :return) ("tab" . :tab) ("bs" . :backspace)
    ("esc" . :escape) ("space" . #\Space) ("home" . :home) ("end" . :end)
    ("pgup" . :page-up) ("pgdn" . :page-down) ("del" . :delete)))

(defun parse-key-token (tok)
  "One script token -> (values KEY ARG STATE). KEY is what BROWSER-KEY wants, or
   :CLICK with ARG the (x . y) for BROWSER-CLICK; ARG is NIL for a plain key. The
   arity is uniform on purpose — returning two values for a key and three for a
   click made STATE arrive as NIL and every scripted keystroke die in LOGBITP.
   Modifier prefixes stack: C- Ctrl, M- Meta, S- Shift, s- Super."
  (let ((state 0))
    (loop
      (cond ((starts-with "C-" tok) (setf state (logior state 4) tok (subseq tok 2)))
            ((starts-with "M-" tok) (setf state (logior state 8) tok (subseq tok 2)))
            ((starts-with "S-" tok) (setf state (logior state 1) tok (subseq tok 2)))
            ((starts-with "s-" tok) (setf state (logior state 64) tok (subseq tok 2)))
            (t (return))))
    (cond
      ((starts-with "wheel:" tok)
       ;; a wheel notch count: + scrolls toward the end. The real wheel arrives as
       ;; evdev/X11 events the harness has no way to synthesise, so replay the line
       ;; count BROWSER-SCROLL would have been handed.
       (values :wheel (parse-integer (subseq tok 6)) state))
      ((starts-with "drag:" tok)
       (values :drag (mapcar #'parse-integer (split-on #\, (subseq tok 5))) state))
      ((starts-with "click:" tok)
       (let ((xy (split-on #\, (subseq tok 6))))
         (values :click (cons (parse-integer (first xy)) (parse-integer (second xy))) state)))
      ((cdr (assoc (string-downcase tok) *key-names* :test #'string=))
       (values (cdr (assoc (string-downcase tok) *key-names* :test #'string=)) nil state))
      ((= 1 (length tok)) (values (char tok 0) nil state))
      (t (error "unknown key token ~s — see -h" tok)))))

(defun pump-terminals (seconds)
  "Give every open terminal's program SECONDS to write, feeding its engine — the
   guest's poll loop, done by the clock. A no-op without terminals."
  (when *terminals*
    (let ((deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
      (loop while (< (get-internal-real-time) deadline)
            do (dolist (s (copy-list *terminals*)) (ignore-errors (service-one s)))
               (sleep 0.05)))))

(defun replay-keys (script canvas w h)
  "Replay SCRIPT (space-separated tokens) through the real input entry points — the
   world's event routing (poc/morph.lisp), exactly as the guest's loop feeds it. The
   world is repaired between steps because clicks hit-test against geometry the
   drawing records (a pane's PANE-ROWS, PANE-BODY-TOP and PANE-CELLS) — without a
   draw in between, a scripted click would test against a stale layout, which is
   precisely the class of bug this harness exists to catch."
  (declare (ignore w h))
  (dolist (tok (split-on #\Space script))
    (multiple-value-bind (key arg state) (parse-key-token tok)
      (lol.morph:world-repair *world* canvas)
      (flet ((send (kind x y &optional (st state))
               (lol.morph:world-dispatch *world* (lol.morph:make-event :kind kind :x x :y y
                                                                       :state st))))
        (case key
          ;; a click is a press and its release: a press on a row that opens
          ;; something acts on the release (a press that moves picks it up instead —
          ;; doc/morphic.org §13)
          (:click (send :down (car arg) (cdr arg))
                  (send :up (car arg) (cdr arg)))
          ;; drag:X1,Y1,X2,Y2 — press, move with the button held, release: a text
          ;; selection, or a window carried by its title bar (doc/morphic.org)
          (:drag (destructuring-bind (x1 y1 x2 y2) arg
                   (send :down x1 y1)
                   (lol.morph:world-repair *world* canvas)
                   (send :drag x2 y2 0)
                   (lol.morph:world-repair *world* canvas)
                   (send :up x2 y2 0)))
          ;; the wheel token counts LINES, as BROWSER-SCROLL does (not notches): a whole
          ;; number of notches goes through the world, at the middle of the screen
          (:wheel (if (zerop (mod arg +wheel-lines+))
                      (lol.morph:world-dispatch
                       *world* (lol.morph:make-event
                                :kind :wheel :notches (- (floor arg +wheel-lines+)) :state state
                                :x (floor (third (lol.morph:morph-bounds *world*)) 2)
                                :y (floor (fourth (lol.morph:morph-bounds *world*)) 2)))
                      ;; what scrolled: the current pane's body
                      (progn (browser-scroll arg)
                             (lol.morph:invalidate (or (pane-body (current-pane)) *world*)))))
          (t (lol.morph:world-dispatch *world* (lol.morph:make-event :kind :key :key key
                                                                     :state state)))))
      (pump-terminals 0.3))))

;;; ---- PPM out -------------------------------------------------------------

(defun write-ppm (canvas path)
  "Dump CANVAS as a binary PPM (P6). The canvas words are #x00RRGGBB, so each pixel
   is three byte extractions — the same words the framebuffer receives verbatim, just
   unpacked into a format anything can open."
  (ensure-directories-exist path)
  (let* ((w (lol.canvas:canvas-w canvas))
         (h (lol.canvas:canvas-h canvas))
         (px (lol.canvas:canvas-pixels canvas))
         (header (format nil "P6~%~d ~d~%255~%" w h))
         (bytes (make-array (* w h 3) :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 32) (*)) px))
    (loop for i of-type fixnum below (* w h)
          for o of-type fixnum from 0 by 3
          for v of-type (unsigned-byte 32) = (aref px i)
          do (setf (aref bytes o)       (ldb (byte 8 16) v)
                   (aref bytes (+ o 1)) (ldb (byte 8 8) v)
                   (aref bytes (+ o 2)) (ldb (byte 8 0) v)))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede :if-does-not-exist :create)
      (write-sequence (map '(vector (unsigned-byte 8)) #'char-code header) s)
      (write-sequence bytes s))
    path))

;;; ---- main ----------------------------------------------------------------

(defun main (argv)
  (let ((subject-spec nil)
        (out (merge-pathnames "out/frame.ppm" *poc-dir*))
        (w 1280) (h 720) (script nil) (size nil) (font-path nil))
    (setf *spotter-query* nil)
    (loop while argv
          for a = (pop argv)
          do (cond
               ((string= a "-h") (write-string *usage*) (return-from main 0))
               ((string= a "-o") (setf out (pop argv)))
               ((string= a "-k") (setf script (pop argv)))
               ;; --size N: the font's pixel size (SuperL-= in the guest); --font PATH
               ((string= a "--size") (setf size (parse-integer (pop argv))))
               ((string= a "--font") (setf font-path (pop argv)))
               ;; --spotter QUERY renders the Spotter palette over the chosen subject,
               ;; which is otherwise reachable only by booting the guest
               ((string= a "--spotter") (setf *spotter-query* (or (pop argv) "")))
               ((string= a "--halo")
                (setf *halo-at* (mapcar #'parse-integer (split-on #\, (pop argv)))))
               ((string= a "-s")
                (let ((parts (split-on #\x (string-downcase (pop argv)))))
                  (setf w (parse-integer (first parts)) h (parse-integer (second parts)))))
               ((null subject-spec) (setf subject-spec a))
               (t (error "unexpected argument ~s — see -h" a))))
    (unless subject-spec (write-string *usage*) (return-from main 1))
    ;; the font: --font PATH (a .ttf, or a .lolf for the bitmap fallback), else
    ;; $LOL_FONT, else the host's DejaVu Sans Mono, else the committed bitmap — the
    ;; same order build.sh uses to choose what the image ships
    (set-browser-font
     (if (and font-path (search ".lolf" font-path))
         (let ((f (lol.canvas:load-font font-path))) (when size (lol.canvas:set-font-size f size)) f)
         (open-browser-font (list font-path (sb-ext:posix-getenv "LOL_FONT")
                                  "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf")
                            (merge-pathnames "cozette.lolf" *poc-dir*) :size size)))
    (let ((canvas (lol.canvas:make-canvas w h)))
      (open-environment (parse-subject subject-spec))
      (setf *world* (make-desk w h))        ; the tiled shell as one morph (morphic.lisp)
      (when *terminals*                 ; a terminal: size it (a first draw), let it run
        (draw-browser canvas w h)
        (pump-terminals 1.5))
      (when script (replay-keys script canvas w h))
      (if *spotter-query*
          ;; the Spotter draws the browser itself, as its backdrop
          (let* ((cands (spotter-candidates))
                 (hits (spotter-results *spotter-query* cands)))
            (draw-spotter-card canvas w h *spotter-query* hits 0))
          ;; the whole world, freshly: the shell's morphs and any windows
          (progn (lol.morph:invalidate *world*)
                 (lol.morph:world-repair *world* canvas)))
      (when *halo-at*
        ;; the halo walks the morph tree under the point, as it does in the guest
        (destructuring-bind (hx hy &optional (depth 0)) *halo-at*
          (let* ((layers (halo-layers hx hy))
                 (depth (min depth (1- (length layers)))))
            (loop for l in layers for i from 0
                  do (format t "~&  ~:[ ~;*~] ~d ~a — ~a~%" (= i depth) i (getf l :name)
                             (getf l :title)))
            (let ((entries (halo-entries (nth depth layers))))
              (format t "~&  handles: ~{~a~^ · ~}~%" (mapcar (lambda (e) (getf e :label)) entries))
              (draw-halo canvas w h layers depth entries 0 hx hy)))))
      (write-ppm canvas out)
      (mapc #'terminal-close (copy-list *terminals*))
      (format t "~&~a  ~dx~d  ~a~@[  keys: ~a~]~%"
              out w h (view-title (pane-view (current-pane))) script))
    0))

;; A host test may LOAD this file for its stack and helpers (poc/morphic-test.lisp)
;; by binding CL-USER::*RENDER-FRAME-LIBRARY* first: then there is no main.
(unless (and (boundp '*render-frame-library*) (symbol-value '*render-frame-library*))
  (handler-case (sb-ext:exit :code (main (rest sb-ext:*posix-argv*)))
    (serious-condition (c)
      (format *error-output* "~&render-frame: ~a~%" c)
      (sb-ext:exit :code 1))))
