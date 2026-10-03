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
;;;; optionally replays a script of keystrokes and clicks, calls the real
;;;; DRAW-BROWSER into a canvas, and dumps the pixels as a PPM.
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
  '("initramfs/model.lisp"        ; CLOS/introspection model
    "initramfs/present.lisp"      ; the present / commands-for protocol
    "initramfs/examples.lisp"     ; COLLIDE — a real 2-axis dispatch matrix to render
    "initramfs/stage.lisp"        ; the COLLIDE stage (§3b) — bodies, dispatch, enact
    "poc/canvas.lisp"             ; the pixel buffer, the font, draw-string
    "poc/textview.lisp"           ; the editable, syntax-colouring text view
    "poc/widgets.lisp"            ; phase 3 toolkit: panel / list / scrollbar / grid
    "initramfs/ansi.lisp"         ; needed by line-editor
    "initramfs/line-editor.lisp"  ; token-start / longest-common-prefix, used by the shell
    "poc/clipboard.lisp"          ; lol.clipboard — the shell's copy / paste / rectangles
    "initramfs/sysinfo.lisp"      ; the systray's battery + clock
    "poc/inflate.lisp"            ; lol.inflate — gzip previews
    "poc/image.lisp"              ; lol.image — image previews
    "initramfs/files.lisp"        ; the file system as data
    "initramfs/file-views.lisp"   ; folders and files as subjects
    "poc/shell.lisp"              ; draw-browser itself
    "poc/file-manager.lisp"       ; the file manager's keys and prompts
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
  changes                  the change set
  dir:PATH                 the file manager on a folder   e.g. dir:~/lisp-over-linux
  file:PATH                a file, drilled (an editor for text)    e.g. file:README.org
                           (PATH: absolute, ~/..., or relative to the current
                           directory; a FILE opens beside its folder's listing so
                           the frame shows the file manager's split as well)

OPTIONS

  -o FILE     output path (default poc/out/frame.ppm)
  -s WxH      canvas size (default 1280x720; the QEMU guest is 1280x800)
  -k \"KEYS\"   replay a script before drawing, so a frame can be a DRILLED state:
              tokens are up down left right ret tab bs esc space pgup pgdn,
              a bare character, click:X,Y, or wheel:N (+N lines toward the end) —
              each may take C- M- S- s- prefixes
              (Ctrl / Meta / Shift / Super).  e.g. -k \"down down ret S-ret\"
  --spotter Q draw the Spotter palette, filtered by Q, over the chosen subject
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
      ;; dir:PATH / file:PATH — the file manager (doc/file-manager.org). The path is
      ;; resolved like a prompt's: ~ is $HOME, a relative path is relative to where
      ;; the harness was started.
      ((and (member head '("dir" "file") :test #'string=) rest)
       (let ((path (fm-resolve rest (string-right-trim "/" (sb-ext:native-namestring
                                                               *default-pathname-defaults*)))))
         (if (string= head "dir")
             (subj-dir path)
             (subj-file path))))
      ((string= head "changes")   (subj-change-set))
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
      ((starts-with "click:" tok)
       (let ((xy (split-on #\, (subseq tok 6))))
         (values :click (cons (parse-integer (first xy)) (parse-integer (second xy))) state)))
      ((cdr (assoc (string-downcase tok) *key-names* :test #'string=))
       (values (cdr (assoc (string-downcase tok) *key-names* :test #'string=)) nil state))
      ((= 1 (length tok)) (values (char tok 0) nil state))
      (t (error "unknown key token ~s — see -h" tok)))))

(defun replay-keys (script canvas w h)
  "Replay SCRIPT (space-separated tokens) through the real input entry points. The
   canvas is redrawn between steps because clicks hit-test against geometry that
   DRAW-BROWSER records (*crumb-rects*, *content-top*, *matrix-cells*) — without a
   draw in between, a scripted click would test against a stale layout, which is
   precisely the class of bug this harness exists to catch."
  (dolist (tok (split-on #\Space script))
    (multiple-value-bind (key arg state) (parse-key-token tok)
      (draw-browser canvas w h)
      (case key
        (:click (browser-click (car arg) (cdr arg)
                               (lol.canvas:ctrl-p state) (lol.canvas:super-p state)))
        (:wheel (browser-scroll arg))
        (t (browser-key key state))))))

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
        (w 1280) (h 720) (script nil))
    (setf *spotter-query* nil)
    (loop while argv
          for a = (pop argv)
          do (cond
               ((string= a "-h") (write-string *usage*) (return-from main 0))
               ((string= a "-o") (setf out (pop argv)))
               ((string= a "-k") (setf script (pop argv)))
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
    (set-browser-font (lol.canvas:load-font (merge-pathnames "cozette.lolf" *poc-dir*)))
    (let ((canvas (lol.canvas:make-canvas w h)))
      (open-environment (parse-subject subject-spec))
      (when script (replay-keys script canvas w h))
      (if *spotter-query*
          ;; the Spotter draws the browser itself, as its backdrop
          (let* ((cands (spotter-candidates))
                 (hits (spotter-results *spotter-query* cands)))
            (draw-spotter-card canvas w h *spotter-query* hits 0))
          (draw-browser canvas w h))
      (when *halo-at*
        ;; the halo reads *DRAWN* from the frame just drawn, as it does in the guest
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
      (format t "~&~a  ~dx~d  ~a~@[  keys: ~a~]~%"
              out w h (view-title (pane-view (current-pane))) script))
    0))

(handler-case (sb-ext:exit :code (main (rest sb-ext:*posix-argv*)))
  (serious-condition (c)
    (format *error-output* "~&render-frame: ~a~%" c)
    (sb-ext:exit :code 1)))
