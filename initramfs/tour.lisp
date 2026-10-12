;;;; tour.lisp — a guided walk through the running system, inside the system.
;;;;
;;;; The HTML guide (doc/guide/, indexed by doc/tour.org) explains how this machine
;;;; works with diagrams and file:line references. This file is its in-environment
;;;; twin, and it makes the Smalltalk point instead of only stating it: the system
;;;; can show you its own code, so a tour of it is simply a LIST OF ITS OWN
;;;; DEFINITIONS, in reading order. Each stop drills into the real, live definition
;;;; — the captured source the image is running (registry.lisp), editable and
;;;; Acceptable like any other — not into a copy written for the tour.
;;;;
;;;;   SuperL-Space, type "tour", Enter.   Down/Up walk the stops: the right-hand
;;;;   column previews each one's source (the shell's nav split) as you move.
;;;;   Enter opens a stop; C-g comes back. (show-view (subj-tour)) at a REPL.
;;;;
;;;; The whole module is DATA (*TOUR-STOPS*) plus one PRESENT method. A stop names
;;;; its symbol by STRINGS — package and name — and is resolved only when the tour
;;;; is shown, because this file loads right after present.lisp and most of what it
;;;; points at (lol.canvas, the shell, fb-browser) is loaded later. A name that does
;;;; not resolve (a renamed function, a module this build left out — render-frame
;;;; loads only part of the stack) becomes a row that SAYS so rather than an error:
;;;; a tour must never be the thing that breaks the browser.
;;;;
;;;; Two stops are not Lisp (preinit.c, build.sh). The image has no copy of them,
;;;; but the repository it was built from is on the laptop's disk, under the
;;;; mounted /home (doc/storage.org) — so those stops open the real file through
;;;; the file manager when it is there, and say where it would be when it is not.

(in-package :cl-user)

(defvar *tour-root*
  (ignore-errors
    ;; Frozen at BUILD time, like sbcl-source.lisp's *SBCL-SOURCE-ROOT*: the
    ;; directory this file was loaded from, one level up = the repository. On the
    ;; laptop the same path exists again under /home; under QEMU with the stand-in
    ;; disk it usually does not.
    (namestring (truename (merge-pathnames "../" (make-pathname :name nil :type nil
                                                                :defaults *load-truename*)))))
  "The repository this image was built from (ends in /), or NIL.")

(defparameter *tour-stops*
  ;; (CHAPTER  WHAT  NOTE)
  ;; WHAT is (PACKAGE NAME [KIND]) for a definition, or (:file "relative/path").
  ;; Keep it in step with doc/guide/paths.html — the same walk, on paper.
  '(("1 · boot: firmware to the environment"
     (:file "initramfs/preinit.c")        "PID 1 in C: mounts, finds the disk, drops to the user, execs the Lisp")
    ("1 · boot: firmware to the environment"
     ("CL-USER" "INIT-TOPLEVEL")          "the saved image's entry point: argv picks the role")
    ("1 · boot: firmware to the environment"
     ("CL-USER" "SUPERVISOR-MAIN")        "PID 1 in Lisp: diagnostics, DHCP, then straight into the environment")
    ("1 · boot: firmware to the environment"
     ("CL-USER" "RUN-BROWSER-FB")         "the environment's loop: draw, wait in poll(2), dispatch")
    ("2 · build: how the source got into the image"
     (:file "build.sh")                   "LISP_SOURCES (load order), save-lisp-and-die, the cpio")
    ("2 · build: how the source got into the image"
     ("CL-USER" "LOAD-RECORDING")         "load a module AND file its verbatim source in the image")
    ("2 · build: how the source got into the image"
     ("CL-USER" "*REGISTRY*" :variable)   "name -> DEFN records: what every source view reads")
    ("3 · present: subjects become views"
     ("CL-USER" "PRESENT")                "the one generic function: subject -> VIEW of ITEMs")
    ("3 · present: subjects become views"
     ("CL-USER" "COMMANDS-FOR")           "what a subject affords: menu, halo, key")
    ("3 · present: subjects become views"
     ("CL-USER" "OPEN-PANE")              "a subject becomes a PANE: present it, wrap text in an editor")
    ("3 · present: subjects become views"
     ("CL-USER" "TRAIL-PUSH")             "drilling = pushing a pane onto the trail")
    ("4 · pixels: a view becomes a frame"
     ("CL-USER" "DRAW-BROWSER")           "the whole frame: tabs, breadcrumbs, the pane, the status line")
    ("4 · pixels: a view becomes a frame"
     ("LOL.WIDGETS" "DRAW-LIST")          "rows, sections, selection, scrollbar; returns a hit map")
    ("4 · pixels: a view becomes a frame"
     ("LOL.CANVAS" "DRAW-STRING")         "text: TrueType glyphs, cached per size, blended into the pixel vector")
    ("4 · pixels: a view becomes a frame"
     ("LOL.FB" "PRESENT-FB")              "the canvas IS the screen's format: one pwrite to /dev/fb0")
    ("5 · input: a key becomes a command"
     ("CL-USER" "FBB-WAIT")               "poll(2) over the tty and every keyboard and pointer, hotplug included")
    ("5 · input: a key becomes a command"
     ("CL-USER" "FB-READ-KEY")            "raw tty bytes -> a key and a modifier mask")
    ("5 · input: a key becomes a command"
     ("CL-USER" "READ-MOUSE")             "24-byte input_event records -> pointer, clicks, wheel")
    ("5 · input: a key becomes a command"
     ("CL-USER" "BROWSER-KEY")            "one key, routed by modifier and by the kind of pane")
    ("6 · live: changing the running system"
     ("CL-USER" "JUMP-AT-POINT")          "M-.: the symbol under the caret -> its definition")
    ("6 · live: changing the running system"
     ("CL-USER" "WORKSPACE-DO-IT")        "C-x C-e: read the form before point, eval it, show the value")
    ("6 · live: changing the running system"
     ("CL-USER" "ACCEPT-SOURCE")          "C-c C-c: compile into the live image, keep the old version")
    ("6 · live: changing the running system"
     ("CL-USER" "CHANGED-DEFINITIONS")    "the change set: everything Accepted since boot")
    ("6 · live: changing the running system"
     ("CL-USER" "RUN-DEBUGGER-FB")        "a condition with its stack still standing: restarts, frames")
    ("7 · the model: Lisp describing itself"
     ("CL-USER" "DISPATCH-MATRIX")        "which class pairs a generic function has a method for")
    ("7 · the model: Lisp describing itself"
     ("CL-USER" "RETENTION-PATH")         "why is this object alive? the GC's own answer")
    ("8 · the machine around it"
     ("CL-USER" "CONFIGURE-ETH0")         "DHCP in pure Lisp, then ioctls to bring eth0 up")
    ("8 · the machine around it"
     ("CL-USER" "POWER-OFF")              "hand the disk back read-only, then reboot(2)")
    ("8 · the machine around it"
     ("CL-USER" "*TOUR-STOPS*" :variable) "you are here: this tour is just a list in the image"))
  "The tour, in reading order: (CHAPTER WHAT NOTE) per stop. See the file header.")

(defstruct (subj-tour (:constructor subj-tour ())))

(defun tour-stop-subject (what)
  "The subject a stop drills into, or (values NIL WHY) when it cannot be shown here."
  (if (eq (first what) :file)
      (let ((path (and *tour-root* (concatenate 'string *tour-root* (second what)))))
        (cond ((null path) (values nil "repository path unknown"))
              ((not (probe-file path)) (values nil (format nil "not on this disk: ~a" path)))
              ((fboundp 'subj-file) (funcall 'subj-file path))
              (t (values nil "the file manager (file-views.lisp) is not loaded"))))
      (destructuring-bind (pkg name &optional kind) what
        (let* ((p (find-package pkg))
               (sym (and p (find-symbol name p))))
          (cond ((null sym) (values nil (format nil "~a::~a is not in this image" pkg name)))
                ;; a generic function opens its own view (methods, dispatch matrix),
                ;; not one source blob of every method on it
                ((and (null kind) (fboundp sym) (typep (fdefinition sym) 'generic-function))
                 (fdefinition sym))
                ((or (fboundp sym) (boundp sym) kind) (subj-defn sym kind))
                (t (values nil (format nil "~(~a~) is not defined in this image" name))))))))

(defun tour-stop-kind (what subject)
  "The concept a stop's row is tinted by, so the tour reads like every other list."
  (cond ((eq (first what) :file) nil)          ; a plain file: untinted, as in the file manager
        ((typep subject 'generic-function) :generic-function)
        ((and (subj-defn-p subject) (subj-defn-kind subject)))
        ((and (subj-defn-p subject) (macro-function (subj-defn-name subject))) :macro)
        (t :function)))

(defmethod present ((s subj-tour))
  (make-view
   :title "tour — the running system, stop by stop (dn: preview · ret: open · C-g: back)"
   :kind :list :subject s
   :items (loop for (chapter what note) in *tour-stops*
                for i from 1
                collect (multiple-value-bind (subject why)
                            (ignore-errors (tour-stop-subject what))
                          (make-item
                           ;; a path for a file stop, the symbol's name otherwise
                           :label (format nil "~2,'0d  ~(~a~)" i (second what))
                           :detail (if subject note (format nil "~a  [~a]" note (or why "unavailable")))
                           :kind (and subject (ignore-errors (tour-stop-kind what subject)))
                           :group chapter
                           :subject subject)))))

;; The Spotter offers the tour as a surface, next to the Workspace and the change
;; set (present.lisp's *SPOTTER-SOURCES* hook — the same way file-views.lisp adds
;; "files" and "home" without present.lisp naming them).
(pushnew (lambda ()
           (list (list :label "tour" :detail "surface · a guided walk through this system's code"
                       :group "surfaces" :subject (subj-tour))))
         *spotter-sources*)
