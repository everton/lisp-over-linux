;;;; stage-workspace-test.lisp — host test: the Workspace beside the stage reads in
;;;; LOL.STAGE (doc/code-browser.org §3b).
;;;;
;;;;     sbcl --script poc/stage-workspace-test.lisp     ; exits 0 if every check passes
;;;;
;;;; The stage's Workspace view names its package (VIEW-PACKAGE); the shell binds it as
;;;; *PACKAGE* around a key (PANE-PACKAGE in BROWSER-KEY), so every verb that reads the
;;;; buffer agrees about a bare word. Driven through the real keys (render-frame's
;;;; REPLAY-KEYS, the world's event routing): w opens it, C-x C-e Do it, C-c C-p Print
;;;; it, C-c C-c Accept, Tab completes. And the ordinary Workspace still reads in
;;;; CL-USER.
;;;;
;;;; Loads poc/render-frame.lisp as a library for the real stack and its key parser.

(defvar cl-user::*render-frame-library* t)
(load (merge-pathnames "render-frame.lisp" (make-pathname :directory (pathname-directory *load-truename*))))

(in-package :cl-user)

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (name ok)
  (incf *checks*)
  (format t "~&  ~70a ~:[FAIL~;PASS~]~%" name ok)
  (unless ok (incf *fails*)))

(set-browser-font (open-browser-font (list "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf")
                                     (merge-pathnames "cozette.lolf" *poc-dir*)))
;; Tab asks *COMPLETER*, which repl.lisp sets in the image; render-frame does not load it
(unless *completer* (load (merge-pathnames "../initramfs/repl.lisp" *poc-dir*)))

(defparameter *canvas* (lol.canvas:make-canvas 1280 720))
(open-environment (parse-subject "stage:0"))
(setf *world* (make-desk 1280 720))
(setf (lol.stage:stage-running (lol.stage:collide-stage)) nil)   ; a still world
(replay-keys "w" *canvas* 1280 720)
(defparameter *side* (pane-side (current-pane)))

(defun run (text keys)
  "Put TEXT in the stage's Workspace, the caret at its end, then replay KEYS; return
   the status line."
  (let ((tv (pane-tv *side*)))
    (lol.textview:tv-set-text tv text)
    (let ((lines (lol.textview:tv-lines tv)))
      (lol.textview:tv-move-point tv (1- (length lines)) (length (aref lines (1- (length lines))))))
    (setf *browser-status* nil)
    (replay-keys keys *canvas* 1280 720)
    *browser-status*))

(format t "~&the stage's Workspace~%")
(check "w opens the Workspace beside the stage" (and *side* (workspace-pane-p *side*)))
(check "its package is LOL.STAGE" (eq (pane-package *side*) (find-package :lol.stage)))

;; every form of its primer, as written, by Do it — the primer is bare words now
(let ((text lol.stage::*stage-workspace-text*) (pos 0) (n 0))
  (loop (multiple-value-bind (form next)
            (let ((*package* (find-package :lol.stage))) (read-from-string text nil :eof :start pos))
          (when (eq form :eof) (return))
          (let ((st (run (subseq text pos next) "C-c C-e")))
            (incf n)
            (check (format nil "primer form ~d by Do it: ~a" n (lol.widgets:clip-text (or st "") 40))
                   (and st (search "=>" st) (not (search "error" st)))))
          (setf pos next))))

(let ((st (run "(package-name *package*)" "C-x C-e")))
  (check "Do it evaluates in LOL.STAGE" (and st (search "\"LOL.STAGE\"" st))))
(run "(collide-stage)" "C-c C-p")
(check "Print it prints from LOL.STAGE (#S(STAGE, unqualified)"
       (search "=> #S(STAGE" (lol.textview:tv-text (pane-tv *side*))))
(run "(defun zz-near (x) x)" "C-c C-c")
(check "Accept defines a fresh name in LOL.STAGE, not CL-USER"
       (and (fboundp (find-symbol "ZZ-NEAR" :lol.stage)) (not (find-symbol "ZZ-NEAR" :cl-user))))
(run "(defmethod stage-enact ((tag (eql :near-miss)) stage a b)
  (stage-log stage \"near miss\" :note))" "C-c C-c")
(check "Accept adds a method to the stage's own STAGE-ENACT"
       (find-method #'lol.stage:stage-enact '()
                    (list (sb-mop:intern-eql-specializer :near-miss) t t t) nil))
(run "(collide-st" "tab")
(check "Tab completes a LOL.STAGE name bare"
       (search "(collide-stage" (lol.textview:tv-text (pane-tv *side*))))
(let ((*package* (pane-package *side*)))
  (check "M-. / Ctrl-click resolve a bare stage word"
         (eq (resolve-symbol "step-stage") 'lol.stage:step-stage)))
(check "a drop on it inserts the bare name"
       (equal (drop-reference 'lol.stage:step-stage (pane-package *side*)) "'STEP-STAGE"))
(check "a drop on a CL-USER text qualifies it"
       (equal (drop-reference 'lol.stage:step-stage) "'LOL.STAGE:STEP-STAGE"))

(format t "~&the ordinary Workspace~%")
(let ((ws (find-if #'workspace-pane-p (mapcan #'copy-list *trails*))))
  (check "the tab-2 Workspace still reads in CL-USER"
         (and ws (eq *package* (find-package :cl-user)) (eq (pane-package ws) *package*))))

(format t "~&~d checks, ~d failed~%" *checks* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
