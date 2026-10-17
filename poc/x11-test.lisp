;;;; x11-test.lisp — the X11 host backend's loop, turned headless (doc/x11.org, morphic.org §13).
;;;;
;;;;     sbcl --script poc/x11-test.lisp        ; exits 0 if every check passes
;;;;
;;;; poc/run-browser.lisp's loop is HOST-CYCLE: one turn reads X events, routes them
;;;; through the world, steps, and presents what was repaired — and every overlay that
;;;; waits for an answer (a menu, the value prompt, the clipboard manager, the debugger)
;;;; waits by turning it, as on the guest. Its X specifics are three functions; here they
;;;; are a script of X events (lol.x11's own EVENT structure, as next-event builds them
;;;; from the wire), a presenter that counts frames and keeps the last one, and a key
;;;; decoder that is the identity (each scripted key event carries its key). So this is
;;;; the real cycle and the real overlays, with no X server: there is none on the build
;;;; host (no Xvfb), which is why it is tested this way.
;;;;
;;;; After every overlay, the presented canvas must be the world painted afresh.

(defvar cl-user::*run-browser-library* t)
(load (merge-pathnames "run-browser.lisp" (make-pathname :directory (pathname-directory *load-truename*))))

(in-package :cl-user)

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (name ok)
  (incf *checks*)
  (format t "~&  ~70a ~:[FAIL~;PASS~]~%" name ok)
  (unless ok (incf *fails*)))

(defun clock-string (&optional u) (declare (ignore u)) "2026-10-17 12:00:00")
(defun battery-status (&key fresh) (declare (ignore fresh)) nil)

(set-browser-font
 (open-browser-font (list "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf")
                    (merge-pathnames "cozette.lolf" (make-pathname :directory (pathname-directory *load-truename*)))))

(defparameter +w+ 1000)
(defparameter +h+ 720)

;;; ---- the scripted display -------------------------------------------------------
;;; *SCRIPT* holds X events, and functions: a function is run (to look at the world in
;;; the middle of a wait) and the next item is taken. An empty script ends any wait: it
;;; closes whatever overlay is up, so a broken overlay cannot hang the test.

(defvar *script* '())
(defvar *frames* 0)
(defvar *seen* '() "What the functions in the script recorded, newest first.")

(defun xev (kind &rest args) (apply #'lol.x11::make-event :kind kind args))
(defun key (k &optional (state 0)) (xev :key-press :detail k :state state :x 0 :y 0))
(defun press (x y &optional (button 1)) (xev :button-press :detail button :x x :y y :state 0))
(defun release (x y) (xev :button-release :detail 1 :x x :y y :state 0))
(defun note (fn) (lambda () (push (funcall fn) *seen*)))

(setf *host-next-event*
      (lambda (wait-ms)
        (declare (ignore wait-ms))
        (loop
          (let ((e (pop *script*)))
            (cond ((functionp e) (funcall e))
                  (e (return e))
                  (t (let ((o (top-overlay))) (when o (close-overlay o)))
                     (return nil)))))))
(setf *host-present* (lambda (canvas) (declare (ignore canvas)) (incf *frames*)))
(setf *host-decode* (lambda (code state) (declare (ignore state)) code))

(defun run (&rest items)
  "Script ITEMS, then turn the loop until they are used up."
  (setf *script* (append *script* items))
  (loop while *script* do (host-cycle))
  (host-cycle))

(defun screen-right-p ()
  (lol.morph:world-repair *world* *host-canvas*)
  (let ((fresh (lol.canvas:make-canvas +w+ +h+)))
    (lol.morph:world-paint *world* fresh :fresh t)
    (equalp (lol.canvas:canvas-pixels fresh) (lol.canvas:canvas-pixels *host-canvas*))))

(open-environment (find-package :cl-user))         ; the CL-USER list + a Workspace
(install-host-backend +w+ +h+)

(format t "~&== the X11 host loop: HOST-CYCLE is the world's cycle ==~%")
(check "the host installs its cycle, the chooser and the value prompt"
       (and (eq *world-cycle* 'host-cycle) (eq *chooser* 'run-chooser)
            (eq *value-prompt* 'run-value-prompt)))
(run)
(check "a turn repairs the whole first frame and presents it" (and (= *frames* 1) (screen-right-p)))
(let ((n *frames*))
  (run (xev :motion :x 300 :y 300 :state 0))
  (check "a bare pointer move presents nothing" (= n *frames*)))
(let* ((p (current-pane)) (row (find 2 (pane-rows p) :key #'cdr))
       (y (+ (car row) 4)) (depth (length (current-trail))))
  (run (press 60 y) (release 60 y))
  (check "a press and release on a row drills it" (= (1+ depth) (length (current-trail))))
  (check "the screen is right" (screen-right-p))
  (run (key :backspace)))

(format t "~&== the overlays that wait, turning this loop ==~%")
;; a right-click menu: it waits; the loop goes on (Down moves it), Esc closes it
(run (press 300 300 3)
     (note (lambda () (type-of (top-overlay))))
     (key :down)
     (note (lambda () (and (top-overlay) (chooser-sel (top-overlay)))))
     (key :escape))
(check "a right click opens the menu, an overlay waiting in HOST-CYCLE"
       (eq 'chooser-overlay (car (last *seen*))))
(check "... Down moves its selection while it waits" (numberp (first *seen*)))
(check "... Esc closes it" (null (top-overlay)))
(check "the screen is right" (screen-right-p))
;; the halo: the middle button
(setf *seen* '())
(run (press 300 300 2)
     (note (lambda () (and (top-overlay) (chooser-kind (top-overlay)))))
     (key :escape))
(check "the middle button opens the halo (an overlay), Esc closes it"
       (and (eq :halo (first *seen*)) (null (top-overlay))))
;; the clipboard manager: SuperL-h, Down, Return picks the older entry
(lol.clipboard:clip-copy "x11-one") (lol.clipboard:clip-copy "x11-two")
(setf *seen* '())
(run (key #\h 64)
     (note (lambda () (type-of (top-overlay))))
     (key :down) (key :return))
(check "SuperL-h: the clipboard manager waits in this loop; Down, Return picks"
       (and (eq 'clipboard-overlay (first *seen*)) (null (top-overlay))
            (equal "x11-one" (lol.clipboard:clip-current))))
(check "the screen is right" (screen-right-p))
;; the value prompt: an inspector's field, C-c C-c, a typed value
(defvar zz-x11-target 0)
(new-trail 'zz-x11-target)
(let* ((p (current-pane))
       (row (position-if (lambda (it) (search ":VALUE" (princ-to-string (item-label it))))
                         (view-items (pane-view p)))))
  (set-pane-sel p row)
  (setf *seen* '())
  (run (key #\c 4) (key #\c 4)
       (note (lambda () (type-of (top-overlay))))
       (key #\4) (key #\2) (key :return))
  (check "C-c C-c on a field: the value prompt waits in this loop, and sets it"
         (and (eq 'prompt-overlay (first *seen*)) (eql zz-x11-target 42)))
  (check "the screen is right" (screen-right-p))
  (run (key #\w 64)))
;; the debugger: Debug it on an error in the Workspace; C-g aborts out of the wait
(run (key #\2 64))
(let* ((p (current-pane)) (tv (pane-tv p)))
  (lol.textview:tv-set-text tv "(error \"x11 boom\")")
  (lol.textview::move tv :end nil)
  (setf *seen* '())
  (run (key #\c 4) (key #\d 4)
       (note (lambda () (list (type-of (top-overlay)) (getf *dbg-state* :condition))))
       (key #\g 4))
  (check "C-c C-d: the debugger is an overlay waiting in this loop, on the condition"
         (and (eq 'debugger-overlay (first (first *seen*)))
              (search "x11 boom" (or (second (first *seen*)) ""))))
  (check "... C-g aborts: the restart unwinds through the wait, the debugger is gone"
         (and (null (top-overlay)) (null *dbg-state*)))
  (check "the screen is right" (screen-right-p)))
;; the Spotter and the cheatsheet: open and return at once; keys go to them
(setf *seen* '())
(run (key #\Space 64) (note (lambda () (type-of (top-overlay)))) (key #\d) (key #\r)
     (note (lambda () (spotter-query (top-overlay)))) (key :escape))
(check "SuperL-Space: the Spotter; typing reaches its query; Esc closes it"
       (and (equal "dr" (first *seen*)) (eq 'spotter-overlay (second *seen*)) (null (top-overlay))))
(setf *seen* '())
(run (key #\? 64) (note (lambda () (type-of (top-overlay)))) (key #\x))
(check "SuperL-?: the cheatsheet, dismissed by a key"
       (and (eq 'cheatsheet-overlay (first *seen*)) (null (top-overlay))))
(check "the screen is right" (screen-right-p))

(format t "~&== windows and drag and drop, through X events ==~%")
(run (key #\l 64) (key #\g 64) (key :escape))
(check "SuperL-g tears a trail off into a window here too" (= 1 (length (desk-windows))))
(let* ((w (first (desk-windows))) (b0 (copy-list (lol.morph:morph-bounds w))))
  (run (press (+ (first b0) 60) (+ (second b0) 5))
       (xev :motion :x (+ (first b0) 10) :y (+ (second b0) 45) :state #x100)
       (release (+ (first b0) 10) (+ (second b0) 45)))
  (check "dragging its title bar (motion with Button1Mask) moves it"
         (equal (subseq (lol.morph:morph-bounds w) 0 2) (list (- (first b0) 50) (+ (second b0) 40))))
  (check "the screen is right" (screen-right-p))
  ;; a row of the window's list, dragged by X motion onto the tab strip: a new trail
  (let* ((p (window-pane w)) (row (find 2 (pane-rows p) :key #'cdr))
         (x (+ (first (lol.morph:morph-bounds w)) 60))
         (y (+ (second (lol.morph:morph-bounds w)) (car row) 4))
         (n (length *trails*)))
    (run (press x y)
         (xev :motion :x (+ x 20) :y (+ y 15) :state #x100)
         (note (lambda () (and (lol.morph:hand-payload (lol.morph:world-hand *world*)) t)))
         (xev :motion :x 500 :y 8 :state #x100)
         (release 500 8))
    (check "a row dragged out of the window: the hand carries it, and the tab strip takes it"
           (and (first *seen*) (= (1+ n) (length *trails*))))
    (check "the screen is right" (screen-right-p))))
(check "SuperL-q here: the leave menu waits in this loop; Esc stays"
       (progn (setf *seen* '())
              (run (key #\q 64) (note (lambda () (type-of (top-overlay)))) (key :escape))
              (and (eq 'chooser-overlay (first *seen*)) (null *world-quit*))))

(format t "~&~d checks, ~d failed~%" *checks* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
