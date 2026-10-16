;;;; check-debugger.lisp — boot the image and set a local in a standing frame.
;;;;
;;;;     LOL_PORT=4101 sbcl --script check-debugger.lisp   ; exits 0 if all pass
;;;;     LOL_SHOTS=DIR …                                   ; also keep screendumps
;;;;
;;;; The framebuffer debugger's frame locals are PLACES (doc/inspector-history.org
;;;; §5.4): Tab to the locals strip, pick one, Enter or C-c C-c, type a value, and it
;;;; is written into the frame that is still waiting for ERROR to return. Then either
;;;; CONTINUE (when the error offers it) or R, which re-enters the frame with the
;;;; arguments it now holds. This drives all of it with REAL keys over QMP — the
;;;; Workspace's Debug it (C-c C-d) opens the debugger, exactly as a user would —
;;;; and reads two things back over the net REPL: *DBG-STATE* (what the debugger is
;;;; showing: frames, focus, locals, the note in the strip's header) and *ZZ-OUT*,
;;;; which each specimen sets on its way out. The second is the real assertion:
;;;; not that sb-di accepted a write, but that the FUNCTION went on to compute with it.
;;;;
;;;;   1. CONTINUE: an argument at the image's default (debug 1), 7 -> 21, => 42.
;;;;   2. R: an argument set, the frame restarted, the function called again => 42.
;;;;   3. The indirect local (a dynamic-extent FLET reading its parent's LET), for
;;;;      which SBCL's own setter overwrites a frame pointer. A regression here is
;;;;      not a failed check but a dead guest — PID 1 memory-faulting is a panic.
;;;;   4. A (debug 0) frame: nothing settable, and the note says why; C-g aborts.
;;;;
;;;; Host-side companion, with the cases this cannot reach by typing (the fixnum
;;;; guard, closures, a value computed in the frame): poc/frame-locals-test.lisp.
;;;; Run ./build.sh first; this boots iso_root as is.
;;;;
;;;; All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(use-package :lol.guest)

(defvar *shots* (sb-ext:posix-getenv "LOL_SHOTS"))
(defun shot (name)
  (when *shots*
    (qmp (format nil "{\"execute\":\"screendump\",\"arguments\":{\"filename\":\"~a/~a.ppm\"}}"
                 *shots* name))))

(defun row (label value)
  (format t "~&  ~44a ~a~%" label value) (finish-output) value)

(defun qmp-chord (modifier qcode)
  "MODIFIER+QCODE in one send-key — a chord the TTY spells as one byte (C-c is 3)."
  (qmp (format nil "{\"execute\":\"send-key\",\"arguments\":{\"keys\":[~
                    {\"type\":\"qcode\",\"data\":\"~a\"},{\"type\":\"qcode\",\"data\":\"~a\"}]}}"
               modifier qcode))
  (sleep 0.6))

(defun key (qcode &optional (n 1))
  (dotimes (i n) (qmp-key qcode) (sleep 0.25)))

(defun wait-until (form-string &key (seconds 20) (want "T"))
  "Probe FORM-STRING every half second until it prints WANT; the last answer."
  (let ((last nil))
    (loop repeat (* 2 seconds)
          do (setf last (probe form-string))
             (when (string= last want) (return))
             (sleep 0.5))
    last))

;;; ---- the gestures -------------------------------------------------------------
(defun debug-it (form)
  "Put FORM alone in the Workspace, caret after it, and C-c C-d (Debug it). True once
   the debugger is up."
  (probe (format nil "(let ((tv (pane-tv (current-pane)))) (lol.textview:tv-set-text tv ~s) (lol.textview:tv-move-point tv 0 ~d) t)"
                 form (length form)))
  (sleep 0.5)
  (qmp-chord "ctrl" "c") (qmp-chord "ctrl" "d")
  (string= (wait-until "(and *dbg-state* t)") "T"))

(defun state (key)
  (probe (format nil "(getf *dbg-state* ~s)" key)))

(defun select-frame (substring)
  "Tab from the restarts to the stack, walk down to the first frame whose label holds
   SUBSTRING, then Tab on into its locals. The frame's index, or NIL."
  (let ((i (ignore-errors
             (parse-integer
              (probe (format nil "(position-if (lambda (l) (search ~s l)) (getf *dbg-state* :frames))"
                             substring))))))
    (when i
      (key "tab") (key "down" i) (key "tab")
      i)))

(defun select-local (name)
  (let ((i (ignore-errors
             (parse-integer
              (probe (format nil "(position ~s (getf *dbg-state* :locals) :key #'first :test #'string=)"
                             name))))))
    (when i (key "down" i) i)))

(defun set-selected-local (digits)
  "C-c C-c, type DIGITS into the value prompt, Enter. The note the strip then shows."
  (qmp-chord "ctrl" "c") (qmp-chord "ctrl" "c") (sleep 0.5)
  (qmp-type digits) (sleep 0.3)
  (key "ret") (sleep 0.8)
  (probe "(getf *dbg-state* :note)"))

(defun invoke-restart-named (name)
  "Select the restart called NAME by its digit and invoke it with Enter (from the
   restart rail: Tab round to it first, since Enter on a local SETS it)."
  (let ((i (parse-integer (probe (format nil "(position '~a (getf *dbg-state* :restarts))" name)))))
    (key "tab")                                       ; locals -> restarts
    (key (princ-to-string i)) (key "ret")
    i))

;;; ---- the specimens, defined in the guest over the net REPL ---------------------
(defparameter *specimens*
  '("(defvar *zz-out* nil)"
    "(defun zz-cont (x) (cerror \"Go on with x as it is now\" \"x is small: ~a\" x) (setf *zz-out* (list :cont (* x 2))))"
    "(defun zz-restart (x) (when (< x 10) (error \"x too small: ~a\" x)) (setf *zz-out* (list :restart (* x 2))))"
    "(defun zz-call (f) (funcall f))"
    "(defun zz-dx (x) (declare (optimize (debug 2))) (let ((acc (* x 10))) (flet ((h () (cerror \"go on\" \"boom\") (incf acc x))) (declare (dynamic-extent (function h))) (zz-call (function h)) (setf *zz-out* (list :dx acc x)))))"
    "(defun zz-d0 (x) (declare (optimize (debug 0))) (cerror \"go on\" \"boom\") (setf *zz-out* (list :d0 x)))"))

(defun checks ()
  (quiet-kernel-console)
  (check "the environment came up" (wait-for-browser 60))
  (dolist (f *specimens*) (probe f 20.0))
  (check "the specimens are defined"
         (string= (probe "(every #'fboundp '(zz-cont zz-restart zz-dx zz-d0))") "T"))
  (qmp-hold "meta_l" "1") (sleep 1.0)                ; the Workspace tab

  (format t "~&~%== 1. set an argument, then CONTINUE ==~%")
  (check "Debug it on (zz-cont 7) opens the debugger" (debug-it "(zz-cont 7)"))
  (row "frames" (state :frames))
  (check "the ZZ-CONT frame is on the stack" (row "frame index" (select-frame "ZZ-CONT")))
  (check "focus is on the locals" (string= (row "focus" (state :focus)) ":LOCALS"))
  (check "x is listed, as 7, settable"
         (string= (row "locals" (state :locals)) "((\"x\" \"7\" T))"))
  (shot "debugger-locals")
  (let ((note (row "note after setting x to 21" (set-selected-local "21"))))
    (check "the note reports the value read back"
           (and (search "x = 21" note) (search "read back" note))))
  (check "the strip now shows x = 21"
         (string= (row "locals" (state :locals)) "((\"x\" \"21\" T))"))
  (shot "debugger-local-set")
  (invoke-restart-named "CONTINUE")
  (check "CONTINUE: the function computed with the new x"
         (string= (row "*zz-out*" (wait-until "*zz-out*" :want "(:CONT 42)")) "(:CONT 42)"))
  (check "the debugger has gone" (string= (probe "*dbg-state*") "NIL"))

  (format t "~&~%== 2. set an argument, then R (restart the frame) ==~%")
  (probe "(setf *zz-out* nil)")
  (check "Debug it on (zz-restart 7)" (debug-it "(zz-restart 7)"))
  (check "the ZZ-RESTART frame" (select-frame "ZZ-RESTART"))
  (row "note after x := 21" (set-selected-local "21"))
  (key "r")
  (check "R: the frame ran again with x = 21"
         (string= (row "*zz-out*" (wait-until "*zz-out*" :want "(:RESTART 42)")) "(:RESTART 42)"))
  (check "the Workspace reports the restarted call's value"
         (search "42" (row "status" (probe "*browser-status*"))))

  (format t "~&~%== 3. the indirect local (SBCL's own setter would fault) ==~%")
  (probe "(setf *zz-out* nil)")
  (check "Debug it on (zz-dx 3)" (debug-it "(zz-dx 3)"))
  (check "the (FLET H :IN ZZ-DX) frame" (select-frame "FLET H"))
  (check "acc is listed among the locals"
         (row "acc's place" (select-local "acc")))
  (row "note after acc := 100" (set-selected-local "100"))
  (invoke-restart-named "CONTINUE")
  (check "the parent's ACC took the write: (incf acc 3) => 103"
         (string= (row "*zz-out*" (wait-until "*zz-out*" :want "(:DX 103 3)")) "(:DX 103 3)"))
  (check "and the guest is alive" (string= (probe "(+ 1 1)") "2"))

  (format t "~&~%== 4. a (debug 0) frame: nothing to set, and why ==~%")
  (check "Debug it on (zz-d0 5)" (debug-it "(zz-d0 5)"))
  (check "the ZZ-D0 frame" (select-frame "ZZ-D0"))
  (row "locals" (state :locals))
  (let ((note (row "note after trying to set" (set-selected-local "9"))))
    (check "the note says it cannot be set" (search "cannot be set" note)))
  (key "r") (sleep 0.5)
  (check "R is refused with a reason"
         (search "can't restart" (row "note after R" (state :note))))
  (shot "debugger-refused")
  (qmp-chord "ctrl" "g") (sleep 1.0)
  (check "C-g aborts back to the Workspace"
         (string= (row "status" (probe "*browser-status*")) "\"aborted\""))
  (check "the guest is still alive" (eql (probe-int "(+ 1 1)") 2)))

(run-guest-checks "check-debugger" #'checks)
