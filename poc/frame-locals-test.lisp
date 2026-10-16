;;;; frame-locals-test.lisp — host test for settable debugger frame locals.
;;;;
;;;;   sbcl --script poc/frame-locals-test.lisp      ; exits 0 if all pass
;;;;
;;;; initramfs/model.lisp's FRAME-LOCAL-PLACES / SET-FRAME-VAR / RESTART-FRAME, the
;;;; model half of doc/inspector-history.org §5.4. Each case signals an error from a
;;;; small function, and the HANDLER-BIND handler plays the debugger: it finds the
;;;; function's frame on the still-standing stack, writes a local through the place,
;;;; then CONTINUEs (or restarts the frame) — and the check is what the function
;;;; returned, i.e. whether the FRAME, not just sb-di, saw the new value.
;;;;
;;;; Two of the cases are the traps the prototype found: a fixnum-stored variable
;;;; that would take a string, and the "indirect" variable for which SBCL's own
;;;; (SETF DEBUG-VAR-VALUE) overwrites a frame pointer (a memory fault — this test
;;;; would die there, not fail, if the guard regressed).
;;;;
;;;; All original code, under this project's MIT licence.

(require :sb-introspect)
(let ((root (merge-pathnames "../" (make-pathname :directory (pathname-directory *load-truename*)))))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "initramfs/registry.lisp" root))
    (funcall 'load-recording (merge-pathnames "initramfs/model.lisp" root))))

(defvar *failures* 0)
(defun ok (name got want)
  (if (equal got want)
      (format t "~&  ok    ~a~%" name)
      (progn (incf *failures*)
             (format t "~&  FAIL  ~a~%        got  ~s~%        want ~s~%" name got want))))

(defun frame-named (name)
  "The innermost standing frame whose function is NAME (EQUAL, so (FLET H :IN G) works)."
  (loop for f = (sb-di:top-frame) then (sb-di:frame-down f)
        while f
        when (equal (ignore-errors (sb-di:debug-fun-name (sb-di:frame-debug-fun f))) name)
          return f))

(defun place (frame name)
  (find name (frame-local-places frame) :key (lambda (p) (getf p :name)) :test #'string=))

(defun with-debugger (thunk frame-name action)
  "Call THUNK; on an error, call ACTION with FRAME-NAME's frame and the condition
   (with the stack standing), then CONTINUE if ACTION returned normally."
  (handler-bind ((error (lambda (c)
                          (funcall action (frame-named frame-name) c)
                          (continue c))))
    (funcall thunk)))

;;; ---- the specimens --------------------------------------------------------
(defun fl-default (x)                    ; the image's default policy: (debug 1)
  (let ((y (* x 3)))
    (cerror "go on" "small ~a" x)
    (list x y)))

(defun fl-debug2 (x)
  (declare (optimize (debug 2)))
  (let ((y (* x 3)) (s (format nil "s~a" x)))
    (cerror "go on" "boom")
    (list x y s)))

(defun fl-typed (x)
  (declare (optimize (debug 2)) (fixnum x))
  (let ((n (the fixnum (* x 3))) (d (* 1.5d0 x)))
    (declare (fixnum n) (double-float d))
    (cerror "go on" "boom")
    (list n d)))

(defun fl-call-it (f) (funcall f))
(defun fl-indirect (x)
  (declare (optimize (debug 2)))
  (let ((acc (* x 10)))
    (flet ((h () (cerror "go on" "boom") (incf acc x)))
      (declare (dynamic-extent #'h))
      (fl-call-it #'h)
      (list acc x))))

(defvar *fl-keep* nil)
(defun fl-closures (x)
  (declare (optimize (debug 2)))
  (let ((counter x) (fixed (* x 2)))
    (setf *fl-keep* (list (lambda () (incf counter)) (lambda () fixed)))
    (cerror "go on" "boom")
    ;; COUNTER must be read here, after the error: a variable only the closures still
    ;; use is not live in this frame at all (sb-di says :INVALID) — measured.
    (list (funcall (first *fl-keep*)) (funcall (second *fl-keep*)) fixed counter)))

(defun fl-debug0 (x)
  (declare (optimize (debug 0)))
  (cerror "go on" "boom")
  (* x 2))

(defvar *fl-calls* 0)
(defun fl-restartable (x)
  (incf *fl-calls*)
  (when (< x 10) (error "x too small: ~a" x))
  (* x 2))

;;; ---- the checks -------------------------------------------------------------
(format t "~&== which locals exist, and which can be set ==~%")
(with-debugger (lambda () (fl-default 7)) 'fl-default
  (lambda (f c) (declare (ignore c))
    (ok "(debug 1): only the argument is a debug variable"
        (mapcar (lambda (p) (getf p :name)) (frame-local-places f)) '("x"))
    (ok "...and it is settable" (functionp (getf (place f "x") :set)) t)))
(with-debugger (lambda () (fl-debug0 7)) 'fl-debug0
  (lambda (f c) (declare (ignore c))
    (ok "(debug 0): the locals are not settable, and say why"
        (mapcar (lambda (p) (list (getf p :set) (getf p :why))) (frame-local-places f))
        '((nil "location unknown here (compiled at low debug)")))
    (ok "(debug 0): the frame cannot be restarted"
        (frame-restart-refusal f) "no unwind tag — compiled at (debug 0)")))

(format t "~&== a set local is what the frame computes with after CONTINUE ==~%")
(ok "argument, default policy"
    (with-debugger (lambda () (fl-default 7)) 'fl-default
      (lambda (f c) (declare (ignore c)) (funcall (getf (place f "x") :set) 21)))
    '(21 21))                                          ; y was computed before the error
(ok "LET locals at (debug 2): a number and a string"
    (with-debugger (lambda () (fl-debug2 7)) 'fl-debug2
      (lambda (f c) (declare (ignore c))
        (funcall (getf (place f "y") :set) 1000)
        (funcall (getf (place f "s") :set) "NEW")))
    '(7 1000 "NEW"))
(ok "the setter returns the value READ BACK from the frame"
    (let (back)
      (with-debugger (lambda () (fl-debug2 7)) 'fl-debug2
        (lambda (f c) (declare (ignore c)) (setf back (funcall (getf (place f "y") :set) 5))))
      back)
    5)

(format t "~&== trap 1: the storage class decides what may be written ==~%")
(let (msgs)
  (ok "a fixnum and a double-float written as themselves"
      (with-debugger (lambda () (fl-typed 7)) 'fl-typed
        (lambda (f c) (declare (ignore c))
          (funcall (getf (place f "n") :set) 100)
          (funcall (getf (place f "d") :set) 2.5d0)
          (dolist (bad '(("n" "a string") ("d" 3)))
            (handler-case (funcall (getf (place f (first bad)) :set) (second bad))
              (error (e) (push (princ-to-string e) msgs))))))
      '(100 2.5d0))
  (ok "a string refused for the fixnum, 3 for the double — with the reason"
      (reverse msgs)
      '("n is stored as a raw fixnum in this frame; \"a string\" is not one"
        "d is stored as a raw double-float in this frame; 3 is not one")))

(format t "~&== trap 2: an indirect variable (SBCL's own setter faults here) ==~%")
(ok "the DX flet's view of its parent's ACC is written through the pointer"
    (with-debugger (lambda () (fl-indirect 3)) '(flet h :in fl-indirect)
      (lambda (f c) (declare (ignore c)) (funcall (getf (place f "acc") :set) 100)))
    '(103 3))

(format t "~&== trap 3: closures — a value cell is shared, a copy is not ==~%")
(ok "COUNTER (closed over and set) changes for the closure; FIXED's closure keeps 14"
    (with-debugger (lambda () (fl-closures 7)) 'fl-closures
      (lambda (f c) (declare (ignore c))
        (funcall (getf (place f "counter") :set) 100)
        (funcall (getf (place f "fixed") :set) 200)))
    '(101 14 200 101))

(format t "~&== a value computed in the frame's context ==~%")
(ok "(* x 100) typed for a local reads the frame's own x"
    (with-debugger (lambda () (fl-debug2 7)) 'fl-debug2
      (lambda (f c) (declare (ignore c))
        (funcall (getf (place f "y") :set) (eval-in-frame-context "(* x 100)" f))))
    '(7 700 "s7"))

(format t "~&== RESTART-FRAME: the call again, with the argument as set ==~%")
(setf *fl-calls* 0)
(ok "x set to 21 and the frame restarted => 42"
    (handler-bind ((error (lambda (c) (declare (ignore c))
                            (let ((f (frame-named 'fl-restartable)))
                              (funcall (getf (place f "x") :set) 21)
                              (restart-frame f)))))
      (fl-restartable 7))
    42)
(ok "...called exactly twice" *fl-calls* 2)
(ok "frame-call-form reads the current argument values"
    (block got
      (handler-bind ((error (lambda (c) (declare (ignore c))
                              (let ((f (frame-named 'fl-restartable)))
                                (funcall (getf (place f "x") :set) 8)
                                (return-from got (frame-call-form f))))))
        (fl-restartable 3)))
    '(fl-restartable 8))

(format t "~&== cost ==~%")
(with-debugger (lambda () (fl-default 1)) 'fl-default
  (lambda (f c) (declare (ignore c))
    (let ((set (getf (place f "x") :set)) (t0 (get-internal-real-time)))
      (dotimes (i 10000) (funcall set i))
      (format t "~&  10,000 set-and-read-backs: ~,1f ms~%"
              (/ (- (get-internal-real-time) t0) (/ internal-time-units-per-second 1000)))
      (funcall set 1))))

(format t "~&~:[ALL PASS~;~:*~d FAILED~]~%" (and (plusp *failures*) *failures*))
(sb-ext:exit :code (if (zerop *failures*) 0 1))
