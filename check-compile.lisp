;;;; check-compile.lisp — assert that the guest can COMPILE at runtime.
;;;;
;;;;     LOL_PORT=4025 sbcl --script check-compile.lisp   ; exits 0 if all pass
;;;;
;;;; Until 2026-10-02 it could not: compiling untyped arithmetic killed the guest
;;;; ("Unhandled memory fault at #x1A"), an AREF on an (unsigned-byte 32) vector
;;;; failed with "NIL is not of type SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES",
;;;; and so C-c C-c could not accept most real definitions. The cause was SBCL
;;;; 2.6.8's LOGCOUNT on CPUs without POPCNT — QEMU's default qemu64 is one — and
;;;; the repair is initramfs/logcount-fix.lisp. Story: doc/inspector-history.org §5.6.
;;;;
;;;; This test is only meaningful on a CPU WITHOUT popcnt, so it first checks that
;;;; it got one (the harness boots QEMU's default CPU unless LOL_QEMU_CPU says
;;;; otherwise). On a CPU with POPCNT the fixed path is never taken and every
;;;; assertion below would pass with or without the fix — so that case FAILS the
;;;; first check rather than passing vacuously.
;;;;
;;;; Order matters: the forms that used to memory-fault come last, after the
;;;; ones that only used to give wrong answers, so a regression is reported as
;;;; precisely as possible before a fault can do worse. (Over the net REPL the
;;;; faults proved survivable: run without the fix, every check failed except
;;;; the last, "still alive".)
;;;;
;;;; All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(use-package :lol.guest)

(defun row (label value)
  (format t "~&  ~44a ~a~%" label value)
  (finish-output)
  value)

(defun checks ()
  (format t "~&~%== the setting: a CPU without POPCNT ==~%")
  (let ((popcnt (row "guest CPU advertises popcnt"
                     (probe "(with-open-file (s \"/proc/cpuinfo\") (loop for l = (read-line s nil) while l when (search \"flags\" l) return (if (search \" popcnt\" l) :yes :no)))"))))
    (check "the guest CPU has no POPCNT (else this proves nothing)"
           (string= popcnt ":NO")))

  (format t "~&~%== the repair is in the image ==~%")
  (check "LOGCOUNT routine carries the RDX patch"
         (string= (row "routine bytes = patched bytes"
                       (probe "(equalp (logcount-routine-bytes) (logcount-routine-patched))"))
                  "T"))

  (format t "~&~%== answers that used to be wrong ==~%")
  (check "(logcount #xFF) = 8"
         (eql (row "(logcount #xFF)" (probe-int "(logcount #xFF)")) 8))
  (check "(unsigned-byte 32) prints as itself, not (mod ...)"
         (string= (row "type-specifier of (unsigned-byte 32)"
                       (probe "(sb-kernel:type-specifier (sb-kernel:specifier-type '(unsigned-byte 32)))"))
                  "(UNSIGNED-BYTE 32)"))
  (check "a typed AREF compiles"
         (string= (row "compile (aref (the (simple-array (ub 32)...)))"
                       (probe "(functionp (compile nil '(lambda (v) (aref (the (simple-array (unsigned-byte 32) (*)) v) 5))))" 20.0))
                  "T"))

  (format t "~&~%== forms that used to memory-fault ==~%")
  (check "(logcount (expt 2 100)) = 1  [bignum path]"
         (eql (row "(logcount (expt 2 100))" (probe-int "(logcount (expt 2 100))")) 1))
  (check "(signed-byte 64) prints as itself"
         (string= (row "type-specifier of (signed-byte 64)"
                       (probe "(sb-kernel:type-specifier (sb-kernel:specifier-type '(signed-byte 64)))"))
                  "(SIGNED-BYTE 64)"))
  (check "(compile nil '(lambda (x) (* x 2))) => 42"
         (eql (row "(funcall (compile ... (* x 2)) 21)"
                   (probe-int "(funcall (compile nil '(lambda (x) (* x 2))) 21)" 20.0))
              42))
  (check "untyped + - < 1+ all compile"
         (string= (row "compile + - < 1+"
                       (probe "(every #'functionp (mapcar (lambda (f) (compile nil f)) '((lambda (x) (+ x 1)) (lambda (x) (- x 1)) (lambda (x y) (< x y)) (lambda (x) (1+ x)))))" 30.0))
                  "T"))
  (check "the Workspace's COMPILE-d counter counts"
         (eql (row "compiled make-counter, called twice"
                   (probe-int "(let ((c (funcall (compile nil '(lambda (start step) (lambda (&optional (bump 1)) (incf start (* step bump))))) 10 2))) (funcall c) (funcall c))" 20.0))
              14))
  (when (wait-for-browser)
    (check "the AREF probe that check-pointer had to dodge"
           (string= (row "(aref (canvas-pixels *fb-canvas*) 5) via compile"
                         (probe "(integerp (aref (lol.canvas:canvas-pixels *fb-canvas*) 5))" 20.0))
                    "T")))

  (format t "~&~%== C-c C-c: accept-source, the function Accept calls ==~%")
  (check "accept-source of (defun zz-b (x) (* x 2)) succeeds"
         (string= (row "accept-source returns"
                       (probe "(accept-source \"(defun zz-b (x) (* x 2))\")" 20.0))
                  "ZZ-B"))
  (check "and the accepted ZZ-B works: (zz-b 21) = 42"
         (eql (row "(zz-b 21)" (probe-int "(zz-b 21)")) 42))
  (check "and the registry holds its new source"
         (string= (row "recorded source mentions (* x 2)"
                       (probe "(and (search \"(* x 2)\" (source-of 'zz-b)) t)"))
                  "T"))
  (check "the guest is still alive at the end"
         (eql (row "(+ 1 1) after all that" (probe-int "(+ 1 1)")) 2)))

(run-guest-checks "check-compile" #'checks :qmp nil)
