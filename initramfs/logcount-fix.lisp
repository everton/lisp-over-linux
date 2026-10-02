;;;; logcount-fix.lisp — repair SBCL 2.6.8's LOGCOUNT on CPUs without POPCNT.
;;;;
;;;; THE SYMPTOM. In QEMU, compiling almost anything with untyped arithmetic —
;;;; (lambda (x) (* x 2)) — killed the image with "Unhandled memory fault at
;;;; #x1A", and compiling an AREF on an (unsigned-byte 32) vector signalled
;;;; "NIL is not of type SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES". The same
;;;; image compiled both happily on the laptop. For a while that read as
;;;; "the guest is broken"; it is really "the guest's CPU is OLD".
;;;;
;;;; WHY THE CPU. QEMU without -cpu emulates "qemu64", a deliberately minimal
;;;; x86-64: no SSSE3, no SSE4, no POPCNT. POPCNT is the instruction that
;;;; counts the 1-bits of a word, which is LOGCOUNT. SBCL's compiled LOGCOUNT
;;;; asks at run time whether the CPU has it (a bit the C runtime sets at
;;;; startup from CPUID, kept in static space next to NIL):
;;;;
;;;;       test byte [nil+97], 2     ; cpu-feature-bits: has POPCNT?
;;;;       je   slow
;;;;       popcnt rdx, rax           ; the fast path every modern CPU takes
;;;;       ...
;;;;   slow:
;;;;       mov  rdx, rax             ; argument in RDX ...
;;;;       call LOGCOUNT             ; ... a hand-written assembly routine ...
;;;;                                 ; ... answer expected back in RDX
;;;;
;;;; THE BUG (upstream, in SBCL 2.6.8 itself — not in this project). The
;;;; assembly routine declares its argument as "Lisp argument register 0".
;;;; In SBCL 2.6.6 those registers were RDX RDI RSI (SB-VM::*REGISTER-ARG-
;;;; OFFSETS* is (2 7 6) there), so register 0 was RDX and both sides agreed.
;;;; By 2.6.8 they had been reordered to RDI RSI RDX — (7 6 2) — and the
;;;; routine followed, but the caller above still hardcodes RDX
;;;; (sbcl/src/assembly/x86-64/arith.lisp, the DEF-IT macrolet:
;;;; ":offset rdx-offset"). So on a CPU without POPCNT:
;;;;
;;;;   * the routine counts the bits of whatever happened to be in RDI, and
;;;;     leaves that count IN RDI — destroying a live value the caller kept
;;;;     there (in BIGNUM-LOGCOUNT, the bignum itself: next instruction loads
;;;;     through it, address 0x11 — the "memory fault at a tiny address");
;;;;   * the caller reads RDX, which still holds the ARGUMENT, so
;;;;     (logcount x) => x. The type printer asks "is 2^32 a power of two?"
;;;;     as (= (logcount 2^32) 1), gets "no", and spells (unsigned-byte 32)
;;;;     as (mod 4294967296) — which FIND-SAETP has never heard of: the NIL.
;;;;
;;;; The compiler does this kind of arithmetic constantly while it derives
;;;; types, which is why the Workspace's Do it (no compiler) worked and every
;;;; Accept (the compiler) was a coin toss. Every modern CPU has POPCNT, so
;;;; the slow path never runs on a developer's machine — on the real laptop
;;;; this bug is invisible, and SBCL's own test suite never meets it.
;;;;
;;;; THE FIX. The routine has exactly one caller (that VOP), so the routine's
;;;; real contract is "argument and result in RDX". We rewrite the routine to
;;;; honour it: every RDI operand becomes RDX. On x86-64 a register-to-register
;;;; operand lives in the instruction's ModRM byte — two bits of mode (11 =
;;;; registers), three bits "reg", three bits "r/m" — and RDI is 7, RDX is 2.
;;;; Both are "low" registers needing no REX change, so swapping one 3-bit
;;;; field for the other changes no instruction's length: a byte-for-byte,
;;;; in-place patch. RAX and RCX the routine already saves and restores; it
;;;; now leaves RDI alone too, as the caller assumed.
;;;;
;;;; The patch runs at BUILD time, before save-lisp-and-die, so the corrected
;;;; routine is simply part of the frozen heap. It refuses to touch anything it
;;;; does not recognise byte for byte (another SBCL version gets a loud warning,
;;;; never a silent half-patch), and it proves itself on the build host by
;;;; switching POPCNT OFF — clearing that same feature bit — and running the
;;;; slow path for real. Measurements and the dead ends: doc/inspector-history.org
;;;; §5.6.
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

(defparameter +logcount-routine-original+
  #(#x50 #x51                                     ; push rax / push rcx
    #x48 #x8B #xC7                                ; mov rax, rdi
    #x48 #xD1 #xEF                                ; shr rdi, 1
    #x48 #xB9 #x55 #x55 #x55 #x55 #x55 #x55 #x55 #x55  ; mov rcx, #x5555...
    #x48 #x21 #xCF                                ; and rdi, rcx
    #x48 #x29 #xF8                                ; sub rax, rdi
    #x48 #x8B #xF8                                ; mov rdi, rax
    #x48 #xC1 #xE8 #x02                           ; shr rax, 2
    #x48 #xB9 #x33 #x33 #x33 #x33 #x33 #x33 #x33 #x33  ; mov rcx, #x3333...
    #x48 #x21 #xCF                                ; and rdi, rcx
    #x48 #x21 #xC8                                ; and rax, rcx
    #x48 #x01 #xC7                                ; add rdi, rax
    #x48 #x8B #xC7                                ; mov rax, rdi
    #x48 #xC1 #xEF #x04                           ; shr rdi, 4
    #x48 #x01 #xC7                                ; add rdi, rax
    #x48 #xB9 #x0F #x0F #x0F #x0F #x0F #x0F #x0F #x0F  ; mov rcx, #x0F0F...
    #x48 #x21 #xCF                                ; and rdi, rcx
    #x48 #xB9 #x01 #x01 #x01 #x01 #x01 #x01 #x01 #x01  ; mov rcx, #x0101...
    #x48 #x0F #xAF #xF9                           ; imul rdi, rcx
    #x48 #xC1 #xEF #x38                           ; shr rdi, 56
    #x59 #x58                                     ; pop rcx / pop rax
    #xC3)                                         ; ret
  "SBCL 2.6.8's LOGCOUNT assembly routine, byte for byte, as it ships. The
   patch only proceeds when the image holds exactly this.")

(defparameter +logcount-routine-modrm-offsets+
  '(4 7 20 23 26 43 49 52 55 59 72 86 89)
  "Where, in the bytes above, the thirteen instructions that name RDI keep
   their ModRM byte — the byte after the opcode (two after it for 0F AF IMUL).")

(defun modrm-rdi->rdx (byte)
  "BYTE, a register-direct ModRM (mode 11), with RDI (7) replaced by RDX (2)
   in whichever of its two 3-bit register fields names it."
  (assert (= (ldb (byte 2 6) byte) #b11) () "not a register-direct ModRM: ~x" byte)
  (flet ((swap (r) (if (= r 7) 2 r)))
    (dpb (swap (ldb (byte 3 3) byte)) (byte 3 3)
         (dpb (swap (ldb (byte 3 0) byte)) (byte 3 0) byte))))

(defun logcount-routine-sap ()
  (sb-sys:int-sap (sb-fasl::get-asm-routine 'sb-vm::logcount)))

(defun logcount-routine-bytes ()
  (let ((sap (logcount-routine-sap)))
    (map 'vector (lambda (i) (sb-sys:sap-ref-8 sap i))
         (loop for i below (length +logcount-routine-original+) collect i))))

(defun logcount-routine-patched ()
  "The routine as it should be: the original with each listed ModRM rewritten."
  (let ((v (copy-seq +logcount-routine-original+)))
    (dolist (i +logcount-routine-modrm-offsets+ v)
      (setf (aref v i) (modrm-rdi->rdx (aref v i))))))

(defun patch-logcount-routine ()
  "Rewrite SBCL's LOGCOUNT assembly routine in place to take and return its
   argument in RDX, the register its only caller uses. Idempotent. Returns
   :PATCHED, :ALREADY-PATCHED, or :UNRECOGNISED (and then changes nothing)."
  (let ((now (logcount-routine-bytes)))
    (cond ((equalp now (logcount-routine-patched)) :already-patched)
          ((not (equalp now +logcount-routine-original+))
           (warn "logcount-fix: the LOGCOUNT routine is not the SBCL 2.6.8 one I ~
                  know; left untouched. Check whether this SBCL still has the bug ~
                  (doc/inspector-history.org §5.6).")
           :unrecognised)
          (t (let ((sap (logcount-routine-sap)))
               (loop for i from 0 for b across (logcount-routine-patched)
                     do (setf (sb-sys:sap-ref-8 sap i) b)))
             :patched))))

;;; ---- proving it on a CPU that HAS popcnt ---------------------------------

;;; Both numbers below are compile-time constants inside SBCL's own build
;;; (sbcl/src/compiler/x86-64/parms.lisp) that do not survive into the image,
;;; so they are spelled out here, with their derivation.

(defconstant +cpu-feature-bits-offset+ (- (* (+ 6 7) 8) 7)
  "97: the trailer constant CPU-FEATURE-BITS is the 8th (index 7) of
   +STATIC-SPACE-TRAILER-CONSTANTS+, which start 6 words past NIL's native
   address; NIL's tagged pointer carries LIST-POINTER-LOWTAG (7), so from it
   the byte is at 13*8 - 7. The disassembler prints the same: [R12+97].")

(defconstant +cpu-has-popcnt-mask+ 2
  "Bit CPU-HAS-POPCNT (1), as the C runtime sets it (sbcl/src/runtime/
   x86-64-arch.c: \"if (cpuid_fn1_ecx & (1<<23)) our_cpu_feature_bits |= 2\").")

(defun cpu-feature-bits-sap ()
  "The byte the compiled code tests: [nil + 97]."
  (sb-sys:int-sap (+ (sb-kernel:get-lisp-obj-address nil) +cpu-feature-bits-offset+)))

(defun call-without-popcnt (fn)
  "Call FN as if this CPU had no POPCNT, as qemu64 has none: clear the feature
   bit so every compiled LOGCOUNT takes the slow path, then put it back."
  (let* ((sap (cpu-feature-bits-sap))
         (old (sb-sys:sap-ref-8 sap 0)))
    (unwind-protect
         (progn (setf (sb-sys:sap-ref-8 sap 0) (logandc2 old +cpu-has-popcnt-mask+))
                (funcall fn))
      (setf (sb-sys:sap-ref-8 sap 0) old))))

(defun word-logcount (x)
  (declare (type (unsigned-byte 64) x))
  (logcount x))                         ; the inline VOP, unsigned-reg flavour

(defun logcount-slow-path-ok-p ()
  "Every question the guest got wrong, asked again with POPCNT switched off.
   (Asking them BEFORE the patch is what crashed: don't.)"
  (call-without-popcnt
   (lambda ()
     (and (= (word-logcount #xFF) 8)
          (= (word-logcount (1- (expt 2 64))) 64)
          (= (logcount (expt 2 100)) 1)          ; bignum: the 0x11 fault
          (equal (sb-kernel:type-specifier
                  (sb-kernel:specifier-type '(unsigned-byte 32)))
                 '(unsigned-byte 32))            ; the (mod 4294967296) / NIL
          (equal (sb-kernel:type-specifier
                  (sb-kernel:specifier-type '(signed-byte 64)))
                 '(signed-byte 64))
          (= (funcall (compile nil '(lambda (x) (* x 2))) 21) 42)))))

;;; Runs once, at build time (build.sh loads this before save-lisp-and-die). A
;;; patch that does not verify stops the build: shipping it would put the
;;; crash back in the guest with nothing on screen to say why.
(let ((how (patch-logcount-routine)))
  (unless (eq how :unrecognised)
    (unless (logcount-slow-path-ok-p)
      (error "logcount-fix: patched, but LOGCOUNT's slow path is still wrong")))
  (format t "~&;; logcount-fix: ~(~a~)~:[~;, slow path verified with POPCNT off~]~%"
          how (not (eq how :unrecognised))))
