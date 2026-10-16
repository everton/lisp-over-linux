;;;; tls.lisp — HTTPS's encryption, borrowed from the HOST's OpenSSL at runtime.
;;;;
;;;; TLS (the "S" in HTTPS) is a handshake — agree on keys, check the server's
;;;; certificate chain up to a trusted root, check the name — and then a record layer
;;;; that encrypts every byte. Writing it ourselves is agent.org §9's late
;;;; sub-project (~1500 lines of carefully tested crypto). Until then we do what the
;;;; project allows when writing it ourselves is unrealistic (common.md, steps.org):
;;;; call a host library. The laptop's Debian has OpenSSL 3 (libssl.so.3 +
;;;; libcrypto.so.3), mounted read-only at /mnt/host (doc/storage.org).
;;;;
;;;; THE ONE RULE: the library is loaded at RUNTIME, never at build time. A saved SBCL
;;;; image remembers every shared object it had loaded and tries to reopen it on
;;;; startup; if libssl were loaded while building, /sbin/lisp would refuse to start
;;;; on a machine without /mnt/host. So nothing here touches OpenSSL until the first
;;;; HTTPS request calls ENSURE-OPENSSL, and every OpenSSL function is looked up BY
;;;; NAME at call time (SSL-SYM), not linked when this file is compiled. Without
;;;; /mnt/host, an HTTPS request fails with one clear sentence (TLS-ERROR) and
;;;; everything else carries on.
;;;;
;;;; Why it works at all: the image ships the BUILD host's glibc (initramfs.sbcl.list),
;;;; and /mnt/host IS that host — so the libssl found there was built against the libc
;;;; we already run. If the host upgrades glibc and the image is not rebuilt, dlopen
;;;; can fail with "version `GLIBC_2.xx' not found"; the error says so, and the cure
;;;; is ./build.sh.
;;;;
;;;; libssl needs libcrypto, which needs libz and libzstd (`ldd`, Debian 14). The
;;;; dynamic loader would look for those in OUR /lib, where they are not; but it never
;;;; looks for a library already loaded under the same SONAME. So we load them
;;;; ourselves, deepest first, each by full path: libz, libzstd, libcrypto, libssl.
;;;;
;;;; What we check, so "encrypted" also means "talking to the right server":
;;;;   - the chain, against the host's CA bundle (/etc/ssl/certs/ca-certificates.crt,
;;;;     Debian's ca-certificates) — SSL_VERIFY_PEER + load_verify_locations;
;;;;   - the NAME: SSL_set1_host makes OpenSSL require the certificate to be for the
;;;;     host we asked for (a valid certificate for evil.example is still refused);
;;;;   - SNI: we say which host we want in the ClientHello, so a server hosting many
;;;;     sites presents the right certificate;
;;;;   - TLS 1.2 at least.
;;;; And the clock matters: a certificate is valid between two dates (sntp.lisp).

(define-condition tls-error (error)
  ((message :initarg :message :reader tls-error-message))
  (:report (lambda (c s) (write-string (tls-error-message c) s))))

(defun tls-fail (fmt &rest args)
  (error 'tls-error :message (apply #'format nil fmt args)))

(defvar *host-root* "/mnt/host"
  "Where the host's Debian is mounted. \"\" when testing on the host itself.")

(defparameter *openssl-libdirs* '("/usr/lib/x86_64-linux-gnu/" "/lib/x86_64-linux-gnu/"))
(defparameter *openssl-libs* '("libz.so.1" "libzstd.so.1" "libcrypto.so.3" "libssl.so.3")
  "Loaded in THIS order — each one's dependencies before it (see the header).")
(defparameter *ca-bundle* "/etc/ssl/certs/ca-certificates.crt" "Relative to *HOST-ROOT*.")

(defvar *openssl-loaded* nil "T once the four libraries are in.")
(defvar *ssl-ctx* nil "One SSL_CTX (the configured 'factory' for connections), made once.")
(defvar *ssl-lock* (sb-thread:make-mutex :name "openssl setup"))
(watch-locks (lambda () (list *ssl-lock*)))   ; named in a STALL report (session-log.lisp)

(defun host-file (path) (concatenate 'string *host-root* path))

(defun find-host-lib (name)
  (loop for dir in *openssl-libdirs*
        for p = (host-file (concatenate 'string dir name))
        when (probe-file p) return p))

(defun ensure-openssl ()
  "Load the host's OpenSSL once. Signals TLS-ERROR with the reason when it cannot."
  (sb-thread:with-recursive-lock (*ssl-lock*)
    (unless *openssl-loaded*
      (unless (probe-file (host-file "/usr/"))
        (tls-fail "HTTPS needs the host's OpenSSL, and there is no host system at ~a ~
                   (this boot found no disk — doc/storage.org)" (if (string= *host-root* "") "/" *host-root*)))
      (dolist (lib *openssl-libs*)
        (let ((path (find-host-lib lib)))
          (unless path
            (tls-fail "HTTPS: ~a not found under ~a~{~a~^ or ~} (is the host's openssl installed?)"
                      lib *host-root* *openssl-libdirs*))
          (handler-case (sb-alien:load-shared-object path :dont-save t)
            (error (c)
              (tls-fail "HTTPS: loading ~a failed: ~a~@[~%  (the host's libraries are newer than ~
                         the image's glibc — rebuild with ./build.sh)~]"
                        path c (search "GLIBC_" (princ-to-string c)))))))
      (setf *openssl-loaded* t))
    t))

;;; ---- calling a function we only know by name ---------------------------------

(defvar *ssl-syms* (make-hash-table :test 'equal))

(defun ssl-sym (name)
  "The address of the OpenSSL function NAME, as a SAP. Looked up in the loaded
   libraries when first asked (dlsym, in effect), then remembered."
  (or (gethash name *ssl-syms*)
      (let ((addr (sb-sys:find-dynamic-foreign-symbol-address name)))
        (unless addr (tls-fail "HTTPS: ~a is missing from the host's OpenSSL" name))
        (setf (gethash name *ssl-syms*) (sb-sys:int-sap addr)))))

(defmacro define-ssl-call (lisp-name c-name result-type &rest args)
  "Define LISP-NAME to call the C function C-NAME through its runtime address. ARGS
   are (name alien-type). Spelled out once here so each call below reads like the C
   prototype it mirrors."
  `(defun ,lisp-name ,(mapcar #'first args)
     (sb-alien:alien-funcall
      (sb-alien:sap-alien (ssl-sym ,c-name)
                          (function ,result-type ,@(mapcar #'second args)))
      ,@(mapcar #'first args))))

;; Pointers to OpenSSL objects (SSL_CTX *, SSL *) stay opaque: SYSTEM-AREA-POINTERs.
(define-ssl-call %tls-client-method "TLS_client_method" sb-sys:system-area-pointer)
(define-ssl-call %ssl-ctx-new "SSL_CTX_new" sb-sys:system-area-pointer
  (method sb-sys:system-area-pointer))
(define-ssl-call %ssl-ctx-set-verify "SSL_CTX_set_verify" sb-alien:void
  (ctx sb-sys:system-area-pointer) (mode sb-alien:int) (cb sb-sys:system-area-pointer))
(define-ssl-call %ssl-ctx-load-verify-locations "SSL_CTX_load_verify_locations" sb-alien:int
  (ctx sb-sys:system-area-pointer) (file sb-alien:c-string) (dir sb-sys:system-area-pointer))
(define-ssl-call %ssl-ctx-ctrl "SSL_CTX_ctrl" sb-alien:long
  (ctx sb-sys:system-area-pointer) (cmd sb-alien:int) (larg sb-alien:long)
  (parg sb-sys:system-area-pointer))
(define-ssl-call %ssl-new "SSL_new" sb-sys:system-area-pointer (ctx sb-sys:system-area-pointer))
(define-ssl-call %ssl-free "SSL_free" sb-alien:void (ssl sb-sys:system-area-pointer))
(define-ssl-call %ssl-set-fd "SSL_set_fd" sb-alien:int (ssl sb-sys:system-area-pointer) (fd sb-alien:int))
(define-ssl-call %ssl-ctrl "SSL_ctrl" sb-alien:long
  (ssl sb-sys:system-area-pointer) (cmd sb-alien:int) (larg sb-alien:long) (parg sb-alien:c-string))
(define-ssl-call %ssl-set1-host "SSL_set1_host" sb-alien:int
  (ssl sb-sys:system-area-pointer) (host sb-alien:c-string))
(define-ssl-call %ssl-set-alpn-protos "SSL_set_alpn_protos" sb-alien:int
  (ssl sb-sys:system-area-pointer) (protos sb-sys:system-area-pointer) (len sb-alien:unsigned-int))
(define-ssl-call %ssl-connect "SSL_connect" sb-alien:int (ssl sb-sys:system-area-pointer))
(define-ssl-call %ssl-get-verify-result "SSL_get_verify_result" sb-alien:long
  (ssl sb-sys:system-area-pointer))
(define-ssl-call %x509-verify-cert-error-string "X509_verify_cert_error_string" sb-alien:c-string
  (code sb-alien:long))
(define-ssl-call %ssl-read "SSL_read" sb-alien:int
  (ssl sb-sys:system-area-pointer) (buf sb-sys:system-area-pointer) (n sb-alien:int))
(define-ssl-call %ssl-write "SSL_write" sb-alien:int
  (ssl sb-sys:system-area-pointer) (buf sb-sys:system-area-pointer) (n sb-alien:int))
(define-ssl-call %ssl-get-error "SSL_get_error" sb-alien:int
  (ssl sb-sys:system-area-pointer) (ret sb-alien:int))
(define-ssl-call %ssl-shutdown "SSL_shutdown" sb-alien:int (ssl sb-sys:system-area-pointer))
(define-ssl-call %ssl-get-version "SSL_get_version" sb-alien:c-string (ssl sb-sys:system-area-pointer))
(define-ssl-call %err-get-error "ERR_get_error" sb-alien:unsigned-long)
(define-ssl-call %err-error-string-n "ERR_error_string_n" sb-alien:void
  (e sb-alien:unsigned-long) (buf sb-sys:system-area-pointer) (len sb-alien:unsigned-long))

(defconstant +ssl-verify-peer+ 1)
(defconstant +ssl-ctrl-set-tlsext-hostname+ 55)   ; SSL_set_tlsext_host_name is a macro over this
(defconstant +ssl-ctrl-set-min-proto-version+ 123)
(defconstant +tls1-2-version+ #x0303)
;; SSL_get_error's answers we tell apart:
(defconstant +ssl-error-zero-return+ 6)           ; the peer closed TLS cleanly: EOF
(defconstant +ssl-error-syscall+ 5)               ; the socket failed (or our timeout fired)

(defun openssl-errors ()
  "Drain OpenSSL's error queue into one string (its errors are a per-thread queue,
   not a return value)."
  (let ((buf (make-array 256 :element-type '(unsigned-byte 8))))
    (with-output-to-string (o)
      (loop for e = (%err-get-error) until (zerop e)
            do (sb-sys:with-pinned-objects (buf)
                 (%err-error-string-n e (sb-sys:vector-sap buf) 256))
               (format o "~&  ~a" (sb-ext:octets-to-string buf :end (or (position 0 buf) 256)
                                                               :external-format :latin-1))))))

(defun ssl-context ()
  "The one SSL_CTX: client method, verify the peer, the host's CA bundle, TLS >= 1.2."
  (ensure-openssl)
  (sb-thread:with-recursive-lock (*ssl-lock*)
    (or *ssl-ctx*
        (let ((ctx (%ssl-ctx-new (%tls-client-method)))
              (ca (host-file *ca-bundle*)))
          (when (zerop (sb-sys:sap-int ctx)) (tls-fail "SSL_CTX_new failed:~a" (openssl-errors)))
          (unless (probe-file ca) (tls-fail "HTTPS: no CA bundle at ~a — cannot check certificates" ca))
          (%ssl-ctx-set-verify ctx +ssl-verify-peer+ (sb-sys:int-sap 0))
          (when (/= 1 (%ssl-ctx-load-verify-locations ctx ca (sb-sys:int-sap 0)))
            (tls-fail "HTTPS: cannot load ~a:~a" ca (openssl-errors)))
          (%ssl-ctx-ctrl ctx +ssl-ctrl-set-min-proto-version+ +tls1-2-version+ (sb-sys:int-sap 0))
          (setf *ssl-ctx* ctx)))))

;;; ---- a TLS session over a connected socket ------------------------------------

(defstruct (tls (:constructor %make-tls (ssl fd host))) ssl fd host)

(defun tls-interrupted-p (ssl-error errno started)
  "Did the OpenSSL call that just returned SSL-ERROR (with ERRNO) get interrupted by a
   signal, rather than time out or fail? SBCL stops every thread with a signal for each
   GC, and the kernel does NOT restart a recv/send on a socket with SO_RCVTIMEO /
   SO_SNDTIMEO (signal(7)): it returns EINTR, which OpenSSL reports as WANT_READ /
   WANT_WRITE — the same code as our timeout. A real timeout says EAGAIN (11) and takes
   at least the timeout; EINTR (4), or a WANT_* well before any timeout could fire,
   is an interruption, and the call is simply made again. STARTED is the
   GET-INTERNAL-REAL-TIME before the call."
  (or (and (member ssl-error '(2 3 5)) (eql errno 4))
      (and (member ssl-error '(2 3)) (not (eql errno 11))
           (< (- (get-internal-real-time) started) (floor internal-time-units-per-second 2)))))

(defun tls-connect (fd host)
  "Run the TLS handshake on the connected socket FD for HOST (name checked against
   the certificate). Returns a TLS object, or signals TLS-ERROR saying WHY — a
   certificate problem is named (\"certificate has expired\", \"hostname mismatch\").
   A handshake step a signal interrupted (TLS-INTERRUPTED-P) is made again."
  (let ((ssl (%ssl-new (ssl-context))))
    (when (zerop (sb-sys:sap-int ssl)) (tls-fail "SSL_new failed:~a" (openssl-errors)))
    (handler-bind ((error (lambda (c) (declare (ignore c)) (%ssl-free ssl))))
      (%ssl-set-fd ssl fd)
      (unless (parse-ip-address host)                ; SNI carries names, never addresses
        (%ssl-ctrl ssl +ssl-ctrl-set-tlsext-hostname+ 0 host))
      (when (/= 1 (%ssl-set1-host ssl host)) (tls-fail "SSL_set1_host failed for ~a" host))
      ;; ALPN: say we speak HTTP/1.1 (some servers would otherwise offer h2 only).
      (let ((alpn (coerce #(8 104 116 116 112 47 49 46 49) '(simple-array (unsigned-byte 8) (*)))))
        (sb-sys:with-pinned-objects (alpn) (%ssl-set-alpn-protos ssl (sb-sys:vector-sap alpn) 9)))
      (loop for tries from 0
            do (let* ((t0 (get-internal-real-time))
                      (r (%ssl-connect ssl))
                      (errno (sb-alien:get-errno)))
                 (when (= r 1) (return))
                 (let ((e (%ssl-get-error ssl r)))
                   (unless (and (< tries 200) (tls-interrupted-p e errno t0))
                     (let ((v (%ssl-get-verify-result ssl)))
                       (if (/= v 0)
                           (tls-fail "TLS handshake with ~a: certificate rejected: ~a~@[~%  (is the clock right? ~a)~]"
                                     host (%x509-verify-cert-error-string v)
                                     (and (member v '(9 10)) (fboundp 'describe-clock) (describe-clock)))
                           (tls-fail "TLS handshake with ~a failed (SSL error ~d)~a"
                                     host e (openssl-errors))))))))
      (%make-tls ssl fd host))))

(defun tls-read (tls buf start end)
  "Read up to END-START decrypted octets into BUF at START. Returns the count, 0 at
   end of stream. Signals TLS-ERROR on a broken stream or a timeout. A read that a
   signal interrupted (TLS-INTERRUPTED-P — a GC, typically) is not a timeout: it is
   made again."
  (let ((ssl (tls-ssl tls)))
    (sb-sys:with-pinned-objects (buf)
      (loop for tries from 0
            do (let* ((t0 (get-internal-real-time))
                      (n (%ssl-read ssl (sb-sys:sap+ (sb-sys:vector-sap buf) start) (- end start)))
                      (errno (sb-alien:get-errno)))
                 (when (plusp n) (return n))
                 (let ((e (%ssl-get-error ssl n)))
                   (cond ((= e +ssl-error-zero-return+) (return 0))
                         ;; a peer that just closes the TCP connection, without TLS's
                         ;; close_notify, is common and harmless for Connection: close
                         ((and (= e +ssl-error-syscall+) (zerop n) (not (member errno '(11 4))))
                          (return 0))
                         ((and (< tries 200) (tls-interrupted-p e errno t0)))  ; read again
                         ;; our SO_RCVTIMEO fired: EAGAIN, which OpenSSL reports as
                         ;; WANT_READ (2) — "try again" — or as SYSCALL + errno 11
                         ((or (member e '(2 3)) (and (= e +ssl-error-syscall+) (eql errno 11)))
                          (tls-fail "timed out waiting for ~a" (tls-host tls)))
                         (t (tls-fail "TLS read from ~a failed (SSL error ~d)~a"
                                      (tls-host tls) e (openssl-errors))))))))))

(defun tls-write (tls octets)
  "Encrypt and send all of OCTETS. A write a signal interrupted is made again."
  (let ((ssl (tls-ssl tls)) (off 0) (len (length octets)) (tries 0))
    (sb-sys:with-pinned-objects (octets)
      (loop while (< off len)
            do (let* ((t0 (get-internal-real-time))
                      (n (%ssl-write ssl (sb-sys:sap+ (sb-sys:vector-sap octets) off) (- len off)))
                      (errno (sb-alien:get-errno)))
                 (if (plusp n)
                     (incf off n)
                     (let ((e (%ssl-get-error ssl n)))
                       (unless (and (< (incf tries) 200) (tls-interrupted-p e errno t0))
                         (tls-fail "TLS write to ~a failed (SSL error ~d)~a"
                                   (tls-host tls) e (openssl-errors))))))))
    len))

(defun tls-close (tls)
  "Say goodbye (close_notify) and free the session. The socket is the caller's."
  (when (tls-ssl tls)
    (ignore-errors (%ssl-shutdown (tls-ssl tls)))
    (%ssl-free (tls-ssl tls))
    (setf (tls-ssl tls) nil)))

(defun tls-version (tls) (%ssl-get-version (tls-ssl tls)))
