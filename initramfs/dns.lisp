;;;; dns.lisp — a stub resolver in pure Lisp: names -> addresses.
;;;;
;;;; "Stub" means we do not walk the DNS tree ourselves (root -> com -> example.com);
;;;; we ask a RECURSIVE resolver — the one DHCP named in the lease (QEMU's SLIRP
;;;; answers at 10.0.2.3; a home router usually at its own address) — and it does the
;;;; walking. Our side is small: build one query datagram, send it over UDP to port
;;;; 53, parse the answer. That is what glibc's resolver does too, but glibc would
;;;; drag in NSS (/etc/nsswitch.conf, dlopen'ed libnss_*.so plugins) — networking.org
;;;; §5 chose a Lisp resolver instead, and agent.org §10 scheduled it (phase N2).
;;;;
;;;; A DNS message (RFC 1035 §4), every field big-endian:
;;;;
;;;;   header     ID(16) FLAGS(16) QDCOUNT ANCOUNT NSCOUNT ARCOUNT     12 bytes
;;;;   question   NAME  TYPE(16) CLASS(16)
;;;;   answers    NAME  TYPE CLASS TTL(32) RDLENGTH(16) RDATA          (ANCOUNT of them)
;;;;   authority / additional — same shape; we read only the SOA's TTL from authority
;;;;
;;;; A NAME is a sequence of LABELS, each a length byte then that many characters,
;;;; ending in a zero byte: "example.com" = 7 e x a m p l e 3 c o m 0. In a reply, a
;;;; name may instead END in a POINTER — two bytes whose top bits are 11 and whose
;;;; other 14 bits are an offset back into the message where the rest of the name
;;;; already appears. That is DNS's compression, and the one subtle bit of parsing.
;;;;
;;;; What this file does beyond one query, because real networks need it:
;;;;   - CNAME chasing: www.example.com may answer "that is an alias of X" plus X's
;;;;     address — or only the alias, and then we ask again for X (at most 8 hops);
;;;;   - TC -> TCP: a UDP answer too big for 512 bytes comes back TRUNCATED (the TC
;;;;     flag); then we ask again over TCP, where each message is preceded by its
;;;;     2-byte length;
;;;;   - a TTL cache, positive and negative, so a page of 30 requests to one host is
;;;;     one query;
;;;;   - timeouts and retries across every server the lease named;
;;;;   - overrides first, /etc/hosts-style: our /etc/hosts, then ~/.lol/hosts
;;;;     (yours to edit), then anything (setf (dns-override ...)) said at the REPL;
;;;;   - AAAA (IPv6) only when there is an IPv6 default route — asking for addresses
;;;;     we cannot reach just makes every connect try a dead path first.
;;;;
;;;; It also WRITES /etc/resolv.conf from each lease (a *LEASE-HOOK*), so programs
;;;; that use glibc's resolver — the host's tools run in a container (the containers
;;;; track binds our resolv.conf in) — resolve through the same servers.
;;;;
;;;; Public API (consumed by http.lisp and the agent track; package CL-USER):
;;;;   (resolve-host "example.com")            => #(93 184 215 14)   or signals DNS-ERROR
;;;;   (resolve-addresses "example.com")       => list of #(...) vectors, v4 first
;;;;   (dns-query "example.com" :mx)           => the raw records, for the curious

(define-condition dns-error (error)
  ((name :initarg :name :reader dns-error-name)
   (reason :initarg :reason :reader dns-error-reason))
  (:report (lambda (c s) (format s "cannot resolve ~a: ~a" (dns-error-name c) (dns-error-reason c)))))

(defparameter +dns-types+ '((:a . 1) (:ns . 2) (:cname . 5) (:soa . 6) (:ptr . 12)
                            (:mx . 15) (:txt . 16) (:aaaa . 28) (:srv . 33) (:any . 255)))

(defun dns-type-code (type) (or (cdr (assoc type +dns-types+)) (error "unknown DNS type ~s" type)))
(defun dns-type-name (code) (or (car (rassoc code +dns-types+)) code))

(defvar *dns-timeout* 2 "Seconds to wait for one server's answer.")
(defvar *dns-rounds* 2 "How many times to go round all the servers before giving up.")
(defvar *dns-servers* nil
  "A list of server addresses (octet vectors) to use INSTEAD of the lease's — for a
   test, or a network whose DHCP names a broken resolver. NIL = use the lease.")

;;; ---- building and parsing a message ------------------------------------------

(defun encode-dns-name (name out)
  "Append NAME as labels (see the header) to the adjustable octet vector OUT."
  (dolist (label (split-dots (string-right-trim "." name)))
    (when (or (zerop (length label)) (> (length label) 63))
      (error 'dns-error :name name :reason "a label is empty or longer than 63 characters"))
    (vector-push-extend (length label) out)
    (loop for c across label do (vector-push-extend (logand (char-code c) #xff) out)))
  (vector-push-extend 0 out))

(defun split-dots (s)
  (loop with start = 0
        for dot = (position #\. s :start start)
        collect (subseq s start dot)
        while dot do (setf start (1+ dot))))

(defun build-dns-query (id name type)
  "The query datagram: header (RD = 'recursion desired' set), one question."
  (let ((out (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((u16 (n) (vector-push-extend (ldb (byte 8 8) n) out) (vector-push-extend (ldb (byte 8 0) n) out)))
      (u16 id) (u16 #x0100)               ; flags: standard query, RD
      (u16 1) (u16 0) (u16 0) (u16 0)     ; one question, nothing else
      (encode-dns-name name out)
      (u16 (dns-type-code type)) (u16 1)) ; class IN
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun u16-at (v i) (logior (ash (aref v i) 8) (aref v (1+ i))))
(defun u32-at (v i) (logior (ash (u16-at v i) 16) (u16-at v (+ i 2))))

(defun decode-dns-name (v i)
  "Read the name starting at offset I of message V. Returns the name (\"a.b.c\") and
   the offset just past it IN PLACE — after a pointer that is 2 bytes on, however
   long the name it pointed to was. A pointer may lead to another pointer; a loop of
   them is a malformed (or hostile) message, so we stop after 64 jumps."
  (let ((labels '()) (end nil) (jumps 0))
    (loop
      (let ((len (aref v i)))
        (cond ((zerop len) (unless end (setf end (1+ i))) (return))
              ((= (logand len #xc0) #xc0)          ; a compression pointer
               (unless end (setf end (+ i 2)))
               (when (> (incf jumps) 64) (error "DNS name compression loop"))
               (setf i (logand (u16-at v i) #x3fff)))
              (t (push (map 'string #'code-char (subseq v (1+ i) (+ i 1 len))) labels)
                 (incf i (1+ len))))))
    (values (format nil "~{~a~^.~}" (nreverse labels)) end)))

(defun parse-rdata (v type start len)
  (case type
    (1  (subseq v start (+ start 4)))                                    ; A: 4 octets
    (28 (subseq v start (+ start 16)))                                   ; AAAA: 16 octets
    ((5 2 12) (values (decode-dns-name v start)))                        ; CNAME/NS/PTR: a name
    (15 (list (u16-at v start) (values (decode-dns-name v (+ start 2))))) ; MX: pref + name
    (16 (loop with i = start while (< i (+ start len))                   ; TXT: length-prefixed strings
              collect (map 'string #'code-char (subseq v (1+ i) (+ i 1 (aref v i))))
              do (incf i (1+ (aref v i)))))
    (6 (multiple-value-bind (mname j) (decode-dns-name v start)          ; SOA: we want MINIMUM
         (multiple-value-bind (rname k) (decode-dns-name v j)
           (list mname rname :minimum (u32-at v (+ k 16))))))
    (t (subseq v start (+ start len)))))

(defun parse-dns-reply (v)
  "A reply as a plist: :id :rcode :truncated and :answers / :authority, each a list
   of records (:name :type :ttl :data). RCODE 0 = fine, 3 = NXDOMAIN (no such name)."
  (let* ((id (u16-at v 0)) (flags (u16-at v 2))
         (qd (u16-at v 4)) (an (u16-at v 6)) (ns (u16-at v 8))
         (i 12))
    (dotimes (q qd) (setf i (+ (nth-value 1 (decode-dns-name v i)) 4)))  ; skip the question
    (flet ((records (n)
             (loop repeat n
                   collect (multiple-value-bind (name j) (decode-dns-name v i)
                             (let* ((type (u16-at v j)) (ttl (u32-at v (+ j 4)))
                                    (len (u16-at v (+ j 8))) (start (+ j 10)))
                               (setf i (+ start len))
                               (list :name name :type (dns-type-name type) :ttl ttl
                                     :data (parse-rdata v type start len)))))))
      (let* ((answers (records an)) (authority (records ns)))
        (list :id id :rcode (logand flags #xf) :truncated (logbitp 9 flags)
              :answers answers :authority authority)))))

;;; ---- the wire: UDP first, TCP when truncated -----------------------------------

(defun dns-servers ()
  "Where to send queries: *DNS-SERVERS* if set, else the current lease's servers,
   else every lease's, else the nameserver lines of /etc/resolv.conf."
  (or *dns-servers*
      (let ((l (and (fboundp 'current-lease) (current-lease))))
        (and l (mapcar (lambda (o) (coerce o 'vector)) (lease-dns l))))
      (loop for l in (and (boundp '*leases*) *leases*)
            append (mapcar (lambda (o) (coerce o 'vector)) (lease-dns l)))
      (loop for line in (%read-lines "/etc/resolv.conf")
            for f = (%split-ws line)
            when (and (equal (first f) "nameserver") (second f) (not (find #\: (second f))))
              collect (coerce (ipv4-octets (second f)) 'vector))))

(defun dns-udp-exchange (server query)
  "Send QUERY to SERVER:53 over UDP, return the reply octets or NIL on timeout."
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp))
        (buf (make-array 4096 :element-type '(unsigned-byte 8))))
    (unwind-protect
         (progn
           (set-rcv-timeout (sb-bsd-sockets:socket-file-descriptor s) *dns-timeout*)
           (sb-bsd-sockets:socket-connect s server 53)       ; a "connected" UDP socket
           (sb-bsd-sockets:socket-send s query (length query)) ; only hears from SERVER
           (let ((deadline (deadline-in *dns-timeout*)))
             (loop (let ((len (socket-receive-patiently s buf deadline)))   ; EINTR-proof (dhcp.lisp)
                     (cond ((null len) (return nil))           ; timeout / error
                           ((and (>= len 12) (= (u16-at buf 0) (u16-at query 0)))
                            (return (subseq buf 0 len))))))))  ; else: stray, keep waiting
      (ignore-errors (sb-bsd-sockets:socket-close s)))))

(defun read-n-octets (stream n)
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (when (= (read-sequence v stream) n) v)))

(defun dns-tcp-exchange (server query)
  "The same question over TCP: each message is preceded by its length (2 bytes)."
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (ignore-errors
          (let ((fd (sb-bsd-sockets:socket-file-descriptor s)))
            (set-rcv-timeout fd *dns-timeout*)
            (set-snd-timeout fd *dns-timeout*)          ; on Linux this bounds connect() too
            (sb-bsd-sockets:socket-connect s server 53)
            (let ((stream (sb-bsd-sockets:socket-make-stream s :input t :output t
                                                               :element-type '(unsigned-byte 8))))
              (write-byte (ldb (byte 8 8) (length query)) stream)
              (write-byte (ldb (byte 8 0) (length query)) stream)
              (write-sequence query stream) (finish-output stream)
              (let ((len (read-n-octets stream 2)))
                (and len (read-n-octets stream (u16-at len 0)))))))
      (ignore-errors (sb-bsd-sockets:socket-close s)))))

(defun set-snd-timeout (fd seconds)
  "SO_SNDTIMEO (21): like SET-RCV-TIMEOUT, for sends — and for connect()."
  (let ((tv (sb-alien:make-alien sb-alien:unsigned-char 16)))
    (unwind-protect
         (progn (dotimes (i 16) (setf (sb-alien:deref tv i) 0))
                (dotimes (i 8) (setf (sb-alien:deref tv i) (ldb (byte 8 (* 8 i)) seconds)))
                (%setsockopt fd +sol-socket+ 21 tv 16))
      (sb-alien:free-alien tv))))

(defun dns-ask (name type)
  "One question, every server, *DNS-ROUNDS* rounds. Returns the parsed reply of the
   first server that answers at all (an NXDOMAIN is an answer), or signals."
  (let ((servers (dns-servers)))
    (unless servers
      (error 'dns-error :name name :reason "no DNS server — is the network up? (no lease)"))
    (dotimes (round *dns-rounds*)
      (dolist (server servers)
        (let* ((query (build-dns-query (random #x10000) name type))
               (raw (dns-udp-exchange server query)))
          (when raw
            (let ((reply (parse-dns-reply raw)))
              (when (getf reply :truncated)                    ; too big for UDP: use TCP
                (let ((tcp (dns-tcp-exchange server query)))
                  (when tcp (setf reply (parse-dns-reply tcp)))))
              (return-from dns-ask reply))))))
    (error 'dns-error :name name
                      :reason (format nil "no answer from ~{~a~^, ~} (~d s each, ~d rounds)"
                                      (mapcar (lambda (s) (octets->dotted* (coerce s 'list))) servers)
                                      *dns-timeout* *dns-rounds*))))

;;; ---- the cache ---------------------------------------------------------------
;;;
;;; Keyed by (name . type); each entry is (expires-at . records). An NXDOMAIN or an
;;; empty answer is cached too (as NIL records) for the SOA's MINIMUM, capped at five
;;; minutes — the "negative caching" of RFC 2308 — so a typo is not re-asked on
;;; every keystroke of a retry loop.

(defvar *dns-cache* (make-hash-table :test 'equal))
(defvar *dns-cache-lock* (sb-thread:make-mutex :name "dns cache"))

(defun dns-cache-get (name type)
  (sb-thread:with-mutex (*dns-cache-lock*)
    (let ((e (gethash (cons (string-downcase name) type) *dns-cache*)))
      (when (and e (< (get-universal-time) (car e))) (values (cdr e) t)))))

(defun dns-cache-put (name type records ttl)
  (sb-thread:with-mutex (*dns-cache-lock*)
    (setf (gethash (cons (string-downcase name) type) *dns-cache*)
          (cons (+ (get-universal-time) (max 1 ttl)) records))))

(defun dns-flush () (sb-thread:with-mutex (*dns-cache-lock*) (clrhash *dns-cache*)))

(defun dns-query (name type &key (depth 0))
  "The records of TYPE for NAME after following CNAMEs, as (:name :type :ttl :data)
   plists — from the cache when fresh. NIL when the name exists but has no such
   record; signals DNS-ERROR when it does not exist (NXDOMAIN) or no server answers."
  (multiple-value-bind (cached hit) (dns-cache-get name type)
    (when hit
      (if (eq cached :nxdomain)
          (error 'dns-error :name name :reason "no such name (NXDOMAIN, cached)")
          (return-from dns-query cached))))
  (when (> depth 8) (error 'dns-error :name name :reason "CNAME chain longer than 8"))
  (let* ((reply (dns-ask name type))
         (rcode (getf reply :rcode))
         (soa (find :soa (getf reply :authority) :key (lambda (r) (getf r :type))))
         (neg-ttl (min 300 (or (and soa (getf (cddr (getf soa :data)) :minimum)) 60))))
    (cond
      ((= rcode 3)
       (dns-cache-put name type :nxdomain neg-ttl)
       (error 'dns-error :name name :reason "no such name (NXDOMAIN)"))
      ((/= rcode 0)
       (error 'dns-error :name name
                         :reason (format nil "server error, rcode ~d~:[~; (SERVFAIL)~]" rcode (= rcode 2))))
      (t
       ;; Walk the chain inside the answer: name -> CNAME -> CNAME -> records.
       (let ((answers (getf reply :answers)) (target name) (min-ttl most-positive-fixnum))
         (loop for cname = (find-if (lambda (r) (and (eq (getf r :type) :cname)
                                                     (string-equal (getf r :name) target)))
                                    answers)
               repeat 8 while cname
               do (setf target (getf cname :data)
                        min-ttl (min min-ttl (getf cname :ttl))))
         (let ((hits (remove-if-not (lambda (r) (and (eq (getf r :type) type)
                                                     (string-equal (getf r :name) target)))
                                    answers)))
           (cond
             (hits
              (dns-cache-put name type hits (reduce #'min hits :key (lambda (r) (getf r :ttl))
                                                               :initial-value min-ttl))
              hits)
             ((string-not-equal target name)     ; an alias with no address in this reply
              (let ((more (dns-query target type :depth (1+ depth))))
                (dns-cache-put name type more (if more (getf (first more) :ttl) neg-ttl))
                more))
             (t (dns-cache-put name type nil neg-ttl) nil))))))))

;;; ---- overrides: /etc/hosts and friends ----------------------------------------

(defvar *dns-overrides* (make-hash-table :test 'equalp)
  "name -> list of address vectors, consulted before everything. (setf (dns-override
   \"box\") \"10.0.2.2\") at the REPL.")

(defun dns-override (name) (gethash name *dns-overrides*))
(defun (setf dns-override) (address name)
  (setf (gethash name *dns-overrides*)
        (and address (list (if (stringp address) (parse-ip-address address) address)))))

(defun hosts-files ()
  (remove nil (list "/etc/hosts" (lol-path "hosts"))))   ; no ~/.lol/hosts on a diskless boot

(defun hosts-file-lookup (name)
  "Addresses for NAME in the hosts files (\"ADDRESS name alias…\" per line, # comments)."
  (loop for file in (hosts-files)
        append (loop for line in (%read-lines file)
                     for f = (%split-ws (subseq line 0 (position #\# line)))
                     when (and (rest f) (member name (rest f) :test #'string-equal))
                       append (let ((a (ignore-errors (parse-ip-address (first f))))) (and a (list a))))))

;;; ---- addresses ------------------------------------------------------------------

(defun parse-ip-address (string)
  "\"10.0.2.2\" -> #(10 0 2 2); \"::1\" or \"[2001:db8::1]\" -> a 16-octet vector;
   anything else -> NIL. Lets a URL or a REPL user give a literal address."
  (let ((s (string-trim "[]" string)))
    (cond
      ((and (= 3 (count #\. s)) (every (lambda (c) (or (digit-char-p c) (char= c #\.))) s))
       (let ((o (ignore-errors (ipv4-octets s))))
         (and o (every (lambda (x) (<= 0 x 255)) o) (coerce o 'vector))))
      ((and (find #\: s) (every (lambda (c) (or (digit-char-p c 16) (char= c #\:))) s))
       (let* ((dc (search "::" s))
              (parse (lambda (part) (and (plusp (length part))
                                         (mapcar (lambda (g) (parse-integer g :radix 16))
                                                 (split-colons part)))))
              (head (funcall parse (if dc (subseq s 0 dc) s)))
              (tail (and dc (funcall parse (subseq s (+ dc 2)))))
              (groups (if dc (append head (make-list (- 8 (length head) (length tail)) :initial-element 0) tail)
                          head)))
         (when (= (length groups) 8)
           (let ((v (make-array 16 :element-type '(unsigned-byte 8))))
             (loop for g in groups for i from 0 by 2
                   do (setf (aref v i) (ldb (byte 8 8) g) (aref v (1+ i)) (ldb (byte 8 0) g)))
             v))))
      (t nil))))

(defun split-colons (s)
  (loop with start = 0 for c = (position #\: s :start start)
        collect (subseq s start c) while c do (setf start (1+ c))))

(defun address-string (v)
  "#(10 0 2 2) -> \"10.0.2.2\"; a 16-octet vector -> its IPv6 text."
  (if (= (length v) 4)
      (octets->dotted* (coerce v 'list))
      (%hex->ipv6 (format nil "~{~2,'0x~}" (coerce v 'list)))))

(defun ipv6-usable-p ()
  "True when IPv6 can leave this link: a v6 default route exists (net.lisp)."
  (and (ipv6-default-route) t))

(defun resolve-addresses (name)
  "Every address for NAME, IPv4 first, then IPv6 when IPV6-USABLE-P. A literal
   address is returned as itself; overrides and hosts files win over DNS. Signals
   DNS-ERROR when there is nothing at all.

   Why v4 first, against RFC 6724's v6 preference: on the networks this machine
   meets (QEMU's SLIRP, a home router) the v4 path is the one we have tested; a
   half-working v6 path would cost every connect a timeout before falling back.
   The caller (http.lisp) tries the list in order, so v6 is still used when v4
   fails."
  (let ((literal (parse-ip-address name)))
    (when literal (return-from resolve-addresses (list literal))))
  (let ((o (or (dns-override name) (hosts-file-lookup name))))
    (when o (return-from resolve-addresses o)))
  (let* ((v4 (mapcar (lambda (r) (getf r :data)) (dns-query name :a)))
         (v6 (and (ipv6-usable-p)
                  (ignore-errors (mapcar (lambda (r) (getf r :data)) (dns-query name :aaaa)))))
         (all (append v4 v6)))
    (or all (error 'dns-error :name name :reason "the name exists but has no address"))))

(defun resolve-host (name)
  "THE entry point: one address for NAME as an octet vector, #(93 184 215 14) —
   IPv4 when there is one. Signals DNS-ERROR on failure."
  (first (resolve-addresses name)))

;;; ---- /etc/resolv.conf: for everybody else --------------------------------------
;;;
;;; Our own code never reads it (it asks the lease), but a host program run inside a
;;; container — or any glibc program — looks there. Our / is the initramfs, where
;;; /etc is root's; preinit creates /etc/resolv.conf and hands THAT ONE FILE to the
;;; user before dropping root (preinit.c, GIVE_RESOLV_CONF), so PID 1 can rewrite it
;;; with nothing more than ordinary file permission. We write it in place (open with
;;; O_TRUNC): a rename-into-place would need write permission on /etc itself, which
;;; the user deliberately does not have.

(defparameter *resolv-conf* "/etc/resolv.conf")

(defun resolv-conf-text (lease)
  (with-output-to-string (o)
    (format o "# Written by lisp-over-linux (initramfs/dns.lisp) from the DHCP lease on ~a.~%"
            (lease-iface lease))
    (format o "# Rewritten on every lease change; edits here do not survive.~%")
    (dolist (d (lease-dns lease)) (format o "nameserver ~a~%" (octets->dotted d)))
    (when (lease-domain lease) (format o "search ~a~%" (lease-domain lease)))))

(defun write-file-in-place (path text)
  "Overwrite PATH's contents with TEXT without creating or renaming anything."
  (let ((octets (sb-ext:string-to-octets text :external-format :utf-8)))
    (multiple-value-bind (fd err) (sb-unix:unix-open path (logior sb-unix:o_wronly sb-unix:o_trunc) 0)
      (unless fd (error "cannot write ~a: ~a" path (sb-int:strerror err)))
      (unwind-protect
           (sb-sys:with-pinned-objects (octets)
             (sb-unix:unix-write fd octets 0 (length octets)))
        (sb-unix:unix-close fd)))
    path))

(defun write-resolv-conf (lease)
  "The *LEASE-HOOK*: point /etc/resolv.conf at LEASE's servers, and empty the cache
   (a new network may answer differently)."
  (dns-flush)
  (when (lease-dns lease)
    (handler-case (write-file-in-place *resolv-conf* (resolv-conf-text lease))
      (error (c) (format t "~&dns: ~a~%" c)))))

(pushnew 'write-resolv-conf *lease-hooks*)
