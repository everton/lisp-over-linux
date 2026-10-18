;;;; dhcp.lisp — get an IP from a DHCP server, in pure Lisp, and keep it.
;;;;
;;;; Replaces the hard-coded static address with a real DHCP handshake:
;;;;   DISCOVER  --broadcast-->  (server)  --OFFER-->   (we get an offered IP)
;;;;   REQUEST   --broadcast-->  (server)  --ACK-->     (lease confirmed)
;;;; then we configure the interface from the lease (reusing net.lisp's ioctls),
;;;; install the DEFAULT ROUTE through the leased router (net.lisp's SIOCADDRT),
;;;; and keep the lease as a Lisp object that a background thread renews before it
;;;; runs out (the "netd" thread at the end of this file).
;;;;
;;;; The chicken-and-egg of DHCP is that we must talk on an interface that has NO
;;;; IP yet. SENDING is easy over an ordinary UDP socket:
;;;;   - SO_BINDTODEVICE ties the socket to one interface, so a broadcast goes
;;;;     out the right NIC even with no routing table entry.
;;;; RECEIVING is the hard half. We set the DHCP *broadcast flag*, asking the server
;;;; to BROADCAST its replies to 255.255.255.255:68, which a UDP socket receives.
;;;; QEMU's server obeys it — but real routers may not (the laptop's home mesh
;;;; router, 2026-10-12, associated fine and then never gave an address): they
;;;; UNICAST the offer to the address being offered, at our MAC. The kernel drops
;;;; that packet before any UDP socket sees it — the interface does not own that
;;;; address yet. So we also open a PACKET socket (AF_PACKET), which sees every IP
;;;; packet the NIC receives before the IP layer judges it, and pick the DHCP
;;;; replies out ourselves (IPv4 header -> UDP header -> destination port 68). It
;;;; needs CAP_NET_RAW, one of PID 1's five capabilities (storage.org); without it
;;;; we fall back to the UDP socket alone and say so in the netd log.
;;;; Built on the sb-bsd-sockets contrib baked into the image. See networking.org.
;;;;
;;;; Nothing here is eth0-specific any more: the same code leases an address on
;;;; wlan0 once wifi.lisp has associated it with an access point.

(defconstant +siocgifhwaddr+ #x8927)    ; get interface MAC
(defconstant +sol-socket+      1)
(defconstant +so-rcvtimeo+    20)

(sb-alien:define-alien-routine ("setsockopt" %setsockopt) sb-alien:int
  (fd sb-alien:int) (level sb-alien:int) (optname sb-alien:int)
  (optval (sb-alien:* sb-alien:unsigned-char)) (optlen sb-alien:int))

(defvar *gateway* nil "Default-gateway octets from the last DHCP lease, or NIL.")
(defvar *dns*     nil "DNS-server octets from the last DHCP lease (the first server), or NIL.")

;;; ---- small helpers --------------------------------------------------------

(defun octets->dotted (octets)
  "(10 0 2 15) -> \"10.0.2.15\"."
  (format nil "~{~d~^.~}" octets))

(defun be32 (bytes)
  "Four big-endian octets -> integer (0 if BYTES is NIL)."
  (if bytes (+ (ash (first bytes) 24) (ash (second bytes) 16)
               (ash (third bytes) 8) (fourth bytes))
      0))

(defun interface-mac (ifname)
  "The 6-byte MAC of IFNAME as a list, via SIOCGIFHWADDR, or NIL."
  (let ((fd (%socket +af-inet+ +sock-dgram+ 0)))
    (when (minusp fd) (return-from interface-mac nil))
    (unwind-protect
         (let ((buf (sb-alien:make-alien sb-alien:unsigned-char +ifreq-size+)))
           (unwind-protect
                (progn
                  (%zero-ifreq buf) (%set-ifname buf ifname)
                  (when (not (minusp (%ioctl fd +siocgifhwaddr+ buf)))
                    ;; ifr_hwaddr: sa_family at 16..17, the 6 MAC bytes at 18..23
                    (loop for i from 18 below 24 collect (sb-alien:deref buf i))))
             (sb-alien:free-alien buf)))
      (%close fd))))

(defun set-rcv-timeout (fd seconds)
  "SO_RCVTIMEO so a missing DHCP server doesn't block us forever."
  (let ((tv (sb-alien:make-alien sb-alien:unsigned-char 16)))   ; struct timeval {sec; usec}
    (unwind-protect
         (progn
           (dotimes (i 16) (setf (sb-alien:deref tv i) 0))
           (dotimes (i 8) (setf (sb-alien:deref tv i) (ldb (byte 8 (* 8 i)) seconds))) ; tv_sec LE
           (%setsockopt fd +sol-socket+ +so-rcvtimeo+ tv 16))
      (sb-alien:free-alien tv))))

;;; SO_RCVTIMEO has a catch: a socket read with a receive timeout is NEVER restarted
;;; after a signal handler runs, even with SA_RESTART (signal(7), "Interruption of
;;; system calls"). SBCL stops every thread with a signal for each GC, so any read
;;; here can come back early with EINTR, and SB-BSD-SOCKETS reports that as an
;;; empty receive (NIL), while a real timeout SIGNALS. Read as "timed out", an EINTR
;;; ended a DHCP attempt, a DNS query or an SNTP exchange on whatever GC happened to
;;; land in it (review of 2026-10-16).

(defun deadline-in (seconds)
  "The internal-real-time SECONDS from now."
  (+ (get-internal-real-time) (round (* seconds internal-time-units-per-second))))

(defun socket-receive-patiently (socket buf deadline)
  "SB-BSD-SOCKETS:SOCKET-RECEIVE into BUF, reading again after an EINTR until DEADLINE
   (internal-real-time) has passed. -> the length received, or NIL on timeout or
   error. Each read is still bounded by the socket's own SO_RCVTIMEO."
  (loop
    (multiple-value-bind (b len)
        (handler-case (sb-bsd-sockets:socket-receive socket buf (length buf))
          (error () (return nil)))           ; EAGAIN: the socket's timeout ran out
      (declare (ignore b))
      (cond (len (return len))
            ((>= (get-internal-real-time) deadline) (return nil))))))   ; EINTR: again

(defun group-octets (bytes &optional (n 4))
  "A DHCP option that lists several addresses is just their bytes end to end:
   (10 0 2 3 8 8 8 8) -> ((10 0 2 3) (8 8 8 8))."
  (loop for rest on bytes by (lambda (l) (nthcdr n l))
        when (>= (length rest) n) collect (subseq rest 0 n)))

;;; ---- the DHCP packet (BOOTP header + options) -----------------------------

;; Option 55, the PARAMETER REQUEST LIST: which options we want in the reply.
;;   1 subnet mask · 3 router · 6 DNS servers · 15 domain name · 42 NTP servers
;;   51 lease time · 58 renewal time (T1) · 59 rebinding time (T2)
(defparameter +dhcp-wanted-options+ '(1 3 6 15 42 51 58 59))

(defun build-dhcp (msg-type mac xid &key requested-ip server-id)
  "Build a DHCP packet (a (unsigned-byte 8) vector). MSG-TYPE is 1=DISCOVER /
   3=REQUEST. REQUESTED-IP and SERVER-ID (octet lists) are added for REQUEST;
   a REQUEST with a REQUESTED-IP but no SERVER-ID is the 'I had this address,
   may I keep it?' form a renewal uses (RFC 2131 §4.3.2, INIT-REBOOT)."
  (let ((p (make-array 300 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref p 0) 1 (aref p 1) 1 (aref p 2) 6)   ; op=BOOTREQUEST htype=ETHER hlen=6
    (setf (aref p 4) (ldb (byte 8 24) xid) (aref p 5) (ldb (byte 8 16) xid)  ; xid (big-endian)
          (aref p 6) (ldb (byte 8 8) xid)  (aref p 7) (ldb (byte 8 0) xid))
    (setf (aref p 10) #x80)                          ; flags = 0x8000 (broadcast replies)
    (dotimes (i 6) (setf (aref p (+ 28 i)) (nth i mac)))   ; chaddr = MAC
    (setf (aref p 236) #x63 (aref p 237) #x82        ; DHCP magic cookie 63 82 53 63
          (aref p 238) #x53 (aref p 239) #x63)
    (let ((o 240))
      (flet ((emit (&rest bytes) (dolist (b bytes) (setf (aref p o) b) (incf o))))
        (emit 53 1 msg-type)                         ; opt 53: message type
        (when requested-ip (emit 50 4) (apply #'emit requested-ip))   ; opt 50: requested IP
        (when server-id    (emit 54 4) (apply #'emit server-id))      ; opt 54: server id
        (emit 55 (length +dhcp-wanted-options+))     ; opt 55: what we want back
        (apply #'emit +dhcp-wanted-options+)
        (emit 255)))                                  ; opt 255: end
    p))

(defun parse-dhcp (vec len)
  "Parse a DHCP reply into an alist: (:xid . n) (:yiaddr a b c d) and (code . bytes)
   for each option. NIL if it isn't a DHCP packet."
  (when (and (>= len 240)
             (= (aref vec 236) #x63) (= (aref vec 237) #x82)
             (= (aref vec 238) #x53) (= (aref vec 239) #x63))
    (let ((acc (list (cons :xid (be32 (list (aref vec 4) (aref vec 5) (aref vec 6) (aref vec 7))))
                     (cons :yiaddr (list (aref vec 16) (aref vec 17) (aref vec 18) (aref vec 19)))))
          (i 240))
      (loop while (< i len) do
        (let ((code (aref vec i)))
          (cond ((= code 0) (incf i))                ; pad
                ((= code 255) (loop-finish))          ; end
                ((< (1+ i) len)
                 (let ((olen (aref vec (1+ i))))
                   (push (cons code (loop for k below olen
                                          when (< (+ i 2 k) len)
                                            collect (aref vec (+ i 2 k))))
                         acc)
                   (incf i (+ 2 olen))))
                (t (loop-finish)))))
      acc)))

;;; ---- the exchange ---------------------------------------------------------

(defconstant +af-packet+ 17)
(defconstant +eth-p-ip-be+ #x0008)   ; htons(ETH_P_IP = #x0800) on little-endian x86

(sb-alien:define-alien-routine ("recv" %recv) sb-alien:long
  (fd sb-alien:int) (buf sb-sys:system-area-pointer) (len sb-alien:unsigned-long)
  (flags sb-alien:int))
(sb-alien:define-alien-routine ("bind" %bind) sb-alien:int
  (fd sb-alien:int) (addr (sb-alien:* sb-alien:unsigned-char)) (len sb-alien:int))

(defvar *dhcp-raw-fd* nil
  "While CALL-WITH-DHCP-SOCKET runs: the PACKET socket replies are read from, or
   NIL when it could not be opened (then the UDP socket is read instead).")
(defvar *dhcp-packet-socket* t
  "NIL reads replies from the UDP socket only — how this file worked before
   2026-10-12; poc/dhcp-test.lisp sets it to prove a unicasting server defeats that.")
(defvar *dhcp-timeout* 2 "Seconds each DHCP attempt waits for its reply.")
(defvar *dhcp-stats* nil "Counts for the attempt in progress: (:sent :seen :dhcp).")

(defun dhcp-reply-from-ip-packet (buf len)
  "BUF holds one IPv4 packet (a PACKET socket of type SOCK_DGRAM strips the link
   header). If it is UDP to port 68, return the DHCP payload's offset, else NIL.
     IPv4: byte 0 = version<<4 | IHL (header length in 32-bit words), byte 9 = protocol
     UDP : 8 bytes after the IP header — source port, destination port, length, checksum"
  (when (>= len 28)
    (let ((ihl (* 4 (logand (aref buf 0) #xf))))
      (and (= (ash (aref buf 0) -4) 4)
           (= (aref buf 9) 17)                                ; 17 = UDP
           (>= len (+ ihl 8))
           (= 68 (logior (ash (aref buf (+ ihl 2)) 8) (aref buf (+ ihl 3))))
           (+ ihl 8)))))

(defun dhcp-receive (sock buf)
  "One packet's worth of DHCP reply, as (values VECTOR LENGTH) of the DHCP part,
   :INTERRUPTED when a signal (a GC) cut the wait short, or NIL on timeout. From the
   PACKET socket when there is one, else the UDP socket."
  (if *dhcp-raw-fd*
      (let ((n (sb-sys:with-pinned-objects (buf)
                 (%recv *dhcp-raw-fd* (sb-sys:vector-sap buf) (length buf) 0))))
        (cond ((plusp n)
               (let ((off (dhcp-reply-from-ip-packet buf n)))
                 (if off
                     (values (subseq buf off n) (- n off))
                     (values :other 0))))
              ((and (minusp n) (eql (sb-alien:get-errno) sb-unix:eintr)) :interrupted)))
      (multiple-value-bind (b len) (sb-bsd-sockets:socket-receive sock buf (length buf))
        (declare (ignore b))
        (cond ((null len) :interrupted)        ; SB-BSD-SOCKETS' EINTR (a timeout signals)
              ((plusp len) (values buf len))))))

(defun dhcp-xchg (sock packet xid tries)
  "Broadcast PACKET and wait for a matching (same XID) DHCP reply, retrying up to
   TRIES times. Each attempt keeps listening until its *DHCP-TIMEOUT* runs out: a
   packet that is not our reply (another client's, an ARP-triggered IP packet on
   the PACKET socket) is skipped, not counted as a failed attempt — on a busy
   Wi-Fi network there are many. Returns the parsed reply, or NIL."
  (let ((buf (make-array 2048 :element-type '(unsigned-byte 8))))
    (dotimes (attempt tries)
      (ignore-errors
        (sb-bsd-sockets:socket-send sock packet (length packet)
                                    :address (list #(255 255 255 255) 67))
        (when *dhcp-stats* (incf (getf *dhcp-stats* :sent))))
      (let ((deadline (+ (get-internal-real-time)
                         (* *dhcp-timeout* internal-time-units-per-second))))
        (loop while (< (get-internal-real-time) deadline) do
          (handler-case
              (multiple-value-bind (v len) (dhcp-receive sock buf)
                (cond ((null v) (loop-finish))           ; the socket's own timeout
                      ((eq v :interrupted))                 ; a GC's signal: listen on
                      ((eq v :other) (when *dhcp-stats* (incf (getf *dhcp-stats* :seen))))
                      (t (when *dhcp-stats* (incf (getf *dhcp-stats* :seen)))
                         (let ((reply (parse-dhcp v len)))
                           (when reply
                             (when *dhcp-stats* (incf (getf *dhcp-stats* :dhcp)))
                             (when (eql (cdr (assoc :xid reply)) xid)
                               (return-from dhcp-xchg reply)))))))
            (serious-condition () (loop-finish))))))   ; timeout / transient error -> retry
    nil))

(defun reply->plist (ack)
  "An ACK's options as the plist DHCP-ACQUIRE returns."
  (list :ip     (cdr (assoc :yiaddr ack))
        :mask   (cdr (assoc 1 ack))
        :router (first (group-octets (cdr (assoc 3 ack))))
        :dns    (group-octets (cdr (assoc 6 ack)))
        :domain (let ((d (cdr (assoc 15 ack))))
                  (and d (string-right-trim '(#\Nul) (map 'string #'code-char d))))
        :ntp    (group-octets (cdr (assoc 42 ack)))
        :server (cdr (assoc 54 ack))
        :lease  (be32 (cdr (assoc 51 ack)))
        :t1     (let ((t1 (cdr (assoc 58 ack)))) (and t1 (be32 t1)))))

(defconstant +siocgifindex+ #x8933)   ; linux/include/uapi/linux/sockios.h

(defun interface-index (ifname)
  "The kernel's index for IFNAME (SIOCGIFINDEX: ifr_ifindex, an int at byte 16 of
   the ifreq), or NIL. Not /sys/class/net/IF/ifindex: sysfs shows the network
   namespace it was MOUNTED in, which is wrong inside a test namespace."
  (let ((fd (%socket +af-inet+ +sock-dgram+ 0)))
    (unless (minusp fd)
      (let ((buf (sb-alien:make-alien sb-alien:unsigned-char +ifreq-size+)))
        (unwind-protect
             (progn
               (%zero-ifreq buf) (%set-ifname buf ifname)
               (unless (minusp (%ioctl fd +siocgifindex+ buf))
                 (loop for i below 4 sum (ash (sb-alien:deref buf (+ 16 i)) (* 8 i)))))
          (sb-alien:free-alien buf)
          (%close fd))))))

(defun open-dhcp-packet-socket (ifname timeout)
  "A PACKET socket (SOCK_DGRAM: link header stripped) that receives every IPv4
   packet arriving on IFNAME, with a TIMEOUT-second receive timeout; or NIL (no
   CAP_NET_RAW, no such interface). Bound with a struct sockaddr_ll:
     u16 family | u16 protocol (big-endian) | i32 ifindex | u16 hatype | u8 pkttype
     | u8 halen | u8 addr[8]      — 20 bytes; only the first three matter to bind."
  (let ((index (interface-index ifname)))
    (when index
      (let ((fd (%socket +af-packet+ +sock-dgram+ +eth-p-ip-be+)))
        (unless (minusp fd)
          (let ((sll (sb-alien:make-alien sb-alien:unsigned-char 20)))
            (unwind-protect
                 (progn
                   (dotimes (i 20) (setf (sb-alien:deref sll i) 0))
                   (setf (sb-alien:deref sll 0) +af-packet+
                         (sb-alien:deref sll 2) (ldb (byte 8 0) +eth-p-ip-be+)
                         (sb-alien:deref sll 3) (ldb (byte 8 8) +eth-p-ip-be+))
                   (dotimes (i 4) (setf (sb-alien:deref sll (+ 4 i)) (ldb (byte 8 (* 8 i)) index)))
                   (cond ((minusp (%bind fd sll 20)) (%close fd) nil)
                         (t (set-rcv-timeout fd timeout) fd)))
              (sb-alien:free-alien sll))))))))

(defun call-with-dhcp-socket (ifname timeout fn)
  "Open the port-68 UDP socket bound to IFNAME (see the header) and, when allowed,
   the PACKET socket replies are read from; call FN with the UDP socket. The UDP
   socket stays even then: it SENDS, and holding port 68 keeps the kernel from
   answering the server's broadcasts with 'port unreachable'."
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp))
        (raw nil))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address sock) t
                 (sb-bsd-sockets:sockopt-broadcast sock)     t
                 (sb-bsd-sockets:sockopt-bind-to-device sock) ifname)
           (set-rcv-timeout (sb-bsd-sockets:socket-file-descriptor sock) timeout)
           (sb-bsd-sockets:socket-bind sock #(0 0 0 0) 68)
           (setf raw (and *dhcp-packet-socket* (open-dhcp-packet-socket ifname 1)))   ; 1 s: DHCP-XCHG keeps the deadline
           (let ((*dhcp-raw-fd* raw) (*dhcp-timeout* timeout))
             (funcall fn sock)))
      (when raw (%close raw))
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))

(defun dhcp-note-stats (ifname what stats)
  "Leave a line in netd's log saying how an exchange went — the evidence needed
   when an address never comes (wrong network? no server? replies we can't see?)."
  (when (fboundp 'netd-note)
    (funcall 'netd-note "~a: DHCP ~a — sent ~d, ~d packet~:p seen, ~d DHCP repl~:@p (~a)"
             ifname what (getf stats :sent) (getf stats :seen) (getf stats :dhcp)
             (if (getf stats :raw) "packet socket" "UDP socket only: no CAP_NET_RAW?"))))

(defun dhcp-acquire (ifname &key (tries 4) (timeout 2))
  "Run a DHCP DISCOVER/REQUEST handshake on IFNAME (which must be UP). On success
   return a plist (:ip :mask :router octet-lists, :dns :ntp lists of octet-lists,
   :domain, :server, :lease seconds, :t1); else NIL."
  (let ((mac (interface-mac ifname))
        (xid (logand (get-internal-real-time) #xffffffff)))
    (unless mac (return-from dhcp-acquire nil))
    (let ((*dhcp-stats* (list :sent 0 :seen 0 :dhcp 0 :raw nil)) (result nil) (what "no offer"))
      (call-with-dhcp-socket
       ifname timeout
       (lambda (sock)
         (setf (getf *dhcp-stats* :raw) (and *dhcp-raw-fd* t))
         (let ((offer (dhcp-xchg sock (build-dhcp 1 mac xid) xid tries)))   ; DISCOVER->OFFER
           (when offer
             (setf what "offer but no ACK")
             (let ((ack (dhcp-xchg sock (build-dhcp 3 mac xid           ; REQUEST->ACK
                                                    :requested-ip (cdr (assoc :yiaddr offer))
                                                    :server-id (cdr (assoc 54 offer)))
                                   xid tries)))
               (when (and ack (equal (cdr (assoc 53 ack)) '(5)))   ; 53=msg-type, 5=ACK
                 (setf what "leased" result (reply->plist ack))))))))
      (dhcp-note-stats ifname what *dhcp-stats*)
      result)))

(defun dhcp-renew (ifname ip &key (tries 3) (timeout 2))
  "Ask to KEEP IP on IFNAME: one REQUEST with option 50 and no server id. Returns
   the ACK as a plist, :NAK if the server said no (we moved networks, or it forgot
   us — start over with DISCOVER), or NIL if nobody answered."
  (let ((mac (interface-mac ifname))
        (xid (logand (+ 7 (get-internal-real-time)) #xffffffff)))
    (unless mac (return-from dhcp-renew nil))
    (call-with-dhcp-socket
     ifname timeout
     (lambda (sock)
       (let ((ack (dhcp-xchg sock (build-dhcp 3 mac xid :requested-ip ip) xid tries)))
         (cond ((null ack) nil)
               ((equal (cdr (assoc 53 ack)) '(5)) (reply->plist ack))
               ((equal (cdr (assoc 53 ack)) '(6)) :nak)        ; 6 = DHCPNAK
               (t nil)))))))

;;; ---- the lease as an object ---------------------------------------------------
;;;
;;; A lease is not just "an address": it is an address WITH A DEADLINE, plus the
;;; facts the rest of the machine needs (where the router is, which DNS servers to
;;; ask, which NTP servers the network recommends). Keeping it as one struct lets
;;; the renewal thread, the resolver (dns.lisp), the clock (sntp.lisp) and the
;;; network view all read the same thing.

(defstruct lease
  iface                ; "eth0" / "wlan0"
  ip mask router       ; octet lists
  (dns '())            ; list of octet lists, in the server's order of preference
  (ntp '())            ; ditto, NTP servers (option 42), often empty
  domain               ; the search domain (option 15), or NIL
  server               ; the DHCP server's id (option 54)
  (seconds 0)          ; lease length the server granted
  t1                   ; when to renew, seconds after OBTAINED (default half the lease)
  obtained             ; GET-UNIVERSAL-TIME when the ACK arrived
  (static nil))        ; T for the QEMU static fallback (never renewed)

(defvar *leases* '() "Every lease we hold, one per interface, newest first.")

(defvar *lease-hooks* '()
  "Functions of one argument (a LEASE), called after a lease is applied or renewed
   with a change. dns.lisp writes /etc/resolv.conf from here and sntp.lisp checks
   the clock — dhcp.lisp loads first and names neither.")

(defun lease-renew-at (l)
  (+ (lease-obtained l) (or (lease-t1 l) (floor (lease-seconds l) 2))))

(defun lease-expires-at (l)
  (+ (lease-obtained l) (lease-seconds l)))

(defun lease-for (ifname) (find ifname *leases* :key #'lease-iface :test #'string=))

(defun current-lease ()
  "The lease that owns the default route (the interface the internet is through),
   else the newest one, else NIL."
  (let ((dr (ipv4-default-route)))
    (or (and dr (lease-for (getf dr :iface)))
        (first *leases*))))

(defun plist->lease (ifname p &key static)
  (make-lease :iface ifname :ip (getf p :ip) :mask (or (getf p :mask) '(255 255 255 0))
              :router (getf p :router) :dns (getf p :dns) :ntp (getf p :ntp)
              :domain (getf p :domain) :server (getf p :server)
              :seconds (let ((s (getf p :lease 0))) (if (plusp s) s 3600))
              :t1 (getf p :t1) :obtained (get-universal-time) :static static))

(defun run-lease-hooks (lease)
  (dolist (h *lease-hooks*)
    (handler-case (funcall h lease)
      (serious-condition (c) (format t "~&lease hook ~(~a~) failed: ~a~%" h c)))))

(defun apply-lease (lease)
  "Make LEASE true in the kernel: address + mask on the interface, then the default
   route through the router (replacing another interface's: the newest link wins,
   which is what you want when Wi-Fi joins after a dead wire). Records it, updates
   *GATEWAY* / *DNS*, and runs *LEASE-HOOKS*."
  (let ((ifname (lease-iface lease)))
    (bring-up-interface ifname (octets->dotted (lease-ip lease)) (octets->dotted (lease-mask lease)))
    (when (lease-router lease)
      (let ((old (ipv4-default-route)))
        (when (and old (or (string/= (getf old :iface) ifname)
                           (string/= (getf old :gateway) (octets->dotted (lease-router lease)))))
          (delete-default-route (getf old :gateway) :ifname (getf old :iface))))
      (add-default-route (lease-router lease) :ifname ifname))
    (setf *leases* (cons lease (remove ifname *leases* :key #'lease-iface :test #'string=))
          *gateway* (lease-router lease)
          *dns* (first (lease-dns lease)))
    (run-lease-hooks lease)
    lease))

(defun forget-lease (ifname)
  "Drop IFNAME's lease (its link went away): address to 0.0.0.0, its default
   route gone with it (the kernel removes routes through an interface that loses
   its address)."
  (when (lease-for ifname)
    (ignore-errors (bring-up-interface ifname "0.0.0.0" "0.0.0.0"))
    (setf *leases* (remove ifname *leases* :key #'lease-iface :test #'string=))))

(defun describe-lease (l)
  "One line: \"eth0 10.0.2.15/255.255.255.0 gw 10.0.2.2 dns 10.0.2.3 lease 86400s\"."
  (format nil "~a ~a/~a~@[  gw ~a~]~@[  dns ~{~a~^,~}~]~:[  lease ~ds~;~*  (static)~]"
          (lease-iface l) (octets->dotted (lease-ip l)) (octets->dotted (lease-mask l))
          (and (lease-router l) (octets->dotted (lease-router l)))
          (and (lease-dns l) (mapcar #'octets->dotted (lease-dns l)))
          (lease-static l) (lease-seconds l)))

;;; ---- bringing an interface up ------------------------------------------------

(defun interface-driver (ifname)
  "The kernel driver behind IFNAME (\"virtio_net\", \"iwlwifi\"), or NIL."
  (let ((p (ignore-errors (truename (format nil "/sys/class/net/~a/device/driver" ifname)))))
    (and p (car (last (pathname-directory p))))))

(defun dhcp-configure (ifname &key (tries 4) (timeout 2) (verbose t))
  "Bring IFNAME up, lease an address over DHCP and apply it (address, default
   route, resolv.conf through the hooks). Returns the LEASE, or NIL. Under QEMU
   (a virtio NIC) a silent network falls back to SLIRP's static 10.0.2.15/24, as
   before; a real NIC gets no made-up address."
  (ignore-errors (bring-up-interface ifname "0.0.0.0" "0.0.0.0"))   ; link up, no address
  (let ((p (ignore-errors (dhcp-acquire ifname :tries tries :timeout timeout))))
    (cond
      ((and p (getf p :ip))
       (let ((l (apply-lease (plist->lease ifname p))))
         (when verbose (format t "~&~a via DHCP: ~a~%" ifname (describe-lease l)))
         l))
      ((equal (interface-driver ifname) "virtio_net")
       (let ((l (apply-lease (make-lease :iface ifname :ip '(10 0 2 15) :mask '(255 255 255 0)
                                         :router '(10 0 2 2) :dns '((10 0 2 3))
                                         :obtained (get-universal-time) :static t))))
         (when verbose
           (format t "~&~a: no DHCP answer — fell back to QEMU's static 10.0.2.15/24.~%" ifname))
         l))
      (t (when verbose (format t "~&~a: no DHCP answer.~%" ifname))
         nil))))

(defun configure-eth0 (&key (ifname "eth0"))
  "The boot-time entry point of old: DHCP on IFNAME. Kept for the REPL and the
   docs that name it; NETWORK-UP is what the supervisor runs now."
  (prog1 (dhcp-configure ifname) (finish-output)))

;;; ---- renewal: the "netd" thread -------------------------------------------------
;;;
;;; A lease runs out. RFC 2131's clock: at T1 (default half the lease) ask the same
;;; server to extend it; if that fails, keep asking; at the end, give the address
;;; up and start over. Here one background thread wakes every few seconds, and for
;;; each lease past its T1 sends the renewal REQUEST. It also runs *NETD-HOOKS* —
;;; wifi.lisp's link watcher — so there is ONE network thread, not one per concern.
;;; Every iteration is guarded: an error in a hook must not stop renewals.

(defvar *netd-thread* nil)
(defvar *netd-period* 5 "Seconds between netd's wake-ups.")
(defvar *netd-hooks* '() "Functions of no arguments netd calls on every wake-up.")
(defvar *netd-log* '() "The last few things netd did, newest first: (universal-time . text).")

(defun network-log (text)
  "Append one time-stamped line to network.log. /tmp dies with the machine; on the
   laptop this file is on /home (~/.lol/log/), so the NEXT boot (or Debian) can read
   what went wrong. The writing, the size cap and the per-boot archiving are
   session-log.lisp's, shared by every log. Never signals."
  (append-log-line (log-path "network") text))

(defun netd-note (fmt &rest args)
  (network-log (apply #'format nil fmt args))
  (push (cons (get-universal-time) (apply #'format nil fmt args)) *netd-log*)
  (when (> (length *netd-log*) 40) (setf *netd-log* (subseq *netd-log* 0 40))))

(defun maybe-renew (l)
  (let ((now (get-universal-time)))
    (when (and (not (lease-static l)) (>= now (lease-renew-at l)))
      (let ((r (ignore-errors (dhcp-renew (lease-iface l) (lease-ip l)))))
        (cond ((consp r)
               (let ((new (plist->lease (lease-iface l) r)))
                 (if (and (equal (lease-ip new) (lease-ip l))
                          (equal (lease-router new) (lease-router l))
                          (equal (lease-dns new) (lease-dns l)))
                     (setf (lease-obtained l) (lease-obtained new)      ; same facts: just
                           (lease-seconds l) (lease-seconds new)        ; a new deadline
                           (lease-t1 l) (lease-t1 new))
                     (apply-lease new))
                 (netd-note "~a: lease renewed (~ds)" (lease-iface l) (lease-seconds new))))
              ((or (eq r :nak) (>= now (lease-expires-at l)))
               (netd-note "~a: lease ~:[expired~;refused (NAK)~] — starting over"
                          (lease-iface l) (eq r :nak))
               (forget-lease (lease-iface l))
               (dhcp-configure (lease-iface l) :verbose nil))
              (t                                  ; no answer: try again in a minute
               (setf (lease-t1 l) (+ (- now (lease-obtained l)) 60))
               (netd-note "~a: no answer to the renewal, retrying" (lease-iface l))))))))

(defun netd-loop ()
  (loop
    (dolist (l (copy-list *leases*))
      (handler-case (maybe-renew l)
        (serious-condition (c) (netd-note "renewal error: ~a" c))))
    (dolist (h *netd-hooks*)
      (handler-case (funcall h)
        (serious-condition (c) (netd-note "~(~a~): ~a" h c))))
    (sleep *netd-period*)))

(defun start-netd ()
  "Start the network thread once. Returns it."
  (unless (and *netd-thread* (sb-thread:thread-alive-p *netd-thread*))
    (setf *netd-thread* (sb-thread:make-thread #'netd-loop :name "netd")))
  *netd-thread*)

;;; ---- the boot-time entry point ---------------------------------------------------

(defun network-up ()
  "What the supervisor runs at boot: DHCP on every WIRED interface that has a
   carrier (eth0 under QEMU; on the laptop usually none — it has no Ethernet
   port), then the netd thread, which renews leases and, through wifi.lisp's hook,
   brings Wi-Fi up in the background. Prints what it did; never signals."
  ;; The LOOPBACK first. The kernel creates lo DOWN, and nothing else here raised
  ;; it until 2026-10-12 — so 127.0.0.1 did not exist: a Rails server in a
  ;; container could not listen on localhost, nor Chrome reach it. Raising IFF_UP
  ;; is all it takes: on NETDEV_UP of a loopback device the kernel gives it
  ;; 127.0.0.1/8 itself (net/ipv4/devinet.c, inetdev_event) and IPv6 adds ::1.
  (unless (ignore-errors (set-interface-up "lo"))
    (format t "~&lo: could not be brought up — 127.0.0.1 will not work~%"))
  (dolist (i (ignore-errors (interfaces)))
    (when (eq (getf i :kind) :wired)
      (let ((name (getf i :name)))
        (ignore-errors (set-interface-up name))
        ;; a cable takes a moment to report carrier after UP; virtio is instant
        (loop repeat 10 until (equal (%sys-net name "carrier") "1") do (sleep 0.2))
        (if (equal (%sys-net name "carrier") "1")
            (ignore-errors (dhcp-configure name))
            (format t "~&~a: no carrier (cable unplugged?) — skipped~%" name)))))
  (ignore-errors (start-netd))
  (finish-output))
