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
;;;; IP yet. Two tricks make it work over an ordinary UDP socket (no AF_PACKET):
;;;;   - SO_BINDTODEVICE ties the socket to one interface, so a broadcast goes
;;;;     out the right NIC even with no routing table entry;
;;;;   - we set the DHCP *broadcast flag*, so the server BROADCASTS its replies to
;;;;     255.255.255.255:68 (which we can receive) instead of unicasting them to
;;;;     an address we do not have yet.
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

(defun dhcp-xchg (sock packet xid tries)
  "Broadcast PACKET and wait for a matching (same XID) DHCP reply, retrying up to
   TRIES times. Returns the parsed reply, or NIL."
  (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
    (dotimes (attempt tries)
      (ignore-errors
        (sb-bsd-sockets:socket-send sock packet (length packet)
                                    :address (list #(255 255 255 255) 67)))
      (handler-case
          (multiple-value-bind (b len) (sb-bsd-sockets:socket-receive sock buf 1024)
            (declare (ignore b))
            (when (and len (plusp len))
              (let ((reply (parse-dhcp buf len)))
                (when (and reply (eql (cdr (assoc :xid reply)) xid))
                  (return-from dhcp-xchg reply)))))
        (serious-condition () nil)))    ; timeout / transient error -> retry
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

(defun call-with-dhcp-socket (ifname timeout fn)
  "Open the port-68 UDP socket bound to IFNAME (see the header), call FN with it."
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address sock) t
                 (sb-bsd-sockets:sockopt-broadcast sock)     t
                 (sb-bsd-sockets:sockopt-bind-to-device sock) ifname)
           (set-rcv-timeout (sb-bsd-sockets:socket-file-descriptor sock) timeout)
           (sb-bsd-sockets:socket-bind sock #(0 0 0 0) 68)
           (funcall fn sock))
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))

(defun dhcp-acquire (ifname &key (tries 4) (timeout 2))
  "Run a DHCP DISCOVER/REQUEST handshake on IFNAME (which must be UP). On success
   return a plist (:ip :mask :router octet-lists, :dns :ntp lists of octet-lists,
   :domain, :server, :lease seconds, :t1); else NIL."
  (let ((mac (interface-mac ifname))
        (xid (logand (get-internal-real-time) #xffffffff)))
    (unless mac (return-from dhcp-acquire nil))
    (call-with-dhcp-socket
     ifname timeout
     (lambda (sock)
       (let ((offer (dhcp-xchg sock (build-dhcp 1 mac xid) xid tries)))   ; DISCOVER->OFFER
         (when offer
           (let ((ack (dhcp-xchg sock (build-dhcp 3 mac xid           ; REQUEST->ACK
                                                  :requested-ip (cdr (assoc :yiaddr offer))
                                                  :server-id (cdr (assoc 54 offer)))
                                 xid tries)))
             (when (and ack (equal (cdr (assoc 53 ack)) '(5)))   ; 53=msg-type, 5=ACK
               (reply->plist ack)))))))))

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

(defun netd-note (fmt &rest args)
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
