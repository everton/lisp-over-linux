;;;; dhcp.lisp — get an IP from a DHCP server, in pure Lisp.
;;;;
;;;; Replaces the hard-coded static address with a real DHCP handshake:
;;;;   DISCOVER  --broadcast-->  (server)  --OFFER-->   (we get an offered IP)
;;;;   REQUEST   --broadcast-->  (server)  --ACK-->     (lease confirmed)
;;;; then we configure eth0 from the lease (reusing net.lisp's ioctls).
;;;;
;;;; The chicken-and-egg of DHCP is that we must talk on an interface that has NO
;;;; IP yet. Two tricks make it work over an ordinary UDP socket (no AF_PACKET):
;;;;   - SO_BINDTODEVICE ties the socket to eth0, so a broadcast goes out the
;;;;     right NIC even with no routing table entry;
;;;;   - we set the DHCP *broadcast flag*, so the server BROADCASTS its replies to
;;;;     255.255.255.255:68 (which we can receive) instead of unicasting them to
;;;;     an address we do not have yet.
;;;; Built on the sb-bsd-sockets contrib baked into the image. See networking.org.

(defconstant +siocgifhwaddr+ #x8927)    ; get interface MAC
(defconstant +sol-socket+      1)
(defconstant +so-rcvtimeo+    20)

(sb-alien:define-alien-routine ("setsockopt" %setsockopt) sb-alien:int
  (fd sb-alien:int) (level sb-alien:int) (optname sb-alien:int)
  (optval (sb-alien:* sb-alien:unsigned-char)) (optlen sb-alien:int))

(defvar *gateway* nil "Default-gateway octets from the last DHCP lease, or NIL.")
(defvar *dns*     nil "DNS-server octets from the last DHCP lease, or NIL.")

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

;;; ---- the DHCP packet (BOOTP header + options) -----------------------------

(defun build-dhcp (msg-type mac xid &key requested-ip server-id)
  "Build a DHCP packet (a (unsigned-byte 8) vector). MSG-TYPE is 1=DISCOVER /
   3=REQUEST. REQUESTED-IP and SERVER-ID (octet lists) are added for REQUEST."
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
        (emit 55 4 1 3 6 51)                         ; opt 55: want mask, router, dns, lease
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

(defun dhcp-acquire (ifname &key (tries 4) (timeout 2))
  "Run a DHCP DISCOVER/REQUEST handshake on IFNAME (which must be UP). On success
   return a plist (:ip :mask :router :dns octet-lists, :lease seconds); else NIL."
  (let ((mac (interface-mac ifname))
        (xid (logand (get-internal-real-time) #xffffffff)))
    (unless mac (return-from dhcp-acquire nil))
    (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp)))
      (unwind-protect
           (progn
             (setf (sb-bsd-sockets:sockopt-reuse-address sock) t
                   (sb-bsd-sockets:sockopt-broadcast sock)     t
                   (sb-bsd-sockets:sockopt-bind-to-device sock) ifname)
             (set-rcv-timeout (sb-bsd-sockets:socket-file-descriptor sock) timeout)
             (sb-bsd-sockets:socket-bind sock #(0 0 0 0) 68)
             (let ((offer (dhcp-xchg sock (build-dhcp 1 mac xid) xid tries)))   ; DISCOVER->OFFER
               (when offer
                 (let ((ack (dhcp-xchg sock (build-dhcp 3 mac xid           ; REQUEST->ACK
                                                        :requested-ip (cdr (assoc :yiaddr offer))
                                                        :server-id (cdr (assoc 54 offer)))
                                       xid tries)))
                   (when (and ack (equal (cdr (assoc 53 ack)) '(5)))   ; 53=msg-type, 5=ACK
                     (list :ip     (cdr (assoc :yiaddr ack))
                           :mask   (cdr (assoc 1 ack))
                           :router (cdr (assoc 3 ack))
                           :dns    (cdr (assoc 6 ack))
                           :lease  (be32 (cdr (assoc 51 ack)))))))))
        (ignore-errors (sb-bsd-sockets:socket-close sock))))))

;;; ---- the boot-time entry point --------------------------------------------

(defun configure-eth0 (&key (ifname "eth0"))
  "Bring IFNAME up, try DHCP, and configure it from the lease. Falls back to a
   static address if no DHCP server answers. Prints what it did. Note: we apply
   the IP+mask, which gives an on-link route for the whole subnet (enough for the
   QEMU SLIRP gateway/DNS, all in 10.0.2.0/24); an off-subnet default route via
   the leased :router is left for the outbound/DNS stage (networking.org §2c)."
  (ignore-errors (bring-up-interface ifname "0.0.0.0" "0.0.0.0"))   ; link up, no address
  (let ((lease (ignore-errors (dhcp-acquire ifname))))
    (cond
      ((and lease (getf lease :ip))
       (let ((ip   (octets->dotted (getf lease :ip)))
             (mask (octets->dotted (or (getf lease :mask) '(255 255 255 0)))))
         (bring-up-interface ifname ip mask)
         (setf *gateway* (getf lease :router) *dns* (getf lease :dns))
         (format t "~&eth0 via DHCP: ~a/~a" ip mask)
         (when *gateway* (format t "  gw ~a" (octets->dotted *gateway*)))
         (when *dns*     (format t "  dns ~a" (octets->dotted *dns*)))
         (when (plusp (getf lease :lease 0)) (format t "  lease ~ds" (getf lease :lease)))
         (terpri)))
      (t
       (ignore-errors (bring-up-interface ifname "10.0.2.15" "255.255.255.0"))
       (format t "~&eth0: no DHCP answer — fell back to static 10.0.2.15/24.~%")))
    (finish-output)))
