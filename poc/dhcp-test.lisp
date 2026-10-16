;;;; dhcp-test.lisp — host test of dhcp.lisp against a real kernel and a fake server.
;;;;
;;;;   sbcl --script poc/dhcp-test.lisp
;;;;
;;;; No root needed: the script re-runs itself under `unshare -rn` — a new user
;;;; namespace (where we are "root" for this namespace only) with a private network
;;;; namespace, then builds a veth pair, the two ends of a virtual cable:
;;;;     v0 (the client, no address)  <====>  v1 (the "router", 192.168.77.1/24)
;;;; v1 is moved into a SECOND network namespace, run by a child copy of this script
;;;; (--server), so the two ends are two machines. In one namespace the kernel would
;;;; give the client's DISCOVER the router's own address as its source (any local
;;;; address will do for a device with none), and v1 drops a packet "from itself".
;;;; The server reads its manner from a file before each reply. It runs in two manners:
;;;;   :broadcast  replies to 255.255.255.255 — what QEMU's server does, and what
;;;;               the broadcast flag asks for;
;;;;   :unicast    replies to the offered address at the client's MAC, ignoring
;;;;               the flag — what the laptop's home router did (2026-10-12).
;;;; The unicast case is the bug: a UDP socket never sees that reply, because the
;;;; kernel drops a packet for an address the interface does not hold yet. The test
;;;; shows it (with *DHCP-PACKET-SOCKET* off) and shows the PACKET socket fixing it.

(require :sb-bsd-sockets)

;;; ---- into a namespace of our own ----------------------------------------------
(unless (string= (string-trim '(#\Newline) (with-output-to-string (o)
                                             (sb-ext:run-program "/usr/bin/id" '("-u") :output o)))
                 "0")
  (let ((p (sb-ext:run-program "/usr/bin/unshare"
                               (list "-rn" "sbcl" "--script"
                                     (namestring *load-truename*))
                               :search t :output t :error t
                               :environment (list* "LOL_DHCP_TEST_INNER=1" (sb-ext:posix-environ)))))
    (sb-ext:exit :code (sb-ext:process-exit-code p))))

(defvar *here* (make-pathname :name nil :type nil :defaults *load-truename*))
(load (merge-pathnames "host-logging.lisp" *here*))   ; the modules log (session-log.lisp)
(handler-bind ((warning #'muffle-warning))
  (dolist (f '("net" "dhcp"))
    (load (merge-pathnames (format nil "../initramfs/~a.lisp" f) *here*))))

(defun sh (fmt &rest args)
  (let ((cmd (apply #'format nil fmt args)))
    (unless (zerop (sb-ext:process-exit-code
                    (sb-ext:run-program "/bin/sh" (list "-c" cmd) :output t :error t)))
      (error "setup failed: ~a" cmd))))

(defvar *mode-file* (format nil "/tmp/lol-dhcp-test-~d.mode" (sb-ext:posix-getenv "PPID")))

;;; ---- the fake server (the child, in its own network namespace) -----------------
(defun server-reply (req type mac)
  "A BOOTREPLY for REQ (a parsed request) of TYPE 2=OFFER / 5=ACK, as a vector."
  (let ((p (make-array 300 :element-type '(unsigned-byte 8) :initial-element 0))
        (xid (cdr (assoc :xid req))))
    (setf (aref p 0) 2 (aref p 1) 1 (aref p 2) 6)
    (loop for i from 0 for sh from 24 downto 0 by 8 do (setf (aref p (+ 4 i)) (ldb (byte 8 sh) xid)))
    (replace p '(192 168 77 50) :start1 16)                     ; yiaddr
    (replace p mac :start1 28)
    (replace p '(#x63 #x82 #x53 #x63) :start1 236)
    (replace p (list 53 1 type  54 4 192 168 77 1  51 4 0 0 #x0e #x10
                     1 4 255 255 255 0  3 4 192 168 77 1  6 4 192 168 77 1  255)
             :start1 240)
    p))

(defun read-mode ()
  (or (ignore-errors (with-open-file (i (second (member "--server" sb-ext:*posix-argv* :test #'string=)))
                       (read i)))
      '(:broadcast nil)))

(defun run-server ()
  (loop until (interface-index "v1") do (sleep 0.05))          ; the parent moves it in
  (sh "ip addr add 192.168.77.1/24 dev v1 && ip link set v1 up")
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp))
        (buf (make-array 1024 :element-type '(unsigned-byte 8)))
        (neigh nil))
    (setf (sb-bsd-sockets:sockopt-reuse-address s) t
          (sb-bsd-sockets:sockopt-broadcast s) t
          (sb-bsd-sockets:sockopt-bind-to-device s) "v1")
    (sb-bsd-sockets:socket-bind s #(0 0 0 0) 67)
    (loop
      (multiple-value-bind (b len) (sb-bsd-sockets:socket-receive s buf 1024)
        (declare (ignore b))
        (let ((req (parse-dhcp buf len)))
          (when (and req (= (aref buf 0) 1))                     ; a BOOTREQUEST
            (destructuring-bind (mode noise) (read-mode)
              (let* ((mac (coerce (subseq buf 28 34) 'list))
                     (type (if (eql (first (cdr (assoc 53 req))) 1) 2 5))
                     (to (if (eq mode :unicast) #(192 168 77 50) #(255 255 255 255))))
                ;; a real server writes the frame to the client's MAC itself (the client
                ;; cannot answer ARP for an address it does not have yet); a permanent
                ;; neighbour entry does the same job here
                (unless neigh
                  (sh "ip neigh replace 192.168.77.50 lladdr ~{~2,'0x~^:~} dev v1 nud permanent" mac)
                  (setf neigh t))
                (when noise
                  (let ((other (server-reply (cons (cons :xid 12345) req) type mac)))
                    (sb-bsd-sockets:socket-send s other (length other)
                                                :address (list #(255 255 255 255) 68))))
                (let ((r (server-reply req type mac)))
                  (sb-bsd-sockets:socket-send s r (length r) :address (list to 68)))))))))))

(when (member "--server" sb-ext:*posix-argv* :test #'string=)
  (run-server)
  (sb-ext:exit))

;;; ---- the client side: build the cable, start the router ------------------------
(defvar *mode* '(:broadcast nil))
(defun set-mode (mode &optional noise)
  (with-open-file (o *mode-file* :direction :output :if-exists :supersede)
    (prin1 (list mode noise) o)))
(set-mode :broadcast)

(sh "ip link set lo up && ip link add v0 type veth peer name v1 && ip link set v0 up")
;; A new namespace copies the host's sysctls, and Debian sets rp_filter=2: drop a
;; packet whose source this machine has no route back to — which is every DHCP
;; reply before the lease. The image never sets it, so it runs with the kernel's
;; default, 0. Match the image, not the host.
(sh "for f in all default v0; do echo 0 > /proc/sys/net/ipv4/conf/$f/rp_filter; done")
(defvar *server*
  (sb-ext:run-program "/usr/bin/unshare"
                      (list "-n" "sbcl" "--script" (namestring *load-truename*) "--server" *mode-file*)
                      :search t :wait nil :output t :error t))
(sleep 0.5)
(sh "ip link set v1 netns ~d" (sb-ext:process-pid *server*))
(sleep 1)

;;; ---- checks --------------------------------------------------------------------
(defvar *fails* 0)
(defmacro check (label form)
  `(let ((ok (handler-case ,form (error (c) (format t "    (~a)~%" c) nil))))
     (format t "  ~60a ~:[FAIL~;ok~]~%" ,label ok)
     (unless ok (incf *fails*))))

(defun lease-ip-of (plist) (getf plist :ip))

(format t "dhcp.lisp against a fake server on a veth pair~%")
(check "an IPv4/UDP:68 packet's DHCP part is found"
       (let ((v (make-array 40 :element-type '(unsigned-byte 8) :initial-element 0)))
         (setf (aref v 0) #x45 (aref v 9) 17 (aref v 22) 0 (aref v 23) 68)
         (eql (dhcp-reply-from-ip-packet v 40) 28)))
(check "... and a TCP or wrong-port packet is not"
       (let ((v (make-array 40 :element-type '(unsigned-byte 8) :initial-element 0)))
         (setf (aref v 0) #x45 (aref v 9) 6 (aref v 23) 68)
         (and (null (dhcp-reply-from-ip-packet v 40))
              (progn (setf (aref v 9) 17 (aref v 23) 67) (null (dhcp-reply-from-ip-packet v 40))))))
(check "a PACKET socket opens on v0 (CAP_NET_RAW in this namespace)"
       (let ((fd (open-dhcp-packet-socket "v0" 1))) (and fd (progn (%close fd) t))))

(set-mode :broadcast)
(check "broadcasting server: leased 192.168.77.50"
       (equal (lease-ip-of (dhcp-acquire "v0" :tries 2 :timeout 1)) '(192 168 77 50)))

(check "broadcasting server, UDP socket only: leased too (why QEMU passed)"
       (let ((*dhcp-packet-socket* nil))
         (equal (lease-ip-of (dhcp-acquire "v0" :tries 2 :timeout 1)) '(192 168 77 50))))

(set-mode :unicast)
(check "unicasting server, UDP socket only: NO lease (the laptop's bug)"
       (let ((*dhcp-packet-socket* nil)) (null (dhcp-acquire "v0" :tries 2 :timeout 1))))
(check "unicasting server, with the PACKET socket: leased"
       (equal (lease-ip-of (dhcp-acquire "v0" :tries 2 :timeout 1)) '(192 168 77 50)))

(set-mode :unicast t)
(check "someone else's reply first does not cost the attempt"
       (equal (lease-ip-of (dhcp-acquire "v0" :tries 1 :timeout 1)) '(192 168 77 50)))
(set-mode :unicast)

(check "the exchange is in netd's log with its counts"
       (search "DHCP leased — sent" (cdr (first *netd-log*))))
(check "dhcp-configure puts the address on v0"
       (and (dhcp-configure "v0" :tries 2 :timeout 1 :verbose nil)
            (equal (interface-ipv4 "v0") "192.168.77.50")))

(when (plusp *fails*) (format t "netd: ~s~%" *netd-log*))
(ignore-errors (sb-ext:process-kill *server* 15))
(ignore-errors (delete-file *mode-file*))
(format t "~:[~d FAILED~;all passed~]~%" (zerop *fails*) *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1) :abort t)
