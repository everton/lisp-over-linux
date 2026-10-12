;;;; net.lisp — bring the network up from Lisp, and serve the REPL over TCP.
;;;;
;;;; Two halves of networking.org's userland plan:
;;;;   1. CONFIGURE the interface (give eth0 an IP, bring it UP) with the classic
;;;;      ioctl path — SIOCSIFADDR / SIOCSIFNETMASK / SIOCSIFFLAGS on a throwaway
;;;;      UDP socket. Pure FFI into the libc we already ship (the same trick as
;;;;      power-off's reboot(2) and the line editor's termios) — no `ip`/`ifconfig`
;;;;      binary, no new native library.
;;;;   2. USE the network: a tiny TCP REPL server, built on the sb-bsd-sockets
;;;;      contrib baked into the image at build time (build.sh requires it before
;;;;      save-lisp-and-die). Reachable from the host over QEMU's hostfwd.
;;;;
;;;; Under QEMU user-mode (SLIRP) networking the guest is 10.0.2.15, gateway
;;;; 10.0.2.2 — so the address below is hard-coded for that first test (static IP;
;;;; DHCP is a later stage, see networking.org §2).

;;; ---- interface configuration via ioctl (no `ifconfig` binary) ------------

;; struct ifreq is 40 bytes: char ifr_name[16] then a 24-byte union. We build it
;; as a raw byte block and fill only the member each ioctl reads. The union holds
;; either a struct sockaddr_in (addr/netmask) or a short ifr_flags, both at byte 16.
(defconstant +ifreq-size+     40)
(defconstant +af-inet+         2)
(defconstant +sock-dgram+      2)
(defconstant +iff-up+        #x1)
(defconstant +iff-running+   #x40)
(defconstant +siocsifaddr+    #x8916)   ; set interface address
(defconstant +siocsifnetmask+ #x891c)   ; set interface netmask
(defconstant +siocgifflags+   #x8913)   ; get interface flags
(defconstant +siocsifflags+   #x8914)   ; set interface flags
(defconstant +siocgifaddr+    #x8915)   ; get interface address

(sb-alien:define-alien-routine ("socket" %socket) sb-alien:int
  (domain sb-alien:int) (type sb-alien:int) (protocol sb-alien:int))
(sb-alien:define-alien-routine ("close" %close) sb-alien:int
  (fd sb-alien:int))
;; ioctl is variadic in C; for the SIOC* calls the 3rd arg is a struct ifreq *.
(sb-alien:define-alien-routine ("ioctl" %ioctl) sb-alien:int
  (fd sb-alien:int) (request sb-alien:unsigned-long)
  (argp (sb-alien:* sb-alien:unsigned-char)))

(defun ipv4-octets (string)
  "\"10.0.2.15\" -> (10 0 2 15)."
  (let ((out '()) (start 0))
    (loop
      (let ((dot (position #\. string :start start)))
        (push (parse-integer string :start start :end dot) out)
        (if dot (setf start (1+ dot)) (return))))
    (nreverse out)))

(defun %zero-ifreq (buf)
  (dotimes (i +ifreq-size+) (setf (sb-alien:deref buf i) 0)))

(defun %set-ifname (buf name)
  "Write NAME into ifr_name (bytes 0..15); caller must have zeroed BUF first."
  (dotimes (i (min (length name) 15))
    (setf (sb-alien:deref buf i) (char-code (char name i)))))

(defun %set-sockaddr-in (buf dotted)
  "Fill the sockaddr_in in the union (offset 16): sin_family=AF_INET, port 0,
   sin_addr = DOTTED's four octets (already network byte order = written order)."
  (setf (sb-alien:deref buf 16) +af-inet+      ; sin_family (little-endian short)
        (sb-alien:deref buf 17) 0
        (sb-alien:deref buf 18) 0              ; sin_port = 0
        (sb-alien:deref buf 19) 0)
  (loop for o in (ipv4-octets dotted) for i from 20 do (setf (sb-alien:deref buf i) o)))

(defun bring-up-interface (name ip netmask)
  "Assign IP + NETMASK to interface NAME and bring it UP|RUNNING, via SIOC*
   ioctls on a throwaway UDP socket (the classic ifconfig path). Returns T on
   success, NIL on any failure. Needs PID-1/root privileges (we are PID 1)."
  (let ((fd (%socket +af-inet+ +sock-dgram+ 0)))
    (when (minusp fd) (return-from bring-up-interface nil))
    (unwind-protect
         (let ((buf (sb-alien:make-alien sb-alien:unsigned-char +ifreq-size+)))
           (unwind-protect
                (flet ((io (req) (not (minusp (%ioctl fd req buf)))))
                  ;; address
                  (%zero-ifreq buf) (%set-ifname buf name) (%set-sockaddr-in buf ip)
                  (unless (io +siocsifaddr+) (return-from bring-up-interface nil))
                  ;; netmask
                  (%zero-ifreq buf) (%set-ifname buf name) (%set-sockaddr-in buf netmask)
                  (io +siocsifnetmask+)
                  ;; flags: read, OR in UP|RUNNING, write back (ifr_flags = short at 16)
                  (%zero-ifreq buf) (%set-ifname buf name)
                  (unless (io +siocgifflags+) (return-from bring-up-interface nil))
                  (let ((flags (logior (sb-alien:deref buf 16)
                                       (ash (sb-alien:deref buf 17) 8)
                                       +iff-up+ +iff-running+)))
                    (setf (sb-alien:deref buf 16) (logand flags #xff)
                          (sb-alien:deref buf 17) (logand (ash flags -8) #xff)))
                  (io +siocsifflags+))
             (sb-alien:free-alien buf)))
      (%close fd))))

(defun interface-ipv4 (name)
  "Dotted IPv4 address of NAME via SIOCGIFADDR, or NIL if it has none."
  (let ((fd (%socket +af-inet+ +sock-dgram+ 0)))
    (when (minusp fd) (return-from interface-ipv4 nil))
    (unwind-protect
         (let ((buf (sb-alien:make-alien sb-alien:unsigned-char +ifreq-size+)))
           (unwind-protect
                (progn
                  (%zero-ifreq buf) (%set-ifname buf name)
                  (when (not (minusp (%ioctl fd +siocgifaddr+ buf)))
                    (format nil "~d.~d.~d.~d"               ; sin_addr at bytes 20..23
                            (sb-alien:deref buf 20) (sb-alien:deref buf 21)
                            (sb-alien:deref buf 22) (sb-alien:deref buf 23))))
             (sb-alien:free-alien buf)))
      (%close fd))))

;;; ---- the routing table: SIOCADDRT, and reading it back -------------------
;;;
;;; Giving eth0 an address and a mask (above) makes the kernel add ONE route by
;;; itself: "10.0.2.0/24 is on-link, out of eth0". Anything outside that subnet —
;;; the whole internet — needs a DEFAULT route: "for every other destination, hand
;;; the packet to the gateway 10.0.2.2". That is one more entry in the kernel's
;;; routing table, and there are two ways to ask for it:
;;;
;;;   - the ioctl SIOCADDRT with a `struct rtentry` (what the old `route add`
;;;     does) — the same throwaway-socket idiom as SIOCSIFADDR above;
;;;   - rtnetlink: an AF_NETLINK socket and an RTM_NEWROUTE message (what `ip
;;;     route add` does).
;;;
;;; We use the ioctl. It is IPv4-only and cannot express everything (several
;;; tables, policy routing), but it is ~40 lines in an idiom this file already
;;; teaches, and it needs nothing a netlink message would add: one default route
;;; through one gateway. IPv6 routes we never ADD ourselves — the kernel installs
;;; the v6 default route on its own from the router's advertisement (SLAAC) — so
;;; the one place netlink would earn its keep does not arise. Reading the table
;;; back needs no netlink either: the kernel prints it as text in /proc/net/route
;;; (v4) and /proc/net/ipv6_route (v6). Both checked against linux/net/ipv4/
;;; fib_frontend.c (SIOCADDRT wants CAP_NET_ADMIN — one of PID 1's five).
;;;
;;; struct rtentry on x86-64, measured with offsetof() against glibc's
;;; <net/route.h> rather than worked out by hand (120 bytes):
;;;    0 rt_pad1 · 8 rt_dst (sockaddr) · 24 rt_gateway · 40 rt_genmask
;;;   56 rt_flags (u16) · 80 rt_metric (i16) · 88 rt_dev (char *) · …
;;; rt_metric is stored PLUS ONE by the kernel's ioctl path (0 means "default"),
;;; so metric 0 asks for the kernel default.

(defconstant +rtentry-size+ 120)
(defconstant +siocaddrt+ #x890b)
(defconstant +siocdelrt+ #x890c)
(defconstant +rtf-up+      #x1)
(defconstant +rtf-gateway+ #x2)

(defun %put-sockaddr-in (buf offset octets)
  "A struct sockaddr_in at OFFSET in BUF: AF_INET, port 0, the four OCTETS."
  (setf (sb-alien:deref buf offset) +af-inet+
        (sb-alien:deref buf (+ offset 1)) 0)
  (loop for o in octets for i from (+ offset 4) do (setf (sb-alien:deref buf i) o)))

(defun %route-ioctl (request gateway &key (ifname nil) (metric 0))
  "Add or delete (REQUEST = SIOCADDRT / SIOCDELRT) the DEFAULT route via GATEWAY
   (an octet list), optionally pinned to IFNAME. Returns T, or NIL and the errno."
  (let ((fd (%socket +af-inet+ +sock-dgram+ 0))
        (buf (sb-alien:make-alien sb-alien:unsigned-char +rtentry-size+))
        (dev (and ifname (sb-alien:make-alien-string ifname))))
    (unwind-protect
         (progn
           (dotimes (i +rtentry-size+) (setf (sb-alien:deref buf i) 0))
           (%put-sockaddr-in buf 8  '(0 0 0 0))     ; rt_dst     0.0.0.0  = "anywhere"
           (%put-sockaddr-in buf 24 gateway)        ; rt_gateway the router
           (%put-sockaddr-in buf 40 '(0 0 0 0))     ; rt_genmask /0       = matches all
           (setf (sb-alien:deref buf 56) (logior +rtf-up+ +rtf-gateway+))
           (setf (sb-alien:deref buf 80) (ldb (byte 8 0) metric)
                 (sb-alien:deref buf 81) (ldb (byte 8 8) metric))
           (when dev                                ; rt_dev: a char * at 88, little-endian
             (let ((addr (sb-sys:sap-int (sb-alien:alien-sap dev))))
               (dotimes (i 8) (setf (sb-alien:deref buf (+ 88 i)) (ldb (byte 8 (* 8 i)) addr)))))
           (if (minusp (%ioctl fd request buf))
               (values nil (sb-alien:get-errno))
               t))
      (sb-alien:free-alien buf)
      (when dev (sb-alien:free-alien dev))
      (%close fd))))

(defun add-default-route (gateway &key ifname)
  "Install the default route via GATEWAY (octets or \"a.b.c.d\"). Idempotent: an
   identical route already present (EEXIST, errno 17) counts as success."
  (let ((gw (if (stringp gateway) (ipv4-octets gateway) gateway)))
    (multiple-value-bind (ok errno) (%route-ioctl +siocaddrt+ gw :ifname ifname)
      (or ok (eql errno 17)))))

(defun delete-default-route (gateway &key ifname)
  "Remove the default route via GATEWAY. T on success."
  (let ((gw (if (stringp gateway) (ipv4-octets gateway) gateway)))
    (values (%route-ioctl +siocdelrt+ gw :ifname ifname))))

(defun octets->dotted* (octets)
  "(10 0 2 2) -> \"10.0.2.2\". dhcp.lisp's OCTETS->DOTTED does the same; this copy
   exists because net.lisp loads first."
  (format nil "~{~d~^.~}" octets))

(defun %hex-le-ipv4 (hex)
  "/proc/net/route prints each address as the 32-bit word in HOST byte order
   (little-endian here), so \"0202000A\" is 10.0.2.2: read the bytes backwards."
  (let ((n (parse-integer hex :radix 16)))
    (list (ldb (byte 8 0) n) (ldb (byte 8 8) n) (ldb (byte 8 16) n) (ldb (byte 8 24) n))))

(defun %split-ws (line)
  "LINE split on runs of spaces/tabs."
  (let ((out '()) (start nil))
    (loop for i from 0 to (length line)
          for c = (if (< i (length line)) (char line i) #\Space)
          do (if (member c '(#\Space #\Tab))
                 (when start (push (subseq line start i) out) (setf start nil))
                 (unless start (setf start i))))
    (nreverse out)))

(defun %read-lines (path)
  "Every line of the text file PATH, or NIL if it cannot be read."
  (ignore-errors
   (with-open-file (s path :external-format :latin-1)
     (loop for l = (read-line s nil nil) while l collect l))))

(defun ipv4-routes ()
  "The IPv4 routing table as plists (:iface :dest :gateway :mask :flags :metric),
   addresses as dotted strings — read from /proc/net/route."
  (loop for line in (rest (%read-lines "/proc/net/route"))
        for f = (%split-ws line)
        when (>= (length f) 8)
          collect (list :iface (first f)
                        :dest (octets->dotted* (%hex-le-ipv4 (second f)))
                        :gateway (octets->dotted* (%hex-le-ipv4 (third f)))
                        :flags (parse-integer (fourth f) :radix 16)
                        :metric (parse-integer (seventh f))
                        :mask (octets->dotted* (%hex-le-ipv4 (eighth f))))))

(defun ipv4-default-route ()
  "The IPv4 default route (dest 0.0.0.0, mask 0.0.0.0) as a plist, or NIL."
  (find-if (lambda (r) (and (string= (getf r :dest) "0.0.0.0")
                            (string= (getf r :mask) "0.0.0.0")))
           (ipv4-routes)))

;;; IPv6 (tag #26 turned CONFIG_IPV6 on). We configure nothing by hand: when an
;;; interface comes UP the kernel gives it a link-local fe80:: address by itself,
;;; and when a router on the link sends a Router Advertisement the kernel builds a
;;; global address from the advertised prefix + its own identifier (SLAAC,
;;; "stateless address autoconfiguration") and installs the v6 default route. So
;;; for v6 the Lisp only READS: /proc/net/if_inet6 and /proc/net/ipv6_route, both
;;; 32-hex-digit addresses with no colons.

(defun %hex->ipv6 (hex)
  "\"fe800000000000000000000000000001\" -> \"fe80::1\" (RFC 5952 shortening:
   the longest run of two or more zero groups becomes ::)."
  (let* ((groups (loop for i from 0 below 32 by 4
                       collect (parse-integer hex :start i :end (+ i 4) :radix 16)))
         (best-start nil) (best-len 1))
    (loop with start = nil
          for g in (append groups '(1)) for i from 0
          do (if (zerop g)
                 (unless start (setf start i))
                 (when start
                   (when (> (- i start) best-len) (setf best-start start best-len (- i start)))
                   (setf start nil))))
    (if best-start
        (format nil "~{~(~x~)~^:~}::~{~(~x~)~^:~}"
                (subseq groups 0 best-start) (subseq groups (+ best-start best-len)))
        (format nil "~{~(~x~)~^:~}" groups))))

(defun ipv6-addresses (&optional ifname)
  "The interfaces' IPv6 addresses as plists (:iface :address :prefix :scope),
   from /proc/net/if_inet6; only IFNAME's when given. :scope is :global, :link or
   :host (loopback)."
  (loop for line in (%read-lines "/proc/net/if_inet6")
        for f = (%split-ws line)
        when (and (= (length f) 6) (or (null ifname) (string= (sixth f) ifname)))
          collect (list :iface (sixth f)
                        :address (%hex->ipv6 (first f))
                        :prefix (parse-integer (third f) :radix 16)
                        :scope (case (parse-integer (fourth f) :radix 16)
                                 (#x00 :global) (#x20 :link) (#x10 :host) (t :other)))))

(defun ipv6-default-route ()
  "The IPv6 default route (::/0 with a gateway) as a plist (:iface :gateway), or
   NIL. Its presence is the test for 'is IPv6 usable beyond this link' — the
   resolver asks for AAAA records only when it is true."
  (loop for line in (%read-lines "/proc/net/ipv6_route")
        for f = (%split-ws line)
        when (and (= (length f) 10)
                  (string= (first f) "00000000000000000000000000000000")
                  (string= (second f) "00")
                  (string/= (fifth f) "00000000000000000000000000000000")
                  (string/= (tenth f) "lo"))
          return (list :iface (tenth f) :gateway (%hex->ipv6 (fifth f)))))

;;; ---- which interfaces exist: /sys/class/net --------------------------------
;;;
;;; The kernel names interfaces itself (eth0, wlan0 — there is no udev here to
;;; rename them to enp0s3/wlp0s20f3) and lists each one as a directory in
;;; /sys/class/net. A few files in it say what it is: `type' (1 = Ethernet-like,
;;; which Wi-Fi also is; 772 = loopback), `address' (the MAC), `operstate', and a
;;; `wireless'/`phy80211' entry that only 802.11 interfaces have.

(defun %sys-net (ifname file)
  (let ((l (%slurp-small (format nil "/sys/class/net/~a/~a" ifname file))))
    (and l (string-trim '(#\Space #\Newline) l))))

(defun interface-names ()
  "Names of every network interface the kernel has, sorted."
  (sort (mapcar (lambda (p) (car (last (pathname-directory p))))
                (directory "/sys/class/net/*/" :resolve-symlinks nil))
        #'string<))

(defun interface-wireless-p (ifname)
  (and (probe-file (format nil "/sys/class/net/~a/phy80211/" ifname)) t))

(defun interface-info (ifname)
  "A plist describing IFNAME: :name :kind (:loopback :wireless :wired) :mac
   :state (\"up\"/\"down\"/…) :carrier :ipv4 :ipv6 (a list of address plists)."
  (list :name ifname
        :kind (cond ((equal (%sys-net ifname "type") "772") :loopback)
                    ((interface-wireless-p ifname) :wireless)
                    (t :wired))
        :mac (%sys-net ifname "address")
        :state (%sys-net ifname "operstate")
        :carrier (equal (%sys-net ifname "carrier") "1")
        :ipv4 (ignore-errors (interface-ipv4 ifname))
        :ipv6 (ipv6-addresses ifname)))

(defun interfaces ()
  "INTERFACE-INFO for every interface."
  (mapcar #'interface-info (interface-names)))

(defun set-interface-up (ifname &optional (up t))
  "Raise (or lower) IFNAME's IFF_UP flag without touching its address. Wi-Fi must
   be UP before the supplicant can scan; T on success."
  (let ((fd (%socket +af-inet+ +sock-dgram+ 0))
        (buf (sb-alien:make-alien sb-alien:unsigned-char +ifreq-size+)))
    (unwind-protect
         (progn
           (%zero-ifreq buf) (%set-ifname buf ifname)
           (when (minusp (%ioctl fd +siocgifflags+ buf)) (return-from set-interface-up nil))
           (let* ((flags (logior (sb-alien:deref buf 16) (ash (sb-alien:deref buf 17) 8)))
                  (new (if up (logior flags +iff-up+) (logandc2 flags +iff-up+))))
             (setf (sb-alien:deref buf 16) (logand new #xff)
                   (sb-alien:deref buf 17) (logand (ash new -8) #xff)))
           (not (minusp (%ioctl fd +siocsifflags+ buf))))
      (sb-alien:free-alien buf)
      (%close fd))))

;;; ---- a TCP REPL server (sb-bsd-sockets, baked into the image) ------------
;;;
;;; SECURITY: this is plaintext, unauthenticated remote =eval= — i.e. remote
;;; ROOT, since the REPL is PID 1. Only safe when every wire that can reach the
;;; port is trusted: the QEMU hostfwd (host loopback) or a direct dev cable.
;;; NEVER expose it to an untrusted LAN. (networking.org §6.) It starts ON at boot
;;; for convenience and can be toggled off from the menu ('t') — acceptable only
;;; because the one wire that can reach it is the trusted QEMU hostfwd.
;;;
;;; We serve with the FULL line editor (editing-repl), not plain-repl: the host
;;; client (host-client/lol-repl-client.c) puts the *host* terminal in raw mode
;;; and forwards keystrokes byte-by-byte, so the editor here receives arrows etc.
;;; and drives history/editing remotely — the redraw escapes go back to the
;;; client's terminal. No PTY needed (see networking.org §6a).

(defparameter +net-repl-port+ 4005)
(defvar *net-repl-server* nil "Listening socket while the network REPL is enabled, else NIL.")
(defvar *net-repl-client* nil "Socket of the currently-connected client, if any.")
(defvar *net-repl-thread* nil "The background accept/serve thread, if running.")

(defun net-repl-running-p ()
  "True when the network REPL is currently accepting connections."
  (and *net-repl-thread* (sb-thread:thread-alive-p *net-repl-thread*)))

(defun %serve-net-repl ()
  "Accept clients one at a time and run the editing REPL over each. Exits when the
   listening socket is closed (that makes socket-accept error) — i.e. on stop."
  (handler-case
      (loop
        (let ((client (sb-bsd-sockets:socket-accept *net-repl-server*)))
          (setf *net-repl-client* client)
          (unwind-protect
               (ignore-errors
                 (let ((stream (sb-bsd-sockets:socket-make-stream
                                client :input t :output t
                                       :element-type 'character :buffering :full)))
                   (unwind-protect
                        (let ((*standard-input* stream) (*standard-output* stream))
                          (format stream "~&lisp-over-linux network REPL.~%~
                                          Arrows/history work via the host client; ~
                                          Ctrl-C aborts a line, Ctrl-D on an empty line disconnects.~%")
                          (finish-output stream)
                          (editing-repl stream stream))
                     (ignore-errors (close stream)))))
            (setf *net-repl-client* nil)
            (ignore-errors (sb-bsd-sockets:socket-close client)))))
    (serious-condition () nil)))          ; listening socket closed on stop -> end thread

(defun start-net-repl (&optional (port +net-repl-port+))
  "Begin serving the REPL on PORT in a background thread. No-op if already up."
  (when (net-repl-running-p) (return-from start-net-repl nil))
  (let ((server (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address server) t)
    (sb-bsd-sockets:socket-bind server #(0 0 0 0) port)
    (sb-bsd-sockets:socket-listen server 1)
    (setf *net-repl-server* server
          *net-repl-thread* (sb-thread:make-thread #'%serve-net-repl :name "net-repl")))
  t)

(defun stop-net-repl ()
  "Stop accepting connections and drop any active session. No-op if already down.
   We must fully tear the accept thread down before returning, or a later restart
   hits EADDRINUSE: closing the listening socket does NOT reliably wake a blocked
   socket-accept in another thread, so we *shutdown* it (which does), then
   *join* the thread so the port is released before we let anyone rebind it."
  (unless (net-repl-running-p) (return-from stop-net-repl nil))
  (let ((server *net-repl-server*) (client *net-repl-client*) (thread *net-repl-thread*))
    (ignore-errors (when client (sb-bsd-sockets:socket-close client)))        ; kick active session
    (ignore-errors (sb-bsd-sockets:socket-shutdown server :direction :io))    ; wake the accept
    (ignore-errors (sb-thread:join-thread thread :timeout 2))                 ; let the thread exit
    (ignore-errors (sb-bsd-sockets:socket-close server))                      ; now release the port
    (setf *net-repl-server* nil *net-repl-client* nil *net-repl-thread* nil))
  t)

(defun toggle-net-repl ()
  "Flip the network REPL on/off. Returns :started or :stopped."
  (if (net-repl-running-p) (progn (stop-net-repl) :stopped)
      (progn (start-net-repl) :started)))

;;; ---- off by default: who may switch it on --------------------------------
;;;
;;; The network REPL is remote eval with no authentication — on the laptop, on
;;; a café's Wi-Fi, that is a door anyone could walk through. So since tag #26 it
;;; is OFF unless the boot ASKS for it, and only two things can ask:
;;;
;;;   1. QEMU, through fw_cfg. `-fw_cfg name=opt/lol/netrepl,string=1` puts a
;;;      named blob in QEMU's firmware-config device; with CONFIG_FW_CFG_SYSFS the
;;;      kernel shows it at /sys/firmware/qemu_fw_cfg/by_name/opt/lol/netrepl/raw.
;;;      Real hardware has no fw_cfg device, so this switch cannot exist there —
;;;      which is exactly what makes it a safe test hook. build.sh and every
;;;      check-*.lisp pass it. The fw_cfg files are root-only (0400) and the
;;;      Lisp usually runs as the user, so preinit reads the blob while still
;;;      root and passes LOL_NETREPL=1 down in the environment.
;;;   2. The kernel cmdline: `lol.netrepl=1` (e.g. typed into the host's GRUB
;;;      entry for a debugging session). Our built-in CONFIG_CMDLINE never has it.
;;;
;;; The supervisor menu's `t' still toggles it by hand — a deliberate act at the
;;; console, not something the machine does on its own.

(defparameter *fw-cfg-netrepl* "/sys/firmware/qemu_fw_cfg/by_name/opt/lol/netrepl/raw")

(defun %slurp-small (path)
  "The first line of PATH as a string, or NIL if it cannot be read."
  (ignore-errors
   (with-open-file (s path :external-format :latin-1) (read-line s nil nil))))

(defun net-repl-wanted-p ()
  "True when this boot asked for the network REPL (fw_cfg or kernel cmdline)."
  (let ((fw (%slurp-small *fw-cfg-netrepl*))
        (cmdline (%slurp-small "/proc/cmdline")))
    (or (equal (sb-ext:posix-getenv "LOL_NETREPL") "1") ; preinit read fw_cfg as root
        (and fw (string= (string-trim '(#\Space #\Newline #\Nul) fw) "1"))
        (and cmdline (search "lol.netrepl=1" cmdline) t))))
