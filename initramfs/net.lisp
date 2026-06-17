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
