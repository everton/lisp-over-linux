;;;; wifi.lisp — Wi-Fi: the radio, the host's wpa_supplicant, and our own control of it.
;;;;
;;;; The laptop has no Ethernet port; Wi-Fi is its only network (networking.org §7).
;;;; Four layers have to line up, and only the last is ours:
;;;;
;;;;   1. the kernel's 802.11 stack + the iwlwifi driver (tag #26: CFG80211,
;;;;      MAC80211, IWLWIFI, IWLMVM), built in;
;;;;   2. the card's FIRMWARE, which the driver loads from /lib/firmware when it
;;;;      probes — build.sh copies it from the host into the initramfs (never into
;;;;      git), plus the signed regulatory database;
;;;;   3. a SUPPLICANT: joining a WPA2/WPA3 network is a cryptographic handshake (the
;;;;      EAPOL 4-way handshake, or SAE for WPA3) that no one should hand-roll. We run
;;;;      the HOST's /usr/sbin/wpa_supplicant from /mnt/host — through the host's own
;;;;      dynamic loader with the host's library path, because its libraries
;;;;      (libnl, libssl, libdbus…) are not in our initramfs. It inherits PID 1's
;;;;      ambient CAP_NET_ADMIN + CAP_NET_RAW, which is exactly what it needs;
;;;;   4. THIS FILE: switch the radio on (rfkill), start the supplicant, tell it which
;;;;      networks we know, listen to its events, and run DHCP (dhcp.lisp) on wlan0
;;;;      the moment it says CONNECTED.
;;;;
;;;; We talk to wpa_supplicant over its CONTROL INTERFACE: a Unix-domain DATAGRAM
;;;; socket per interface (/tmp/lol-wifi/ctrl/wlan0). Each request is one datagram of
;;;; text ("SCAN", "STATUS", "ADD_NETWORK"…) and the reply is one datagram back to the
;;;; address we sent from — so the client must BIND its own socket to a path too (a
;;;; datagram has nowhere to return to otherwise). A client that sends "ATTACH" then
;;;; also receives EVENTS as they happen: "<3>CTRL-EVENT-CONNECTED - Connection to
;;;; … completed", "CTRL-EVENT-DISCONNECTED", "CTRL-EVENT-SSID-TEMP-DISABLED …
;;;; reason=WRONG_KEY". One thread here listens for those.
;;;;
;;;; Which networks we know (credentials never go into git or the image):
;;;;   - ~/.config/lol/wifi/<ssid in hex>.net — one readable plist per network, mode
;;;;     0600, written when a connection with a typed passphrase SUCCEEDS;
;;;;   - the host's NetworkManager profiles: root-only on the host, so lol-rootd copies
;;;;     them to /tmp/lol-nm-import/ on request (preinit.c, ROOTD_SIG_NMIMPORT) — the
;;;;     laptop already knows the networks it has joined from Debian.
;;;; All of them are handed to the supplicant at start, enabled, and IT picks the
;;;; strongest one it can see and roams — the job it is good at.

(defvar *wifi-dir* "/tmp/lol-wifi/" "Our scratch: the supplicant's config, socket dir, log.")
(defvar *wifi-enabled* t "NIL after WIFI-OFF: netd then leaves the radio alone.")
(defvar *wpa-process* nil "The wpa_supplicant child process, while it runs.")
(defvar *wpa-iface* nil "The interface it was started on.")
(defvar *wifi-state* nil
  "The last STATUS, as a plist (:iface :state :ssid :bssid :freq :signal :ip), or NIL
   when there is no wireless interface. Refreshed by netd every few seconds; the
   systray reads only this, never the socket (it repaints every second).")
(defvar *wifi-message* nil "The last thing worth telling the user (\"wrong passphrase for X\").")
(defvar *wifi-events* '() "The supplicant's recent events, newest first: (universal-time . text).")
(defvar *wifi-scan* '() "The last scan results, strongest first (plists, see WIFI-SCAN-RESULTS).")
(defvar *wifi-pending* nil "(ssid . credentials) being tried; saved when CONNECTED arrives.")
(defvar *wifi-last-start* 0 "When we last tried to start the supplicant (rate limit).")

;;; ---- which interface; the kill switch ----------------------------------------

(defun wireless-interfaces ()
  "Names of the 802.11 interfaces (those with a phy80211 link in sysfs) — (\"wlan0\")
   on the laptop, NIL under QEMU, which emulates no Wi-Fi."
  (remove-if-not #'interface-wireless-p (interface-names)))

(defun wifi-iface () (first (wireless-interfaces)))

;;; rfkill is the kernel's "radio off" switch, in two kinds: HARD (a physical switch
;;; or a firmware state we cannot override) and SOFT (software can flip it — Fn-F8,
;;; or a previous OS's last setting, which some laptops restore at boot). Each radio
;;; is a directory /sys/class/rfkill/rfkillN with `type', `soft', `hard'. To unblock
;;; we write one 8-byte struct rfkill_event to /dev/rfkill (linux/rfkill.h):
;;;   u32 idx · u8 type · u8 op · u8 soft · u8 hard
;;; with op = RFKILL_OP_CHANGE_ALL (3), type = RFKILL_TYPE_WLAN (1), soft = 0.
;;; preinit gives /dev/rfkill to the user; the kernel then checks nothing more.

(defun rfkill-radios ()
  "Every radio's kill-switch state: plists (:name :type :soft :hard)."
  (loop for dir in (directory "/sys/class/rfkill/*/" :resolve-symlinks nil)
        for name = (car (last (pathname-directory dir)))
        for read = (lambda (f) (let ((l (%slurp-small (format nil "/sys/class/rfkill/~a/~a" name f))))
                                 (and l (string-trim '(#\Newline #\Space) l))))
        collect (list :name (funcall read "name") :type (funcall read "type")
                      :soft (equal (funcall read "soft") "1")
                      :hard (equal (funcall read "hard") "1"))))

(defun wifi-blocked ()
  "NIL, :soft or :hard — the worst state of any wlan radio."
  (let ((wlan (remove-if-not (lambda (r) (equal (getf r :type) "wlan")) (rfkill-radios))))
    (cond ((some (lambda (r) (getf r :hard)) wlan) :hard)
          ((some (lambda (r) (getf r :soft)) wlan) :soft))))

(defun rfkill-unblock-wifi ()
  "Clear the SOFT block on every wlan radio. T if the write went through."
  (let ((ev (make-array 8 :element-type '(unsigned-byte 8) :initial-contents '(0 0 0 0 1 3 0 0))))
    (multiple-value-bind (fd err) (sb-unix:unix-open "/dev/rfkill" sb-unix:o_wronly 0)
      (unless fd (setf *wifi-message* (format nil "rfkill: ~a" (sb-int:strerror err)))
        (return-from rfkill-unblock-wifi nil))
      (unwind-protect (sb-sys:with-pinned-objects (ev) (eql 8 (sb-unix:unix-write fd ev 0 8)))
        (sb-unix:unix-close fd)))))

;;; ---- the host's wpa_supplicant ------------------------------------------------------

(defparameter *host-libdir* "/usr/lib/x86_64-linux-gnu")

(defun host-program-argv (program &rest args)
  "The argv that runs the HOST's PROGRAM (a path inside the host root) here: its own
   dynamic loader, told where its libraries are. A program's ELF header names its
   loader as /lib64/ld-linux-x86-64.so.2 — OUR loader, which would search OUR /lib —
   so we run the host's loader explicitly and give it the host's library path. (No
   chroot: that needs CAP_SYS_CHROOT, and the program must still see our /dev,
   /tmp and network.)"
  (list* (host-file (concatenate 'string *host-libdir* "/ld-linux-x86-64.so.2"))
         "--library-path" (format nil "~a~a:~a/lib/x86_64-linux-gnu"
                                  *host-root* *host-libdir* *host-root*)
         (host-file program)
         args))

(defun wpa-supplicant-available-p ()
  (and (probe-file (host-file "/usr/sbin/wpa_supplicant")) t))

(defun wpa-supplicant-version ()
  "First line of the host supplicant's -v, or a reason it cannot run."
  (if (not (wpa-supplicant-available-p))
      (format nil "no wpa_supplicant at ~a" (host-file "/usr/sbin/wpa_supplicant"))
      (let* ((argv (host-program-argv "/usr/sbin/wpa_supplicant" "-v"))
             (out (with-output-to-string (s)
                    (sb-ext:run-program (first argv) (rest argv) :output s :error s))))
        (subseq out 0 (position #\Newline out)))))

(defun ctrl-dir () (concatenate 'string *wifi-dir* "ctrl"))
(defun ctrl-path (iface) (format nil "~a/~a" (ctrl-dir) iface))

(defun wpa-running-p ()
  (and *wpa-process* (sb-ext:process-alive-p *wpa-process*)))

(defun start-wpa-supplicant (iface &key (driver "nl80211"))
  "Launch the host's wpa_supplicant on IFACE, controlled through (ctrl-dir). Returns
   T once its control socket answers PING, else NIL with *WIFI-MESSAGE* saying why.
   DRIVER is nl80211 for Wi-Fi; check-net.lisp passes \"wired\" (802.1X on Ethernet)
   to exercise this file's control-socket code under QEMU, which has no radio."
  (setf *wifi-last-start* (get-universal-time))
  (unless (wpa-supplicant-available-p)
    (setf *wifi-message* (format nil "no wpa_supplicant at ~a — install it on the host (apt install wpasupplicant)"
                                 (host-file "/usr/sbin/wpa_supplicant")))
    (return-from start-wpa-supplicant nil))
  (ensure-directories-exist *wifi-dir*)
  (%chmod *wifi-dir* #o700)        ; files.lisp's libc chmod — no sb-posix (processes.org §6)
  (let ((conf (concatenate 'string *wifi-dir* "wpa_supplicant.conf")))
    ;; The config holds no networks: they are added over the socket, so no secret
    ;; is ever written into it. update_config=0: the supplicant never rewrites it.
    (with-open-file (o conf :direction :output :if-exists :supersede)
      (format o "# written by lisp-over-linux (initramfs/wifi.lisp); networks come over the socket~%")
      (format o "ctrl_interface=~a~%update_config=0~%" (ctrl-dir)))
    (let ((argv (host-program-argv "/usr/sbin/wpa_supplicant" "-i" iface "-D" driver "-c" conf)))
      (setf *wpa-iface* iface
            *wpa-process* (sb-ext:run-program (first argv) (rest argv)
                                              :wait nil :input nil
                                              :output (concatenate 'string *wifi-dir* "wpa_supplicant.log")
                                              :if-output-exists :supersede :error :output))))
  (loop repeat 50                                          ; up to 5 s for the socket
        do (sleep 0.1)
           (unless (wpa-running-p)
             (setf *wifi-message* (format nil "wpa_supplicant exited (code ~a) — see ~awpa_supplicant.log"
                                          (sb-ext:process-exit-code *wpa-process*) *wifi-dir*))
             (return nil))
           (when (equal (ignore-errors (wpa-request "PING" :timeout 1)) "PONG")
             (return t))))

(defun stop-wpa-supplicant ()
  (when (wpa-running-p)
    (ignore-errors (wpa-request "TERMINATE" :timeout 1))
    (sleep 0.3)
    (when (wpa-running-p) (sb-ext:process-kill *wpa-process* 15)))
  ;; a wireless link without its supplicant is down; a WIRED one (the test driver)
  ;; still works — dropping its lease would cut the very connection we came in on
  (when (and *wpa-iface* (interface-wireless-p *wpa-iface*)) (forget-lease *wpa-iface*))
  (setf *wpa-process* nil *wifi-state* nil))

;;; ---- the control socket ------------------------------------------------------------

(defvar *wpa-client-counter* 0)
(defvar *wpa-lock* (sb-thread:make-mutex :name "wpa ctrl"))

(defun call-with-ctrl-socket (iface fn)
  "A fresh datagram socket bound to our own path and connected to IFACE's control
   socket; FN gets it. Fresh per request, so a reply can never be confused with an
   earlier one."
  (let ((me (format nil "~alisp-~d-~d" *wifi-dir* (sb-unix:unix-getpid)
                    (sb-thread:with-mutex (*wpa-lock*) (incf *wpa-client-counter*))))
        (s (make-instance 'sb-bsd-sockets:local-socket :type :datagram)))
    (unwind-protect
         (progn (ignore-errors (delete-file me))
                (sb-bsd-sockets:socket-bind s me)
                (sb-bsd-sockets:socket-connect s (ctrl-path iface))
                (funcall fn s))
      (ignore-errors (sb-bsd-sockets:socket-close s))
      (ignore-errors (delete-file me)))))

(defun wpa-request (command &key (iface (or *wpa-iface* (wifi-iface))) (timeout 5))
  "Send COMMAND to the supplicant and return its reply text (trailing newline
   trimmed). Signals on timeout. \"OK\" / \"FAIL\" / data, per command."
  (call-with-ctrl-socket
   iface
   (lambda (s)
     (set-rcv-timeout (sb-bsd-sockets:socket-file-descriptor s) timeout)
     (let ((octets (sb-ext:string-to-octets command :external-format :utf-8))
           (buf (make-array 65536 :element-type '(unsigned-byte 8))))
       (sb-bsd-sockets:socket-send s octets (length octets))
       (multiple-value-bind (b len) (sb-bsd-sockets:socket-receive s buf 65536)
         (declare (ignore b))
         (unless (and len (plusp len)) (error "wpa_supplicant did not answer ~s" command))
         (string-right-trim '(#\Newline)
                            (sb-ext:octets-to-string buf :end len :external-format '(:utf-8 :replacement #\?))))))))

(defun wpa-ok (command)
  (let ((r (wpa-request command)))
    (unless (equal r "OK") (error "wpa_supplicant: ~a -> ~a" (subseq command 0 (min 30 (length command))) r))
    r))

(defun key-value-lines (text)
  "\"a=1\\nb=2\" -> ((\"a\" . \"1\") (\"b\" . \"2\")) — STATUS and SIGNAL_POLL's shape."
  (loop for line in (wpa-split-lines text)
        for eq = (position #\= line)
        when eq collect (cons (subseq line 0 eq) (subseq line (1+ eq)))))

(defun wpa-split-lines (text)
  "TEXT split at each newline, a trailing empty line kept. Its own name: it was
   called SPLIT-LINES, which silently replaced fb-browser.lisp's (that one drops the
   trailing empty line, and the debugger header relies on it) — 2026-10-16."
  (loop with start = 0 for nl = (position #\Newline text :start start)
        collect (subseq text start nl) while nl do (setf start (1+ nl))))

(defun split-tabs (line)
  (loop with start = 0 for tab = (position #\Tab line :start start)
        collect (subseq line start tab) while tab do (setf start (1+ tab))))

;;; ---- SSIDs: bytes, not text ---------------------------------------------------------
;;; An SSID is up to 32 arbitrary BYTES. The supplicant prints it with C-style escapes
;;; (\xNN for anything unprintable, \\ and \"), and accepts it back as HEX. We keep
;;; SSIDs as Lisp strings decoded from UTF-8 (what every phone and router uses).

(defun unescape-ssid (s)
  (let ((out (make-array (length s) :element-type '(unsigned-byte 8) :fill-pointer 0)) (i 0))
    (loop while (< i (length s))
          do (let ((c (char s i)))
               (cond ((and (char= c #\\) (< (1+ i) (length s)) (char= (char s (1+ i)) #\x)
                           (<= (+ i 4) (length s)))
                      (vector-push (parse-integer s :start (+ i 2) :end (+ i 4) :radix 16) out)
                      (incf i 4))
                     ((and (char= c #\\) (< (1+ i) (length s)))
                      (vector-push (char-code (char s (1+ i))) out) (incf i 2))
                     (t (loop for b across (sb-ext:string-to-octets (string c) :external-format :utf-8)
                              do (vector-push b out))
                        (incf i)))))
    (sb-ext:octets-to-string (coerce out '(vector (unsigned-byte 8)))
                             :external-format '(:utf-8 :replacement #\?))))

(defun ssid-hex (ssid)
  (format nil "~{~(~2,'0x~)~}" (coerce (sb-ext:string-to-octets ssid :external-format :utf-8) 'list)))

;;; ---- scanning -------------------------------------------------------------------------

(defun signal-percent (dbm)
  "A rough 0–100 from dBm, the way NetworkManager shows it: -100 dBm is 0, -50 is 100."
  (max 0 (min 100 (* 2 (+ dbm 100)))))

(defun security-of (flags)
  (cond ((search "SAE" flags) (if (search "PSK" flags) :wpa2/3 :wpa3))
        ((search "PSK" flags) :wpa2)
        ((search "EAP" flags) :enterprise)
        ((search "WEP" flags) :wep)
        (t :open)))

(defun wifi-scan-results ()
  "The supplicant's current scan table, one plist per SSID (the strongest BSSID of
   each): :ssid :bssid :freq :signal (dBm) :security :flags :known. Strongest first."
  (let ((rows (loop for line in (rest (wpa-split-lines (wpa-request "SCAN_RESULTS")))
                    for f = (split-tabs line)
                    when (>= (length f) 5)
                      collect (let ((ssid (unescape-ssid (fifth f))))
                                (list :ssid ssid :bssid (first f)
                                      :freq (ignore-errors (parse-integer (second f)))
                                      :signal (or (ignore-errors (parse-integer (third f))) -100)
                                      :flags (fourth f) :security (security-of (fourth f)))))))
    (let ((best (make-hash-table :test 'equal)))
      (dolist (r rows)
        (let ((old (gethash (getf r :ssid) best)))
          (when (and (plusp (length (getf r :ssid)))          ; hidden networks: no name
                     (or (null old) (> (getf r :signal) (getf old :signal))))
            (setf (gethash (getf r :ssid) best) r))))
      (sort (loop for r being the hash-values of best
                  collect (append r (list :known (and (wifi-credentials (getf r :ssid)) t))))
            #'> :key (lambda (r) (getf r :signal))))))

(defun wifi-scan ()
  "Ask for a fresh scan. The results arrive a few seconds later as an event, which
   refreshes *WIFI-SCAN*."
  (wpa-request "SCAN"))

;;; ---- credentials ---------------------------------------------------------------------

(defun wifi-config-dir ()
  (format nil "~a/.config/lol/wifi/" (string-right-trim "/" (or (sb-ext:posix-getenv "HOME") "/tmp"))))

(defun credentials-file (ssid) (format nil "~a~a.net" (wifi-config-dir) (ssid-hex ssid)))

(defun read-plist-file (path)
  "A plist from PATH, read as DATA: *READ-EVAL* off, so #.(…) in a file cannot run."
  (ignore-errors
   (with-open-file (s path :external-format :utf-8)
     (with-standard-io-syntax
       (let ((*read-eval* nil) (*package* (find-package :keyword)))
         (read s nil nil))))))

(defun save-wifi-credentials (ssid creds)
  "Write CREDS (:psk / :security) for SSID to ~/.config/lol/wifi/ (a 0700
   directory). The FILE is created 0600 by open(2) itself — O_CREAT|O_EXCL with the
   mode — never chmod'ed after the fact: a passphrase must not sit world-readable
   even for a moment."
  (let ((dir (wifi-config-dir)))
    (ensure-directories-exist dir)
    (%chmod dir #o700)
    (let* ((text (with-standard-io-syntax
                   (let ((*package* (find-package :keyword)))
                     (format nil ";; Wi-Fi network, saved by lisp-over-linux (initramfs/wifi.lisp)~%~s~%"
                             (list* :ssid ssid creds)))))
           (path (credentials-file ssid))
           (octets (sb-ext:string-to-octets text :external-format :utf-8)))
      (ignore-errors (delete-file path))
      (multiple-value-bind (fd err)
          (sb-unix:unix-open path (logior sb-unix:o_wronly sb-unix:o_creat sb-unix:o_excl) #o600)
        (unless fd (error "cannot save ~a: ~a" path (sb-int:strerror err)))
        (unwind-protect (sb-sys:with-pinned-objects (octets) (sb-unix:unix-write fd octets 0 (length octets)))
          (sb-unix:unix-close fd)))
      path)))

(defun saved-networks ()
  "Every network saved under ~/.config/lol/wifi/, as plists (:ssid :psk :security)."
  (remove nil (mapcar #'read-plist-file (directory (concatenate 'string (wifi-config-dir) "*.net")))))

(defun forget-wifi-network (ssid)
  (ignore-errors (delete-file (credentials-file ssid)))
  (setf *nm-networks* (remove ssid *nm-networks* :key (lambda (n) (getf n :ssid)) :test #'equal))
  (when (wpa-running-p)
    (let ((id (supplicant-network-id ssid)))
      (when id (ignore-errors (wpa-ok (format nil "REMOVE_NETWORK ~d" id)))))))

;;; NetworkManager's keyfiles are INI: [connection] type=wifi, [wifi] ssid=…,
;;; [wifi-security] key-mgmt=wpa-psk|sae, psk=…. Older NM wrote the SSID as a list of
;;; byte values ("ssid=72;111;109;101;") — both forms are read. A psk-flags=1 profile
;;; keeps its secret in a keyring agent, not the file: there is nothing to import.

(defvar *nm-import-dir* "/tmp/lol-nm-import/")
(defvar *nm-networks* nil "Networks parsed from the host's NetworkManager profiles.")
(defvar *nm-import-requested* nil)

(defun parse-nmconnection (path)
  (let ((section nil) (keys '()))
    (dolist (line (%read-lines-utf8 path))
      (let ((l (string-trim '(#\Space #\Tab #\Return) line)))
        (cond ((or (zerop (length l)) (char= (char l 0) #\#)))
              ((char= (char l 0) #\[) (setf section (string-trim "[]" l)))
              (t (let ((eq (position #\= l)))
                   (when eq (push (cons (format nil "~a.~a" section (subseq l 0 eq)) (subseq l (1+ eq)))
                                  keys)))))))
    (flet ((val (k) (cdr (assoc k keys :test #'string=))))
      (let ((ssid (val "wifi.ssid")))
        (when (and (equal (val "connection.type") "wifi") ssid)
          (when (every (lambda (c) (or (digit-char-p c) (char= c #\;))) ssid)   ; the byte-list form
            (setf ssid (sb-ext:octets-to-string
                        (coerce (mapcar #'parse-integer (remove "" (split-on #\; ssid) :test #'string=))
                                '(vector (unsigned-byte 8)))
                        :external-format '(:utf-8 :replacement #\?))))
          (let ((km (val "wifi-security.key-mgmt")) (psk (val "wifi-security.psk")))
            (cond ((null km) (list :ssid ssid :security :open :from :networkmanager))
                  ((and psk (member km '("wpa-psk" "sae") :test #'string=))
                   (list :ssid ssid :psk psk :security (if (string= km "sae") :wpa3 :wpa2)
                         :from :networkmanager)))))))))

(defun split-on (ch s)
  (loop with start = 0 for p = (position ch s :start start)
        collect (subseq s start p) while p do (setf start (1+ p))))

(defun %read-lines-utf8 (path)
  (ignore-errors
   (with-open-file (s path :external-format '(:utf-8 :replacement #\?))
     (loop for l = (read-line s nil nil) while l collect l))))

(defun request-nm-import (&key (wait 3))
  "Ask lol-rootd to copy the host's NetworkManager profiles (they are root-only), then
   parse them into *NM-NETWORKS*. As root (a diskless boot) read them directly."
  (let ((src (host-file "/etc/NetworkManager/system-connections/")))
    (cond ((zerop (sb-unix:unix-getuid))
           (setf *nm-networks* (remove nil (mapcar #'parse-nmconnection
                                                   (directory (concatenate 'string src "*.nmconnection"))))))
          ((rootd-pid)
           (%sigqueue (rootd-pid) 41 0)             ; preinit.c's ROOTD_SIG_NMIMPORT
           (loop repeat (* 10 wait) until (probe-file *nm-import-dir*) do (sleep 0.1))
           (sleep 0.3)
           (setf *nm-networks* (remove nil (mapcar #'parse-nmconnection
                                                   (directory (concatenate 'string *nm-import-dir*
                                                                           "*.nmconnection"))))))))
  (setf *nm-import-requested* t)
  *nm-networks*)

(defun known-networks ()
  "Everything we can join without asking: saved networks, then NetworkManager's
   (a saved one wins over an imported one with the same SSID)."
  (let ((saved (saved-networks)))
    (append saved
            (remove-if (lambda (n) (find (getf n :ssid) saved :key (lambda (s) (getf s :ssid)) :test #'equal))
                       *nm-networks*))))

(defun wifi-credentials (ssid)
  (find ssid (known-networks) :key (lambda (n) (getf n :ssid)) :test #'equal))

;;; ---- telling the supplicant about networks ---------------------------------------------

(defun supplicant-networks ()
  "LIST_NETWORKS as plists (:id :ssid :flags)."
  (loop for line in (rest (wpa-split-lines (wpa-request "LIST_NETWORKS")))
        for f = (split-tabs line)
        when (>= (length f) 2)
          collect (list :id (parse-integer (first f)) :ssid (unescape-ssid (second f))
                        :flags (or (fourth f) ""))))

(defun supplicant-network-id (ssid)
  (getf (find ssid (supplicant-networks) :key (lambda (n) (getf n :ssid)) :test #'equal) :id))

(defun quote-for-supplicant (passphrase)
  "A passphrase as SET_NETWORK wants it: 64 hex digits are a raw key and go bare;
   anything else is a passphrase (8–63 characters) inside double quotes."
  (if (and (= (length passphrase) 64) (every (lambda (c) (digit-char-p c 16)) passphrase))
      passphrase
      (format nil "\"~a\"" passphrase)))

(defun configure-network (id creds)
  "SET_NETWORK the security of network ID from CREDS."
  (let ((psk (getf creds :psk)))
    (cond
      ((or (null psk) (eq (getf creds :security) :open))
       (wpa-ok (format nil "SET_NETWORK ~d key_mgmt NONE" id)))
      (t
       (when (< (length psk) 8) (error "a WPA passphrase has at least 8 characters"))
       ;; Offer WPA2 and WPA3 alike; the AP picks. WPA3 (SAE) requires protected
       ;; management frames, so allow them (ieee80211w 1 = optional).
       (wpa-ok (format nil "SET_NETWORK ~d key_mgmt WPA-PSK WPA-PSK-SHA256 SAE" id))
       (wpa-ok (format nil "SET_NETWORK ~d ieee80211w 1" id))
       (wpa-ok (format nil "SET_NETWORK ~d psk ~a" id (quote-for-supplicant psk)))))))

(defun add-network (ssid creds &key (enable t))
  "Make sure the supplicant knows SSID with CREDS; returns its network id."
  (let ((id (or (supplicant-network-id ssid)
                (let ((r (wpa-request "ADD_NETWORK")))
                  (or (ignore-errors (parse-integer r)) (error "ADD_NETWORK -> ~a" r))))))
    (wpa-ok (format nil "SET_NETWORK ~d ssid ~a" id (ssid-hex ssid)))   ; hex: any bytes at all
    (wpa-ok (format nil "SET_NETWORK ~d scan_ssid 1" id))              ; finds hidden ones too
    (configure-network id creds)
    (when enable (wpa-ok (format nil "ENABLE_NETWORK ~d" id)))
    id))

(defun load-known-networks ()
  (dolist (n (known-networks))
    (handler-case (add-network (getf n :ssid) n)
      (error (c) (wifi-note "could not add ~a: ~a" (getf n :ssid) c)))))

;;; ---- connect / disconnect (what the network view's keys call) -------------------------

(defun wifi-connect (ssid &optional passphrase)
  "Join SSID: with PASSPHRASE when given (a new or changed network), else with what we
   already know, else as an open network. Returns at once; CONNECTED arrives as an
   event, DHCP follows, and only then are new credentials saved."
  (unless (wpa-running-p) (error "Wi-Fi is not running~@[: ~a~]" *wifi-message*))
  (let* ((scan (find ssid *wifi-scan* :key (lambda (r) (getf r :ssid)) :test #'equal))
         (creds (cond (passphrase (list :psk passphrase
                                        :security (if scan (getf scan :security) :wpa2)))
                      ((wifi-credentials ssid))
                      ((and scan (eq (getf scan :security) :open)) (list :security :open))
                      (t (error "~a needs a passphrase" ssid))))
         (id (add-network ssid creds :enable nil)))
    (setf *wifi-pending* (cons ssid creds)
          *wifi-message* (format nil "joining ~a…" ssid))
    (wpa-ok (format nil "SELECT_NETWORK ~d" id))     ; this one now, the others paused
    id))

(defun wifi-disconnect ()
  (when (wpa-running-p) (wpa-ok "DISCONNECT"))
  (when (and *wpa-iface* (interface-wireless-p *wpa-iface*)) (forget-lease *wpa-iface*))
  (setf *wifi-message* "disconnected (reconnect: connect to a network)"))

(defun wifi-off ()
  "Stop the supplicant and keep netd from restarting it."
  (setf *wifi-enabled* nil)
  (stop-wpa-supplicant)
  (setf *wifi-message* "Wi-Fi off"))

(defun wifi-on () (setf *wifi-enabled* t *wifi-last-start* 0 *wifi-message* "Wi-Fi on"))

;;; ---- events ------------------------------------------------------------------------------

(defvar *wifi-event-thread* nil)

(defun wifi-note (fmt &rest args)
  (network-log (format nil "wifi: ~?" fmt args))
  (push (cons (get-universal-time) (apply #'format nil fmt args)) *wifi-events*)
  (when (> (length *wifi-events*) 50) (setf *wifi-events* (subseq *wifi-events* 0 50))))

(defvar *wifi-dhcp-busy* nil "True while a DHCP exchange for the Wi-Fi link runs.")
(defvar *wifi-dhcp-last* 0 "When WIFI-DHCP last ran (universal time).")

(defun wifi-dhcp (iface)
  "Ask DHCP for an address on the associated IFACE and say how it went. Called on
   association (the event thread) and again by WIFI-TICK while associated without
   a lease — one round of DISCOVERs is not enough on every network, and the first
   one can go out before the access point forwards our frames."
  (unless *wifi-dhcp-busy*
    (setf *wifi-dhcp-busy* t *wifi-dhcp-last* (get-universal-time))
    (unwind-protect
         (progn
           (setf *wifi-message* "associated — asking DHCP for an address…")
           (let ((lease (dhcp-configure iface :verbose nil)))
             (setf *wifi-message*
                   (if lease (format nil "connected: ~a" (describe-lease lease))
                       (format nil "associated, but no DHCP answer yet — retrying; ~
                                    the network view's log says what was seen")))))
      (setf *wifi-dhcp-busy* nil))))

(defun on-wifi-event (text iface)
  "React to one event line (its \"<3>\" priority already stripped)."
  (wifi-note "~a" text)
  (cond
    ((search "CTRL-EVENT-CONNECTED" text)
     (let ((pending *wifi-pending*))
       (wifi-dhcp iface)
       (when pending
         ;; it WORKED: now the passphrase is worth keeping, and the other networks
         ;; may be offered again for roaming
         (when (getf (cdr pending) :psk)
           (ignore-errors (save-wifi-credentials (car pending) (cdr pending))))
         (setf *wifi-pending* nil)
         (ignore-errors (wpa-ok "ENABLE_NETWORK all")))))
    ((search "CTRL-EVENT-DISCONNECTED" text)
     (forget-lease iface))
    ((search "reason=WRONG_KEY" text)
     (setf *wifi-message* (format nil "wrong passphrase~@[ for ~a~]" (car *wifi-pending*))
           *wifi-pending* nil))
    ((search "CTRL-EVENT-SCAN-RESULTS" text)
     (let ((r (ignore-errors (wifi-scan-results))))
       (when r (setf *wifi-scan* r))))))

(defun wifi-event-loop (iface)
  "ATTACH to the supplicant and handle its events until it goes away."
  (call-with-ctrl-socket
   iface
   (lambda (s)
     (let ((buf (make-array 8192 :element-type '(unsigned-byte 8))))
       (set-rcv-timeout (sb-bsd-sockets:socket-file-descriptor s) 30)
       (sb-bsd-sockets:socket-send s (sb-ext:string-to-octets "ATTACH") 6)
       (loop while (wpa-running-p)
             do (multiple-value-bind (b len) (ignore-errors (sb-bsd-sockets:socket-receive s buf 8192))
                  (declare (ignore b))
                  (when (and len (plusp len))
                    (let* ((text (sb-ext:octets-to-string buf :end len :external-format '(:utf-8 :replacement #\?)))
                           (text (if (and (plusp (length text)) (char= (char text 0) #\<))
                                     (subseq text (1+ (or (position #\> text) 0)))
                                     text)))
                      (unless (member text '("OK" "PONG") :test #'string=)
                        (handler-case (on-wifi-event text iface)
                          (error (c) (wifi-note "event handler: ~a" c))))))))))))

(defun start-wifi-events (iface)
  (unless (and *wifi-event-thread* (sb-thread:thread-alive-p *wifi-event-thread*))
    (setf *wifi-event-thread*
          (sb-thread:make-thread (lambda () (ignore-errors (wifi-event-loop iface))) :name "wifi-events"))))

;;; ---- the netd hook: keep Wi-Fi up, keep *WIFI-STATE* fresh ------------------------------

(defun refresh-wifi-state (iface)
  (let* ((st (key-value-lines (wpa-request "STATUS" :timeout 2)))
         (sig (ignore-errors (key-value-lines (wpa-request "SIGNAL_POLL" :timeout 2))))
         (val (lambda (k alist) (cdr (assoc k alist :test #'string=)))))
    (setf *wifi-state*
          (list :iface iface
                :state (funcall val "wpa_state" st)
                :ssid (let ((s (funcall val "ssid" st))) (and s (unescape-ssid s)))
                :bssid (funcall val "bssid" st)
                :freq (funcall val "freq" st)
                :signal (ignore-errors (parse-integer (funcall val "RSSI" sig)))
                :ip (funcall val "ip_address" st)))))

(defun wifi-tick ()
  "Called by netd every few seconds: bring Wi-Fi up when there is a radio and it is
   not running (at most every 30 s, so a broken setup does not spin), else refresh
   *WIFI-STATE*."
  (let ((iface (wifi-iface)))
    (cond
      ((null iface) (setf *wifi-state* nil))
      ((not *wifi-enabled*) (setf *wifi-state* (list :iface iface :state "OFF")))
      ((wpa-running-p)
       (handler-case (refresh-wifi-state iface)
         (error () (setf *wifi-state* (list :iface iface :state "UNKNOWN"))))
       ;; associated but no address: keep asking, every 20 s
       (when (and (equal (getf *wifi-state* :state) "COMPLETED")
                  (not (lease-for iface))
                  (> (- (get-universal-time) *wifi-dhcp-last*) 20))
         (wifi-dhcp iface)))
      ((> (- (get-universal-time) *wifi-last-start*) 30)
       (setf *wifi-state* (list :iface iface :state "STARTING"))
       (when (eq (wifi-blocked) :soft) (rfkill-unblock-wifi) (sleep 0.5))
       (if (eq (wifi-blocked) :hard)
           (setf *wifi-message* "the radio is switched off in hardware (a switch or Fn key)"
                 *wifi-last-start* (get-universal-time))
           (progn
             (set-interface-up iface)
             (unless *nm-import-requested* (ignore-errors (request-nm-import)))
             (when (start-wpa-supplicant iface)
               (start-wifi-events iface)
               (load-known-networks)
               (ignore-errors (wifi-scan))
               (setf *wifi-message* (format nil "Wi-Fi up on ~a, ~d known network~:p"
                                            iface (length (known-networks)))))))))))

(pushnew 'wifi-tick *netd-hooks*)

;;; ---- the kernel's side of the story -----------------------------------------------------
;;; When Wi-Fi does not come up on new hardware, the answer is in the kernel log: did
;;; iwlwifi find its firmware, did cfg80211 accept the regulatory database? There is no
;;; `dmesg' here, but /dev/kmsg IS the kernel log as a file: each read(2) returns one
;;; record, "PRIORITY,SEQUENCE,MICROSECONDS,FLAGS;message". Opened O_NONBLOCK, the
;;; read that would wait for a NEW record fails with EAGAIN instead — that is the end.
;;; Readable by the user because this kernel has no DMESG_RESTRICT (tag #26 config).

(defun kernel-log (&optional (pattern "") (limit 40))
  "The kernel log's messages containing PATTERN (case-insensitive), the last LIMIT."
  (let ((out '()) (buf (make-array 8192 :element-type '(unsigned-byte 8))))
    ;; O_NONBLOCK is #o4000 on x86-64 (linux/include/uapi/asm-generic/fcntl.h,
    ;; 1 << 11); sb-unix exports no name for it
    (multiple-value-bind (fd err) (sb-unix:unix-open "/dev/kmsg" (logior sb-unix:o_rdonly #o4000) 0)
      (unless fd (return-from kernel-log (list (format nil "/dev/kmsg: ~a" (sb-int:strerror err)))))
      (unwind-protect
           (loop
             (let ((n (sb-sys:with-pinned-objects (buf)
                        (sb-unix:unix-read fd (sb-sys:vector-sap buf) 8192))))
               (unless n (return))                      ; EAGAIN: no more records
               (when (plusp n)
                 (let* ((rec (sb-ext:octets-to-string buf :end n :external-format '(:utf-8 :replacement #\?)))
                        (msg (string-right-trim '(#\Newline) (subseq rec (1+ (or (position #\; rec) -1))))))
                   (when (search pattern msg :test #'char-equal) (push msg out))))))
        (sb-unix:unix-close fd)))
    (let ((all (nreverse out)))
      (last all limit))))

(defun wifi-kernel-log ()
  "The lines that say whether the radio's driver, firmware and regulatory db loaded."
  (remove-if-not (lambda (m) (some (lambda (p) (search p m :test #'char-equal))
                                   '("iwlwifi" "cfg80211" "wlan" "regulatory" "rfkill" "firmware")))
                 (kernel-log "" 400)))

;;; ---- the systray item ----------------------------------------------------------------
;;; The host's mode line shows the SSID (`iwgetid -r`) or "Not connected!" (doc/systray.org).

(defun network-tray-item ()
  "A tray plist for sysinfo.lisp's TRAY-ITEMS: Wi-Fi's SSID and signal, \"Not
   connected!\" with a radio but no association, else the wired link with a lease,
   else NIL (nothing to say). Reads only cached state — never blocks."
  (let ((w *wifi-state*))
    (cond
      ((and w (equal (getf w :state) "COMPLETED") (getf w :ssid))
       (let ((pct (and (getf w :signal) (signal-percent (getf w :signal)))))
         (list :kind :wifi :text (format nil "~a~@[ ~d%~]" (getf w :ssid) pct)
               :tone (cond ((null pct) :ok) ((< pct 30) :low) ((< pct 55) :warn) (t :ok)))))
      (w (list :kind :wifi :text "Not connected!" :tone :low))
      (t (let ((l (and (fboundp 'current-lease) (current-lease))))
           (and l (list :kind :wired :text (format nil "~a ~a" (lease-iface l) (octets->dotted (lease-ip l)))
                        :tone :ok)))))))
