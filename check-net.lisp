;;;; check-net.lisp — assert the NETWORK in the booted guest: routes, DNS, HTTP(S), time.
;;;;
;;;;     LOL_GUEST_HOSTROOT=1 sbcl --script check-net.lisp      ; exits 0 if all pass
;;;;
;;;; LOL_GUEST_HOSTROOT=1 shares the build host's / with the guest (read-only, 9p), so
;;;; the guest runs as the USER — the laptop's situation — and can load the host's
;;;; libssl for HTTPS and launch the host's wpa_supplicant. Without it the HTTPS and
;;;; Wi-Fi checks report what they could not do and fail.
;;;;
;;;; Needs the internet on the host (QEMU's SLIRP NATs the guest through it). Checks,
;;;; in the order doc/networking.org builds them up:
;;;;
;;;;   - the lease: eth0 has 10.0.2.15, and a DEFAULT ROUTE via 10.0.2.2 (SIOCADDRT);
;;;;   - /etc/resolv.conf was written from the lease, by the user, in our root;
;;;;   - DNS: a public name resolves through SLIRP's 10.0.2.3, NXDOMAIN is an error;
;;;;   - plain HTTP to a server on THIS host (host-client/http-test-server.lisp,
;;;;     reached at 10.0.2.2): Content-Length, chunked (three pieces through
;;;;     ON-CHUNK), a redirect;
;;;;   - HTTPS to https://example.com/ through the host's libssl, and a bad
;;;;     certificate refused;
;;;;   - the clock: SNTP answered, and lol-rootd steps the clock on request (we push
;;;;     it 100 s ahead and let SNTP bring it back);
;;;;   - IPv6: SLAAC gave eth0 a global address, and AAAA is asked only with a v6
;;;;     default route;
;;;;   - Wi-Fi with no radio: the network view and the systray cope, and the host's
;;;;     wpa_supplicant starts from /mnt/host (and is stopped again).
;;;; All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(load (merge-pathnames "host-client/http-test-server.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(use-package :lol.guest)

(defun row (label value)
  (format t "~&  ~44a ~a~%" label value)
  (finish-output)
  value)

(defparameter *http-port* (+ 18000 (mod lol.guest::*host-port* 1000))
  "The host test server's port, derived from LOL_PORT so two runs cannot collide.")

(defun unquote (s) (string-trim "\"" s))

(defun checks ()
  (let ((hostroot (equal (sb-posix:getenv "LOL_GUEST_HOSTROOT") "1")))
    (unless hostroot
      (format t "~&LOL_GUEST_HOSTROOT is not set — HTTPS and Wi-Fi checks will fail.~%"))
    (lol.http-test-server:start-test-http-server *http-port*)

    (format t "~&~%== the lease and the route ==~%")
    (row "lease" (probe "(describe-lease (current-lease))"))
    (check "eth0 leased 10.0.2.15" (equal (unquote (probe "(octets->dotted (lease-ip (lease-for \"eth0\")))")) "10.0.2.15"))
    (check "default route via 10.0.2.2 on eth0"
           (equal (row "default route" (probe "(let ((r (ipv4-default-route))) (list (getf r :iface) (getf r :gateway)))"))
                  "(\"eth0\" \"10.0.2.2\")"))
    (check "netd (the renewal thread) is running"
           (equal "T" (probe "(and (sb-thread:thread-alive-p *netd-thread*) t)")))
    (when hostroot
      (check "PID 1 is the user (uid 1000) — the laptop's situation"
             (= 1000 (or (probe-int "(sb-unix:unix-getuid)") -1))))
    (let ((rc (row "/etc/resolv.conf" (probe "(with-open-file (s \"/etc/resolv.conf\") (loop for l = (read-line s nil) while l when (search \"nameserver\" l) return l))"))))
      (check "/etc/resolv.conf names SLIRP's 10.0.2.3" (search "nameserver 10.0.2.3" rc)))
    (check "/etc/resolv.conf is owned by the user who wrote it"
           (= (or (probe-int "(nth-value 5 (sb-unix:unix-stat \"/etc/resolv.conf\"))") -2)
              (or (probe-int "(sb-unix:unix-getuid)") -1)))

    (format t "~&~%== DNS ==~%")
    (let ((a (row "resolve-host example.com" (probe "(address-string (resolve-host \"example.com\"))" 15))))
      (check "example.com resolves to an IPv4 address" (= 3 (count #\. a))))
    (check "a CNAME is chased (www.github.com)"
           (equal "T" (row "www.github.com" (probe "(and (dns-query \"www.github.com\" :a) t)" 15))))
    (check "NXDOMAIN signals DNS-ERROR"
           (equal "T" (probe "(typep (handler-case (resolve-host \"no-such-name.invalid\") (error (c) c)) 'dns-error)" 15)))
    (check "a second lookup comes from the cache"
           (equal "T" (probe "(and (dns-cache-get \"example.com\" :a) t)")))

    (format t "~&~%== plain HTTP to this host (10.0.2.2:~d) ==~%" *http-port*)
    (flet ((url (p) (format nil "\"http://10.0.2.2:~d~a\"" *http-port* p)))
      (check "Content-Length body"
             (equal "\"hello, len!\"" (row "/len" (probe (format nil "(http-get ~a)" (url "/len")) 15))))
      (check "chunked body, de-chunked"
             (equal (format nil "~s" lol.http-test-server:+chunked-body+)
                    (row "/chunked" (probe (format nil "(http-get ~a)" (url "/chunked")) 15))))
      (check "chunked arrives as 3 pieces through ON-CHUNK"
             (equal "(12 21 5)"
                    (row "/chunked pieces"
                         (probe (format nil "(let ((p '())) (http-request ~a :on-chunk (lambda (v) (push (length v) p))) (reverse p))"
                                        (url "/chunked")) 15))))
      (check "a redirect is followed"
             (equal "200" (probe (format nil "(values (http-request ~a))" (url "/redirect")) 15))))

    (format t "~&~%== HTTPS (the host's libssl from /mnt/host) ==~%")
    (let ((r (row "GET https://example.com/"
                  (probe "(multiple-value-bind (s h b) (http-request \"https://example.com/\") (declare (ignore h)) (list s (and (search \"Example Domain\" b) t)))" 40))))
      (check "HTTPS GET example.com -> 200 with its page" (equal r "(200 T)")))
    (let ((r (row "https://expired.badssl.com/"
                  (probe "(handler-case (progn (http-request \"https://expired.badssl.com/\" :timeout 15) :accepted) (tls-error (c) (if (search \"expired\" (princ-to-string c)) :refused-expired c)) (error (c) (princ-to-string c)))" 40))))
      (check "an expired certificate is refused" (equal r ":REFUSED-EXPIRED")))

    (format t "~&~%== the clock ==~%")
    (row "clock" (probe "(progn (sync-clock) (describe-clock))" 20))
    (check "SNTP answered" (member (probe "(getf *clock-report* :action)") '(":OK" ":STEPPED") :test #'equal))
    (when hostroot
      ;; lol-rootd only exists when PID 1 dropped to the user
      (let ((moved (row "clock after asking lol-rootd for +100 s"
                        (probe "(let ((before (get-universal-time))) (set-system-clock (* 1000 (+ (- before 2208988800) 100))) (- (get-universal-time) before))" 10))))
        (check "lol-rootd steps the clock on request" (let ((n (ignore-errors (parse-integer moved)))) (and n (<= 95 n 105)))))
      (row "and SNTP brings it back" (probe "(progn (sync-clock) (describe-clock))" 20))
      (check "SNTP stepped it back (|offset| was ~100 s)"
             (let ((o (ignore-errors (read-from-string (probe "(getf *clock-report* :offset)")))))
               (and (realp o) (< 90 (abs o) 110)))))

    (format t "~&~%== IPv6 ==~%")
    (row "eth0 v6 addresses" (probe "(mapcar (lambda (a) (getf a :address)) (ipv6-addresses \"eth0\"))"))
    (let ((route (row "v6 default route" (probe "(ipv6-default-route)"))))
      (check "AAAA asked only when a v6 default route exists"
             (equal (probe "(not (null (ipv6-usable-p)))") (if (equal route "NIL") "NIL" "T"))))

    (format t "~&~%== Wi-Fi, on a machine without a radio ==~%")
    (row "wireless interfaces" (probe "(wireless-interfaces)"))
    (check "no wireless interface is reported as such, not an error"
           (equal "NIL" (probe "(wireless-interfaces)")))
    (check "the network view presents without a radio"
           (equal "T" (row "network view" (probe "(and (view-items (present (subj-network))) t)" 10))))
    (check "the systray shows the wired link instead"
           (equal (row "tray item" (probe "(getf (network-tray-item) :text)")) "\"eth0 10.0.2.15\""))
    (when hostroot
      (let ((v (row "host wpa_supplicant -v" (probe "(wpa-supplicant-version)" 15))))
        (check "the host's wpa_supplicant runs from /mnt/host" (search "wpa_supplicant v" v)))
      ;; No radio here, but the supplicant's WIRED driver (802.1X on Ethernet) brings
      ;; up the same control socket — so every request this file's UI makes is real.
      (check "it starts on eth0 (wired driver) with ambient caps, ctrl socket answers"
             (equal "T" (row "start-wpa-supplicant eth0 :driver wired"
                             (probe "(start-wpa-supplicant \"eth0\" :driver \"wired\")" 15))))
      (row "STATUS" (probe "(cdr (assoc \"wpa_state\" (key-value-lines (wpa-request \"STATUS\")) :test #'string=))"))
      (check "ADD_NETWORK + SET_NETWORK (hex SSID, quoted psk) round-trip"
             ;; a non-ASCII SSID, compared IN the guest (the REPL's wire is not UTF-8-clean)
             (equal "T"
                    (row "non-ASCII SSID listed back"
                         (probe "(let ((name (format nil \"Caf~c net\" (code-char 233)))) (add-network name (list :psk \"correct horse\" :security :wpa2) :enable nil) (equal (mapcar (lambda (n) (getf n :ssid)) (supplicant-networks)) (list name)))" 10))))
      (check "an ATTACHed event listener gets events"
             (equal "T" (probe "(progn (start-wifi-events \"eth0\") (sleep 1) (wpa-request \"REASSOCIATE\") (sleep 2) (and *wifi-events* t))" 10)))
      (probe "(stop-wpa-supplicant)" 5)
      (check "...and it stops" (equal "NIL" (probe "(wpa-running-p)")))
      (check "lol-rootd's NM import makes a user-owned /tmp/lol-nm-import"
             (= 1000 (or (probe-int "(progn (request-nm-import) (nth-value 5 (sb-unix:unix-stat \"/tmp/lol-nm-import\")))" 10) -1)))
      (row "NM profiles readable through 9p" (probe "(length *nm-networks*)"))
      (check "a NetworkManager keyfile parses (both SSID spellings)"
             (equal "((\"Home\" \"secretpw1\" :WPA2) (\"Hi\" NIL :OPEN))"
                    (row "parsed keyfiles"
                         (probe "(progn (with-open-file (o \"/tmp/a.nmconnection\" :direction :output :if-exists :supersede) (format o \"[connection]~%id=x~%type=wifi~%[wifi]~%ssid=Home~%[wifi-security]~%key-mgmt=wpa-psk~%psk=secretpw1~%\")) (with-open-file (o \"/tmp/b.nmconnection\" :direction :output :if-exists :supersede) (format o \"[connection]~%type=wifi~%[wifi]~%ssid=72;105;~%\")) (mapcar (lambda (f) (let ((n (parse-nmconnection f))) (list (getf n :ssid) (getf n :psk) (getf n :security)))) (list \"/tmp/a.nmconnection\" \"/tmp/b.nmconnection\")))"))))
      (check "saved credentials are 0600 and read back"
             (equal "(384 \"correct horse\")"
                    (row "save-wifi-credentials"
                         (probe "(let ((p (save-wifi-credentials \"TestNet\" (list :psk \"correct horse\" :security :wpa2)))) (list (logand #o777 (nth-value 3 (sb-unix:unix-stat p))) (getf (wifi-credentials \"TestNet\") :psk)))")))))
    (lol.http-test-server:stop-test-http-server)))

(run-guest-checks "net" #'checks :qmp nil)
