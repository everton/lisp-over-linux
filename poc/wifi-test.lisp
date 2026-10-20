;;;; wifi-test.lisp — host test of wifi.lisp's pure parts.
;;;;
;;;;   sbcl --script poc/wifi-test.lisp
;;;;
;;;; Everything in wifi.lisp that is text in, data out, checked without a radio or a
;;;; supplicant: NetworkManager keyfiles (both SSID spellings, psk-flags agents, open
;;;; networks), the supplicant's escaped SSIDs, the hex SSIDs we send back, the flags
;;;; -> security mapping, passphrase quoting, and the systray item for each state.
;;;; The parts that need a supplicant run in the guest: check-net.lisp.

(require :sb-bsd-sockets)
(defvar *here* (make-pathname :name nil :type nil :defaults *load-truename*))
(load (merge-pathnames "host-logging.lisp" *here*))   ; the modules log (session-log.lisp)
(handler-bind ((warning #'muffle-warning))
  (dolist (f '("process" "net" "dhcp" "dns" "sntp" "tls" "wifi"))
    (load (merge-pathnames (format nil "../initramfs/~a.lisp" f) *here*))))

(defvar *fails* 0)
(defmacro check (label form)
  `(let ((ok (handler-case ,form (error (c) (format t "    (~a)~%" c) nil))))
     (format t "  ~50a ~:[FAIL~;ok~]~%" ,label ok)
     (unless ok (incf *fails*))))

(defun keyfile (text)
  (let ((p (format nil "/tmp/wifi-test-~d.nmconnection" (random 1000000))))
    (with-open-file (o p :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string text o))
    (prog1 (parse-nmconnection p) (delete-file p))))

(defun lines (&rest l) (format nil "~{~a~%~}" l))

(check "NM keyfile: WPA-PSK"
       (equal (keyfile (lines "[connection]" "id=Home" "type=wifi" "[wifi]" "ssid=Home"
                              "[wifi-security]" "key-mgmt=wpa-psk" "psk=secretpw1"))
              '(:ssid "Home" :psk "secretpw1" :security :wpa2 :from :networkmanager)))
(check "NM keyfile: SAE is WPA3"
       (eq :wpa3 (getf (keyfile (lines "[connection]" "type=wifi" "[wifi]" "ssid=X"
                                       "[wifi-security]" "key-mgmt=sae" "psk=12345678")) :security)))
(check "NM keyfile: SSID as a byte list (older NM)"
       (equal "Hi" (getf (keyfile (lines "[connection]" "type=wifi" "[wifi]" "ssid=72;105;")) :ssid)))
(check "NM keyfile: no security section = open"
       (eq :open (getf (keyfile (lines "[connection]" "type=wifi" "[wifi]" "ssid=Cafe")) :security)))
(check "NM keyfile: secret kept by an agent -> nothing to import"
       (null (keyfile (lines "[connection]" "type=wifi" "[wifi]" "ssid=Work" "[wifi-security]"
                             "key-mgmt=wpa-psk" "psk-flags=1"))))
(check "NM keyfile: ethernet profile ignored"
       (null (keyfile (lines "[connection]" "type=ethernet"))))
(check "supplicant SSID escapes decode (\\x, \\\\, \\\")"
       (equal (unescape-ssid "Caf\\xc3\\xa9 \\\"a\\\\b\\\"")
              (format nil "Caf~c \"a\\b\"" (code-char 233))))
(check "hex SSID for SET_NETWORK" (equal (ssid-hex "Hi") "4869"))
(check "flags -> security"
       (equal (mapcar #'security-of '("[WPA2-PSK-CCMP][ESS]" "[WPA2-SAE-CCMP][ESS]" "[WPA2-PSK+SAE-CCMP]"
                                      "[WPA2-EAP-CCMP]" "[ESS]"))
              '(:wpa2 :wpa3 :wpa2/3 :enterprise :open)))
(check "dBm -> percent" (equal (mapcar #'signal-percent '(-40 -50 -75 -100 -110)) '(100 100 50 0 0)))
(check "passphrase quoted, raw 64-hex key bare"
       (and (equal (quote-for-supplicant "pass word") "\"pass word\"")
            (= 64 (length (quote-for-supplicant (make-string 64 :initial-element #\a))))))
(check "tray: connected -> SSID and signal"
       (let ((*wifi-state* '(:iface "wlan0" :state "COMPLETED" :ssid "Home" :signal -60)))
         (equal (getf (network-tray-item) :text) "Home 80%")))
(check "tray: radio, no association -> Not connected!"
       (let ((*wifi-state* '(:iface "wlan0" :state "SCANNING")))
         (equal (getf (network-tray-item) :text) "Not connected!")))
(check "tray: no radio, no lease -> nothing"
       (let ((*wifi-state* nil) (*leases* nil)) (null (network-tray-item))))

(format t "~&~:[ALL PASS~;~:*~d FAILURE~:p~]~%" (and (plusp *fails*) *fails*))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
