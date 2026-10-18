;;;; net-views.lisp — the network as a SUBJECT: one view of links, routes, DNS, time
;;;; and Wi-Fi, with keys to act on it. SuperL-n opens it; so does "network" or "wifi"
;;;; in the Spotter.
;;;;
;;;; Same shape as the file manager (file-views.lisp + poc/file-manager.lisp): PRESENT
;;;; describes, SUBJECT-KEY acts, and every action re-presents the pane rather than
;;;; patching it — the view is derived from the machine's state, so derive it again.
;;;; Nothing here blocks the screen for long: a scan, a connect and a lease renewal
;;;; only START things (the supplicant and netd finish them), and `g' shows how far
;;;; they got.

(defstruct (subj-network (:constructor subj-network ())))
(defstruct (subj-wifi-network (:constructor subj-wifi-network (ssid))) ssid)

(defun ago (universal-time)
  (let ((s (- (get-universal-time) universal-time)))
    (cond ((< s 60) (format nil "~ds ago" s))
          ((< s 3600) (format nil "~dm ago" (floor s 60)))
          (t (format nil "~dh~2,'0dm ago" (floor s 3600) (mod (floor s 60) 60))))))

(defun in-time (universal-time)
  (let ((s (- universal-time (get-universal-time))))
    (if (minusp s) "overdue"
        (format nil "in ~dh~2,'0dm" (floor s 3600) (mod (floor s 60) 60)))))

(defun band-of (freq)
  (cond ((null freq) "")
        ((< freq 3000) "2.4 GHz") ((< freq 5900) "5 GHz") (t "6 GHz")))

(defun wifi-network-detail (r)
  (format nil "~3d% ~(~a~) ~a~:[~; · known~]"
          (signal-percent (getf r :signal)) (getf r :security) (band-of (getf r :freq)) (getf r :known)))

(defun wifi-status-line ()
  (let ((w *wifi-state*) (iface (wifi-iface)))
    (cond
      ((null iface) "no wireless interface on this machine (QEMU emulates none)")
      ((null w) (format nil "~a: waiting for netd" iface))
      ((equal (getf w :state) "COMPLETED")
       (format nil "~a: connected to ~a~@[, ~d%~]~@[ (~a dBm)~]~@[ · ~a~]"
               iface (getf w :ssid) (and (getf w :signal) (signal-percent (getf w :signal)))
               (getf w :signal) (band-of (ignore-errors (parse-integer (getf w :freq))))))
      (t (format nil "~a: ~(~a~)" iface (getf w :state))))))

(defmethod present ((s subj-network))
  (let ((items '()))
    (flet ((row (group label &key detail kind subject)
             (push (make-item :label label :detail detail :kind kind :group group :subject subject) items)))
      ;; --- Wi-Fi first: on the laptop it is the only link
      (row "Wi-Fi" (wifi-status-line)
           :detail (let ((b (wifi-blocked))) (and b (format nil "radio blocked (~(~a~))" b))))
      (when *wifi-message* (row "Wi-Fi" *wifi-message* :kind :condition))
      (when (wifi-iface)
        (if *wifi-scan*
            (dolist (r *wifi-scan*)
              (row "Wi-Fi networks — c/ret connect, s scan" (getf r :ssid)
                   :detail (wifi-network-detail r)
                   :kind (if (getf r :known) :class nil)
                   :subject (subj-wifi-network (getf r :ssid))))
            (row "Wi-Fi networks — c/ret connect, s scan" "(no scan results yet — press s)")))
      ;; --- not connected? then what the kernel said about the radio is the answer
      ;;     (doc/networking.org §7.4: these are the lines to send back)
      (unless (equal (getf *wifi-state* :state) "COMPLETED")
        (dolist (m (last (ignore-errors (wifi-kernel-log)) 8))
          (row "kernel log (iwlwifi, cfg80211, firmware)" m)))
      ;; --- the links
      (dolist (i (ignore-errors (interfaces)))
        (unless (eq (getf i :kind) :loopback)
          (row "interfaces" (getf i :name)
               :detail (format nil "~(~a~) · ~a~@[ · ~a~] · ~a"
                               (getf i :kind) (or (getf i :state) "?") (getf i :ipv4) (getf i :mac)))
          (dolist (a (getf i :ipv6))
            (row "interfaces" (format nil "  ~a/~d" (getf a :address) (getf a :prefix))
                 :detail (format nil "IPv6, ~(~a~)" (getf a :scope))))))
      ;; --- the lease and the routes
      (dolist (l *leases*)
        (row "leases" (describe-lease l)
             :detail (if (lease-static l) "static fallback"
                         (format nil "renew ~a, from ~a" (in-time (lease-renew-at l))
                                 (if (lease-server l) (octets->dotted (lease-server l)) "?")))))
      (let ((d4 (ipv4-default-route)) (d6 (ipv6-default-route)))
        (row "routes" (if d4 (format nil "default via ~a dev ~a" (getf d4 :gateway) (getf d4 :iface))
                          "no IPv4 default route — nothing beyond this link")
             :kind (if d4 nil :condition))
        (row "routes" (if d6 (format nil "default via ~a dev ~a (IPv6, from the router's advertisement)"
                                     (getf d6 :gateway) (getf d6 :iface))
                          "no IPv6 default route (AAAA lookups skipped)")))
      ;; --- names and time
      (row "DNS" (format nil "servers: ~{~a~^, ~}" (or (mapcar #'address-string (ignore-errors (dns-servers)))
                                                        '("none")))
           :detail (format nil "~d cached answer~:p" (hash-table-count *dns-cache*)))
      (let ((l (current-lease)))
        (when (and l (lease-domain l)) (row "DNS" (format nil "search domain: ~a" (lease-domain l)))))
      (row "time" (describe-clock))
      ;; --- what netd and the supplicant have been doing
      (loop for (at . what) in (subseq *netd-log* 0 (min 5 (length *netd-log*)))
            do (row "recent" what :detail (ago at)))
      (loop for (at . what) in (subseq *wifi-events* 0 (min 8 (length *wifi-events*)))
            do (row "recent" what :detail (ago at))))
    (make-view :title "network" :kind :list :subject s :items (nreverse items))))

(defmethod present ((s subj-wifi-network))
  (let* ((ssid (subj-wifi-network-ssid s))
         (r (find ssid *wifi-scan* :key (lambda (x) (getf x :ssid)) :test #'equal))
         (creds (wifi-credentials ssid)))
    (make-view
     :title (format nil "Wi-Fi ~a" ssid) :kind :list :subject s
     :items (remove nil
                    (list (make-item :label ssid :detail (if r (wifi-network-detail r) "not in the last scan"))
                          (and r (make-item :label (format nil "BSSID ~a, ~d MHz, ~d dBm" (getf r :bssid)
                                                           (getf r :freq) (getf r :signal))
                                            :detail (getf r :flags)))
                          (make-item :label (cond ((null creds) "not known — c asks for the passphrase")
                                                  ((eq (getf creds :from) :networkmanager)
                                                   "known from the host's NetworkManager")
                                                  (t "saved in ~/.lol/wifi/"))
                                     :kind (if creds :class nil)))))))

(defmethod commands-for ((s subj-wifi-network))
  (let ((ssid (subj-wifi-network-ssid s)))
    (list (make-command :label "Connect" :key "c" :action (lambda () (network-connect-ui ssid)))
          (make-command :label "Forget" :key "f" :action (lambda () (forget-wifi-network ssid))))))

(defmethod commands-for ((s subj-network))
  (list (make-command :label "Scan for Wi-Fi networks" :key "s" :action #'wifi-scan)
        (make-command :label "Disconnect Wi-Fi" :key "d" :action #'wifi-disconnect)
        ;; off the environment's thread, as the keys do (SUBJECT-KEY below): an SNTP
        ;; exchange per server can take seconds, and the screen must not wait
        (make-command :label "Import NetworkManager profiles" :key "i"
                      :action (lambda () (in-background "nm-import" #'request-nm-import)))
        (make-command :label "Check the clock (SNTP)" :key "t"
                      :action (lambda () (in-background "sntp" #'sync-clock)))))

;;; ---- acting: keys on the network view ------------------------------------------

(defun network-connect-ui (ssid)
  "Join SSID; ask for the passphrase in the minibuffer — MASKED — when we do not know
   one and the network is not open."
  (let ((r (find ssid *wifi-scan* :key (lambda (x) (getf x :ssid)) :test #'equal)))
    (if (or (wifi-credentials ssid) (and r (eq (getf r :security) :open)))
        (progn (wifi-connect ssid) (setf *browser-status* (format nil "joining ~a…  (g refreshes)" ssid)))
        (minibuffer-read (format nil "Passphrase for ~a: " ssid)
                         (lambda (pass)
                           (handler-case (progn (wifi-connect ssid pass)
                                                (setf *browser-status* (format nil "joining ~a…  (g refreshes)" ssid)))
                             (error (c) (setf *browser-status* (princ-to-string c)))))
                         :secret t :hint "Enter  Esc cancels  (saved only if it works)"))))

(defun network-pane-p (pane) (subj-network-p (pane-subject pane)))

(defun refresh-network-panes ()
  (dolist (trail *trails*)
    (dolist (p trail)
      (when (or (subj-network-p (pane-subject p)) (subj-wifi-network-p (pane-subject p)))
        (let ((v (ignore-errors (present (pane-subject p)))))
          (when v (setf (pane-view p) v)))))))

(defun selected-ssid (pane)
  (let* ((it (selected-item pane)) (s (and it (item-subject it))))
    (and (subj-wifi-network-p s) (subj-wifi-network-ssid s))))

(defun in-background (label fn)
  "Run FN in its own thread (DHCP, SNTP and a scan take seconds; the screen must not
   wait), reporting failure in *WIFI-MESSAGE*."
  (sb-thread:make-thread (lambda () (handler-case (funcall fn)
                                      (error (c) (setf *wifi-message* (format nil "~a: ~a" label c)))))
                         :name label))

(defmethod subject-key ((s subj-network) pane key state)
  (declare (ignore state))
  (flet ((done (msg) (setf *browser-status* msg) (refresh-network-panes) t))
    (handler-case
        (cond
          ((and (member key '(#\c :return #\Return)) (selected-ssid pane))
           (network-connect-ui (selected-ssid pane)) (refresh-network-panes) t)
          ((eql key #\s) (wifi-scan) (done "scanning… (results in a few seconds: g)"))
          ((eql key #\g) (done "refreshed"))
          ((eql key #\d) (wifi-disconnect) (done "Wi-Fi disconnected"))
          ((and (eql key #\f) (selected-ssid pane))
           (forget-wifi-network (selected-ssid pane)) (done (format nil "forgot ~a" (selected-ssid pane))))
          ((eql key #\i) (in-background "nm-import" #'request-nm-import)
           (done "importing the host's NetworkManager profiles… (g)"))
          ((eql key #\w) (if *wifi-enabled* (wifi-off) (wifi-on))
           (done (if *wifi-enabled* "Wi-Fi on (netd starts it within 5 s)" "Wi-Fi off")))
          ((eql key #\r)
           (let ((l (current-lease)))
             (if l (progn (in-background "renew" (lambda () (dhcp-configure (lease-iface l) :verbose nil)))
                          (done (format nil "asking DHCP again on ~a… (g)" (lease-iface l))))
                 (done "no lease to renew"))))
          ((eql key #\t) (in-background "sntp" #'sync-clock) (done "asking NTP… (g)"))
          (t nil))
      (error (c) (done (princ-to-string c))))))

(defmethod subject-key ((s subj-wifi-network) pane key state)
  (declare (ignore pane state))
  (case key
    ((#\c :return #\Return) (network-connect-ui (subj-wifi-network-ssid s)) t)
    (#\f (forget-wifi-network (subj-wifi-network-ssid s))
     (setf *browser-status* "forgotten") t)
    (t nil)))

(defmethod subject-hints ((s subj-network) pane)
  (declare (ignore pane))
  '(("c/ret" . "connect") ("s" . "scan") ("g" . "refresh") ("d" . "disconnect") ("f" . "forget")
    ("i" . "import NM") ("w" . "Wi-Fi on/off") ("r" . "renew lease") ("t" . "clock")))

(defun open-network ()
  "SuperL-n: the network view — the tab already showing it, else a new one."
  (let ((i (position-if (lambda (trail) (subj-network-p (pane-subject (first trail)))) *trails*)))
    (if i (progn (activate-trail i) (refresh-network-panes))
        (new-trail (subj-network)))))

(defun network-spotter-candidates ()
  (list (list :label "network" :detail "surface · links, routes, DNS, Wi-Fi" :group "surfaces"
              :subject (subj-network))
        (list :label "wifi" :detail "surface · Wi-Fi networks (the network view)" :group "surfaces"
              :subject (subj-network))))

(pushnew 'network-spotter-candidates *spotter-sources*)
