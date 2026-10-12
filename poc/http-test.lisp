;;;; http-test.lisp — host test of the network modules: DNS, HTTP framing, HTTPS.
;;;;
;;;;   sbcl --script poc/http-test.lisp            (needs the internet for the last part)
;;;;
;;;; Loads initramfs/{net,dhcp,dns,sntp,tls,http}.lisp into a plain SBCL on the HOST,
;;;; points the resolver at the host's own nameserver (there is no DHCP lease here)
;;;; and TLS at the host's / (instead of /mnt/host), then checks:
;;;;   - every body framing against host-client/http-test-server.lisp on loopback —
;;;;     Content-Length, read-to-close, chunked with an extension and a trailer,
;;;;     streaming through ON-CHUNK (three chunks arrive as three calls), :stream,
;;;;     a relative redirect, a POST body, a UTF-8 charset;
;;;;   - DNS: an A record, a CNAME chased, NXDOMAIN as a DNS-ERROR;
;;;;   - HTTPS to example.com: a 200 through the host's libssl, and a certificate for
;;;;     the wrong name refused (we connect to an address of example.com but ask it to
;;;;     be "wrong.host.invalid" — no CA vouches for that name).
;;;; The same checks run in the guest: check-net.lisp.

(require :sb-bsd-sockets)
(defvar *here* (make-pathname :name nil :type nil :defaults *load-truename*))
(defun rel (p) (merge-pathnames p *here*))

(handler-bind ((warning #'muffle-warning))
  (load (rel "../host-client/http-test-server.lisp"))
  (dolist (f '("net" "dhcp" "dns" "sntp" "tls" "http"))
    (load (rel (format nil "../initramfs/~a.lisp" f)))))

(setf *host-root* "")                       ; the host's own /, not /mnt/host
(setf *dns-servers*                         ; the host's resolver, from its resolv.conf
      (loop for line in (%read-lines "/etc/resolv.conf")
            for f = (%split-ws line)
            when (and (equal (first f) "nameserver") (parse-ip-address (second f))
                      (= 4 (length (parse-ip-address (second f)))))
              collect (parse-ip-address (second f))))

(defvar *fails* 0)
(defmacro check (label form)
  `(let ((ok (handler-case ,form (error (c) (format t "    (~a)~%" c) nil))))
     (format t "  ~50a ~:[FAIL~;ok~]~%" ,label ok)
     (unless ok (incf *fails*))))

(defvar *port* 18731)
(lol.http-test-server:start-test-http-server *port*)
(sleep 0.2)
(defun local (path) (format nil "http://127.0.0.1:~d~a" *port* path))

(format t "~&HTTP framing (loopback test server)~%")
(check "Content-Length body" (equal (http-get (local "/len")) "hello, len!"))
(check "read-until-close body" (equal (http-get (local "/eof")) "until close"))
(check "chunked body, extension + trailer"
       (equal (http-get (local "/chunked")) lol.http-test-server:+chunked-body+))
(check "chunked streams: ON-CHUNK sees 3 pieces"
       (let ((pieces '()))
         (http-request (local "/chunked") :on-chunk (lambda (v) (push (length v) pieces)))
         (equal (reverse pieces) '(12 21 5))))
(check ":want :stream reads the body"
       (multiple-value-bind (st h s) (http-request (local "/chunked") :want :stream)
         (declare (ignore h))
         (unwind-protect
              (let ((v (make-array 100 :element-type '(unsigned-byte 8))))
                (and (= st 200)
                     (equal (map 'string #'code-char (subseq v 0 (read-sequence v s)))
                            lol.http-test-server:+chunked-body+)))
           (close s))))
(check "relative redirect followed"
       (multiple-value-bind (st h body url) (http-request (local "/redirect"))
         (declare (ignore h))
         (and (= st 200) (equal body "hello, len!") (search "/len" url))))
(check "POST body arrives" (equal (http-get (local "/echo") :method :post :body "abc")
                                  "POST 3 abc"))
(check "UTF-8 charset decoded"
       (equal (http-get (local "/utf8")) (format nil "ol~c, ~c" (code-char #xe1) (code-char #x3bb))))
(check "404 is a status, not an error" (= 404 (http-request (local "/nope"))))
(check "nothing listening -> HTTP-ERROR"
       (typep (handler-case (http-request "http://127.0.0.1:1/") (error (c) c)) 'http-error))
(check "URL parsing: [v6]:port, query, fragment"
       (let ((u (parse-url "https://[::1]:8443/a?b=c#frag")))
         (and (equal (url-host u) "::1") (= (url-port u) 8443) (equal (url-path u) "/a?b=c"))))

(format t "~&DNS (host resolver ~{~a~^, ~})~%" (mapcar #'address-string *dns-servers*))
(check "A record for example.com" (= 4 (length (resolve-host "example.com"))))
(check "CNAME chased (www.github.com)" (dns-query "www.github.com" :a))
(check "NXDOMAIN -> DNS-ERROR"
       (typep (handler-case (resolve-host "no-such-name.invalid") (error (c) c)) 'dns-error))

(format t "~&HTTPS via the host's libssl~%")
(check "GET https://example.com/ -> 200"
       (multiple-value-bind (st h body) (http-request "https://example.com/")
         (declare (ignore h))
         (and (= st 200) (search "Example Domain" body))))
;; badssl.com serves deliberately broken certificates; each must be REFUSED, and
;; the refusal must name the reason (OpenSSL's verify result, not just "failed").
(dolist (case '(("wrong.host.badssl.com" "mismatch")
                ("expired.badssl.com" "expired")
                ("self-signed.badssl.com" "self")))
  (destructuring-bind (host why) case
    (check (format nil "~a refused (~a)" host why)
           (let ((c (handler-case (http-request (format nil "https://~a/" host) :timeout 15)
                      (error (c) c))))
             (and (typep c 'tls-error) (search why (string-downcase (princ-to-string c))))))))

(lol.http-test-server:stop-test-http-server)
(format t "~&~:[ALL PASS~;~:*~d FAILURE~:p~]~%" (and (plusp *fails*) *fails*))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
