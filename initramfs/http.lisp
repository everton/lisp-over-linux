;;;; http.lisp — an HTTP/1.1 client: http:// in pure Lisp, https:// over tls.lisp.
;;;;
;;;; HTTP/1.1 (RFC 9112) is text up to the body. We send
;;;;
;;;;     GET /path?query HTTP/1.1          <- request line
;;;;     Host: example.com                 <- headers, one per line, CRLF-terminated
;;;;     Connection: close
;;;;                                       <- an empty line ends the headers
;;;;     [body: exactly Content-Length octets]
;;;;
;;;; and read back a status line ("HTTP/1.1 200 OK"), headers, an empty line, and a
;;;; body framed in ONE of three ways — the part a client must get right:
;;;;
;;;;   Content-Length: N             exactly N octets follow;
;;;;   Transfer-Encoding: chunked    a series of CHUNKS, each "<size in hex>\r\n",
;;;;                                 that many octets, "\r\n" — ending with a chunk of
;;;;                                 size 0 (and optional trailer headers). This is how
;;;;                                 a server streams something whose length it does
;;;;                                 not know yet: server-sent events, an LLM's tokens;
;;;;   neither                       the body runs until the server closes.
;;;;
;;;; Every request says "Connection: close": one request per TCP connection. That
;;;; costs a handshake per request and buys simplicity — no pool, no "is this
;;;; connection still good" — which is the right trade until something measures it.
;;;;
;;;; Public API (the agent track consumes it; package CL-USER):
;;;;
;;;;   (http-request url &key (method :get) headers body (want :string) on-chunk
;;;;                          (timeout 30) connect-timeout (redirects 5))
;;;;     => (values status headers body final-url)
;;;;
;;;;   URL      "http://…" or "https://…" (TLS through tls.lisp; needs /mnt/host)
;;;;   HEADERS  an alist (("Authorization" . "…") …); Host, Connection and
;;;;            Content-Length are ours
;;;;   BODY     a string (sent as UTF-8) or an octet vector, or NIL
;;;;   WANT     :string (decoded per the Content-Type charset, UTF-8 by default),
;;;;            :octets, or :stream — an octet input stream over the body; the caller
;;;;            must CLOSE it, which closes the connection
;;;;   ON-CHUNK a function of one octet vector, called with body data AS IT ARRIVES
;;;;            (de-chunked); then BODY comes back NIL. This is the SSE hook.
;;;;   TIMEOUT  seconds, for the connect and for EACH read — a stream that is quiet
;;;;            longer than this fails with HTTP-ERROR
;;;;   CONNECT-TIMEOUT  seconds for the connect alone, when it should be shorter
;;;;            than TIMEOUT (the agent: 15 s to connect, 300 s between reads)
;;;;   => STATUS 200, HEADERS an alist with lower-case names, in arrival order.
;;;;
;;;; Redirects (301 302 303 307 308) are followed up to REDIRECTS times; a 303 — and,
;;;; as browsers do, a 301/302 answering a POST — turns into a GET without a body.

(define-condition http-error (error)
  ((message :initarg :message :reader http-error-message))
  (:report (lambda (c s) (write-string (http-error-message c) s))))

(defun http-fail (fmt &rest args) (error 'http-error :message (apply #'format nil fmt args)))

;;; ---- URLs -------------------------------------------------------------------

(defstruct (url (:constructor %make-url)) scheme host port path)

(defun parse-url (string)
  "\"https://example.com:8443/a/b?c=d\" -> a URL with scheme :https, host, port and
   path \"/a/b?c=d\" (the part sent on the request line). An IPv6 literal host is
   written in brackets: http://[::1]:8080/."
  (let* ((sep (search "://" string))
         (scheme (and sep (intern (string-upcase (subseq string 0 sep)) :keyword))))
    (unless (member scheme '(:http :https))
      (http-fail "not an http:// or https:// URL: ~s" string))
    (let* ((rest (subseq string (+ sep 3)))
           (slash (or (position-if (lambda (c) (member c '(#\/ #\? #\#))) rest) (length rest)))
           (authority (subseq rest 0 slash))
           (path (subseq rest slash))
           (authority (subseq authority (1+ (or (position #\@ authority) -1)))) ; drop user@
           (bracket (and (plusp (length authority)) (char= (char authority 0) #\[)
                         (position #\] authority)))
           (colon (if bracket
                      (and (< (1+ bracket) (length authority)) (1+ bracket))
                      (position #\: authority)))
           (host (cond (bracket (subseq authority 1 bracket))
                       (colon (subseq authority 0 colon))
                       (t authority)))
           (port (if colon (parse-integer authority :start (1+ colon))
                     (if (eq scheme :https) 443 80))))
      (when (zerop (length host)) (http-fail "no host in ~s" string))
      (let ((hash (position #\# path)))                   ; a #fragment never goes on the wire
        (when hash (setf path (subseq path 0 hash))))
      (%make-url :scheme scheme :host host :port port
                 :path (cond ((zerop (length path)) "/")
                             ((char= (char path 0) #\?) (concatenate 'string "/" path))
                             (t path))))))

(defun url-string (u)
  (format nil "~(~a~)://~:[~a~;[~a]~]~:[:~d~;~*~]~a"
          (url-scheme u) (find #\: (url-host u)) (url-host u)
          (= (url-port u) (if (eq (url-scheme u) :https) 443 80)) (url-port u) (url-path u)))

(defun merge-location (base location)
  "Where a redirect's Location points, relative to BASE (a URL)."
  (cond ((search "://" location) (parse-url location))
        ((and (> (length location) 1) (string= "//" location :end2 2))
         (parse-url (format nil "~(~a~):~a" (url-scheme base) location)))
        ((and (plusp (length location)) (char= (char location 0) #\/))
         (let ((u (copy-url base))) (setf (url-path u) location) u))
        (t (let* ((u (copy-url base))
                  (p (url-path base))
                  (dir (subseq p 0 (1+ (or (position #\/ p :from-end t) 0)))))
             (setf (url-path u) (concatenate 'string dir location))
             u))))

;;; ---- a connection: a socket, maybe wrapped in TLS ---------------------------------

(defstruct conn socket fd tls host)

(defun open-connection (host port tls-p timeout &optional (connect-timeout timeout))
  "Resolve HOST, connect to the first address that answers (DNS order: v4 first,
   dns.lisp says why), and run the TLS handshake if TLS-P. Returns a CONN.
   CONNECT-TIMEOUT bounds connect() alone; TIMEOUT every read and write after it."
  (when tls-p (ensure-openssl))                  ; fail early, before any packet
  (let ((errors '()))
    (dolist (addr (resolve-addresses host))
      (let ((s (make-instance (if (= (length addr) 4)
                                  'sb-bsd-sockets:inet-socket
                                  'sb-bsd-sockets:inet6-socket)
                              :type :stream :protocol :tcp)))
        (handler-case
            (let ((fd (sb-bsd-sockets:socket-file-descriptor s)))
              (set-snd-timeout fd connect-timeout)   ; bounds connect() too (dns.lisp)
              (set-rcv-timeout fd timeout)
              (sb-bsd-sockets:socket-connect s addr port)
              (unless (eql connect-timeout timeout) (set-snd-timeout fd timeout))
              (let ((c (make-conn :socket s :fd fd :host host)))
                (when tls-p
                  (handler-bind ((error (lambda (e) (declare (ignore e))
                                          (ignore-errors (sb-bsd-sockets:socket-close s)))))
                    (setf (conn-tls c) (tls-connect fd host))))
                (return-from open-connection c)))
          (tls-error (e) (error e))             ; a certificate problem: no point retrying
          (error (e)
            (push (format nil "~a: ~a" (address-string addr) e) errors)
            (ignore-errors (sb-bsd-sockets:socket-close s))))))
    (http-fail "cannot connect to ~a:~d~{~%  ~a~}" host port (reverse errors))))

(defun conn-read (c buf start end)
  "Read some octets into BUF; the count, 0 at end of stream."
  (if (conn-tls c)
      (tls-read (conn-tls c) buf start end)
      (sb-sys:with-pinned-objects (buf)
        (multiple-value-bind (n err)
            (sb-unix:unix-read (conn-fd c) (sb-sys:sap+ (sb-sys:vector-sap buf) start) (- end start))
          (cond (n n)
                ((eql err 11) (http-fail "timed out waiting for ~a" (conn-host c)))  ; EAGAIN
                ((eql err 4) (conn-read c buf start end))                           ; EINTR
                (t (http-fail "reading from ~a: ~a" (conn-host c) (sb-int:strerror err))))))))

(defun conn-write (c octets)
  (if (conn-tls c)
      (tls-write (conn-tls c) octets)
      (let ((off 0))
        (sb-sys:with-pinned-objects (octets)
          (loop while (< off (length octets))
                do (multiple-value-bind (n err)
                       (sb-unix:unix-write (conn-fd c) octets off (- (length octets) off))
                     (unless n (http-fail "writing to ~a: ~a" (conn-host c) (sb-int:strerror err)))
                     (incf off n)))))))

(defun conn-close (c)
  (when (conn-tls c) (ignore-errors (tls-close (conn-tls c))))
  (ignore-errors (sb-bsd-sockets:socket-close (conn-socket c))))

;;; ---- buffered reading: lines for the head, octets for the body --------------------

(defstruct (reader (:constructor make-reader (conn)))
  conn
  (buf (make-array 16384 :element-type '(unsigned-byte 8)))
  (pos 0) (end 0))

(defun reader-fill (r)
  "Read once from the connection into the (emptied) buffer. NIL at end of stream."
  (setf (reader-pos r) 0
        (reader-end r) (conn-read (reader-conn r) (reader-buf r) 0 (length (reader-buf r))))
  (plusp (reader-end r)))

(defun reader-line (r)
  "One header line, without its CRLF, as a Latin-1 string (header bytes are octets,
   not UTF-8, by the standard). NIL at end of stream."
  (let ((out (make-array 80 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop
      (when (and (>= (reader-pos r) (reader-end r)) (not (reader-fill r)))
        (return (and (plusp (length out)) (coerce out 'simple-string))))
      (let ((b (aref (reader-buf r) (reader-pos r))))
        (incf (reader-pos r))
        (cond ((= b 10) (return (string-right-trim '(#\Return) (coerce out 'simple-string))))
              ((> (length out) 65536) (http-fail "a header line longer than 64 KB"))
              (t (vector-push-extend (code-char b) out)))))))

(defun reader-take (r max)
  "Up to MAX octets: what is already buffered, else one fresh read. NIL at end."
  (when (and (>= (reader-pos r) (reader-end r)) (not (reader-fill r)))
    (return-from reader-take nil))
  (let* ((n (min max (- (reader-end r) (reader-pos r))))
         (v (subseq (reader-buf r) (reader-pos r) (+ (reader-pos r) n))))
    (incf (reader-pos r) n)
    v))

;;; ---- the response's body as a sequence of pieces ------------------------------------
;;;
;;; Whatever the framing, the body is handed out by a NEXT function: each call returns
;;; the next octet vector, or NIL at the end. :string/:octets collect them, ON-CHUNK
;;; gets each one as it comes, :stream reads from them — one decoder, three consumers.

(defun body-pieces (r headers no-body-p)
  "The NEXT function for this response (see above)."
  (let ((te (header-value headers "transfer-encoding"))
        (cl (header-value headers "content-length")))
    (cond
      (no-body-p (lambda () nil))
      ((and te (search "chunked" (string-downcase te)))
       (let ((left 0) (done nil))                         ; octets left in this chunk
         (lambda ()
           (loop
             (when done (return nil))
             (when (plusp left)
               (let ((v (reader-take r left)))
                 (unless v (http-fail "connection closed inside a chunk"))
                 (decf left (length v))
                 (when (zerop left) (reader-line r))      ; the CRLF after the chunk data
                 (return v)))
             (let* ((line (or (reader-line r) (http-fail "connection closed before the last chunk")))
                    (size (ignore-errors          ; "1a3f;name=value" — extensions after ;
                           (parse-integer line :end (position #\; line) :radix 16))))
               (unless size (http-fail "bad chunk size line ~s" line))
               (if (zerop size)
                   (progn (loop for l = (reader-line r) while (and l (plusp (length l))))  ; trailers
                          (setf done t))
                   (setf left size)))))))
      (cl
       (let ((left (or (ignore-errors (parse-integer cl)) (http-fail "bad Content-Length ~s" cl))))
         (lambda ()
           (when (plusp left)
             (let ((v (reader-take r left)))
               (unless v (http-fail "connection closed with ~d octets of the body missing" left))
               (decf left (length v))
               v)))))
      (t (lambda () (reader-take r most-positive-fixnum))))))   ; until the server closes

(defun header-value (headers name)
  (cdr (assoc name headers :test #'string-equal)))

(defun charset-of (headers)
  "The external format for a text body: the Content-Type's charset, UTF-8 by default."
  (let* ((ct (string-downcase (or (header-value headers "content-type") "")))
         (p (search "charset=" ct))
         (cs (and p (string-trim "\"' " (subseq ct (+ p 8) (position #\; ct :start p))))))
    (cond ((member cs '("iso-8859-1" "latin1" "latin-1" "us-ascii" "ascii") :test #'equal) :latin-1)
          (t :utf-8))))

;;; ---- :want :stream — the body as a Lisp stream -----------------------------------

(defclass http-body-stream (sb-gray:fundamental-binary-input-stream)
  ((next :initarg :next) (conn :initarg :conn)
   (piece :initform nil) (index :initform 0))
  (:documentation "An octet input stream over a response body (WANT :stream). CLOSE
   it to close the connection."))

(defmethod stream-element-type ((s http-body-stream)) '(unsigned-byte 8))

(defun body-stream-piece (s)
  "The current piece with octets left in it, fetching the next one if needed."
  (with-slots (next piece index) s
    (loop while (or (null piece) (>= index (length piece)))
          do (setf piece (funcall next) index 0)
             (unless piece (return-from body-stream-piece nil)))
    piece))

(defmethod sb-gray:stream-read-byte ((s http-body-stream))
  (let ((p (body-stream-piece s)))
    (if p (prog1 (aref p (slot-value s 'index)) (incf (slot-value s 'index))) :eof)))

(defmethod sb-gray:stream-read-sequence ((s http-body-stream) seq &optional (start 0) end)
  (let ((end (or end (length seq))) (i start))
    (loop while (< i end)
          do (let ((p (body-stream-piece s)))
               (unless p (return))
               (with-slots (index) s
                 (let ((n (min (- end i) (- (length p) index))))
                   (replace seq p :start1 i :start2 index :end2 (+ index n))
                   (incf i n) (incf index n)))))
    i))

(defmethod close ((s http-body-stream) &key abort)
  (declare (ignore abort))
  (conn-close (slot-value s 'conn))
  (call-next-method))

;;; ---- one exchange, then the redirect loop ------------------------------------------

(defun request-octets (method url headers body-octets)
  (let ((head (with-output-to-string (o)
                (format o "~a ~a HTTP/1.1~c~c" (string-upcase method) (url-path url) #\Return #\Linefeed)
                (format o "Host: ~:[~a~;[~a]~]~:[:~d~;~*~]~c~c"
                        (find #\: (url-host url)) (url-host url)
                        (= (url-port url) (if (eq (url-scheme url) :https) 443 80)) (url-port url)
                        #\Return #\Linefeed)
                (unless (header-value headers "user-agent")
                  (format o "User-Agent: lisp-over-linux~c~c" #\Return #\Linefeed))
                (unless (header-value headers "accept-encoding")   ; we decode no gzip
                  (format o "Accept-Encoding: identity~c~c" #\Return #\Linefeed))
                (format o "Connection: close~c~c" #\Return #\Linefeed)
                (dolist (h headers)
                  (unless (member (car h) '("host" "connection" "content-length") :test #'string-equal)
                    (format o "~a: ~a~c~c" (car h) (cdr h) #\Return #\Linefeed)))
                (when (or body-octets (member method '(:post :put :patch)))
                  (format o "Content-Length: ~d~c~c" (length body-octets) #\Return #\Linefeed))
                (format o "~c~c" #\Return #\Linefeed))))
    (concatenate '(vector (unsigned-byte 8))
                 (sb-ext:string-to-octets head :external-format :utf-8)
                 (or body-octets #()))))

(defun read-response-head (r)
  "Status code, reason and headers (an alist, lower-case names). 1xx interim answers
   (100 Continue) are skipped."
  (loop
    (let* ((status-line (or (reader-line r) (http-fail "the server closed without answering")))
           (sp (position #\Space status-line))
           (status (and sp (string= "HTTP/" status-line :end2 (min 5 (length status-line)))
                        (ignore-errors (parse-integer status-line :start (1+ sp) :junk-allowed t)))))
      (unless status (http-fail "not an HTTP response: ~s" status-line))
      (let ((headers (loop for line = (reader-line r)
                           while (and line (plusp (length line)))
                           for colon = (position #\: line)
                           when colon
                             collect (cons (string-downcase (subseq line 0 colon))
                                           (string-trim '(#\Space #\Tab) (subseq line (1+ colon)))))))
        (unless (<= 100 status 199)
          (return (values status headers)))))))

(defun http-exchange (url method headers body-octets want on-chunk timeout
                      &optional (connect-timeout timeout))
  "One request on a fresh connection. Returns status, headers and the body as WANT
   asks (or the open stream / NIL for :stream / ON-CHUNK)."
  (let ((c (open-connection (url-host url) (url-port url) (eq (url-scheme url) :https)
                            timeout connect-timeout))
        (keep-open nil))
    (unwind-protect
         (progn
           (conn-write c (request-octets method url headers body-octets))
           (let ((r (make-reader c)))
             (multiple-value-bind (status rheaders) (read-response-head r)
               (let ((next (body-pieces r rheaders (or (eq method :head) (= status 204) (= status 304)))))
                 (values status rheaders
                         (cond
                           ((and (<= 300 status 399) (header-value rheaders "location")) nil) ; redirect
                           (on-chunk (loop for v = (funcall next) while v do (funcall on-chunk v)) nil)
                           ((eq want :stream)
                            (setf keep-open t)
                            (make-instance 'http-body-stream :next next :conn c))
                           (t (let ((all (let ((acc (make-array 0 :element-type '(unsigned-byte 8)
                                                                  :adjustable t :fill-pointer 0)))
                                           (loop for v = (funcall next) while v
                                                 do (loop for b across v do (vector-push-extend b acc)))
                                           (coerce acc '(simple-array (unsigned-byte 8) (*))))))
                                (if (eq want :octets) all
                                    (sb-ext:octets-to-string
                                     all :external-format (list (charset-of rheaders) :replacement #\?)))))))))))
      (unless keep-open (conn-close c)))))

(defun http-request (url &key (method :get) headers body (want :string) on-chunk
                              (timeout 30) connect-timeout (redirects 5))
  "Make an HTTP(S) request; see the file header for every argument.
   => (values status headers body final-url)"
  (let ((u (if (url-p url) url (parse-url url)))
        (method (intern (string-upcase method) :keyword))
        (body-octets (etypecase body
                       (null nil)
                       (string (sb-ext:string-to-octets body :external-format :utf-8))
                       (vector (coerce body '(vector (unsigned-byte 8)))))))
    (loop
      (multiple-value-bind (status rheaders rbody)
          (http-exchange u method headers body-octets want on-chunk timeout
                         (or connect-timeout timeout))
        (let ((location (header-value rheaders "location")))
          (if (and (member status '(301 302 303 307 308)) location (plusp redirects))
              (progn
                (decf redirects)
                (setf u (merge-location u location))
                (when (or (= status 303) (and (member status '(301 302)) (eq method :post)))
                  (setf method :get body-octets nil)))
              (return (values status rheaders rbody (url-string u)))))))))

(defun http-get (url &rest keys)
  "Shorthand: the body of a GET as a string, or HTTP-ERROR unless the status is 2xx."
  (multiple-value-bind (status headers body) (apply #'http-request url keys)
    (declare (ignore headers))
    (unless (<= 200 status 299) (http-fail "GET ~a: HTTP ~d" url status))
    body))
