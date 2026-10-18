;;;; http-test-server.lisp — a tiny HTTP/1.1 server for testing OUR client.
;;;;
;;;; HOST-side. It answers a handful of fixed paths, each built to exercise one way a
;;;; response can be framed, so http.lisp's decoder meets every case on purpose rather
;;;; than whichever one a public site happens to use today:
;;;;
;;;;   /len       Content-Length body
;;;;   /eof       no length at all: the body ends when we close
;;;;   /chunked   Transfer-Encoding: chunked, three chunks with pauses between them
;;;;              (so a streaming client sees them arrive separately), a chunk
;;;;              extension and a trailer header — the corners a lazy decoder misses
;;;;   /redirect  302 -> /len (relative Location)
;;;;   /echo      echoes the method, the request body and its length
;;;;   /utf8      a UTF-8 body with a charset
;;;;   /creds     echoes the credential headers it received (x-api-key, authorization,
;;;;              cookie), so a client's redirect manners can be checked
;;;;   /to?URL    302 -> URL (any origin: the cross-origin redirect)
;;;;   anything else: 404
;;;;
;;;; Used by poc/http-test.lisp (loopback, on the host) and by check-net.lisp (the
;;;; guest reaches the host's loopback at 10.0.2.2, QEMU SLIRP's alias for it).
;;;; Pure sb-bsd-sockets, one thread per connection. All original code, MIT.

(require :sb-bsd-sockets)

(defpackage :lol.http-test-server
  (:use :cl)
  (:export #:start-test-http-server #:stop-test-http-server #:+chunked-body+))
(in-package :lol.http-test-server)

(defparameter +chunked-body+ "first chunk|second, longer chunk|third"
  "What /chunked's three chunks spell when de-chunked correctly.")

(defun crlf (s) (write-char #\Return s) (write-char #\Linefeed s))

(defun read-request (stream)
  "Method, path, headers (alist) and body (a string) of one request."
  (let* ((line (string-right-trim '(#\Return) (or (read-line stream nil) "")))
         (sp1 (position #\Space line)) (sp2 (and sp1 (position #\Space line :start (1+ sp1))))
         (headers (loop for h = (string-right-trim '(#\Return) (or (read-line stream nil) ""))
                        while (plusp (length h))
                        for c = (position #\: h)
                        when c collect (cons (string-downcase (subseq h 0 c))
                                             (string-trim " " (subseq h (1+ c))))))
         (len (let ((cl (cdr (assoc "content-length" headers :test #'string=))))
                (if cl (parse-integer cl) 0)))
         (body (let ((s (make-string len))) (read-sequence s stream) s)))
    (values (and sp1 (subseq line 0 sp1)) (and sp2 (subseq line (1+ sp1) sp2)) headers body)))

(defun respond (s status &key headers body)
  (format s "HTTP/1.1 ~d X" status) (crlf s)
  (dolist (h headers) (format s "~a: ~a" (car h) (cdr h)) (crlf s))
  (format s "Connection: close") (crlf s) (crlf s)
  (when body (write-string body s)))

(defun serve (stream)
  (multiple-value-bind (method path headers body) (read-request stream)
    (cond
      ((equal path "/len")
       (respond stream 200 :headers '(("Content-Length" . "11") ("Content-Type" . "text/plain"))
                           :body "hello, len!"))
      ((equal path "/eof")
       (respond stream 200 :headers '(("Content-Type" . "text/plain")) :body "until close"))
      ((equal path "/chunked")
       (respond stream 200 :headers '(("Transfer-Encoding" . "chunked") ("Content-Type" . "text/plain")))
       (let ((pieces '("first chunk|" "second, longer chunk|" "third")))
         (loop for p in pieces for i from 0
               do (format stream "~x~:[~;;ext=1~]" (length p) (= i 1)) (crlf stream)
                  (write-string p stream) (crlf stream) (force-output stream)
                  (sleep 0.3)))
       (format stream "0") (crlf stream)
       (format stream "X-Trailer: yes") (crlf stream) (crlf stream))
      ((equal path "/redirect")
       (respond stream 302 :headers '(("Location" . "len") ("Content-Length" . "0"))))
      ((equal path "/echo")
       (let ((reply (format nil "~a ~d ~a" method (length body) body)))
         (respond stream 200 :headers (list (cons "Content-Length" (princ-to-string (length reply))))
                             :body reply)))
      ((equal path "/creds")
       (let ((reply (format nil "~{~a~^;~}"
                            (loop for k in '("x-api-key" "authorization" "cookie")
                                  for v = (cdr (assoc k headers :test #'string=))
                                  when v collect (format nil "~a=~a" k v)))))
         (respond stream 200 :headers (list (cons "Content-Length" (princ-to-string (length reply))))
                             :body reply)))
      ((and path (> (length path) 4) (string= "/to?" path :end2 4))
       (respond stream 302 :headers (list (cons "Location" (subseq path 4)) '("Content-Length" . "0"))))
      ((equal path "/utf8")
       ;; built from code points, so this file's own encoding cannot matter
       (let ((octets (sb-ext:string-to-octets (format nil "ol~c, ~c" (code-char #xe1) (code-char #x3bb))
                                              :external-format :utf-8)))
         (respond stream 200 :headers (list (cons "Content-Length" (princ-to-string (length octets)))
                                            (cons "Content-Type" "text/plain; charset=utf-8")))
         (finish-output stream)
         (write-sequence (map 'string #'code-char octets) stream)))
      (t (respond stream 404 :headers '(("Content-Length" . "9")) :body "not found")))
    (finish-output stream)))

(defvar *server* nil)

(defun start-test-http-server (port &key (address #(127 0 0 1)))
  "Listen on ADDRESS:PORT in a background thread. Returns the listening socket."
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address s) t)
    (sb-bsd-sockets:socket-bind s address port)
    (sb-bsd-sockets:socket-listen s 8)
    (setf *server* s)
    (sb-thread:make-thread
     (lambda ()
       (loop
         (let ((c (handler-case (sb-bsd-sockets:socket-accept s) (error () (return)))))
           (sb-thread:make-thread
            (lambda ()
              (let ((stream (sb-bsd-sockets:socket-make-stream
                             c :input t :output t :element-type 'character
                               :external-format :latin-1 :buffering :full)))
                (ignore-errors (serve stream))
                (ignore-errors (close stream))
                (ignore-errors (sb-bsd-sockets:socket-close c))))))))
     :name "test-http-server")
    s))

(defun stop-test-http-server ()
  (when *server* (ignore-errors (sb-bsd-sockets:socket-close *server*)) (setf *server* nil)))
