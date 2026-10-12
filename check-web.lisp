;;;; check-web.lisp — assert the WEB PANE in the booted guest: Chrome in a container.
;;;;
;;;;     LOL_GUEST_HOSTROOT=1 LOL_PORT=4015 sbcl --script check-web.lisp   ; exits 0 if all pass
;;;;
;;;; Needs the build host's real root shared into the guest (LOL_GUEST_HOSTROOT=1:
;;;; virtio-9p, read-only, doc/storage.org), because Chrome is the HOST's
;;;; /usr/bin/google-chrome, run inside a container whose root is that Debian
;;;; (lol-ct, doc/containers.org). doc/web-browser.org §8 is what this checks:
;;;;
;;;;   - SuperL-b (a real held Super) opens a web tab with the address prompt;
;;;;   - an address typed over QMP loads a page this script serves on the host
;;;;     (QEMU's user network reaches it at 10.0.2.2), and its title reaches the
;;;;     tab strip;
;;;;   - a real pointer click on a link navigates; SuperL-Left goes back (Alt-Left
;;;;     switches virtual terminals on the console, so it never reaches us);
;;;;   - keys typed into a focused input reach it (read back with Runtime.evaluate);
;;;;   - the wheel scrolls the page;
;;;;   - a public HTTPS page loads, when the guest has a resolver (it is skipped and
;;;;     says so when /etc/resolv.conf is missing — DNS is another track's);
;;;;   - power-off's WEB-SHUTDOWN ends every Chrome process and leaves no zombie.
;;;;
;;;; It also prints what the doc records: Chrome's memory (PSS over its processes,
;;;; read from the guest's /proc), time to first page, input-to-frame latency and
;;;; the JPEG decode cost. LOL_SHOTS=DIR keeps screendumps. LOL_QEMU_MEM raises the
;;;; guest's RAM if 2 GB proves too small.
;;;;
;;;; All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(in-package :lol.guest)

(defun row (label value)
  (format t "~&  ~44a ~a~%" label value)
  (finish-output)
  value)

(defun shot (name)
  (let ((dir (sb-posix:getenv "LOL_SHOTS")))
    (when (and dir (plusp (length dir)))
      (qmp (format nil "{\"execute\":\"screendump\",\"arguments\":{\"filename\":\"~a/~a.ppm\"}}"
                   (string-right-trim "/" dir) name)))))

;;; ---- a tiny HTTP server on the host, for the pages the guest loads ---------------

(defparameter *http-port* (+ 1000 (or (ignore-errors (parse-integer (sb-posix:getenv "LOL_PORT"))) 4005))
  "Where the test pages are served: LOL_PORT + 1000, so two runs never collide.")

(defparameter *pages*
  `(("/index.html" . "<!doctype html><title>LoL test index</title>
<body style='font:20px sans-serif;margin:30px'>
<h1>The web pane, in the guest</h1>
<p><a id=go href='/two.html' style='font-size:40px'>go to page two</a></p>
<p><input id=q style='font-size:20px;width:300px'></p>
<div style='height:3000px;background:linear-gradient(#fff,#88a)'>tall</div></body>")
    ("/two.html" . "<!doctype html><title>LoL page two</title><h1>Page two</h1>")))

(defun serve-one (stream)
  (let* ((line (read-line stream nil ""))
         (path (second (split-sequence #\Space line)))
         (body (cdr (assoc path *pages* :test #'equal))))
    (loop for l = (read-line stream nil "") until (<= (length (string-trim '(#\Return) l)) 0))
    (format stream "HTTP/1.0 ~a~c~%Content-Type: text/html; charset=utf-8~c~%Content-Length: ~d~c~%~c~%~a"
            (if body "200 OK" "404 Not Found") #\Return #\Return
            (length (sb-ext:string-to-octets (or body "") :external-format :utf-8)) #\Return #\Return
            (or body ""))
    (finish-output stream)))

(defun split-sequence (ch s)
  (loop with start = 0 for pos = (position ch s :start start)
        collect (subseq s start pos) while pos do (setf start (1+ pos))))

(defun start-http ()
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
    (sb-bsd-sockets:socket-bind sock #(127 0 0 1) *http-port*)
    (sb-bsd-sockets:socket-listen sock 8)
    (sb-thread:make-thread
     (lambda ()
       (loop (let ((c (sb-bsd-sockets:socket-accept sock)))
               (ignore-errors
                 (let ((s (sb-bsd-sockets:socket-make-stream c :input t :output t
                                                               :external-format :utf-8)))
                   (serve-one s)))
               (ignore-errors (sb-bsd-sockets:socket-close c)))))
     :name "http")))

;;; ---- driving --------------------------------------------------------------------

(defun qmp-move (dx dy)
  (qmp-events (format nil "{\"type\":\"rel\",\"data\":{\"axis\":\"x\",\"value\":~d}}" dx)
              (format nil "{\"type\":\"rel\",\"data\":{\"axis\":\"y\",\"value\":~d}}" dy)))

(defun glide-to (x y)
  "Move the guest's pointer to X,Y with real relative motion, correcting twice (a
   PS/2 mouse has no absolute position to jump to)."
  (dotimes (pass 3)
    (let ((cx (probe-int "*cursor-x*")) (cy (probe-int "*cursor-y*")))
      (when (and cx cy (or (/= cx x) (/= cy y)))
        (dotimes (i 4) (qmp-move (round (- x cx) 4) (round (- y cy) 4)) (sleep 0.15))
        (sleep 0.5)))))

(defun click ()
  (qmp-events "{\"type\":\"btn\",\"data\":{\"down\":true,\"button\":\"left\"}}") (sleep 0.3)
  (qmp-events "{\"type\":\"btn\",\"data\":{\"down\":false,\"button\":\"left\"}}") (sleep 0.8))

(defun type-text (string)
  "Type STRING over QMP, with the shifted characters an address needs."
  (loop for ch across string
        do (case ch
             (#\: (qmp-hold "shift" "semicolon"))
             (#\/ (qmp-key "slash")) (#\. (qmp-key "dot")) (#\- (qmp-key "minus"))
             (#\Space (qmp-key "spc"))
             (t (qmp-key (string-downcase (string ch)))))
           (sleep 0.08)))

(defun js (expr &optional (seconds 15))
  "The visible tab's page evaluates EXPR; its value, printed (\"?\" on any failure)."
  (probe (format nil "(web-js ~s)" expr) seconds))

(defun wait-until (seconds fn)
  (loop repeat (* 2 seconds)
        thereis (funcall fn)
        do (sleep 0.5)))

(defun title () (probe "(let ((tb (web-visible-tab))) (and tb (web-tab-title tb)))"))

(defun element-center (id)
  "The screen point at the middle of element ID, worked out IN the guest from the
   page's own layout (getBoundingClientRect) and the pane's rect."
  (let ((s (probe (format nil "(let* ((r (read-json (coerce (web-js \"JSON.stringify(document.getElementById('~a').getBoundingClientRect())\") 'simple-string)))) (list (round (+ (first *web-page-rect*) (* *web-zoom* (+ (jget r \"x\") (/ (jget r \"width\") 2))))) (round (+ (second *web-page-rect*) (* *web-zoom* (+ (jget r \"y\") (/ (jget r \"height\") 2)))))))" id)
                      15)))
    (ignore-errors (let ((*read-eval* nil)) (read-from-string s)))))

(defun chrome-pss-mb ()
  (probe "(let ((total 0)) (dolist (d (directory \"/proc/*/\")) (ignore-errors (let ((cmd (with-open-file (s (merge-pathnames \"cmdline\" d)) (read-line s nil \"\")))) (when (search \"chrome\" cmd) (with-open-file (s (merge-pathnames \"smaps_rollup\" d)) (loop for l = (read-line s nil) while l when (and (> (length l) 4) (string= \"Pss:\" l :end2 4)) do (incf total (parse-integer l :start 4 :junk-allowed t)))))))) (round total 1024))" 30))

(defun mem-available-mb ()
  (probe "(with-open-file (s \"/proc/meminfo\") (loop for l = (read-line s nil) while l when (search \"MemAvailable\" l) return (round (parse-integer l :start 13 :junk-allowed t) 1024)))"))

;;; ---- the checks -------------------------------------------------------------------

(defun checks ()
  (unless (equal (sb-posix:getenv "LOL_GUEST_HOSTROOT") "1")
    (format t "~&LOL_GUEST_HOSTROOT=1 is not set — Chrome lives on the host's root.~%"))
  (start-http)
  (quiet-kernel-console)
  (wait-for-browser)
  (format t "~&~%== where Chrome comes from ==~%")
  (row "/mnt/host/usr/bin/google-chrome" (probe "(and (probe-file \"/mnt/host/usr/bin/google-chrome\") t)"))
  (row "container launcher loaded" (probe "(fboundp 'container-command-line)"))
  (row "MemAvailable before (MB)" (mem-available-mb))

  (format t "~&~%== SuperL-b, an address, a page ==~%")
  (let ((t0 (get-internal-real-time)))
    (qmp-hold "meta_l" "b") (sleep 1.5)
    (check "SuperL-b opens a web tab" (string= (probe "(and (web-visible-tab) t)") "T"))
    (check "... with the address prompt open" (string= (probe "(and *minibuffer* t)") "T"))
    (type-text (format nil "10.0.2.2:~d/index.html" *http-port*))
    (qmp-key "ret")
    (check "the page loads; its title is read back"
           (wait-until 180 (lambda () (search "LoL test index" (title)))))
    (row "seconds to first page (Chrome cold start)"
         (round (- (get-internal-real-time) t0) internal-time-units-per-second)))
  (row "Chrome state" (probe "*web-chrome-state*"))
  (row "error, if any" (probe "*web-error*"))
  (row "sandbox" (probe "(web-sandbox-p)"))
  (check "a frame arrived and was decoded"
         (wait-until 30 (lambda () (string= (probe "(and (web-tab-frame-px (web-visible-tab)) t)") "T"))))
  (check "the tab strip shows the page title"
         (wait-until 10 (lambda () (search "LoL test index" (probe "(trail-title (current-trail))")))))
  (shot "web-index")

  (format t "~&~%== the pointer and the keys ==~%")
  (let ((xy (element-center "go")))
    (row "link 'go' at" xy)
    (when xy (glide-to (first xy) (second xy)) (click)))
  (check "a real click on a link navigates"
         (wait-until 30 (lambda () (search "LoL page two" (title)))))
  (sleep 1)
  (qmp-hold "meta_l" "left")
  (check "SuperL-Left goes back"
         (wait-until 30 (lambda () (search "LoL test index" (title)))))
  (sleep 1)
  (let ((xy (element-center "q")))
    (when xy (glide-to (first xy) (second xy)) (click)))
  (wait-until 10 (lambda () (search "q" (js "document.activeElement.id"))))
  (type-text "hello web")
  (check "typed keys reach the focused input"
         (wait-until 20 (lambda () (search "hello web" (js "document.getElementById('q').value")))))
  (qmp-wheel "down" 3)
  (check "the wheel scrolls the page"
         (wait-until 20 (lambda () (let ((y (ignore-errors (parse-integer (js "window.scrollY") :junk-allowed t))))
                                     (and y (> y 50))))))
  (shot "web-scrolled")
  (row "input -> frame latencies (ms, newest first)" (probe "(web-tab-latencies (web-visible-tab))"))
  (row "last JPEG decode (ms)" (probe "(web-tab-last-frame-ms (web-visible-tab))"))
  (row "frame size" (probe "(list (web-tab-frame-w (web-visible-tab)) (web-tab-frame-h (web-visible-tab)))"))
  (row "frames received" (probe "(web-tab-frames (web-visible-tab))"))
  (row "Chrome PSS, all its processes (MB)" (chrome-pss-mb))
  (row "MemAvailable with one page (MB)" (mem-available-mb))

  (format t "~&~%== the public web, and the sandbox ==~%")
  ;; No precondition on OUR /etc/resolv.conf: lol-ct gives the container one (ours,
  ;; else the host's), and Chrome resolves inside the container.
  (probe "(web-navigate (web-visible-tab) \"https://example.com/\")")
  (check "a public HTTPS page loads (example.com)"
         (wait-until 60 (lambda () (search "Example Domain" (title)))))
  (shot "web-example")
  ;; chrome://sandbox is Chrome's own report on its renderers' confinement
  (probe "(web-navigate (web-visible-tab) \"chrome://sandbox\")")
  (wait-until 20 (lambda () (search "Sandbox" (title))))
  (sleep 2)
  (let ((text (row "chrome://sandbox says"
                   (js "document.body.innerText.split('\\n').filter(l=>/sandbox|Namespace|Seccomp|Layer/i.test(l)).join(' | ')"))))
    (check "Chrome's own sandbox is on inside the container (chrome://sandbox)"
           (search "adequately sandboxed" text)))

  (format t "~&~%== shutting Chrome down ==~%")
  (probe "(web-shutdown)" 30)
  (sleep 2)
  (check "WEB-SHUTDOWN leaves no Chrome process"
         (string= "0" (probe "(count-if (lambda (d) (ignore-errors (search \"chrome\" (with-open-file (s (merge-pathnames \"cmdline\" d)) (read-line s nil \"\"))))) (directory \"/proc/*/\"))")))
  (check "... and no zombie child of PID 1"
         (string= "0" (probe "(count-if (lambda (d) (ignore-errors (with-open-file (s (merge-pathnames \"stat\" d)) (let ((l (read-line s nil \"\"))) (search \") Z \" l))))) (directory \"/proc/*/\"))"))))

(run-guest-checks "web" #'checks)
