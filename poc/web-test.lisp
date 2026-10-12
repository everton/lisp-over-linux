;;;; web-test.lisp — the web pane against the HOST's Chrome, headless, no QEMU.
;;;;
;;;;   sbcl --script poc/web-test.lisp [-o DIR]
;;;;
;;;; Loads the real stack (as poc/render-frame.lisp does) plus initramfs/web-cdp.lisp
;;;; and initramfs/web.lisp, starts the host's /usr/bin/google-chrome exactly the way
;;;; the image does (headless, DevTools on a pipe), and drives a web tab through the
;;;; SHELL's own entry points — BROWSER-CLICK, BROWSER-KEY, BROWSER-SCROLL — the ones
;;;; the framebuffer loop calls. Every assertion reads the PAGE's own state back over
;;;; CDP (Runtime.evaluate), never pixels; the frames are written as PPMs to DIR
;;;; (default poc/out/web-*.ppm) so a person can look at them too.
;;;;
;;;; The guest twin is check-web.lisp, which does the same inside QEMU with Chrome in
;;;; a container. This one is the fast loop: about ten seconds.
;;;;
;;;; All original code, under this project's MIT licence.

(require :sb-introspect)
(require :sb-posix)

(defparameter *poc-dir* (make-pathname :directory (pathname-directory *load-truename*)))
(defparameter *root-dir* (merge-pathnames "../" *poc-dir*))

(let ((*load-verbose* nil) (*load-print* nil))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "initramfs/registry.lisp" *root-dir*))
    (dolist (m '("initramfs/sbcl-source.lisp" "initramfs/model.lisp" "initramfs/present.lisp"
                 "initramfs/examples.lisp" "initramfs/stage.lisp"
                 "poc/canvas.lisp" "poc/textview.lisp" "poc/widgets.lisp"
                 "initramfs/ansi.lisp" "initramfs/line-editor.lisp" "poc/clipboard.lisp"
                 "initramfs/sysinfo.lisp" "poc/inflate.lisp" "poc/image.lisp"
                 "initramfs/files.lisp" "initramfs/file-views.lisp"
                 "poc/shell.lisp" "poc/file-manager.lisp"
                 "initramfs/web-cdp.lisp" "initramfs/web.lisp"))
      (funcall 'load-recording (merge-pathnames m *root-dir*)))))

(in-package :cl-user)

(defparameter *out*
  (let ((i (position "-o" sb-ext:*posix-argv* :test #'string=)))
    (if i
        (string-right-trim "/" (nth (1+ i) sb-ext:*posix-argv*))
        (namestring (merge-pathnames "out" *poc-dir*)))))

(defparameter *w* 1280)
(defparameter *h* 800)
(defvar *canvas* (lol.canvas:make-canvas *w* *h*))
(defvar *cursor-x* 0 "fb-browser.lisp's pointer, which the wheel reads; not loaded on the host.")
(defvar *cursor-y* 0)
(defvar *results* '())

(defun check (label ok)
  (push (cons label (and ok t)) *results*)
  (format t "~&  ~52a ~a~%" label (if ok "PASS" "FAIL"))
  (finish-output))

(defun write-ppm (path)
  (let* ((px (lol.canvas:canvas-pixels *canvas*))
         (bytes (make-array (* *w* *h* 3) :element-type '(unsigned-byte 8))))
    (dotimes (i (* *w* *h*))
      (let ((v (aref px i)))
        (setf (aref bytes (* 3 i)) (ldb (byte 8 16) v)
              (aref bytes (+ 1 (* 3 i))) (ldb (byte 8 8) v)
              (aref bytes (+ 2 (* 3 i))) (ldb (byte 8 0) v))))
    (ensure-directories-exist path)
    (with-open-file (o path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (write-sequence (sb-ext:string-to-octets (format nil "P6~%~d ~d~%255~%" *w* *h*)) o)
      (write-sequence bytes o))
    (format t "~&  wrote ~a~%" path)))

(defun frame ()
  "What the framebuffer loop does each turn, minus the screen: service, then draw."
  (web-service)
  (draw-browser *canvas* *w* *h*))

(defun pump-until (pred &optional (seconds 15))
  "Run frames until PRED is true. -> its value, or NIL on timeout."
  (loop with deadline = (+ (get-internal-real-time) (* seconds internal-time-units-per-second))
        do (frame)
           (let ((v (ignore-errors (funcall pred)))) (when v (return v)))
           (when (> (get-internal-real-time) deadline) (return nil))
           (when *web-chrome* (cdp-wait-readable *web-chrome* 50))))

(defun tab () (web-visible-tab))
(defun js (expr) (cdp-eval *web-chrome* (web-tab-session (tab)) expr))
(defun title () (ignore-errors (js "document.title")))

(defun element-center (id)
  "Screen coordinates of the middle of element ID in the visible tab."
  (let ((r (read-json (coerce (js (format nil "JSON.stringify(document.getElementById('~a').getBoundingClientRect())" id))
                              'simple-string))))
    (destructuring-bind (px py pw ph) *web-page-rect*
      (declare (ignore pw ph))
      (values (round (+ px (* *web-zoom* (+ (jget r "x") (/ (jget r "width") 2)))))
              (round (+ py (* *web-zoom* (+ (jget r "y") (/ (jget r "height") 2)))))))))

(defun type-string (s) (loop for c across s do (browser-key c 0) (frame)))

;;; ---- the test pages ----------------------------------------------------------

(defparameter *site*
  (format nil "/tmp/lol-web-test-~d/" (sb-posix:getpid)))

(defun page (name html)
  (with-open-file (o (concatenate 'string *site* name) :direction :output :if-exists :supersede
                                                       :external-format :utf-8)
    (write-string html o)))

(defun url (name) (format nil "file://~a~a" *site* name))

(defun make-site ()
  (ensure-directories-exist *site*)
  (page "index.html" "<!doctype html><title>LoL test index</title>
<body style='font:20px sans-serif;margin:30px'>
<h1>The web pane</h1>
<p><a id=go href='two.html' style='font-size:28px'>go to page two</a></p>
<p><a id=pop href='three.html' target=_blank>open page three in a new tab</a></p>
<p><input id=q style='font-size:20px;width:300px'></p>
<p><a id=dl href='data:text/plain,downloaded%20by%20the%20web%20pane' download='notes.txt'>download notes.txt</a> and a needle in the haystack</p>
<p>Ünïcødé — ✓</p>
<div style='height:3000px;background:linear-gradient(#fff,#88a)'>tall</div>
</body>")
  (page "two.html" "<!doctype html><title>LoL page two</title><h1>Page two</h1>")
  (page "three.html" "<!doctype html><title>LoL page three</title><h1>Page three</h1>"))

;;; ---- the run --------------------------------------------------------------------

(defun pss-kb ()
  "Total PSS of every Chrome process using our profile — the honest memory number."
  (let ((total 0))
    (dolist (d (directory "/proc/*/"))
      (ignore-errors
        (let ((cmd (with-open-file (s (merge-pathnames "cmdline" d)) (read-line s nil ""))))
          (when (search "lol-web-test-prof" cmd)
            (with-open-file (s (merge-pathnames "smaps_rollup" d))
              (loop for line = (read-line s nil) while line
                    when (and (> (length line) 4) (string= "Pss:" line :end2 4))
                      do (incf total (parse-integer line :start 4 :junk-allowed t))))))))
    total))

(defun main ()
  (unless (probe-file *web-chrome-binary*)
    (format t "~&no ~a on this host — nothing to test~%" *web-chrome-binary*)
    (sb-ext:exit :code 0))
  (make-site)
  (setf *web-profile* (format nil "/tmp/lol-web-test-prof-~d" (sb-posix:getpid)))
  ;; downloads go to $HOME/Downloads: a scratch HOME, so a test never writes the real one
  (sb-posix:setenv "HOME" (format nil "/tmp/lol-web-test-home-~d" (sb-posix:getpid)) 1)
  (set-browser-font (lol.canvas:load-font (namestring (merge-pathnames "cozette.lolf" *poc-dir*))))
  (open-environment (subj-workspace))
  (format t "~&web pane, host Chrome (~a)~%" *web-chrome-binary*)
  (unwind-protect
       (progn
         (let ((t0 (get-internal-real-time)))
           (web-new-tab (url "index.html"))
           (check "Chrome starts and the tab loads the page"
                  (pump-until (lambda () (equal (title) "LoL test index")) 30))
           (format t "~&  first page in ~,2fs~%"
                   (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
         (check "a frame arrived and was decoded"
                (pump-until (lambda () (web-tab-frame-px (tab)))))
         (check "the tab strip shows the page title"
                (pump-until (lambda () (search "LoL test index" (trail-title (current-trail))))))
         (check "the viewport is the pane (innerWidth = page rect width)"
                (pump-until (lambda () (= (js "innerWidth") (third *web-page-rect*)))))
         (frame) (write-ppm (format nil "~a/web-index.ppm" *out*))
         ;; a click on a link, through the shell
         (multiple-value-bind (x y) (element-center "go")
           (browser-click x y))
         (check "clicking a link navigates"
                (pump-until (lambda () (equal (title) "LoL page two"))))
         (check "back (M-left) returns"
                (progn (pump-until (lambda () (web-can-go-p (tab) -1)))
                       (browser-key :left 8)
                       (pump-until (lambda () (equal (title) "LoL test index")))))
         ;; typing
         (pump-until (lambda () (js "!!document.getElementById('q')")))
         (multiple-value-bind (x y) (element-center "q") (browser-click x y))
         (pump-until (lambda () (equal (js "document.activeElement.id") "q")))
         (type-string "hello, web")
         (check "typing reaches the focused input"
                (pump-until (lambda () (equal (js "document.getElementById('q').value") "hello, web"))))
         (browser-key :backspace 0)
         (check "Backspace edits it"
                (pump-until (lambda () (equal (js "document.getElementById('q').value") "hello, we"))))
         ;; copy: C-a selects the field, C-c copies into OUR clipboard
         (browser-key #\a 4) (frame)
         (browser-key #\c 4)
         (check "C-a C-c copies the field into lol.clipboard"
                (pump-until (lambda () (equal (lol.clipboard:clip-yank) "hello, we"))))
         (lol.clipboard:clip-copy "pasted!")
         (browser-key #\v 4)
         (check "C-v pastes lol.clipboard's newest entry"
                (pump-until (lambda () (equal (js "document.getElementById('q').value") "pasted!"))))
         ;; find in the page: C-f, typed text selects the match
         (browser-key #\f 4)
         (type-string "needle")
         (check "C-f finds as you type (the match is selected)"
                (pump-until (lambda () (equal (js "String(window.getSelection())") "needle"))))
         (browser-key :return 0)
         ;; A download lands in ~/Downloads. The link is a data: URL because a file://
         ;; page's download attribute is IGNORED (a file URL is an opaque origin, so
         ;; Chrome just navigates to the file) — tried first, and that is what it did.
         (multiple-value-bind (x y) (element-center "dl") (browser-click x y))
         (check "a download is saved in ~/Downloads"
                (pump-until (lambda () (probe-file (format nil "~a/notes.txt" (web-downloads-dir))))))
         (check "... and the status line says so"
                (pump-until (lambda () (search "saved in ~/Downloads" (or *browser-status* "")))))
         ;; the wheel
         (setf *cursor-x* 600 *cursor-y* 500)
         (browser-scroll 9)
         (check "the wheel scrolls the page"
                (pump-until (lambda () (> (js "window.scrollY") 100))))
         (frame) (write-ppm (format nil "~a/web-scrolled.ppm" *out*))
         (browser-scroll -30)
         (pump-until (lambda () (zerop (js "window.scrollY"))))
         ;; a target=_blank link: a new trail
         (let ((n (length *trails*)))
           (multiple-value-bind (x y) (element-center "pop") (browser-click x y))
           (check "a target=_blank link opens a new tab (trail)"
                  (pump-until (lambda () (> (length *trails*) n))))
           (check "... showing the new page"
                  (pump-until (lambda () (equal (title) "LoL page three"))))
           (check "... titled in the tab strip, with a frame of its own"
                  (pump-until (lambda () (and (web-tab-frame-px (tab))
                                              (search "LoL page three" (trail-title (current-trail)))))))
           (frame) (write-ppm (format nil "~a/web-popup.ppm" *out*))
           ;; closing that trail closes the Chrome page
           (close-trail)
           (check "closing the trail closes its Chrome page"
                  (pump-until (lambda ()
                                (= 1 (count "page" (jget (cdp-call *web-chrome* "Target.getTargets") "targetInfos")
                                            :key (lambda (ti) (jget ti "type")) :test #'equal))))))
         ;; the address bar: C-l, type, Enter
         (setf *active* (position-if (lambda (tr) (web-tab-of (first tr))) *trails*))
         (browser-key #\l 4)
         (check "C-l opens the address prompt" *minibuffer*)
         (browser-key #\u 4)                  ; clear the line
         (type-string (url "two.html"))
         (browser-key :return 0)
         (check "Enter in the address prompt loads the URL"
                (pump-until (lambda () (equal (title) "LoL page two"))))
         (check "web-url-for: host-ish text gets https, localhost http, words search"
                (and (equal (web-url-for "github.com/x") "https://github.com/x")
                     (equal (web-url-for "localhost:3000") "http://localhost:3000")
                     (equal (web-url-for "10.0.2.2:8000/a") "http://10.0.2.2:8000/a")
                     (search "q=lisp%20machine" (web-url-for "lisp machine"))))
         ;; numbers worth writing down
         (format t "~&  frames received ~d, last decode ~a ms at ~dx~d~%"
                 (web-tab-frames (tab)) (web-tab-last-frame-ms (tab))
                 (web-tab-frame-w (tab)) (web-tab-frame-h (tab)))
         (format t "~&  Chrome PSS, all processes: ~,1f MB~%" (/ (pss-kb) 1024.0)))
    (web-shutdown))
  (let ((ok (every #'cdr *results*)))
    (format t "~&===== ~a (~d checks) =====~%" (if ok "ALL PASS" "FAILURES") (length *results*))
    (sb-ext:exit :code (if ok 0 1))))

(main)
