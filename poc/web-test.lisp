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
(require :sb-bsd-sockets)

(defparameter *poc-dir* (make-pathname :directory (pathname-directory *load-truename*)))
(defparameter *root-dir* (merge-pathnames "../" *poc-dir*))

(let ((*load-verbose* nil) (*load-print* nil))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "poc/host-logging.lisp" *root-dir*))   ; web-cdp.lisp logs (session-log.lisp)
    (load (merge-pathnames "initramfs/registry.lisp" *root-dir*))
    (dolist (m '("initramfs/lol-home.lisp" "initramfs/sbcl-source.lisp" "initramfs/model.lisp" "initramfs/present.lisp"
                 "initramfs/examples.lisp" "initramfs/stage.lisp"
                 "poc/ttf.lisp" "poc/canvas.lisp" "poc/textview.lisp" "poc/widgets.lisp" "poc/morph.lisp"
                 "initramfs/ansi.lisp" "initramfs/line-editor.lisp" "poc/clipboard.lisp"
                 "initramfs/sysinfo.lisp" "poc/inflate.lisp" "poc/image.lisp"
                 "initramfs/files.lisp" "initramfs/file-views.lisp"
                 "poc/shell.lisp" "poc/file-manager.lisp" "initramfs/runs.lisp"
                 "poc/morphic.lisp"     ; the desk: what is on screen is morphs in a world
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
  "What the framebuffer loop does each turn, minus the screen: service, then repair
   the world (doc/morphic.org: the panes are morphs in it)."
  (web-service)
  (lol.morph:world-repair *world* *canvas*))

(defun browser-click (x y)
  "A left click at the screen point X,Y, as the loop routes one: through the world, to
   the morph under it (a web page's pane, its toolbar)."
  (lol.morph:world-dispatch *world* (lol.morph:make-event :kind :down :x x :y y))
  (lol.morph:world-dispatch *world* (lol.morph:make-event :kind :up :x x :y y)))

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
    (destructuring-bind (px py pw ph) (web-tab-page-rect (tab))
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
  ;; the pickers (doc/web-browser.org §4.4): a <select> with groups and a disabled
  ;; option whose onchange retitles the page, a datalist, a colour and a date
  (page "pick.html" "<!doctype html><title>LoL pickers</title>
<body style='font:20px sans-serif;margin:30px'>
<p><select id=s style='font-size:20px' onchange='window.changes=(window.changes||0)+1;document.title=\"picked \"+this.value'>
<optgroup label=Fruit><option value=a>Apple<option value=b>Banana</optgroup>
<optgroup label=Roots><option value=c disabled>Carrot<option value=d>Daikon</optgroup></select></p>
<p><input id=dl list=L style='font-size:20px'><datalist id=L><option value=one><option value=two><option value=three></datalist></p>
<p><input id=co type=color> <input id=dt type=date style='font-size:20px'></p></body>")
  (page "three.html" "<!doctype html><title>LoL page three</title><h1>Page three</h1>"))

;;; ---- calls: the microphone, the camera, the shared screen (§4.8) ------------------
;;; A file: page has no origin, and nothing can be granted to "null", so the call page
;;; is served over http from 127.0.0.1 — a loopback address is a secure context, as
;;; an https page is.

(defparameter +call-page+ "<!doctype html><title>LoL call</title>
<body style='margin:0;background:#222'>
<video id=v muted autoplay playsinline style='width:320px;height:200px'></video>
<script>
window.R = {}; window.TICKS = 0; setInterval(function(){ TICKS++; }, 100);
function run(name, p){ R[name] = 'waiting';
  p.then(function(s){ window[name] = s; R[name] = 'ok'; },
         function(e){ R[name] = 'rej ' + e.name; }); return 1; }
/* the shared screen as a viewer sees it: the stream's newest frame, one pixel */
function pixel(x, y){
  var v = document.getElementById('v'), c = document.createElement('canvas');
  c.width = v.videoWidth; c.height = v.videoHeight;
  if (!c.width) return 'no video';
  var g = c.getContext('2d'); g.drawImage(v, 0, 0);
  var d = g.getImageData(x, y, 1, 1).data; return [d[0], d[1], d[2]].join(',');
}
</script>")

(defun serve-call-page ()
  "A one-page HTTP server on 127.0.0.1 -> (values port socket)."
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address s) t)
    (sb-bsd-sockets:socket-bind s #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen s 8)
    (sb-thread:make-thread
     (lambda ()
       (loop
         (let ((c (ignore-errors (sb-bsd-sockets:socket-accept s))))
           (unless c (return))
           (ignore-errors
             (let ((io (sb-bsd-sockets:socket-make-stream c :input t :output t :element-type :default
                                                            :external-format :utf-8)))
               (loop for l = (read-line io nil "") while (> (length (string-trim '(#\Return) l)) 0))
               (let ((body (sb-ext:string-to-octets +call-page+ :external-format :utf-8)))
                 (format io "HTTP/1.1 200 OK~c~cContent-Type: text/html; charset=utf-8~c~cContent-Length: ~d~c~cConnection: close~c~c~c~c"
                         #\Return #\Newline #\Return #\Newline (length body) #\Return #\Newline
                         #\Return #\Newline #\Return #\Newline)
                 (finish-output io)
                 (write-sequence (map 'string #'code-char body) io))
               (finish-output io)))
           (ignore-errors (sb-bsd-sockets:socket-close c)))))
     :name "call page")
    (values (nth-value 1 (sb-bsd-sockets:socket-name s)) s)))

(defun serve-slow-page (seconds)
  "An HTTP server on 127.0.0.1 whose /slow page answers after SECONDS, its / page
   linking there at once -> the port. For the loading strip (doc/web-browser.org §4.9)."
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address s) t)
    (sb-bsd-sockets:socket-bind s #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen s 8)
    (sb-thread:make-thread
     (lambda ()
       (loop
         (let ((c (ignore-errors (sb-bsd-sockets:socket-accept s))))
           (unless c (return))
           (let ((c c))
             (sb-thread:make-thread
              (lambda ()
                (ignore-errors
                 (let* ((io (sb-bsd-sockets:socket-make-stream c :input t :output t
                                                                 :element-type :default))
                        (req (read-line io nil "")))
                   (loop for l = (read-line io nil "")
                         while (> (length (string-trim '(#\Return) l)) 0))
                   (let ((body (if (search "/slow" req)
                                   (progn (sleep seconds) "<title>LoL slow</title><h1>slow</h1>")
                                   "<title>LoL fast</title><a id=go href='/slow' style='font-size:40px'>slow</a>")))
                     (format io "HTTP/1.1 200 OK~c~cContent-Type: text/html~c~cContent-Length: ~d~c~cConnection: close~c~c~c~c~a"
                             #\Return #\Newline #\Return #\Newline (length body) #\Return #\Newline
                             #\Return #\Newline #\Return #\Newline body)
                     (finish-output io))))
                (ignore-errors (sb-bsd-sockets:socket-close c)))
              :name "slow page")))))
       :name "slow server")
    (nth-value 1 (sb-bsd-sockets:socket-name s))))

(defun strip-bar ()
  "Where the visible tab's loading strip is lit: (first last) columns in the accent,
   or NIL when it is all background."
  (destructuring-bind (x y w h) (web-tab-progress-rect (tab))
    (declare (ignore h))
    (let ((px (lol.canvas:canvas-pixels *canvas*)) (lit '()))
      (dotimes (i w)
        (when (= (aref px (+ (* y *w*) x i)) *ui-accent*) (push i lit)))
      (and lit (list (car (last lit)) (first lit))))))

(defun loop-turn ()
  "What fb-browser.lisp's loop does with the web service each turn: WEB-LOOP-SERVICE
   (which paints a frame or the loading strip by itself), then the world's repair."
  (web-loop-service *canvas* nil)
  (lol.morph:world-repair *world* *canvas*)
  (when *web-chrome* (cdp-wait-readable *web-chrome* 20)))

(defun loading-checks ()
  (let* ((port (serve-slow-page 2)))
    (web-navigate (tab) (format nil "http://127.0.0.1:~d/" port))
    (pump-until (lambda () (and (equal (title) "LoL fast") (not (web-tab-loading (tab))))))
    (check "a loaded page's strip is still (background only)"
           (progn (frame) (null (strip-bar))))
    (multiple-value-bind (x y) (element-center "go") (browser-click x y))
    (let ((t0 (get-internal-real-time)))
      (check "a click on a slow link: loading within 200 ms, the strip lit"
             (and (pump-until (lambda () (web-tab-loading (tab))) 2)
                  (< (- (get-internal-real-time) t0) (* 0.2 internal-time-units-per-second))
                  (progn (loop-turn) (strip-bar)))))
    (let ((a (strip-bar)))
      (loop repeat 15 do (loop-turn) (sleep 0.02))
      (check "... and its bar moves while the server has not answered"
             (and (web-tab-loading (tab)) (strip-bar) (not (equal a (strip-bar))))))
    (write-ppm (format nil "~a/web-loading.ppm" *out*))
    (check "the timeout wakes the loop for it (50 ms)" (<= 1 (web-timeout) 50))
    (check "loaded: the strip is still again"
           (and (pump-until (lambda () (and (equal (title) "LoL slow")
                                            (not (web-tab-loading (tab))))) 10)
                (progn (frame) (null (strip-bar)))))))

(defun fill-rect (canvas x y w h colour)
  (let ((px (lol.canvas:canvas-pixels canvas)) (cw (lol.canvas:canvas-w canvas)))
    (loop for j from y below (+ y h) do
      (loop for i from x below (+ x w) do (setf (aref px (+ (* j cw) i)) colour)))))

(defun near-rgb-p (text r g b)
  "TEXT (\"r,g,b\" from the page) within a video codec's rounding of R G B."
  (let ((v (ignore-errors (mapcar #'parse-integer (split-commas text)))))
    (and (= (length v) 3)
         (every (lambda (a e) (<= (abs (- a e)) 12)) v (list r g b)))))

(defun split-commas (s)
  (loop with start = 0 for i = (position #\, s :start start)
        collect (subseq s start i) while i do (setf start (1+ i))))

(defun answer-prompt (needle key)
  "Wait for the minibuffer to ask with NEEDLE in its prompt, then press KEY. -> T."
  (and (pump-until (lambda () (and *minibuffer* (search needle (mb-prompt *minibuffer*)))) 10)
       (progn (browser-key key 0) (frame) t)))

(defun call-checks ()
  (multiple-value-bind (port sock) (serve-call-page)
    (let* ((share (lol.canvas:make-canvas 320 200))
           (origin (format nil "http://127.0.0.1:~d" port))
           (*web-share-canvas* (lambda () share)))
      (fill-rect share 0 0 320 200 #xFF8000)
      (web-new-tab (format nil "~a/call.html" origin))
      (check "calls: the call page loads from 127.0.0.1 (a secure context)"
             (pump-until (lambda () (and (equal (title) "LoL call") (eq (js "isSecureContext") t)))))
      (let ((call (tab)))
        ;; the microphone: asked in the minibuffer, then Chrome's own call goes through
        (js "run('mic', navigator.mediaDevices.getUserMedia({audio: true}))")
        (check "calls: getUserMedia asks in the minibuffer (\"wants your microphone\")"
               (answer-prompt "wants your microphone" #\y))
        (check "calls: y grants the origin: the page's permission says granted"
               (pump-until (lambda () (equal (js "navigator.permissions.query({name:'microphone'}).then(p => p.state)")
                                             "granted"))))
        (check "calls: ... and the page's call is not refused (no NotAllowedError)"
               (let ((r (pump-until (lambda () (let ((r (js "R.mic"))) (and (not (equal r "waiting")) r))))))
                 (format t "~&    (getUserMedia: ~a)~%" r)
                 (and r (not (search "NotAllowed" r)))))
        (check "calls: what the page did is in session.log (the script, the call, its tracks)"
               (pump-until
                (lambda ()
                  (let ((log (ignore-errors
                              (with-open-file (i (format nil "~asession.log" (host-log-dir)))
                                (let ((s (make-string (file-length i))))
                                  (subseq s 0 (read-sequence s i)))))))
                    (and log (search "web: page" log) (search "(in http://127.0.0.1" log)
                         (not (search "index.html" log))   ; a page that never asked
                         (search "getUserMedia {\"audio\":true}" log)
                         (search "getUserMedia ok: audio" log))))))
        ;; the prototype's function, as a page may call it, is asked too
        (js "run('mic3', MediaDevices.prototype.getUserMedia.call(navigator.mediaDevices, {audio: true}))")
        (check "calls: MediaDevices.prototype.getUserMedia goes through us (granted: no ask)"
               (equal (pump-until (lambda () (let ((r (js "R.mic3"))) (and (not (equal r "waiting")) r))))
                      "ok"))
        (js "window.mic3 && mic3.getTracks().forEach(t => t.stop())")
        (js "run('mic2', navigator.mediaDevices.getUserMedia({audio: true}))")
        (check "calls: a second ask of a granted origin is not asked again"
               (pump-until (lambda () (and (not (equal (js "R.mic2") "waiting")) (null *minibuffer*)))))
        (js "window.mic && mic.getTracks().forEach(t => t.stop()); window.mic2 && mic2.getTracks().forEach(t => t.stop())")
        (js "run('cam', navigator.mediaDevices.getUserMedia({video: true}))")
        (check "calls: n refuses the camera: NotAllowedError"
               (and (answer-prompt "wants your camera" #\n)
                    (pump-until (lambda () (equal (js "R.cam") "rej NotAllowedError")))))
        ;; asked again the way Meet asks: with pan, tilt and zoom. A grant naming a
        ;; permission Chrome does not know is refused WHOLE, and the camera with PTZ
        ;; stayed denied while the plain camera said granted — Meet then kept asking
        (js "run('ptz', navigator.mediaDevices.getUserMedia({video: {pan: true, tilt: true, zoom: true}}))")
        (check "calls: y to a pan/tilt/zoom camera: not refused (no NotAllowedError)"
               (and (answer-prompt "wants your camera" #\y)
                    (let ((r (pump-until (lambda () (let ((r (js "R.ptz"))) (and (not (equal r "waiting")) r))))))
                      (and r (not (search "NotAllowed" r))))))
        (check "calls: ... the camera granted with and without panTiltZoom"
               (pump-until
                (lambda ()
                  (equal (js "Promise.all([navigator.permissions.query({name:'camera'}),
                                           navigator.permissions.query({name:'camera', panTiltZoom: true})])
                                .then(ps => ps.map(p => p.state).join(' '))")
                         "granted granted"))))
        (js "window.ptz && ptz.getTracks().forEach(t => t.stop())")
        (check "calls: the tab in a call is focused (timers at full speed)"
               (pump-until (lambda () (eq (js "document.hasFocus()") t))))
        ;; the screen
        (js "run('scr', navigator.mediaDevices.getDisplayMedia({video: true}))")
        (check "calls: getDisplayMedia asks (\"wants to see your screen\")"
               (answer-prompt "wants to see your screen" #\y))
        (check "calls: ... and gets a stream the screen's size, a monitor"
               (pump-until (lambda ()
                             (and (equal (js "R.scr") "ok")
                                  (equal (js "(s => s.width + 'x' + s.height + ' ' + s.displaySurface)(scr.getVideoTracks()[0].getSettings())")
                                         "320x200 monitor")))))
        (check "calls: the tray says SHARING"
               (search "SHARING 127.0.0.1" (getf (web-call-tray-item) :text)))
        (js "document.getElementById('v').srcObject = scr")
        (check "calls: a viewer of the stream sees our screen (orange)"
               (pump-until (lambda () (near-rgb-p (js "pixel(160, 100)") 255 128 0))))
        ;; a changed rect goes alone
        (fill-rect share 40 30 100 60 #x0080FF)
        (web-share-note (list (list 40 30 100 60)) nil)
        (check "calls: a repaired rect reaches the viewer (blue there, orange beside)"
               (pump-until (lambda () (and (near-rgb-p (js "pixel(90, 60)") 0 128 255)
                                           (near-rgb-p (js "pixel(200, 150)") 255 128 0)))))
        (format t "~&    (shared: ~d updates, ~d octets, the last in ~d ms)~%"
                (web-share-updates *web-share*) (web-share-octets *web-share*)
                (web-share-last-ms *web-share*))
        ;; the call goes on with another tab on screen
        (web-new-tab (url "two.html"))
        (pump-until (lambda () (equal (title) "LoL page two")))
        (let ((t0 (cdp-eval *web-chrome* (web-tab-session call) "TICKS"))
              (u0 (web-share-updates *web-share*)))
          (fill-rect share 200 120 60 40 #x00C000)
          (web-share-note (list (list 200 120 60 40)) nil)
          (loop repeat 40 do (frame) (cdp-wait-readable *web-chrome* 50))
          (let ((ticks (- (cdp-eval *web-chrome* (web-tab-session call) "TICKS") t0)))
            (check "calls: in the background, the call's timers still run (focus emulation)"
                   (> ticks 10))
            (check "calls: ... and the shared screen's updates are still taken"
                   (and (> (web-share-updates *web-share*) u0) (not (web-share-busy *web-share*))))
            (format t "~&    (background: ~d ticks of 100 ms in about 2 s)~%" ticks)))
        ;; stopping, from either side
        (cdp-eval *web-chrome* (web-tab-session call) "scr.getVideoTracks()[0].stop()")
        (check "calls: the page stopping its track stops the sharing"
               (pump-until (lambda () (null *web-share*))))
        (cdp-eval *web-chrome* (web-tab-session call)
                  "run('scr2', navigator.mediaDevices.getDisplayMedia({video: true}))")
        (answer-prompt "wants to see your screen" #\y)
        (pump-until (lambda () (equal (cdp-eval *web-chrome* (web-tab-session call) "R.scr2") "ok")))
        (cdp-eval *web-chrome* (web-tab-session call)
                  "scr2.getVideoTracks()[0].onended = function(){ R.ended = 'yes'; }")
        (web-share-stop)
        (check "calls: stopping it here ends the page's track (its ended event fires)"
               (pump-until (lambda () (equal (cdp-eval *web-chrome* (web-tab-session call) "R.ended") "yes"))))
        (check "calls: the encoder: #x123456 is base64 \"EjRW\", four octets a pixel"
               (let* ((c (lol.canvas:make-canvas 2 1)))
                 (setf (aref (lol.canvas:canvas-pixels c) 0) #x123456
                       (aref (lol.canvas:canvas-pixels c) 1) #xFFFFFF)
                 (let ((m (map 'string #'code-char (web-share-message 7 "S" nil (list (list 0 0 2 1)) c -1 -1))))
                   (and (search "[[0,0,2,1,'EjRW////']],-1,-1)" m)
                        (char= (char m (1- (length m))) (code-char 0))
                        (read-json (subseq m 0 (1- (length m))))))))
        ;; what a whole laptop screen costs: encode + write, then the page's answer
        (let ((big (lol.canvas:make-canvas 1920 1200)))
          (fill-rect big 0 0 1920 1200 #x336699)
          (let ((*web-share-canvas* (lambda () big)))
            (cdp-eval *web-chrome* (web-tab-session call)
                      "run('scr3', navigator.mediaDevices.getDisplayMedia({video: true}))")
            (answer-prompt "wants to see your screen" #\y)
            (pump-until (lambda () (and *web-share* (web-share-busy *web-share*))))
            (let ((t0 (get-internal-real-time)) (o0 (web-share-octets *web-share*))
                  (u0 (web-share-updates *web-share*)))
              ;; a whole screen goes as bands of at most +WEB-SHARE-MAX-PIXELS+ (1 MB
              ;; each): the laptop's first shared call lost Chrome to one 8.8 MB write
              (check "calls: a whole 1920x1200 screen arrives, in 1 MB bands (the page answers ok)"
                     (pump-until (lambda () (and *web-share* (not (web-share-busy *web-share*))
                                                 (null (web-share-damage *web-share*))
                                                 (not (web-share-full *web-share*))))
                                 30))
              ;; (the first band left before U0 was read: 8 more of the 9)
              (check "calls: ... in 8 more updates, none bigger than 1.1 MB"
                     (and (< (web-share-last-ms *web-share*) 1000)
                          (>= (- (web-share-updates *web-share*) u0) 8)
                          (< (/ (- (web-share-octets *web-share*) o0) (- (web-share-updates *web-share*) u0))
                             (* 1.1 1048576))))
              (format t "~&    (1920x1200: ~,1f MB in ~d updates, the last encoded + written in ~d ms; all there after ~d ms)~%"
                      (/ (- (web-share-octets *web-share*) o0) 1048576.0)
                      (- (web-share-updates *web-share*) u0) (web-share-last-ms *web-share*)
                      (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second)))
            (web-share-stop)))
        ;; a site whose camera and microphone are BLOCKED: Meet's case, a block
        ;; stored in Chrome's profile. The page reads "denied" and never calls
        ;; getUserMedia, so the first denied query must ask us. Chrome is made to
        ;; answer denied with Browser.setPermission (what the page sees is the same;
        ;; that our grant overrides a block really stored in the profile was measured
        ;; separately, doc/web-browser.org §4.8 M2)
        (flet ((call-js (e) (cdp-eval *web-chrome* (web-tab-session call) e)))
          (cdp-send *web-chrome* "Browser.resetPermissions" (jobj))
          (setf *web-grants* '())
          (dolist (name '("camera" "microphone"))
            (cdp-send *web-chrome* "Browser.setPermission"
                      (jobj "permission" (jobj "name" name) "setting" "denied" "origin" origin)))
          (pump-until (lambda () (equal (call-js "navigator.permissions.query({name:'camera'}).then(p => p.state)")
                                        "denied")))
          (call-js "navigator.permissions.query({name:'microphone'}).then(p => window.Q1 = p.state); 1")
          ;; Meet's laptop try: the question was up, the user clicked Meet's own
          ;; "allow" button, and the press dismissed our question (any press ended
          ;; the minibuffer). A page's question is STICKY now: only a key answers it
          (check "calls: the blocked site's question survives a press (sticky), the page's foot pointing at it"
                 (and (pump-until (lambda () (and *minibuffer* (search "blocked" (mb-prompt *minibuffer*)))))
                      (progn (press-prelude) (frame) t)   ; what every press does first
                      *minibuffer* (search "blocked" (mb-prompt *minibuffer*))
                      (equal (web-tab-status call) +web-asking-note+)))
          (check "calls: a site with its camera and microphone blocked is asked about (\"blocked\")"
                 (answer-prompt "has your microphone and camera blocked" #\y))
          (check "calls: ... answered: the page's foot is clear again"
                 (null (web-tab-status call)))
          (check "calls: ... y: the page's query answers granted (the block overridden)"
                 (pump-until (lambda () (equal (call-js "window.Q1") "granted"))))
          (call-js "run('unb', MediaDevices.prototype.getUserMedia.call(navigator.mediaDevices, {audio: true}))")
          (check "calls: ... and getUserMedia goes through without asking again"
                 (equal (pump-until (lambda () (let ((r (call-js "R.unb"))) (and (not (equal r "waiting")) r))))
                        "ok"))
          (call-js "window.unb && unb.getTracks().forEach(t => t.stop()); 1"))))
    (ignore-errors (sb-bsd-sockets:socket-close sock))))

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
  (setf *world* (make-desk *w* *h*))
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
                (pump-until (lambda () (= (js "innerWidth") (third (web-tab-page-rect (tab)))))))
         ;; what Google's sign-in and search read as a robot (doc/web-browser.org §4.5)
         (check "the page is not told it is automated (navigator.webdriver false)"
                (eq (js "navigator.webdriver") :false))
         (check "the screen and the window hold the page (none smaller, as headless says)"
                (eq (js "screen.width >= outerWidth && outerWidth >= innerWidth
                         && screen.height >= outerHeight && outerHeight >= innerHeight
                         && screen.width > 800") t))
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
         ;; ---- the pickers: what Chrome draws where the screencast cannot see ----
         (web-navigate (tab) (url "pick.html"))
         (check "the picker page loads"
                (pump-until (lambda () (equal (title) "LoL pickers"))))
         (check "the page script is installed (window.__lolPickerApply)"
                (pump-until (lambda () (equal (js "typeof window.__lolPickerApply") "function"))))
         (let* ((calls '()) (answer nil)
                (*chooser* (lambda (kind entries x y &optional layers)
                             (declare (ignore layers))
                             (push (list kind entries x y) calls)
                             (and answer (find answer entries :key (lambda (e) (getf e :label))
                                                              :test #'equal)))))
           (setf answer "Daikon")
           (multiple-value-bind (x y) (element-center "s") (browser-click x y))
           (check "a click on a <select> opens OUR chooser (:select)"
                  (pump-until (lambda () (and calls (eq (first (first calls)) :select)))))
           (destructuring-bind (&optional kind entries cx cy) (first calls)
             (declare (ignore kind))
             (check "... with every option, the optgroups as sections"
                    (and (equal (mapcar (lambda (e) (getf e :label)) entries)
                                '("Apple" "Banana" "Carrot" "Daikon"))
                         (equal (mapcar (lambda (e) (getf e :group)) entries)
                                '("Fruit" "Fruit" "Roots" "Roots"))))
             (check "... Carrot disabled, Apple the current one"
                    (and (getf (third entries) :disabled) (getf (first entries) :selected)
                         (notany (lambda (e) (getf e :disabled)) (list (first entries) (second entries) (fourth entries)))))
             (multiple-value-bind (sx sy) (element-center "s")
               (check "... placed under the control, at its left edge"
                      (and cx cy (< (abs (- cy (+ sy 12))) 14) (< (- sx cx) 200) (> cy sy)))))
           (check "Chrome's own popup did NOT open (the select is not :open)"
                  (equal (js "document.getElementById('s').matches(':open')") :false))
           (check "choosing Daikon sets the value and fires change (the title)"
                  (pump-until (lambda () (and (equal (js "document.getElementById('s').value") "d")
                                              (equal (title) "picked d")))))
           (setf answer nil calls '())
           (multiple-value-bind (x y) (element-center "s") (browser-click x y))
           (pump-until (lambda () calls))
           (pump-until (lambda () nil) 1)
           (check "Esc (no choice) leaves the value and fires nothing"
                  (and calls (equal (js "document.getElementById('s').value") "d")
                       (eql (js "window.changes") 1)))
           (setf answer "Banana" calls '())
           (browser-key #\Space 0)
           (check "Space on the focused select opens the chooser too"
                  (pump-until (lambda () calls)))
           (check "... and its choice lands"
                  (pump-until (lambda () (equal (js "document.getElementById('s').value") "b"))))
           ;; a datalist: headless Chrome shows no suggestions at all
           (setf answer "two" calls '())
           (multiple-value-bind (x y) (element-center "dl") (browser-click (- x 100) y))
           (pump-until (lambda () (equal (js "document.activeElement.id") "dl")))
           (type-string "t")
           (browser-key :down 0)
           (check "Down in a datalist input offers its suggestions, filtered"
                  (pump-until (lambda () (and calls (equal (mapcar (lambda (e) (getf e :label)) (second (first calls)))
                                                           '("two" "three"))))))
           (check "... and the choice becomes the input's value"
                  (pump-until (lambda () (equal (js "document.getElementById('dl').value") "two")))))
         ;; colour and date: the minibuffer asks for the value
         (multiple-value-bind (x y) (element-center "co") (browser-click x y))
         (check "a click on a colour input asks in the minibuffer"
                (pump-until (lambda () (and *minibuffer* (search "color" (mb-prompt *minibuffer*))))))
         (browser-key #\u 4)
         (type-string "#ff8800")
         (browser-key :return 0)
         (check "... and sets what was typed"
                (pump-until (lambda () (equal (js "document.getElementById('co').value") "#ff8800"))))
         (js "document.getElementById('dt').focus()")
         (browser-key :down 8)                ; Alt-Down, the picker key
         (check "Alt-Down on a date input asks in the minibuffer"
                (pump-until (lambda () (and *minibuffer* (search "yyyy-mm-dd" (mb-prompt *minibuffer*))))))
         (browser-key #\u 4)
         (type-string "2026-10-13")
         (browser-key :return 0)
         (check "... and sets the date"
                (pump-until (lambda () (equal (js "document.getElementById('dt').value") "2026-10-13"))))
         (check "web-pick-anchor maps CSS px through the zoom"
                (let ((tab (%make-web-tab :page-rect (list 10 100 800 600))) (*web-zoom* 2))
                  (equal (multiple-value-list
                          (web-pick-anchor tab (read-json "{\"x\":5,\"y\":7.5,\"w\":40,\"h\":20}")))
                         '(20 115 80 40))))
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
         ;; page zoom fills the pane (doc/web-browser.org §4.7): Chrome's screencast
         ;; frames come at CSS size, so a zoomed page is upscaled at once and then
         ;; replaced by a device-sized screenshot once it is still
         (destructuring-bind (px py pw ph) (web-tab-page-rect (tab))
           (declare (ignore px py))
           (browser-key #\+ 4) (browser-key #\+ 4)           ; C-+ twice: 150%
           (check "C-+ C-+ is 150%: Chrome's devicePixelRatio, the CSS viewport narrower"
                  (pump-until (lambda () (and (eql (js "devicePixelRatio") 1.5d0)
                                              (< (js "innerWidth") (* 0.7 pw))))))
           (check "a zoomed page fills the pane at once (the frame upscaled, soft)"
                  (pump-until (lambda () (and (web-tab-soft (tab))
                                              (= (web-tab-frame-w (tab)) pw)
                                              (= (web-tab-frame-h (tab)) ph)))))
           (check "... and is sharpened when still: a device-sized screenshot, still full"
                  (pump-until (lambda () (and (not (web-tab-soft (tab)))
                                              (= (web-tab-frame-w (tab)) pw)
                                              (= (web-tab-frame-h (tab)) ph)))
                              5))
           (frame) (write-ppm (format nil "~a/web-zoom-150.ppm" *out*))
           (browser-key #\0 4)
           (check "C-0 is 100% again, the frame the pane's size"
                  (pump-until (lambda () (and (eql (js "devicePixelRatio") 1)
                                              (= (web-tab-frame-w (tab)) pw))))))
         (check "web-upscale: bilinear, both edges exact"
                (let* ((src (make-array 4 :element-type '(unsigned-byte 32)
                                          :initial-contents '(#x000000 #xFF0000 #x000000 #xFF0000)))
                       (out (web-upscale 2 2 src 5 3)))
                  (and (= (aref out 0) 0) (= (aref out 4) #xFF0000)
                       (= (ldb (byte 8 16) (aref out 2)) 128))))
         (loading-checks)
         (call-checks)
         ;; numbers worth writing down
         (format t "~&  frames received ~d, last decode ~a ms at ~dx~d~%"
                 (web-tab-frames (tab)) (web-tab-last-frame-ms (tab))
                 (web-tab-frame-w (tab)) (web-tab-frame-h (tab)))
         (format t "~&  Chrome PSS, all processes: ~,1f MB~%" (/ (pss-kb) 1024.0)))
    (web-shutdown))
  ;; a Chrome that stops reading its command pipe: the write gives up after
  ;; +CDP-WRITE-SECONDS+ instead of blocking the environment's thread for good. The
  ;; stand-in is a process that never reads its stdin (sleep), with 200 KB to write
  ;; into a 64 KB pipe.
  (let* ((p (sb-ext:run-program "/bin/sleep" '("30") :input :stream :output :stream :wait nil))
         (c (%make-chrome :process p :to-fd (sb-sys:fd-stream-fd (sb-ext:process-input p))
                          :from-fd (sb-sys:fd-stream-fd (sb-ext:process-output p))))
         (+cdp-write-seconds+ 1)
         (t0 (get-internal-real-time)))
    (set-nonblocking (chrome-to-fd c))
    (let ((err (handler-case (progn (cdp-write-octets c (make-array 200000 :element-type '(unsigned-byte 8))) nil)
                 (error (e) (princ-to-string e))))
          (secs (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
      (check "a Chrome that stops reading: the write gives up (no freeze)"
             (and err (search "stopped reading" err) (< secs 3)))
      (check "... and closes the pipe, so later writes fail at once"
             (and (null (chrome-to-fd c))
                  (handler-case (progn (cdp-write-octets c (make-array 1 :element-type '(unsigned-byte 8))) nil)
                    (error () t)))))
    (ignore-errors (sb-ext:process-kill p 9)) (ignore-errors (sb-ext:process-close p)))
  ;; ... but a Chrome that READS, slowly, is not cut off: the wait is for no progress,
  ;; not for the whole message (it was a deadline for the whole message, and a busy
  ;; Chrome reading a shared screen's 8.8 MB frame missed it: 2026-10-20). The
  ;; stand-in reads 64 KB every 0.2 s: 1 MB takes ~3 s, three times the wait
  (let* ((p (sb-ext:run-program "/bin/sh" '("-c" "while dd bs=65536 count=1 status=none of=/dev/null; do sleep 0.2; done")
                                :input :stream :output :stream :wait nil))
         (c (%make-chrome :process p :to-fd (sb-sys:fd-stream-fd (sb-ext:process-input p))
                          :from-fd (sb-sys:fd-stream-fd (sb-ext:process-output p))))
         (+cdp-write-seconds+ 1)
         (t0 (get-internal-real-time)))
    (set-nonblocking (chrome-to-fd c))
    (let ((err (handler-case (progn (cdp-write-octets c (make-array (* 1024 1024) :element-type '(unsigned-byte 8))) nil)
                 (error (e) (princ-to-string e))))
          (secs (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
      (check "a Chrome that reads slowly but steadily keeps its pipe (1 MB over ~3 s, wait 1 s)"
             (and (null err) (chrome-to-fd c) (> secs 1.5))))
    (ignore-errors (sb-ext:process-kill p 9)) (ignore-errors (sb-ext:process-close p)))
  (check "web-share-take: a 1920x1200 screen is cut into whole-row bands within the budget"
         (multiple-value-bind (taken rest) (web-share-take (list (list 0 0 1920 1200)))
           (and (equal taken (list (list 0 0 1920 (floor +web-share-max-pixels+ 1920))))
                (equal rest (list (list 0 (floor +web-share-max-pixels+ 1920) 1920
                                        (- 1200 (floor +web-share-max-pixels+ 1920))))))))
  (check "web-share-take: small rects all fit, nothing left"
         (multiple-value-bind (taken rest) (web-share-take (list (list 0 0 10 10) (list 5 5 20 20)))
           (and (= (length taken) 2) (null rest))))
  (let ((ok (every #'cdr *results*)))
    (format t "~&===== ~a (~d checks) =====~%" (if ok "ALL PASS" "FAILURES") (length *results*))
    (sb-ext:exit :code (if ok 0 1))))

(main)
