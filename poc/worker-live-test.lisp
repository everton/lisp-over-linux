;;;; worker-live-test.lisp — the live-worker protocol, on the host, in seconds.
;;;;
;;;;     sbcl --script poc/worker-live-test.lisp      ; exits non-zero on a failure
;;;;
;;;; There is no /sbin/lisp on the host, so SPAWN-LIVE-WORKER's child is a THREAD
;;;; speaking the protocol over two real pipes (initramfs/worker-live.lisp). That
;;;; exercises everything but the fork: framing, handles, interning, invalidation,
;;;; the one-invalidate-per-handle rule, pane refresh through the service's :RUN, and
;;;; a child that goes away. check-worker.lisp boots the image for the real process.
;;;;
;;;; All original code, under this project's MIT licence.

(require :sb-introspect)

(defparameter *poc-dir* (make-pathname :directory (pathname-directory *load-truename*)))
(defparameter *root-dir* (merge-pathnames "../" *poc-dir*))

(let ((*load-verbose* nil) (*load-print* nil))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "initramfs/registry.lisp" *root-dir*))
    (dolist (m '("initramfs/sbcl-source.lisp" "initramfs/model.lisp" "initramfs/present.lisp"
                 "initramfs/examples.lisp" "initramfs/stage.lisp" "initramfs/worker.lisp"
                 "poc/ttf.lisp" "poc/canvas.lisp" "poc/textview.lisp" "poc/widgets.lisp" "poc/morph.lisp"
                 "initramfs/ansi.lisp" "initramfs/line-editor.lisp" "poc/clipboard.lisp"
                 "initramfs/sysinfo.lisp" "poc/inflate.lisp" "poc/image.lisp"
                 "initramfs/files.lisp" "initramfs/file-views.lisp"
                 "poc/shell.lisp" "initramfs/web-cdp.lisp"
                 "initramfs/worker-live.lisp" "initramfs/sandbox.lisp"))
      (funcall 'load-recording (merge-pathnames m *root-dir*)))))

(in-package :cl-user)

(defvar *fails* 0)
(defvar *count* 0)
(defun check (name ok)
  (incf *count*)
  (format t "~&  ~:[FAIL~;ok  ~]  ~a~%" ok name)
  (finish-output)
  (unless ok (incf *fails*))
  ok)

(defun settle (&optional (seconds 3) (until (constantly nil)))
  "Run the service's :RUN as the browser loop would, until UNTIL or SECONDS.
   -> how many turns asked for a redraw."
  (let ((end (+ (get-internal-real-time) (* seconds internal-time-units-per-second)))
        (redraws 0))
    (loop until (or (funcall until) (> (get-internal-real-time) end))
          do (when (eq (worker-live-run nil nil) :redraw) (incf redraws))
             (sleep 0.02))
    redraws))

(defun row (view group)
  (find group (view-items view) :key #'item-group :test #'equal))

(format t "~&== the wire ==~%")
(multiple-value-bind (r w) (sb-unix:unix-pipe)
  (set-nonblocking r)
  (let ((a (make-wire r w))
        (half (wire-encode '(:invalidate 7))))
    (wire-send a (list :view 3 (list :title (format nil "two~%lines"))))
    (wire-send a (list :view 4 (list :title (format nil "nul~ahere" (code-char 0)))))
    (wire-send a '(:launch-missiles))
    (sb-sys:with-pinned-objects (half)              ; a frame cut in half: two reads
      (sb-unix:unix-write w (sb-sys:vector-sap half) 0 5))
    (wire-pump a)
    (let ((msgs (wire-take a)))
      (check "two whole frames come out, the unknown head is dropped" (= 2 (length msgs)))
      (check "a newline inside a string survives"
             (find #\Newline (getf (third (first msgs)) :title)))
      (check "a NUL inside a string does not end the frame"
             (search "here" (getf (third (second msgs)) :title))))
    (sb-sys:with-pinned-objects (half)
      (sb-unix:unix-write w (sb-sys:vector-sap half) 5 (- (length half) 5)))
    (wire-pump a)
    (check "the second half completes the frame" (equal (wire-take a) '((:invalidate 7))))
    (check "#. on the wire is not evaluated"
           (null (read-worker-form (make-string-input-stream "(:view #.(error \"pwned\"))"))))
    (sb-unix:unix-close w)
    (wire-pump a)
    (check "closing the writer reads as EOF" (wire-eof a))
    (sb-unix:unix-close r)))

(format t "~&== a live ticker, opened in a pane ==~%")
(setf *trails* '() *active* 0)
(let* ((root (subj-live :ticker))
       (conn (subj-remote-conn root)))
  (trail-root root)
  (let* ((pane (current-pane))
         (ticks (lambda () (let ((it (find "ticks" (view-items (pane-view pane))
                                           :key #'item-label :test #'equal)))
                             (and it (parse-integer (item-detail it)))))))
    (check "the first view arrived synchronously" (funcall ticks))
    (check "the provenance row admits it is a thread on the host"
           (search "THREAD" (item-label (first (view-items (pane-view pane))))))
    (let* ((t0 (funcall ticks))
           (redraws (settle 3.5)))
      (check "ticks advanced with nothing but the service running" (> (funcall ticks) t0))
      (check "and the service asked for a redraw each time" (>= redraws 2)))
    (check "the pane is still the same pane, the subject still EQ"
           (and (eq pane (current-pane)) (eq (pane-subject pane) root)
                (eq root (remote-subject conn 0))))
    ;; drill: a row whose subject has NO name — only a handle reaches it
    (let ((obj (item-subject (row (pane-view pane) "drill"))))
      (check "the object row carries a remote subject, not a name" (subj-remote-p obj))
      (check "presenting again gives the SAME subject (interned on both sides)"
             (eq obj (item-subject (row (present root) "drill"))))
      (trail-push obj)
      (check "the drilled view is the CHILD's inspector of its own struct"
             (find-if (lambda (it) (search "TICKS" (string-upcase (item-label it))))
                      (view-items (pane-view (current-pane))))))
    ;; hide its tab: a second trail with something local in it
    (new-trail (subj-defn 'subj-live))
    (settle 1.5)
    (let* ((before (worker-conn-updates conn))
           (redraws (settle 2.5)))
      (check "with its tab hidden, no redraws" (zerop redraws))
      (check "... and no views asked for"
             (= before (worker-conn-updates conn))))
    (activate-trail 0)
    (check "back on its tab, it catches up" (plusp (settle 2.5)))
    (format t "~&== letting go ==~%")
    ;; (fresh panes: the old trails' preview caches go with them)
    (setf *trails* (list (list (open-pane (subj-defn 'subj-live)))) *active* 0)
    (let ((old +worker-idle-seconds+))
      (setf +worker-idle-seconds+ 1)
      (settle 4 (lambda () (not (member conn *worker-conns*))))
      (setf +worker-idle-seconds+ old))
    (check "a worker no pane shows is closed and forgotten" (not (member conn *worker-conns*)))
    (sb-thread:join-thread (worker-conn-thread conn) :default nil :timeout 3)
    (check "... and its child (a thread, here) has ended"
           (not (sb-thread:thread-alive-p (worker-conn-thread conn))))))

(format t "~&== the live sweep ==~%")
(setf *trails* '() *active* 0)
(let* ((root (subj-live :sweep)) (conn (subj-remote-conn root)))
  (trail-root root)
  (flet ((progress () (item-label (second (view-items (pane-view (current-pane))))))
         (done-p () (search "done" (view-title (pane-view (current-pane))))))
    (let ((first (progress)))
      (settle 60 #'done-p)
      (check "the progress row moved" (string/= first (progress)))
      (check "the sweep finished" (done-p))
      (check "and found what the synchronous sweep finds"
             (= (length (getf (job-signallers) :rows))
                (count "results" (view-items (pane-view (current-pane)))
                       :key #'item-group :test #'equal)))
      (let ((r (row (pane-view (current-pane)) "results")))
        (check "a result drills into the child's condition view"
               (and r (subj-remote-p (item-subject r))
                    (plusp (length (view-items (present (item-subject r))))))))))
  (worker-close conn :exited "test over"))

(format t "~&== no such job ==~%")
(check "an unknown live job is a view saying so"
       (search "no such live job" (view-title (present (subj-live :no-such)))))

(format t "~&== the sandbox on the host ==~%")
(check "Accept is unsandboxed on the host, and says so"
       (and (accept-source "(defun zz-host-sand (x) (* x 3))")
            (eq :unsandboxed (getf *last-sandbox* :outcome))))

(format t "~&~d/~d passed~%" (- *count* *fails*) *count*)
(sb-ext:exit :code (if (zerop *fails*) 0 1) :abort t)
