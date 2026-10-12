;;;; check-input.lisp — every keyboard and every pointer at once, and the ones plugged in later.
;;;;
;;;;     sbcl --script check-input.lisp            ; exits 0 if all assertions pass
;;;;
;;;; The bug this guards (doc/input.org §4¾): the environment opened ONE pointer and ONE
;;;; keyboard, chosen once at startup. On the laptop that meant the external mouse OR
;;;; the touchpad, never both; the laptop's own keyboard for Ctrl/Super, never the
;;;; external one; and nothing plugged in afterwards.
;;;;
;;;; So the guest boots with several of each, and each one is driven ON ITS OWN:
;;;;
;;;;   pointers   the PS/2 mouse every PC has (stands in for a touchpad's mouse node),
;;;;              a USB mouse, and a USB tablet (absolute, like a pad's coordinates)
;;;;   keyboards  the PS/2 keyboard and a USB keyboard
;;;;
;;;; Steering QMP events at ONE device takes two different tricks, because QEMU routes
;;;; an event to "the first input handler that can take it":
;;;;
;;;;   - relative motion and buttons: the HMP command `mouse_set N` makes mouse N the
;;;;     active one (N from `info mice`). Absolute events can only go to the tablet —
;;;;     it is the one device with that capability.
;;;;   - keys: a handler BOUND to a display console is tried first for events sent
;;;;     with that console's `device`; unbound ones take the rest. So the USB keyboard
;;;;     is bound to the VGA device (display=vga0): keys sent with "device":"vga0" reach
;;;;     it, keys sent without reach the PS/2 keyboard.
;;;;     (usb-mouse has no display property in QEMU 11.1, which is why mice use mouse_set.)
;;;;
;;;; Then hotplug: QMP device_add a USB mouse while the environment runs, move it, and
;;;; device_del it — the environment must open it, read it, close it, and keep going
;;;; without spinning on the dead fd. With test-disk.img present the guest runs as the
;;;; USER (uid 1000), so the hotplugged node must also have been handed over by lol-rootd
;;;; before it could be opened at all — the open itself proves the ownership path.
;;;;
;;;; virtio-input (virtio-tablet-pci / virtio-mouse-pci) would be a third bus, but
;;;; CONFIG_VIRTIO_INPUT is off (tag #26): USB and PS/2 are what the laptop has.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))

(in-package :lol.guest)

;;; The disk makes the guest drop to the user — the interesting case for hotplug.
(when (and (null (sb-posix:getenv "LOL_GUEST_DISK"))
           (probe-file (merge-pathnames "test-disk.img" *root*)))
  (sb-posix:setenv "LOL_GUEST_DISK" "test-disk.img" 1))

(setf *extra-qemu-args*
      (list "-vga" "none" "-device" "VGA,id=vga0"
            "-device" "qemu-xhci,id=xhci"
            "-device" "usb-mouse,id=umouse,bus=xhci.0"
            "-device" "usb-tablet,id=utab,bus=xhci.0"
            "-device" "usb-kbd,id=ukbd,bus=xhci.0,display=vga0"))

;;; ---- QMP helpers -----------------------------------------------------------

(defun hmp (command)
  "Run a human-monitor COMMAND through QMP; its text output, \\r\\n left in."
  (qmp (format nil "{\"execute\":\"human-monitor-command\",\"arguments\":{\"command-line\":\"~a\"}}"
               command)))

(defun mouse-indexes (name)
  "The `info mice` indexes of every mouse whose name contains NAME, in order."
  (let ((text (hmp "info mice")) (out '()) (i 0))
    (loop
      (let ((p (search "Mouse #" text :start2 i)))
        (unless p (return (nreverse out)))
        (let* ((colon (position #\: text :start p))
               (end (or (search "\\r" text :start2 colon) (search "\\n" text :start2 colon)
                        (length text)))
               (idx (parse-integer text :start (+ p 7) :end colon)))
          (when (search name (subseq text colon end)) (push idx out))
          (setf i end))))))

(defun select-mouse (index) (hmp (format nil "mouse_set ~d" index)) (sleep 0.3))

(defun rel-move (dx dy)
  (qmp-events (format nil "{\"type\":\"rel\",\"data\":{\"axis\":\"x\",\"value\":~d}}" dx)
              (format nil "{\"type\":\"rel\",\"data\":{\"axis\":\"y\",\"value\":~d}}" dy))
  (sleep 0.8))

(defun abs-move (x y)
  "Absolute position, 0..32767 on each axis — only the tablet can take these."
  (qmp-events (format nil "{\"type\":\"abs\",\"data\":{\"axis\":\"x\",\"value\":~d}}" x)
              (format nil "{\"type\":\"abs\",\"data\":{\"axis\":\"y\",\"value\":~d}}" y))
  (sleep 0.8))

(defun click ()
  (flet ((btn (down) (format nil "{\"type\":\"btn\",\"data\":{\"down\":~a,\"button\":\"left\"}}"
                             (if down "true" "false"))))
    (qmp-events (btn t)) (sleep 0.15) (qmp-events (btn nil)) (sleep 0.8)))

(defun key-on (device down qcode)
  "One key event, at DEVICE's console (\"vga0\" = the USB keyboard) or, with DEVICE
   nil, at whatever takes unbound keys (the PS/2 keyboard)."
  (qmp (format nil "{\"execute\":\"input-send-event\",\"arguments\":{~@[\"device\":\"~a\",~]\"events\":[~a]}}"
               device (key-event down qcode)))
  (sleep 0.5))

;;; ---- the guest's view ------------------------------------------------------

(defun devices ()
  "The guest's open devices, as host lists (NODE KIND NAME RECORDS CLICKS)."
  (let ((s (probe "(mapcar (lambda (d) (list (input-dev-node d) (input-dev-kind d) (input-dev-name d) (input-dev-records d) (input-dev-clicks d))) *input-devices*)" 5.0)))
    (or (ignore-errors (let ((*read-eval* nil)) (read-from-string s))) '())))

(defun dev-named (name &optional (devs (devices)))
  (find-if (lambda (d) (search name (third d))) devs))

(defun records-of (node &optional (devs (devices)))
  (fourth (find node devs :key #'first :test #'equal)))

(defun clicks-of (node &optional (devs (devices)))
  (fifth (find node devs :key #'first :test #'equal)))

(defun cursor () (list (probe-int "*cursor-x*") (probe-int "*cursor-y*")))

(defun move-and-see (label node mover)
  "Drive one pointer with MOVER, and check that THAT device's records grew and the
   cursor moved."
  (let ((before (records-of node)) (c0 (cursor)))
    (funcall mover)
    (let ((after (records-of node)) (c1 (cursor)))
      (format t "~&  ~a (~a): records ~a -> ~a, cursor ~a -> ~a~%" label node before after c0 c1)
      (check (format nil "~a: its own records are read" label)
             (and before after (> after before)))
      (check (format nil "~a: the cursor moved" label) (not (equal c0 c1))))))

(defun click-and-see (label node)
  (let ((before (clicks-of node)))
    (click)
    (let ((after (clicks-of node)))
      (check (format nil "~a: a click is seen" label) (and before after (> after before))))))

(defun checks ()
  (quiet-kernel-console)
  (check "environment came up" (wait-for-browser))
  (let ((user-p (string= (probe "(sb-unix:unix-getuid)") "1000")))
    (format t "~&running as ~:[root (no test-disk.img)~;the user, uid 1000~]~%" user-p)

    ;; ---- A. everything is open at once
    (let* ((devs (devices))
           (kinds (mapcar #'second devs)))
      (format t "~&open devices:~%~{  ~s~%~}" devs)
      (check "A. two keyboards open at once" (>= (count :keyboard kinds) 2))
      (check "A. the PS/2 mouse is open"     (dev-named "PS/2" devs))
      (check "A. the USB mouse is open"      (dev-named "USB Mouse" devs))
      (check "A. the USB tablet is open"     (eq :tablet (second (dev-named "Tablet" devs))))
      (check "A. Spotter offers 'input devices'"
             (string= "T" (probe "(and (find \"input devices\" (spotter-extra-candidates) :key (lambda (c) (getf c :label)) :test #'string=) t)")))
      (check "A. the diagnostic view lists every open device"
             (let ((n (probe-int "(count-if (lambda (i) (search \"every one\" (or (item-group i) \"\"))) (view-items (present (subj-input-devices))))")))
               (and n (= n (length devs))))))

    ;; ---- B. each pointer, on its own
    (let ((ps2 (first (dev-named "PS/2")))
          (usb (first (dev-named "USB Mouse")))
          (tab (first (dev-named "Tablet"))))
      (select-mouse (first (mouse-indexes "PS/2")))
      (move-and-see "B. PS/2 mouse" ps2 (lambda () (rel-move 40 30)))
      (click-and-see "B. PS/2 mouse" ps2)
      (select-mouse (first (mouse-indexes "HID Mouse")))
      (move-and-see "B. USB mouse" usb (lambda () (rel-move -60 25)))
      (click-and-see "B. USB mouse" usb)
      (move-and-see "B. USB tablet" tab (lambda () (abs-move 8000 12000)))
      (let ((c (cursor)) (xres (probe-int "*fb-xres*")))
        (check "B. the tablet lands where it was aimed"
               (and xres (<= (abs (- (first c) (floor (* 8000 xres) 32768))) 2))))
      ;; and the first one again: a device read before must still be read after the
      ;; others have been — the bug was exactly that only one ever was
      (select-mouse (first (mouse-indexes "PS/2")))
      (move-and-see "B. PS/2 mouse again" ps2 (lambda () (rel-move 15 -10))))

    ;; ---- C. each keyboard's modifiers, and their OR
    (let* ((ukbd (first (dev-named "USB Keyboard")))
           (atkbd (first (dev-named "AT Translated")))
           (u0 (records-of ukbd)) (a0 (records-of atkbd)))
      (key-on "vga0" t "ctrl")
      (check "C. Ctrl held on the USB keyboard is seen" (string= (probe "*ctrl-down*") "T"))
      (check "C. ...through the USB keyboard's own node, not the PS/2 one"
             (and (> (records-of ukbd) u0) (= (records-of atkbd) a0)))
      (key-on nil t "ctrl")
      (check "C. the PS/2 keyboard's Ctrl comes through its node"
             (> (records-of atkbd) a0))
      (key-on "vga0" nil "ctrl")
      (check "C. ...and Ctrl is still held while the PS/2 one holds it"
             (string= (probe "*ctrl-down*") "T"))
      (key-on nil nil "ctrl")
      (check "C. released on both, it is released" (string= (probe "*ctrl-down*") "NIL"))
      (key-on "vga0" t "meta_l")
      (check "C. Super on the USB keyboard is seen" (string= (probe "*super-down*") "T"))
      (key-on "vga0" nil "meta_l")
      (check "C. ...and released" (string= (probe "*super-down*") "NIL")))

    ;; ---- P. a touchpad, through the per-device state. QEMU emulates no pad, so a
    ;; file of synthetic records stands in for one (a regular file is an fd, and the
    ;; non-blocking drain ends at EOF just as at an empty queue). One finger lands at
    ;; x=1000 and slides to 2000 on a 0..32767 axis: a pad must move the cursor by the
    ;; DIFFERENCE, scaled by *pad-speed* (1000 * xres * 3/2 / 32767), not jump to an
    ;; absolute place — and the remembered finger must stay the DEVICE's, not leak
    ;; into the globals a real mouse is read against. Not registered in
    ;; *input-devices*: poll would call a regular file readable forever.
    (let ((r (probe "(let* ((p \"/tmp/check-input-pad.bin\") (recs '((1 330 1) (3 0 1000) (3 1 1000) (0 0 0) (3 0 2000) (3 1 1000) (0 0 0)))) (with-open-file (o p :direction :output :if-exists :supersede :element-type '(unsigned-byte 8)) (dolist (r recs) (dotimes (i 16) (write-byte 0 o)) (destructuring-bind (ty co va) r (write-byte (ldb (byte 8 0) ty) o) (write-byte (ldb (byte 8 8) ty) o) (write-byte (ldb (byte 8 0) co) o) (write-byte (ldb (byte 8 8) co) o) (dotimes (k 4) (write-byte (ldb (byte 8 (* 8 k)) va) o))))) (let* ((fd (open-evdev p)) (d (%make-input-dev :node \"fake\" :kind :pad :fd fd)) (x0 *cursor-x*)) (setf (input-dev-state d) (fresh-device-state fd :pad)) (with-device-state (d) (read-mouse fd *fb-xres* *fb-yres*)) (%fbb-close fd) (list (- *cursor-x* x0) (floor (* 1000 *fb-xres* 3/2) 32767) (device-var d '*pad-x*) *pad-x*)))" 10.0)))
      (format t "~&pad replay: (moved expected device-pad-x global-pad-x) = ~a~%" r)
      (let ((v (ignore-errors (read-from-string r))))
        (check "P. a touchpad moves the cursor by the difference, not to a place"
               (and (consp v) (<= (abs (- (first v) (second v))) 1)))
        (check "P. ...and its finger is remembered per device"
               (and (consp v) (eql (third v) 2000) (null (fourth v))))))

    ;; ---- C'. hotplug a USB keyboard, and unplug it
    (let ((before (mapcar #'first (devices))) (new nil))
      (qmp "{\"execute\":\"device_add\",\"arguments\":{\"driver\":\"usb-kbd\",\"id\":\"hotkbd\",\"bus\":\"xhci.0\"}}")
      (dotimes (i 20)
        (sleep 0.5)
        (setf new (find-if (lambda (d) (and (eq (second d) :keyboard)
                                            (not (member (first d) before :test #'equal))))
                           (devices)))
        (when new (return)))
      (check "C'. a hotplugged USB keyboard is opened" new)
      (qmp "{\"execute\":\"device_del\",\"arguments\":{\"id\":\"hotkbd\"}}")
      (let ((gone nil))
        (dotimes (i 20)
          (sleep 0.5)
          (when (and new (not (member (first new) (mapcar #'first (devices)) :test #'equal)))
            (setf gone t) (return)))
        (check "C'. ...and forgotten when unplugged" gone)))

    ;; ---- D. hotplug a USB mouse
    (let ((before (mapcar #'first (devices))))
      (qmp "{\"execute\":\"device_add\",\"arguments\":{\"driver\":\"usb-mouse\",\"id\":\"hotmouse\",\"bus\":\"xhci.0\"}}")
      (let ((new nil))
        (dotimes (i 20)
          (sleep 0.5)
          (setf new (find-if (lambda (d) (and (eq (second d) :mouse)
                                              (not (member (first d) before :test #'equal))))
                             (devices)))
          (when new (return)))
        (format t "~&hotplugged: ~s~%" new)
        (check (format nil "D. a hotplugged USB mouse is opened~:[~; (as uid 1000)~]" user-p) new)
        (when new
          (select-mouse (car (last (mouse-indexes "HID Mouse"))))
          (move-and-see "D. hotplugged mouse" (first new) (lambda () (rel-move 30 30)))
          (click-and-see "D. hotplugged mouse" (first new)))

        ;; ---- E. unplug it
        (qmp "{\"execute\":\"device_del\",\"arguments\":{\"id\":\"hotmouse\"}}")
        (let ((gone nil))
          (dotimes (i 20)
            (sleep 0.5)
            (when (and new (not (member (first new) (mapcar #'first (devices)) :test #'equal)))
              (setf gone t) (return)))
          (check "E. the unplugged mouse is closed and forgotten" gone))
        (check "E. the environment survives" (string= (probe "(and *fb-canvas* t)") "T"))
        (let ((w0 (probe-int "*input-wakeups*")))
          (sleep 4)
          (let ((w1 (probe-int "*input-wakeups*")))
            (format t "~&idle wakeups over 4 s after the unplug: ~a~%" (and w0 w1 (- w1 w0)))
            ;; the clock ticks once a second and the pointer hides once: a handful.
            ;; A poll spinning on a dead fd would be tens of thousands.
            (check "E. no busy loop on the dead fd" (and w0 w1 (< (- w1 w0) 20)))))
        (select-mouse (first (mouse-indexes "PS/2")))
        (move-and-see "E. PS/2 mouse after the unplug"
                      (first (dev-named "PS/2")) (lambda () (rel-move -20 -20)))))))

(run-guest-checks "check-input" #'checks)
