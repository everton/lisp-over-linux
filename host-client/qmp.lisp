;;;; qmp.lisp — talk to a running QEMU over its QMP monitor socket, in Common Lisp.
;;;;
;;;; Every script in this project that boots the guest and wants to press a key or
;;;; take a screenshot needs the same twenty lines: connect to a Unix socket, say
;;;; hello, send a command, read the reply. Those twenty lines used to be embedded
;;;; Python in three different shell scripts (build.sh, poc/build-poc.sh,
;;;; refresh-screenshot.sh), which is why python3 was a hard dependency of a Lisp
;;;; project. This is that, once, in Lisp.
;;;;
;;;; QMP is line-delimited JSON: one JSON object per line, both directions. We EMIT
;;;; JSON, which is FORMAT, and the only thing we ever ask of a reply is whether it
;;;; said "return" or "error", which is SEARCH — so there is no JSON parser here and
;;;; no need for one. If a caller ever needs a field out of a reply, add a parser
;;;; then; until then it would be weight for nothing.
;;;;
;;;; Not shipped in the image: a host tool, like everything in host-client/.

(defpackage :lol.qmp
  (:use :cl)
  (:export #:with-qmp #:qmp-connect #:qmp-close #:command
           #:key #:keys #:hold #:type-string #:wheel #:click #:screendump
           #:*qmp*))

(in-package :lol.qmp)

(defvar *qmp* nil "The QMP character stream of the connected monitor.")

;;; ---- connection ----------------------------------------------------------

(defun qmp-connect (socket-path &key (tries 120))
  "Connect to SOCKET-PATH and complete the capabilities handshake. Retries, because
   a script normally starts QEMU and connects in the same breath — the socket does
   not exist for the first second or so."
  (dotimes (i tries (error "no QMP socket appeared at ~a" socket-path))
    (let ((s (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
      (when (ignore-errors (sb-bsd-sockets:socket-connect s socket-path) t)
        (setf *qmp* (sb-bsd-sockets:socket-make-stream
                     s :input t :output t :buffering :none
                     :element-type 'character :external-format :latin-1))
        (read-line *qmp* nil "")              ; the greeting banner
        (command "{\"execute\":\"qmp_capabilities\"}")
        (return *qmp*)))
    (sleep 1)))

(defun qmp-close ()
  (when *qmp* (ignore-errors (close *qmp*)) (setf *qmp* nil)))

(defmacro with-qmp ((socket-path &key (tries 120)) &body body)
  `(unwind-protect (progn (qmp-connect ,socket-path :tries ,tries) ,@body)
     (qmp-close)))

(defun command (json)
  "Send one JSON command; return the reply line. QEMU interleaves asynchronous
   events with replies, so skip lines until one carries a result."
  (write-string json *qmp*) (write-char #\Newline *qmp*) (force-output *qmp*)
  (loop for line = (read-line *qmp* nil nil)
        while line
        when (or (search "\"return\"" line) (search "\"error\"" line))
          do (return line)))

;;; ---- input ---------------------------------------------------------------

(defun keys (&rest qcodes)
  "Press QCODES together as one chord and release them. QEMU's send-key: fine for
   ordinary keys and for chords the guest reads from one input stream."
  (command (format nil "{\"execute\":\"send-key\",\"arguments\":{\"keys\":[~{~a~^,~}]}}"
                   (mapcar (lambda (q)
                             (format nil "{\"type\":\"qcode\",\"data\":\"~a\"}" q))
                           qcodes))))

(defun key (qcode) (keys qcode))

(defun send-events (&rest events)
  (command (format nil "{\"execute\":\"input-send-event\",\"arguments\":{\"events\":[~{~a~^,~}]}}"
                   events)))

(defun key-event (down qcode)
  (format nil "{\"type\":\"key\",\"data\":{\"down\":~a,\"key\":{\"type\":\"qcode\",\"data\":\"~a\"}}}"
          (if down "true" "false") qcode))

(defun hold (modifier qcode)
  "Press QCODE while MODIFIER is GENUINELY held down, with real time between the
   events. SEND-KEY will not do: it presses and releases the whole chord at once,
   which is too fast for the framebuffer browser, because that reads the modifier
   from the raw evdev KEYBOARD and the character from the cooked tty. If the
   modifier is already released when the byte is read, the chord is lost — this is
   exactly how a Super-Space used to arrive as a plain space."
  (send-events (key-event t modifier))   (sleep 0.2)
  (send-events (key-event t qcode))      (sleep 0.1)
  (send-events (key-event nil qcode))    (sleep 0.2)
  (send-events (key-event nil modifier)))

(defparameter *qcode-names*
  '((#\Space . "spc") (#\- . "minus") (#\. . "dot") (#\, . "comma")
    (#\/ . "slash") (#\; . "semicolon") (#\' . "apostrophe") (#\= . "equal")
    (#\[ . "bracket_left") (#\] . "bracket_right") (#\\ . "backslash")
    (#\` . "grave_accent"))
  "The punctuation a qcode does not simply spell. Letters and digits are their own.")

(defun type-string (string &key (cps 0.08) (shifted "()\"*&^%$#@!+_:<>?~|{}"))
  "Type STRING one key at a time. Characters in SHIFTED need shift held, which is
   spelled out per keyboard layout and only covers what our scripts actually type."
  (loop for ch across string
        do (let ((name (cdr (assoc ch *qcode-names*))))
             (cond (name (key name))
                   ((find ch shifted)
                    ;; the shifted characters we need, by the unshifted key they sit on
                    (let ((base (case ch
                                  (#\( "9") (#\) "0") (#\" "apostrophe") (#\* "8")
                                  (#\& "7") (#\^ "6") (#\% "5") (#\$ "4") (#\# "3")
                                  (#\@ "2") (#\! "1") (#\+ "equal") (#\_ "minus")
                                  (#\: "semicolon") (#\< "comma") (#\> "dot")
                                  (#\? "slash") (#\~ "grave_accent") (#\| "backslash")
                                  (#\{ "bracket_left") (#\} "bracket_right"))))
                      (when base (keys "shift" base))))
                   ;; a capital is shift + the letter's own key. Without this a
                   ;; capital was silently typed as lower case, which a test then
                   ;; reported as a FAILING ASSERTION rather than as mistyping —
                   ;; found exactly that way.
                   ((upper-case-p ch) (keys "shift" (string-downcase (string ch))))
                   (t (key (string-downcase (string ch))))))
           (sleep cps)))

(defun wheel (direction &key (notches 1) (settle 0.9))
  "Roll the mouse wheel. QEMU models it as BUTTONS — wheel-up / wheel-down — which
   its PS/2 mouse turns into the evdev REL_WHEEL records the guest actually decodes.
   So this exercises the real input path rather than shortcutting into the browser."
  (flet ((btn (down)
           (format nil "{\"type\":\"btn\",\"data\":{\"down\":~a,\"button\":\"wheel-~a\"}}"
                   (if down "true" "false") direction)))
    (dotimes (i notches)
      (send-events (btn t)) (sleep 0.05) (send-events (btn nil)) (sleep settle))))

(defun click (x y &key (settle 2.0))
  "Left-click at absolute X,Y.

   The guest's PS/2 mouse is RELATIVE and the browser accumulates deltas into its own
   clamped cursor, so there is no way to say 'go here'. Instead: slam at the top-left
   corner with a huge negative delta, which the clamp turns into a known origin of
   (0,0), then move by exactly (x,y). The dwell between them is NOT cosmetic —
   queued relative deltas are summed before the browser clamps, so a slam and a move
   sent together become one motion that still clamps to the corner."
  (flet ((rel (axis v)
           (format nil "{\"type\":\"rel\",\"data\":{\"axis\":\"~a\",\"value\":~d}}" axis v))
         (btn (down)
           (format nil "{\"type\":\"btn\",\"data\":{\"down\":~a,\"button\":\"left\"}}"
                   (if down "true" "false"))))
    (send-events (rel "x" -4000) (rel "y" -4000)) (sleep settle)
    (send-events (rel "x" x) (rel "y" y))         (sleep settle)
    (send-events (btn t)) (sleep 0.12) (send-events (btn nil))))

;;; ---- output --------------------------------------------------------------

(defun screendump (path)
  "Ask QEMU to write the current screen to PATH as a binary PPM."
  (command (format nil "{\"execute\":\"screendump\",\"arguments\":{\"filename\":\"~a\"}}"
                   path)))
