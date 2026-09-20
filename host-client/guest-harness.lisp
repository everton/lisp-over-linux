;;;; guest-harness.lisp — boot the image and interrogate it, for assertion scripts.
;;;;
;;;; HOST-side, like everything else here: it drives QEMU from outside and never
;;;; runs in the guest. Two channels, and the split between them is the whole idea:
;;;;
;;;;   QMP      — synthetic INPUT. Keys, held modifiers, wheel notches. QEMU turns
;;;;              these into real evdev records, so a test exercises the same path
;;;;              a finger would (doc/input.org), not a shortcut into the handler.
;;;;   net REPL — the guest's OWN STATE, read back. Assertions are on values the
;;;;              image computed about itself, never on pixels.
;;;;
;;;; This began inside check-scroll.lisp, whose header listed its two hard-won
;;;; pieces "if you write another guest assertion". check-worker.lisp was that
;;;; second one, so the pieces moved here rather than being copied — the same
;;;; instinct as one specializer printer, or one %put-pollfd.
;;;;
;;;; THE TWO PIECES WORTH KNOWING, both of which cost a debugging session to find:
;;;;
;;;;   QMP-HOLD — press a key while a modifier is GENUINELY held down. A plain
;;;;   send-key chord releases everything at once, too fast for the browser: it
;;;;   reads modifiers from the raw evdev KEYBOARD and the character from the cooked
;;;;   tty (fb-browser.lisp's poll loop refreshes Ctrl/Shift/Super first, then reads
;;;;   the byte). Release the modifier before the byte is read and the chord is lost
;;;;   — which is exactly how a Super-Space came back as a plain space.
;;;;
;;;;   PROBE — the guest serves the EDITING repl, so it echoes every character back
;;;;   wrapped in ANSI colour and a naive read returns echo, not answers. So we make
;;;;   the answer self-identifying: the form prints <<value>> and we look for that.
;;;;   The echo of the source can never match it, because the echo contains the
;;;;   literal ~s rather than a value.
;;;;
;;;; No JSON library, deliberately: we only ever EMIT JSON (that is FORMAT), and the
;;;; only thing we ask of a reply is whether it says "return" or "error" (that is
;;;; SEARCH). Same reasoning as the hand-rolled DHCP in doc/networking.org.
;;;;
;;;; All original code, under this project's MIT licence.

(require :sb-bsd-sockets)
(require :sb-posix)

(defpackage :lol.guest
  (:use :cl)
  (:export #:*root* #:*work* #:path
           #:boot-guest #:kill-guest
           #:qmp #:qmp-connect #:qmp-key #:qmp-events #:key-event
           #:qmp-hold #:qmp-wheel #:qmp-type
           #:repl-drain #:repl-connect #:strip-ansi #:between-markers
           #:probe #:probe-int #:quiet-kernel-console
           #:*results* #:check #:report-results #:run-guest-checks))

(in-package :lol.guest)

(defparameter *root*
  (merge-pathnames "../" (make-pathname :directory (pathname-directory *load-truename*)))
  "The project root — this file lives in host-client/, so nothing is hardcoded.")

(defparameter *ovmf-code* "/usr/share/OVMF/OVMF_CODE_4M.fd")
(defparameter *ovmf-vars* "/usr/share/OVMF/OVMF_VARS_4M.fd")

(defvar *work* nil "Scratch directory for this run; set by BOOT-GUEST.")

(defun path (name) (concatenate 'string *work* name))

;;; ---- QEMU ----------------------------------------------------------------

(defvar *qemu* nil)

(defun boot-guest (&optional (name "guest"))
  "Boot iso_root headless with a QMP control socket and the net REPL forwarded.
   NAME only distinguishes the scratch directory, so two runs cannot collide."
  (setf *work* (format nil "/tmp/lol-~a-~d/" name (sb-posix:getpid)))
  (ensure-directories-exist *work*)
  (sb-ext:run-program "cp" (list *ovmf-vars* (path "vars.fd")) :search t :wait t)
  (setf *qemu*
        (sb-ext:run-program
         "qemu-system-x86_64"
         (list "-machine" "q35" "-m" "2048"
               "-drive" (format nil "if=pflash,format=raw,readonly=on,file=~a" *ovmf-code*)
               "-drive" (format nil "if=pflash,format=raw,file=~a" (path "vars.fd"))
               ;; built as a pathname, not spliced into a format string: the first
               ;; attempt read "~airo_root", where the directive had quietly eaten
               ;; the s, and QEMU dutifully looked for a directory called iro_root
               "-drive" (format nil "file=fat:rw:~a,format=raw,if=ide"
                                (namestring (merge-pathnames "iso_root/" *root*)))
               "-netdev" "user,id=net0,hostfwd=tcp::4005-:4005"
               "-device" "virtio-net-pci,netdev=net0"
               "-serial" (format nil "file:~a" (path "serial.log"))
               "-display" "none" "-no-reboot"
               "-qmp" (format nil "unix:~a,server,nowait" (path "qmp.sock")))
         :search t :wait nil
         :error (path "qemu.err") :if-error-exists :supersede)))

(defun kill-guest ()
  (when *qemu* (ignore-errors (sb-ext:process-kill *qemu* 9))))

(defun preflight ()
  "Fail early and legibly when the host is missing what a boot needs."
  (dolist (f (list *ovmf-code* *ovmf-vars*))
    (unless (probe-file f) (error "missing ~a (apt install ovmf)" f)))
  (dolist (f '("iso_root/efi/boot/bootx64.efi" "iso_root/initramfs.cpio"))
    (unless (probe-file (merge-pathnames f *root*))
      (error "missing ~a — run ./build.sh first" f))))

;;; ---- QMP: a line protocol, and we only ever emit JSON ---------------------

(defvar *qmp* nil "The QMP character stream.")

(defun qmp-connect (&optional (tries 120))
  "Connect to the QMP socket once QEMU has created it, and complete the handshake."
  (dotimes (i tries (error "no QMP socket at ~a" (path "qmp.sock")))
    (let ((s (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
      (when (ignore-errors (sb-bsd-sockets:socket-connect s (path "qmp.sock")) t)
        (setf *qmp* (sb-bsd-sockets:socket-make-stream
                     s :input t :output t :buffering :none
                     :element-type 'character :external-format :latin-1))
        (read-line *qmp* nil "")               ; the greeting
        (qmp "{\"execute\":\"qmp_capabilities\"}")
        (return)))
    (sleep 1)))

(defun qmp (json)
  "Send one JSON command and wait for its reply. A QMP reply is one JSON object per
   line; async events arrive interleaved, so skip lines until one carries a result —
   which is a SEARCH, not a parse, because that is all we ever need to know."
  (write-string json *qmp*) (write-char #\Newline *qmp*) (force-output *qmp*)
  (loop for line = (read-line *qmp* nil nil)
        while line
        when (or (search "\"return\"" line) (search "\"error\"" line))
          do (return line)))

(defun qmp-key (qcode)
  (qmp (format nil "{\"execute\":\"send-key\",\"arguments\":{\"keys\":[~
                    {\"type\":\"qcode\",\"data\":\"~a\"}]}}" qcode)))

(defun qmp-events (&rest events)
  (qmp (format nil "{\"execute\":\"input-send-event\",\"arguments\":{\"events\":[~{~a~^,~}]}}"
               events)))

(defun key-event (down qcode)
  (format nil "{\"type\":\"key\",\"data\":{\"down\":~a,\"key\":~
               {\"type\":\"qcode\",\"data\":\"~a\"}}}" (if down "true" "false") qcode))

(defun qmp-hold (modifier qcode)
  "Press QCODE while MODIFIER is genuinely held — see the header."
  (qmp-events (key-event t modifier))   (sleep 0.2)
  (qmp-events (key-event t qcode))      (sleep 0.1)
  (qmp-events (key-event nil qcode))    (sleep 0.2)
  (qmp-events (key-event nil modifier)))

(defun qmp-wheel (direction &optional (notches 1))
  "Roll the wheel. QEMU models it as BUTTONS (wheel-up / wheel-down), which its PS/2
   mouse turns into the REL_WHEEL records READ-MOUSE decodes — so this exercises the
   real input path, not a shortcut into browser-scroll."
  (flet ((btn (down)
           (format nil "{\"type\":\"btn\",\"data\":{\"down\":~a,\"button\":\"wheel-~a\"}}"
                   (if down "true" "false") direction)))
    (dotimes (i notches)
      (qmp-events (btn t)) (sleep 0.05) (qmp-events (btn nil)) (sleep 0.9))))

(defun qmp-type (string &optional (cps 0.08))
  "Type STRING one key at a time. Letters and digits are their own qcodes; the few
   punctuation marks these tests need are spelled out."
  (loop for ch across string
        do (qmp-key (case ch
                      (#\- "minus") (#\Space "spc") (#\. "dot")
                      (t (string-downcase (string ch)))))
           (sleep cps)))

;;; ---- the net REPL: read the guest's own state ----------------------------

(defvar *repl* nil "The net-REPL character stream.")

(defun repl-drain (seconds)
  "Everything readable within SECONDS. Non-blocking, because the REPL never signals
   end-of-answer — it just stops talking."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second)))
        (out (make-string-output-stream)))
    (loop while (< (get-internal-real-time) deadline)
          do (let ((c (read-char-no-hang *repl* nil nil)))
               (if c (write-char c out) (sleep 0.02))))
    (get-output-stream-string out)))

(defun repl-connect (&optional (tries 120))
  "Connect to the guest's net REPL and confirm it is really EVALUATING — QEMU's
   hostfwd opens the host side of :4005 at t=0, so connecting proves nothing."
  (dotimes (i tries (error "net REPL on :4005 never answered"))
    (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
      (when (ignore-errors (sb-bsd-sockets:socket-connect s #(127 0 0 1) 4005) t)
        (setf *repl* (sb-bsd-sockets:socket-make-stream
                      s :input t :output t :buffering :none
                      :element-type 'character :external-format :latin-1))
        (repl-drain 1.0)                       ; swallow the banner
        (write-string "(+ 1 1)" *repl*) (write-char #\Newline *repl*) (force-output *repl*)
        (when (search "2" (repl-drain 3.0)) (return))
        (ignore-errors (close *repl*))))
    (sleep 1.5)))

(defun strip-ansi (string)
  "STRING without ESC [ … <letter> sequences — the editing REPL colours its echo."
  (with-output-to-string (out)
    (let ((i 0) (n (length string)))
      (loop while (< i n)
            do (if (and (char= (char string i) (code-char 27))
                        (< (1+ i) n) (char= (char string (1+ i)) #\[))
                   (progn (incf i 2)
                          (loop while (and (< i n) (not (alpha-char-p (char string i))))
                                do (incf i))
                          (when (< i n) (incf i)))
                   (progn (write-char (char string i) out) (incf i)))))))

(defun between-markers (string)
  "The LAST <<…>> payload in STRING, or NIL. The echo of the source cannot match,
   because it contains the literal ~s rather than a value."
  (let ((best nil) (i 0))
    (loop
      (let ((a (search "<<" string :start2 i)))
        (unless a (return best))
        (let ((b (search ">>" string :start2 (+ a 2))))
          (unless b (return best))
          (let ((payload (subseq string (+ a 2) b)))
            (unless (find #\~ payload) (setf best payload)))
          (setf i (+ b 2)))))))

(defun probe (form-string &optional (seconds 3.0))
  "Evaluate FORM-STRING in the guest and return its printed value as a string.

   NB: no \"~\" line continuations inside FORM-STRING — that is a FORMAT directive,
   and these are plain strings, so the tilde and newline would travel to the guest
   inside the form and its reader would choke on them. (It did, once.)"
  (write-string (format nil "(format t \"~~%<<~~s>>~~%\" (ignore-errors ~a))" form-string)
                *repl*)
  (write-char #\Newline *repl*) (force-output *repl*)
  (or (between-markers (strip-ansi (repl-drain seconds))) "?"))

(defun probe-int (form-string &optional (seconds 3.0))
  "PROBE, parsed as an integer, or NIL. SECONDS matters more than it looks: a probe
   that times out returns \"?\" and parses to NIL, which is indistinguishable from a
   failed assertion — so give a slow form (a whole-image sweep, say) real room."
  (let ((s (probe form-string seconds)))
    (ignore-errors (parse-integer (string-trim " " s)))))

(defun quiet-kernel-console ()
  "Lower the console log level, so a late printk cannot repaint over whatever the
   browser has drawn. Any test that looks at the SCREEN wants this; one that only
   asks the guest questions does not care."
  (write-string
   (concatenate 'string
                "(ignore-errors (with-open-file (o \"/proc/sys/kernel/printk\""
                " :direction :output :if-exists :append)"
                " (write-string \"1\" o) (terpri o)))")
   *repl*)
  (write-char #\Newline *repl*) (force-output *repl*)
  (repl-drain 2.0))

;;; ---- assertions ----------------------------------------------------------

(defvar *results* '())

(defun check (label ok) (push (cons label (and ok t)) *results*))

(defun report-results ()
  "Print the results block. True when everything passed."
  (format t "~&~%===== RESULTS =====~%")
  (let ((ok t))
    (dolist (r (reverse *results*))
      (format t "  ~52a ~a~%" (car r) (if (cdr r) "PASS" "FAIL"))
      (unless (cdr r) (setf ok nil)))
    (format t "===== ~a =====~%" (if ok "ALL PASS" "FAILURES"))
    (finish-output)
    ok))

(defun run-guest-checks (name body-fn &key (qmp t))
  "Boot, connect, run BODY-FN, report, and ALWAYS kill the guest. Exits the process
   with 0 when every check passed. QMP nil skips the input channel for tests that
   only ask the guest questions."
  (let ((ok nil))
    (unwind-protect
         (setf ok (handler-case
                      (progn
                        (preflight)
                        (boot-guest name)
                        (format t "~&booting the guest…~%") (finish-output)
                        (when qmp (qmp-connect))
                        (repl-connect)
                        (format t "~&net REPL up~%") (finish-output)
                        (funcall body-fn)
                        (report-results))
                    (serious-condition (c)
                      (format *error-output* "~&~a: ~a~%" name c)
                      nil)))
      (kill-guest))
    (sb-ext:exit :code (if ok 0 1))))
