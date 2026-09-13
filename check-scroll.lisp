;;;; check-scroll.lisp — assert the browser's scrolling behaviour in the BOOTED guest.
;;;;
;;;;     sbcl --script check-scroll.lisp        ; exits 0 if all assertions pass
;;;;
;;;; poc/render-frame.lisp checks what a frame LOOKS like. This checks what the
;;;; machine DOES with real input, which no host render can: it boots the actual
;;;; image under QEMU, drives the framebuffer browser from outside — synthetic key
;;;; presses, and wheel events that QEMU's PS/2 mouse turns into genuine evdev
;;;; REL_WHEEL records — then reads the browser's OWN state back over the net REPL.
;;;; The assertions are on PANE-TOP / PANE-SEL / TV-SCROLL, never on pixels.
;;;;
;;;; Run ./build.sh first if you changed the Lisp; this boots iso_root as it stands.
;;;;
;;;; WHY THIS IS LISP AND NOT A SCRIPT IN SOMETHING ELSE. It needs four things —
;;;; spawn a process, speak a line protocol over a Unix socket, speak another over
;;;; TCP, and compare some integers — and SBCL has all four in the box:
;;;; SB-EXT:RUN-PROGRAM and SB-BSD-SOCKETS' LOCAL-SOCKET and INET-SOCKET. The one
;;;; thing that looks like it needs a library is QMP's JSON, and it does not: we
;;;; only ever EMIT JSON, which is FORMAT, and the only thing we ask of a reply is
;;;; whether it says "return" or "error", which is SEARCH. A parser would be weight
;;;; for nothing. Same reasoning the rest of the project runs on (doc/networking.org
;;;; hand-rolls DHCP, doc/x11.org an X client): write the small thing you need.
;;;;
;;;; TWO REUSABLE PIECES, if you write another guest assertion:
;;;;
;;;;   QMP-HOLD — press a key while a modifier is GENUINELY held down. A plain
;;;;   send-key chord releases everything at once, too fast for the browser: it
;;;;   reads modifiers from the raw evdev KEYBOARD and the character from the cooked
;;;;   tty (fb-browser.lisp's poll loop refreshes Ctrl/Shift/Super first, then reads
;;;;   the byte). Release the modifier before the byte is read and the chord is lost
;;;;   — which is exactly how a Super-Space came back as a plain space.
;;;;
;;;;   REPL-PROBE — the guest serves the EDITING repl, so it echoes every character
;;;;   back wrapped in ANSI colour and a naive read returns echo, not answers. So we
;;;;   make the answer self-identifying: the form prints <<value>> and we look for
;;;;   that. The echo of the source can never match it, because the echo contains
;;;;   the literal ~s.
;;;;
;;;; TWO WAYS THIS TEST PREVIOUSLY PROVED NOTHING, both fixed below and both worth
;;;; remembering when writing the next one: the module index is ~19 rows and FITS on
;;;; an 800-line screen, so its viewport cannot move and every wheel assertion
;;;; passed vacuously; and drilling whatever happened to be under the cursor landed
;;;; on a three-line defparameter, which is shorter than its viewport and so cannot
;;;; scroll either. Hence the flat CL-USER list, and reaching a long definition
;;;; deliberately through the Spotter.

(require :sb-bsd-sockets)
(require :sb-posix)

(defparameter *root*
  (make-pathname :directory (pathname-directory *load-truename*))
  "The project root — this file lives there, so no path is hardcoded.")

(defparameter *ovmf-code* "/usr/share/OVMF/OVMF_CODE_4M.fd")
(defparameter *ovmf-vars* "/usr/share/OVMF/OVMF_VARS_4M.fd")

(defparameter *work*
  (format nil "/tmp/lol-check-scroll-~d/" (sb-posix:getpid)))

(defun path (name) (concatenate 'string *work* name))

;;; ---- QEMU ----------------------------------------------------------------

(defvar *qemu* nil)

(defun boot-guest ()
  "Boot iso_root headless with a QMP control socket and the net REPL forwarded."
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
   punctuation marks this test needs are spelled out."
  (loop for ch across string
        do (qmp-key (case ch
                      (#\- "minus") (#\Space "spc") (#\. "dot")
                      (t (string-downcase (string ch)))))
           (sleep cps)))

;;; ---- the net REPL: read the browser's own state --------------------------

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
  "Evaluate FORM-STRING in the guest and return its printed value as a string."
  (write-string (format nil "(format t \"~~%<<~~s>>~~%\" (ignore-errors ~a))" form-string)
                *repl*)
  (write-char #\Newline *repl*) (force-output *repl*)
  (or (between-markers (strip-ansi (repl-drain seconds))) "?"))

(defun probe-int (form-string)
  (let ((s (probe form-string)))
    (ignore-errors (parse-integer (string-trim " " s)))))

;;; ---- the assertions ------------------------------------------------------

(defvar *results* '())

(defun check (label ok) (push (cons label (and ok t)) *results*))

(defun state (label)
  "Read the focused pane's scroll state. Returns (SEL TOP TV-SCROLL POINT)."
  (let ((kind   (probe "(view-kind (pane-view (current-pane)))"))
        (sel    (probe-int "(pane-sel (current-pane))"))
        (top    (probe-int "(pane-top (current-pane))"))
        ;; NB: no "~" line continuations in these — that is a FORMAT directive, and
        ;; these are plain strings, so the tilde and newline would travel to the
        ;; guest inside the form and the reader would choke on them. (It did.)
        (scroll (probe-int "(if (pane-tv (current-pane)) (lol.textview::tv-scroll (pane-tv (current-pane))) -1)"))
        (point  (probe-int "(if (pane-tv (current-pane)) (lol.textview:tv-point-line (pane-tv (current-pane))) -1)")))
    (format t "~&  ~22a kind=~a sel=~a top=~a scroll=~a point=~a~%"
            label kind sel top scroll point)
    (finish-output)
    (list sel top scroll point)))

(defun main ()
  (dolist (f (list *ovmf-code* *ovmf-vars*))
    (unless (probe-file f) (format *error-output* "~&missing ~a~%" f) (sb-ext:exit :code 2)))
  (boot-guest)
  (format t "~&booting the guest…~%") (finish-output)
  (qmp-connect)
  (repl-connect)
  (format t "~&net REPL up~%") (finish-output)
  ;; quiet the kernel console so a late printk cannot repaint over the browser
  (write-string
   (concatenate 'string
                "(ignore-errors (with-open-file (o \"/proc/sys/kernel/printk\""
                " :direction :output :if-exists :append)"
                " (write-string \"1\" o) (terpri o)))")
   *repl*)
  (write-char #\Newline *repl*) (force-output *repl*)
  (repl-drain 2.0)
  (sleep 6)

  ;; 'l' — the FLAT CL-USER list, several hundred rows. NOT 'b' (the module index):
  ;; that is nineteen rows and fits on screen, so its viewport cannot move at all.
  (qmp-key "l") (sleep 0.2) (qmp-key "ret") (sleep 9)

  (format t "~&~%== A. list pane (the flat CL-USER package view) ==~%")
  (let* ((a0 (state "fresh")))
    (qmp-key "pgdn") (sleep 1.5)
    (let ((a1 (state "after PageDown")))
      (qmp-key "pgup") (sleep 1.5)
      (let ((a2 (state "after PageUp")))
        (qmp-wheel "down" 2) (sleep 0.5)
        (let ((a3 (state "after wheel-down x2")))
          (qmp-wheel "up" 1) (sleep 0.5)
          (let ((a4 (state "after wheel-up x1")))
            (qmp-key "down") (sleep 1.5)
            (let ((a5 (state "after down (re-attach)")))
              (check "list PageDown moves the selection down"
                     (and (first a0) (first a1) (> (first a1) (first a0))))
              (check "list PageUp returns it" (eql (first a2) (first a0)))
              (check "list wheel-down x2 moves the VIEWPORT +6"
                     (and (second a3) (second a2) (= (- (second a3) (second a2)) 6)))
              (check "list wheel leaves the SELECTION alone"
                     (eql (first a3) (first a2)))
              (check "list wheel-up x1 moves the viewport -3"
                     (and (second a4) (second a3) (= (- (second a3) (second a4)) 3)))
              ;; re-attaching scrolls only far enough to show the selected row — from
              ;; a viewport at 3 with the selection at 1 that is a viewport of 1, NOT
              ;; a jump back to the top
              (check "an arrow key re-attaches the viewport to the selection"
                     (and (first a5) (second a5) (first a4) (second a4)
                          (= (first a5) (1+ (first a4)))
                          (<= (second a5) (first a5))
                          (/= (second a5) (second a4))))))))))

  ;; Reach a LONG definition on purpose, through the Spotter — see the header.
  (format t "~&~%== B. source pane (a long definition, via the Spotter) ==~%")
  (qmp-hold "meta_l" "spc") (sleep 2.0)
  (qmp-type "draw-brow") (sleep 1.5)
  (qmp-key "ret") (sleep 3.5)
  (let ((b0 (state "fresh source")))
    (qmp-wheel "down" 3) (sleep 0.5)
    (let ((b1 (state "after wheel-down x3")))
      (qmp-wheel "up" 1) (sleep 0.5)
      (let ((b2 (state "after wheel-up x1")))
        (qmp-key "pgdn") (sleep 1.5)
        (let ((b3 (state "after PageDown")))
          (check "source wheel-down scrolls the viewport DOWN"
                 (and (third b0) (third b1) (> (third b1) (third b0))))
          (check "wheel-down x3 == 9 lines (3 x +wheel-lines+)"
                 (and (third b0) (third b1) (= (- (third b1) (third b0)) 9)))
          (check "source wheel-up scrolls back UP"
                 (and (third b2) (third b1) (< (third b2) (third b1))))
          (check "wheel-up x1 == 3 lines"
                 (and (third b1) (third b2) (= (- (third b1) (third b2)) 3)))
          (check "the wheel leaves point alone" (eql (fourth b0) (fourth b1)))
          (check "source PageDown moves point"
                 (and (fourth b3) (fourth b1) (/= (fourth b3) (fourth b1))))))))

  (format t "~&~%===== RESULTS =====~%")
  (let ((ok t))
    (dolist (r (reverse *results*))
      (format t "  ~52a ~a~%" (car r) (if (cdr r) "PASS" "FAIL"))
      (unless (cdr r) (setf ok nil)))
    (format t "===== ~a =====~%" (if ok "ALL PASS" "FAILURES"))
    (finish-output)
    ok))

(let ((ok nil))
  (unwind-protect
       (setf ok (handler-case (main)
                  (serious-condition (c)
                    (format *error-output* "~&check-scroll: ~a~%" c)
                    nil)))
    (kill-guest))
  (sb-ext:exit :code (if ok 0 1)))
