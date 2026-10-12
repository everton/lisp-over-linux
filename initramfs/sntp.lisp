;;;; sntp.lisp — ask the network what time it is (SNTP, RFC 4330), and fix the clock.
;;;;
;;;; Why bother: TLS (http.lisp over tls.lisp) checks that a server's certificate is
;;;; valid NOW — "not before" / "not after" dates — so a clock that is wrong by a year
;;;; makes every HTTPS connection fail with a confusing certificate error.
;;;;
;;;; Where our time comes from at boot: the PC's battery-backed RTC (the CMOS clock).
;;;; x86 Linux reads it once at boot even with CONFIG_RTC_CLASS off (doc/systray.org),
;;;; and it holds UTC on this laptop. Under QEMU the emulated RTC starts at the host's
;;;; time. So the clock is usually RIGHT already, and this file's main job is to
;;;; REPORT drift; it corrects only when the drift is worth correcting
;;;; (*CLOCK-STEP-THRESHOLD*, default 2 s) — an RTC whose battery died, a laptop that
;;;; spent a month off.
;;;;
;;;; The protocol is one UDP datagram each way, port 123, 48 bytes:
;;;;   byte 0      LI (2 bits) | version (3 bits, = 4) | mode (3 bits, 3 = client)
;;;;   bytes 24-31 ORIGINATE  — the server copies our transmit time here
;;;;   bytes 32-39 RECEIVE    — when the request reached the server   (T2)
;;;;   bytes 40-47 TRANSMIT   — when the reply left the server        (T3)
;;;; Timestamps are 32 bits of seconds since 1900-01-01 + 32 bits of fraction. 1900 is
;;;; exactly Common Lisp's universal-time epoch — a happy accident worth noticing.
;;;; With T1 = when we sent and T4 = when the reply arrived (our clock):
;;;;     offset = ((T2 - T1) + (T3 - T4)) / 2       round trip = (T4 - T1) - (T3 - T2)
;;;; i.e. assume the trip took as long each way, and see how far our clock is off.
;;;;
;;;; Setting the clock needs CAP_SYS_TIME. PID 1 runs as the user with five
;;;; capabilities and that is not one of them (doc/storage.org §5) — on purpose: a
;;;; live-editable image should not hold it. So the one write goes through lol-rootd as
;;;; a single queued signal carrying the new time (preinit.c, ROOTD_SIG_SETTIME). A
;;;; diskless boot still runs as root and sets it directly.
;;;;
;;;; The honest weakness, said out loud (agent.org §0.4): SNTP is unauthenticated, so
;;;; anyone on the path can lie about the time, and we then use that time to judge
;;;; certificates. Universally accepted, still a weakness; the RTC is the cross-check
;;;; (we refuse to step to a time before 2026, and lol-rootd refuses it too).

(defconstant +ntp-unix-delta+ 2208988800 "Seconds from 1900-01-01 to 1970-01-01.")
(defvar *ntp-servers* '("pool.ntp.org" "time.cloudflare.com")
  "Fallback NTP servers, used when the lease names none (DHCP option 42).")
(defvar *clock-step-threshold* 2 "Correct the clock only when it is off by more seconds than this.")
(defvar *clock-report* nil
  "The last SNTP result as a plist: :server :offset (seconds, + = we are behind)
   :delay :at (universal time) :action (:ok :stepped :step-failed :no-answer).")

(defun now-ntp ()
  "The current time in NTP seconds (a rational, sub-second precision)."
  (multiple-value-bind (sec usec) (sb-ext:get-time-of-day)
    (+ sec +ntp-unix-delta+ (/ usec 1000000))))

(defun ntp-timestamp-at (v i)
  "The 64-bit NTP timestamp at offset I of V as a rational number of seconds."
  (+ (u32-at v i) (/ (u32-at v (+ i 4)) (expt 2 32))))

(defun sntp-query (server &key (timeout 2))
  "Ask SERVER (an address vector) for the time. Returns (values offset delay) in
   seconds, or NIL on timeout / a bogus reply (a 'kiss of death' stratum 0, say)."
  (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp))
        (req (make-array 48 :element-type '(unsigned-byte 8) :initial-element 0))
        (buf (make-array 64 :element-type '(unsigned-byte 8))))
    (unwind-protect
         (progn
           (setf (aref req 0) #x23)                   ; LI 0, version 4, mode 3 (client)
           (set-rcv-timeout (sb-bsd-sockets:socket-file-descriptor s) timeout)
           (sb-bsd-sockets:socket-connect s server 123)
           (let* ((t1 (now-ntp))
                  (secs (floor t1)) (frac (floor (* (- t1 secs) (expt 2 32)))))
             (loop for k from 0 below 4                ; our transmit time -> bytes 40-47
                   do (setf (aref req (+ 40 k)) (ldb (byte 8 (* 8 (- 3 k))) secs)
                            (aref req (+ 44 k)) (ldb (byte 8 (* 8 (- 3 k))) frac)))
             (sb-bsd-sockets:socket-send s req 48)
             (multiple-value-bind (b len) (ignore-errors (sb-bsd-sockets:socket-receive s buf 64))
               (declare (ignore b))
               (let ((t4 (now-ntp)))
                 (when (and len (>= len 48)
                            (= (logand (aref buf 0) 7) 4)                 ; mode 4 = server
                            (plusp (aref buf 1))                          ; stratum 1..15
                            (= (u32-at buf 24) secs))                     ; it answers US
                   (let ((t2 (ntp-timestamp-at buf 32)) (t3 (ntp-timestamp-at buf 40)))
                     (values (/ (+ (- t2 t1) (- t3 t4)) 2)
                             (- (- t4 t1) (- t3 t2)))))))))
      (ignore-errors (sb-bsd-sockets:socket-close s)))))

;;; ---- setting the clock -------------------------------------------------------

(sb-alien:define-alien-routine ("clock_settime" %clock-settime) sb-alien:int
  (clock sb-alien:int) (ts (* (sb-alien:array sb-alien:long 2))))
;; sigqueue(pid, sig, union sigval): the union is one machine word passed in a
;; register (x86-64 SysV ABI), so declaring it a LONG is the same call.
(sb-alien:define-alien-routine ("sigqueue" %sigqueue) sb-alien:int
  (pid sb-alien:int) (sig sb-alien:int) (value sb-alien:long))

(defconstant +rootd-sig-settime+ 40 "preinit.c's ROOTD_SIG_SETTIME — a fixed number, not SIGRTMIN+n.")

(defun rootd-pid ()
  (let ((p (sb-ext:posix-getenv "LOL_ROOTD_PID")))
    (and p (ignore-errors (parse-integer p)))))

(defun set-system-clock (unix-ms)
  "Set the wall clock to UNIX-MS (milliseconds since 1970). As root: clock_settime
   directly. As the user: ask lol-rootd (one queued signal). Returns :stepped, or
   :step-failed with a reason."
  (cond
    ((zerop (sb-unix:unix-getuid))
     (let ((ts (sb-alien:make-alien (sb-alien:array sb-alien:long 2))))
       (unwind-protect
            (progn (setf (sb-alien:deref (sb-alien:deref ts) 0) (floor unix-ms 1000)
                         (sb-alien:deref (sb-alien:deref ts) 1) (* (mod unix-ms 1000) 1000000))
                   (if (zerop (%clock-settime 0 ts)) :stepped
                       (values :step-failed (sb-int:strerror (sb-alien:get-errno)))))
         (sb-alien:free-alien ts))))
    ((rootd-pid)
     (let ((before (get-universal-time)))
       (if (zerop (%sigqueue (rootd-pid) +rootd-sig-settime+ unix-ms))
           ;; lol-rootd acts on its next wake-up (a signal cuts its 1 s sleep short):
           ;; wait until the clock has visibly moved, or give up after 2 s.
           (loop repeat 20 do (sleep 0.1)
                 when (> (abs (- (get-universal-time) before)) *clock-step-threshold*)
                   return :stepped
                 finally (return (values :step-failed "lol-rootd did not act (see dmesg)")))
           (values :step-failed (sb-int:strerror (sb-alien:get-errno))))))
    (t (values :step-failed "not root and no lol-rootd to ask"))))

;;; ---- the whole job ------------------------------------------------------------

(defun ntp-candidates ()
  "Addresses to ask: the lease's NTP servers, then the fallback names resolved."
  (append (let ((l (and (fboundp 'current-lease) (current-lease))))
            (and l (mapcar (lambda (o) (coerce o 'vector)) (lease-ntp l))))
          (loop for name in *ntp-servers*
                append (ignore-errors (remove-if-not (lambda (a) (= (length a) 4))
                                                     (resolve-addresses name))))))

(defun sync-clock (&key (set t))
  "Measure our clock against the network and, when SET and it is off by more than
   *CLOCK-STEP-THRESHOLD*, step it. Records and returns *CLOCK-REPORT*."
  (setf *clock-report*
        (or (loop for server in (ntp-candidates) repeat 4
                  do (multiple-value-bind (offset delay) (ignore-errors (sntp-query server))
                       (when offset
                         (let ((action :ok) (why nil))
                           (when (and set (> (abs offset) *clock-step-threshold*))
                             (multiple-value-setq (action why)
                               (set-system-clock (round (* 1000 (- (+ (now-ntp) offset) +ntp-unix-delta+))))))
                           (return (list :server (address-string server)
                                         :offset (float offset 1d0) :delay (float delay 1d0)
                                         :at (get-universal-time) :action action :why why))))))
            (list :at (get-universal-time) :action :no-answer))))

(defun describe-clock ()
  "One line about the clock: \"clock: 0.012 s ahead of 10.0.2.3 (ok)\"."
  (let ((r *clock-report*))
    (if (or (null r) (eq (getf r :action) :no-answer))
        "clock: not checked against the network (no NTP answer yet)"
        (format nil "clock: ~,3f s ~:[ahead of~;behind~] ~a (~(~a~)~@[: ~a~])"
                (abs (getf r :offset)) (plusp (getf r :offset)) (getf r :server)
                (getf r :action) (getf r :why)))))

(defun sync-clock-on-lease (lease)
  "The *LEASE-HOOK*: check the clock in the background (DNS + NTP can take seconds,
   and a lease is applied on the boot path)."
  (declare (ignore lease))
  (sb-thread:make-thread (lambda () (ignore-errors (sync-clock))) :name "sntp"))

(pushnew 'sync-clock-on-lease *lease-hooks*)
