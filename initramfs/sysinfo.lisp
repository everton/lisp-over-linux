;;;; sysinfo.lisp — the DATA behind the browser's systray: battery and clock.
;;;;
;;;; The browser's tab strip ends, top right, in a small tray (the drawing
;;;; lives in the shell; this file only answers "what should it say?"). The
;;;; tray copies the user's StumpWM mode line on the host, field for field and
;;;; in the same order, so the two machines look alike:
;;;;
;;;;     BAT -45% 1:20  |  <wifi SSID>  |  2026-10-02 09:01:52  |  <tray icons>
;;;;
;;;; This module supplies the first and the third. (Wi-Fi waits on a Wi-Fi
;;;; driver the kernel does not have; the icons are the shell's.) The full
;;;; story, and the roadmap for the missing fields, is doc/systray.org.
;;;;
;;;; Two sources, both of them files or the kernel's own clock — no daemons,
;;;; no ioctls, no FFI:
;;;;
;;;;  * BATTERY: the kernel's power_supply class. ACPI_BATTERY and ACPI_AC
;;;;    (both built in since tag #19, when CONFIG_ACPI went on) each register
;;;;    a device under /sys/class/power_supply/, one directory per supply,
;;;;    with one plain-text file per property ("capacity" → "87\n"). Reading
;;;;    the battery is therefore reading a handful of tiny files. The kernel
;;;;    regenerates each value on read, from the firmware's _BST method, so a
;;;;    read is cheap but not free — hence the small cache below.
;;;;
;;;;  * CLOCK: GET-UNIVERSAL-TIME. The kernel set the system clock from the
;;;;    CMOS RTC at boot (x86 does that even without CONFIG_RTC_CLASS), and the
;;;;    laptop keeps its RTC in UTC. There is no time-zone database in the
;;;;    image, so local time is UTC plus *UTC-OFFSET*, which the build bakes
;;;;    in from the host's `date +%z`.
;;;;
;;;; Everything here degrades to "show less": no battery directory (QEMU, a
;;;; desktop) means no battery item, never an error. A tray that crashed the
;;;; browser would be a poor trade for a percentage.
;;;;
;;;; Host-testable: point *POWER-SUPPLY-ROOT* at a fake directory tree —
;;;; poc/sysinfo-test.lisp does exactly that.

;;; ------------------------------------------------------------------------
;;; Reading sysfs
;;; ------------------------------------------------------------------------

(defvar *power-supply-root* "/sys/class/power_supply/"
  "Directory holding one subdirectory per power supply (the kernel's
   power_supply class). A variable, not a constant, so a test can point it at
   a fake tree. Must end in a slash: it is merged as a directory pathname.")

(defun sysfs-string (dir file)
  "The first line of DIR/FILE with whitespace trimmed, or NIL if the file is
   missing or unreadable. sysfs attribute files hold one value and a newline.
   IGNORE-ERRORS because a supply can vanish between listing and reading it
   (a battery pulled out, a USB charger unplugged)."
  (ignore-errors
    (with-open-file (s (merge-pathnames file dir) :if-does-not-exist nil)
      (when s
        (let ((line (read-line s nil nil)))
          (and line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))))))

(defun sysfs-integer (dir file)
  "DIR/FILE parsed as an integer, or NIL. Power values are integers in micro
   units (µAh, µWh, µA, µW), and capacity is a plain percentage."
  (let ((s (sysfs-string dir file)))
    (and s (ignore-errors (parse-integer s)))))

(defun power-supplies ()
  "Every supply directory under *POWER-SUPPLY-ROOT*, sorted by name.
   :RESOLVE-SYMLINKS NIL matters: each entry in /sys/class/power_supply is a
   symlink into /sys/devices/..., and resolving it would hand back the long
   device path instead of the short name (BAT0, AC). Sorting makes \"the
   first battery\" deterministic — BAT0 before BAT1."
  (sort (ignore-errors
          (directory (merge-pathnames "*/" *power-supply-root*)
                     :resolve-symlinks nil))
        #'string< :key #'namestring))

(defun supply-type (dir)
  "The kernel's own classification: \"Battery\", \"Mains\", \"USB\", ...
   We go by this file and NOT by the directory name, because names are the
   firmware's choice — the AC adapter is AC on one laptop, ADP1 or ACAD on
   another."
  (sysfs-string dir "type"))

(defun system-battery-p (dir)
  "True if DIR is a battery that powers the MACHINE. A wireless mouse's or a
   phone's battery also shows up as type Battery, but with scope=Device;
   the laptop's own has no scope file (ACPI does not set one) or scope=System.
   A battery bay that is empty reports present=0 — skip that too."
  (and (equal (supply-type dir) "Battery")
       (not (equal (sysfs-string dir "scope") "Device"))
       (not (equal (sysfs-string dir "present") "0"))))

;;; ------------------------------------------------------------------------
;;; Battery
;;; ------------------------------------------------------------------------

(defun status-keyword (string)
  "The kernel's status text as a keyword. These five strings are the whole
   vocabulary (POWER_SUPPLY_STATUS_TEXT in drivers/power/supply/
   power_supply_sysfs.c)."
  (cond ((equal string "Charging")     :charging)
        ((equal string "Discharging")  :discharging)
        ((equal string "Full")         :full)
        ((equal string "Not charging") :not-charging)
        (t                             :unknown)))

(defun battery-amounts (dir)
  "Return (values NOW FULL RATE) for the battery in DIR, in whichever unit
   it reports, or NILs for what is missing.
   An ACPI battery reports in ONE of two unit families, chosen by the firmware
   (_BIF's power unit, drivers/acpi/battery.c): charge in µAh with a current
   in µA, or energy in µWh with a power in µW. Within a family the ratio
   NOW/RATE is hours either way, so we never need to convert between them —
   we only need to not mix them. The rate is reported unsigned on ACPI, but
   some drivers sign it, so take the absolute value."
  (let ((energy (sysfs-integer dir "energy_now")))
    (multiple-value-bind (now full rate)
        (if energy
            (values energy (sysfs-integer dir "energy_full") (sysfs-integer dir "power_now"))
            (values (sysfs-integer dir "charge_now") (sysfs-integer dir "charge_full")
                    (sysfs-integer dir "current_now")))
      (values now full (and rate (abs rate))))))

(defun minutes-left (status now full rate)
  "Minutes to empty while discharging, to full while charging, else NIL.
   This is the arithmetic `acpi -b` does for the host's mode line. A rate of
   zero (or a tiny trickle while 'Full', as the host's battery shows) means
   no meaningful estimate, so NIL rather than a ten-thousand-hour guess."
  (when (and now rate (> rate 0))
    (case status
      (:discharging (round (* 60 now) rate))
      (:charging    (and full (> full now) (round (* 60 (- full now)) rate))))))

(defun read-battery-status ()
  "Uncached read; see BATTERY-STATUS."
  (let* ((supplies  (power-supplies))
         (battery   (find-if #'system-battery-p supplies))
         (mains     (remove-if-not (lambda (d) (equal (supply-type d) "Mains"))
                                   supplies)))
    (when battery
      (let ((status (status-keyword (sysfs-string battery "status"))))
        (multiple-value-bind (now full rate) (battery-amounts battery)
          (list :percent      (or (sysfs-integer battery "capacity")
                                  ;; capacity is near-universal; derive it if absent
                                  (and now full (> full 0) (round (* 100 now) full)))
                :status       status
                ;; On mains? Ask the Mains supply if there is one; a laptop
                ;; without one is still certainly plugged in while charging.
                :ac           (if mains
                                  (some (lambda (d) (equal (sysfs-string d "online") "1"))
                                        mains)
                                  (and (member status '(:charging :full)) t))
                :minutes-left (minutes-left status now full rate)
                :name         (car (last (pathname-directory battery)))))))))

(defvar *battery-cache-seconds* 5
  "How long a battery reading is reused. The tray is redrawn every second
   (the clock ticks); the battery moves by a percent every few minutes, and
   each read makes the kernel run a firmware method. 0 disables the cache.")

(defvar *battery-cache* nil
  "(TIME ROOT . PLIST) of the last read: internal-real-time it was taken, the
   *POWER-SUPPLY-ROOT* it was taken from (so a test that repoints the root is
   never served a stale answer), and the result — NIL is a valid cached answer.")

(defun battery-status (&key fresh)
  "The machine's battery as a plist, or NIL if there is none:
     (:percent 87 :status :discharging :ac nil :minutes-left 192 :name \"BAT0\")
   :STATUS is one of :charging :discharging :full :not-charging :unknown.
   :AC is T on mains power. :MINUTES-LEFT is to empty (discharging) or to full
   (charging), or NIL when there is no estimate. :PERCENT may be NIL on a
   battery that reports neither capacity nor charge.
   Cached for *BATTERY-CACHE-SECONDS*; FRESH forces a re-read. Never signals."
  (let ((now (get-internal-real-time))
        (c   *battery-cache*))
    (if (and (not fresh) c
             (equal (second c) *power-supply-root*)
             (< (- now (first c))
                (* *battery-cache-seconds* internal-time-units-per-second)))
        (cddr c)
        (let ((b (ignore-errors (read-battery-status))))
          (setf *battery-cache* (list* now *power-supply-root* b))
          b))))

(defun battery-tone (b)
  "The colour class of a battery reading, as the host's mode line picks it
   (~/.local/bin/battery-modeline): on battery it goes :LOW below 20% (red),
   :WARN below 50% (yellow), else :OK (green); plugged in it is :CHARGING
   (also green there — the shell may give it a colour of its own)."
  (let ((pct (or (getf b :percent) 100)))
    (if (member (getf b :status) '(:discharging :unknown))
        (cond ((< pct 20) :low)
              ((< pct 50) :warn)
              (t          :ok))
        :charging)))

(defun battery-string (b)
  "The mode line's battery text: \"BAT -45% 1:20\", \"BAT +37% 3:13\",
   \"BAT 100%\". '+' charging, '-' discharging, then H:MM to empty/full."
  (let ((sign (case (getf b :status) (:charging "+") (:discharging "-") (t "")))
        (mins (getf b :minutes-left))
        (pct  (getf b :percent)))
    (format nil "BAT ~a~:[--~;~:*~d%~]~@[ ~a~]"
            sign pct
            (and mins (format nil "~d:~2,'0d" (floor mins 60) (mod mins 60))))))

;;; ------------------------------------------------------------------------
;;; Clock
;;; ------------------------------------------------------------------------

(defvar *utc-offset* 0
  "Local time's offset from UTC in SECONDS EAST (the sign `date +%z` uses):
   -10800 for Brazil's -0300. The image carries no zoneinfo database, so this
   one number IS the time zone. build.sh bakes the host's offset in with
   SET-UTC-OFFSET; change it live with (set-utc-offset \"+0100\").
   It is a fixed offset, so it does not follow daylight saving — a zone with
   DST will be an hour off for half the year until reset (doc/systray.org).")

(defun set-utc-offset (offset)
  "Set *UTC-OFFSET* from an integer (seconds east) or a `date +%z` string
   such as \"-0300\" or \"+0530\". Returns the new value in seconds."
  (setf *utc-offset*
        (etypecase offset
          (integer offset)
          (string
           (let* ((s     (string-trim " " offset))
                  (sign  (if (char= (char s 0) #\-) -1 1))
                  (digits (string-left-trim "+-" s)))
             (assert (and (= (length digits) 4) (every #'digit-char-p digits)) ()
                     "UTC offset ~s is not in +HHMM form" offset)
             (* sign (+ (* 3600 (parse-integer digits :end 2))
                        (* 60   (parse-integer digits :start 2)))))))))

(defvar *clock-seconds* t
  "Show seconds, as the host's mode line does (\"%F %T\"). The shell must then
   repaint the tray once a second; set NIL for \"2026-10-02 09:01\" and a
   once-a-minute repaint.")

(defun clock-string (&optional (universal-time (get-universal-time)))
  "UNIVERSAL-TIME as local time in the host mode line's format —
   StumpWM's *time-modeline-string* is \"%F %T\", i.e. \"2026-10-02 09:01:52\".
   DECODE-UNIVERSAL-TIME takes the zone as HOURS WEST of Greenwich (the
   opposite sign of `date +%z`, and a rational for half-hour zones), and
   passing it explicitly is what keeps the libc/zoneinfo lookup — which has
   nothing to find in the image — out of the picture."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time universal-time (- (/ *utc-offset* 3600)))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d~:[~;~:*:~2,'0d~]"
            year month day hour min (and *clock-seconds* sec))))

;;; ------------------------------------------------------------------------
;;; The tray
;;; ------------------------------------------------------------------------

(defun tray-items ()
  "What the tray shows, LEFT TO RIGHT, in the host mode line's order: a list
   of plists, each with :KIND and :TEXT, e.g.
     ((:kind :battery :text \"BAT -45% 1:20\" :tone :warn :data (...))
      (:kind :clock   :text \"2026-10-02 09:01:52\"))
   :TONE (battery only) is :OK :WARN :LOW or :CHARGING — see BATTERY-TONE.
   :DATA is the raw BATTERY-STATUS plist, for a tooltip or a fancier glyph.
   No battery, no battery item. Never signals: a failing field drops out."
  (let ((b (battery-status)))
    (remove nil
            (list (when b
                    (list :kind :battery :text (battery-string b)
                          :tone (battery-tone b) :data b))
                  ;; the host's field 2, the Wi-Fi SSID (or the wired link): wifi.lisp
                  ;; loads later, so it is called by name, and only reads cached state
                  (and (fboundp 'network-tray-item)
                       (ignore-errors (funcall 'network-tray-item)))
                  ;; the agent's authority and the session's cost (initramfs/agent-ui.lisp,
                  ;; loaded later — doc/agent.org §8.9: a meter that is always on screen)
                  (and (fboundp 'agent-tray-item) (ignore-errors (funcall 'agent-tray-item)))
                  ;; a recording in progress (initramfs/recorder.lisp): REC 01:23 and the mic
                  (and (fboundp 'recorder-tray-item) (ignore-errors (funcall 'recorder-tray-item)))
                  ;; the sound card's volume (initramfs/mixer.lisp, tag #29): nothing
                  ;; without a card
                  (and (fboundp 'sound-tray-item) (ignore-errors (funcall 'sound-tray-item)))
                  (let ((c (ignore-errors (clock-string))))
                    (and c (list :kind :clock :text c)))))))
