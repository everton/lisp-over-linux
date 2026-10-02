;;;; sysinfo-test.lisp — host test for initramfs/sysinfo.lisp (the systray data).
;;;;
;;;;   sbcl --script poc/sysinfo-test.lisp
;;;;
;;;; QEMU has no ACPI battery device, so the guest can never show a battery
;;;; and the battery code cannot be exercised by booting. Instead this builds
;;;; FAKE /sys/class/power_supply trees in a scratch directory — plain files
;;;; with the same names and contents the kernel's power_supply class exposes —
;;;; points *POWER-SUPPLY-ROOT* at each, and checks what comes back. The
;;;; shapes are copied from real machines: "charge" units (µAh/µA) are what
;;;; this laptop's BAT0 reports, "energy" units (µWh/µW) the other ACPI family.
;;;;
;;;; Prints one line per check and exits non-zero on any failure.

(load (merge-pathnames "../initramfs/sysinfo.lisp" *load-truename*))

(defvar *failures* 0)

(defun check (name got want)
  (if (equal got want)
      (format t "ok    ~a~%" name)
      (progn (incf *failures*)
             (format t "FAIL  ~a~%      got  ~s~%      want ~s~%" name got want))))

(defvar *scratch*
  (pathname (format nil "/tmp/sysinfo-test-~d/" (sb-unix:unix-getpid))))

(defun fake-supply (tree name &rest files)
  "Create TREE/NAME/ with FILES, a plist of filename strings to contents.
   Each file gets the trailing newline a real sysfs attribute has."
  (let ((dir (merge-pathnames (format nil "~a/~a/" tree name) *scratch*)))
    (ensure-directories-exist dir)
    (loop for (file value) on files by #'cddr
          do (with-open-file (s (merge-pathnames file dir) :direction :output
                                                           :if-exists :supersede)
               (format s "~a~%" value)))))

(defun use-tree (tree)
  (setf *power-supply-root*
        (namestring (merge-pathnames (format nil "~a/" tree) *scratch*)))
  (ensure-directories-exist *power-supply-root*))

(setf *battery-cache-seconds* 0)        ; every call re-reads

;;; 1. No supplies at all (QEMU, a desktop): no battery, tray shows only the clock.
(use-tree "empty")
(check "no battery -> NIL" (battery-status) nil)
(check "no battery -> only the clock item"
       (mapcar (lambda (i) (getf i :kind)) (tray-items)) '(:clock))
(setf *power-supply-root* "/nonexistent/power_supply/")
(check "missing root -> NIL, no error" (battery-status) nil)

;;; 2. Mains only (a desktop with an AC device, no battery): still no battery item.
(fake-supply "aconly" "AC" "type" "Mains" "online" "1")
(use-tree "aconly")
(check "AC but no battery -> NIL" (battery-status) nil)

;;; 3. Discharging, charge units, AC under a non-"AC" name (ADP1).
;;;    2,000,000 µAh left at 1,000,000 µA = 2 h = 120 min.
(fake-supply "dis" "ADP1" "type" "Mains" "online" "0")
(fake-supply "dis" "BAT0" "type" "Battery" "status" "Discharging" "present" "1"
             "capacity" "45" "charge_now" "2000000" "charge_full" "4400000"
             "current_now" "1000000")
(use-tree "dis")
(check "discharging plist"
       (battery-status)
       '(:percent 45 :status :discharging :ac nil :minutes-left 120 :name "BAT0"))
(check "discharging text" (battery-string (battery-status)) "BAT -45% 2:00")
(check "45% on battery is :warn" (battery-tone (battery-status)) :warn)

;;; 4. Charging, energy units. (50,000,000 - 20,000,000) µWh at 15,000,000 µW = 2 h.
(fake-supply "chg" "AC" "type" "Mains" "online" "1")
(fake-supply "chg" "BAT1" "type" "Battery" "status" "Charging"
             "capacity" "40" "energy_now" "20000000" "energy_full" "50000000"
             "power_now" "15000000")
(use-tree "chg")
(check "charging: ac, minutes to full"
       (let ((b (battery-status))) (list (getf b :ac) (getf b :minutes-left)))
       '(t 120))
(check "charging text" (battery-string (battery-status)) "BAT +40% 2:00")
(check "charging tone" (battery-tone (battery-status)) :charging)

;;; 5. Full with a trickle current (exactly what this laptop's BAT0 shows on AC):
;;;    no ETA, no sign.
(fake-supply "full" "AC" "type" "Mains" "online" "1")
(fake-supply "full" "BAT0" "type" "Battery" "status" "Full" "capacity" "100"
             "charge_now" "2306000" "charge_full" "2306000" "current_now" "1000")
(use-tree "full")
(check "full text" (battery-string (battery-status)) "BAT 100%")
(check "full: no minutes" (getf (battery-status) :minutes-left) nil)

;;; 6. Low, no capacity file (percent derived), zero rate (no estimate).
(fake-supply "low" "BAT0" "type" "Battery" "status" "Discharging"
             "charge_now" "300000" "charge_full" "2306000" "current_now" "0")
(use-tree "low")
(check "derived percent" (getf (battery-status) :percent) 13)
(check "low tone" (battery-tone (battery-status)) :low)
(check "zero rate -> no ETA" (battery-string (battery-status)) "BAT -13%")
(check "no Mains device, discharging -> :ac nil" (getf (battery-status) :ac) nil)

;;; 7. A wireless mouse's battery (scope=Device) and an empty bay are not THE battery.
(fake-supply "periph" "hid-mouse-battery" "type" "Battery" "scope" "Device"
             "status" "Discharging" "capacity" "5")
(fake-supply "periph" "BAT0" "type" "Battery" "present" "0" "status" "Unknown")
(use-tree "periph")
(check "device-scope and absent batteries ignored" (battery-status) nil)

;;; 8. The cache: a re-read inside the window returns the old answer, FRESH does not.
(use-tree "dis")
(setf *battery-cache-seconds* 60)
(battery-status :fresh t)
(fake-supply "dis" "BAT0" "capacity" "44")
(check "cached within window" (getf (battery-status) :percent) 45)
(check ":fresh re-reads" (getf (battery-status :fresh t) :percent) 44)
(use-tree "empty")
(check "changing the root bypasses the cache" (battery-status) nil)
(setf *battery-cache-seconds* 0)

;;; 9. The clock: the mode line's "%F %T", in local time by a fixed offset.
;;;    3,000,000,000 universal = 1995-01-25 05:20:00 UTC (checked with `date -u -d @`
;;;    on 3000000000 - 2208988800 = 791011200).
(let ((ut 3000000000))
  (set-utc-offset 0)
  (check "UTC" (clock-string ut) "1995-01-25 05:20:00")
  (set-utc-offset "-0300")
  (check "offset -0300 in seconds" *utc-offset* -10800)
  (check "Sao Paulo (-0300)" (clock-string ut) "1995-01-25 02:20:00")
  (set-utc-offset "+0530")
  (check "half-hour zone (+0530)" (clock-string ut) "1995-01-25 10:50:00")
  (set-utc-offset "-0600")
  (check "crosses midnight backwards" (clock-string ut) "1995-01-24 23:20:00")
  (let ((*clock-seconds* nil))
    (check "no seconds" (clock-string ut) "1995-01-24 23:20"))
  (check "bad offset signals"
         (handler-case (progn (set-utc-offset "-3") :no-error) (error () :error))
         :error))

;;; 10. Against the host's own clock and zone: our string must equal `date`'s.
(let* ((zone (string-trim '(#\Newline)
                          (with-output-to-string (s)
                            (sb-ext:run-program "/bin/date" '("+%z") :output s))))
       (ut   (get-universal-time))
       (want (string-trim '(#\Newline)
                          (with-output-to-string (s)
                            (sb-ext:run-program "/bin/date"
                                                (list "-d" (format nil "@~d" (- ut 2208988800))
                                                      "+%F %T")
                                                :output s)))))
  (set-utc-offset zone)
  (check (format nil "matches host `date` (zone ~a)" zone) (clock-string ut) want))

(sb-ext:delete-directory *scratch* :recursive t)
(format t "~%~:[all checks passed~;~:*~d FAILED~]~%"
        (and (plusp *failures*) *failures*))
(sb-ext:exit :code (if (plusp *failures*) 1 0))
