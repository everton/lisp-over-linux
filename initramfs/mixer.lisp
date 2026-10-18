;;;; mixer.lisp — the sound card's mixer, from Lisp: ALSA "controls" through ioctl.
;;;;
;;;; Tag #29 gave the kernel sound (doc/screencast.org §3). A sound card is more than
;;;; the PCM devices that play and record (/dev/snd/pcmC0D0p, pcmC0D0c): its codec has
;;;; amplifiers, switches and selectors, and the kernel shows each one as a CONTROL on
;;;; /dev/snd/controlC0 — a numbered element with a name ("Master Playback Volume"), a
;;;; type (boolean, integer, enumerated), a count (2 for a stereo pair) and, for an
;;;; integer, a range. That is all `alsamixer' shows; PulseAudio's `pactl
;;;; set-sink-volume' ends up writing the same controls, through a sound server we do
;;;; not run. Here, four ioctls do it (include/uapi/sound/asound.h):
;;;;
;;;;   SNDRV_CTL_IOCTL_ELEM_LIST   how many controls, and their ids
;;;;   SNDRV_CTL_IOCTL_ELEM_INFO   one control's type, count and range
;;;;   SNDRV_CTL_IOCTL_ELEM_READ   its current values
;;;;   SNDRV_CTL_IOCTL_ELEM_WRITE  new values
;;;;
;;;; The structs are filled as raw bytes at offsets measured with a C compiler against
;;;; that header (2026-10-17, x86-64): snd_ctl_elem_id 64 bytes (numid @0, iface @4,
;;;; device @8, subdevice @12, name[44] @16, index @60); snd_ctl_elem_list 80 (offset
;;;; @0, space @4, used @8, count @12, pids @16); snd_ctl_elem_info 272 (type @64,
;;;; access @68, count @72, value.integer.min/max/step @80/@88/@96); snd_ctl_elem_value
;;;; 1224 (value.integer.value[i], a long each, @72 + 8i — booleans use it too).
;;;;
;;;; On top: the VOLUME KEYS (the host's StumpWM binds XF86AudioRaiseVolume,
;;;; XF86AudioLowerVolume and XF86AudioMute to pactl; fb-browser.lisp sees the same
;;;; keys on the keyboard's evdev, KEY_VOLUMEUP 115 / KEY_VOLUMEDOWN 114 / KEY_MUTE 113,
;;;; plus KEY_MICMUTE 248), the tray's VOL item, the level kept in ~/.lol/settings, and
;;;; RESTORE-AUDIO at boot: no alsactl or WirePlumber runs here to set the codec's
;;;; switches, so we do. Never signals: no card means every function answers NIL.
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

(defparameter *mixer-card* 0 "The card whose mixer we drive: /dev/snd/controlC<N>.")

(defconstant +ctl-elem-list+  #xc0505510)
(defconstant +ctl-elem-info+  #xc1105511)
(defconstant +ctl-elem-read+  #xc4c85512)
(defconstant +ctl-elem-write+ #xc4c85513)
(defconstant +ctl-type-boolean+ 1)
(defconstant +ctl-type-integer+ 2)

(sb-alien:define-alien-routine ("ioctl" %snd-ioctl) sb-alien:int
  (fd sb-alien:int) (request sb-alien:unsigned-long) (argp sb-sys:system-area-pointer))

(defun control-device () (format nil "/dev/snd/controlC~d" *mixer-card*))

(defun sound-card-p ()
  "Is there a sound card at all? (No card before tag #29, or none in a QEMU without
   -device intel-hda.)"
  (and (probe-file (control-device)) t))

(defmacro with-control-fd ((fd &optional (mode 'sb-unix:o_rdwr)) &body body)
  "Run BODY with FD open on the card's control device, or return NIL if it cannot be
   opened (no card, or lol-rootd has not given /dev/snd to the user yet)."
  `(let ((,fd (sb-unix:unix-open (control-device) ,mode 0)))
     (when ,fd
       (unwind-protect (progn ,@body) (sb-unix:unix-close ,fd)))))

(defun ctl-ioctl (fd request buf)
  "One ioctl on BUF, a byte vector; T on success."
  (sb-sys:with-pinned-objects (buf)
    (zerop (%snd-ioctl fd request (sb-sys:vector-sap buf)))))

;;; little-endian fields in a byte vector (x86-64)
(defun snd-u32 (buf off) (logior (aref buf off) (ash (aref buf (+ off 1)) 8)
                             (ash (aref buf (+ off 2)) 16) (ash (aref buf (+ off 3)) 24)))
(defun (setf snd-u32) (v buf off)
  (dotimes (i 4 v) (setf (aref buf (+ off i)) (ldb (byte 8 (* 8 i)) v))))
(defun snd-s64 (buf off)
  (let ((v (loop for i below 8 sum (ash (aref buf (+ off i)) (* 8 i)))))
    (if (logbitp 63 v) (- v (ash 1 64)) v)))
(defun (setf snd-s64) (v buf off)
  (let ((u (ldb (byte 64 0) v)))
    (dotimes (i 8 v) (setf (aref buf (+ off i)) (ldb (byte 8 (* 8 i)) u)))))

(defun snd-bytes (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

(defun snd-id-name (buf off)
  "The NUL-terminated name[44] of the snd_ctl_elem_id at OFF."
  (let ((end (or (position 0 buf :start (+ off 16) :end (+ off 60)) (+ off 60))))
    (map 'string #'code-char (subseq buf (+ off 16) end))))

;;; ---- listing and reading ------------------------------------------------------

(declaim (ftype function control-info))   ; below: it reads one control

(defun mixer-controls ()
  "Every control of the card, as plists
     (:numid N :name \"Master Playback Volume\" :index 0 :type :integer :count 2
      :min 0 :max 87 :values (60 60))
   or NIL with no card. :TYPE is :BOOLEAN, :INTEGER or another keyword we leave alone."
  (with-control-fd (fd)
    (let ((list (snd-bytes 80)))
      (when (ctl-ioctl fd +ctl-elem-list+ list)        ; space 0: just the count
        (let* ((n (snd-u32 list 12))
               (ids (snd-bytes (* 64 n))))
          (setf (snd-u32 list 4) n)                        ; space
          (sb-sys:with-pinned-objects (ids)
            (setf (snd-s64 list 16) (sb-sys:sap-int (sb-sys:vector-sap ids)))
            (ctl-ioctl fd +ctl-elem-list+ list))
          (loop for i below (snd-u32 list 8)               ; used
                for numid = (snd-u32 ids (* 64 i))
                for info = (control-info fd numid)
                when info collect info))))))

(defun control-info (fd numid)
  "ELEM_INFO + ELEM_READ for control NUMID, as MIXER-CONTROLS' plist."
  (let ((info (snd-bytes 272)))
    (setf (snd-u32 info 0) numid)
    (when (ctl-ioctl fd +ctl-elem-info+ info)
      (let* ((type (snd-u32 info 64))
             (count (snd-u32 info 72))
             (plist (list :numid numid :name (snd-id-name info 0) :index (snd-u32 info 60)
                          :type (case type (1 :boolean) (2 :integer) (3 :enumerated)
                                  (t (intern (format nil "TYPE-~d" type) :keyword)))
                          :count count)))
        (when (= type +ctl-type-integer+)
          (setf plist (append plist (list :min (snd-s64 info 80) :max (snd-s64 info 88)))))
        (when (member type (list +ctl-type-boolean+ +ctl-type-integer+))
          (let ((v (snd-bytes 1224)))
            (setf (snd-u32 v 0) numid)
            (when (ctl-ioctl fd +ctl-elem-read+ v)
              (setf plist (append plist
                                  (list :values (loop for i below (min count 128)
                                                      collect (snd-s64 v (+ 72 (* 8 i))))))))))
        plist))))

(defun find-control (name &optional (index 0))
  "The control called NAME (exactly, as ALSA spells it), or NIL."
  (find-if (lambda (c) (and (string= (getf c :name) name) (eql (getf c :index) index)))
           (mixer-controls)))

(defun mixer-write (control values)
  "Write VALUES (one per channel; a single value is used for all) to CONTROL, a plist
   from MIXER-CONTROLS. Integers are clamped to the control's range. T on success."
  (when control
    (with-control-fd (fd)
      (let ((v (snd-bytes 1224))
            (lo (getf control :min 0)) (hi (getf control :max 1)))
        (setf (snd-u32 v 0) (getf control :numid))
        (dotimes (i (getf control :count))
          (let ((x (if (listp values) (or (nth i values) (car (last values))) values)))
            (setf (snd-s64 v (+ 72 (* 8 i))) (max lo (min hi x)))))
        (ctl-ioctl fd +ctl-elem-write+ v)))))

;;; ---- volume, the way a desktop shows it ----------------------------------------

(defparameter *volume-controls*
  '("Master Playback Volume" "PCM Playback Volume"
    "Headphone Playback Volume" "Speaker Playback Volume")
  "Where \"the volume\" lives, first found wins: Master on a real codec (the ALC289
   has one), PCM or a single output on simpler ones (QEMU's hda-duplex).")

(defparameter *volume-step* 5 "Percent per volume-key press.")

(defun volume-control (&optional (controls (mixer-controls)))
  (loop for name in *volume-controls*
        thereis (find name controls :key (lambda (c) (getf c :name)) :test #'string=)))

(defun mute-control (&optional (controls (mixer-controls)))
  "The switch beside the volume control: \"Master Playback Switch\" for Master."
  (let ((vol (volume-control controls)))
    (and vol
         (let ((name (concatenate 'string
                                  (subseq (getf vol :name) 0 (search " Volume" (getf vol :name)))
                                  " Switch")))
           (find name controls :key (lambda (c) (getf c :name)) :test #'string=)))))

(defun control-percent (c)
  "The mean of C's channels as 0-100 of its range."
  (let ((lo (getf c :min)) (hi (getf c :max)) (vs (getf c :values)))
    (if (and vs (> hi lo))
        (round (* 100 (- (/ (reduce #'+ vs) (length vs)) lo)) (- hi lo))
        0)))

(defvar *volume-intent* nil
  "(PERCENT . RAW): the level SET-VOLUME-PERCENT last asked for, and the raw control
   value it became. A codec has fewer steps than there are percents (the ALC289's
   Master has 88, QEMU's codec fewer), so percent -> raw -> percent does not come back
   where it started, and stepping from the read-back value drifts: 55 % + 5 gave 59 %,
   then - 5 gave 54 % (check-sound.lisp, 2026-10-18). So while the card still holds
   RAW, the volume IS that percent; if something else moved the control, the card wins.")

(defun volume-percent ()
  "The volume as 0-100, or NIL with no card."
  (let ((c (volume-control)))
    (and c
         (let ((intent *volume-intent*))
           (if (and intent (every (lambda (v) (= v (cdr intent))) (getf c :values)))
               (car intent)
               (control-percent c))))))

(defun volume-muted-p ()
  "T when the volume's switch is off (a boolean control: 1 = sound on)."
  (let ((s (mute-control)))
    (and s (every #'zerop (getf s :values)))))

(defun set-volume-percent (p)
  "Set the volume to P percent (clamped to 0-100). The new percent, or NIL."
  (let ((c (volume-control)))
    (when c
      (let* ((p (max 0 (min 100 p)))
             (lo (getf c :min)) (hi (getf c :max))
             (raw (+ lo (round (* p (- hi lo)) 100))))
        (when (mixer-write c raw)
          (setf *volume-intent* (cons p raw))
          p)))))

(defun set-muted (muted)
  (let ((s (mute-control)))
    (and s (mixer-write s (if muted 0 1)))))

(defun mic-switch ()
  (find "Capture Switch" (mixer-controls) :key (lambda (c) (getf c :name)) :test #'string=))

;;; ---- boot: what alsactl would have done ------------------------------------------

(defparameter *default-volume* 50 "The volume on a first boot, in percent.")

(defun restore-audio ()
  "Put the card in a usable state, as Debian's alsactl / WirePlumber would: every
   playback and capture SWITCH on (so nothing is silent by accident — the HD-Audio
   driver's own boot defaults are not a promise), every other output volume that sits
   at its minimum raised to its maximum (Master stays the one knob), the capture
   volume likewise, and Master set to the user's saved level (:VOLUME in
   ~/.lol/settings, else *DEFAULT-VOLUME*), muted if :MUTED was saved. Logs what it
   did. NIL with no card. Called by the supervisor at boot."
  (let ((controls (mixer-controls)))
    (when controls
      (let ((vol (volume-control controls)) (changed '()))
        (dolist (c controls)
          (let ((name (getf c :name)))
            (cond ((and (eq (getf c :type) :boolean)
                        (or (search "Playback Switch" name) (search "Capture Switch" name))
                        (some #'zerop (getf c :values)))
                   (mixer-write c 1) (push name changed))
                  ((and (eq (getf c :type) :integer) (not (eq c vol))
                        (or (search "Playback Volume" name) (string= name "Capture Volume"))
                        (every (lambda (v) (= v (getf c :min))) (getf c :values)))
                   (mixer-write c (getf c :max)) (push name changed)))))
        (let ((level (if (fboundp 'setting) (funcall 'setting :volume *default-volume*)
                         *default-volume*)))
          (set-volume-percent (if (integerp level) level *default-volume*)))
        (when (and (fboundp 'setting) (funcall 'setting :muted nil))
          (set-muted t))
        (when (fboundp 'session-log)
          (funcall 'session-log "sound: card ~d, ~d controls; volume ~a% via ~s~@[, muted~]; switched on / raised: ~{~a~^, ~}"
                   *mixer-card* (length controls) (volume-percent) (getf vol :name)
                   (volume-muted-p) (reverse changed)))
        t))))

;;; ---- the keys ---------------------------------------------------------------------

(defun remember-volume ()
  (when (fboundp 'save-setting)
    (funcall 'save-setting :volume (volume-percent))
    (funcall 'save-setting :muted (volume-muted-p))))

(defun audio-key (key)
  "KEY is :VOLUME-UP, :VOLUME-DOWN, :MUTE or :MIC-MUTE (fb-browser.lisp reads them
   from the keyboard's evdev). Acts on the mixer and returns the message for the
   status line, e.g. \"Volume 55%\"."
  (cond
    ((not (sound-card-p)) "No sound card (tag #29 or later, and lol-rootd's /dev/snd)")
    ((eq key :mic-mute)
     (let ((s (mic-switch)))
       (if (not s) "No microphone switch on this card"
           (let ((on (some #'plusp (getf s :values))))
             (mixer-write s (if on 0 1))
             (if on "Microphone muted" "Microphone on")))))
    ((eq key :mute)
     (set-muted (not (volume-muted-p)))
     (remember-volume)
     (if (volume-muted-p) "Volume muted" (format nil "Volume ~d%" (volume-percent))))
    (t
     (let ((p (volume-percent)))
       (if (not p) "No volume control on this card"
           (progn
             (set-volume-percent (+ p (if (eq key :volume-up) *volume-step* (- *volume-step*))))
             ;; a volume key unmutes, as on the host's desktop
             (when (volume-muted-p) (set-muted nil))
             (remember-volume)
             (format nil "Volume ~d%" (volume-percent))))))))

(defvar *sound-tray-cache* nil "(TIME . ITEM): the tray asks once a second; the card is read at most every 2 s.")

(defun sound-tray-item ()
  "The tray's VOL field (sysinfo.lisp's TRAY-ITEMS), or NIL with no card."
  (let ((now (get-universal-time)))
    (if (and *sound-tray-cache* (< (- now (car *sound-tray-cache*)) 2))
        (cdr *sound-tray-cache*)
        (cdr (setf *sound-tray-cache*
                   (cons now
                         (let ((p (ignore-errors (volume-percent))))
                           (and p (list :kind :volume
                                        :text (if (ignore-errors (volume-muted-p))
                                                  "VOL mute"
                                                  (format nil "VOL ~d%" p)))))))))))

(defun forget-sound-tray ()
  "A key just changed the volume: show it now, not in 2 s."
  (setf *sound-tray-cache* nil))
