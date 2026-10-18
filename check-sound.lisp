;;;; check-sound.lisp — assert SOUND in the booted guest (tag #29).
;;;;
;;;;     LOL_GUEST_HOSTROOT=1 LOL_PORT=4019 sbcl --script check-sound.lisp
;;;;
;;;; QEMU gets TWO HD-Audio cards, because its wav backend only records what the guest
;;;; PLAYS — a capture stream on it never delivers a sample, and ffmpeg waits forever
;;;; (found 2026-10-17: exit 143 at the timeout). So:
;;;;     card 0  intel-hda + hda-duplex on audiodev none   — records silence; the mixer
;;;;     card 1  ich9-intel-hda + hda-output on audiodev wav — plays into WORK/out.wav
;;;; (QEMU's own codecs, driven by the kernel's generic HDA parser, SND_HDA_GENERIC —
;;;; not the laptop's Realtek, which is one card: there it is plughw:0,0 both ways.)
;;;; LOL_GUEST_HOSTROOT=1 gives the container the build host's ffmpeg and mplayer. Checks (doc/screencast.org):
;;;;
;;;;   - the kernel found a card; /dev/snd's nodes belong to PID 1's user;
;;;;   - mixer.lisp lists the card's controls and finds a volume; RESTORE-AUDIO ran at
;;;;     boot (the volume at its saved or default level, unmuted);
;;;;   - the volume keys over QMP (qcodes volumeup / volumedown / audiomute — KEY_VOLUMEUP
;;;;     / KEY_VOLUMEDOWN / KEY_MUTE on the PS/2 keyboard's evdev): the level moves by a
;;;;     step, the status line says so, the tray's VOL item follows, mute toggles;
;;;;   - ffmpeg in the container RECORDS from plughw:0,0 (2 s of the backend's silence);
;;;;   - mplayer in the container, started by the Workspace form doc/screencast.org §8.5
;;;;     gives, PLAYS a 440 Hz tone through plughw:1,0 (card 1 here) into QEMU's WAV file: not silent,
;;;;     and its zero crossings say ~440 Hz.
;;;;
;;;; All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(in-package :lol.guest)

(defvar *wav* nil "QEMU's WAV file of what the guest played; set once *WORK* is known.")

;;; The wav path must be in the scratch dir BOOT-GUEST creates, which does not exist
;;; yet; QEMU creates the file itself, so name the dir the way BOOT-GUEST will.
(setf *wav* (format nil "/tmp/lol-sound-~d/out.wav" (sb-posix:getpid)))
(setf *extra-qemu-args*
      (list "-audiodev" "none,id=mic0" "-device" "intel-hda,id=hda0" "-device" "hda-duplex,audiodev=mic0,bus=hda0.0"
            "-audiodev" (format nil "wav,id=out0,path=~a" *wav*)
            "-device" "ich9-intel-hda,id=hda1" "-device" "hda-output,audiodev=out0,bus=hda1.0"))

(defun row (label value)
  (format t "~&  ~44a ~a~%" label value)
  (finish-output)
  value)

(defun wait-until (form-string &optional (seconds 10))
  (loop repeat (* 2 seconds)
        when (string= "T" (probe form-string)) do (return t)
        do (sleep 0.5)))

(defun wav-samples ()
  "Channel 0 of QEMU's WAV (S16LE stereo; the header's sizes are written at close,
   so skip its 44 bytes and read whatever has been streamed)."
  (with-open-file (s *wav* :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (when s
      (let* ((n (file-length s)) (buf (make-array n :element-type '(unsigned-byte 8))))
        (read-sequence buf s)
        (loop for i from 44 below (- n 3) by 4
              collect (let ((v (logior (aref buf i) (ash (aref buf (1+ i)) 8))))
                        (if (>= v 32768) (- v 65536) v)))))))

(defun loud-run (samples)
  "The samples from the first to the last one above a whisper."
  (let ((a (position-if (lambda (x) (> (abs x) 500)) samples))
        (b (position-if (lambda (x) (> (abs x) 500)) samples :from-end t)))
    (and a b (> b a) (subseq samples a b))))

(defun crossings-hz (run rate)
  (let ((n 0))
    (loop for (x y) on run while y when (and (minusp x) (not (minusp y))) do (incf n))
    (/ (* n rate) (length run))))

(defun checks ()
  (wait-for-browser)
  (quiet-kernel-console)
  (format t "~&~%== the card ==~%")
  (check "the kernel found a sound card"
         (search "HDA" (row "/proc/asound/cards"
                            (probe "(with-open-file (s \"/proc/asound/cards\") (read-line s nil \"\"))"))))
  (let ((uid (row "PID 1's uid" (probe "(sb-unix:unix-getuid)")))
        (owner (row "owner of /dev/snd/pcmC0D0p"
                    (probe "(nth-value 5 (sb-unix:unix-stat \"/dev/snd/pcmC0D0p\"))"))))
    (check "/dev/snd belongs to PID 1's user (lol-rootd)" (equal uid owner)))
  (format t "~&~%== the mixer ==~%")
  (row "controls" (probe "(mapcar (lambda (c) (getf c :name)) (mixer-controls))"))
  (check "mixer.lisp finds a volume control"
         (string/= "NIL" (row "volume control" (probe "(getf (volume-control) :name)"))))
  (let ((v0 (probe-int "(volume-percent)")))
    (row "volume after boot" v0)
    ;; the level SAVED in ~/.lol/settings, or 50 % on a first boot. Under
    ;; LOL_GUEST_HOSTROOT=1 the home is the build host's own (under a tmpfs overlay), so
    ;; this is whatever the user last set on the laptop — 56 % on 2026-10-18, which is
    ;; how this check first learned that the laptop's volume keys work.
    (let ((saved (probe-int "(let ((v (setting :volume 50))) (if (integerp v) v 50))")))
      (row "saved level (:volume, else 50)" saved)
      (check "RESTORE-AUDIO set the saved level exactly, unmuted"
             (and saved (eql v0 saved) (string= "NIL" (probe "(volume-muted-p)")))))
    (format t "~&~%== the volume keys ==~%")
    (qmp-key "volumeup") (sleep 1)
    (let ((v1 (probe-int "(volume-percent)")))
      (row "after volumeup" v1)
      (check "volumeup raises it one step, exactly (no drift through the raw steps)"
             (eql v1 (and v0 (min 100 (+ v0 5)))))
      (check "the status line says the level"
             (search (format nil "Volume ~d%" v1) (row "status" (probe "*browser-status*"))))
      (sleep 2.5)
      (check "the tray shows it"
             (search (format nil "VOL ~d%" v1) (row "tray" (probe "(getf (sound-tray-item) :text)")))))
    (qmp-key "volumedown") (sleep 1)
    (check "volumedown lowers it back" (eql (row "after volumedown" (probe-int "(volume-percent)")) v0))
    (qmp-key "audiomute") (sleep 1)
    (check "audiomute mutes" (string= "T" (row "muted?" (probe "(volume-muted-p)"))))
    (check "the tray says mute" (search "mute" (row "tray" (probe "(getf (sound-tray-item) :text)"))))
    (qmp-key "audiomute") (sleep 1)
    (check "audiomute again unmutes" (string= "NIL" (row "muted?" (probe "(volume-muted-p)")))))
  (format t "~&~%== recording: ffmpeg in the container ==~%")
  (let ((r (row "ffmpeg -f alsa -i plughw:0,0 -t 2"
                (probe "(nth-value 1 (container-run-output \"ffmpeg -hide_banner -loglevel error -y -f alsa -i plughw:0,0 -t 2 /tmp/rec.wav && stat -c %s /tmp/rec.wav\" :timeout 30))" 40))))
    (check "ffmpeg recorded from the card and exited 0" (equal r "0"))
    (check "the recording has ~2 s of 48 kHz stereo"
           (let ((n (probe-int "(with-open-file (s \"/tmp/rec.wav\" :element-type '(unsigned-byte 8)) (file-length s))")))
             (row "bytes" n)
             (and n (< 300000 n 450000)))))
  (format t "~&~%== playing: mplayer, as the Workspace form starts it ==~%")
  (probe "(nth-value 1 (container-run-output \"ffmpeg -hide_banner -loglevel error -y -f lavfi -i sine=frequency=440:duration=3 -c:a aac /tmp/tone.m4a\" :timeout 30))" 40)
  (let ((before (length (wav-samples))))
    (probe "(progn (open-terminal (container-command-line (list \"mplayer\" \"-really-quiet\" \"-nolirc\" \"-novideo\" \"-ao\" \"alsa:device=plughw=1.0\" \"/tmp/tone.m4a\")) :title \"mplayer\") t)" 10)
    (sleep 7)
    (let* ((all (wav-samples))
           (new (and all (nthcdr before all)))
           (run (loud-run new)))
      (row "samples written by QEMU" (length new))
      (check "QEMU's WAV is not silent: the tone reached the card" run)
      (when run
        (let ((hz (crossings-hz run 44100)))
          (row "tone length (s)" (float (/ (length run) 44100)))
          (row "frequency by zero crossings (Hz)" (float hz))
          (check "it is the 440 Hz tone" (< 400 hz 480)))))))

(run-guest-checks "sound" #'checks)
