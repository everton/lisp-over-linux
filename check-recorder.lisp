;;;; check-recorder.lisp — record the screen with sound, and play a video, in the booted guest.
;;;;
;;;;     LOL_GUEST_HOSTROOT=1 LOL_PORT=4021 sbcl --script check-recorder.lisp
;;;;
;;;; doc/screencast.org steps 2-4. LOL_GUEST_HOSTROOT=1 gives the container the build
;;;; host's ffmpeg / ffprobe / mplayer, and a real home (the host's /home under a tmpfs
;;;; overlay — nothing reaches the host) for ~/.lol/rec. QEMU's sound card here is
;;;; intel-hda + hda-duplex on audiodev `none`: it RECORDS SILENCE, which is what makes
;;;; the silent-take warning testable. Checks, all driven by QMP keys like a hand:
;;;;
;;;;   A. the preflight passes; SuperL-r starts: state, tray REC, status line
;;;;   B. 4 s recorded (typing into the Workspace meanwhile), SuperL-r pauses, 2 s
;;;;      paused, SuperL-r resumes, 3 s, SuperL-S-r stops; SAVING, then done
;;;;   C. the result: as long as the two recorded spans by the host's clock (the pause is
;;;;      not in it), 2 segments joined, H.264 + AAC at
;;;;      the screen's size, the frames = 30 x the time, the file and result.lisp there
;;;;   D. the picture is the screen: a frame decoded from the file, compared with the
;;;;      canvas channel by channel (a red/blue swap — the wrong pix_fmt — fails here)
;;;;   E. the sound is silence, and the recording says so (peak < -50 dB, "silent!")
;;;;   F. the recorder pane (Spotter's "record") lists it
;;;;   G. the player: a test clip played full screen — the screen lent (*SCREEN-LENT*),
;;;;      the screendump the clip's bars, not the environment; q gives the screen back
;;;;      and the next screendump is the environment again
;;;;   H. the same take from a DAMAGE LOG (step 5): m in the recorder pane switches,
;;;;      REC LOG in the tray, a .dmg written, pause / resume / stop, the logs replayed
;;;;      and encoded: the spans' length, 2 segments, H.264 + AAC, 30 frames a second,
;;;;      each segment's sound offset read from ffmpeg, the logs removed, the colours
;;;;
;;;; All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(in-package :lol.guest)

(setf *extra-qemu-args*
      (list "-audiodev" "none,id=mic0" "-device" "intel-hda" "-device" "hda-duplex,audiodev=mic0"))

(defun row (label value) (format t "~&  ~44a ~a~%" label value) (finish-output) value)

(defun wait-until (form-string &optional (seconds 10))
  (loop repeat (* 2 seconds)
        when (string= "T" (probe form-string 5)) do (return t)
        do (sleep 0.5)))

(defun shot (name)
  (let ((p (path (format nil "~a.ppm" name))))
    (qmp (format nil "{\"execute\":\"screendump\",\"arguments\":{\"filename\":\"~a\"}}" p))
    (sleep 0.5)
    p))

(defun ppm-pixel (file x y)
  "(r g b) of pixel X,Y in a binary PPM (P6, maxval 255) — QEMU's screendump."
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (let ((fields '()) (cur '()))
      ;; header: P6 W H 255, whitespace-separated, then one byte of whitespace
      (loop while (< (length fields) 4)
            do (let ((b (read-byte s)))
                 (if (member b '(32 10 13 9))
                     (when cur (push (map 'string #'code-char (reverse cur)) fields) (setf cur '()))
                     (push b cur))))
      ;; FIELDS was pushed, so reversed it is (P6 W H 255): the width is the SECOND
      ;; (the first version took the third, the height, and read the wrong pixel)
      (let ((w (parse-integer (second (reverse fields)) :junk-allowed t)) (start (file-position s)))
        (file-position s (+ start (* 3 (+ x (* y w)))))
        (list (read-byte s) (read-byte s) (read-byte s))))))

(defparameter +grid+ "(loop for gy from 1 to 6 nconc (loop for gx from 1 to 8 collect (let* ((x (floor (* gx *fb-xres*) 9)) (y (floor (* gy *fb-yres*) 7)) (v (aref (lol.canvas:canvas-pixels *fb-canvas*) (+ x (* y *fb-xres*))))) (list x y (ldb (byte 8 16) v) (ldb (byte 8 8) v) (ldb (byte 8 0) v)))))"
  "A guest form: 48 points of the canvas as (x y r g b).")

(defun screen-matches-canvas (name)
  "How many of the 48 grid points of a fresh screendump equal the canvas there."
  (let* ((pts (ignore-errors (read-from-string (probe +grid+ 10))))
         (p (shot name)))
    (if (not (listp pts)) 0
        (count-if (lambda (pt) (equal (ppm-pixel p (first pt) (second pt)) (cddr pt))) pts))))

(defun checks ()
  (wait-for-browser)
  (quiet-kernel-console)
  (format t "~&~%== A. start ==~%")
  ;; QEMU's home is a tmpfs overlay in the guest's RAM (~0.3 GB free): the 1 GB rule is
  ;; for the laptop, so the check lowers it — the refusal itself was seen, 2026-10-18
  (row "preflight with the 1 GB rule" (probe "(rec-preflight)" 10))
  (probe "(setf *rec-min-free* 0)")
  (check "the preflight passes" (string= "NIL" (row "preflight problems" (probe "(rec-preflight)" 10))))
  (defparameter *t-start* (get-internal-real-time))
  (qmp-hold "meta_l" "r") (sleep 2)
  (check "SuperL-r starts recording" (string= ":RECORDING" (row "state" (probe "(rec-state *rec*)"))))
  (check "the status line says so" (search "Recording" (row "status" (probe "*browser-status*"))))
  (check "the tray shows REC" (search "REC 00:0" (row "tray" (probe "(getf (recorder-tray-item) :text)"))))
  (format t "~&~%== B. record, pause, resume, stop ==~%")
  (qmp-type "hello screencast") (sleep 3)
  (defparameter *t-pause* (get-internal-real-time))
  (qmp-hold "meta_l" "r") (sleep 2)
  (row "state just after" (probe "(rec-state *rec*)"))
  (check "SuperL-r pauses (the segment ends on a thread)" (wait-until "(eq (rec-state *rec*) :paused)" 10))
  (check "the tray says PAUSED" (search "PAUSED" (row "tray" (probe "(getf (recorder-tray-item) :text)"))))
  (sleep 2)
  (defparameter *t-resume* (get-internal-real-time))
  (qmp-hold "meta_l" "r") (sleep 1)
  (check "SuperL-r resumes" (string= ":RECORDING" (row "state" (probe "(rec-state *rec*)"))))
  (qmp-type " again") (sleep 2)
  ;; Super and Shift genuinely held (KEY-EVENT only builds the event; QMP-EVENTS sends it)
  (defparameter *t-stop* (get-internal-real-time))
  (qmp-events (key-event t "meta_l")) (sleep 0.2) (qmp-events (key-event t "shift")) (sleep 0.2)
  (qmp-events (key-event t "r")) (sleep 0.1) (qmp-events (key-event nil "r")) (sleep 0.2)
  (qmp-events (key-event nil "shift")) (qmp-events (key-event nil "meta_l"))
  (sleep 1)
  (row "state after SuperL-S-r" (probe "(rec-state *rec*)"))
  (check "SuperL-S-r stops, and the join finishes" (wait-until "(eq (rec-state *rec*) :done)" 60))
  (row "message" (probe "(rec-message *rec*)"))
  (format t "~&~%== C. the result ==~%")
  (let* ((res (probe "(rec-result *rec*)"))
         (dur (ignore-errors (read-from-string (probe "(getf (rec-result *rec*) :duration)")))))
    (row "result" res)
    ;; what was recorded, by the host's clock: start..pause and resume..stop (each key
    ;; is 0.5 s of held-modifier choreography, hence the second of slack)
    (let ((expected (/ (+ (- *t-pause* *t-start*) (- *t-stop* *t-resume*)) internal-time-units-per-second)))
      (row "recorded spans by the host's clock (s)" (float expected))
      (check "as long as the two recorded spans: the pause is not in it"
             (and (realp dur) (< (abs (- dur expected)) 1.0))))
    (check "two segments, joined" (string= "2" (probe "(getf (rec-result *rec*) :segments)")))
    (check "H.264 video, AAC sound"
           (string= "(\"h264\" \"aac\")" (probe "(list (getf (rec-result *rec*) :video) (getf (rec-result *rec*) :audio))")))
    (check "the screen's own size" (string= "T" (probe "(and (eql (getf (rec-result *rec*) :width) *fb-xres*) (eql (getf (rec-result *rec*) :height) *fb-yres*))")))
    (check "frames = 30 x the time (+/- a second's worth)"
           (string= "T" (probe "(let ((r (rec-result *rec*))) (< (abs (- (getf r :frames) (* 30 (getf r :duration)))) 30))")))
    (row "stale frames" (probe "(getf (rec-result *rec*) :stale)"))
    (check "the video and result.lisp are in ~/.lol/rec"
           (string= "T" (probe "(and (probe-file (getf (rec-result *rec*) :file)) (probe-file (rec-file \"result.lisp\")) t)")))
    (check "the segments were removed after the join"
           (string= "NIL" (probe "(directory (rec-file \"seg-*.mp4\"))"))))
  (format t "~&~%== D. the picture is the screen ==~%")
  ;; a frame from the end of the file (the screen has not changed since) against the
  ;; canvas now, as mean colour per channel over the whole frame
  (probe "(nth-value 1 (container-run-output (format nil \"ffmpeg -v error -y -sseof -1 -i ~a -frames:v 1 -f rawvideo -pix_fmt bgr0 /tmp/last.raw\" (getf (rec-result *rec*) :file)) :timeout 30))" 40)
  (let ((means (probe "(let* ((px (lol.canvas:canvas-pixels *fb-canvas*)) (n (length px)) (raw (with-open-file (s \"/tmp/last.raw\" :element-type '(unsigned-byte 8)) (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence b s) b)))) (flet ((mean (f) (round (loop for i below n by 97 sum (funcall f i)) (ceiling n 97)))) (list (mean (lambda (i) (ldb (byte 8 16) (aref px i)))) (mean (lambda (i) (ldb (byte 8 0) (aref px i)))) (mean (lambda (i) (aref raw (+ 2 (* 4 i))))) (mean (lambda (i) (aref raw (* 4 i)))))))" 20)))
    (row "canvas R B / file R B (means)" means)
    (check "red and blue as on screen (within 6)"
           (let ((m (ignore-errors (read-from-string means))))
             (and (listp m) (= 4 (length m))
                  (<= (abs (- (first m) (third m))) 6) (<= (abs (- (second m) (fourth m))) 6)))))
  (format t "~&~%== E. the sound: QEMU records silence ==~%")
  (check "the take is flagged silent (peak < -50 dB)"
         (string= "T" (row "peak" (probe "(< (getf (rec-result *rec*) :peak) -50)"))))
  (format t "~&~%== F. the recorder pane ==~%")
  (probe "(progn (open-recorder) t)") (sleep 1)
  (check "the recorder pane lists the recording"
         (string= "T" (probe "(and (some (lambda (it) (search \"screencast-\" (item-label it))) (view-items (pane-view (current-pane)))) t)")))
  (probe "(recording-key (subj-recording (rec-dir *rec*)) #\\f)")
  (check "f on a recording opens its own folder, the video selected"
         (string= "T" (row "folder tab" (probe "(let ((s (pane-subject (current-pane)))) (and (subj-dir-p s) (search \"screencast-\" (subj-dir-select s)) t))"))))
  (format t "~&~%== G. the player: the screen lent, and given back ==~%")
  (probe "(nth-value 1 (container-run-output \"ffmpeg -v error -y -f lavfi -i testsrc2=size=640x400:rate=30 -t 20 -c:v libx264 -pix_fmt yuv420p /tmp/bars.mp4\" :timeout 60))" 70)
  (sleep 1)
  (check "before playing, the screen is the canvas (the comparison itself works)"
         (= 48 (row "grid points equal, before" (screen-matches-canvas "before"))))
  (probe "(progn (play-video \"/tmp/bars.mp4\") t)" 10)
  (sleep 4)
  (check "the screen is lent while it plays" (string= "T" (row "*screen-lent*" (probe "*screen-lent*"))))
  (check "the screen shows the clip, not the environment (most points differ)"
         (< (row "grid points equal, playing" (screen-matches-canvas "playing")) 24))
  (qmp-key "spc") (sleep 1)
  (qmp-key "q") (sleep 3)
  (check "q ends it and gives the screen back" (string= "NIL" (row "*screen-lent*" (probe "*screen-lent*"))))
  (check "no player left" (string= "NIL" (probe "*player*")))
  (check "the screen is the environment again, every point"
         (= 48 (row "grid points equal, back" (screen-matches-canvas "back"))))
  ;; a video opened from the file manager plays with p (poc/file-manager.lisp)
  (probe "(progn (new-trail (subj-file (normalize-path \"/tmp/bars.mp4\"))) t)") (sleep 1)
  (qmp-key "p") (sleep 3)
  (check "p on a video opened in the file manager plays it (the screen lent)"
         (string= "T" (row "*screen-lent* after p" (probe "*screen-lent*"))))
  (qmp-key "q") (sleep 3)
  (check "and q gives it back" (string= "NIL" (probe "*screen-lent*")))
  (format t "~&~%== H. the same, from a damage log (step 5) ==~%")
  ;; back to the Workspace, the recorder in :LOG mode by its own key (m in its pane)
  (probe "(progn (open-recorder) t)") (sleep 1)
  (qmp-key "m") (sleep 1)
  (check "m in the recorder pane switches to the damage log" (string= ":LOG" (row "*rec-mode*" (probe "*rec-mode*"))))
  (qmp-hold "meta_l" "1") (sleep 1)
  (let ((t0 (get-internal-real-time)) t1 t2 t3)
    (qmp-hold "meta_l" "r") (sleep 1)
    (check "SuperL-r starts a logged recording" (string= "(:RECORDING :LOG)" (row "state, mode" (probe "(list (rec-state *rec*) (rec-mode *rec*))"))))
    (check "the tray says REC LOG" (search "REC LOG" (row "tray" (probe "(getf (recorder-tray-item) :text)"))))
    (qmp-type "logged") (sleep 2)
    (check "the log is being written" (string= "T" (row "a .dmg exists" (probe "(and (directory (rec-file \"seg-*.dmg\")) t)"))))
    (setf t1 (get-internal-real-time))
    (qmp-hold "meta_l" "r")
    (check "SuperL-r pauses it" (wait-until "(eq (rec-state *rec*) :paused)" 10))
    (sleep 1)
    (setf t2 (get-internal-real-time))
    (qmp-hold "meta_l" "r") (sleep 1)
    (qmp-type " more") (sleep 2)
    (setf t3 (get-internal-real-time))
    (qmp-events (key-event t "meta_l")) (sleep 0.2) (qmp-events (key-event t "shift")) (sleep 0.2)
    (qmp-events (key-event t "r")) (sleep 0.1) (qmp-events (key-event nil "r")) (sleep 0.2)
    (qmp-events (key-event nil "shift")) (qmp-events (key-event nil "meta_l"))
    (check "SuperL-S-r stops it, and the logs are encoded and joined" (wait-until "(eq (rec-state *rec*) :done)" 120))
    (row "result" (probe "(rec-result *rec*)"))
    (let ((dur (ignore-errors (read-from-string (probe "(getf (rec-result *rec*) :duration)"))))
          (expected (/ (+ (- t1 t0) (- t3 t2)) internal-time-units-per-second)))
      (row "recorded spans by the host's clock (s)" (float expected))
      (check "as long as the two recorded spans" (and (realp dur) (< (abs (- dur expected)) 1.0))))
    (check "made from a log, 2 segments, H.264 + AAC"
           (string= "(:LOG 2 \"h264\" \"aac\")"
                    (probe "(let ((r (rec-result *rec*))) (list (getf r :mode) (getf r :segments) (getf r :video) (getf r :audio)))")))
    (check "frames = 30 x the time (+/- a second's worth)"
           (string= "T" (probe "(let ((r (rec-result *rec*))) (< (abs (- (getf r :frames) (* 30 (getf r :duration)))) 30))")))
    (check "each segment's sound start was read from ffmpeg (0 <= offset < 1 s)"
           (string= "T" (row "offsets" (probe "(let ((o (getf (rec-result *rec*) :offsets))) (and (= 2 (length o)) (every (lambda (x) (and x (<= 0 x 1))) o)))"))))
    (check "the logs and the sound files were removed"
           (string= "NIL" (probe "(append (directory (rec-file \"seg-*.dmg\")) (directory (rec-file \"seg-*.flac\")))"))))
  (probe "(nth-value 1 (container-run-output (format nil \"ffmpeg -v error -y -sseof -1 -i ~a -frames:v 1 -f rawvideo -pix_fmt bgr0 /tmp/last.raw\" (getf (rec-result *rec*) :file)) :timeout 30))" 40)
  (let ((means (probe "(let* ((px (lol.canvas:canvas-pixels *fb-canvas*)) (n (length px)) (raw (with-open-file (s \"/tmp/last.raw\" :element-type '(unsigned-byte 8)) (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence b s) b)))) (flet ((mean (f) (round (loop for i below n by 97 sum (funcall f i)) (ceiling n 97)))) (list (mean (lambda (i) (ldb (byte 8 16) (aref px i)))) (mean (lambda (i) (ldb (byte 8 0) (aref px i)))) (mean (lambda (i) (aref raw (+ 2 (* 4 i))))) (mean (lambda (i) (aref raw (* 4 i)))))))" 20)))
    (row "canvas R B / file R B (means)" means)
    (check "the replayed picture is the screen (red and blue within 6)"
           (let ((m (ignore-errors (read-from-string means))))
             (and (listp m) (= 4 (length m))
                  (<= (abs (- (first m) (third m))) 6) (<= (abs (- (second m) (fourth m))) 6))))))


(run-guest-checks "recorder" #'checks)
