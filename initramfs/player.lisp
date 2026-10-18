;;;; player.lisp — play a video with the host's MPlayer, full screen, then come back.
;;;;
;;;; doc/screencast.org §8 (step 4 of §10). We do not build a player (the user's call,
;;;; 2026-10-17): decoding, A/V sync, seeking and the sound are MPlayer's, the host's
;;;; own, run in the default container. Our part is the screen and the keys.
;;;;
;;;; The screen: only one process may write /dev/fb0 at a time (doc/processes.org), so
;;;; playing LENDS it — *SCREEN-LENT* (fb-browser.lisp) turns off the five functions
;;;; that are every write of ours, MPlayer draws with -vo fbdev2, and when it ends the
;;;; whole frame is presented once. The world goes on meanwhile (the clock, a terminal,
;;;; a streaming answer repair the canvas); it all appears at the hand-back.
;;;;
;;;; The keys: while the screen is lent the loop hands every input event to
;;;; PLAYER-EVENT instead of the world, which turns keys into MPlayer's slave-mode
;;;; commands on its stdin (plain text; `mplayer -input cmdlist`). The volume keys
;;;; still work: they are read before events reach anyone (mixer.lisp).
;;;;
;;;;   SPC pause   <- / -> 5 s   S-<- / S-> 30 s   0-9 jump to 0-90 %
;;;;   o the on-screen clock   q or Esc stop (and come back)
;;;;
;;;; :WINDOW T plays in a 640-wide window instead, the interim way of §8.5: MPlayer in
;;;; a terminal tab drawing over the right of the screen (two writers — fine for a look,
;;;; and why full screen is the default).
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

(defvar *player* nil "The MPlayer process while one owns the screen, else NIL.")
(defvar *player-quit-at* nil "When q was sent: kill it if it is still there 3 s later.")

(defun resolve-video (file)
  "FILE (\"~/x.mp4\", a name in your home, or absolute) as the absolute path mplayer
   needs, or an error. PROBE-FILE understands ~/ (SBCL reads it as :HOME); NAMESTRING
   would keep the ~, and there is no shell between us and mplayer to expand it."
  (let ((found (probe-file (merge-pathnames file (user-homedir-pathname)))))
    (if found (sb-ext:native-namestring found) (error "no such file: ~a" file))))

(defun play-video (file &key window)
  "Play FILE with the host's mplayer: full screen, the screen lent until it ends; or
   with :WINDOW T in a window on the right, from a terminal tab. -> a message."
  (handler-case
      (let ((path (resolve-video file)))
        (cond
          (window
           (open-terminal
            (container-command-line
             (list "mplayer" "-really-quiet" "-nolirc" "-ao" "alsa:device=plughw=0.0"
                   "-vo" "fbdev" "-geometry" "100%:50%" "-vf" "scale=640:-3" path))
            :title "mplayer")
           (setf *browser-status* "playing in a window — q in its tab stops"))
          (*player* (setf *browser-status* "already playing"))
          (t
           (setf *player*
                 (sb-ext:run-program
                  *lol-ct* (rest (container-command-line
                                  (list "mplayer" "-slave" "-quiet" "-nolirc" "-noconsolecontrols"
                                        "-input" "nodefault-bindings" "-ao" "alsa:device=plughw=0.0"
                                        "-vo" "fbdev2"
                                        "-vf" (format nil "scale=~d:~d" *fb-xres* *fb-yres*) path)
                                  :no-input nil))
                  :input :stream :output nil :error nil :wait nil
                  :environment (container-environment))
                 *player-quit-at* nil
                 *screen-lent* t)
           (when (fboundp 'session-log) (funcall 'session-log "player: playing ~a full screen" path))
           (setf *browser-status* "playing — SPC pause, arrows seek, q stops"))))
    (error (e) (setf *browser-status* (format nil "play: ~a" e)))))

(defun player-send (command)
  "One slave-mode command line to MPlayer."
  (let ((s (and *player* (sb-ext:process-input *player*))))
    (when s (ignore-errors (write-line command s) (finish-output s)))))

(defun player-event (e)
  "An input event while the screen is lent: keys become MPlayer's commands; the
   pointer is ignored (nothing of ours is on screen to point at)."
  (when (eq (lol.morph:event-kind e) :key)
    (let ((key (lol.morph:event-key e)) (state (lol.morph:event-state e)))
      (cond ((member key '(#\q #\Q :escape #\Esc))
             (player-send "quit")
             (setf *player-quit-at* (get-internal-real-time)))
            ((eql key #\Space) (player-send "pause"))
            ((eq key :left) (player-send (if (lol.canvas:shift-p state) "seek -30 0" "seek -5 0")))
            ((eq key :right) (player-send (if (lol.canvas:shift-p state) "seek 30 0" "seek 5 0")))
            ((and (characterp key) (digit-char-p key))
             (player-send (format nil "seek ~d 1" (* 10 (digit-char-p key)))))
            ((eql key #\o) (player-send "osd_show_progression"))))))

(defun player-timeout () (if *player* 250 -1))

(defun player-run (canvas dirty)
  "Each turn while playing: has MPlayer ended (the video ended, q, or it died)?
   Then hand the screen back and present the whole frame."
  (declare (ignore dirty))
  (when *player*
    (when (and *player-quit-at* (sb-ext:process-alive-p *player*)
               (> (- (get-internal-real-time) *player-quit-at*) (* 3 internal-time-units-per-second)))
      (ignore-errors (sb-ext:process-kill *player* sb-unix:sigkill)))
    (unless (sb-ext:process-alive-p *player*)
      (ignore-errors (close (sb-ext:process-input *player*)))
      (setf *player* nil *screen-lent* nil)
      (when (fboundp 'session-log) (funcall 'session-log "player: done, screen back"))
      (setf *browser-status* "back")
      (present-frame canvas)))
  nil)

(fb-register-service :player :timeout 'player-timeout :run 'player-run)

;;; ---- what a video is: ffprobe -------------------------------------------------------

(defun probe-video (path)
  "What ffprobe says of PATH: (:duration s :size bytes :width :height :video codec
   :audio codec :fps \"30/1\"), from its JSON (lol.json). Runs the host's ffprobe in
   the default container and waits for it: call it on a thread (the recorder's join,
   the preview maker), not on the environment's."
  (let ((out (with-output-to-string (s)
               (sb-ext:run-program *lol-ct* (rest (container-command-line
                                                   (list "ffprobe" "-v" "error" "-of" "json"
                                                         "-show_format" "-show_streams" path)))
                                   :output s :error nil :wait t :environment (container-environment)))))
    (let* ((j (lol.json:parse-json out))
           (format (lol.json:jget j "format"))
           (streams (coerce (lol.json:jget j "streams" #()) 'list))
           (v (find "video" streams :key (lambda (x) (lol.json:jget x "codec_type")) :test #'equal))
           (a (find "audio" streams :key (lambda (x) (lol.json:jget x "codec_type")) :test #'equal)))
      (list :duration (ignore-errors (let ((*read-default-float-format* 'double-float))
                                       (float (read-from-string (lol.json:jget format "duration")) 1.0)))
            :size (ignore-errors (parse-integer (lol.json:jget format "size")))
            :width (and v (lol.json:jget v "width")) :height (and v (lol.json:jget v "height"))
            :video (and v (lol.json:jget v "codec_name")) :fps (and v (lol.json:jget v "avg_frame_rate"))
            :audio (and a (lol.json:jget a "codec_name"))
            :sample-rate (and a (lol.json:jget a "sample_rate"))))))

;;; ---- the file manager's preview of a video ----------------------------------------------
;;;
;;; A frame from the video, with what ffprobe says in the title. Both need the host's
;;; ffmpeg in a container: a few hundred ms per file, too slow for every move of the
;;; selection. So FILE-VIEW (file-views.lisp) asks VIDEO-VIEW, which answers at once
;;; from a cache keyed by path and modification time, and otherwise says "making a
;;; preview…" and asks the PREVIEW MAKER thread for it. The thread works on the newest
;;; request only — scrolling past ten videos makes one preview, not ten — and when it
;;; has one, the :VIDEO-PREVIEWS service (on the environment's thread) forgets the
;;; panes' stale previews so the next repair draws the frame.

(defparameter *video-thumb-width* 640 "How wide the preview's frame is made (it is scaled to fit).")

(defvar *video-previews* (make-hash-table :test 'equal)
  "(path . mtime) -> (:state :done :info PLIST :w :h :pixels) or (:state :failed :error STRING).")
(defvar *video-wanted* nil "The newest (path . mtime) a preview was asked for.")
(defvar *video-ready* '() "Paths whose preview became ready, for the service to refresh.")
(defvar *video-lock* (sb-thread:make-mutex :name "video previews"))
(defvar *video-cv* (sb-thread:make-waitqueue :name "video previews"))
(defvar *video-maker* nil)

(defun video-thumbnail (path info)
  "A frame of PATH as (values W H PIXELS): ffmpeg takes the frame at a tenth of the
   way in (an opening is often black), at most 30 s in, scaled to *VIDEO-THUMB-WIDTH*,
   as a PPM in /tmp (the container shares our /tmp), decoded by lol.image."
  (let* ((at (min 30 (* 0.1 (or (getf info :duration) 0))))
         (out (format nil "/tmp/lol-thumb-~d.ppm" (sb-unix:unix-getpid))))
    (ignore-errors (delete-file out))
    (sb-ext:run-program *lol-ct* (rest (container-command-line
                                        (list "ffmpeg" "-v" "error" "-y" "-ss" (format nil "~,2f" at)
                                              "-i" path "-frames:v" "1"
                                              "-vf" (format nil "scale=~d:-2" *video-thumb-width*)
                                              "-f" "image2" "-c:v" "ppm" out)))
                        :output nil :error nil :wait t :environment (container-environment))
    (let ((octets (with-open-file (s out :element-type '(unsigned-byte 8) :if-does-not-exist nil)
                    (and s (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
                             (read-sequence b s) b)))))
      (ignore-errors (delete-file out))
      (unless octets (error "ffmpeg made no frame"))
      (multiple-value-bind (w h pixels) (lol.image:decode-image octets)
        (unless (and (integerp w) pixels) (error "the frame did not decode"))
        (values w h pixels)))))

(defun video-maker-loop ()
  "The preview maker thread: the newest wanted video, probed and given a frame."
  (loop
    (let ((key (sb-thread:with-mutex (*video-lock*)
                 (loop until (and *video-wanted* (not (gethash *video-wanted* *video-previews*)))
                       do (sb-thread:condition-wait *video-cv* *video-lock*))
                 *video-wanted*)))
      (let ((entry (handler-case
                       (let ((info (probe-video (car key))))
                         (multiple-value-bind (w h pixels) (video-thumbnail (car key) info)
                           (list :state :done :info info :w w :h h :pixels pixels)))
                     (error (e) (list :state :failed :error (princ-to-string e))))))
        (sb-thread:with-mutex (*video-lock*)
          (setf (gethash key *video-previews*) entry)
          (push (car key) *video-ready*))))))

(defun want-video-preview (key)
  (sb-thread:with-mutex (*video-lock*)
    (setf *video-wanted* key)
    (sb-thread:condition-notify *video-cv*))
  (unless (and *video-maker* (sb-thread:thread-alive-p *video-maker*))
    (setf *video-maker* (sb-thread:make-thread #'video-maker-loop :name "video-previews"))))

(defun video-info-line (info)
  (format nil "~@[~ax~]~@[~a~] ~@[~a~]~@[/~a~] ~a"
          (getf info :width) (getf info :height) (getf info :video) (getf info :audio)
          (mm-ss (or (getf info :duration) 0))))

(defun video-view (s path e preview)
  "The view of a video file: its frame and ffprobe's facts when the cache has them,
   else a list saying a preview is being made (and asking for it)."
  (declare (ignore preview))
  (let* ((key (cons path (fentry-mtime e)))
         (entry (sb-thread:with-mutex (*video-lock*) (gethash key *video-previews*))))
    (cond
      ((and entry (eq (getf entry :state) :done))
       (make-view :title (file-title "video" path e (video-info-line (getf entry :info)))
                  :kind :image :subject s
                  :data (list :width (getf entry :w) :height (getf entry :h)
                              :pixels (getf entry :pixels) :format "video frame" :path path)))
      (t
       (unless entry (want-video-preview key))
       (make-view :title (file-title "video" path e (human-size (fentry-size e))) :kind :list :subject s
                  :items (list (make-item :label (if entry
                                                     (format nil "(no preview: ~a)" (getf entry :error))
                                                     "making a preview… (the host's ffmpeg)"))
                               (make-item :label "p plays it full screen, w in a window")
                               (make-item :label "file information" :subject (subj-file-info path))))))))

(defun video-previews-timeout ()
  (if (or *video-ready* (and *video-wanted* (not (gethash *video-wanted* *video-previews*)))) 200 -1))

(defun video-previews-run (canvas dirty)
  "On the environment's thread: a preview became ready — forget the stale views of that
   file (a side preview, or the file's own pane), so the next repair shows the frame."
  (declare (ignore canvas dirty))
  (let ((ready (sb-thread:with-mutex (*video-lock*) (shiftf *video-ready* '()))))
    (when ready
      (flet ((ours-p (subject) (and (subj-file-p subject)
                                    (member (subj-file-path subject) ready :test #'equal))))
        (dolist (trail *trails*)
          (dolist (p trail)
            (when (ours-p (pane-preview-subject p))
              (setf (pane-preview-subject p) nil)
              (damage-pane p))
            (when (ours-p (pane-subject p))
              (let ((v (ignore-errors (present (pane-subject p)))))
                (when v (setf (pane-view p) v) (damage-pane p))))))))
    nil))

(fb-register-service :video-previews :timeout 'video-previews-timeout :run 'video-previews-run)
