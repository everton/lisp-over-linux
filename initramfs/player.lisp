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
