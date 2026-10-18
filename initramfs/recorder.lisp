;;;; recorder.lisp — screencasts of the environment, with sound, recorded from inside it.
;;;;
;;;; doc/screencast.org is the design; this is steps 2 and 3 of its §10. The idea in
;;;; two sentences: the environment already draws every frame into a canvas in Lisp
;;;; memory and copies rects of it to /dev/fb0 (fb-browser.lisp's PRESENT-REPAIR), so a
;;;; recording is "copy the canvas 30 times a second", not "read the screen back"; and
;;;; the encoder is the HOST's ffmpeg, run in the default container (containers.lisp),
;;;; which also records the microphone through ALSA (plughw:0,0 — never ALSA's
;;;; `default`, which the host's configuration points at PipeWire, §2).
;;;;
;;;;   SuperL-r          start; while recording, pause; while paused, resume
;;;;   SuperL-S-r        stop: finish, join the segments, show the result
;;;;   Spotter           "record" (the recorder pane), "recordings"
;;;;
;;;; The pieces, in the order a frame meets them:
;;;;
;;;;   PRESENT-REPAIR / PRESENT-FRAME   call *FRAME-TAP* (fb-browser.lisp): "the canvas
;;;;                     changed". Nothing is copied there.
;;;;   the SAMPLER       a service in the poll loop (:TIMEOUT / :RUN): every 1/30 s,
;;;;                     copy the canvas into a pooled buffer if it changed or the
;;;;                     pointer moved, draw the pointer on the copy (the arrow is
;;;;                     drawn only on /dev/fb0, never on the canvas), and queue it;
;;;;                     if nothing changed, the last frame is queued AGAIN. So the
;;;;                     number of frames is always 30 x the time recorded — the video's
;;;;                     clock is the frame count, exact, which is what keeps it in step
;;;;                     with the sound (§5.1: measured on the host, a constant 32 ms
;;;;                     and no drift over 3 minutes).
;;;;   the WRITER        a thread: pops (buffer . copies) and writes the raw pixels to
;;;;                     ffmpeg's stdin. The environment's thread NEVER writes to the
;;;;                     pipe, so a slow encoder or disk cannot freeze the screen
;;;;                     (doc/diagnostics.org). When no buffer is free the sampler
;;;;                     repeats the last frame instead of copying — timing kept,
;;;;                     content stale — and counts it (*STALE*), shown in the result.
;;;;   ffmpeg            in the container: rawvideo bgr0 from stdin + ALSA capture ->
;;;;                     H.264 (ultrafast) + AAC into seg-NNN.mp4, and a second output,
;;;;                     the mic as 8 kHz mono s16 into a FIFO in /tmp (the container
;;;;                     shares our /tmp), which the METER thread reads for the level.
;;;;   PAUSE             ends the segment: close ffmpeg's stdin; with -shortest it
;;;;                     ends the file when the video ends. RESUME starts the next.
;;;;   STOP              ends the last segment, then (a thread) joins them with
;;;;                     `ffmpeg -f concat -c copy` — no re-encoding — reading its
;;;;                     -progress file for the percentage, and asks ffprobe what came
;;;;                     out. RESULT.LISP beside the video keeps that for the list.
;;;;
;;;; Files: ~/.lol/rec/<stamp>/ (0700) holds the segments while recording, then
;;;; screencast-<stamp>.mp4, result.lisp and ffmpeg.log. Without a home on a disk
;;;; there is nowhere to record to, and the preflight says so.
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

;;; ---- settings -------------------------------------------------------------------

(defparameter *rec-fps* 30 "Frames per second of a recording (doc/screencast.org §12).")
(defparameter *rec-pool-size* 4 "Frame buffers the sampler and the writer share.")
(defparameter *rec-audio-device* "plughw:0,0"
  "The ALSA capture device: card 0, device 0 (the ALC289's analog input on the laptop).
   plug converts the format and rate the hardware offers into what ffmpeg asks for.")
(defparameter *rec-video-args*
  '("-c:v" "libx264" "-preset" "ultrafast" "-crf" "23" "-pix_fmt" "yuv420p" "-g" "60")
  "How a segment's video is encoded: H.264 that every player takes (yuv420p).
   ultrafast: measured 4.1x real time at 1920x1200 on the laptop, about one thread.")
(defparameter *rec-audio-args* '("-c:a" "aac" "-b:a" "160k" "-ar" "48000"))
(defparameter *rec-min-free* (* 1024 1024 1024) "Refuse to start with less disk free.")

;;; ---- state ----------------------------------------------------------------------

(defstruct (rec (:constructor %make-rec))
  (state :idle)          ; :preflight :recording :paused :finishing :joining :done :failed
  dir name
  (segments '())         ; paths of finished segments, oldest first
  process                ; the current segment's ffmpeg (through lol-ct)
  (fd nil)               ; its stdin
  (seg-start 0)          ; internal real time the current segment began
  (elapsed 0)            ; seconds recorded in finished segments
  (next-slot 0)          ; internal real time the next frame is due
  (slot 0)               ; frames sampled in the current segment
  (frames 0) (stale 0)   ; frames written in all; frames that had to repeat for lack of a buffer
  (level -90.0)          ; the mic's peak over the last ~50 ms, dBFS
  (peak -90.0)           ; the loudest of the whole take
  (progress 0)           ; the join's percent
  message                ; the last thing to tell the user
  result                 ; plist from ffprobe when done
  after                  ; while :finishing, what comes next: :paused or :stop
  w h)

(defvar *rec* nil "The recording in progress, or the last one (its result), or NIL.")
(defvar *rec-lock* (sb-thread:make-mutex :name "recorder"))
(defvar *rec-queue* '() "(buffer . copies) entries for the writer, oldest first.")
(defvar *rec-queue-cv* (sb-thread:make-waitqueue :name "recorder queue"))
(defvar *rec-pool* '() "The frame buffers: canvases the size of the screen.")
(defvar *rec-busy* (make-hash-table :test 'eq) "Buffer -> copies still to be written.")
(defvar *rec-last* nil "The buffer sampled last, which a repeat writes again.")
(defvar *rec-dirty* t "Has the canvas been presented since the last sample?")
(defvar *rec-pointer* nil "(x y hidden) at the last sample.")
(defvar *rec-writer* nil)
(defvar *rec-meter* nil)

(defconstant +o-nonblock+ #o4000 "O_NONBLOCK on x86-64 Linux (asm-generic/fcntl.h); sb-unix has no name for it.")

(defun rec-now () (get-internal-real-time))
(defun rec-seconds (ticks) (/ ticks internal-time-units-per-second))

(defun rec-recording-p () (and *rec* (eq (rec-state *rec*) :recording)))
(defun rec-active-p ()
  "Recording, paused, or finishing — anything a new SuperL-r must not start over."
  (and *rec* (member (rec-state *rec*) '(:recording :paused :finishing :joining))))

(defun rec-time ()
  "Seconds recorded so far: the finished segments plus the running one."
  (if (null *rec*) 0
      (+ (rec-elapsed *rec*)
         (if (rec-recording-p) (rec-seconds (- (rec-now) (rec-seg-start *rec*))) 0))))

(defun mm-ss (seconds)
  (multiple-value-bind (m s) (floor (round seconds) 60) (format nil "~2,'0d:~2,'0d" m s)))

(defun rec-say (fmt &rest args)
  "Note FMT on the recording and in session.log; return the message."
  (let ((m (apply #'format nil fmt args)))
    (when *rec* (setf (rec-message *rec*) m))
    (when (fboundp 'session-log) (funcall 'session-log "recorder: ~a" m))
    m))

;;; ---- the tap: called by PRESENT-REPAIR and PRESENT-FRAME (fb-browser.lisp) -------

(defun rec-note-frame () (setf *rec-dirty* t))
(setf *frame-tap* 'rec-note-frame)

;;; ---- files ------------------------------------------------------------------------

(defun rec-root () (lol-path "rec/"))

(defun rec-stamp ()
  "\"2026-10-18_09-01-52\" — the local time, safe in a file name."
  (substitute #\_ #\Space (substitute #\- #\: (clock-string))))

(defun ensure-private-dir (path)
  "PATH as a folder of ours alone (0700), made with its parents if missing."
  (ensure-directories-exist path)
  (%chmod (string-right-trim "/" path) #o700)          ; files.lisp
  path)

(defun rec-file (name) (concatenate 'string (rec-dir *rec*) name))

;;; ---- ffmpeg, in the container -------------------------------------------------------

(defun rec-meter-fifo () (format nil "/tmp/lol-rec-meter-~d" (sb-unix:unix-getpid)))

(defun rec-segment-argv (out)
  "The ffmpeg command for one segment written to OUT."
  (append
   (list "ffmpeg" "-hide_banner" "-loglevel" "warning" "-nostats" "-y"
         "-thread_queue_size" "64"
         "-f" "rawvideo" "-pix_fmt" "bgr0" "-s" (format nil "~dx~d" (rec-w *rec*) (rec-h *rec*))
         "-r" (princ-to-string *rec-fps*) "-i" "pipe:0"
         "-thread_queue_size" "1024" "-f" "alsa" "-ac" "2" "-i" *rec-audio-device*
         "-map" "0:v" "-map" "1:a")
   *rec-video-args* *rec-audio-args*
   ;; -shortest: the file ends when the video does — closing stdin ends a segment.
   ;; aresample=async=1 stretches the sound by a sample now and then so it keeps to its
   ;; own timestamps (the card's clock and ours are not quite the same clock).
   (list "-af" "aresample=async=1" "-shortest" "-movflags" "+faststart" out
         ;; the second output: the mic for the meter, 8000 samples/s mono s16
         "-map" "1:a" "-ac" "1" "-ar" "8000" "-f" "s16le" "-y" (rec-meter-fifo))))

(defun rec-spawn (argv &key input)
  "Run ARGV in the default container; stderr appended to ffmpeg.log. INPUT :STREAM
   gives the process a stdin pipe we write. Returns the SB-EXT:PROCESS."
  (sb-ext:run-program *lol-ct* (rest (container-command-line argv :cwd (rec-dir *rec*)))
                      :input input :output nil
                      :error (rec-file "ffmpeg.log") :if-error-exists :append
                      :wait nil :environment (container-environment)))

(defun rec-run (argv)
  "Run ARGV in the container and wait; -> its exit code."
  (let ((p (rec-spawn argv)))
    (sb-ext:process-wait p)
    (sb-ext:process-exit-code p)))

;;; ---- the frame pool and the writer ----------------------------------------------------

(defun rec-make-pool (w h)
  (setf *rec-pool* (loop repeat *rec-pool-size* collect (lol.canvas:make-canvas w h))
        *rec-busy* (make-hash-table :test 'eq)
        *rec-last* nil
        *rec-queue* '()))

(defun rec-free-buffer ()
  "A pooled buffer nobody is writing and the sampler will not repeat, or NIL."
  (sb-thread:with-mutex (*rec-lock*)
    (find-if (lambda (b) (and (not (eq b *rec-last*)) (zerop (gethash b *rec-busy* 0))))
             *rec-pool*)))

(defun rec-enqueue (buffer)
  "One more frame of BUFFER for the writer: added to the last entry when it is the
   same buffer (a repeat), else a new entry."
  (sb-thread:with-mutex (*rec-lock*)
    (let ((last (car (last *rec-queue*))))
      (if (and last (eq (car last) buffer))
          (incf (cdr last))
          (setf *rec-queue* (append *rec-queue* (list (cons buffer 1))))))
    (incf (gethash buffer *rec-busy* 0))
    (sb-thread:condition-notify *rec-queue-cv*)))

(defun write-all (fd vector nbytes)
  "Write NBYTES of VECTOR's storage to FD, through partial writes and EINTR. T, or NIL
   when the pipe is gone (ffmpeg died: EPIPE)."
  (sb-sys:with-pinned-objects (vector)
    (let ((sap (sb-sys:vector-sap vector)) (off 0))
      (loop while (< off nbytes)
            do (multiple-value-bind (n errno)
                   (sb-unix:unix-write fd sap off (- nbytes off))
                 (cond (n (incf off n))
                       ((eql errno sb-unix:eintr))
                       (t (return-from write-all nil)))))
      t)))

(defun rec-writer-loop (fd)
  "The writer thread: frames from the queue into ffmpeg's stdin until the segment ends
   (:END in the queue) or the pipe breaks."
  (loop
    (let ((entry (sb-thread:with-mutex (*rec-lock*)
                   (loop until *rec-queue* do (sb-thread:condition-wait *rec-queue-cv* *rec-lock*))
                   (first *rec-queue*))))
      (when (eq entry :end)
        (sb-thread:with-mutex (*rec-lock*) (pop *rec-queue*))
        (return :end))
      (let* ((buffer (car entry))
             (pixels (lol.canvas:canvas-pixels buffer))
             (nbytes (* 4 (length pixels))))
        (unless (write-all fd pixels nbytes)
          (return :broken))
        (sb-thread:with-mutex (*rec-lock*)
          (when *rec* (incf (rec-frames *rec*)))
          (decf (gethash buffer *rec-busy* 0))
          (if (> (cdr entry) 1)
              (decf (cdr entry))
              (pop *rec-queue*)))))))

;;; ---- the meter: the mic's level, from the FIFO -----------------------------------------

(sb-alien:define-alien-routine ("mkfifo" %mkfifo) sb-alien:int (path sb-alien:c-string) (mode sb-alien:unsigned-int))

(defun rec-meter-loop (path)
  "Read 8 kHz mono s16 from PATH and keep the peak of each 50 ms block in REC-LEVEL.
   Opened non-blocking, so a FIFO nobody writes yet cannot hang the thread; a read of
   0 bytes (no writer, or between segments) just waits a little."
  (let ((fd (sb-unix:unix-open path (logior sb-unix:o_rdonly +o-nonblock+) 0))
        (buf (make-array 800 :element-type '(unsigned-byte 8))))
    (unwind-protect
         (loop while (and fd *rec* (rec-active-p))
               do (let ((n (sb-sys:with-pinned-objects (buf)
                             (sb-unix:unix-read fd (sb-sys:vector-sap buf) 800))))
                    (if (and n (plusp n))
                        (let ((peak 1))
                          (loop for i from 0 below (1- n) by 2
                                do (let* ((v (logior (aref buf i) (ash (aref buf (1+ i)) 8)))
                                          (v (abs (if (>= v 32768) (- v 65536) v))))
                                     (when (> v peak) (setf peak v))))
                          (let ((db (* 20 (log (/ peak 32768.0) 10))))
                            (setf (rec-level *rec*) db)
                            (when (> db (rec-peak *rec*)) (setf (rec-peak *rec*) db))))
                        (sleep 0.05))))
      (when fd (sb-unix:unix-close fd)))))

(defun meter-bar (db &optional (width 10))
  "A level as a bar of WIDTH cells over -60..0 dBFS: \"######....\"."
  (let ((n (max 0 (min width (round (* width (/ (+ 60 db) 60)))))))
    (concatenate 'string (make-string n :initial-element #\#)
                 (make-string (- width n) :initial-element #\.))))

;;; ---- segments ----------------------------------------------------------------------------

(defun rec-segment-path (n) (rec-file (format nil "seg-~3,'0d.mp4" n)))

(defun rec-start-segment ()
  "Start ffmpeg for the next segment and the writer that feeds it."
  (let* ((n (1+ (length (rec-segments *rec*))))
         (p (rec-spawn (rec-segment-argv (rec-segment-path n)) :input :stream)))
    (setf (rec-process *rec*) p
          (rec-fd *rec*) (sb-sys:fd-stream-fd (sb-ext:process-input p))
          (rec-seg-start *rec*) (rec-now)
          (rec-next-slot *rec*) (rec-now)
          (rec-slot *rec*) 0
          *rec-dirty* t
          (rec-state *rec*) :recording)
    (let ((fd (rec-fd *rec*)))
      (setf *rec-writer* (sb-thread:make-thread (lambda () (rec-writer-loop fd)) :name "rec-writer")))
    (unless (and *rec-meter* (sb-thread:thread-alive-p *rec-meter*))
      (let ((path (rec-meter-fifo)))
        (setf *rec-meter* (sb-thread:make-thread (lambda () (rec-meter-loop path)) :name "rec-meter"))))))

(defun rec-close-segment ()
  "On the environment's thread, at once: stop sampling and count the segment's time.
   The slow part, REC-END-SEGMENT, then runs on a thread."
  (incf (rec-elapsed *rec*) (rec-seconds (- (rec-now) (rec-seg-start *rec*))))
  (setf (rec-state *rec*) :finishing))

(defun rec-end-segment ()
  "Finish the running segment (on a thread — never the environment's: it can take
   seconds). The writer drains the queue and stops; ffmpeg's stdin is closed; then,
   after a second to read the last frames, ffmpeg gets SIGINT, its graceful stop: it
   flushes the encoder and writes the file's index (+faststart). -shortest alone is not
   enough: the meter's output is the live mic only and never ends (found 2026-10-18:
   every segment waited for the timeout). So exit 255 — ffmpeg's answer to SIGINT —
   is success too, when ffprobe finds a video in the file. -> the path, or NIL."
  (let ((n (1+ (length (rec-segments *rec*))))
        (p (rec-process *rec*)))
    (sb-thread:with-mutex (*rec-lock*)
      (setf *rec-queue* (append *rec-queue* (list :end)))
      (sb-thread:condition-notify *rec-queue-cv*))
    (ignore-errors (sb-thread:join-thread *rec-writer* :timeout 60))
    (ignore-errors (close (sb-ext:process-input p)))
    (sleep 1)
    (when (sb-ext:process-alive-p p) (sb-ext:process-kill p sb-unix:sigint))
    (let ((deadline (+ (rec-now) (* 20 internal-time-units-per-second))))
      (loop while (and (sb-ext:process-alive-p p) (< (rec-now) deadline)) do (sleep 0.05))
      (when (sb-ext:process-alive-p p) (sb-ext:process-kill p sb-unix:sigkill)))
    (sb-ext:process-wait p)
    (setf (rec-process *rec*) nil (rec-fd *rec*) nil)
    (let ((path (rec-segment-path n)) (code (sb-ext:process-exit-code p)))
      (if (and (member code '(0 255)) (probe-file path)
               (ignore-errors (getf (probe-video path) :video)))
          (progn (setf (rec-segments *rec*) (append (rec-segments *rec*) (list path))) path)
          (progn (rec-say "segment ~d failed (ffmpeg exit ~a) — see ~a" n code (rec-file "ffmpeg.log"))
                 nil)))))

;;; ---- the sampler: a service in the poll loop -----------------------------------------------

(defun rec-timeout ()
  "ms until the next frame is due while recording; while joining, a second (the
   progress); else -1."
  (cond ((rec-recording-p)
         (max 0 (ceiling (* 1000 (rec-seconds (- (rec-next-slot *rec*) (rec-now)))))))
        ((and *rec* (eq (rec-state *rec*) :joining)) 1000)
        (t -1)))

(defun rec-sample (canvas)
  "One frame: a fresh copy when the canvas or the pointer changed (and a buffer is
   free), else the last frame again."
  (let* ((pointer (list *cursor-x* *cursor-y* (or *cursor-hidden* (null *fb-mouse*))))
         (changed (or *rec-dirty* (not (equal pointer *rec-pointer*)) (null *rec-last*)))
         (buffer (and changed (rec-free-buffer))))
    (cond (buffer
           (replace (lol.canvas:canvas-pixels buffer) (lol.canvas:canvas-pixels canvas))
           (unless (third pointer) (draw-cursor buffer (first pointer) (second pointer)))
           (setf *rec-last* buffer *rec-dirty* nil *rec-pointer* pointer))
          (changed (incf (rec-stale *rec*))))      ; no buffer: repeat, and say so later
    (rec-enqueue *rec-last*)))

(defun rec-service-run (canvas dirty)
  (declare (ignore dirty))
  (when (rec-recording-p)
    ;; every slot that is due gets a frame, so a late wake-up still writes the right
    ;; NUMBER of frames: the count is the clock
    (loop while (and (rec-recording-p) (>= (rec-now) (rec-next-slot *rec*)))
          do (rec-sample canvas)
             (incf (rec-slot *rec*))
             (setf (rec-next-slot *rec*)
                   (+ (rec-seg-start *rec*)
                      (round (* (rec-slot *rec*) internal-time-units-per-second) *rec-fps*))))
    (unless (sb-thread:thread-alive-p *rec-writer*)
      (rec-say "the encoder stopped (see ffmpeg.log); recording ended")
      (ignore-errors (rec-stop))))
  (when (rec-pane-shown-p) (refresh-recorder-panes))
  nil)

(fb-register-service :recorder :timeout 'rec-timeout :run 'rec-service-run)

;;; ---- preflight ----------------------------------------------------------------------------

(defun rec-preflight ()
  "Every reason a recording cannot start, as strings (none: it can)."
  (let ((problems '()))
    (flet ((no (fmt &rest args) (push (apply #'format nil fmt args) problems)))
      (cond ((not (sound-card-p))
             (no "no sound card: this kernel has none (tag #29 or later), or no card was found"))
            ((not (let ((fd (sb-unix:unix-open "/dev/snd/pcmC0D0c" (logior sb-unix:o_rdonly +o-nonblock+) 0)))
                    (when fd (sb-unix:unix-close fd) t)))
             (no "cannot open /dev/snd/pcmC0D0c (lol-rootd gives /dev/snd to the user; or the mic is busy)")))
      (let ((root (rec-root)))
        (if (null root)
            (no "no home on a disk: there is nowhere to keep a recording")
            (let ((free (ignore-errors (disk-free (string-right-trim "/" (lol-home))))))
              (when (and free (< free *rec-min-free*))
                (no "only ~,1f GB free in your home; a recording wants at least 1 GB" (/ free 1e9))))))
      (cond ((not (containers-available-p)) (no "no containers: the host's ffmpeg runs in one"))
            ((not (probe-file (host-path "/usr/bin/ffmpeg"))) (no "the host has no /usr/bin/ffmpeg"))))
    (nreverse problems)))

;;; ---- start, pause, resume, stop -------------------------------------------------------------

(defun rec-start ()
  "Begin a recording. -> a message for the status line."
  (when (rec-active-p) (return-from rec-start "already recording"))
  (let ((problems (rec-preflight)))
    (when problems
      (setf *rec* (%make-rec :state :failed :message (format nil "~{~a~^; ~}" problems)))
      (return-from rec-start (format nil "cannot record: ~a" (first problems)))))
  (let* ((stamp (rec-stamp))
         (dir (ensure-private-dir (format nil "~a~a/" (rec-root) stamp))))
    (ensure-private-dir (rec-root))
    (setf *rec* (%make-rec :state :preflight :dir dir :name (format nil "screencast-~a.mp4" stamp)
                           :w (lol.canvas:canvas-w *fb-canvas*) :h (lol.canvas:canvas-h *fb-canvas*)))
    (let ((fifo (rec-meter-fifo)))
      (unless (probe-file fifo) (%mkfifo fifo #o600)))
    (rec-make-pool (rec-w *rec*) (rec-h *rec*))
    (handler-case (rec-start-segment)
      (error (e) (setf (rec-state *rec*) :failed)
        (return-from rec-start (rec-say "could not start ffmpeg: ~a" e))))
    (forget-rec-tray)
    (rec-say "recording ~dx~d to ~a" (rec-w *rec*) (rec-h *rec*) dir)
    "Recording — SuperL-r pauses, SuperL-S-r stops"))

(defun rec-in-background (name fn)
  "Run FN on a thread: a segment's end and the join take seconds, and the environment's
   thread must not wait for them."
  (sb-thread:make-thread (lambda () (handler-case (funcall fn)
                                      (error (e) (setf (rec-state *rec*) :failed) (rec-say "~a" e)))
                           (forget-rec-tray))
                         :name name))

(defun rec-pause ()
  (when (rec-recording-p)
    (rec-close-segment)
    (setf (rec-after *rec*) :paused)
    (rec-in-background "rec-pause"
                       (lambda () (setf (rec-state *rec*) (if (rec-end-segment) :paused :failed))))
    (forget-rec-tray)
    (format nil "Paused at ~a — SuperL-r resumes, SuperL-S-r stops" (mm-ss (rec-time)))))

(defun rec-resume ()
  (when (and *rec* (eq (rec-state *rec*) :paused))
    (handler-case (rec-start-segment)
      (error (e) (setf (rec-state *rec*) :failed) (return-from rec-resume (rec-say "could not resume: ~a" e))))
    (forget-rec-tray)
    (format nil "Recording again (segment ~d)" (1+ (length (rec-segments *rec*))))))

(defun rec-stop ()
  "End the recording: stop sampling now; on a thread, finish the last segment, join
   the segments and probe the result. -> a message."
  (when (and *rec* (member (rec-state *rec*) '(:recording :paused)))
    (let ((running (rec-recording-p)))
      (when running (rec-close-segment))
      (setf (rec-after *rec*) :stop)
      (rec-in-background "rec-stop"
                         (lambda ()
                           (when running (rec-end-segment))
                           (if (null (rec-segments *rec*))
                               (setf (rec-state *rec*) :failed)
                               (progn (setf (rec-state *rec*) :joining (rec-progress *rec*) 0)
                                      (rec-finish)))))
      (forget-rec-tray)
      (format nil "Stopped at ~a — saving…" (mm-ss (rec-time))))))

(defun rec-finish ()
  "The join thread: one segment is simply renamed; several are joined with the
   concat demuxer and -c copy (no re-encoding), its -progress file read for the
   percentage. Then ffprobe describes the result, kept in result.lisp."
  (handler-case
      (let* ((segs (rec-segments *rec*))
             (out (rec-file (rec-name *rec*))))
        (if (= 1 (length segs))
            (rename-file (first segs) out)
            (let ((list (rec-file "segments.txt")) (prog (rec-file "progress.txt")))
              (with-open-file (s list :direction :output :if-exists :supersede)
                (dolist (seg segs) (format s "file '~a'~%" seg)))
              (let ((p (rec-spawn (list "ffmpeg" "-hide_banner" "-loglevel" "warning" "-y"
                                        "-progress" prog "-nostats"
                                        "-f" "concat" "-safe" "0" "-i" list
                                        "-c" "copy" "-movflags" "+faststart" out))))
                (loop while (sb-ext:process-alive-p p)
                      do (sleep 0.5)
                         (let ((us (progress-out-time-us prog)))
                           (when us
                             (setf (rec-progress *rec*)
                                   (min 99 (round (* 100 (/ us 1e6)) (max 1 (rec-elapsed *rec*))))))))
                (unless (eql 0 (sb-ext:process-exit-code p))
                  (error "joining the segments failed (ffmpeg exit ~a)" (sb-ext:process-exit-code p))))
              (dolist (seg segs) (ignore-errors (delete-file seg)))
              (ignore-errors (delete-file list)) (ignore-errors (delete-file prog))))
        (let ((result (probe-video out)))
          (setf result (append result (list :stale (rec-stale *rec*) :frames (rec-frames *rec*)
                                            :segments (length segs) :peak (round (rec-peak *rec*))
                                            :file out)))
          (with-open-file (s (rec-file "result.lisp") :direction :output :if-exists :supersede)
            (with-standard-io-syntax (prin1 result s) (terpri s)))
          (setf (rec-result *rec*) result (rec-progress *rec*) 100 (rec-state *rec*) :done)
          (rec-say "saved ~a (~a, ~,1f MB)" out (mm-ss (or (getf result :duration) 0))
                   (/ (or (getf result :size) 0) 1e6))))
    (error (e)
      (setf (rec-state *rec*) :failed)
      (rec-say "~a" e)))
  (forget-rec-tray))

(defun progress-out-time-us (path)
  "The last out_time_us= ffmpeg's -progress wrote to PATH, or NIL."
  (with-open-file (s path :if-does-not-exist nil)
    (when s
      (let ((last nil))
        (loop for line = (read-line s nil) while line
              when (and (> (length line) 12) (string= "out_time_us=" line :end2 12))
                do (setf last (ignore-errors (parse-integer line :start 12))))
        last))))

(defun probe-video (path)
  "What ffprobe says of PATH: (:duration s :size bytes :width :height :video codec
   :audio codec :fps \"30/1\"), from its JSON (lol.json)."
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

;;; ---- the keys ------------------------------------------------------------------------------

(defun recorder-key (stop)
  "SuperL-r (STOP nil): start, pause or resume. SuperL-S-r (STOP t): stop. Sets the
   status line."
  (setf *browser-status*
        (handler-case
            (cond (stop (or (rec-stop) "not recording"))
                  ((rec-recording-p) (rec-pause))
                  ((and *rec* (eq (rec-state *rec*) :paused)) (rec-resume))
                  ((rec-active-p) "a moment: the recorder is finishing a segment")
                  (t (rec-start)))
          (error (e) (format nil "recorder: ~a" e)))))

;;; ---- the tray --------------------------------------------------------------------------------

(defvar *rec-tray-cache* nil)
(defun forget-rec-tray () (setf *rec-tray-cache* nil))

(defun recorder-tray-item ()
  "REC 01:23 #####..... (the mic) / PAUSED 01:23 / SAVING 42% / nothing."
  (when *rec*
    (case (rec-state *rec*)
      (:recording (list :kind :recording :tone :low
                        :text (format nil "REC ~a ~a" (mm-ss (rec-time)) (meter-bar (rec-level *rec*) 6))))
      ((:paused) (list :kind :recording :tone :warn :text (format nil "PAUSED ~a" (mm-ss (rec-time)))))
      (:finishing (if (eq (rec-after *rec*) :paused)
                      (list :kind :recording :tone :warn :text (format nil "PAUSED ~a" (mm-ss (rec-time))))
                      (list :kind :recording :tone :warn :text "SAVING 0%")))
      ((:joining) (list :kind :recording :tone :warn
                                   :text (format nil "SAVING ~d%" (rec-progress *rec*)))))))

;;; ---- the recorder pane and the recordings -----------------------------------------------------

(defstruct (subj-recorder (:constructor subj-recorder ())))
(defstruct (subj-recording (:constructor subj-recording (dir))) dir)

(defun recording-dirs ()
  "Every recording kept, newest first: the folders of ~/.lol/rec/ with a result.lisp."
  (let ((root (rec-root)))
    (when root
      (sort (mapcar (lambda (p) (namestring (make-pathname :name nil :type nil :defaults p)))
                    (directory (concatenate 'string root "*/result.lisp")))
            #'string>))))

(defun read-result (dir)
  (with-open-file (s (concatenate 'string dir "result.lisp") :if-does-not-exist nil)
    (and s (let ((*read-eval* nil)) (ignore-errors (read s))))))

(defun result-summary (r)
  (format nil "~a  ~,1f MB  ~ax~a  ~a/~a~@[  ~a segments~]~:[~;  (silent!)~]"
          (mm-ss (or (getf r :duration) 0)) (/ (or (getf r :size) 0) 1e6)
          (getf r :width) (getf r :height) (getf r :video) (getf r :audio)
          (let ((n (getf r :segments))) (and n (> n 1) n))
          (and (getf r :peak) (< (getf r :peak) -50))))

(defmethod present ((s subj-recorder))
  (let ((items '()) (r *rec*))
    (flet ((row (group label &key detail kind subject)
             (push (make-item :label label :detail detail :kind kind :group group :subject subject) items)))
      (row "Recorder — SuperL-r record / pause / resume, SuperL-S-r stop"
           (if r (format nil "~(~a~)~@[  ~a~]" (rec-state r)
                         (and (member (rec-state r) '(:recording :paused :finishing)) (mm-ss (rec-time))))
               "idle")
           :detail (and r (rec-message r)) :kind (and r (eq (rec-state r) :failed) :condition))
      (when (and r (member (rec-state r) '(:recording :paused)))
        (row "Recorder — SuperL-r record / pause / resume, SuperL-S-r stop"
             (format nil "mic ~a ~4d dB" (meter-bar (rec-level r) 20) (round (rec-level r)))
             :detail (format nil "loudest so far ~d dB~:[~;  — is the mic on?~]" (round (rec-peak r))
                             (and (> (rec-time) 5) (< (rec-peak r) -50))))
        (row "Recorder — SuperL-r record / pause / resume, SuperL-S-r stop"
             (format nil "video ~dx~d @ ~d  ~d frames~@[, ~d stale~]" (rec-w r) (rec-h r) *rec-fps*
                     (rec-frames r) (and (plusp (rec-stale r)) (rec-stale r)))
             :detail (format nil "segment ~d" (1+ (length (rec-segments r))))))
      (when (and r (eq (rec-state r) :joining))
        (row "Recorder — SuperL-r record / pause / resume, SuperL-S-r stop"
             (format nil "saving ~d%" (rec-progress r))))
      (when (null (rec-root))
        (row "Recorder — SuperL-r record / pause / resume, SuperL-S-r stop"
             "no home on a disk: nothing can be recorded" :kind :condition))
      (dolist (dir (recording-dirs))
        (let ((res (read-result dir)))
          (row "Recordings — Enter opens, p plays" (pathname-name (getf res :file))
               :detail (result-summary res) :subject (subj-recording dir)))))
    (make-view :title "recorder" :kind :list :subject s :items (nreverse items))))

(defmethod present ((s subj-recording))
  (let* ((dir (subj-recording-dir s)) (r (read-result dir)) (items '()))
    (flet ((row (label detail) (push (make-item :label label :detail detail :group "Recording — p play, f show in files, D delete") items)))
      (row "file" (getf r :file))
      (row "length" (mm-ss (or (getf r :duration) 0)))
      (row "size" (format nil "~,1f MB" (/ (or (getf r :size) 0) 1e6)))
      (row "video" (format nil "~a ~ax~a at ~a fps" (getf r :video) (getf r :width) (getf r :height) (getf r :fps)))
      (row "sound" (format nil "~a ~a Hz, loudest ~a dB~:[~;  — silent? check the mic~]"
                           (getf r :audio) (getf r :sample-rate) (getf r :peak)
                           (and (getf r :peak) (< (getf r :peak) -50))))
      (row "frames" (format nil "~a written~@[, ~a stale (the encoder fell behind)~]" (getf r :frames)
                            (let ((n (getf r :stale))) (and n (plusp n) n))))
      (row "segments" (princ-to-string (getf r :segments))))
    (make-view :title (pathname-name (getf r :file)) :kind :list :subject s :items (nreverse items))))

(defmethod subject-hints ((s subj-recorder) pane)
  (declare (ignore pane))
  '(("Super-r" . "record/pause/resume") ("Super-S-r" . "stop") ("ret" . "open") ("p" . "play")))
(defmethod subject-hints ((s subj-recording) pane)
  (declare (ignore pane))
  '(("p" . "play") ("w" . "play in a window") ("f" . "show in files") ("D" . "delete")))

(defun selected-recording (pane)
  (let* ((it (selected-item pane)) (s (and it (item-subject it))))
    (and (subj-recording-p s) s)))

(defun recording-file (s) (getf (read-result (subj-recording-dir s)) :file))

(defun recording-key (s key)
  (case key
    (#\p (play-video (recording-file s)) t)
    (#\w (play-video (recording-file s) :window t) t)
    ;; a new tab on the recording's own folder, its video selected (OPEN-FILES would
    ;; focus whichever folder tab is already open instead)
    (#\f (new-trail (subj-dir (normalize-path (subj-recording-dir s))
                              :select (file-namestring (recording-file s))))
     t)
    (#\D (if (rec-active-p) (setf *browser-status* "not while recording")
             (progn (delete-recording s) (refresh-recorder-panes)))
     t)))

(defmethod subject-key ((s subj-recorder) pane key state)
  (declare (ignore state))
  (let ((r (selected-recording pane)))
    (and r (member key '(#\p #\w #\f #\D)) (recording-key r key))))

(defmethod subject-key ((s subj-recording) pane key state)
  (declare (ignore pane state))
  (recording-key s key))

(defun delete-recording (s)
  "Delete a recording's folder: its video, result and log."
  (let ((dir (subj-recording-dir s)))
    (dolist (f (directory (concatenate 'string dir "*.*"))) (ignore-errors (delete-file f)))
    (ignore-errors (%rmdir (string-right-trim "/" dir)))
    (setf *browser-status* (format nil "deleted ~a" dir))))

(defun rec-pane-shown-p ()
  (some (lambda (trail) (some (lambda (p) (subj-recorder-p (pane-subject p))) trail)) *trails*))

(defvar *rec-refreshed* 0)
(defun refresh-recorder-panes ()
  "Re-present the recorder panes, at most twice a second."
  (when (> (- (rec-now) *rec-refreshed*) (floor internal-time-units-per-second 2))
    (setf *rec-refreshed* (rec-now))
    (dolist (trail *trails*)
      (dolist (p trail)
        (when (subj-recorder-p (pane-subject p))
          (let ((v (ignore-errors (present (pane-subject p)))))
            (when v (setf (pane-view p) v))))))))

(defun open-recorder ()
  (let ((i (position-if (lambda (trail) (subj-recorder-p (pane-subject (first trail)))) *trails*)))
    (if i (progn (activate-trail i) (setf *rec-refreshed* 0) (refresh-recorder-panes))
        (new-trail (subj-recorder)))))

(defun recorder-spotter-candidates ()
  (list (list :label "record" :detail "surface · the screen recorder (SuperL-r)" :group "surfaces"
              :subject (subj-recorder))
        (list :label "recordings" :detail "surface · screencasts kept in ~/.lol/rec" :group "surfaces"
              :subject (subj-recorder))))

(pushnew 'recorder-spotter-candidates *spotter-sources*)
