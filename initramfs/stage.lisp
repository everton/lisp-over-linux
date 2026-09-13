;;;; stage.lisp — the COLLIDE specimens, moving, dispatching for real (§3b).
;;;;
;;;; The dispatch matrix (§3) shows which collisions HAVE a rule. This shows collisions
;;;; happening: every sprite is a live CLOS instance from lol.examples, every contact is
;;;; a real COLLIDE call, and every hole in the matrix is a no-applicable-method waiting
;;;; to open the debugger with the whole stack standing.
;;;;
;;;; Three rules shape everything here, all of them decisions rather than accidents:
;;;;
;;;;   1. THE SPECIMEN STAYS PURE. lol.examples::space-object gains no slots. A class
;;;;      whose job is to be four readable lines in a class view must not sprout x/y/vx/vy
;;;;      for a demo's benefit. Positions live in BODY structs here; the instance is a
;;;;      slot OF the body, never the other way round. Nothing on screen can tell you it
;;;;      has ever been on a stage.
;;;;
;;;;   2. BOTH DIRECTIONS, EVERY CONTACT. Contact is symmetric and COLLIDE is not, so
;;;;      rather than inventing an argument order we call it both ways and record both.
;;;;      That makes the author's symmetric pairs visible ((ship asteroid) delegates to
;;;;      (asteroid ship)) and makes the matrix's missing MISSILE column load-bearing:
;;;;      nothing specialises argument B on a missile, so every missile hit prints one
;;;;      value and one "no applicable method". ONLY A TOTAL ABSENCE SIGNALS — one side
;;;;      answering is the ordinary case. Two ships, where neither direction resolves,
;;;;      is what opens the debugger.
;;;;
;;;;   3. COLLIDE RETURNS DATA; STAGE-ENACT MAKES IT HAPPEN. The effect of a collision
;;;;      is a consequence of the value COLLIDE returned, so redefining a method visibly
;;;;      changes the world without the stage knowing anything happened. STAGE-ENACT
;;;;      dispatches with EQL specialisers on the returned tag — the browser's first
;;;;      eql-specialised generic function, which is a dispatch shape worth having a real
;;;;      specimen of.
;;;;
;;;; No pixels here. This file loads before canvas.lisp; the drawing lives in
;;;; poc/shell.lisp (draw-stage-body), the same way present.lisp describes and the shell
;;;; paints. The tick is driven by the browser's own poll loop (fb-browser.lisp) — there
;;;; is no thread.
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

;;; ---- geometry per class -----------------------------------------------------
;;; Radius and speed are the only per-class physics. They are here rather than in
;;; examples.lisp for rule 1: the specimen must not know it is being drawn.

(defparameter *stage-radius*
  '(:asteroid 26 :ship 20 :station 40 :missile 11)
  "Collision radius in pixels, by class name keyword.")

(defparameter *stage-speed*
  '(:asteroid 55 :ship 110 :station 8 :missile 240)
  "Nominal speed in hundredths of a pixel per tick, by class name keyword.

   Tuned so the opening scene produces its first contact inside a couple of seconds at
   sixty ticks a second. A stage you have to wait at is a stage nobody watches long
   enough to see the point.")

(defparameter *stage-names*
  '(:ship     ("Kestrel" "Wren" "Petrel" "Fulmar" "Shrike")
    :asteroid ("Ceres-9" "Vesta-4" "Juno-7" "Pallas-2")
    :missile  ("Hornet" "Wasp" "Dart" "Sting")
    :station  ("Anvil" "Bastion" "Keystone"))
  "Proper nouns, so a transcript line reads. (:DESTROYED \"Kestrel\") tells you which
   ship at a glance; (:DESTROYED \"ship-2\") does not.")

(defparameter +stage-transcript-max+ 64
  "Transcript lines kept. The pane shows the tail; the rest is dropped, not scrolled —
   this is a running log, not a document.")

;;; ---- the body: where a sprite's position lives ------------------------------

(defstruct (body (:constructor %make-body))
  "One thing on the stage. OBJ is the CLOS instance — the subject of every click and
   every COLLIDE call. Everything else is the stage's own bookkeeping, held HERE so
   the instance stays a clean specimen (rule 1)."
  obj                     ; the lol.examples instance
  kind                    ; its class name as a keyword, cached (:ship :missile …)
  (x 0) (y 0)             ; centre, in whole stage pixels
  (vx 0) (vy 0)           ; velocity, hundredths of a pixel per tick
  (ax 0) (ay 0)           ; sub-pixel remainder, hundredths. Without these a body
                          ; slower than one pixel per tick — every asteroid, and the
                          ; station — rounds to zero and never moves at all.
  (r 10)                  ; collision radius
  (rot 0) (spin 0)        ; facing, in milliradians, for the classes that tumble
  (flash 0)               ; ticks left on the impact highlight
  (flash-kind :method)    ; :method or :condition — which colour the flash wears
  (docked nil)            ; a body this one is parked on, or NIL
  (breached nil)          ; a ship that survived a hull breach wears a mark
  (poly nil))             ; per-instance rock silhouette: radius multipliers, percent

(defstruct (stage (:constructor %make-stage))
  "A stage is the whole demo's state, and it is ordinary data — no threads, no timers,
   no globals beyond the one the view holds. STEP-STAGE advances it; everything else
   reads it."
  (w 800) (h 520)
  (bodies '())
  (transcript '())        ; newest FIRST; each entry a plist (:text :kind)
  (running t)
  (ticks 0)
  (seed 20260913)         ; deterministic: a headless frame must replay exactly
  (next-id 1)
  (cooldown '())          ; alist ((id-a . id-b) . ticks-left)
  (pass '())              ; pairs told to drift through each other
  (selected nil)          ; a BODY, or NIL
  (pending nil))          ; a body pair awaiting a debugger decision, or NIL

;;; ---- a deterministic little RNG ---------------------------------------------
;;; CL's RANDOM is fine, but a stage frame in the docs has to be reproducible, and
;;; *random-state* is global. A 31-bit LCG owned by the stage is two lines and makes
;;; (render a stage, tick it 240 times) identical on every machine.

(defun stage-rand (stage n)
  "The next pseudo-random integer in [0,N), advancing STAGE's own seed."
  (setf (stage-seed stage) (mod (+ (* 1103515245 (stage-seed stage)) 12345) 2147483648))
  (mod (ash (stage-seed stage) -8) (max 1 n)))

;;; ---- the transcript ----------------------------------------------------------

(defun stage-log (stage text &optional (kind :value))
  "Push a line onto STAGE's transcript. KIND is :value (a method answered), :absent
   (this direction had none), :warn (the two directions disagree) or :note."
  (push (list :text text :kind kind) (stage-transcript stage))
  (let ((tr (stage-transcript stage)))
    (when (> (length tr) +stage-transcript-max+)
      (setf (stage-transcript stage) (subseq tr 0 +stage-transcript-max+))))
  text)

(defun ppo (obj)
  "An instance the way the transcript names it: #<SHIP Kestrel>."
  (format nil "#<~a ~a>" (class-name (class-of obj)) (lol.examples:name obj)))

;;; ---- building a stage --------------------------------------------------------

(defun class-keyword (obj)
  "OBJ's class name as a keyword — :ship, :missile … The stage keys its per-class
   geometry by this rather than by the class object, so a table stays readable."
  (intern (symbol-name (class-name (class-of obj))) :keyword))

(defun stage-name-for (stage kind)
  "An unused proper noun for KIND, or a generated one when the pool runs dry."
  (let* ((pool (getf *stage-names* kind))
         (taken (mapcar (lambda (b) (lol.examples:name (body-obj b))) (stage-bodies stage)))
         (free (remove-if (lambda (n) (member n taken :test #'string=)) pool)))
    (if free
        (nth (stage-rand stage (length free)) free)
        (format nil "~(~a~)-~d" kind (stage-next-id stage)))))

(defun stage-spawn (stage kind &key name x y dir)
  "Make a KIND instance, give it a body, and put it on STAGE. Returns the body.

   The instance is constructed exactly as anyone would at the REPL — (make-instance
   'lol.examples:ship :name \"Kestrel\") — which is the point: these are not sprites
   that happen to wrap objects, they are objects that happen to be drawn."
  (let* ((cls (ecase kind
                (:asteroid 'lol.examples:asteroid)
                (:ship     'lol.examples:ship)
                (:station  'lol.examples:station)
                (:missile  'lol.examples:missile)))
         (nm  (or name (stage-name-for stage kind)))
         (obj (make-instance cls :name nm))
         (r   (getf *stage-radius* kind))
         (spd (getf *stage-speed* kind))
         (ang (or dir (stage-rand stage 6283)))          ; milliradians
         (b (%make-body
             :obj obj :kind kind
             :x (or x (+ r (stage-rand stage (max 1 (- (stage-w stage) (* 2 r))))))
             :y (or y (+ r (stage-rand stage (max 1 (- (stage-h stage) (* 2 r))))))
             :r r
             :vx (round (* spd (cos (/ ang 1000.0))))
             :vy (round (* spd (sin (/ ang 1000.0))))
             :spin (case kind
                     (:asteroid (- (stage-rand stage 16) 8))
                     (:station 2)
                     (t 0)))))
    (when (eq kind :asteroid)
      (setf (body-poly b) (loop repeat 9 collect (+ 72 (stage-rand stage 46)))))
    (setf (stage-next-id stage) (1+ (stage-next-id stage)))
    (setf (stage-bodies stage) (append (stage-bodies stage) (list b)))
    b))

(defun aim (from-x from-y to-x to-y)
  "The direction from one point to another, in milliradians — how the opening scene
   says what it means. Seeding a body with a bare angle and hoping it runs into
   something is how you get a stage where nothing happens for twenty seconds."
  (round (* 1000 (atan (- to-y from-y) (- to-x from-x)))))

(defun make-collide-stage (&key (w 800) (h 520))
  "The opening scene: a station to dock with, a ship aimed at it, a rock drifting
   across, and a missile aimed at the rock.

   Every pair here resolves in at least one direction, so the stage runs without
   meeting the debugger until you ask it to — spawn a second ship and the (ship ship)
   hole in the matrix opens it. The three seeded contacts each demonstrate something
   different: the ship DOCKS (a collision method that isn't a collision), the missile
   is DEFLECTED by the rock (the missile loses), and either of those against the
   station shows the one-sided dispatch, since nothing specialises argument B on a
   missile."
  (flet ((pct (n p) (floor (* n p) 100)))
    (let* ((s (%make-stage :w w :h h))
           (ax (pct w 62)) (ay (pct h 45))          ; Anvil, the station
           (kx (pct w 22)) (ky (pct h 26))          ; Kestrel, aimed at it
           (cx (pct w 84)) (cy (pct h 74))          ; Ceres-9, drifting across
           (hx (pct w 10)) (hy (pct h 80)))         ; Hornet, aimed at the rock
      (stage-spawn s :station  :name "Anvil"   :x ax :y ay)
      (stage-spawn s :ship     :name "Kestrel" :x kx :y ky :dir (aim kx ky ax ay))
      (stage-spawn s :asteroid :name "Ceres-9" :x cx :y cy :dir (aim cx cy (pct w 30) (pct h 42)))
      (stage-spawn s :missile  :name "Hornet"  :x hx :y hy :dir (aim hx hy cx cy))
      (stage-log s "stage collide — 4 objects seeded" :note)
      s)))

(defun stage-resize (stage w h)
  "Give STAGE a new viewport, carrying the bodies with it proportionally.

   The pane is the authority on how much room there is, and it only finds out at draw
   time — so a stage is always built at some provisional size and resized on its first
   frame. Scaling the positions rather than just widening the walls is what keeps that
   from stranding every body in a corner of a suddenly much larger world; it also means
   a headless frame and the guest agree about where things are, which is the whole
   point of the deterministic seed."
  (unless (and (= w (stage-w stage)) (= h (stage-h stage)))
    (let ((sx (/ (float w) (max 1 (stage-w stage))))
          (sy (/ (float h) (max 1 (stage-h stage)))))
      (dolist (b (stage-bodies stage))
        (setf (body-x b) (round (* (body-x b) sx))
              (body-y b) (round (* (body-y b) sy))
              ;; velocity scales with the world too, or the scene's TIMING becomes a
              ;; function of the window size: the same seeded contact that lands three
              ;; seconds in on a small pane would take eleven on a full screen. The
              ;; opening scene is authored in proportions, so it has to run in them.
              (body-vx b) (round (* (body-vx b) sx))
              (body-vy b) (round (* (body-vy b) sy))))
      (setf (stage-w stage) w (stage-h stage) h)))
  stage)

(defun stage-remove (stage body)
  (setf (stage-bodies stage) (remove body (stage-bodies stage)))
  (dolist (b (stage-bodies stage))
    (when (eq (body-docked b) body) (setf (body-docked b) nil)))
  (when (eq (stage-selected stage) body) (setf (stage-selected stage) nil))
  body)

(defun stage-find (stage name)
  "The INSTANCE named NAME, or NIL — what (find-obj \"Kestrel\") gives you in the
   Workspace. Deliberately returns the object, not the body: the body is the stage's
   business, the object is yours."
  (let ((b (find name (stage-bodies stage)
                 :key (lambda (b) (lol.examples:name (body-obj b)))
                 :test #'string-equal)))
    (and b (body-obj b))))

(defun stage-body-of (stage obj)
  (find obj (stage-bodies stage) :key #'body-obj))

;;; ---- motion ------------------------------------------------------------------

(defun stage-pair-key (a b)
  "An order-independent key for a pair of bodies — the identity a cooldown and a
   pass-through are recorded against. Ordered by name so (a b) and (b a) agree."
  (if (string< (lol.examples:name (body-obj a)) (lol.examples:name (body-obj b)))
      (cons (body-obj a) (body-obj b))
      (cons (body-obj b) (body-obj a))))

(defun stage-bounce (a b)
  "Separate two bodies and reflect each velocity about the contact normal. The whole
   of the stage's physics; anything more would be code with nothing to teach."
  (let* ((dx (- (body-x b) (body-x a))) (dy (- (body-y b) (body-y a)))
         (d (max 1 (isqrt (+ (* dx dx) (* dy dy)))))
         (nx (/ dx (float d))) (ny (/ dy (float d)))
         (push-out (max 1 (floor (- (+ (body-r a) (body-r b)) d) 2))))
    (flet ((reflect (bd sx sy)
             (unless (or (eq (body-kind bd) :station) (body-docked bd))
               (let ((dot (+ (* (body-vx bd) sx) (* (body-vy bd) sy))))
                 (when (> dot 0)
                   (setf (body-vx bd) (round (- (body-vx bd) (* 2 dot sx)))
                         (body-vy bd) (round (- (body-vy bd) (* 2 dot sy)))))))))
      (unless (or (eq (body-kind a) :station) (body-docked a))
        (decf (body-x a) (round (* nx push-out)))
        (decf (body-y a) (round (* ny push-out))))
      (unless (or (eq (body-kind b) :station) (body-docked b))
        (incf (body-x b) (round (* nx push-out)))
        (incf (body-y b) (round (* ny push-out))))
      (reflect a nx ny)
      (reflect b (- nx) (- ny)))))

;;; ---- enact: eql dispatch on the returned tag (rule 3) ------------------------

(defgeneric stage-enact (tag stage a b)
  (:documentation "Turn the value COLLIDE returned into something visible on STAGE.

   Specialised with EQL on TAG, so the set of outcomes the stage understands is a set
   of methods you can list, browse and extend — and so the browser has one real
   eql-specialised generic function to look at. A and B are BODIES, in the order of
   the call that answered.

   A tag with no method just bounces, which is the honest default: the stage does not
   pretend to understand a value it has no rule for."))

(defmethod stage-enact ((tag t) stage a b)
  (declare (ignore tag stage))
  (stage-bounce a b))

(defmethod stage-enact ((tag (eql :destroyed)) stage a b)
  "The missile found its mark: the target leaves the stage."
  (declare (ignore tag))
  (stage-remove stage b))

(defmethod stage-enact ((tag (eql :deflected)) stage a b)
  "The missile wasted itself on a rock: the missile leaves."
  (declare (ignore tag b))
  (stage-remove stage a))

(defmethod stage-enact ((tag (eql :shatter)) stage a b)
  "Two rocks become gravel — both leave, two smaller ones take their place."
  (declare (ignore tag))
  (let ((x (floor (+ (body-x a) (body-x b)) 2))
        (y (floor (+ (body-y a) (body-y b)) 2)))
    (stage-remove stage a) (stage-remove stage b)
    (when (< (length (stage-bodies stage)) 8)
      (dotimes (i 2)
        (let ((f (stage-spawn stage :asteroid
                              :x (+ x (if (zerop i) -30 30))
                              :y (+ y (if (zerop i) -20 20)))))
          (setf (body-r f) 15))))))

(defmethod stage-enact ((tag (eql :dock)) stage a b)
  "Not a collision at all — the ship parks on the station's rim and stays there."
  (declare (ignore tag stage))
  (let ((ship (if (eq (body-kind a) :ship) a b))
        (stn  (if (eq (body-kind a) :ship) b a)))
    (setf (body-docked ship) stn (body-vx ship) 0 (body-vy ship) 0)))

(defmethod stage-enact ((tag (eql :hull-breach)) stage a b)
  "The ship survives, wearing the breach."
  (declare (ignore tag stage))
  (let ((ship (if (eq (body-kind a) :ship) a b)))
    (setf (body-breached ship) t))
  (stage-bounce a b))

;;; ---- dispatch: both directions, every contact (rule 2) -----------------------

(defun collide-applicable-p (a b)
  "True when COLLIDE has a method for (A B) in that order. COMPUTE-APPLICABLE-METHODS
   asks the same question the call would, without making it — which is also what the
   stage's heads-up line needs, so the HUD and the dispatch cannot disagree."
  (not (null (compute-applicable-methods #'lol.examples:collide (list a b)))))

(defun stage-signal (stage ba bb)
  "Neither direction resolves: make the call for real and let it signal.

   The four restarts here are the stage's own — established around the call, so the
   debugger's rail offers ways out that mean something in this context rather than
   only ABORT. This is the condition system used the way it is meant to be: the code
   that knows what the recovery options ARE is the code that establishes them."
  (let ((a (body-obj ba)) (b (body-obj bb)))
    (setf (body-flash ba) 20 (body-flash-kind ba) :condition
          (body-flash bb) 20 (body-flash-kind bb) :condition)
    (stage-log stage (format nil "(collide ~a ~a) -> no applicable method (either way)"
                            (ppo a) (ppo b)) :absent)
    (restart-case (lol.examples:collide a b)
      (pass-through ()
        :report "Let them drift through each other."
        (push (stage-pair-key ba bb) (stage-pass stage))
        nil)
      (remove-both ()
        :report "Delete both objects from the stage."
        (stage-remove stage ba) (stage-remove stage bb)
        nil)
      (use-value (v)
        :report "Return a value from this collision."
        (stage-log stage (format nil "  use-value -> ~s" v) :value)
        (push (stage-pair-key ba bb) (stage-pass stage))
        v)
      (define-method ()
        :report "Open the Workspace on a defmethod template for this pair."
        (setf (stage-pending stage) (list (body-kind ba) (body-kind bb)))
        (push (stage-pair-key ba bb) (stage-pass stage))
        nil))))

(defun stage-dispatch (stage ba bb)
  "Resolve a contact between bodies BA and BB.

   Calls COLLIDE both ways and records both. Only when NEITHER direction is applicable
   does it make the call for real — which signals, and opens the debugger with the
   stack still standing. STAGE-ENACT runs once, on whichever direction answered."
  (let* ((a (body-obj ba)) (b (body-obj bb))
         (ab (collide-applicable-p a b))
         (ba* (collide-applicable-p b a)))
    (cond
      ((and (not ab) (not ba*))
       (stage-signal stage ba bb))
      (t
       (let ((va (when ab (lol.examples:collide a b)))
             (vb (when ba* (lol.examples:collide b a))))
         (if ab
             (stage-log stage (format nil "(collide ~a ~a) -> ~s" (ppo a) (ppo b) va) :value)
             (stage-log stage (format nil "(collide ~a ~a) -> no applicable method"
                                     (ppo a) (ppo b)) :absent))
         (if ba*
             (stage-log stage (format nil "(collide ~a ~a) -> ~s" (ppo b) (ppo a) vb) :value)
             (stage-log stage (format nil "(collide ~a ~a) -> no applicable method"
                                     (ppo b) (ppo a)) :absent))
         ;; The pairs the author wrote symmetrically agree. Where they do not — and
         ;; (asteroid asteroid) already does not, since :SHATTER keeps argument order —
         ;; say so rather than silently picking one.
         (when (and ab ba* (not (equal va vb)))
           (stage-log stage "  agreement: asymmetric — the two directions differ" :warn))
         (setf (body-flash ba) 12 (body-flash-kind ba) :method
               (body-flash bb) 12 (body-flash-kind bb) :method)
         (if ab
             (stage-enact (first va) stage ba bb)
             (stage-enact (first vb) stage bb ba)))))))

(defun stage-defmethod-template (stage)
  "The defmethod source the [define-method] restart asks for, or NIL. Consumed by the
   shell, which drops it into the Workspace with the specialisers already filled in —
   the point being that you never retype what the debugger already knew."
  (let ((p (stage-pending stage)))
    (when p
      (setf (stage-pending stage) nil)
      (format nil "(defmethod lol.examples:collide ((a lol.examples:~(~a~)) (b lol.examples:~(~a~)))~%  (list :?? (lol.examples:name a) (lol.examples:name b)))"
              (first p) (second p)))))

(defun step-stage (stage)
  "Advance STAGE one tick: move, bounce off the walls, then resolve AT MOST ONE
   contact.

   One contact per tick on purpose. A tick that resolved several could remove a body
   another pair still refers to, and — more to the point — the transcript is meant to
   be read. Two lines per contact at sixty ticks a second is already the fastest a
   human can follow."
  (incf (stage-ticks stage))
  (let ((w (stage-w stage)) (h (stage-h stage)))
    (dolist (b (stage-bodies stage))
      (when (plusp (body-flash b)) (decf (body-flash b)))
      (incf (body-rot b) (body-spin b))
      (cond
        ((body-docked b)
         (let ((s (body-docked b)))
           (setf (body-x b) (+ (body-x s) (body-r s) (body-r b) -4)
                 (body-y b) (body-y s))))
        (t
         (incf (body-ax b) (body-vx b))
         (incf (body-ay b) (body-vy b))
         (let ((dx (truncate (body-ax b) 100)) (dy (truncate (body-ay b) 100)))
           (incf (body-x b) dx) (decf (body-ax b) (* dx 100))
           (incf (body-y b) dy) (decf (body-ay b) (* dy 100)))
         (let ((r (body-r b)))
           (when (< (body-x b) r)       (setf (body-x b) r          (body-vx b) (abs (body-vx b))))
           (when (> (body-x b) (- w r)) (setf (body-x b) (- w r)    (body-vx b) (- (abs (body-vx b)))))
           (when (< (body-y b) r)       (setf (body-y b) r          (body-vy b) (abs (body-vy b))))
           (when (> (body-y b) (- h r)) (setf (body-y b) (- h r)    (body-vy b) (- (abs (body-vy b))))))))))
  ;; cooldowns
  (setf (stage-cooldown stage)
        (remove-if (lambda (c) (<= (decf (cdr c)) 0)) (stage-cooldown stage)))
  ;; one contact
  (loop for (a . rest) on (stage-bodies stage) do
    (dolist (b rest)
      (let* ((dx (- (body-x b) (body-x a))) (dy (- (body-y b) (body-y a)))
             (d (isqrt (+ (* dx dx) (* dy dy))))
             (key (stage-pair-key a b)))
        (when (and (<= d (+ (body-r a) (body-r b)))
                   (not (assoc key (stage-cooldown stage) :test #'equal))
                   (not (member key (stage-pass stage) :test #'equal))
                   (not (eq (body-docked a) b)) (not (eq (body-docked b) a)))
          (push (cons key 54) (stage-cooldown stage))
          (stage-dispatch stage a b)
          (return-from step-stage stage)))))
  stage)

(defun stage-nearest-pair (stage)
  "The closest pair of bodies and their gap in pixels, or NIL — what the heads-up line
   reports on. Introspection as a HUD: it says which methods WOULD run before they do."
  (let ((best nil) (gap most-positive-fixnum))
    (loop for (a . rest) on (stage-bodies stage) do
      (dolist (b rest)
        (let* ((dx (- (body-x b) (body-x a))) (dy (- (body-y b) (body-y a)))
               (g (- (isqrt (+ (* dx dx) (* dy dy))) (body-r a) (body-r b))))
          (when (< g gap) (setf gap g best (list a b))))))
    (and best (values best gap))))

(defun stage-hud-line (stage)
  "One line naming what would happen if the nearest pair touched now."
  (multiple-value-bind (pair gap) (stage-nearest-pair stage)
    (if (or (null pair) (> gap 160))
        "applicable: nothing in range"
        (let* ((a (body-obj (first pair))) (b (body-obj (second pair)))
               (ab (collide-applicable-p a b)) (ba* (collide-applicable-p b a)))
          (format nil "applicable: (~(~a~) ~(~a~)) ~a  (~(~a~) ~(~a~)) ~a~a"
                  (body-kind (first pair)) (body-kind (second pair)) (if ab "yes" "no")
                  (body-kind (second pair)) (body-kind (first pair)) (if ba* "yes" "no")
                  (cond ((and ab ba*) "")
                        ((or ab ba*) "   one side only")
                        (t "   will signal")))))))

;;; ---- the subject, and its view ------------------------------------------------
;;; A stage is presented like anything else: a subject in, a VIEW out, no pixels. The
;;; :stage kind is new; poc/shell.lisp knows how to paint it, exactly as it already
;;; knows :matrix.

(defstruct (subj-stage (:constructor subj-stage (name)))
  "The stage for generic function NAME. Only COLLIDE has one; the struct is named for
   the general case because the moment a second specimen wants one, nothing here
   changes."
  name)

(defvar *the-collide-stage* nil
  "The one live stage. A stage is a running world, not a query — reopening the pane
   must show you what you left, so it outlives the view that presents it.")

(defun collide-stage ()
  (or *the-collide-stage* (setf *the-collide-stage* (make-collide-stage))))

(defun reset-collide-stage ()
  (setf *the-collide-stage* (make-collide-stage)))

(defmethod present ((s subj-stage))
  (let ((st (collide-stage)))
    (make-view :title (format nil "stage ~(~a~)" (subj-stage-name s))
               :kind :stage :subject s :data st)))

(defmethod commands-for ((s subj-stage))
  (let ((st (collide-stage)))
    (list (make-command :label (if (stage-running st) "Pause" "Run") :key "spc"
                        :surfaces '(:menu :key)
                        :action (lambda () (setf (stage-running st) (not (stage-running st)))))
          (make-command :label "Step one tick" :key "n" :surfaces '(:menu :key)
                        :action (lambda () (step-stage st)))
          (make-command :label "Spawn a ship" :key "s" :surfaces '(:menu :key)
                        :action (lambda () (stage-spawn st :ship)))
          (make-command :label "Spawn an asteroid" :key "a" :surfaces '(:menu :key)
                        :action (lambda () (stage-spawn st :asteroid)))
          (make-command :label "Spawn a missile" :key "m" :surfaces '(:menu :key)
                        :action (lambda () (stage-spawn st :missile)))
          (make-command :label "Spawn a station" :key "t" :surfaces '(:menu :key)
                        :action (lambda () (stage-spawn st :station)))
          (make-command :label "Dispatch matrix" :surfaces '(:menu :halo)
                        :target (subj-matrix (subj-stage-name s)))
          (make-command :label "Reset the stage" :surfaces '(:menu)
                        :action (lambda () (reset-collide-stage))))))

;;; present.lisp loads first and cannot mention this file, so the generic-function
;;; view asks through this hook whether the GF it is presenting has a stage.
(setf *stage-subject-fn*
      (lambda (name)
        (when (eq name 'lol.examples:collide)
          (subj-stage name))))
