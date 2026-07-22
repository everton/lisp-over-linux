;;;; examples.lisp — a tiny, self-contained gallery of CLOS specimens for the browser.
;;;;
;;;; Everything here exists to be *looked at* in the code browser, not to do real work.
;;;; The centrepiece is COLLIDE: a generic function that dispatches on BOTH of its
;;;; arguments (multiple dispatch), so its dispatch matrix is a genuine 2-D grid —
;;;; (row class × column class) — with real gaps where nobody wrote a rule. Single-
;;;; argument generic functions (like our own PRESENT) can only ever degrade to a
;;;; list; this is the example that shows the grid.
;;;;
;;;; To see it: Super-Space -> "collide" -> drill "dispatch matrix". Click a filled
;;;; cell to drop into that class-pair's effective-method onion; the (missile ship)
;;;; cell has :around/:before/:after methods so its onion has real layers.
;;;;
;;;; All original code, under this project's MIT licence.

(defpackage :lol.examples
  (:use :cl)
  (:documentation "Didactic CLOS specimens for the code browser — see COLLIDE and LAUNCH.")
  (:export #:space-object #:asteroid #:ship #:station #:missile
           #:name #:collide
           #:off-course #:steer #:launch))

(in-package :lol.examples)

;;; Keep full debug info: this package exists to be inspected, so every local should
;;; stay live in the debugger (no variables optimised away at the error's PC).
(declaim (optimize (debug 3)))

;;; ---- a little class hierarchy ------------------------------------------------
;;; A common base with a name, then four concrete kinds of thing in space. No method
;;; specialises on SPACE-OBJECT itself, so it stays off the dispatch grid — the grid's
;;; axes are only the classes some method actually mentions.

(defclass space-object ()
  ((name :initarg :name :reader name :initform "?"))
  (:documentation "Anything that can drift through space and bump into things."))

(defclass asteroid (space-object) ()
  (:documentation "A dumb rock. Massive, unguided."))

(defclass ship (space-object) ()
  (:documentation "A crewed vessel. Fragile; would rather dock than collide."))

(defclass station (space-object) ()
  (:documentation "A big fixed installation. Shrugs off most impacts."))

(defclass missile (space-object) ()
  (:documentation "A guided warhead. The interesting collisions are its."))

;;; ---- the multiple-dispatch generic ------------------------------------------
;;; COLLIDE picks its method from the classes of BOTH arguments. Read its dispatch
;;; matrix as: "for a (row-thing, column-thing) collision, is there a rule?" A blank
;;; cell is a real hole — calling that pair signals no-applicable-method (a fine way
;;; to meet the debugger from the Workspace).

(defgeneric collide (a b)
  (:documentation "Resolve a collision between two space objects. The outcome depends
   on the classes of BOTH — that is multiple dispatch, and why this GF has a real
   2-axis dispatch matrix rather than a flat method list."))

(defmethod collide ((a asteroid) (b asteroid))
  "Two rocks grind each other into gravel."
  (list :shatter (name a) (name b)))

(defmethod collide ((a asteroid) (b ship))
  "A rock against a hull: the ship takes the breach."
  (list :hull-breach (name b)))

(defmethod collide ((a ship) (b asteroid))
  "A ship flying into a rock — the same outcome, argued from the ship's side."
  (collide b a))

(defmethod collide ((a asteroid) (b station))
  "A rock against a station barely chips the paint."
  (list :chipped (name b)))

(defmethod collide ((a station) (b asteroid))
  "Symmetric to the above."
  (collide b a))

(defmethod collide ((a ship) (b station))
  "A ship doesn't collide with a station — it docks."
  (list :dock (name a) (name b)))

(defmethod collide ((a station) (b ship))
  "And the station accepts the docking."
  (collide b a))

(defmethod collide ((a missile) (b ship))
  "A missile finds its mark: the ship is destroyed."
  (list :destroyed (name b)))

(defmethod collide ((a missile) (b station))
  "A missile against a station: a dent, not a kill."
  (list :dented (name b)))

(defmethod collide ((a missile) (b asteroid))
  "A missile wastes itself on a rock."
  (list :deflected (name a)))

;;; ---- combination: qualifiers on one cell ------------------------------------
;;; These make the (missile ship) collision's effective-method onion worth opening —
;;; an :around wrapping a :before, the primary, then an :after. They add no new axis
;;; values (missile and ship are already on the grid), so the matrix stays tight.

(defun armed-p (missile)
  "Stub predicate for the :around demo — always armed here."
  (declare (ignore missile))
  t)

(defmethod collide :around ((a missile) (b ship))
  "Outermost: only a live warhead detonates; a dud just bumps."
  (if (armed-p a) (call-next-method) (list :dud (name a))))

(defmethod collide :before ((a missile) (b ship))
  "Before the hit: arm the proximity fuse."
  (declare (ignorable a b)))

(defmethod collide :after ((a missile) (b ship))
  "After the hit: log the kill for the scoreboard."
  (declare (ignorable a b)))

;;; ---- a rich restart menu, for exercising the debugger -----------------------
;;; Type  (lol.examples:launch)  in the Workspace and hit Debug it (C-c C-d): the
;;; launch drifts OFF-COURSE and signals, and the debugger's restart rail fills with
;;; several named ways out — the whole point of the condition system. Walk the stack
;;; (Tab, then up/down) to see STEER's and LAUNCH's locals; pick a restart to resolve.

(define-condition off-course (error)
  ((degrees :initarg :degrees :reader off-course-degrees))
  (:documentation "Signalled when a launch drifts off its intended heading.")
  (:report (lambda (c s)
             (format s "Launch is off course by ~a degrees." (off-course-degrees c)))))

(defun steer (degrees)
  "Signal OFF-COURSE with a menu of restarts — this is what fills the debugger's
   amber restart rail. Each restart returns the value STEER yields if you pick it."
  (restart-case
      (error 'off-course :degrees degrees)
    (correct-course ()
      :report "Steer back to zero and continue the launch."
      :on-course)
    (hold-heading ()
      :report "Hold the current heading and continue anyway."
      degrees)
    (use-heading (h)
      ;; no :interactive supplier on purpose — it takes a required value, so the
      ;; debugger's Enter detects that and prompts you for the heading.
      :report "Continue on a heading you supply."
      h)
    (scrub ()
      :report "Scrub the launch."
      :scrubbed)))

(defun launch (&optional (drift 37))
  "The demo entry point: begin a launch that drifts OFF-COURSE by DRIFT degrees, so
   STEER signals. STEER's result is kept (rather than tail-called) so LAUNCH stays on
   the stack — walk to its frame to see its own locals (stage, fuel)."
  (let ((stage "ascent") (fuel 82))
    (let ((outcome (steer drift)))          ; not a tail call: LAUNCH's frame remains
      (list :launched stage fuel outcome))))

;;; Leave the reader back in the default package so the next module loads cleanly
;;; regardless of order (this file is position-independent in the build list).
(in-package :cl-user)
