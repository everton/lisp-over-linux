;;;; widgets.lisp — the reusable half of the browser's chrome (phase 3's toolkit).
;;;;
;;;; canvas.lisp gives us rectangles, glyphs and a pixel buffer. textview.lisp gives
;;;; us an editable body of text. Between those two there was nothing: every pane
;;;; frame, every list, every grid and every scroll hint was drawn ad hoc inside
;;;; shell.lisp, and each new surface (the Spotter, the debugger, the cheatsheet)
;;;; re-invented them slightly differently. This file is what doc/code-browser.org
;;;; §8 phase 3 still owed — "panes, lists, scrollbars, menus" — as widgets.
;;;;
;;;; The rule that keeps them widgets and not just moved code: *a widget knows
;;;; nothing about the browser*. It never sees an ITEM, a VIEW, a PANE or a SUBJECT
;;;; — those are the present protocol's, one layer up. It is handed geometry and a
;;;; vector of ROWs, and it hands back where it drew them. So the Spotter's
;;;; candidates, the debugger's restarts and a package's definitions are all the
;;;; same widget with different rows, which is exactly the deduplication phase 3
;;;; was asking for.
;;;;
;;;; DRAW RECORDS WHAT IT DID. Every widget that can be clicked returns a hit map
;;;; rather than letting the caller re-derive geometry from a formula. That is not
;;;; ceremony: the browser has already shipped two bugs of exactly that shape (a
;;;; click landing a gutter-width right of the pointer, and a click computing row N
;;;; from y after group headers made row N no longer item N). A hit map cannot
;;;; disagree with the picture, because it IS the picture's own record.
;;;;
;;;; Backend-blind, like canvas.lisp: nothing here knows whether the buffer ends up
;;;; on /dev/fb0 or in an X11 window.

(defpackage :lol.widgets
  (:use :cl)
  (:export
   ;; theme
   #:*font* #:*ui-bg* #:*ui-panel* #:*ui-hi* #:*ui-rule* #:*ui-text* #:*ui-dim*
   #:*ui-accent* #:*ui-sel* #:*ui-amber* #:*ui-lit*
   #:*concept-colors* #:concept-color
   ;; metrics
   #:+row-h+ #:+bar-h+ #:+scrollbar-w+
   ;; rows
   #:row #:make-row #:row-label #:row-detail #:row-tint #:row-stripe #:row-marker
   #:row-group
   ;; widgets
   #:draw-panel #:draw-bar #:draw-scrollbar #:draw-list #:draw-grid #:draw-card
   #:draw-query-line #:scrim #:row-match
   ;; hit testing
   #:hit-row #:hit-cell #:clip-text))

(in-package :lol.widgets)

;;; ---- theme ---------------------------------------------------------------
;;; One palette, here, so a widget and its caller cannot drift apart. The
;;; Lisp-concept colours are the semantic scheme from media/lisp-concepts-visual.html:
;;; a list of 350 definitions reads by COLOUR rather than as a grey wall.

(defvar *font* nil "The UI font (a lol.canvas:font). Set once at startup.")

(defparameter *ui-bg*     !#14161A "the page behind everything")
(defparameter *ui-panel*  !#1B1E24 "a pane's body")
(defparameter *ui-hi*     !#242931 "a header band / a collapsed bar")
(defparameter *ui-rule*   !#2E343D "hairlines and borders")
(defparameter *ui-text*   !#D6D3C8 "ordinary text")
(defparameter *ui-dim*    !#6B727D "secondary text: details, hints, headers")
(defparameter *ui-accent* !#7FDBCA "focus: the selected row's edge, the active tab")
(defparameter *ui-sel*    !#2F4F4A "the selected row's ground")
(defparameter *ui-amber*  !#FF9F43 "an affordance that opens somewhere ELSE")
(defparameter *ui-lit*    !#FFFFFF
  "The characters a search actually matched. Deliberately NOT the accent: the accent
   is #7FDBCA, which is also the colour of every function row, so lighting a function
   name in the accent lights nothing. White is the one value brighter than every
   concept colour, which is what \"this is the part you typed\" needs to be.")

(defparameter *concept-colors*
  (list :package          !#FFCB6B   ; gold
        :function         !#7FDBCA   ; teal
        :generic-function !#A8FF78   ; green
        :method           !#A8FF78   ; green
        :macro            !#C792EA   ; purple
        :compiler-macro   !#C792EA   ; purple
        :class            !#89DDFF   ; blue
        :subclass         !#5E93AD   ; blue, dimmed — a class seen from ABOVE. Same hue,
                                     ; less light: in a class view the supers are where
                                     ; you came from and the subs are where you may go,
                                     ; and the eye should tell the two groups apart
                                     ; without reading the detail column.
        :struct           !#89DDFF   ; blue
        :type             !#89DDFF   ; blue
        :condition        !#FF6B6B   ; red
        :restart          !#FF9F43   ; amber
        :variable         !#FF9F43   ; amber
        :constant         !#FF9F43)  ; amber
  "Colour per Lisp concept. A kind with no entry falls back to *UI-TEXT*.")

(defun concept-color (kind)
  "The colour for a definition KIND (:function :class :macro …), or *UI-TEXT* when
   the kind has no semantic colour (a slot, a plain line of text)."
  (or (getf *concept-colors* kind) *ui-text*))

;;; ---- metrics -------------------------------------------------------------

(defparameter +row-h+ 20
  "Height of one list row, in pixels.

   Was 17, which is the 6x15 font's 15px cell plus two — technically legible and
   visually cramped: at 17 a pane of forty rows is a solid block of text with no air
   in it, and the eye has nothing to travel along. 20 gives each row a little room
   above and below without costing many rows on a 720-line screen (36 instead of 42).
   Everything else derives from this rather than repeating it, so changing it again
   is a one-line change.")
(defparameter +bar-h+ 20 "Height of a header band / collapsed breadcrumb bar.")
(defparameter +scrollbar-w+ 5 "Width of a scrollbar's track.")

;;; ---- text helpers --------------------------------------------------------

(defun clip-text (string maxlen)
  "STRING trimmed to MAXLEN characters, elided with \"..\".

   The elision is two ASCII dots on purpose. The font blob is indexed by
   (logand (char-code c) #xff), so the single-glyph … (codepoint 8230) would blit
   as a stray & — 8230 mod 256 is 38. See doc/fonts.org."
  (let ((s (princ-to-string string)))
    (if (> (length s) maxlen)
        (concatenate 'string (subseq s 0 (max 1 (- maxlen 2))) "..")
        s)))

;;; ---- panel: a framed pane with a header band -----------------------------

(defun draw-panel (canvas x y w h &key title badge (tint *ui-text*) (marker "v"))
  "A framed pane: body, border, a header band with MARKER + TITLE in TINT, and a
   right-aligned BADGE in dim. Returns the screen-Y where the BODY starts, so the
   caller never has to know the band's height."
  (lol.canvas:fill-rect canvas x y w h *ui-panel*)
  (lol.canvas:draw-rect canvas x y w h *ui-rule*)
  (lol.canvas:fill-rect canvas (1+ x) y (- w 2) +bar-h+ *ui-hi*)
  (lol.canvas:fill-rect canvas (1+ x) (+ y +bar-h+ -1) (- w 2) 1 *ui-rule*)  ; seam
  (when marker
    (lol.canvas:draw-string canvas *font* marker (+ x 8) (+ y 3) tint))
  (when title
    (lol.canvas:draw-string canvas *font* (princ-to-string title) (+ x 26) (+ y 3) tint))
  (when badge
    (let ((b (princ-to-string badge)))
      (lol.canvas:draw-string canvas *font* b
                              (- (+ x w) (lol.canvas:string-px *font* b) 2) (+ y 3) *ui-dim*)))
  (+ y +bar-h+))

(defun draw-bar (canvas x y w &key marker label right (tint *ui-text*))
  "A single collapsed bar — the browser's breadcrumb, and any other one-line strip:
   a filled band, MARKER and LABEL on the left, RIGHT right-aligned in the accent."
  (lol.canvas:fill-rect canvas x y w +bar-h+ *ui-hi*)
  (when marker (lol.canvas:draw-string canvas *font* marker (+ x 8) (+ y 3) *ui-dim*))
  (when label (lol.canvas:draw-string canvas *font* (princ-to-string label)
                                      (+ x 26) (+ y 3) tint))
  (when right
    (let ((r (princ-to-string right)))
      (lol.canvas:draw-string canvas *font* r
                              (- (+ x w) (lol.canvas:string-px *font* r) 8) (+ y 3) *ui-accent*)))
  (+ y +bar-h+))

;;; ---- scrollbar -----------------------------------------------------------

(defun draw-scrollbar (canvas x y w h total visible first)
  "A vertical scrollbar down the right edge of a rect: a dim track and a brighter
   thumb whose length is the visible FRACTION and whose position is FIRST's share of
   the scrollable range. Draws nothing when everything already fits — a scrollbar
   that is always full length is noise, not information.

   TOTAL rows exist, VISIBLE fit, FIRST is the topmost one shown."
  (when (and (plusp h) (> total visible) (plusp visible))
    (let* ((bx (- (+ x w) +scrollbar-w+))
           (thumb-h (max 12 (floor (* h visible) total)))
           (span (- h thumb-h))                       ; pixels the thumb can travel
           (scrollable (max 1 (- total visible)))
           (ty (+ y (floor (* span (min first scrollable)) scrollable))))
      (lol.canvas:fill-rect canvas bx y +scrollbar-w+ h *ui-hi*)
      (lol.canvas:fill-rect canvas bx ty +scrollbar-w+ thumb-h *ui-rule*)
      bx)))

;;; ---- list: rows, sections, selection, viewport ---------------------------

(defstruct (row (:constructor make-row (label &key detail tint stripe marker group match)))
  "One line in a list widget. Deliberately NOT the browser's ITEM: a widget that
   only spoke ITEM could not draw the Spotter's candidates or the debugger's
   restarts, which are not items and never will be."
  label       ; the primary text
  detail      ; secondary text, right-aligned when it fits, else trailing. Or NIL.
  (tint *ui-text*)  ; the label's colour
  stripe      ; colour of the 3px kind stripe at the left edge, or NIL for none
  marker      ; NIL, :dim or :bright — the » "this opens something" affordance
  group       ; section label (a string) or NIL; consecutive equal groups are one block
  match)      ; a query string whose characters should be lit inside LABEL, or NIL

(defun light-match (canvas x y label query)
  "Re-draw, lit, the characters of LABEL that QUERY matched — the same
   greedy left-to-right subsequence the filter used to accept the row. Showing WHY a
   row matched is most of what makes a fuzzy finder feel honest rather than magical;
   the design mock-up bolds them, and with one weight of bitmap type colour is the
   available emphasis.

   Monospace is assumed, as everywhere else that maps characters to pixels here: the
   nth character is n advances along. LOL.TEXTVIEW makes the same assumption and says
   so in the same words."
  (let ((qi 0) (ql (length query)) (cw (lol.canvas:font-cw *font*)))
    (when (plusp ql)
      (loop for i below (length label)
            while (< qi ql)
            do (when (char-equal (char label i) (char query qi))
                 (lol.canvas:draw-string canvas *font* (string (char label i))
                                         (+ x (* i cw)) y *ui-lit*)
                 (incf qi))))))

(defun draw-row (canvas x y w row selected &key value-col)
  "One row, at the row's TOP-left. SELECTED paints the ground and a focus edge.

   With VALUE-COL (a pixel offset), the detail is drawn as a second COLUMN starting
   there, in full-strength text rather than dim — the inspector's shape, where the
   value is the thing you came to read and the slot name is the label on the drawer."
  ;; The box fills the slot; the text is CENTRED in it rather than sitting at a
  ;; hard-coded offset, so +row-h+ can be retuned without every row drifting off
  ;; its own highlight.
  (let* ((by y)
         (ty (+ y (max 0 (floor (- +row-h+ (lol.canvas:font-ch *font*)) 2)))))
    (when selected
      (lol.canvas:fill-rect canvas x by w +row-h+ *ui-sel*)
      (lol.canvas:fill-rect canvas x by 2 +row-h+ *ui-accent*))
    (let* ((label (princ-to-string (row-label row)))
           (tint (or (row-tint row) *ui-text*))
           (marker-x (- (+ x w) 16)))
      (when (row-stripe row)
        (lol.canvas:fill-rect canvas (+ x 3) by 3 +row-h+ (row-stripe row)))
      (lol.canvas:draw-string canvas *font* label (+ x 8) ty tint)
      (when (row-match row) (light-match canvas (+ x 8) ty label (row-match row)))
      (when (row-detail row)
        (let* ((dt (princ-to-string (row-detail row)))
               (label-px (lol.canvas:string-px *font* label)))
          (if value-col
              ;; two columns: name left, VALUE at a fixed offset and emphasised. The
              ;; value is clipped rather than allowed to run past the marker.
              (let* ((vx (+ x value-col))
                     (room (max 4 (floor (- marker-x vx) (lol.canvas:font-cw *font*)))))
                (lol.canvas:fill-rect canvas (- vx 6) by 1 +row-h+ *ui-rule*) ; column rule
                (lol.canvas:draw-string canvas *font* (clip-text dt room) vx ty *ui-text*))
              ;; otherwise: right-align it into its own column when it fits, else run
              ;; it on after the label — a long label must never be overwritten
              (let ((rx (- marker-x (lol.canvas:string-px *font* dt) 8)))
                (if (> rx (+ x 8 label-px 14))
                    (lol.canvas:draw-string canvas *font* dt rx ty *ui-dim*)
                    (lol.canvas:draw-string canvas *font* (format nil "  ~a" dt)
                                            (+ x 8 label-px) ty *ui-dim*))))))
      (when (row-marker row)
        ;; » = "drilling this opens something"; amber when it opens ELSEWHERE
        (lol.canvas:draw-string canvas *font* (string (code-char #xBB)) marker-x ty
                                (if (eq (row-marker row) :bright) *ui-amber* *ui-dim*))))))

(defun draw-section-header (canvas x y w label)
  "A section header: a hairline across the pane, broken by the group's name. The
   rule does the work — it turns 'superclass, superclass, subclass' from a wall of
   near-identical lines into legible blocks."
  (let* ((ty (+ y (max 0 (floor (- +row-h+ (lol.canvas:font-ch *font*)) 2))))
         (ry (+ ty (floor (lol.canvas:font-ch *font*) 2)))     ; through the text's middle
         (px (lol.canvas:string-px *font* label)))
    (lol.canvas:fill-rect canvas (+ x 3) ry (- w 6) 1 *ui-rule*)
    (lol.canvas:fill-rect canvas (+ x 8) ry (+ px 8) 1 *ui-panel*)   ; a gap for the text
    (lol.canvas:draw-string canvas *font* label (+ x 12) ty *ui-dim*)))

(defun list-layout (rows)
  "ROWS -> a vector of SLOTS, each (:header . LABEL) or (:row . INDEX). A header is
   emitted wherever the group changes, so a list with no groups lays out exactly one
   slot per row and pays nothing for the feature."
  (let ((slots '()) (prev :none))
    (loop for r across rows for i from 0
          do (let ((g (row-group r)))
               (unless (equal g prev)
                 (when g (push (cons :header g) slots))
                 (setf prev g))
               (push (cons :row i) slots)))
    (coerce (nreverse slots) 'vector)))

(defun slot-of-row (layout index)
  "Which LAYOUT slot shows row INDEX (0 if it is not laid out)."
  (or (position-if (lambda (s) (and (eq (car s) :row) (eql (cdr s) index))) layout) 0))

(defun draw-list (canvas x y w h rows &key (sel 0) (top 0) (follow-sel t) (scrollbar t)
                                            value-col)
  "Draw ROWS (a vector) into the rect, with sections, selection and a scrollbar.

   TOP is the first visible LAYOUT SLOT — not the first row, because a section
   header occupies a slot of its own. FOLLOW-SEL keeps the selected row on screen
   (what arrow keys want); pass NIL to scroll the viewport independently of the
   selection (what a wheel wants).

   VALUE-COL turns the rows into a two-column name/value table (see DRAW-ROW).

   Returns (values TOP HITS): TOP possibly adjusted, and HITS an alist
   ((slot-y . row-index) …) of the rows actually painted — the hit map."
  (let* ((layout (list-layout rows))
         (nslots (length layout))
         (visible (max 1 (floor h +row-h+)))
         (max-top (max 0 (- nslots visible)))
         (top (max 0 (min top max-top)))
         (body-w (if (and scrollbar (> nslots visible)) (- w +scrollbar-w+ 2) w))
         (hits '()))
    (when follow-sel
      (let ((sel-slot (slot-of-row layout sel)))
        (when (< sel-slot top) (setf top sel-slot))
        (when (>= sel-slot (+ top visible)) (setf top (- sel-slot visible -1)))))
    (loop for s from top below (min nslots (+ top visible))
          for ry from y by +row-h+
          for slot = (aref layout s)
          do (if (eq (car slot) :header)
                 (draw-section-header canvas x ry body-w (cdr slot))
                 (progn
                   (draw-row canvas x ry body-w (aref rows (cdr slot)) (= (cdr slot) sel)
                             :value-col value-col)
                   (push (cons ry (cdr slot)) hits))))
    (when scrollbar (draw-scrollbar canvas x y w h nslots visible top))
    (values top (nreverse hits))))

(defun hit-row (hits py)
  "The row index HITS says was painted at PY, or NIL. A click on a section header
   or on empty space below the last row correctly hits nothing."
  (let ((hit (find-if (lambda (e) (<= (car e) py (+ (car e) +row-h+ -1))) hits)))
    (and hit (cdr hit))))

;;; ---- card: a floating surface over a dimmed page -------------------------

(defun scrim (canvas factor)
  "Darken every pixel of CANVAS to FACTOR/256 of its brightness — a modal scrim. The
   canvas has no alpha channel (one pixel format, no compositor — poc/README.org), so
   \"behind a floating panel\" is spelled by multiplying the whole buffer down. Only
   ever run when an overlay is up, so a full-buffer pass is affordable."
  (declare (type (integer 0 256) factor))
  (let ((px (lol.canvas:canvas-pixels canvas)))
    (declare (type (simple-array (unsigned-byte 32) (*)) px)
             (optimize (speed 3) (safety 0)))
    (dotimes (i (length px))
      (let ((p (aref px i)))
        (setf (aref px i)
              (logior (ash (ash (* (ldb (byte 8 16) p) factor) -8) 16)
                      (ash (ash (* (ldb (byte 8 8) p) factor) -8) 8)
                      (ash (* (ldb (byte 8 0) p) factor) -8)))))))

(defun draw-card (canvas x y w h &key title right dim)
  "A raised surface — the Spotter, the cheatsheet, a modal. With DIM (a 0..256
   factor), first knock the whole page back with SCRIM so the card reads as floating
   ABOVE what it covers rather than pasted into it. TITLE goes top-left, RIGHT
   top-right (a match count, a hint). Returns the Y where the body starts."
  (when dim (scrim canvas dim))
  (lol.canvas:fill-rect canvas (+ x 5) (+ y 6) w h !#080A0C)   ; a hard drop shadow
  (lol.canvas:fill-rect canvas x y w h *ui-panel*)
  (lol.canvas:fill-rect canvas x y w 2 *ui-accent*)            ; accent top rule
  (lol.canvas:draw-rect canvas x y w h *ui-rule*)
  (when title
    (lol.canvas:draw-string canvas *font* (princ-to-string title) (+ x 12) (+ y 8) *ui-dim*))
  (when right
    (let ((r (princ-to-string right)))
      (lol.canvas:draw-string canvas *font* r
                              (- (+ x w) (lol.canvas:string-px *font* r) 12) (+ y 8) *ui-dim*)))
  (+ y 24))

(defun draw-query-line (canvas x y w text)
  "The Spotter's (or any finder's) input line: a sunken field with a prompt, the text
   so far, and a block cursor. There is no caret widget because there is no editing —
   a finder only ever appends and backspaces."
  (lol.canvas:fill-rect canvas x y w 20 *ui-hi*)
  (lol.canvas:draw-rect canvas x y w 20 *ui-rule*)
  (lol.canvas:draw-string canvas *font* (format nil "> ~a_" text) (+ x 6) (+ y 3) *ui-accent*)
  (+ y 20))

;;; ---- grid: a 2-axis matrix of counted cells ------------------------------

(defparameter +cell-w+ 78 "Width of one grid cell.")
(defparameter +cell-h+ 18 "Height of one grid row.")

(defun draw-grid (canvas x y w h row-labels col-labels counts
                  &key (header-w 130) (empty-mark (string (code-char 183))))
  "A 2-axis grid: COL-LABELS across the top, ROW-LABELS down the left, and COUNTS
   (a list of rows, each a list of integers) in the cells. A zero is drawn as a dim
   mark — a real gap in the coverage is the whole point of the view, so it must be
   visible rather than blank.

   Returns (values HITS BOTTOM-Y): a list of (cx cy cw ch ROW-LABEL COL-LABEL) for
   every cell drawn, and the Y just below the last row — so a caller can put a caption
   under the grid without knowing the cell metrics."
  (let* ((x0 (+ x header-w))
         (ncols (max 1 (floor (- w header-w) +cell-w+)))
         (green (concept-color :generic-function))
         (hits '())
         (bottom (+ y +cell-h+)))
    ;; a header band, so the column names read as headings and not as a first row
    (lol.canvas:fill-rect canvas x y (- w 2) (- +cell-h+ 2) *ui-hi*)
    (lol.canvas:fill-rect canvas x (+ y +cell-h+ -3) (- w 2) 1 *ui-rule*)
    (loop for c in col-labels for ci from 0 below ncols
          for cx = (+ x0 (* ci +cell-w+))
          do (lol.canvas:draw-string canvas *font* (clip-text (string-downcase c) 12)
                                     (+ cx 2) (+ y 2) (concept-color :class)))
    (when (> (length col-labels) ncols)
      (lol.canvas:draw-string canvas *font*
                              (format nil ".. +~a" (- (length col-labels) ncols))
                              (- (+ x w) 50) (+ y 2) *ui-dim*))
    ;; How many rows fit below the header band? If they all do, draw them all; if
    ;; they do not, give up one line to say so — a grid that simply stops is
    ;; indistinguishable from a grid that is complete, which is the one thing a
    ;; coverage view must never be.
    (let* ((nrows (length row-labels))
           (fits (max 0 (floor (- h +cell-h+ 4) +cell-h+)))
           (shown (if (> nrows fits) (max 0 (1- fits)) nrows)))
      (loop for r in row-labels
            for cellrow in counts
            for ri from 0 below shown
            for ry = (+ y +cell-h+ (* ri +cell-h+))
            do (lol.canvas:draw-string canvas *font* (clip-text (string-downcase r) 20)
                                       x (+ ry 2) (concept-color :function))
               (loop for n in cellrow for ci from 0 below ncols
                     for cx = (+ x0 (* ci +cell-w+))
                     for col in col-labels
                     do (when (plusp n)              ; a covered pair: tinted, green edge
                          (lol.canvas:fill-rect canvas (1+ cx) (1+ ry) (- +cell-w+ 1)
                                                (- +cell-h+ 1) !#1E2A22)
                          (lol.canvas:fill-rect canvas (1+ cx) (1+ ry) 2 (- +cell-h+ 1) green))
                        (lol.canvas:draw-rect canvas cx ry +cell-w+ +cell-h+ *ui-rule*)
                        (lol.canvas:draw-string canvas *font*
                          (if (zerop n) empty-mark (format nil "~c~d" (code-char 215) n))
                          (+ cx (floor +cell-w+ 2) (if (zerop n) -3 -9)) (+ ry 2)
                          (if (zerop n) *ui-dim* green))
                        (push (list cx ry +cell-w+ +cell-h+ r col) hits)))
      (when (< shown nrows)
        (lol.canvas:draw-string canvas *font* (format nil ".. +~a" (- nrows shown))
                                x (+ y +cell-h+ (* shown +cell-h+) 2) *ui-dim*))
      (setf bottom (+ y +cell-h+ (* shown +cell-h+) (if (< shown nrows) +cell-h+ 0))))
    (values hits bottom)))

(defun hit-cell (hits px py)
  "The (ROW-LABEL . COL-LABEL) of the grid cell at PX,PY, or NIL."
  (let ((hit (find-if (lambda (c)
                        (destructuring-bind (cx cy cw ch r col) c
                          (declare (ignore r col))
                          (and (<= cx px (+ cx cw)) (<= cy py (+ cy ch)))))
                      hits)))
    (when hit (cons (fifth hit) (sixth hit)))))
