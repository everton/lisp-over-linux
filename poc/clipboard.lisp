;;;; clipboard.lisp — the clipboard, the kill ring and rectangles, as plain data.
;;;;
;;;; On a desktop, "the clipboard" is a PROTOCOL: X11 selections are owned by a
;;;; client and fetched by another through the X server (ICCCM), and a manager such
;;;; as Parcellite is a separate daemon that watches the owner change and keeps a
;;;; history. Here there is one process — the Lisp image — and every editor that
;;;; can copy or paste lives inside it. So the clipboard collapses to what it
;;;; always was underneath: a list of strings and a few rules about it. No server,
;;;; no daemon, no ownership hand-off. Design and the rationale for every rule:
;;;; doc/clipboard.org.
;;;;
;;;; Three things live here, all pure enough to test on the host with plain SBCL:
;;;;
;;;;   1. The HISTORY — Parcellite's copy history and Emacs's kill ring are the
;;;;      same idea (newest first, bounded, browsable), so they are ONE list.
;;;;      Parcellite contributes the manners: refuse blank copies, de-duplicate,
;;;;      cap the size. Emacs contributes the verbs: consecutive kills accumulate
;;;;      into one entry, and M-y walks back through older ones.
;;;;   2. PRIMARY — X's second selection: whatever the mouse last selected, pasted
;;;;      by a middle click. It is a single string kept apart from the history.
;;;;   3. RECTANGLES — Emacs's column-block commands (C-x r ...), as functions from
;;;;      a vector of line strings to a NEW vector of line strings. Emacs keeps the
;;;;      last killed rectangle apart from the kill ring, and so do we.
;;;;
;;;; No dependency on any other project file: it loads into the image and into a
;;;; bare host SBCL alike. It knows nothing about pixels, keys or textviews — the
;;;; editor decides WHEN to copy (which key, whether a kill was consecutive), this
;;;; file decides WHAT that does to the stored text.
;;;;
;;;; Text model, shared with poc/textview.lisp: a buffer is a SIMPLE-VECTOR of
;;;; strings, one per line, no newline characters inside; a position is a line
;;;; index and a column index (= a character index — a tab counts as one column,
;;;; which is honest here because the textview inserts spaces for Tab).

(defpackage :lol.clipboard
  (:use :cl)
  (:export
   ;; the board (all the state, in one object)
   #:clipboard #:make-clipboard #:*clipboard*
   #:board-limit #:board-max-length
   ;; history
   #:blank-string-p
   #:clip-copy #:clip-current #:clip-history #:clip-count #:clip-nth
   #:clip-delete #:clip-clear #:clip-promote
   #:clip-yank #:clip-yank-pop
   #:clip-filter #:clip-label
   ;; PRIMARY
   #:set-primary #:primary #:clip-select
   ;; the separate rectangle register
   #:clip-rectangle #:clip-set-rectangle
   ;; plain text over a lines vector
   #:region-text #:region-delete #:insert-text #:kill-line-span
   ;; rectangles
   #:rect-extract #:rect-kill #:rect-delete #:rect-yank
   #:rect-open #:rect-clear #:rect-string))

(in-package :lol.clipboard)

;;; ===========================================================================
;;; The board — all the clipboard state in one object
;;; ===========================================================================
;;;
;;; Why a struct and not five DEFVARs: the test file makes a fresh board per
;;; case, so no test can leak state into the next; and the running image has
;;; exactly one, *CLIPBOARD*, which every function uses unless told otherwise.

(defstruct (clipboard (:conc-name board-))
  (entries '())        ; the history, NEWEST FIRST — a list of strings
  (limit 50)           ; keep at most this many (Parcellite: history_limit)
  (max-length 1000000) ; refuse an entry longer than this; NIL = no cap
  (yank-index 0)       ; where C-y / M-y read from (Emacs's kill-ring-yank-pointer)
  (kill-tail nil)      ; text of the kill SEQUENCE in progress (see CLIP-COPY)
  (tail-entry nil)     ; the history string that sequence produced, compared by EQ
  (primary nil)        ; X PRIMARY: the last mouse selection, or NIL
  (rectangle nil))     ; Emacs killed-rectangle: a list of strings, or NIL

(defvar *clipboard* (make-clipboard)
  "THE clipboard of the running image. One per process is the whole point: every
   editor in the image shares it, so copying in one pane and pasting in another
   needs no protocol at all.")

;;; ===========================================================================
;;; The history
;;; ===========================================================================

(defun blank-string-p (string)
  "True when STRING holds nothing but whitespace (or nothing at all).
   Parcellite's 'ignore whitespace-only' rule: a copy that is all blanks is far
   more often a mis-click than an intent, and keeping it would push a real entry
   off the end of the history in exchange for nothing you could see in the list."
  (every (lambda (ch) (member ch '(#\Space #\Tab #\Newline #\Return #\Page)))
         string))

(defun push-entry (board string)
  "Put STRING at the front of BOARD's history. De-duplicates (an older copy of the
   same text is REMOVED, so re-copying something moves it up rather than listing
   it twice — Parcellite does this, Emacs by default does not) and then trims to
   the LIMIT. Resets the yank pointer: after a new copy, C-y means the new copy."
  (setf (board-entries board)
        (cons string (remove string (board-entries board) :test #'string=)))
  (let ((n (board-limit board)))
    (when (> (length (board-entries board)) n)
      (setf (board-entries board) (subseq (board-entries board) 0 n))))
  (setf (board-yank-index board) 0)
  string)

(defun refusal (board string)
  "Why STRING may not enter the history, as a keyword, or NIL if it may."
  (cond ((blank-string-p string) :blank)
        ((and (board-max-length board) (> (length string) (board-max-length board)))
         :too-long)))

(defun clip-copy (string &key append (board *clipboard*))
  "Record STRING as the newest copy/kill. Returns the stored entry, or
   (values NIL reason) when nothing was stored — REASON is :BLANK or :TOO-LONG, so
   the caller can say 'nothing copied' and why.

   APPEND makes this kill CONTINUE the previous one, the way C-k C-k C-k builds a
   single entry in Emacs. :AFTER (or T) adds STRING at the end — forward kills;
   :BEFORE adds it at the front — backward kills (M-DEL). Deciding that two kills
   are consecutive is the CALLER's job: it knows whether the last command was a
   kill; this file does not see commands.

   Why a separate KILL-TAIL rather than just editing entry 0: C-k at the end of a
   line kills only the newline. Alone that is blank and refused — but as the
   second half of 'foo' + newline it is exactly the text the user meant. So the
   sequence accumulates in KILL-TAIL whether or not each fragment was storable,
   the blank rule is applied to the ACCUMULATED text, and entry 0 is replaced
   only if it is still the very string this sequence put there (EQ) — if the user
   promoted something else meanwhile, the continued kill becomes a new entry
   instead of silently overwriting an unrelated one."
  ;; A fresh (non-append) copy starts a new sequence, and that sequence owns NO
  ;; entry yet. Without this reset, a refused first fragment would leave the
  ;; PREVIOUS sequence's entry marked as ours, and the next append would replace
  ;; it — the host test 'refused fragment carried into the sequence' caught that.
  (unless append (setf (board-tail-entry board) nil))
  (setf (board-kill-tail board)
        (if (and append (board-kill-tail board))
            (if (eq append :before)
                (concatenate 'string string (board-kill-tail board))
                (concatenate 'string (board-kill-tail board) string))
            string))
  (let* ((text (board-kill-tail board))
         (why  (refusal board text)))
    (when why
      (return-from clip-copy (values nil why)))
    ;; Continuing our own entry: take it out first so PUSH-ENTRY puts the grown
    ;; version back in its place (and the de-dup does not see the old fragment).
    (when (and append (board-tail-entry board)
               (eq (first (board-entries board)) (board-tail-entry board)))
      (pop (board-entries board)))
    (let ((entry (copy-seq text)))
      (setf (board-tail-entry board) entry)
      (push-entry board entry))))

(defun clip-current (&key (board *clipboard*))
  "The newest entry — what Parcellite would call the clipboard's contents — or NIL."
  (first (board-entries board)))

(defun clip-history (&key (board *clipboard*))
  "All entries, newest first. A fresh list: the caller may keep or sort it."
  (copy-list (board-entries board)))

(defun clip-count (&key (board *clipboard*))
  (length (board-entries board)))

(defun clip-nth (n &key (board *clipboard*))
  "Entry N, 0 = newest, or NIL when there is none."
  (nth n (board-entries board)))

(defun forget-tail (board)
  "Any history change other than a kill ends the kill sequence: an append after it
   must not reach back into an entry the user has just rearranged."
  (setf (board-kill-tail board) nil (board-tail-entry board) nil))

(defun clip-delete (n &key (board *clipboard*))
  "Drop entry N (the manager's Delete key). Returns the removed string or NIL."
  (let ((victim (nth n (board-entries board))))
    (when victim
      (setf (board-entries board)
            (append (subseq (board-entries board) 0 n)
                    (nthcdr (1+ n) (board-entries board))))
      (forget-tail board)
      (setf (board-yank-index board) 0))
    victim))

(defun clip-clear (&key (board *clipboard*))
  "Empty the history. PRIMARY and the rectangle register are separate stores and
   survive — clearing a list should not also change what middle-click pastes."
  (setf (board-entries board) '() (board-yank-index board) 0)
  (forget-tail board)
  nil)

(defun clip-promote (n &key (board *clipboard*))
  "Make entry N the newest — what choosing an old item in Parcellite's menu does,
   and what Enter does in our manager. Returns it, or NIL for a bad N."
  (let ((entry (nth n (board-entries board))))
    (when entry
      (forget-tail board)
      (push-entry board entry))))

;;; ---- yank and yank-pop (C-y, M-y) ------------------------------------------
;;;
;;; Emacs does not rotate the ring on M-y; it moves a POINTER into it. C-y inserts
;;; the entry under the pointer; M-y (right after a yank) replaces the text just
;;; inserted by the next older entry and moves the pointer there, wrapping at the
;;; end. A new copy puts the pointer back on the newest. The CALLER remembers
;;; where the yanked text sits in its buffer, because only it can replace it.

(defun clip-yank (&key (board *clipboard*))
  "The entry C-y / Shift-Insert should insert: the one under the yank pointer."
  (nth (board-yank-index board) (board-entries board)))

(defun clip-yank-pop (&key (n 1) (board *clipboard*))
  "Move the yank pointer N entries OLDER (negative N: newer), wrapping round, and
   return the entry now under it — the replacement text for M-y. NIL if empty."
  (let ((count (length (board-entries board))))
    (when (plusp count)
      (setf (board-yank-index board) (mod (+ (board-yank-index board) n) count))
      (clip-yank :board board))))

;;; ---- for the manager card ---------------------------------------------------

(defun clip-filter (query &key (board *clipboard*))
  "Entries containing QUERY (case-insensitive), as a list of (INDEX . STRING) so the
   manager can filter what it SHOWS while still promoting/deleting by the real
   history index. An empty QUERY matches everything."
  (loop for entry in (board-entries board)
        for i from 0
        when (search query entry :test #'char-equal)
          collect (cons i entry)))

(defun clip-label (string width)
  "STRING squeezed onto one row of at most WIDTH characters for the manager list:
   each line break shown as the two characters \\n (Lisp's own spelling, and
   plain ASCII, so any font has it), runs of whitespace collapsed, and an
   overlong label cut with '...'. Parcellite does the same ('single line' +
   'item length'), because a multi-line entry drawn as-is would eat the list."
  (let ((out (make-string-output-stream)) (gap nil))
    (loop for ch across string
          do (cond ((char= ch #\Newline) (write-string "\\n" out) (setf gap nil))
                   ((member ch '(#\Space #\Tab #\Return))
                    (unless gap (write-char #\Space out) (setf gap t)))
                   (t (write-char ch out) (setf gap nil))))
    (let ((s (string-trim " " (get-output-stream-string out))))
      (if (> (length s) width)
          (concatenate 'string (subseq s 0 (max 0 (- width 3))) "...")
          s))))

;;; ===========================================================================
;;; PRIMARY — the mouse selection
;;; ===========================================================================
;;;
;;; X has two everyday selections. CLIPBOARD is what an explicit Copy sets.
;;; PRIMARY is whatever was last selected, set just by selecting, and pasted by
;;; the middle button. They are independent: middle-click does not paste what you
;;; copied with Ctrl-C, and Shift-Insert (in most programs) does not paste what
;;; you only selected. We keep that split — PRIMARY is one string, not a history.

(defun set-primary (string &key (board *clipboard*))
  "Make STRING the PRIMARY selection. Any non-empty string is accepted, blanks
   included: X sets PRIMARY on every selection, and the blank rule exists to
   protect the HISTORY's limited slots, which PRIMARY does not have."
  (when (plusp (length string))
    (setf (board-primary board) (copy-seq string))))

(defun primary (&key (board *clipboard*))
  "The PRIMARY selection (what a middle click pastes), or NIL."
  (board-primary board))

(defun clip-select (string &key (board *clipboard*))
  "A mouse selection has FINISHED: set PRIMARY and ALSO record it in the history
   (the 'select and it is copied' behaviour asked for — Parcellite's 'synchronise
   selections'). Returns what CLIP-COPY returns.

   Call this on button RELEASE, never during the drag. A drag reports a growing
   selection many times a second; committing each would fill the history with
   'f', 'fo', 'foo' — and de-duplication cannot save us, because those are all
   different strings. The release is the one moment the selection means
   something."
  (set-primary string :board board)
  (forget-tail board)
  (clip-copy string :board board))

;;; ===========================================================================
;;; The rectangle register
;;; ===========================================================================

(defun clip-rectangle (&key (board *clipboard*))
  "The last killed/copied rectangle (a list of equal-width strings), or NIL.
   Emacs keeps this in its own variable, killed-rectangle, apart from the kill
   ring — a rectangle is a 2-D shape and pasting it as ordinary text would lose
   that shape — so C-x r y and C-y never fight over the same slot."
  (board-rectangle board))

(defun clip-set-rectangle (rect &key (board *clipboard*))
  "Keep RECT (from RECT-KILL or RECT-EXTRACT) as the rectangle C-x r y inserts.
   Copied, so a caller that later edits its own list cannot reach into the register."
  (setf (board-rectangle board) (mapcar #'copy-seq rect)))

;;; ===========================================================================
;;; Plain text over a lines vector (what C-w, M-w, C-k and C-y need)
;;; ===========================================================================
;;;
;;; poc/textview.lisp keeps these operations private to its own struct; these are
;;; the same edits over a bare vector, so the clipboard verbs can be tested
;;; without a textview and wired into one with a single SETF of TV-LINES.

(defun order-positions (l0 c0 l1 c1)
  "(values la ca lb cb) with (la,ca) <= (lb,cb). The selection anchor may sit
   after the point; normalising first is the cure for the classic selection bug."
  (if (or (< l0 l1) (and (= l0 l1) (<= c0 c1)))
      (values l0 c0 l1 c1)
      (values l1 c1 l0 c0)))

(defun clamp-col (lines l c)
  (max 0 (min c (length (aref lines l)))))

(defun region-text (lines l0 c0 l1 c1)
  "The text between two positions (either order), lines joined by newlines —
   what M-w copies and C-w kills."
  (multiple-value-bind (la ca lb cb) (order-positions l0 c0 l1 c1)
    (let ((ca (clamp-col lines la ca)) (cb (clamp-col lines lb cb)))
      (if (= la lb)
          (subseq (aref lines la) ca cb)
          (with-output-to-string (s)
            (write-string (subseq (aref lines la) ca) s)
            (loop for i from (1+ la) below lb
                  do (terpri s) (write-string (aref lines i) s))
            (terpri s)
            (write-string (subseq (aref lines lb) 0 cb) s))))))

(defun region-delete (lines l0 c0 l1 c1)
  "A NEW lines vector with the region removed (the input is not touched), plus the
   position where the text was, as (values new-lines line col)."
  (multiple-value-bind (la ca lb cb) (order-positions l0 c0 l1 c1)
    (let* ((ca (clamp-col lines la ca)) (cb (clamp-col lines lb cb))
           (joined (concatenate 'string (subseq (aref lines la) 0 ca)
                                (subseq (aref lines lb) cb))))
      (values (concatenate 'simple-vector (subseq lines 0 la) (vector joined)
                           (subseq lines (1+ lb)))
              la ca))))

(defun split-lines (string)
  "STRING cut at its newlines. Always at least one element: \"\" -> (\"\")."
  (loop with start = 0
        for nl = (position #\Newline string :start start)
        collect (subseq string start nl)
        while nl do (setf start (1+ nl))))

(defun insert-text (lines line col string)
  "Insert STRING (newlines allowed) at LINE/COL. Returns (values new-lines
   end-line end-col) — the end is where the caret goes after a yank, and with the
   start it is the span M-y will replace."
  (let* ((col   (clamp-col lines line col))
         (s     (aref lines line))
         (head  (subseq s 0 col))
         (tail  (subseq s col))
         (parts (split-lines string))
         (n     (length parts))
         (last  (car (last parts)))
         (mid   (if (= n 1)
                    (list (concatenate 'string head last tail))
                    (append (list (concatenate 'string head (first parts)))
                            (butlast (rest parts))
                            (list (concatenate 'string last tail))))))
    (values (concatenate 'simple-vector (subseq lines 0 line) mid
                         (subseq lines (1+ line)))
            (+ line n -1)
            (if (= n 1) (+ col (length last)) (length last)))))

(defun kill-line-span (lines line col)
  "The region Emacs's C-k (kill-line) removes from LINE/COL, as (values l0 c0 l1
   c1), or NIL at the very end of the buffer (nothing to kill). Emacs's own rule,
   from its docstring: 'kill the rest of the current line; if no nonblanks there,
   kill thru newline'. So text after point is killed up to the end of the line,
   while a rest-of-line that is empty or only blanks goes WITH the newline,
   joining the next line on. On the last line there is no newline to take, so
   only the blanks go. (Emacs's kill-whole-line option, off by default, would
   take text and newline together at column 0; we follow the default.)"
  (let* ((col  (clamp-col lines line col))
         (s    (aref lines line))
         (len  (length s))
         (rest-blank (every (lambda (ch) (member ch '(#\Space #\Tab)))
                            (subseq s col))))
    (cond ((not rest-blank)               (values line col line len))
          ((< line (1- (length lines)))   (values line col (1+ line) 0))
          ((< col len)                    (values line col line len))
          (t nil))))


;;; ===========================================================================
;;; Rectangles (C-x r ...)
;;; ===========================================================================
;;;
;;; A RECTANGLE is the block of text between two corners, taken column-wise: every
;;; line from the top corner's line to the bottom one, and on each of them the
;;; columns from the left corner's column to the right one — regardless of which
;;; corner was the point and which the mark. Lines are RAGGED, so some lines stop
;;; short of the rectangle, or inside it. Each command below follows what Emacs's
;;; rect.el does on such a line (default behaviour, no prefix argument):
;;;
;;;   extract  pads the copied piece with spaces, so a rectangle is always a list
;;;            of strings all exactly as wide as the rectangle — a true block.
;;;   delete   removes what is there; a line ending before the rectangle is left
;;;            alone (never padded just to delete nothing).
;;;   open     inserts blanks only on lines that have text to push right.
;;;   clear    blanks the block in place; a line ending inside it is just cut.
;;;   string   pads short lines out to the left edge, then writes the string.
;;;
;;; Every function returns a NEW vector and never mutates its input, so an undo
;;; can simply keep the old vector.

(defun rect-bounds (l0 c0 l1 c1)
  "Normalise two corners to (values top left bottom right). Unlike a region,
   each AXIS is sorted on its own: the rectangle is the same whichever corner the
   drag started from."
  (values (min l0 l1) (min c0 c1) (max l0 l1) (max c0 c1)))

(defun spaces (n) (make-string (max 0 n) :initial-element #\Space))

(defun pad-to (string col)
  "STRING extended with spaces to at least COL characters."
  (if (< (length string) col)
      (concatenate 'string string (spaces (- col (length string))))
      string))

(defun map-rect-lines (fn lines top bottom)
  "A copy of LINES with each line TOP..BOTTOM replaced by (FN line). Lines past
   the end of the buffer are simply not there — a rectangle cannot reach them."
  (let ((new (copy-seq lines)))
    (loop for i from top to (min bottom (1- (length new)))
          do (setf (aref new i) (funcall fn (aref new i))))
    new))

(defun rect-extract (lines l0 c0 l1 c1)
  "C-x r M-w (copy-rectangle-as-kill): the rectangle's text as a list of strings,
   one per line, each padded with spaces to the full width."
  (multiple-value-bind (top left bottom right) (rect-bounds l0 c0 l1 c1)
    (loop for i from top to (min bottom (1- (length lines)))
          collect (subseq (pad-to (aref lines i) right) left right))))

(defun delete-span (line left right)
  (if (< (length line) left)
      line
      (concatenate 'string (subseq line 0 left)
                   (subseq line (min right (length line))))))

(defun rect-delete (lines l0 c0 l1 c1)
  "C-x r d (delete-rectangle): remove the block, closing each line up. Nothing is
   saved — that is the difference from kill."
  (multiple-value-bind (top left bottom right) (rect-bounds l0 c0 l1 c1)
    (map-rect-lines (lambda (s) (delete-span s left right)) lines top bottom)))

(defun rect-kill (lines l0 c0 l1 c1)
  "C-x r k (kill-rectangle): (values new-lines rectangle) — delete the block and
   return what it held, for the caller to keep with CLIP-SET-RECTANGLE."
  (values (rect-delete lines l0 c0 l1 c1)
          (rect-extract lines l0 c0 l1 c1)))

(defun rect-yank (lines rect line col)
  "C-x r y (yank-rectangle): insert RECT with its top-left corner at LINE/COL.
   Line k of the rectangle goes into buffer line LINE+k at the SAME column: short
   lines are padded with spaces out to COL, and if the rectangle runs past the
   last line, new lines are added. Returns (values new-lines end-line end-col),
   the end being the bottom-right corner — where Emacs leaves point."
  (let* ((n      (length rect))
         (needed (+ line n))
         (new    (if (> needed (length lines))
                     (concatenate 'simple-vector lines
                                  (make-array (- needed (length lines))
                                              :initial-element ""))
                     (copy-seq lines))))
    (loop for piece in rect
          for i from line
          do (let ((s (pad-to (aref new i) col)))
               (setf (aref new i)
                     (concatenate 'string (subseq s 0 col) piece (subseq s col)))))
    (values new (+ line (max 0 (1- n))) (+ col (length (car (last rect)))))))

(defun rect-open (lines l0 c0 l1 c1)
  "C-x r o (open-rectangle): insert a block of blanks, pushing the text to its
   right further right. A line that ends at or before the left edge has nothing
   to push, so it is left as it was (no trailing spaces are invented)."
  (multiple-value-bind (top left bottom right) (rect-bounds l0 c0 l1 c1)
    (map-rect-lines (lambda (s)
                      (if (> (length s) left)
                          (concatenate 'string (subseq s 0 left)
                                       (spaces (- right left)) (subseq s left))
                          s))
                    lines top bottom)))

(defun rect-clear (lines l0 c0 l1 c1)
  "C-x r c (clear-rectangle): overwrite the block with blanks, in place — nothing
   moves. A line that ends inside the block is cut at the left edge instead of
   being filled with trailing spaces; a line ending before it is untouched."
  (multiple-value-bind (top left bottom right) (rect-bounds l0 c0 l1 c1)
    (map-rect-lines (lambda (s)
                      (cond ((< (length s) left) s)
                            ((<= (length s) right) (subseq s 0 left))
                            (t (concatenate 'string (subseq s 0 left)
                                            (spaces (- right left))
                                            (subseq s right)))))
                    lines top bottom)))

(defun rect-string (lines l0 c0 l1 c1 string)
  "C-x r t (string-rectangle): replace the block on every line with STRING. A
   zero-width rectangle (both corners in one column) therefore INSERTS STRING down
   a column — the common trick for prefixing lines with ';; '. Short lines are
   padded out to the left edge first, as Emacs does."
  (multiple-value-bind (top left bottom right) (rect-bounds l0 c0 l1 c1)
    (map-rect-lines (lambda (s)
                      (let ((s (pad-to s left)))
                        (concatenate 'string (subseq s 0 left) string
                                     (subseq s (min right (length s))))))
                    lines top bottom)))
