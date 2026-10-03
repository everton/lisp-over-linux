;;;; check-clipboard.lisp — the clipboard, the menus and the halo, in the booted guest.
;;;;
;;;;     sbcl --script check-clipboard.lisp        ; exits 0 if all assertions pass
;;;;     LOL_SHOTS=DIR sbcl --script check-clipboard.lisp   ; also keep screendumps
;;;;
;;;; Everything here is a GESTURE — keys and buttons — so it is driven the way a hand
;;;; would drive it: QMP key events and real PS/2 button and motion records, decoded
;;;; by the same read-mouse / fb-read-key the laptop uses. The assertions read the
;;;; guest's own state back over the net REPL (host-client/guest-harness.lisp).
;;;;
;;;;   A. the environment opens on the Workspace alone
;;;;   B. Emacs keys: C-k kills into the history, C-y yanks it back, Shift-Insert too
;;;;   C. a mouse drag sets PRIMARY and the history; a middle click pastes PRIMARY
;;;;   D. rectangles: C-x SPC, motion, C-x r k keeps the column block
;;;;   E. the right button opens the context menu; Esc closes it
;;;;   F. the middle button on a list row opens the halo; Up walks out through the
;;;;      layers (row subject, row, list, pane); Inspect opens the PANE instance;
;;;;      on the Workspace text the halo is about the TEXT VIEW, and Code opens tv-draw
;;;;   G. SuperL-h opens the clipboard manager
;;;;   H. the pointer hides after a few idle seconds and comes back on a move
;;;;   I. the systray is drawn
;;;;   J. double click selects a word, triple click the line (into PRIMARY); C-n / C-p
;;;;      move the Spotter's selection; SuperL-q asks how to leave
;;;;   K. undo: C-_ takes back typing, C-/ goes further, a motion ends the run and
;;;;      C-x u then redoes (Emacs's undo-the-undo)
;;;;
;;;; The touchpad's emulated buttons (two-finger press = middle, bottom-right press =
;;;; right) cannot be driven here — QEMU emulates no touchpad — so they were checked by
;;;; replaying synthetic Synaptics packets through read-mouse (doc/input.org).

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))

(in-package :lol.guest)

(defvar *shots* (sb-ext:posix-getenv "LOL_SHOTS")
  "A directory to keep screendumps in, for looking at the overlays; NIL keeps none.")

(defun shot (name)
  (when *shots*
    (qmp (format nil "{\"execute\":\"screendump\",\"arguments\":{\"filename\":\"~a/~a.ppm\"}}"
                 (string-right-trim "/" *shots*) name))
    (sleep 0.5)))

(defun qmp-chord (modifier qcode)
  "MODIFIER+QCODE in one send-key — enough for a chord the TTY spells as a byte
   (C-k is byte 11, M-w is ESC w), which is how fb-read-key reads those."
  (qmp (format nil "{\"execute\":\"send-key\",\"arguments\":{\"keys\":[~
                    {\"type\":\"qcode\",\"data\":\"~a\"},{\"type\":\"qcode\",\"data\":\"~a\"}]}}"
               modifier qcode))
  (sleep 0.6))

(defun qmp-move (dx dy)
  (qmp-events (format nil "{\"type\":\"rel\",\"data\":{\"axis\":\"x\",\"value\":~d}}" dx)
              (format nil "{\"type\":\"rel\",\"data\":{\"axis\":\"y\",\"value\":~d}}" dy)))

(defun glide (dx dy)
  "Move the pointer by DX,DY in a few steps, like a hand."
  (dotimes (i 4) (qmp-move (round dx 4) (round dy 4)) (sleep 0.2))
  (sleep 0.6))

(defun glide-to (x y)
  (let ((cx (probe-int "*cursor-x*")) (cy (probe-int "*cursor-y*")))
    (when (and cx cy) (glide (- x cx) (- y cy)))))

(defun button (name down)
  (qmp-events (format nil "{\"type\":\"btn\",\"data\":{\"down\":~a,\"button\":\"~a\"}}"
                      (if down "true" "false") name))
  (sleep 0.4))

(defun click (name) (button name t) (button name nil) (sleep 0.8))

(defun yes (form) (string= (probe form) "T"))

(defun ws-text-has (string)
  (yes (format nil "(and (search ~s (lol.textview:tv-text (pane-tv (current-pane)))) t)" string)))

(defun checks ()
  (quiet-kernel-console)
  (wait-for-browser)

  (format t "~&~%== A. the environment opens on the Workspace alone ==~%")
  (check "one tab at boot" (eql (probe-int "(length *trails*)") 1))
  (check "and it is the Workspace" (string= (probe "(view-kind (pane-view (current-pane)))") ":WORKSPACE"))

  (format t "~&~%== B. Emacs keys and the history ==~%")
  (qmp-chord "ctrl" "end")
  (qmp-key "ret") (qmp-type "hello world") (sleep 0.5)
  (qmp-chord "ctrl" "a")
  (qmp-chord "ctrl" "k")
  (check "C-k kills the line into the history"
         (string= (probe "(lol.clipboard:clip-current)") "\"hello world\""))
  (check "and out of the buffer" (not (ws-text-has "hello world")))
  (qmp-chord "ctrl" "y")
  (check "C-y yanks it back" (ws-text-has "hello world"))
  (qmp-hold "shift" "insert") (sleep 0.6)
  (check "Shift-Insert pastes it again" (ws-text-has "hello worldhello world"))
  (qmp-chord "ctrl" "a")
  (qmp-chord "ctrl" "k") (qmp-chord "ctrl" "k")       ; the line, then its newline: one entry
  (check "consecutive C-k builds ONE entry"
         (yes "(and (search \"hello worldhello world\" (lol.clipboard:clip-current)) (eql 2 (lol.clipboard:clip-count)) t)"))

  (format t "~&~%== C. mouse selection -> PRIMARY, middle click pastes it ==~%")
  (let ((tx (probe-int "(lol.textview::tv-x (pane-tv (current-pane)))"))
        (ty (probe-int "(lol.textview::tv-y (pane-tv (current-pane)))")))
    (glide-to (+ tx 90) (+ ty 30))
    (button "left" t) (glide 70 0) (button "left" nil) (sleep 0.8)
    (let ((primary (probe "(lol.clipboard:primary)")))
      (format t "~&  PRIMARY after the drag: ~a~%" primary)
      (check "a drag sets PRIMARY" (and primary (> (length primary) 3) (char= (char primary 0) #\")))
      (check "and enters the history"
             (yes "(equal (lol.clipboard:primary) (lol.clipboard:clip-current))"))
      (let ((before (probe-int "(length (lol.textview:tv-text (pane-tv (current-pane))))"))
            (plen (probe-int "(length (lol.clipboard:primary))")))
        (glide 0 60)
        (click "middle")
        (let ((after (probe-int "(length (lol.textview:tv-text (pane-tv (current-pane))))")))
          (format t "~&  middle click: ~a -> ~a chars (PRIMARY is ~a)~%" before after plen)
          (check "a middle click in editable text pastes PRIMARY"
                 (and before after plen (= after (+ before plen))))))))

  (format t "~&~%== D. rectangles ==~%")
  (qmp-chord "ctrl" "end")
  (dolist (l '("abcd" "efgh" "ijkl")) (qmp-key "ret") (qmp-type l))
  (sleep 0.5)
  (qmp-key "up") (qmp-key "up") (qmp-chord "ctrl" "a")
  (qmp-chord "ctrl" "x") (qmp-key "spc") (sleep 0.4)
  (qmp-key "down") (qmp-key "down") (qmp-key "right") (qmp-key "right") (sleep 0.4)
  (shot "rectangle-mark")
  (qmp-chord "ctrl" "x") (qmp-key "r") (qmp-key "k") (sleep 0.8)
  (let ((rect (probe "(lol.clipboard:clip-rectangle)")))
    (format t "~&  killed rectangle: ~a~%" rect)
    (check "C-x r k keeps the column block" (string= rect "(\"ab\" \"ef\" \"ij\")"))
    (check "and leaves the rest of each line" (ws-text-has (format nil "cd~%gh~%kl"))))

  (format t "~&~%== E. the right button: the context menu ==~%")
  (probe "(setf *menu-rect* nil)")
  (click "right")
  (shot "context-menu")
  (check "a right click opens the menu" (yes "(and *menu-rect* t)"))
  (qmp-key "esc") (sleep 0.8)

  (format t "~&~%== F. the middle button on a row: the halo ==~%")
  (qmp-hold "meta_l" "m") (sleep 2.5)
  (check "SuperL-m opens the module index in a new tab"
         (and (eql (probe-int "(length *trails*)") 2)
              (yes "(typep (pane-subject (current-pane)) 'subj-modules)")))
  (let ((top (probe-int "*content-top*")))
    (glide-to 120 (+ top 12)))
  (probe "(setf *halo-handles* nil)")
  (click "middle")
  (shot "halo-row")
  (check "a middle click on a row opens the halo" (yes "(and *halo-handles* t)"))
  (check "about what the row opens, first"
         (string= (probe "(getf *halo-layer* :role)") ":THING"))
  (qmp-key "up") (sleep 0.8)
  (check "Up walks out to the row itself (an ITEM)"
         (yes "(and (item-p (getf *halo-layer* :object)) t)"))
  (qmp-key "up") (qmp-key "up") (sleep 0.8)
  (shot "halo-pane")
  (check "then the list, then the PANE instance"
         (yes "(and (pane-p (getf *halo-layer* :object)) t)"))
  (qmp-key "1") (sleep 1.5)                         ; Inspect
  (format t "~&  after Inspect: ~a / ~a~%" (probe "(type-of (pane-subject (current-pane)))")
          (probe "(view-title (pane-view (current-pane)))"))
  (check "Inspect on the pane opens the PANE's fields, not what it shows"
         (yes "(and (pane-p (subject-value (pane-subject (current-pane)))) (eq :inspector (view-kind (pane-view (current-pane)))) t)"))

  (format t "~&~%== F2. the halo on the Workspace text: the code that draws it ==~%")
  (qmp-hold "meta_l" "1") (sleep 1.5)                ; back to the Workspace tab
  (probe "(setf (lol.clipboard::board-primary lol.clipboard::*clipboard*) nil)") ; nothing to paste
  (let ((tx (probe-int "(lol.textview::tv-x (pane-tv (current-pane)))"))
        (ty (probe-int "(lol.textview::tv-y (pane-tv (current-pane)))")))
    (glide-to (+ tx 1000) (+ ty 40)))                ; empty space right of the text
  (probe "(setf *halo-layer* nil)")
  (click "middle")
  (shot "halo-textview")
  (check "a middle click on the Workspace's empty text is about the TEXT VIEW instance"
         (yes "(and (lol.textview::textview-p (getf *halo-layer* :object)) t)"))
  (format t "~&  halo layer: ~a~%" (probe "(getf *halo-layer* :title)"))
  (qmp-key "2") (sleep 1.5)                         ; Code: tv-draw
  (format t "~&  after Code: ~a~%" (probe "(view-title (pane-view (current-pane)))"))
  (check "and its Code handle opens the definition that draws it"
         (yes "(and (subj-defn-p (pane-subject (current-pane))) (eq (subj-defn-name (pane-subject (current-pane))) 'lol.textview:tv-draw) t)"))

  (format t "~&~%== G. SuperL-h: the clipboard manager ==~%")
  (probe "(setf *clip-rect* nil)")
  (qmp-hold "meta_l" "h") (sleep 1.5)
  (shot "clipboard-manager")
  (check "SuperL-h opens the clipboard history" (yes "(and *clip-rect* t)"))
  (qmp-key "esc") (sleep 0.8)

  (format t "~&~%== H. the pointer hides when idle ==~%")
  (sleep 4.0)
  (check "hidden after a few idle seconds" (yes "*cursor-hidden*"))
  (glide 5 5)
  (check "back on the next move" (string= (probe "*cursor-hidden*") "NIL"))

  (format t "~&~%== I. the systray ==~%")
  (check "the tray is drawn" (yes "(and *tray-rect* t)"))
  (format t "~&  tray items: ~a~%" (probe "(tray-items)"))
  (qmp-hold "meta_l" "slash") (sleep 1.5)
  (shot "cheatsheet")
  (qmp-key "esc") (sleep 0.8)
  (shot "tray")

  (format t "~&~%== J. double / triple click, C-n in the Spotter, SuperL-q ==~%")
  ;; the Workspace tab is showing tv-draw's source since F2: aim at line 0, column 2,
  ;; which is inside "defun"
  (probe "(setf (lol.textview::tv-scroll (pane-tv (current-pane))) 0)")
  (let* ((geo (probe "(let ((tv (pane-tv (current-pane)))) (list (+ (lol.textview::tv-x tv) (lol.textview::tv-pad tv) lol.textview::*gutter-w* (floor (* 5 (lol.textview::tv-cw tv)) 2)) (+ (lol.textview::tv-y tv) (lol.textview::tv-pad tv) (floor (lol.textview:tv-line-height tv) 2))))"))
         (xy (read-from-string geo))
         (line0 (probe "(aref (lol.textview:tv-lines (pane-tv (current-pane))) 0)")))
    (glide-to (first xy) (second xy))
    (flet ((fast-click ()                          ; BUTTON's pauses would be too slow
             (dolist (down '("true" "false"))
               (qmp-events (format nil "{\"type\":\"btn\",\"data\":{\"down\":~a,\"button\":\"left\"}}" down))
               (sleep 0.05))))
      (fast-click) (fast-click) (sleep 0.6)
      (format t "~&  after a double click PRIMARY is ~a~%" (probe "(lol.clipboard:primary)"))
      (check "a double click selects the word" (string= (probe "(lol.clipboard:primary)") "\"defun\""))
      (sleep 1.0)
      (fast-click) (fast-click) (fast-click) (sleep 0.6)
      (check "a triple click selects the whole line, newline included"
             (string= (probe "(lol.clipboard:primary)")
                      (format nil "\"~a~%\"" (read-from-string line0))))))

  (qmp-hold "meta_l" "spc") (sleep 1.5)
  (qmp-type "draw") (sleep 1.0)
  (qmp-chord "ctrl" "n") (qmp-chord "ctrl" "n") (qmp-chord "ctrl" "p")
  (let ((before (probe-int "(length *trails*)")))
    (qmp-key "ret") (sleep 1.5)
    (format t "~&  opened: ~a~%" (probe "(view-title (pane-view (current-pane)))"))
    (check "C-n C-n C-p in the Spotter: the SECOND hit for \"draw\" opens (no n or p typed)"
           (and (eql (probe-int "(length *trails*)") (1+ before))
                (yes "(equal (view-title (pane-view (current-pane))) (view-title (present (getf (nth 1 (spotter-results \"draw\" (spotter-candidates) 60)) :subject))))"))))

  (probe "(setf *menu-rect* nil)")
  (qmp-hold "meta_l" "q") (sleep 1.5)
  (shot "leave-menu")
  (check "SuperL-q asks: a menu, and the environment still running" (yes "(and *menu-rect* t)"))
  (qmp-key "esc") (sleep 1.0)
  (check "Esc keeps you in the environment" (yes "(and *fb-canvas* (eq *leave-action* nil) t)"))

  (format t "~&~%== K. undo ==~%")
  (qmp-hold "meta_l" "w") (sleep 1.0)               ; close the Spotter's tab
  (qmp-hold "meta_l" "1") (sleep 1.0)
  (qmp-chord "ctrl" "g") (sleep 0.8)                ; tv-draw -> back to the Workspace
  (check "back on the Workspace" (string= (probe "(view-kind (pane-view (current-pane)))") ":WORKSPACE"))
  (let ((start (probe "(lol.textview:tv-text (pane-tv (current-pane)))")))
    (qmp-chord "ctrl" "end")
    (qmp-key "ret") (qmp-type "undo me") (sleep 0.5)
    (qmp-key "ret") (qmp-type "and me") (sleep 0.5)
    (check "typed two lines" (ws-text-has (format nil "undo me~%and me")))
    (qmp-chord "ctrl" "minus")
    (format t "~&  status: ~a~%" (probe "*browser-status*"))
    (check "C-_ takes back the last word typed" (and (ws-text-has "undo me") (not (ws-text-has "and me"))))
    (qmp-chord "ctrl" "slash") (qmp-chord "ctrl" "slash") (qmp-chord "ctrl" "slash")
    (check "C-/ (DEL + a held Ctrl) goes further back, to before the typing"
           (string= (probe "(lol.textview:tv-text (pane-tv (current-pane)))") start))
    (qmp-key "right")                                ; any other command ends the run
    ;; the run's LAST undo took out the first newline, so the first redo puts back
    ;; exactly that — and a second C-x u (the run goes on across the C-x prefix) the line
    (qmp-chord "ctrl" "x") (qmp-key "u") (sleep 0.6)
    (check "after a motion, C-x u undoes the last undo (Emacs's redo): the newline"
           (string= (probe "(lol.textview:tv-text (pane-tv (current-pane)))")
                    (prin1-to-string (format nil "~a~%" (read-from-string start)))))
    (qmp-chord "ctrl" "x") (qmp-key "u") (sleep 0.6)
    (check "and C-x u again, the line typed after it" (ws-text-has (format nil "~%undo me"))))
  t)

(run-guest-checks "check-clipboard" #'checks)
