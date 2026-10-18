;;;; check-font.lisp — assert runtime font resizing in the BOOTED guest.
;;;;
;;;;     sbcl --script check-font.lisp                              ; diskless
;;;;     LOL_GUEST_DISK=test-disk.img sbcl --script check-font.lisp ; + persistence
;;;;
;;;; poc/font-test.lisp checks the TrueType engine on the host and poc/render-frame.lisp
;;;; (--size N) checks what a frame looks like at a size. This checks what the MACHINE
;;;; does with the keys: it boots the real image, presses SuperL-= / SuperL-- /
;;;; SuperL-0 and the Emacs chord C-x C-= / C-- / C-0 through QMP — real evdev records,
;;;; Super genuinely held (QMP-HOLD) — and reads back, over the net REPL, the font's
;;;; size and cell, the row metrics the layout derives from it, the status line, and a
;;;; count of DRAW-BROWSER calls (sb-int:encapsulate, as check-pointer.lisp does), so
;;;; "the screen redrew" is an assertion and not a hope. A screendump before and after
;;;; must differ, too.
;;;;
;;;; With the stand-in disk attached (snapshot=on, so nothing persists past the run)
;;;; the guest has a real home, and the chosen size must land in
;;;; ~/.lol/settings. Diskless, HOME is / and nothing may be written — that is
;;;; asserted instead.
;;;;
;;;; Run ./build.sh first; this boots iso_root as it stands. LOL_PORT as for every check.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(load (merge-pathnames "host-client/png.lisp" lol.guest:*root*))
(use-package :lol.guest)

(defun font-state (label)
  "(SIZE CW CH ROW-H DRAWS STATUS) as the guest sees them."
  (let ((size (probe-int "(lol.canvas:font-size *bfont*)"))
        (cw (probe-int "(lol.canvas:font-cw *bfont*)"))
        (ch (probe-int "(lol.canvas:font-ch *bfont*)"))
        (row (probe-int "lol.widgets:+row-h+"))
        (draws (probe-int "(length *draws*)"))
        (status (probe "*browser-status*")))
    (format t "~&  ~26a size=~a cell=~ax~a row-h=~a draws=~a status=~a~%"
            label size cw ch row draws status)
    (finish-output)
    (list size cw ch row draws status)))

(defun screen-bytes ()
  "The guest display as raw RGB bytes, from a fresh QMP screendump."
  (let ((ppm (namestring (path "screen.ppm"))))
    (qmp (format nil "{\"execute\":\"screendump\",\"arguments\":{\"filename\":\"~a\"}}" ppm))
    (sleep 0.5)
    (nth-value 2 (lol.png:read-ppm ppm))))

(defun super (qcode) (qmp-hold "meta_l" qcode) (sleep 1.5))
(defun ctrl (qcode) (qmp-hold "ctrl" qcode) (sleep 1.0))

(defun checks ()
  (quiet-kernel-console)
  (wait-for-browser)
  (check "the environment draws with an OUTLINE font (the TTF loaded)"
         (string= "T" (probe "(lol.canvas:font-outline-p *bfont*)")))
  (format t "~&  font: ~a~%" (probe "(lol.canvas:font-description *bfont*)"))
  ;; count every frame from here on
  ;; (a frame is a repair that drew: the screen's morphs, damage by damage)
  (probe "(progn (defvar *draws* nil) (setf *draws* nil) (sb-int:encapsulate 'lol.morph:world-repair 'count (lambda (f &rest args) (let ((n (lol.morph:world-repairs *world*))) (multiple-value-prog1 (apply f args) (unless (= n (lol.morph:world-repairs *world*)) (push t *draws*)))))) t)")
  (let* ((disk (string= "T" (probe "(and (settings-path) t)")))
         (s0 (font-state "at boot"))
         (shot0 (screen-bytes)))
    (check "boots at the default size (18 px) when no settings say otherwise"
           (or disk (eql 18 (first s0))))
    (check "the row height is derived from the font: cell + 5"
           (and (third s0) (eql (fourth s0) (+ (third s0) 5))))

    (format t "~&~%== SuperL-= / SuperL-- / SuperL-0 ==~%")
    (super "equal")
    (let ((s1 (font-state "after SuperL-=")) (shot1 (screen-bytes)))
      (check "SuperL-= makes the font bigger" (and (first s1) (first s0) (> (first s1) (first s0))))
      (check "...and the cell with it" (and (second s1) (> (second s1) (second s0))
                                            (> (third s1) (third s0))))
      (check "...and the layout's row height follows" (eql (fourth s1) (+ (third s1) 5)))
      (check "...and the screen was redrawn (the world repaired it)"
             (and (fifth s1) (fifth s0) (> (fifth s1) (fifth s0))))
      (check "...and the pixels changed (screendump differs)"
             (and shot0 shot1 (not (equalp shot0 shot1))))
      (check "...and the status line says the new size"
             (search (format nil "~dpx" (first s1)) (sixth s1)))
      (super "minus")
      (let ((s2 (font-state "after SuperL--")))
        (check "SuperL-- makes it smaller again" (eql (first s2) (first s0))))
      (super "equal") (super "equal")
      (super "0")
      (let ((s3 (font-state "after = = then SuperL-0")))
        (check "SuperL-0 resets to the size it booted with" (eql (first s3) (first s0)))))

    (format t "~&~%== C-x C-= / C-x C-- / then bare + - (the Emacs repeat) ==~%")
    (ctrl "x") (ctrl "equal")
    (let ((e1 (font-state "after C-x C-=")))
      (check "C-x C-= makes it bigger" (and (first e1) (> (first e1) (first s0))))
      (qmp-key "equal") (sleep 1.0)
      (let ((e2 (font-state "then a bare =")))
        (check "a bare = right after repeats it (bigger still)" (and (first e2) (> (first e2) (first e1))))
        (qmp-key "minus") (sleep 1.0) (qmp-key "minus") (sleep 1.0)
        (let ((e3 (font-state "then - -")))
          (check "bare - - walk it back down" (eql (first e3) (first s0))))
        (ctrl "x") (ctrl "minus")
        (let ((e4 (font-state "after C-x C--")))
          (check "C-x C-- makes it smaller than the default" (and (first e4) (< (first e4) (first s0))))
          (ctrl "x") (ctrl "0")
          (check "C-x C-0 resets it" (eql (first (font-state "after C-x C-0")) (first s0))))))

    (format t "~&~%== the chord does not leak: a letter after C-x C-0 is typed, not eaten ==~%")
    (let ((before (probe-int "(length (lol.textview:tv-text (pane-tv (current-pane))))")))
      (qmp-key "z") (sleep 1.0)
      (check "the Workspace got the z (prefix cleared, nothing lost)"
             (eql (probe-int "(length (lol.textview:tv-text (pane-tv (current-pane))))")
                  (and before (1+ before)))))
    (qmp-key "backspace") (sleep 0.5)

    (format t "~&~%== persistence (~:[diskless: nothing to write~;a real home~]) ==~%" disk)
    (super "equal")
    (let ((size (first (font-state "after one more SuperL-="))))
      (if disk
          (progn
            (check "the size is saved in ~/.lol/settings"
                   (eql size (probe-int "(setting :font-size)")))
            (check "...and that file is HOME/.lol/settings (lol-home.lisp)"
                   (string= "T" (probe "(and (string= (settings-path) (concatenate 'string (sb-ext:posix-getenv \"HOME\") \"/.lol/settings\")) (probe-file (settings-path)) t)"))))
          (check "diskless: no settings file is written (HOME is /)"
                 (string= "NIL" (probe "(settings-path)")))))
    (super "0"))
  (probe "(sb-int:unencapsulate 'lol.morph:world-repair 'count)")
  t)

(run-guest-checks "check-font" #'checks)
