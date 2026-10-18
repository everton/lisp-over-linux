;;;; check-isearch.lisp — C-s / C-r, the incremental search, in the booted guest.
;;;;
;;;;     sbcl --script check-isearch.lisp        ; exits 0 if all assertions pass
;;;;
;;;; The semantics are host-tested key for key against Emacs (poc/isearch-test.lisp);
;;;; this proves the KEYS: that C-s and C-r arrive from the console (raw mode has IXON
;;;; off, or the tty would eat C-s as XOFF — doc/diagnostics.org §1), reach the search,
;;;; and that the keys after it do what the search says. Driven by QMP key events; the
;;;; guest's state is read back over the net REPL (host-client/guest-harness.lisp).
;;;;
;;;;   A. C-s in the Workspace: the prompt, typing moves to a match, C-s the next one
;;;;   B. Enter ends it at the match with the mark at the start; C-x C-x goes back
;;;;   C. C-r searches backward; C-g cancels back to where it began
;;;;   D. a key that is not a search key ends the search and acts (C-a)
;;;;   E. C-x C-s is still "save", never a search

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))

(in-package :lol.guest)

(defun qmp-chord (modifier qcode)
  (qmp (format nil "{\"execute\":\"send-key\",\"arguments\":{\"keys\":[~
                    {\"type\":\"qcode\",\"data\":\"~a\"},{\"type\":\"qcode\",\"data\":\"~a\"}]}}"
               modifier qcode))
  (sleep 0.6))

(defun yes (form) (string= (probe form) "T"))

(defun point-is (line col)
  (yes (format nil "(let ((tv (pane-tv (current-pane)))) (and (= (lol.textview:tv-point-line tv) ~d) (= (lol.textview:tv-point-col tv) ~d)))"
               line col)))

(defun searching-p () (yes "(and (lol.textview:tv-isearch (pane-tv (current-pane))) t)"))

(defun checks ()
  (quiet-kernel-console)
  (wait-for-browser)
  (check "the Workspace is up" (string= (probe "(view-kind (pane-view (current-pane)))") ":WORKSPACE"))
  ;; a known text at the end of the buffer: two lines, "beta" three times
  (qmp-chord "ctrl" "end")
  (qmp-key "ret") (qmp-type "zq alpha beta") (qmp-key "ret") (qmp-type "zq gamma beta beta")
  (sleep 0.5)
  (let ((l1 (probe-int "(- (length (lol.textview:tv-lines (pane-tv (current-pane)))) 2)")))
    (qmp-chord "ctrl" "a")
    (qmp-key "up")                                   ; point: the start of "zq alpha beta"

    (format t "~&~%== A. C-s ==~%")
    (qmp-chord "ctrl" "s")
    (check "C-s starts a search (it reached the environment)" (searching-p))
    (check "the status line is the search's prompt"
           (yes "(eq :isearch (first (status-line-content (current-pane))))"))
    (qmp-type "beta") (sleep 0.5)
    (check "typing \"beta\" moves to the end of the first match" (point-is l1 13))
    (check "the prompt shows it" (search "I-search: \" \"beta\"" (probe "(status-line-content (current-pane))")))
    (qmp-chord "ctrl" "s")
    (check "C-s again: the next match, on the next line" (point-is (1+ l1) 13))

    (format t "~&~%== B. Enter, the mark, C-x C-x ==~%")
    (qmp-key "ret") (sleep 0.5)
    (check "Enter ends the search at the match" (and (not (searching-p)) (point-is (1+ l1) 13)))
    (check "\"Mark saved where search started\""
           (string= (probe "*browser-status*") "\"Mark saved where search started\""))
    (check "the text is unchanged (Enter did not insert)" (yes "(and (search \"gamma beta beta\" (lol.textview:tv-text (pane-tv (current-pane)))) t)"))
    (qmp-chord "ctrl" "x") (qmp-chord "ctrl" "x")
    (check "C-x C-x: back where the search began, the region shown"
           (and (point-is l1 0) (yes "(lol.textview:tv-mark-active (pane-tv (current-pane)))")))
    (qmp-chord "ctrl" "spc")                         ; C-SPC: drop the region again

    (format t "~&~%== C. C-r, C-g ==~%")
    (qmp-chord "ctrl" "e") (qmp-key "down") (qmp-chord "ctrl" "e")   ; end of "zq gamma beta beta"
    (qmp-chord "ctrl" "r")
    (qmp-type "beta") (sleep 0.5)
    (check "C-r beta: the start of the last match" (point-is (1+ l1) 14))
    (check "the prompt says backward" (search "I-search backward: " (probe "(status-line-content (current-pane))")))
    (qmp-chord "ctrl" "r")
    (check "C-r again: the one before" (point-is (1+ l1) 9))
    (qmp-chord "ctrl" "g")
    (check "C-g cancels: back at the end of the line, no search"
           (and (not (searching-p)) (point-is (1+ l1) 18)))

    (format t "~&~%== D. any other key ends the search and acts ==~%")
    (qmp-chord "ctrl" "s") (qmp-type "gam") (sleep 0.4)
    (qmp-chord "ctrl" "a")
    (check "C-a: the search is over and point at the line's start"
           (and (not (searching-p)) (point-is (1+ l1) 0)))

    (format t "~&~%== E. C-x C-s is save ==~%")
    (qmp-chord "ctrl" "x") (qmp-chord "ctrl" "s")
    (check "C-x C-s in the Workspace answers as save does, no search started"
           (and (not (searching-p))
                (string= (probe "*browser-status*") "\"C-x C-s: this pane is not a file\"")))
    (check "the environment's thread is alive (the console was not stopped)"
           (string= (probe "(+ 1 2)") "3")))
  t)

(run-guest-checks "check-isearch" #'checks)
