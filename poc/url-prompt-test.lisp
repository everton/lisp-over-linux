;;;; url-prompt-test.lisp — host test for the web pane's address prompt: the
;;;; minibuffer's candidate panel (poc/shell.lisp, "The candidate panel"; the morph in
;;;; poc/morphic.lisp), paths typed into C-l (a folder listed like a small file manager)
;;;; and the history (initramfs/web.lisp, "the history"). See doc/web-browser.org §4.6.
;;;;
;;;;     sbcl --script poc/url-prompt-test.lisp      ; exits 0 if every check passes
;;;;
;;;; No Chrome: the web tab is made by hand and never attached, so WEB-NAVIGATE only
;;;; records what it would have loaded (WEB-TAB-OPEN-URL). HOME is a scratch folder
;;;; with a few files in it, so the history file is written there, never to the real
;;;; ~/.lol. Like poc/morphic-test.lisp, every step checks that the screen repaired
;;;; incrementally is the screen the whole world paints from scratch — the panel comes
;;;; and goes over the panes, and a missed rect would show as a difference.
;;;;
;;;; All original code, under this project's MIT licence.

(require :sb-posix)

(defvar cl-user::*render-frame-library* t)
(load (merge-pathnames "render-frame.lisp" (make-pathname :directory (pathname-directory *load-truename*))))

(in-package :cl-user)

(let ((*load-verbose* nil) (*load-print* nil))
  (handler-bind ((warning #'muffle-warning))
    (dolist (m '("initramfs/lol-home.lisp" "initramfs/web-cdp.lisp" "initramfs/web.lisp"))
      (funcall 'load-recording (merge-pathnames m *root-dir*)))))

(defvar *fails* 0)
(defvar *checks* 0)
(defun check (name ok)
  (incf *checks*)
  (format t "~&  ~70a ~:[FAIL~;PASS~]~%" name ok)
  (unless ok (incf *fails*)))

(defparameter +w+ 1280)
(defparameter +h+ 720)

;; the clock and the battery are frozen: two draws a second apart must agree
(defun clock-string (&optional u) (declare (ignore u)) "2026-10-20 12:00:00")
(defun battery-status (&key fresh) (declare (ignore fresh)) nil)

(set-browser-font
 (open-browser-font (list "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf")
                    (merge-pathnames "cozette.lolf" *poc-dir*)))

;;; ---- a scratch home -----------------------------------------------------------

(defparameter *home* (format nil "/tmp/lol-url-prompt-test-~d" (sb-posix:getpid)))

(defun touch (rel &optional (bytes 0))
  (let ((path (format nil "~a/~a" *home* rel)))
    (with-open-file (s path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (dotimes (i bytes) (write-byte 65 s)))
    path))

(sb-posix:mkdir *home* #o700)
(dolist (d '("Documents" "Downloads" "Documents/old")) (sb-posix:mkdir (format nil "~a/~a" *home* d) #o755))
(touch "Documents/Report 2026.pdf" 2048)
(touch "Documents/notes.txt" 10)
(touch "todo.org" 5)
(touch ".hidden" 1)
(sb-posix:setenv "HOME" *home* 1)

;;; ---- the world, with one web tab that never meets Chrome ----------------------

(defvar *screen* (lol.canvas:make-canvas +w+ +h+) "Built incrementally, as in the guest.")
(defvar *fresh* (lol.canvas:make-canvas +w+ +h+) "Drawn from scratch, to compare with.")

(defun repair () (lol.morph:world-repair *world* *screen*))

(defun screen-right-p ()
  (repair)
  (lol.canvas:fill-rect *fresh* 0 0 +w+ +h+ #x123456)
  (lol.morph:world-paint *world* *fresh* :fresh t)
  (equalp (lol.canvas:canvas-pixels *screen*) (lol.canvas:canvas-pixels *fresh*)))

(defun ev (kind &rest args)
  (lol.morph:world-dispatch *world* (apply #'lol.morph:make-event :kind kind args)))

(defun keys (script)
  "Dispatch SCRIPT (render-frame's key tokens), repairing after each."
  (dolist (tok (split-on #\Space script))
    (multiple-value-bind (key arg state) (parse-key-token tok)
      (declare (ignore arg))
      (ev :key :key key :state state)
      (repair))))

(defun type-text (s)
  (loop for c across s do (ev :key :key c :state 0) (repair)))

(defun shot (name)
  "With LOL_SHOTS=DIR, keep the screen as DIR/url-prompt-NAME.ppm, for a person to look at."
  (let ((dir (sb-ext:posix-getenv "LOL_SHOTS")))
    (when (and dir (plusp (length dir)))
      (write-ppm *screen* (format nil "~a/url-prompt-~a.ppm" (string-right-trim "/" dir) name)))))

(defun panel ()
  (find-if (lambda (m) (typep m 'minibuffer-panel)) (lol.morph:morph-children *world*)))
(defun cands () (and *minibuffer* (mb-cands *minibuffer*)))
(defun shown () (mapcar (lambda (c) (getf c :label)) (cands)))
(defun text () (and *minibuffer* (mb-text *minibuffer*)))

(open-environment (parse-subject "modules"))
(defvar *tab* (%make-web-tab :url "about:blank"))
(push *tab* *web-tabs*)
(new-trail (subj-web *tab*))
(setf *world* (make-desk +w+ +h+))
(repair)

;;; ---------------------------------------------------------------------------

(format t "~&== what the address bar makes of a path ==~%")
(check "/etc/hosts -> file:///etc/hosts"
       (equal (web-url-for "/etc/hosts") "file:///etc/hosts"))
(check "~/Documents/Report 2026.pdf -> the home's file, the space encoded"
       (equal (web-url-for "~/Documents/Report 2026.pdf")
              (format nil "file://~a/Documents/Report%202026.pdf" *home*)))
(check "~/Documents/ keeps its trailing slash (a folder: Chrome lists it)"
       (equal (web-url-for "~/Documents/") (format nil "file://~a/Documents/" *home*)))
(check "a host and a search are as before"
       (and (equal (web-url-for "example.org") "https://example.org")
            (let ((u (web-url-for "lisp machine"))) (and (search "lisp" u) (not (search "file:" u))))
            (equal (web-url-for "localhost:3000") "http://localhost:3000")))

(format t "~&== the history ==~%")
(check "starts empty, with no file" (and (null (web-history)) (not (probe-file (web-history-path)))))
(web-history-note "https://github.com/sbcl/sbcl")
(web-history-note "https://github.com/sbcl/sbcl" :title "SBCL on GitHub")
(web-history-note "https://news.ycombinator.com/")
(web-history-note "https://news.ycombinator.com/" :title "Hacker News")
(web-history-note "https://en.wikipedia.org/wiki/Lisp_machine")
(web-history-note "https://en.wikipedia.org/wiki/Lisp_machine" :title "Lisp machine - Wikipedia")
(web-history-note "about:blank")
(web-history-note "data:text/plain,x")
(web-history-note (web-home-url))
(check "three pages remembered; about:, data: and the home page are not"
       (equal (mapcar (lambda (e) (getf e :url)) (web-history))
              '("https://en.wikipedia.org/wiki/Lisp_machine" "https://news.ycombinator.com/"
                "https://github.com/sbcl/sbcl")))
(check "a title names the entry without counting a visit"
       (let ((e (find "https://github.com/sbcl/sbcl" (web-history) :key (lambda (e) (getf e :url)) :test #'equal)))
         (and (equal (getf e :title) "SBCL on GitHub") (= 1 (getf e :visits)))))
(check "the first write happened at once, 0600"
       (and (probe-file (web-history-path))
            (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (web-history-path)))))))
(web-history-save :force t)
(let ((before (copy-tree (web-history))))
  (setf *web-history* :unread)
  (check "read back from ~/.lol/web/history.lisp, equal" (equal (web-history) before)))
(check "words match the URL or the title, ignoring case"
       (and (equal (mapcar (lambda (e) (getf e :url)) (web-history-matches "LISP wiki"))
                   '("https://en.wikipedia.org/wiki/Lisp_machine"))
            (equal (mapcar (lambda (e) (getf e :url)) (web-history-matches "hacker"))
                   '("https://news.ycombinator.com/"))))
(check "a page whose address starts with the text comes first (git -> github.com)"
       (progn (web-history-note "https://example.org/git-tutorial")
              (equal (getf (first (web-history-matches "git")) :url) "https://github.com/sbcl/sbcl")))

(format t "~&== C-l: the history in the panel ==~%")
(keys "C-l")
(check "C-l on a blank tab opens an empty prompt" (equal (text) ""))
(check "... and lists the recent pages at once, newest first"
       (and (equal (mb-title *minibuffer*) "recent pages")
            (equal (getf (first (cands)) :text) "https://example.org/git-tutorial")))
(check "the panel is a morph of the desk, above the status line, full width"
       (let ((p (panel)))
         (and p (destructuring-bind (x y w h) (lol.morph:morph-bounds p)
                  (and (= x 0) (= w +w+) (= (+ y h) (- +h+ (status-h))))))))
(check "the screen is right with the panel up" (screen-right-p))
(shot "history")
(type-text "hack")
(check "typing narrows it: \"hack\" finds Hacker News"
       (equal (shown) '("Hacker News")))
(check "the panel shrank to its one row, and the screen is right"
       (and (panel) (< (fourth (lol.morph:morph-bounds (panel))) (* 4 +row-h+)) (screen-right-p)))
(check "nothing is chosen yet" (null (mb-sel *minibuffer*)))
(keys "C-n")
(check "C-n chooses the first row" (eql (mb-sel *minibuffer*) 0))
(check "the chosen row is drawn, and the screen is right" (screen-right-p))
(keys "ret")
(check "Enter opens the chosen page, not the typed text"
       (equal (web-tab-open-url *tab*) "https://news.ycombinator.com/"))
(check "the prompt and its panel are gone, what it covered repaired"
       (and (null *minibuffer*) (null (panel)) (screen-right-p)))

(format t "~&== C-l: a path lists its folder ==~%")
(setf (web-tab-url *tab*) "about:blank")
(keys "C-l")
(type-text "~/")
(check "\"~/\" lists the home folder, folders first, no dot files"
       (equal (shown) '("Documents/" "Downloads/" "todo.org")))
(check "... in sections, the title naming the folder"
       (and (equal (mapcar (lambda (c) (getf c :group)) (cands)) '("Folders" "Folders" "Files"))
            (equal (mb-title *minibuffer*) "files in ~/")))
(check "the screen is right" (screen-right-p))
(type-text "Doc")
(check "\"~/Doc\" narrows to Documents/" (equal (shown) '("Documents/")))
(keys "tab")
(check "Tab completes like a shell: ~/Documents/" (equal (text) "~/Documents/"))
(check "... and the panel now lists inside it, the folder first"
       (and (equal (first (shown)) "old/")
            (null (set-exclusive-or (shown) '("old/" "notes.txt" "Report 2026.pdf") :test #'equal))))
(shot "folder")
(check "a file's row says its size"
       (search "2.0K" (getf (find "Report 2026.pdf" (cands) :key (lambda (c) (getf c :label)) :test #'equal)
                            :detail)))
(keys "down down")
(check "Down twice chooses the second row"
       (equal (getf (minibuffer-chosen *minibuffer*) :label) (second (shown))))
(keys "up up")
(check "Up past the first row gives the choice back to the text" (null (mb-sel *minibuffer*)))
(keys "down tab")
(check "Tab on a chosen folder goes into it" (equal (text) "~/Documents/old/"))
(check "an empty folder lists nothing: no panel, the screen right"
       (and (null (cands)) (null (panel)) (screen-right-p)))
(keys "M-bs")
(check "M-Backspace climbs back out" (equal (text) "~/Documents/"))
(type-text "Rep")
(keys "down ret")
(check "Enter on a file opens it as file://, encoded"
       (equal (web-tab-open-url *tab*) (format nil "file://~a/Documents/Report%202026.pdf" *home*)))
(keys "C-l")
(type-text "/no/such/dir/")
(check "a folder that does not exist lists nothing, and the prompt stays"
       (and *minibuffer* (null (cands))))
(keys "esc")

(format t "~&== in the guest, a path outside /home is listed from Chrome's root ==~%")
;; Chrome's container has the host's Debian as its root; the environment has it at
;; /mnt/host (WEB-CHROME-ROOT). Stand a fake one up in the scratch home.
(dolist (d '("hostroot" "hostroot/usr" "hostroot/usr/share" "hostroot/usr/lib"))
  (sb-posix:mkdir (format nil "~a/~a" *home* d) #o755))
(let ((real (fdefinition 'web-chrome-root)))
  (setf (fdefinition 'web-chrome-root) (lambda () (format nil "~a/hostroot" *home*)))
  (unwind-protect
       (progn
         (check "\"/usr/sh\" is listed from Chrome's root, the row as Chrome names it"
                (multiple-value-bind (cands title) (web-path-candidates "/usr/sh")
                  (and (equal (mapcar (lambda (c) (getf c :text)) cands) '("/usr/share/"))
                       (equal title "files in /usr/"))))
         (check "a path under /home or /tmp is listed as we see it (the same in both)"
                (equal (mapcar (lambda (c) (getf c :label))
                               (web-path-candidates (format nil "~a/Doc" *home*)))
                       '("Documents/"))))
    (setf (fdefinition 'web-chrome-root) real)))
(check "on the host there is no second root" (null (web-chrome-root)))

(format t "~&== the mouse, and forgetting a page ==~%")
(setf (web-tab-url *tab*) "about:blank")
(keys "C-l")
(type-text "ycomb")
(let* ((p (panel)) (hits (panel-hits p)))
  (check "a click on a row answers it"
         (and hits
              (progn (ev :down :x 200 :y (+ 3 (car (first hits)))) (repair)
                     (and (null *minibuffer*)
                          (equal (web-tab-open-url *tab*) "https://news.ycombinator.com/"))))))
(check "the screen is right after the click" (screen-right-p))
(setf (web-tab-url *tab*) "about:blank")
(keys "C-l")
(type-text "wiki")
(keys "down S-del")
(check "Shift-Delete forgets the chosen page, from the panel and the file"
       (and (null (cands))
            (progn (setf *web-history* :unread)
                   (null (web-history-matches "wiki")))))
(keys "esc")
(check "the prompt opened on a page's own URL lists nothing until edited"
       (progn (setf (web-tab-url *tab*) "https://github.com/sbcl/sbcl")
              (keys "C-l")
              (prog1 (and (equal (text) "https://github.com/sbcl/sbcl") (null (cands)))
                (keys "esc"))))

(format t "~&== the Spotter offers the history too ==~%")
(check "\"hacker\" in the Spotter finds Hacker News from the history"
       (find-if (lambda (r) (search "history" (getf r :detail))) (web-spotter-rows "hacker")))

;;; ---- clean up -----------------------------------------------------------------

(sb-ext:run-program "/bin/rm" (list "-rf" *home*))
(format t "~&~d checks, ~d failed~%" *checks* *fails*)
(sb-ext:exit :code (if (zerop *fails*) 0 1))
