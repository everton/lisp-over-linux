;;;; lol-home-test.lisp — host test of initramfs/lol-home.lisp: where things live, and
;;;; the move from the pre-2026-10-17 layout into ~/.lol.
;;;;
;;;;   sbcl --script poc/lol-home-test.lisp
;;;;
;;;; Every HOME here is a fresh folder under /tmp (or $LOL_HOME_TEST_DIR), passed to
;;;; the functions explicitly — $HOME is never read or written, so the developer's
;;;; real ~/.lol, ~/.config/lol and ~/.local stay untouched. Checks:
;;;;   - LOL-PATH: under HOME/.lol, NIL for HOME=/ or a missing HOME;
;;;;   - a full old layout (settings, hosts, wifi/ 0700 with a 0600 network, the API
;;;;     key, agent.lisp, a Chrome profile, the journal and the desk) moved, contents
;;;;     and modes intact, old folders removed;
;;;;   - a second run moves nothing (idempotent);
;;;;   - never overwrite: an old file whose new place exists is left, the new one
;;;;     unchanged, and the old folder kept because it is not empty;
;;;;   - two folders merged entry by entry;
;;;;   - an unrelated file in ~/.config/lol keeps the folder;
;;;;   - no home: nothing done, NIL.
;;;;
;;;; Prints one line per check and exits non-zero on any failure.

(load (merge-pathnames "../initramfs/lol-home.lisp" *load-truename*))

(sb-alien:define-alien-routine ("chmod" %chmod) sb-alien:int (path sb-alien:c-string) (mode sb-alien:unsigned-int))

(defvar *failures* 0)
(defun check (name ok)
  (format t "~:[FAIL~;ok  ~]  ~a~%" ok name)
  (unless ok (incf *failures*)))

(defvar *base* (format nil "~a/lol-home-test-~d"
                       (string-right-trim "/" (or (sb-ext:posix-getenv "LOL_HOME_TEST_DIR") "/tmp"))
                       (sb-unix:unix-getpid)))

(defvar *n* 0)
(defun fresh-home ()
  (let ((h (format nil "~a/home~d" *base* (incf *n*))))
    (ensure-directories-exist (concatenate 'string h "/"))
    h))

(defun put (home rel text &optional (mode #o644))
  (let ((p (concatenate 'string home "/" rel)))
    (ensure-directories-exist p)
    (with-open-file (s p :direction :output :if-exists :supersede) (write-string text s))
    (%chmod p mode)
    p))

(defun slurp (path)
  (with-open-file (s path :if-does-not-exist nil)
    (and s (let ((str (make-string (file-length s)))) (subseq str 0 (read-sequence str s))))))

(defun mode-of (path)
  (multiple-value-bind (ok dev ino mode) (sb-unix:unix-lstat path)
    (declare (ignore dev ino))
    (and ok (logand mode #o777))))

(defun there (home rel) (%lol-exists-p (concatenate 'string home "/" rel)))

(defun migrate (home)
  (let ((lines '()))
    (values (migrate-lol-home :home home :log (lambda (l) (push l lines)))
            (reverse lines))))

;;; ---- lol-path -----------------------------------------------------------------

(let ((h (fresh-home)))
  (check "lol-path is under HOME/.lol"
         (equal (lol-path "settings" h) (concatenate 'string h "/.lol/settings")))
  (check "a trailing slash is kept, a leading one dropped"
         (equal (lol-path "/log/" h) (concatenate 'string h "/.lol/log/")))
  (check "HOME with a trailing slash" (equal (lol-home (concatenate 'string h "/"))
                                             (concatenate 'string h "/.lol"))))
(check "HOME=/ has no lol-path (diskless)" (null (lol-path "settings" "/")))
(check "a HOME that does not exist has none" (null (lol-path "settings" (format nil "~a/nowhere" *base*))))

;;; ---- the whole old layout ------------------------------------------------------

(let ((h (fresh-home)))
  (put h ".config/lol/settings" "(:FONT-SIZE 24)")
  (put h ".config/lol/hosts" "10.0.2.2 box")
  (put h ".config/lol/wifi/686f6d65.net" "(:SSID \"home\" :PSK \"secret\")" #o600)
  (%chmod (concatenate 'string h "/.config/lol/wifi") #o700)
  (put h ".config/lol/anthropic-api-key" "sk-test-not-a-key" #o600)
  (put h ".config/lol/agent.lisp" "(:model \"m\")")
  (put h ".config/lol/chrome/Default/Preferences" "{}")
  (put h ".local/share/lol/changes.lisp" "(:seq 1)" #o600)
  (put h ".local/share/lol/changes-unfolded.lisp" ";; x")
  (put h ".local/share/lol/replay-off" "")
  (put h ".local/state/lol/desk.lisp" "(:tabs nil)")
  (put h ".local/state/lol/desk-autosave.lisp" "(:tabs nil)")
  (put h ".local/share/other-app/keep" "not ours")
  (multiple-value-bind (n lines) (migrate h)
    (check "eight old paths moved" (eql n 8))
    (check "each move logged" (= 8 (count-if (lambda (l) (search "moved " l)) lines))))
  (check "settings moved, content kept" (equal (slurp (concatenate 'string h "/.lol/settings")) "(:FONT-SIZE 24)"))
  (check "hosts moved" (there h ".lol/hosts"))
  (check "a saved network moved, still 0600" (eql #o600 (mode-of (concatenate 'string h "/.lol/wifi/686f6d65.net"))))
  (check "the wifi folder is still 0700" (eql #o700 (mode-of (concatenate 'string h "/.lol/wifi"))))
  (check "the API key moved, still 0600" (eql #o600 (mode-of (concatenate 'string h "/.lol/agent/anthropic-api-key"))))
  (check "the agent folder it made is 0700" (eql #o700 (mode-of (concatenate 'string h "/.lol/agent"))))
  (check "agent.lisp -> agent/settings.lisp" (equal (slurp (concatenate 'string h "/.lol/agent/settings.lisp")) "(:model \"m\")"))
  (check "the Chrome profile moved whole" (there h ".lol/web/chrome/Default/Preferences"))
  (check "the journal moved" (equal (slurp (concatenate 'string h "/.lol/journal/changes.lisp")) "(:seq 1)"))
  (check "replay-off moved with it" (there h ".lol/journal/replay-off"))
  (check "the desk moved" (there h ".lol/desk/desk-autosave.lisp"))
  (check "~/.config/lol removed (empty)" (not (there h ".config/lol")))
  (check "~/.local/share/lol and ~/.local/state/lol removed"
         (and (not (there h ".local/share/lol")) (not (there h ".local/state/lol"))))
  (check "another app's folder untouched" (there h ".local/share/other-app/keep"))
  (multiple-value-bind (n lines) (migrate h)
    (check "a second run moves nothing and logs nothing" (and (eql n 0) (null lines)))))

;;; ---- never overwrite -------------------------------------------------------------

(let ((h (fresh-home)))
  (put h ".config/lol/settings" "(:FONT-SIZE 30)")
  (put h ".lol/settings" "(:FONT-SIZE 18)")
  (put h ".config/lol/notes.txt" "mine")
  (multiple-value-bind (n lines) (migrate h)
    (check "nothing moved over an existing file" (eql n 0))
    (check "the refusal is logged" (some (lambda (l) (search "already exists" l)) lines)))
  (check "the new settings unchanged" (equal (slurp (concatenate 'string h "/.lol/settings")) "(:FONT-SIZE 18)"))
  (check "the old settings left where it was" (equal (slurp (concatenate 'string h "/.config/lol/settings")) "(:FONT-SIZE 30)"))
  (check "~/.config/lol kept (not empty)" (there h ".config/lol/notes.txt")))

;;; ---- two folders merge -----------------------------------------------------------

(let ((h (fresh-home)))
  (put h ".config/lol/wifi/a.net" "old a")
  (put h ".config/lol/wifi/b.net" "old b")
  (put h ".lol/wifi/a.net" "new a")
  (multiple-value-bind (n lines) (migrate h)
    (check "the missing entry moved into the existing folder" (eql n 1))
    (check "the clash logged" (some (lambda (l) (search "a.net" l)) lines)))
  (check "b.net moved" (equal (slurp (concatenate 'string h "/.lol/wifi/b.net")) "old b"))
  (check "a.net not overwritten" (equal (slurp (concatenate 'string h "/.lol/wifi/a.net")) "new a"))
  (check "the old a.net kept, so its folder too" (there h ".config/lol/wifi/a.net")))

;;; ---- no home ---------------------------------------------------------------------

(check "HOME=/ migrates nothing" (null (migrate-lol-home :home "/" :log (lambda (l) (error "logged ~a" l)))))

(sb-ext:run-program "/bin/rm" (list "-rf" *base*))
(format t "~:[all ok~;~:*~d FAILED~]~%" (and (plusp *failures*) *failures*))
(sb-ext:exit :code (if (zerop *failures*) 0 1))
