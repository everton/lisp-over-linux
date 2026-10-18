;;;; lol-home.lisp — the ONE place that names where the environment keeps things.
;;;;
;;;;   ~/.lol/settings              your preferences: font size, tap-to-click (settings.lisp)
;;;;   ~/.lol/hosts                 /etc/hosts-style name overrides (dns.lisp)
;;;;   ~/.lol/wifi/                 saved Wi-Fi networks, 0700, each file 0600 (wifi.lisp)
;;;;   ~/.lol/agent/                the agent: anthropic-api-key, settings.lisp,
;;;;                                conversations/ (agent.lisp, agent-store.lisp)
;;;;   ~/.lol/journal/              every Accept (changes.lisp) and its kin: changes-damaged,
;;;;                                changes-unfolded, changes-discarded-N, replay-off
;;;;                                (journal.lisp, file-out.lisp)
;;;;   ~/.lol/desk/                 tabs + Workspace text: desk.lisp, desk-autosave.lisp (desk.lisp)
;;;;   ~/.lol/web/chrome/           the web pane's own Chrome profile (web.lisp)
;;;;   ~/.lol/ct/<name>/            a container's overlay uppers (containers.lisp, lol-ct.c)
;;;;   ~/.lol/log/                  session, kernel, stderr and network logs (session-log.lisp)
;;;;
;;;; Until 2026-10-17 these were spread over three trees — XDG's ~/.config/lol/,
;;;; ~/.local/share/lol/ and ~/.local/state/lol/ — beside ~/.lol/ for the logs and the
;;;; containers. The user chose one tree on reviewing doc/session-persistence.org §7.8:
;;;; everything the environment writes about you is in one folder you can back up,
;;;; read from Debian, or delete. MIGRATE-LOL-HOME (below) moves an older layout over.
;;;;
;;;; THE RULE every module keeps: no module spells a home path itself. It asks
;;;; LOL-PATH, which answers NIL when there is no real home — a diskless boot runs with
;;;; HOME=/ (the initramfs, a RAM disk gone at power-off), and writing a preference or a
;;;; journal THERE would only pretend to keep it. What a module does with NIL is its own
;;;; affair: settings and the journal write nothing, the logs fall back to /tmp/lol-log/.
;;;;
;;;; Loaded SECOND in build.sh (right after logcount-fix.lisp), before session-log.lisp,
;;;; so it uses nothing but SBCL itself: SB-UNIX and three libc calls.
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

(defparameter +lol-dir-name+ ".lol" "The folder under $HOME that holds everything.")

(defun %lol-directory-p (path)
  "True when PATH names a directory (following symlinks: a home may be one)."
  (multiple-value-bind (ok dev ino mode) (sb-unix:unix-stat path)
    (declare (ignore dev ino))
    (and ok (= (logand mode #o170000) #o040000))))

(defun lol-home (&optional home)
  "$HOME/.lol (no trailing slash), or NIL when there is no real home: HOME unset,
   empty or /, or not a directory. HOME, when given, stands in for $HOME (the tests,
   journal.lisp's *PERSIST-HOME*). The folder itself need not exist yet."
  (let ((h (or home (sb-ext:posix-getenv "HOME"))))
    (when (and h (plusp (length h)))
      (let ((h (if (> (length h) 1) (string-right-trim "/" h) h)))
        (when (and (string/= h "/") (%lol-directory-p h))
          (concatenate 'string h "/" +lol-dir-name+))))))

(defun lol-path (sub &optional home)
  "SUB under ~/.lol — e.g. (lol-path \"settings\"), (lol-path \"log/\") with its
   trailing slash kept — or NIL when there is no real home (LOL-HOME)."
  (let ((root (lol-home home)))
    (and root (concatenate 'string root "/" (string-left-trim "/" sub)))))

;;; ===========================================================================
;;; Migration: the layout before 2026-10-17 -> ~/.lol
;;; ===========================================================================
;;;
;;; The laptop already has files at the old places (the font size, saved Wi-Fi
;;; networks, the API key, a journal and a desk). At boot, before anything reads
;;; them, each old path is RENAMED to its new place — rename(2) within /home, one
;;; filesystem, so a folder of any size moves in one step and nothing is copied.
;;;
;;; Its promises:
;;;   * never overwrite: renameat2(RENAME_NOREPLACE) refuses atomically when the new
;;;     name exists (a plain rename would replace it). Old and new both present is
;;;     logged and left alone — except two folders, whose entries are moved one by one
;;;     under the same rule;
;;;   * never delete: a file is only ever renamed. An old FOLDER is removed with
;;;     rmdir(2) once it is empty, which cannot remove anything else — a move that
;;;     failed leaves its file, so the folder stays;
;;;   * idempotent: a second run finds no old paths and does nothing;
;;;   * never signals: a boot with a read-only or missing home just logs the reasons.

(defparameter *lol-migrations*
  '((".config/lol/settings"            . "settings")
    (".config/lol/hosts"               . "hosts")
    (".config/lol/wifi"                . "wifi")
    (".config/lol/anthropic-api-key"   . "agent/anthropic-api-key")
    (".config/lol/agent.lisp"          . "agent/settings.lisp")
    (".config/lol/chrome"              . "web/chrome")
    (".local/share/lol"                . "journal")
    (".local/state/lol"                . "desk"))
  "(OLD . NEW): OLD relative to $HOME, NEW relative to ~/.lol. Each OLD that exists
   is moved to NEW when NEW does not exist.")

(defparameter *lol-old-directories*
  '(".config/lol" ".local/share/lol" ".local/state/lol")
  "Old folders removed afterwards — with rmdir(2), so only when empty.")

(sb-alien:define-alien-routine ("renameat2" %lol-renameat2) sb-alien:int
  (olddirfd sb-alien:int) (oldpath sb-alien:c-string)
  (newdirfd sb-alien:int) (newpath sb-alien:c-string) (flags sb-alien:unsigned-int))
(sb-alien:define-alien-routine ("rmdir" %lol-rmdir) sb-alien:int (path sb-alien:c-string))

(defun %lol-exists-p (path)
  "True when PATH exists at all — lstat, so a dangling symlink counts as there."
  (and (sb-unix:unix-lstat path) t))

(defun %lol-mkdirs (dir)
  "mkdir -p DIR, each folder it makes 0700 (they may hold the API key)."
  (let ((parent (subseq dir 0 (or (position #\/ dir :from-end t) 0))))
    (when (and (plusp (length parent)) (not (%lol-exists-p parent)))
      (%lol-mkdirs parent)))
  (or (%lol-directory-p dir) (sb-unix:unix-mkdir dir #o700)))

(defun %lol-rename-noreplace (from to)
  "Rename FROM to TO, refusing if TO exists. Returns NIL or an error string.
   renameat2's RENAME_NOREPLACE makes the refusal atomic; a filesystem without it
   (EINVAL) gets a check and a plain rename(2) — on one PID 1 nobody races us."
  (if (zerop (%lol-renameat2 -100 from -100 to 1))   ; AT_FDCWD, RENAME_NOREPLACE
      nil
      (let ((errno (sb-alien:get-errno)))
        (cond ((/= errno 22) (sb-int:strerror errno))   ; 22 = EINVAL: no RENAME_NOREPLACE here
              ((%lol-exists-p to) "the new name exists")
              (t (multiple-value-bind (ok err) (sb-unix:unix-rename from to)
                   (if ok nil (sb-int:strerror err))))))))

(defun %lol-directory-entries (dir)
  "The names in DIR (not . or ..), read with CL DIRECTORY, symlinks unresolved."
  (mapcar (lambda (p)
            (let ((s (string-right-trim "/" (sb-ext:native-namestring p))))
              (subseq s (1+ (position #\/ s :from-end t)))))
          (ignore-errors
           (directory (merge-pathnames (make-pathname :name :wild :type :wild)
                                       (sb-ext:parse-native-namestring
                                        (concatenate 'string dir "/")))
                      :resolve-symlinks nil))))

(defun %lol-move (from to log)
  "Move FROM to TO under the rules above; LOG gets one line per thing done or refused.
   Returns the number of paths moved."
  (cond ((not (%lol-exists-p from)) 0)
        ((not (%lol-exists-p to))
         (let ((parent (subseq to 0 (position #\/ to :from-end t))))
           (%lol-mkdirs parent)
           (let ((err (%lol-rename-noreplace from to)))
             (cond (err (funcall log (format nil "NOT moved ~a -> ~a: ~a (left where it was)" from to err))
                        0)
                   (t (funcall log (format nil "moved ~a -> ~a" from to))
                      1)))))
        ((and (%lol-directory-p from) (%lol-directory-p to))
         ;; both folders: merge one entry at a time, then drop FROM if that emptied it
         (let ((n (loop for name in (%lol-directory-entries from)
                        sum (%lol-move (concatenate 'string from "/" name)
                                       (concatenate 'string to "/" name) log))))
           (%lol-rmdir from)
           n))
        (t (funcall log (format nil "NOT moved ~a: ~a already exists (both kept)" from to))
           0)))

(defun migrate-lol-home (&key home (log (lambda (line) (format t "~&lol-home: ~a~%" line))))
  "Move every file of the pre-2026-10-17 layout into ~/.lol (see *LOL-MIGRATIONS*).
   HOME stands in for $HOME (the host test). LOG is called with one string per move
   or refusal — the supervisor passes SESSION-LOG. Returns the number moved; NIL
   when there is no real home. Never signals."
  (let ((root (lol-home home)))
    (when root
      (let ((h (subseq root 0 (- (length root) (length +lol-dir-name+) 1))))
        (handler-case
            (prog1 (loop for (old . new) in *lol-migrations*
                         sum (%lol-move (concatenate 'string h "/" old)
                                        (concatenate 'string root "/" new) log))
              (dolist (d *lol-old-directories*)
                (%lol-rmdir (concatenate 'string h "/" d))))   ; only if empty
          (error (e)
            (ignore-errors (funcall log (format nil "migration stopped: ~a" e)))
            0))))))
