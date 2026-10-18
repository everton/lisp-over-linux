;;;; check-persist.lisp — session persistence across REAL reboots of the guest.
;;;;
;;;;     sbcl --script check-persist.lisp            ; exits 0 if every assertion passes
;;;;     LOL_PERSIST_DIR=/some/scratch sbcl --script check-persist.lisp
;;;;
;;;; The other disk checks attach test-disk.img with snapshot=on, which is exactly
;;;; wrong here: a second boot must see the first boot's writes. So this one COPIES
;;;; test-disk.img (cp --sparse=always) into a scratch directory and attaches the copy
;;;; writable (the harness's *DISK-COPY*). The pristine image is never touched, the
;;;; real disk never comes near QEMU, and the copy is deleted at the end.
;;;;
;;;; Before boot 1 a FIXTURE checkout is planted in the copy with debugfs (no root
;;;; needed for an image file): initramfs/examples.lisp at the absolute path build.sh
;;;; recorded, owned by uid 1000 — so file-out has a real file to fold into.
;;;;
;;;;   boot 1  Accept (defun zz-persist () 42) and an edit to lol.examples::make-counter
;;;;           (the Accept-package fix, §1.2); type into the Workspace; open a second
;;;;           tab and tear it off into a window (SuperL-g, moved); copy to the clipboard
;;;;           (with :desk-clipboard on); File out; then SuperL-q, "Save and shut down"
;;;;           — QEMU must exit by itself.
;;;;   between debugfs: the journal and the desk owned by uid 1000; dumpe2fs: clean;
;;;;           the fixture changed in exactly one place.
;;;;   boot 2  everything is back, the window where it was; the change set lists both;
;;;;           then the CRASH case: one more Accept, a torn half-record appended, and
;;;;           QEMU killed with -9.
;;;;   boot 3  the crash Accept survived (fsync per Accept); the torn tail was cut and
;;;;           reported; the journal view opened by itself; replay-off planted, then
;;;;           "Shut down without saving".
;;;;   boot 4  replay was off: nothing replayed, no desk restored. Then the files are put
;;;;           BACK at the pre-2026-10-17 places (~/.lol/journal -> ~/.local/share/lol,
;;;;           ~/.lol/desk -> ~/.local/state/lol, ~/.lol/settings with a font size ->
;;;;           ~/.config/lol/settings), synced, and QEMU killed.
;;;;   boot 5  MIGRATION (lol-home.lisp): every one moved back into ~/.lol, the old
;;;;           folders gone, each move in the session log; the font size kept, the
;;;;           journal replayed from its new place.
;;;;   boot 6  DISKLESS: persistence off, an Accept writes nothing under /.lol or /.local.
;;;;
;;;; doc/session-persistence.org §5. All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))

(in-package :lol.guest)

(defun row (label value)
  (format t "~&  ~44a ~a~%" label value) (finish-output) value)

(defun yes (form &optional (seconds 3.0)) (string= (probe form seconds) "T"))

(defparameter *scratch*
  (format nil "~a/lol-persist-~d/"
          (string-right-trim "/" (or (sb-posix:getenv "LOL_PERSIST_DIR") "/tmp")) (sb-posix:getpid)))
(defparameter *copy* (concatenate 'string *scratch* "disk.img"))
(defparameter *root-path* (string-right-trim "/" (namestring (truename *root*))))
(defparameter *fixture* (concatenate 'string *root-path* "/initramfs/examples.lisp"))
(defparameter *user-home* "/home/everton")      ; make-test-disk.sh: /home/<build user>
(defparameter *debugfs* "/sbin/debugfs")

(defun sh (program &rest args)
  "Run PROGRAM, return its stdout (stderr merged) as a string."
  (with-output-to-string (o)
    (sb-ext:run-program program args :search t :output o :error o :wait t)))

(defun debugfs (&rest commands)
  "Run COMMANDS in one writable debugfs session on the copy."
  (let ((f (concatenate 'string *scratch* "debugfs.cmd")))
    (with-open-file (o f :direction :output :if-exists :supersede)
      (format o "~{~a~%~}" commands))
    (sh *debugfs* "-w" "-f" f *copy*)))

(defun plant-fixture ()
  "Make the fixture's folders (each owned by 1000) and write examples.lisp into them."
  (let* ((rel (subseq *fixture* (length *user-home*)))       ; /lisp-over-linux/…/examples.lisp
         (parts (loop with start = 1 for pos = (position #\/ rel :start start)
                      collect (subseq rel start pos) while pos do (setf start (1+ pos))))
         (cmds '()) (dir *user-home*))
    (dolist (p (butlast parts))
      (setf dir (concatenate 'string dir "/" p))
      (push (format nil "mkdir ~a" dir) cmds)
      (push (format nil "sif ~a uid 1000" dir) cmds)
      (push (format nil "sif ~a gid 1000" dir) cmds))
    (push (format nil "cd ~a" dir) cmds)
    (push (format nil "write ~a ~a" *fixture* (car (last parts))) cmds)
    (push (format nil "sif ~a uid 1000" (car (last parts))) cmds)
    (push (format nil "sif ~a gid 1000" (car (last parts))) cmds)
    (debugfs-list (reverse cmds))))

(defun debugfs-list (cmds) (apply #'debugfs cmds))

(defun owner-of (path)
  (let* ((out (sh *debugfs* "-R" (format nil "stat ~a" path) *copy*))
         (i (search "User:" out)))
    (and i (parse-integer out :start (+ i 5) :junk-allowed t))))

(defun dump-file (path)
  (let ((out (concatenate 'string *scratch* "dumped")))
    (ignore-errors (delete-file out))
    (sh *debugfs* "-R" (format nil "dump ~a ~a" path out) *copy*)
    (with-open-file (s out :external-format :utf-8)
      (let ((buf (make-string (file-length s)))) (subseq buf 0 (read-sequence buf s))))))

(defun slurp (path)
  (with-open-file (s path :external-format :utf-8)
    (let ((buf (make-string (file-length s)))) (subseq buf 0 (read-sequence buf s)))))

(defun boot (n)
  (format t "~&~%==== boot ~d ====~%" n) (finish-output)
  (preflight)
  (boot-guest "persist")
  (qmp-connect)
  (repl-connect)
  (wait-for-browser 120))

(defvar *window-bounds* nil "The window's bounds as boot 1 left them (a printed list).")

(defun journal-count () (probe-int "(length (read-journal))" 6.0))

(defun boot-1 ()
  (boot 1)
  (check "boot 1: persistence is on (the copy's /home, rw)" (yes "(persistence-available-p)"))
  (row "replay at boot" (probe "(replay-summary)"))
  (check "boot 1: Accept a new definition"
         (string= "ZZ-PERSIST" (row "accept zz-persist" (probe "(accept-source \"(defun zz-persist () 42)\")" 30.0))))
  (check "boot 1: Accept an edit to a lol.* function, in its package"
         (search "LOL.EXAMPLES"
                  (row "edit make-counter"
                       (probe "(package-name (symbol-package (accept-source (replace-once \"(* step bump)\" \"(* step bump 1)\" (source-of 'lol.examples::make-counter :function)) 'lol.examples::make-counter :function)))" 30.0))))
  (check "boot 1: two records in the journal" (eql 2 (row "journal records" (journal-count))))
  (qmp-type "zzscratch") (sleep 1)
  (check "boot 1: typed into the Workspace"
         (yes "(and (search \"zzscratch\" (lol.textview:tv-text (pane-tv (first (first *trails*))))) t)"))
  (qmp-hold "meta_l" "m") (sleep 3)
  (check "boot 1: SuperL-m opened a second tab" (eql 2 (row "tabs" (probe-int "(length *trails*)"))))
  ;; a window (doc/morphic.org): the module index torn off and moved three cells right
  (qmp-hold "meta_l" "g") (sleep 1.5)
  (dotimes (i 3) (qmp-key "right") (sleep 0.4))
  (qmp-key "esc") (sleep 0.8)
  (setf *window-bounds* (row "the window's bounds" (probe "(lol.morph:morph-bounds (first (desk-windows)))")))
  (check "boot 1: SuperL-g tore the module index off into a window"
         (yes "(and (= 1 (length (desk-windows))) (typep (pane-subject (first (window-trail (first (desk-windows))))) 'subj-modules))"))
  (probe "(progn (save-setting :desk-clipboard 3) (lol.clipboard:clip-copy \"zzclip\") t)")
  ;; statuses, not FILE-OUT-SUMMARY: that names ~/.lol/…, and the harness refuses
  ;; any answer containing a tilde (it could be the echo of a FORMAT string)
  (let ((fo (row "file out" (probe "(sort (mapcar (lambda (r) (getf r :status)) (file-out-changes)) #'string<)" 30.0))))
    (check "boot 1: file-out folded the edit, left the new one unfolded"
           (string= fo "(:FOLDED :UNFOLDED)")))
  (qmp-hold "meta_l" "q") (sleep 2)
  (qmp-key "2")                                        ; Save and shut down
  (check "boot 1: Save and shut down powered the guest off" (wait-guest-exit 180)))

(defun between ()
  (format t "~&~%==== between boots (debugfs on the copy) ====~%")
  (check "the journal is owned by uid 1000"
         (eql 1000 (row "owner of changes.lisp" (owner-of (format nil "~a/.lol/journal/changes.lisp" *user-home*)))))
  (check "the desk is owned by uid 1000"
         (eql 1000 (row "owner of desk.lisp" (owner-of (format nil "~a/.lol/desk/desk.lisp" *user-home*)))))
  (check "the file system is clean (release-disk ran after the saves)"
         (search "clean" (row "dumpe2fs state"
                              (let ((o (sh "/sbin/dumpe2fs" "-h" *copy*)))
                                (let ((i (search "Filesystem state:" o)))
                                  (and i (string-trim " " (subseq o (+ i 17) (position #\Newline o :start i)))))))))
  (let* ((orig (slurp *fixture*))
         (want (let ((p (search "(* step bump)" orig)))
                 (concatenate 'string (subseq orig 0 p) "(* step bump 1)" (subseq orig (+ p 13)))))
         (got (dump-file *fixture*)))
    (check "file-out changed the fixture in exactly that one place" (string= got want))
    (row "fixture bytes before / after" (format nil "~d / ~d" (length orig) (length got)))))

(defun boot-2 ()
  (boot 2)
  (row "replay at boot" (probe "(replay-summary)"))
  (check "boot 2: two records replayed" (eql 2 (probe-int "(count :replayed (getf *replay-report* :entries) :key #'re-status)")))
  (check "boot 2: (zz-persist) => 42" (eql 42 (row "(zz-persist)" (probe-int "(zz-persist)"))))
  (check "boot 2: the lol.* edit is live in its package"
         (yes "(and (search \"(* step bump 1)\" (source-of 'lol.examples::make-counter :function)) (not (find-symbol \"MAKE-COUNTER\" :cl-user)) t)"))
  (check "boot 2: the change set lists both"
         (yes "(and (find 'zz-persist (changed-definitions) :key #'first) (find 'lol.examples::make-counter (changed-definitions) :key #'first) t)"))
  (check "boot 2: two tabs, the modules one active"
         (string= "(2 1)" (row "tabs / active" (probe "(list (length *trails*) *active*)"))))
  (check "boot 2: the module index is a window again, where it was"
         (and *window-bounds*
              (string= *window-bounds*
                       (row "the window's bounds" (probe "(and (= 1 (length (desk-windows))) (typep (pane-subject (first (window-trail (first (desk-windows))))) 'subj-modules) (lol.morph:morph-bounds (first (desk-windows))))")))))
  (check "boot 2: the Workspace text is back"
         (yes "(and (search \"zzscratch\" (lol.textview:tv-text (pane-tv (first (first *trails*))))) t)"))
  (check "boot 2: the clipboard is back (it was asked for)"
         (yes "(and (member \"zzclip\" (lol.clipboard:clip-history) :test #'string=) t)"))
  ;; the crash case: an Accept, a torn half-record, and no shutdown at all
  (probe "(accept-source \"(defun zz-crash () 7)\")" 30.0)
  (probe "(progn (append-synced (journal-path) \"(:seq 999 :package \\\"COMMON-LISP-USER\\\" :source \\\"(defun zz-torn\") t)")
  (check "boot 2: four journal lines read as 3 records + a stop"
         (string= "(3 T)" (row "records / stopped" (probe "(multiple-value-bind (r s) (read-journal) (list (length r) (and s t)))"))))
  (kill-guest)
  (wait-guest-exit 30))

(defun boot-3 ()
  (boot 3)
  (row "replay at boot" (probe "(replay-summary)"))
  (check "boot 3: the Accept made just before the kill survived (fsync)"
         (eql 7 (row "(zz-crash)" (probe-int "(zz-crash)"))))
  (check "boot 3: the torn tail was reported and moved aside"
         (yes "(and (getf *replay-report* :stop) (getf *replay-report* :quarantined) t)"))
  (check "boot 3: the journal reads clean again" (yes "(null (nth-value 1 (read-journal)))"))
  (check "boot 3: the journal view opened by itself"
         (yes "(and (find-if (lambda (tr) (subj-journal-p (pane-subject (first tr)))) *trails*) t)"))
  (probe "(progn (append-synced (persist-path :journal \"replay-off\") \"\") t)")
  (qmp-hold "meta_l" "q") (sleep 2)
  (qmp-key "3")                                        ; Shut down without saving
  (check "boot 3: Shut down without saving powered off" (wait-guest-exit 180)))

(defun boot-4 ()
  (boot 4)
  (check "boot 4: replay-off was honoured" (yes "(and (getf *replay-report* :disabled) t)"))
  (check "boot 4: nothing replayed" (yes "(not (fboundp 'zz-persist))"))
  (check "boot 4: no desk restored: the Workspace alone" (eql 1 (probe-int "(length *trails*)")))
  ;; the OLD layout, made by hand: what a laptop that ran an image from before
  ;; 2026-10-17 has on its disk. Renamed with the guest's own rename(2), then sync(2),
  ;; because QEMU is killed next and the guest's page cache would die with it.
  (probe "(progn (delete-file (persist-path :journal \"replay-off\")) (save-setting :font-size 24) t)")
  (check "boot 4: the old layout recreated (journal, desk, settings moved back)"
         (string= "(T T T)"
                  (row "renames"
                       (probe "(let ((h (sb-ext:posix-getenv \"HOME\"))) (flet ((p (s) (concatenate 'string h \"/\" s))) (ensure-directories-exist (p \".local/share/\")) (ensure-directories-exist (p \".local/state/\")) (ensure-directories-exist (p \".config/lol/\")) (prog1 (list (and (sb-unix:unix-rename (p \".lol/journal\") (p \".local/share/lol\")) t) (and (sb-unix:unix-rename (p \".lol/desk\") (p \".local/state/lol\")) t) (and (sb-unix:unix-rename (p \".lol/settings\") (p \".config/lol/settings\")) t)) (sb-alien:alien-funcall (sb-alien:extern-alien \"sync\" (function sb-alien:void))))))" 10.0))))
  (kill-guest) (wait-guest-exit 30))

(defun there-p (rel)
  "A probe form: does HOME/REL exist in the guest?"
  (format nil "(and (probe-file (concatenate 'string (sb-ext:posix-getenv \"HOME\") \"/~a\")) t)" rel))

(defun boot-migrate ()
  (boot 5)
  (check "boot 5: the journal moved to HOME/.lol/journal" (yes (there-p ".lol/journal/changes.lisp")))
  (check "boot 5: the desk moved to HOME/.lol/desk" (yes (there-p ".lol/desk/desk.lisp")))
  (check "boot 5: the settings moved to HOME/.lol/settings" (yes (there-p ".lol/settings")))
  (check "boot 5: the old folders are gone"
         (yes (format nil "(not (or ~a ~a ~a))"
                      (there-p ".local/share/lol/") (there-p ".local/state/lol/") (there-p ".config/lol/"))))
  (check "boot 5: each move is in the session log"
         (eql 3 (row "lol-home lines"
                     (probe-int "(with-open-file (s (log-path \"session\")) (loop for l = (read-line s nil) while l count (search \"lol-home: moved\" l)))"))))
  (check "boot 5: the font size was kept (24 px)"
         (eql 24 (row "font size" (probe-int "(lol.canvas:font-size *bfont*)"))))
  (row "replay at boot" (probe "(replay-summary)"))
  (check "boot 5: the journal replayed from its new place"
         (and (eql 42 (row "(zz-persist)" (probe-int "(zz-persist)")))
              (eql 7 (row "(zz-crash)" (probe-int "(zz-crash)")))))
  (kill-guest) (wait-guest-exit 30))

(defun boot-6 ()
  (setf *disk-copy* nil)
  (boot 6)
  (check "boot 6 (diskless): persistence is off" (yes "(not (persistence-available-p))"))
  (probe "(accept-source \"(defun zz-diskless () 1)\")" 30.0)
  (check "boot 6: nothing written under /.lol or /.local"
         (yes "(not (or (probe-file \"/.local/\") (probe-file \"/.lol/journal/\")))"))
  (kill-guest) (wait-guest-exit 30))

(defun main ()
  (let ((ok nil))
    (unwind-protect
         (setf ok
               (handler-case
                   (progn
                     (ensure-directories-exist *scratch*)
                     (format t "~&copying test-disk.img to ~a~%" *copy*)
                     (sh "cp" "--sparse=always" (namestring (merge-pathnames "test-disk.img" *root*)) *copy*)
                     (setf *disk-copy* *copy*)
                     (plant-fixture)
                     (check "the fixture is planted, owned by 1000" (eql 1000 (owner-of *fixture*)))
                     (boot-1) (between) (boot-2) (boot-3) (boot-4) (boot-migrate) (boot-6)
                     (report-results))
                 (serious-condition (c)
                   (format *error-output* "~&persist: ~a~%" c)
                   (report-results)
                   nil)))
      (kill-guest)
      (sh "rm" "-rf" *scratch*))
    (sb-ext:exit :code (if ok 0 1))))

(main)
