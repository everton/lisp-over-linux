;;;; journal.lisp — the change journal: every Accept kept on the disk, replayed at boot.
;;;;
;;;;   ~/.local/share/lol/changes.lisp      one printed plist per Accept, appended + fsync'd
;;;;
;;;; The image is a frozen heap in a RAM-only initramfs, so until this file existed a
;;;; definition you Accepted (C-c C-c) lived exactly as long as the machine stayed on.
;;;; Smalltalk solved this forty years ago with its .changes file: every accepted
;;;; method is appended to a log THE MOMENT it is accepted, so a crash or a pulled plug
;;;; loses nothing that was accepted, and the log is replayed to recover. This is that
;;;; log, for this image (doc/session-persistence.org §2b — the design, the choices
;;;; and the reasons; this header only says what the code does).
;;;;
;;;; WRITE side.  ACCEPT-SOURCE (registry.lisp) runs *ACCEPT-HOOKS* after every
;;;; successful Accept; JOURNAL-ACCEPT, below, is one of them. It appends ONE record:
;;;;
;;;;   (:seq 12 :time 3968650123 :build "a2ed0be 2026-10-16T10:47Z"
;;;;    :package "LOL.TEXTVIEW" :name "TV-DRAW" :kind :function :label nil
;;;;    :source "(defun tv-draw (tv canvas) …)")
;;;;
;;;; Names and source are STRINGS, so reading a record interns nothing: a record that
;;;; names a package this image no longer has still READS, and fails alone at replay.
;;;; The file is opened O_APPEND and fsync'd before the Accept returns, which is the
;;;; whole crash guarantee — fsync(2) is the system call that does not return until
;;;; the bytes are on the device, not merely in the kernel's page cache.
;;;;
;;;; READ side.  BOOT-REPLAY-JOURNAL runs once, from SUPERVISOR-MAIN, before the
;;;; environment opens. Each record is replayed through ACCEPT-SOURCE in its own
;;;; HANDLER-CASE with a timeout, so the rule is: a bad entry is SKIPPED AND REPORTED,
;;;; never fatal — PID 1 must not die because of a line in a file. The outcomes:
;;;;
;;;;   :replayed       compiled into the image again (version history rebuilt too)
;;;;   :retired        its text is ALREADY the image's source — it was folded into the
;;;;                   repo and rebuilt (file-out.lisp), so it removes itself
;;;;   :pending        written by a DIFFERENT build: the code around it may have moved,
;;;;                   so it waits in the journal view to be Accepted or dropped by hand
;;;;   :needs-rebuild  a DEFSTRUCT: redefining a struct breaks every live instance
;;;;                   (§2b, "Structs are the sharp edge"), so it is never replayed
;;;;   :failed         it would not read, or not compile, or ran past the timeout
;;;;
;;;; A torn last record (the power went mid-write) stops the reader; the readable
;;;; prefix is kept, the tail is moved to changes-damaged.lisp beside it, and the
;;;; report says so.
;;;;
;;;; NEVER on a diskless boot. There HOME is "/" — the initramfs, which evaporates —
;;;; and writing there would be the silent failure that looks like success. So
;;;; PERSISTENCE-AVAILABLE-P asks for a real home on a mounted, writable /home, and
;;;; everything here is a no-op without one (the leave menu says "nothing is kept").
;;;;
;;;; OPTING OUT for one boot: lol.replay=off on the kernel command line (one edit in
;;;; GRUB), or a file named replay-off beside the journal (touch it from Debian). The
;;;; journal is then read but nothing in it is run, and the desk is not restored.
;;;;
;;;; Loaded after the shell (it brings the journal VIEW, whose keys need SUBJECT-KEY)
;;;; and before supervisor.lisp, which calls BOOT-REPLAY-JOURNAL.

(in-package :cl-user)

;;; ---- the build stamp ---------------------------------------------------------------
;;; build.sh sets this from git (revision, "+dirty" when the tree had changes) and the
;;; time, just before save-lisp-and-die. A journal record carries the stamp of the
;;; image that wrote it, which is how replay tells "this session's own image" (replay
;;; freely) from "an image built since" (show it, do not run it blind).

(defvar *build-stamp* "unbuilt"
  "Which build this image is: \"<git rev>[+dirty] <UTC time>\", set by build.sh.")

;;; ---- where things live, and whether they may ----------------------------------------

(defvar *persist-home* nil
  "When a directory string, the home to persist under, with the disk checks skipped —
   for the host tests (poc/journal-test.lisp), which must never touch the real $HOME.
   NIL (always, in the image) means: the real home, if there is a disk.")

(defun home-mounted-p ()
  "True when /proc/mounts lists a mount AT /home — the laptop's /home bind-mounted
   by preinit (doc/storage.org §4), or the 9p test mode's overlay. A diskless boot
   has none: its /home is just a directory in the initramfs."
  (ignore-errors
    (with-open-file (in "/proc/mounts")
      (loop for line = (read-line in nil) while line
            thereis (equal (second (split-on-space line)) "/home")))))

(defvar *persistence-cache* nil
  "(INTERNAL-REAL-TIME . ANSWER) of the last real check: the poll loop asks on every
   turn (the desk's autosave deadline), and /proc/mounts is not free to read.")

(defun persistence-available-p ()
  "May this session keep anything? Only with a real home on a mounted, writable
   /home. Diskless: HOME is \"/\" (preinit's main), and the initramfs evaporates.
   After RELEASE-DISK: /home is read-only, and every write would fail. The answer is
   remembered for two seconds."
  (or (and *persist-home* t)
      (let ((now (get-internal-real-time)) (c *persistence-cache*))
        (if (and c (< (- now (car c)) (* 2 internal-time-units-per-second)))
            (cdr c)
            (cdr (setf *persistence-cache*
                       (cons now
                             (let ((home (home-directory)))
                               (and (string/= home "/")
                                    (eql 0 (search "/home/" home))
                                    (eq (followed-type home) :directory)
                                    (home-mounted-p)
                                    (not (home-read-only-p))
                                    t)))))))))

(defun persist-path (place name)
  "The file NAME under the XDG directory for PLACE: :DATA is ~/.local/share/lol
   (worth keeping: the journal, file-out leftovers), :STATE is ~/.local/state/lol
   (reused on restart: the desk). NIL when persistence is off."
  (when (persistence-available-p)
    (path-join (path-join (or *persist-home* (home-directory))
                          (ecase place
                            (:data ".local/share/lol")
                            (:state ".local/state/lol")))
               name)))

(defun ensure-directories-under-home (path)
  "mkdir -p for PATH's folder, one component at a time (MAKE-DIRECTORY is mkdir(2),
   which makes one level). An existing folder is fine."
  (let ((dirs '()))
    (loop for d = (path-parent path) then (path-parent d)
          until (or (null d) (string= d "/") (path-exists-p d))
          do (push d dirs))
    (dolist (d dirs) (make-directory d))))

(defun journal-path () (persist-path :data "changes.lisp"))

;;; ---- writing: append one record, and make it durable ---------------------------------

(defvar *journal-replaying* nil
  "True while replay runs: an Accept that IS the replay must not be journalled again.")

(defvar *journal-seq* 0 "The last sequence number written (or read at boot).")

(defvar *journal-last-error* nil
  "Why the last journal write failed, or NIL — shown in the journal view.")

(defun print-record (plist)
  "PLIST as one READable line, for the journal. WITH-STANDARD-IO-SYNTAX so that what
   is printed is exactly what READ gets back, whatever the REPL's printer settings."
  (with-standard-io-syntax
    (let ((*print-readably* nil) (*package* (find-package :keyword)))
      (format nil "~s~%" plist))))

(defun append-synced (path string &key (mode #o600))
  "Append STRING (as UTF-8) to PATH and fsync before returning. O_APPEND makes the
   kernel put every write at the current end of the file, so a write cannot land in
   the middle of an earlier record. MODE applies when the file is created: 0600,
   because a journal is code you wrote and nobody else's business."
  (ensure-directories-under-home path)
  (let ((octets (sb-ext:string-to-octets string :external-format :utf-8)))
    (multiple-value-bind (fd errno)
        (sb-unix:unix-open path (logior sb-unix:o_wronly sb-unix:o_creat sb-unix:o_append) mode)
      (unless fd (fs-fail "open" path errno))
      (unwind-protect
           (let ((pos 0) (len (length octets)))
             (sb-sys:with-pinned-objects (octets)
               (loop while (< pos len) do
                 (multiple-value-bind (n err) (sb-unix:unix-write fd octets pos (- len pos))
                   (unless (and n (plusp n)) (fs-fail "write" path err))
                   (incf pos n))))
             (when (minusp (%fsync fd)) (fs-fail "fsync" path (sb-alien:get-errno))))
        (sb-unix:unix-close fd))))
  path)

(defun printed-name (name package-name)
  "NAME as text, unqualified when read in PACKAGE-NAME: \"TV-DRAW\", \"(SETF FOO)\"."
  (let ((*package* (or (find-package package-name) (find-package :cl-user))))
    (with-standard-io-syntax
      (let ((*package* (or (find-package package-name) (find-package :cl-user))))
        (prin1-to-string name)))))

(defun journal-accept (name kind label package-name text)
  "The *ACCEPT-HOOKS* entry: append the Accept just made to the journal. Returns a
   note for the status line when something went wrong, else NIL (silence = kept)."
  (cond (*journal-replaying* nil)                ; this IS a replay
        ((null name) nil)                         ; not a definition: nothing to keep
        ((not (persistence-available-p)) nil)     ; diskless: the leave menu says so
        (t
         (handler-case
             (progn
               (append-synced (journal-path)
                              (print-record
                               (list :seq (incf *journal-seq*)
                                     :time (get-universal-time)
                                     :build *build-stamp*
                                     :package package-name
                                     :name (printed-name name package-name)
                                     :kind kind :label label
                                     :source text)))
               (setf *journal-last-error* nil)
               nil)
           (error (c)
             (setf *journal-last-error* (princ-to-string c))
             (format nil "NOT journalled: ~a" c))))))

(pushnew 'journal-accept *accept-hooks*)

;;; ---- reading: every whole record, and where it stopped -------------------------------

(defun record-p (x)
  "Is X shaped like a journal record? (Readable is not enough: a stray symbol is
   readable.) The source and package must be strings — the two replay needs."
  (and (listp x) (ignore-errors (evenp (length x)))
       (stringp (getf x :source)) (stringp (getf x :package))))

(defun read-journal-text (text)
  "Parse TEXT (a journal's contents) -> (values RECORDS STOP), RECORDS in file order.
   STOP is NIL when the whole text read, else (:offset N :reason STRING): the byte…
   the CHARACTER offset where the first unreadable form began. Reading is done with
   *READ-EVAL* off and in the KEYWORD package, so #.(…) runs nothing and a bare word
   is a keyword, never a symbol interned somewhere that matters."
  (let ((records '()) (stop nil))
    (with-input-from-string (s text)
      (let ((*read-eval* nil) (*package* (find-package :keyword)))
        (loop
          (let ((start (file-position s)))
            (multiple-value-bind (form err)
                (handler-case (read s nil :eof)
                  (error (c) (values nil c)))
              (cond (err (setf stop (list :offset start :reason (princ-to-string err)))
                         (return))
                    ((eq form :eof) (return))
                    ((record-p form) (push form records))
                    (t (setf stop (list :offset start :reason "not a journal record"))
                       (return))))))))
    (values (nreverse records) stop)))

(defun slurp-utf-8 (path)
  "PATH's contents as a string (UTF-8; a malformed byte becomes a replacement)."
  (with-open-file (s (native-path path) :external-format '(:utf-8 :replacement #\?))
    (let* ((buf (make-string (file-length s)))
           (n (read-sequence buf s)))
      (subseq buf 0 n))))

(defun read-journal (&optional (path (journal-path)))
  "The journal's records and where reading stopped (see READ-JOURNAL-TEXT), plus the
   whole text as a third value. No file is simply no records."
  (if (and path (path-exists-p path))
      (let ((text (slurp-utf-8 path)))
        (multiple-value-bind (records stop) (read-journal-text text)
          (values records stop text)))
      (values '() nil "")))

(defun write-records (path records)
  "Replace the journal at PATH with RECORDS, atomically (a crash leaves the old file
   or the new, never half): used to retire, drop and quarantine — never on Accept,
   which only ever appends."
  (ensure-directories-under-home path)
  (unless (path-exists-p path) (append-synced path ""))      ; so it is born 0600
  (write-file-atomically
   path (sb-ext:string-to-octets
         (with-output-to-string (o) (dolist (r records) (write-string (print-record r) o)))
         :external-format :utf-8)))

(defun quarantine-tail (path text stop records)
  "The journal at PATH stopped reading at STOP: move everything from there on into
   changes-damaged.lisp (appended, so nothing is ever lost) and rewrite the journal as
   its readable prefix. Without this the NEXT Accept would be appended after the torn
   record and swallowed into it, unreadable forever."
  (let ((tail (subseq text (getf stop :offset))))
    (append-synced (persist-path :data "changes-damaged.lisp")
                   (format nil ";; ~a: cut from changes.lisp at character ~d (~a)~%~a~%"
                           (get-universal-time) (getf stop :offset) (getf stop :reason) tail))
    (write-records path records)))

;;; ---- replay ---------------------------------------------------------------------------

(defstruct (replay-entry (:conc-name re-))
  "What happened to one journal record at replay — a row of the journal view."
  record status message)

(defvar *replay-report* nil
  "This boot's replay, as a plist: :ENTRIES (REPLAY-ENTRY list, journal order),
   :STOP (where reading stopped, or NIL), :DISABLED (why replay was off, or NIL),
   :QUARANTINED (true when a torn tail was moved aside), :RAN (true once replay ran).")

(defparameter +replay-timeout+ 30
  "Seconds one entry may take before it is abandoned (a DEFPARAMETER whose init form
   loops, say). Generous because QEMU without KVM compiles slowly.")

(defun replay-disabled-p ()
  "Why replay is off for this boot, or NIL: lol.replay=off on the kernel command line
   (one edit at the GRUB prompt) or a file named replay-off beside the journal."
  (let ((cmdline (ignore-errors
                  (with-open-file (s "/proc/cmdline") (read-line s nil "")))))
    (cond ((and cmdline (search "lol.replay=off" cmdline)) "lol.replay=off on the command line")
          ((let ((f (persist-path :data "replay-off"))) (and f (path-exists-p f)))
           "~/.local/share/lol/replay-off exists")
          (t nil))))

(defun record-defn (form)
  "The registry's DEFN that FORM (re)defines, or NIL — matched by name and kind, and
   for a method by its specializer label, exactly as ACCEPT-SOURCE matches."
  (multiple-value-bind (name kind label) (defn-name-and-kind form)
    (when name
      (find-if (lambda (d) (and (eq (defn-kind d) kind)
                                (or (not (eq kind :method)) (equal (defn-label d) label))))
               (gethash name *registry*)))))

(defun classify-record (record)
  "Before running anything: what should replay do with RECORD? -> (values STATUS
   MESSAGE FORM PACKAGE). Reads the source (no evaluation; *READ-EVAL* off) only to
   learn which definition it is."
  (let ((pkg (find-package (getf record :package))))
    (unless pkg
      (return-from classify-record
        (values :failed (format nil "no package ~a in this image" (getf record :package)))))
    (multiple-value-bind (form err)
        (handler-case (let ((*package* pkg) (*read-eval* nil))
                        (values (read-from-string (getf record :source)) nil))
          (error (c) (values nil c)))
      (cond
        (err (values :failed (format nil "unreadable: ~a" err)))
        ((null (defn-name-and-kind form))
         (values :failed "not a definition the registry tracks"))
        (t
         (let ((d (record-defn form)))
           (cond
             ((and d (string= (defn-source d) (getf record :source)))
              (values :retired "already the image's own source" form pkg))
             ((eq (nth-value 1 (defn-name-and-kind form)) :struct)
              (values :needs-rebuild
                      "a DEFSTRUCT: replaying it would break every live instance - fold it in and rebuild"
                      form pkg))
             ((not (equal (getf record :build) *build-stamp*))
              (values :pending (format nil "written by build ~a" (getf record :build)) form pkg))
             (t (values :replay nil form pkg)))))))))

(defun run-record (record pkg)
  "Accept RECORD's source again, in PKG, guarded: -> (values OK MESSAGE). Warnings
   (a redefinition, a style note) are muffled — they would print over the boot."
  (handler-case
      (handler-bind ((warning #'muffle-warning))
        (let ((*package* pkg) (*journal-replaying* t))
          (sb-ext:with-timeout +replay-timeout+
            (accept-source (getf record :source))))
        (values t nil))
    (sb-ext:timeout () (values nil (format nil "took longer than ~d s" +replay-timeout+)))
    (serious-condition (c) (values nil (princ-to-string c)))))

(defun replay-records (records)
  "Replay RECORDS in journal order -> a list of REPLAY-ENTRY. Order matters and is
   the journal's: a DEFCLASS before the DEFMETHOD that specializes on it, a DEFMACRO
   before its callers. Each record is classified AFTER the ones before it ran, so
   the same-text test sees the image as the earlier records left it."
  (loop for r in records
        collect (multiple-value-bind (status message form pkg) (classify-record r)
                  (declare (ignore form))
                  (if (eq status :replay)
                      (multiple-value-bind (ok why) (run-record r pkg)
                        (make-replay-entry :record r :status (if ok :replayed :failed)
                                           :message why))
                      (make-replay-entry :record r :status status :message message)))))

(defun max-seq (records)
  (reduce #'max records :key (lambda (r) (let ((s (getf r :seq))) (if (integerp s) s 0)))
                        :initial-value 0))

(defun replay-journal ()
  "Read the journal, quarantine a torn tail, replay what may be replayed, retire what
   is already in the build, and keep the outcome in *REPLAY-REPORT*. Never signals."
  (handler-case
      (let ((path (journal-path)))
        (if (null path)
            (setf *replay-report* (list :ran t :entries '() :nodisk t))
            (multiple-value-bind (records stop text) (read-journal path)
              (setf *journal-seq* (max *journal-seq* (max-seq records)))
              (let ((quarantined (and stop (ignore-errors
                                            (quarantine-tail path text stop records) t)))
                    (disabled (replay-disabled-p)))
                (if disabled
                    (setf *replay-report*
                          (list :ran t :disabled disabled :stop stop :quarantined quarantined
                                :entries (mapcar (lambda (r) (make-replay-entry
                                                              :record r :status :skipped
                                                              :message "replay off for this boot"))
                                                 records)))
                    (let ((entries (replay-records records)))
                      ;; a retired record has done its job: the build carries it now
                      (when (find :retired entries :key #'re-status)
                        (ignore-errors
                          (write-records path (mapcar #'re-record
                                                      (remove :retired entries :key #'re-status)))))
                      (setf *replay-report* (list :ran t :entries entries :stop stop
                                                  :quarantined quarantined))))))))
    (serious-condition (c)
      (setf *replay-report* (list :ran t :entries '() :error (princ-to-string c))))))

(defun replay-counts (&optional (report *replay-report*))
  "An alist (STATUS . COUNT) for REPORT's entries, in a fixed order."
  (loop for s in '(:replayed :failed :pending :needs-rebuild :retired :skipped)
        for n = (count s (getf report :entries) :key #'re-status)
        when (plusp n) collect (cons s n)))

(defun replay-summary (&optional (report *replay-report*))
  "One line saying what replay did, e.g. \"journal: 14 replayed, 1 failed\"."
  (let ((counts (replay-counts report)))
    (format nil "journal: ~:[nothing to replay~;~:*~{~a~^, ~}~]~@[; ~a~]~@[; read stopped at character ~d~]"
            (mapcar (lambda (c) (format nil "~d ~(~a~)" (cdr c) (car c))) counts)
            (getf report :disabled)
            (getf (getf report :stop) :offset))))

(defun journal-needs-attention-p (&optional (report *replay-report*))
  "Should the environment open the journal view at boot? When something did not go
   through by itself: a failure, a pending or struct entry, or a torn tail."
  (or (getf report :stop) (getf report :error)
      (some (lambda (e) (member (re-status e) '(:failed :pending :needs-rebuild)))
            (getf report :entries))))

(defun boot-replay-journal ()
  "Called once by SUPERVISOR-MAIN before the environment opens. Prints one line on
   the console either way, so a boot log shows what happened."
  (unless (getf *replay-report* :ran)
    (replay-journal)
    (cond ((getf *replay-report* :nodisk)
           (format t "~&journal: no disk - this session keeps nothing.~%"))
          (t (format t "~&~a~%" (replay-summary))))
    ;; and into session.log (session-log.lisp), so a skipped entry is on the disk
    ;; for Debian to read even when nobody looked at the screen
    (session-log "~a" (replay-summary))
    (dolist (e (getf *replay-report* :entries))
      (when (member (re-status e) '(:failed :pending :needs-rebuild))
        (session-log "journal #~a ~a ~(~a~): ~a" (getf (re-record e) :seq)
                     (entry-label (re-record e)) (re-status e) (re-message e))))
    (when (getf *replay-report* :stop)
      (session-log "journal read stopped at character ~d: ~a"
                   (getf (getf *replay-report* :stop) :offset)
                   (getf (getf *replay-report* :stop) :reason)))
    (finish-output)))

;;; ---- acting on entries later: Accept a pending one, drop one, discard all ------------

(defun same-record-p (a b)
  (and (eql (getf a :seq) (getf b :seq)) (equal (getf a :source) (getf b :source))))

(defun journal-remove-record (record)
  "Rewrite the journal without RECORD (re-read from the disk first, so Accepts made
   since boot are kept). T when it was there."
  (let ((path (journal-path)))
    (when path
      (let ((records (read-journal path)))
        (when (find record records :test #'same-record-p)
          (write-records path (remove record records :test #'same-record-p))
          t)))))

(defun replay-entry-for (record)
  (find record (getf *replay-report* :entries) :key #'re-record :test #'same-record-p))

(defun journal-accept-entry (entry)
  "Accept a pending (or failed) ENTRY's source now, by hand. On success the Accept
   journals itself afresh — stamped with THIS build — and the old record is dropped,
   so it stops being pending. Returns the status line."
  (let* ((r (re-record entry))
         (pkg (find-package (getf r :package))))
    (if (null pkg)
        (format nil "no package ~a" (getf r :package))
        (handler-case
            (let ((n (let ((*package* pkg)) (accept-source (getf r :source)))))
              (journal-remove-record r)
              (setf (re-status entry) :replayed (re-message entry) "accepted by hand")
              (format nil "Accepted: ~(~a~)~@[  - ~a~]" n *accept-note*))
          (serious-condition (c)
            (setf (re-message entry) (princ-to-string c))
            (format nil "Accept failed: ~a" c))))))

(defun journal-drop-entry (entry)
  "Remove ENTRY's record from the journal: it will not be offered again."
  (if (journal-remove-record (re-record entry))
      (progn (setf (re-status entry) :dropped (re-message entry) "dropped from the journal")
             "dropped from the journal")
      "not in the journal any more"))

(defun discard-journal ()
  "Forget every journalled change — by MOVING the journal aside to
   changes-discarded-<time>.lisp, not deleting it: Smalltalk's 'quit without saving'
   still leaves .changes behind, and so does this. The running image keeps the
   definitions until power-off; the next boot simply does not replay them."
  (let ((path (journal-path)))
    (cond ((null path) "no disk - there is no journal")
          ((not (path-exists-p path)) "the journal is already empty")
          (t (rename-path path (persist-path :data (format nil "changes-discarded-~d.lisp"
                                                           (get-universal-time))))
             (setf *journal-seq* 0)
             "journal moved aside (changes-discarded-*.lisp); the next boot replays nothing"))))

;;; ---- the journal as a subject ----------------------------------------------------------

(defstruct (subj-journal (:constructor subj-journal ())))
(defstruct (subj-journal-entry (:constructor subj-journal-entry (entry))) entry)

(defun entry-label (record)
  (string-downcase (or (getf record :name) "?")))

(defparameter +replay-groups+
  '((:failed "failed - skipped, the boot went on")
    (:pending "pending - from another build: a Accept, d drop")
    (:needs-rebuild "needs a rebuild - a struct is never replayed")
    (:replayed "replayed")
    (:skipped "not replayed (replay off for this boot)")
    (:dropped "dropped")
    (:retired "retired - already in the build")))

(defun journal-entries-now ()
  "This boot's replay entries, plus a row for every record journalled SINCE boot
   (the Accepts of this session), so the view is the whole journal."
  (let* ((boot (getf *replay-report* :entries))
         (now (ignore-errors (read-journal)))
         (new (remove-if (lambda (r) (find r boot :key #'re-record :test #'same-record-p)) now)))
    (append boot (mapcar (lambda (r) (make-replay-entry :record r :status :session
                                                        :message "accepted this session"))
                         new))))

(defmethod present ((s subj-journal))
  (let* ((entries (journal-entries-now))
         (report *replay-report*)
         (path (journal-path)))
    (make-view
     :title "journal - every Accept, kept on the disk and replayed at boot"
     :kind :list :subject s
     :items
     (append
      (list (make-item :label (if path (abbreviate-home path) "no disk - nothing is kept")
                       :detail (if path (replay-summary report)
                                   "a diskless boot writes nothing (doc/session-persistence.org §3)")
                       :kind :package :group "journal"))
      (when (getf report :stop)
        (list (make-item :label (format nil "reading stopped at character ~d" (getf (getf report :stop) :offset))
                         :detail (format nil "~a~:[~; - the tail is in changes-damaged.lisp~]"
                                         (getf (getf report :stop) :reason) (getf report :quarantined))
                         :kind :condition :group "journal")))
      (when *journal-last-error*
        (list (make-item :label "the last write FAILED" :detail *journal-last-error*
                         :kind :condition :group "journal")))
      (loop for (status heading) in (append +replay-groups+ '((:session "accepted this session")))
            append (loop for e in entries
                         when (eq (re-status e) status)
                           collect (make-item :label (entry-label (re-record e))
                                              :detail (format nil "#~a ~(~a~)~@[ - ~a~]"
                                                              (getf (re-record e) :seq)
                                                              (getf (re-record e) :package)
                                                              (re-message e))
                                              :kind (case status
                                                      (:failed :condition) (:pending :restart)
                                                      (:needs-rebuild :struct) (t :function))
                                              :group heading
                                              :subject (subj-journal-entry e))))
      (list (make-item :label "File out to source..." :group "do" :kind :restart
                       :detail "F - write each changed definition back into its .lisp file"
                       :action (lambda () (funcall 'file-out-and-report)))
            (make-item :label "Discard the journal..." :group "do" :kind :restart
                       :detail "D - move it aside; the next boot replays nothing"
                       :action (lambda () (confirm-discard-journal))))))))

(defmethod present ((s subj-journal-entry))
  (let* ((e (subj-journal-entry-entry s)) (r (re-record e)))
    (make-view
     :title (format nil "journal #~a ~a" (getf r :seq) (entry-label r))
     :kind :reference :subject s
     :text (format nil ";; ~(~a~)~@[: ~a~]~%;; package ~a, build ~a, ~a~%~%~a~%"
                   (re-status e) (re-message e) (getf r :package) (getf r :build)
                   (ignore-errors (format-mtime (- (getf r :time) (encode-universal-time 0 0 0 1 1 1970 0))))
                   (getf r :source)))))

(defun confirm-discard-journal ()
  (minibuffer-read "Discard the journal? (type yes) "
                   (lambda (text)
                     (setf *browser-status*
                           (if (string-equal (string-trim " " text) "yes")
                               (discard-journal)
                               "kept the journal"))
                     (refresh-journal-panes))))

(defun refresh-journal-panes ()
  "Re-present every open journal pane (after an Accept, a drop, a discard)."
  (dolist (trail *trails*)
    (dolist (p trail)
      (when (subj-journal-p (pane-subject p))
        (setf (pane-view p) (present (pane-subject p)))))))

(defun selected-journal-entry (pane)
  (let ((it (selected-item pane)))
    (and it (subj-journal-entry-p (item-subject it))
         (subj-journal-entry-entry (item-subject it)))))

(defmethod subject-key ((s subj-journal) pane key state)
  (declare (ignore s state))
  (let ((e (selected-journal-entry pane)))
    (flet ((done (msg) (setf *browser-status* msg) (refresh-journal-panes) t))
      (case key
        (:return (and (run-selected-action pane) t))
        (#\a (if (and e (member (re-status e) '(:pending :failed :needs-rebuild)))
                 (done (journal-accept-entry e))
                 (done "a: Accepts a pending or failed entry")))
        (#\d (if (and e (member (re-status e) '(:pending :failed :needs-rebuild)))
                 (done (journal-drop-entry e))
                 (done "d: drops a pending or failed entry")))
        (#\F (funcall 'file-out-and-report) t)
        (#\D (confirm-discard-journal) t)
        (#\g (done "refreshed"))
        (t nil)))))

(defmethod subject-hints ((s subj-journal) pane)
  (declare (ignore s pane))
  '(("ret" . "open") ("a" . "Accept entry") ("d" . "drop entry") ("F" . "file out")
    ("D" . "discard journal") ("g" . "refresh")))

(defun open-journal ()
  "Focus the journal's tab, or open one."
  (let ((i (position-if (lambda (trail) (subj-journal-p (pane-subject (first trail)))) *trails*)))
    (if i (activate-trail i) (new-trail (subj-journal)))))

(defun journal-spotter-candidates ()
  (list (list :label "journal" :detail "surface · every Accept kept on the disk, the boot's replay"
              :group "surfaces" :kind :package :subject (subj-journal))))

(pushnew 'journal-spotter-candidates *spotter-sources*)

(defun show-journal ()
  "REPL entry point: print the journal view."
  (show-view (subj-journal)))
