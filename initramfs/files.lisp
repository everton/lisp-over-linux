;;;; files.lisp — the file system as plain data: the file manager's MODEL layer.
;;;;
;;;; The machine has no shell and no console, so the browser's file manager is the
;;;; ONLY way to make, rename or delete anything on the user's real disk. This file
;;;; is its headless half, built the way model.lisp was: no pixels, no keys, every
;;;; function REPL-testable on the host against a scratch directory
;;;; (poc/files-test.lisp does exactly that). The views live in file-views.lisp,
;;;; the keys and prompts in poc/file-manager.lisp. See doc/file-manager.org.
;;;;
;;;; How we reach the kernel. Not sb-posix: its entry points are DECLAIM INLINE over
;;;; groveled alien structs, and code that calls them cannot be recompiled inside
;;;; the guest (doc/processes.org §6) — which would quietly cost C-c C-c on every
;;;; definition in this file. So, two layers only:
;;;;
;;;;   - SB-UNIX, SBCL's own internal wrappers, where they exist and are ordinary
;;;;     functions: UNIX-LSTAT / UNIX-STAT (through the runtime's stat_wrapper, which
;;;;     copies the libc struct into a fixed layout, so we never count its offsets),
;;;;     UNIX-READLINK, UNIX-MKDIR, UNIX-RENAME, UNIX-UNLINK, UNIX-OPEN, UNIX-REALPATH.
;;;;   - EXTERN-ALIEN straight to libc for the rest — symlink(2), link(2), rmdir(2),
;;;;     chmod(2), fsync(2), renameat2(2), opendir(3)/readdir(3), statvfs(3) — with
;;;;     only INT / C-STRING / pointer types, which survive save-lisp-and-die and so
;;;;     recompile in the guest (process.lisp's reboot(2) is the precedent).
;;;;
;;;; SB-UNIX's directory readers (UNIX-OPENDIR & co.) are NOT usable: SBCL lists them
;;;; in *!REMOVABLE-SYMBOLS* and the shipped core has dropped them (checked: the
;;;; symbol is not even interned in ./sbcl's core). That is why readdir is ours.
;;;;
;;;; Every operation that changes the disk either succeeds or signals FS-ERROR, a
;;;; condition carrying the operation, the path and the errno. Nothing here prints,
;;;; prompts or swallows: the UI layer catches FS-ERROR and puts its report in the
;;;; status line, so EACCES is a message and never a dead PID 1.
;;;;
;;;; Loaded via load-recording, after sysinfo.lisp (for *UTC-OFFSET*).

;;; ---- errors ----------------------------------------------------------------

(defparameter +errno-names+
  '((1 . "EPERM") (2 . "ENOENT") (5 . "EIO") (9 . "EBADF") (12 . "ENOMEM")
    (13 . "EACCES") (16 . "EBUSY") (17 . "EEXIST") (18 . "EXDEV") (20 . "ENOTDIR")
    (21 . "EISDIR") (22 . "EINVAL") (24 . "EMFILE") (26 . "ETXTBSY") (27 . "EFBIG")
    (28 . "ENOSPC") (30 . "EROFS") (31 . "EMLINK") (36 . "ENAMETOOLONG")
    (38 . "ENOSYS") (39 . "ENOTEMPTY") (40 . "ELOOP") (95 . "EOPNOTSUPP")
    (122 . "EDQUOT"))
  "The errno values a file manager actually meets, by their C names. The kernel
   returns a bare number; strerror(3) gives the sentence, and the C name is what you
   would search for. Both are shown, because each is useless without the other.")

(defun errno-text (errno)
  "\"Permission denied (EACCES)\" for 13 — strerror's sentence plus the C name."
  (if (integerp errno)
      (format nil "~a~@[ (~a)~]"
              (or (ignore-errors (sb-int:strerror errno)) (format nil "errno ~d" errno))
              (cdr (assoc errno +errno-names+)))
      "unknown error"))

(define-condition fs-error (error)
  ((op     :initarg :op     :reader fs-error-op)
   (path   :initarg :path   :reader fs-error-path)
   (errno  :initarg :errno  :initform nil :reader fs-error-errno)
   (detail :initarg :detail :initform nil :reader fs-error-detail))
  (:report (lambda (c s)
             (format s "~a ~a: ~a" (fs-error-op c) (fs-error-path c)
                     (or (fs-error-detail c) (errno-text (fs-error-errno c))))))
  (:documentation "A file-system operation failed. OP is a verb (\"rename\"), PATH
   what it was applied to, ERRNO the kernel's answer, DETAIL a sentence of our own
   when the refusal is ours rather than the kernel's (\"refusing to overwrite\")."))

(defun fs-fail (op path &optional errno detail)
  (error 'fs-error :op op :path path :errno errno :detail detail))

(defun errno-of (code)
  "The errno for a failed syscall: CODE when SB-UNIX already handed it back, else
   the thread's errno — read at once, before anything else can overwrite it."
  (if (integerp code) code (sb-alien:get-errno)))

;;; ---- libc entry points SB-UNIX does not wrap --------------------------------
;;; Each returns the C result (0, or -1 with errno set). Thin on purpose: the
;;; checking lives in the Lisp functions further down, where it can say WHICH
;;; operation on WHICH path failed.

(defmacro define-libc (lisp-name c-name result &rest arg-types)
  "LISP-NAME calls libc's C-NAME through EXTERN-ALIEN. Only basic alien types are
   used (INT, UNSIGNED, C-STRING, SYSTEM-AREA-POINTER), because those are part of
   the core and so a call written with them recompiles inside the guest."
  (let ((args (loop for i below (length arg-types) collect (gensym "A"))))
    `(defun ,lisp-name ,args
       (sb-alien:alien-funcall
        (sb-alien:extern-alien ,c-name (function ,result ,@arg-types))
        ,@args))))

(define-libc %symlink   "symlink"   sb-alien:int sb-alien:c-string sb-alien:c-string)
(define-libc %link      "link"      sb-alien:int sb-alien:c-string sb-alien:c-string)
(define-libc %rmdir     "rmdir"     sb-alien:int sb-alien:c-string)
(define-libc %chmod     "chmod"     sb-alien:int sb-alien:c-string sb-alien:unsigned)
(define-libc %fchmod    "fchmod"    sb-alien:int sb-alien:int sb-alien:unsigned)
(define-libc %fsync     "fsync"     sb-alien:int sb-alien:int)
(define-libc %opendir   "opendir"   sb-alien:system-area-pointer sb-alien:c-string)
(define-libc %readdir   "readdir"   sb-alien:system-area-pointer sb-alien:system-area-pointer)
(define-libc %closedir  "closedir"  sb-alien:int sb-alien:system-area-pointer)
(define-libc %statvfs   "statvfs"   sb-alien:int sb-alien:c-string sb-alien:system-area-pointer)
;; renameat2(olddirfd, oldpath, newdirfd, newpath, flags) — rename(2) with flags.
;; The flag we want is RENAME_NOREPLACE: "fail with EEXIST rather than replace".
(define-libc %renameat2 "renameat2" sb-alien:int sb-alien:int sb-alien:c-string
                                    sb-alien:int sb-alien:c-string sb-alien:unsigned)

(defconstant +at-fdcwd+ -100 "AT_FDCWD: 'relative to the current directory' for the *at() calls.")
(defconstant +rename-noreplace+ 1 "RENAME_NOREPLACE, from linux/fs.h.")

;;; ---- paths: plain strings, never CL pathnames --------------------------------
;;; CL's pathname parser gives * and [ and ? meanings (wildcards), and a file
;;; manager meets file names containing them. So every path here is a plain native
;;; namestring, and the one place we must hand CL a pathname (OPEN) goes through
;;; SB-EXT:PARSE-NATIVE-NAMESTRING, which takes the string literally.

(defun native-path (path) (sb-ext:parse-native-namestring path))

(defun home-directory ()
  "$HOME without a trailing slash, or \"/\". On the guest the preinit sets it to the
   user's home (e.g. /home/everton), bind-mounted from the laptop's disk."
  (let ((h (sb-ext:posix-getenv "HOME")))
    (if (and h (plusp (length h)))
        (if (and (> (length h) 1) (char= (char h (1- (length h))) #\/))
            (string-right-trim "/" h)
            h)
        "/")))

(defun path-join (dir name)
  "DIR/NAME, without doubling the slash when DIR is \"/\"."
  (cond ((zerop (length dir)) name)
        ((char= (char dir (1- (length dir))) #\/) (concatenate 'string dir name))
        (t (concatenate 'string dir "/" name))))

(defun path-name (path)
  "The last component of PATH: \"c\" for \"/a/b/c\", \"/\" for \"/\"."
  (let* ((p (if (and (> (length path) 1) (char= (char path (1- (length path))) #\/))
                (string-right-trim "/" path) path))
         (i (position #\/ p :from-end t)))
    (cond ((string= p "/") "/")
          (i (subseq p (1+ i)))
          (t p))))

(defun path-parent (path)
  "The directory containing PATH: \"/a/b\" for \"/a/b/c\", \"/\" for \"/a\" and \"/\"."
  (let* ((p (string-right-trim "/" path))
         (i (position #\/ p :from-end t)))
    (cond ((or (null i) (zerop i)) "/")
          (t (subseq p 0 i)))))

(defun normalize-path (path)
  "PATH made absolute-looking and tidy WITHOUT touching the disk: repeated slashes
   collapse, \".\" disappears and \"..\" eats the component before it. Lexical on
   purpose — resolving symlinks would turn the folder you walked into (through a
   link) into a different name for it, and `..` would then take you somewhere you
   never were. The shells call this `cd -L`."
  (let ((parts '()))
    (dolist (c (split-path path))
      (cond ((or (string= c "") (string= c ".")))
            ((string= c "..") (pop parts))
            (t (push c parts))))
    (if parts
        (format nil "~{/~a~}" (reverse parts))
        "/")))

(defun split-path (path)
  "PATH split on #\\/ (empty pieces kept, NORMALIZE-PATH drops them)."
  (let ((out '()) (start 0))
    (loop for i = (position #\/ path :start start)
          do (push (subseq path start i) out)
             (if i (setf start (1+ i)) (return)))
    (nreverse out)))

(defun abbreviate-home (path)
  "PATH with $HOME shown as ~ — titles are short, and every path you care about on
   this machine starts with the same fourteen characters."
  (let ((home (home-directory)))
    (cond ((string= home "/") path)
          ((string= path home) "~")
          ((and (> (length path) (length home))
                (string= home path :end2 (length home))
                (char= (char path (length home)) #\/))
           (concatenate 'string "~" (subseq path (length home))))
          (t path))))

(defun hidden-name-p (name)
  "A dot file: hidden by convention (ls(1) hides it without -a). Not \".\"/\"..\" —
   LIST-DIRECTORY never returns those."
  (and (plusp (length name)) (char= (char name 0) #\.)))

(defun file-extension (name)
  "The lowercase extension of NAME, \"\" when it has none. A leading dot is not an
   extension: \".bashrc\" has none, \"notes.txt\" has \"txt\"."
  (let ((i (position #\. name :from-end t)))
    (if (and i (plusp i) (< i (1- (length name))))
        (string-downcase (subseq name (1+ i)))
        "")))

;;; ---- one entry: lstat, and for a link, what it points at --------------------

(defconstant +s-ifmt+   #o170000 "st_mode's file-type bits.")
(defparameter +mode-types+
  '((#o040000 . :directory) (#o100000 . :file) (#o120000 . :symlink)
    (#o010000 . :fifo) (#o140000 . :socket) (#o020000 . :char-device)
    (#o060000 . :block-device))
  "st_mode's S_IFMT field -> our type keyword. One number per kind of inode.")

(defun mode-type (mode)
  (or (cdr (assoc (logand mode +s-ifmt+) +mode-types+)) :unknown))

(defstruct fentry
  "One directory entry, as the file manager sees it. Built by FILE-ENTRY from lstat(2),
   so a symbolic link is described AS a link (TYPE :symlink), and what it points at is
   recorded beside it — TARGET is the link's text, TARGET-TYPE the type stat(2) found
   by following it, BROKEN true when following it failed."
  name path
  type          ; :file :directory :symlink :fifo :socket :char-device :block-device :unknown
  mode          ; the full st_mode (type + permission bits)
  size mtime    ; bytes; mtime as a Unix time (seconds since 1970)
  nlink uid dev ino
  target        ; a symlink's contents (readlink), else NIL
  target-type   ; what a symlink resolves to, or NIL (broken, or not a link)
  target-size
  broken)       ; T for a symlink whose target does not exist (or cannot be reached)

(defun file-entry (path &optional name)
  "An FENTRY for PATH, or signals FS-ERROR (\"lstat\") when PATH cannot be lstat'd."
  (multiple-value-bind (ok dev ino mode nlink uid gid rdev size atime mtime)
      (sb-unix:unix-lstat path)
    (declare (ignore gid rdev atime))
    (unless ok (fs-fail "lstat" path dev))           ; on failure the 2nd value is errno
    (let ((e (make-fentry :name (or name (path-name path)) :path path
                          :type (mode-type mode) :mode mode :size size :mtime mtime
                          :nlink nlink :uid uid :dev dev :ino ino)))
      (when (eq (fentry-type e) :symlink)
        (setf (fentry-target e) (ignore-errors (sb-unix:unix-readlink path)))
        ;; stat (not lstat) FOLLOWS the link, all the way down a chain of them
        (multiple-value-bind (tok tdev tino tmode tnlink tuid tgid trdev tsize)
            (sb-unix:unix-stat path)
          (declare (ignore tdev tino tnlink tuid tgid trdev))
          (if tok
              (setf (fentry-target-type e) (mode-type tmode)
                    (fentry-target-size e) tsize)
              (setf (fentry-broken e) t))))
      e)))

(defun entry-dir-p (e)
  "True for a directory, or a symlink that resolves to one — the things you can
   walk INTO. (Walking into a link does not resolve it: the path keeps the link's
   name, as `cd` does.)"
  (or (eq (fentry-type e) :directory)
      (and (eq (fentry-type e) :symlink) (eq (fentry-target-type e) :directory))))

(defun entry-executable-p (e)
  "A regular file (or a link to one) with any execute bit set."
  (and (member (if (eq (fentry-type e) :symlink) (fentry-target-type e) (fentry-type e))
               '(:file))
       (logtest #o111 (fentry-mode e))))

(defun path-type (path)
  "The type of PATH without following a final symlink, or NIL when it does not exist."
  (multiple-value-bind (ok dev ino mode) (sb-unix:unix-lstat path)
    (declare (ignore dev ino))
    (and ok (mode-type mode))))

(defun path-exists-p (path) (and (path-type path) t))

(defun followed-type (path)
  "The type of whatever PATH finally names, following symlinks; NIL if nothing."
  (multiple-value-bind (ok dev ino mode) (sb-unix:unix-stat path)
    (declare (ignore dev ino))
    (and ok (mode-type mode))))

;;; ---- reading a directory: opendir(3) / readdir(3) by hand -------------------
;;; readdir returns a pointer to a `struct dirent`, and on x86-64 Linux (glibc and
;;; musl agree) that struct is:
;;;
;;;     offset 0   d_ino     u64     the inode number
;;;     offset 8   d_off     i64     an opaque cookie for seekdir
;;;     offset 16  d_reclen  u16     this record's length
;;;     offset 18  d_type    u8      DT_DIR 4, DT_REG 8, DT_LNK 10, ... 0 = unknown
;;;     offset 19  d_name    char[]  the name, NUL-terminated
;;;
;;; So the name is the bytes from offset 19 up to the first zero. D_TYPE comes free
;;; with the name and lets a recursive find skip lstat on every plain file — but a
;;; file system may answer DT_UNKNOWN (0), and then only lstat can tell.

(defconstant +dirent-type-offset+ 18)
(defconstant +dirent-name-offset+ 19)
(defparameter +dirent-types+
  '((1 . :fifo) (2 . :char-device) (4 . :directory) (6 . :block-device)
    (8 . :file) (10 . :symlink) (12 . :socket))
  "d_type -> our type keyword. Missing (0, DT_UNKNOWN) means: ask lstat.")

(defun dirent-name (ent)
  "The d_name of the dirent at SAP ENT, decoded as UTF-8. A name that is not valid
   UTF-8 (Linux allows any bytes but / and NUL) decodes with ? in the bad places —
   shown, but not addressable: an operation on it will report ENOENT."
  (let ((n (loop for i from +dirent-name-offset+
                 until (zerop (sb-sys:sap-ref-8 ent i))
                 count t)))
    (let ((octets (make-array n :element-type '(unsigned-byte 8))))
      (dotimes (i n) (setf (aref octets i) (sb-sys:sap-ref-8 ent (+ +dirent-name-offset+ i))))
      (sb-ext:octets-to-string octets :external-format '(:utf-8 :replacement #\?)))))

(defun map-directory-names (fn dir)
  "Call (FN NAME DTYPE) for every entry of DIR except . and .., DTYPE being a type
   keyword or NIL when the file system did not say. Signals FS-ERROR when DIR cannot
   be opened (EACCES, ENOTDIR, ENOENT). Always closes the directory, even on a
   non-local exit out of FN — which is how a capped walk stops early."
  (let ((d (%opendir dir)))
    (when (zerop (sb-sys:sap-int d)) (fs-fail "opendir" dir (sb-alien:get-errno)))
    (unwind-protect
         (loop
           (let ((ent (%readdir d)))
             (when (zerop (sb-sys:sap-int ent)) (return))
             (let ((name (dirent-name ent)))
               (unless (or (string= name ".") (string= name ".."))
                 (funcall fn name
                          (cdr (assoc (sb-sys:sap-ref-8 ent +dirent-type-offset+)
                                      +dirent-types+)))))))
      (%closedir d))))

(defparameter +listing-cap+ 20000
  "At most this many entries are lstat'd for one listing. A folder of a million files
   would otherwise freeze PID 1 for minutes; past the cap the view says so.")

(defun list-directory (dir &key (limit +listing-cap+))
  "Every entry of DIR as an FENTRY (hidden ones included — hiding is the view's job),
   in the order readdir gave them. Returns (values ENTRIES TRUNCATED-P). An entry
   that vanishes between readdir and lstat is simply skipped."
  (let ((out '()) (n 0) (truncated nil))
    (block walk
      (map-directory-names
       (lambda (name dtype)
         (declare (ignore dtype))
         (when (>= n limit) (setf truncated t) (return-from walk))
         (let ((e (ignore-errors (file-entry (path-join dir name) name))))
           (when e (push e out) (incf n))))
       dir))
    (values (nreverse out) truncated)))

;;; ---- sorting and filtering --------------------------------------------------

(defun natural-lessp (a b)
  "Case-insensitive, digit-aware comparison: file2 < file10, as a person counts. Runs
   of digits compare by value; everything else character by character."
  (let ((i 0) (j 0) (la (length a)) (lb (length b)))
    (loop
      (cond ((>= i la) (return (< j lb)))
            ((>= j lb) (return nil)))
      (let ((ca (char a i)) (cb (char b j)))
        (if (and (digit-char-p ca) (digit-char-p cb))
            (let* ((ea (or (position-if-not #'digit-char-p a :start i) la))
                   (eb (or (position-if-not #'digit-char-p b :start j) lb))
                   (na (parse-integer a :start i :end ea))
                   (nb (parse-integer b :start j :end eb)))
              (cond ((< na nb) (return t))
                    ((> na nb) (return nil))
                    (t (setf i ea j eb))))
            (let ((da (char-downcase ca)) (db (char-downcase cb)))
              (cond ((char< da db) (return t))
                    ((char> da db) (return nil))
                    (t (incf i) (incf j)))))))))

(defparameter +sort-keys+ '(:name :mtime :size :type)
  "The orders a listing can take, in the order the `s` key cycles through them.")

(defun sort-entries (entries &key (by :name) reverse (dirs-first t))
  "ENTRIES sorted, freshly. BY is :name (natural, A-Z), :mtime (NEWEST first), :size
   (LARGEST first) or :type (by extension, then name) — each key's natural reading
   order; REVERSE flips it. Ties fall back to the name, so the order is total and the
   same listing always reads the same. With DIRS-FIRST, folders (and links to them)
   come before everything else whichever way the rest is sorted — reversing a list
   should not bury the folders at the bottom."
  (flet ((key-less (a b)
           (flet ((by-name () (natural-lessp (fentry-name a) (fentry-name b))))
             (ecase by
               (:name (by-name))
               (:mtime (let ((ma (or (fentry-mtime a) 0)) (mb (or (fentry-mtime b) 0)))
                         (if (= ma mb) (by-name) (> ma mb))))
               (:size (let ((sa (or (fentry-size a) 0)) (sb (or (fentry-size b) 0)))
                        (if (= sa sb) (by-name) (> sa sb))))
               (:type (let ((xa (file-extension (fentry-name a)))
                            (xb (file-extension (fentry-name b))))
                        (if (string= xa xb) (by-name) (string< xa xb))))))))
    (stable-sort (copy-list entries)
                 (lambda (a b)
                   (let ((da (and dirs-first (entry-dir-p a)))
                         (db (and dirs-first (entry-dir-p b))))
                     (cond ((and da (not db)) t)
                           ((and db (not da)) nil)
                           (reverse (key-less b a))
                           (t (key-less a b))))))))

(defun glob-match-p (pattern name)
  "Shell-style glob, case-insensitive: * any run, ? any one character. Recursive and
   small, because names are short."
  (labels ((m (pj ni)
             (cond ((= pj (length pattern)) (= ni (length name)))
                   ((char= (char pattern pj) #\*)
                    (loop for k from ni to (length name) thereis (m (1+ pj) k)))
                   ((= ni (length name)) nil)
                   ((or (char= (char pattern pj) #\?)
                        (char-equal (char pattern pj) (char name ni)))
                    (m (1+ pj) (1+ ni)))
                   (t nil))))
    (m 0 0)))

(defun name-matches-p (query name)
  "Does NAME answer QUERY? A query with * or ? is a glob over the WHOLE name
   (\"*.lisp\"); anything else is a case-insensitive SUBSTRING (\"conf\" finds
   nginx.conf and config/). Substring rather than the Spotter's fuzzy subsequence:
   in a folder you are narrowing a list you can see, and a fuzzy match keeps rows
   that visibly do not contain what you typed."
  (cond ((zerop (length query)) t)
        ((find-if (lambda (c) (member c '(#\* #\?))) query) (glob-match-p query name))
        (t (search query name :test #'char-equal))))

(defun filter-entries (entries &key query (hidden nil))
  "ENTRIES that pass the two filters a listing has: dot files only when HIDDEN, and
   only names matching QUERY (NAME-MATCHES-P) when there is one."
  (remove-if-not (lambda (e)
                   (and (or hidden (not (hidden-name-p (fentry-name e))))
                        (or (null query) (name-matches-p query (fentry-name e)))))
                 entries))

;;; ---- recursive find: bounded, so it can never hang PID 1 --------------------

(defun find-files (root query &key (max-results 500) (max-dirs 20000) (max-seconds 3)
                                   hidden (same-device t) interrupt-fn)
  "Every entry under ROOT whose name matches QUERY (NAME-MATCHES-P), breadth-first so
   near results come first. Returns (values RESULTS INFO): RESULTS a list of FENTRYs,
   INFO a plist (:dirs N :results N :stopped REASON) where REASON is NIL when the walk
   finished, else :results / :dirs / :time / :interrupted — the cap that stopped it.

   The caps are the point. PID 1 runs this on the UI thread: a find under / on a
   laptop is millions of entries, and nothing may block that long. So it stops at
   MAX-RESULTS matches, MAX-DIRS directories or MAX-SECONDS, whichever comes first,
   and every 32 directories (from the first) asks INTERRUPT-FN (a function of no arguments the
   backend supplies — the framebuffer's checks for Esc) whether to give up.

   It never descends a SYMLINK (a link to an ancestor is an infinite tree), never a
   hidden directory unless HIDDEN, and with SAME-DEVICE (find -xdev) never crosses
   onto another file system — under / that is what keeps it out of /proc, /sys and
   the host's read-only mount. Unreadable directories are skipped, not fatal."
  (let* ((results '()) (nres 0) (ndirs 0) (stopped nil)
         (deadline (+ (get-internal-real-time)
                      (* max-seconds internal-time-units-per-second)))
         (root-dev (multiple-value-bind (ok dev) (sb-unix:unix-stat root) (and ok dev)))
         (queue (list root)) (tail nil))
    (setf tail queue)
    (flet ((enqueue (dir)
             (let ((cell (list dir)))
               (if queue (setf (cdr tail) cell tail cell) (setf queue cell tail cell)))))
      (block walk
        (loop while queue do
          (let ((dir (pop queue)))
            (when (null queue) (setf tail nil))
            (incf ndirs)
            (cond ((> ndirs max-dirs) (setf stopped :dirs) (return-from walk))
                  ((> (get-internal-real-time) deadline) (setf stopped :time) (return-from walk))
                  ((and interrupt-fn (= 1 (mod ndirs 32)) (ignore-errors (funcall interrupt-fn)))
                   (setf stopped :interrupted) (return-from walk)))
            (ignore-errors                      ; an unreadable folder is skipped
              (map-directory-names
               (lambda (name dtype)
                 (let* ((path (path-join dir name))
                        (type (or dtype (path-type path))))
                   (when (name-matches-p query name)
                     (let ((e (ignore-errors (file-entry path name))))
                       (when e
                         (push e results)
                         (when (>= (incf nres) max-results)
                           (setf stopped :results) (return-from walk)))))
                   (when (and (eq type :directory)
                              (or hidden (not (hidden-name-p name)))
                              (or (not same-device) (null root-dev)
                                  (multiple-value-bind (ok dev) (sb-unix:unix-lstat path)
                                    (and ok (eql dev root-dev)))))
                     (enqueue path))))
               dir))))))
    (values (nreverse results)
            (list :dirs ndirs :results nres :stopped stopped))))

(defun tree-size (path &key (limit 200000))
  "How much is under PATH: (values ENTRIES BYTES COMPLETE-P), ENTRIES not counting PATH
   itself. Symlinks are counted, never followed. Stops after LIMIT entries (then
   COMPLETE-P is NIL) — this is what the delete prompt quotes, and a prompt must not
   take a minute to appear."
  (let ((n 0) (bytes 0) (complete t))
    (labels ((walk (dir)
               (map-directory-names
                (lambda (name dtype)
                  (when (>= n limit) (setf complete nil) (return-from tree-size (values n bytes nil)))
                  (incf n)
                  (let* ((p (path-join dir name))
                         (e (ignore-errors (file-entry p name))))
                    (when e
                      (incf bytes (or (fentry-size e) 0))
                      (when (eq (or dtype (fentry-type e)) :directory)
                        (ignore-errors (walk p))))))
                dir)))
      (when (eq (path-type path) :directory) (walk path)))
    (values n bytes complete)))

;;; ---- changing the disk -------------------------------------------------------
;;; Each signals FS-ERROR on failure and returns the path it made on success.

(defun fs-check (op path result)
  "Turn a C-style RESULT (0 / -1 + errno) into a return or an FS-ERROR."
  (if (and (integerp result) (minusp result))
      (fs-fail op path (sb-alien:get-errno))
      path))

(defun make-directory (path)
  "mkdir PATH. Mode 0777, which the kernel narrows by the umask — exactly what
   mkdir(1) does, so a folder made here looks like one made anywhere else."
  (multiple-value-bind (ok errno) (sb-unix:unix-mkdir path #o777)
    (unless ok (fs-fail "mkdir" path errno)))
  path)

(defun create-file (path)
  "Create PATH as an empty file. O_CREAT|O_EXCL: the kernel refuses (EEXIST) if
   anything is already there, atomically — no check-then-create race."
  (multiple-value-bind (fd errno)
      (sb-unix:unix-open path (logior sb-unix:o_wronly sb-unix:o_creat sb-unix:o_excl) #o666)
    (unless fd (fs-fail "create" path errno))
    (sb-unix:unix-close fd))
  path)

(defun rename-path (from to)
  "Rename FROM to TO, REFUSING to replace an existing TO. Plain rename(2) silently
   replaces a file at TO, which in a file manager is how you lose data with one
   keystroke. renameat2(…, RENAME_NOREPLACE) makes the refusal atomic; where a file
   system does not support the flag (EINVAL) or the kernel predates it (ENOSYS), we
   fall back to an lstat check and then rename(2) — racy in principle, fine here."
  (when (string= from to) (return-from rename-path to))
  (let ((r (ignore-errors (%renameat2 +at-fdcwd+ from +at-fdcwd+ to +rename-noreplace+))))
    (cond ((eql r 0) to)
          ((and (integerp r) (not (member (sb-alien:get-errno) '(22 38))))
           (fs-fail "rename" from (sb-alien:get-errno)))
          (t
           (when (path-exists-p to) (fs-fail "rename" to 17))
           (multiple-value-bind (ok errno) (sb-unix:unix-rename from to)
             (unless ok (fs-fail "rename" from errno)))
           to))))

(defun delete-path (path)
  "Remove PATH: unlink(2) for anything but a directory, rmdir(2) for a directory
   (which must then be empty — ENOTEMPTY otherwise; DELETE-TREE is the explicit
   recursive one). A symlink is removed itself, never what it points at."
  (if (eq (path-type path) :directory)
      (fs-check "rmdir" path (%rmdir path))
      (multiple-value-bind (ok errno) (sb-unix:unix-unlink path)
        (unless ok (fs-fail "unlink" path errno))
        path)))

(defun delete-tree (path)
  "Remove PATH and, if it is a real directory, everything under it, depth first.
   Symlinks inside are unlinked, NEVER followed: a link to / inside the tree deletes
   one link. Stops at the first failure (the FS-ERROR names the entry), so a
   permission problem leaves the rest untouched rather than half-trying everything."
  (when (eq (path-type path) :directory)
    (let ((children '()))
      (map-directory-names (lambda (name dtype) (declare (ignore dtype))
                             (push (path-join path name) children))
                           path)
      (dolist (c children) (delete-tree c))))
  (delete-path path))

(defun make-symlink (target link)
  "Create LINK, a symbolic link whose content is the string TARGET. TARGET is stored
   VERBATIM — a link is text, resolved only when used — so a relative TARGET is
   relative to LINK's folder, not to wherever we happen to be. (FM-SYMLINK picks
   which form to write; see doc/file-manager.org, 'links'.)"
  (fs-check "symlink" link (%symlink target link)))

(defun make-hard-link (existing new)
  "Create NEW as a second name for the inode EXISTING names. Same file system only
   (EXDEV otherwise), never a directory (EPERM): that is the kernel's rule, not ours."
  (fs-check "link" new (%link existing new)))

(defun set-mode (path mode)
  "chmod PATH to MODE's permission bits."
  (fs-check "chmod" path (%chmod path (logand mode #o7777))))

(defun toggle-executable (path)
  "chmod +x / -x, the way a person means it: if any execute bit is set, clear them
   all; otherwise grant execute to exactly those who may read (r-- -> r-x). Returns
   the new mode."
  (let* ((e (file-entry path))
         (m (logand (fentry-mode e) #o7777))
         (new (if (logtest m #o111)
                  (logandc2 m #o111)
                  (logior m (ash (logand m #o444) -2)))))
    (set-mode path new)
    new))

;;; ---- bytes in and out --------------------------------------------------------

(defun read-prefix (path n &key (start 0))
  "At most N octets of the regular file PATH, from byte START. Never more: this is
   what keeps a preview of a 4 GB file a 64 KB read. The caller must have checked
   that PATH is a regular file — opening a FIFO for reading would block forever."
  (with-open-file (s (native-path path) :element-type '(unsigned-byte 8))
    (when (plusp start) (file-position s start))
    (let* ((buf (make-array n :element-type '(unsigned-byte 8)))
           (got (read-sequence buf s)))
      (if (= got n) buf (subseq buf 0 got)))))

(defun octets->text (octets &key truncated)
  "OCTETS as a string when they are TEXT — no NUL byte and valid UTF-8 — else NIL.
   With TRUNCATED (OCTETS is a prefix of a longer file) a multi-byte character cut
   in half at the end is forgiven: up to three trailing bytes are dropped and the
   decode retried, instead of calling the whole file binary because of where we
   stopped reading."
  (when (find 0 octets) (return-from octets->text nil))
  (loop for drop from 0 to (if truncated (min 3 (length octets)) 0)
        do (let ((s (handler-case
                        (sb-ext:octets-to-string octets :external-format :utf-8
                                                        :end (- (length octets) drop))
                      (error () nil))))
             (when s (return s)))))

(defparameter +save-counter+ 0)

(defun write-file-atomically (path octets)
  "Replace PATH's contents with OCTETS so that a crash at ANY moment leaves either
   the old file or the new one — never a truncated one.

     1. resolve PATH through any symlinks (realpath), so we replace the FILE the link
        names and the link itself stays a link;
     2. write OCTETS to a temp file BESIDE it (same folder => same file system, which
        rename(2) requires) and fsync it, so the bytes are on the disk;
     3. give the temp file the original's permission bits (fchmod);
     4. rename(2) the temp file over the original. rename is atomic: every reader
        sees the old inode or the new one.

   What this does not preserve, honestly: other HARD links to the old inode (they
   keep the old contents) and the old owner if it was someone else's file — on this
   machine the browser runs as the user, so it is never someone else's. A NEW file
   (PATH does not exist) gets 0666 less the umask, like any other created file."
  (let* ((real (or (and (path-exists-p path) (ignore-errors (sb-unix:unix-realpath path)))
                   path))
         (old-mode (multiple-value-bind (ok dev ino mode) (sb-unix:unix-stat real)
                     (declare (ignore dev ino))
                     (and ok (logand mode #o7777))))
         (tmp (path-join (path-parent real)
                         (format nil ".~a.lol-save-~d-~d" (path-name real)
                                 (sb-unix:unix-getpid) (incf +save-counter+)))))
    (multiple-value-bind (fd errno)
        (sb-unix:unix-open tmp (logior sb-unix:o_wronly sb-unix:o_creat sb-unix:o_excl) #o666)
      (unless fd (fs-fail "save" tmp errno))
      (let ((ok nil))
        (unwind-protect
             (progn
               (let ((pos 0) (len (length octets)))
                 (sb-sys:with-pinned-objects (octets)
                   (loop while (< pos len) do
                     (multiple-value-bind (n err)
                         (sb-unix:unix-write fd octets pos (- len pos))
                       (unless (and n (plusp n)) (fs-fail "write" tmp err))
                       (incf pos n)))))
               (when (minusp (%fsync fd)) (fs-fail "fsync" tmp (sb-alien:get-errno)))
               (when old-mode
                 (when (minusp (%fchmod fd old-mode)) (fs-fail "fchmod" tmp (sb-alien:get-errno))))
               (setf ok t))
          (sb-unix:unix-close fd)
          (unless ok (ignore-errors (sb-unix:unix-unlink tmp))))))
    (multiple-value-bind (ok errno) (sb-unix:unix-rename tmp real)
      (unless ok
        (ignore-errors (sb-unix:unix-unlink tmp))
        (fs-fail "rename" real errno)))
    real))

(defun copy-file (from to)
  "Copy the regular file FROM to a NEW file TO (EEXIST if TO exists — copying never
   overwrites), 64 KB at a time, keeping FROM's permission bits. The modification
   time is not carried over: the copy is a new file, made now."
  (let ((mode (fentry-mode (file-entry from))))
    (multiple-value-bind (out errno)
        (sb-unix:unix-open to (logior sb-unix:o_wronly sb-unix:o_creat sb-unix:o_excl) #o600)
      (unless out (fs-fail "copy" to errno))
      (let ((ok nil))
        (unwind-protect
             (with-open-file (in (native-path from) :element-type '(unsigned-byte 8))
               (let ((buf (make-array 65536 :element-type '(unsigned-byte 8))))
                 (loop for n = (read-sequence buf in)
                       while (plusp n) do
                         (let ((pos 0))
                           (sb-sys:with-pinned-objects (buf)
                             (loop while (< pos n) do
                               (multiple-value-bind (w err) (sb-unix:unix-write out buf pos (- n pos))
                                 (unless (and w (plusp w)) (fs-fail "write" to err))
                                 (incf pos w)))))))
               (%fchmod out (logand mode #o7777))
               (setf ok t))
          (sb-unix:unix-close out)
          (unless ok (ignore-errors (sb-unix:unix-unlink to)))))))
  to)

(defun copy-recursively (from to)
  "Copy FROM to TO recursively: a directory is re-made and filled, a symlink is
   re-made with the SAME text (so a relative link still points at its sibling in the
   copy), a regular file is COPY-FILE'd. Sockets, FIFOs and devices are skipped and
   counted — copying a device node is not what anyone means. Returns the number
   skipped. Refuses to copy a folder into itself (the copy would contain the copy)."
  (let ((skipped 0)
        (nf (normalize-path from)) (nt (normalize-path to)))
    (when (and (> (length nt) (length nf))
               (string= nf nt :end2 (length nf))
               (char= (char nt (length nf)) #\/))
      (fs-fail "copy" to nil "refusing to copy a folder into itself"))
    (labels ((cp (src dst)
               (case (path-type src)
                 (:directory
                  (make-directory dst)
                  (map-directory-names (lambda (name dtype) (declare (ignore dtype))
                                         (cp (path-join src name) (path-join dst name)))
                                       src))
                 (:symlink (make-symlink (or (sb-unix:unix-readlink src)
                                             (fs-fail "readlink" src (sb-alien:get-errno)))
                                         dst))
                 (:file (copy-file src dst))
                 ((nil) (fs-fail "copy" src 2))
                 (t (incf skipped)))))
      (cp from to))
    skipped))

(defun move-path (from to)
  "Move FROM to TO: a rename when both are on one file system; across file systems
   (EXDEV — rename cannot move an inode between disks) a COPY-RECURSIVELY then a
   DELETE-TREE of the original, which is what mv(1) does too."
  (handler-case (rename-path from to)
    (fs-error (c)
      (if (eql (fs-error-errno c) 18)
          (progn (copy-recursively from to) (delete-tree from) to)
          (error c)))))

;;; ---- the disk, and people ----------------------------------------------------

(defun disk-free (path)
  "(values FREE-BYTES TOTAL-BYTES) of the file system holding PATH, from statvfs(3),
   or NIL. struct statvfs on x86-64 begins with eight 8-byte fields — f_bsize,
   f_frsize, f_blocks, f_bfree, f_bavail, … — and sizes are counted in f_frsize
   units; f_bavail (not f_bfree) is what an unprivileged user may still use."
  (let ((buf (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-sys:with-pinned-objects (buf)
      (when (zerop (%statvfs path (sb-sys:vector-sap buf)))
        (flet ((u64 (off) (sb-sys:sap-ref-64 (sb-sys:vector-sap buf) off)))
          (let ((frsize (u64 8)))
            (values (* frsize (u64 32)) (* frsize (u64 16)))))))))

(defvar *user-names* nil "uid -> name, read once from /etc/passwd (or NIL).")

(defun user-name (uid)
  "The login name for UID from /etc/passwd, else the number as a string. The guest's
   root is the tiny initramfs, so its /etc/passwd may well be absent; the host's is
   read-only at /mnt/host/etc/passwd and is tried second."
  (unless *user-names*
    (setf *user-names* (list :loaded))
    (dolist (f '("/etc/passwd" "/mnt/host/etc/passwd"))
      (ignore-errors
        (with-open-file (s (native-path f))
          (loop for line = (read-line s nil) while line do
            (let* ((p1 (position #\: line)) (p2 (and p1 (position #\: line :start (1+ p1))))
                   (p3 (and p2 (position #\: line :start (1+ p2)))))
              (when p3
                (let ((id (ignore-errors (parse-integer line :start (1+ p2) :end p3))))
                  (when id (push (cons id (subseq line 0 p1)) *user-names*))))))))))
  (or (cdr (assoc uid (rest *user-names*))) (princ-to-string uid)))

;;; ---- formatting --------------------------------------------------------------

(defun human-size (bytes)
  "1536 -> \"1.5K\", 812 -> \"812B\": powers of 1024, one decimal while it matters,
   the way ls -h prints them."
  (cond ((null bytes) "?")
        ((< bytes 1024) (format nil "~dB" bytes))
        (t (let ((units '("K" "M" "G" "T" "P")) (v (/ bytes 1024.0)))
             (loop while (and (>= v 1024) (cdr units))
                   do (setf v (/ v 1024) units (cdr units)))
             (if (< v 10) (format nil "~,1f~a" v (car units))
                 (format nil "~d~a" (round v) (car units)))))))

(defconstant +unix-epoch+ 2208988800 "1970-01-01 as a CL universal time.")

(defun format-mtime (unix-time)
  "A Unix time as local \"2026-10-03 14:02\". Local means *UTC-OFFSET* (sysinfo.lisp;
   the image has no zoneinfo), passed explicitly, as CLOCK-STRING does."
  (if (null unix-time) "?"
      (multiple-value-bind (sec min hour day month year)
          (decode-universal-time (+ unix-time +unix-epoch+)
                                 (- (/ (if (boundp '*utc-offset*) (symbol-value '*utc-offset*) 0)
                                       3600)))
        (declare (ignore sec))
        (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day hour min))))

(defun mode-string (mode)
  "\"drwxr-xr-x\" from st_mode: the type letter, then rwx for owner, group, other,
   with s/t for setuid, setgid and sticky the way ls(1) shows them."
  (let ((s (make-string 10 :initial-element #\-)))
    (setf (char s 0) (case (mode-type mode)
                       (:directory #\d) (:symlink #\l) (:fifo #\p) (:socket #\s)
                       (:char-device #\c) (:block-device #\b) (t #\-)))
    (loop for i from 0 below 9
          for bit = (ash 1 (- 8 i))
          do (when (logtest mode bit) (setf (char s (1+ i)) (char "rwxrwxrwx" i))))
    (when (logtest mode #o4000) (setf (char s 3) (if (logtest mode #o100) #\s #\S)))
    (when (logtest mode #o2000) (setf (char s 6) (if (logtest mode #o010) #\s #\S)))
    (when (logtest mode #o1000) (setf (char s 9) (if (logtest mode #o001) #\t #\T)))
    s))

;;; ---- the Spotter's path grammar (doc/file-manager.org, "The Spotter reads paths")

(defun percent-decode (s)
  "file:///home/me/My%20Notes -> /home/me/My Notes. %XX is one OCTET, so the result
   is decoded as UTF-8 at the end (é arrives as %C3%A9). A malformed escape is kept
   literally rather than refused."
  (let ((out (make-array (length s) :element-type '(unsigned-byte 8) :fill-pointer 0))
        (i 0))
    (loop while (< i (length s)) do
      (let ((c (char s i)))
        (if (and (char= c #\%) (< (+ i 2) (length s))
                 (digit-char-p (char s (+ i 1)) 16) (digit-char-p (char s (+ i 2)) 16))
            (progn (vector-push (parse-integer s :start (1+ i) :end (+ i 3) :radix 16) out)
                   (incf i 3))
            (progn (loop for b across (sb-ext:string-to-octets (string c) :external-format :utf-8)
                         do (vector-push-extend b out))
                   (incf i)))))
    (sb-ext:octets-to-string (coerce out '(vector (unsigned-byte 8)))
                             :external-format '(:utf-8 :replacement #\?))))

(defun path-query-p (query)
  "Is this Spotter QUERY a FILE PATH rather than a fuzzy search? Yes when it starts
   with / ./ ../ ~/ or file://, or is exactly ~ (or . or ..)."
  (and (plusp (length query))
       (or (char= (char query 0) #\/)
           (member query '("~" "." "..") :test #'string=)
           (some (lambda (p) (and (>= (length query) (length p))
                                  (string= p query :end2 (length p))))
                 '("./" "../" "~/" "file://")))))

(defun parse-path-query (query &key cwd home)
  "Split a path QUERY into (values DIR PARTIAL PREFIX): DIR the absolute folder to
   complete in, PARTIAL what is typed of the last component, PREFIX the query text up
   to and including the last slash — kept as TYPED, so a completion of ~/Doc reads
   ~/Documents/ and not /home/everton/Documents/. NIL when QUERY is not a path.

   ./ and ../ are relative to CWD (the file manager's folder, when one has focus),
   else to HOME; ~ is HOME; file:// is stripped and percent-decoded (file://host/…
   keeps only the path)."
  (unless (path-query-p query) (return-from parse-path-query nil))
  (let* ((home (or home (home-directory)))
         (cwd (or cwd home))
         (q (cond ((member query '("~" "." "..") :test #'string=)
                   (concatenate 'string query "/"))
                  (t query))))
    (when (and (>= (length q) 7) (string= "file://" q :end2 7))
      (let* ((rest (percent-decode (subseq q 7)))
             (slash (position #\/ rest)))
        ;; file:///x has an empty host; file://localhost/x names one — drop it
        (setf q (if slash (subseq rest slash) "/"))))
    (let* ((last-slash (position #\/ q :from-end t))
           (prefix (subseq q 0 (1+ last-slash)))
           (partial (subseq q (1+ last-slash)))
           (dir-text (subseq q 0 last-slash))
           (abs (cond ((char= (char q 0) #\/) dir-text)
                      ((char= (char q 0) #\~) (concatenate 'string home (subseq dir-text 1)))
                      (t (path-join cwd dir-text)))))
      (values (normalize-path (if (zerop (length abs)) "/" abs))
              partial
              ;; a file:// query completes into plain paths from here on
              (if (and (>= (length query) 7) (string= "file://" query :end2 7))
                  (let ((d (normalize-path (if (zerop (length abs)) "/" abs))))
                    (if (string= d "/") "/" (concatenate 'string d "/")))
                  prefix)))))

(defun path-completions (query &key cwd home (limit 200))
  "The entries a path QUERY could complete to, as (NAME DIR-P FULL-PATH LABEL) lists,
   folders first then by name. Shell rules: a case-sensitive PREFIX match on the last
   component; if nothing matches that way, a case-insensitive prefix; if still
   nothing, a case-insensitive substring — so ~/doc finds ~/Documents without making
   you remember its capital. Dot files only when what you typed starts with a dot.
   LABEL is what the row shows: the query's own prefix plus the name (and / for a
   folder). NIL when QUERY is not a path or its folder cannot be read."
  (multiple-value-bind (dir partial prefix) (parse-path-query query :cwd cwd :home home)
    (unless dir (return-from path-completions nil))
    (let ((names '()))
      (ignore-errors
        (map-directory-names
         (lambda (name dtype)
           (when (< (length names) 5000)
             (push (cons name (or dtype (path-type (path-join dir name)))) names)))
         dir))
      (let* ((show-hidden (and (plusp (length partial)) (char= (char partial 0) #\.)))
             (pool (remove-if (lambda (n) (and (not show-hidden) (hidden-name-p (car n)))) names))
             (hits (or (remove-if-not (lambda (n) (and (>= (length (car n)) (length partial))
                                                       (string= partial (car n) :end2 (length partial))))
                                      pool)
                       (remove-if-not (lambda (n) (and (>= (length (car n)) (length partial))
                                                       (string-equal partial (car n) :end2 (length partial))))
                                      pool)
                       (remove-if-not (lambda (n) (search partial (car n) :test #'char-equal)) pool))))
        (flet ((dirp (n) (or (eq (cdr n) :directory)
                             (and (eq (cdr n) :symlink)
                                  (eq (followed-type (path-join dir (car n))) :directory)))))
          (let ((rows (mapcar (lambda (n)
                                (let ((d (dirp n)))
                                  (list (car n) d (path-join dir (car n))
                                        (concatenate 'string prefix (car n) (if d "/" "")))))
                              hits)))
            (setf rows (sort rows (lambda (a b)
                                    (cond ((and (second a) (not (second b))) t)
                                          ((and (second b) (not (second a))) nil)
                                          (t (natural-lessp (first a) (first b)))))))
            (if (> (length rows) limit) (subseq rows 0 limit) rows)))))))

(defun common-prefix (strings)
  "The longest string every one of STRINGS starts with (case-sensitive)."
  (if (null strings) ""
      (reduce (lambda (a b)
                (subseq a 0 (or (mismatch a b) (min (length a) (length b)))))
              strings)))

(defun complete-path-query (query &key cwd home)
  "Tab in the Spotter, as a shell does it: extend QUERY by what every candidate has
   in common; with exactly one candidate, complete it outright — and a folder gets its
   trailing slash so the next Tab lists inside it. Returns QUERY unchanged when there
   is nothing to add. The query keeps its own form (~ stays ~)."
  (multiple-value-bind (dir partial prefix) (parse-path-query query :cwd cwd :home home)
    (unless dir (return-from complete-path-query query))
    (let ((rows (path-completions query :cwd cwd :home home :limit 5000)))
      ;; only TRUE prefix matches extend the text; the looser fallbacks in
      ;; PATH-COMPLETIONS are for showing, and their names do not start with PARTIAL
      (setf rows (remove-if-not (lambda (r) (and (>= (length (first r)) (length partial))
                                                 (string= partial (first r) :end2 (length partial))))
                                rows))
      (cond ((null rows) query)
            ((null (rest rows))
             (concatenate 'string prefix (first (first rows)) (if (second (first rows)) "/" "")))
            (t (let ((cp (common-prefix (mapcar #'first rows))))
                 (if (> (length cp) (length partial))
                     (concatenate 'string prefix cp)
                     query)))))))
