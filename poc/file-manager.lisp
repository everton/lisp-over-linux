;;;; file-manager.lisp — the file manager's HANDS: keys, prompts, and the operations.
;;;;
;;;; initramfs/files.lisp is the disk as data, initramfs/file-views.lisp is that data
;;;; as views; this file is the part of the shell that ACTS: it binds the letters in a
;;;; folder (r rename, d delete, / filter, …), asks its questions in the minibuffer
;;;; (shell.lisp), runs the operation, turns any FS-ERROR into a status-line message,
;;;; and re-presents every pane the change touched. Nothing here draws.
;;;;
;;;; It is the ONLY way to change the user's disk on this machine — there is no shell —
;;;; so three rules run through it:
;;;;   1. every operation is guarded: an EACCES is a sentence, never a dead PID 1;
;;;;   2. nothing destructive happens without a question: delete always asks, and
;;;;      deleting a non-empty folder asks you to TYPE "yes", quoting what is inside;
;;;;   3. nothing is overwritten implicitly: rename, copy, move and link all refuse
;;;;      an existing destination (files.lisp makes those refusals atomic).
;;;;
;;;; doc/file-manager.org has the design and the key table. Loaded after shell.lisp.

(in-package :cl-user)

(defvar *fm-interrupt-fn* nil
  "A function of no arguments the BACKEND installs, true when the user asked to stop
   a long operation (the framebuffer checks for Esc / C-g). A recursive find polls
   it; NIL means 'cannot be interrupted, rely on the caps'.")

;;; ---- where we are ------------------------------------------------------------

(defun fm-pane ()
  "The pane the file manager's keys act on: the focused one."
  (current-pane))

(defun fm-dir ()
  "The folder of the focused pane — where a new file goes, what ./ means — or HOME."
  (or (ignore-errors (folder-of-subject (pane-subject (fm-pane)))) (home-directory)))

(defun item-path (item)
  "The path a listing row stands for, or NIL (a note row, an archive member)."
  (and item (subject-path (item-subject item))))

(defun fm-selected-paths (pane)
  "What an operation acts on: the MARKED entries when the folder has any, else the
   selected row. Marks are dired's answer to 'do this to several files' — select
   with Space, then one key acts on all of them."
  (let ((s (pane-subject pane)))
    (if (and (subj-dir-p s) (subj-dir-marks s))
        (mapcar (lambda (n) (path-join (subj-dir-path s) n)) (subj-dir-marks s))
        (let ((p (item-path (selected-item pane)))) (and p (list p))))))

(defun open-files (&optional (path (home-directory)))
  "SuperL-f: the file manager. Focus the tab already showing a folder if there is one
   (and it is not the one you are on); otherwise open a new tab at PATH (home)."
  (let ((i (position-if (lambda (trail) (subj-dir-p (pane-subject (first trail)))) *trails*)))
    (if (and i (/= i *active*))
        (activate-trail i)
        (new-trail (subj-dir (normalize-path path))))))

(defun open-folder-selecting (path)
  "Open the folder containing PATH with PATH's entry selected, in this trail."
  (trail-push (subj-dir (path-parent path) :select (path-name path))))

;;; ---- keeping the panes honest --------------------------------------------------
;;; A view is a rendering of the disk at the moment it was presented. After a change,
;;; every pane that shows a folder is re-presented — not patched — for the same reason
;;; SET-FIELD re-presents an inspector: the view is derived, so derive it again.

(defun represent-pane (pane)
  "Re-present PANE keeping its selection by NAME (its index may move)."
  (let ((s (pane-subject pane)))
    (when (subj-dir-p s)
      (let ((p (item-path (selected-item pane))))
        (when (and p (null (subj-dir-select s))) (setf (subj-dir-select s) (path-name p)))))
    (let ((v (ignore-errors (present s))))
      (when v
        (setf (pane-view pane) v)
        (set-pane-sel pane (view-sel v))))
    (when (subj-dir-p s) (setf (subj-dir-select s) nil))))

(defun fm-refresh (&key select (pane (fm-pane)))
  "Re-present every folder pane in every trail (a change in one folder can show in
   another: a move has two ends), selecting SELECT (a name) in PANE. Also drops the
   cached nav-split preview, which may describe a file that no longer exists."
  (dolist (trail *trails*)
    (dolist (p trail)
      (when (and (subj-dir-p (pane-subject p)) (not (eq p pane)))
        (represent-pane p))))
  (when (and pane (subj-dir-p (pane-subject pane)))
    (when select (setf (subj-dir-select (pane-subject pane)) select))
    (represent-pane pane))
  (setf *preview-subject* nil *preview-pane* nil))

(defmacro with-fs-errors ((verb) &body body)
  "Run BODY; an FS-ERROR (or anything else) becomes the status line — rule 1 above."
  `(handler-case (progn ,@body)
     (fs-error (c) (setf *browser-status* (format nil "~a failed: ~a" ,verb c)) nil)
     (serious-condition (c) (setf *browser-status* (format nil "~a failed: ~a" ,verb c)) nil)))

;;; ---- reading a name or a path from the user -------------------------------------

(defun valid-name-p (name)
  "A single path component: not empty, not . or .., no slash, no NUL."
  (and (plusp (length name))
       (not (member name '("." "..") :test #'string=))
       (not (find #\/ name)) (not (find (code-char 0) name))))

(defun fm-resolve (text dir)
  "TEXT as typed in a prompt -> an absolute path: ~ and ~/… from home, /… as is,
   anything else relative to DIR. Lexically normalised (NORMALIZE-PATH); a trailing
   slash is reported as the second value, since 'into this folder' is what it means."
  (let* ((s (string-trim '(#\Space #\Tab) text))
         (slash (and (plusp (length s)) (char= (char s (1- (length s))) #\/)))
         (abs (cond ((string= s "~") (home-directory))
                    ((and (>= (length s) 2) (string= "~/" s :end2 2))
                     (path-join (home-directory) (subseq s 2)))
                    ((and (plusp (length s)) (char= (char s 0) #\/)) s)
                    (t (path-join dir s)))))
    (values (normalize-path abs) slash)))

(defun path-completer (dir)
  "A minibuffer Tab that completes paths like the Spotter does — relative text is
   completed inside DIR."
  (lambda (text)
    (if (path-query-p text)
        (complete-path-query text :cwd dir)
        (let ((c (complete-path-query (concatenate 'string "./" text) :cwd dir)))
          (if (and (>= (length c) 2) (string= "./" c :end2 2)) (subseq c 2) text)))))

(defun ask-name (prompt initial on-name &key hint)
  "Read a plain file NAME (no slashes) and call ON-NAME with it; refuse anything else."
  (minibuffer-read prompt
                   (lambda (text)
                     (let ((name (string-trim '(#\Space #\Tab) text)))
                       (if (valid-name-p name)
                           (funcall on-name name)
                           (setf *browser-status*
                                 (format nil "~s is not a file name (no /, not . or ..)" name)))))
                   :initial initial :hint (or hint "Enter  Esc cancels")))

;;; ---- the operations -------------------------------------------------------------

(defun fm-new-folder ()
  (let ((dir (fm-dir)))
    (ask-name (format nil "New folder in ~a/: " (abbreviate-home dir)) ""
              (lambda (name)
                (with-fs-errors ("mkdir")
                  (make-directory (path-join dir name))
                  (fm-refresh :select name)
                  (setf *browser-status* (format nil "made folder ~a/" name)))))))

(defun fm-new-file ()
  (let ((dir (fm-dir)))
    (ask-name (format nil "New file in ~a/: " (abbreviate-home dir)) ""
              (lambda (name)
                (with-fs-errors ("create")
                  (create-file (path-join dir name))
                  (fm-refresh :select name)
                  (setf *browser-status* (format nil "made ~a (empty) - Enter edits it" name)))))))

(defun fm-rename (path)
  (let ((old (path-name path)) (dir (path-parent path)))
    (ask-name (format nil "Rename ~a to: " old) old
              (lambda (name)
                (if (string= name old)
                    (setf *browser-status* "rename: same name, nothing to do")
                    (with-fs-errors ("rename")
                      (rename-path path (path-join dir name))
                      (fm-refresh :select name)
                      (setf *browser-status* (format nil "renamed ~a -> ~a" old name))))))))

(defun fm-transfer (verb paths)
  "Copy or move PATHS (VERB :copy / :move). The destination is read as a path: an
   existing folder (or anything typed with a trailing /) means INTO it, keeping each
   name; otherwise it is the new name — which only makes sense for one source."
  (let* ((dir (fm-dir))
         (what (if (rest paths) (format nil "~d items" (length paths)) (path-name (first paths)))))
    (minibuffer-read
     (format nil "~:[Copy~;Move~] ~a to: " (eq verb :move) what)
     (lambda (text)
       (multiple-value-bind (dest slash) (fm-resolve text dir)
         (let ((into (or slash (eq (followed-type dest) :directory))))
           (cond
             ((and (rest paths) (not into))
              (setf *browser-status* (format nil "~a is not a folder - several items need one"
                                             (abbreviate-home dest))))
             (t
              (let ((done 0) (skipped 0))
                (with-fs-errors ((if (eq verb :move) "move" "copy"))
                  (dolist (p paths)
                    (let ((to (if into (path-join dest (path-name p)) dest)))
                      (if (eq verb :move)
                          (move-path p to)
                          (incf skipped (copy-recursively p to)))
                      (incf done))))
                (let ((s (pane-subject (fm-pane))))
                  (when (subj-dir-p s) (setf (subj-dir-marks s) '())))
                (fm-refresh)
                (when (plusp done)
                  (setf *browser-status*
                        (format nil "~:[copied~;moved~] ~d item~:p to ~a~[~:;, ~:*~d special file~:p skipped~]"
                                (eq verb :move) done
                                (abbreviate-home (if into dest (path-parent dest)))
                                skipped)))))))))
     :initial (concatenate 'string (abbreviate-home dir) "/")
     :completer (path-completer dir)
     :hint "Tab completes  a folder = into it")))

(defun delete-summary (paths)
  "(values TEXT NEEDS-YES): what deleting PATHS would remove, in words, and whether
   it includes a non-empty folder (which must be confirmed by typing yes)."
  (let ((files 0) (dirs 0) (inside 0) (bytes 0) (partial nil))
    (dolist (p paths)
      (if (eq (path-type p) :directory)
          (multiple-value-bind (n b complete) (tree-size p)
            (incf dirs) (incf inside n) (incf bytes b)
            (unless complete (setf partial t)))
          (progn (incf files) (incf bytes (or (ignore-errors (fentry-size (file-entry p))) 0)))))
    (values
     (cond ((and (= 1 (length paths)) (zerop dirs))
            (format nil "Delete ~a?" (path-name (first paths))))
           ((and (= 1 (length paths)) (zerop inside))
            (format nil "Delete the empty folder ~a/?" (path-name (first paths))))
           ((= 1 (length paths))
            (format nil "Delete folder ~a/ and the ~:d~:[~;+~] entr~@p inside (~a)?"
                    (path-name (first paths)) inside partial inside (human-size bytes)))
           (t (format nil "Delete ~d items~a (~a)?"
                      (length paths)
                      (if (plusp dirs)
                          (format nil ", ~d folder~:p with ~:d~:[~;+~] entries inside" dirs inside partial)
                          "")
                      (human-size bytes))))
     (plusp inside))))

(defun fm-delete (paths)
  "Delete PATHS after asking. A file, a link or an empty folder: y/n. Anything with a
   non-empty folder in it: type yes — the prompt quotes how many entries that is."
  (multiple-value-bind (question needs-yes) (delete-summary paths)
    (flet ((doit ()
             (let ((pane (fm-pane)) (n 0))
               ;; select a neighbour, so the cursor does not jump to the top
               (let* ((items (view-items (pane-view pane)))
                      (i (pane-sel pane))
                      (next (loop for j from (1+ i) below (length items)
                                  for p = (item-path (nth j items))
                                  when (and p (not (member p paths :test #'string=)))
                                    return (path-name p))))
                 (with-fs-errors ("delete")
                   (dolist (p paths) (delete-tree p) (incf n)))
                 (let ((s (pane-subject pane)))
                   (when (subj-dir-p s) (setf (subj-dir-marks s) '())))
                 (fm-refresh :select next)
                 (when (plusp n)
                   (setf *browser-status* (format nil "deleted ~d item~:p" n)))))))
      (if needs-yes
          (minibuffer-read (format nil "~a  Type yes: " question)
                           (lambda (text)
                             (if (string-equal (string-trim " " text) "yes")
                                 (doit)
                                 (setf *browser-status* "not deleted (you did not type yes)")))
                           :hint "this deletes everything inside - Esc cancels")
          (minibuffer-read (format nil "~a (y/n) " question)
                           (lambda (k)
                             (if (member k '(#\y #\Y))
                                 (doit)
                                 (setf *browser-status* "not deleted")))
                           :single-key t)))))

(defun fm-link (path kind)
  "Make a link to PATH — KIND :symbolic or :hard — asking for the link's name or path.
   A SYMBOLIC link stores text. When the link sits in the same folder as its target
   the text is just the target's NAME (relative), so the pair keeps working if the
   folder is moved or reached through another mount; anywhere else it is the target's
   ABSOLUTE path, which survives moving the LINK. A HARD link stores nothing: it is a
   second name for the same inode, so there is no text to choose."
  (let* ((dir (fm-dir)) (name (path-name path)))
    (minibuffer-read
     (format nil "~:[Hard~;Symbolic~] link to ~a, named: " (eq kind :symbolic) name)
     (lambda (text)
       (let ((link (fm-resolve text dir)))
         (with-fs-errors ("link")
           (if (eq kind :symbolic)
               (make-symlink (if (string= (path-parent link) (path-parent path)) name path) link)
               (make-hard-link path link))
           (fm-refresh :select (and (string= (path-parent link) dir) (path-name link)))
           (setf *browser-status* (format nil "~a -> ~a" (abbreviate-home link)
                                          (if (eq kind :symbolic)
                                              (or (ignore-errors (sb-unix:unix-readlink link)) name)
                                              (format nil "~a (same inode)" name)))))))
     :initial (format nil "~a-link" name)
     :completer (path-completer dir)
     :hint "a name here, or a path")))

(defun fm-filter ()
  "/ — narrow the folder to names containing what you type, live. Enter keeps the
   filter (the title says so: '12 items of 340'); Esc drops it."
  (let* ((pane (fm-pane)) (s (pane-subject pane)) (before (subj-dir-filter s)))
    (flet ((apply-filter (text)
             (setf (subj-dir-filter s) (if (plusp (length text)) text nil))
             (let ((v (present s)))
               (setf (pane-view pane) v)
               (set-pane-sel pane 0))))
      (minibuffer-read "Filter: "
                       (lambda (text) (apply-filter text)
                         (setf *browser-status* (and (plusp (length text))
                                                     (format nil "filtered by ~s - / then Esc clears it" text))))
                       :initial (or before "")
                       :on-change #'apply-filter
                       :on-cancel (lambda () (apply-filter ""))
                       :hint "substring, or a glob like *.lisp"))))

(defun fm-find ()
  "f — find by name under this folder, recursively, bounded (FIND-FILES). The walk
   runs once, now; its results open as a list in this trail."
  (let ((dir (fm-dir)))
    (minibuffer-read (format nil "Find under ~a/: " (abbreviate-home dir))
                     (lambda (text)
                       (let ((q (string-trim " " text)))
                         (when (plusp (length q))
                           (with-fs-errors ("find")
                             (multiple-value-bind (results info)
                                 (find-files dir q :hidden *show-hidden* :interrupt-fn *fm-interrupt-fn*)
                               (trail-push (subj-find dir q results info))
                               (setf *browser-status*
                                     (format nil "~d found, ~d folders searched~@[ - stopped: ~(~a~)~]"
                                             (length results) (getf info :dirs) (getf info :stopped))))))))
                     :hint "substring, or a glob like *.org  Esc stops a search")))

(defun fm-parent ()
  "^ — up to the containing folder. When the trail came DOWN from it, this is just
   back; otherwise (you arrived by the Spotter, or at home) the pane is replaced by
   its parent, with the folder you were in selected."
  (let* ((pane (fm-pane)) (s (pane-subject pane))
         (path (subject-path s))
         (parent (and path (path-parent path)))
         (trail (current-trail))
         (above (and (cdr trail) (car (last (butlast trail))))))
    (cond
      ((null path) nil)
      ((string= path "/") (setf *browser-status* "already at /"))
      ((and above (subj-dir-p (pane-subject above))
            (string= (subj-dir-path (pane-subject above)) parent))
       (trail-pop))
      (t (let ((np (open-pane (subj-dir parent :select (path-name path)))))
           (set-current-trail (append (butlast trail) (list np))))))))

(defun fm-sort (how)
  (case how
    (:next (setf *file-sort* (or (second (member *file-sort* +sort-keys+)) (first +sort-keys+))))
    (:reverse (setf *file-sort-reverse* (not *file-sort-reverse*))))
  (fm-refresh)
  (setf *browser-status* (format nil "sorted by ~a (s: next order, S: reverse)" (sort-word))))

(defun fm-toggle-hidden ()
  (setf *show-hidden* (not *show-hidden*))
  (fm-refresh)
  (setf *browser-status* (if *show-hidden* "dot files shown (. hides them)" "dot files hidden")))

(defun fm-toggle-mark (pane)
  "Space — mark or unmark the selected entry, and move down (dired's m / u in one)."
  (let* ((s (pane-subject pane)) (p (item-path (selected-item pane))))
    (when (and p (subj-dir-p s))
      (let ((name (path-name p)))
        (setf (subj-dir-marks s)
              (if (member name (subj-dir-marks s) :test #'string=)
                  (remove name (subj-dir-marks s) :test #'string=)
                  (cons name (subj-dir-marks s))))
        (let ((sel (pane-sel pane)))
          (setf (pane-view pane) (present s))
          (set-pane-sel pane (1+ sel)))
        (setf *browser-status* (format nil "~d marked - d / c / m act on all of them, U clears"
                                       (length (subj-dir-marks s))))))))

(defun fm-jump-target (path)
  "j — for a symlink, go to what it points at: its folder, with it selected."
  (let ((real (ignore-errors (sb-unix:unix-realpath path))))
    (cond ((not (eq (path-type path) :symlink)) (setf *browser-status* "j: not a symbolic link"))
          ((null real) (setf *browser-status* "j: the link is broken - its target does not exist"))
          (t (open-folder-selecting real)))))

(defun fm-run (op &optional path)
  "Run file-manager operation OP — the one entry point the commands (menu, halo,
   palette) and the keys share. PATH is the entry it applies to; NIL means the
   selection (or the marks)."
  (let* ((pane (fm-pane))
         (paths (if path (list path) (fm-selected-paths pane)))
         (one (first paths)))
    (flet ((need-one () (or one (progn (setf *browser-status* "nothing selected") nil))))
      (case op
        (:new-folder (fm-new-folder))
        (:new-file (fm-new-file))
        (:filter (if (subj-dir-p (pane-subject pane)) (fm-filter)
                     (setf *browser-status* "/ filters a folder")))
        (:find (fm-find))
        (:sort (fm-sort :next))
        (:reverse (fm-sort :reverse))
        (:hidden (fm-toggle-hidden))
        (:parent (fm-parent))
        (:home (trail-push (subj-dir (home-directory))))
        (:refresh (fm-refresh) (setf *browser-status* "refreshed"))
        (:rename (when (need-one) (fm-rename one)))
        (:copy (when (need-one) (fm-transfer :copy paths)))
        (:move (when (need-one) (fm-transfer :move paths)))
        (:delete (when (need-one) (fm-delete paths)))
        (:symlink (when (need-one) (fm-link one :symbolic)))
        (:hardlink (when (need-one) (fm-link one :hard)))
        (:chmod-x (when (need-one)
                    (with-fs-errors ("chmod")
                      (let ((m (toggle-executable one)))
                        (fm-refresh)
                        (setf *browser-status* (format nil "~a is now ~a" (path-name one)
                                                       (subseq (mode-string m) 1)))))))
        (:copy-path (when (need-one)
                      (copy-to-clipboard (if (rest paths) (format nil "~{~a~^ ~}" paths) one))))
        (:info (when (need-one) (trail-push (subj-file-info one))))
        (:jump (when (need-one) (fm-jump-target one)))
        (t (setf *browser-status* (format nil "unknown file operation ~s" op)))))))

;;; ---- the keys -------------------------------------------------------------------
;;; Plain characters, as in dired and ranger: a folder is a LIST, it has no text to
;;; type into, so its letters are free to be verbs. Arrows, Enter, Backspace and q
;;; keep the meaning they have in every list (shell.lisp's BROWSER-KEY), and so does
;;; everything with Ctrl or Super. The table is doc/file-manager.org's.

(defparameter +fm-keys+
  '((#\/ . :filter) (#\f . :find) (#\s . :sort) (#\S . :reverse) (#\. . :hidden)
    (#\^ . :parent) (#\~ . :home) (#\g . :refresh)
    (#\+ . :new-folder) (#\n . :new-file)
    (#\r . :rename) (#\c . :copy) (#\m . :move) (#\d . :delete) (:delete . :delete)
    (#\l . :symlink) (#\L . :hardlink) (#\x . :chmod-x) (#\y . :copy-path)
    (#\i . :info) (#\j . :jump))
  "Key -> FM-RUN operation, in a folder.")

(defmethod subject-key ((s subj-dir) pane key state)
  (declare (ignore state))
  (cond
    ((eql key #\Space) (fm-toggle-mark pane) t)
    ((eql key #\U) (setf (subj-dir-marks s) '())
     (represent-pane pane) (setf *browser-status* "marks cleared") t)
    ((assoc key +fm-keys+) (fm-run (cdr (assoc key +fm-keys+))) t)
    (t nil)))

(defmethod subject-key ((s subj-find) pane key state)
  (declare (ignore state pane))
  ;; the results are entries too: the per-entry verbs apply; the folder ones do not
  (let ((op (cdr (assoc key +fm-keys+))))
    (when (member op '(:rename :copy :move :delete :symlink :hardlink :chmod-x
                       :copy-path :info :jump :find :hidden))
      (fm-run op)
      t)))

(defmethod subject-key ((s subj-file) pane key state)
  (declare (ignore state pane))
  ;; a non-text file (an image, an archive, a hex dump): ^ goes to its folder
  (when (eql key #\^) (open-folder-selecting (subj-file-path s)) t))

(defmethod subject-hints ((s subj-dir) pane)
  (declare (ignore pane))
  '(("ret" . "open") ("/" . "filter") ("f" . "find") ("s/S" . "sort") ("." . "dot files")
    ("+/n" . "new folder/file") ("r" . "rename") ("c/m" . "copy/move") ("d" . "delete")
    ("l" . "link") ("spc" . "mark") ("^" . "up")))

(defmethod subject-hints ((s subj-find) pane)
  (declare (ignore pane))
  '(("ret" . "open") ("r" . "rename") ("d" . "delete") ("c/m" . "copy/move")
    ("i" . "info") ("bs" . "back to the folder")))

;;; ---- saving a file: C-x C-s ------------------------------------------------------

(defun save-file-pane (pane)
  "Write PANE's buffer to its file — WRITE-FILE-ATOMICALLY: a temp file beside it,
   fsync, the old permission bits, rename over. If the file changed on disk since it
   was loaded (someone else wrote it), the first C-x C-s only warns; a second one
   overwrites, as Emacs's 'file changed on disk; really edit?' does."
  (let* ((v (pane-view pane))
         (data (view-data v))
         (path (getf data :path))
         (tv (pane-tv pane)))
    (cond
      ((null path) (setf *browser-status* "this pane has no file"))
      ((not (file-pane-dirty-p pane))
       (setf *browser-status* "(no changes need to be saved)"))
      (t
       (let ((disk-mtime (ignore-errors
                           (let ((e (file-entry path)))
                             (if (eq (fentry-type e) :symlink)
                                 (fentry-mtime (file-entry (sb-unix:unix-realpath path)))
                                 (fentry-mtime e))))))
         (if (and disk-mtime (getf data :mtime) (/= disk-mtime (getf data :mtime))
                  (not (getf data :overwrite-ok)))
             (progn (setf (getf (view-data v) :overwrite-ok) t)
                    (setf *browser-status*
                          "the file changed on disk since you opened it - C-x C-s again overwrites it"))
             (with-fs-errors ("save")
               (let* ((lines (lol.textview:tv-lines tv))
                      (octets (sb-ext:string-to-octets (lol.textview:tv-text tv)
                                                       :external-format :utf-8))
                      (real (write-file-atomically path octets)))
                 (setf (getf (view-data v) :saved-lines) lines
                       (getf (view-data v) :mtime)
                       (ignore-errors (fentry-mtime (file-entry real)))
                       (getf (view-data v) :overwrite-ok) nil
                       *unsaved-warned* nil)
                 (fm-refresh :pane nil)
                 (setf *browser-status* (format nil "wrote ~a (~a)" (abbreviate-home path)
                                                (human-size (length octets))))))))))))

(defmethod subject-hints ((s subj-file) pane)
  (declare (ignore pane))
  ;; a file shown as a LIST or an IMAGE (an archive, a picture, a broken link) — the
  ;; text ones are file panes and get the editor's hints instead
  '(("^" . "its folder") ("bs" . "back") ("i" . "info via the menu")))
