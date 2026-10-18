;;;; files-test.lisp — host test for the file manager's data layer.
;;;;
;;;;   sbcl --script poc/files-test.lisp
;;;;
;;;; initramfs/files.lisp is the only code in the image that CHANGES the user's disk,
;;;; so it is tested where a mistake is cheap: in a scratch directory made fresh for
;;;; the run (under $FILES_TEST_DIR if set, else /tmp), never anywhere near $HOME.
;;;; Every operation the file manager offers is exercised — create, rename (and its
;;;; refusal to overwrite), delete (and rmdir's refusal of a non-empty folder),
;;;; recursive delete that must not follow a symlink out of the tree, symlinks
;;;; (relative and broken), hard links, copy and move of trees, the atomic save that
;;;; keeps the file's mode and writes through a link — plus the read side: hidden
;;;; filtering, the four sort orders, the bounded recursive find, and the Spotter's
;;;; path grammar. The sniffers and archive readers of initramfs/file-views.lisp are
;;;; checked against files made here with the system's own tar, gzip and zip.
;;;;
;;;; Prints one line per check and exits non-zero on any failure.

(require :sb-introspect)
(let ((root (merge-pathnames "../" (make-pathname :directory (pathname-directory *load-truename*)))))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "initramfs/registry.lisp" root))
    (dolist (m '("initramfs/model.lisp" "initramfs/present.lisp" "initramfs/sysinfo.lisp"
                 "poc/inflate.lisp" "initramfs/files.lisp" "initramfs/file-views.lisp"))
      (funcall 'load-recording (merge-pathnames m root)))))

(defvar *failures* 0)

(defun ok (name got want)
  (if (equal got want)
      (format t "ok    ~a~%" name)
      (progn (incf *failures*)
             (format t "FAIL  ~a~%      got  ~s~%      want ~s~%" name got want))))

(defmacro fails-with (errno &body body)
  "The FS-ERROR errno BODY signals, or :NO-ERROR — for checking refusals."
  `(handler-case (progn ,@body :no-error)
     (fs-error (c) (if (eql (fs-error-errno c) ,errno) ,errno (list :other (fs-error-errno c) (princ-to-string c))))))

(defvar *dir*
  (let ((base (or (sb-ext:posix-getenv "FILES_TEST_DIR") "/tmp")))
    (format nil "~a/files-test-~d" (string-right-trim "/" base) (sb-unix:unix-getpid))))

(defun p (&rest parts) (format nil "~a~{/~a~}" *dir* parts))
(defun spit (path text)
  (with-open-file (s (native-path path) :direction :output :if-exists :supersede)
    (write-string text s)))
(defun slurp (path)
  (with-open-file (s (native-path path)) (let ((str (make-string (file-length s)))) (subseq str 0 (read-sequence str s)))))
(defun names (dir &key hidden (by :name) reverse)
  (mapcar #'fentry-name (sort-entries (filter-entries (list-directory dir) :hidden hidden)
                                      :by by :reverse reverse)))
(defun sh (cmd) (sb-ext:run-program "/bin/sh" (list "-c" cmd) :output nil :error nil))

(when (path-exists-p *dir*) (error "scratch ~a already exists" *dir*))
(make-directory *dir*)

;;; 1. creating, and refusing to create twice
(ok "mkdir" (progn (make-directory (p "a")) (path-type (p "a"))) :directory)
(ok "mkdir again -> EEXIST" (fails-with 17 (make-directory (p "a"))) 17)
(ok "create file" (progn (create-file (p "a" "f.txt")) (path-type (p "a" "f.txt"))) :file)
(ok "create again -> EEXIST (O_EXCL)" (fails-with 17 (create-file (p "a" "f.txt"))) 17)
(ok "create in a missing folder -> ENOENT" (fails-with 2 (create-file (p "nope" "x"))) 2)

;;; 2. rename never overwrites
(spit (p "a" "f.txt") "one")
(spit (p "a" "g.txt") "two")
(ok "rename onto an existing name -> EEXIST" (fails-with 17 (rename-path (p "a" "f.txt") (p "a" "g.txt"))) 17)
(ok "... and both files are intact" (list (slurp (p "a" "f.txt")) (slurp (p "a" "g.txt"))) '("one" "two"))
(rename-path (p "a" "f.txt") (p "a" "h.txt"))
(ok "rename" (list (path-exists-p (p "a" "f.txt")) (slurp (p "a" "h.txt"))) '(nil "one"))

;;; 3. listing: hidden files, sorting
(spit (p "a" ".secret") "s")
(make-directory (p "a" "zdir"))
(spit (p "a" "file10") "1234567890")
(spit (p "a" "file2") "12")
(ok "hidden files are hidden" (names (p "a")) '("zdir" "file2" "file10" "g.txt" "h.txt"))
(ok "... and shown on request" (names (p "a") :hidden t) '("zdir" ".secret" "file2" "file10" "g.txt" "h.txt"))
(ok "by name, reversed (folders stay first)" (names (p "a") :reverse t) '("zdir" "h.txt" "g.txt" "file10" "file2"))
(ok "by size (largest first)" (names (p "a") :by :size) '("zdir" "file10" "g.txt" "h.txt" "file2"))
(ok "by type (extension, then name)" (names (p "a") :by :type) '("zdir" "file2" "file10" "g.txt" "h.txt"))
(sh (format nil "touch -d '2020-01-01' '~a'" (p "a" "g.txt")))
(ok "by date (newest first; g.txt is from 2020)" (car (last (names (p "a") :by :mtime))) "g.txt")
(ok "filter: substring" (mapcar #'fentry-name (filter-entries (list-directory (p "a")) :query "FILE"))
    (mapcar #'fentry-name (filter-entries (list-directory (p "a")) :query "file")))
(ok "filter: glob" (sort (mapcar #'fentry-name (filter-entries (list-directory (p "a")) :query "*.txt")) #'string<)
    '("g.txt" "h.txt"))
(ok "natural order: file2 < file10" (natural-lessp "file2" "file10") t)

;;; 4. links
(make-symlink "h.txt" (p "a" "rel-link"))
(make-symlink "/no/such/target" (p "a" "broken"))
(let ((e (file-entry (p "a" "rel-link"))) (b (file-entry (p "a" "broken"))))
  (ok "symlink is lstat'd as a link" (fentry-type e) :symlink)
  (ok "... its text is kept verbatim (relative)" (fentry-target e) "h.txt")
  (ok "... and it resolves to a file" (list (fentry-target-type e) (fentry-broken e)) '(:file nil))
  (ok "broken link is marked broken" (fentry-broken b) t))
(make-hard-link (p "a" "h.txt") (p "a" "hard"))
(ok "hard link shares the inode" (= (fentry-ino (file-entry (p "a" "hard"))) (fentry-ino (file-entry (p "a" "h.txt")))) t)
(ok "hard link to a folder -> EPERM" (fails-with 1 (make-hard-link (p "a" "zdir") (p "a" "zhard"))) 1)

;;; 5. the safe save: mode kept, through a link, nothing left behind
(sh (format nil "chmod 640 '~a'" (p "a" "h.txt")))
(write-file-atomically (p "a" "rel-link") (sb-ext:string-to-octets "saved through the link" :external-format :utf-8))
(ok "save through a symlink writes the target" (slurp (p "a" "h.txt")) "saved through the link")
(ok "... the link is still a link" (path-type (p "a" "rel-link")) :symlink)
(ok "... the mode is kept (0640)" (logand #o7777 (fentry-mode (file-entry (p "a" "h.txt")))) #o640)
(ok "... no temp file left behind"
    (remove-if-not (lambda (n) (search "lol-save" n)) (names (p "a") :hidden t)) '())
(write-file-atomically (p "a" "new.txt") (sb-ext:string-to-octets "fresh" :external-format :utf-8))
(ok "save creates a new file" (slurp (p "a" "new.txt")) "fresh")

;;; 6. delete: rmdir refuses, delete-tree never follows a link out
(make-directory (p "keep"))
(spit (p "keep" "precious") "do not delete")
(make-directory (p "tree"))
(make-directory (p "tree" "inner"))
(spit (p "tree" "inner" "x") "x")
(make-symlink (p "keep") (p "tree" "escape"))
(ok "rmdir of a non-empty folder -> ENOTEMPTY" (fails-with 39 (delete-path (p "tree"))) 39)
(ok "tree-size counts entries and does not follow links"
    (subseq (multiple-value-list (tree-size (p "tree"))) 0 1) '(3))
(delete-tree (p "tree"))
(ok "delete-tree removes the tree" (path-exists-p (p "tree")) nil)
(ok "... but not what a link inside pointed at" (slurp (p "keep" "precious")) "do not delete")
(ok "delete a symlink removes the link only"
    (progn (delete-path (p "a" "rel-link")) (list (path-exists-p (p "a" "rel-link")) (path-exists-p (p "a" "h.txt"))))
    '(nil t))

;;; 7. copy and move
(make-directory (p "src"))
(make-directory (p "src" "d"))
(spit (p "src" "d" "f") "payload")
(sh (format nil "chmod 750 '~a'" (p "src" "d" "f")))
(make-symlink "f" (p "src" "d" "lnk"))
(copy-recursively (p "src") (p "copy"))
(ok "copy-recursively copies contents" (slurp (p "copy" "d" "f")) "payload")
(ok "... keeps the mode" (logand #o7777 (fentry-mode (file-entry (p "copy" "d" "f")))) #o750)
(ok "... re-makes a link with the same text" (fentry-target (file-entry (p "copy" "d" "lnk"))) "f")
(ok "copy onto an existing name -> EEXIST" (fails-with 17 (copy-file (p "src" "d" "f") (p "copy" "d" "f"))) 17)
(ok "copy a folder into itself is refused"
    (handler-case (progn (copy-recursively (p "src") (p "src" "d" "inside")) :copied)
      (fs-error () :refused))
    :refused)
(move-path (p "copy") (p "moved"))
(ok "move" (list (path-exists-p (p "copy")) (slurp (p "moved" "d" "f"))) '(nil "payload"))
(ok "toggle-executable: r-x for who may read"
    (progn (sh (format nil "chmod 640 '~a'" (p "a" "g.txt")))
           (toggle-executable (p "a" "g.txt"))
           (logand #o777 (fentry-mode (file-entry (p "a" "g.txt")))))
    #o750)

;;; 8. find: bounded, and never down a symlink
(make-directory (p "deep"))
(let ((d (p "deep")))
  (dotimes (i 30) (setf d (path-join d (format nil "l~d" i))) (make-directory d))
  (spit (path-join d "needle.lisp") "(found)"))
(make-symlink (p "deep") (p "deep" "loop"))          ; a cycle through a link
(multiple-value-bind (r info) (find-files (p "deep") "needle")
  (ok "find reaches 30 levels down" (mapcar #'fentry-name r) '("needle.lisp"))
  (ok "... and finished (no cap hit, the link cycle not followed)" (getf info :stopped) nil))
(multiple-value-bind (r info) (find-files (p "deep") "needle" :max-dirs 5)
  (ok "find stops at the folder cap" (list r (getf info :stopped)) '(nil :dirs)))
(multiple-value-bind (r info) (find-files *dir* "*.txt" :max-results 2)
  (ok "find stops at the result cap" (list (length r) (getf info :stopped)) '(2 :results)))
(ok "find can be interrupted"
    (getf (nth-value 1 (find-files (p "deep") "zzz" :interrupt-fn (constantly t))) :stopped)
    :interrupted)

;;; 9. the Spotter's path grammar
(let ((home *dir*))
  (flet ((parse (q &optional (cwd (p "a"))) (multiple-value-list (parse-path-query q :cwd cwd :home home))))
    (ok "not a path" (path-query-p "draw") nil)
    (ok "~ alone" (parse "~") (list home "" "~/"))
    (ok "~/a/fi" (parse "~/a/fi") (list (p "a") "fi" "~/a/"))
    (ok "./ is the current folder" (parse "./ze") (list (p "a") "ze" "./"))
    (ok "../ goes up" (parse "../src/") (list (p "src") "" "../src/"))
    (ok "file:// is stripped and percent-decoded"
        (parse (format nil "file://~a/a/My%20F" *dir*))
        (list (p "a") "My F" (concatenate 'string (p "a") "/")))
    (ok "completions: folders first, a / on a folder"
        (mapcar #'fourth (path-completions "~/a/" :home home))
        '("~/a/zdir/" "~/a/broken" "~/a/file2" "~/a/file10" "~/a/g.txt" "~/a/h.txt" "~/a/hard"
          "~/a/new.txt"))
    (ok "dot files only when typed" (mapcar #'first (path-completions "~/a/.s" :home home)) '(".secret"))
    (ok "case-insensitive fallback" (mapcar #'first (path-completions "~/a/ZD" :home home)) '("zdir"))
    (ok "Tab: unique folder completes with its slash" (complete-path-query "~/a/z" :home home) "~/a/zdir/")
    (ok "Tab: common prefix" (complete-path-query "~/a/fi" :home home) "~/a/file")
    (ok "Tab: nothing to add" (complete-path-query "~/a/file" :home home) "~/a/file")))
(ok "percent-decode UTF-8" (percent-decode "caf%C3%A9") (format nil "caf~c" (code-char 233)))
(ok "normalize-path" (normalize-path "/a//b/./c/../d/") "/a/b/d")

;;; 10. sniffing and archives (file-views.lisp), against files made by the real tools
(sh (format nil "cd '~a' && tar cf t.tar src && tar czf t.tgz src && gzip -c a/g.txt > g.gz && (zip -qr z.zip src || true)" *dir*))
(flet ((kind (f) (nth-value 0 (sniff (read-prefix (p f) 65536) f))))
  (ok "sniff tar" (kind "t.tar") :tar)
  (ok "sniff gzip" (kind "t.tgz") :gzip)
  (ok "sniff ELF" (nth-value 0 (sniff (read-prefix "/bin/sh" 4096) "sh")) :elf)
  (ok "sniff text" (kind "a/new.txt") :text))
(ok "tar members (no data read)"
    (sort (mapcar (lambda (m) (getf m :name))
                  (tar-members (lambda (off n) (read-prefix (p "t.tar") n :start off))
                               (fentry-size (file-entry (p "t.tar")))))
          #'string<)
    '("src/" "src/d/" "src/d/f" "src/d/lnk"))
(when (path-exists-p (p "z.zip"))
  (ok "zip members from the central directory"
      (sort (mapcar (lambda (m) (getf m :name)) (zip-members (p "z.zip") (fentry-size (file-entry (p "z.zip")))))
            #'string<)
      '("src/" "src/d/" "src/d/f" "src/d/lnk")))
(ok "gzip preview inflates" (view-kind (present (subj-file (p "g.gz")))) :reference)
;; a gzip bomb whose trailer LIES: 100 MB of zeros (~100 KB compressed) claiming to
;; inflate to 16 bytes. The preview must stop at its 64 MB cap, not trust the claim
(sh (format nil "cd '~a' && head -c 100M /dev/zero | gzip -c > bomb.gz && printf '\\020\\000\\000\\000' | dd of=bomb.gz bs=1 seek=$(( $(stat -c %s bomb.gz) - 4 )) conv=notrunc 2>/dev/null" *dir*))
(ok "a gzip bomb with a lying trailer stops at the cap"
    (and (search "output exceeds" (view-text (present (subj-file (p "bomb.gz"))))) t) t)
(ok "a text file opens in the editor" (view-kind (present (subj-file (p "a" "new.txt")))) :file)
(ok "... and previews read-only" (view-kind (present-preview (subj-file (p "a" "new.txt")))) :reference)

(delete-tree *dir*)
(ok "scratch removed" (path-exists-p *dir*) nil)
(format t "~%~:[all checks passed~;~:*~d FAILED~]~%" (and (plusp *failures*) *failures*))
(sb-ext:exit :code (if (plusp *failures*) 1 0))
