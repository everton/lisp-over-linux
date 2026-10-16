;;;; journal-test.lisp — host test for session persistence: the journal, its replay,
;;;; file-out and the desk (initramfs/journal.lisp, file-out.lisp, desk.lisp).
;;;;
;;;;   sbcl --script poc/journal-test.lisp
;;;;
;;;; These files write to the user's home on the laptop, so they are tested where a
;;;; mistake is cheap: everything goes to a scratch directory made fresh for the run
;;;; ($JOURNAL_TEST_DIR if set, else /tmp). HOME is pointed there too, and
;;;; *PERSIST-HOME* skips the "is there a disk?" checks that only the guest can pass.
;;;;
;;;; Checks, in the order the doc tells them (doc/session-persistence.org):
;;;;   - an Accept appends one record, stamped with the build and the right package
;;;;     (a lol.* edit lands in its package, §1.2), and fsync'd text reads back;
;;;;   - replay: replayed / failed (no package, unreadable, a compile error, a runaway
;;;;     form stopped by the timeout) / retired / pending / needs-rebuild, a failure
;;;;     never stopping the rest, and replay journalling nothing itself;
;;;;   - a torn last record: reading stops there, the tail is quarantined, the next
;;;;     Accept is readable;
;;;;   - replay-off; Accept-by-hand and drop of a pending entry; discard moves aside;
;;;;   - file-out: exactly-once replace leaving the rest byte-identical, a second run
;;;;     :already, a duplicated text and a born definition going to unfolded;
;;;;   - the desk: references out and back (a trail cut at an opaque subject, the
;;;;     Workspace text and point), no symbol interned by reading, autosave vs save vs
;;;;     leave-without-saving, the clipboard kept only when asked.
;;;;
;;;; Prints one line per check and exits non-zero on any failure.

(require :sb-introspect)
(require :sb-posix)
(require :sb-bsd-sockets)

(defvar *dir* (format nil "~a/lol-journal-test-~d/"
                      (string-right-trim "/" (or (sb-posix:getenv "JOURNAL_TEST_DIR") "/tmp"))
                      (sb-posix:getpid)))
(ensure-directories-exist *dir*)
(sb-posix:setenv "HOME" (string-right-trim "/" *dir*) 1)   ; SETTING reads $HOME

(defparameter *root* (merge-pathnames "../" (make-pathname :directory (pathname-directory *load-truename*))))

(let ((*load-verbose* nil))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "initramfs/registry.lisp" *root*))
    (dolist (m '("poc/deflate.lisp" "initramfs/session-log.lisp" "initramfs/model.lisp" "initramfs/present.lisp"
                 "initramfs/tour.lisp" "initramfs/examples.lisp" "initramfs/stage.lisp"
                 "poc/ttf.lisp" "poc/canvas.lisp" "poc/textview.lisp" "poc/widgets.lisp" "poc/morph.lisp"
                 "initramfs/ansi.lisp" "initramfs/line-editor.lisp" "poc/clipboard.lisp"
                 "initramfs/sysinfo.lisp" "poc/inflate.lisp" "poc/image.lisp" "initramfs/files.lisp"
                 "initramfs/settings.lisp" "initramfs/file-views.lisp" "poc/terminal.lisp"
                 "initramfs/pty.lisp" "poc/shell.lisp" "poc/file-manager.lisp" "initramfs/runs.lisp"
                 "initramfs/containers.lisp" "initramfs/process.lisp"))
      (funcall 'load-recording (merge-pathnames m *root*)))
    ;; the poll loop's service registry lives in fb-browser.lisp, which wants /dev/fb0;
    ;; the desk only registers with it, so a stand-in will do
    (unless (fboundp 'fb-register-service) (defun fb-register-service (&rest r) r))
    (dolist (m '("net" "dhcp" "dns" "sntp" "tls" "http" "wifi" "net-views"
                 "journal" "file-out" "desk"))
      (funcall 'load-recording (merge-pathnames (format nil "initramfs/~a.lisp" m) *root*)))))

(setf *log-dir* (concatenate 'string *dir* "log/"))
(setf *persist-home* (string-right-trim "/" *dir*))
(setf *build-stamp* "test-build-1")
(set-browser-font (lol.canvas:load-font (merge-pathnames "poc/cozette.lolf" *root*)))

(defvar *failures* 0)
(defun ok (name got want)
  (if (equal got want)
      (format t "  ok    ~a~%" name)
      (progn (incf *failures*) (format t "  FAIL  ~a~%        got  ~s~%        want ~s~%" name got want))))
(defun yes (name got) (ok name (and got t) t))

(defun journal-file () (journal-path))
(defun records () (read-journal))

;;; ---- 1. the write side ------------------------------------------------------------

(format t "~&== Accept appends ==~%")
(yes "persistence on (a test home)" (persistence-available-p))
(ok "the journal lives under ~/.local/share/lol"
    (journal-file) (concatenate 'string *dir* ".local/share/lol/changes.lisp"))
(accept-source "(defun zz-journal-a () 42)")
(let ((rs (records)))
  (ok "one record after one Accept" (length rs) 1)
  (ok "it carries the build stamp" (getf (first rs) :build) "test-build-1")
  (ok "its name, as a string" (getf (first rs) :name) "ZZ-JOURNAL-A")
  (ok "the file is 0600" (logand (nth-value 3 (sb-unix:unix-stat (journal-file))) #o777) #o600))
(accept-source (source-of 'lol.examples::make-counter :function) 'lol.examples::make-counter :function)
(ok "a lol.* edit is journalled in ITS package" (getf (second (records)) :package) "LOL.EXAMPLES")
(accept-source "(+ 1 2)")
(ok "a non-definition is not journalled" (length (records)) 2)
(ignore-errors (accept-source "(defun zz-broken ( "))
(ok "a failed Accept is not journalled" (length (records)) 2)

;;; ---- 2. replay ----------------------------------------------------------------------

(format t "~&== replay ==~%")
(defun rec (source &key (package "COMMON-LISP-USER") (build "test-build-1") (seq 100))
  (list :seq seq :time 0 :build build :package package :name "X" :kind :function :source source))

(let* ((before (length (records)))
       (+replay-timeout+ 2)
       (entries (replay-records
                 (list (rec "(defun zz-replayed () :back)")
                       (rec "(defun zz-nopkg () 1)" :package "NO-SUCH-PACKAGE")
                       (rec "(defun zz-unreadable ( ")
                       (rec "(defun zz-type-error () (car 5))")
                       (rec "(defparameter *zz-runaway* (loop))")
                       (rec (source-of 'zz-journal-a))
                       (rec "(defstruct zz-pt x y)")
                       (rec "(defun zz-pending () 1)" :build "some-other-build")
                       (rec "(defun zz-after () :still-ran)")))))
  (format t "~&        messages: ~s~%" (mapcar (function re-message) entries))
  (ok "statuses, in journal order" (mapcar #'re-status entries)
      '(:replayed :failed :failed :replayed :failed :retired :needs-rebuild :pending :replayed))
  (ok "the replayed function works" (funcall 'zz-replayed) :back)
  (ok "an entry AFTER failures still ran" (funcall 'zz-after) :still-ran)
  (yes "the runaway was stopped by the timeout" (search "longer than" (re-message (nth 4 entries))))
  (yes "the missing package is named" (search "NO-SUCH-PACKAGE" (re-message (nth 1 entries))))
  (ok "the struct was not defined" (find-class 'zz-pt nil) nil)
  (ok "the pending one was not defined" (fboundp 'zz-pending) nil)
  (ok "replay journalled nothing itself" (length (records)) before))

;;; ---- 3. a torn tail ----------------------------------------------------------------

(format t "~&== a torn last record ==~%")
(let ((good (records)))
  (with-open-file (o (journal-file) :direction :output :if-exists :append)
    (write-string "(:seq 99 :package \"COMMON-LISP-USER\" :source \"(defun zz-torn" o))
  (multiple-value-bind (rs stop) (records)
    (ok "reading keeps the whole records" (length rs) (length good))
    (yes "and says where it stopped" (getf stop :offset)))
  (setf *replay-report* nil)
  (replay-journal)
  (yes "the report names the stop" (getf *replay-report* :stop))
  (yes "the tail was quarantined" (getf *replay-report* :quarantined))
  (yes "changes-damaged.lisp has it"
       (search "zz-torn" (slurp-utf-8 (persist-path :data "changes-damaged.lisp"))))
  (ok "the journal reads clean again (its retired records gone too)" (nth-value 1 (records)) nil)
  (accept-source "(defun zz-after-torn () 1)")
  (ok "an Accept after the cut is readable" (getf (car (last (records))) :name) "ZZ-AFTER-TORN"))

;;; ---- 4. replay-off, pending by hand, discard -----------------------------------------

(format t "~&== replay-off, pending, discard ==~%")
(append-synced (persist-path :data "replay-off") "")
(setf *replay-report* nil)
(replay-journal)
(yes "replay-off is reported" (getf *replay-report* :disabled))
(ok "every entry skipped" (remove-duplicates (mapcar #'re-status (getf *replay-report* :entries))) '(:skipped))
(delete-path (persist-path :data "replay-off"))

(append-synced (journal-file) (print-record (rec "(defun zz-from-old-build () :old)" :build "older" :seq 500)))
(setf *replay-report* nil)
(replay-journal)
(let ((e (find "older" (getf *replay-report* :entries)
               :key (lambda (e) (getf (re-record e) :build)) :test #'equal)))
  (ok "an older build's record is pending" (re-status e) :pending)
  (journal-accept-entry e)
  (ok "Accepted by hand, it runs" (funcall 'zz-from-old-build) :old)
  (ok "the old record is gone, a new one stamped with THIS build"
      (mapcar (lambda (r) (getf r :build))
              (remove-if-not (lambda (r) (search "zz-from-old-build" (getf r :source))) (records)))
      '("test-build-1")))
(append-synced (journal-file) (print-record (rec "(defun zz-drop-me () 1)" :build "older" :seq 501)))
(setf *replay-report* nil)
(replay-journal)
(let ((e (find "older" (getf *replay-report* :entries)
               :key (lambda (e) (getf (re-record e) :build)) :test #'equal)))
  (journal-drop-entry e)
  (ok "a dropped record is gone" (find-if (lambda (r) (search "zz-drop-me" (getf r :source))) (records)) nil))
(yes "the journal view presents" (view-items (present (subj-journal))))
(discard-journal)
(ok "discard empties the journal" (length (records)) 0)
(yes "and kept the old one aside"
     (find-if (lambda (n) (search "changes-discarded-" n))
              (mapcar #'fentry-name (list-directory (path-parent (journal-file))))))

;;; ---- 5. file-out --------------------------------------------------------------------------

(format t "~&== file out ==~%")
(defparameter *module* (concatenate 'string *dir* "fixture.lisp"))
(defparameter *module-text*
  (format nil ";;;; fixture~%~%;; doubles~%(defun zz-fo-a (x)~%  (* 2 x))~%~%(defun zz-fo-b () :b)~%~%;; twins~%(defun zz-fo-twin () 1)~%"))
(with-open-file (o *module* :direction :output :if-exists :supersede) (write-string *module-text* o))
(load-recording *module*)
(accept-source "(defun zz-fo-a (x)
  (* 3 x))")
(accept-source "(defun zz-fo-born () :new)")
(let* ((results (file-out-changes :defns (list (find :function (gethash 'zz-fo-a *registry*) :key #'defn-kind)
                                               (first (gethash 'zz-fo-born *registry*)))))
       (after (slurp-utf-8 *module*)))
  (ok "statuses" (mapcar (lambda (r) (getf r :status)) results) '(:folded :unfolded))
  (ok "the file changed in exactly that one place"
      after (replace-once "(* 2 x))" "(* 3 x))" *module-text*))
  (yes "the born one is in changes-unfolded.lisp"
       (search "(defun zz-fo-born () :new)" (slurp-utf-8 (persist-path :data "changes-unfolded.lisp"))))
  (ok "a second file-out finds it already there"
      (getf (first (file-out-changes :defns (list (find :function (gethash 'zz-fo-a *registry*) :key #'defn-kind))
                                     :unfolded-path nil))
            :status)
      :already))
;; a file that has moved on: the build's text twice
(with-open-file (o *module* :direction :output :if-exists :append)
  (format o "~%;; twins~%(defun zz-fo-twin () 1)~%"))
(accept-source "(defun zz-fo-twin () 2)")
(let ((before (slurp-utf-8 *module*)))
  (ok "two copies of the build's text: left alone"
      (getf (first (file-out-changes :defns (gethash 'zz-fo-twin *registry*) :unfolded-path nil)) :status)
      :unfolded)
  (ok "and the file is untouched" (slurp-utf-8 *module*) before))

;;; ---- 6. the desk -------------------------------------------------------------------------

(format t "~&== the desk ==~%")
(setf *trails* nil *active* 0)
(open-environment (subj-workspace))
(let ((tv (pane-tv (current-pane))))
  (lol.textview:tv-set-text tv (format nil ";; my scratch~%(+ 1 2)~%"))
  (lol.textview:tv-move-point tv 1 3))
(new-trail (subj-modules))
(trail-push (subj-module "initramfs/examples"))
(trail-push (subj-defn 'lol.examples::make-counter :function))
(new-trail (subj-dir *dir*))
(new-trail (find-package :lol.examples))
(trail-push (subj-object (list 1 2)))            ; opaque: the trail is cut here
(new-trail (subj-object (make-hash-table)))      ; a whole trail of nothing describable
(setf *active* 2)
(yes "save-desk writes" (save-desk))
(let ((d (read-desk-file (desk-path))))
  (ok "four trails kept (the opaque one dropped)" (length (getf d :trails)) 4)
  (ok "a trail cut at its first opaque pane" (length (fourth (getf d :trails))) 1)
  (ok "the subject references"
      (mapcar (lambda (tr) (mapcar (lambda (p) (first (getf p :ref))) tr)) (getf d :trails))
      '((:workspace) (:modules :module :defn) (:dir) (:package)))
  (ok "the clipboard is not kept by default" (getf d :clipboard) nil))
(lol.clipboard:clip-copy "a secret")
(save-setting :desk-clipboard 5)
(autosave-desk)
(yes "the autosave was written" (path-exists-p (desk-autosave-path)))
(ok "and keeps the clipboard when asked" (getf (read-desk-file (desk-autosave-path)) :clipboard) '("a secret"))
;; "reboot": nothing open, restore once
(setf *trails* nil *active* 0 *desk-tried* nil *replay-report* nil)
(open-environment)
(ok "restored from the autosave (the session did not shut down)" *desk-restored-from* (desk-autosave-path))
(ok "the trails back" (mapcar (lambda (tr) (length tr)) *trails*) '(1 3 1 1))
(ok "the active tab back" *active* 2)
(let ((tv (pane-tv (first (first *trails*)))))
  (ok "the Workspace text" (lol.textview:tv-text tv) (format nil ";; my scratch~%(+ 1 2)~%"))
  (ok "the Workspace point" (list (lol.textview:tv-point-line tv) (lol.textview:tv-point-col tv)) '(1 3)))
(ok "the defn pane is the lol.examples one"
    (subj-defn-name (pane-subject (third (second *trails*)))) 'lol.examples::make-counter)
(setf *trails* nil *replay-report* nil)
(open-environment)
(ok "restore happens once per boot: then a fresh Workspace"
    (mapcar (lambda (tr) (type-of (pane-subject (first tr)))) *trails*) '(subj-workspace))
(setf *desk-tried* nil *trails* nil)
(leave-without-saving)
(ok "leaving without saving removes the autosave" (path-exists-p (desk-autosave-path)) nil)
(setf *trails* nil *desk-tried* nil)
(open-environment)
(ok "then the explicit save is what comes back" *desk-restored-from* (desk-path))
(ok "an unknown reference shape is refused" (desk-subject '(:eval "(run-program …)")) nil)
(ok "a missing symbol is not interned by reading the desk"
    (list (desk-subject '(:defn ("COMMON-LISP-USER" "ZZ-NEVER-INTERNED") :function))
          (nth-value 1 (find-symbol "ZZ-NEVER-INTERNED" :cl-user)))
    '(nil nil))
(ok "a vanished folder is refused" (desk-subject (list :dir (concatenate 'string *dir* "gone"))) nil)

;;; ---- done -----------------------------------------------------------------------------------

(sb-ext:run-program "rm" (list "-rf" *dir*) :search t)
(format t "~&~:[ALL PASS~;~:*~d FAILURE(S)~]~%" (and (plusp *failures*) *failures*))
(sb-ext:exit :code (if (zerop *failures*) 0 1))
