;;;; agent-store.lisp — a conversation survives power-off (doc/agent.org §12 phase 4).
;;;;
;;;;   ~/.lol/agent/conversations/20261016-141503-3.json      one file per conversation
;;;;
;;;; The image is a frozen heap in a RAM-only initramfs; a conversation is a CLOS object
;;;; in that heap, so it died with every power-off. The only part of the laptop's disk
;;;; we may write is /home (doc/storage.org), so a conversation is written there, under
;;;; ~/.lol/ beside the containers' state (~/.lol/ct) and the network log (~/.lol/log).
;;;;
;;;; WHAT IS SAVED is what it takes to CONTINUE the conversation, not only to read it:
;;;;   - the messages, every content block exactly as the API knows it — a thinking
;;;;     block's signature included, byte for byte, or the next request is refused;
;;;;   - the tools array as first sent (CONVERSATION-TOOLS): a thinking signature is
;;;;     bound to the request that produced it, tools included, so a reopened
;;;;     conversation must send the very same tools, not today's (§14.8½);
;;;;   - the turns (question, notes, failure, cost), the tool calls (input, result,
;;;;     summary), the plan — what the pane shows.
;;;; The format is JSON (lol.json), because the messages and tools already ARE JSON:
;;;; they go back out exactly as they came in, with no second mapping to get wrong.
;;;;
;;;; WHAT IS NOT SAVED:
;;;;   - the API key. It is never in a message (it travels in a header), every tool
;;;;     result has it taken out (REDACT-API-KEY, agent.lisp), and the file is checked
;;;;     once more before it is written: a file that would contain the key is not
;;;;     written at all. The endpoint is not saved either — it is a setting.
;;;;   - a tool call's SUBJECT (a live object: a class, a pane). The common ones are
;;;;     found again from the call's input when it is reopened (RESTORED-SUBJECT).
;;;;   - subagents: a subagent is a helper of one turn; its report is already in its
;;;;     parent's transcript.
;;;;
;;;; WHEN: at the end of every turn, however it ended (*TURN-END-HOOKS*). A turn still
;;;; running at power-off is lost; the conversation up to the turn before is not.
;;;;
;;;; The promises settings.lisp makes, made again here, because saving runs at the end
;;;; of every turn on the turn's thread and reading runs in the Spotter:
;;;;   * neither ever signals — a failure is a NIL and a line on *ERROR-OUTPUT*;
;;;;   * nothing is written without a real home (a diskless boot's HOME is /, a tmpfs);
;;;;   * a write is atomic (WRITE-FILE-ATOMICALLY: temp file + rename), and the file is
;;;;     the user's alone — 0600 in a 0700 folder: a conversation quotes whatever its
;;;;     tools read.
;;;;
;;;; REOPENING: the Spotter lists every saved conversation ("saved conversation: …",
;;;; and "saved conversations" for the whole list). Drilling one reopens it — it
;;;; becomes a live conversation again, in its own tab, and the next question goes on
;;;; from where it stopped, into the same file.
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

(defparameter +conversation-format+ "lol-agent-conversation"
  "The file's \"format\" field: a reader refuses any other.")
(defparameter +conversation-version+ 1)

;;; ===========================================================================
;;; Where
;;; ===========================================================================

(defun conversation-store-dir ()
  "~/.lol/agent/conversations, or NIL when there is no home on a disk (a diskless boot
   runs with HOME=/, which is the initramfs, gone at power-off)."
  (let ((home (ignore-errors (home-directory))))
    (when (and home (string/= home "/") (eq (followed-type home) :directory))
      (path-join home ".lol/agent/conversations"))))

(defun ensure-private-directory (path)
  "mkdir -p PATH, each folder this makes 0700 (one that exists is left as it is)."
  (let ((parent (path-parent path)))
    (when (and parent (string/= parent path) (not (path-exists-p parent)))
      (ensure-private-directory parent)))
  (unless (path-exists-p path)
    (multiple-value-bind (ok errno) (sb-unix:unix-mkdir path #o700)
      (unless (or ok (path-exists-p path)) (fs-fail "mkdir" path errno))))
  path)

(defvar *conversation-stores* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "CONVERSATION -> its store record, a plist: :FILE (its name in the store folder),
   :CREATED, and the spend of earlier sessions (:PRIOR-COST, :PRIOR-USAGE) — kept
   apart so the tray's session cost stays this session's. A side table rather than a
   slot, like *CONVERSATION-ACTIVITIES*: a running image takes this file without the
   CONVERSATION class being redefined under live conversations.")

(defun store-stamp (&optional (time (get-universal-time)) (sep ""))
  (multiple-value-bind (s mi h d mo y) (decode-universal-time time 0)
    (format nil "~4,'0d~2,'0d~2,'0d~a~2,'0d~2,'0d~2,'0d" y mo d (if (string= sep "") "-" sep) h mi s)))

(defun conversation-store (conv)
  "CONV's store record, made (with a new file name) on first use."
  (or (gethash conv *conversation-stores*)
      (setf (gethash conv *conversation-stores*)
            (list :file (format nil "~a-~d.json" (store-stamp) (conversation-id conv))
                  :created (store-stamp (get-universal-time) "T")
                  :prior-cost 0 :prior-usage nil))))

;;; ===========================================================================
;;; Lisp -> JSON
;;; ===========================================================================

(defun json-or-null (x) (if (null x) :null x))
(defun json-bool (x) (if x t :false))
(defun json-strings (list) (coerce (mapcar (lambda (x) (if (stringp x) x (princ-to-string x))) list) 'simple-vector))

(defun usage->json (u)
  "A usage plist (:input :output :cache-read :cache-write [:requests] [:cost]) as JSON."
  (let ((o (lol.json:jobj)))
    (loop for (k v) on u by #'cddr
          do (lol.json:jset o (substitute #\_ #\- (string-downcase (symbol-name k)))
                            (if (realp v) v :null)))
    o))

(defun json->usage (o)
  (and (lol.json:json-object-p o)
       (loop for (k . v) in (lol.json:json-object-pairs o)
             append (list (intern (string-upcase (substitute #\- #\_ k)) :keyword)
                          (if (realp v) v nil)))))

(defun add-usages (a b)
  "Two usage plists summed key by key (a NIL value is unknown, and stays unknown)."
  (let ((out (copy-list a)))
    (loop for (k v) on b by #'cddr
          do (let ((have (getf out k 0)))
               (setf (getf out k) (and (realp have) (realp v) (+ have v)))))
    out))

(defun conversation->json (conv)
  "CONV as the JSON object a file holds (see the file header). Under its lock, so a
   turn on another thread cannot change it halfway."
  (with-conversation-lock (conv)
    (let* ((store (conversation-store conv))
           (act (conversation-activity conv))
           (cost (let ((now (conversation-cost conv)) (prior (getf store :prior-cost)))
                   (and (realp now) (realp prior) (+ now prior)))))
      (lol.json:jobj
       "format" +conversation-format+ "version" +conversation-version+
       "title" (conversation-title conv)
       "model" (conversation-model conv)
       "authority" (json-or-null (and (conversation-authority conv)
                                      (string-downcase (symbol-name (conversation-authority conv)))))
       "created" (getf store :created)
       "saved" (store-stamp (get-universal-time) "T")
       "cost" (json-or-null cost)
       "usage" (usage->json (add-usages (getf store :prior-usage) (conversation-usage conv)))
       "tools" (json-or-null (conversation-tools-json conv))
       "messages" (map 'simple-vector
                       (lambda (m)
                         (lol.json:jobj "role" (message-role m)
                                        "content" (map 'simple-vector #'block-json (message-blocks m))))
                       (conversation-messages conv))
       "turns" (map 'simple-vector
                    (lambda (tu)
                      (let ((f (turn-failure tu)))
                        (lol.json:jobj
                         "question" (or (turn-question tu) "")
                         "start" (turn-start tu)
                         "notes" (json-strings (turn-notes tu))
                         "usage" (if (turn-usage tu) (usage->json (turn-usage tu)) :null)
                         "failure" (if f
                                       (lol.json:jobj "headline" (or (getf f :headline) "")
                                                      "details" (json-strings (getf f :details))
                                                      "advice" (json-strings (getf f :advice)))
                                       :null)
                         "partial" (or (turn-partial tu) "")
                         "again" (json-bool (turn-again tu)))))
                    (conversation-turns conv))
       "calls" (map 'simple-vector
                    (lambda (c)
                      (lol.json:jobj "id" (call-id c) "number" (call-number c) "name" (call-name c)
                                     "input" (json-or-null (call-input c))
                                     "raw_input" (json-or-null (call-raw-input c))
                                     "status" (string-downcase (symbol-name (call-status c)))
                                     "result" (json-or-null (call-result c))
                                     "summary" (json-or-null (call-summary c))))
                    (conversation-calls conv))
       "plan" (coerce (loop for (title . state) in (activity-plan act)
                            collect (lol.json:jobj "title" title "state" (string-downcase (symbol-name state))))
                      'simple-vector)))))

;;; ===========================================================================
;;; Saving
;;; ===========================================================================

(defun create-private-file (path)
  "Make PATH an empty file with mode 0600 if it does not exist, so that
   WRITE-FILE-ATOMICALLY — which gives its result the old file's mode — writes a file
   only the user can read."
  (unless (path-exists-p path)
    (multiple-value-bind (fd errno)
        (sb-unix:unix-open path (logior sb-unix:o_wronly sb-unix:o_creat sb-unix:o_excl) #o600)
      (if fd (sb-unix:unix-close fd) (unless (path-exists-p path) (fs-fail "create" path errno))))))

(defun save-conversation (conv)
  "Write CONV to its file in the store (see the file header). Returns the path, or NIL
   when there is no home, nothing to save, or the write failed — never signals."
  (handler-case
      (let ((dir (conversation-store-dir)))
        (when (and dir (plusp (length (conversation-turns conv))))
          (let* ((text (lol.json:to-json (conversation->json conv)))
                 (key *agent-api-key*))
            ;; the last line of defence: the key is in no message and no tool result,
            ;; but a file that would hold it is not written, whatever put it there
            (when (and key (>= (length key) 8) (search key text))
              (error "the conversation's text contains the API key; not saved"))
            (ensure-private-directory dir)
            (let ((path (path-join dir (getf (conversation-store conv) :file))))
              (create-private-file path)
              (write-file-atomically path (sb-ext:string-to-octets text :external-format :utf-8))
              path))))
    (serious-condition (c)
      ;; said in ~/.lol/log/session.log (session-log.lisp) when it is loaded — the
      ;; guest always has it; the host test does not
      (ignore-errors
        (if (fboundp 'log-condition)
            (funcall 'log-condition (format nil "agent-store: ~a not saved" (conversation-title conv)) c)
            (format *error-output* "~&agent-store: ~a not saved: ~a~%" (conversation-title conv) c)))
      nil)))

(defun save-after-turn (conv turn)
  "*TURN-END-HOOKS*: save CONV when a turn ends — not a subagent's (its report is in
   its parent's transcript)."
  (declare (ignore turn))
  (unless (activity-parent (conversation-activity conv))
    (save-conversation conv)))

(pushnew 'save-after-turn *turn-end-hooks*)

;;; ===========================================================================
;;; Reading the store
;;; ===========================================================================

(defun read-conversation-json (path)
  "The JSON object in PATH if it is a conversation file of a version we read, else NIL.
   Never signals."
  (ignore-errors
    (let ((o (lol.json:parse-json
              (with-open-file (s (native-path path) :external-format :utf-8)
                (let ((text (make-string (file-length s))))
                  (subseq text 0 (read-sequence text s)))))))
      (and (lol.json:json-object-p o)
           (equal (lol.json:jget o "format") +conversation-format+)
           (eql (lol.json:jget o "version") +conversation-version+)
           o))))

(defun saved-conversations ()
  "Every saved conversation, newest first, as plists (:file :path :title :saved :turns
   :cost). NIL on a diskless boot or with nothing saved. Never signals."
  (let ((dir (conversation-store-dir)))
    (when (and dir (eq (followed-type dir) :directory))
      (sort (loop for e in (ignore-errors (list-directory dir))
                  for name = (fentry-name e)
                  for path = (path-join dir name)
                  for o = (and (> (length name) 5) (string= ".json" name :start2 (- (length name) 5))
                               (read-conversation-json path))
                  when o
                    collect (list :file name :path path
                                  :title (or (lol.json:jget o "title") name)
                                  :saved (or (lol.json:jget o "saved") "")
                                  :turns (length (or (lol.json:jget o "turns") #()))
                                  :cost (let ((c (lol.json:jget o "cost"))) (and (realp c) c))))
            #'string> :key (lambda (s) (getf s :saved))))))

(defun live-conversation-for-file (file)
  "The conversation of this session already saved as FILE (reopened, or never closed)."
  (find-if (lambda (c) (equal (getf (gethash c *conversation-stores*) :file) file)) *conversations*))

;;; ===========================================================================
;;; JSON -> a live conversation
;;; ===========================================================================

(defun restored-subject (name input)
  "What a tool call was about, found again from its NAME and INPUT — the subject of a
   reopened call, so C-c C-o on it still opens something. Best effort; NIL if not."
  (ignore-errors
    (flet ((in (k) (lol.json:jget input k))
           (sym (k) (agent-find-symbol (lol.json:jget input k))))
      (cond ((member name '("lisp-source") :test #'string=) (let ((s (sym "name"))) (and s (subj-defn s))))
            ((string= name "lisp-senders") (let ((s (sym "name"))) (and s (subj-xref :senders s))))
            ((string= name "lisp-describe-class") (let ((s (sym "name"))) (and s (find-class s nil))))
            ((string= name "lisp-describe-gf") (let ((s (sym "name"))) (and s (fboundp s) (fdefinition s))))
            ((member name '("file-read" "file-write") :test #'string=) (subj-file (agent-path (in "path"))))
            ((string= name "file-list") (subj-dir (agent-path (or (in "path") "~/"))))
            ((string= name "lisp-modules") (if (in "module") nil (subj-modules)))))))

(defun json->conversation (o file)
  "A live CONVERSATION from the JSON object O (read from FILE in the store), added to
   *CONVERSATIONS* and made current. Its spend so far is recorded as prior, so this
   session's cost stays this session's."
  (let* ((authority (let ((a (lol.json:jget o "authority")))
                      (and (stringp a) (find a '(:read-only :ask :allow) :test #'string-equal))))
         (conv (make-instance 'conversation :id (incf *conversation-counter*)
                                            :title (or (lol.json:jget o "title") file)
                                            :model (or (lol.json:jget o "model") *agent-model*)
                                            :authority authority))
         (calls (make-hash-table :test 'equal)))
    ;; the tool calls first, so the tool_use blocks can be linked to them
    (loop for c across (or (lol.json:jget o "calls") #())
          do (let* ((input (let ((i (lol.json:jget c "input"))) (and (lol.json:json-object-p i) i)))
                    (call (make-instance 'tool-call :id (lol.json:jget c "id") :number (lol.json:jget c "number")
                                                    :name (lol.json:jget c "name") :input input
                                                    :raw-input (let ((r (lol.json:jget c "raw_input"))) (and (stringp r) r)))))
               (setf (call-status call) (or (find (lol.json:jget c "status") '(:pending :waiting :running :done :denied :error)
                                                  :test #'string-equal)
                                            :done)
                     (call-result call) (let ((r (lol.json:jget c "result"))) (and (stringp r) r))
                     (call-summary call) (let ((r (lol.json:jget c "summary"))) (and (stringp r) r))
                     (call-subject call) (and input (restored-subject (call-name call) input)))
               ;; a call cut off by the power-off never ran: say so, do not show it running
               (when (member (call-status call) '(:pending :waiting :running))
                 (setf (call-status call) :error (call-summary call) "not run (the session ended)"))
               (vector-push-extend call (conversation-calls conv))
               (setf (gethash (call-id call) calls) call)))
    (loop for m across (or (lol.json:jget o "messages") #())
          do (vector-push-extend
              (make-instance 'agent-message
                             :role (lol.json:jget m "role")
                             :blocks (map 'list (lambda (b)
                                                  (make-instance 'content-block :json b
                                                                 :call (and (equal (lol.json:jget b "type") "tool_use")
                                                                            (gethash (lol.json:jget b "id") calls))))
                                          (lol.json:jget m "content")))
              (conversation-messages conv)))
    (loop for tu across (or (lol.json:jget o "turns") #())
          do (let ((f (lol.json:jget tu "failure")))
               (vector-push-extend
                (make-turn :question (lol.json:jget tu "question") :prompt nil
                           :start (or (lol.json:jget tu "start") 0)
                           :notes (coerce (or (lol.json:jget tu "notes") #()) 'list)
                           :done t :origin :restored :announced t
                           :usage (json->usage (lol.json:jget tu "usage"))
                           :failure (and (lol.json:json-object-p f)
                                         (list :headline (lol.json:jget f "headline")
                                               :details (coerce (or (lol.json:jget f "details") #()) 'list)
                                               :advice (coerce (or (lol.json:jget f "advice") #()) 'list)))
                           :partial (or (lol.json:jget tu "partial") "")
                           :again (eq (lol.json:jget tu "again") t))
                (conversation-turns conv))))
    (let ((tools (lol.json:jget o "tools")))
      (when (vectorp tools) (setf (conversation-tools-json conv) tools)))
    (setf (activity-plan (conversation-activity conv))
          (loop for p across (or (lol.json:jget o "plan") #())
                collect (cons (lol.json:jget p "title")
                              (or (find (lol.json:jget p "state") '(:done :active :pending :failed) :test #'string-equal)
                                  :pending))))
    (setf (gethash conv *conversation-stores*)
          (list :file file :created (or (lol.json:jget o "created") (store-stamp (get-universal-time) "T"))
                :prior-cost (let ((c (lol.json:jget o "cost"))) (and (realp c) c))
                :prior-usage (json->usage (lol.json:jget o "usage"))))
    (touch conv "reopened")
    (push conv *conversations*)
    (setf *conversation* conv)))

(defun reopen-conversation (file)
  "The live conversation saved as FILE (a name in the store folder): the one already
   open in this session, else read back from the disk. NIL if it cannot be read."
  (or (live-conversation-for-file file)
      (let* ((dir (conversation-store-dir))
             (o (and dir (read-conversation-json (path-join dir file)))))
        (and o (json->conversation o file)))))

(defun delete-saved-conversation (file)
  "Remove FILE from the store (the live conversation, if open, stays — and is saved
   again, as a new file, at the end of its next turn)."
  (let ((dir (conversation-store-dir)))
    (when dir
      (delete-path (path-join dir file))
      (let ((live (live-conversation-for-file file)))
        (when live (remhash live *conversation-stores*)))
      t)))

;;; ===========================================================================
;;; Subjects: a saved conversation, and the list of them
;;; ===========================================================================

(defstruct (subj-saved-conversation (:constructor subj-saved-conversation (file &optional title)))
  "A conversation in the store. Drilling it REOPENS it: PRESENT returns the live
   conversation's own view (a Workspace whose data names the conversation, so every
   conversation key works in it). The preview — the nav split's look while the row is
   only selected — reads the file and loads nothing."
  file title)

(defstruct (subj-saved-conversations (:constructor subj-saved-conversations ()))
  "Every saved conversation, newest first.")

(defmethod present ((s subj-saved-conversation))
  (let ((conv (reopen-conversation (subj-saved-conversation-file s))))
    (if conv
        (present conv)
        (make-view :title (format nil "saved conversation · ~a" (subj-saved-conversation-file s))
                   :kind :reference :subject s :syntax :plain
                   :text (format nil ";; ~a could not be read (moved, or not a conversation file)."
                                 (subj-saved-conversation-file s))))))

(defmethod present-preview ((s subj-saved-conversation))
  (let* ((dir (conversation-store-dir))
         (o (and dir (read-conversation-json (path-join dir (subj-saved-conversation-file s))))))
    (make-view :title (format nil "saved · ~a" (or (and o (lol.json:jget o "title")) (subj-saved-conversation-file s)))
               :kind :reference :subject s :syntax :plain
               :text (if o
                         (format nil ";; ~a~%;; saved ~a · model ~a · ~a~%;; Enter reopens it: it goes on from where it stopped.~%~%~{;; ▸ ~a~%~}"
                                 (lol.json:jget o "title") (lol.json:jget o "saved") (lol.json:jget o "model")
                                 (let ((c (lol.json:jget o "cost"))) (if (realp c) (format-cost c) "cost unknown"))
                                 (map 'list (lambda (tu) (lol.widgets:clip-text (first-line (or (lol.json:jget tu "question") "")) 80))
                                      (lol.json:jget o "turns")))
                         ";; (unreadable)"))))

(defmethod commands-for ((s subj-saved-conversation))
  (list (make-command :label "Delete this saved conversation (the file)"
                      :action (lambda ()
                                (delete-saved-conversation (subj-saved-conversation-file s))
                                (when (boundp '*browser-status*)
                                  (setf (symbol-value '*browser-status*) "saved conversation deleted"))))))

(defmethod present ((s subj-saved-conversations))
  (let ((saved (saved-conversations)))
    (make-view :title "saved conversations" :kind :list :subject s
               :items (if saved
                          (mapcar (lambda (e)
                                    (make-item :label (getf e :title)
                                               :detail (format nil "~a · ~d turn~:p · ~a" (getf e :saved) (getf e :turns)
                                                               (if (getf e :cost) (format-cost (getf e :cost)) "?"))
                                               :kind :restart :group (or (conversation-store-dir) "")
                                               :subject (subj-saved-conversation (getf e :file) (getf e :title))))
                                  saved)
                          (list (make-item :label (if (conversation-store-dir) "none saved yet" "no home on this boot")
                                           :detail (if (conversation-store-dir)
                                                       "a conversation is saved at the end of each turn"
                                                       "a diskless boot has nowhere to keep them")))))))

;;; The Spotter finds the store and each saved conversation not already open. A named
;;; function, so loading this file again (C-c C-c, a reload) does not add a second source.
(defun saved-conversation-candidates ()
  (let ((saved (saved-conversations)))
    (cons (list :label "saved conversations" :detail (format nil "agent · ~d on disk" (length saved))
                :group "surfaces" :subject (subj-saved-conversations))
          (loop for e in saved
                unless (live-conversation-for-file (getf e :file))
                  collect (list :label (format nil "saved conversation: ~a" (getf e :title))
                                :detail (format nil "agent · saved ~a" (getf e :saved))
                                :group "surfaces"
                                :subject (subj-saved-conversation (getf e :file) (getf e :title)))))))

(pushnew 'saved-conversation-candidates *spotter-sources*)
