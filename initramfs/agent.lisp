;;;; agent.lisp — an LLM client that lives in the image: the conversation, the
;;;; Messages protocol, and a tool surface made of the image's own introspection.
;;;;
;;;; doc/agent.org is the design; this is its agent track (A0 + A1). The layers, from
;;;; the bottom (doc/agent.org §3):
;;;;
;;;;   transport   HTTP-REQUEST (initramfs/http.lisp, the networking track: https via
;;;;               the host's libssl, loaded at runtime from /mnt/host)
;;;;   wire        JSON (json.lisp, package LOL.JSON) and SSE (agent-wire.lisp)
;;;;   protocol    the Messages API, streamed: request shaping, the SSE events
;;;;               assembled back into content blocks, retries, cancellation, usage
;;;;   tools       §7 — LISP-SOURCE is SOURCE-OF, LISP-SENDERS is the compiler's xref,
;;;;               LISP-ACCEPT is the very ACCEPT-SOURCE that C-c C-c calls
;;;;
;;;; The conversation is a CLOS object (§6), so it can be driven from the REPL, from
;;;; a browser tab or from the debugger without any of them owning it, and so PRESENT
;;;; can show it (agent-ui.lisp). Writes are GATED, and the gate is a CONDITION with
;;;; RESTARTS (§8.4): whoever is around decides — the environment's chooser, the
;;;; REPL's numbered prompt, or a test's HANDLER-BIND — with no new mechanism.
;;;;
;;;; The two dials of §8.9 are special variables: *AGENT-INITIATIVE* and
;;;; *AGENT-AUTHORITY*. Bind them with LET for a dynamic extent, SETF them globally.
;;;;
;;;; CL-USER like the other initramfs modules (the tools call SOURCE-OF, SENDERS, … by
;;;; their plain names). doc/agent.org §13 had asked whether the agent should get its
;;;; own package so that LISP-APROPOS can skip it; it filters by FILE instead (the
;;;; registry knows which file every definition came from), which needs no package.
;;;;
;;;; THE KEY. Read at RUNTIME from ~/.config/lol/anthropic-api-key (on /home, so it
;;;; is never in the frozen heap build.sh saves — §11), kept in one special variable,
;;;; never printed, never logged, and sent ONLY to https://api.anthropic.com. Any
;;;; other endpoint (a mock server, a broker) gets no key at all.
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

;; LOL.JSON's names are written package-qualified (lol.json:jget …) rather than
;; imported: CL-USER is shared by every module, and another one defining its own JOBJ
;; there must not collide with — or silently redefine — ours.

;;; ===========================================================================
;;; Settings: special variables, overridable from ~/.config/lol/agent.lisp
;;; ===========================================================================

(defvar *agent-model* "claude-opus-5-5"
  "The model asked. Change it with SETF, or :MODEL in ~/.config/lol/agent.lisp.")
(defvar *agent-max-tokens* 32000
  "The cap on one reply — a ceiling, not a spend. Streaming makes a large one safe.")
(defvar *agent-effort* nil
  "output_config.effort: \"low\" \"medium\" \"high\" \"xhigh\" \"max\", or NIL for the
   model's default (medium on claude-opus-5-5).")
(defvar *agent-thinking-display* "summarized"
  "How thinking is returned: \"summarized\" (a readable summary, shown dim in the
   conversation) or \"omitted\" (nothing to show; the same thinking happens either way).")
(defvar *agent-endpoint* "https://api.anthropic.com"
  "Where requests go. A http:// endpoint (a mock server, the broker of doc/agent.org
   §4) is spoken to without TLS and is NEVER sent the API key.")
(defvar *agent-fallbacks* t
  "Ask for server-side fallbacks (fallbacks: \"default\"): if the model's safety
   classifiers decline a request, the API re-runs it on the model Anthropic
   recommends for that category instead of returning the refusal. Claude API only,
   and only for the models in +FALLBACK-MODELS+.")
(defvar *agent-retries* 6 "How many times a 429 / 5xx / overloaded reply, or a dropped or timed-out connection, is retried.")
(defvar *agent-connect-timeout* 15
  "Seconds a TCP connect to the endpoint may take before it counts as failed.")
(defvar *agent-wrap* 92 "Columns the conversation's prose is wrapped at.")
(defvar *agent-max-rounds* 25
  "Tool rounds one turn may take before it stops and says so — a runaway loop costs
   money, and on this machine nobody else is watching the meter.")

(defvar *agent-initiative* :reacting
  "§8.9's first dial — may the agent act unasked? :OFF (inert: no sockets, no tokens),
   :REACTING (it answers when asked: the default), :INTERACTING (it may offer on its
   own; nothing does so yet — the dial and its indicator exist so that when something
   does, it is already visible).")
(defvar *agent-authority* :ask
  "§8.9's second dial (§7.2's policy) — what may it touch? :READ-ONLY (write tools
   are not even offered), :ASK (each write is a TOOL-APPROVAL-REQUEST you answer: the
   default), :ALLOW (writes run).")

(defvar *agent-system-prompt*)   ; the text is defined at the end of this file

(defvar *agent-transport* nil
  "A function designator with HTTP-REQUEST's signature, or NIL for HTTP-REQUEST
   itself (initramfs/http.lisp). A test can put a fake here.")

(defparameter +fallback-models+ '("claude-opus-5-5" "claude-opus-5" "claude-fable-5-1" "claude-sonnet-5-5")
  "Models the server-side `fallbacks: \"default\"` form is offered for. Claude Haiku
   5.5 has no server-side fallback (a list there is a 400), so it is not here.")

(defparameter +agent-prices+
  ;; model              input  output  cache-read   ($ per million tokens)
  '(("claude-opus-5-5"   4.00  20.00  0.20)
    ("claude-opus-5"     5.00  25.00  0.50)
    ("claude-fable-5-1" 10.00  50.00  0.25)
    ("claude-sonnet-5-5" 2.00  10.00  0.20)
    ("claude-sonnet-5"   2.00  10.00  0.20)
    ("claude-haiku-5-5"  0.10   0.50  0.01)
    ("claude-opus-4-8"   5.00  25.00  0.50)
    ("claude-sonnet-4-6" 3.00  15.00  0.30)
    ("claude-haiku-4-5"  1.00   5.00  0.10))
  "What a token costs, per model, as of 2026-10-06 (the published first-party API
   rates). A cache WRITE costs 1.25x input (the 5-minute cache). A model missing here
   is still usable; its cost just shows as unknown.")

(defun home-path (relative)
  "RELATIVE under $HOME, or NIL when there is no real home (a diskless boot runs as
   root with HOME unset or /). Never hard-codes /home/<user>."
  (let ((home (ignore-errors (home-directory))))
    (and home (string/= home "/") (path-join home relative))))

(defun agent-key-path () (home-path ".config/lol/anthropic-api-key"))
(defun agent-settings-path () (home-path ".config/lol/agent.lisp"))

(defvar *agent-api-key* nil
  "The key, once read — in this one variable only, never in the saved image (it is
   read at runtime, after save-lisp-and-die), never printed.")

(defun last-report-line (c)
  "The last line of condition C's report — for a file error, strerror's own words."
  (let ((r (string-trim '(#\Space #\Newline) (or (ignore-errors (princ-to-string c)) ""))))
    (string-trim " " (subseq r (1+ (or (position #\Newline r :from-end t) -1))))))

(defun agent-api-key ()
  "The API key from ~/.config/lol/anthropic-api-key, or NIL. A file that exists but
   may not be read (wrong owner, mode 0000) is its own failure, said in words — never
   a bare FILE-ERROR, and never anything of the contents."
  (or *agent-api-key*
      (let ((path (agent-key-path)))
        (and path (probe-file (native-path path))
             (let ((k (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (handler-case (with-open-file (s (native-path path)) (read-line s nil ""))
                                     (error (c)
                                       (error 'agent-api-error :kind "key_unreadable"
                                              :message (format nil "~a exists but cannot be read (~a)"
                                                               path (last-report-line c))))))))
               (and (plusp (length k)) (setf *agent-api-key* k)))))))

(defun forget-agent-key ()
  "Drop the cached key (it is re-read from the file on the next request)."
  (setf *agent-api-key* nil) t)

(defparameter +agent-setting-vars+
  (flet ((one-of (&rest xs) (lambda (x) (member x xs :test #'equal))))
    (list (list :model '*agent-model* #'stringp)
          (list :max-tokens '*agent-max-tokens* #'integerp)
          (list :effort '*agent-effort* (one-of nil "low" "medium" "high" "xhigh" "max"))
          (list :thinking-display '*agent-thinking-display* (one-of "summarized" "omitted"))
          (list :endpoint '*agent-endpoint* #'stringp)
          (list :fallbacks '*agent-fallbacks* (one-of t nil))
          (list :wrap '*agent-wrap* #'integerp)
          (list :max-rounds '*agent-max-rounds* #'integerp)
          (list :initiative '*agent-initiative* (one-of :off :reacting :interacting))
          (list :authority '*agent-authority* (one-of :read-only :ask :allow))))
  "What ~/.config/lol/agent.lisp may set: key, the variable, and a check.")

(defvar *agent-settings-loaded* nil)

(defun load-agent-settings ()
  "Read ~/.config/lol/agent.lisp — ONE plist, e.g.
     (:model \"claude-sonnet-5-5\" :max-tokens 16000 :authority :ask)
   READ, never LOAD, and with *READ-EVAL* off: a settings file must not be able to run
   code (#. would). Unknown keys and ill-typed values are reported and skipped.
   Returns the list of what was set."
  (setf *agent-settings-loaded* t)
  (let ((path (agent-settings-path)) (set '()))
    (when (and path (probe-file (native-path path)))
      (let ((plist (with-open-file (s (native-path path))
                     (let ((*read-eval* nil) (*package* (find-package :keyword)))
                       (read s nil nil)))))
        (loop for (key value) on plist by #'cddr
              do (let ((entry (assoc key +agent-setting-vars+)))
                   (cond ((null entry)
                          (format *error-output* "~&agent.lisp: unknown setting ~s~%" key))
                         ((not (funcall (third entry) value))
                          (format *error-output* "~&agent.lisp: bad value ~s for ~s~%" value key))
                         (t (setf (symbol-value (second entry)) value)
                            (push key set)))))))
    (nreverse set)))

(defun save-agent-setting (key value)
  "Set KEY to VALUE in ~/.config/lol/agent.lisp, keeping every other setting there, so
   it holds on the next boot too. The file is READ (never LOADed) by
   LOAD-AGENT-SETTINGS; it is rewritten whole through a temporary file and a rename, so
   a crash leaves the old file or the new one, never half of one. Returns the plist."
  (unless (assoc key +agent-setting-vars+) (error "~s is not an agent setting" key))
  (let* ((path (native-path (agent-settings-path)))
         (tmp (native-path (concatenate 'string (agent-settings-path) ".tmp")))
         (plist (when (probe-file path)
                  (with-open-file (s path)
                    (let ((*read-eval* nil) (*package* (find-package :keyword)))
                      (read s nil nil))))))
    (setf (getf plist key) value)
    (ensure-directories-exist path)
    (with-open-file (s tmp :direction :output :if-exists :supersede)
      (let ((*package* (find-package :keyword)) (*print-readably* nil))
        (format s ";; The agent's settings: ONE plist, read (never loaded) at boot.~%")
        (format s ";; Keys and their checks: +AGENT-SETTING-VARS+ in agent.lisp.~%")
        (prin1 plist s) (terpri s)))
    (rename-file tmp path)
    plist))

(defun allow-all-writes (&key conversation save)
  "Stop asking: from now on every write tool runs without a TOOL-APPROVAL-REQUEST.
   Sets *AGENT-AUTHORITY* to :ALLOW for this session (and CONVERSATION's own authority,
   if it has one, which would otherwise win). With SAVE, also writes :AUTHORITY :ALLOW
   to ~/.config/lol/agent.lisp, so it holds after a reboot. The way back is
   ASK-BEFORE-WRITES (or SuperL-Space › agent › authority)."
  (setf *agent-authority* :allow)
  (when (and conversation (conversation-authority conversation))
    (setf (conversation-authority conversation) :allow))
  (when save (save-agent-setting :authority :allow))
  :allow)

(defun ask-before-writes (&key save)
  "Undo ALLOW-ALL-WRITES: every write tool asks again. With SAVE, also in
   ~/.config/lol/agent.lisp."
  (setf *agent-authority* :ask)
  (when save (save-agent-setting :authority :ask))
  :ask)

(defun ensure-agent-settings ()
  (unless *agent-settings-loaded* (ignore-errors (load-agent-settings))))

;;; ===========================================================================
;;; The conversation as a live object (§6)
;;; ===========================================================================

(defclass agent-message ()
  ((role :initarg :role :reader message-role)                 ; "user" | "assistant"
   (blocks :initarg :blocks :accessor message-blocks))       ; a list of CONTENT-BLOCK
  (:documentation "One message of the transcript, exactly as sent or received."))

(defclass content-block ()
  ((json :initarg :json :accessor block-json
         :documentation "The block as the API knows it — and as it is SENT BACK: an
          assistant's blocks are echoed unmodified (thinking signatures included), or
          the next request is refused.")
   (call :initarg :call :initform nil :accessor block-call
         :documentation "For a tool_use block: its TOOL-CALL, what the UI drills into."))
  (:documentation "A text, thinking, tool_use or tool_result block (or any type this
   client does not know, carried through untouched)."))

(defun block-type (b) (lol.json:jget (block-json b) "type"))

(defclass tool-call ()
  ((id :initarg :id :reader call-id)
   (number :initarg :number :reader call-number)          ; #N in the conversation, for C-c C-o
   (name :initarg :name :reader call-name)
   (input :initarg :input :accessor call-input)            ; a JSON-OBJECT (or NIL if unparseable)
   (raw-input :initarg :raw-input :initform nil :accessor call-raw-input)
   (status :initform :pending :accessor call-status)      ; :pending :waiting :running :done :denied :error
   (result :initform nil :accessor call-result)           ; the text sent back
   (subject :initform nil :accessor call-subject)         ; what it was about — a browser subject
   (summary :initform nil :accessor call-summary))        ; one line for the transcript
  (:documentation "A tool call made by the model: what was asked, what came back, and
   the SUBJECT it was about — so 'how do you know that?' is one keystroke (§8.6)."))

(defmethod print-object ((c tool-call) s)
  (print-unreadable-object (c s :type t)
    (format s "#~d ~a ~(~a~)" (call-number c) (call-name c) (call-status c))))

(defstruct turn
  "One exchange begun by a question: the messages from START on belong to it."
  question            ; what the transcript shows the user asked
  prompt              ; what was actually sent (the question plus any context)
  start               ; index of its first message in the conversation
  (notes '())         ; things worth saying that are not messages: retries, errors, 'stopped'
  (done nil)
  usage               ; the turn's usage plist, once done
  origin              ; who asked: a pane, :repl, :debugger … (the UI uses it)
  failure             ; NIL, or why the turn FAILED: (:headline :details :advice), in
                      ; words, from DESCRIBE-AGENT-FAILURE — the transcript's error block
  partial             ; the text the model had streamed when it failed ("" if none)
  again               ; T when this turn re-asks a failed one (C-c C-a on an empty draft)
  announced)          ; the UI has put the failure on the status line (agent-ui.lisp)

(defclass conversation ()
  ((id :initarg :id :reader conversation-id)
   (title :initarg :title :accessor conversation-title)
   (model :initarg :model :accessor conversation-model)
   (messages :initform (make-array 0 :adjustable t :fill-pointer 0) :accessor conversation-messages)
   (turns :initform (make-array 0 :adjustable t :fill-pointer 0) :accessor conversation-turns)
   (calls :initform (make-array 0 :adjustable t :fill-pointer 0) :accessor conversation-calls)
   (authority :initarg :authority :initform nil :accessor conversation-authority
              :documentation "NIL: follow *AGENT-AUTHORITY*. Else this conversation's own.")
   (tools :initform nil :accessor conversation-tools-json
          :documentation "The tools array as first sent, then sent unchanged (CONVERSATION-TOOLS).")
   (usage :initform (list :input 0 :output 0 :cache-read 0 :cache-write 0 :requests 0)
          :accessor conversation-usage)
   (cost :initform 0 :accessor conversation-cost)
   (state :initform :idle :accessor conversation-state)   ; :idle :working :waiting
   (status :initform "" :accessor conversation-status)
   (live :initform nil :accessor conversation-live)       ; the reply being streamed, or NIL
   (lock :initform (sb-thread:make-mutex :name "conversation") :reader conversation-lock)
   (revision :initform 0 :accessor conversation-revision) ; bumped on every visible change
   (thread :initform nil :accessor conversation-thread)
   (cancel :initform nil :accessor conversation-cancel)
   (jobs :initform '() :accessor conversation-jobs))       ; tool calls waiting for the UI thread
  (:documentation "A conversation with the model (doc/agent.org §6). Several coexist;
   *CONVERSATION* is the one a bare ? or Ask continues."))

(defmethod print-object ((c conversation) s)
  (print-unreadable-object (c s :type t)
    (format s "~d ~s ~(~a~), ~d turn~:p" (conversation-id c) (conversation-title c)
            (conversation-state c) (length (conversation-turns c)))))

(defvar *conversations* '() "Every conversation this session, newest first.")
(defvar *conversation* nil "The current conversation — what a bare ? continues.")
(defvar *conversation-counter* 0)

(defun make-conversation (&key title (model *agent-model*) authority)
  (let ((c (make-instance 'conversation :id (incf *conversation-counter*)
                                        :title (or title (format nil "conversation ~d" *conversation-counter*))
                                        :model model :authority authority)))
    (push c *conversations*)
    (setf *conversation* c)))

(defmacro with-conversation-lock ((conv) &body body)
  "BODY holding CONV's lock. Recursive, because a locked section may call TOUCH."
  `(sb-thread:with-recursive-lock ((conversation-lock ,conv)) ,@body))

(defun touch (conv &optional status)
  "Something visible changed: bump the revision the UI polls (and set the status)."
  (with-conversation-lock (conv)
    (when status (setf (conversation-status conv) status))
    (incf (conversation-revision conv))))

(defun conversation-authority* (conv)
  (or (and conv (conversation-authority conv)) *agent-authority*))

(defun conversation-busy-p (conv)
  (and conv (not (eq (conversation-state conv) :idle))))

(defun current-turn (conv)
  (let ((ts (conversation-turns conv)))
    (and (plusp (length ts)) (aref ts (1- (length ts))))))

;;; ===========================================================================
;;; Money: what a request cost
;;; ===========================================================================

(defun usage-cost (model input output cache-read cache-write)
  "Dollars for a request's token counts, or NIL for a model we have no price for."
  (let ((p (assoc model +agent-prices+ :test #'string=)))
    (when p
      (destructuring-bind (in out cr) (rest p)
        (/ (+ (* input in) (* output out) (* cache-read cr) (* cache-write in 1.25d0)) 1d6)))))

(defun add-usage (conv usage-json)
  "Fold one response's usage into the conversation's totals; returns the request's
   (values PLIST COST)."
  (let* ((in (or (lol.json:jget usage-json "input_tokens") 0))
         (out (or (lol.json:jget usage-json "output_tokens") 0))
         (cr (or (lol.json:jget usage-json "cache_read_input_tokens") 0))
         (cw (or (lol.json:jget usage-json "cache_creation_input_tokens") 0))
         (in (if (integerp in) in 0)) (cr (if (integerp cr) cr 0)) (cw (if (integerp cw) cw 0))
         (cost (usage-cost (conversation-model conv) in out cr cw)))
    (with-conversation-lock (conv)
      (let ((u (conversation-usage conv)))
        (incf (getf u :input) in) (incf (getf u :output) out)
        (incf (getf u :cache-read) cr) (incf (getf u :cache-write) cw)
        (incf (getf u :requests))
        (setf (conversation-usage conv) u))
      (when cost (incf (conversation-cost conv) cost)))
    (values (list :input in :output out :cache-read cr :cache-write cw) cost)))

(defun session-cost ()
  "Every conversation's spend this session, in dollars."
  (reduce #'+ *conversations* :key #'conversation-cost :initial-value 0))

(defun format-cost (dollars)
  (cond ((null dollars) "$?")
        ((< dollars 0.01) (format nil "$~,4f" dollars))
        (t (format nil "$~,2f" dollars))))

;;; ===========================================================================
;;; The tool surface (§7): a table of schemas over functions that already exist
;;; ===========================================================================

(defstruct agent-tool
  name description schema
  handler          ; (lambda (input call) ...) => result string; may set the call's subject
  write-p          ; a WRITE tool: gated by the authority dial
  available-p)     ; NIL, or a predicate: offer it only when true (container-run)

(defvar *agent-tools* '() "Every tool, in the order they are offered (that order is cached).")

(defmacro define-agent-tool (name (&key write available) description properties required
                             (input call) &body body)
  "Define a tool the model may call. PROPERTIES is a list of (NAME TYPE DESCRIPTION);
   REQUIRED names the ones it must give. BODY runs with INPUT (the JSON-OBJECT the
   model sent, already checked for the required strings) and CALL (the TOOL-CALL)."
  `(let ((tool (make-agent-tool
                :name ,name :description ,description
                :schema (lol.json:jobj "type" "object"
                              "properties" (apply #'lol.json:jobj
                                                  (loop for (n type doc) in ',properties
                                                        append (list n (lol.json:jobj "type" type "description" doc))))
                              "required" (coerce ',required 'simple-vector))
                :write-p ,write :available-p ,available
                :handler (lambda (,input ,call) (declare (ignorable ,input ,call)) ,@body))))
     (setf *agent-tools* (append (remove ,name *agent-tools* :key #'agent-tool-name :test #'string=)
                                 (list tool)))
     ,name))

(defun find-agent-tool (name) (find name *agent-tools* :key #'agent-tool-name :test #'string=))

(defun offered-tools (conv)
  "The tools this request offers: all of them, minus the write tools under
   :READ-ONLY (§7.2: not offered at all), minus any whose AVAILABLE-P says no."
  (remove-if (lambda (tool)
               (or (and (agent-tool-write-p tool) (eq (conversation-authority* conv) :read-only))
                   (and (agent-tool-available-p tool) (not (funcall (agent-tool-available-p tool))))))
             *agent-tools*))

(defun conversation-tools (conv)
  "The tools array CONV sends — computed on its FIRST request and then frozen.
   A thinking block's signature is bound to the request that produced it, tools
   included: send a different tools list later (the authority dial moved off
   :READ-ONLY, a tool's AVAILABLE-P changed its mind, C-c C-c on a DEFINE-AGENT-TOOL
   moved it to the end of *AGENT-TOOLS*) and the API refuses the whole conversation
   with a 400, \"Invalid `signature` in `thinking` block … The `tools` list differs\".
   So the list never changes under a conversation: what the dial and AVAILABLE-P say
   NOW is enforced when a call runs (GATED-RUN, EXECUTE-TOOL-CALL), and a new or
   redefined tool reaches the model in the next conversation."
  (or (conversation-tools-json conv)
      (setf (conversation-tools-json conv) (tools-json (offered-tools conv)))))

(defun tools-json (tools)
  "The request's tools array. EAGER-INPUT-STREAMING makes a long input (a whole
   definition for LISP-ACCEPT) stream as it is written instead of arriving in one
   burst — and so the client must check what it receives (CHECK-TOOL-INPUT). The LAST
   tool carries the cache breakpoint: tools + system are one cached prefix."
  (coerce (loop for (tool . more) on tools
                collect (let ((o (lol.json:jobj "name" (agent-tool-name tool)
                                       "description" (agent-tool-description tool)
                                       "input_schema" (agent-tool-schema tool)
                                       "eager_input_streaming" t)))
                          (unless more (lol.json:jset o "cache_control" (lol.json:jobj "type" "ephemeral")))
                          o))
          'simple-vector))

(defun check-tool-input (tool input)
  "NIL when INPUT satisfies TOOL's schema well enough to run, else a complaint. With
   eager streaming the API no longer validates the input for us."
  (cond ((not (lol.json:json-object-p input)) "the input is not a JSON object")
        (t (let ((props (lol.json:jget (agent-tool-schema tool) "properties")))
             (or (loop for r across (lol.json:jget (agent-tool-schema tool) "required")
                       unless (lol.json:jhas input r) return (format nil "missing ~s" r))
                 (loop for (k . v) in (lol.json:json-object-pairs input)
                       for want = (lol.json:jget (lol.json:jget props k) "type")
                       when (and want (not (case (intern (string-upcase want) :keyword)
                                             (:string (stringp v))
                                             (:integer (integerp v))
                                             (:boolean (member v '(t :false)))
                                             (:array (simple-vector-p v))
                                             (t t))))
                         return (format nil "~s should be a ~a" k want)))))))

(defun clip-result (string &optional (limit 20000))
  "A tool result capped at LIMIT characters, saying so when it was cut."
  (if (> (length string) limit)
      (format nil "~a~%… [truncated: ~d of ~d characters shown]" (subseq string 0 limit) limit (length string))
      string))

;;; ---- resolving a name the model typed, without interning anything ----------

(defun agent-find-symbol (name)
  "The symbol NAME denotes: \"pkg:sym\" / \"pkg::sym\", or a bare name looked up in
   CL-USER and then in every package. FIND-SYMBOL, never INTERN or READ: a model's
   typo must not litter the image with symbols. NIL if there is none."
  (let* ((name (string-trim " '#" name))
         (colon (position #\: name)))
    (flet ((up (s) (if (and (plusp (length s)) (char= (char s 0) #\|))
                       (string-trim "|" s)
                       (string-upcase s))))
      (if colon
          (let* ((pkg (find-package (up (subseq name 0 colon))))
                 (sym (up (string-left-trim ":" (subseq name colon)))))
            (and pkg (values (find-symbol sym pkg))))
          (let ((up (up name)))
            (or (multiple-value-bind (s status) (find-symbol up :cl-user) (and status s))
                (loop for p in (list-all-packages)
                      for s = (multiple-value-bind (s status) (find-symbol up p) (and status s))
                      when (and s (or (gethash s *registry*) (fboundp s) (find-class s nil) (boundp s)))
                        return s)))))))

(defun require-symbol (name)
  (or (agent-find-symbol name)
      (error "no symbol named ~a in this image (try lisp-apropos)" name)))

(defun captured-output (thunk)
  "What THUNK prints, as a string."
  (with-output-to-string (s)
    (let ((*standard-output* s) (*print-length* 50) (*print-level* 6))
      (funcall thunk))))

;;; ---- the paths the file tools may touch -----------------------------------

(defun agent-path (path &key for-write)
  "PATH, made absolute and checked: the file tools work under the user's home and
   nowhere else (doc/storage.org: /home is the user's; the rest of the root is the
   image's or the host's, read-only). Resolves symlinks before checking, so a link
   cannot lead a write out of /home."
  (let* ((home (home-directory))
         (abs (normalize-path (cond ((and (plusp (length path)) (char= (char path 0) #\/)) path)
                                    ((and (>= (length path) 2) (string= "~/" path :end2 2))
                                     (path-join home (subseq path 2)))
                                    (t (path-join home path)))))
         (real (or (ignore-errors (sb-unix:unix-realpath abs))
                   (let ((parent (ignore-errors (sb-unix:unix-realpath (path-parent abs)))))
                     (and parent (path-join parent (path-name abs))))
                   abs)))
    (unless (and (string/= home "/")
                 (or (string= real home)
                     (and (> (length real) (length home))
                          (string= home real :end2 (length home))
                          (char= (char real (length home)) #\/))))
      (error "~a is outside your home (~a); the file tools ~:[read~;write~] only under it"
             path home for-write))
    real))

;;; ---- read-only tools -------------------------------------------------------

(define-agent-tool "lisp-apropos" ()
  "Find definitions, packages and modules in this Lisp image whose names match a query
   (a fuzzy subsequence match, best first). Start here when you do not know a name."
  (("query" "string" "Part of a name, e.g. \"draw-al\" or \"fb\"")
   ("limit" "integer" "Most results to return (default 40)"))
  ("query")
  (input call)
  (let* ((limit (let ((l (lol.json:jget input "limit"))) (if (integerp l) (max 1 (min l 200)) 40)))
         (hits (spotter-filter (lol.json:jget input "query") (spotter-candidates) limit)))
    (setf (call-summary call) (format nil "~d match~:*~[es~;~:;es~]" (length hits)))
    (if hits
        (format nil "~{~a~%~}"
                (mapcar (lambda (h) (format nil "~a  — ~a" (getf h :label) (getf h :detail))) hits))
        "No matches. CL and SBCL builtins are not indexed here; lisp-describe works on any symbol.")))

(define-agent-tool "lisp-source" ()
  "The source text of a definition in this image. Our own code comes from the registry
   (the exact text that was compiled, including any edit accepted since boot); a CL/SBCL
   builtin is described, with SBCL's source read from disk when it is there."
  (("name" "string" "The name, e.g. \"draw-alien\" or \"lol.canvas:draw-string\""))
  ("name")
  (input call)
  (let* ((sym (require-symbol (lol.json:jget input "name")))
         (defns (definitions-of sym)))
    (setf (call-subject call) (subj-defn sym))
    (if defns
        (progn
          (setf (call-summary call) (format nil "~d definition~:p" (length defns)))
          (format nil "~{~a~^~%~%~}"
                  (mapcar (lambda (d)
                            (format nil ";; ~(~a~) ~s~@[ ~a~] — from ~a~%~a"
                                    (defn-kind d) (defn-name d) (defn-label d)
                                    (ignore-errors (file-namestring (defn-file d))) (defn-source d)))
                          defns)))
        (progn (setf (call-summary call) "builtin")
               (captured-output (lambda () (show-reference sym)))))))

(define-agent-tool "lisp-senders" ()
  "Who uses a name, from the compiler's cross-reference database (exact, macro
   expansions included — not a text search). relation: senders (callers, default),
   referrers (readers of a variable), binders, setters, macro-users."
  (("name" "string" "The name")
   ("relation" "string" "senders | referrers | binders | setters | macro-users"))
  ("name")
  (input call)
  (let* ((sym (require-symbol (lol.json:jget input "name")))
         (rel (or (lol.json:jget input "relation") "senders"))
         (fn (cdr (assoc rel '(("senders" . senders) ("referrers" . referrers) ("binders" . binders)
                               ("setters" . setters) ("macro-users" . macro-users))
                         :test #'string=))))
    (unless fn (error "unknown relation ~a" rel))
    (let ((names (remove-duplicates (funcall fn sym) :test #'equal)))
      (setf (call-subject call) (subj-xref (if (string= rel "senders") :senders :referrers) sym)
            (call-summary call) (format nil "~d ~a" (length names) rel))
      (if names
          (format nil "~{~(~s~)~%~}" names)
          (format nil "No ~a of ~(~s~) recorded." rel sym)))))

(define-agent-tool "lisp-describe-class" ()
  "A class: its kind, superclasses, subclasses, precedence list, slots, and the methods
   specialised on it."
  (("name" "string" "The class name"))
  ("name")
  (input call)
  (let* ((sym (require-symbol (lol.json:jget input "name")))
         (info (or (class-info sym) (error "~(~s~) is not a class" sym))))
    (setf (call-subject call) (find-class sym) (call-summary call) "class")
    (with-output-to-string (s)
      (let ((*print-case* :downcase))
        (format s "class ~s (~a)~@[~%doc: ~a~]~%supers: ~s~%subs: ~s~%precedence: ~s~%slots:~%"
                (getf info :name) (getf info :kind) (getf info :doc)
                (getf info :supers) (getf info :subs) (getf info :cpl))
        (dolist (sl (getf info :all-slots)) (format s "  ~s~%" sl))
        (format s "methods specialised on it:~%")
        (dolist (m (ignore-errors (methods-on-class sym)))
          (format s "  ~s~%" (list (getf m :name) (getf m :qualifiers) (getf m :specializers))))))))

(define-agent-tool "lisp-describe-gf" ()
  "A generic function: lambda list, documentation, and every method with its
   qualifiers and specializers."
  (("name" "string" "The generic function's name"))
  ("name")
  (input call)
  (let* ((sym (require-symbol (lol.json:jget input "name")))
         (info (or (gf-info sym) (error "~(~s~) is not a generic function" sym))))
    (setf (call-subject call) (fdefinition sym)
          (call-summary call) (format nil "~d method~:p" (length (getf info :methods))))
    (with-output-to-string (s)
      (let ((*print-case* :downcase))
        (format s "generic ~s ~s~@[~%doc: ~a~]~%methods:~%"
                (getf info :name) (getf info :lambda-list) (getf info :doc))
        (dolist (m (getf info :methods))
          (format s "  ~s ~s~%" (getf m :qualifiers) (getf m :specializers)))))))

(define-agent-tool "lisp-describe" ()
  "CL's DESCRIBE of a symbol: what it names (function, macro, variable and its value,
   class), its lambda list and documentation. Works for builtins too."
  (("name" "string" "The symbol's name"))
  ("name")
  (input call)
  (let ((sym (require-symbol (lol.json:jget input "name"))))
    (setf (call-subject call) sym (call-summary call) "describe")
    (clip-result (captured-output (lambda () (describe sym))) 8000)))

(define-agent-tool "lisp-modules" ()
  "The image's modules (the source files it was built from) with how many definitions
   each holds; with a module name, the definitions in it."
  (("module" "string" "Optional: a module, e.g. \"initramfs/net\" or \"poc/shell\""))
  ()
  (input call)
  (let ((groups (categorize :file)) (want (lol.json:jget input "module")))
    (if want
        (let ((g (find want groups :key (lambda (g) (string-downcase (princ-to-string (car g))))
                                   :test #'string-equal)))
          (unless g (error "no module ~a (call lisp-modules with no argument for the list)" want))
          (setf (call-subject call) (subj-module (car g))
                (call-summary call) (format nil "~d definitions" (length (cdr g))))
          (format nil "~{~(~s~)~%~}" (cdr g)))
        (progn
          (setf (call-subject call) (subj-modules) (call-summary call) (format nil "~d modules" (length groups)))
          (format nil "~{~a~%~}"
                  (mapcar (lambda (g) (format nil "~(~a~)  ~d" (car g) (length (cdr g)))) groups))))))

(define-agent-tool "file-read" ()
  "Read a text file under the user's home directory (the laptop's /home). Paths may be
   absolute, ~/relative, or relative to home. Returns at most 64 KB from OFFSET."
  (("path" "string" "The file")
   ("offset" "integer" "Byte to start at (default 0)"))
  ("path")
  (input call)
  (let* ((path (agent-path (lol.json:jget input "path")))
         (offset (let ((o (lol.json:jget input "offset"))) (if (integerp o) (max 0 o) 0))))
    (unless (eq (followed-type path) :regular) (error "~a is not a regular file" path))
    (let* ((size (fentry-size (file-entry path)))
           (bytes (read-prefix path 65536 :start offset))
           (text (octets->text bytes :truncated (< (+ offset (length bytes)) size))))
      (setf (call-subject call) (subj-file path)
            (call-summary call) (format nil "~d of ~d bytes" (length bytes) size))
      (cond ((null text) (format nil "~a is binary (~d bytes); not shown." path size))
            ((< (+ offset (length bytes)) size)
             (format nil "~a~%… [~d more bytes; read again with offset ~d]" text
                     (- size offset (length bytes)) (+ offset (length bytes))))
            (t text)))))

(define-agent-tool "file-list" ()
  "List a directory under the user's home: name, type, size, modification time."
  (("path" "string" "The directory (default: home)"))
  ()
  (input call)
  (let* ((path (agent-path (or (lol.json:jget input "path") "~/")))
         (entries (sort-entries (list-directory path) :by :name)))
    (setf (call-subject call) (subj-dir path)
          (call-summary call) (format nil "~d entr~:@p" (length entries)))
    (clip-result
     (format nil "~a~%~{~a~%~}" path
             (mapcar (lambda (e)
                       (format nil "~a~:[~;/~]  ~(~a~)  ~a  ~a" (fentry-name e) (entry-dir-p e)
                               (fentry-type e) (human-size (fentry-size e)) (format-mtime (fentry-mtime e))))
                     entries)))))

;;; ---- write tools: gated (§7.2, §8.4) --------------------------------------

(defun read-in-package (text package-name)
  "Read the first form of TEXT in PACKAGE-NAME (default CL-USER) with *READ-EVAL* off:
   #. inside a proposed form would run at READ time, before anyone approved it."
  (let ((*package* (or (and package-name (find-package (string-upcase package-name)))
                       (find-package :cl-user)))
        (*read-eval* nil))
    (read-from-string text)))

(define-agent-tool "lisp-eval" (:write t)
  "Evaluate one Lisp form in the running image and return its values and anything it
   printed. A WRITE: the user approves each call unless they have allowed writes."
  (("form" "string" "One form, as text")
   ("package" "string" "Package to read it in (default CL-USER)"))
  ("form")
  (input call)
  (let* ((form (read-in-package (lol.json:jget input "form") (lol.json:jget input "package")))
         (out (make-string-output-stream))
         (values (let ((*standard-output* out) (*trace-output* out))
                   (multiple-value-list (eval form))))
         (printed (get-output-stream-string out)))
    (setf (call-summary call) (format nil "=> ~a" (lol.widgets:clip-text
                                                  (let ((*print-length* 8) (*print-level* 3))
                                                    (format nil "~{~s~^, ~}" values))
                                                  50)))
    (when (and values (not (typep (first values) '(or number string symbol))))
      (setf (call-subject call) (first values)))   ; drill: inspect the value
    (clip-result (let ((*print-length* 200) (*print-level* 8))
                   (format nil "~@[printed:~%~a~%~]values: ~{~s~^, ~}"
                           (and (plusp (length printed)) printed) values)))))

(define-agent-tool "lisp-accept" (:write t)
  "Compile a top-level definition (defun, defmethod, defclass, defvar, …) into the
   running image and record it in the change set — exactly what the user's C-c C-c
   does. The new source replaces the old in the registry, so lisp-source shows it next."
  (("source" "string" "The complete definition, as text"))
  ("source")
  (input call)
  (let ((text (lol.json:jget input "source")))
    ;; ACCEPT-SOURCE reads the form itself; read it once here first with #. disabled
    (let ((*read-eval* nil)) (read-from-string text))
    (let ((name (let ((*read-eval* nil)) (accept-source text))))
      (setf (call-subject call) (if (symbolp name) (subj-defn name) (subj-change-set))
            (call-summary call) (format nil "accepted ~(~a~)" name))
      (format nil "Accepted ~(~s~): compiled into the image and recorded in the change set." name))))

(define-agent-tool "file-write" (:write t)
  "Write a text file under the user's home, replacing it atomically (a temp file and a
   rename, so a crash leaves the old or the new file, never half of one)."
  (("path" "string" "The file")
   ("content" "string" "The complete new contents"))
  ("path" "content")
  (input call)
  (let* ((path (agent-path (lol.json:jget input "path") :for-write t))
         (octets (sb-ext:string-to-octets (lol.json:jget input "content") :external-format :utf-8)))
    (write-file-atomically path octets)
    (setf (call-subject call) (subj-file path) (call-summary call) (format nil "wrote ~d bytes" (length octets)))
    (format nil "Wrote ~d bytes to ~a." (length octets) path)))

(define-agent-tool "container-run" (:write t :available (lambda () (fboundp 'run-in-container)))
  "Run a program inside the user's container (the laptop's own Debian, with the network;
   see doc/containers.org) and return its exit code and output. argv is the program
   and its arguments, e.g. [\"ruby\", \"-v\"]; no shell unless you run one."
  (("argv" "array" "Program and arguments")
   ("cwd" "string" "Working directory (default: home)"))
  ("argv")
  (input call)
  (let* ((argv (coerce (lol.json:jget input "argv") 'list))
         (out (make-string-output-stream))
         (proc (progn (unless (and argv (every #'stringp argv)) (error "argv must be strings"))
                      (funcall 'run-in-container argv :cwd (lol.json:jget input "cwd") :output out :wait t)))
         (code (ignore-errors (sb-ext:process-exit-code proc))))
    (setf (call-summary call) (format nil "exit ~a" code))
    (clip-result (format nil "exit ~a~%~a" code (get-output-stream-string out)))))

(defun write-tool-p (name) (let ((tool (find-agent-tool name))) (and tool (agent-tool-write-p tool))))

;;; ===========================================================================
;;; Approval is a condition, not a dialog (§8.4)
;;; ===========================================================================

(define-condition tool-approval-request (condition)
  ((call :initarg :call :reader approval-call)
   (conversation :initarg :conversation :reader approval-conversation))
  (:report (lambda (c s)
             (format s "The agent wants to run ~a" (call-headline (approval-call c))))))

(defun call-headline (call)
  "One line naming a call and its main argument."
  (let* ((in (call-input call))
         (arg (or (lol.json:jget in "form") (lol.json:jget in "name") (lol.json:jget in "path") (lol.json:jget in "query")
                  (let ((src (lol.json:jget in "source"))) (and src (first-line src)))
                  (let ((argv (lol.json:jget in "argv"))) (and argv (format nil "~{~a~^ ~}" (coerce argv 'list)))))))
    (format nil "~a~@[ ~a~]" (call-name call) (and arg (lol.widgets:clip-text (substitute #\Space #\Newline arg) 70)))))

(defun first-line (s) (subseq s 0 (or (position #\Newline s) (length s))))

(defun declined (reason)
  (format nil "Declined by the user — not run.~@[ Reason: ~a~]" reason))

(defun gated-run (conv call thunk)
  "Run THUNK (the tool's work) as the authority dial allows. Under :ASK this SIGNALS a
   TOOL-APPROVAL-REQUEST with three restarts — ALLOW, DENY, EDIT — and whoever handles
   it decides: the environment's chooser, the REPL's numbered prompt, a test's
   HANDLER-BIND. Nobody handling it means nobody was there to say yes, so it is
   declined; a denial goes back to the model as a result, never as an error (§7.2)."
  (ecase (conversation-authority* conv)
    (:allow (funcall thunk))
    (:read-only (setf (call-status call) :denied)
     (declined "the agent is read-only (authority :read-only)"))
    (:ask
     (setf (call-status call) :waiting)
     (touch conv (format nil "waiting for approval: ~a" (call-headline call)))
     (restart-case
         (progn (signal 'tool-approval-request :call call :conversation conv)
                (setf (call-status call) :denied)
                (declined "no one was there to approve it"))
       (allow ()
         :report "Allow — run it"
         (setf (call-status call) :running)
         (funcall thunk))
       (deny (&optional reason)
         :report "Deny — tell the agent no"
         :interactive (lambda () (list (prompt-for-reason)))
         (setf (call-status call) :denied)
         (declined (and reason (plusp (length reason)) reason)))
       (edit (new-text)
         :report "Edit the input first, then run it"
         :interactive (lambda () (list (prompt-for-reason "New input: ")))
         (let ((key (editable-input-key call)))
           (unless key (error "this call has no single text input to edit"))
           (lol.json:jset (call-input call) key new-text))
         (setf (call-status call) :running)
         (funcall thunk))))))

(defun editable-input-key (call)
  "The input field EDIT rewrites: the form, the source, or the content."
  (find-if (lambda (k) (lol.json:jhas (call-input call) k)) '("form" "source" "content")))

(defun prompt-for-reason (&optional (prompt "Reason (optional): "))
  (format *query-io* "~&~a" prompt) (finish-output *query-io*)
  (read-line *query-io* nil ""))

;;; ===========================================================================
;;; Running one tool call
;;; ===========================================================================

(defun execute-tool-call (conv call)
  "Do CALL: check its input, pass a write through the gate, run it, and return the
   tool_result block. Every failure becomes an is_error result the model can read and
   recover from — a tool error must never take down the turn, let alone PID 1."
  (let* ((tool (find-agent-tool (call-name call)))
         (input (call-input call)))
    (flet ((result (text &optional error)
             (setf (call-result call) text
                   (call-status call) (cond (error :error)
                                            ((eq (call-status call) :denied) :denied)
                                            (t :done)))
             (unless (call-summary call)
               (setf (call-summary call) (if error "error" (first-line text))))
             (lol.json:jobj "type" "tool_result" "tool_use_id" (call-id call)
                   "content" text "is_error" (if error t :false))))
      (cond
        ((null tool) (result (format nil "There is no tool named ~a." (call-name call)) t))
        ((and (agent-tool-available-p tool) (not (funcall (agent-tool-available-p tool))))
         ;; offered when the conversation began, gone since (CONVERSATION-TOOLS)
         (result (format nil "The tool ~a is not available in this image right now." (call-name call)) t))
        ((null input)
         ;; eager streaming hands us the raw fragments; if they never became JSON, say
         ;; so in the form the API's guidance asks for, so the model can retry
         (result (lol.json:to-json (lol.json:jobj "INVALID_JSON" (or (call-raw-input call) ""))) t))
        ((check-tool-input tool input)
         (result (format nil "Invalid input: ~a." (check-tool-input tool input)) t))
        (t
         (handler-case
             (let ((thunk (lambda () (funcall (agent-tool-handler tool) input call))))
               (result (clip-result
                        (if (agent-tool-write-p tool) (gated-run conv call thunk) (funcall thunk)))))
           (serious-condition (c)
             (result (format nil "Error: ~a" (or (ignore-errors (princ-to-string c)) (type-of c))) t))))))))

;;; How a call reaches the thread that runs it. In the REPL a call runs where the turn
;;; runs. In the environment the turn streams on its own thread (so the screen keeps
;;; moving, §8.7), but TOOLS run on the environment's thread: the image's state is
;;; touched by one thread at a time, and an approval has a screen to ask on. The turn
;;; posts a JOB and waits; the environment's frame tick (agent-ui.lisp) runs it.

(defvar *tool-executor* nil
  "NIL: run tool calls in the calling thread. Else a function (CONV CALL THUNK) that
   arranges for THUNK to run somewhere and returns its value.")

(defstruct agent-job conv call thunk gated
  (sem (sb-thread:make-semaphore :count 0)) result done)

(defun post-job-and-wait (conv call thunk)
  "Hand THUNK to the environment's thread and wait for it, staying cancellable."
  (let ((job (make-agent-job :conv conv :call call :thunk thunk
                             :gated (and (write-tool-p (call-name call))
                                         (eq (conversation-authority* conv) :ask)))))
    (with-conversation-lock (conv)
      (setf (conversation-jobs conv) (append (conversation-jobs conv) (list job))))
    (touch conv)
    (unwind-protect
         (loop until (agent-job-done job)
               do (sb-thread:wait-on-semaphore (agent-job-sem job) :timeout 0.2)
                  (check-cancel conv))
      (with-conversation-lock (conv)
        (setf (conversation-jobs conv) (remove job (conversation-jobs conv)))))
    (agent-job-result job)))

(defun run-job (job)
  "Run a posted job (on the environment's thread) and wake its turn."
  (unwind-protect
       (setf (agent-job-result job)
             (execute-tool-call (agent-job-conv job) (agent-job-call job)))
    (setf (agent-job-done job) t)
    (sb-thread:signal-semaphore (agent-job-sem job))))

;;; ===========================================================================
;;; The protocol: one streamed request
;;; ===========================================================================

(define-condition agent-api-error (error)
  ((status :initarg :status :initform nil :reader api-error-status)
   (kind :initarg :kind :initform nil :reader api-error-kind)        ; "overloaded_error" …
   (message :initarg :message :initform "" :reader api-error-message)
   (retry-after :initarg :retry-after :initform nil :reader api-error-retry-after))
  (:report (lambda (c s)
             (format s "API ~@[~a ~]~@[~a: ~]~a" (api-error-status c) (api-error-kind c)
                     (api-error-message c)))))

(define-condition agent-not-ready (error)
  ((failure :initarg :failure :reader agent-not-ready-failure))
  (:report (lambda (c s) (write-string (getf (agent-not-ready-failure c) :headline) s)))
  (:documentation "The preflight's verdict: this request cannot work, and here is why
   (a FAILURE plist). Never retried — nothing will change in two seconds."))

(defun retryable-p (c)
  "Worth trying again: rate limits, server errors, overload, a dropped connection, a
   TLS read that timed out or broke. Not worth it: a 400 (our request is wrong), a
   401/403 (the key is), a certificate that does not verify, no OpenSSL at all."
  (cond
    ((typep c 'agent-api-error)
     (and (or (member (api-error-status c) '(408 409 429 500 502 503 504 529))
              (member (api-error-kind c) '("overloaded_error" "api_error" "rate_limit_error"
                                           "incomplete")    ; a stream cut mid-answer
                      :test #'equal))
          t))
    ((typep c 'agent-not-ready) nil)
    ;; a certificate that does not verify will not verify in two seconds either, nor
    ;; will a missing OpenSSL appear; but "timed out waiting for …" and a read or
    ;; write that broke mid-stream are the network, and the network comes back
    ((condition-of-type-p c 'tls-error)
     (let ((text (or (ignore-errors (princ-to-string c)) "")))
       (not (some (lambda (s) (search s text))
                  '("certificate" "OpenSSL" "HTTPS:" "no host system" "SSL_set1_host")))))
    ((and (condition-of-type-p c 'dns-error) (search "NXDOMAIN" (princ-to-string c))) nil)
    (t t)))

(defun condition-of-type-p (c type-name)
  "(TYPEP C TYPE-NAME), for a condition class a LATER module defines (dns.lisp, tls.lisp
   and http.lisp load after this file) — false while that class does not exist."
  (and (find-class type-name nil) (typep c type-name)))

(defun check-cancel (conv)
  (when (conversation-cancel conv) (throw 'agent-cancel :cancelled)))

(defun anthropic-endpoint-p (url)
  "Is URL the real API, over TLS? The ONE place the key may go."
  (multiple-value-bind (scheme host) (agent-parse-url url)
    (and (string= scheme "https") (string-equal host "api.anthropic.com"))))

(defun request-headers (url)
  (let ((h (list (cons "content-type" "application/json")
                 (cons "anthropic-version" "2023-06-01")
                 (cons "accept" "text/event-stream"))))
    (when (and *agent-fallbacks* (member *agent-model* +fallback-models+ :test #'string=))
      (push (cons "anthropic-beta" "server-side-fallback-2026-07-01") h))
    (when (anthropic-endpoint-p url)
      (let ((key (agent-api-key)))
        (unless key
          (error 'agent-api-error :kind "no_api_key"
                 :message (format nil "no API key: put it in ~a (mode 0600)"
                                  (or (agent-key-path) "~/.config/lol/anthropic-api-key — and this boot has no /home"))))
        (push (cons "x-api-key" key) h)))
    (reverse h)))

(defun transport ()
  (or *agent-transport*
      (if (fboundp 'http-request)
          'http-request
          (error 'agent-api-error :kind "no_transport"
                                  :message "no HTTP client in this image (initramfs/http.lisp)"))))

(defun messages-json (conv)
  (coerce (loop for m across (conversation-messages conv)
                collect (lol.json:jobj "role" (message-role m)
                              "content" (coerce (mapcar #'block-json (message-blocks m)) 'simple-vector)))
          'simple-vector))

(defun request-body (conv)
  "The request, field by field in a FIXED order: the API caches by exact bytes, so the
   same conversation must serialise identically every time. Cache breakpoints: the
   last tool (tools + system are one stable prefix) and the top-level automatic one,
   which caches the conversation so far."
  (let ((body (lol.json:jobj "model" (conversation-model conv)
                    "max_tokens" *agent-max-tokens*
                    "stream" t
                    "system" (vector (lol.json:jobj "type" "text" "text" *agent-system-prompt*
                                           "cache_control" (lol.json:jobj "type" "ephemeral")))
                    "tools" (conversation-tools conv)
                    "messages" (messages-json conv)
                    "thinking" (lol.json:jobj "type" "adaptive" "display" *agent-thinking-display*)
                    "cache_control" (lol.json:jobj "type" "ephemeral"))))
    (when *agent-effort* (lol.json:jset body "output_config" (lol.json:jobj "effort" *agent-effort*)))
    (when (and *agent-fallbacks* (member (conversation-model conv) +fallback-models+ :test #'string=))
      (lol.json:jset body "fallbacks" "default"))
    body))

;;; A reply arrives as events. We rebuild its content blocks from them:
;;;   content_block_start  {index, content_block}   a block begins (its type, its id)
;;;   content_block_delta  {index, delta}           text_delta / thinking_delta /
;;;                                                 signature_delta / input_json_delta
;;;   content_block_stop   {index}                  it is complete
;;;   message_start / message_delta                 the envelope, stop_reason, usage
;;;   error                                         e.g. overloaded_error, mid-stream

(defstruct live-block index json type (text (make-string-output-stream)) (shown "")
  (signature (make-string-output-stream)) (partial (make-string-output-stream)) done)

(defstruct live-reply (blocks '()) stop-reason stop-details usage model)

(defun live-text (lb)
  "The text streamed into a live block so far (kept readable for the UI)."
  (live-block-shown lb))

(defun handle-event (conv reply name data)
  (declare (ignore name))
  (let* ((ev (lol.json:parse-json data))
         (type (lol.json:jget ev "type")))
    (flet ((block-at (i) (find i (live-reply-blocks reply) :key #'live-block-index)))
      (cond
        ((equal type "message_start")
         (setf (live-reply-usage reply) (lol.json:jget* ev "message" "usage")
               (live-reply-model reply) (lol.json:jget* ev "message" "model")))
        ((equal type "content_block_start")
         (let* ((cb (lol.json:jget ev "content_block"))
                (lb (make-live-block :index (lol.json:jget ev "index") :json cb :type (lol.json:jget cb "type"))))
           (with-conversation-lock (conv)
             (setf (live-reply-blocks reply) (append (live-reply-blocks reply) (list lb))))
           (touch conv)))
        ((equal type "content_block_delta")
         (let ((lb (block-at (lol.json:jget ev "index"))) (d (lol.json:jget ev "delta")))
           (when lb
             (let ((dt (lol.json:jget d "type")))
               (cond ((equal dt "text_delta")
                      (write-string (lol.json:jget d "text") (live-block-text lb))
                      (with-conversation-lock (conv)
                        (setf (live-block-shown lb) (concatenate 'string (live-block-shown lb) (lol.json:jget d "text")))))
                     ((equal dt "thinking_delta")
                      (write-string (lol.json:jget d "thinking") (live-block-text lb))
                      (with-conversation-lock (conv)
                        (setf (live-block-shown lb) (concatenate 'string (live-block-shown lb) (lol.json:jget d "thinking")))))
                     ((equal dt "signature_delta") (write-string (lol.json:jget d "signature") (live-block-signature lb)))
                     ((equal dt "input_json_delta") (write-string (lol.json:jget d "partial_json") (live-block-partial lb)))))
             (touch conv))))
        ((equal type "content_block_stop")
         (let ((lb (block-at (lol.json:jget ev "index")))) (when lb (setf (live-block-done lb) t)) (touch conv)))
        ((equal type "message_delta")
         (setf (live-reply-stop-reason reply) (lol.json:jget* ev "delta" "stop_reason")
               (live-reply-stop-details reply) (lol.json:jget* ev "delta" "stop_details"))
         ;; message_delta's usage is CUMULATIVE: it supersedes message_start's counts
         ;; field by field (output_tokens always; the input counts when present)
         (let ((u (lol.json:jget ev "usage")))
           (when (lol.json:json-object-p u)
             (let ((merged (or (live-reply-usage reply) (lol.json:jobj))))
               (loop for (k . v) in (lol.json:json-object-pairs u) do (lol.json:jset merged k v))
               (setf (live-reply-usage reply) merged)))))
        ((equal type "error")
         (error 'agent-api-error :status 200 :kind (lol.json:jget* ev "error" "type")
                                 :message (or (lol.json:jget* ev "error" "message") "error event")))
        (t nil)))))                          ; ping, message_stop, anything new

(defun finish-block (conv lb)
  "A live block, complete, as the CONTENT-BLOCK we keep and echo back."
  (let* ((json (make-instance 'lol.json:json-object :pairs (copy-alist (lol.json:json-object-pairs (live-block-json lb)))))
         (type (live-block-type lb)))
    (cond
      ((equal type "text") (lol.json:jset json "text" (get-output-stream-string (live-block-text lb)))
       (make-instance 'content-block :json json))
      ((equal type "thinking")
       (lol.json:jset json "thinking" (get-output-stream-string (live-block-text lb)))
       (let ((sig (get-output-stream-string (live-block-signature lb))))
         (when (plusp (length sig)) (lol.json:jset json "signature" sig)))
       (make-instance 'content-block :json json))
      ((equal type "tool_use")
       (let* ((raw (get-output-stream-string (live-block-partial lb)))
              (input (cond ((zerop (length raw)) (or (lol.json:jget json "input") (lol.json:jobj)))
                           (t (ignore-errors (lol.json:parse-json raw)))))
              (call (make-instance 'tool-call :id (lol.json:jget json "id") :name (lol.json:jget json "name")
                                              :number (length (conversation-calls conv))
                                              :input input :raw-input raw)))
         (vector-push-extend call (conversation-calls conv))
         ;; the echoed block carries the input as an object; an unparseable input is
         ;; sent back as {} with the error in the tool_result
         (lol.json:jset json "input" (if (lol.json:json-object-p input) input (lol.json:jobj)))
         (make-instance 'content-block :json json :call call)))
      (t (make-instance 'content-block :json json)))))   ; redacted_thinking, fallback, …

(defun stream-message (conv)
  "One streamed request. Returns (values BLOCKS STOP-REASON USAGE STOP-DETAILS).
   While it streams, (CONVERSATION-LIVE CONV) is the LIVE-REPLY the UI draws."
  (let* ((url (concatenate 'string (string-right-trim "/" *agent-endpoint*) "/v1/messages"))
         (headers (request-headers url))
         (body (lol.json:to-json (request-body conv)))
         (reply (make-live-reply))
         (raw (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (parser (make-sse-parser (lambda (name data) (handle-event conv reply name data)))))
    (with-conversation-lock (conv) (setf (conversation-live conv) reply))
    (multiple-value-bind (status resp-headers)
        (funcall (transport) url :method :post :headers headers :body body
                 :on-chunk (lambda (octets)
                             (check-cancel conv)
                             (when (< (fill-pointer raw) 65536)  ; kept only to report an error body
                               (loop for b across octets do (vector-push-extend b raw)))
                             (sse-feed parser octets))
                 ;; 300 s between two reads (a long thought streams nothing for a
                 ;; while); but a CONNECT that has not happened in 15 s will not —
                 ;; without this, a black-holed address held the turn on "thinking..."
                 ;; for the kernel's two minutes of SYN retries, per attempt
                 :timeout 300 :connect-timeout *agent-connect-timeout*)
      (when (eql status 401) (forget-agent-key))   ; the file may be fixed before the retry
      (unless (eql status 200)
        (let* ((text (ignore-errors (sb-ext:octets-to-string raw :external-format :utf-8)))
               (err (ignore-errors (lol.json:parse-json text))))
          (error 'agent-api-error :status status
                                  :kind (lol.json:jget* err "error" "type")
                                  :message (or (lol.json:jget* err "error" "message")
                                               (and text (lol.widgets:clip-text text 200)) "")
                                  :retry-after (let ((ra (header-value resp-headers "retry-after")))
                                                 (and ra (ignore-errors (parse-integer ra :junk-allowed t)))))))
      (sse-finish parser)
      (unless (live-reply-stop-reason reply)
        (error 'agent-api-error :kind "incomplete" :message "the stream ended before the message did"))
      (values (mapcar (lambda (lb) (finish-block conv lb)) (live-reply-blocks reply))
              (live-reply-stop-reason reply)
              (or (live-reply-usage reply) (lol.json:jobj))
              (live-reply-stop-details reply)))))

(defun interruptible-sleep (conv seconds)
  (loop repeat (ceiling (* seconds 10)) do (check-cancel conv) (sleep 0.1)))

(defun note (conv text)
  "Add a line to the current turn's notes (what the transcript says besides messages)."
  (let ((turn (current-turn conv)))
    (with-conversation-lock (conv)
      (when turn (setf (turn-notes turn) (append (turn-notes turn) (list text)))))
    (touch conv)
    (agent-listen :note text)))

;;; ===========================================================================
;;; When it fails: say so, in words, where you asked
;;; ===========================================================================
;;;
;;; A turn that fails must say WHY in the conversation itself — not in a status line
;;; you may not be looking at, not as a condition's raw report. On the laptop, with
;;; DHCP failed, a question once sat on "thinking..." with nothing else said: the turn
;;; was spending a 300 s socket timeout per connect attempt, five attempts with
;;; backoff, before its first word of explanation. So: (1) a PREFLIGHT that reads the
;;; routing table, the lease and the key file — sending nothing — and fails at once,
;;; in plain words, when the request cannot possibly work; (2) a short connect
;;; timeout; (3) every failure turned into a FAILURE plist — a headline, what we know,
;;; what to do — which agent-ui.lisp draws as a red block and the REPL prints in red.

(defun failure (headline &key details advice)
  "A FAILURE plist: HEADLINE one sentence; DETAILS and ADVICE strings or lists of them."
  (flet ((lst (x) (remove nil (if (listp x) x (list x)))))
    (list :headline headline :details (lst details) :advice (lst advice))))

(defun endpoint-host ()
  (or (ignore-errors (nth-value 1 (agent-parse-url *agent-endpoint*))) "the API"))

(defun key-file-name ()
  (or (agent-key-path) "~/.config/lol/anthropic-api-key (and this boot has no /home)"))

(defun offline-reason ()
  "Why this machine has no way out, from the lease table — one clause."
  (let ((lease (and (fboundp 'current-lease) (funcall 'current-lease))))
    (if lease
        (format nil "~a has a DHCP lease, but no default route goes through it" (funcall 'lease-iface lease))
        "DHCP got no lease, so there is no IPv4 address (Wi-Fi not connected, or no cable)")))

(defun ipv4-route-to-p (octets)
  "Is there an IPv4 route covering OCTETS (a vector or list of 4)? Loopback always is."
  (let ((a (coerce octets 'list)))
    (or (= (first a) 127)
        (some (lambda (r)
                (let ((dest (funcall 'ipv4-octets (getf r :dest)))
                      (mask (funcall 'ipv4-octets (getf r :mask))))
                  (every (lambda (x m d) (= (logand x m) d)) a mask dest)))
              (funcall 'ipv4-routes)))))

(defun network-failure (host)
  "Why HOST cannot be reached from here — a FAILURE — or NIL when nothing obvious is
   wrong. Cheap: reads /proc/net/route, the lease and resolv.conf; sends nothing.
   Without net.lisp (a host test that never loaded it) it judges nothing."
  (when (fboundp 'ipv4-routes)
    (let* ((literal (or (and (fboundp 'parse-ip-address) (funcall 'parse-ip-address host))
                        (first (and (fboundp 'dns-override) (funcall 'dns-override host)))
                        (first (and (fboundp 'hosts-file-lookup) (ignore-errors (funcall 'hosts-file-lookup host))))))
           (advice (list "Connect first: SuperL-n opens the network view (Wi-Fi networks, the lease)."
                         "Then press C-c C-a here with nothing typed: it asks the same question again.")))
      (cond
        ((and literal (= (length literal) 4))
         (unless (ipv4-route-to-p literal)
           (failure "No network: there is no route to the API."
                    :details (let ((dotted (format nil "~{~d~^.~}" (coerce literal 'list))))
                               (format nil "Nothing in the routing table reaches ~a~:[ (~a)~;~*~]: ~a."
                                       host (string= host dotted) dotted (offline-reason)))
                    :advice advice)))
        (literal nil)                                   ; an IPv6 literal: not judged
        (t (hostname-network-failure host advice))))))

(defun hostname-network-failure (host advice)
  "NETWORK-FAILURE for a NAME: it needs a way out AND a DNS server we can reach. The
   servers are IPv4 (dns.lisp asks over v4), so an IPv6 route alone — a router's
   advertisement can give one while DHCP fails — does not make a name resolvable."
  (let* ((v4 (funcall 'ipv4-default-route))
         (v6 (and (fboundp 'ipv6-default-route) (funcall 'ipv6-default-route)))
         (servers (and (fboundp 'dns-servers) (funcall 'dns-servers)))
         (dotted (mapcar (lambda (o) (format nil "~{~d~^.~}" (coerce o 'list))) servers)))
    (cond
      ((not (or v4 v6))
       (failure "No network: this machine is not connected."
                :details (format nil "There is no default route, so ~a cannot be reached: ~a."
                                 host (offline-reason))
                :advice advice))
      ((and (fboundp 'dns-servers) (null servers))
       (if v4
           (failure (format nil "No DNS server, so ~a cannot be looked up." host)
                    :details "No DHCP lease named a DNS server, and /etc/resolv.conf lists none."
                    :advice advice)
           (failure "No network: this machine has no IPv4 connection."
                    :details (format nil "There is no IPv4 default route: ~a. An IPv6 route exists, but no DNS server is known to look ~a up with."
                                     (offline-reason) host)
                    :advice advice)))
      ((and servers (notany #'ipv4-route-to-p servers))
       (failure "No network: the DNS server cannot be reached."
                :details (format nil "No route leads to ~{~a~^, ~}, so ~a cannot be looked up: ~a."
                                 dotted host (offline-reason))
                :advice advice)))))

(defun agent-preflight ()
  "Signal AGENT-NOT-READY, with every reason found, when a request to the endpoint
   cannot work: no key for api.anthropic.com, no network, no HTTPS library. Sends
   nothing and costs a few file reads — run before every turn's first request."
  (let* ((url (concatenate 'string (string-right-trim "/" *agent-endpoint*) "/v1/messages"))
         (host (endpoint-host))
         (problems '()))
    (let ((net (network-failure host)))
      (when net (push net problems)))
    (when (anthropic-endpoint-p url)
      (handler-case
          (unless (agent-api-key)
            (push (failure "No API key."
                           :details (format nil "The agent reads its key from ~a, and that file ~:[does not exist~;is empty~]."
                                            (key-file-name)
                                            (and (agent-key-path) (probe-file (native-path (agent-key-path)))))
                           :advice (list (format nil "Put your key on one line in that file (chmod 600) — a key comes from console.anthropic.com.")
                                         "Then press C-c C-a here with nothing typed: it asks the same question again."))
                  problems))
        (agent-api-error (c)
          (push (failure "The API key file cannot be read."
                         :details (api-error-message c)
                         :advice "Check the file's owner and mode: it must be yours, readable (chmod 600).")
                problems))))
    (when (and (string-equal (or (ignore-errors (agent-parse-url url)) "") "https") (fboundp 'ensure-openssl))
      (handler-case (funcall 'ensure-openssl)
        (error (c) (push (describe-agent-failure c) problems))))
    (when problems
      (setf problems (nreverse problems))
      ;; the first is the headline; the others ride along as details, so fixing the
      ;; network does not merely uncover the missing key one attempt later
      (error 'agent-not-ready
             :failure (if (rest problems)
                          (list :headline (getf (first problems) :headline)
                                :details (append (getf (first problems) :details)
                                                 (loop for p in (rest problems)
                                                       append (cons (format nil "Also: ~a" (getf p :headline))
                                                                    (getf p :details))))
                                ;; every fix first, how to ask again last
                                :advice (stable-sort (remove-duplicates (loop for p in problems append (getf p :advice))
                                                                        :test #'string= :from-end t)
                                                     #'< :key (lambda (a) (if (search "C-c C-a" a) 1 0))))
                          (first problems))))))

(defun api-failure (c host)
  "An AGENT-API-ERROR — an HTTP status from the API, an error event, a stream that
   stopped — as a FAILURE."
  (let* ((status (api-error-status c))
         (kind (api-error-kind c))
         (said (let ((m (api-error-message c)))
                 (and m (plusp (length m)) (format nil "The API said: ~a" m))))
         (again "Press C-c C-a here with nothing typed to ask the same question again."))
    (cond
      ((equal kind "no_api_key")
       (failure "No API key." :details (api-error-message c)
                              :advice (list "Put your key on one line in that file (chmod 600)." again)))
      ((equal kind "key_unreadable")
       (failure "The API key file cannot be read." :details (api-error-message c)
                :advice "Check the file's owner and mode: it must be yours, readable (chmod 600)."))
      ((equal kind "no_transport")
       (failure "This image has no HTTP client." :details (api-error-message c)))
      ((equal kind "incomplete")
       (failure "The answer was cut off: the stream stopped before the message ended."
                :details (format nil "The connection to ~a dropped mid-answer (Wi-Fi?), and the retries did not get through either." host)
                :advice again))
      ((eql status 401)
       (failure "The API refused the key (401 authentication error)."
                :details (list said (format nil "The key was read from ~a." (key-file-name)))
                :advice (list "Check the key in that file: a typo, a revoked key, or a key from another account. It is re-read on the next try."
                              again)))
      ((eql status 403)
       (failure "The API refused this request (403 permission error)." :details said
                :advice "The key is valid but not allowed this — the model, or the account's limits."))
      ((eql status 404)
       (failure "The API does not know this model or endpoint (404)."
                :details (list said (format nil "Model ~s, endpoint ~a." *agent-model* *agent-endpoint*))
                :advice "Check the model and endpoint: SuperL-Space › agent shows both, C-c C-c on a row sets it."))
      ((eql status 413)
       (failure "The conversation is too large for one request (413)." :details said
                :advice "Start a new conversation: SuperL-Space › agent › New conversation."))
      ((eql status 400)
       (failure "The API rejected the request (400)." :details said
                :advice (list "A setting (model, max tokens, effort) or the conversation's state is something the API will not take."
                              "A new conversation gets past the second (SuperL-Space › agent › New conversation); if it repeats there, it is a bug here.")))
      ((or (eql status 429) (equal kind "rate_limit_error"))
       (failure "Rate limited (429): too many requests or tokens for now." :details said
                :advice (list (format nil "Retried ~d times already. Wait a minute." *agent-retries*) again)))
      ((or (eql status 529) (equal kind "overloaded_error"))
       (failure (if (eql status 200)
                    "The API is overloaded: it broke off the answer."
                    "The API is overloaded (529).")
                :details (list said "Nothing is wrong here: Anthropic's servers are busy.")
                :advice (list (format nil "Retried ~d times already. Try again in a minute or two." *agent-retries*) again)))
      ((and status (<= 500 status 599))
       (failure (format nil "The API had a server error (~d)." status) :details said
                :advice (list "Nothing is wrong here. Try again in a little while." again)))
      ((eql status 200)
       (failure (format nil "The API broke off the answer with an error~@[ (~a)~]." kind) :details said
                :advice again))
      (t
       (failure (format nil "The API answered with an error~@[ (HTTP ~d)~]~@[: ~a~]." status kind)
                :details said :advice again)))))

(defun describe-agent-failure (c)
  "Condition C, from anywhere under a turn, as a FAILURE: (:headline :details :advice),
   in words a person can act on. The one place the agent's failures are explained."
  (let ((host (endpoint-host))
        (text (or (ignore-errors (princ-to-string c)) (string (type-of c))))
        (again "Press C-c C-a here with nothing typed to ask the same question again."))
    (cond
      ((typep c 'agent-not-ready) (agent-not-ready-failure c))
      ((typep c 'agent-api-error) (api-failure c host))
      ((condition-of-type-p c 'dns-error)
       (failure (format nil "Could not look up ~a (DNS)." host)
                :details text
                :advice (list (if (search "NXDOMAIN" text)
                                  "The name does not exist: check the endpoint (SuperL-Space › agent)."
                                  "The DNS server did not answer: the network may be down or half up (SuperL-n shows it).")
                              again)))
      ((condition-of-type-p c 'tls-error)
       (cond ((or (search "OpenSSL" text) (search "HTTPS:" text) (search "no host system" text))
              (failure "HTTPS is not available on this boot." :details text
                       :advice "HTTPS uses the laptop's own OpenSSL from /mnt/host, which this boot does not have (doc/storage.org)."))
             ((search "certificate" text)
              (failure (format nil "The secure connection to ~a was refused: its certificate did not verify." host)
                       :details text
                       :advice (list "If the clock is wrong, certificates look expired or not yet valid: the network view (SuperL-n) can set it by SNTP."
                                     again)))
             (t (failure (format nil "The secure connection to ~a failed." host) :details text :advice again))))
      ((condition-of-type-p c 'http-error)
       (cond ((search "cannot connect" text)
              (failure (format nil "Could not connect to ~a." host) :details text
                       :advice (list "The network may be down, or something between here and there refuses the connection (SuperL-n shows the network)."
                                     again)))
             ((search "timed out" text)
              (failure (format nil "~a stopped answering (timeout)." host) :details text :advice again))
             ((or (search "closed" text) (search "chunk" text))
              (failure "The answer was cut off: the connection closed mid-answer." :details text :advice again))
             (t (failure (format nil "The HTTP exchange with ~a failed." host) :details text :advice again))))
      ((typep c 'stream-error)
       (failure (format nil "The connection to ~a broke." host) :details text :advice again))
      ((and (find-package "SB-BSD-SOCKETS")
            (condition-of-type-p c (find-symbol "SOCKET-ERROR" "SB-BSD-SOCKETS")))
       (failure (format nil "The network refused the connection to ~a." host) :details text
                :advice (list "SuperL-n shows the network's state." again)))
      (t
       (failure "Something went wrong inside the agent."
                :details (format nil "~(~s~): ~a" (type-of c) text)
                :advice (list "This is a bug in the agent's code, not your network or your key; the line above is what to report."
                              again))))))

(defun stream-with-retries (conv)
  "STREAM-MESSAGE, retried with exponential backoff (or the server's retry-after)
   on anything RETRYABLE-P. Each retry is a note in the transcript, in words, because
   waiting silently on a remote oracle is indistinguishable from being stuck."
  (loop for attempt from 1
        do (handler-case (return (stream-message conv))
             ;; the transport's own failures too: HTTP-ERROR, DNS-ERROR, a socket or a
             ;; stream dying, a timeout — RETRYABLE-P sorts them
             (error (c)
               (if (and (retryable-p c) (< attempt *agent-retries*))
                   (let ((wait (or (and (typep c 'agent-api-error) (api-error-retry-after c))
                                   (min 32 (expt 2 attempt)))))
                     (note conv (format nil "~a — trying again in ~ds (~d/~d)"
                                        (getf (describe-agent-failure c) :headline) wait attempt *agent-retries*))
                     (interruptible-sleep conv wait))
                   (error c))))))

;;; ===========================================================================
;;; A turn: question → (reply → tools)* → done
;;; ===========================================================================

(defvar *agent-listener* nil
  "A function (KIND &rest ARGS) told what happens during a turn, or NIL — the REPL
   doorway binds one to print as it streams.")

(defun agent-listen (kind &rest args)
  (when *agent-listener* (ignore-errors (apply *agent-listener* kind args))))

(defun append-message (conv role blocks)
  (with-conversation-lock (conv)
    (vector-push-extend (make-instance 'agent-message :role role :blocks blocks)
                        (conversation-messages conv)))
  (touch conv))

(defun dangling-tool-uses (conv)
  "tool_use blocks of the last assistant message that never got a result — after a
   cancel, a max_tokens stop or a refusal. The next user message must answer them
   first, or the API refuses the whole request."
  (let* ((ms (conversation-messages conv))
         (last (and (plusp (length ms)) (aref ms (1- (length ms))))))
    (and last (equal (message-role last) "assistant")
         (remove-if-not (lambda (b) (equal (block-type b) "tool_use")) (message-blocks last)))))

(defun add-user-text (conv text)
  "Append the user's TEXT — closing any dangling tool calls first, and folding into a
   trailing user message (one that never got an answer) rather than adding a second.
   Neither touches anything the model already replied to: the history stays
   append-only, which is what keeps earlier thinking valid and the cache warm."
  (let ((closers (mapcar (lambda (b)
                           (make-instance 'content-block
                                          :json (lol.json:jobj "type" "tool_result"
                                                      "tool_use_id" (lol.json:jget (block-json b) "id")
                                                      "content" "Not run: the turn ended before this call could run."
                                                      "is_error" t)))
                         (dangling-tool-uses conv)))
        (text-block (make-instance 'content-block :json (lol.json:jobj "type" "text" "text" text)))
        (ms (conversation-messages conv)))
    (if (and (null closers) (plusp (length ms)) (equal (message-role (aref ms (1- (length ms)))) "user"))
        (with-conversation-lock (conv)
          (setf (message-blocks (aref ms (1- (length ms))))
                (append (message-blocks (aref ms (1- (length ms)))) (list text-block))))
        (append-message conv "user" (append closers (list text-block))))))

(defun live-partial-text (conv)
  "The text the reply being streamed had shown so far — kept when a turn fails, so
   what had arrived is not lost with the connection."
  (with-conversation-lock (conv)
    (let ((live (conversation-live conv)))
      (if live
          (format nil "~{~a~}" (loop for lb in (live-reply-blocks live)
                                     when (equal (live-block-type lb) "text") collect (live-text lb)))
          ""))))

(defun fail-turn (conv condition &key turn question)
  "Record CONDITION as the failure of TURN (default: CONV's current one), in words.
   With no turn in progress — the failure came before one began — a turn is made
   for QUESTION, so the failure still has a place in the transcript. Returns it.
   A TURN passed in is RUN-TURN's own, still running: its cleanup marks it done (and
   counts its usage) after this; any other is marked done here."
  (let* ((f (describe-agent-failure condition))
         (running turn)
         (turn (or turn
                  (let ((cur (current-turn conv)))
                    (and cur (not (turn-done cur)) cur))
                  (let ((new (make-turn :question (or question "(the question)")
                                        :start (length (conversation-messages conv)))))
                    (with-conversation-lock (conv) (vector-push-extend new (conversation-turns conv)))
                    new))))
    (with-conversation-lock (conv)
      (unless (turn-failure turn)
        (setf (turn-failure turn) f
              (turn-partial turn) (live-partial-text conv)))
      (unless running
        (unless (turn-usage turn) (setf (turn-usage turn) (list :input 0 :output 0 :cache-read 0 :cache-write 0 :cost 0)))
        (setf (turn-done turn) t)))
    (touch conv (getf f :headline))
    (agent-listen :failure f)
    turn))

(defun failed-turn (conv)
  "CONV's last turn, when it ended in a failure (what a retry asks again), else NIL."
  (let ((turn (current-turn conv)))
    (and turn (turn-done turn) (turn-failure turn) turn)))

(defun run-turn (conv prompt &key (question prompt) origin again)
  "Ask PROMPT in CONV and drive the tool loop until the model is done. Blocking: the
   REPL calls it directly, the environment on a thread of its own (agent-ui.lisp).
   QUESTION is what the transcript shows; PROMPT may carry more (the context Ask it
   gathered). AGAIN re-asks a failed turn: when the history already ends in the
   user's unanswered message, that is sent as it is rather than said twice. Returns
   the conversation. A failure never escapes once the turn exists: it is the turn's
   FAILURE, which the transcript shows (FAIL-TURN)."
  (ensure-agent-settings)
  (when (eq *agent-initiative* :off)
    (error "The agent is off (*agent-initiative* is :OFF). (setf *agent-initiative* :reacting) wakes it."))
  (let ((turn (make-turn :question question :prompt prompt :again again
                         :start (length (conversation-messages conv)) :origin origin)))
    (with-conversation-lock (conv)
      (vector-push-extend turn (conversation-turns conv))
      (setf (conversation-state conv) :working (conversation-cancel conv) nil))
    (let ((turn-usage (list :input 0 :output 0 :cache-read 0 :cache-write 0)) (turn-cost 0))
      (unwind-protect
           (let ((outcome
                   (catch 'agent-cancel
                     (handler-case
                         (progn
                           (when (equal (conversation-title conv) (format nil "conversation ~d" (conversation-id conv)))
                             (setf (conversation-title conv) (lol.widgets:clip-text (first-line question) 40)))
                           (let ((ms (conversation-messages conv)))
                             (unless (and again (plusp (length ms))
                                          (equal (message-role (aref ms (1- (length ms)))) "user"))
                               (add-user-text conv prompt)))
                           (touch conv "checking the network...")
                           (agent-preflight)
                           (touch conv "thinking...")
                           (loop for round from 1
                                 do (multiple-value-bind (blocks stop usage details) (stream-with-retries conv)
                                      (with-conversation-lock (conv) (setf (conversation-live conv) nil))
                                      (append-message conv "assistant" blocks)
                                      (dolist (b blocks)
                                        (when (equal (block-type b) "text")
                                          (agent-listen :text (lol.json:jget (block-json b) "text"))))
                                      (multiple-value-bind (u cost) (add-usage conv usage)
                                        (loop for (k v) on u by #'cddr do (incf (getf turn-usage k) v))
                                        (when (and cost turn-cost) (incf turn-cost cost))
                                        (unless cost (setf turn-cost nil)))
                                      (let ((uses (remove-if-not (lambda (b) (equal (block-type b) "tool_use")) blocks)))
                                        (cond
                                          ((equal stop "refusal")
                                           (note conv (format nil "the model declined~@[ (~a)~]"
                                                              (lol.json:jget details "category")))
                                           (return :refused))
                                          ((and uses (equal stop "max_tokens"))
                                           (note conv "stopped at max_tokens in the middle of a tool call; not run")
                                           (return :max-tokens))
                                          ((equal stop "pause_turn") nil)   ; the server paused: just continue
                                          ((null uses) (return :done))
                                          ((> round *agent-max-rounds*)
                                           (note conv (format nil "stopped after ~d tool rounds (*agent-max-rounds*)" round))
                                           (return :too-many-rounds))
                                          (t
                                           ;; ALL results go back in ONE user message, in order
                                           (let ((results
                                                   (loop for b in uses
                                                         for call = (block-call b)
                                                         do (touch conv (format nil "running ~a" (call-headline call)))
                                                            (agent-listen :tool call)
                                                         collect (let ((r (if *tool-executor*
                                                                              (funcall *tool-executor* conv call
                                                                                       (lambda () (execute-tool-call conv call)))
                                                                              (execute-tool-call conv call))))
                                                                   (agent-listen :tool-done call)
                                                                   (make-instance 'content-block :json r)))))
                                             (append-message conv "user" results)
                                             (touch conv "thinking..."))))))))
                       ;; EVERY failure becomes the turn's FAILURE — said in the
                       ;; transcript in words, never only in a status line or a log
                       (serious-condition (c)
                         (fail-turn conv c :turn turn) :error)))))
             (when (eq outcome :cancelled) (note conv "stopped (C-g)")))
        (with-conversation-lock (conv)
          (setf (conversation-live conv) nil
                (conversation-state conv) :idle
                (conversation-thread conv) nil
                (conversation-jobs conv) '()
                (turn-usage turn) (append turn-usage (list :cost turn-cost))
                (turn-done turn) t))
        (touch conv (if (turn-failure turn)
                        (format nil "failed: ~a" (getf (turn-failure turn) :headline))
                        (format nil "done · ~a" (format-cost (conversation-cost conv)))))
        (agent-listen :done turn)))
    conv))

(defun cancel-conversation (conv)
  "Stop CONV's turn: set the flag the stream checks between chunks, and — because a
   turn can be blocked in a read with no chunk coming — interrupt its thread with a
   THROW to the same catch. The socket is closed by the transport's UNWIND-PROTECT."
  (when (conversation-busy-p conv)
    (setf (conversation-cancel conv) t)
    (let ((th (conversation-thread conv)))
      (when (and th (not (eq th sb-thread:*current-thread*)) (sb-thread:thread-alive-p th))
        (ignore-errors (sb-thread:interrupt-thread th (lambda () (throw 'agent-cancel :cancelled))))))
    t))

;;; ===========================================================================
;;; The system prompt — a definition in the registry, so it is browsable, editable
;;; and Accept-able like everything else (doc/agent.org §13 leaned this way)
;;; ===========================================================================

(defparameter *agent-system-prompt*
  "You are the agent that lives inside lisp-over-linux (LoL): a minimal Linux whose
PID 1 is a Steel Bank Common Lisp image. There is no shell, no coreutils and no
source tree on disk for this system: the source of every definition was captured into
the running image (the registry), and the user's whole environment — a Smalltalk-style
code browser drawn on the bare framebuffer — is Lisp code in the same heap as you.

You reason about the running system by querying it, not by reading a snapshot:
- lisp-apropos finds names; lisp-source shows a definition's exact source (edits
  accepted since boot included); lisp-senders asks the compiler's cross-reference
  database who calls or uses a name (exact, not a text search).
- lisp-describe-class, lisp-describe-gf and lisp-describe describe live objects;
  lisp-modules lists the files the image was built from.
- file-read and file-list see the user's home directory (the laptop's /home).
- Writes — lisp-eval, lisp-accept (compile a definition into the image, recorded in
  the change set: the same thing the user's C-c C-c does), file-write, and
  container-run when it is offered — need the user's approval unless they allowed
  writes. A declined call is a normal answer, not an error: adapt and carry on.

How your reply is shown: in a Workspace-like buffer where your prose appears as Lisp
comments and a ```lisp fenced block appears as live code the user can evaluate
(C-x C-e) or compile into the image (C-c C-c). So put a definition you propose in a
```lisp block, one top-level form per block where you can. Keep prose short and plain
(no tables, no headings); the screen is a small framebuffer and lines are wrapped at
about 90 columns. Mention symbols by their Lisp names, so the user can jump to them.

Be exact: when you claim something about this system, check it with a tool first. If
a tool fails, say what failed. The user is here to understand the system; explain the
why, briefly."
  "The system prompt: what the model is told about where it is (doc/agent.org §7.1).
   Stable text: it is the start of the cached prefix, so it must not vary per request.")

;;; ===========================================================================
;;; The REPL doorway (§8.2): (ask "…") and the ? prefix
;;; ===========================================================================

(defun repl-approval-handler (stream-in stream-out)
  "A HANDLER-BIND handler for TOOL-APPROVAL-REQUEST in the REPL: print the restarts —
   the same three the environment shows — and invoke the one typed. 'a' allows this
   call and every later one this session (ALLOW-ALL-WRITES)."
  (lambda (c)
    (let ((restarts (remove-if-not (lambda (r) (member (restart-name r) '(allow deny edit)))
                                   (compute-restarts c))))
      (format stream-out "~&~a~a~a~%" (sgr 93) c (col-reset))
      (let ((input (lol.json:jget (call-input (approval-call c)) (or (editable-input-key (approval-call c)) "path"))))
        (when input (format stream-out "~a~a~a~%" (sgr 90) input (col-reset))))
      (loop for r in restarts for i from 1
            do (format stream-out "  ~d: ~a~%" i r))
      (format stream-out "  a: Allow all — stop asking this session~%")
      (loop
        (format stream-out "choose 1-~d or a: " (length restarts)) (finish-output stream-out)
        (let* ((line (string-trim " " (read-line stream-in nil "")))
               (n (ignore-errors (parse-integer line :junk-allowed t))))
          (cond ((and (plusp (length line)) (char-equal (char line 0) #\a) (find-restart 'allow c))
                 (allow-all-writes :conversation (approval-conversation c))
                 (invoke-restart 'allow))
                ((and n (<= 1 n (length restarts)))
                 (let ((*query-io* (make-two-way-stream stream-in stream-out)))
                   (invoke-restart-interactively (nth (1- n) restarts))))))))))

(defun repl-listener (out)
  "Print a turn as it happens: text in plain, tool calls as one dim line each."
  (lambda (kind &rest args)
    (case kind
      (:text (format out "~&~a~%" (first args)))
      (:tool (format out "~&~a  > ~a~a~%" (sgr 90) (call-headline (first args)) (col-reset)))
      (:tool-done (let ((c (first args)))
                    (format out "~&~a    ~(~a~)~@[ · ~a~]~a~%" (sgr 90) (call-status c) (call-summary c) (col-reset))))
      (:note (format out "~&~a  ! ~a~a~%" (sgr 93) (first args) (col-reset)))
      (:failure (let ((f (first args)))
                  (format out "~&~aFAILED: ~a~a~%" (col-error) (getf f :headline) (col-reset))
                  (dolist (d (getf f :details)) (format out "  ~a~%" d))
                  ;; the advice is written for the pane; here, asking again is a ? line
                  (dolist (a (getf f :advice))
                    (format out "  ~a~a~a~%" (sgr 93)
                            (if (search "C-c C-a" a) "Ask again with the same ?question line." a)
                            (col-reset)))))
      (:done (let ((u (turn-usage (first args))))
               (format out "~&~a  ~d in (~d cached, ~d written) · ~d out · ~a · session ~a~a~%"
                       (sgr 90) (getf u :input) (getf u :cache-read) (getf u :cache-write)
                       (getf u :output) (format-cost (getf u :cost)) (format-cost (session-cost))
                       (col-reset)))))
    (finish-output out)))

(defun ask (&rest args)
  "Ask the agent from the REPL: (ask \"why does draw-alien flip the rows?\") continues
   *CONVERSATION*; (ask :new \"…\") starts a fresh one. Prints the reply as it comes and
   RETURNS THE CONVERSATION, so * holds it: (present *) opens it, (show-inspect *)
   inspects it. Write tools ask for approval right here, as numbered restarts."
  (let* ((new (eq (first args) :new))
         (text (format nil "~{~a~^ ~}" (if new (rest args) args)))
         (conv (if (or new (null *conversation*)) (make-conversation) *conversation*))
         (in *standard-input*) (out *standard-output*))
    (when (conversation-busy-p conv)
      (error "~a is busy with a turn; wait, or C-g it in the environment" conv))
    (setf (conversation-thread conv) sb-thread:*current-thread*)
    (let ((*agent-listener* (repl-listener out))
          (*tool-executor* nil))
      (handler-bind ((tool-approval-request (repl-approval-handler in out)))
        (run-turn conv text :origin :repl)))))

(defun repl-question-p (line)
  "Is this REPL line prose for the agent? It starts with ? (§8.2)."
  (let ((s (string-left-trim " " line)))
    (and (plusp (length s)) (char= (char s 0) #\?))))

(defun repl-ask-line (text out)
  "The REPL's ? path: TEXT is the prose after the ?. Errors are reported, never
   propagated — the REPL must survive an unreachable endpoint."
  (handler-case (progn (ask text) (values))
    (serious-condition (c)
      (format out "~&~aagent:~a ~a~%" (col-error) (col-reset) c))))
