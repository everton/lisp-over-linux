;;;; agent-ui.lisp — the agent in the environment: a conversation is a Workspace,
;;;; Ask is a verb, approval is the chooser, and the state is always on screen.
;;;;
;;;; doc/agent.org §8 is the design; its first rule (§8.0) is that the agent may only
;;;; use the four moves the environment already has — evaluate a form, drill into a
;;;; subject, choose from what is offered, accept a change. So this file adds almost
;;;; no interaction of its own. What it adds:
;;;;
;;;;   PRESENT on a CONVERSATION — a :workspace view. The model's prose arrives as Lisp
;;;;     COMMENTS and its ```lisp blocks as live code, so every Workspace verb works on
;;;;     the reply as it stands: C-x C-e evaluates a proposed form, C-c C-c compiles a
;;;;     proposed definition into the image and the change set (§8.3). You ask the
;;;;     next question by typing it at the end and pressing C-c C-a.
;;;;   ASK IT (C-c C-a, §8.1) on any pane: the selection, the form before point, or the
;;;;     pane's subject becomes the question's context — you are always somewhere.
;;;;   A TICK the environment's loop calls each frame: it streams the reply into any
;;;;     pane showing the conversation (~15 Hz while a turn runs, nothing when idle,
;;;;     §8.7), and runs the tool calls the turn's thread posts — on THIS thread, the
;;;;     one that owns the image's UI state. A write tool signals TOOL-APPROVAL-REQUEST
;;;;     there; the handler offers its restarts in the environment's own chooser — the
;;;;     card the right-button menu uses — and invokes the one picked (§8.4).
;;;;   INDICATORS (§8.9): the tray says the authority and the session's cost, the tab
;;;;     of a conversation carries a dot (its colour the authority; it pulses while
;;;;     working), and the status line says when the agent is waiting for you.
;;;;
;;;; Never steal focus (§8.5): asking from elsewhere opens the conversation's tab
;;;; BEHIND you; an approval is only asked while you are looking at the conversation.
;;;;
;;;; Loaded after poc/shell.lisp (it calls the shell's pane functions); the shell and
;;;; fb-browser.lisp reach it through a handful of FBOUNDP-guarded calls, so the
;;;; browser still builds and runs without it.
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

;; LOL.JSON's names are written package-qualified (lol.json:jget …) rather than
;; imported: CL-USER is shared by every module, and another one defining its own JOBJ
;; there must not collide with — or silently redefine — ours.

;;; ===========================================================================
;;; Rendering a conversation as Lisp-shaped text
;;; ===========================================================================

(defparameter +your-turn+ ";; --- your turn: type below, then C-c C-a asks ---"
  "The line that ends every finished turn. What follows the LAST one is the draft —
   the next question — so the boundary survives whatever you edit above it.")

(defun wrap-words (text width)
  "TEXT broken into lines of at most WIDTH characters at spaces (a word longer than
   WIDTH is cut). Leading indentation of TEXT is kept on continuation lines."
  (let* ((indent (or (position #\Space text :test-not #'char=) 0))
         (pad (make-string (min indent 8) :initial-element #\Space))
         (lines '()) (line (subseq text 0 indent)))
    (dolist (word (loop with start = indent
                        for sp = (position #\Space text :start start)
                        for w = (subseq text start sp)
                        when (plusp (length w)) collect w
                        while sp do (setf start (1+ sp))))
      (loop while (> (length word) width)
            do (unless (string= (string-trim " " line) "") (push line lines) (setf line pad))
               (push (concatenate 'string line (subseq word 0 (- width (length line)))) lines)
               (setf word (subseq word (- width (length line))) line pad))
      (cond ((string= (string-trim " " line) "") (setf line (concatenate 'string line word)))
            ((> (+ (length line) 1 (length word)) width)
             (push line lines) (setf line (concatenate 'string pad word)))
            (t (setf line (concatenate 'string line " " word)))))
    (push line lines)
    (nreverse lines)))

(defun split-lines* (text)
  (loop with start = 0
        for nl = (position #\Newline text :start start)
        collect (subseq text start nl)
        while nl do (setf start (1+ nl))))

(defun lisp-fence-p (lang)
  (member (string-downcase (string-trim " " lang)) '("" "lisp" "common-lisp" "cl" "elisp") :test #'string=))

(defun reply-lines (text)
  "A reply's markdown as buffer lines: prose wrapped and commented (;; ), a ```lisp
   block raw — live code — and any other fenced block commented with a bar (;; | ).
   A blank line stays blank, so paragraphs read as paragraphs."
  (let ((out '()) (fence nil) (width (max 30 (- *agent-wrap* 3))))
    (dolist (line (split-lines* text))
      (let ((trimmed (string-left-trim " " line)))
        (cond
          ((and (>= (length trimmed) 3) (string= "```" trimmed :end2 3))
           (setf fence (if fence nil (if (lisp-fence-p (subseq trimmed 3)) :lisp :other))))
          ((eq fence :lisp) (push line out))
          ((eq fence :other) (push (concatenate 'string ";; | " line) out))
          ((string= trimmed "") (push "" out))
          (t (dolist (l (wrap-words line width)) (push (concatenate 'string ";; " l) out))))))
    (nreverse out)))

(defun call-status-word (call)
  (case (call-status call)
    (:waiting "WAITING FOR YOUR APPROVAL")
    (:running "running") (:pending "...") (:denied "declined") (:error "error") (t nil)))

(defun call-lines (call)
  "A tool call as transcript lines: one ;; #N line (C-c C-o on it opens what it was
   about, §8.6), and for a WRITE its input in full, so what you approve is on screen."
  (cons (format nil ";; #~d ~a~@[ -> ~a~]~@[  [~a]~]" (call-number call) (call-headline call)
                (call-summary call) (call-status-word call))
        (when (write-tool-p (call-name call))
          (let ((body (and (call-input call)
                           (lol.json:jget (call-input call) (or (editable-input-key call) "")))))
            (when (and body (find #\Newline body))
              (mapcar (lambda (l) (concatenate 'string ";;   | " l)) (split-lines* body)))))))

(defun thinking-lines (text)
  (let ((s (string-trim '(#\Space #\Newline) (substitute #\Space #\Newline text))))
    (when (plusp (length s))
      (list (format nil ";; (thinking: ~a)" (lol.widgets:clip-text s (- *agent-wrap* 16)))))))

(defun block-lines (b)
  (let ((type (block-type b)))
    (cond ((equal type "text") (reply-lines (lol.json:jget (block-json b) "text")))
          ((equal type "thinking") (thinking-lines (or (lol.json:jget (block-json b) "thinking") "")))
          ((equal type "tool_use") (and (block-call b) (call-lines (block-call b))))
          (t nil))))

(defun live-block-lines (lb)
  (let ((type (live-block-type lb)))
    (cond ((equal type "text") (reply-lines (live-text lb)))
          ((equal type "thinking") (thinking-lines (live-text lb)))
          ((equal type "tool_use") (list (format nil ";; > ~a ..." (lol.json:jget (live-block-json lb) "name"))))
          (t nil))))

(defun usage-line (conv turn)
  (let ((u (turn-usage turn)))
    (format nil ";; -- ~:d in, ~:d cached, ~:d written · ~:d out · ~a · this conversation ~a --"
            (getf u :input) (getf u :cache-read) (getf u :cache-write) (getf u :output)
            (format-cost (getf u :cost)) (format-cost (conversation-cost conv)))))

(defun turn-lines (conv turn)
  "TURN as buffer lines: the question, every assistant block after it (and the reply
   still streaming), the notes, and — once done — the usage and the your-turn line."
  (with-conversation-lock (conv)
    (let* ((ms (conversation-messages conv))
           (lines (append
                   (mapcar (lambda (l) (concatenate 'string ";; ? " l))
                           (loop for q in (split-lines* (turn-question turn))
                                 append (wrap-words q (- *agent-wrap* 5))))
                   (loop for i from (turn-start turn) below (length ms)
                         for m = (aref ms i)
                         when (equal (message-role m) "assistant")
                           append (loop for b in (message-blocks m) append (block-lines b)))
                   (when (and (not (turn-done turn)) (conversation-live conv))
                     (loop for lb in (live-reply-blocks (conversation-live conv))
                           append (live-block-lines lb)))
                   (mapcar (lambda (n) (concatenate 'string ";; ! " n)) (turn-notes turn)))))
      (if (turn-done turn)
          (append lines (list (usage-line conv turn) +your-turn+ ""))
          (append lines (list (format nil ";; ... ~a   (C-g stops)" (conversation-status conv))))))))

(defun conversation-header (conv)
  (list (format nil ";; Agent — ~a · ~a · authority ~(~s~)" (conversation-title conv)
                (conversation-model conv) (conversation-authority* conv))
        ";; Type a question at the end and press C-c C-a. Code the agent proposes is live:"
        ";; C-x C-e evaluates it, C-c C-c compiles it into the image (the change set)."
        ";; C-c C-o on a ;; #N line opens what that tool call was about. C-g stops a turn."
        ""))

(defun conversation-lines (conv)
  (append (conversation-header conv)
          (if (zerop (length (conversation-turns conv)))
              (list +your-turn+ "")
              (loop for turn across (conversation-turns conv) append (turn-lines conv turn)))))

(defun lines-text (lines) (format nil "~{~a~^~%~}" lines))

;;; ===========================================================================
;;; Subjects: the conversation, a tool call, and the agent itself
;;; ===========================================================================

(defmethod present ((c conversation))
  (make-view :title (format nil "agent · ~a" (conversation-title c))
             :kind :workspace :subject c
             :text (lines-text (conversation-lines c))
             :data (list :conversation c)))

(defmethod commands-for ((c conversation))
  (list (make-command :label "Stop the turn" :action (lambda () (cancel-conversation c)) :key "C-g")
        (make-command :label "Inspect the conversation object" :target (subj-object c))
        (make-command :label "The agent's state and modes" :target (subj-agent))))

(defmethod present ((call tool-call))
  (make-view
   :title (format nil "tool call #~d · ~a" (call-number call) (call-name call))
   :kind :reference :subject call :syntax :plain
   :text (format nil ";; tool call #~d — ~a — ~(~a~)~%;; input:~%~a~%~%;; result~@[ (~a)~]:~%~a~@[~%~%;; about: ~a — C-c C-o opens it~]"
                 (call-number call) (call-name call) (call-status call)
                 (if (call-input call) (lol.json:to-json (call-input call)) (or (call-raw-input call) ""))
                 (call-summary call) (or (call-result call) "(none yet)")
                 (and (call-subject call) (object-title (call-subject call))))))

(defmethod commands-for ((call tool-call))
  (when (call-subject call)
    (list (make-command :label "Open what it was about" :target (call-subject call)
                        :disposition :in-place :key "C-c C-o"))))

(defstruct (subj-agent (:constructor subj-agent ())))

(defun setting-item (label var &optional (doc ""))
  (make-item :label label :detail (format nil "~s  ~a" (symbol-value var) doc)
             :edit (lambda (new) (setf (symbol-value var) new))))

(defmethod present ((s subj-agent))
  "The agent's state as a subject (§8.9 'changing modes needs no new idiom'): the two
   dials, the model, the meter — each row settable with C-c C-c like an inspector
   field — and the conversations, which drill."
  (make-view
   :title "agent — modes, model, cost, conversations" :kind :list :subject s
   :items (append
           (list (setting-item "initiative" '*agent-initiative* ":off :reacting :interacting")
                 (setting-item "authority" '*agent-authority* ":read-only :ask :allow")
                 (setting-item "model" '*agent-model*)
                 (setting-item "max tokens" '*agent-max-tokens*)
                 (setting-item "effort" '*agent-effort* "nil = the model's default")
                 (setting-item "endpoint" '*agent-endpoint*)
                 (make-item :label "API key"
                            :detail (cond ((not (anthropic-endpoint-p (concatenate 'string *agent-endpoint* "/")))
                                           "not used: the endpoint is not https://api.anthropic.com")
                                          ((agent-api-key) (format nil "present (~a)" (agent-key-path)))
                                          (t (format nil "MISSING — put it in ~a"
                                                     (or (agent-key-path) "~/.config/lol/anthropic-api-key (no /home on this boot)")))))
                 (make-item :label "session cost" :detail (format-cost (session-cost))))
           (mapcar (lambda (c)
                     (make-item :label (conversation-title c) :group "conversations"
                                :detail (format nil "~(~a~) · ~d turn~:p · ~a" (conversation-state c)
                                                (length (conversation-turns c)) (format-cost (conversation-cost c)))
                                :kind :restart :subject c))
                   *conversations*))))

(defmethod commands-for ((s subj-agent))
  (list (make-command :label "New conversation" :action (lambda () (new-trail (make-conversation))))
        (make-command :label "Authority: read-only" :action (lambda () (setf *agent-authority* :read-only)))
        (make-command :label "Authority: ask" :action (lambda () (setf *agent-authority* :ask)))
        (make-command :label "Authority: allow" :action (lambda () (setf *agent-authority* :allow)))
        (make-command :label "Agent off" :action (lambda () (setf *agent-initiative* :off)))
        (make-command :label "Agent on (reacting)" :action (lambda () (setf *agent-initiative* :reacting)))
        (make-command :label "Reload ~/.config/lol/agent.lisp" :action #'load-agent-settings)
        (make-command :label "Forget the cached API key" :action #'forget-agent-key)))

;;; Every subject gains Ask (§8.1): one :AROUND method, no per-subject work.
(defmethod commands-for :around ((x t))
  (let ((cmds (call-next-method)))
    (if (or (typep x 'conversation) (subj-agent-p x))
        cmds
        (append cmds
                (list (make-command :label "Ask the agent about this" :key "C-c C-a"
                                    :surfaces '(:menu :halo)
                                    :action (lambda () (agent-ask-about-subject x))))))))

;;; The Spotter finds the agent and every conversation (§8.3).
(pushnew (lambda ()
           (cons (list :label "agent" :detail "modes, model, cost" :group "surfaces" :subject (subj-agent))
                 (mapcar (lambda (c)
                           (list :label (format nil "conversation: ~a" (conversation-title c))
                                 :detail (format nil "agent · ~(~a~)" (conversation-state c))
                                 :group "surfaces" :subject c))
                         *conversations*)))
         *spotter-sources*)

;;; ===========================================================================
;;; The panes showing a conversation, kept live
;;; ===========================================================================

(defun pane-conversation (pane)
  "The conversation PANE shows, or NIL."
  (and pane (pane-view pane) (listp (view-data (pane-view pane)))
       (getf (view-data (pane-view pane)) :conversation)))

(defun all-panes ()
  (loop for trail in *trails*
        append (loop for p in trail append (remove nil (list p (pane-side p))))))

(defun conversation-panes (conv)
  (remove-if-not (lambda (p) (and (eq (pane-conversation p) conv) (pane-tv p))) (all-panes)))

(defun conversation-visible-p (conv)
  "Is CONV on screen now — the focused pane, or the side pane beside it?"
  (let ((p (ignore-errors (current-pane))))
    (and p (or (eq (pane-conversation p) conv) (eq (pane-conversation (pane-side p)) conv)))))

(defun refresh-conversation-pane (pane conv)
  "Bring PANE's buffer up to date with CONV's current turn. The turn occupies the
   buffer's tail from :LIVE-START; it is re-rendered there and nothing above it is
   touched — your edits to earlier turns survive. The caret follows the tail only if
   it was already there. Returns T when the buffer changed."
  (let* ((tv (pane-tv pane))
         (data (view-data (pane-view pane)))
         (turn (current-turn conv)))
    (when (and turn (not (and (eq (getf data :turn) turn) (getf data :finished))))
      (unless (eq (getf data :turn) turn)               ; a new turn: it starts at the end
        (setf (getf data :turn) turn
              (getf data :finished) nil
              (getf data :live-start) (length (lol.textview:tv-lines tv))))
      (let* ((start (min (getf data :live-start) (length (lol.textview:tv-lines tv))))
             (old (lol.textview:tv-lines tv))
             (follow (>= (lol.textview:tv-point-line tv) (max 0 (1- start))))
             (new (concatenate 'simple-vector (subseq old 0 start) (turn-lines conv turn))))
        (when (turn-done turn) (setf (getf data :finished) t))
        (setf (view-data (pane-view pane)) data)
        (if follow
            (let ((last (1- (length new))))
              (lol.textview:tv-replace tv new last (length (aref new last))))
            (setf (lol.textview:tv-lines tv) new))
        t))))

(defun conversation-draft (tv)
  "The text after the last your-turn line, and the index where it starts."
  (let* ((lines (lol.textview:tv-lines tv))
         (marker (position +your-turn+ lines :test #'string= :from-end t))
         (start (if marker (1+ marker) (length lines))))
    (values (string-trim '(#\Space #\Newline #\Tab)
                         (format nil "~{~a~^~%~}" (coerce (subseq lines start) 'list)))
            start)))

;;; ===========================================================================
;;; Asking: from a conversation, from any pane, from a subject
;;; ===========================================================================

(defun start-turn-async (conv prompt question origin &key (executor #'post-job-and-wait))
  "Run a turn on its own thread, so the environment keeps drawing (§8.7). Tool calls
   come back to the environment's thread as jobs (POST-JOB-AND-WAIT)."
  (when (conversation-busy-p conv)
    (return-from start-turn-async (setf *browser-status* "the agent is still answering — C-g stops it")))
  (when (eq *agent-initiative* :off)
    (return-from start-turn-async
      (setf *browser-status* "the agent is off — SuperL-Space › agent turns it on")))
  (setf (conversation-state conv) :working)          ; busy from this instant, not from the thread's start
  (setf (conversation-thread conv)
        (sb-thread:make-thread
         (lambda ()
           (let ((*tool-executor* executor))
             (handler-case (run-turn conv prompt :question question :origin origin)
               (serious-condition (c)
                 (setf (conversation-state conv) :idle)
                 (touch conv (format nil "error: ~a" c))))))
         :name (format nil "agent turn ~d" (conversation-id conv))))
  conv)

(defun ensure-conversation-tab (conv)
  "Open CONV in a tab of its own if none shows it — BEHIND the current one: asking
   from elsewhere must not move you (§8.7: the tab fills in behind you)."
  (unless (conversation-panes conv)
    (let ((active *active*))
      (new-trail conv)
      (caret-to-end (current-pane))
      (setf *active* active))))

(defun agent-ask (question &key prompt origin (conv (or *conversation* (make-conversation))))
  "Ask QUESTION (shown in the transcript) — PROMPT, if given, is what is sent — in CONV,
   asynchronously. The environment's entry point; the REPL's is ASK."
  (ensure-agent-settings)
  (ensure-conversation-tab conv)
  (start-turn-async conv (or prompt question) question origin)
  (unless (conversation-visible-p conv)
    (setf *browser-status* "asked — SuperL-a shows the conversation"))
  conv)

(defun ask-from-conversation-pane (pane conv)
  "C-c C-a in a conversation: the draft below the last your-turn line is the question.
   The draft is taken out of the buffer; the turn renders it back as ;; ? lines."
  (multiple-value-bind (draft start) (conversation-draft (pane-tv pane))
    (cond
      ((zerop (length draft))
       (setf *browser-status* "type your question below the last ;; --- your turn line, then C-c C-a"))
      (t
       (let ((lines (lol.textview:tv-lines (pane-tv pane))))
         (lol.textview:tv-replace (pane-tv pane) (subseq lines 0 start) (max 0 (1- start)) 0))
       (setf *conversation* conv)
       (start-turn-async conv draft draft pane)))))

(defun subject-context (subject)
  "What to tell the model about SUBJECT, the thing the pane presents: a definition's
   source, or a description. The model can widen it with its tools (§8.1)."
  (let ((*print-case* :downcase) (*print-length* 20) (*print-level* 4))
    (cond
      ((subj-defn-p subject)
       (let ((src (source-of (subj-defn-name subject) (subj-defn-kind subject))))
         (format nil "the definition of ~s~@[ (~a)~]~:[ — a builtin, not recorded here~;:~%```lisp~%~a~%```~]"
                 (subj-defn-name subject) (subj-defn-kind subject) src
                 (if (listp src) (format nil "~{~a~^~%~%~}" src) src))))
      ((typep subject 'conversation) "this conversation")
      (t (format nil "~a (a ~(~a~))" (ignore-errors (object-title subject)) (type-of subject))))))

(defun agent-ask-about (about context default-question &key question)
  "Ask about something: with QUESTION, at once; else prompt for one in the minibuffer
   (an empty answer asks DEFAULT-QUESTION)."
  (flet ((go-ask (q)
           (let* ((q (if (zerop (length (string-trim " " q))) default-question q)))
             (agent-ask (format nil "~a   [about ~a]" q about)
                        :prompt (format nil "~a~%~%---~%Context from the environment: the user pressed Ask (C-c C-a) on ~a.~%~a"
                                        q about context)
                        :origin :ask-it))))
    (if question
        (go-ask question)
        (minibuffer-read (format nil "Ask about ~a: " (lol.widgets:clip-text about 40)) #'go-ask
                         :hint "Enter with nothing asks what it is"))))

(defun agent-ask-about-subject (subject)
  (agent-ask-about (or (ignore-errors (object-title subject)) "this")
                   (subject-context subject)
                   "What is this, what is it for, and is there anything notable about it?"))

(defun reads-as-form-p (text)
  (let ((*read-eval* nil))
    (handler-case (multiple-value-bind (form end) (read-from-string text)
                    (declare (ignore form))
                    (string= "" (string-trim '(#\Space #\Newline #\Tab) (subseq text end))))
      (error () nil))))

(defun agent-ask-at-pane (pane)
  "C-c C-a — Ask it (§8.1). In a conversation: send the draft. Elsewhere, like the
   other verbs, act on the selection, else the form before point, else the pane's
   subject: prose selected IS the question; a form or a subject is what it is about."
  (let ((conv (pane-conversation pane))
        (tv (pane-tv pane))
        (subject (pane-subject pane))
        (title (view-title (pane-view pane))))
    (cond
      (conv (ask-from-conversation-pane pane conv))
      ((and tv (editor-region-text tv))
       (let ((sel (editor-region-text tv)))
         (if (reads-as-form-p sel)
             (agent-ask-about "the selected form" (format nil "In ~a:~%```lisp~%~a~%```" title sel)
                              "What does this do, and is anything wrong with it?")
             (agent-ask-about title (subject-context subject) "" :question sel))))
      ((and tv (workspace-pane-p pane) (form-text-before-point tv))
       (let ((form (form-text-before-point tv)))
         (agent-ask-about (format nil "the form ~a" (lol.widgets:clip-text (first-line form) 30))
                          (format nil "In the Workspace:~%```lisp~%~a~%```" form)
                          "What does this do, and is anything wrong with it?")))
      (t (agent-ask-about-subject
          (let ((it (and (null tv) (selected-item pane))))
            (or (and it (item-subject it)) subject)))))))

(defun agent-show ()
  "SuperL-a: the current conversation's tab (made if needed), or a new conversation."
  (let* ((conv (or *conversation* (make-conversation)))
         (i (position-if (lambda (trail) (some (lambda (p) (eq (pane-conversation p) conv)) trail))
                         *trails*)))
    (if i
        (activate-trail i)
        (progn (new-trail conv) (caret-to-end (current-pane))))
    (setf *browser-status* nil)))

(defun caret-to-end (pane)
  "Put PANE's caret on its last line — where the next question is typed."
  (let* ((tv (pane-tv pane)) (lines (and tv (lol.textview:tv-lines tv))))
    (when lines
      (lol.textview:tv-replace tv lines (1- (length lines)) (length (aref lines (1- (length lines))))))))

(defun agent-cancel-at-pane (pane)
  "C-g in a conversation that is answering stops the turn (T); otherwise NIL, and C-g
   means back, as everywhere."
  (let ((conv (pane-conversation pane)))
    (when (and conv (conversation-busy-p conv))
      (cancel-conversation conv)
      (setf *browser-status* "stopping...")
      t)))

(defun agent-pane-busy-p (pane)
  "A conversation pane is read-only while its turn streams into it."
  (conversation-busy-p (pane-conversation pane)))

(defun call-at-point (pane)
  "The tool call named by the ;; #N line under the caret, or NIL."
  (let* ((tv (pane-tv pane)) (conv (pane-conversation pane)))
    (when (and tv conv)
      (let ((line (aref (lol.textview:tv-lines tv) (lol.textview:tv-point-line tv))))
        (when (and (> (length line) 4) (string= ";; #" line :end2 4))
          (let ((n (parse-integer line :start 4 :junk-allowed t)))
            (and n (< -1 n (length (conversation-calls conv))) (aref (conversation-calls conv) n))))))))

(defun agent-open-at-point (pane)
  "C-c C-o on a ;; #N line: open what that call was about — the definition it read,
   the senders it listed, the file — the view you would have reached by hand (§8.6).
   A call with no such subject opens as itself (its input and result)."
  (let ((call (call-at-point pane)))
    (if call
        (trail-push (or (call-subject call) call))
        (setf *browser-status* "C-c C-o: put the caret on a ;; #N tool-call line"))))

;;; ===========================================================================
;;; The tick: stream into panes, run posted tool calls, ask for approval
;;; ===========================================================================

(defparameter +agent-frame-ms+ 66 "Redraw budget while a turn streams: about 15 Hz (§8.7).")

(defvar *agent-seen-revisions* (make-hash-table :test 'eq :weakness :key))

(defun agent-ui-timeout ()
  "The environment loop's poll deadline for the agent: a frame period while any
   conversation is working, -1 (sleep until input) when none is."
  (if (some (lambda (c) (or (conversation-busy-p c)
                            ;; a last change not yet drawn: the turn's final TOUCH can land
                            ;; just after the tick that saw it go idle
                            (not (eql (conversation-revision c) (gethash c *agent-seen-revisions*)))))
            *conversations*)
      +agent-frame-ms+ -1))

(defun approval-entries (c)
  "The approval request's restarts as chooser entries — the restarts themselves, not
   a parallel list: what you pick is what gets invoked."
  (let* ((call (approval-call c))
         (group (format nil "approve #~d · ~a" (call-number call) (call-name call))))
    (loop for r in (compute-restarts c)
          for name = (restart-name r)
          append (case name
                   (allow (list (menu-entry "Allow — run it" (let ((r r)) (lambda () (invoke-restart r)))
                                            :key "1" :group group)))
                   (deny (list (menu-entry "Deny" (let ((r r)) (lambda () (invoke-restart r nil)))
                                           :key "2" :group group)
                               (menu-entry "Deny, with a reason..."
                                           (let ((r r))
                                             (lambda () (invoke-restart r (and *value-prompt* (funcall *value-prompt* "Reason")))))
                                           :key "3" :group group)))
                   (edit (when (editable-input-key call)
                           (list (menu-entry "Edit the input first..."
                                             (let ((r r))
                                               (lambda ()
                                                 (let ((new (and *value-prompt* (funcall *value-prompt* (editable-input-key call)))))
                                                   (if new (invoke-restart r new) (invoke-restart (find-restart 'deny c) "edit cancelled")))))
                                             :key "4" :group group))))))))

(defun environment-approval (c)
  "Handle a TOOL-APPROVAL-REQUEST in the environment: offer its restarts in the chooser
   — the same card as the right-button menu, keys 1-4, Esc — and invoke the pick.
   Esc declines: the call cannot wait forever with the turn holding its thread."
  (let ((entries (approval-entries c)))
    (when (and *chooser* entries)
      (let ((conv (approval-conversation c)))
        (touch conv)
        (dolist (p (conversation-panes conv)) (ignore-errors (refresh-conversation-pane p conv))))
      (ignore-errors
        (when (boundp '*fb-canvas*)
          (draw-browser (symbol-value '*fb-canvas*) (symbol-value '*fb-xres*) (symbol-value '*fb-yres*))))
      (let* ((w (if (boundp '*fb-xres*) (symbol-value '*fb-xres*) 1280))
             (h (if (boundp '*fb-yres*) (symbol-value '*fb-yres*) 800))
             (e (funcall *chooser* :menu entries (floor w 3) (floor h 3))))
        (if e
            (funcall (getf e :action))
            (invoke-restart (find-restart 'deny c) "dismissed (Esc)"))))))

(defun run-agent-jobs ()
  "Run the tool calls turns have posted. A gated one runs only while its conversation
   is on screen (its approval needs you looking); others run at once. Returns T if
   anything ran."
  (let ((ran nil))
    (dolist (conv *conversations*)
      (dolist (job (with-conversation-lock (conv) (copy-list (conversation-jobs conv))))
        (unless (agent-job-done job)
          (cond
            ((and (agent-job-gated job) (not (conversation-visible-p conv)))
             (unless (eq (call-status (agent-job-call job)) :waiting)
               (setf (call-status (agent-job-call job)) :waiting)
               (touch conv "waiting for your approval"))
             (setf *browser-status* (format nil "the agent waits for your approval (~a) — SuperL-a"
                                            (call-name (agent-job-call job)))))
            (t
             (with-conversation-lock (conv)
               (setf (conversation-jobs conv) (remove job (conversation-jobs conv))))
             (handler-bind ((tool-approval-request #'environment-approval))
               (run-job job))
             (setf ran t))))))
    ran))

(defun agent-ui-tick ()
  "Called by the environment's loop every iteration. Runs posted tool calls, and
   refreshes every pane showing a conversation whose revision moved. T = redraw."
  (let ((dirty nil))
    ;; panes first, so an approval about to be asked shows its call on screen
    (dolist (conv *conversations*)
      (let ((rev (conversation-revision conv)))
        (unless (eql rev (gethash conv *agent-seen-revisions*))
          (setf (gethash conv *agent-seen-revisions*) rev)
          (dolist (p (conversation-panes conv))
            (when (refresh-conversation-pane p conv) (setf dirty t)))
          (setf dirty t))))
    (when (run-agent-jobs) (setf dirty t))
    ;; the working dot pulses: redraw on the frame deadline while anything works
    (or dirty (and (some #'conversation-busy-p *conversations*) t))))

;;; ===========================================================================
;;; Indicators (§8.9)
;;; ===========================================================================

(defun authority-tone ()
  (if (eq *agent-initiative* :off)
      :off
      (ecase *agent-authority* (:read-only :safe) (:ask :warn) (:allow :low))))

(defun agent-tray-item ()
  "The tray's agent item: the authority and the session's cost, always on screen —
   an environment that can spend money owes you a visible meter (§8.9). A trailing
   … while a turn runs, ? while one waits for you."
  (list :kind :agent :tone (authority-tone)
        :text (format nil "agent ~(~a~) ~a~a"
                      (if (eq *agent-initiative* :off) "off" *agent-authority*)
                      (format-cost (session-cost))
                      (cond ((some (lambda (c) (conversation-jobs c)) *conversations*) " ?")
                            ((some #'conversation-busy-p *conversations*) " ...")
                            (t "")))))

(defun trail-agent-dot (trail)
  "For a tab showing a conversation: (values COLOUR FILLED-P), else NIL. The colour is
   the authority; filled when initiative is :interacting; while working it pulses
   (alternates every 400 ms), and while it waits for you it is amber and solid."
  (let ((conv (some #'pane-conversation trail)))
    (when conv
      (let ((tone (authority-tone)))
        (cond
          ((conversation-jobs conv) (values *ui-amber* t))
          ((conversation-busy-p conv)
           (values *ui-accent* (evenp (floor (get-internal-real-time) (floor internal-time-units-per-second 2.5)))))
          (t (values (tray-tone-color tone) (eq *agent-initiative* :interacting))))))))

;;; ===========================================================================
;;; The debugger doorway (§8.8): ASK is one more restart
;;; ===========================================================================
;;;
;;; fb-browser.lisp's RUN-DEBUGGER-FB establishes an ASK-THE-AGENT restart around its
;;; own loop when this file is loaded, so it appears on the rail with the others and is
;;; chosen the same way. Choosing it seeds a NEW conversation with the condition, the
;;; backtrace and the source of the failing definition — which exists because the
;;; image owns it — and shows the answer streaming in a viewer over the debugger; the
;;; stack stays standing underneath. C-c C-c in the viewer accepts the definition
;;; before the caret (ACCEPT-SOURCE, into the change set); then Esc goes back to the
;;; rail, where retrying is the condition's own restart — offered only if it has one,
;;; because abort and continue are not the same thing.
;;;
;;; This conversation is READ-ONLY, and its tools run on its own thread: the debugger
;;; is modal and runs no frame tick to hand them to, and the debugger is the highest-
;;; stakes place to let a model write — the one write here is your C-c C-c.

(defun condition-prompt (condition frames)
  (let ((*print-length* 10) (*print-level* 4) (*print-case* :downcase))
    (with-output-to-string (s)
      (format s "A condition was signalled in this image and the debugger is open on it.~%")
      (format s "Explain what went wrong and why, and if a fix is a change to a definition, ~
                 propose it as one ```lisp block I can accept.~%~%")
      (format s "Condition: ~s~%Report: ~a~%" (type-of condition)
              (or (ignore-errors (princ-to-string condition)) "(unprintable)"))
      (format s "Restarts: ~{~a~^; ~}~%" (mapcar #'princ-to-string (compute-restarts condition)))
      (format s "~%Backtrace (innermost first):~%~{  ~a~%~}"
              (mapcar #'frame-label (subseq frames 0 (min 15 (length frames)))))
      (let ((name (some (lambda (f) (let ((n (ignore-errors (frame-defn-name f))))
                                      (and n (source-of n) n)))
                        frames)))
        (when name
          (let ((src (source-of name)))
            (format s "~%Source of ~s, the innermost frame we have source for:~%```lisp~%~a~%```~%"
                    name (if (listp src) (format nil "~{~a~^~%~%~}" src) src)))
          (let ((frame (find name frames :key (lambda (f) (ignore-errors (frame-defn-name f))))))
            (when frame
              (format s "~%Its frame's locals:~%~{  ~s~%~}" (ignore-errors (frame-locals frame))))))))))

(defun agent-ask-about-condition (condition)
  "The ASK restart's work: a read-only conversation about CONDITION, then the viewer."
  (let* ((frames (ignore-errors (backtrace-frame-objects 40 6)))
         (conv (make-conversation :title (format nil "debugger: ~(~a~)" (type-of condition))
                                  :authority :read-only)))
    (ensure-conversation-tab conv)
    (start-turn-async conv (condition-prompt condition frames)
                      (format nil "Explain this ~(~a~) and propose a fix." (type-of condition))
                      :debugger :executor nil)
    (run-agent-debugger-view conv)))

(defun run-agent-debugger-view (conv)
  "A modal view of CONV over the debugger: it fills in as the answer streams (waking
   at the frame budget while the turn runs), arrows and PageUp/Down move the caret,
   C-c C-c accepts the form before it, C-g stops the turn, Esc returns to the rail."
  (let* ((canvas (symbol-value '*fb-canvas*)) (w (symbol-value '*fb-xres*)) (h (symbol-value '*fb-yres*))
         (tv (lol.textview:make-textview :font *bfont* :focus t))
         (seen -1) (prefix nil) (note nil))
    (lol.textview:tv-geometry tv 12 32 (- w 24) (- h 56))
    (loop
      (let ((rev (conversation-revision conv)))
        (when (/= rev seen)
          (setf seen rev)
          (let ((lines (coerce (conversation-lines conv) 'simple-vector)))
            (lol.textview:tv-replace tv lines (1- (length lines)) 0))))
      (lol.canvas:fill-rect canvas 0 0 w h *ui-bg*)
      (lol.canvas:fill-rect canvas 0 0 w 24 *ui-hi*)
      (lol.canvas:draw-string canvas *bfont*
        (format nil "ASK — ~a   ·   C-c C-c accept the form before the caret · C-g stop · Esc back to the restarts"
                (if (conversation-busy-p conv) "answering..." "done"))
        12 5 *ui-accent*)
      (lol.textview:tv-draw canvas tv)
      (when note (lol.canvas:draw-string canvas *bfont* note 12 (- h 20) *ui-amber*))
      (funcall 'show-frame canvas)
      (multiple-value-bind (kb ms kev)
          (funcall 'fbb-wait 0 (symbol-value '*fb-mouse*) (symbol-value '*fb-kbd*)
                   (if (conversation-busy-p conv) +agent-frame-ms+ 500))   ; idle: still catch the last touch
        (declare (ignore ms))
        (when kev (funcall 'read-keyboard-evdev (symbol-value '*fb-kbd*)))
        (when kb
          (multiple-value-bind (key state) (funcall 'fb-read-key 0)
            (let ((ctrl (or (lol.canvas:ctrl-p state) (symbol-value '*ctrl-down*))))
              (cond
                ((eq key :escape) (return))
                ((and ctrl (member key '(#\g #\G))) (cancel-conversation conv) (setf note "stopping..."))
                ((and prefix ctrl (member key '(#\c #\C)))
                 (setf prefix nil)
                 (let ((text (form-text-before-point tv)))
                   (setf note
                         (if (null text) "no form before the caret"
                             (handler-case (format nil "Accepted ~(~a~) — Esc, then a restart to retry"
                                                   (let ((*read-eval* nil)) (accept-source text)))
                               (serious-condition (c) (format nil "Accept failed: ~a" c)))))))
                ((and ctrl (member key '(#\c #\C))) (setf prefix t))
                ((member key '(:up :down :left :right :page-up :page-down :home :end))
                 (setf prefix nil)
                 (lol.textview:tv-key tv key 0))
                (t (setf prefix nil))))))))))
