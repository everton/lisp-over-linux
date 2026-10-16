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

(defun clean-markdown (line)
  "LINE of a reply with the markdown that a comment cannot show taken out: **bold**
   and __bold__ markers dropped (backticks stay — they mark Lisp names), and a leading
   * or - bullet drawn as •."
  (let ((s line))
    (dolist (m '("**" "__"))
      (loop for p = (search m s) while p
            do (setf s (concatenate 'string (subseq s 0 p) (subseq s (+ p 2))))))
    (let ((indent (or (position #\Space s :test-not #'char=) 0)))
      (if (and (< (1+ indent) (length s))
               (member (char s indent) '(#\* #\-))
               (char= (char s (1+ indent)) #\Space))
          (concatenate 'string (subseq s 0 indent) "•" (subseq s (1+ indent)))
          s))))

(defun reply-lines (text)
  "A reply's markdown as buffer lines: prose wrapped and commented (;; ), a ```lisp
   block raw — live code — and any other fenced block commented with a bar (;; | ).
   A blank line stays blank, so paragraphs read as paragraphs. A # heading becomes a
   ;; ▌ line; a bullet is drawn • and its continuation lines hang under its text."
  (let ((out '()) (fence nil) (width (max 30 (- *agent-wrap* 3))))
    (dolist (line (split-lines* text))
      (let ((trimmed (string-left-trim " " line)))
        (cond
          ((and (>= (length trimmed) 3) (string= "```" trimmed :end2 3))
           (setf fence (if fence nil (if (lisp-fence-p (subseq trimmed 3)) :lisp :other))))
          ((eq fence :lisp) (push line out))
          ((eq fence :other) (push (concatenate 'string ";; | " line) out))
          ((string= trimmed "") (push "" out))
          ((char= (char trimmed 0) #\#)
           (push (concatenate 'string ";; ▌ " (clean-markdown (string-left-trim "# " trimmed))) out))
          (t
           (let* ((clean (clean-markdown line))
                  (indent (or (position #\Space clean :test-not #'char=) 0))
                  (bullet (and (< indent (length clean)) (char= (char clean indent) #\•))))
             (if bullet
                 (let ((hang (make-string (+ indent 2) :initial-element #\Space)))
                   (loop for l in (wrap-words (subseq clean (+ indent 2)) (- width indent 2))
                         for first = t then nil
                         do (push (concatenate 'string ";; " (if first (subseq clean 0 (+ indent 2)) hang) l) out)))
                 (dolist (l (wrap-words clean width)) (push (concatenate 'string ";; " l) out))))))))
    (nreverse out)))

(defun call-status-word (call)
  (case (call-status call)
    (:waiting "WAITING FOR YOUR APPROVAL")
    (:running "running") (:pending "...") (:denied "declined") (:error "error") (t nil)))

(defun call-mark (call)
  "One glyph for a tool call's state, read before its name: ✓ done, ✗ declined or
   failed, ● running, ? waiting for you, … not started."
  (case (call-status call)
    (:done "✓") ((:denied :error) "✗") (:running "●") (:waiting "?") (t "…")))

(defun call-lines (call)
  "A tool call as transcript lines: one ;; #N line — its state as a glyph, what it did,
   → what came back (C-c C-o on it opens what it was about, §8.6) — and for a WRITE its
   input in full, so what you approve is on screen."
  (cons (format nil ";; #~d ~a ~a~@[ → ~a~]~@[  [~a]~]" (call-number call) (call-mark call)
                (call-headline call) (call-summary call)
                (and (not (eq (call-status call) :done)) (call-status-word call)))
        (when (write-tool-p (call-name call))
          (let ((body (and (call-input call)
                           (lol.json:jget (call-input call) (or (editable-input-key call) "")))))
            (when (and body (find #\Newline body))
              (mapcar (lambda (l) (concatenate 'string ";;   | " l)) (split-lines* body)))))))

(defun thinking-lines (text)
  (let ((s (string-trim '(#\Space #\Newline) (substitute #\Space #\Newline text))))
    (when (plusp (length s))
      (list (format nil ";; ⋯ ~a" (lol.widgets:clip-text s (- *agent-wrap* 6)))))))

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
          ((equal type "tool_use") (list (format nil ";; ● ~a …" (lol.json:jget (live-block-json lb) "name"))))
          (t nil))))

(defun usage-line (conv turn)
  (let ((u (turn-usage turn)))
    (format nil ";; ── ~:d in · ~:d cached · ~:d written · ~:d out · ~a   (conversation ~a)"
            (getf u :input) (getf u :cache-read) (getf u :cache-write) (getf u :output)
            (format-cost (getf u :cost)) (format-cost (conversation-cost conv)))))

(defparameter +error-prefix+ ";; !! "
  "What starts every line of a failure's block. The pane's TINT draws a line that
   starts with it in the error colour — decided from the text, so it holds after any
   edit — and the block stays a Lisp comment, so the buffer still evaluates.")

(defun error-line-p (line)
  (and (>= (length line) (length +error-prefix+))
       (string= +error-prefix+ line :end2 (length +error-prefix+))))

(defun conversation-tint (line)
  "A conversation pane's TINT: one colour per kind of line, so the transcript can be
   read at a glance. The reply's prose is bright (it is the answer, not a comment to
   skip), the question gold, tool calls teal (amber while one waits for you, red when
   declined or failed), thinking and the meter dim, the your-turn line green, error
   blocks red — the colour the browser gives a condition everywhere. Code lines (no
   leading ;;) return NIL and keep the Lisp lexer's colours."
  (flet ((starts (p) (and (>= (length line) (length p)) (string= p line :end2 (length p)))))
    (cond
      ((not (starts ";;")) nil)
      ((string= line +your-turn+) (concept-color :generic-function))
      ((error-line-p line) (concept-color :condition))
      ((starts ";; ━") *ui-dim*)
      ((or (starts ";; ▸ ") (starts ";; ? ")) (concept-color :package))
      ((starts ";; ▌") (concept-color :package))
      ((and (starts ";; #") (> (length line) 4) (digit-char-p (char line 4)))
       (cond ((search "WAITING FOR YOUR APPROVAL" line) *ui-amber*)
             ((or (search " ✗ " line) (search "[declined]" line) (search "[error]" line))
              (concept-color :condition))
             (t (concept-color :subclass))))
      ((starts ";;   | ") (concept-color :class))
      ((starts ";; | ") (concept-color :class))
      ((starts ";; ◆") (concept-color :function))                 ; a finding: bright teal
      ((starts ";; ✔") (concept-color :generic-function))         ; a summary / report: green
      ((and (starts ";; ⋯") (search "✗" line)) (concept-color :condition))
      ((and (starts ";; ⋯") (search "●" line)) *ui-amber*)      ; steps still running
      ((or (starts ";; ⋯") (starts ";; (thinking")) *ui-dim*)
      ((or (starts ";; ──") (starts ";; --") (starts ";; ·")) *ui-dim*)
      ((or (starts ";; ●") (starts ";; ! ") (starts ";; ...") (starts ";; >")) *ui-amber*)
      (t *ui-text*))))

(defun failure-lines (turn)
  "TURN's failure as the transcript's error block: the headline, what is known, what
   to do — every line ;; !! so it reads as one red block — and below it how to retry.
   Whatever had streamed before the failure is kept above it, as reply text."
  (let ((f (turn-failure turn))
        (width (max 30 (- *agent-wrap* (length +error-prefix+) 2))))
    (flet ((error-block-lines (text indent)
             (loop for l in (split-lines* text)
                   append (mapcar (lambda (w) (concatenate 'string +error-prefix+ indent w))
                                  (wrap-words (string-trim " " l) width)))))
      (append
       (let ((p (turn-partial turn)))
         (when (and p (plusp (length (string-trim '(#\Space #\Newline) p))))
           (append (reply-lines p) (list ";; (the answer broke off here)"))))
       (error-block-lines (format nil "FAILED: ~a" (getf f :headline)) "")
       (loop for d in (getf f :details) append (error-block-lines d "  "))
       (loop for a in (getf f :advice)
             for first = t then nil
             append (error-block-lines (format nil "~:[  ~;What to do: ~]~a" first a) "  "))))))

(defun authority-words (authority)
  "AUTHORITY (:read-only :ask :allow) as the transcript says it."
  (case authority
    (:allow "allow all — tools run without asking")
    (:ask "asks before each write")
    (:read-only "read-only")
    (t (string-downcase (princ-to-string authority)))))

(defun turn-rule (n authority)
  "The heavy rule that opens turn N: its number and the authority it ran under, so a
   long transcript reads as a list of exchanges and the scrollbar of your eye has
   something to stop on."
  (let ((head (format nil ";; ━━ ~d ━━ ~a " n (authority-words authority))))
    (if (< (length head) *agent-wrap*)
        (concatenate 'string head (make-string (- *agent-wrap* (length head)) :initial-element #\━))
        head)))

(defvar *agent-steps-detail* :concise
  "How a conversation shows its tool calls. :CONCISE folds each run of them into one
   dim line (⋯ 6 steps · read 4 · ran 2 ✓) and hides the thinking; :VERBOSE shows every
   ;; #N line (C-c C-o opens one) and the thinking. A call waiting for your approval is
   always shown in full. The conversation's menu toggles it.")

(defun call-verb (name)
  "The word a steps line counts tool NAME under."
  (cond ((member name '("lisp-source" "lisp-senders" "lisp-describe" "lisp-describe-class"
                        "lisp-describe-gf" "lisp-apropos" "lisp-modules" "file-read" "file-list"
                        "lisp-inspect" "lisp-applicable" "lisp-backtrace")
                 :test #'string=) "read")
        ((string= name "lisp-eval") "evaluated")
        ((string= name "lisp-accept") "compiled")
        ((string= name "file-write") "wrote")
        ((string= name "container-run") "ran")
        ((string= name "update-plan") "planned")
        ((string= name "spawn-subagent") "spawned")
        ((string= name "await-subagents") "awaited")
        ((string= name "browse") "showed")
        ((string= name "web-fetch") "fetched")
        (t name)))

(defun steps-line (calls live-name)
  "One dim line for a run of tool CALLS: ⋯ N steps · read 3 · compiled 2 ✓. LIVE-NAME,
   when the run is still growing, is the tool being written right now: the line then
   ends with what is happening (● name …) instead of the verdict."
  (let* ((n (+ (length calls) (if live-name 1 0)))
         (counts '()))
    (dolist (c calls)
      (let* ((v (call-verb (call-name c))) (cell (assoc v counts :test #'string=)))
        (if cell (incf (cdr cell)) (push (cons v 1) counts))))
    (let* ((bad (count-if (lambda (c) (member (call-status c) '(:error :denied))) calls))
           (running (find-if (lambda (c) (member (call-status c) '(:running :pending))) calls :from-end t))
           (now (cond (live-name (format nil "● ~a …" live-name))
                      (running (format nil "● ~a" (lol.widgets:clip-text (call-headline running) 50)))
                      ((plusp bad) (format nil "✗ ~d failed" bad))
                      (t "✓"))))
      (format nil ";; ⋯ ~d step~:p~{ · ~a~} ~a" n
              (mapcar (lambda (c) (format nil "~a ~d" (car c) (cdr c))) (reverse counts))
              now))))

(defun turn-body-lines (conv turn)
  "The assistant's side of TURN as lines: its text as replies, and its tool calls —
   folded into STEPS-LINEs under *AGENT-STEPS-DETAIL* :CONCISE (a call that waits for
   your approval is shown in full regardless), or one ;; #N line each under :VERBOSE,
   with the thinking. Includes the reply still streaming."
  (let ((out '()) (run '()) (verbose (eq *agent-steps-detail* :verbose)))
    (labels ((emit (lines) (dolist (l lines) (push l out)))
             (flush (&optional live-name)
               (when (or run live-name)
                 (let ((calls (reverse run)))
                   (if verbose
                       (dolist (c calls) (emit (call-lines c)))
                       (progn
                         (emit (list (steps-line calls live-name)))
                         (dolist (c calls)
                           (when (eq (call-status c) :waiting) (emit (call-lines c)))))))
                 (setf run '())))
             (block-in (b)
               (let ((type (block-type b)))
                 (cond ((equal type "tool_use")
                        (when (block-call b)
                          (if verbose (emit (call-lines (block-call b))) (push (block-call b) run))))
                       ((equal type "text")
                        (let ((text (lol.json:jget (block-json b) "text")))
                          (when (plusp (length (string-trim '(#\Space #\Newline) (or text ""))))
                            (flush) (emit (reply-lines text)))))
                       ((and verbose (equal type "thinking"))
                        (emit (thinking-lines (or (lol.json:jget (block-json b) "thinking") "")))))))
             (live-in (lb)
               (let ((type (live-block-type lb)))
                 (cond ((equal type "text")
                        (let ((text (live-text lb)))
                          (when (plusp (length (string-trim '(#\Space #\Newline) (or text ""))))
                            (flush) (emit (reply-lines text)))))
                       ((equal type "tool_use")
                        (let ((name (lol.json:jget (live-block-json lb) "name")))
                          (if verbose
                              (emit (list (format nil ";; ● ~a …" name)))
                              (progn (flush name) (setf run '())))))
                       ((and verbose (equal type "thinking"))
                        (emit (thinking-lines (live-text lb))))))))
      (let ((ms (conversation-messages conv)))
        (loop for i from (turn-start turn) below (length ms)
              for m = (aref ms i)
              when (equal (message-role m) "assistant")
                do (dolist (b (message-blocks m)) (block-in b))))
      (let ((live (and (not (turn-done turn)) (conversation-live conv))))
        (if live
            (let ((blocks (live-reply-blocks live)))
              ;; a live tool_use joins the run before it, on the same line
              (if (and (not verbose) run blocks
                       (every (lambda (lb) (equal (live-block-type lb) "tool_use")) blocks))
                  (flush (lol.json:jget (live-block-json (car (last blocks))) "name"))
                  (progn (dolist (lb blocks) (live-in lb)) (flush))))
            (flush))))
    (nreverse out)))

(defun turn-lines (conv turn)
  "TURN as buffer lines: a rule with its number, the question (;; ▸), the assistant's
   replies with its tool calls folded into steps lines (TURN-BODY-LINES), the notes, a
   failure's error block, and — once done — the usage and the your-turn line."
  (with-conversation-lock (conv)
    (let* ((n (1+ (or (position turn (conversation-turns conv)) 0)))
           (lines (append
                   (list (turn-rule n (conversation-authority* conv)))
                   (mapcar (lambda (l) (concatenate 'string ";; ▸ " l))
                           (loop for q in (split-lines* (if (turn-again turn)
                                                            (format nil "~a   (asked again)" (turn-question turn))
                                                            (turn-question turn)))
                                 append (wrap-words q (- *agent-wrap* 5))))
                   (list "")
                   (turn-body-lines conv turn)
                   (mapcar (lambda (n) (concatenate 'string ";; ! " n)) (turn-notes turn))
                   (when (turn-failure turn) (failure-lines turn)))))
      (if (turn-done turn)
          (append lines
                  ;; a failed turn that never got an answer spent nothing: no meter line
                  (unless (and (turn-failure turn) (zerop (or (getf (turn-usage turn) :input) 0)))
                    (list (usage-line conv turn)))
                  (list +your-turn+ ""))
          (append lines (list (format nil ";; ● ~a   (C-g stops)" (conversation-status conv))))))))

(defun conversation-header (conv)
  (list (format nil ";; ▌ Agent · ~a · ~a" (conversation-title conv) (conversation-model conv))
        (format nil ";; · authority: ~a   (right-click the pane to change it)"
                (authority-words (conversation-authority* conv)))
        ";; · Type below the last \"your turn\" line, then C-c C-a asks. C-g stops a turn."
        ";; · Code in a reply is live: C-x C-e evaluates it, C-c C-c compiles it (change set)."
        ";; · C-c C-o on a ;; #N line opens what that tool call read. A red ;; !! block is a"
        ";; · failure: C-c C-a with nothing typed asks the same question again."
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
             ;; the symbol, not #'…: a redefined CONVERSATION-TINT reaches open panes
             :tint 'conversation-tint
             :data (list :conversation c)))

(defmethod commands-for ((c conversation))
  (list (make-command :label "Stop the turn" :action (lambda () (cancel-conversation c)) :key "C-g")
        (if (some #'activity-side-p (conversation-panes c))
            (make-command :label "Hide the activity panel" :action (lambda () (toggle-activity-panel c)))
            (make-command :label "Show the activity panel" :action (lambda () (toggle-activity-panel c))))
        (make-command :label (if (eq *agent-steps-detail* :verbose)
                                 "Steps: concise (fold tool calls into one line)"
                                 "Steps: verbose (every tool call, and the thinking)")
                      :action #'toggle-steps-detail)
        (if (eq (conversation-authority* c) :allow)
            (make-command :label "Ask before writes again"
                          :action (lambda () (ask-before-writes)
                                    (when (conversation-authority c) (setf (conversation-authority c) :ask))))
            (make-command :label "Allow all writes (stop asking, this session)"
                          :action (lambda () (allow-all-writes :conversation c))))
        (make-command :label "Allow all writes, always (save to ~/.config/lol/agent.lisp)"
                      :action (lambda () (allow-all-writes :conversation c :save t)))
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

(defstruct (subj-agent-activity (:constructor subj-agent-activity (conversation)))
  "What CONVERSATION is doing, as a subject: its plan, what waits for you, its
   subagents and what it changed. Shown in the conversation pane's side split
   (SuperL-o focuses it; the conversation's menu hides or shows it) — layout A of
   doc/design/agent-workbench.html. Not chrome: a list view like any other, whose rows
   drill (a subagent to its conversation, a change to its definition, a waiting call
   to its input), which keeps doc/agent.org §8.0's four moves."
  conversation)

(defun plan-state-mark (state)
  (case state (:done "✓") (:active "●") (:failed "✗") (t "○")))

(defun plan-state-kind (state)
  "A plan row's colour, by the concept palette: done green, active gold, failed red,
   pending plain."
  (case state (:done :generic-function) (:active :package) (:failed :condition) (t nil)))

(defun conversation-changes (conv)
  "What CONV's tool calls changed, oldest first: (KIND LABEL SUBJECT) with KIND :defn
   (a lisp-accept) or :file (a file-write). Only calls that ran."
  (let ((seen (make-hash-table :test 'equal)) (out '()))
    (loop for call across (conversation-calls conv)
          when (eq (call-status call) :done)
            do (let* ((name (call-name call))
                      (kind (cond ((string= name "lisp-accept") :defn)
                                  ((string= name "file-write") :file)))
                      (summary (or (call-summary call) ""))
                      (label (cond ((eq kind :defn)
                                    (let ((p (search "accepted " summary)))
                                      (if p (subseq summary (+ p 9)) summary)))
                                   ((eq kind :file)
                                    (or (ignore-errors (lol.json:jget (call-input call) "path")) summary)))))
                 (when (and kind (not (gethash label seen)))
                   (setf (gethash label seen) t)
                   (push (list kind label (call-subject call)) out))))
    (nreverse out)))

(defun subagent-elapsed (child)
  (let ((t0 (activity-started (conversation-activity child))))
    (if t0 (format nil "~ds" (round (- (get-internal-real-time) t0) internal-time-units-per-second)) "")))

(defmethod present ((s subj-agent-activity))
  "The Activity view: four groups — plan, waiting for you, subagents, changes — each
   row coloured by state (done green, active gold, waiting amber, failed red)."
  (let* ((conv (subj-agent-activity-conversation s))
         (act (conversation-activity conv))
         (plan (activity-plan act))
         (done (count :done plan :key #'cdr))
         (kids (activity-children act))
         (running (count-if #'conversation-busy-p kids))
         (waiting (loop for c across (conversation-calls conv)
                        when (eq (call-status c) :waiting) collect c))
         (all-changes (conversation-changes conv))
         ;; newest first, and only the last dozen: the rest are one drill away (the
         ;; change set, SuperL-Space › change set)
         (changes (subseq (reverse all-changes) 0 (min 12 (length all-changes))))
         (items '()))
    (flet ((add (&rest args) (push (apply #'make-item args) items)))
      ;; now: one line saying what it is doing, and what it has cost
      (add :label (format nil "~a ~a" (if (conversation-busy-p conv) "●" "○")
                          (if (conversation-busy-p conv) (conversation-status conv) "idle"))
           :detail (format nil "~a · ~(~a~)" (format-cost (conversation-cost conv))
                           (conversation-authority* conv))
           :kind (if (conversation-busy-p conv) :package nil) :group "now")
      ;; the plan
      (let ((g (if plan
                   (format nil "plan · ~d/~d ~a" done (length plan)
                           (let ((n 12))
                             (with-output-to-string (o)
                               (dotimes (i n)
                                 (write-char (if (< i (round (* n done) (max 1 (length plan)))) #\━ #\─) o)))))
                   "plan")))
        (if plan
            (loop for (title . state) in plan
                  do (add :label (format nil "~a ~a" (plan-state-mark state) title)
                          :kind (plan-state-kind state) :group g))
            (add :label "no plan yet" :detail "the agent writes one with update-plan" :group g)))
      ;; what waits for you
      (when waiting
        (dolist (c waiting)
          (add :label (format nil "? #~d ~a" (call-number c) (call-name c))
               :detail "1 allow · 2 deny · 5 allow all" :kind :restart
               :subject c :group (format nil "waiting for you · ~d" (length waiting)))))
      ;; subagents
      (let ((g (format nil "subagents · ~d running · ~d done" running (- (length kids) running))))
        (if kids
            (dolist (k kids)
              (let* ((kact (conversation-activity k))
                     (busy (conversation-busy-p k))
                     (failed (and (not busy) (failed-turn k)))
                     (report (and (not busy) (not failed) (conversation-final-text k))))
                (add :label (format nil "◇~d ~a ~a" (conversation-id k)
                                    (cond (busy "●") (failed "✗") (t "✓"))
                                    (lol.widgets:clip-text (first-line (or (activity-goal kact) "")) 44))
                     :detail (format nil "~a · ~a · ~a"
                                     (cond (busy (conversation-status k))
                                           (failed "failed")
                                           (report (lol.widgets:clip-text
                                                    (string-left-trim "✔ Report:" (first-line report)) 40))
                                           (t "done"))
                                     (subagent-elapsed k) (format-cost (conversation-cost k)))
                     :kind (cond (busy :class) (failed :condition) (t :subclass))
                     :subject k :group g)))
            (add :label "none" :detail "the agent starts them with spawn-subagent" :group g)))
      ;; changes
      (let ((g (format nil "changes · ~d" (length changes))))
        (if changes
            (loop for (kind label subject) in changes
                  do (add :label (format nil "~a ~a" (if (eq kind :defn) "λ" "▤") label)
                          :detail (if (eq kind :defn) "in the image" "written")
                          :kind (if (eq kind :defn) :function :directory)
                          :subject subject :group g))
            (add :label "nothing changed yet" :group g))))
    (make-view :title (format nil "activity · ~a" (conversation-title conv))
               :kind :list :subject s :items (nreverse items))))

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

(defun start-turn-async (conv prompt question origin &key (executor #'post-job-and-wait) again)
  "Run a turn on its own thread, so the environment keeps drawing (§8.7). Tool calls
   come back to the environment's thread as jobs (POST-JOB-AND-WAIT). AGAIN re-asks a
   failed turn (RUN-TURN). Nothing that goes wrong on the thread is lost: RUN-TURN
   makes any failure the turn's, and anything that escapes even that is still made a
   failed turn here, in the transcript — a thread that dies silently is the one
   outcome this must never have."
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
             (handler-case (run-turn conv prompt :question question :origin origin :again again)
               (serious-condition (c)
                 (log-condition (format nil "agent turn ~d failed outside RUN-TURN" (conversation-id conv)) c)
                 (ignore-errors (fail-turn conv c :question question))
                 (with-conversation-lock (conv)
                   (setf (conversation-state conv) :idle (conversation-thread conv) nil))
                 (touch conv)))))
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
      ;; nothing typed after a failed turn: ask that question again (its own message
      ;; is still the last one in the history — RUN-TURN's AGAIN sends it as it is)
      ((and (zerop (length draft)) (failed-turn conv))
       (let ((failed (failed-turn conv)))
         (setf *conversation* conv
               *browser-status* "asking again...")
         (start-turn-async conv (or (turn-prompt failed) (turn-question failed)) (turn-question failed) pane
                           :again t)))
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
   a parallel list: what you pick is what gets invoked. Below them, ALLOW ALL: run this
   call and stop asking, for the session (5) or for good (6, saved in
   ~/.config/lol/agent.lisp) — see ALLOW-ALL-WRITES."
  (let* ((call (approval-call c))
         (conv (approval-conversation c))
         (group (format nil "approve #~d · ~a" (call-number call) (call-name call))))
    (append
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
                                              :key "4" :group group))))))
     (let ((allow (find-restart 'allow c)))
       (when allow
         (list (menu-entry "Allow all — stop asking this session"
                           (lambda () (allow-all-writes :conversation conv) (invoke-restart allow))
                           :key "5" :group group)
               (menu-entry "Allow all — always (saved to ~/.config/lol/agent.lisp)"
                           (lambda ()
                             ;; a settings file that cannot be written must not cost you
                             ;; the call: fall back to this session only
                             (unless (ignore-errors (allow-all-writes :conversation conv :save t))
                               (allow-all-writes :conversation conv))
                             (invoke-restart allow))
                           :key "6" :group group)))))))

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
          (draw-backdrop (symbol-value '*fb-canvas*) (symbol-value '*fb-xres*) (symbol-value '*fb-yres*))))
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
               ;; named for the watcher: a tool that hangs here hangs the screen
               (with-main-activity ("agent tool ~a (conversation ~d)"
                                    (call-name (agent-job-call job)) (conversation-id conv))
                 (run-job job)))
             (setf ran t))))))
    ran))

(defun activity-side-p (pane)
  "Is PANE's side pane an Activity view?"
  (and (pane-side pane) (subj-agent-activity-p (pane-subject (pane-side pane)))))

(defun refresh-activity-side (pane)
  "Re-present PANE's Activity side pane from the live conversation, keeping its
   selection where it was (clamped). Returns T."
  (let* ((side (pane-side pane))
         (v (present (pane-subject side))))
    (setf (pane-view side) v
          (pane-sel side) (max 0 (min (pane-sel side) (1- (max 1 (length (view-items v)))))))
    t))

(defun ensure-activity-sides ()
  "Give every pane showing a conversation its Activity side pane (layout A), unless
   you hid it there (the conversation's menu: Hide the activity panel). Pinned, so C-g
   and a click back in the transcript do not take it away. Returns T if one was added."
  (let ((added nil))
    (dolist (p (all-panes))
      (let ((conv (pane-conversation p)))
        (when (and conv (pane-tv p) (null (pane-side p))
                   (not (getf (view-data (pane-view p)) :activity-hidden)))
          (setf (pane-side p) (open-pane (subj-agent-activity conv))
                (pane-side-pinned p) t
                added t))))
    added))

(defvar *activity-refreshed-at* 0
  "GET-INTERNAL-REAL-TIME of the last Activity refresh: while anything works the rail
   is redrawn once a second even without a revision, so its timers move.")

(defun refresh-activity-sides (&optional force)
  "Re-present every Activity side pane: on FORCE (a conversation's revision moved — a
   subagent's included, whose card is in its parent's rail), or once a second while
   any conversation works. Returns T if anything was refreshed."
  (let ((now (get-internal-real-time)))
    (when (or force
              (and (some #'conversation-busy-p *conversations*)
                   (> (- now *activity-refreshed-at*) internal-time-units-per-second)))
      (setf *activity-refreshed-at* now)
      (let ((any nil))
        (dolist (p (all-panes))
          (when (activity-side-p p) (refresh-activity-side p) (setf any t)))
        any))))

(defun toggle-activity-panel (conv)
  "Hide the Activity side pane beside CONV's panes, or show it again."
  (dolist (p (conversation-panes conv))
    (let ((v (pane-view p)))
      (if (activity-side-p p)
          (setf (getf (view-data v) :activity-hidden) t
                (pane-side p) nil (pane-side-pinned p) nil *editing-side* nil)
          (setf (getf (view-data v) :activity-hidden) nil))))
  (ensure-activity-sides))

(defun toggle-steps-detail ()
  "Concise (steps folded into one line) <-> verbose (every ;; #N call and the
   thinking). Re-renders every open conversation in place."
  (setf *agent-steps-detail* (if (eq *agent-steps-detail* :verbose) :concise :verbose))
  (dolist (p (all-panes))
    (let ((conv (pane-conversation p)))
      (when (and conv (pane-tv p))
        (let ((lines (coerce (conversation-lines conv) 'simple-vector)))
          (lol.textview:tv-replace (pane-tv p) lines (1- (length lines)) 0)
          ;; the tail is re-rendered from here on by REFRESH-CONVERSATION-PANE
          (let ((turn (current-turn conv)))
            (setf (getf (view-data (pane-view p)) :turn) turn
                  (getf (view-data (pane-view p)) :finished) (and turn (turn-done turn))
                  (getf (view-data (pane-view p)) :live-start)
                  (- (length lines) (if turn (length (turn-lines conv turn)) 0))))))))
  (setf *browser-status* (format nil "agent steps: ~(~a~)" *agent-steps-detail*)))

(defun agent-ui-tick ()
  "Called by the environment's loop every iteration. Runs posted tool calls, refreshes
   every pane showing a conversation whose revision moved, and keeps their Activity
   side panes attached and current (layout A). T = redraw."
  (let ((dirty nil) (moved nil))
    ;; panes first, so an approval about to be asked shows its call on screen
    (dolist (conv *conversations*)
      (let ((rev (conversation-revision conv)))
        (unless (eql rev (gethash conv *agent-seen-revisions*))
          (setf (gethash conv *agent-seen-revisions*) rev moved t)
          (dolist (p (conversation-panes conv))
            (when (refresh-conversation-pane p conv) (setf dirty t)))
          (setf dirty t)))
      ;; a failure is said on the status line too, once — set HERE, on the UI thread
      ;; that owns *BROWSER-STATUS*, never from the turn's thread
      (let ((failed (failed-turn conv)))
        (when (and failed (not (turn-announced failed)))
          (setf (turn-announced failed) t
                *browser-status* (format nil "agent: ~a ~:[— SuperL-a shows why~;— C-c C-a with nothing typed asks again~]"
                                         (getf (turn-failure failed) :headline) (conversation-visible-p conv))
                dirty t))))
    (when (ignore-errors (ensure-activity-sides)) (setf dirty t moved t))
    (when (ignore-errors (refresh-activity-sides moved)) (setf dirty t))
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
  "The tray's agent item: the agent's creature (DRAW-AGENT-ICON), the authority and the
   session's cost, always on screen — an environment that can spend money owes you a
   visible meter (§8.9). A trailing … while a turn runs, ? while one waits for you."
  (list :kind :agent :icon :agent :tone (authority-tone)
        :text (format nil "~(~a~) ~a~a"
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
    ;; lisp-backtrace reads this copy of the stack (agent-tools.lisp says why a copy)
    (when (fboundp 'note-debugger-context) (funcall 'note-debugger-context conv condition frames))
    (ensure-conversation-tab conv)
    (start-turn-async conv (condition-prompt condition frames)
                      (format nil "Explain this ~(~a~) and propose a fix." (type-of condition))
                      :debugger :executor nil)
    (run-agent-debugger-view conv)))

(defun run-agent-debugger-view (conv)
  "A modal view of CONV over the debugger: it fills in as the answer streams, arrows
   and PageUp/Down move the caret, C-c C-c accepts the form before it, C-g stops the
   turn, Esc returns to the rail. An overlay the caller waits in (RUN-AGENT-VIEW,
   fb-browser.lisp — the framebuffer debugger's own file, loaded after this one)."
  (funcall 'run-agent-view conv))
