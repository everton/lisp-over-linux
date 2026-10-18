;;;; agent-tools.lisp — the rest of the agent's tool surface (doc/agent.org §2.2, §8.5):
;;;; inspect a live object, preview a generic function's dispatch, read the stack the
;;;; debugger stood on, show the user a subject, and fetch a web page.
;;;;
;;;;   lisp-inspect     an object's fields, as places — the inspector (model.lisp
;;;;                    INSPECT-PLACES), drilled by a PATH of field labels
;;;;   lisp-applicable  which methods run, and in what order, for a call dispatching on
;;;;                    some classes — the effective-method onion (EFFECTIVE-METHOD-ONION)
;;;;   lisp-backtrace   the frames and locals of the condition a debugger conversation
;;;;                    was opened on (a SNAPSHOT taken when ASK was chosen — below)
;;;;   browse           §8.5 "it can show you things": put a subject in front of the
;;;;                    user, BESIDE the conversation, never in place of what they see
;;;;   web-fetch        GET a URL with the image's own HTTP-REQUEST, return its text —
;;;;                    GATED like a write, because a URL can carry data out
;;;;
;;;; Every tool here is a DEFINE-AGENT-TOOL like the ones in agent.lisp, and so goes
;;;; through the same EXECUTE-TOOL-CALL: input checked against the schema, a gated one
;;;; through the TOOL-APPROVAL-REQUEST condition, any error turned into an is_error
;;;; result. Loaded after agent-ui.lisp, so the tools can reach the environment's
;;;; panes — always through FBOUNDP / BOUNDP guards, so the host test (which loads no
;;;; shell) runs them too.
;;;;
;;;; One rule holds for all of them (doc/agent.org commit 803f7f4, §14.8½): a
;;;; conversation's tools are frozen at its first request. These reach the model in
;;;; conversations started after this file is loaded — or, on a running image, after
;;;; C-c C-c on one of them.
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

;;; ===========================================================================
;;; Printing values for the model: short, one line each, and never the key
;;; ===========================================================================

(defun short-value (x &optional (limit 160))
  "X printed on one line, bounded — what a field's value looks like in a tool result.
   The API key is never printed (REDACT-API-KEY in agent.lisp does the same to every
   tool result, as a second line of defence)."
  (if (and *agent-api-key* (stringp x) (string= x *agent-api-key*))
      "\"[the API key — never shown]\""
      (let ((s (or (ignore-errors
                    (let ((*print-length* 8) (*print-level* 2) (*print-lines* 1)
                          (*print-circle* t) (*print-pretty* nil) (*print-case* :downcase))
                      (prin1-to-string x)))
                   (format nil "#<unprintable ~(~a~)>" (ignore-errors (type-of x))))))
        (setf s (substitute #\Space #\Newline s))
        (if (> (length s) limit) (concatenate 'string (subseq s 0 (1- limit)) "…") s))))

(defun field-label-string (label)
  (if (stringp label) label
      (let ((*print-case* :downcase)) (princ-to-string label))))

;;; ===========================================================================
;;; lisp-inspect
;;; ===========================================================================

(defun tool-call-numbered (conv n)
  "The tool call #N of CONV, or NIL."
  (and conv (integerp n)
       (find n (conversation-calls conv) :key #'call-number)))

(defun inspect-root (input)
  "The object lisp-inspect starts from: an earlier call's value (CALL), or what the
   symbol NAME names — its value when it is bound, else its function, else its class,
   else the symbol itself — unless OF says which. Never evaluates anything."
  (let ((n (lol.json:jget input "call"))
        (name (lol.json:jget input "name"))
        (of (string-downcase (or (lol.json:jget input "of") ""))))
    (cond
      (n (let ((call (or (tool-call-numbered *tool-conversation* n)
                         (error "there is no tool call #~a in this conversation" n))))
           (unless (call-subject call)
             (error "tool call #~a has no object to inspect (it returned only text)" n))
           (subject-value (call-subject call))))
      (name
       (let ((sym (require-symbol name)))
         (cond ((string= of "symbol") sym)
               ((string= of "value")
                (if (boundp sym) (symbol-value sym) (error "~(~s~) has no value" sym)))
               ((string= of "function")
                (if (fboundp sym) (fdefinition sym) (error "~(~s~) names no function" sym)))
               ((string= of "class")
                (or (find-class sym nil) (error "~(~s~) names no class" sym)))
               ((and (boundp sym) (not (constantp sym))) (symbol-value sym))
               ((fboundp sym) (fdefinition sym))
               ((find-class sym nil))
               (t sym))))
      (t (error "give NAME (a symbol) or CALL (an earlier tool call's number)")))))

(defun inspect-step (obj step)
  "The field of OBJ that STEP (a string) names: a label, compared as printed and
   without regard to case — or, failing that, the field's position as a number."
  (let* ((places (inspect-places obj))
         (hit (or (find step places :key (lambda (p) (field-label-string (getf p :label)))
                                    :test #'string-equal)
                  (let ((i (ignore-errors (parse-integer step))))
                    (and i (< -1 i (length places)) (nth i places))))))
    (unless hit
      (error "~a has no field ~s (its fields: ~{~a~^, ~})" (object-title obj) step
             (mapcar (lambda (p) (field-label-string (getf p :label)))
                     (subseq places 0 (min 20 (length places))))))
    (getf hit :value)))

(define-agent-tool "lisp-inspect" ()
  "Inspect a live object field by field — the inspector the user has. Start from a
   symbol (NAME: its value if bound, else its function, else its class; OF picks one
   of value | function | class | symbol) or from the value an earlier tool call
   returned (CALL, its #N — e.g. what lisp-eval gave back). PATH drills down: each
   element is a field label from the previous answer (or its position). Read-only:
   it never evaluates anything, so it can look at any state the image holds."
  (("name" "string" "A symbol, e.g. \"*conversations*\" or \"lol.shell::*trails*\"")
   ("of" "string" "value | function | class | symbol (default: the first that applies)")
   ("call" "integer" "Start from tool call #N's value instead of a symbol")
   ("path" "array" "Field labels (or positions) to drill through, e.g. [\"0\", \"messages\"]"))
  ()
  (input call)
  (let* ((path (coerce (or (lol.json:jget input "path") #()) 'list))
         (obj (inspect-root input)))
    (unless (every #'stringp path) (error "path must be strings"))
    (dolist (step path) (setf obj (inspect-step obj step)))
    (let* ((places (inspect-places obj))
           (shown (subseq places 0 (min 200 (length places)))))
      (setf (call-subject call) (inspect-subject obj)
            (call-summary call) (format nil "~a · ~d field~:p" (object-title obj) (length places)))
      (with-output-to-string (s)
        (format s "~a~%type: ~(~s~)~%" (short-value obj 300) (type-of obj))
        (when path (format s "path: ~{~a~^ › ~}~%" path))
        (format s "~d field~:p~:[~;, the first 200 shown~]~@[ (settable ones marked =)~]:~%"
                (length places) (> (length places) 200)
                (some (lambda (p) (getf p :set)) shown))
        (loop for p in shown for i from 0
              do (format s "  ~3d ~a ~a ~a~%" i (field-label-string (getf p :label))
                         (if (getf p :set) "=" ":") (short-value (getf p :value))))
        (format s "Drill with path [~{~s, ~}\"<label>\"].~%" path)))))

;;; ===========================================================================
;;; lisp-applicable
;;; ===========================================================================

(define-agent-tool "lisp-applicable" ()
  "Which methods of a generic function run for a call dispatching on given classes,
   and in what order: the effective method — :around outermost first, then :before,
   the primary call-next-method chain, :after — exactly as CLOS will weave it. CLASSES
   gives one class name per required argument (\"t\" for an argument that does not
   matter). Says when an eql method might also apply."
  (("name" "string" "The generic function, e.g. \"present\" or \"lol.examples:collide\"")
   ("classes" "array" "One class name per required argument, e.g. [\"ship\", \"asteroid\"]"))
  ("name" "classes")
  (input call)
  (let* ((gf (require-symbol (lol.json:jget input "name")))
         (names (coerce (lol.json:jget input "classes") 'list)))
    (unless (generic-function-p gf) (error "~(~s~) is not a generic function" gf))
    (unless (every #'stringp names) (error "classes must be class names, as strings"))
    (let ((arity (gf-required-arity gf)))
      (unless (= (length names) arity)
        (error "~(~s~) dispatches on ~d argument~:p; give that many classes" gf arity)))
    (let* ((classes (mapcar (lambda (n)
                              (let ((sym (require-symbol n)))
                                (unless (find-class sym nil) (error "~(~s~) is not a class" sym))
                                sym))
                            names))
           (onion (apply #'effective-method-onion gf classes))
           (n (+ (length (getf onion :around)) (length (getf onion :before))
                 (length (getf onion :primary)) (length (getf onion :after)))))
      (setf (call-subject call) (subj-onion gf classes)
            (call-summary call) (format nil "~d applicable method~:p" n))
      (format-onion onion))))

;;; ===========================================================================
;;; lisp-backtrace — over a SNAPSHOT of the debugger's stack
;;; ===========================================================================
;;; Why a snapshot and not the live stack. The debugger is modal on the environment's
;;; thread, and the conversation the ASK restart opens runs its tools on a thread of
;;; its own (agent-ui.lisp, RUN-AGENT-DEBUGGER-VIEW). sb-di frames are pointers into
;;; ANOTHER thread's control stack: reading them from here would be reading memory
;;; that thread owns, while it runs. So at the moment ASK is chosen — on the debugger's
;;; own thread, with the stack standing — every frame's name, definition and locals
;;; are copied out as plain strings. The stack cannot change under a modal debugger
;;; anyway, so the copy is exactly what the user sees on the rail.

(defvar *conversation-debug-contexts* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "CONVERSATION -> the debugger snapshot it was opened on (a plist), for lisp-backtrace.")

(defun note-debugger-context (conv condition frames)
  "Copy CONDITION and FRAMES (sb-di frame objects, innermost first) into CONV's
   snapshot. Called by AGENT-ASK-ABOUT-CONDITION on the debugger's thread. Never
   signals: the debugger must not fail while it reports a failure."
  (ignore-errors
    (setf (gethash conv *conversation-debug-contexts*)
          (list :type (type-of condition)
                :report (or (ignore-errors (princ-to-string condition)) "(unprintable)")
                :restarts (ignore-errors (mapcar #'princ-to-string (compute-restarts condition)))
                :frames (loop for f in frames for i from 0
                              collect (list :index i :label (frame-label f)
                                            :defn (ignore-errors (frame-defn-name f))
                                            :locals (frame-locals f)))))))

(define-agent-tool "lisp-backtrace" ()
  "In a conversation opened from the debugger (its ASK restart): the condition, the
   restarts, and the backtrace, innermost frame first. With FRAME, that frame's local
   variables and the source of its definition. The stack is the one the debugger
   stood on when ASK was chosen. In any other conversation it lists the image's
   threads instead (there is no condition to look at)."
  (("frame" "integer" "A frame's index, for its locals and source")
   ("limit" "integer" "Most frames to list (default 40)"))
  ()
  (input call)
  (let ((ctx (gethash *tool-conversation* *conversation-debug-contexts*))
        (frame (lol.json:jget input "frame")))
    (cond
      ((null ctx)
       (let ((threads (sb-thread:list-all-threads)))
         (setf (call-summary call) (format nil "no condition · ~d threads" (length threads)))
         (format nil "This conversation was not opened from the debugger, so there is no ~
                      condition and no stack to show.~%Threads in the image:~%~{  ~a~%~}"
                 (mapcar (lambda (th) (format nil "~a~:[ (finished)~;~]"
                                              (sb-thread:thread-name th) (sb-thread:thread-alive-p th)))
                         threads))))
      (frame
       (let ((f (or (find frame (getf ctx :frames) :key (lambda (f) (getf f :index)))
                    (error "no frame ~a (there are ~d)" frame (length (getf ctx :frames))))))
         (let* ((name (getf f :defn))
                (src (and name (ignore-errors (source-of name)))))
           (when name (setf (call-subject call) (subj-defn name)))
           (setf (call-summary call) (format nil "frame ~d · ~a" frame (getf f :label)))
           (with-output-to-string (s)
             (format s "frame ~d: ~a~%locals:~%" frame (getf f :label))
             (if (getf f :locals)
                 (dolist (l (getf f :locals))
                   (format s "  ~a = ~a~%" (getf l :name)
                           (if (eq (getf l :value) :unavailable) "(not live here)" (getf l :value))))
                 (format s "  (none recorded — compiled without debug variables)~%"))
             (cond (src (format s "source of ~(~s~):~%~a~%" name
                                (if (listp src) (format nil "~{~a~^~%~%~}" src) src)))
                   (name (format s "~(~s~) has no recorded source (a builtin: lisp-source shows what is known).~%" name)))))))
      (t
       (let* ((limit (let ((l (lol.json:jget input "limit"))) (if (integerp l) (max 1 l) 40)))
              (frames (getf ctx :frames)))
         (setf (call-summary call) (format nil "~(~a~) · ~d frames" (getf ctx :type) (length frames)))
         (with-output-to-string (s)
           (format s "condition: ~(~s~)~%report: ~a~%restarts:~%~{  ~a~%~}backtrace (innermost first):~%"
                   (getf ctx :type) (getf ctx :report) (getf ctx :restarts))
           (loop for f in frames repeat limit
                 do (format s "  ~2d ~a~@[   [source: ~(~s~)]~]~%" (getf f :index) (getf f :label)
                            (and (getf f :defn) (ignore-errors (source-of (getf f :defn))) (getf f :defn))))
           (format s "frame N gives a frame's locals and source.~%")))))))

;;; ===========================================================================
;;; browse — §8.5: "it can show you things"
;;; ===========================================================================
;;; The answer to "where is this used?" should be the senders view, open and
;;; drillable, not a list of names in prose. The consent rule is OPEN BESIDE, NEVER
;;; STEAL FOCUS. A conversation pane already has a side — the Activity panel
;;; (agent-ui.lisp) — so a browsed subject becomes a row in it, in a "shown to you"
;;; group: beside the conversation, one Enter away, and nothing you were looking at
;;; moves. When no Activity panel shows the conversation (a narrow screen, the panel
;;; hidden, the conversation not on screen at all) the subject opens in a tab of its
;;; own BEHIND the current one, the way an answer asked from elsewhere does.

(defvar *conversation-shown* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "CONVERSATION -> what browse showed, newest first: ((TITLE . SUBJECT) ...).")

(defparameter +browse-kinds+
  '("definition" "senders" "referrers" "class" "generic" "onion" "module" "modules"
    "file" "folder" "value" "call")
  "What browse can open (its WHAT).")

(defun browse-subject (what name)
  "The browser subject WHAT/NAME denotes — the same subjects the other tools record,
   so what the model shows you is the view you would reach by hand."
  (flet ((sym () (require-symbol (or name (error "~a needs a name" what)))))
    (cond
      ((string= what "definition") (let ((s (sym))) (unless (definitions-of s) (error "~(~s~) has no recorded definition" s)) (subj-defn s)))
      ((string= what "senders") (subj-xref :senders (sym)))
      ((string= what "referrers") (subj-xref :referrers (sym)))
      ((string= what "class") (or (find-class (sym) nil) (error "~a names no class" name)))
      ((string= what "generic") (let ((s (sym))) (if (generic-function-p s) (fdefinition s) (error "~(~s~) is not a generic function" s))))
      ((string= what "module")
       (let ((g (find name (categorize :file) :key (lambda (g) (string-downcase (princ-to-string (car g))))
                                              :test #'string-equal)))
         (unless g (error "no module ~a (lisp-modules lists them)" name))
         (subj-module (car g))))
      ((string= what "modules") (subj-modules))
      ((string= what "file") (let ((p (agent-path (or name "~/")))) (unless (eq (followed-type p) :regular) (error "~a is not a file" p)) (subj-file p)))
      ((string= what "folder") (let ((p (agent-path (or name "~/")))) (unless (eq (followed-type p) :directory) (error "~a is not a folder" p)) (subj-dir p)))
      ((string= what "value") (let ((s (sym))) (if (boundp s) (inspect-subject (symbol-value s)) (error "~(~s~) has no value" s))))
      ((string= what "call")
       (let* ((n (or (ignore-errors (parse-integer (string-left-trim "#" name))) (error "call needs a number, e.g. \"3\"")))
              (c (or (tool-call-numbered *tool-conversation* n) (error "no tool call #~d here" n))))
         (or (call-subject c) (error "tool call #~d was not about anything that can be shown" n))))
      (t (error "browse cannot show ~s; WHAT is one of ~{~a~^, ~}" what +browse-kinds+)))))

(defun view-digest (view &optional (rows 40))
  "What VIEW shows, as text for the model: its title, and its first ROWS rows (or
   lines of text) — so it knows what the user is looking at."
  (with-output-to-string (s)
    (format s "~a  [~(~a~) view]~%" (view-title view) (view-kind view))
    (let ((items (view-items view)) (group nil))
      (cond
        (items
         (loop for it in items repeat rows
               do (unless (equal (item-group it) group)
                    (setf group (item-group it))
                    (when group (format s "~a:~%" group)))
                  (format s "  ~a~@[  — ~a~]~%" (item-label it) (item-detail it)))
         (when (> (length items) rows) (format s "  … ~d more rows~%" (- (length items) rows))))
        ((view-text view)
         (let ((lines (split-sequence-lines (view-text view))))
           (format s "~{~a~%~}" (subseq lines 0 (min rows (length lines))))
           (when (> (length lines) rows) (format s "… ~d more lines~%" (- (length lines) rows)))))))))

(defun activity-shows-p (conv)
  "Is there a pane showing CONV with its Activity panel beside it?"
  (and (fboundp 'conversation-panes) (fboundp 'activity-side-p)
       (some (lambda (p) (funcall 'activity-side-p p)) (funcall 'conversation-panes conv))))

(defun show-subject-beside (conv subject title)
  "Put SUBJECT in front of the user for CONV (see the section comment). Returns a
   phrase saying where it went. Touches the environment's state only on its own
   thread — the main thread, where the environment runs — and only when there is an
   environment (*TRAILS* has a pane): a debugger conversation's tools run elsewhere,
   and a REPL has no screen."
  (let ((table *conversation-shown*))
    (setf (gethash conv table)
          (cons (cons title subject)
                (remove subject (gethash conv table) :key #'cdr :test #'equalp))))
  (when conv (touch conv))
  (cond
    ((not (and (sb-thread:main-thread-p) (boundp '*trails*) (symbol-value '*trails*)
               (fboundp 'new-trail)))
     "noted in the conversation's Activity panel (no environment on screen here)")
    ((activity-shows-p conv)
     "beside the conversation, in its Activity panel (\"shown to you\"); Enter opens it")
    (t
     (let ((active (symbol-value '*active*)))
       (funcall 'new-trail subject)
       (setf (symbol-value '*active*) active))   ; behind you: focus does not move
     "in a new tab, behind the one the user is reading")))

(define-agent-tool "browse" ()
  "Show the user a subject in the environment — the view they would reach by hand,
   live and drillable — instead of describing it in prose. It opens BESIDE the
   conversation (its Activity panel) and never takes the user's focus. WHAT is one of:
   definition, senders, referrers, class, generic (NAME a symbol); onion (NAME a
   generic, CLASSES its dispatch classes); module (NAME e.g. \"initramfs/net\"),
   modules; file, folder (NAME a path under home); value (NAME a variable, inspected);
   call (NAME an earlier tool call's number). Returns what the view shows."
  (("what" "string" "definition | senders | referrers | class | generic | onion | module | modules | file | folder | value | call")
   ("name" "string" "The symbol, path, module or call number")
   ("classes" "array" "For onion: one class name per required argument"))
  ("what")
  (input call)
  (let* ((what (string-downcase (lol.json:jget input "what")))
         (name (lol.json:jget input "name"))
         (subject (if (string= what "onion")
                      (let ((gf (require-symbol (or name (error "onion needs the generic's name"))))
                            (cs (mapcar #'require-symbol (coerce (or (lol.json:jget input "classes") #()) 'list))))
                        (unless (generic-function-p gf) (error "~(~s~) is not a generic function" gf))
                        (subj-onion gf cs))
                      (browse-subject what name)))
         (view (present subject))
         (where (show-subject-beside *tool-conversation* subject (view-title view))))
    (setf (call-subject call) subject
          (call-summary call) (format nil "showed ~a" (lol.widgets:clip-text (view-title view) 40)))
    (format nil "Shown to the user ~a.~%What it shows:~%~a" where (view-digest view))))

;;; The "shown to you" group of the Activity panel. An :AFTER-style :AROUND on the
;;; panel's PRESENT, so agent-ui.lisp needs no change: the rows go last, after the
;;; changes, newest first, at most eight.
(when (find-class 'subj-agent-activity nil)
  (defmethod present :around ((s subj-agent-activity))
    (let ((v (call-next-method))
          (shown (gethash (subj-agent-activity-conversation s) *conversation-shown*)))
      (when shown
        (setf (view-items v)
              (append (view-items v)
                      (loop for (title . subject) in shown repeat 8
                            collect (make-item :label (format nil "◈ ~a" (lol.widgets:clip-text title 48))
                                               :detail "shown by the agent" :kind :restart
                                               :subject subject
                                               :group (format nil "shown to you · ~d" (length shown)))))))
      v)))

;;; ===========================================================================
;;; web-fetch — a page's text, over the image's own HTTP-REQUEST
;;; ===========================================================================
;;; GATED (:write t), though it changes nothing here: a GET carries its URL out, and a
;;; URL can carry anything the conversation has read — a file under your home, a
;;; value in the image. The authority dial treats "data leaves the machine" like a
;;; write, so it asks under :ask and is not offered at all under :read-only (which is
;;; why a subagent cannot fetch).

(defun html-entity (name)
  "The character an HTML entity NAME (without & and ;) stands for, or NIL."
  (cond ((and (> (length name) 1) (char= (char name 0) #\#))
         (let ((code (ignore-errors
                      (if (char-equal (char name 1) #\x)
                          (parse-integer name :start 2 :radix 16)
                          (parse-integer name :start 1)))))
           (and code (< 0 code char-code-limit) (code-char code))))
        (t (cdr (assoc name '(("amp" . #\&) ("lt" . #\<) ("gt" . #\>) ("quot" . #\")
                              ("apos" . #\') ("nbsp" . #\Space) ("mdash" . #\—) ("ndash" . #\–)
                              ("hellip" . #\…) ("copy" . #\©))
                       :test #'string=)))))

(defparameter +html-block-tags+
  '("p" "div" "br" "li" "ul" "ol" "tr" "table" "h1" "h2" "h3" "h4" "h5" "h6" "pre"
    "section" "article" "header" "footer" "blockquote" "hr" "title" "dt" "dd")
  "Tags that start a new line in the text rendering.")

(defun html-text (html)
  "HTML as plain text, the way a reader skims it: <script>, <style> and comments
   dropped, block tags as line breaks, every other tag removed, entities decoded,
   runs of blank space collapsed. Not a parser — a page is not a tree we need, only
   its words. (The web pane, web.lisp, is the place for a page as a page.)"
  (let ((out (make-string-output-stream)) (i 0) (n (length html)))
    (flet ((skip-past (end) (let ((p (search end html :start2 i :test #'char-equal)))
                              (setf i (if p (+ p (length end)) n)))))
      (loop while (< i n)
            do (let ((c (char html i)))
                 (cond
                   ((and (char= c #\<) (string= "<!--" html :start2 i :end2 (min n (+ i 4))))
                    (skip-past "-->"))
                   ((char= c #\<)
                    (let* ((close (or (position #\> html :start i) n))
                           (tag (string-downcase
                                 (string-trim "/ " (subseq html (1+ i)
                                                           (or (position-if (lambda (ch) (member ch '(#\Space #\> #\Newline #\Tab)))
                                                                            html :start (1+ i))
                                                               close))))))
                      (setf i (min n (1+ close)))
                      (cond ((member tag '("script" "style" "noscript" "svg") :test #'string=)
                             (skip-past (format nil "</~a" tag)) (skip-past ">"))
                            ((member tag +html-block-tags+ :test #'string=) (write-char #\Newline out)))))
                   ((char= c #\&)
                    (let* ((semi (position #\; html :start i :end (min n (+ i 10))))
                           (ch (and semi (html-entity (subseq html (1+ i) semi)))))
                      (if ch (progn (write-char ch out) (setf i (1+ semi)))
                          (progn (write-char c out) (incf i)))))
                   (t (write-char c out) (incf i))))))
    ;; collapse: trim every line, keep at most one blank line in a row
    (let ((lines '()) (blank nil))
      (dolist (l (split-sequence-lines (get-output-stream-string out)))
        (let ((l (string-trim '(#\Space #\Tab #\Return) (substitute #\Space #\Tab l))))
          (loop for p = (search "  " l) while p do (setf l (concatenate 'string (subseq l 0 p) (subseq l (1+ p)))))
          (if (zerop (length l))
              (unless blank (setf blank t) (push "" lines))
              (progn (setf blank nil) (push l lines)))))
      (string-trim '(#\Newline) (format nil "~{~a~^~%~}" (nreverse lines))))))

(define-agent-tool "web-fetch" (:write t :off-main t :available (lambda () (fboundp 'http-request)))
  "Fetch a web page (http or https) and return its text: HTML is reduced to its words
   (scripts, styles and tags removed), plain text and JSON come back as they are.
   Asks the user first, like a write: a URL can carry data out of this machine. At
   most 20000 characters come back."
  (("url" "string" "The address, http:// or https://"))
  ("url")
  (input call)
  (let ((url (lol.json:jget input "url")))
    (unless (or (string-equal "http://" url :end2 (min 7 (length url)))
                (string-equal "https://" url :end2 (min 8 (length url))))
      (error "only http:// and https:// URLs can be fetched"))
    (multiple-value-bind (status headers body final)
        (funcall 'http-request url :headers '(("Accept" . "text/html, text/plain, application/json;q=0.9, */*;q=0.5")
                                              ("User-Agent" . "lisp-over-linux agent"))
                 :timeout 30)
      (let* ((type (string-downcase (or (funcall 'header-value headers "content-type") "")))
             (text (cond ((not (stringp body)) "(no text body)")
                         ((search "html" type) (html-text body))
                         ((or (search "text" type) (search "json" type) (search "xml" type) (string= type "")) body)
                         (t (format nil "(a ~a body of ~d characters; not text, not shown)" type (length body))))))
        (setf (call-summary call) (format nil "HTTP ~d · ~d chars" status (length text)))
        (format nil "HTTP ~d~:[~*~; (redirected to ~a)~]~%content-type: ~a~%~%~a"
                status (and final (string/= final url)) final type text)))))
