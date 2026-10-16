;;;; check-agent.lisp — boot the image and talk to the agent, end to end, against a
;;;; mock Messages API on the host.
;;;;
;;;;     LOL_PORT=4014 sbcl --script check-agent.lisp     ; exits 0 if all pass
;;;;     LOL_SHOTS=DIR …                                   ; also keep screendumps
;;;;
;;;; The mock (host-client/mock-anthropic.lisp) runs inside this process on MOCK_PORT
;;;; (default 18014); the guest reaches it as 10.0.2.2, QEMU's SLIRP address for the
;;;; host — so this needs neither TLS nor a key, and spends nothing. What it drives is
;;;; the real path: keys through QMP into the real evdev, the environment's own loop,
;;;; the turn's thread, plain HTTP out of the guest, the stream back into the pane.
;;;;
;;;;   1. SuperL-a opens a conversation; a question typed at its end and C-c C-a is
;;;;      answered, streamed into the pane (the buffer is read back over the net REPL).
;;;;   2. A question that makes the model call lisp-eval — a WRITE — pops the approval
;;;;      chooser; key 1 (Allow) runs it, and the result goes back to the model.
;;;;   3. A slow answer is stopped with C-g.
;;;;   3b. Every failure is said IN THE PANE, as a red ;; !! block with the cause and
;;;;      what to do (doc/agent.org §14.8): a 401, a 529 after its retries, a stream
;;;;      cut mid-answer (its partial text kept), no route to the endpoint, no lease
;;;;      and no resolv.conf, https on a diskless boot, an error inside the transport;
;;;;      C-c C-a on an empty draft asks the failed question again, not twice.
;;;;   4. The REPL doorway: "?question" on the net REPL streams the answer as text.
;;;;   5. The debugger doorway: Debug it on the primer's (lol.examples:launch), pick
;;;;      the ASK restart, and a read-only conversation seeded with the condition, the
;;;;      backtrace and the failing definition's source is answered.
;;;;   6. The mock refused nothing (signatures echoed, every tool_use answered) and
;;;;      never saw an API key.
;;;;
;;;; Every probe is ASCII: the net REPL's host end is Latin-1, so a literal é sent to the
;;;; guest's UTF-8 reader ended the session (broken pipe) — the check spells it
;;;; (code-char 233) instead.
;;;;
;;;; All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(use-package :lol.guest)

(defvar *mock-library-only* t)
(load (merge-pathnames "host-client/mock-anthropic.lisp" *root*))

(defparameter *mock-port* (or (ignore-errors (parse-integer (sb-posix:getenv "MOCK_PORT"))) 18014))
(defvar *shots* (sb-ext:posix-getenv "LOL_SHOTS"))

(defun shot (name)
  (when *shots*
    (qmp (format nil "{\"execute\":\"screendump\",\"arguments\":{\"filename\":\"~a/~a.ppm\"}}"
                 *shots* name))))

(defun qmp-chord (modifier qcode)
  "MODIFIER+QCODE in one send-key — a chord the TTY spells as one byte (C-c is 3)."
  (qmp (format nil "{\"execute\":\"send-key\",\"arguments\":{\"keys\":[~
                    {\"type\":\"qcode\",\"data\":\"~a\"},{\"type\":\"qcode\",\"data\":\"~a\"}]}}"
               modifier qcode))
  (sleep 0.6))

(defun row (label value)
  (format t "~&  ~44a ~a~%" label value) (finish-output) value)

(defun wait-until (form-string &key (seconds 30) (want "T"))
  "Probe FORM-STRING every half second until it prints WANT; the last answer."
  (let ((last nil))
    (loop repeat (* 2 seconds)
          do (setf last (probe form-string))
             (when (string= last want) (return))
             (sleep 0.5))
    last))

(defun pane-has (text)
  (format nil "(and (search ~s (lol.textview:tv-text (pane-tv (current-pane)))) t)" text))

(defun ready-to-type ()
  "Wait until the current conversation's turn has ended (its pane is read-only while
   one streams, so keys typed then are lost), focus the transcript, caret to the end."
  (wait-until "(not (conversation-busy-p *conversation*))" :seconds 30)
  (probe "(progn (setf *editing-side* nil) (caret-to-end (current-pane)) t)")
  (sleep 0.3))

(defun checks ()
  (quiet-kernel-console)
  (check "the environment came up" (wait-for-browser 60))
  (probe (format nil "(setf *agent-endpoint* \"http://10.0.2.2:~d\")" *mock-port*))

  (format t "~&~%== 1. a question, streamed into the conversation ==~%")
  (qmp-hold "meta_l" "a") (sleep 1.5)
  (check "SuperL-a opened a conversation pane"
         (string= (row "current pane shows a conversation" (probe "(and (pane-conversation (current-pane)) t)")) "T"))
  (qmp-type "hello guest") (sleep 0.3)
  (qmp-chord "ctrl" "c") (qmp-chord "ctrl" "a")
  (check "the answer streamed into the pane"
         (string= (row "pane shows the mock's reply" (wait-until (pane-has "Hello from the mock"))) "T"))
  (check "UTF-8 survived the trip (caf\\u00e9)"
         (string= (row "pane shows cafe-acute" (probe "(and (search (format nil \"caf~c\" (code-char 233)) (lol.textview:tv-text (pane-tv (current-pane)))) t)")) "T"))
  (check "the proposed defun is live code, not a comment"
         (string= (row "a line is exactly the defun" (probe "(and (find \"(defun mock-answer () 42)\" (lol.textview:tv-lines (pane-tv (current-pane))) :test #'string=) t)")) "T"))
  (check "the turn ended with a your-turn line"
         (string= (row "last marker present" (probe (pane-has "--- your turn"))) "T"))
  (check "usage and cost were counted"
         (string= (row "session cost > 0" (probe "(and (plusp (session-cost)) t)")) "T"))
  ;; the word "agent" became the creature (DRAW-AGENT-ICON, §14.8¾): the text is "ask $0.01"
  (check "the tray shows the agent"
         (string= (row "tray item" (probe "(let ((it (agent-tray-item))) (and (eq (getf it :icon) :agent) (search \"ask $\" (getf it :text)) t))")) "T"))
  (shot "agent-answer")

  (format t "~&~%== 2. a gated write: approve it from the chooser ==~%")
  (qmp-type "please eval this") (sleep 0.3)
  (qmp-chord "ctrl" "c") (qmp-chord "ctrl" "a")
  (check "the lisp-eval call waits for approval"
         (string= (row "a call is :waiting" (wait-until "(and (find :waiting (conversation-calls *conversation*) :key #'call-status) t)" :seconds 20)) "T"))
  (sleep 1.5)
  (shot "agent-approval")
  (qmp-key "1")                                   ; Allow — run it
  (check "allowed: it ran and returned 42"
         (string= (row "the call's result" (wait-until "(let ((c (find \"lisp-eval\" (conversation-calls *conversation*) :key #'call-name :test #'string=))) (and c (eq (call-status c) :done) (search \"values: 42\" (call-result c)) t))")) "T"))
  (check "the model saw the result"
         (string= (row "pane quotes the tool's answer" (wait-until (pane-has "The tool answered: values: 42"))) "T"))
  ;; folded into one steps line since the Activity panel (*agent-steps-detail*
  ;; :concise, doc/agent.org §14.8¾); this asserted a ;; #0 line until then
  (check "the call shows in the pane's steps line"
         (string= (row "a steps line: evaluated 1" (probe (pane-has "evaluated 1"))) "T"))

  (format t "~&~%== 2b. the tools of agent-tools.lisp ==~%")
  (flet ((ask-for (question)
           ;; the pane is read-only while a turn streams, so typing into it then is
           ;; lost: wait for the last turn to end, caret to the draft, then type
           (ready-to-type)
           (qmp-type question) (sleep 0.3)
           (qmp-chord "ctrl" "c") (qmp-chord "ctrl" "a"))
         (last-call-of (name &optional (test "(eq (call-status c) :done)"))
           (format nil "(let ((c (find ~s (conversation-calls *conversation*) :key #'call-name :test #'string= :from-end t))) (and c ~a t))"
                   name test)))
    (ask-for "inspectme please")
    (check "lisp-inspect ran in the guest and listed the fields"
           (string= (row "lisp-inspect result" (wait-until (last-call-of "lisp-inspect" "(eq (call-status c) :done) (search \"car\" (call-result c))"))) "T"))
    (wait-until "(not (conversation-busy-p *conversation*))" :seconds 20)
    (ask-for "applicable please")
    (check "lisp-applicable showed the effective method"
           (string= (row "lisp-applicable result" (wait-until (last-call-of "lisp-applicable" "(eq (call-status c) :done) (search \"primary\" (call-result c))"))) "T"))
    (wait-until "(not (conversation-busy-p *conversation*))" :seconds 20)
    (ask-for "browse please")
    (check "browse put the senders view beside the conversation"
           (string= (row "browse said where" (wait-until (last-call-of "browse" "(search \"beside the conversation\" (call-result c))"))) "T"))
    (check "...as a 'shown to you' row of the Activity panel"
           (string= (row "activity row"
                         (wait-until "(let ((side (pane-side (current-pane)))) (and side (find-if (lambda (it) (search \"shown to you\" (or (item-group it) \"\"))) (view-items (pane-view side))) t))" :seconds 10))
                    "T"))
    (check "...and the focus did not move"
           (string= (row "current pane still the conversation" (probe "(and (pane-conversation (current-pane)) t)")) "T"))
    (shot "agent-browse")
    (wait-until "(not (conversation-busy-p *conversation*))" :seconds 20)
    (ask-for "fetchpage please")
    (check "web-fetch is gated: it waits for approval"
           (string= (row "web-fetch :waiting" (wait-until (last-call-of "web-fetch" "(eq (call-status c) :waiting)") :seconds 20)) "T"))
    (sleep 1.5)
    (qmp-key "1")                                   ; Allow
    (check "allowed: the page came back as words"
           (string= (row "web-fetch result" (wait-until (last-call-of "web-fetch" "(eq (call-status c) :done) (search \"Fish & chips\" (call-result c)) (not (search \"hidden\" (call-result c)))"))) "T"))
    (row "idle again" (wait-until "(not (conversation-busy-p *conversation*))" :seconds 20)))

  (format t "~&~%== 3. C-g stops a slow answer ==~%")
  (ready-to-type)
  (qmp-type "slow answer please") (sleep 0.3)
  (qmp-chord "ctrl" "c") (qmp-chord "ctrl" "a")
  (row "the slow answer is streaming" (wait-until (pane-has "word3") :seconds 20))
  (qmp-chord "ctrl" "g")
  (check "stopped: the conversation is idle again"
         (string= (row "state" (wait-until "(conversation-state *conversation*)" :want ":IDLE" :seconds 15)) ":IDLE"))
  (check "the transcript says it was stopped"
         (string= (row "a stopped note" (wait-until (pane-has "stopped (C-g)") :seconds 10)) "T"))
  (shot "agent-stopped")

  (format t "~&~%== 3b. every failure is said in the pane, in words ==~%")
  ;; fewer retries, so a failure that retries (529, a cut stream) ends in seconds
  (probe "(setf *agent-retries* 2)")
  (flet ((ask-and-fail (question want &key (seconds 40))
           "Type QUESTION, C-c C-a, and wait for an error block containing WANT."
           (qmp-type question) (sleep 0.3)
           (qmp-chord "ctrl" "c") (qmp-chord "ctrl" "a")
           (string= (row (format nil "pane: ~a" (subseq want 0 (min 40 (length want))))
                         (wait-until (format nil "(and (not (conversation-busy-p *conversation*)) ~a)"
                                             (pane-has want))
                                     :seconds seconds))
                    "T")))
    (check "401: a red FAILED block naming the refused key"
           (ask-and-fail "badkey please" "FAILED: The API refused the key (401"))
    (check "401: the API's own words are shown"
           (string= (row "invalid x-api-key in the pane" (probe (pane-has "invalid x-api-key"))) "T"))
    (check "the block is drawn in the condition colour (the pane's tint)"
           (string= (row "tint of a ;; !! line"
                         (probe "(let* ((tv (pane-tv (current-pane))) (l (find-if #'error-line-p (lol.textview:tv-lines tv)))) (and l (eql (funcall (lol.textview:tv-tint tv) l) (concept-color :condition)) t))"))
                    "T"))
    (check "the status line says it too"
           (string= (row "status" (probe "(and *browser-status* (search \"agent: The API refused the key\" *browser-status*) t)")) "T"))
    (shot "agent-failed-401")
    ;; C-c C-a with nothing typed: the same question again, not said twice
    (qmp-chord "ctrl" "c") (qmp-chord "ctrl" "a")
    (check "C-c C-a on an empty draft asks the failed question again"
           (string= (row "an (asked again) turn that failed the same way"
                         (wait-until "(let ((tu (current-turn *conversation*))) (and (turn-again tu) (turn-done tu) (turn-failure tu) (equal (turn-question tu) \"badkey please\") t))" :seconds 20))
                    "T"))
    (check "...and the question was not appended twice"
           (string= (row "last user message has one text block"
                         (probe "(let* ((ms (conversation-messages *conversation*)) (m (aref ms (1- (length ms))))) (and (equal (message-role m) \"user\") (= 1 (count \"badkey please\" (message-blocks m) :key (lambda (b) (lol.json:jget (block-json b) \"text\")) :test #'equal)) t))"))
                    "T"))
    (check "529 every time: 'overloaded', after the retries"
           (ask-and-fail "busy please" "FAILED: The API is overloaded (529)"))
    (check "a stream cut mid-answer: 'cut off'"
           (ask-and-fail "cutoff please" "FAILED: The answer was cut off"))
    (check "...with the text that had arrived kept above the block"
           (string= (row "partial text in the pane" (probe (pane-has "This answer will be cut off"))) "T"))
    (shot "agent-failed-cutoff")
    ;; no network: take the default route away (the net REPL is on-link and survives)
    (probe "(let ((dr (ipv4-default-route))) (and dr (delete-default-route (getf dr :gateway))))")
    (check "the default route is gone" (string= (row "default route" (probe "(ipv4-default-route)")) "NIL"))
    (probe "(setf *agent-endpoint* \"http://192.0.2.1:18014\")")       ; TEST-NET-1: off-link
    (check "an endpoint with no route: 'No network', at once"
           (ask-and-fail "hello with no route" "FAILED: No network: there is no route" :seconds 10))
    ;; the laptop's case: DHCP got no lease at all — no lease, no resolv.conf. (SLIRP
    ;; also advertises IPv6, so a v6 default route may remain: the check is then
    ;; "no IPv4 connection" rather than "not connected"; both start "No network".)
    (probe "(progn (defparameter cl-user::*saved-lease* (current-lease)) (setf *leases* nil) (write-file-in-place \"/etc/resolv.conf\" \"\") (dns-flush) t)")
    (probe "(setf *agent-endpoint* \"http://api.example.invalid:18014\")")
    (check "a named endpoint, no lease: 'No network', at once"
           (ask-and-fail "hello with no lease" "FAILED: No network: this machine" :seconds 10))
    (check "...saying DHCP got no lease"
           (string= (row "the lease is named" (probe (pane-has "DHCP got no lease"))) "T"))
    (check "...and what to do (the network view)"
           (string= (row "advice in the pane" (probe (pane-has "SuperL-n opens the network view"))) "T"))
    (shot "agent-failed-no-network")
    (probe "(progn (push cl-user::*saved-lease* *leases*) (write-resolv-conf cl-user::*saved-lease*) (add-default-route \"10.0.2.2\"))")
    (check "the default route is back" (string= (row "default route back" (probe "(and (ipv4-default-route) t)")) "T"))
    ;; HTTPS on a diskless boot: no /mnt/host, so no OpenSSL to load
    (probe (format nil "(setf *agent-endpoint* \"https://10.0.2.2:~d\")" *mock-port*))
    (check "https with no host system: 'HTTPS is not available'"
           (ask-and-fail "hello over tls" "FAILED: HTTPS is not available" :seconds 10))
    ;; an error nobody planned for, on the turn's own thread
    (probe (format nil "(setf *agent-endpoint* \"http://10.0.2.2:~d\")" *mock-port*))
    (probe "(setf *agent-transport* (lambda (&rest args) (declare (ignore args)) (error \"boom in the transport\")))")
    (check "an unexpected error on the turn's thread: said, not lost"
           (ask-and-fail "hello into a bug" "FAILED: Something went wrong inside the agent"))
    (check "...naming it" (string= (row "the error's text" (probe (pane-has "boom in the transport"))) "T"))
    (probe "(setf *agent-transport* nil)")
    (check "after all that, a question is answered again"
           (progn (qmp-type "hello once more") (sleep 0.3)
                  (qmp-chord "ctrl" "c") (qmp-chord "ctrl" "a")
                  (string= (row "answered" (wait-until "(let ((tu (current-turn *conversation*))) (and (turn-done tu) (null (turn-failure tu)) t))" :seconds 30)) "T"))))
  (probe "(setf *agent-retries* 5)")

  (format t "~&~%== 4. the REPL doorway ==~%")
  (write-line "?hello from the repl" lol.guest::*repl*) (force-output lol.guest::*repl*)
  (let ((out (strip-ansi (repl-drain 30 (lambda (s) (search "session" (strip-ansi s)))))))
    (check "?prose on the REPL streams the answer"
           (row "REPL printed the mock's reply" (and (search "Hello from the mock" out) t))))

  (format t "~&~%== 5. the debugger doorway: ASK is a restart ==~%")
  (qmp-hold "meta_l" "1") (sleep 1.0)                ; the Workspace tab
  ;; caret to the end of the primer's (lol.examples:launch) line, then Debug it
  (probe "(let* ((tv (pane-tv (current-pane))) (i (position \"(lol.examples:launch)\" (lol.textview:tv-lines tv) :test #'string=))) (lol.textview:tv-move-point tv i 21) i)")
  (qmp-chord "ctrl" "c") (qmp-chord "ctrl" "d") (sleep 2.0)
  (shot "agent-debugger-rail")
  (dotimes (i 12) (qmp-key "down") (sleep 0.1))     ; ASK is the last row
  (qmp-key "ret")
  (check "ASK started a conversation about the condition"
         (string= (row "a debugger: conversation answered"
                       (wait-until "(let ((c (find-if (lambda (c) (search \"debugger:\" (conversation-title c))) *conversations*))) (and c (eq (conversation-state c) :idle) (plusp (length (conversation-turns c))) (turn-done (aref (conversation-turns c) 0)) t))" :seconds 40))
                  "T"))
  (check "lisp-backtrace has the debugger's stack, copied when ASK was chosen"
         (string= (row "a snapshot with frames"
                       (probe "(let* ((c (find-if (lambda (c) (search \"debugger:\" (conversation-title c))) *conversations*)) (ctx (gethash c *conversation-debug-contexts*))) (and ctx (plusp (length (getf ctx :frames))) t))"))
                  "T"))
  (check "it was seeded with the failing definition's source"
         (string= (row "the prompt carried source"
                       (probe "(let ((c (find-if (lambda (c) (search \"debugger:\" (conversation-title c))) *conversations*))) (and (search \"Source of\" (turn-prompt (aref (conversation-turns c) 0))) t))"))
                  "T"))
  (sleep 1.5)
  (shot "agent-debugger-ask")
  (qmp-key "esc") (sleep 1.5)                        ; back to the rail, stack standing
  (qmp-chord "ctrl" "g") (sleep 1.5)                 ; abort: back to the Workspace
  (check "back in the environment after the debugger"
         (string= (row "status after abort" (probe "*browser-status*")) "\"aborted\""))

  (format t "~&~%== 5b. a conversation survives power-off (LOL_GUEST_DISK only) ==~%")
  (if (not (sb-posix:getenv "LOL_GUEST_DISK"))
      (format t "~&  (skipped: no disk, so no /home to save into — LOL_GUEST_DISK=test-disk.img runs it)~%")
      (progn
        ;; the conversation section 1 began (titled by its first question)
        (probe "(defun first-conv () (find \"hello guest\" *conversations* :key #'conversation-title :test #'string=))")
        (check "the store is under the user's home"
               (string= (row "conversation-store-dir" (probe "(and (conversation-store-dir) (search \"/home/\" (conversation-store-dir)) t)")) "T"))
        (check "the first conversation was saved at the end of its turns"
               (string= (row "its file is listed"
                             (probe "(let ((f (getf (gethash (first-conv) *conversation-stores*) :file))) (and f (find f (saved-conversations) :key (lambda (e) (getf e :file)) :test #'equal) t))"))
                        "T"))
        (check "...as the user's file, mode 0600"
               (string= (row "mode" (probe "(let ((p (path-join (conversation-store-dir) (getf (gethash (first-conv) *conversation-stores*) :file)))) (multiple-value-bind (ok dev ino mode) (sb-unix:unix-stat p) (declare (ignore dev ino)) (and ok (logand mode #o777))))"))
                        "384"))                     ; #o600
        ;; "power-off": this session forgets the conversation, only the file is left
        (probe "(progn (defparameter cl-user::*forgotten* (first-conv)) (setf *conversations* (remove cl-user::*forgotten* *conversations*)) (remhash cl-user::*forgotten* *conversation-stores*) t)")
        (qmp-hold "meta_l" "spc") (sleep 2.0)
        (qmp-type "saved conversation hello guest") (sleep 1.5)
        (shot "agent-spotter-saved")
        (qmp-key "ret") (sleep 3.0)
        (check "the Spotter reopened it, in a pane of its own"
               (string= (row "a new conversation object in the pane"
                             (probe "(let ((c (pane-conversation (current-pane)))) (and c (not (eq c cl-user::*forgotten*)) (member c *conversations*) t))"))
                        "T"))
        (check "...showing its earlier turns"
               (string= (row "pane shows the first answer" (probe (pane-has "Hello from the mock"))) "T"))
        (probe "(progn (caret-to-end (current-pane)) t)")
        (qmp-type "hello again after the reboot") (sleep 0.3)
        (qmp-chord "ctrl" "c") (qmp-chord "ctrl" "a")
        (check "...and it goes on: the next question is answered"
               (string= (row "answered after reopening"
                             (wait-until (pane-has "You asked: hello again after the reboot") :seconds 30))
                        "T"))
        (shot "agent-reopened")))

  (format t "~&~%== 6. what the guest sent ==~%")
  (check "the guest spoke through the image's own HTTP-REQUEST"
         (string= (row "transport" (probe "(transport)")) "HTTP-REQUEST"))
  (check "the mock refused nothing" (null (row "mock problems" lol.mock:*problems*)))
  (check "no API key was ever sent to the mock"
         (row "requests without x-api-key"
              (notany (lambda (r) (assoc "x-api-key" (getf r :headers) :test #'string-equal))
                      lol.mock:*requests*)))
  (check "the guest is still alive" (eql (probe-int "(+ 1 1)") 2)))

(lol.mock:start-mock *mock-port*)
(unwind-protect (run-guest-checks "check-agent" #'checks)
  (lol.mock:stop-mock))
