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
  (check "the tray shows the agent"
         (string= (row "tray item" (probe "(and (search \"agent ask\" (getf (agent-tray-item) :text)) t)")) "T"))
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
  (check "the call shows as #N in the pane"
         (string= (row "a ;; #0 lisp-eval line" (probe (pane-has ";; #0 lisp-eval"))) "T"))

  (format t "~&~%== 3. C-g stops a slow answer ==~%")
  (qmp-type "slow answer please") (sleep 0.3)
  (qmp-chord "ctrl" "c") (qmp-chord "ctrl" "a")
  (wait-until (pane-has "word3") :seconds 20)
  (qmp-chord "ctrl" "g")
  (check "stopped: the conversation is idle again"
         (string= (row "state" (wait-until "(conversation-state *conversation*)" :want ":IDLE" :seconds 15)) ":IDLE"))
  (check "the transcript says it was stopped"
         (string= (row "a stopped note" (wait-until (pane-has "stopped (C-g)") :seconds 10)) "T"))
  (shot "agent-stopped")

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
