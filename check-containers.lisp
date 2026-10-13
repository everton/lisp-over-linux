;;;; check-containers.lisp — assert CONTAINERS in the booted guest (doc/containers.org).
;;;;
;;;;     LOL_GUEST_HOSTROOT=1 sbcl --script check-containers.lisp   ; exits 0 if all pass
;;;;     LOL_GUEST_DISK=test-disk.img sbcl --script check-containers.lisp
;;;;
;;;; TWO MODES, because no single QEMU setup is the laptop:
;;;;
;;;;   LOL_GUEST_HOSTROOT=1 — the build host's REAL Debian, shared read-only over
;;;;     virtio-9p (guest-harness.lisp, preinit.c MOUNT-STORAGE-9P). The container's
;;;;     root is that tree, so ruby, rails, postgres, git and curl are the host's
;;;;     own. But /home is then an OVERLAY (host /home + tmpfs), and an overlay cannot
;;;;     be another overlay's upper layer — so here the uppers fall back to a tmpfs.
;;;;   LOL_GUEST_DISK=test-disk.img — ext4, as on the laptop, so the uppers CAN live in
;;;;     ~/.lol/ct/<name>. The disk has an /etc but no Debian, so nothing can be
;;;;     executed inside: this mode asserts the LAYOUT lol-ct built, and re-asks the
;;;;     §3.3 whole-root question on ext4.
;;;;
;;;; Host-root checks, cheapest first:
;;;;   - lol-ct is in the image and a host root is there;
;;;;   - the §3.3 question, re-asked in the guest (recorded, not asserted);
;;;;   - inside you are yourself (uid 1000), with NO capabilities, as PID 2 of your
;;;;     own pid namespace, on hostname lol-default;
;;;;   - a write to /etc works, and the host's apt dirs stay unwritable (§3.5);
;;;;   - ruby loads openssl; rails answers -v (when the host has them);
;;;;   - a private PostgreSQL: initdb + pg_ctl start + psql + stop;
;;;;   - git status in a repository under /home;
;;;;   - outbound HTTPS from inside (curl);
;;;;   - stopping a run: SIGTERM reaches the container's PID 2, nothing is left;
;;;;   - THE PANE: x in the containers view, typed over QMP, opens a run pane that
;;;;     fills while the command runs (the poll loop's tick), keeps its ANSI colour,
;;;;     and shows the exit status.
;;;;
;;;; All original code, under this project's MIT licence.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(use-package :lol.guest)

(defun row (label value)
  (format t "~&  ~44a ~a~%" label value)
  (finish-output)
  value)

(defun ct (command &optional (seconds 60) cwd)
  "Run COMMAND in the default container; return its exit code, a #, and its output
   lines joined by | (the last line is lol's own '-- exit N after T s --')."
  (probe (format nil "(multiple-value-bind (lines code) (container-run-output ~s ~@[:cwd ~s ~]:timeout ~d) (format nil \"~~a#~~{~~a~~^|~~}\" code lines))"
                 command cwd seconds)
         (+ seconds 10)))

(defun code-of (result)
  "The exit code CT put before the #. (PROBE hands back the PRINTED string, so it
   starts with a double quote — which once made every exit code parse as NIL.)"
  (let ((s (string-left-trim "\"" (or result ""))))
    (ignore-errors (parse-integer s :end (position #\# s)))))

(defun has (result needle) (and result (search needle result) t))

;;; Every command below prints its answers between = signs (=uid=1000=) and the
;;; checks look for exactly that. Not decoration: the guest runs `bash -lc`, so the
;;; user's login dotfiles run first and may print anything — this host's start an
;;; ssh-agent and print an ssh-add passphrase prompt WITHOUT a newline, which glues
;;; itself to the front of the command's first line of output.

(defun probe-root-question ()
  (format t "~&~%== §3.3 again: the whole root as a lower layer ==~%")
  (row "lol-ct --probe-root"
       (probe "(substitute #\\| #\\Newline (with-output-to-string (o) (sb-ext:run-program \"/sbin/lol-ct\" '(\"--probe-root\") :output o :error o)))" 20)))

(defun host-root-checks ()
  (format t "~&~%== the helper and the host root ==~%")
  (check "lol-ct is in the image and a host root is mounted"
         (string= "T" (row "containers-available-p" (probe "(containers-available-p)"))))
  (row "/mnt/host is" (probe "(with-open-file (s \"/proc/mounts\") (loop for l = (read-line s nil) while l when (search \" /mnt/host \" l) return (subseq l 0 (position #\\, l))))"))
  (probe-root-question)

  (format t "~&~%== identity and isolation ==~%")
  (let ((r (row "uid, hostname, pid, CapEff inside"
                (ct "echo =uid=$(id -u)= =host=$(hostname)= =pid=$$= =cap=$(awk '/CapEff/ {print $2}' /proc/self/status)="))))
    (check "inside you are uid 1000" (has r "=uid=1000="))
    (check "hostname is lol-default (own uts namespace)" (has r "=host=lol-default="))
    (check "the command is PID 2 (own pid namespace)" (has r "=pid=2="))
    (check "no capabilities inside (CapEff 0)" (has r "=cap=0000000000000000=")))
  (row "layout of that run"
       (probe "(format nil \"~{~a~^|~}\" (container-layout \"default\"))"))
  (let ((r (row "write /etc, then /etc/apt"
                (ct "touch /etc/lol-probe && echo =etc=wrote=; touch /etc/apt/lol-probe 2>/dev/null || echo =apt=refused="))))
    (check "a write to /etc works inside" (has r "=etc=wrote="))
    (check "/etc/apt (root-owned on the host) is refused" (has r "=apt=refused=")))

  ;; ping needs no capability once preinit opens net.ipv4.ping_group_range; before
  ;; 2026-10-12 it failed in every container. SLIRP answers its gateway itself.
  (let ((r (row "ping the gateway" (ct "ping -c 1 -W 3 10.0.2.2 >/dev/null 2>&1 && echo =ping=ok= || echo =ping=failed=" 30))))
    (check "ping works without capabilities (ping_group_range)" (has r "=ping=ok=")))

  (format t "~&~%== the host's toolchain ==~%")
  (let ((r (row "ruby openssl" (ct "ruby -e 'require \"openssl\"; puts \"=ssl=#{OpenSSL::VERSION}=\"'" 60))))
    (check "ruby loads openssl" (and (eql 0 (code-of r)) (has r "=ssl="))))
  (let ((r (row "rails -v" (ct "if command -v rails >/dev/null; then echo =rails=$(rails -v)=; else echo =rails=absent=; fi" 120))))
    (check "rails answers -v (or is absent on the host)" (or (has r "=rails=Rails ") (has r "=rails=absent="))))

  (format t "~&~%== a private PostgreSQL ==~%")
  (let ((r (row "initdb + pg_ctl + psql"
                (ct (concatenate 'string
                                 "set -e; B=$(ls -d /usr/lib/postgresql/*/bin | sort -V | tail -1); "
                                 "D=$HOME/.lol/ct/default/pg; rm -rf \"$D\"; "
                                 "$B/initdb -D \"$D\" -A trust -U \"$USER\" >/dev/null; "
                                 "$B/pg_ctl -D \"$D\" -o \"-k /tmp -c listen_addresses=''\" -l \"$D.log\" -w start >/dev/null; "
                                 "echo =pg=$(psql -h /tmp -d postgres -tAc 'select 1 + 41')=; "
                                 "$B/pg_ctl -D \"$D\" -w stop >/dev/null; echo =pg=stopped=")
                    180))))
    (check "postgres starts, answers select, stops" (and (has r "=pg=42=") (has r "=pg=stopped=")))
    (unless (has r "=pg=42=")
      (row "postgres log" (ct "tail -5 $HOME/.lol/ct/default/pg.log" 20))))

  (format t "~&~%== git, network ==~%")
  (let* ((repo (string-right-trim "/" (namestring *root*)))
         (r (row "git status in the project"
                 (ct "echo =git=$(git status --short --branch | head -1)=" 30 repo))))
    (check "git status works in a repo under /home" (has r "=git=## ")))
  (row "/etc/resolv.conf inside" (ct "echo =dns=$(grep -m1 nameserver /etc/resolv.conf)=" 20))
  (let ((r (row "curl https://example.com"
                (ct "echo =curl=$(curl -sS -o /dev/null -w '%{http_code}' --max-time 20 https://example.com 2>&1)=" 40))))
    (check "outbound HTTPS from inside the container" (has r "=curl=200=")))

  (format t "~&~%== stopping a run ==~%")
  (probe "(progn (defparameter cl-user::*ct-sleeper* (start-run (container-command-line '(\"sleep\" \"300\") :no-input t) :title \"sleep\" :container \"default\" :environment (container-environment))) t)")
  (sleep 3)
  (check "a container is busy while sleep runs"
         (string= "T" (row "container-busy-p while running" (probe "(container-busy-p \"default\")"))))
  (probe "(stop-run cl-user::*ct-sleeper*)")
  (sleep 2)
  (let ((state (row "after SIGTERM" (probe "(run-state-string cl-user::*ct-sleeper*)"))))
    (check "SIGTERM stops a command that is the container's PID 2"
           (or (search "signal 15" state) (search "exit 143" state))))
  (check "nothing of the container is left running"
         (string= "NIL" (row "container-busy-p after" (probe "(container-busy-p \"default\")"))))

  (format t "~&~%== the run pane, driven like a person ==~%")
  (wait-for-browser)
  (probe "(progn (new-trail (subj-containers)) t)")
  (sleep 1)
  (qmp-key "x") (sleep 1)
  ;; the minibuffer is pre-filled with the last command; replace its text with one
  ;; that prints over three seconds, in colour (typing it over QMP would need a
  ;; qcode for every punctuation mark in it)
  (probe "(progn (setf (mb-text *minibuffer*) \"for i in 1 2 3; do printf '\\\\033[31mred %s\\\\033[0m\\\\n' $i; sleep 1; done\" (mb-point *minibuffer*) 0) t)")
  (qmp-key "ret")
  (sleep 1.5)
  (let ((early (row "lines in the pane after 1.5 s"
                    (probe "(length (lol.textview:tv-lines (pane-tv (current-pane))))"))))
    (sleep 4)
    (let ((late (row "lines in the pane after 5.5 s"
                     (probe "(length (lol.textview:tv-lines (pane-tv (current-pane))))")))
          (title (row "pane title" (probe "(view-title (pane-view (current-pane)))")))
          ;; the run (start . end) covering the text "red 1", and its colour
          (red (row "colour of 'red 1'"
                    (probe "(let* ((line (find-if (lambda (l) (search \"red 1\" (car l))) (run-snapshot (pane-subject (current-pane))))) (at (search \"red 1\" (car line)))) (format nil \"~x\" (cddr (find-if (lambda (r) (<= (first r) at (1- (second r)))) (cdr line)))))"))))
      (check "the pane is a run pane" (has title "run "))
      (check "output arrived while it ran (live, not at exit)"
             (let ((e (ignore-errors (parse-integer early))) (l (ignore-errors (parse-integer late))))
               (and e l (< e l))))
      (check "the title shows the exit status" (has title "exit 0"))
      (check "SGR red came through as the palette's red" (has red "E06C75")))))

(defun ext4-checks ()
  "The laptop's case: /home on ext4, so the uppers can live in the state directory.
   test-disk.img has /etc but no Debian, so nothing runs INSIDE — lol-ct builds the
   layout, fails to exec (127), and the layout file says what each piece got."
  (format t "~&~%== on ext4 (test-disk.img): the layout, without a Debian ==~%")
  (check "lol-ct is in the image"
         (string= "T" (row "probe-file /sbin/lol-ct" (probe "(and (probe-file \"/sbin/lol-ct\") t)"))))
  (probe-root-question)
  (let ((code (row "lol-ct -- /no/such/program (exit code)"
                   (probe "(sb-ext:process-exit-code (sb-ext:run-program \"/sbin/lol-ct\" '(\"--\" \"/no/such/program\") :output nil :error nil :environment (container-environment)))" 30))))
    (check "the container is built; only the exec fails (127)" (equal code "127")))
  (let ((layout (row "layout"
                     (probe "(format nil \"~{~a~^|~}\" (container-layout \"default\"))"))))
    (check "/etc is an overlay with its upper in ~/.lol/ct/default"
           (has layout "/etc: overlay, upper /home/"))
    (check "the root was entered with pivot_root" (has layout "root: pivot_root"))))

(run-guest-checks "containers"
                  (lambda ()
                    (if (equal (sb-posix:getenv "LOL_GUEST_HOSTROOT") "1")
                        (host-root-checks)
                        (ext4-checks))))
