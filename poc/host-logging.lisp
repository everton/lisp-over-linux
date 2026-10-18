;;;; host-logging.lisp — the session log, for a host test that loads image modules.
;;;;
;;;;     (load (merge-pathnames "host-logging.lisp" <poc dir>))   ; before any initramfs module
;;;;
;;;; Since 2026-10-16 several modules log (initramfs/session-log.lisp): tls.lisp and
;;;; agent.lisp name their locks to the stall watcher, dhcp.lisp's NETWORK-LOG writes
;;;; through APPEND-LOG-LINE, the agent logs its turns. In the image session-log.lisp
;;;; loads early (build.sh); a host test that loads modules one by one must load it
;;;; too, or the first such call is an undefined function.
;;;;
;;;; It also points the logs at a scratch directory: a host test must never write
;;;; into the developer's real ~/.lol/log, which is where the LAPTOP's sessions are
;;;; read back after a freeze. (HOST-LOG-DIR says where they went.)
;;;;
;;;; All original code, under this project's MIT licence.

(let ((here (make-pathname :name nil :type nil :defaults *load-truename*)))
  (load (merge-pathnames "../initramfs/lol-home.lisp" here))   ; LOL-PATH: where the logs live
  (load (merge-pathnames "deflate.lisp" here))
  (load (merge-pathnames "../initramfs/session-log.lisp" here)))

(setf *log-dir* (format nil "/tmp/lol-host-log-~d/" (sb-unix:unix-getpid)))

(defun host-log-dir () *log-dir*)
