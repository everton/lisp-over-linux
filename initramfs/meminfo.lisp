;;;; meminfo.lisp — the "m) memory" menu action: how much RAM we use.
;;;;
;;;; On this box SBCL *is* userspace — PID 1 is the frozen Lisp image — so
;;;; "memory" has two honest layers, and we show both:
;;;;   1. the Lisp heap inside the image (dynamic-space live bytes, total consed)
;;;;   2. the OS process (RSS the kernel actually made resident)
;;;;   3. the whole machine (total / available RAM)
;;;;
;;;; Deliberately COMPACT. tty0 is 80x25 with NO fbcon scrollback (mainline
;;;; dropped it; see doc/framebuffer.org), so once output scrolls off it is
;;;; gone. This prints a fixed handful of lines that always fit, and never
;;;; calls ROOM (which dumps ~20+ lines) — reach for (room) over the serial or
;;;; network REPL when you want the per-type breakdown.

(defun proc-field-kib (path prefix)
  "Scan PATH for the first line beginning with PREFIX (a \"Name:\" tag from a
   /proc file whose values are in KiB) and return that value as an integer
   number of KiB, or NIL if the file or field is absent."
  (ignore-errors
    (with-open-file (s path :if-does-not-exist nil)
      (when s
        (loop for line = (read-line s nil nil) while line
              when (and (>= (length line) (length prefix))
                        (string= prefix line :end2 (length prefix)))
                do (return (parse-integer line :start (length prefix)
                                               :junk-allowed t)))))))

(defun mib (bytes)
  "BYTES as a one-decimal MiB string (e.g. \"44.2\"); NIL prints as \"?\"."
  (if bytes (format nil "~,1F" (/ bytes 1048576.0d0)) "?"))

(defun report-memory ()
  "The 'm) memory' action: a fixed, compact readout sized for 80x25.
   Lisp heap — live dynamic-space bytes + total ever consed since boot.
   This proc — resident set size (RSS), from /proc/self/status.
   Machine   — used / total RAM (MemTotal - MemAvailable), from /proc/meminfo."
  (let* ((live   (sb-kernel:dynamic-usage))
         (consed (sb-ext:get-bytes-consed))
         (rss    (proc-field-kib "/proc/self/status" "VmRSS:"))
         (total  (proc-field-kib "/proc/meminfo"     "MemTotal:"))
         (avail  (proc-field-kib "/proc/meminfo"     "MemAvailable:")))
    (format t "~&~%---- memory ----~%")
    (format t "  lisp heap : ~a MiB live  (~a MiB consed since boot)~%"
            (mib live) (mib consed))
    (format t "  this proc : ~a MiB resident (RSS)~%"
            (mib (and rss (* rss 1024))))
    (if (and total avail)
        (format t "  machine   : ~a / ~a MiB used  (~d%)~%"
                (mib (* (- total avail) 1024)) (mib (* total 1024))
                (round (* 100 (- total avail)) total))
        (format t "  machine   : (/proc/meminfo unavailable)~%"))
    (format t "----------------~%")
    (finish-output)))
