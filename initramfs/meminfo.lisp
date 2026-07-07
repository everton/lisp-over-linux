;;;; meminfo.lisp — the "m) memory" menu action: how much RAM we use.
;;;;
;;;; On this box SBCL *is* userspace — PID 1 is the frozen Lisp image — so
;;;; "memory" has several honest layers, from the Lisp heap out to the whole
;;;; physical machine, and we show them all:
;;;;   1. the Lisp heap inside the image (dynamic-space live bytes, total consed)
;;;;   2. the OS process (RSS the kernel actually made resident)
;;;;   3. the kernel's own resident image (code+rodata+data+bss, /proc/iomem)
;;;;   4. the whole machine, measured against REAL RAM (see below)
;;;;
;;;; The machine line is deliberately NOT the usual MemTotal-relative view.
;;;; MemTotal is what the kernel's page allocator manages — it already has the
;;;; kernel image and early/firmware reservations carved out of it, so a
;;;; MemTotal-relative "used" silently hides everything the kernel spent on
;;;; itself. Instead we take physical RAM as the sum of /proc/iomem's
;;;; "System RAM" ranges (the real RAM the kernel sees, kernel image included)
;;;; and report consumed = physical - MemFree. That is the honest whole-"distro"
;;;; footprint: kernel image + reserved + kernel dynamic + userland, vs real RAM.
;;;; (Reading real addresses out of /proc/iomem needs root — PID 1 has it.)
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

(defun scan-iomem ()
  "Scan /proc/iomem once and return two values, both in BYTES:
     1. physical RAM the kernel sees — sum of the top-level 'System RAM' ranges
        (this already INCLUDES the kernel image, which is nested inside them);
     2. the resident kernel image — sum of the nested 'Kernel code/rodata/data/
        bss' ranges.
   Both NIL if /proc/iomem is unreadable. Note the values are only real when we
   run as root (else the kernel zeroes the addresses); PID 1 qualifies.
   Each line is 'START-END : LABEL' with START/END hex and inclusive, so a
   range spans (1+ (- END START)) bytes."
  (ignore-errors
    (with-open-file (s "/proc/iomem" :if-does-not-exist nil)
      (when s
        (let ((ram 0) (kern 0))
          (loop for line = (read-line s nil nil) while line
                for colon = (search " : " line)
                for dash  = (and colon (position #\- line :end colon))
                when (and colon dash) do
                  (let ((start (parse-integer line :end dash
                                                   :radix 16 :junk-allowed t))
                        (end   (parse-integer line :start (1+ dash) :end colon
                                                   :radix 16 :junk-allowed t))
                        (label (string-trim " " (subseq line (+ colon 3)))))
                    (when (and start end)
                      (let ((size (- (1+ end) start)))
                        (cond
                          ((string= label "System RAM") (incf ram size))
                          ((and (>= (length label) 7)
                                (string= "Kernel " label :end2 7))
                           (incf kern size)))))))
          (values ram kern))))))

(defun report-memory ()
  "The 'm) memory' action: a fixed, compact readout sized for 80x25.
   Lisp heap  — live dynamic-space bytes + total ever consed since boot.
   This proc  — resident set size (RSS), from /proc/self/status.
   Kernel img — resident kernel image (code+rodata+data+bss), from /proc/iomem.
   Machine    — consumed / physical RAM: physical is the sum of /proc/iomem
                'System RAM' (real RAM the kernel sees, kernel image included),
                consumed = physical - MemFree. Everything the distro is using,
                kernel and all, against real RAM — not a MemTotal-relative view."
  (let* ((live   (sb-kernel:dynamic-usage))
         (consed (sb-ext:get-bytes-consed))
         (rss    (proc-field-kib "/proc/self/status" "VmRSS:"))
         (free   (proc-field-kib "/proc/meminfo"     "MemFree:")))
    (multiple-value-bind (ram kern) (scan-iomem)
      (format t "~&~%---- memory ----~%")
      (format t "  lisp heap : ~a MiB live  (~a MiB consed since boot)~%"
              (mib live) (mib consed))
      (format t "  this proc : ~a MiB resident (RSS)~%"
              (mib (and rss (* rss 1024))))
      (if (and kern (plusp kern))
          (format t "  kernel img: ~a MiB (code+rodata+data+bss)~%" (mib kern))
          (format t "  kernel img: (/proc/iomem unavailable)~%"))
      (if (and ram (plusp ram) free)
          (let ((used (- ram (* free 1024))))
            (format t "  machine   : ~a / ~a MiB used  (~d%)  free ~a~%"
                    (mib used) (mib ram) (round (* 100 used) ram)
                    (mib (* free 1024))))
          (format t "  machine   : (physical RAM unavailable)~%"))
      (format t "----------------~%")
      (finish-output))))
