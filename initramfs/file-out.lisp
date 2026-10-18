;;;; file-out.lisp — write live edits back into the repo's own .lisp files.
;;;;
;;;; The journal (journal.lisp) keeps an Accept across a REBOOT. It cannot keep one
;;;; across a REBUILD for long: a newer image treats an old record as "pending", to be
;;;; looked at by hand, because the code around it may have moved. What survives a
;;;; rebuild is source in the repo. So this file closes the loop Smalltalk's file-out
;;;; leaves open: instead of producing a file for a human to merge, it puts each
;;;; changed definition back where it came from (doc/session-persistence.org §2b,
;;;; "File-out to the repo's source").
;;;;
;;;; Why that is possible at all: build.sh loads each module by its ABSOLUTE path in
;;;; the checkout, so DEFN-FILE is e.g. /home/everton/lisp-over-linux/poc/textview.lisp
;;;; — and on the laptop the image's /home IS the host's (doc/storage.org), so that
;;;; path exists, writable by us, inside the running image.
;;;;
;;;; The rule is VERBATIM, and it is deliberately the only rule. For an EDITED
;;;; definition the registry still holds the text the build read — the OLDEST entry of
;;;; DEFN-VERSIONS — and LOAD-RECORDING captured it with its comments and spacing. So:
;;;;
;;;;   that text occurs EXACTLY ONCE in the file  -> replace it with the current source
;;;;   the CURRENT text is already there instead  -> :already (filed out before)
;;;;   anything else (0 or 2+ copies, no file)    -> leave the file alone and append the
;;;;                                                 change to changes-unfolded.lisp
;;;;
;;;; A definition BORN this session has no home text to replace, and choosing a file
;;;; for it is a judgement (open question 4 in the doc, decided by default: always
;;;; unfolded, for you to place). Every file written goes through
;;;; WRITE-FILE-ATOMICALLY: a crash leaves the old file or the new one.
;;;;
;;;; Then, from Debian: git diff, commit, rebuild. On the next boot each journal record
;;;; whose text is now the build's own is RETIRED by replay, by itself.

(in-package :cl-user)

(defun count-occurrences (needle haystack)
  "How many times NEEDLE occurs in HAYSTACK (non-overlapping)."
  (if (zerop (length needle))
      0
      (loop with start = 0 for pos = (search needle haystack :start2 start)
            while pos count t do (setf start (+ pos (length needle))))))

(defun replace-once (old new text)
  "TEXT with its single occurrence of OLD replaced by NEW."
  (let ((pos (search old text)))
    (concatenate 'string (subseq text 0 pos) new (subseq text (+ pos (length old))))))

(defun comment-preamble (text)
  "The blank and comment lines TEXT starts with, before its first form — or \"\".
   LOAD-RECORDING's span runs from the end of the previous form, so a recorded
   definition carries the comment written above it (and the file's header, for the
   first form in a file)."
  (let ((i 0) (n (length text)))
    (loop while (< i n)
          do (let* ((eol (or (position #\Newline text :start i) n))
                    (line (string-left-trim '(#\Space #\Tab) (subseq text i eol))))
               (if (or (zerop (length line)) (char= (char line 0) #\;))
                   (setf i (min n (1+ eol)))
                   (return))))
    (subseq text 0 i)))

(defun replacement-text (original current)
  "What goes in ORIGINAL's place. CURRENT is the accepted source; when it was typed
   without the comments the build's text had above it (an Accept from the Workspace,
   or the agent's tool, sends only the form), those comments are kept — file-out must
   not delete a file's header or a definition's doc-comment as a side effect."
  (let ((pre (comment-preamble original)))
    (if (and (plusp (length pre)) (zerop (length (comment-preamble current))))
        (concatenate 'string pre current)
        current)))

(defun file-out-candidates ()
  "Every DEFN changed this session: edited (it has VERSIONS) or born. File order is
   kept within a file so several edits in one module are applied in turn."
  (let ((out '()))
    (maphash (lambda (name defns)
               (declare (ignore name))
               (dolist (d defns)
                 (when (or (defn-versions d) (defn-born d)) (push d out))))
             *registry*)
    (sort out #'string< :key (lambda (d) (format nil "~a ~a" (or (defn-file d) "") (defn-name d))))))

(defun plan-file-out (defns)
  "Decide, without writing anything, what to do with each DEFN.
   -> (values RESULTS NEW-TEXTS): RESULTS is a list of plists (:defn D :status S
   :reason R), NEW-TEXTS an alist (PATH . TEXT) of the files to rewrite."
  (let ((texts '()) (results '()))
    (flet ((text-of (path)
             (let ((cell (assoc path texts :test #'string=)))
               (if cell (cdr cell)
                   (let ((tx (ignore-errors (slurp-utf-8 path))))
                     (when tx (push (cons path tx) texts))
                     tx))))
           (note (d status reason) (push (list :defn d :status status :reason reason) results)))
      (dolist (d defns)
        (let* ((file (defn-file d))
               (original (car (last (defn-versions d))))
               (current (defn-source d)))
          (cond
            ((defn-born d) (note d :unfolded "new this session - its place in a file is yours to choose"))
            ((string= original current) (note d :unchanged "edited back to the build's own text"))
            ((or (null file) (not (stringp file)) (not (path-exists-p file)))
             (note d :unfolded (format nil "~a is not on this disk" (or file "its file"))))
            (t
             (let* ((text (text-of file))
                    (n (and text (count-occurrences original text))))
               (cond ((null text) (note d :unfolded (format nil "~a could not be read" file)))
                     ((eql n 1)
                      (setf (cdr (assoc file texts :test #'string=))
                            (replace-once original (replacement-text original current) text))
                      (note d :folded file))
                     ((and (eql n 0)
                           (plusp (count-occurrences (replacement-text original current) text)))
                      (note d :already file))
                     (t (note d :unfolded
                              (format nil "the build's text occurs ~d times in ~a - the file has moved on"
                                      n file))))))))))
    (let ((results (reverse results)))
      (values results
              ;; only the files something was folded into
              (remove-if-not (lambda (cell)
                               (find-if (lambda (r) (and (eq (getf r :status) :folded)
                                                         (string= (getf r :reason) (car cell))))
                                        results))
                             texts)))))

(defun unfolded-text (results)
  "The changes-unfolded.lisp entries for RESULTS' :UNFOLDED ones: each a comment
   saying where it came from and why it was not placed, an IN-PACKAGE so the form
   reads in its package when pasted or loaded, and the source."
  (with-output-to-string (o)
    (dolist (r results)
      (when (eq (getf r :status) :unfolded)
        (let ((d (getf r :defn)))
          (format o "~%;;; ---- ~(~a~) ~a  (~a)~%;;; from ~a, build ~a~%;;; not placed: ~a~%(in-package ~s)~%~a~%"
                  (defn-kind d) (string-downcase (princ-to-string (defn-name d)))
                  (multiple-value-bind (s mi h dd mo y) (decode-universal-time (get-universal-time) 0)
                    (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" y mo dd h mi s))
                  (or (defn-file d) "no file") *build-stamp* (getf r :reason)
                  (or (defn-package d) "CL-USER") (defn-source d)))))))

(defun file-out-changes (&key (defns (file-out-candidates)) (unfolded-path :default))
  "File the session's changes out: fold what can be folded into its own .lisp file,
   put the rest in changes-unfolded.lisp beside the journal. Returns the RESULTS
   (see PLAN-FILE-OUT), each :FOLDED one now on the disk. UNFOLDED-PATH NIL writes
   no leftovers file (the host test asks for it explicitly)."
  (multiple-value-bind (results texts) (plan-file-out defns)
    (dolist (cell texts)
      (handler-case
          (write-file-atomically (car cell) (sb-ext:string-to-octets (cdr cell) :external-format :utf-8))
        (error (c)
          ;; the file refused: everything planned into it becomes unfolded instead
          (dolist (r results)
            (when (and (eq (getf r :status) :folded) (string= (getf r :reason) (car cell)))
              (setf (getf r :status) :unfolded
                    (getf r :reason) (format nil "writing ~a failed: ~a" (car cell) c)))))))
    (let ((path (if (eq unfolded-path :default) (persist-path :journal "changes-unfolded.lisp") unfolded-path))
          (text (unfolded-text results)))
      (when (and path (plusp (length text)))
        (append-synced path text :mode #o644)))
    results))

(defun file-out-summary (results)
  (let ((counts (loop for s in '(:folded :already :unfolded :unchanged)
                      for n = (count s results :key (lambda (r) (getf r :status)))
                      when (plusp n) collect (format nil "~d ~(~a~)" n s))))
    (if counts
        (format nil "file out: ~{~a~^, ~}~:[~; (unfolded ones in ~~/.lol/journal/changes-unfolded.lisp)~]"
                counts (find :unfolded results :key (lambda (r) (getf r :status))))
        "file out: nothing changed this session")))

(defun file-out-and-report ()
  "The journal view's F: file out, say what happened in the status line."
  (setf *browser-status*
        (handler-case (file-out-summary (file-out-changes))
          (error (c) (format nil "file out failed: ~a" c)))))
