;;;; sbcl-source.lisp — SBCL's OWN source, read from the disk on demand.
;;;;
;;;; Our modules are captured into the image by load-recording (registry.lisp), so
;;;; their source is always there. SBCL's are not: the core is compiled machine code,
;;;; and its src/ tree is 34 MB of text we deliberately do not freeze into the heap
;;;; (doc/code-browser.org, "Two kinds of subject"). What the core DOES keep, for
;;;; every definition, is a *source location*: a logical pathname such as
;;;; SYS:SRC;CODE;LIST.LISP plus where in that file the form starts.
;;;;
;;;; Since tag #25 the laptop's /home is mounted (doc/storage.org), and the ./sbcl
;;;; tree this image was built from lives under /home — so the file that location
;;;; names is, on the real machine, simply THERE. This module turns a location into
;;;; text, the way SLIME's M-. does:
;;;;
;;;;   1. logical -> physical: SYS:SRC;CODE;LIST.LISP -> <root>/src/code/list.lisp,
;;;;      where <root> is *SBCL-SOURCE-ROOT* (baked at build time; settable).
;;;;   2. find the form: SBCL records the CHARACTER OFFSET where the top-level form's
;;;;      text begins (just after the previous form — so comments may come first)
;;;;      for functions and macros; for classes, structures and variables it records
;;;;      only the top-level form's INDEX in the file. Either way we then cut the one
;;;;      balanced form out of the text.
;;;;   3. check it: the cut must mention the name. If the file changed since the
;;;;      build, or the index counted a form our scanner did not, fall back to looking
;;;;      for "(def… NAME" in the file.
;;;;
;;;; It never READs or evaluates SBCL's source. Those files need SBCL's internal
;;;; packages and build-time features (#+sb-xc-host) to read, and all we want is to
;;;; SHOW them — so the scanner below is textual: it knows strings, ; and #| |#
;;;; comments, #\x characters and |escaped| symbols, which is all it takes to find
;;;; where a parenthesised form ends.
;;;;
;;;; Read-only by design: these are reference views. Redefining SBCL is possible but
;;;; a different, louder act (see show-reference in registry.lisp).

(defvar *sbcl-source-root*
  (ignore-errors
    ;; The core's SYS host was pointed at the build tree when ./sbcl was built, so
    ;; at OUR build time this resolves to that tree; DEFVAR freezes the string into
    ;; the image. On the laptop the same path exists again, under the mounted /home.
    (let* ((p (namestring (translate-logical-pathname "SYS:SRC;CODE;LIST.LISP")))
           (cut (search "/src/code/" p :from-end t)))
      (and cut (subseq p 0 (1+ cut)))))
  "The SBCL source tree (ends in /), holding src/ and contrib/. NIL if unknown.
   Set it to another checkout of the SAME SBCL version to read sources from there.")

(defparameter *sbcl-source-max-chars* 200000
  "The longest single form we will show. A top-level form in SBCL is rarely more than
   a few hundred lines; this only guards against a scanner gone wrong on a huge file.")

;;; ---- 1. logical -> physical ------------------------------------------------

(defun sbcl-physical-path (location)
  "LOCATION (a pathname or namestring from sb-introspect) as a path in
   *SBCL-SOURCE-ROOT*. SYS:SRC;CODE;LIST.LISP -> <root>src/code/list.lisp: the host
   goes, each ; becomes a /, and the name is lowercased, as SBCL's own translation
   does on Unix. A trailing .NEWEST (a logical pathname's version) is dropped. A
   physical location is returned as is. NIL when there is no root."
  (let ((s (if (pathnamep location) (namestring location) location)))
    (cond ((null s) nil)
          ((not (and (> (length s) 4) (string-equal "SYS:" s :end2 4))) s)
          ((null *sbcl-source-root*) nil)
          (t (let* ((rel (string-downcase (substitute #\/ #\; (subseq s 4))))
                    (rel (if (and (> (length rel) 7)
                                  (string= ".newest" rel :start2 (- (length rel) 7)))
                             (subseq rel 0 (- (length rel) 7))
                             rel)))
               (concatenate 'string *sbcl-source-root* rel))))))

(defun read-text-file (path)
  "The whole file as a string, or NIL if it is missing or unreadable. SBCL's sources
   are UTF-8 (a few carry non-ASCII in comments); a bad byte is replaced, not fatal."
  (ignore-errors
    (with-open-file (in path :external-format '(:utf-8 :replacement #\?))
      (let* ((buf (make-string (file-length in)))
             (n (read-sequence buf in)))
        (subseq buf 0 n)))))

;;; ---- 2. the textual scanner ------------------------------------------------

(defun skip-blank (text i)
  "The index of the first character at or after I that is not whitespace or inside
   a comment (; to end of line, #| … |# nested). (length TEXT) if none."
  (let ((n (length text)))
    (loop
      (cond ((>= i n) (return n))
            ((member (char text i) '(#\Space #\Tab #\Newline #\Return #\Page)) (incf i))
            ((char= (char text i) #\;)
             (setf i (or (position #\Newline text :start i) n)))
            ((and (char= (char text i) #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
             (setf i (skip-block-comment text (+ i 2))))
            (t (return i))))))

(defun skip-block-comment (text i)
  "Past the |# that closes a #| whose body starts at I. They nest, as in the reader."
  (let ((n (length text)) (depth 1))
    (loop
      (cond ((>= i (1- n)) (return n))
            ((and (char= (char text i) #\|) (char= (char text (1+ i)) #\#))
             (incf i 2) (when (zerop (decf depth)) (return i)))
            ((and (char= (char text i) #\#) (char= (char text (1+ i)) #\|))
             (incf i 2) (incf depth))
            (t (incf i))))))

(defun form-end (text start)
  "The index just past the one form that begins at START (no leading blanks). Knows
   lists, strings, #\\x characters, |symbols|, comments, and the prefixes ' ` , ,@ #'
   #. and #+feature / #-feature (whose feature expression and form count as one).
   NIL if the text ends first — an unbalanced cut is reported, never guessed."
  (let ((n (length text)))
    (labels ((token-end (i)
               ;; an atom: runs until a delimiter; |…| and \x escape inside it
               (loop
                 (when (>= i n) (return n))
                 (let ((c (char text i)))
                   (cond ((char= c #\\) (incf i 2))
                         ((char= c #\|)
                          (setf i (let ((e (position #\| text :start (1+ i))))
                                    (if e (1+ e) n))))
                         ((member c '(#\Space #\Tab #\Newline #\Return #\Page
                                      #\( #\) #\" #\; #\' #\`))
                          (return i))
                         (t (incf i))))))
             (one (i)
               (setf i (skip-blank text i))
               (when (>= i n) (return-from form-end nil))
               (let ((c (char text i)))
                 (cond
                   ((char= c #\()
                    (let ((j (1+ i)))
                      (loop
                        (setf j (skip-blank text j))
                        (cond ((>= j n) (return-from form-end nil))
                              ((char= (char text j) #\)) (return (1+ j)))
                              (t (setf j (one j)))))))
                   ((char= c #\")
                    (let ((j (1+ i)))
                      (loop
                        (cond ((>= j n) (return-from form-end nil))
                              ((char= (char text j) #\\) (incf j 2))
                              ((char= (char text j) #\") (return (1+ j)))
                              (t (incf j))))))
                   ((member c '(#\' #\`)) (one (1+ i)))
                   ((char= c #\,)
                    (one (if (and (< (1+ i) n) (member (char text (1+ i)) '(#\@ #\.)))
                             (+ i 2) (1+ i))))
                   ((and (char= c #\#) (< (1+ i) n))
                    (let ((d (char text (1+ i))))
                      (case d
                        (#\\ (token-end (+ i 3)))     ; #\( is one character, then the name
                        ((#\' #\.) (one (+ i 2)))
                        ((#\+ #\-) (one (one (+ i 2)))) ; the feature, then the form
                        (#\( (one (1+ i)))            ; #( vector )
                        (t (token-end (+ i 2))))))    ; #x1F, #:foo, #p"…" is close enough
                   (t (token-end i))))))
      (one start))))

(defun form-at (text offset)
  "The text of the first form at or after OFFSET (blanks and comments skipped), and
   its start index. NIL when nothing balanced is there."
  (let* ((start (skip-blank text (max 0 (min offset (length text)))))
         (end (and (< start (length text)) (form-end text start))))
    (when (and end (<= (- end start) *sbcl-source-max-chars*))
      (values (subseq text start end) start))))

(defun nth-form-start (text n)
  "The index where the Nth top-level form of TEXT starts (0-based), counting the way
   our scanner does. Close to the compiler's numbering, which counts only forms the
   reader kept: a #+feature form that was dropped at SBCL's build is one here and
   none there. That is what the name check in SBCL-DEFINITION-SOURCE is for."
  (let ((i 0))
    (dotimes (k n (skip-blank text i))
      (let ((e (form-end text (skip-blank text i))))
        (unless e (return nil))
        (setf i e)))))

(defun line-of (text index)
  "1-based line number of INDEX in TEXT."
  (1+ (count #\Newline text :end (min index (length text)))))

;;; ---- 3. finding a definition, and checking we found it ----------------------

(defun mentions-name-p (form name)
  "Does FORM's text contain NAME as a whole symbol (any case, package prefix allowed)?"
  (let ((sym (string-downcase (symbol-name name)))
        (low (string-downcase form)))
    (loop for p = (search sym low) then (search sym low :start2 (1+ p))
          while p
          thereis (flet ((delim (i) (or (< i 0) (>= i (length low))
                                        (find (char low i) " ()'`,:
	\"|#"))))
                    (and (delim (1- p)) (delim (+ p (length sym))))))))

(defun find-def-by-name (text name)
  "Fallback: the start of a line-initial definition form whose first argument is NAME
   — (defun car …), (sb-xc:defmacro loop …), (defstruct (hash-table …) …). The
   operator may carry a package prefix and must start with DEF. NIL if none."
  (let ((sym (string-downcase (symbol-name name)))
        (low (string-downcase text))
        (blank '(#\Space #\Tab #\Newline)))
    (loop for p = (search (string #\Newline) low) then (search (string #\Newline) low :start2 (1+ p))
          for at = (and p (1+ p))
          while p
          do (when (and (< at (length low)) (char= (char low at) #\())
               (let* ((op-end (position-if (lambda (c) (member c blank)) low :start at))
                      (op (and op-end (subseq low (1+ at) op-end)))
                      (op (and op (subseq op (1+ (or (position #\: op :from-end t) -1)))))
                      (arg (and op-end (position-if-not (lambda (c) (member c (cons #\( blank)))
                                                        low :start op-end)))
                      (e (and arg (+ arg (length sym)))))
                 (when (and op (> (length op) 3) (string= "def" op :end2 3)
                            (<= e (length low))
                            (string= sym low :start2 arg :end2 e)
                            (or (= e (length low)) (member (char low e) (list* #\) blank))))
                   (return at)))))))

(defun first-form-mentioning (text name)
  "Fallback of the fallback: the start of the first top-level form whose text names
   NAME at all. Finds what is defined INSIDE another definition — a condition's slot
   reader lives in its DEFINE-CONDITION, a structure's accessor in its DEFSTRUCT."
  (let ((i 0) (n (length text)))
    (loop
      (let ((start (skip-blank text i)))
        (when (>= start n) (return nil))
        (let ((end (form-end text start)))
          (unless end (return nil))
          (when (mentions-name-p (subseq text start end) name) (return start))
          (setf i end))))))

(defparameter +introspect-kinds+
  '(:function :macro :generic-function :compiler-macro :variable :constant
    :structure :condition :class :type :setf-expander :method-combination)
  "The sb-introspect definition kinds, tried in this order when none is given.")

(defun sbcl-definition-source (name &optional kind)
  "SBCL's own source for NAME, read from *SBCL-SOURCE-ROOT* on the disk. Returns
   (values TEXT PATH LINE), or NIL with a REASON string as the second value when the
   location is unknown, the file is not on this disk, or the form cannot be found.
   KIND is an sb-introspect kind (:function, :class, …); NIL tries them all.
   Never signals: the browser calls it while drawing."
  (handler-case
      (let ((src (loop for k in (if kind (list kind) +introspect-kinds+)
                       for s = (first (ignore-errors
                                       (sb-introspect:find-definition-sources-by-name name k)))
                       when (and s (sb-introspect:definition-source-pathname s)) return s)))
        (if (null src)
            (values nil "SBCL records no source location for it")
            (let* ((loc (sb-introspect:definition-source-pathname src))
                   (path (sbcl-physical-path loc))
                   (text (and path (read-text-file path))))
              (cond
                ((null path) (values nil (format nil "~a (no SBCL source root known)" loc)))
                ((null text) (values nil (format nil "~a (not on this disk)" path)))
                (t
                 (let* ((off (sb-introspect:definition-source-character-offset src))
                        (tlf (first (sb-introspect:definition-source-form-path src)))
                        (start (cond (off off)
                                     (tlf (nth-form-start text tlf)))))
                   (multiple-value-bind (form at) (and start (form-at text start))
                     (if (and form (mentions-name-p form name))
                         (values form path (line-of text at))
                         (let ((p (or (find-def-by-name text name)
                                        (first-form-mentioning text name))))
                           (multiple-value-bind (form2 at2) (and p (form-at text p))
                             (if form2
                                 (values form2 path (line-of text at2))
                                 (values nil (format nil "~a (the form was not found)"
                                                     path)))))))))))))
    (error (e) (values nil (format nil "~a" e)))))
