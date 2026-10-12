;;;; json.lisp — JSON in and out, with a mapping chosen to fit Lisp rather than fight it.
;;;;
;;;; The agent (doc/agent.org) speaks the Messages API, whose every request and every
;;;; streamed event is JSON. A JSON library is about two hundred lines; the part worth
;;;; thinking about is the MAPPING — which Lisp object each JSON value becomes — and
;;;; doc/agent.org §5 decided it before a line was written:
;;;;
;;;;     JSON      Lisp                      why
;;;;     object    a JSON-OBJECT instance    so PRESENT can dispatch on it: an API reply
;;;;               (an ordered alist inside)   is browsable like any other subject
;;;;     array     a SIMPLE-VECTOR           distinguishable from a list at a glance
;;;;     string    a STRING
;;;;     number    an INTEGER or DOUBLE-FLOAT
;;;;     true      T
;;;;     false     :FALSE                    never NIL ...
;;;;     null      :NULL                     ... because NIL would mean three things
;;;;
;;;; NIL is the trap every Lisp JSON library has to decide about. If false, null, []
;;;; and {} all read as NIL, then nothing that reads them back can tell them apart,
;;;; and the API does care: "stop_sequence": null is not "stop_sequence": false. So
;;;; here NIL is NOT A JSON VALUE AT ALL — the writer refuses it, with a message that
;;;; names the four things you might have meant. That costs one keyword at a few call
;;;; sites and buys a reader and writer that round-trip exactly.
;;;;
;;;; ORDER IS PRESERVED, and that is not cosmetic. The API caches a prompt by its
;;;; exact bytes (prompt caching, doc/agent.org §5): serialise the same request twice
;;;; with the keys in a different order and the cache silently misses. So an object
;;;; keeps its keys in insertion order, the writer emits them in that order, and the
;;;; same Lisp value always prints as the same bytes.
;;;;
;;;; Strings: JSON is Unicode text; we read and write Lisp strings of characters and
;;;; leave the bytes to the caller (UTF-8 on the wire, octets-to-string at the edge).
;;;; The one place Unicode leaks into the syntax is \uXXXX, which counts UTF-16 code
;;;; units — so a character outside the Basic Multilingual Plane (an emoji, U+1F600)
;;;; arrives as a SURROGATE PAIR, two escapes that must be glued back into one
;;;; character. A lone half of a pair is malformed; we read it as U+FFFD (the
;;;; replacement character) rather than fail a whole API reply over one bad glyph.
;;;;
;;;; Pure, dependency-free, CL only: it loads into the image and into a bare host SBCL
;;;; alike (host test: poc/json-test.lisp). Spec: RFC 8259.
;;;;
;;;; All original code, under this project's MIT licence.

(defpackage :lol.json
  (:use :cl)
  (:export #:json-object #:json-object-p #:json-object-pairs
           #:jobj #:jget #:jget* #:jset #:jkeys #:jhas
           #:json-parse-error #:json-error-position
           #:parse-json #:to-json #:write-json
           #:json-true-p #:json-null-p))

(in-package :lol.json)

;;; ===========================================================================
;;; Objects: a class, so the rest of the image can dispatch on them
;;; ===========================================================================

(defclass json-object ()
  ((pairs :initarg :pairs :initform '() :accessor json-object-pairs
          :documentation "An alist (KEY . VALUE), KEY a string, in insertion order."))
  (:documentation "A JSON object. A class rather than a hash table or a plist so that
   (PRESENT obj) can show an API reply as a drillable tree (doc/agent.org §5)."))

(defun json-object-p (x) (typep x 'json-object))

(defmethod print-object ((o json-object) stream)
  (print-unreadable-object (o stream :type nil)
    (format stream "json ~{~a~^ ~}~:[~; …~]"
            (mapcar #'car (subseq (json-object-pairs o) 0 (min 4 (length (json-object-pairs o)))))
            (> (length (json-object-pairs o)) 4))))

(defun jobj (&rest keys-and-values)
  "A JSON object from alternating KEY VALUE arguments, in that order:
   (jobj \"type\" \"text\" \"text\" \"hi\"). Keys are strings; a symbol key is
   downcased, so (jobj :type \"text\") reads the same."
  (let ((pairs '()))
    (loop for (k v) on keys-and-values by #'cddr
          do (push (cons (if (symbolp k) (string-downcase (symbol-name k)) k) v) pairs))
    (make-instance 'json-object :pairs (nreverse pairs))))

(defun jget (object key &optional default)
  "The value of KEY in OBJECT, or DEFAULT. Second value: was it present? Safe on
   anything — a non-object simply has no keys, which keeps deep lookups short."
  (if (json-object-p object)
      (let ((cell (assoc key (json-object-pairs object) :test #'string=)))
        (if cell (values (cdr cell) t) (values default nil)))
      (values default nil)))

(defun jget* (object &rest path)
  "Walk PATH, a list of keys (strings) and indices (integers), from OBJECT:
   (jget* reply \"usage\" \"output_tokens\"), (jget* reply \"content\" 0 \"text\").
   NIL as soon as a step is missing."
  (let ((here object))
    (dolist (step path here)
      (setf here (cond ((integerp step)
                        (and (simple-vector-p here) (< -1 step (length here)) (svref here step)))
                       (t (jget here step))))
      (unless here (return nil)))))

(defun jhas (object key) (nth-value 1 (jget object key)))

(defun jset (object key value)
  "Set KEY to VALUE in OBJECT: replace in place if present (keeping its position),
   else append at the END — so building a request field by field keeps that order."
  (let ((cell (assoc key (json-object-pairs object) :test #'string=)))
    (if cell
        (setf (cdr cell) value)
        (setf (json-object-pairs object)
              (append (json-object-pairs object) (list (cons key value)))))
    value))

(defun jkeys (object) (mapcar #'car (json-object-pairs object)))

(defun json-true-p (x) (eq x t))
(defun json-null-p (x) (eq x :null))

;;; ===========================================================================
;;; Reading
;;; ===========================================================================

(define-condition json-parse-error (error)
  ((position :initarg :position :reader json-error-position)
   (message  :initarg :message  :reader json-error-message))
  (:report (lambda (c s)
             (format s "JSON: ~a at character ~d" (json-error-message c) (json-error-position c)))))

(defun json-fail (pos fmt &rest args)
  (error 'json-parse-error :position pos :message (apply #'format nil fmt args)))

;;; The reader is a recursive descent over a STRING with an explicit index — no
;;; streams, because every JSON text we meet is already in hand (one SSE data line, one
;;; HTTP body, one tool input accumulated from fragments).

(declaim (inline ws-p))
(defun ws-p (c) (member c '(#\Space #\Tab #\Newline #\Return)))

(defun skip-ws (s i)
  (loop while (and (< i (length s)) (ws-p (char s i))) do (incf i))
  i)

(defun parse-json (string &key (start 0) (junk-allowed nil))
  "Read one JSON value from STRING. Returns (values VALUE END-INDEX). Signals
   JSON-PARSE-ERROR on malformed input, or on anything but whitespace after the value
   unless JUNK-ALLOWED."
  (let ((s (if (simple-string-p string) string (coerce string 'simple-string))))
    (multiple-value-bind (value end) (read-value s (skip-ws s start))
      (let ((end (skip-ws s end)))
        (when (and (not junk-allowed) (< end (length s)))
          (json-fail end "unexpected ~s after the value" (char s end)))
        (values value end)))))

(defun read-value (s i)
  (when (>= i (length s)) (json-fail i "unexpected end of input"))
  (let ((c (char s i)))
    (case c
      (#\{ (read-object s (1+ i)))
      (#\[ (read-array s (1+ i)))
      (#\" (read-string s (1+ i)))
      (#\t (read-literal s i "true" t))
      (#\f (read-literal s i "false" :false))
      (#\n (read-literal s i "null" :null))
      (t (if (or (char= c #\-) (digit-char-p c))
             (read-number s i)
             (json-fail i "unexpected ~s" c))))))

(defun read-literal (s i word value)
  (let ((end (+ i (length word))))
    (unless (and (<= end (length s)) (string= word s :start2 i :end2 end))
      (json-fail i "expected ~a" word))
    (values value end)))

(defun read-object (s i)
  (let ((pairs '()) (i (skip-ws s i)))
    (when (and (< i (length s)) (char= (char s i) #\}))
      (return-from read-object (values (make-instance 'json-object) (1+ i))))
    (loop
      (unless (and (< i (length s)) (char= (char s i) #\"))
        (json-fail i "expected a string key"))
      (multiple-value-bind (key j) (read-string s (1+ i))
        (setf j (skip-ws s j))
        (unless (and (< j (length s)) (char= (char s j) #\:))
          (json-fail j "expected ':' after key ~s" key))
        (multiple-value-bind (value k) (read-value s (skip-ws s (1+ j)))
          (push (cons key value) pairs)
          (setf k (skip-ws s k))
          (cond ((>= k (length s)) (json-fail k "unterminated object"))
                ((char= (char s k) #\,) (setf i (skip-ws s (1+ k))))
                ((char= (char s k) #\})
                 (return (values (make-instance 'json-object :pairs (nreverse pairs))
                                 (1+ k))))
                (t (json-fail k "expected ',' or '}' in object"))))))))

(defun read-array (s i)
  (let ((items '()) (i (skip-ws s i)))
    (when (and (< i (length s)) (char= (char s i) #\]))
      (return-from read-array (values (vector) (1+ i))))
    (loop
      (multiple-value-bind (value k) (read-value s i)
        (push value items)
        (setf k (skip-ws s k))
        (cond ((>= k (length s)) (json-fail k "unterminated array"))
              ((char= (char s k) #\,) (setf i (skip-ws s (1+ k))))
              ((char= (char s k) #\])
               (return (values (coerce (nreverse items) 'simple-vector) (1+ k))))
              (t (json-fail k "expected ',' or ']' in array")))))))

(defun read-hex4 (s i)
  "The four hex digits at I as an integer (a UTF-16 code unit)."
  (unless (<= (+ i 4) (length s)) (json-fail i "short \\u escape"))
  (or (ignore-errors (parse-integer s :start i :end (+ i 4) :radix 16))
      (json-fail i "bad \\u escape")))

(defun read-string (s i)
  "Read a string body starting just after its opening quote. Returns (values STRING END)."
  (let ((out (make-string-output-stream)) (n (length s)))
    (loop
      (when (>= i n) (json-fail i "unterminated string"))
      (let ((c (char s i)))
        (cond
          ((char= c #\") (return (values (get-output-stream-string out) (1+ i))))
          ((char< c (code-char 32))
           ;; RFC 8259: control characters must be escaped inside a string
           (json-fail i "raw control character U+~4,'0x in a string" (char-code c)))
          ((char/= c #\\) (write-char c out) (incf i))
          (t
           (when (>= (1+ i) n) (json-fail i "unterminated escape"))
           (let ((e (char s (1+ i))))
             (incf i 2)
             (case e
               (#\" (write-char #\" out)) (#\\ (write-char #\\ out)) (#\/ (write-char #\/ out))
               (#\b (write-char (code-char 8) out))  (#\f (write-char (code-char 12) out))
               (#\n (write-char #\Newline out))      (#\r (write-char (code-char 13) out))
               (#\t (write-char #\Tab out))
               (#\u
                (let ((u (read-hex4 s i)))
                  (incf i 4)
                  (cond
                    ;; a HIGH surrogate (D800-DBFF) must be followed by \u LOW (DC00-DFFF):
                    ;; together they encode one code point above U+FFFF
                    ((<= #xD800 u #xDBFF)
                     (let ((lo (and (<= (+ i 6) n) (char= (char s i) #\\) (char= (char s (1+ i)) #\u)
                                    (read-hex4 s (+ i 2)))))
                       (if (and lo (<= #xDC00 lo #xDFFF))
                           (progn (write-char (code-char (+ #x10000 (ash (- u #xD800) 10) (- lo #xDC00))) out)
                                  (incf i 6))
                           (write-char (code-char #xFFFD) out))))   ; a lone high half
                    ((<= #xDC00 u #xDFFF) (write-char (code-char #xFFFD) out)) ; a lone low half
                    (t (write-char (code-char u) out)))))
               (t (json-fail (- i 1) "unknown escape \\~a" e))))))))))

(defun read-number (s i)
  "RFC 8259's grammar: -? int frac? exp?. An integer stays an INTEGER (token counts,
   ids, indices: exact); anything with a fraction or exponent is a DOUBLE-FLOAT."
  (let ((start i) (n (length s)) (float nil))
    (flet ((digits ()
             (let ((d0 i))
               (loop while (and (< i n) (digit-char-p (char s i))) do (incf i))
               (when (= i d0) (json-fail i "expected a digit")))))
      (when (char= (char s i) #\-) (incf i))
      (if (and (< i n) (char= (char s i) #\0))
          (incf i)                                   ; a leading 0 stands alone
          (digits))
      (when (and (< i n) (char= (char s i) #\.))
        (setf float t) (incf i) (digits))
      (when (and (< i n) (char-equal (char s i) #\e))
        (setf float t) (incf i)
        (when (and (< i n) (member (char s i) '(#\+ #\-))) (incf i))
        (digits))
      (let ((text (subseq s start i)))
        (values (if float
                    ;; READ-FROM-STRING with the double format, on text the grammar above
                    ;; has already proved is a number — and *READ-EVAL* off regardless.
                    (let ((*read-default-float-format* 'double-float) (*read-eval* nil))
                      (coerce (read-from-string text) 'double-float))
                    (parse-integer text))
                i)))))

;;; ===========================================================================
;;; Writing
;;; ===========================================================================

(defun write-json-string (string out)
  (write-char #\" out)
  (loop for c across string
        for code = (char-code c)
        do (case c
             (#\" (write-string "\\\"" out))
             (#\\ (write-string "\\\\" out))
             (#\Newline (write-string "\\n" out))
             (#\Tab (write-string "\\t" out))
             (t (cond ((= code 13) (write-string "\\r" out))
                      ((= code 8) (write-string "\\b" out))
                      ((= code 12) (write-string "\\f" out))
                      ;; other controls, and the two line separators JavaScript chokes on
                      ((or (< code 32) (= code #x2028) (= code #x2029))
                       (format out "\\u~4,'0x" code))
                      (t (write-char c out))))))   ; everything else raw: the wire is UTF-8
  (write-char #\" out))

(defun write-json-number (x out)
  (cond ((integerp x) (format out "~d" x))
        ((or (sb-ext:float-infinity-p (float x 1d0)) (sb-ext:float-nan-p (float x 1d0)))
         (error "JSON has no infinities or NaN: ~s" x))
        (t
         ;; PRIN1 of a double, with doubles as the default format, prints 1.5 and
         ;; 1.0e20 — both valid JSON — rather than 1.5d0.
         (let ((*read-default-float-format* 'double-float))
           (prin1 (float x 1d0) out)))))

(defun write-json (value &optional (out *standard-output*))
  "Write VALUE as compact JSON to OUT. See the header for the mapping. NIL is
   refused: it could mean false, null, an empty array or an empty object."
  (cond
    ((eq value t) (write-string "true" out))
    ((eq value :false) (write-string "false" out))
    ((eq value :null) (write-string "null" out))
    ((null value)
     (error "NIL is not a JSON value here: write :FALSE, :NULL, #() or (JOBJ) — ~
             whichever you meant (lol.json's header says why)."))
    ((stringp value) (write-json-string value out))
    ((realp value) (write-json-number value out))
    ((json-object-p value)
     (write-char #\{ out)
     (loop for (k . v) in (json-object-pairs value) for first = t then nil
           do (unless first (write-char #\, out))
              (write-json-string k out) (write-char #\: out) (write-json v out))
     (write-char #\} out))
    ((vectorp value)
     (write-char #\[ out)
     (loop for v across value for first = t then nil
           do (unless first (write-char #\, out)) (write-json v out))
     (write-char #\] out))
    ((symbolp value)
     ;; a keyword other than the three above is a programming slip; say which
     (error "~s is not a JSON value (strings, numbers, T, :FALSE, :NULL, vectors, objects)" value))
    (t (error "~s cannot be written as JSON" value)))
  value)

(defun to-json (value)
  "VALUE as a JSON string."
  (with-output-to-string (s) (write-json value s)))
