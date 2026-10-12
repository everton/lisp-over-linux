;;;; json-test.lisp — host test for initramfs/json.lisp.
;;;;
;;;;   sbcl --script poc/json-test.lisp
;;;;
;;;; The cases are the ones a Messages-API client actually meets: nested objects with
;;;; arrays, every string escape, a surrogate pair (an emoji in a model's reply), the
;;;; four values NIL must never stand for, numbers in each grammar form, key ORDER
;;;; surviving a round trip (prompt caching depends on it, doc/agent.org §5), and the
;;;; malformed inputs that must fail loudly rather than half-parse.
;;;;
;;;; Prints one line per check and exits non-zero on any failure.

(load (merge-pathnames "../initramfs/json.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(use-package :lol.json)

(defvar *failures* 0)

(defun ok (name got want &key (test #'equal))
  (if (funcall test got want)
      (format t "  ok    ~a~%" name)
      (progn (incf *failures*)
             (format t "  FAIL  ~a~%        got  ~s~%        want ~s~%" name got want))))

(defun fails (name thunk)
  (if (handler-case (progn (funcall thunk) nil) (error () t))
      (format t "  ok    ~a~%" name)
      (progn (incf *failures*) (format t "  FAIL  ~a (no error)~%" name))))

(format t "~&== reading ==~%")
(ok "true / false / null are T, :FALSE, :NULL"
    (list (parse-json "true") (parse-json "false") (parse-json "null")) '(t :false :null))
(ok "empty array is #(), not NIL" (parse-json "[]") #() :test #'equalp)
(ok "empty object is a JSON-OBJECT, not NIL" (json-object-p (parse-json "{}")) t)
(ok "integers stay integers" (parse-json "-12345678901234567890") -12345678901234567890)
(ok "zero" (parse-json "0") 0)
(ok "a fraction is a double" (parse-json "1.5") 1.5d0)
(ok "an exponent is a double" (parse-json "2E3") 2000d0)
(ok "negative exponent" (parse-json "-1.25e-2") -0.0125d0)
(ok "string escapes"
    (parse-json "\"a\\\"b\\\\c\\/d\\n\\t\\r\\b\\f\"")
    (coerce (list #\a #\" #\b #\\ #\c #\/ #\d #\Newline #\Tab (code-char 13) (code-char 8) (code-char 12))
            'string))
(ok "\\u escape in the BMP" (parse-json "\"caf\\u00e9\"") (format nil "caf~c" (code-char #xe9)))
(ok "a surrogate pair becomes ONE character (U+1F600)"
    (parse-json "\"\\ud83d\\ude00\"") (string (code-char #x1F600)))
(ok "a lone high surrogate reads as U+FFFD"
    (parse-json "\"x\\ud83dy\"") (format nil "x~cy" (code-char #xFFFD)))
(ok "a lone low surrogate reads as U+FFFD"
    (parse-json "\"\\ude00\"") (string (code-char #xFFFD)))
(ok "raw UTF-8 characters pass through"
    (parse-json (format nil "\"~c\"" (code-char #x3bb))) (string (code-char #x3bb)))
(let ((v (parse-json "{\"type\":\"message\",\"content\":[{\"type\":\"text\",\"text\":\"hi\"}],
                      \"usage\":{\"input_tokens\":12,\"output_tokens\":3},\"stop_sequence\":null}")))
  (ok "jget on an object" (jget v "type") "message")
  (ok "jget* through an array" (jget* v "content" 0 "text") "hi")
  (ok "jget* into usage" (jget* v "usage" "output_tokens") 3)
  (ok "null is present and :NULL" (multiple-value-list (jget v "stop_sequence")) '(:null t))
  (ok "a missing key is NIL, not present" (multiple-value-list (jget v "nope")) '(nil nil))
  (ok "keys keep their order" (jkeys v) '("type" "content" "usage" "stop_sequence")))
(ok "whitespace everywhere" (jget (parse-json (format nil " {~% \"a\" : [ 1 , 2 ] } ")) "a")
    #(1 2) :test #'equalp)
(ok "junk-allowed returns where it stopped"
    (multiple-value-list (parse-json "{} trailing" :junk-allowed t)) (list nil 3)
    :test (lambda (g w) (= (second g) (second w))))

(format t "~&== malformed input fails loudly ==~%")
(fails "trailing garbage" (lambda () (parse-json "{} x")))
(fails "unterminated string" (lambda () (parse-json "\"abc")))
(fails "unterminated object" (lambda () (parse-json "{\"a\":1")))
(fails "missing colon" (lambda () (parse-json "{\"a\" 1}")))
(fails "trailing comma" (lambda () (parse-json "[1,]")))
(fails "leading zero then digit" (lambda () (parse-json "01")))
(fails "a bare minus" (lambda () (parse-json "-")))
(fails "raw newline inside a string" (lambda () (parse-json (format nil "\"a~%b\""))))
(fails "unknown escape" (lambda () (parse-json "\"\\q\"")))
(fails "single quotes" (lambda () (parse-json "{'a':1}")))
(fails "truncated true" (lambda () (parse-json "tru")))
(fails "empty input" (lambda () (parse-json "   ")))

(format t "~&== writing ==~%")
(ok "an object in insertion order"
    (to-json (jobj "model" "claude-opus-5-5" "max_tokens" 1024 "stream" t))
    "{\"model\":\"claude-opus-5-5\",\"max_tokens\":1024,\"stream\":true}")
(ok "symbol keys are downcased" (to-json (jobj :type "text")) "{\"type\":\"text\"}")
(ok "false, null, empty array, empty object"
    (to-json (vector :false :null #() (jobj))) "[false,null,[],{}]")
(fails "NIL is refused (it is ambiguous)" (lambda () (to-json nil)))
(fails "a list is refused (arrays are vectors)" (lambda () (to-json '(1 2))))
(fails "an unknown keyword is refused" (lambda () (to-json :maybe)))
(ok "escapes on the way out"
    (to-json (format nil "q\"b\\n~%t~cc~c" #\Tab (code-char 1)))
    "\"q\\\"b\\\\n\\nt\\tc\\u0001\"")
(ok "non-ASCII is written raw (the wire is UTF-8)"
    (to-json (string (code-char #x1F600))) (format nil "\"~c\"" (code-char #x1F600)))
(ok "doubles print as JSON, not 1.5d0" (to-json 1.5d0) "1.5")
(ok "a ratio is written as a double" (to-json 1/4) "0.25")
(ok "jset replaces in place and appends new keys at the end"
    (let ((o (jobj "a" 1 "b" 2))) (jset o "a" 9) (jset o "c" 3) (to-json o))
    "{\"a\":9,\"b\":2,\"c\":3}")

(format t "~&== round trips ==~%")
(dolist (text '("{\"a\":[1,2.5,\"x\",true,false,null,{},[]],\"b\":{\"c\":\"\\u0001\"}}"
                "[\"\\\"quoted\\\"\",\"tab\\there\",-0.5,1.0e20]"
                "{\"tool_use\":{\"id\":\"toolu_01\",\"input\":{\"form\":\"(+ 1 2)\"}}}"))
  (ok (format nil "parse . write = identity: ~a" (subseq text 0 (min 30 (length text))))
      (to-json (parse-json text)) text))
(let ((s (coerce (list #\a (code-char #x1F600) #\Newline (code-char #xe9) #\") 'string)))
  (ok "write . parse = identity on a tricky string" (parse-json (to-json s)) s))

(format t "~&~:[ALL PASS~;~:*~d FAILURE(S)~]~%" (and (plusp *failures*) *failures*))
(sb-ext:exit :code (if (zerop *failures*) 0 1))
