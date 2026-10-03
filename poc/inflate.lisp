;;;; inflate.lisp — DEFLATE decompression (RFC 1951), with its ZLIB (RFC 1950) and
;;;; GZIP (RFC 1952) wrappers, in plain Common Lisp.
;;;;
;;;; host-client/png.lisp WRITES deflate (LZ77 + the fixed Huffman code). This is the
;;;; other half: reading it. It exists because a file manager wants to preview a PNG,
;;;; and every PNG's pixels are a ZLIB stream — and because the image is a frozen SBCL
;;;; heap with no zlib.so to call. So: pure Lisp, no FFI, no dependencies, loadable
;;;; into the image and into a bare host SBCL alike. Design notes: doc/previews.org §2.
;;;;
;;;; What a DEFLATE stream is, in one paragraph. A sequence of BLOCKS. Each starts
;;;; with 3 header bits: BFINAL (is this the last block?) and BTYPE — 00 stored
;;;; (raw bytes, length-prefixed), 01 fixed Huffman (a code table everyone knows),
;;;; 10 dynamic Huffman (the block first transmits its own code table). A Huffman
;;;; block is a stream of SYMBOLS: 0-255 are literal bytes, 256 ends the block, and
;;;; 257-285 announce a back-reference ("copy LENGTH bytes from DISTANCE bytes ago"),
;;;; the length refined by some extra bits, the distance coded by a second Huffman
;;;; table plus extra bits. That back-reference is LZ77; the Huffman codes are how
;;;; the symbols are squeezed. Decoding is the easy direction: no searching, just
;;;; following instructions.
;;;;
;;;; THE BIT-ORDER TRAP (same as the encoder's header says): the stream is packed
;;;; LSB-first, but Huffman codes are sent MSB-first. The decoder keeps an LSB-first
;;;; bit buffer, so a code appears in it BIT-REVERSED. We build the lookup table
;;;; indexed by the reversed code, and the problem disappears.
;;;;
;;;; SPEED. The two things that make an inflater fast, both done here:
;;;;   1. a BIT BUFFER: a fixnum holding up to ~56 unread bits, refilled a byte at
;;;;      a time — not a function call per bit;
;;;;   2. TABLE-DRIVEN Huffman decoding: peek the next 10 bits, index a 1024-entry
;;;;      table, get (symbol, code length) in one AREF. Codes longer than 10 bits
;;;;      (rare: they are by construction the rarest symbols) fall to a slow
;;;;      canonical walk.
;;;; The whole block loop is one function whose state lives in local variables
;;;; (MACROLET, not closures), so SBCL keeps them in registers.
;;;;
;;;; Errors: corrupt input signals INFLATE-ERROR (and never runs off the end of an
;;;; array — SAFETY stays at 1). The caller (poc/image.lisp) catches it; nothing
;;;; here needs to know it might run inside PID 1.

(defpackage :lol.inflate
  (:use :cl)
  (:export #:octets #:inflate #:zlib-decompress #:gunzip #:decompress
           #:inflate-error #:crc32 #:adler32))

(in-package :lol.inflate)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(define-condition inflate-error (error)
  ((message :initarg :message :reader inflate-error-message))
  (:report (lambda (c s) (format s "inflate: ~a" (inflate-error-message c))))
  (:documentation "The compressed data is corrupt, truncated or unsupported."))

(defun inflate-fail (fmt &rest args)
  (error 'inflate-error :message (apply #'format nil fmt args)))

;;; ===========================================================================
;;; Checksums — ZLIB closes with Adler-32, GZIP with CRC-32 (PNG chunks use CRC-32
;;; too, which is why poc/image.lisp imports it from here)
;;; ===========================================================================

(defparameter *crc-table*
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (k 8)
          (setf c (if (oddp c) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
        (setf (aref table n) c))))
  "The standard CRC-32 table (reflected polynomial #xEDB88320).")

(defun crc32 (bytes &optional (start 0) (end (length bytes)) (crc 0))
  "CRC-32 of BYTES[START,END). CRC continues a previous result (for chunked input)."
  (declare (type octets bytes) (type fixnum start end) (type (unsigned-byte 32) crc)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((c (logxor crc #xFFFFFFFF))
        (table *crc-table*))
    (declare (type (unsigned-byte 32) c) (type (simple-array (unsigned-byte 32) (256)) table))
    (loop for i of-type fixnum from start below end
          do (setf c (logxor (aref table (logand #xFF (logxor c (aref bytes i))))
                             (ash c -8))))
    (logxor c #xFFFFFFFF)))

(defun adler32 (bytes &optional (start 0) (end (length bytes)))
  "Adler-32 of BYTES[START,END): two running sums mod 65521. The modulo is deferred
   for 5552 bytes at a time — the most that cannot overflow 32 bits (zlib's NMAX)."
  (declare (type octets bytes) (type fixnum start end)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((a 1) (b 0) (i start))
    (declare (type (unsigned-byte 32) a b) (type fixnum i))
    (loop while (< i end)
          do (let ((stop (min end (+ i 5552))))
               (declare (type fixnum stop))
               (loop while (< i stop)
                     do (setf a (+ a (aref bytes i))
                              b (+ b a))
                        (incf i))
               (setf a (mod a 65521) b (mod b 65521))))
    (logior (ash b 16) a)))

;;; ===========================================================================
;;; Huffman tables
;;; ===========================================================================
;;;
;;; DEFLATE never sends a Huffman TREE — only each symbol's code LENGTH. Codes are
;;; "canonical": shorter codes first, and within one length in symbol order, each
;;; code is the previous one plus one. So the lengths alone rebuild the code
;;; (RFC 1951 §3.2.2). From them we build two structures:
;;;
;;;   FAST     — 1024 entries indexed by the next 10 stream bits. For every code of
;;;              length L <= 10, all 2^(10-L) indices whose low L bits equal the
;;;              (reversed) code hold  symbol*16 + L.  0 = "longer than 10 bits".
;;;   COUNTS / SYMBOLS — how many codes of each length, and the symbols sorted by
;;;              (length, value). Enough to decode any code bit by bit (the method
;;;              of zlib's reference decoder "puff"): the slow path.

(defconstant +fast-bits+ 10)

(defstruct (huff (:constructor %make-huff))
  (fast (make-array 1024 :element-type '(unsigned-byte 16))
   :type (simple-array (unsigned-byte 16) (1024)))
  (counts (make-array 16 :element-type '(unsigned-byte 16))
   :type (simple-array (unsigned-byte 16) (16)))
  (symbols (make-array 320 :element-type '(unsigned-byte 16))
   :type (simple-array (unsigned-byte 16) (320))))

(defun make-huff () (%make-huff))

(defun build-huff (h lengths start n)
  "Fill huff H from the N code lengths LENGTHS[START, START+N). Rejects an
   over-subscribed set (more codes than bit patterns); an INCOMPLETE set is legal
   (a block may use a single distance code) — its unused patterns stay 0 and decode
   as an error if the stream ever sends one."
  (declare (type huff h) (type (simple-array (unsigned-byte 8) (*)) lengths)
           (type fixnum start n)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((fast (huff-fast h)) (counts (huff-counts h)) (symbols (huff-symbols h))
        (offs (make-array 16 :element-type '(unsigned-byte 16) :initial-element 0))
        (next (make-array 16 :element-type '(unsigned-byte 16) :initial-element 0)))
    (fill fast 0) (fill counts 0)
    (dotimes (i n) (incf (aref counts (aref lengths (+ start i)))))
    (setf (aref counts 0) 0)
    ;; over-subscription check: at each length, the codes still available
    (let ((left 1))
      (declare (type fixnum left))
      (loop for len from 1 to 15
            do (setf left (- (* left 2) (aref counts len)))
               (when (< left 0) (inflate-fail "over-subscribed Huffman code"))))
    ;; symbols sorted by length (canonical order), via per-length offsets
    (loop for len from 1 below 15
          do (setf (aref offs (1+ len)) (+ (aref offs len) (aref counts len))))
    (dotimes (i n)
      (let ((len (aref lengths (+ start i))))
        (when (plusp len)
          (setf (aref symbols (aref offs len)) i)
          (incf (aref offs len)))))
    ;; first canonical code of each length (RFC 1951 §3.2.2 step 2)
    (let ((code 0))
      (declare (type fixnum code))
      (loop for len from 1 to 15
            do (setf code (ash (+ code (aref counts (1- len))) 1))
               (setf (aref next len) code)))
    ;; the fast table: reverse each short code, replicate over the unused high bits
    (dotimes (sym n)
      (let ((len (aref lengths (+ start sym))))
        (when (<= 1 len +fast-bits+)
          (let* ((code (aref next len))
                 (rev 0))
            (declare (type fixnum code rev))
            (incf (aref next len))
            (dotimes (k len) (setf rev (logior (ash rev 1) (logand 1 (ash code (- k))))))
            (loop for idx of-type fixnum from rev below 1024 by (ash 1 len)
                  do (setf (aref fast idx) (logior (ash sym 4) len)))))
        (when (> len +fast-bits+) (incf (aref next len)))))
    h))

(defun huff-slow (h bits)
  "Decode one symbol from BITS (the bit buffer, at least 15 valid bits) the slow,
   canonical way. Returns symbol*16 + length, or -1 if no code matches."
  (declare (type huff h) (type fixnum bits)
           (optimize (speed 3) (safety 1)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((counts (huff-counts h)) (symbols (huff-symbols h))
        (code 0) (first 0) (index 0))
    (declare (type fixnum code first index))
    (loop for len from 1 to 15
          do (setf code (logior code (logand 1 (ash bits (- 1 len)))))
             (let ((count (aref counts len)))
               (when (< (- code first) count)
                 (return-from huff-slow
                   (logior (ash (aref symbols (+ index (- code first))) 4) len)))
               (incf index count)
               (setf first (ash (+ first count) 1)
                     code (ash code 1))))
    -1))

;;; The fixed code of RFC 1951 §3.2.6, built once at load.
(defparameter *fixed-lit*
  (let ((l (make-array 288 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 288
          do (setf (aref l i) (cond ((< i 144) 8) ((< i 256) 9) ((< i 280) 7) (t 8))))
    (build-huff (make-huff) l 0 288)))

(defparameter *fixed-dist*
  (build-huff (make-huff)
              (make-array 30 :element-type '(unsigned-byte 8) :initial-element 5) 0 30))

(defparameter *length-base*
  (coerce '(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163
            195 227 258 0 0)
          '(simple-array (unsigned-byte 16) (31))))
(defparameter *length-extra*
  (coerce '(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0 0 0)
          '(simple-array (unsigned-byte 8) (31))))
(defparameter *dist-base*
  (coerce '(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537 2049
            3073 4097 6145 8193 12289 16385 24577 0 0)
          '(simple-array (unsigned-byte 16) (32))))
(defparameter *dist-extra*
  (coerce '(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13 0 0)
          '(simple-array (unsigned-byte 8) (32))))

;;; The order in which a dynamic block sends the code-length code's own lengths:
;;; most-likely-used first, so trailing zeros can be left off.
(defparameter *clen-order*
  (coerce '(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)
          '(simple-array (unsigned-byte 8) (19))))

;;; ===========================================================================
;;; The inflater
;;; ===========================================================================

(defun inflate (in &key (start 0) (end (length in)) size-hint max-output)
  "Decompress the raw DEFLATE stream in octet vector IN starting at START.
   Returns (values OUTPUT NEXT): OUTPUT a fresh octet vector, NEXT the index of
   the first byte after the stream (where a ZLIB/GZIP trailer starts).
   SIZE-HINT presizes the output; MAX-OUTPUT, when given, makes a stream that
   expands beyond it an error (a guard against decompression bombs)."
  (declare (type octets in) (type fixnum start end)
           (optimize (speed 3) (safety 1) (debug 0))
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((pos start)
        (overrun 0)                     ; zero bytes invented past END
        (bitbuf 0) (bitcnt 0)
        (out (make-array (max 1024 (min (or size-hint 0) (ash 1 30)))
                         :element-type '(unsigned-byte 8)))
        (opos 0)
        (limit (or max-output most-positive-fixnum))
        (lit (make-huff)) (dist (make-huff))
        (lengths (make-array 320 :element-type '(unsigned-byte 8)))
        (length-base *length-base*) (length-extra *length-extra*)
        (dist-base *dist-base*) (dist-extra *dist-extra*))
    (declare (type fixnum pos overrun opos limit) (type (integer 0 64) bitcnt)
             (type (unsigned-byte 62) bitbuf) (type octets out lengths)
             (type (simple-array (unsigned-byte 16) (31)) length-base)
             (type (simple-array (unsigned-byte 8) (31)) length-extra)
             (type (simple-array (unsigned-byte 16) (32)) dist-base)
             (type (simple-array (unsigned-byte 8) (32)) dist-extra))
    (macrolet ((refill ()
                 ;; top the buffer up to more than 48 bits; past END feed zeros and
                 ;; count them, so a truncated stream is detected, not looped on
                 `(when (< bitcnt 32)
                    (loop while (<= bitcnt 48)
                          do (let ((b (if (< pos end)
                                          (aref in pos)
                                          (progn (incf overrun) 0))))
                               (incf pos)
                               (setf bitbuf (logior bitbuf (the (unsigned-byte 62) (ash b bitcnt))))
                               (incf bitcnt 8)))
                    (when (> (* 8 overrun) (+ bitcnt 64))
                      (inflate-fail "truncated stream"))))
               (bits (n)
                 ;; the next N (<= 32) bits, LSB-first
                 `(let ((n ,n))
                    (declare (type (integer 0 32) n))
                    (refill)
                    (prog1 (logand bitbuf (1- (ash 1 n)))
                      (setf bitbuf (ash bitbuf (- n)))
                      (decf bitcnt n))))
               (decode (h)
                 ;; one Huffman symbol: fast table, else the canonical walk
                 `(progn
                    (refill)
                    (let ((e (aref (huff-fast ,h) (logand bitbuf 1023))))
                      (when (zerop e)
                        (setf e (huff-slow ,h (logand bitbuf #x7FFF)))
                        (when (minusp e) (inflate-fail "bad Huffman code")))
                      (let ((len (logand e 15)))
                        (setf bitbuf (ash bitbuf (- len)))
                        (decf bitcnt len))
                      (ash e -4))))
               (ensure (extra)
                 ;; room for EXTRA more output bytes: grow by doubling
                 `(let ((need (+ opos ,extra)))
                    (when (> need limit) (inflate-fail "output exceeds ~d bytes" limit))
                    (when (> need (length out))
                      (let ((new (make-array (max need (* 2 (length out)))
                                             :element-type '(unsigned-byte 8))))
                        (replace new out :end2 opos)
                        (setf out new))))))
      (loop
        (let ((final (bits 1)) (type (bits 2)))
          (case type
            (0                          ; STORED: byte-align, LEN, ~LEN, raw bytes
             (let ((drop (logand bitcnt 7)))
               (setf bitbuf (ash bitbuf (- drop))) (decf bitcnt drop))
             (let ((len (bits 16)) (nlen (bits 16)))
               (unless (= len (logxor nlen #xFFFF)) (inflate-fail "stored length check"))
               ;; give back the whole bytes still buffered, then copy directly
               (decf pos (ash bitcnt -3))
               (setf bitbuf 0 bitcnt 0)
               (when (> overrun 0) (inflate-fail "truncated stream"))
               (when (> (+ pos len) end) (inflate-fail "truncated stored block"))
               (ensure len)
               (replace out in :start1 opos :start2 pos :end2 (+ pos len))
               (incf opos len) (incf pos len)))
            ((1 2)
             (let ((lt *fixed-lit*) (dt *fixed-dist*))
               (declare (type huff lt dt))
               (when (= type 2)
                 ;; DYNAMIC: first the code-length code (19 symbols, 3-bit lengths),
                 ;; then with it the literal/length and distance code lengths,
                 ;; run-length coded by symbols 16 (repeat previous), 17, 18 (zeros)
                 (let ((hlit (+ 257 (bits 5))) (hdist (+ 1 (bits 5))) (hclen (+ 4 (bits 4))))
                   (when (or (> hlit 286) (> hdist 30)) (inflate-fail "bad code counts"))
                   (fill lengths 0)
                   (dotimes (i hclen)
                     (setf (aref lengths (aref *clen-order* i)) (bits 3)))
                   (build-huff dist lengths 0 19) ; DIST is free: borrow it
                   (let ((i 0) (total (+ hlit hdist)))
                     (declare (type fixnum i total))
                     (fill lengths 0)
                     (loop while (< i total)
                           do (let ((sym (decode dist)))
                                (cond ((< sym 16) (setf (aref lengths i) sym) (incf i))
                                      (t
                                       (multiple-value-bind (val rep)
                                           (case sym
                                             (16 (when (zerop i) (inflate-fail "repeat with no previous length"))
                                              (values (aref lengths (1- i)) (+ 3 (bits 2))))
                                             (17 (values 0 (+ 3 (bits 3))))
                                             (t  (values 0 (+ 11 (bits 7)))))
                                         (when (> (+ i rep) total) (inflate-fail "length repeat overflows"))
                                         (fill lengths val :start i :end (+ i rep))
                                         (incf i rep))))))
                     (when (zerop (aref lengths 256)) (inflate-fail "no end-of-block code"))
                     ;; the distance lengths sit right after the literal ones
                     (build-huff lit lengths 0 hlit)
                     (build-huff dist lengths hlit hdist)
                     (setf lt lit dt dist))))
               ;; the symbol loop — where all the time goes
               (loop
                 (let ((sym (decode lt)))
                   (declare (type fixnum sym))
                   (cond ((< sym 256)
                          (when (>= opos (length out)) (ensure 1))
                          (setf (aref out opos) sym)
                          (incf opos))
                         ((= sym 256) (return))
                         (t
                          (let* ((li (- sym 257)))
                            (when (> li 28) (inflate-fail "bad length symbol"))
                            (let* ((len (+ (aref length-base li) (bits (aref length-extra li))))
                                   (di (decode dt)))
                              (declare (type fixnum len di))
                              (when (> di 29) (inflate-fail "bad distance symbol"))
                              (let ((d (+ (aref dist-base di) (bits (aref dist-extra di)))))
                                (declare (type fixnum d))
                                (when (> d opos) (inflate-fail "distance ~d reaches before start" d))
                                (ensure len)
                                (let ((o out) (from (- opos d)))
                                  (declare (type octets o) (type fixnum from))
                                  (if (>= d len)
                                      (replace o o :start1 opos :start2 from :end2 (+ from len))
                                      ;; overlapping copy: must go byte by byte, since
                                      ;; it re-reads bytes it has just written (a run)
                                      (loop for k of-type fixnum from 0 below len
                                            do (setf (aref o (+ opos k)) (aref o (+ from k))))))
                                (incf opos len))))))))))
            (t (inflate-fail "reserved block type 3")))
          (when (= final 1) (return))))
      ;; byte after the stream: drop the partial byte, give back whole buffered bytes
      (let ((next (- pos (ash bitcnt -3))))
        (when (> next end) (inflate-fail "truncated stream"))
        (values (if (= opos (length out)) out (subseq out 0 opos))
                next)))))

;;; ===========================================================================
;;; The wrappers
;;; ===========================================================================

(defun zlib-decompress (in &key (start 0) (end (length in)) size-hint max-output (verify t))
  "Decompress a ZLIB stream (RFC 1950): a 2-byte header (method 8 = deflate, and a
   check that the header is a multiple of 31), the DEFLATE data, then the Adler-32
   of the OUTPUT, big-endian. A preset dictionary (FDICT) is not supported — PNG
   forbids it. Returns the octets."
  (declare (type octets in))
  (when (< (- end start) 6) (inflate-fail "zlib stream too short"))
  (let ((cmf (aref in start)) (flg (aref in (1+ start))))
    (unless (= (logand cmf 15) 8) (inflate-fail "zlib method ~d is not deflate" (logand cmf 15)))
    (unless (zerop (mod (+ (* cmf 256) flg) 31)) (inflate-fail "zlib header check"))
    (when (logbitp 5 flg) (inflate-fail "zlib preset dictionary unsupported"))
    (multiple-value-bind (out next)
        (inflate in :start (+ start 2) :end end :size-hint size-hint :max-output max-output)
      (when verify
        (when (> (+ next 4) end) (inflate-fail "missing Adler-32"))
        (let ((want (logior (ash (aref in next) 24) (ash (aref in (+ next 1)) 16)
                            (ash (aref in (+ next 2)) 8) (aref in (+ next 3)))))
          (unless (= want (adler32 out)) (inflate-fail "Adler-32 mismatch"))))
      out)))

(defun gunzip (in &key max-output (verify t))
  "Decompress a GZIP file (RFC 1952): one or more members, each a 10-byte header
   (magic 1F 8B, method 8, flags), optional extra/name/comment/header-CRC fields,
   the DEFLATE data, then CRC-32 and the size mod 2^32, little-endian. Concatenated
   members (what `cat a.gz b.gz` makes) decompress to the concatenation."
  (declare (type octets in))
  (let ((pos 0) (end (length in)) (parts '()))
    (flet ((u8 () (when (>= pos end) (inflate-fail "truncated gzip header"))
             (prog1 (aref in pos) (incf pos)))
           (le32 (p) (logior (aref in p) (ash (aref in (+ p 1)) 8)
                             (ash (aref in (+ p 2)) 16) (ash (aref in (+ p 3)) 24))))
      (loop
        (unless (and (< (+ pos 1) end) (= (aref in pos) #x1F) (= (aref in (1+ pos)) #x8B))
          (if parts (return) (inflate-fail "not gzip (bad magic)")))
        (incf pos 2)
        (unless (= (u8) 8) (inflate-fail "gzip method is not deflate"))
        (let ((flg (u8)))
          (incf pos 6)                  ; MTIME(4) XFL OS
          (when (logbitp 2 flg)         ; FEXTRA
            (let ((xlen (logior (u8) (ash (u8) 8)))) (incf pos xlen)))
          (when (logbitp 3 flg) (loop until (zerop (u8))))   ; FNAME, zero-terminated
          (when (logbitp 4 flg) (loop until (zerop (u8))))   ; FCOMMENT
          (when (logbitp 1 flg) (incf pos 2))                ; FHCRC
          (when (> pos end) (inflate-fail "truncated gzip header")))
        (multiple-value-bind (out next) (inflate in :start pos :max-output max-output)
          (when (> (+ next 8) end) (inflate-fail "missing gzip trailer"))
          (when verify
            (unless (= (le32 next) (crc32 out)) (inflate-fail "CRC-32 mismatch"))
            (unless (= (le32 (+ next 4)) (logand (length out) #xFFFFFFFF))
              (inflate-fail "gzip size mismatch")))
          (push out parts)
          (setf pos (+ next 8))
          (when (>= pos end) (return)))))
    (if (null (rest parts))
        (first parts)
        (let* ((parts (nreverse parts))
               (r (make-array (reduce #'+ parts :key #'length) :element-type '(unsigned-byte 8)))
               (p 0))
          (dolist (x parts r) (replace r x :start1 p) (incf p (length x)))))))

(defun decompress (in &key max-output)
  "Sniff IN and decompress it: GZIP by its magic, else ZLIB by its header check,
   else raw DEFLATE."
  (declare (type octets in))
  (cond ((and (> (length in) 2) (= (aref in 0) #x1F) (= (aref in 1) #x8B))
         (gunzip in :max-output max-output))
        ((and (> (length in) 2) (= (logand (aref in 0) 15) 8)
              (zerop (mod (+ (* 256 (aref in 0)) (aref in 1)) 31)))
         (zlib-decompress in :max-output max-output))
        (t (values (inflate in :max-output max-output)))))
