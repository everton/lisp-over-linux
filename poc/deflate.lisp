;;;; deflate.lisp — DEFLATE, zlib and gzip WRITERS, in Common Lisp.
;;;;
;;;; poc/inflate.lisp READS deflate (every PNG, every .gz preview). This is the other
;;;; direction, and it serves two masters:
;;;;
;;;;   - host-client/png.lisp, which wraps it in ZLIB for a PNG's IDAT chunk (the
;;;;     build and screenshot scripts' PPM -> PNG); and
;;;;   - initramfs/session-log.lisp, which gzips the previous boot's logs in the
;;;;     image, so ~/.lol/log stays small without anyone remembering it exists.
;;;;
;;;; It lived inside png.lisp until the image needed it too; the rest of png.lisp is a
;;;; host tool and stays out of the image.
;;;;
;;;; THE WHOLE FORMAT, at the setting we use. DEFLATE (RFC 1951) is LZ77 — "repeat the
;;;; LEN bytes that start DIST bytes back" — whose literals, lengths and distances are
;;;; then written with a Huffman code. We use the FIXED Huffman code (§3.2.6): a table
;;;; both sides already know, so no code tree is built or sent. One block, BFINAL set.
;;;; The two wrappers differ only in their envelope:
;;;;
;;;;   zlib (RFC 1950)  2-byte header, the deflate data, Adler-32 (big-endian)
;;;;   gzip (RFC 1952)  10-byte header (magic 1F 8B, method 8, mtime, OS), the deflate
;;;;                    data, CRC-32 then the input length mod 2^32 (both little-endian)
;;;;
;;;; MEASURED (2026-10-16): on the 1280x720 module-index screenshot, 29 316 bytes
;;;; against zlib -9's 12 765; on a 39 KB network.log, 10.3% of the text (gzip -9: 8.4%),
;;;; in a few milliseconds; random base64 runs at about 10 MB/s. The 29 316 was
;;;; measured with the older, unoptimised copy in png.lisp; the matcher is the same.
;;;; The remaining gap is DYNAMIC Huffman (a per-block tree, sent with the block), a
;;;; known, self-contained ~150 lines if those bytes ever matter.
;;;;
;;;; THE BIT-ORDER TRAP, stated once because it is the only genuinely confusing part
;;;; of DEFLATE: the stream is packed LSB-first — bit 0 of a byte is filled first —
;;;; but Huffman CODES are defined MSB-first, most significant bit of the code first.
;;;; Everything else (the block header, the extra bits after a length or distance) is
;;;; LSB-first like the rest. Get this backwards and you produce a file that looks
;;;; right and no decoder will read. We resolve it ONCE, when the tables are built:
;;;; every fixed code is stored bit-reversed (+LIT-CODES+, +DIST-CODES+), so from then
;;;; on the writer has a single LSB-first PUT-BITS and nothing else to get wrong.
;;;;
;;;; All original code, under this project's MIT licence.

(defpackage :lol.deflate
  (:use :cl)
  (:export #:octets #:crc32 #:adler32
           #:deflate-raw #:zlib-compress #:gzip-compress #:gzip-file))

(in-package :lol.deflate)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

;;; ---- checksums -----------------------------------------------------------

(defparameter *crc-table*
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (k 8)
          (setf c (if (oddp c) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
        (setf (aref table n) c))))
  "The standard CRC-32 table (polynomial 0xEDB88320, reflected).")

(defun crc32 (bytes &optional (start 0) (end (length bytes)))
  "CRC-32 of BYTES[START,END) — what a PNG chunk and a gzip trailer carry."
  (declare (type (array (unsigned-byte 8) (*)) bytes) (type fixnum start end))
  (let ((c #xFFFFFFFF)
        (table *crc-table*))
    (declare (type (unsigned-byte 32) c)
             (type (simple-array (unsigned-byte 32) (256)) table))
    (loop for i of-type fixnum from start below end
          do (setf c (logxor (aref table (logand #xFF (logxor c (aref bytes i))))
                             (ash c -8))))
    (logxor c #xFFFFFFFF)))

(defun adler32 (bytes)
  "Adler-32 of BYTES — the checksum that closes a ZLIB stream."
  (declare (type (array (unsigned-byte 8) (*)) bytes))
  (let ((a 1) (b 0))
    (declare (type (unsigned-byte 32) a b))
    (loop for x across bytes
          do (setf a (mod (+ a x) 65521)
                   b (mod (+ b a) 65521)))
    (logior (ash b 16) a)))

;;; ---- a bit stream, LSB-first ---------------------------------------------

(defstruct (bitstream (:conc-name bs-))
  (bytes (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (acc 0 :type (unsigned-byte 32))      ; bits not yet a whole byte, low bits first
  (nbits 0 :type (integer 0 32)))

(declaim (inline put-bits))
(defun put-bits (bs value n)
  "Append the low N bits of VALUE (N <= 16), least significant first. The ONE writer:
   Huffman codes come pre-reversed from the tables, see the header."
  (declare (type (unsigned-byte 16) value) (type (integer 0 16) n))
  (let ((acc (logior (bs-acc bs) (the (unsigned-byte 32) (ash value (bs-nbits bs)))))
        (nbits (+ (bs-nbits bs) n)))
    (declare (type (unsigned-byte 32) acc) (type (integer 0 32) nbits))
    (loop while (>= nbits 8)
          do (vector-push-extend (logand acc #xFF) (bs-bytes bs))
             (setf acc (ash acc -8))
             (decf nbits 8))
    (setf (bs-acc bs) acc (bs-nbits bs) nbits)))

(defun bs-finish (bs)
  "Flush any partial byte (zero-padded) and return the bytes."
  (when (plusp (bs-nbits bs))
    (vector-push-extend (logand (bs-acc bs) #xFF) (bs-bytes bs))
    (setf (bs-acc bs) 0 (bs-nbits bs) 0))
  (bs-bytes bs))

;;; ---- DEFLATE's fixed Huffman code (RFC 1951 §3.2.6) ----------------------

(defun reverse-bits (code n)
  (let ((r 0))
    (dotimes (i n r)
      (setf r (logior (ash r 1) (logand 1 (ash code (- i))))))))

(defun fixed-literal-code (value)
  "The fixed code for literal/length symbol VALUE, as (values CODE NBITS), MSB-first."
  (cond ((< value 144) (values (+ #x30 value) 8))          ; 0-143   -> 8 bits
        ((< value 256) (values (+ #x190 (- value 144)) 9)) ; 144-255 -> 9 bits
        ((< value 280) (values (- value 256) 7))           ; 256-279 -> 7 bits
        (t             (values (+ #xC0 (- value 280)) 8)))) ; 280-287 -> 8 bits

(defparameter +lit-codes+
  (let ((v (make-array 288 :element-type '(unsigned-byte 16))))
    (dotimes (s 288 v)
      (multiple-value-bind (code n) (fixed-literal-code s)
        (setf (aref v s) (reverse-bits code n)))))
  "Literal/length symbol -> its fixed code, BIT-REVERSED so PUT-BITS can write it.")
(defparameter +lit-lens+
  (let ((v (make-array 288 :element-type '(unsigned-byte 8))))
    (dotimes (s 288 v) (setf (aref v s) (nth-value 1 (fixed-literal-code s))))))
(defparameter +dist-codes+
  (let ((v (make-array 30 :element-type '(unsigned-byte 16))))
    (dotimes (d 30 v) (setf (aref v d) (reverse-bits d 5))))
  "Distance symbol -> its 5-bit fixed code, bit-reversed.")

(defparameter *length-base*
  (coerce #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258)
          '(simple-array fixnum (*))))
(defparameter *length-extra*
  (coerce #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0)
          '(simple-array fixnum (*))))
(defparameter *dist-base*
  (coerce #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769
            1025 1537 2049 3073 4097 6145 8193 12289 16385 24577)
          '(simple-array fixnum (*))))
(defparameter *dist-extra*
  (coerce #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13)
          '(simple-array fixnum (*))))

(defparameter +length-index+
  (let ((v (make-array 259 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for len from 3 to 258
          do (setf (aref v len)
                   (loop for i from 28 downto 0
                         when (>= len (aref *length-base* i)) return i)))
    v)
  "Match length (3..258) -> index of its length code (symbol 257 + index).")

(defun dist-index (dist)
  (declare (type fixnum dist))
  (let ((base *dist-base*))
    (loop for i of-type fixnum from 29 downto 0
          when (>= dist (aref base i)) return i)))

(declaim (inline put-symbol))
(defun put-symbol (bs s)
  (put-bits bs (aref +lit-codes+ s) (aref +lit-lens+ s)))

(defun put-match (bs len dist)
  "Emit a back-reference: <length symbol><extra><distance symbol><extra>."
  (declare (type fixnum len dist))
  (let ((lc (aref +length-index+ len)) (dc (dist-index dist)))
    (put-symbol bs (+ 257 lc))
    (put-bits bs (- len (aref *length-base* lc)) (aref *length-extra* lc))
    (put-bits bs (aref +dist-codes+ dc) 5)
    (put-bits bs (- dist (aref *dist-base* dc)) (aref *dist-extra* dc))))

;;; ---- LZ77 ----------------------------------------------------------------

(defconstant +window+ 32768 "How far back a match may reach (DEFLATE's limit).")
(defconstant +max-match+ 258)
(defconstant +min-match+ 3)
(defconstant +hash-size+ (ash 1 15))

(defparameter *max-chain* 64
  "How many earlier candidates to try per position: the whole speed/ratio dial.")

(defun deflate-raw (input)
  "INPUT (octets) -> one fixed-Huffman DEFLATE block, final, as octets: the bare
   RFC 1951 stream, with no envelope. ZLIB-COMPRESS and GZIP-COMPRESS add one.

   The match finder is the textbook one, as in zlib: HEAD maps a 3-byte hash to the
   latest position with that hash, and PREV, a ring the size of the window, chains
   each position to the previous one with the same hash. The ring is why memory is
   fixed (two 32K tables) whatever the input's size. A chain is followed only while
   it goes strictly BACKWARDS and stays inside the window — a ring slot may since have
   been reused by a newer position, and that test is what notices."
  (let* ((input (coerce input 'octets))
         (n (length input))
         (bs (make-bitstream))
         (head (make-array +hash-size+ :element-type 'fixnum :initial-element -1))
         (prev (make-array +window+ :element-type 'fixnum :initial-element -1))
         (max-chain *max-chain*))
    (declare (type octets input) (type fixnum n max-chain)
             (type (simple-array fixnum (*)) head prev))
    (flet ((hash-at (i)
             (declare (type fixnum i))
             ;; a 3-byte hash; DEFLATE's minimum match is 3
             (logand (logxor (ash (aref input i) 10)
                             (ash (aref input (+ i 1)) 5)
                             (aref input (+ i 2)))
                     (1- +hash-size+))))
      (declare (inline hash-at))
      (flet ((insert (i)
               (declare (type fixnum i))
               (when (< (+ i 2) n)
                 (let ((h (hash-at i)))
                   (setf (aref prev (logand i (1- +window+))) (aref head h)
                         (aref head h) i)))))
        (declare (inline insert))
        (put-bits bs 1 1)                  ; BFINAL = 1: one block, and it is the last
        (put-bits bs 1 2)                  ; BTYPE  = 01: fixed Huffman
        (let ((i 0))
          (declare (type fixnum i))
          (loop while (< i n)
                do (let ((best-len 0) (best-dist 0))
                     (declare (type fixnum best-len best-dist))
                     (when (< (+ i 2) n)
                       (let ((limit (min +max-match+ (- n i)))
                             (chain 0)
                             (cand (aref head (hash-at i))))
                         (declare (type fixnum limit chain cand))
                         (loop while (and (>= cand 0) (< cand i)
                                          (<= (- i cand) +window+)
                                          (< chain max-chain))
                               do (incf chain)
                                  (let ((len 0))
                                    (declare (type fixnum len))
                                    (loop while (and (< len limit)
                                                     (= (aref input (+ cand len))
                                                        (aref input (+ i len))))
                                          do (incf len))
                                    (when (> len best-len)
                                      (setf best-len len best-dist (- i cand))
                                      (when (>= len limit) (return))))
                                  (let ((next (aref prev (logand cand (1- +window+)))))
                                    (if (< next cand) (setf cand next) (return))))))
                     (cond
                       ((>= best-len +min-match+)
                        (put-match bs best-len best-dist)
                        ;; every position the match covers goes into the chains too,
                        ;; so later matches can still find them
                        (loop for k of-type fixnum from i below (+ i best-len)
                              do (insert k))
                        (incf i best-len))
                       (t
                        (insert i)
                        (put-symbol bs (aref input i))
                        (incf i))))))
        (put-symbol bs 256)                ; end of block
        (coerce (bs-finish bs) 'octets)))))

;;; ---- the envelopes -------------------------------------------------------

(defun zlib-compress (input)
  "INPUT (octets) -> a complete ZLIB stream: what a PNG's IDAT chunk contains."
  (let* ((body (deflate-raw input))
         (out (make-array (+ 2 (length body) 4) :element-type '(unsigned-byte 8)))
         (ad (adler32 input)))
    (setf (aref out 0) #x78               ; CMF: deflate, 32K window
          (aref out 1) #x01)              ; FLG: no dict; check bits make CMF*256+FLG = 0 mod 31
    (replace out body :start1 2)
    (let ((p (+ 2 (length body))))
      (loop for k from 0 below 4
            do (setf (aref out (+ p k)) (ldb (byte 8 (- 24 (* 8 k))) ad))))
    out))

(defun gzip-compress (input &key (mtime 0))
  "INPUT (octets) -> a complete gzip member (RFC 1952), what `gzip -d` reads. MTIME
   is the original file's modification time as a Unix time (0 = unknown)."
  (let* ((input (coerce input 'octets))
         (body (deflate-raw input))
         (out (make-array (+ 10 (length body) 8) :element-type '(unsigned-byte 8))))
    (flet ((le32 (pos value)
             (dotimes (k 4) (setf (aref out (+ pos k)) (ldb (byte 8 (* 8 k)) value)))))
      (setf (aref out 0) #x1F (aref out 1) #x8B   ; the magic
            (aref out 2) 8                         ; CM = deflate
            (aref out 3) 0)                        ; FLG: no name, no comment, no extra
      (le32 4 (ldb (byte 32 0) mtime))
      (setf (aref out 8) 0                         ; XFL
            (aref out 9) 3)                        ; OS = Unix
      (replace out body :start1 10)
      (let ((p (+ 10 (length body))))
        (le32 p (crc32 input))
        (le32 (+ p 4) (ldb (byte 32 0) (length input)))))
    out))

(defun gzip-file (in out &key (mtime 0))
  "Compress the file IN to the file OUT (gzip). Writes OUT.tmp and renames it, so a
   crash mid-way never leaves a truncated OUT that looks finished. Returns OUT."
  (let* ((bytes (with-open-file (s in :element-type '(unsigned-byte 8))
                  (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
                    (read-sequence b s)
                    b)))
         (gz (gzip-compress bytes :mtime mtime))
         (tmp (concatenate 'string (namestring out) ".tmp")))
    (with-open-file (s tmp :direction :output :element-type '(unsigned-byte 8)
                           :if-exists :supersede :if-does-not-exist :create)
      (write-sequence gz s))
    (rename-file tmp out)
    out))
