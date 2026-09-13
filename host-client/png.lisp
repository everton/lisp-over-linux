;;;; png.lisp — write a PNG, in Common Lisp, with nothing but the standard library.
;;;;
;;;; The build and screenshot scripts capture the guest's screen as a PPM (QEMU's
;;;; screendump format) and need a PNG to put in the docs. That conversion used to be
;;;; a few lines of embedded Python leaning on `zlib`, which made python3 a hard
;;;; dependency of ./build.sh for the sake of one function. This file removes it.
;;;;
;;;; A PNG is four things, and only one of them is real work:
;;;;
;;;;   1. an 8-byte signature
;;;;   2. chunks: length, 4-char type, payload, CRC-32
;;;;   3. the pixel rows, each prefixed by a filter byte (we use 0 = None)
;;;;   4. those rows wrapped in a ZLIB stream — i.e. DEFLATE
;;;;
;;;; DEFLATE is the work. The cheap way out is a "stored" (uncompressed) block, which
;;;; is perfectly legal and about ten lines — and would turn every screenshot in
;;;; media/ from ~20 KB into ~2.7 MB. These are committed files, so that is the wrong
;;;; trade. Instead this implements the real thing at its simplest useful setting:
;;;; LZ77 matching over a 32 KB window, emitted with DEFLATE's FIXED Huffman code
;;;; (RFC 1951 §3.2.6), which is a fixed table both sides already know — so we never
;;;; have to build or transmit a code tree.
;;;;
;;;; MEASURED, on the 1280x720 module-index screenshot: PPM 2 764 816 bytes, this
;;;; encoder 29 316, Python's zlib -9 12 765, ImageMagick 8 353. So we are 94x better
;;;; than storing and 2.3x worse than zlib — about 16 KB per committed screenshot,
;;;; which is a price worth paying to delete a language from the project. The entire
;;;; remaining gap is DYNAMIC Huffman (a per-block code tree, which also has to be
;;;; transmitted); adding it is a known, self-contained ~150 lines if those kilobytes
;;;; ever matter. Row filtering, tried and measured, is NOT the gap: it moved the
;;;; file by a tenth of a percent, because filtering pays only when zeros are cheap
;;;; and under a fixed code they are not.
;;;;
;;;; THE BIT-ORDER TRAP, stated once because it is the only genuinely confusing part
;;;; of DEFLATE: the stream is packed LSB-first — bit 0 of a byte is filled first —
;;;; but Huffman CODES are written MSB-first, most significant bit of the code first.
;;;; Everything else (the block header, the extra bits after a length or distance) is
;;;; LSB-first like the rest. Get this backwards and you produce a file that looks
;;;; right and no decoder will read. Hence two separate writers below, PUT-BITS and
;;;; PUT-CODE, rather than one with a flag.
;;;;
;;;; Not shipped in the image: this is a host tool, like everything in host-client/.

(defpackage :lol.png
  (:use :cl)
  (:export #:write-png #:read-ppm #:ppm->png #:crc32 #:adler32 #:deflate))

(in-package :lol.png)

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
  "CRC-32 of BYTES, as PNG chunks require."
  (declare (type (array (unsigned-byte 8) (*)) bytes))
  (let ((c #xFFFFFFFF))
    (declare (type (unsigned-byte 32) c))
    (loop for i from start below end
          do (setf c (logxor (aref *crc-table* (logand #xFF (logxor c (aref bytes i))))
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
  (acc 0 :type (unsigned-byte 8))
  (nbits 0 :type fixnum))

(defun put-bits (bs value n)
  "Write the low N bits of VALUE, least significant first — the packing order for
   everything in DEFLATE except a Huffman code."
  (declare (type fixnum n) (type (unsigned-byte 32) value))
  (dotimes (i n)
    (setf (bs-acc bs) (logior (bs-acc bs) (ash (logand 1 (ash value (- i))) (bs-nbits bs))))
    (incf (bs-nbits bs))
    (when (= 8 (bs-nbits bs))
      (vector-push-extend (bs-acc bs) (bs-bytes bs))
      (setf (bs-acc bs) 0 (bs-nbits bs) 0))))

(defun put-code (bs code n)
  "Write an N-bit Huffman CODE, MOST significant bit first. See the header."
  (declare (type fixnum n) (type (unsigned-byte 32) code))
  (loop for i from (1- n) downto 0
        do (put-bits bs (logand 1 (ash code (- i))) 1)))

(defun bs-finish (bs)
  "Flush any partial byte and return the bytes."
  (when (plusp (bs-nbits bs))
    (vector-push-extend (bs-acc bs) (bs-bytes bs))
    (setf (bs-acc bs) 0 (bs-nbits bs) 0))
  (bs-bytes bs))

;;; ---- DEFLATE's fixed Huffman code (RFC 1951 §3.2.6) ----------------------

(defun put-literal (bs value)
  "Emit a literal byte or the end-of-block symbol under the fixed code."
  (cond ((< value 144)  (put-code bs (+ #x30 value) 8))          ; 0-143   -> 8 bits
        ((< value 256)  (put-code bs (+ #x190 (- value 144)) 9)) ; 144-255 -> 9 bits
        ((< value 280)  (put-code bs (- value 256) 7))           ; 256-279 -> 7 bits
        (t              (put-code bs (+ #xC0 (- value 280)) 8))))

(defparameter *length-base*
  #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258))
(defparameter *length-extra*
  #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
(defparameter *dist-base*
  #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769
    1025 1537 2049 3073 4097 6145 8193 12289 16385 24577))
(defparameter *dist-extra*
  #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))

(defun length-code (len)
  (loop for i from 28 downto 0
        when (>= len (aref *length-base* i)) do (return i)))

(defun dist-code (dist)
  (loop for i from 29 downto 0
        when (>= dist (aref *dist-base* i)) do (return i)))

(defun put-match (bs len dist)
  "Emit a back-reference: <length symbol><extra><distance symbol><extra>."
  (let ((lc (length-code len)) (dc (dist-code dist)))
    (put-literal bs (+ 257 lc))
    (put-bits bs (- len (aref *length-base* lc)) (aref *length-extra* lc))
    ;; distance codes are 5 fixed bits, and like every Huffman code, MSB first
    (put-code bs dc 5)
    (put-bits bs (- dist (aref *dist-base* dc)) (aref *dist-extra* dc))))

;;; ---- LZ77 ----------------------------------------------------------------

(defconstant +window+ 32768 "How far back a match may reach (DEFLATE's limit).")
(defconstant +max-match+ 258)
(defconstant +min-match+ 3)
(defconstant +hash-bits+ 15)
(defconstant +hash-size+ (ash 1 +hash-bits+))
(defparameter *max-chain* 64
  "How many candidates to try per position. The whole speed/ratio dial: 64 is plenty
   for screenshots, which match early and often.")

(defun deflate (input)
  "INPUT (a byte vector) -> a complete ZLIB stream: 2-byte header, one fixed-Huffman
   DEFLATE block, Adler-32. This is what a PNG's IDAT chunk contains."
  (declare (type (array (unsigned-byte 8) (*)) input))
  (let* ((n (length input))
         (bs (make-bitstream))
         (head (make-array +hash-size+ :element-type 'fixnum :initial-element -1))
         (prev (make-array (max 1 n) :element-type 'fixnum :initial-element -1)))
    (flet ((hash-at (i)
             ;; a 3-byte rolling hash; DEFLATE's minimum match is 3
             (if (< (+ i 2) n)
                 (logand (logxor (ash (aref input i) 10)
                                 (ash (aref input (+ i 1)) 5)
                                 (aref input (+ i 2)))
                         (1- +hash-size+))
                 0)))
      (put-bits bs 1 1)                    ; BFINAL = 1: one block, and it is the last
      (put-bits bs 1 2)                    ; BTYPE  = 01: fixed Huffman
      (let ((i 0))
        (loop while (< i n)
              do (let ((best-len 0) (best-dist 0))
                   (when (< (+ i 2) n)
                     (let ((h (hash-at i)) (chain 0))
                       (loop for cand = (aref head h) then (aref prev cand)
                             while (and (>= cand 0)
                                        (< chain *max-chain*)
                                        (<= (- i cand) +window+))
                             do (incf chain)
                                (let ((len 0)
                                      (limit (min +max-match+ (- n i))))
                                  (loop while (and (< len limit)
                                                   (= (aref input (+ cand len))
                                                      (aref input (+ i len))))
                                        do (incf len))
                                  (when (> len best-len)
                                    (setf best-len len best-dist (- i cand))
                                    (when (>= len limit) (return)))))))
                   ;; record this position in the chain before moving on
                   (when (< (+ i 2) n)
                     (let ((h (hash-at i)))
                       (setf (aref prev i) (aref head h)
                             (aref head h) i)))
                   (cond
                     ((>= best-len +min-match+)
                      (put-match bs best-len best-dist)
                      ;; insert the positions the match covers, so later matches can
                      ;; still find them, then skip past
                      (loop for k from (1+ i) below (min n (+ i best-len))
                            do (when (< (+ k 2) n)
                                 (let ((h (hash-at k)))
                                   (setf (aref prev k) (aref head h)
                                         (aref head h) k))))
                      (incf i best-len))
                     (t
                      (put-literal bs (aref input i))
                      (incf i))))))
      (put-literal bs 256)                 ; end of block
      (let* ((body (bs-finish bs))
             (out (make-array (+ 2 (length body) 4)
                              :element-type '(unsigned-byte 8)))
             (ad (adler32 input)))
        (setf (aref out 0) #x78            ; CMF: deflate, 32K window
              (aref out 1) #x01)           ; FLG: no dict, check bits make it %31=0
        (replace out body :start1 2)
        (let ((p (+ 2 (length body))))
          (setf (aref out p)       (ldb (byte 8 24) ad)
                (aref out (+ p 1)) (ldb (byte 8 16) ad)
                (aref out (+ p 2)) (ldb (byte 8 8) ad)
                (aref out (+ p 3)) (ldb (byte 8 0) ad)))
        out))))

;;; ---- the PNG container ---------------------------------------------------

(defun be32 (vec pos value)
  (setf (aref vec pos)       (ldb (byte 8 24) value)
        (aref vec (+ pos 1)) (ldb (byte 8 16) value)
        (aref vec (+ pos 2)) (ldb (byte 8 8) value)
        (aref vec (+ pos 3)) (ldb (byte 8 0) value)))

(defun write-chunk (stream type payload)
  "One PNG chunk: length, 4-char TYPE, payload, CRC-32 over type+payload."
  (let* ((len (length payload))
         (buf (make-array (+ 12 len) :element-type '(unsigned-byte 8))))
    (be32 buf 0 len)
    (dotimes (i 4) (setf (aref buf (+ 4 i)) (char-code (char type i))))
    (replace buf payload :start1 8)
    (be32 buf (+ 8 len) (crc32 buf 4 (+ 8 len)))
    (write-sequence buf stream)))

(defun write-png (path width height rgb)
  "Write RGB (width*height*3 bytes, row-major) to PATH as a truecolour 8-bit PNG."
  (declare (type (array (unsigned-byte 8) (*)) rgb))
  (let* ((stride (* width 3))
         ;; Every row carries a filter byte; 0 = None. Real encoders choose a filter
         ;; per row, and it was worth trying — but measured on a real 1280x720
         ;; screenshot, filter selection moved the file from 29316 to 28983 bytes,
         ;; a tenth of a percent. The reason is instructive: filtering pays by making
         ;; ZEROS common, and a zero is only cheap if the entropy coder gives it a
         ;; short code. We emit the FIXED Huffman code, where every literal costs
         ;; eight or nine bits whatever it is. The whole remaining gap to zlib is
         ;; DYNAMIC Huffman, not filtering — so the filter loop was deleted rather
         ;; than kept as decoration.
         (raw (make-array (* height (1+ stride)) :element-type '(unsigned-byte 8))))
    (dotimes (y height)
      (let ((dst (* y (1+ stride))))
        (setf (aref raw dst) 0)
        (replace raw rgb :start1 (1+ dst) :start2 (* y stride) :end2 (* (1+ y) stride))))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede :if-does-not-exist :create)
      (write-sequence #(137 80 78 71 13 10 26 10) s)          ; the signature
      (let ((ihdr (make-array 13 :element-type '(unsigned-byte 8) :initial-element 0)))
        (be32 ihdr 0 width) (be32 ihdr 4 height)
        (setf (aref ihdr 8) 8                                  ; 8 bits per channel
              (aref ihdr 9) 2)                                 ; colour type 2 = RGB
        (write-chunk s "IHDR" ihdr))
      (write-chunk s "IDAT" (deflate raw))
      (write-chunk s "IEND" (make-array 0 :element-type '(unsigned-byte 8))))
    path))

;;; ---- PPM in (QEMU's screendump format) -----------------------------------

(defun read-ppm (path)
  "Read a binary PPM (P6). Returns (values WIDTH HEIGHT RGB-BYTES)."
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence buf s)
      (unless (and (= (aref buf 0) (char-code #\P)) (= (aref buf 1) (char-code #\6)))
        (error "~a is not a binary PPM (P6)" path))
      ;; the header is three ASCII integers, with whitespace and #-comments allowed
      ;; anywhere between them
      (let ((i 2) (fields '()))
        (flet ((wsp (b) (member b '(32 9 10 13))))
          (loop while (< (length fields) 3)
                do (loop while (wsp (aref buf i)) do (incf i))
                   (if (= (aref buf i) (char-code #\#))
                       (loop until (= (aref buf i) 10) do (incf i))
                       (let ((j i))
                         (loop until (wsp (aref buf j)) do (incf j))
                         (push (parse-integer (map 'string #'code-char (subseq buf i j)))
                               fields)
                         (setf i j)))))
        (incf i)                                   ; the single whitespace byte after maxval
        (destructuring-bind (maxval height width) fields
          (declare (ignore maxval))
          (values width height
                  (subseq buf i (+ i (* width height 3)))))))))

(defun ppm->png (ppm png)
  "Convert a P6 PPM file to a PNG file. The whole point of this file."
  (multiple-value-bind (w h rgb) (read-ppm ppm)
    (write-png png w h rgb)
    (values png w h)))
