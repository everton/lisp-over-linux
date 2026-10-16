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
;;;; trade. Instead we use the real thing at its simplest useful setting:
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
;;;; The DEFLATE writer itself — LZ77, the fixed code, the bit-order trap, zlib's and
;;;; gzip's envelopes — now lives in poc/deflate.lisp (lol.deflate), because the image
;;;; needs it too: it gzips the previous boot's logs (initramfs/session-log.lisp).
;;;; This file keeps only what is PNG: chunks, the IHDR, the filter bytes, and PPM in.
;;;;
;;;; Not shipped in the image: this is a host tool, like everything in host-client/.

(load (merge-pathnames "../poc/deflate.lisp"
                       (make-pathname :name nil :type nil :defaults *load-truename*)))

(defpackage :lol.png
  (:use :cl)
  (:import-from :lol.deflate #:crc32 #:adler32 #:octets)
  (:export #:write-png #:read-ppm #:ppm->png #:crc32 #:adler32 #:deflate))

(in-package :lol.png)

(defun deflate (input)
  "INPUT (octets) -> a complete ZLIB stream, what a PNG's IDAT chunk contains."
  (lol.deflate:zlib-compress input))

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
