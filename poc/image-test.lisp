;;;; image-test.lisp — host test for poc/inflate.lisp and poc/image.lisp.
;;;;
;;;;   sbcl --script poc/image-test.lisp          (from anywhere)
;;;;
;;;; Fixtures are GENERATED, not committed: ImageMagick (`magick`, or `convert` for
;;;; IM6) writes one synthetic picture in every format and variant we claim, into a
;;;; temporary directory; we decode each with our library and compare against
;;;; ImageMagick's own decoding of the same file (`magick FILE -depth 8 ppm:-`):
;;;; exact for lossless formats, a small mean error for JPEG (two different IDCTs
;;;; and upsamplers). INFLATE is checked against `gzip`, against ZLIB streams
;;;; (every PNG is one) and against our own encoder in host-client/png.lisp. Then
;;;; every fixture is truncated and bit-flipped to check that DECODE-IMAGE never
;;;; signals. JPEG goes through libstbi.so, built here by poc/build-stbi.sh; the
;;;; test also checks the "library absent" path. Exits non-zero on any failure.



(defvar *here* (directory-namestring *load-truename*))
(load (concatenate 'string *here* "inflate.lisp"))
(load (concatenate 'string *here* "image.lisp"))
(load (concatenate 'string *here* "../host-client/png.lisp"))

(defpackage :lol.image-test (:use :cl :lol.inflate :lol.image))
(in-package :lol.image-test)
(defvar *here* cl-user::*here*)

(defvar *fails* 0)
(defvar *checks* 0)
(defvar *timings* '())

(defun check (ok fmt &rest args)
  (incf *checks*)
  (unless ok
    (incf *fails*)
    (format t "  FAIL: ~?~%" fmt args))
  ok)

(defvar *tmp* (format nil "/tmp/lol-image-test-~d/" (sb-unix:unix-getpid)))
(defun tmp (name &optional (prefix "")) (concatenate 'string prefix *tmp* name))

(defun now () (get-internal-real-time))
(defun ms-since (t0) (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second))

(defun sh (program args &key (output :string))
  "Run PROGRAM; return its stdout (string or octets) and exit code."
  (if (eq output :octets)
      (let* ((file (tmp "stdout.bin"))
             (p (sb-ext:run-program program args :search t :output file
                                                 :if-output-exists :supersede :error nil)))
        (values (read-octets file) (sb-ext:process-exit-code p)))
      (let* ((out (with-output-to-string (o)
                    (setf output (sb-ext:run-program program args :search t :output o :error nil)))))
        (values out (sb-ext:process-exit-code output)))))

(defun read-octets (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s) v)))

(defun write-octets (path v)
  (with-open-file (s path :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
    (write-sequence v s)))

;;; ---- ImageMagick -----------------------------------------------------------

(defvar *magick*
  (cond ((nth-value 1 (ignore-errors (sh "magick" '("-version")))) "magick")
        ((nth-value 1 (ignore-errors (sh "convert" '("-version")))) "convert")))


(defun magick (&rest args)
  (multiple-value-bind (out code) (sh *magick* args)
    (declare (ignore out))
    (or (eql code 0)
        (progn (format t "  [~a ~{~a~^ ~}] exited ~a~%" *magick* args code) nil))))

(defun parse-ppm (d)
  "ImageMagick's P6 output -> (values w h argb-without-alpha)."
  (let ((p 2))
    (flet ((int ()
             (loop while (member (aref d p) '(9 10 13 32)) do (incf p))
             (let ((s p)) (loop while (<= 48 (aref d p) 57) do (incf p))
               (parse-integer (map 'string #'code-char (subseq d s p))))))
      (let* ((w (int)) (h (int)) (maxv (int)) (px (make-array (* w h) :element-type '(unsigned-byte 32))))
        (assert (= maxv 255))
        (incf p)
        (dotimes (i (* w h))
          (setf (aref px i) (logior (ash (aref d p) 16) (ash (aref d (+ p 1)) 8) (aref d (+ p 2))))
          (incf p 3))
        (values w h px)))))

(defun reference (file &rest extra)
  "What ImageMagick says FILE looks like (first frame), as #x00RRGGBB."
  (parse-ppm (sh *magick* (append (list (format nil "~a[0]" file)) extra '("-depth" "8" "ppm:-"))
                 :output :octets)))

(defun compare (name px w h rpx rw rh &key (mae 0) (maxdiff 0))
  "Compare two #x00RRGGBB images; MAE/MAXDIFF are the allowed per-channel errors."
  (unless (check (and (= w rw) (= h rh)) "~a: size ~ax~a, expected ~ax~a" name w h rw rh)
    (return-from compare nil))
  (let ((sum 0) (mx 0) (bad 0))
    (dotimes (i (* w h))
      (let ((a (aref px i)) (b (aref rpx i)) (worst 0))
        (dolist (s '(0 8 16))
          (let ((dd (abs (- (ldb (byte 8 s) a) (ldb (byte 8 s) b)))))
            (incf sum dd) (setf worst (max worst dd))))
        (setf mx (max mx worst))
        (when (> worst maxdiff) (incf bad))))
    (let ((m (/ sum (* 3.0 w h))))
      (check (and (<= m mae) (zerop bad))
             "~a: mean error ~,3f (allowed ~a), max ~d, ~d pixels over ~d" name m mae mx bad maxdiff)
      m)))

;;; ---- INFLATE ---------------------------------------------------------------

(defun random-octets (n &optional (seed 42))
  (let ((v (make-array n :element-type '(unsigned-byte 8)))
        (*random-state* (sb-ext:seed-random-state seed)))
    (dotimes (i n v) (setf (aref v i) (random 256)))))

(defun test-inflate ()
  (format t "~&INFLATE~%")
  (let* ((rand (random-octets 2000000))
         (text (let ((s (with-output-to-string (o)
                          (dolist (f (directory (concatenate 'string *here* "*.lisp")))
                            (write-string (map 'string #'code-char (read-octets f)) o)))))
                 (map 'octets (lambda (c) (logand 255 (char-code c))) s)))
         (runs (let ((v (make-array 300000 :element-type '(unsigned-byte 8))))
                 (dotimes (i (length v) v) (setf (aref v i) (if (< (mod i 1000) 700) 65 (mod i 7)))))))
    (write-octets (tmp "rand.bin") rand)
    (write-octets (tmp "text.bin") text)
    (write-octets (tmp "runs.bin") runs)
    (write-octets (tmp "empty.bin") (make-array 0 :element-type '(unsigned-byte 8)))
    (dolist (name '("rand.bin" "text.bin" "runs.bin" "empty.bin"))
      (let ((orig (read-octets (tmp name))))
        (dolist (level '("-1" "-6" "-9"))
          (let* ((gz (sh "gzip" (list "-c" "-n" level (tmp name)) :output :octets))
                 (t0 (now))
                 (out (gunzip gz)))
            (check (equalp out orig) "gunzip ~a ~a" name level)
            (when (and (equal level "-9") (> (length orig) 100000))
              (push (list (format nil "gunzip ~a (~d KB)" name (floor (length orig) 1024)) (ms-since t0))
                    *timings*))))))
    ;; two members concatenated, as `cat a.gz b.gz` makes
    (let ((a (sh "gzip" (list "-c" "-n" (tmp "runs.bin")) :output :octets))
          (b (sh "gzip" (list "-c" "-n" (tmp "text.bin")) :output :octets)))
      (check (equalp (gunzip (concatenate 'octets a b))
                     (concatenate 'octets (read-octets (tmp "runs.bin")) (read-octets (tmp "text.bin"))))
             "gunzip of two concatenated members"))
    ;; our own encoder (fixed Huffman, overlapping matches) as a ZLIB source
    (let ((z (lol.png:deflate text)))
      (check (equalp (zlib-decompress z) text) "zlib-decompress of host-client/png.lisp's deflate")
      (check (equalp (decompress z) text) "decompress sniffs zlib"))
    ;; corruption must signal INFLATE-ERROR, never anything worse, never hang
    (let ((gz (sh "gzip" (list "-c" "-n" (tmp "text.bin")) :output :octets))
          (other 0) (caught 0) (silent 0)
          (*random-state* (sb-ext:seed-random-state 7)))
      (dotimes (k 200)
        (let ((v (copy-seq gz)))
          (if (evenp k)
              ;; XOR with a non-zero byte, so the "damage" always changes something
              (let ((i (+ 10 (random (- (length v) 20)))))
                (setf (aref v i) (logxor (aref v i) (1+ (random 255)))))
              (setf v (subseq v 0 (random (length v)))))
          (handler-case (progn (gunzip v) (incf silent)
                               (format t "    accepted: ~:[truncation~;byte flip~]~%" (evenp k)))
            (inflate-error () (incf caught))
            (error (e) (incf other) (when (< other 3) (format t "    other error: ~a~%" e))))))
      (check (zerop other) "corrupt gzip: ~d non-INFLATE-ERROR conditions" other)
      (format t "  200 corrupted gzips: ~d rejected, ~d slipped through the checksums~%" caught silent))))

;;; ---- image fixtures --------------------------------------------------------

(defparameter *fixtures*
  ;; (file  magick-output-args  &key source mae alpha note)
  ;; SOURCE :rgb = an opaque 301x203 picture, :alpha = the same with a gradient
  ;; alpha channel, :big = 2000x1500 for timing. Odd sizes on purpose: they
  ;; exercise every row-padding and partial-block edge.
  '(("rgb8.png"        ("PNG24:"))
    ("rgba8.png"       ("PNG32:") :source :alpha)
    ("pal8.png"        ("-colors" "200" "PNG8:"))
    ("pal4.png"        ("-colors" "16" "-define" "png:bit-depth=4" "-define" "png:color-type=3" "png:"))
    ("pal-trns.png"    ("-colors" "64" "PNG8:") :source :alpha)
    ("gray8.png"       ("-colorspace" "gray" "-define" "png:color-type=0" "-depth" "8" "png:"))
    ("gray1.png"       ("-colorspace" "gray" "-threshold" "50%" "-define" "png:color-type=0" "-define" "png:bit-depth=1" "png:"))
    ("gray2.png"       ("-colorspace" "gray" "-define" "png:color-type=0" "-define" "png:bit-depth=2" "png:"))
    ("gray4.png"       ("-colorspace" "gray" "-define" "png:color-type=0" "-define" "png:bit-depth=4" "png:"))
    ;; 16 -> 8 bits: we round (as libpng's strip_16 does); IM truncates gray
    ("gray16.png"      ("-colorspace" "gray" "-define" "png:color-type=0" "-depth" "16" "png:") :maxdiff 1)
    ("graya8.png"      ("-colorspace" "gray" "-define" "png:color-type=4" "png:") :source :alpha)
    ("rgb16.png"       ("-depth" "16" "PNG48:"))
    ("rgba16.png"      ("-depth" "16" "PNG64:") :source :alpha)
    ("adam7-rgb.png"   ("-interlace" "PNG" "PNG24:"))
    ("adam7-rgba16.png" ("-interlace" "PNG" "-depth" "16" "PNG64:") :source :alpha)
    ("adam7-pal.png"   ("-interlace" "PNG" "-colors" "16" "PNG8:"))
    ("adam7-gray1.png" ("-interlace" "PNG" "-colorspace" "gray" "-threshold" "50%" "-define" "png:color-type=0" "-define" "png:bit-depth=1" "png:"))
    ("filter0.png"     ("-quality" "90" "PNG24:"))    ; IM quality x0: filter None
    ("filter4.png"     ("-quality" "94" "PNG24:"))    ; Paeth only
    ("rgb-trns.png"    ("-transparent" "#000000" "PNG24:") :source :key)
    ("big.png"         ("PNG24:") :source :big :time t)
    ("base444.jpg"     ("-quality" "92" "-sampling-factor" "1x1" "jpg:") :mae 1.5)
    ("base420.jpg"     ("-quality" "85" "-sampling-factor" "2x2" "jpg:") :mae 1.5)
    ("base422.jpg"     ("-quality" "85" "-sampling-factor" "2x1" "jpg:") :mae 1.5)
    ("gray.jpg"        ("-colorspace" "gray" "-quality" "85" "jpg:") :mae 1.5)
    ("restart.jpg"     ("-define" "jpeg:restart-interval=3" "-quality" "85" "jpg:") :mae 1.5)
    ("prog420.jpg"     ("-interlace" "JPEG" "-quality" "85" "-sampling-factor" "2x2" "jpg:") :mae 1.5)
    ("prog444.jpg"     ("-interlace" "JPEG" "-quality" "92" "-sampling-factor" "1x1" "jpg:") :mae 1.5)
    ("orient6.jpg"     ("-quality" "90" "jpg:") :mae 1.5 :orient 6)
    ("orient3.jpg"     ("-quality" "90" "jpg:") :mae 1.5 :orient 3)
    ("orient8.jpg"     ("-quality" "90" "jpg:") :mae 1.5 :orient 8)
    ("orient2.jpg"     ("-quality" "90" "jpg:") :mae 1.5 :orient 2)
    ("orient4.jpg"     ("-quality" "90" "jpg:") :mae 1.5 :orient 4)
    ("orient5.jpg"     ("-quality" "90" "jpg:") :mae 1.5 :orient 5)
    ("orient7.jpg"     ("-quality" "90" "jpg:") :mae 1.5 :orient 7)
    ("big-prog.jpg"    ("-interlace" "JPEG" "-quality" "88" "jpg:") :source :big :mae 1.5 :time t)
    ("big-base.jpg"    ("-quality" "88" "jpg:") :source :big :mae 1.5 :time t)
    ("plain.gif"       ("gif:"))
    ("interlaced.gif"  ("-interlace" "GIF" "gif:"))
    ("transp.gif"      ("gif:") :source :alpha)
    ("rgb24.bmp"       ("BMP3:"))
    ("pal8.bmp"        ("-colors" "200" "-type" "palette" "BMP3:"))
    ("pal4.bmp"        ("-colors" "12" "-type" "palette" "BMP3:"))
    ("mono.bmp"        ("-threshold" "50%" "-type" "bilevel" "BMP3:"))
    ("rle8.bmp"        ("-colors" "200" "-type" "palette" "-compress" "RLE" "BMP3:"))
    ("rgba32.bmp"      ("BMP:") :source :alpha)
    ;; n-bit channels: we scale exactly (4-bit 15 -> 255, i.e. x17); ImageMagick
    ;; shifts (15 -> 240), so allow its error: 15 for 4 bits, 1 for 5/6 bits
    ("rgb565.bmp"      ("-define" "bmp:subtype=RGB565" "BMP:") :maxdiff 1)
    ("argb4444.bmp"    ("-define" "bmp:subtype=ARGB4444" "BMP:") :source :alpha :maxdiff 15)
    ("p6.ppm"          ("ppm:"))
    ("p6-16.ppm"       ("-depth" "16" "ppm:"))
    ("p5.pgm"          ("pgm:"))
    ("p4.pbm"          ("pbm:"))
    ("p3.ppm"          ("-compress" "none" "ppm:"))
    ("p2.pgm"          ("-compress" "none" "pgm:"))
    ("p1.pbm"          ("-compress" "none" "pbm:"))
    ("rgba.pam"        ("pam:") :source :alpha)
    ("rgb.qoi"         ("qoi:"))
    ("rgba.qoi"        ("qoi:") :source :alpha)
    ("rgb.tga"         ("-compress" "none" "tga:"))
    ("rle.tga"         ("-compress" "RLE" "tga:"))
    ("rgba.tga"        ("-compress" "none" "tga:") :source :alpha)
    ("rgba-rle.tga"    ("-compress" "RLE" "tga:") :source :alpha)
    ("pal.tga"         ("-colors" "100" "-type" "palette" "-compress" "none" "tga:"))
    ("gray.tga"        ("-colorspace" "gray" "-type" "grayscale" "tga:"))
    ("multi.ico"       ("-define" "icon:auto-resize=48,32,16" "ico:") :source :icon)
    ("png256.ico"      ("-define" "icon:auto-resize=256,32" "ico:") :source :icon)
    ("bmp32.ico"       ("-define" "icon:auto-resize=64" "-define" "icon:format=bmp" "ico:") :source :icon-alpha)
    ("lossless.webp"   ("-define" "webp:lossless=true" "webp:"))
    ("lossless-a.webp" ("-define" "webp:lossless=true" "webp:") :source :alpha)
    ("lossless-pal.webp" ("-colors" "12" "-define" "webp:lossless=true" "webp:"))
    ("lossless-big.webp" ("-define" "webp:lossless=true" "webp:") :source :big :time t)
    ("lossless-2c.webp" ("-colors" "2" "-define" "webp:lossless=true" "webp:"))
    ("lossless-4c.webp" ("-colors" "4" "-define" "webp:lossless=true" "webp:"))
    ("lossless-m6.webp" ("-define" "webp:lossless=true" "-define" "webp:method=6" "webp:") :source :alpha)
    ("lossless-m0.webp" ("-define" "webp:lossless=true" "-define" "webp:method=0" "webp:"))
    ("lossy.webp"      ("-quality" "80" "webp:") :expect-fail t)
    ("image.psd"       ("psd:"))
    ("cmyk.jpg"        ("-colorspace" "CMYK" "-quality" "92" "jpg:") :mae 3)
    ;; HDR is tone-mapped differently by every tool: check it decodes, not pixels
    ("image.hdr"       ("hdr:") :dims-only t)))

(defun make-sources ()
  "The source pictures: a plasma (smooth gradients + noise, so every filter and
   predictor is exercised) with geometric shapes (hard edges), and its alpha twin."
  (and (magick "-seed" "7" "-size" "301x203" "plasma:fractal"
               "-fill" "#ffffff" "-draw" "circle 150,100 150,40"
               "-fill" "#000000" "-draw" "rectangle 10,10 60,190"
               "-fill" "#2060c0" "-draw" "polygon 200,10 290,190 180,150"
               "-type" "TrueColor" (tmp "src.png" "PNG24:"))
       (magick (tmp "src.png") "(" "-size" "301x203" "gradient:white-black" "-evaluate" "multiply" "1.3" ")"
               "-alpha" "off" "-compose" "copy_opacity" "-composite" (tmp "src-alpha.png" "PNG32:"))
       ;; for tRNS: an RGB picture with pure-black pixels to key out
       (magick (tmp "src.png") (tmp "src-key.png" "PNG24:"))
       (magick "-seed" "3" "-size" "2000x1500" "plasma:fractal" "-blur" "0x1"
               "-fill" "#ffffff" "-draw" "circle 1000,750 1000,300" (tmp "src-big.png" "PNG24:"))
       (magick (tmp "src.png") "-resize" "256x256!" (tmp "src-icon.png" "PNG24:"))
       (magick (tmp "src-alpha.png") "-resize" "256x256!" (tmp "src-icon-alpha.png" "PNG32:"))))

(defun source-path (source)
  (tmp (ecase source
         (:rgb "src.png") (:alpha "src-alpha.png") (:key "src-key.png") (:big "src-big.png")
         (:icon "src-icon.png") (:icon-alpha "src-icon-alpha.png"))))

;;; ImageMagick's -orient only sets an EXIF tag if the file already has an EXIF
;;; profile, which a generated picture does not — so we splice one in: the
;;; smallest APP1 segment that carries just the Orientation tag.
(defun add-exif-orientation (path orientation)
  (let* ((d (read-octets path))
         (tiff (list #x49 #x49 #x2A 0  8 0 0 0            ; "II", 42, IFD0 at offset 8
                     1 0                                  ; one entry
                     #x12 #x01  3 0  1 0 0 0  orientation 0 0 0   ; 0x0112 SHORT x1
                     0 0 0 0))                            ; no next IFD
         (payload (append (map 'list #'char-code "Exif") (list 0 0) tiff))
         (len (+ 2 (length payload)))
         (app1 (append (list #xFF #xE1 (ash len -8) (logand len 255)) payload)))
    (write-octets path (concatenate 'octets (subseq d 0 2) app1 (subseq d 2)))))

(defun test-images ()
  (format t "~&IMAGES (ImageMagick: ~a)~%" *magick*)
  (unless (make-sources)
    (check nil "could not generate source images")
    (return-from test-images nil))
  (dolist (f *fixtures*)
    (destructuring-bind (file args &key (source :rgb) (mae 0) (maxdiff (if (plusp mae) 255 0))
                              time orient expect-fail dims-only)
        f
      (let ((path (tmp file)))
        (cond
          ((not (apply #'magick (source-path source) (append (butlast args)
                                                            (list (concatenate 'string (car (last args)) path)))))
           (format t "  skip ~a: ImageMagick could not write it~%" file))
          (t
           (when orient (add-exif-orientation path orient))
           (let* ((d (read-octets path))
                  (alpha (member source '(:alpha :key :icon-alpha)))
                  (t0 (now)))
             (multiple-value-bind (w h px fmt) (decode-image d :background nil)
               (let ((ms (ms-since t0)))
                 (cond
                   (expect-fail
                    (check (null w) "~a should be refused" file)
                    (format t "  ok   ~18a refused as expected: ~a~%" file h))
                   ((not (check w "~a: decode failed: ~a" file h)))
                   (dims-only
                    (check (and (= w 301) (= h 203)) "~a: size ~ax~a" file w h)
                    (format t "  ok   ~18a ~5a ~4dx~4d ~5d ms  (size only)~%" file fmt w h ms))
                   (t
                    (when time
                      (push (list (format nil "~a ~dx~d (~d KB)" file w h (floor (length d) 1024)) ms)
                            *timings*))
                    (multiple-value-bind (rw rh rpx)
                        (if orient (reference path "-auto-orient") (reference path))
                      (let ((m (compare file px w h rpx rw rh :mae (max mae maxdiff) :maxdiff maxdiff)))
                        (format t "  ok   ~18a ~5a ~4dx~4d ~5d ms~@[  mae ~,3f~]~%"
                                file fmt w h ms (and (plusp mae) m))))
                    ;; with alpha: composite onto a colour and compare with IM's flatten
                    (when alpha
                      (multiple-value-bind (w2 h2 px2) (decode-image d :background #x102030)
                        (multiple-value-bind (rw rh rpx)
                            (parse-ppm (sh *magick* (list (format nil "~a[0]" path) "-background" "#102030"
                                                          "-flatten" "-depth" "8" "ppm:-")
                                           :output :octets))
                          (compare (format nil "~a flattened" file) px2 w2 h2 rpx rw rh
                                   :mae (max mae maxdiff 0.6) :maxdiff (+ 2 (* 2 maxdiff))))))))))))))))
  t)

;;; ---- robustness, the stb-absent path, scaling --------------------------------

(defun test-robustness ()
  (format t "~&ROBUSTNESS: truncations and byte flips of every fixture~%")
  (let ((n 0) (signalled 0) (*random-state* (sb-ext:seed-random-state 11)) (t0 (now)))
    (dolist (f *fixtures*)
      (let ((path (tmp (first f))))
        (when (and (probe-file path) (not (getf (cddr f) :time)))
          (let ((d (read-octets path)))
            (dotimes (k 12)
              (let ((v (copy-seq d)))
                (if (evenp k)
                    (setf v (subseq v 0 (random (length v))))
                    (dotimes (j (1+ (random 8)))
                      (setf (aref v (random (length v))) (random 256))))
                (incf n)
                (handler-case (decode-image v)
                  (serious-condition (e)
                    (incf signalled)
                    (format t "    ~a signalled: ~a~%" (first f) e)))))))))
    (check (zerop signalled) "decode-image signalled on ~d of ~d damaged files" signalled n)
    (format t "  ~d damaged files, ~d signalled (~d ms)~%" n signalled (ms-since t0)))
  (multiple-value-bind (w reason) (decode-image (random-octets 1000))
    (check (and (null w) (stringp reason)) "garbage -> NIL + reason (~a)" reason))
  ;; a PNG header claiming 60000x60000 must be refused before allocating
  (let ((d (read-octets (tmp "rgb8.png"))))
    (setf (subseq d 16 24) #(0 0 #xEA #x60 0 0 #xEA #x60))
    (multiple-value-bind (w reason) (decode-image d)
      (check (and (null w) (search "megapixels" reason)) "pixel bomb refused (~a)" reason)))
  (multiple-value-bind (w reason fmt) (decode-image-file "/nonexistent/x.png")
    (check (and (null w) (stringp reason) (null fmt)) "missing file -> NIL + reason")))

(defun test-stbi-absent ()
  (format t "~&STB ABSENT~%")
  (let ((lol.image::*stbi-state* nil)
        (lol.image:*stbi-paths* (list "/nonexistent/libstbi.so")))
    ;; *stbi-state* is only consulted when the library is not loaded yet; here it
    ;; already is, so simulate by checking the message path directly
    (multiple-value-bind (w reason fmt)
        (let ((lol.image::*stbi-state* "libstbi.so not found (simulated)"))
          (handler-case (lol.image::decode-stbi (read-octets (tmp "base420.jpg")))
            (error (e) (values nil (princ-to-string e) :jpeg))))
      (declare (ignore fmt))
      (format t "  without the library: ~a~%" reason)
      (check (and (null w) (search "libstbi" reason)) "JPEG without libstbi.so -> NIL + reason"))))

(defun test-scale ()
  (format t "~&SCALE-TO-FIT~%")
  (multiple-value-bind (w h px) (decode-image-file (tmp "src-big.png"))
    (let ((t0 (now)))
      (multiple-value-bind (nw nh npx) (scale-to-fit w h px 320 240)
        (push (list (format nil "scale-to-fit ~dx~d -> ~dx~d" w h nw nh) (ms-since t0)) *timings*)
        (check (and (= nw 320) (= nh 240)) "scaled to ~dx~d" nw nh)
        ;; compare with ImageMagick's box filter
        (multiple-value-bind (rw rh rpx) (reference (tmp "src-big.png") "-filter" "box" "-resize" "320x240")
          (format t "  vs ImageMagick -filter box: mae ~,3f~%"
                  (compare "scale-to-fit" npx nw nh rpx rw rh :mae 3.0 :maxdiff 255)))))
    (multiple-value-bind (nw nh npx) (scale-to-fit w h px 4000 4000)
      (check (and (= nw w) (= nh h) (eq npx px)) "no upscale by default"))
    (multiple-value-bind (nw nh) (scale-to-fit 10 5 (make-array 50 :element-type '(unsigned-byte 32)) 100 100 :upscale t)
      (check (and (= nw 100) (= nh 50)) "upscale keeps the aspect"))))

;;; ---- main ------------------------------------------------------------------

(defun main ()
  (sb-ext:run-program "mkdir" (list "-p" *tmp*) :search t)
  (unwind-protect
       (progn
         (test-inflate)
         (cond ((null *magick*) (format t "~&ImageMagick not found: image tests SKIPPED~%"))
               (t
                ;; build libstbi.so into the temp dir and point the loader at it
                (multiple-value-bind (out code)
                    (sh "bash" (list (concatenate 'string *here* "build-stbi.sh") *tmp*))
                  (format t "~&~a" out)
                  (check (zerop code) "build-stbi.sh failed"))
                (setf lol.image:*stbi-paths* (list (tmp "libstbi.so")))
                (check (stbi-available-p) "libstbi.so loads")
                (when (test-images)
                  (test-stbi-absent)
                (test-robustness)
                  (test-scale))))
         (format t "~&TIMINGS (host, ~a)~%" (lisp-implementation-version))
         (dolist (x (reverse *timings*)) (format t "  ~45a ~6d ms~%" (first x) (second x))))
    (sb-ext:run-program "rm" (list "-rf" *tmp*) :search t))
  (format t "~&~d checks, ~d failed~%" *checks* *fails*)
  (sb-ext:exit :code (if (zerop *fails*) 0 1)))

(main)
