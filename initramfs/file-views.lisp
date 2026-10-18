;;;; file-views.lisp — the file system as SUBJECTS: present, preview, commands-for.
;;;;
;;;; files.lisp turns the disk into plain data; this file turns that data into the
;;;; browser's own currency — VIEWs of ITEMs — so a folder is not a separate app
;;;; bolted onto the browser but one more kind of thing you can drill into, exactly
;;;; like a package or a class (doc/file-manager.org, "Why a subject and not an
;;;; app"). Everything here is still PURE in the present.lisp sense: subject in, view
;;;; out; it reads the disk but never writes it, never prompts and never draws. The
;;;; keys, prompts and the operations that change things are poc/file-manager.lisp.
;;;;
;;;; Four subjects:
;;;;   SUBJ-DIR   a folder: its listing, sorted, filtered, hidden files hidden
;;;;   SUBJ-FILE  a file: drilled, an EDITOR for text; previewed, a look at it
;;;;   SUBJ-FIND  the stored result of a recursive find (the walk ran once, bounded)
;;;;   SUBJ-FILE-INFO  everything lstat knows, as a table
;;;;
;;;; And the previews — "almost everything that could have a preview has one": text
;;;; (Lisp-coloured when it is Lisp), images (through lol.image, when that module is
;;;; loaded), gzip (decompressed through lol.inflate), tar / .tar.gz / zip (member
;;;; lists), ELF (a header summary), everything else a hex dump with a file(1)-style
;;;; name for what it is. Every read is CAPPED (see the +…-CAP+ parameters): a preview
;;;; of a 4 GB file is a 64 KB read, never the file.
;;;;
;;;; Loaded via load-recording after files.lisp and before shell.lisp.

;;; ---- the subjects ------------------------------------------------------------

(defstruct (subj-dir (:constructor subj-dir (path &key select filter)))
  "A folder. PATH is absolute and normalised. The rest is per-pane state the listing
   reads: SELECT the name to open focused on, FILTER the live / query, MARKS the
   names marked for a bulk operation. They live on the subject because the subject
   is what the pane keeps across a re-present."
  path select filter (marks '()))

(defstruct (subj-file (:constructor subj-file (path))) path)
(defstruct (subj-file-info (:constructor subj-file-info (path))) path)
(defstruct (subj-find (:constructor subj-find (root query results info)))
  "A finished recursive find: ROOT and QUERY as asked, RESULTS the FENTRYs it found,
   INFO the walk's statistics (FIND-FILES). The walk runs ONCE, when you ask, not on
   every re-present — PRESENT must stay cheap, and a bounded walk is still seconds."
  root query results info)

;;; ---- the user's listing preferences ------------------------------------------
;;; Sort order and hidden files are PREFERENCES, not per-folder state: walk into a
;;; sub-folder and it is sorted the way you just asked for (ranger and Finder both
;;; behave so). The live filter, on the other hand, belongs to one folder.

(defvar *file-sort* :name "How folders sort: one of +SORT-KEYS+ (files.lisp).")
(defvar *file-sort-reverse* nil "Flip *FILE-SORT*'s natural order.")
(defvar *show-hidden* nil
  "Show dot files? NIL by default, as ls(1) without -a — a home folder is mostly
   configuration you did not come to look at.")

(defparameter +preview-text-bytes+ 65536 "A text preview reads at most this much.")
(defparameter +preview-text-lines+ 200 "... and shows at most this many lines.")
(defparameter +edit-cap+ (* 2 1024 1024)
  "Text files up to this size open EDITABLE. Larger ones open as a read-only head:
   the editor rebuilds its line vector on each edit, fine for a source file and not
   for a 200 MB log.")
(defparameter +hex-bytes+ 4096 "A hex dump shows at most this many bytes (1 KB in a preview).")
(defparameter +image-preview-cap+ (* 8 1024 1024) "Images up to this size preview.")
(defparameter +image-open-cap+ (* 48 1024 1024) "... and up to this size open.")
(defparameter +gzip-cap+ (* 16 1024 1024)
  "A .gz is decompressed (whole — DEFLATE cannot start mid-stream) only up to this
   many COMPRESSED bytes, and only if its trailer says it inflates to under 64 MB.")
(defparameter +archive-members-cap+ 5000 "At most this many members are listed.")
(defparameter +preview-listing-cap+ 2000 "A folder PREVIEW lstat's at most this many entries.")

;;; ---- optional modules: lol.image and lol.inflate -----------------------------
;;; Both are written in parallel with this file and may not be in the image yet, so
;;; they are reached by NAME at run time rather than by a package prefix the reader
;;; would refuse. When one is missing, the preview degrades to what we can say
;;; without it (the format and the dimensions) and says so.

(defun optional-fn (package name)
  "The function PACKAGE::NAME if that package is loaded and it is fbound, else NIL."
  (let* ((p (find-package package))
         (s (and p (find-symbol name p))))
    (and s (fboundp s) (symbol-function s))))

;;; ---- one entry as a row --------------------------------------------------------

(defun entry-kind (e)
  "The concept keyword a row is tinted by (poc/widgets.lisp *CONCEPT-COLORS*): a
   folder is a container, gold like a package; a link is an indirection, purple like
   a macro; an executable green; a broken link or a device red; a plain file has
   none, and reads as plain text."
  (cond ((and (eq (fentry-type e) :symlink) (fentry-broken e)) :broken)
        ((eq (fentry-type e) :symlink) :symlink)
        ((eq (fentry-type e) :directory) :directory)
        ((entry-executable-p e) :executable)
        ((member (fentry-type e) '(:fifo :socket :char-device :block-device :unknown)) :special)
        (t nil)))

(defun type-word (type)
  (case type
    (:char-device "char device") (:block-device "block device")
    (t (string-downcase (symbol-name type)))))

(defun entry-detail (e)
  "The right-hand column: size and date for a file, what it is for anything else."
  (let ((date (format-mtime (fentry-mtime e))))
    (case (fentry-type e)
      (:directory (format nil "folder   ~a" date))
      (:symlink                          ; the label already says "-> target"
       (cond ((fentry-broken e) "broken link")
             ((eq (fentry-target-type e) :directory) (format nil "folder   ~a" date))
             (t (format nil "~a   ~a" (human-size (fentry-target-size e)) date))))
      (:file (format nil "~:[~;exec  ~]~a   ~a" (entry-executable-p e)
                     (human-size (fentry-size e)) date))
      (t (type-word (fentry-type e))))))

(defun entry-label (e &optional (name (fentry-name e)))
  "NAME as the row shows it: a folder ends in /, a link shows where it points
   (`name -> target`), and a broken one says so. The arrow is ASCII on purpose —
   the font has 256 glyphs (doc/fonts.org)."
  (case (fentry-type e)
    (:directory (concatenate 'string name "/"))
    (:symlink (format nil "~a~:[~;/~] -> ~a~:[~; (broken)~]"
                      name (eq (fentry-target-type e) :directory)
                      (abbreviate-home (or (fentry-target e) "?")) (fentry-broken e)))
    (t name)))

(defun entry-subject (e)
  "What drilling the row opens: a folder (or a link to one) is walked INTO; a device,
   FIFO or socket only ever shows its information, because opening one to read it
   can block forever; everything else is a file."
  (cond ((entry-dir-p e) (subj-dir (fentry-path e)))
        ((member (fentry-type e) '(:fifo :socket :char-device :block-device :unknown))
         (subj-file-info (fentry-path e)))
        ((and (eq (fentry-type e) :symlink)
              (member (fentry-target-type e) '(:fifo :socket :char-device :block-device)))
         (subj-file-info (fentry-path e)))
        (t (subj-file (fentry-path e)))))

(defun entry-item (e &key label marked group)
  (make-item :label (format nil "~:[~;* ~]~a" marked (or label (entry-label e)))
             :detail (entry-detail e)
             :kind (entry-kind e)
             :group group
             :subject (entry-subject e)))

;;; ---- a folder ------------------------------------------------------------------

(defun sort-word ()
  (format nil "~(~a~)~:[~; reversed~]"
          (case *file-sort* (:mtime "date") (t *file-sort*)) *file-sort-reverse*))

(defun dir-title (path n &key filtered-from)
  "\"folder ~/proj — 12 items · by name · 112G free\". The part before the dash
   is what a tab shows (SHORT-TITLE), so the path comes first."
  (let ((dot (string (code-char #xB7)))
        (free (ignore-errors (disk-free path))))
    (format nil "folder ~a ~c ~d item~:p~@[ of ~d~] ~a by ~a~a~a"
            (abbreviate-home path) (code-char #x2014) n filtered-from dot (sort-word)
            (if *show-hidden* (format nil " ~a dot files shown" dot) "")
            (if free (format nil " ~a ~a free" dot (human-size free)) ""))))

(defun listing-view (d &key (limit +listing-cap+))
  "The view of folder D: its entries, hidden ones hidden unless *SHOW-HIDDEN*, D's
   live filter applied, sorted by the preferences with folders first, MARKS shown.
   It opens focused on D's SELECT when that name is listed."
  (let ((path (subj-dir-path d)))
    (handler-case
        (multiple-value-bind (all truncated) (list-directory path :limit limit)
          (let* ((visible (filter-entries all :hidden *show-hidden* :query (subj-dir-filter d)))
                 (sorted (sort-entries visible :by *file-sort* :reverse *file-sort-reverse*))
                 (marks (subj-dir-marks d))
                 (items (mapcar (lambda (e)
                                  (entry-item e :marked (member (fentry-name e) marks :test #'string=)
                                                :group (if (entry-dir-p e) "folders" "files")))
                                sorted))
                 (hidden-n (count-if (lambda (e) (hidden-name-p (fentry-name e))) all)))
            (make-view
             :title (dir-title path (length sorted)
                               :filtered-from (and (subj-dir-filter d) (length all)))
             :kind :list :subject d
             :sel (or (and (subj-dir-select d)
                           (position (subj-dir-select d) sorted :key #'fentry-name :test #'string=))
                      0)
             :items (append
                     items
                     (cond
                       ((and (null items) (subj-dir-filter d))
                        (list (make-item :label (format nil "(nothing matches ~s)" (subj-dir-filter d))
                                         :detail "/ to change the filter, Esc in it clears")))
                       ((and (null items) (plusp hidden-n))
                        (list (make-item :label (format nil "(~d hidden file~:p only)" hidden-n)
                                         :detail ". shows them")))
                       ((null items) (list (make-item :label "(empty folder)"))))
                     (when truncated
                       (list (make-item :label (format nil "(listing stopped at ~d entries)" limit)
                                        :detail "a folder this large is not read whole"
                                        :group "note")))))))
      (fs-error (c)
        (make-view :title (format nil "folder ~a" (abbreviate-home path))
                   :kind :list :subject d
                   :items (list (make-item :label (format nil "(cannot read: ~a)"
                                                          (errno-text (fs-error-errno c)))))))
      (error (c)
        (make-view :title (format nil "folder ~a" (abbreviate-home path)) :kind :list :subject d
                   :items (list (make-item :label (format nil "(cannot list: ~a)" c))))))))

(defmethod present ((d subj-dir)) (listing-view d))
(defmethod present-preview ((d subj-dir)) (listing-view d :limit +preview-listing-cap+))

;;; ---- sniffing: what IS this file? ---------------------------------------------
;;; file(1) works from MAGIC NUMBERS: most formats begin with a fixed signature, put
;;; there precisely so a program can tell what it is holding without trusting the
;;; name. These are the ones a laptop's home folder is full of, plus the ones this
;;; project makes (a cpio initramfs, a bzImage, a PSF font).

(defun octets-at-p (octets offset bytes)
  "Do OCTETS hold BYTES (a list of integers or a string) at OFFSET?"
  (let ((bs (if (stringp bytes) (map 'list #'char-code bytes) bytes)))
    (and (<= (+ offset (length bs)) (length octets))
         (loop for b in bs for i from offset always (= (aref octets i) b)))))

(defun u16le (o i) (logior (aref o i) (ash (aref o (1+ i)) 8)))
(defun u32le (o i) (logior (u16le o i) (ash (u16le o (+ i 2)) 16)))
(defun u64le (o i) (logior (u32le o i) (ash (u32le o (+ i 4)) 32)))
(defun u16be (o i) (logior (ash (aref o i) 8) (aref o (1+ i))))
(defun u32be (o i) (logior (ash (u16be o i) 16) (u16be o (+ i 2))))

(defun image-dimensions (octets kind)
  "Width x height read straight out of an image header, for when the decoder is not
   there to ask: PNG keeps them in IHDR at bytes 16-23 (big-endian), GIF at 6-9 and
   BMP at 18-25 (little-endian). NIL for anything else."
  (ignore-errors
    (case kind
      (:png (and (>= (length octets) 24) (list (u32be octets 16) (u32be octets 20))))
      (:gif (and (>= (length octets) 10) (list (u16le octets 6) (u16le octets 8))))
      (:bmp (and (>= (length octets) 26) (list (u32le octets 18) (abs (- (u32le octets 22)
                                                                          (if (logbitp 31 (u32le octets 22))
                                                                              (expt 2 32) 0))))))
      (t nil))))

(defun sniff (octets &optional (name ""))
  "What OCTETS (a file's first bytes) are: (values KIND LABEL). KIND drives the
   preview (:empty :text :image :gzip :tar :zip :elf :binary); LABEL is the
   file(1)-style description shown in the title."
  (let ((ext (file-extension name)))
    (cond
      ((zerop (length octets)) (values :empty "empty"))
      ((octets-at-p octets 0 '(#x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A)) (values :image "PNG image"))
      ((octets-at-p octets 0 '(#xFF #xD8 #xFF)) (values :image "JPEG image"))
      ((octets-at-p octets 0 "GIF8") (values :image "GIF image"))
      ((and (octets-at-p octets 0 "RIFF") (octets-at-p octets 8 "WEBP")) (values :image "WebP image"))
      ((octets-at-p octets 0 "qoif") (values :image "QOI image"))
      ((and (octets-at-p octets 0 "BM") (string= ext "bmp")) (values :image "BMP image"))
      ((and (>= (length octets) 3) (= (aref octets 0) 80) (member (aref octets 1) '(49 50 51 52 53 54))
            (member (aref octets 2) '(9 10 13 32)))
       (values :image (format nil "netpbm P~c image" (code-char (aref octets 1)))))
      ;; formats with no strong signature (TGA has none at all): trusted only when the
      ;; NAME agrees AND lol.image recognises the bytes — a heuristic alone would call
      ;; some object file a picture and lose its hex dump
      ((and (member ext '("tga" "ico" "cur" "psd" "hdr" "pic") :test #'string=)
            (let ((f (optional-fn "LOL.IMAGE" "IMAGE-FORMAT")))
              (and f (ignore-errors (funcall f octets)))))
       (values :image (format nil "~:@(~a~) image" ext)))
      ((octets-at-p octets 0 '(#x1F #x8B)) (values :gzip "gzip compressed data"))
      ((octets-at-p octets 0 '(#x7F #x45 #x4C #x46)) (values :elf "ELF executable"))
      ((octets-at-p octets 257 "ustar") (values :tar "tar archive"))
      ((or (octets-at-p octets 0 '(#x50 #x4B 3 4)) (octets-at-p octets 0 '(#x50 #x4B 5 6)))
       (values :zip (cond ((member ext '("jar") :test #'string=) "Java archive (zip)")
                          ((member ext '("docx" "xlsx" "pptx" "odt" "ods") :test #'string=)
                           "office document (zip)")
                          ((string= ext "epub") "EPUB book (zip)")
                          (t "zip archive"))))
      ((octets-at-p octets 0 "%PDF") (values :binary "PDF document"))
      ((octets-at-p octets 0 "BZh") (values :binary "bzip2 compressed data"))
      ((octets-at-p octets 0 '(#xFD #x37 #x7A #x58 #x5A 0)) (values :binary "xz compressed data"))
      ((octets-at-p octets 0 '(#x28 #xB5 #x2F #xFD)) (values :binary "zstd compressed data"))
      ((octets-at-p octets 0 "SQLite format 3") (values :binary "SQLite 3 database"))
      ((or (octets-at-p octets 0 "070701") (octets-at-p octets 0 "070702"))
       (values :binary "cpio archive (newc) — an initramfs is one"))
      ((octets-at-p octets 0 "!<arch>") (values :binary "ar archive (.deb, .a)"))
      ((and (octets-at-p octets 0 "MZ") (octets-at-p octets #x202 "HdrS"))
       (values :binary "Linux kernel bzImage (an EFI application too)"))
      ((octets-at-p octets 0 "MZ") (values :binary "DOS/PE executable"))
      ((octets-at-p octets 0 '(#x36 #x04)) (values :binary "PSF1 console font"))
      ((octets-at-p octets 0 '(#x72 #xB5 #x4A #x86)) (values :binary "PSF2 console font"))
      ((and (octets-at-p octets 0 "LF") (string= ext "lolf")) (values :binary "lolf bitmap font (poc/bdf2font.lisp)"))
      ((octets-at-p octets 0 '(0 #x61 #x73 #x6D)) (values :binary "WebAssembly module"))
      ((octets-at-p octets 0 '(#xCA #xFE #xBA #xBE)) (values :binary "Java class / Mach-O fat binary"))
      ((octets-at-p octets 0 "OggS") (values :binary "Ogg media"))
      ((octets-at-p octets 0 "fLaC") (values :binary "FLAC audio"))
      ((octets-at-p octets 0 "ID3") (values :binary "MP3 audio"))
      ((and (octets-at-p octets 0 "RIFF") (octets-at-p octets 8 "WAVE")) (values :binary "WAVE audio"))
      ((octets-at-p octets 4 "ftyp") (values :binary "MP4/QuickTime media"))
      ((octets-at-p octets 0 '(#x1A #x45 #xDF #xA3)) (values :binary "Matroska / WebM media"))
      ((octets->text octets :truncated t)
       (values :text (cond ((octets-at-p octets 0 "#!")
                            (let ((nl (position 10 octets)))
                              (format nil "script, ~a"
                                      (or (ignore-errors
                                            (sb-ext:octets-to-string octets :start 2 :end (min (or nl 80) 80)
                                                                            :external-format :utf-8))
                                          "#!"))))
                           ((lisp-file-p name) "Lisp source")
                           (t "UTF-8 text"))))
      (t (values :binary "data")))))

(defun org-file-p (name)
  "Is NAME an Org file? Then it is coloured and folded as Org (poc/org-mode.lisp)."
  (string-equal (file-extension name) "org"))

(defun text-syntax (name)
  "How a text file named NAME is coloured: :lisp, :org, or :plain."
  (cond ((lisp-file-p name) :lisp) ((org-file-p name) :org) (t :plain)))

(defun lisp-file-p (name)
  "Does NAME look like Lisp source, so it gets the Lisp colouring?"
  (member (file-extension name) '("lisp" "lsp" "cl" "asd" "el" "scm" "ss" "rkt" "clj" "fnl")
          :test #'string=))

(defun detab (line)
  "LINE with tabs expanded to 8-column stops — for PREVIEWS only. The font's tab glyph
   is blank and one cell wide, so unexpanded tabs collapse indentation. (The editor
   keeps the real tabs; changing a file's bytes behind your back is not ours to do.)"
  (if (not (find #\Tab line)) line
      (with-output-to-string (s)
        (let ((col 0))
          (loop for c across line do
            (if (char= c #\Tab)
                (loop do (write-char #\Space s) (incf col) until (zerop (mod col 8)))
                (progn (write-char c s) (incf col))))))))

(defun text-head (text max-lines)
  "The first MAX-LINES lines of TEXT, tabs expanded, plus a closing note when cut."
  (let ((lines '()) (start 0) (n 0) (cut nil))
    (loop
      (let ((nl (position #\Newline text :start start)))
        (when (>= n max-lines) (setf cut t) (return))
        (push (detab (subseq text start (or nl (length text)))) lines)
        (incf n)
        (if nl (setf start (1+ nl)) (return))))
    (format nil "~{~a~^~%~}~:[~;~%~%... (preview: first ~d lines)~]" (nreverse lines) cut max-lines)))

(defun hex-dump (octets &key (offset 0) (limit +hex-bytes+))
  "The classic layout: offset, sixteen bytes in hex, then the same bytes as ASCII
   with . for anything unprintable — hexdump -C, which is what you reach for to see
   what a file really is."
  (with-output-to-string (s)
    (loop for row from 0 below (min (length octets) limit) by 16 do
      (format s "~8,'0x  " (+ offset row))
      (loop for i from row below (+ row 16) do
        (if (< i (length octets)) (format s "~2,'0x " (aref octets i)) (write-string "   " s))
        (when (= i (+ row 7)) (write-char #\Space s)))
      (write-string " |" s)
      (loop for i from row below (min (+ row 16) (length octets)) do
        (let ((b (aref octets i))) (write-char (if (<= 32 b 126) (code-char b) #\.) s)))
      (format s "|~%"))
    (when (> (length octets) limit)
      (format s "... (~d more bytes not shown)~%" (- (length octets) limit)))))

;;; ---- ELF: the header says what kind of program this is -----------------------

(defun elf-summary (octets)
  "A file(1)-style reading of an ELF header (the first 64 bytes): class, byte order,
   type, machine, entry point, and — from the PT_INTERP program header, when it lies
   within OCTETS — the dynamic loader it needs, or 'statically linked'. Fields from
   elf(5): e_ident[4] class, [5] data; e_type u16 at 16; e_machine u16 at 18; for
   ELF64 e_entry at 24, e_phoff at 32, e_phentsize at 54, e_phnum at 56."
  (ignore-errors
    (let* ((bits (case (aref octets 4) (1 32) (2 64) (t "?")))
           (le (= (aref octets 5) 1))
           (type (u16le octets 16))
           (machine (u16le octets 18))
           (interp nil))
      (when (and le (eql bits 64))
        (let ((phoff (u64le octets 32)) (phent (u16le octets 54)) (phnum (u16le octets 56)))
          (dotimes (i (min phnum 64))
            (let ((p (+ phoff (* i phent))))
              (when (and (<= (+ p 56) (length octets)) (= (u32le octets p) 3))   ; PT_INTERP
                (let ((off (u64le octets (+ p 8))) (sz (u64le octets (+ p 32))))
                  (when (<= (+ off sz) (length octets))
                    (setf interp (string-right-trim (string (code-char 0))
                                                    (sb-ext:octets-to-string
                                                     octets :start off :end (+ off sz)
                                                            :external-format :latin-1))))))))))
      (format nil "ELF ~a-bit ~:[big~;little~]-endian ~a, ~a~@[, entry #x~x~]~%~a"
              bits le
              (case type (1 "relocatable object") (2 "executable") (3 "shared object / PIE")
                    (4 "core dump") (t (format nil "type ~d" type)))
              (case machine (3 "x86") (62 "x86-64") (40 "ARM") (183 "AArch64") (243 "RISC-V")
                    (8 "MIPS") (20 "PowerPC") (21 "PowerPC64") (t (format nil "machine ~d" machine)))
              (and le (eql bits 64) (u64le octets 24))
              (cond (interp (format nil "dynamically linked, interpreter ~a" interp))
                    ((and le (eql bits 64)) "statically linked (no PT_INTERP)")
                    (t ""))))))

;;; ---- archives: tar, zip, and gzip around either ---------------------------------

(defun octal-field (o start len)
  "A tar header number: ASCII octal, NUL/space padded — or, when the first byte has
   its high bit set, GNU's base-256 big-endian binary for sizes past 8 GB."
  (if (logbitp 7 (aref o start))
      (let ((v (logand (aref o start) #x7F)))
        (loop for i from (1+ start) below (+ start len) do (setf v (+ (* v 256) (aref o i))))
        v)
      (let ((v 0))
        (loop for i from start below (+ start len)
              for c = (aref o i)
              while (<= 48 c 55) do (setf v (+ (* v 8) (- c 48))))
        ;; skip leading spaces
        (if (and (zerop v) (= (aref o start) 32))
            (let ((j (position-if (lambda (c) (<= 48 c 55)) o :start start :end (+ start len))))
              (if j (octal-field o j (- (+ start len) j)) 0))
            v))))

(defun cstring-field (o start len)
  (let ((end (or (position 0 o :start start :end (+ start len)) (+ start len))))
    (sb-ext:octets-to-string o :start start :end end :external-format '(:utf-8 :replacement #\?))))

(defun tar-members (read-block total)
  "The members of a tar archive as plists (:name :size :mode :mtime :type). READ-BLOCK
   is (lambda (offset n) -> octets), so the same walker reads a tar FILE (a seek and a
   512-byte read per member — the contents are never touched) and a decompressed
   .tar.gz in memory. A tar is a chain of 512-byte headers, each followed by its data
   rounded up to 512; two zero blocks end it. GNU long names ('L') arrive as a member
   of their own whose data is the next member's name."
  (let ((out '()) (pos 0) (long-name nil) (n 0))
    (loop
      (when (or (> (+ pos 512) total) (>= n +archive-members-cap+)) (return))
      (let ((h (funcall read-block pos 512)))
        (when (or (< (length h) 512) (every #'zerop h)) (return))
        (let* ((size (octal-field h 124 12))
               (type (code-char (aref h 156)))
               (prefix (if (octets-at-p h 257 "ustar") (cstring-field h 345 155) ""))
               (name (or long-name
                         (if (plusp (length prefix))
                             (concatenate 'string prefix "/" (cstring-field h 0 100))
                             (cstring-field h 0 100))))
               (data-blocks (* 512 (ceiling size 512))))
          (setf long-name nil)
          (case type
            (#\L (setf long-name (string-right-trim (string (code-char 0))
                                                    (cstring-field (funcall read-block (+ pos 512) size) 0 size))))
            ((#\x #\g))                               ; pax headers: metadata, not members
            (t (incf n)
               (push (list :name name :size size :mode (octal-field h 100 8)
                           :mtime (octal-field h 136 12)
                           :type (case type ((#\5) :directory) ((#\2) :symlink) ((#\1) :hardlink)
                                   (t :file))
                           :link (and (member type '(#\1 #\2)) (cstring-field h 157 100)))
                     out)))
          (incf pos (+ 512 data-blocks)))))
    (nreverse out)))

(defun zip-members (path size)
  "The members of a zip archive, from its CENTRAL DIRECTORY — the index a zip keeps at
   its END. Find the end-of-central-directory record (signature PK\\5\\6) in the last
   64 KB + 22 bytes, read where the directory starts and how many entries it has,
   then walk them (signature PK\\1\\2; sizes at 20/24, name length 28, extra 30,
   comment 32, the name at 46). The file's data is never read."
  (let* ((tail-n (min size (+ 65536 22)))
         (tail (read-prefix path tail-n :start (- size tail-n)))
         (eocd (loop for i from (- (length tail) 22) downto 0
                     when (octets-at-p tail i '(#x50 #x4B 5 6)) return i)))
    (unless eocd (return-from zip-members (values nil "no end-of-central-directory record")))
    (let* ((count (u16le tail (+ eocd 10)))
           (cd-size (u32le tail (+ eocd 12)))
           (cd-off (u32le tail (+ eocd 16))))
      (when (or (= cd-off #xFFFFFFFF) (= count #xFFFF))
        (return-from zip-members (values nil "zip64 archive (central directory not parsed)")))
      (let ((cd (read-prefix path (min cd-size (* 8 1024 1024)) :start cd-off))
            (out '()) (p 0))
        (loop repeat (min count +archive-members-cap+)
              while (and (<= (+ p 46) (length cd)) (octets-at-p cd p '(#x50 #x4B 1 2)))
              do (let* ((method (u16le cd (+ p 10)))
                        (csize (u32le cd (+ p 20))) (usize (u32le cd (+ p 24)))
                        (nlen (u16le cd (+ p 28))) (xlen (u16le cd (+ p 30))) (clen (u16le cd (+ p 32)))
                        (name (sb-ext:octets-to-string cd :start (+ p 46) :end (min (length cd) (+ p 46 nlen))
                                                          :external-format '(:utf-8 :replacement #\?))))
                   (push (list :name name :size usize :csize csize
                               :method (case method (0 "stored") (8 "deflated") (t (format nil "method ~d" method)))
                               :type (if (and (plusp (length name)) (char= (char name (1- (length name))) #\/))
                                         :directory :file))
                         out)
                   (incf p (+ 46 nlen xlen clen))))
        (values (nreverse out) nil count)))))

(defun gzip-header-name (octets)
  "The original file name a gzip header may carry (FNAME, flag bit 3), else NIL.
   Header: 1F 8B, method, FLG, 4 bytes mtime, XFL, OS — then FEXTRA if flag bit 2
   (two-byte length + data), then FNAME, NUL-terminated."
  (ignore-errors
    (let ((flg (aref octets 3)) (p 10))
      (when (logbitp 2 flg) (incf p (+ 2 (u16le octets 10))))
      (when (logbitp 3 flg)
        (let ((end (position 0 octets :start p)))
          (sb-ext:octets-to-string octets :start p :end end :external-format :latin-1))))))

(defun member-items (members &key (date t))
  (mapcar (lambda (m)
            (make-item :label (format nil "~a~@[ -> ~a~]" (getf m :name) (getf m :link))
                       :detail (format nil "~a~@[   ~a~]~@[   ~a~]"
                                       (if (eq (getf m :type) :directory) "folder"
                                           (human-size (getf m :size)))
                                       (getf m :method)
                                       (and date (getf m :mtime) (format-mtime (getf m :mtime))))
                       :kind (case (getf m :type) (:directory :directory) (:symlink :symlink) (t nil))))
          members))

(defun archive-view (title subject members &key note)
  (make-view :title title :kind :list :subject subject
             :items (append (or (member-items members)
                                (list (make-item :label "(no members)")))
                            (when note (list (make-item :label note :group "note")))
                            (when (>= (length members) +archive-members-cap+)
                              (list (make-item :label (format nil "(listing stopped at ~d members)"
                                                              +archive-members-cap+)
                                               :group "note"))))))

;;; ---- the file views --------------------------------------------------------------

(defun file-title (word path e &optional label)
  "\"file ~/notes.txt — 4.2K · rw-r--r-- · 2026-10-03 14:02 · UTF-8 text\". A link
   shows itself and its target: \"link ~/x -> ../y — …\"."
  (format nil "~a ~a~@[ -> ~a~] ~c ~a ~c ~a ~c ~a~@[ ~c ~a~]"
          (if (eq (fentry-type e) :symlink) "link" word)
          (abbreviate-home path)
          (and (eq (fentry-type e) :symlink) (fentry-target e))
          (code-char #x2014)
          (human-size (if (eq (fentry-type e) :symlink) (fentry-target-size e) (fentry-size e)))
          (code-char #xB7)
          (subseq (mode-string (if (eq (fentry-type e) :symlink)
                                   (or (ignore-errors (fentry-mode (file-entry (sb-unix:unix-realpath path))))
                                       (fentry-mode e))
                                   (fentry-mode e)))
                  1)
          (code-char #xB7)
          (format-mtime (fentry-mtime e))
          (and label (code-char #xB7)) label))

(defun info-items (path)
  "Everything lstat (and stat, through a link) can tell about PATH, as label/detail
   rows: the inspector's table, for a file."
  (let ((e (file-entry path)))
    (flet ((row (label detail &optional subject)
             (make-item :label label :detail (princ-to-string detail) :subject subject)))
      (remove nil
              (list (row "name" (fentry-name e))
                    (row "folder" (abbreviate-home (path-parent path))
                         (subj-dir (path-parent path) :select (fentry-name e)))
                    (row "type" (type-word (fentry-type e)))
                    (when (eq (fentry-type e) :symlink)
                      (row "points to" (or (fentry-target e) "?")))
                    (when (eq (fentry-type e) :symlink)
                      (row "resolves to" (if (fentry-broken e) "(nothing — broken)"
                                             (or (ignore-errors (abbreviate-home (sb-unix:unix-realpath path))) "?"))))
                    (when (eq (fentry-type e) :symlink)
                      (row "target type" (if (fentry-broken e) "-" (type-word (fentry-target-type e)))))
                    (row "size" (format nil "~a (~:d bytes)" (human-size (fentry-size e)) (fentry-size e)))
                    (row "mode" (format nil "~a  ~4,'0o" (mode-string (fentry-mode e))
                                        (logand (fentry-mode e) #o7777)))
                    (row "owner" (user-name (fentry-uid e)))
                    (row "modified" (format-mtime (fentry-mtime e)))
                    (row "links" (fentry-nlink e))
                    (row "inode" (fentry-ino e))
                    (multiple-value-bind (free total) (ignore-errors (disk-free path))
                      (and free (row "disk" (format nil "~a free of ~a" (human-size free)
                                                    (human-size total))))))))))

(defmethod present ((s subj-file-info))
  (let ((path (subj-file-info-path s)))
    (handler-case
        (make-view :title (format nil "info ~a" (abbreviate-home path))
                   :kind :list :subject s :items (info-items path))
      (error (c) (make-view :title (format nil "info ~a" (abbreviate-home path)) :kind :list
                            :subject s :items (list (make-item :label (princ-to-string c))))))))

(defun broken-link-view (s path e)
  (make-view :title (file-title "file" path e "broken link")
             :kind :list :subject s
             :items (list (make-item :label (format nil "points to ~a" (fentry-target e))
                                     :detail "which does not exist" :kind :broken)
                          (make-item :label "the folder it is in" :detail (abbreviate-home (path-parent path))
                                     :kind :directory
                                     :subject (subj-dir (path-parent path) :select (fentry-name e))))))

(defun image-view (s path e label octets-reader preview)
  "An :IMAGE view when lol.image can decode it — VIEW-DATA carries the pixels in the
   canvas's own #x00RRGGBB layout, and the shell scales them into the pane — else
   an honest :LIST saying what we know without a decoder."
  (let* ((size (fentry-size (if (eq (fentry-type e) :symlink)
                                (file-entry (sb-unix:unix-realpath path)) e)))
         (cap (if preview +image-preview-cap+ +image-open-cap+))
         (decode (optional-fn "LOL.IMAGE" "DECODE-IMAGE"))
         (head (funcall octets-reader 64))
         (kind (cond ((octets-at-p head 0 '(#x89 #x50)) :png) ((octets-at-p head 0 "GIF8") :gif)
                     ((octets-at-p head 0 "BM") :bmp) (t nil)))
         (dims (image-dimensions head kind)))
    (flet ((fallback (why)
             (make-view :title (file-title "image" path e label) :kind :list :subject s
                        :items (remove nil
                                       (list (make-item :label label
                                                        :detail (if dims (format nil "~dx~d" (first dims) (second dims)) "")
                                                        :kind :directory)
                                             (make-item :label (format nil "(no preview: ~a)" why))
                                             (make-item :label "file information" :subject (subj-file-info path)))))))
      (cond
        ((null decode) (fallback "the image decoder (lol.image) is not loaded"))
        ((> size cap) (fallback (format nil "~a is over the ~a ~:[open~;preview~] cap"
                                        (human-size size) (human-size cap) preview)))
        (t (multiple-value-bind (w h pixels fmt)
               (handler-case (funcall decode (funcall octets-reader size))
                 (error (c) (values nil (princ-to-string c))))
             (if (and w (integerp w) pixels)
                 (make-view :title (file-title "image" path e (format nil "~dx~d ~a" w h
                                                                      (or fmt label)))
                            :kind :image :subject s
                            :data (list :width w :height h :pixels pixels :format fmt :path path))
                 (fallback (or (and h (princ-to-string h)) "it did not decode")))))))))

(defun gzip-view (s path e octets-reader size preview)
  "A .gz: inflate it (whole — DEFLATE has no random access) when lol.inflate is loaded
   and the file is under the cap, then present what came out: a tar listing for a
   .tar.gz, text for text, a hex dump otherwise."
  (let* ((head (funcall octets-reader (min size 4096)))
         (orig (gzip-header-name head))
         (gunzip (optional-fn "LOL.INFLATE" "GUNZIP"))
         (isize (and (>= size 4) (u32le (funcall-tail octets-reader size 4) 0))))
    (flet ((info (why)
             (make-view :title (file-title "file" path e "gzip compressed data") :kind :reference
                        :subject s :syntax :plain
                        :text (format nil "gzip compressed data~@[, was ~s~]~@[, inflates to ~a~]~%~%(~a)~%~%~a"
                                      orig (and isize (human-size isize)) why
                                      (hex-dump head :limit (if preview 1024 +hex-bytes+))))))
      (cond
        ((null gunzip) (info "no inflater: lol.inflate is not loaded"))
        ((> size +gzip-cap+) (info (format nil "~a compressed is over the ~a cap" (human-size size)
                                           (human-size +gzip-cap+))))
        ((and isize (> isize (* 64 1024 1024))) (info "it inflates past 64 MB"))
        (t
         ;; the trailer's size is only a CLAIM (mod 2^32, and one member's of many): a
         ;; few KB can inflate to gigabytes, so the cap is enforced while inflating —
         ;; INFLATE stops at :MAX-OUTPUT instead of growing until the heap is gone
         (let ((out (handler-case (funcall gunzip (funcall octets-reader size)
                                           :max-output (* 64 1024 1024))
                      (error (c) (return-from gzip-view (info (format nil "inflate failed: ~a" c)))))))
           (multiple-value-bind (kind label) (sniff (subseq out 0 (min (length out) 65536)) (or orig ""))
             (case kind
               (:tar (archive-view (file-title "archive" path e "gzip'd tar") s
                                   (tar-members (lambda (off n) (subseq out (min off (length out))
                                                                        (min (+ off n) (length out))))
                                                (length out))))
               (:text (make-view :title (file-title "file" path e (format nil "gzip of ~a" label))
                                 :kind :reference :subject s
                                 :syntax (if (lisp-file-p (or orig "")) :lisp :plain)
                                 :text (text-head (or (octets->text (subseq out 0 (min (length out) 262144))
                                                                    :truncated t)
                                                      "")
                                                  (if preview +preview-text-lines+ 5000))))
               (t (make-view :title (file-title "file" path e (format nil "gzip of ~a" label))
                             :kind :reference :subject s :syntax :plain
                             :text (format nil "~a, ~a inflated~%~%~a" label (human-size (length out))
                                           (hex-dump out :limit (if preview 1024 +hex-bytes+)))))))))))))

(defun funcall-tail (octets-reader size n)
  "The last N bytes of a SIZE-byte file through OCTETS-READER's (n &optional start)."
  (funcall octets-reader n (- size n)))

(defun file-view (s path &key preview)
  "The view of the file at PATH — the dispatcher behind PRESENT and PRESENT-PREVIEW.
   It lstat's, follows a link for the content, reads a capped prefix, SNIFFs it and
   hands it to the view for that kind. PREVIEW selects the smaller caps and, for text,
   a read-only head instead of the editor."
  (let* ((e (file-entry path))
         (link (eq (fentry-type e) :symlink)))
    (when (and link (fentry-broken e)) (return-from file-view (broken-link-view s path e)))
    (let* ((type (if link (fentry-target-type e) (fentry-type e)))
           (size (if link (fentry-target-size e) (fentry-size e))))
      (case type
        (:directory (return-from file-view (listing-view (subj-dir path))))
        (:file)
        (t (return-from file-view (present (subj-file-info path)))))
      (flet ((reader (n &optional (start 0)) (read-prefix path n :start start)))
        (let ((head (reader (min size 65536))))
          (multiple-value-bind (kind label) (sniff head (path-name path))
            (case kind
              (:empty
               (make-view :title (file-title "file" path e "empty") :kind (if preview :reference :file)
                          :subject s :syntax (text-syntax (path-name path))
                          :text "" :data (list :path path :mtime (fentry-mtime e))))
              (:text
               (let ((syntax (text-syntax (path-name path))))
                 (cond
                   (preview
                    (make-view :title (file-title "file" path e label) :kind :reference :subject s
                               :syntax syntax
                               :text (text-head (or (octets->text (subseq head 0 (min (length head)
                                                                                      +preview-text-bytes+))
                                                                  :truncated t)
                                                    "")
                                                +preview-text-lines+)))
                   ((<= size +edit-cap+)
                    (let ((text (octets->text (reader size))))
                      (if text
                          (make-view :title (file-title "file" path e label) :kind :file :subject s
                                     :syntax syntax :text text
                                     :data (list :path path :mtime (fentry-mtime e) :size size))
                          ;; valid UTF-8 in the first 64 KB but not throughout: a
                          ;; binary tail. Never open THAT in the editor — a save would
                          ;; rewrite the bad bytes as replacement characters.
                          (make-view :title (file-title "file" path e "text with binary bytes (read-only)")
                                     :kind :reference :subject s :syntax :plain
                                     :text (hex-dump (reader (min size +hex-bytes+)))))))
                   (t
                    (make-view :title (file-title "file" path e (format nil "~a, too large to edit (over ~a)"
                                                                         label (human-size +edit-cap+)))
                               :kind :reference :subject s :syntax syntax
                               :text (text-head (or (octets->text head :truncated t) "") 2000))))))
              (:image (image-view s path e label #'reader preview))
              (:gzip (gzip-view s path e #'reader size preview))
              (:tar (archive-view (file-title "archive" path e label) s
                                  (tar-members (lambda (off n) (reader (max 0 (min n (- size off))) off))
                                               size)))
              (:zip (multiple-value-bind (members why count) (zip-members path size)
                      (archive-view (file-title "archive" path e
                                                (format nil "~a~@[, ~d entries~]" label count))
                                    s members :note why)))
              (:elf (make-view :title (file-title "file" path e label) :kind :reference :subject s
                               :syntax :plain
                               :text (format nil "~a~%~%~a" (or (elf-summary head) "ELF (unreadable header)")
                                             (hex-dump head :limit (if preview 1024 +hex-bytes+)))))
              (t (make-view :title (file-title "file" path e label) :kind :reference :subject s
                            :syntax :plain
                            :text (format nil "~a, ~a~%~%~a" label (human-size size)
                                          (hex-dump head :limit (if preview 1024 +hex-bytes+))))))))))))

(defun guarded-file-view (s path &key preview)
  (handler-case (file-view s path :preview preview)
    (error (c)
      (make-view :title (format nil "file ~a" (abbreviate-home path)) :kind :list :subject s
                 :items (list (make-item :label (format nil "(cannot open: ~a)" c))
                              (make-item :label "the folder it is in" :kind :directory
                                         :subject (subj-dir (path-parent path)
                                                            :select (path-name path))))))))

(defmethod present ((s subj-file)) (guarded-file-view s (subj-file-path s)))
(defmethod present-preview ((s subj-file)) (guarded-file-view s (subj-file-path s) :preview t))

;;; ---- a find's results ------------------------------------------------------------

(defmethod present ((s subj-find))
  (let* ((root (subj-find-root s))
         (info (subj-find-info s))
         (results (sort-entries (subj-find-results s) :by :name :dirs-first nil))
         (base (length (if (string= root "/") "" root))))
    (make-view
     :title (format nil "find ~s under ~a ~c ~d match~:*~[es~;~:;es~] ~c ~d folder~:p searched"
                    (subj-find-query s) (abbreviate-home root) (code-char #x2014)
                    (length results) (code-char #xB7) (getf info :dirs))
     :kind :list :subject s
     :items (append
             (mapcar (lambda (e)
                       (entry-item e :label (entry-label e (string-left-trim "/" (subseq (fentry-path e) (min base (length (fentry-path e))))))))
                     (sort (copy-list results) #'natural-lessp :key #'fentry-path))
             (when (null results) (list (make-item :label "(nothing found)")))
             (when (getf info :stopped)
               (list (make-item :label (format nil "(search stopped: ~a)"
                                               (case (getf info :stopped)
                                                 (:results "result limit reached")
                                                 (:dirs "folder limit reached")
                                                 (:time "time limit reached")
                                                 (:interrupted "interrupted")
                                                 (t (getf info :stopped))))
                                :detail "narrow the query, or find from a deeper folder"
                                :group "note")))))))

;;; ---- where am I? -------------------------------------------------------------------

(defun folder-of-subject (subject)
  "The folder a subject is IN or IS, or NIL for a subject that is not a file: what ./
   in the Spotter is relative to, and where a new file goes."
  (typecase subject
    (subj-dir (subj-dir-path subject))
    (subj-file (path-parent (subj-file-path subject)))
    (subj-file-info (path-parent (subj-file-info-path subject)))
    (subj-find (subj-find-root subject))
    (t nil)))

(defun subject-path (subject)
  "The path a file subject stands for, or NIL."
  (typecase subject
    (subj-dir (subj-dir-path subject))
    (subj-file (subj-file-path subject))
    (subj-file-info (subj-file-info-path subject))
    (t nil)))

;;; ---- the Spotter: "files" as a surface, and paths as queries ------------------------

(defun files-spotter-candidates ()
  (list (list :label "files" :detail "surface · the file manager, at home" :group "surfaces"
              :kind :directory :subject (subj-dir (home-directory)))
        (list :label "home" :detail (format nil "folder ~a" (home-directory)) :group "surfaces"
              :kind :directory :subject (subj-dir (home-directory)))))

(pushnew 'files-spotter-candidates *spotter-sources*)

(defun spotter-path-results (query &key cwd (limit 200))
  "The Spotter's rows for a PATH query (PATH-QUERY-P): the folder itself first when
   the last component is empty, then every completion. A folder opens the file
   manager there; a FILE opens the file manager at its parent with the file
   selected — the Spotter finds PLACES, and the nav split then previews the file
   beside its neighbours (one more Enter edits it). See doc/file-manager.org."
  (multiple-value-bind (dir partial prefix) (parse-path-query query :cwd cwd)
    (when dir
      (append
       (when (and (zerop (length partial)) (eq (followed-type dir) :directory))
         (list (list :label prefix :detail "open this folder" :group "paths" :kind :directory
                     :subject (subj-dir dir))))
       (mapcar (lambda (c)
                 (destructuring-bind (name dirp full label) c
                   (list :label label
                         :detail (if dirp "folder"
                                     (let ((e (ignore-errors (file-entry full name))))
                                       (if e (entry-detail e) "file")))
                         :group "paths"
                         :kind (if dirp :directory
                                   (let ((e (ignore-errors (file-entry full name))))
                                     (and e (entry-kind e))))
                         :subject (if dirp (subj-dir full) (subj-dir dir :select name)))))
               (path-completions query :cwd cwd :limit limit))))))

;;; ---- commands: what a folder and a file afford --------------------------------------
;;; Declarative, as everywhere (§4½): each command's ACTION is a thunk naming a
;;; poc/file-manager.lisp function by SYMBOL (FUNCALL 'FM-…), because that file
;;; loads after this one. The KEY strings are the accelerators file-manager.lisp
;;; binds, so the menu, the halo and the status line all say the same thing.

(defun fm-command (label key op &key path (surfaces '(:menu :palette :key)))
  (make-command :label label :key key :surfaces surfaces
                :action (lambda () (funcall 'fm-run op path))))

(defmethod commands-for ((d subj-dir))
  (list (fm-command "New folder..." "+" :new-folder)
        (fm-command "New file..." "n" :new-file)
        (fm-command "Filter..." "/" :filter)
        (fm-command "Find below here..." "f" :find)
        (fm-command "Sort: next order" "s" :sort)
        (fm-command "Sort: reverse" "S" :reverse)
        (fm-command "Show/hide dot files" "." :hidden)
        (fm-command "Parent folder" "^" :parent)
        (fm-command "Home folder" "~" :home)
        (fm-command "Refresh" "g" :refresh)
        (fm-command "Copy path" "y" :copy-path :path (subj-dir-path d) :surfaces '(:menu :halo))))

(defun entry-commands (path)
  (list (fm-command "Rename..." "r" :rename :path path :surfaces '(:menu :halo))
        (fm-command "Copy to..." "c" :copy :path path :surfaces '(:menu :halo))
        (fm-command "Move to..." "m" :move :path path :surfaces '(:menu :halo))
        (fm-command "Delete..." "d" :delete :path path :surfaces '(:menu :halo))
        (fm-command "Make symlink to it..." "l" :symlink :path path :surfaces '(:menu))
        (fm-command "Make hard link to it..." "L" :hardlink :path path :surfaces '(:menu))
        (fm-command "Toggle executable" "x" :chmod-x :path path :surfaces '(:menu))
        (fm-command "Copy path" "y" :copy-path :path path :surfaces '(:menu :halo))
        (make-command :label "Information" :key "i" :surfaces '(:menu :halo)
                      :target (subj-file-info path) :disposition :in-place)))

(defmethod commands-for ((f subj-file)) (entry-commands (subj-file-path f)))
(defmethod commands-for ((f subj-file-info)) (entry-commands (subj-file-info-path f)))
(defmethod commands-for ((f subj-find)) '())
