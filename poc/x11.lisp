;;;; x11.lisp — a minimal X11 CLIENT, in pure Common Lisp. No C, no Xlib, no CLX.
;;;;
;;;; This is the load-bearing proof of doc/code-browser.org: that "supporting X"
;;;; never meant running an X *server* (which would need DRM, drivers, xkb, and a
;;;; shared-library userland we do not have). X11 is just a protocol on a socket,
;;;; and we already speak sockets (sb-bsd-sockets, baked into the image).
;;;;
;;;; And because WE do all our own drawing (canvas.lisp), we need an absurdly
;;;; small slice of that protocol. We never ask X to draw a line, render a glyph,
;;;; or load a font. We hand it ONE FINISHED IMAGE. So the whole client is:
;;;;
;;;;    connect + authenticate  (MIT-MAGIC-COOKIE-1 from $XAUTHORITY)
;;;;    CreateWindow · ChangeProperty (title) · MapWindow · CreateGC
;;;;    PutImage                       <- the only drawing call in existence
;;;;    decode KeyPress/ButtonPress/MotionNotify/Expose/ConfigureNotify
;;;;
;;;; That is this file. Everything else X can do, we simply never use.
;;;;
;;;; Byte order: we declare little-endian ('l') and send LE throughout, which is
;;;; also what the canvas's #x00RRGGBB words already are in memory (B,G,R,0).
;;;; So PutImage ships the pixel array with ZERO conversion.

(defpackage :lol.x11
  (:use :cl)
  (:export #:open-display #:close-display #:display-w #:display-h
           #:present #:next-event #:event-kind #:event-x #:event-y
           #:event-detail #:event-w #:event-h #:event-state
           #:decode-key #:shift-p #:ctrl-p))

(in-package :lol.x11)

;;; ---- little-endian byte plumbing -----------------------------------------

(defun u8 (buf i) (aref buf i))
(defun u16 (buf i) (logior (aref buf i) (ash (aref buf (+ i 1)) 8)))
(defun u32 (buf i) (logior (aref buf i)
                           (ash (aref buf (+ i 1)) 8)
                           (ash (aref buf (+ i 2)) 16)
                           (ash (aref buf (+ i 3)) 24)))

(defun put-u8  (s v) (write-byte (logand v #xff) s))
(defun put-u16 (s v) (put-u8 s v) (put-u8 s (ash v -8)))
(defun put-u32 (s v) (put-u16 s v) (put-u16 s (ash v -16)))
(defun put-pad (s n) (dotimes (i n) (put-u8 s 0)))
(defun put-bytes (s v) (write-sequence v s))
(defun pad4 (n) (mod (- 4 (mod n 4)) 4))    ; X pads every field to 4 bytes

;;; ---- the display ---------------------------------------------------------

(defstruct display
  socket stream
  (id-base 0) (root 0) (visual 0) (depth 24)
  (window 0) (gc 0) (next-id 0)
  (w 0) (h 0)
  (max-request 65535)
  ;; the keyboard map, fetched from the server (see FETCH-KEYMAP)
  (min-keycode 8) (syms-per 0) (keysyms #()))

(defun new-id (d)
  "X lets a client mint its own resource IDs out of a server-granted range."
  (prog1 (logior (display-id-base d) (display-next-id d))
    (incf (display-next-id d))))

;;; ---- authentication ------------------------------------------------------
;;;
;;; $XAUTHORITY is a flat binary file of entries, all BIG-endian u16 lengths:
;;;   family, address, number, name, data
;;; We want the MIT-MAGIC-COOKIE-1 cookie; the server compares the 16 bytes.

(defun read-xauth-cookie (&optional (path (sb-posix:getenv "XAUTHORITY")))
  "Return (values auth-name auth-data), or NIL if we cannot find a cookie."
  (unless (and path (probe-file path)) (return-from read-xauth-cookie nil))
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (flet ((be16 () (let ((a (read-byte s nil nil)) (b (read-byte s nil nil)))
                      (and a b (logior (ash a 8) b))))
           (blob (n) (let ((v (make-array n :element-type '(unsigned-byte 8))))
                       (read-sequence v s) v)))
      (loop
        (let ((family (be16)))
          (unless family (return nil))
          (blob (or (be16) 0))                      ; address
          (blob (or (be16) 0))                      ; number
          (let* ((name (blob (or (be16) 0)))
                 (data (blob (or (be16) 0)))
                 (name-string (map 'string #'code-char name)))
            (declare (ignore family))
            (when (string= name-string "MIT-MAGIC-COOKIE-1")
              (return (values name data)))))))))

;;; ---- connect + setup -----------------------------------------------------

(defun open-display (w h &key (title "lisp-over-linux") (path "/tmp/.X11-unix/X0"))
  "Connect to the X server, open a WxH window, and return a DISPLAY ready to
   PRESENT into. Nothing here asks X to draw anything."
  (let* ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect sock path)
    (let ((s (sb-bsd-sockets:socket-make-stream
              sock :input t :output t :element-type '(unsigned-byte 8)
                   :buffering :full)))
      (multiple-value-bind (auth-name auth-data) (read-xauth-cookie)
        (let ((an (or auth-name #())) (ad (or auth-data #())))
          ;; --- setup request ---
          (put-u8 s #x6c) (put-pad s 1)          ; 'l' = little-endian
          (put-u16 s 11) (put-u16 s 0)           ; protocol 11.0
          (put-u16 s (length an)) (put-u16 s (length ad))
          (put-pad s 2)
          (put-bytes s an) (put-pad s (pad4 (length an)))
          (put-bytes s ad) (put-pad s (pad4 (length ad)))
          (finish-output s)))
      ;; --- setup reply ---
      (let ((hdr (make-array 8 :element-type '(unsigned-byte 8))))
        (read-sequence hdr s)
        (let ((status (u8 hdr 0))
              (extra  (* 4 (u16 hdr 6))))
          (let ((body (make-array extra :element-type '(unsigned-byte 8))))
            (read-sequence body s)
            (unless (= status 1)
              (error "X11 connection refused: ~a"
                     (map 'string #'code-char (subseq body 0 (min extra (u8 hdr 1))))))
            (build-display sock s body w h title)))))))

(defun build-display (sock s body w h title)
  "Parse the setup reply far enough to find the root window and its visual,
   then create our window and GC."
  (let* ((id-base  (u32 body 4))
         (max-req  (u16 body 18))
         (nformats (u8 body 21))
         (vendor-len (u16 body 16))
         ;; the screens come after: 32-byte fixed part + vendor(padded) + formats
         (soff (+ 32 vendor-len (pad4 vendor-len) (* 8 nformats)))
         (root   (u32 body soff))
         (visual (u32 body (+ soff 32)))
         (depth  (u8  body (+ soff 38)))
         (d (make-display :socket sock :stream s :id-base id-base
                          :root root :visual visual :depth depth
                          :w w :h h :max-request max-req)))
    (let ((win (new-id d)) (gc (new-id d)))
      (setf (display-window d) win (display-gc d) gc)

      ;; CreateWindow (opcode 1). value-mask #x802 = background-pixel | event-mask.
      (put-u8 s 1) (put-u8 s depth) (put-u16 s 10)
      (put-u32 s win) (put-u32 s root)
      (put-u16 s 0) (put-u16 s 0)                 ; x, y
      (put-u16 s w) (put-u16 s h)
      (put-u16 s 0)                               ; border width
      (put-u16 s 1)                               ; class = InputOutput
      (put-u32 s visual)
      (put-u32 s #x802)
      (put-u32 s #x00000000)                      ; background-pixel = black
      ;; KeyPress|ButtonPress|ButtonRelease|PointerMotion|Exposure|StructureNotify
      (put-u32 s #x0002804d)

      ;; ChangeProperty (18): WM_NAME(atom 39) = STRING(atom 31), so the window
      ;; manager puts a title on it.
      (let* ((tb (map '(vector (unsigned-byte 8)) #'char-code title))
             (n  (length tb)))
        (put-u8 s 18) (put-u8 s 0) (put-u16 s (+ 6 (/ (+ n (pad4 n)) 4)))
        (put-u32 s win) (put-u32 s 39) (put-u32 s 31)
        (put-u8 s 8) (put-pad s 3)
        (put-u32 s n)
        (put-bytes s tb) (put-pad s (pad4 n)))

      ;; CreateGC (55) with no values: PutImage needs a GC, and that is all we
      ;; ever use it for.
      (put-u8 s 55) (put-pad s 1) (put-u16 s 4)
      (put-u32 s gc) (put-u32 s win) (put-u32 s 0)

      ;; Fetch the keyboard map BEFORE mapping the window. X replies and events
      ;; share one stream; doing this while the window is still unmapped means no
      ;; event can be queued ahead of the reply, so we can just read it straight.
      ;; count = max-min+1 keycodes; first+count must not exceed 256.
      (let ((minc (u8 body 26)) (maxc (u8 body 27)))
        (setf (display-min-keycode d) minc)
        (fetch-keymap d minc (1+ (- maxc minc))))

      ;; MapWindow (8) — make it visible.
      (put-u8 s 8) (put-pad s 1) (put-u16 s 2) (put-u32 s win)
      (finish-output s))
    d))

;;; ---- the keyboard --------------------------------------------------------
;;;
;;; X gives us raw KEYCODES (hardware scancodes, layout-independent). Turning
;;; one into "the character the user meant" is the server's keyboard map's job,
;;; so we ask for it once with GetKeyboardMapping and do the lookup ourselves.
;;; Without this you cannot write a text editor — you get numbers, not letters.

(defun fetch-keymap (d first count)
  (let ((s (display-stream d)))
    (put-u8 s 101) (put-pad s 1) (put-u16 s 2)     ; GetKeyboardMapping
    (put-u8 s first) (put-u8 s count) (put-pad s 2)
    (finish-output s)
    (let ((hdr (make-array 32 :element-type '(unsigned-byte 8))))
      (read-sequence hdr s)
      ;; A well-formed reply has byte0 = 1. Anything else (0 = error) must NOT be
      ;; trusted for a length, or we block reading bytes that will never come.
      (unless (= (u8 hdr 0) 1)
        (warn "GetKeyboardMapping did not return a reply (byte0=~a); no keymap."
              (u8 hdr 0))
        (return-from fetch-keymap))
      (let* ((n     (u8 hdr 1))                    ; keysyms per keycode
             (words (u32 hdr 4))
             (buf   (make-array (* 4 words) :element-type '(unsigned-byte 8))))
        (read-sequence buf s)
        (let ((syms (make-array words)))
          (dotimes (i words) (setf (svref syms i) (u32 buf (* 4 i))))
          (setf (display-syms-per d) n
                (display-keysyms d) syms))))))

(defun shift-p (state) (logbitp 0 state))
(defun ctrl-p  (state) (logbitp 2 state))

(defun keysym (d keycode shift)
  "The keysym for KEYCODE at the shifted/unshifted level."
  (let* ((n (display-syms-per d))
         (i (* n (- keycode (display-min-keycode d))))
         (syms (display-keysyms d)))
    (when (and (>= i 0) (< (+ i n) (length syms)) (plusp n))
      (let ((k (svref syms (+ i (if (and shift (> n 1)) 1 0)))))
        (if (zerop k) (svref syms i) k)))))

(defun decode-key (d keycode state)
  "Return either a CHARACTER to insert, or a keyword for a named key, or NIL.
   Latin-1 keysyms ARE their character codes, which is most of the work."
  (let ((k (keysym d keycode (shift-p state))))
    (when k
      (cond
        ((or (<= #x20 k #x7e) (<= #xa0 k #xff)) (code-char k))
        (t (case k
             (#xff08 :backspace) (#xff09 :tab)  (#xff0d :return)
             (#xff1b :escape)    (#xffff :delete)
             (#xff50 :home)      (#xff57 :end)
             (#xff51 :left)      (#xff52 :up)
             (#xff53 :right)     (#xff54 :down)
             (#xff55 :page-up)   (#xff56 :page-down)
             (t nil)))))))

(defun close-display (d)
  (ignore-errors (sb-bsd-sockets:socket-close (display-socket d))))

;;; ---- present: the one and only drawing operation --------------------------

(defun present (d canvas)
  "Ship the whole canvas to the window with PutImage (opcode 72, ZPixmap).

   A single X request is capped at (max-request * 4) bytes, so a 1280x800 frame
   (4 MB) must go in horizontal bands. We compute the tallest band that fits and
   walk down the image. This banding is the ONLY thing PutImage makes us think
   about — and it is why a real toolkit tracks damage rectangles instead of
   pushing full frames. For a POC, full frames are plenty."
  (let* ((s (display-stream d))
         (w (lol.canvas:canvas-w canvas))
         (h (lol.canvas:canvas-h canvas))
         (px (lol.canvas:canvas-pixels canvas))
         (max-words (- (display-max-request d) 6))    ; 6 words of PutImage header
         (rows-per-band (max 1 (floor max-words w)))
         (line (make-array (* w 4) :element-type '(unsigned-byte 8))))
    (loop for y0 from 0 below h by rows-per-band do
      (let* ((rows (min rows-per-band (- h y0)))
             (words (* rows w)))
        (put-u8 s 72) (put-u8 s 2)                    ; PutImage, ZPixmap
        (put-u16 s (+ 6 words))
        (put-u32 s (display-window d)) (put-u32 s (display-gc d))
        (put-u16 s w) (put-u16 s rows)
        (put-u16 s 0) (put-u16 s y0)                  ; dst-x, dst-y
        (put-u8 s 0)                                  ; left-pad
        (put-u8 s (display-depth d))
        (put-pad s 2)
        ;; the pixels themselves: #x00RRGGBB -> B,G,R,0, little-endian
        (dotimes (r rows)
          (let ((base (* (+ y0 r) w)))
            (dotimes (x w)
              (let ((p (aref px (+ base x))) (i (* x 4)))
                (setf (aref line i)       (logand p #xff)          ; B
                      (aref line (+ i 1)) (logand (ash p -8) #xff)  ; G
                      (aref line (+ i 2)) (logand (ash p -16) #xff) ; R
                      (aref line (+ i 3)) 0)))
            (write-sequence line s)))))
    (finish-output s)))

;;; ---- events --------------------------------------------------------------
;;;
;;; Every X event is exactly 32 bytes. We decode the five we care about and
;;; throw the rest away — including errors (code 0), which for a POC we simply
;;; report rather than model.

(defstruct event kind detail x y w h (state 0))

(defun next-event (d &key (wait t))
  "Return the next EVENT, or NIL if WAIT is nil and nothing is pending."
  (let ((s (display-stream d)))
    (when (and (not wait) (not (listen s))) (return-from next-event nil))
    (let ((e (make-array 32 :element-type '(unsigned-byte 8))))
      (unless (= 32 (read-sequence e s)) (return-from next-event nil))
      (let ((code (logand (u8 e 0) #x7f)))
        (case code
          (0  (make-event :kind :error  :detail (u8 e 1)))
          (2  (make-event :kind :key-press    :detail (u8 e 1)
                          :x (u16 e 24) :y (u16 e 26) :state (u16 e 28)))
          (3  (make-event :kind :key-release  :detail (u8 e 1) :state (u16 e 28)))
          (4  (make-event :kind :button-press :detail (u8 e 1)
                          :x (u16 e 24) :y (u16 e 26) :state (u16 e 28)))
          (5  (make-event :kind :button-release :detail (u8 e 1)
                          :x (u16 e 24) :y (u16 e 26) :state (u16 e 28)))
          (6  (make-event :kind :motion :x (u16 e 24) :y (u16 e 26)
                          :state (u16 e 28)))
          (12 (make-event :kind :expose :x (u16 e 8) :y (u16 e 10)
                          :w (u16 e 12) :h (u16 e 14)))
          (22 (make-event :kind :configure :w (u16 e 20) :h (u16 e 22)))
          (t  (make-event :kind :other :detail code)))))))
