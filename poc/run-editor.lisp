;;;; run-editor.lisp — the text-view prototype: the browser's source pane, live.
;;;;
;;;;   sbcl --script poc/run-editor.lisp
;;;;
;;;; What this is testing (doc/code-browser.org §9's "riskiest piece"):
;;;;   - a caret you can place with the MOUSE, in pixels
;;;;   - drag-SELECTION across lines
;;;;   - real typing, with syntax colour and auto-indent
;;;;   - and ACCEPT (C-c C-c): read the buffer, EVAL it, and the definition is
;;;;     now live IN THIS RUNNING IMAGE.
;;;;
;;;; The last one is the whole Smalltalk loop, and the status bar proves it: it
;;;; calls (GREETING) on every frame and prints the result. Edit the string in
;;;; the source pane, press C-c C-c, and watch the bar change. Nothing is
;;;; reloaded; nothing is restarted. That is a code browser.

(require :sb-bsd-sockets)
(require :sb-posix)

(let ((here (make-pathname :directory (pathname-directory *load-truename*))))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "canvas.lisp"   here))
    (load (merge-pathnames "x11.lisp"      here))
    (load (merge-pathnames "textview.lisp" here))))

(use-package :lol.canvas)

;;; The definitions we are "browsing". In the real browser these come out of the
;;; definition registry (doc/code-browser.org §2); here they are just text.
(defparameter *defs*
  '(("greeting"
     "(defun greeting ()
  \"What the status bar below prints. Edit me, then press C-c C-c.\"
  (let ((who \"lisp machine\"))
    (format nil \"hello, ~a\" who)))")
    ("fib"
     "(defun fib (n)
  \"Naive Fibonacci — a second definition to click between.\"
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))")
    ("draw-alien"
     ";; the real thing, from initramfs/framebuffer.lisp
(defun draw-alien (&key (margin 16) announce)
  \"Blit the alien sprite into the top-right corner of /dev/fb0.\"
  (multiple-value-bind (xres yres stride bpp) (read-fb-geometry)
    (let* ((x0 (max 0 (- xres 256 margin)))
           (y0 margin))
      ;; efifb is commonly BGRX: bytes are Blue,Green,Red,pad.
      (blit-row x0 y0 stride bpp))))")))

;;; seed the image with a GREETING so the status bar has something to call
(eval (read-from-string (second (first *defs*))))

(defparameter +bg+     (rgb #x14 #x16 #x1a))
(defparameter +panel+  (rgb #x1b #x1e #x24))
(defparameter +hi+     (rgb #x24 #x29 #x31))
(defparameter +rule+   (rgb #x2e #x34 #x3d))
(defparameter +text+   (rgb #xd6 #xd3 #xc8))
(defparameter +dim+    (rgb #x6b #x72 #x7d))
(defparameter +accent+ (rgb #x7f #xdb #xca))
(defparameter +ok+     (rgb #xa8 #xff #x78))
(defparameter +err+    (rgb #xff #x6b #x6b))
(defparameter +selbg+  (rgb #x2f #x4f #x4a))

(defvar *status* "click a definition, edit it, then C-c C-c (or Ctrl-Enter) to Accept")
(defvar *status-color* +dim+)
(defvar *sel* 0)

(defun draw-all (canvas font tv w h)
  (fill-rect canvas 0 0 w h +bg+)

  ;; ---- definition list (left) ----
  (let ((lw 230))
    (fill-rect canvas 8 8 lw (- h 70) +panel+)
    (draw-rect canvas 8 8 lw (- h 70) +rule+)
    (draw-string canvas font "DEFINITION" 18 16 +dim+)
    (fill-rect canvas 9 34 (- lw 2) 1 +rule+)
    (loop for i from 0 below (length *defs*)
          for name = (first (nth i *defs*))
          for iy = (+ 44 (* i 22))
          do (when (= i *sel*)
               (fill-rect canvas 9 (- iy 3) (- lw 2) 22 +selbg+)
               (fill-rect canvas 9 (- iy 3) 2 22 +accent+))
             (draw-string canvas font name 20 iy (if (= i *sel*) +accent+ +text+)))

    ;; ---- source pane (right) ----
    (let ((sx (+ 16 lw)) (sy 8))
      (let ((sw (- w sx 8)) (sh (- h 70)))
        (fill-rect canvas sx sy sw sh +panel+)
        (draw-rect canvas sx sy sw sh +rule+)
        (fill-rect canvas (+ sx 1) (+ sy 1) (- sw 2) 26 +hi+)
        (draw-string canvas font "Source" (+ sx 12) (+ sy 6) +accent+)
        (draw-string canvas font "Doc   Expand   Disasm   Callers"
                     (+ sx 80) (+ sy 6) +dim+)
        (fill-rect canvas (+ sx 1) (+ sy 27) (- sw 2) 1 +rule+)
        (lol.textview:tv-geometry tv (+ sx 1) (+ sy 28) (- sw 2) (- sh 29))
        (lol.textview:tv-draw canvas tv))))

  ;; ---- the LIVE status bar ----
  (fill-rect canvas 0 (- h 56) w 56 +hi+)
  (fill-rect canvas 0 (- h 56) w 1 +rule+)
  (draw-string canvas font *status* 12 (- h 48) *status-color*)
  ;; call the function the user is editing, right now, in this image:
  (let ((live (handler-case
                  (if (fboundp 'greeting)
                      (format nil "(greeting)  =>  ~s" (funcall 'greeting))
                      "(greeting)  =>  <undefined>")
                (serious-condition (c) (format nil "(greeting)  =>  error: ~a" c)))))
    (draw-string canvas font live 12 (- h 26) +ok+)))

(defun accept (tv)
  "Read the buffer and EVAL it. This is the browser's Accept — the definition is
   compiled into the running image, and the status bar picks it up immediately."
  (handler-case
      (let ((form (read-from-string (lol.textview:tv-text tv))))
        (let ((name (eval form)))
          (setf *status* (format nil "Accepted:  ~s" name)
                *status-color* +ok+)))
    (serious-condition (c)
      (setf *status* (format nil "Accept failed: ~a" c)
            *status-color* +err+))))

(let* ((here   (make-pathname :directory (pathname-directory *load-truename*)))
       (w 1080) (h 660)
       (font   (load-font (merge-pathnames "cozette.lolf" here)))
       (canvas (make-canvas w h))
       (tv     (lol.textview:make-textview :font font :focus t))
       (d      (lol.x11:open-display w h :title "lol - source pane prototype")))
  (lol.textview:tv-set-text tv (second (nth *sel* *defs*)))
  (format t "~&Editor open.  Click / drag to select.  C-c C-c or Ctrl-Enter = Accept.~%")
  (format t "Try: edit the string inside GREETING, Accept, and watch the green line.~%")
  (finish-output)

  (let ((quit nil) (dragging nil) (pending-c-c nil))
    (flet ((handle (e)
             (case (lol.x11:event-kind e)
               (:key-press
                (let* ((st  (lol.x11:event-state e))
                       (key (lol.x11:decode-key d (lol.x11:event-detail e) st)))
                  (cond
                    ;; Esc quits
                    ((eq key :escape) (setf quit t))
                    ;; the C-c C-c prefix chord, exactly as the plan specifies
                    ((and (lol.x11:ctrl-p st) (eql key #\c))
                     (if pending-c-c
                         (progn (accept tv) (setf pending-c-c nil))
                         (setf pending-c-c t)))
                    (t
                     (setf pending-c-c nil)
                     (when (eq :accept (lol.textview:tv-key tv key st))
                       (accept tv))))))
               (:button-press
                (when (= 1 (lol.x11:event-detail e))
                  (let ((x (lol.x11:event-x e)) (y (lol.x11:event-y e)))
                    (cond
                      ((< x 240)
                       ;; clicked the definition list: load that source
                       (let ((i (floor (- y 41) 22)))
                         (when (and (>= i 0) (< i (length *defs*)))
                           (setf *sel* i
                                 (lol.textview:tv-focus tv) nil)
                           (lol.textview:tv-set-text tv (second (nth i *defs*))))))
                      ;; only a click INSIDE the text rectangle moves the caret;
                      ;; the tab strip above and status bar below are dead zones.
                      ((lol.textview:tv-hit-p tv x y)
                       (setf (lol.textview:tv-focus tv) t
                             dragging t)
                       (lol.textview:tv-click tv x y))))))
               (:motion
                (when dragging
                  (lol.textview:tv-drag tv (lol.x11:event-x e) (lol.x11:event-y e))))
               (:button-release (setf dragging nil))
               (:error (format t "~&X11 error ~a~%" (lol.x11:event-detail e)))
               (t nil))))
      (unwind-protect
           (loop
             (draw-all canvas font tv w h)
             (lol.x11:present d canvas)
             (let ((e (lol.x11:next-event d :wait t)))
               (unless e (return))
               (handle e))
             (loop for e = (lol.x11:next-event d :wait nil) while e do (handle e))
             (when quit (return)))
        (lol.x11:close-display d))))
  (format t "~&bye.~%"))
