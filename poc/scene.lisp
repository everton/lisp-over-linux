;;;; scene.lisp — the thing we draw. A mock-up of the code browser itself.
;;;;
;;;; Deliberately NOT a coloured triangle: the point of the POC is to prove the
;;;; real thing is reachable, so we draw the real thing's layout — the root-axis
;;;; tab bar, three list panes, a syntax-coloured source pane, the alien, a
;;;; status line — and we make it respond to the keyboard and mouse.
;;;;
;;;; It knows NOTHING about X11 or /dev/fb0. It draws into a canvas. That is the
;;;; entire architectural claim, in one file.

(defpackage :lol.scene
  (:use :cl :lol.canvas)
  (:export #:make-ui #:draw-ui #:ui-click #:ui-key #:ui-w #:ui-h))

(in-package :lol.scene)

;;; A palette that is ours, not the terminal's 16. This is the freedom we bought
;;; by going pixel-native: real colours, chosen, not whatever the VT had.
(defparameter +bg+       (rgb #x14 #x16 #x1a))
(defparameter +panel+    (rgb #x1b #x1e #x24))
(defparameter +panel-hi+ (rgb #x24 #x29 #x31))
(defparameter +rule+     (rgb #x2e #x34 #x3d))
(defparameter +text+     (rgb #xd6 #xd3 #xc8))
(defparameter +dim+      (rgb #x6b #x72 #x7d))
(defparameter +accent+   (rgb #x7f #xdb #xca))   ; the "current" colour
(defparameter +sel-bg+   (rgb #x2f #x4f #x4a))

;; syntax colours for the source pane
(defparameter +c-paren+  (rgb #x6b #x72 #x7d))
(defparameter +c-kw+     (rgb #xc7 #x92 #xea))
(defparameter +c-name+   (rgb #x7f #xdb #xca))
(defparameter +c-str+    (rgb #xa8 #xd0 #x78))
(defparameter +c-cmt+    (rgb #x5a #x63 #x55))

(defstruct ui
  (w 1000) (h 640)
  font alien
  (packages #("LOL.BROWSER" "LOL.CANVAS" "LOL.X11" "COMMON-LISP" "SB-INTROSPECT"))
  (kinds    #("Functions" "Macros" "Classes" "Generics" "Variables"))
  (defs     #("draw-alien" "read-fb-geometry" "report-memory" "editing-repl"
              "start-net-repl" "with-raw-mode" "colorize-lisp" "complete-symbol"))
  (sel-pkg 0) (sel-kind 0) (sel-def 0))

;;; The source we pretend to be browsing — pre-tokenised into coloured runs, so
;;; the POC exercises exactly the draw-string chaining the real source pane needs.
(defparameter +source+
  '((((:cmt . ";;;; framebuffer.lisp — blit the alien onto /dev/fb0."))))) ; placeholder, see below

(defparameter +lines+
  ;; each line is a list of (colour-key . text) runs
  '(((:cmt . ";; the sprite is composited with a real alpha channel"))
    ()
    ((:paren . "(") (:kw . "defun") (:plain . " ") (:name . "draw-alien")
     (:plain . " ") (:paren . "(") (:plain . "&key ") (:paren . "(")
     (:plain . "margin ") (:num . "16") (:paren . ") ") (:plain . "announce")
     (:paren . "))"))
    ((:plain . "  ") (:str . "\"Blit the alien sprite into the top-right corner.\""))
    ((:plain . "  ") (:paren . "(") (:kw . "multiple-value-bind")
     (:plain . " ") (:paren . "(") (:plain . "xres yres stride bpp")
     (:paren . ")"))
    ((:plain . "      ") (:paren . "(") (:name . "read-fb-geometry")
     (:paren . ")"))
    ((:plain . "    ") (:paren . "(") (:kw . "let*") (:plain . " ")
     (:paren . "((") (:plain . "x0 ") (:paren . "(") (:name . "max")
     (:plain . " ") (:num . "0") (:plain . " ") (:paren . "(") (:name . "-")
     (:plain . " xres ") (:num . "256") (:plain . " margin")
     (:paren . "))))"))
    ((:plain . "      ") (:cmt . "; efifb is BGRX: bytes are B,G,R,pad"))
    ((:plain . "      ") (:paren . "(") (:name . "blit-row") (:plain . " x0 y0")
     (:paren . ")))))"))))

(defun run-color (key)
  (case key
    (:kw +c-kw+) (:name +c-name+) (:str +c-str+) (:cmt +c-cmt+)
    (:paren +c-paren+) (:num +c-str+) (t +text+)))

;;; ---- layout --------------------------------------------------------------

(defparameter +tabs+ #("Packages" "Classes" "Generics" "Files" "Changes" "Search"))

(defun draw-list (ui canvas x y w h title items selected)
  (let ((font (ui-font ui)))
    (fill-rect canvas x y w h +panel+)
    (draw-rect canvas x y w h +rule+)
    (draw-string canvas font title (+ x 8) (+ y 6) +dim+)
    (fill-rect canvas (+ x 1) (+ y 24) (- w 2) 1 +rule+)
    (loop for i from 0 below (length items)
          for iy = (+ y 30 (* i 20))
          while (< (+ iy 18) (+ y h))
          do (when (= i selected)
               (fill-rect canvas (+ x 1) (- iy 3) (- w 2) 20 +sel-bg+)
               (fill-rect canvas (+ x 1) (- iy 3) 2 20 +accent+))
             (draw-string canvas font (aref items i) (+ x 10) iy
                          (if (= i selected) +accent+ +text+)))))

(defun draw-ui (ui canvas)
  (let* ((font (ui-font ui))
         (w (ui-w ui)) (h (ui-h ui))
         (col (floor (- w 24) 3)))
    (fill-rect canvas 0 0 w h +bg+)

    ;; ---- root-axis tab bar (the switchable axis from the plan) ----
    (fill-rect canvas 0 0 w 30 +panel-hi+)
    (fill-rect canvas 0 29 w 1 +rule+)
    (let ((tx 12))
      (loop for i from 0 below (length +tabs+)
            for label = (aref +tabs+ i)
            do (let ((tw (+ 16 (string-px font label))))
                 (when (zerop i)
                   (fill-rect canvas (- tx 8) 0 tw 29 +bg+)
                   (fill-rect canvas (- tx 8) 0 tw 2 +accent+))
                 (draw-string canvas font label tx 8
                              (if (zerop i) +accent+ +dim+))
                 (incf tx (+ tw 8)))))

    ;; ---- three list panes ----
    (draw-list ui canvas 8 38 col 190
               "PACKAGE" (ui-packages ui) (ui-sel-pkg ui))
    (draw-list ui canvas (+ 16 col) 38 col 190
               "CATEGORY" (ui-kinds ui) (ui-sel-kind ui))
    (draw-list ui canvas (+ 24 (* 2 col)) 38 col 190
               "DEFINITION" (ui-defs ui) (ui-sel-def ui))

    ;; ---- the source pane ----
    (let ((sy 236) (sh (- h 236 26)))
      (fill-rect canvas 8 sy (- w 16) sh +panel+)
      (draw-rect canvas 8 sy (- w 16) sh +rule+)
      ;; tab strip
      (let ((tx 18))
        (dolist (tab '("Source" "Doc" "Expand" "Disasm" "Callers" "Versions"))
          (let ((first (string= tab "Source")))
            (when first (fill-rect canvas (- tx 6) (+ sy 4) (+ 12 (string-px font tab)) 18 +panel-hi+))
            (draw-string canvas font tab tx (+ sy 6) (if first +accent+ +dim+))
            (incf tx (+ 12 (string-px font tab) 10)))))
      (fill-rect canvas 9 (+ sy 26) (- w 18) 1 +rule+)
      ;; the code, run by coloured run
      (loop for line in +lines+
            for i from 0
            for ly = (+ sy 36 (* i 19))
            do ;; gutter line number
               (draw-string canvas font (format nil "~2d" (1+ i)) 16 ly +c-cmt+)
               (let ((pen 44))
                 (dolist (run line)
                   (setf pen (draw-string canvas font (cdr run) pen ly
                                          (run-color (car run))))))))

    ;; ---- the alien, composited with alpha, exactly as on the real machine ----
    (when (ui-alien ui)
      (blit-rgba canvas (ui-alien ui) 256 150 (- w 256 20) (- h 150 34)))

    ;; ---- status line ----
    (fill-rect canvas 0 (- h 22) w 22 +panel-hi+)
    (fill-rect canvas 0 (- h 22) w 1 +rule+)
    (draw-string canvas font
                 (format nil "~a  ·  ~a  ·  ~a      arrows / click to browse · q to quit"
                         (aref (ui-packages ui) (ui-sel-pkg ui))
                         (aref (ui-kinds ui) (ui-sel-kind ui))
                         (aref (ui-defs ui) (ui-sel-def ui)))
                 10 (- h 17) +dim+)))

;;; ---- interaction ---------------------------------------------------------
;;; Proves the event path end to end: a click or an arrow changes state, the
;;; scene is redrawn, the backend presents it.

(defun ui-click (ui x y)
  (let ((col (floor (- (ui-w ui) 24) 3)))
    (when (and (> y 60) (< y 228))
      (let ((row (floor (- y 65) 20)))
        (cond
          ((and (>= x 8) (< x (+ 8 col)))
           (when (< row (length (ui-packages ui))) (setf (ui-sel-pkg ui) (max 0 row))))
          ((and (>= x (+ 16 col)) (< x (+ 16 (* 2 col))))
           (when (< row (length (ui-kinds ui))) (setf (ui-sel-kind ui) (max 0 row))))
          ((>= x (+ 24 (* 2 col)))
           (when (< row (length (ui-defs ui))) (setf (ui-sel-def ui) (max 0 row)))))))))

(defun ui-key (ui keysym)
  "KEYSYM here is an X11 keycode (evdev layout) — the POC only needs a few."
  (case keysym
    (111 (setf (ui-sel-def ui) (max 0 (1- (ui-sel-def ui)))))                     ; Up
    (116 (setf (ui-sel-def ui) (min (1- (length (ui-defs ui))) (1+ (ui-sel-def ui))))) ; Down
    ((9 24) :quit)                                                                ; Esc / q
    (t nil)))
