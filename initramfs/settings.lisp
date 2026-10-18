;;;; settings.lisp — the user's preferences, kept as one READable plist on /home.
;;;;
;;;;   ~/.lol/settings      e.g.   (:font-size 20)
;;;;
;;;; The image itself is a frozen heap in a RAM-only initramfs: nothing the user changes
;;;; survives a reboot unless it is written to the laptop's disk, and the only part of
;;;; that disk we may write is /home (doc/storage.org). So a preference lives in the
;;;; user's home, in ~/.lol/ with everything else the environment keeps (lol-home.lisp;
;;;; ~/.config/lol/settings until 2026-10-17, moved at boot), as Lisp DATA: a property list
;;;; the Lisp reader reads back — no parser to write, and a human can edit it.
;;;;
;;;; Two promises, because these run at startup in PID 1:
;;;;   * reading NEVER signals — a missing, empty, unreadable or garbled file is just
;;;;     "no settings", and *READ-EVAL* is off, so #.(...) in the file runs nothing;
;;;;   * writing never signals either, and is skipped entirely when there is no real
;;;;     home (a diskless boot runs with HOME=/, a tmpfs that dies at power-off).
;;;;
;;;; Keys so far: :FONT-SIZE (doc/fonts.org §6) and :TAP-TO-CLICK (doc/input.org; T
;;;; lets a touchpad tap click, absent means off). Any other module may add its own:
;;;; SETTING reads one, SAVE-SETTING writes one and keeps the rest.

(in-package :cl-user)

(defun settings-path ()
  "~/.lol/settings, or NIL when there is no home on a disk to keep it in."
  (lol-path "settings"))

(defun read-settings ()
  "The settings plist, or NIL. Never signals (see the header)."
  (let ((path (settings-path)))
    (when (and path (path-exists-p path))
      (ignore-errors
        (with-open-file (s (native-path path) :external-format :utf-8)
          (let* ((*read-eval* nil)
                 (*package* (find-package :keyword))   ; bare words read as keywords
                 (form (read s nil nil)))
            (and (listp form) (evenp (length form)) form)))))))

(defun setting (key &optional default)
  "The value of KEY in the user's settings, or DEFAULT."
  (getf (read-settings) key default))

(defun save-setting (key value)
  "Set KEY to VALUE in the settings file, keeping every other key, written atomically
   (WRITE-FILE-ATOMICALLY: a crash leaves the old file or the new, never half of one).
   Creates ~/.lol/ when needed. Returns T on success, NIL when there is no home
   or the write failed — never signals: a preference is not worth an error."
  (let ((path (settings-path)))
    (when path
      (ignore-errors
        (let ((plist (copy-list (read-settings))))
          (setf (getf plist key) value)
          ;; mkdir -p, one level at a time (an existing folder is not an error)
          (let ((dir (path-parent path)))
            (loop for d in (list (path-parent dir) dir)
                  unless (path-exists-p d) do (make-directory d)))
          (write-file-atomically
           path
           (sb-ext:string-to-octets
            (with-standard-io-syntax           ; print it so READ gets it back exactly
              (format nil ";; LoL settings — written by the environment, safe to edit.~%~s~%"
                      plist))
            :external-format :utf-8))
          t)))))
