;;;; web.lisp — the web pane: a Chrome tab as a subject in the shell.
;;;;
;;;; doc/web-browser.org is the design and the story; this is the code. In one
;;;; paragraph: a headless Google Chrome renders the page; Page.startScreencast makes
;;;; it send us a JPEG every time the page's pixels change; we decode the JPEG
;;;; (lol.image, through stb_image) and blit it into the pane like any other picture.
;;;; Going the other way, our keys and clicks become Input.dispatchKeyEvent /
;;;; Input.dispatchMouseEvent commands. The wire is web-cdp.lisp. Chrome never
;;;; touches /dev/fb0 or our input devices; the Lisp is still the only process that
;;;; draws, and Chrome is one more thing it draws (doc/processes.org §1).
;;;;
;;;; THE SHAPE, top to bottom:
;;;;
;;;;   SUBJ-WEB          a subject, like a folder or a definition. PRESENT gives a view
;;;;                     of kind :WEB whose DATA is the WEB-TAB, so the shell's tab
;;;;                     strip, trails, breadcrumbs, menus and halo all apply to it.
;;;;   WEB-TAB           one Chrome tab (a CDP "target" we are attached to): its URL,
;;;;                     title, loading state, and the last frame, decoded.
;;;;   *WEB-CHROME*      the one Chrome process, started on first use.
;;;;   WEB-SERVICE       run by the browser's event loop every turn: read what Chrome
;;;;                     sent, keep the visible tab screencasting at the pane's size,
;;;;                     decode its newest frame, close tabs whose trail was closed.
;;;;   DRAW-WEB-BODY     the toolbar (back, forward, reload/stop, address) + the frame.
;;;;   WEB-KEY, WEB-CLICK, WEB-DRAG, WEB-RELEASE, WEB-HOVER, WEB-WHEEL
;;;;                     input, translated into CDP.
;;;;
;;;; TABS ARE TRAILS. Each web tab is a trail of its own, so it sits in the one tab
;;;; strip with the Workspace and the file manager, SuperL-1…9 / SuperL-Tab reach it,
;;;; and SuperL-w closes it. Closing needs no hook in the shell: WEB-SERVICE notices a
;;;; tab no trail holds any more and closes its Chrome target (WEB-COLLECT-TABS). A
;;;; link that opens a new window (target=_blank, window.open, Ctrl-click) becomes a
;;;; new trail the same way, from Chrome's Target.targetCreated event.
;;;;
;;;; NOTHING BLOCKS. Starting Chrome takes a second on the laptop and many under QEMU,
;;;; so every conversation with it is a chain of CALLBACKS run from WEB-SERVICE as the
;;;; replies arrive; the browser keeps drawing (a "starting Chrome…" line) meanwhile.
;;;; Only the test helpers (WEB-EVAL) wait for an answer.
;;;;
;;;; All original code, under this project's MIT licence.

(in-package :cl-user)

;;; ---- settings ---------------------------------------------------------------

(defvar *web-chrome-binary* "/usr/bin/google-chrome"
  "Chrome, as a path INSIDE the container it runs in (the host's Debian), or on the
   host itself when the environment runs there. /usr/bin/google-chrome is Debian's
   alternatives link to /opt/google/chrome/google-chrome, a shell script that execs
   the real binary next to it.")

(defvar *web-profile* nil
  "Chrome's profile directory (--user-data-dir): cookies, logins, history, cache. NIL
   means ~/.lol/web/chrome/ — OUR profile, not the one the host's own Chrome uses.
   Point it at ~/.config/google-chrome to reuse your desktop logins, but only while
   the desktop Chrome is NOT running (a profile is locked by the process using it),
   and see doc/web-browser.org §7.3 on why its saved cookies may not decrypt here.")

(defvar *web-search* "https://www.google.com/search?q=~a"
  "What a non-URL typed in the address bar becomes: FORMAT with the query, percent-
   encoded, as its one argument.")

(defvar *web-guide-index*
  ;; Frozen at BUILD time, the way sbcl-source.lisp finds ./sbcl: this file is being
  ;; loaded from <repo>/initramfs/web.lisp, so the guide is <repo>/doc/guide/. On the
  ;; laptop the repo is under the mounted /home, at the same path — and Chrome's
  ;; container sees the same /home, so a file:// URL works there too.
  (ignore-errors
    (namestring (merge-pathnames "../doc/guide/index.html"
                                 (make-pathname :name nil :type nil
                                                :defaults *load-truename*)))))

(defvar *web-home* nil
  "What a new tab shows when it is opened without a URL (the address prompt opens on
   top of it). NIL means the project's own guide (doc/guide/index.html) when this
   boot can see it, else about:blank — see WEB-HOME-URL. Set a URL string to override.")

(defun web-home-url ()
  "The home page: *WEB-HOME* if set, else the guide when its file exists (a diskless
   boot has no /home, so no repo), else about:blank."
  (or *web-home*
      (let ((g *web-guide-index*))
        (and g (probe-file g)
             (concatenate 'string "file://" (namestring (truename g)))))
      "about:blank"))

(defvar *web-sandbox* :auto
  "Chrome's own sandbox (renderers in namespaces, filtered by seccomp). T keeps it,
   NIL passes --no-sandbox, :AUTO keeps it on the host and inside a container that can
   provide it, and drops it where it cannot be had (doc/web-browser.org §6.4).")

(defvar *web-extra-args* '()
  "More Chrome switches, appended last (e.g. \"--force-dark-mode\").")

(defvar *web-zoom* 1
  "Device pixels per CSS pixel — the page zoom. C-+ / C-- / C-0 change it; it is a
   real deviceScaleFactor, so text is re-laid-out and re-rasterised larger, not a
   blurry enlargement of the frame.")

(defvar *web-quality* 80 "JPEG quality Chrome encodes screencast frames at (0-100).")

(defvar *web-launcher* nil
  "A function of ARGV returning the argv that actually starts it — how Chrome gets
   into a container in the guest. NIL picks one (WEB-LAUNCH-PREFIX).")

;;; ---- the subject, and one tab ------------------------------------------------

(defstruct (web-tab (:constructor %make-web-tab))
  (url "")                ; what the address bar shows
  (title nil)             ; the page's <title>, once Chrome says
  target session          ; Chrome's targetId, and our session on it (NIL until attached)
  (state :new)            ; :new :creating :live :dead (closed or Chrome gone) :crashed
  loading                 ; T between frameStartedLoading and frameStoppedLoading
  frame-w frame-h frame-px ; the newest decoded frame: size + #x00RRGGBB pixels
  pending                 ; the newest UNDECODED frame's base64 string, or NIL
  pending-sharp           ; ... T when it is a screenshot: already device-sized (§4.7)
  meta                    ; that frame's metadata (scroll offset, scale, device size)
  (want-w 0) (want-h 0)   ; the page rect the last draw gave this tab, in device px
  (sent-w 0) (sent-h 0) (sent-zoom 1) ; ... and what Chrome was last told
  screencasting           ; is Chrome streaming frames of this tab to us?
  (history nil)           ; (entries . current-index), refreshed after navigation
  status                  ; a line for the pane's foot: a download, an error
  open-url                ; a URL waiting to be loaded once the tab is attached
  (frames 0)              ; frames received, for the doc's measurements
  (cast-at 0)             ; internal-real-time the screencast was (re)started
  input-at                ; when the last input was sent, until a frame answers it
  (latencies '())         ; ms from an input to the frame after it, newest first (≤ 32)
  capturing               ; a Page.captureScreenshot is in flight (WEB-NUDGE, WEB-SHARPEN)
  soft                    ; T when the frame on show was UPSCALED (page zoom): sharpen it
  (frame-at 0)            ; internal-real-time the last screencast frame was decoded
  (last-frame-ms nil)     ; ms the last JPEG took to decode + convert
  ;; where the pane showing it last drew it, in that pane's coordinates (its
  ;; surface's: the screen's, or a window's) — what clicks and frames are placed by
  page-rect               ; (x y w h) of the page image
  (bar-rects '())         ; ((:back x0 x1) (:forward …) (:reload …) (:address …))
  bar-y)                  ; (y0 . y1) of the toolbar

(defstruct (subj-web (:constructor subj-web (tab))) tab)

(defvar *web-tabs* '() "Every WEB-TAB we hold, live or not.")
(defvar *web-chrome* nil "The Chrome process (a CHROME), or NIL before first use.")
(defvar *web-chrome-state* :off
  ":off — never started (or stopped); :starting — launched, no answer yet; :ready;
   :failed — it did not start (*WEB-ERROR* says why).")
(defvar *web-error* nil "Why Chrome is :failed or a tab could not be opened, for the pane.")
(defvar *web-ua* nil "Chrome's user agent with \"HeadlessChrome\" made plain \"Chrome\".")
(defvar *web-chrome-version* nil
  "Chrome's full version, \"148.0.7778.215\", from Browser.getVersion's product.")

(defvar *web-changed* nil "Something the pane SHOWS changed (a title, a load state): redraw.")
(defvar *web-adopt* '() "targetIds of pages Chrome opened on its own, to attach to.")
(defvar *web-focus-next* t "Should the next adopted page take focus? (NIL after a Ctrl-click.)")
(defvar *web-creating* 0 "How many Target.createTarget calls of OURS are in flight.")
(defvar *web-downloads* '() "(guid . plist) for every download this session.")

(defun web-tab-of (pane)
  "PANE's WEB-TAB when it is a web pane, else NIL."
  (and pane (eq (view-kind (pane-view pane)) :web) (view-data (pane-view pane))))

(defun web-pane-p (pane) (and (web-tab-of pane) t))

(defun web-visible-tab ()
  "The tab on screen: the current pane's, when the current pane is a web pane."
  (ignore-errors (web-tab-of (current-pane))))

(defun web-tab-label (tab)
  (let ((title (web-tab-title tab)))
    (if (and title (plusp (length title))) title (web-tab-url tab))))

(defmethod present ((s subj-web))
  (let ((tab (subj-web-tab s)))
    (make-view :title (web-tab-label tab) :kind :web :data tab :subject s)))

(defun web-retitle (tab)
  "Chrome told us TAB's title or URL changed: every view showing it says so. Views
   are built once per pane, so the title is pushed into them rather than re-presented."
  (dolist (trail *trails*)
    (dolist (p trail)
      (when (eq (web-tab-of p) tab)
        (setf (view-title (pane-view p)) (web-tab-label tab)))))
  (setf *web-changed* t))

;;; ---- URLs -------------------------------------------------------------------

(defun url-encode (string)
  "STRING percent-encoded as a query component: unreserved characters as they are,
   every other UTF-8 octet as %XX."
  (with-output-to-string (o)
    (loop for b across (sb-ext:string-to-octets string :external-format :utf-8)
          do (if (or (<= 48 b 57) (<= 65 b 90) (<= 97 b 122) (member b '(45 46 95 126)))
                 (write-char (code-char b) o)
                 (format o "%~2,'0X" b)))))

(defun web-url-for (text)
  "What the address bar does with TEXT: a URL as it is; something shaped like a host
   (\"github.com/x\", \"localhost:3000\", \"10.0.2.2:8000/a\") gets a scheme — http for
   localhost and bare IPv4 addresses, where development servers live, https for
   everything else; any other text is a search."
  (let ((s (string-trim " " text)))
    (cond
      ((zerop (length s)) (web-home-url))
      ((search "://" s) s)
      ;; a path — / ~/ ./ ../ — is a file on the disk (it used to become a search)
      ((path-query-p s) (web-file-url (web-expand-path s)))
      ((some (lambda (p) (and (>= (length s) (length p)) (string-equal p s :end2 (length p))))
             '("about:" "data:" "file:" "chrome:" "view-source:" "javascript:"))
       s)
      ((and (not (find #\Space s))
            (let* ((host-end (or (position-if (lambda (c) (find c "/?#")) s) (length s)))
                   (host (subseq s 0 host-end))
                   (bare (subseq host 0 (or (position #\: host) (length host)))))
              (or (find #\. bare) (string-equal bare "localhost"))))
       (let* ((host (subseq s 0 (or (position-if (lambda (c) (find c ":/?#")) s) (length s)))))
         (format nil "~a://~a"
                 (if (or (string-equal host "localhost")
                         (every (lambda (c) (or (digit-char-p c) (char= c #\.))) host))
                     "http" "https")
                 s)))
      (t (format nil *web-search* (url-encode s))))))

(defun web-expand-path (text)
  "A typed path as an absolute one: ~ is the home folder, ./ and ../ are taken from
   it too (the address bar has no folder of its own), and a trailing slash is kept —
   it is how a folder was typed."
  (let* ((home (home-directory))
         (abs (cond ((char= (char text 0) #\/) text)
                    ((char= (char text 0) #\~) (concatenate 'string home (subseq text 1)))
                    (t (path-join home text))))
         (norm (normalize-path abs)))
    (if (and (> (length text) 1) (char= (char text (1- (length text))) #\/)
             (string/= norm "/"))
        (concatenate 'string norm "/")
        norm)))

(defun web-file-url (path)
  "The file:// URL for the absolute PATH: every octet but the unreserved characters
   and the slashes percent-encoded, so a space or a # in a file name stays part of it."
  (with-output-to-string (o)
    (write-string "file://" o)
    (loop for b across (sb-ext:string-to-octets path :external-format :utf-8)
          do (if (or (<= 48 b 57) (<= 65 b 90) (<= 97 b 122) (member b '(45 46 95 126 47)))
                 (write-char (code-char b) o)
                 (format o "%~2,'0X" b)))))

;;; ---- the history: every page you went to ------------------------------------------
;;; Chrome keeps a history of its own, in an SQLite file in the profile, which we would
;;; need an SQLite reader to use. Ours is the simplest thing that answers "where did I
;;; go?": one plist per URL — (:url :title :visits :last) — newest visit first, in
;;; ~/.lol/web/history.lisp as one READable list (0600: what you read is private). It
;;; is written at most every +WEB-HISTORY-SAVE-EVERY+ seconds and when Chrome is shut
;;; down, not on every visit: the write is fsync'd, on the environment's thread. A crash
;;; can lose the last few seconds of it; nothing else. A diskless boot keeps it in
;;; memory only.

(defparameter +web-history-max+ 2000 "Pages kept; the least recent go first.")
(defparameter +web-history-save-every+ 10 "Seconds between two writes of the file.")

(defvar *web-history* :unread
  "The history, newest first, or :UNREAD until it is first needed (WEB-HISTORY).")
(defvar *web-history-dirty* nil "Changed since it was last written?")
(defvar *web-history-saved-at* 0 "GET-UNIVERSAL-TIME of the last write.")

(defun web-history-path () (lol-path "web/history.lisp"))

(defun web-history ()
  "The history, read from the disk the first time. Never signals: an unreadable file
   is an empty history (and is left alone — it is written over only after a visit)."
  (when (eq *web-history* :unread)
    (setf *web-history*
          (let ((path (web-history-path)))
            (or (and path (path-exists-p path)
                     (ignore-errors
                       (with-open-file (s (native-path path) :external-format :utf-8)
                         (let* ((*read-eval* nil) (form (read s nil nil)))
                           (and (listp form)
                                (remove-if-not (lambda (e) (and (listp e) (stringp (getf e :url))))
                                               form))))))
                '()))))
  *web-history*)

(defun web-history-save (&key force)
  "Write the history when it changed — at once with FORCE, else only when the last
   write is +WEB-HISTORY-SAVE-EVERY+ seconds old. Never signals."
  (let ((path (web-history-path)) (now (get-universal-time)))
    (when (and path *web-history-dirty* (listp *web-history*)
               (or force (>= (- now *web-history-saved-at*) +web-history-save-every+)))
      (ignore-errors
        (let ((dir (path-parent path)))
          (loop for d in (list (path-parent dir) dir)
                unless (path-exists-p d) do (make-directory d)))
        (write-file-atomically
         path (sb-ext:string-to-octets
               (with-standard-io-syntax
                 (let ((*print-readably* nil))
                   (format nil ";; LoL web history — newest first; safe to edit or delete.~%~s~%"
                           *web-history*)))
               :external-format :utf-8))
        (set-mode path #o600)
        (setf *web-history-dirty* nil *web-history-saved-at* now)))))

(defun web-history-keeps-p (url)
  "Is URL worth remembering? Pages, yes; Chrome's own and the blank page, data: and
   javascript: URLs, and the home page (the guide, always one key away), no."
  (and (stringp url) (plusp (length url))
       (not (some (lambda (p) (and (>= (length url) (length p)) (string-equal p url :end2 (length p))))
                  '("about:" "data:" "chrome:" "chrome-error:" "devtools:" "javascript:" "view-source:")))
       (not (equal url (web-home-url)))))

(defun web-history-note (url &key title)
  "Chrome went to URL: count a visit and move it to the front. With TITLE, only name
   the entry (the title arrives after the visit, once the page says it)."
  (when (web-history-keeps-p url)
    (let* ((h (web-history))
           (e (find url h :key (lambda (e) (getf e :url)) :test #'string=)))
      (cond (title
             (when (and e (string/= title (or (getf e :title) "")))
               (setf (getf e :title) title *web-history-dirty* t)))
            (t
             (let ((e (or e (list :url url :title nil :visits 0 :last 0))))
               (incf (getf e :visits))
               (setf (getf e :last) (get-universal-time))
               (setf *web-history* (cons e (remove e h :test #'eq))
                     *web-history-dirty* t)
               (when (> (length *web-history*) +web-history-max+)
                 (setf *web-history* (subseq *web-history* 0 +web-history-max+)))))))
    (web-history-save)))

(defun web-history-forget (url)
  "Remove URL from the history (Shift-Delete on its row), and write that at once —
   a page you asked to forget should not wait on the disk."
  (when (listp (web-history))
    (setf *web-history* (remove url *web-history* :key (lambda (e) (getf e :url)) :test #'string=)
          *web-history-dirty* t)
    (web-history-save :force t)))

(defun web-url-sans-scheme (url)
  "\"https://www.example.org/a\" -> \"example.org/a\": what a person types of it."
  (let* ((s (let ((p (search "://" url))) (if p (subseq url (+ p 3)) url))))
    (if (and (> (length s) 4) (string-equal "www." s :end2 4)) (subseq s 4) s)))

(defun web-history-matches (query &key (limit 50))
  "The history entries QUERY finds, best first. Every space-separated word of it must
   occur in the URL or the title, ignoring case. Best is a blend of how recent and how
   often (Firefox calls it frecency): each visit counts as a day more recent, up to a
   month's worth, and a page whose address STARTS with what you typed (\"git\" for
   github.com) comes first — that is the page you are most likely typing."
  (let* ((words (loop with s = (string-trim " " query) and start = 0
                      for sp = (position #\Space s :start start)
                      for w = (subseq s start sp)
                      when (plusp (length w)) collect w
                      while sp do (setf start (1+ sp))))
         (first-word (first words))
         (scored '()))
    (dolist (e (web-history))
      (let ((url (getf e :url)) (title (or (getf e :title) "")))
        (when (every (lambda (w) (or (search w url :test #'char-equal) (search w title :test #'char-equal)))
                     words)
          (let ((bare (web-url-sans-scheme url)))
            (push (cons (+ (getf e :last 0)
                           (* 86400 (min 30 (getf e :visits 1)))
                           (if (and first-word (>= (length bare) (length first-word))
                                    (string-equal first-word bare :end2 (length first-word)))
                               (* 86400 365) 0))
                        e)
                  scored)))))
    (let ((sorted (mapcar #'cdr (stable-sort (nreverse scored) #'> :key #'car))))
      (if (> (length sorted) limit) (subseq sorted 0 limit) sorted))))

;;; ---- the address prompt's candidates ----------------------------------------------
;;; What MINIBUFFER-READ's candidate panel (poc/shell.lisp) shows under C-l, decided by
;;; the first characters typed: a path (/ ~/ ./ ../ file://) lists the folder it is in,
;;; like a small file manager — folders first, then files with their size and date;
;;; anything else lists the history. Tab on a folder's row goes into it, Enter opens
;;; the row (a folder opens as Chrome's listing of it).

;;; One subtlety: the panel lists files as WE see them, but Chrome opens them inside its
;;; container (doc/containers.org), whose root is the host's Debian. The two views
;;; agree only where lol-ct.c binds our folders in — /home, /tmp, /dev, /sys. In the
;;; guest every other absolute path is listed from /mnt/host, the host's root as we
;;; mount it, so /usr/share/doc/ shows what Chrome will open there and not the
;;; initramfs's own few files. (The container's overlays can add to what /mnt/host
;;; shows; for a listing, that is close enough.)

(defun web-chrome-root ()
  "Where Chrome's / is in our file system when they differ: /mnt/host in the guest,
   NIL on the host (there Chrome runs in the same root as we do)."
  (and (web-in-guest-p) (path-exists-p "/mnt/host/usr") "/mnt/host"))

(defun web-shared-path-p (abs)
  "Is the absolute path ABS one Chrome's container sees exactly as we do?"
  (some (lambda (d) (or (string= abs d)
                        (and (> (length abs) (length d)) (string= d abs :end2 (length d))
                             (char= (char abs (length d)) #\/))))
        '("/home" "/tmp" "/dev" "/sys")))

(defun web-path-candidates (text)
  "The folder entries a path TEXT could complete to, as candidate plists; and the
   panel's title, which says which folder they are in. An absolute path outside the
   shared folders is listed from Chrome's root (WEB-CHROME-ROOT, above)."
  (let* ((root (and (char= (char text 0) #\/) (not (web-shared-path-p (web-expand-path text)))
                    (web-chrome-root)))
         (query (if root (concatenate 'string root text) text))
         (unroot (lambda (s) (if (and root (>= (length s) (length root)) (string= root s :end2 (length root)))
                                 (subseq s (length root)) s))))
    (multiple-value-bind (dir partial prefix) (parse-path-query query :home (home-directory))
      (let ((rows (path-completions query :home (home-directory) :limit 300)))
        (values
       (mapcar (lambda (r)
                 (destructuring-bind (name dir-p full label) r
                   (setf label (funcall unroot label))
                   (let ((e (and (not dir-p) (ignore-errors (file-entry full)))))
                     (list :label (if dir-p (concatenate 'string name "/") name)
                           :detail (cond (dir-p "folder")
                                         (e (format nil "~a  ~a"
                                                    (human-size (if (eq (fentry-type e) :symlink)
                                                                    (fentry-target-size e)
                                                                    (fentry-size e)))
                                                    (format-mtime (fentry-mtime e))))
                                         (t ""))
                           :text label
                           :group (if dir-p "Folders" "Files")
                           :stripe (and dir-p *ui-accent*)
                           :match (and (plusp (length partial)) partial)))))
               rows)
         ;; the folder as you typed it (~ stays ~), like the rows' own text
         (if dir (format nil "files in ~a" (funcall unroot prefix)) "no such folder"))))))

(defun web-address-candidates (tab)
  "The candidate function for TAB's address prompt (see above). The URL the prompt
   opened with lists nothing — it is the page you are on — until you edit it."
  (let ((opened-with (web-tab-url tab)))
    (lambda (text)
      (let ((q (string-trim " " text)))
        (cond
          ((path-query-p q) (web-path-candidates q))
          ((equal text opened-with) nil)
          (t (values
              (mapcar (lambda (e)
                        (let ((url (getf e :url)) (title (getf e :title)))
                          (list :label (if (and title (plusp (length title))) title (web-url-sans-scheme url))
                                :detail (format nil "~a~@[  · ~d visits~]" (web-url-sans-scheme url)
                                                (let ((v (getf e :visits 1))) (and (> v 1) v)))
                                :text url
                                :match (and (plusp (length q)) (not (find #\Space q)) q)
                                :forget (lambda () (web-history-forget url)))))
                      (web-history-matches q))
              (if (zerop (length q)) "recent pages" (format nil "history matching ~s" q)))))))))

;;; ---- starting Chrome ----------------------------------------------------------

(defun web-home-dir ()
  (let ((h (sb-ext:posix-getenv "HOME")))
    (string-right-trim "/" (if (and h (plusp (length h))) h "/tmp"))))

(defun web-profile-dir ()
  "~/.lol/web/chrome (LOL-PATH); on a diskless boot /tmp/lol-web/chrome, which the
   container shares with us (lol-ct.c binds /tmp)."
  (or *web-profile* (lol-path "web/chrome") "/tmp/lol-web/chrome"))

(defun web-downloads-dir () (format nil "~a/Downloads" (web-home-dir)))

(defun web-in-guest-p ()
  "Are we the image (PID 1 on the bare machine) rather than a host SBCL? The image has
   no /bin/sh of its own, which is exactly what decides how Chrome is launched."
  (not (probe-file "/bin/sh")))

(defun web-launch-prefix ()
  "How to start a program in the host's userland. On the host: directly. In the
   guest: through the containers module when it is loaded (CONTAINER-COMMAND-LINE,
   initramfs/containers.lisp — the host's Debian as the root, our /home, our network),
   and otherwise not at all: Chrome needs the host's whole userland (its libraries,
   fonts, fontconfig, /proc/self/exe), which only a container can hand it."
  (cond (*web-launcher*)
        ((not (web-in-guest-p)) #'identity)
        ;; its own container, "web", so Chrome's writes to / (none expected: its
        ;; profile is in /home, its scratch in /tmp and /dev/shm) never mix with the
        ;; ones a project's container keeps. lol-ct builds the program's environment
        ;; afresh, so what Chrome needs from ours travels as -e arguments.
        ((fboundp 'container-command-line)
         (lambda (argv)
           (funcall 'container-command-line argv :name "web"
                                                 :env (list "LANG=C.UTF-8"))))
        (t nil)))

(defun web-sandbox-p ()
  (case *web-sandbox*
    ((t) t)
    ((nil) nil)
    ;; :auto — on the host Chrome's own sandbox works as it does on any desktop; in
    ;; the guest it needs user namespaces INSIDE the container (tag #26 has USER_NS)
    ;; and Chrome refuses it when it runs as root of its namespace. We try it unless
    ;; told the container maps us to root.
    (t (not (and (boundp '*web-container-root*) (symbol-value '*web-container-root*))))))

(defun web-chrome-argv (w h)
  "The Chrome command line, in a W x H window. Each switch earns its place:"
  (append
   (list *web-chrome-binary*
         ;; the modern headless mode: the real browser, with no window — not the old
         ;; separate "headless shell", which renders differently
         "--headless=new"
         ;; CDP on fds 3/4 (web-cdp.lisp) instead of a TCP port
         "--remote-debugging-pipe"
         (format nil "--user-data-dir=~a" (web-profile-dir))
         ;; no welcome tab, no "make me your default browser", no sync promo
         "--no-first-run" "--no-default-browser-check"
         ;; saved passwords and cookie keys in the profile itself: there is no GNOME
         ;; keyring or KWallet in the image to hold them (doc/web-browser.org §7.3)
         "--password-store=basic"
         ;; the size the first frame is laid out at; the pane resizes it at once
         (format nil "--window-size=~d,~d" (max 200 w) (max 200 h))
         ;; nothing to talk to: no D-Bus, no GPU. These only quieten the log.
         "--disable-gpu" "--disable-features=MediaRouter,DialMediaRouteProvider")
   (unless (web-sandbox-p) (list "--no-sandbox"))
   *web-extra-args*
   (list "about:blank")))

(defun web-start-chrome (&optional (w 1024) (h 700))
  "Launch Chrome (asynchronously: the handshake finishes in WEB-SERVICE)."
  (let ((prefix (web-launch-prefix)))
    (unless prefix
      (setf *web-chrome-state* :failed
            *web-error* (concatenate 'string
                                     "Chrome runs from the host's Debian inside a container, "
                                     "and no container launcher is loaded "
                                     "(containers.lisp — doc/web-browser.org §6)."))
      (return-from web-start-chrome nil))
    (handler-case
        (progn
          (ensure-directories-exist (format nil "~a/" (web-profile-dir)))
          (setf *web-chrome* (start-chrome-process
                              (web-chrome-argv w h)
                              :prefix prefix :env (web-environment)
                              :log (format nil "/tmp/lol-chrome-~d.log" (sb-unix:unix-getpid)))
                *web-chrome-state* :starting
                *web-error* nil)
          (setf (chrome-on-event *web-chrome*) #'web-event)
          ;; the first answer is the handshake: once Chrome replies, it is up
          (cdp-send *web-chrome* "Browser.getVersion" nil :callback #'web-chrome-ready)
          t)
      (error (e)
        (setf *web-chrome-state* :failed *web-error* (format nil "could not start Chrome: ~a" e))
        nil))))

(defun web-environment ()
  "Chrome's environment: ours, plus a HOME (the container maps /home) and the XDG
   variables a desktop session would set, so its caches land under the profile's
   home rather than in /."
  (let ((home (web-home-dir)))
    (remove-duplicates
     (append (list (format nil "HOME=~a" home)
                   (format nil "XDG_CONFIG_HOME=~a/.config" home)
                   (format nil "XDG_CACHE_HOME=~a/.cache" home)
                   "LANG=C.UTF-8")
             (sb-ext:posix-environ))
     :key (lambda (s) (subseq s 0 (or (position #\= s) (length s))))
     :test #'string= :from-end t)))

(defun web-ua-metadata ()
  "The User-Agent Client Hints that go WITH *WEB-UA* (Emulation.setUserAgentOverride's
   userAgentMetadata). Overriding the UA string ALONE blanks them: navigator.userAgentData
   .brands becomes [] and no Sec-CH-UA header is sent — measured on Chrome 148 — and a
   Chrome that sends no client hints is an automation fingerprint. Google's sign-in
   refuses such a browser (\"This browser or app may not be secure\"). So we send the
   hints a desktop Chrome on Linux sends: the brands the headless Chrome itself
   reports (Chromium, Google Chrome, and the GREASE brand), its full version, Linux, x86."
  (let* ((full (or *web-chrome-version* "0.0.0.0"))
         (major (subseq full 0 (or (position #\. full) (length full)))))
    (flet ((brand (b v) (jobj "brand" b "version" v)))
      (jobj "brands" (vector (brand "Chromium" major) (brand "Google Chrome" major)
                             (brand "Not/A)Brand" "99"))
            "fullVersionList" (vector (brand "Chromium" full) (brand "Google Chrome" full)
                                      (brand "Not/A)Brand" "99.0.0.0"))
            "fullVersion" full
            "platform" "Linux" "platformVersion" "" "architecture" "x86" "bitness" "64"
            "model" "" "mobile" :false "wow64" :false))))

(defun web-chrome-ready (reply)
  "Chrome answered Browser.getVersion: it is up. Learn its user agent, turn on the
   events we live by, and open every tab that was waiting for it."
  (let ((ua (jget reply "result" "userAgent"))
        (product (jget reply "result" "product")))       ; "HeadlessChrome/148.0.7778.215"
    ;; Some sites refuse, or serve a lesser page to, a browser that says it is
    ;; HEADLESS. It is not, from the user's side, so we say what it is: Chrome — and
    ;; send the client hints to match (WEB-UA-METADATA), or Google sign-in refuses us.
    (setf *web-ua* (and ua (let ((i (search "HeadlessChrome" ua)))
                             (if i (concatenate 'string (subseq ua 0 i) (subseq ua (+ i 8))) ua)))
          *web-chrome-version* (and product (position #\/ product)
                                    (subseq product (1+ (position #\/ product))))))
  (setf *web-chrome-state* :ready *web-changed* t)
  (let ((c *web-chrome*))
    ;; Target.* events about every tab: created (a popup), info changed (a title),
    ;; destroyed (window.close())
    (cdp-send c "Target.setDiscoverTargets" (jobj "discover" t))
    ;; downloads go to ~/Downloads under their own names, and we hear about them
    (ensure-directories-exist (format nil "~a/" (web-downloads-dir)))
    (cdp-send c "Browser.setDownloadBehavior"
              (jobj "behavior" "allow" "downloadPath" (web-downloads-dir) "eventsEnabled" t))
    ;; The about:blank Chrome opened with is closed — our tabs are the ones we
    ;; create — and only THEN are the waiting tabs created, so the one list of
    ;; targets this asks for cannot contain one of ours.
    (cdp-send c "Target.getTargets" nil
              :callback (lambda (r)
                          (loop for ti across (or (jget r "result" "targetInfos") #())
                                when (equal (jget ti "type") "page")
                                  do (cdp-send c "Target.closeTarget"
                                               (jobj "targetId" (jget ti "targetId"))))
                          (dolist (tab *web-tabs*)
                            (when (member (web-tab-state tab) '(:new :dead))
                              (web-create-target tab)))))))

;;; ---- tabs: create, attach, adopt, close ----------------------------------------

(defun web-create-target (tab)
  "Ask Chrome for a new page for TAB, then attach to it (see WEB-ATTACH)."
  (setf (web-tab-state tab) :creating)
  (incf *web-creating*)
  (cdp-send *web-chrome* "Target.createTarget" (jobj "url" "about:blank")
            :callback (lambda (r)
                        (decf *web-creating*)
                        (let ((tid (jget r "result" "targetId")))
                          (if tid
                              (progn (setf (web-tab-target tab) tid) (web-attach tab))
                              (setf (web-tab-state tab) :dead
                                    (web-tab-status tab) (format nil "Chrome would not open a tab: ~a"
                                                                 (jget r "error" "message"))))))))

(defun web-attach (tab)
  "Attach to TAB's target. flatten: the tab's traffic then travels on OUR pipe tagged
   with its sessionId, instead of being nested inside Target.sendMessageToTarget."
  (cdp-send *web-chrome* "Target.attachToTarget" (jobj "targetId" (web-tab-target tab) "flatten" t)
            :callback (lambda (r)
                        (let ((sid (jget r "result" "sessionId")))
                          (if sid
                              (progn (setf (web-tab-session tab) sid (web-tab-state tab) :live)
                                     (web-tab-setup tab))
                              (setf (web-tab-state tab) :dead))))))

(defun web-tab-setup (tab)
  "A freshly attached tab: enable the event domains we read, present ourselves as
   Chrome, and load whatever it was opened for."
  (let ((c *web-chrome*) (s (web-tab-session tab)))
    (cdp-send c "Page.enable" nil :session s)
    ;; alert()/confirm()/prompt() pause the page until answered (WEB-EVENT)
    (cdp-send c "Page.setInterceptFileChooserDialog" (jobj "enabled" :false) :session s)
    ;; the UA string AND its client hints: the string alone blanks Sec-CH-UA, which
    ;; Google's sign-in reads as an automated browser (WEB-UA-METADATA)
    (when *web-ua*
      (cdp-send c "Emulation.setUserAgentOverride"
                (jobj "userAgent" *web-ua* "userAgentMetadata" (web-ua-metadata)) :session s))
    ;; the pickers headless Chrome draws where the screencast cannot see them
    ;; (WEB-PICKER-SETUP, "pickers" below)
    (web-picker-setup tab)
    (setf (web-tab-sent-w tab) 0 (web-tab-screencasting tab) nil)
    ;; an ADOPTED page (a popup) was already loading before we attached, so its
    ;; load events are gone: ask for what they would have told us
    (web-refresh-title tab)
    (web-refresh-history tab)
    (let ((url (shiftf (web-tab-open-url tab) nil)))
      (when (and url (not (string= url "about:blank")))
        (web-navigate tab url)))))

(defun web-new-tab (&optional url &key (focus t))
  "Open URL (or the home page) in a new web tab — a new trail — and start Chrome if
   this is the first. -> the WEB-TAB."
  (let ((tab (%make-web-tab :url (or url (web-home-url)) :open-url (or url (web-home-url)))))
    (push tab *web-tabs*)
    (let ((was *active*))
      (new-trail (subj-web tab))
      (unless focus (setf *active* was)))
    (case *web-chrome-state*
      ((:off :failed) (web-start-chrome))
      (:ready (web-create-target tab)))
    tab))

(defun web-adopt-target (tid)
  "A page WE did not create appeared (a popup, target=_blank, Ctrl-click): give it a
   tab and a trail, and attach."
  (let ((tab (%make-web-tab :url "" :target tid :state :creating)))
    (push tab *web-tabs*)
    (let ((was *active*))
      (new-trail (subj-web tab))
      (unless (shiftf *web-focus-next* t) (setf *active* was)))
    (web-attach tab)))

(defun web-tab-referenced-p (tab)
  (some (lambda (trail) (some (lambda (p) (eq (web-tab-of p) tab)) trail)) *trails*))

(defun web-collect-tabs ()
  "Close the Chrome page of every tab no trail holds any more (SuperL-w, the tab's ×,
   or leaving and re-entering the environment). This is how closing a web tab works
   without the shell knowing web tabs exist."
  (dolist (tab *web-tabs*)
    (unless (web-tab-referenced-p tab)
      (when (and *web-chrome* (web-tab-target tab) (eq (web-tab-state tab) :live))
        (ignore-errors (cdp-send *web-chrome* "Target.closeTarget"
                                 (jobj "targetId" (web-tab-target tab)))))
      (setf (web-tab-state tab) :dead
            *web-tabs* (remove tab *web-tabs*)))))

;;; ---- navigation -----------------------------------------------------------------

(defun web-send (tab method &optional params callback)
  "Send METHOD to TAB's page, if it is attached. -> T when sent."
  (when (and *web-chrome* (eq (web-tab-state tab) :live) (web-tab-session tab))
    (ignore-errors (cdp-send *web-chrome* method params :session (web-tab-session tab)
                                                        :callback callback))
    t))

(defun web-navigate (tab text)
  "Load what TEXT means (WEB-URL-FOR) in TAB."
  (let ((url (web-url-for text)))
    (setf (web-tab-url tab) url (web-tab-status tab) nil (web-tab-loading tab) t)
    (web-retitle tab)
    (unless (web-send tab "Page.navigate" (jobj "url" url)
                      (lambda (r)
                        (let ((err (or (jget r "result" "errorText") (jget r "error" "message"))))
                          (when err
                            (setf (web-tab-status tab) (format nil "~a: ~a" url err)
                                  (web-tab-loading tab) nil *web-changed* t)))))
      (setf (web-tab-open-url tab) url))))      ; not attached yet: load on attach

(defun web-refresh-title (tab)
  "Ask the page for its title. Target.targetInfoChanged carries titles too, but not
   reliably: for a file:// page Chrome 148 kept reporting the file name (verified with
   poc/web-test.lisp), so the title is also read straight from the document whenever
   a load settles."
  (web-send tab "Runtime.evaluate" (jobj "expression" "document.title" "returnByValue" t)
            (lambda (r)
              (let ((title (jget r "result" "result" "value")))
                (when (and (stringp title) (plusp (length title))
                           (not (equal title (web-tab-title tab))))
                  (setf (web-tab-title tab) title)
                  (web-history-note (web-tab-url tab) :title title)
                  (web-retitle tab))))))

(defun web-refresh-history (tab)
  (web-send tab "Page.getNavigationHistory" nil
            (lambda (r)
              (let ((res (jget r "result")))
                (when res
                  (setf (web-tab-history tab)
                        (cons (jget res "entries") (jget res "currentIndex"))
                        *web-changed* t))))))

(defun web-can-go-p (tab delta)
  (let ((h (web-tab-history tab)))
    (and h (< -1 (+ (cdr h) delta) (length (car h))))))

(defun web-go (tab delta)
  "Back (DELTA -1) or forward (+1) through TAB's session history. The history is
   asked for afresh rather than taken from the copy the toolbar draws from: that copy
   is refreshed when a load settles, and a key pressed before then would step from
   the page BEFORE the one on screen."
  (unless (web-send tab "Page.getNavigationHistory" nil
                    (lambda (r)
                      (let* ((entries (jget r "result" "entries"))
                             (i (jget r "result" "currentIndex")))
                        (when entries
                          (setf (web-tab-history tab) (cons entries i)))
                        (if (and entries i (< -1 (+ i delta) (length entries)))
                            (web-send tab "Page.navigateToHistoryEntry"
                                      (jobj "entryId" (jget (aref entries (+ i delta)) "id")))
                            (setf *browser-status* (if (minusp delta) "nothing to go back to"
                                                       "nothing to go forward to")
                                  *web-changed* t)))))
    (setf *browser-status* "this tab is not open in Chrome")))

(defun web-reload (tab)
  (setf (web-tab-loading tab) t *web-changed* t)
  (web-send tab "Page.reload" (jobj "ignoreCache" :false)))

(defun web-stop (tab)
  (web-send tab "Page.stopLoading")
  (setf (web-tab-loading tab) nil *web-changed* t))

;;; ---- events: what Chrome tells us ---------------------------------------------

(defun web-tab-by-session (session)
  (and session (find session *web-tabs* :key #'web-tab-session :test #'equal)))

(defun web-tab-by-target (tid)
  (and tid (find tid *web-tabs* :key #'web-tab-target :test #'equal)))

;; (before WEB-EVENT and WEB-SERVICE, their first users; the pickers section is below)
(defvar *web-picks* '()
  "(tab payload context-id) for every picker a page asked for, oldest first. Filled by
   WEB-EVENT, emptied by WEB-SERVICE: the chooser is MODAL, so it must not run while
   WEB-EVENT is still inside the dispatch loop of CDP-PUMP.")
(defparameter +web-picker-binding+ "__lolPicker"
  "The name of the binding the page script reports through.")
(defvar *web-pump-thread* nil
  "The thread that runs WEB-SERVICE, i.e. the one that reads Chrome's pipe.")

(defun web-event (method params session)
  "Chrome's events. Each case is a fact about a page; none of them draws — they set
   state, and WEB-SERVICE decides whether the screen needs redoing."
  (let ((tab (web-tab-by-session session)))
    (cond
      ;; A NEW FRAME. Acknowledge it at once: Chrome sends the next frame only after
      ;; the previous one was acked, which is the protocol's flow control — a slow
      ;; consumer (us, under QEMU) gets fewer frames rather than a backlog. Only the
      ;; newest undecoded frame is kept; a frame superseded before we drew it is
      ;; never decoded at all.
      ((string= method "Page.screencastFrame")
       (when tab
         (web-send tab "Page.screencastFrameAck" (jobj "sessionId" (jget params "sessionId")))
         (setf (web-tab-pending tab) (jget params "data")
               (web-tab-meta tab) (jget params "metadata"))
         (incf (web-tab-frames tab))
         ;; FRAME LATENCY: input sent -> the first frame after it arrives. What the
         ;; doc's measurements report (plus the decode, WEB-TAB-LAST-FRAME-MS).
         (let ((t0 (shiftf (web-tab-input-at tab) nil)))
           (when t0
             (push (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second)
                   (web-tab-latencies tab))
             (when (> (length (web-tab-latencies tab)) 32)
               (setf (web-tab-latencies tab) (subseq (web-tab-latencies tab) 0 32)))))))
      ;; titles and URLs, for every page — Target.setDiscoverTargets turned these on
      ((string= method "Target.targetInfoChanged")
       (let* ((ti (jget params "targetInfo")) (tb (web-tab-by-target (jget ti "targetId"))))
         (when tb
           (setf (web-tab-title tb) (jget ti "title"))
           (let ((u (jget ti "url"))) (when (and u (plusp (length u))) (setf (web-tab-url tb) u)))
           (web-retitle tb))))
      ;; A page we did not ask for: a popup, a target=_blank link, a Ctrl-click.
      ;; Such a page has an OPENER — the page that opened it. That test matters:
      ;; setDiscoverTargets also reports every page that already exists, Chrome's
      ;; own about:blank included, and those must not become tabs.
      ((string= method "Target.targetCreated")
       (let ((ti (jget params "targetInfo")))
         (when (and (equal (jget ti "type") "page")
                    (jget ti "openerId")
                    (zerop *web-creating*)
                    (eq *web-chrome-state* :ready)
                    (not (web-tab-by-target (jget ti "targetId"))))
           (push (jget ti "targetId") *web-adopt*))))
      ;; window.close(), or Chrome closing the page itself
      ((string= method "Target.targetDestroyed")
       (let ((tb (web-tab-by-target (jget params "targetId"))))
         (when (and tb (eq (web-tab-state tb) :live))
           (setf (web-tab-state tb) :dead (web-tab-status tb) "the page closed itself"
                 *web-changed* t))))
      ((string= method "Inspector.targetCrashed")
       (when tab
         (setf (web-tab-state tab) :crashed (web-tab-status tab) "the page crashed — r reloads it"
               *web-changed* t)))
      ((null tab) (web-browser-event method params))
      ((string= method "Page.frameStartedLoading")
       (when (equal (jget params "frameId") (web-tab-target tab))
         (setf (web-tab-loading tab) t *web-changed* t)))
      ((string= method "Page.frameStoppedLoading")
       (when (equal (jget params "frameId") (web-tab-target tab))
         (setf (web-tab-loading tab) nil *web-changed* t)
         (web-refresh-title tab)
         (web-refresh-history tab)))
      ((string= method "Page.domContentEventFired") (web-refresh-title tab))
      ((string= method "Page.frameNavigated")
       (let ((fr (jget params "frame")))
         (unless (jget fr "parentId")             ; the main frame, not an iframe
           (setf (web-tab-url tab) (jget fr "url"))
           (web-history-note (web-tab-url tab))
           (web-retitle tab)
           (web-refresh-history tab))))
      ((string= method "Page.navigatedWithinDocument")  ; a single-page app's pushState
       (setf (web-tab-url tab) (jget params "url"))
       (web-history-note (web-tab-url tab))
       (web-retitle tab)
       (web-refresh-title tab)
       (web-refresh-history tab))
      ((string= method "Page.javascriptDialogOpening")
       (web-dialog tab params))
      ;; the page script asked for a picker (a <select> was clicked): queued, and
      ;; shown by WEB-SERVICE once this dispatch is over (WEB-PICK)
      ((string= method "Runtime.bindingCalled")
       (when (equal (jget params "name") +web-picker-binding+)
         (setf *web-picks*
               (append *web-picks*
                       (list (list tab (jget params "payload")
                                   (jget params "executionContextId"))))))))))

(defun web-browser-event (method params)
  "Events that belong to the browser rather than to a tab: downloads."
  (cond
    ((string= method "Browser.downloadWillBegin")
     (push (cons (jget params "guid")
                 (list :name (jget params "suggestedFilename") :url (jget params "url")
                       :state "starting" :got 0 :total 0))
           *web-downloads*)
     (setf *browser-status* (format nil "downloading ~a to ~~/Downloads"
                                    (jget params "suggestedFilename"))
           *web-changed* t))
    ((string= method "Browser.downloadProgress")
     (let ((d (assoc (jget params "guid") *web-downloads* :test #'equal)))
       (when d
         (setf (getf (cdr d) :state) (jget params "state")
               (getf (cdr d) :got) (or (jget params "receivedBytes") 0)
               (getf (cdr d) :total) (or (jget params "totalBytes") 0))
         (let ((st (jget params "state")))
           (unless (equal st "inProgress")
             (setf *browser-status*
                   (format nil "download ~a: ~a~@[ (~/web-bytes/)~]"
                           (getf (cdr d) :name)
                           (if (equal st "completed") "saved in ~/Downloads" st)
                           (and (equal st "completed") (getf (cdr d) :got)))
                   *web-changed* t))))))))

(defun web-bytes (stream n &rest ignore)
  (declare (ignore ignore))
  (cond ((< n 1024) (format stream "~d B" n))
        ((< n (* 1024 1024)) (format stream "~,1f KB" (/ n 1024.0)))
        (t (format stream "~,1f MB" (/ n 1048576.0)))))

(defun web-dialog (tab params)
  "alert(), confirm(), prompt() and the leave-this-page question. The page is FROZEN
   until it is answered, so it is answered in the minibuffer, at once: Enter is OK
   (with the typed text, for a prompt), Esc / C-g is Cancel."
  (let* ((type (jget params "type"))
         (msg (substitute #\Space #\Newline (or (jget params "message") "")))
         (answer (lambda (accept &optional text)
                   (web-send tab "Page.handleJavaScriptDialog"
                             (apply #'jobj "accept" (if accept t :false)
                                    (and text (list "promptText" text)))))))
    (minibuffer-read (format nil "page ~a: ~a ~a" type (lol.widgets:clip-text msg 80)
                             (if (equal type "prompt") "" "[Enter OK / C-g cancel]"))
                     (lambda (text) (funcall answer t (and (equal type "prompt") text)))
                     :initial (if (equal type "prompt") (or (jget params "defaultPrompt") "") "")
                     :on-cancel (lambda () (funcall answer nil)))
    (setf *web-changed* t)))

;;; ---- the service: one turn of the browser's loop ---------------------------------

(defun web-chrome-gone ()
  "Chrome exited (crashed, was killed, or never really started): every tab is dead,
   and says so; the next thing that wants a page starts a new Chrome."
  (let ((log (and *web-chrome* (chrome-log *web-chrome*))))
    (ignore-errors (chrome-close *web-chrome* :grace 0))
    (setf *web-error* (format nil "Chrome exited~:[~; while starting~] — r restarts it~@[ (its log: ~a)~]"
                              (eq *web-chrome-state* :starting) log)
          *web-chrome* nil *web-chrome-state* :failed *web-changed* t)
    (dolist (tab *web-tabs*)
      (setf (web-tab-state tab) :dead (web-tab-session tab) nil (web-tab-screencasting tab) nil
            (web-tab-open-url tab) (web-tab-url tab)))))

(defun web-activate (tab)
  "Make TAB Chrome's foreground page. Without a window Chrome still keeps ONE page in
   front, the newest unless told otherwise, and only that one runs its compositor:
   a background page takes clicks and keys (the main thread handles those) but a
   mouseWheel sent to it never completes and the page never scrolls. Measured
   against the host's Chrome 148 on 2026-10-16: a second tab opened, the wheel on
   the first hung; after Target.activateTarget it scrolled 100 px. Called whenever
   a tab starts streaming, which is whenever it comes on screen."
  (when (and *web-chrome* (web-tab-target tab))
    (ignore-errors (cdp-send *web-chrome* "Target.activateTarget"
                             (jobj "targetId" (web-tab-target tab))))))

(defun web-sync-screencast (tab visible)
  "Keep TAB streaming exactly while it is VISIBLE, at the size its pane last drew.
   Hidden tabs cost nothing: no frames are encoded, sent or decoded for them."
  (cond
    ;; not drawn yet: the pane has not said how big it is, so there is nothing to ask for
    ((and visible (< (min (web-tab-want-w tab) (web-tab-want-h tab)) 50)) nil)
    ((not visible)
     (when (web-tab-screencasting tab)
       (web-send tab "Page.stopScreencast")
       (setf (web-tab-screencasting tab) nil)))
    ((or (/= (web-tab-want-w tab) (web-tab-sent-w tab))
         (/= (web-tab-want-h tab) (web-tab-sent-h tab))
         (/= *web-zoom* (web-tab-sent-zoom tab)))
     (let* ((z *web-zoom*) (w (web-tab-want-w tab)) (h (web-tab-want-h tab)))
       (when (and (> w 50) (> h 50))
         ;; THE VIEWPORT: lay the page out as if the window were the pane, in CSS
         ;; pixels (device pixels / zoom), and rasterise at Z device pixels each.
         (web-send tab "Emulation.setDeviceMetricsOverride"
                   (jobj "width" (floor w z) "height" (floor h z)
                         "deviceScaleFactor" z "mobile" :false))
         ;; THE STREAM, restarted at the new size: maxWidth/maxHeight cap the frame
         (when (web-tab-screencasting tab) (web-send tab "Page.stopScreencast"))
         (unless (web-tab-screencasting tab) (web-activate tab))
         (web-send tab "Page.startScreencast"
                   (jobj "format" "jpeg" "quality" *web-quality*
                         "maxWidth" w "maxHeight" h "everyNthFrame" 1))
         (setf (web-tab-screencasting tab) t (web-tab-cast-at tab) (get-internal-real-time)
               (web-tab-sent-w tab) w (web-tab-sent-h tab) h (web-tab-sent-zoom tab) z))))
    ((not (web-tab-screencasting tab))
     (web-activate tab)
     (web-send tab "Page.startScreencast"
               (jobj "format" "jpeg" "quality" *web-quality*
                     "maxWidth" (web-tab-want-w tab) "maxHeight" (web-tab-want-h tab)
                     "everyNthFrame" 1))
     (setf (web-tab-screencasting tab) t (web-tab-cast-at tab) (get-internal-real-time)))))

(defparameter +web-nudge-ms+ 700
  "How long a newly streaming tab may go without its first frame before we ask for one.")

(defun web-nudge (tab)
  "A screencast sends a frame when the page REPAINTS. Starting one usually forces a
   repaint, but not always: in about one run in twenty of poc/web-test.lisp a static
   page started streaming and sent nothing, leaving the pane blank until something
   moved. So a tab with no frame +WEB-NUDGE-MS+ after its stream started gets one
   screenshot (Page.captureScreenshot, the same JPEG) to show meanwhile."
  (when (and (null (web-tab-frame-px tab)) (null (web-tab-pending tab))
             (web-tab-screencasting tab) (not (web-tab-capturing tab))
             (> (- (get-internal-real-time) (web-tab-cast-at tab))
                (* +web-nudge-ms+ (/ internal-time-units-per-second 1000))))
    (setf (web-tab-capturing tab) t)
    (web-send tab "Page.captureScreenshot" (jobj "format" "jpeg" "quality" *web-quality*)
              (lambda (r)
                (setf (web-tab-capturing tab) nil)
                (let ((data (jget r "result" "data")))
                  (when (and data (null (web-tab-frame-px tab)) (null (web-tab-pending tab)))
                    (setf (web-tab-pending tab) data (web-tab-pending-sharp tab) t)))))))

;;; ---- page zoom: upscale, then sharpen (doc/web-browser.org §4.7) ----------------
;;; C-+ zooms the way Chrome's own Ctrl-+ does: the page is laid out in fewer CSS
;;; pixels (the pane's width / the zoom) and rasterised at ZOOM device pixels each
;;; (WEB-SYNC-SCREENCAST's deviceScaleFactor), so window.innerWidth shrinks and text
;;; is drawn larger, not enlarged. But a SCREENCAST frame comes at CSS-pixel size —
;;; Chrome ignores the scale factor there: measured with Chrome 148, a 1256 x 677
;;; pane at 150% gets 837 x 451 frames — and the blit is 1:1, so the page used to sit
;;; in the top-left two thirds of the pane with white around it. A SCREENSHOT does
;;; honour it (1256 x 677), but costs ~54 ms on the dev laptop: too slow for every
;;; frame. So each frame is scaled up at once (bilinear: soft, but the pane is full
;;; while you scroll), and once the page has been still for +WEB-SHARPEN-MS+ one
;;; screenshot replaces it, crisp.
;;;
;;; Ruled out, measured the same day: setDeviceMetricsOverride's "scale" changes
;;; nothing in the frames; CSS zoom on the root element gives full, crisp frames for
;;; free but is not Chrome's zoom (the page still sees the full-width viewport, so a
;;; narrow layout never kicks in), and was declined for that.

(defparameter +web-sharpen-ms+ 150
  "How long a zoomed page must go without a new frame before it is sharpened.")

(defun web-upscale (w h px tw th)
  "The W x H #x00RRGGBB pixels PX scaled to TW x TH, bilinear (each output pixel a
   weighted mix of the four source pixels around where it falls). Fixed point: 16
   fraction bits, so the inner loop is integer arithmetic only."
  (declare (type fixnum w h tw th) (type (simple-array (unsigned-byte 32) (*)) px)
           (optimize (speed 3) (safety 0)) (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let ((out (make-array (* tw th) :element-type '(unsigned-byte 32)))
        ;; the source step per output pixel, rounded so the last output pixel lands
        ;; on the last source pixel (floored, it fell short and the edge darkened)
        (sx (round (* (max 1 (1- w)) 65536) (max 1 (1- tw))))
        (sy (round (* (max 1 (1- h)) 65536) (max 1 (1- th)))))
    (declare (type fixnum sx sy))
    (dotimes (y th out)
      (let* ((fy (* y sy)) (y0 (min (1- h) (ash fy -16))) (y1 (min (1- h) (1+ y0)))
             (wy (logand fy #xFFFF)) (r0 (* y0 w)) (r1 (* y1 w)) (o (* y tw)))
        (declare (type fixnum fy y0 y1 wy r0 r1 o))
        (dotimes (x tw)
          (let* ((fx (* x sx)) (x0 (min (1- w) (ash fx -16))) (x1 (min (1- w) (1+ x0)))
                 (wx (logand fx #xFFFF))
                 (a (aref px (+ r0 x0))) (b (aref px (+ r0 x1)))
                 (c (aref px (+ r1 x0))) (d (aref px (+ r1 x1)))
                 (ix (- 65536 wx)) (iy (- 65536 wy)))
            (declare (type fixnum fx x0 x1 wx ix iy) (type (unsigned-byte 32) a b c d))
            ;; one channel: mix along x on both rows, then along y (written out three
            ;; times rather than looped over the shifts — the loop doubled the cost)
            (macrolet ((channel (shift)
                         `(let ((top (+ (* (ldb (byte 8 ,shift) a) ix) (* (ldb (byte 8 ,shift) b) wx)))
                                (bot (+ (* (ldb (byte 8 ,shift) c) ix) (* (ldb (byte 8 ,shift) d) wx))))
                            (declare (type fixnum top bot))
                            (min 255 (ash (+ (* (ash top -8) iy) (* (ash bot -8) wy) #x800000) -24)))))
              (setf (aref out (+ o x))
                    (logior (ash (channel 16) 16) (ash (channel 8) 8) (channel 0))))))))))

(defun web-pad (w h px tw th)
  "The W x H pixels PX grown to TW x TH by repeating the last column and row. For a
   screenshot of a zoomed page: Chrome renders floor(width / zoom) CSS pixels, so at
   150% a 1256-pixel pane gets a 1255-pixel picture, and resampling it to fill that
   last pixel would blur the crisp image the screenshot was taken for."
  (declare (type fixnum w h tw th) (type (simple-array (unsigned-byte 32) (*)) px))
  (let ((out (make-array (* tw th) :element-type '(unsigned-byte 32))))
    (dotimes (y th out)
      (let ((sy (min y (1- h))))
        (dotimes (x tw)
          (setf (aref out (+ (* y tw) x)) (aref px (+ (* sy w) (min x (1- w))))))))))

(defun web-zoomed-size (tab w h)
  "The size a W x H screencast frame of TAB should be shown at: the pane's page rect
   when the frame is that rect divided by the zoom (give or take a pixel of rounding
   per zoom step), else W x H as it is — a frame from before a resize, which the blit
   shows 1:1 rather than stretching a stale picture."
  (let ((z (web-tab-sent-zoom tab)) (ww (web-tab-want-w tab)) (wh (web-tab-want-h tab)))
    (if (and (/= z 1) (< w ww)
             (<= (abs (- (* w z) ww)) (+ 2 z)) (<= (abs (- (* h z) wh)) (+ 2 z)))
        (values ww wh)
        (values w h))))

(defun web-sharpen (tab)
  "A zoomed page showing an upscaled frame (WEB-TAB-SOFT) that has been still for
   +WEB-SHARPEN-MS+ gets one screenshot, which Chrome renders at device size. It is
   kept only if no newer frame arrived while it was being taken: a page that moved
   on stays with its newer (soft) frame and is sharpened when it stops again."
  (when (and (web-tab-soft tab) (not (web-tab-capturing tab)) (null (web-tab-pending tab))
             (eq (web-tab-state tab) :live)
             (> (- (get-internal-real-time) (web-tab-frame-at tab))
                (* +web-sharpen-ms+ (/ internal-time-units-per-second 1000))))
    (let ((asked-at (web-tab-frame-at tab)) (zoom (web-tab-sent-zoom tab)))
      (setf (web-tab-capturing tab) t)
      (web-send tab "Page.captureScreenshot" (jobj "format" "jpeg" "quality" *web-quality*)
                (lambda (r)
                  (setf (web-tab-capturing tab) nil)
                  (let ((data (jget r "result" "data")))
                    (if (and data (= asked-at (web-tab-frame-at tab))
                             (= zoom (web-tab-sent-zoom tab)) (null (web-tab-pending tab)))
                        (setf (web-tab-pending tab) data (web-tab-pending-sharp tab) t)
                        ;; superseded: try again once the page is still
                        nil)))))))

(defun web-decode-pending (tab)
  "Decode TAB's newest frame, if one is waiting. -> T when the frame changed.
   base64 -> JPEG octets -> stb_image -> #x00RRGGBB pixels, the canvas's own format."
  (let ((data (shiftf (web-tab-pending tab) nil))
        (sharp (shiftf (web-tab-pending-sharp tab) nil)))
    (when data
      (let ((t0 (get-internal-real-time)))
        (multiple-value-bind (w h px) (lol.image:decode-image (base64-decode data) :orient nil)
          (when w
            ;; a zoomed page's screencast frame is CSS-sized: scale it up to the pane
            ;; (§4.7); a screenshot is already device-sized
            (multiple-value-bind (tw th) (if sharp (values w h) (web-zoomed-size tab w h))
              (let ((soft (and (not sharp) (or (/= tw w) (/= th h)))))
                (when soft (setf px (web-upscale w h px tw th) w tw h th))
                ;; a sharp one a pixel or two short of the pane (rounding): pad it
                (let ((ww (web-tab-want-w tab)) (wh (web-tab-want-h tab)) (z (web-tab-sent-zoom tab)))
                  (when (and sharp (/= z 1) (<= 0 (- ww w) 2) (<= 0 (- wh h) 2)
                             (or (< w ww) (< h wh)))
                    (setf px (web-pad w h px ww wh) w ww h wh)))
                (setf (web-tab-soft tab) soft)
                (unless sharp (setf (web-tab-frame-at tab) (get-internal-real-time)))))
            (setf (web-tab-frame-w tab) w (web-tab-frame-h tab) h (web-tab-frame-px tab) px
                  (web-tab-last-frame-ms tab)
                  (round (* 1000 (- (get-internal-real-time) t0)) internal-time-units-per-second))
            t))))))

(defun web-service ()
  "One turn: talk to Chrome, keep the visible tab streaming, decode its new frame.
   -> :REDRAW when the pane's chrome changed (redraw the browser), :FRAME when only
   the page's pixels did (WEB-PRESENT-FRAME can send just that rectangle), else NIL."
  (setf *web-pump-thread* sb-thread:*current-thread*)
  (let ((c *web-chrome*) (frame nil))
    (when c
      (when (or (eq (ignore-errors (cdp-pump c)) :eof) (not (chrome-alive-p c)))
        (web-chrome-gone)
        (return-from web-service :redraw))
      (loop for tid = (pop *web-adopt*) while tid
            do (unless (web-tab-by-target tid) (web-adopt-target tid) (setf *web-changed* t)))
      (web-collect-tabs)
      (let ((vis (web-visible-tab)))
        (dolist (tab *web-tabs*)
          (when (eq (web-tab-state tab) :live)
            (web-sync-screencast tab (eq tab vis))))
        (when vis (web-nudge vis) (web-sharpen vis))
        (when (and vis (web-decode-pending vis)) (setf frame t))
        ;; a picker the page asked for, shown now that no dispatch is running
        (loop for (tab payload ctx) = (pop *web-picks*)
              while tab
              do (when (and (eq tab vis) (eq (web-tab-state tab) :live))
                   (web-pick tab payload ctx)
                   (setf *web-changed* t)))))
    (web-reap-orphans)
    (cond ((shiftf *web-changed* nil) :redraw)
          (frame :frame))))

(defun web-js (expression &key (tab (web-visible-tab)) (timeout 10))
  "Evaluate the JavaScript EXPRESSION in TAB's page and return its value — for tests
   and the REPL. From the thread that reads the pipe this simply waits for the reply
   (CDP-EVAL); from any OTHER thread (the net REPL) reading the pipe would race the
   browser's loop, so it sends with a callback and waits for the loop to deliver it."
  (unless (and tab (web-tab-session tab) *web-chrome*) (error "no live web tab"))
  (if (or (null *web-pump-thread*) (eq *web-pump-thread* sb-thread:*current-thread*))
      (cdp-eval *web-chrome* (web-tab-session tab) expression :timeout timeout)
      (let ((box (list :waiting)))
        (cdp-send *web-chrome* "Runtime.evaluate"
                  (jobj "expression" expression "returnByValue" t "awaitPromise" t)
                  :session (web-tab-session tab)
                  :callback (lambda (r) (setf (car box) r)))
        (loop repeat (* timeout 50) while (eq (car box) :waiting) do (sleep 0.02))
        (when (eq (car box) :waiting) (error "no answer from the page in ~ds" timeout))
        (let ((r (car box)))
          (when (jget r "result" "exceptionDetails")
            (error "page script failed: ~a" (jget r "result" "exceptionDetails" "text")))
          (jget r "result" "result" "value")))))

(defun web-wait-fd ()
  "The fd the browser's poll should watch for us (Chrome's end of the pipe), or NIL."
  (and *web-chrome* (chrome-from-fd *web-chrome*)))

(defun web-timeout ()
  "How soon the loop must run WEB-SERVICE even with nothing to read: never (-1) when
   all is quiet — a frame or a title arrives on the pipe, which wakes the poll. A tab
   waiting to have its size synced needs a prompt second look, one waiting for its
   first frame a slower one, and a zoomed page showing an upscaled frame one when it
   is due to be sharpened."
  (let ((vis (web-visible-tab)))
    (if (and vis (eq (web-tab-state vis) :live)
             (or (/= (web-tab-want-w vis) (web-tab-sent-w vis))
                 (/= (web-tab-want-h vis) (web-tab-sent-h vis))
                 (not (web-tab-screencasting vis))))
        10
        (cond ((and vis (eq (web-tab-state vis) :live) (null (web-tab-frame-px vis)))
               100)                      ; waiting for a first frame: see WEB-NUDGE
              ((and vis (web-tab-soft vis) (not (web-tab-capturing vis)))
               ;; a zoomed page showing an upscaled frame: wake when it is due to be
               ;; sharpened (WEB-SHARPEN)
               (max 1 (- +web-sharpen-ms+
                         (round (* 1000 (- (get-internal-real-time) (web-tab-frame-at vis)))
                                internal-time-units-per-second))))
              (t -1)))))

;;; ---- reaping: PID 1's duty to Chrome's orphans --------------------------------
;;; When a process dies its children are re-parented to PID 1, and a dead child stays
;;; a ZOMBIE (a process-table entry holding its exit status) until its parent WAITs
;;; for it. SBCL's RUN-PROGRAM waits for the processes it started, and only those. A
;;; Chrome that runs outside a PID namespace (on the host there is no problem: the
;;; host's init reaps; in a container, the container's init does) can leave
;;; grandchildren — renderers, the zygote — to us. So: look at every waitable child
;;; WITHOUT reaping it (waitid with WNOWAIT), and reap it only if SBCL does not know
;;; it, so RUN-PROGRAM's own bookkeeping (exit codes) is never stolen from.

(sb-alien:define-alien-routine ("waitid" %web-waitid) sb-alien:int
  (idtype sb-alien:int) (id sb-alien:int) (info (* sb-alien:unsigned-char)) (options sb-alien:int))
(sb-alien:define-alien-routine ("waitpid" %web-waitpid) sb-alien:int
  (pid sb-alien:int) (status (* sb-alien:int)) (options sb-alien:int))

(defun web-reap-orphans ()
  "Reap every exited child SBCL did not start. -> how many. Only PID 1 inherits
   orphans, so elsewhere this finds nothing."
  (let ((n 0))
    (when (= 1 (sb-unix:unix-getpid))
      (sb-alien:with-alien ((info (sb-alien:array sb-alien:unsigned-char 128)))
        (loop
          (dotimes (i 128) (setf (sb-alien:deref info i) 0))
          ;; P_ALL (0), WEXITED (4) | WNOHANG (1) | WNOWAIT (#x01000000): peek only
          (when (minusp (%web-waitid 0 0 (sb-alien:cast info (* sb-alien:unsigned-char))
                                     (logior 4 1 #x01000000)))
            (return))
          ;; siginfo_t: si_pid is the int at byte 16 on x86-64; 0 = nobody waitable
          (let ((pid (logior (sb-alien:deref info 16) (ash (sb-alien:deref info 17) 8)
                             (ash (sb-alien:deref info 18) 16) (ash (sb-alien:deref info 19) 24))))
            (when (or (zerop pid)
                      (find pid (ignore-errors (symbol-value (find-symbol "*ACTIVE-PROCESSES*" "SB-IMPL")))
                            :key #'sb-ext:process-pid)
                      ;; a terminal's program (pty.lisp spawns those itself and waits
                      ;; for them, to show "exited with code N"; doc/terminal.org)
                      (and (fboundp 'pty-child-p) (funcall 'pty-child-p pid)))
              (return))                   ; nothing, or SBCL's own: its handler reaps it
            (%web-waitpid pid nil 1)
            (incf n)))))
    n))

;;; ---- drawing ---------------------------------------------------------------------

(defun web-bar-h () (+ (lol.canvas:font-ch *bfont*) 10))

(defun draw-web-button (canvas label x y h enabled)
  "A text button; -> its right edge."
  (let* ((cw (lol.canvas:font-cw *bfont*))
         (w (+ 12 (* cw (length label)))))
    (lol.canvas:fill-rect canvas x y w h *ui-panel*)
    (lol.canvas:draw-rect canvas x y w h *ui-rule*)
    (lol.canvas:draw-string canvas *bfont* label (+ x 6) (+ y 5) (if enabled *ui-text* *ui-dim*))
    (+ x w)))

(defun draw-web-body (canvas pane x y w h)
  "Render a :WEB view: the toolbar, then the page's newest frame, scaled 1:1."
  (let* ((tab (web-tab-of pane))
         (bh (web-bar-h))
         (cw (lol.canvas:font-cw *bfont*))
         (gap 6) (bx x)
         (rects '()))
    ;; --- the toolbar: back, forward, reload or stop, the address ---
    (lol.canvas:fill-rect canvas x y w bh *ui-bg*)
    (flet ((button (key label enabled)
             (let ((x1 (draw-web-button canvas label bx (+ y 2) (- bh 4) enabled)))
               (push (list key bx x1) rects)
               (setf bx (+ x1 gap)))))
      (button :back "<" (web-can-go-p tab -1))
      (button :forward ">" (web-can-go-p tab 1))
      (if (web-tab-loading tab)
          (button :stop "stop" t)
          (button :reload "reload" t)))
    (let* ((ax bx) (aw (- (+ x w) ax))
           (url (web-tab-url tab))
           (secure (and (>= (length url) 8) (string-equal "https://" url :end2 8))))
      (lol.canvas:fill-rect canvas ax (+ y 2) aw (- bh 4) *ui-panel*)
      (lol.canvas:draw-rect canvas ax (+ y 2) aw (- bh 4) *ui-rule*)
      (lol.canvas:draw-string canvas *bfont*
                              (clip-text url (max 4 (floor (- aw 12) cw)))
                              (+ ax 6) (+ y 7) (if secure *ui-text* *ui-dim*))
      (push (list :address ax (+ ax aw)) rects))
    (setf (web-tab-bar-rects tab) rects (web-tab-bar-y tab) (cons y (+ y bh)))
    ;; --- the page ---
    (let* ((py (+ y bh 2)) (ph (- h bh 2)))
      (setf (web-tab-page-rect tab) (list x py w ph)
            (web-tab-want-w tab) w (web-tab-want-h tab) ph)
      (if (web-tab-frame-px tab)
          (web-blit canvas tab)
          (progn
            (lol.canvas:fill-rect canvas x py w ph !#FFFFFF)
            (lol.canvas:draw-string canvas *bfont* (web-placeholder tab) (+ x 12) (+ py 12)
                                    !#606060)))
      ;; a status for this tab (an error, a crash) sits over the page's foot
      (let ((msg (or (web-tab-status tab)
                     (and (member *web-chrome-state* '(:failed)) *web-error*))))
        (when msg
          (lol.canvas:fill-rect canvas x (- (+ py ph) +bar-h+) w +bar-h+ *ui-hi*)  ; font-sized strip
          (lol.canvas:draw-string canvas *bfont* (clip-text msg (max 4 (floor (- w 12) cw)))
                                  (+ x 6) (+ (- (+ py ph) +bar-h+) (text-dy +bar-h+)) *ui-amber*))))))

(defmethod subject-signature ((s subj-web) pane)
  "What a web pane draws beyond the page's frame (which the service paints itself):
   the toolbar — back, forward, reload or stop, the address — and the placeholder or
   the status over the page's foot. When one of these changes the desk draws the pane
   again (poc/morphic.lisp)."
  (declare (ignore pane))
  (let ((tab (subj-web-tab s)))
    (list (web-tab-url tab) (web-tab-loading tab) (web-tab-state tab) (web-tab-status tab)
          (web-can-go-p tab -1) (web-can-go-p tab 1) *web-chrome-state* *web-error*
          (null (web-tab-frame-px tab)))))

(defun web-placeholder (tab)
  "What an empty page area says while there is no frame yet."
  (case *web-chrome-state*
    (:off "Chrome is not running.")
    (:starting "starting Chrome ...")
    (:failed (or *web-error* "Chrome is not running."))
    (t (case (web-tab-state tab)
         ((:new :creating) "opening a tab ...")
         (:dead (or (web-tab-status tab) "this tab is closed — r reopens it"))
         (:crashed "the page crashed — r reloads it")
         (t (format nil "loading ~a ..." (web-tab-url tab)))))))

(defun web-blit (canvas tab)
  "Copy TAB's frame into the page rect: 1:1, top-left anchored, clipped; whatever the
   frame does not cover (it lags a resize by one frame) is page-white."
  (destructuring-bind (x y w h) (web-tab-page-rect tab)
    (let* ((fw (web-tab-frame-w tab)) (fh (web-tab-frame-h tab))
           (src (web-tab-frame-px tab))
           (cw (lol.canvas:canvas-w canvas))
           (cols (max 0 (min fw w (- cw x))))
           (rows (max 0 (min fh h (- (lol.canvas:canvas-h canvas) y)))))
      (declare (type (simple-array (unsigned-byte 32) (*)) src)
               (type fixnum fw cols rows cw x y))
      ;; through BLIT-PIXELS, so the copy honours the canvas's clip (a damage repair,
      ;; a window over the page — doc/morphic.org §4.1); row by row it is the same
      ;; REPLACE this used to do itself
      (lol.canvas:blit-pixels canvas src fw fh x y :w cols :h rows)
      (when (< cols w) (lol.canvas:fill-rect canvas (+ x cols) y (- w cols) h !#FFFFFF))
      (when (< rows h) (lol.canvas:fill-rect canvas x (+ y rows) cols (- h rows) !#FFFFFF)))))

(defparameter +web-selection-js+
  "(function(){var a=document.activeElement;
     if(a&&(a.tagName=='TEXTAREA'||a.tagName=='INPUT')&&typeof a.selectionStart=='number')
       return a.value.substring(a.selectionStart,a.selectionEnd);
     return String(window.getSelection());})()"
  "The page's selected text: the selection inside a focused text field (which
   getSelection() does not see), else the document's.")

;;; ---- pickers: the popups headless Chrome draws where we cannot see them ---------
;;; A <select>'s list of options is not part of the PAGE. Chrome draws it as a
;;; separate POPUP WIDGET — on a desktop, a little borderless window of its own, so it
;;; can hang below the page's edge — with its own compositor frame. Page.startScreencast
;;; streams the page's frames only, so in a headless Chrome the popup opens (the
;;; <select> matches :open, and it takes the keys) and is never in a frame we get.
;;; Measured in the guest, 2026-10-13 (Chrome 148): after a real click the select
;;; matched :open, the screencast frame held 0 pixels of the popup's highlight blue,
;;; and Page.captureScreenshot — which does composite popups — held 2809
;;; (doc/web-browser.org §4.4). <input type=color> is the same; date/time inputs and <datalist> open
;;; NOTHING headless — Chrome has no picker UI to show.
;;;
;;; So we draw them. A page script, installed into every document (Page.
;;; addScriptToEvaluateOnNewDocument, and once into the one already loaded), catches
;;; the gesture that would open a popup in the CAPTURE phase — before the page's own
;;; handlers, before Chrome's default action — cancels it, and reports the control
;;; through a BINDING: Runtime.addBinding puts a function on the page's window whose
;;; every call becomes a Runtime.bindingCalled event on our pipe, carrying one string.
;;; We show our own chooser (the right-button menu's card, poc/overlays.lisp:
;;; RUN-CHOOSER with kind :SELECT) or the minibuffer, and write the answer back with
;;; Runtime.evaluate IN THE SAME FRAME (the event names its execution context), firing
;;; the input and change events a real choice fires.

(defparameter +web-picker-js+
  "(function(){
if (window.__lolPickerInstalled) return;
window.__lolPickerInstalled = true;
/* element <-> small integer, so Lisp can name the control it answers about */
var ids = new WeakMap(), els = new Map(), next = 1;
function idOf(el){ var i = ids.get(el); if (!i) { i = next++; ids.set(el, i); }
  els.set(i, new WeakRef(el)); return i; }
/* the control's box in TOP-LEVEL viewport CSS px: up through same-origin frames,
   then the pinch-zoom of the visual viewport (1 here: we never pinch) */
function where(el){
  var r = el.getBoundingClientRect(), x = r.left, y = r.top, w = window;
  try { while (w.frameElement) {
    var f = w.frameElement.getBoundingClientRect(), cs = w.parent.getComputedStyle(w.frameElement);
    x += f.left + parseFloat(cs.borderLeftWidth) + parseFloat(cs.paddingLeft);
    y += f.top + parseFloat(cs.borderTopWidth) + parseFloat(cs.paddingTop);
    w = w.parent; } } catch (e) {}
  var vv = w.visualViewport, s = vv ? vv.scale : 1;
  return { x: (x - (vv ? vv.offsetLeft : 0)) * s, y: (y - (vv ? vv.offsetTop : 0)) * s,
           w: r.width * s, h: r.height * s };
}
function send(el, kind, extra){
  var m = where(el); m.kind = kind; m.id = idOf(el); m.value = el.value;
  for (var k in extra) m[k] = extra[k];
  try { window.__lolPicker(JSON.stringify(m)); } catch (e) {}
}
function selectable(el){ return el && el.tagName === 'SELECT' && !el.multiple && !el.disabled && !(el.size > 1); }
function options(el){
  var out = [];
  for (var i = 0; i < el.options.length; i++) {
    var o = el.options[i], g = o.parentNode && o.parentNode.tagName === 'OPTGROUP' ? o.parentNode : null;
    if (o.hidden) continue;
    out.push({ i: i, label: o.label || o.text, value: o.value,
               disabled: !!(o.disabled || (g && g.disabled)), group: g ? g.label : null });
  }
  return out;
}
function openSelect(el){ el.focus(); send(el, 'select', { sel: el.selectedIndex, options: options(el) }); }
var DATES = { date: 'yyyy-mm-dd', time: 'hh:mm', 'datetime-local': 'yyyy-mm-ddThh:mm',
              month: 'yyyy-mm', week: 'yyyy-Www' };
function pickerKind(el){
  if (!el || el.tagName !== 'INPUT' || el.disabled || el.readOnly) return null;
  if (el.type === 'color') return 'color';
  return DATES[el.type] ? 'date' : null;
}
/* A datalist's suggestions are browser UI, not page: headless they are invisible, and
   once typing opens them they swallow the arrow keys. Chrome only offers them to a
   FOCUSED input with a list attribute, so while one is focused the attribute is put
   aside (and put back on blur) and our own list stands in for Chrome's. */
function listOf(el){ return el.list || (el.__lolList ? document.getElementById(el.__lolList) : null); }
function hasList(el){ return el && el.tagName === 'INPUT' && !el.disabled && !el.readOnly && !!listOf(el); }
document.addEventListener('focusin', function(e){
  var t = e.target;
  if (t && t.tagName === 'INPUT' && t.getAttribute('list')) {
    t.__lolList = t.getAttribute('list'); t.removeAttribute('list'); }
}, true);
document.addEventListener('focusout', function(e){
  var t = e.target;
  if (t && t.__lolList) { t.setAttribute('list', t.__lolList); t.__lolList = null; }
}, true);
function openList(el){
  var v = el.value.toLowerCase(), out = [], all = listOf(el).options;
  for (var i = 0; i < all.length; i++) {
    var o = all[i], lab = o.label && o.label !== o.value ? o.value + '  ' + o.label : o.value;
    if (o.disabled) continue;
    if (!v || lab.toLowerCase().indexOf(v) >= 0) out.push({ i: i, label: lab, value: o.value, disabled: false, group: null });
  }
  if (!out.length) return false;
  send(el, 'datalist', { sel: -1, options: out }); return true;
}
/* the little arrow / calendar icon at the right end of the box */
function onIcon(el, e){ var r = el.getBoundingClientRect(); return e.clientX > r.right - Math.max(24, r.height); }
window.addEventListener('mousedown', function(e){
  if (e.button !== 0) return;
  var t = e.target, sel = t && t.closest ? t.closest('select') : null;
  if (selectable(sel)) { e.preventDefault(); openSelect(sel); return; }
  var k = pickerKind(t);
  if (k === 'color' || (k === 'date' && onIcon(t, e))) {
    e.preventDefault(); t.focus(); send(t, k, { type: t.type, format: DATES[t.type] || '#rrggbb' }); return; }
  if (hasList(t) && onIcon(t, e)) { t.focus(); if (openList(t)) e.preventDefault(); }
}, true);
/* a colour input opens its chooser on CLICK (activation), not on mousedown: that is
   cancelled too, or the invisible chooser opens anyway and swallows every key */
window.addEventListener('click', function(e){
  if (pickerKind(e.target) === 'color') e.preventDefault();
}, true);
window.addEventListener('keydown', function(e){
  var el = e.target, plain = !e.ctrlKey && !e.altKey && !e.metaKey;
  var opener = (e.altKey && (e.key === 'ArrowDown' || e.key === 'ArrowUp')) || e.key === 'F4';
  if (selectable(el) && (opener || (plain && (e.key === ' ' || e.key === 'Enter')))) {
    e.preventDefault(); openSelect(el); return; }
  var k = pickerKind(el);
  if (k && (opener || (k === 'color' && plain && (e.key === ' ' || e.key === 'Enter')))) {
    e.preventDefault(); send(el, k, { type: el.type, format: DATES[el.type] || '#rrggbb' }); return; }
  if (hasList(el) && (opener || (plain && e.key === 'ArrowDown')) && openList(el)) e.preventDefault();
}, true);
/* Lisp's answer: V is {index, value} for a list, {value} for a typed one */
window.__lolPickerApply = function(id, v){
  var r = els.get(id), el = r && r.deref();
  if (!el || !el.isConnected) return 'gone';
  if (el.tagName === 'SELECT') {
    var i = v.index;
    if (!(el.options[i] && el.options[i].value === v.value)) {
      i = -1; for (var j = 0; j < el.options.length; j++) if (el.options[j].value === v.value) { i = j; break; } }
    if (i < 0) return 'gone';
    el.focus();
    if (el.selectedIndex === i) return 'same';
    el.selectedIndex = i;
  } else {
    var before = el.value;
    el.value = v.value;
    if (v.value !== '' && el.value.toLowerCase() !== String(v.value).toLowerCase()) { el.value = before; return 'invalid'; }
    el.focus();
    if (el.value === before) return 'same';
  }
  el.dispatchEvent(new Event('input', { bubbles: true, composed: true }));
  el.dispatchEvent(new Event('change', { bubbles: true }));
  return 'ok';
};
})();"
  "The page script behind the pickers (see the section's header). Plain JavaScript in
   the page's own world, written to be read: the capture-phase listeners on WINDOW run
   before any of the page's, and only cancel the gestures that would open a popup.")

(defun web-picker-setup (tab)
  "Give TAB's page the binding and the script — for every document it will load, and
   for the one it already has (an adopted popup was loading before we attached)."
  (let ((s (web-tab-session tab)) (c *web-chrome*))
    ;; Runtime.bindingCalled is an event of the Runtime domain, and like every
    ;; domain's events it is only sent once the domain is enabled (without this the
    ;; script ran, cancelled the click, and its report went nowhere — the first run
    ;; of poc/web-test.lisp's picker checks). The cost: an executionContextCreated
    ;; per frame and a consoleAPICalled per console.log, which WEB-EVENT ignores.
    (cdp-send c "Runtime.enable" nil :session s)
    (cdp-send c "Runtime.addBinding" (jobj "name" +web-picker-binding+) :session s)
    (cdp-send c "Page.addScriptToEvaluateOnNewDocument" (jobj "source" +web-picker-js+) :session s)
    (cdp-send c "Runtime.evaluate" (jobj "expression" +web-picker-js+) :session s)))

(defun web-pick-anchor (tab msg)
  "The rectangle of the control MSG reports in TAB -> (values x y w h), from its
   CSS-pixel box: the page's origin plus CSS px times *WEB-ZOOM*, the same mapping
   WEB-PAGE-POINT inverts. In the coordinates TAB's page was drawn in."
  (destructuring-bind (px py pw ph) (or (web-tab-page-rect tab) (list 0 0 0 0))
    (declare (ignore pw ph))
    (flet ((n (k) (let ((v (jget msg k))) (if (realp v) v 0))))
      (values (round (+ px (* *web-zoom* (n "x"))))
              (round (+ py (* *web-zoom* (n "y"))))
              (round (* *web-zoom* (n "w")))
              (round (* *web-zoom* (n "h")))))))

(defun web-pick-entries (msg)
  "MSG's options as chooser entries: the label, the <optgroup> as the section, dim
   and unpickable when disabled, the current one :SELECTED. The entry's :ACTION is the
   (index . value) to write back."
  (loop for o across (or (jget msg "options") #())
        for i = (jget o "i")
        for group = (jget o "group")
        collect (list :label (let ((l (jget o "label"))) (if (stringp l) l ""))
                      :action (cons i (jget o "value"))
                      :group (and (stringp group) group)
                      :disabled (eq (jget o "disabled") t)
                      :selected (eql i (jget msg "sel")))))

(defun web-pick-apply (tab ctx id answer)
  "Write ANSWER (a JOBJ: index and value, or value alone) to control ID, in the frame
   whose execution context is CTX. The page answers ok / same / invalid / gone."
  (web-send tab "Runtime.evaluate"
            (apply #'jobj "expression"
                   (format nil "window.__lolPickerApply(~d, ~a)" id (json-string answer))
                   "returnByValue" t
                   (and ctx (list "contextId" ctx)))
            (lambda (r)
              (let ((v (jget r "result" "result" "value")))
                (cond ((equal v "invalid")
                       (setf *browser-status* "the page refused that value"))
                      ((not (member v '("ok" "same") :test #'equal))
                       (setf *browser-status* "that control has gone from the page")))
                (setf *web-changed* t)))))

(defun web-press ()
  "The press sent into a page whose release has not been sent yet — the hand's, while
   the button is held (shell.lisp's HAND-PRESS): a plist (:TAB :CX :CY :COUNT :MODS),
   or NIL."
  (let ((p (hand-press))) (and (eq (first p) :web) (rest p))))

(defun take-web-press ()
  "The pending web press (WEB-PRESS), taken off the hand: it is being released now."
  (let ((p (web-press)))
    (when p (setf (hand-press) nil))
    p))

(defun web-finish-press ()
  "The press that opened a picker has not been released to the page yet (the button
   is still down while the chooser comes up, and the chooser will eat its release):
   send the release now, so the page does not think the button is held."
  (let ((p (take-web-press)))
    (when p
      (web-mouse (getf p :tab) "mouseReleased" (getf p :cx) (getf p :cy) :button "left"
                 :count (getf p :count) :mods (getf p :mods)))))

(defun web-pick (tab payload ctx)
  "Show the picker the page asked for (PAYLOAD, the JSON string the script sent from
   execution context CTX), and write back what is chosen."
  (let* ((msg (ignore-errors (read-json (coerce payload 'simple-string))))
         (kind (jget msg "kind"))
         (id (jget msg "id")))
    (when (and msg (integerp id))
      (web-finish-press)
      (multiple-value-bind (x y w h) (web-pick-anchor tab msg)
        (cond
          ((member kind '("select" "datalist") :test #'equal)
           (let ((entries (web-pick-entries msg)))
             (cond
               ((null entries) (setf *browser-status* "that list has no options"))
               ((null *chooser*) (setf *browser-status* "no menus on this backend"))
               (t (let ((e (let ((*menu-width* (max w 60)))
                             (funcall *chooser* :select entries x (+ y h)))))
                    (when e
                      (web-pick-apply tab ctx id (jobj "index" (car (getf e :action))
                                                       "value" (cdr (getf e :action))))))))))
          ;; colour, date, time: Chrome has no picker to show (date) or shows one we
          ;; cannot see (colour); the value is a short string, so the minibuffer asks
          ((member kind '("color" "date") :test #'equal)
           (let ((format (or (jget msg "format") "")) (type (or (jget msg "type") kind)))
             (minibuffer-read (format nil "~a (~a): " type format)
                              (lambda (text)
                                (web-pick-apply tab ctx id (jobj "value" (string-trim " " text))))
                              :initial (let ((v (jget msg "value"))) (if (stringp v) v ""))
                              :hint "Enter sets it · C-g leaves it"))))))))

;;; ---- input: pointer --------------------------------------------------------------

(defun fb-var (name)
  "The value of fb-browser.lisp's special NAME, or NIL where it is not loaded (the
   host renderer, poc/render-frame.lisp)."
  (let ((s (find-symbol name :cl-user)))
    (and s (boundp s) (symbol-value s))))

(defun web-page-point (tab x y)
  "X,Y (where TAB's page was drawn) -> CSS pixel coordinates in the page, or NIL
   outside it."
  (when (web-tab-page-rect tab)
    (destructuring-bind (px py pw ph) (web-tab-page-rect tab)
      (when (and (<= px x (+ px pw -1)) (<= py y (+ py ph -1)))
        (values (/ (- x px) *web-zoom* 1.0) (/ (- y py) *web-zoom* 1.0))))))

(defun web-modifiers (&key ctrl shift alt)
  "CDP's modifier bitfield: Alt 1, Ctrl 2, Meta 4, Shift 8."
  (logior (if alt 1 0) (if ctrl 2 0) (if shift 8 0)))


(defun web-note-input (tab)
  "Start the latency clock for TAB, unless an earlier input is still unanswered."
  (unless (web-tab-input-at tab) (setf (web-tab-input-at tab) (get-internal-real-time))))

(defun web-mouse (tab type cx cy &key (button "none") (buttons 0) (count 0) (mods 0)
                                      (dx 0) (dy 0))
  (unless (string= type "mouseMoved") (web-note-input tab))
  (web-send tab "Input.dispatchMouseEvent"
            (apply #'jobj "type" type "x" cx "y" cy "modifiers" mods
                   "button" button "buttons" buttons "clickCount" count
                   (and (string= type "mouseWheel") (list "deltaX" dx "deltaY" dy)))))

(defun web-click (pane x y &optional ctrl super)
  "A left press at X,Y in a web pane: a toolbar button, the address field, or the page.
   SuperL-click is the environment's \"open in a new trail\"; on a link it is sent as a
   Ctrl-click, which Chrome answers with a background tab (Target.targetCreated). A
   touchpad TAP has no release of its own, so it is sent as press + release at once."
  (let ((tab (web-tab-of pane)))
    (cond
      ((and (web-tab-bar-y tab) (<= (car (web-tab-bar-y tab)) y (cdr (web-tab-bar-y tab))))
       (let ((hit (find-if (lambda (r) (<= (second r) x (third r))) (web-tab-bar-rects tab))))
         (case (first hit)
           (:back (web-go tab -1))
           (:forward (web-go tab 1))
           (:reload (web-reload tab))
           (:stop (web-stop tab))
           (:address (web-prompt-address tab)))))
      ((member (web-tab-state tab) '(:dead :crashed))
       (web-revive tab))
      (t
       (multiple-value-bind (cx cy) (web-page-point tab x y)
         (when cx
           ;; the click count — 2 for a double click, 3 and up for more, as Chrome wants
           ;; it — is the hand's to keep (LOL.MORPH:HAND-COUNT-CLICK), as for text
           (let* ((h (the-hand))
                  (count (if h
                             (min 3 (lol.morph:hand-count-click h tab x y :max most-positive-fixnum))
                             1))
                  (mods (web-modifiers :ctrl (or ctrl super) :shift (fb-var "*SHIFT-DOWN*")
                                       :alt (fb-var "*ALT-DOWN*"))))
             (when (or ctrl super) (setf *web-focus-next* (and super (not ctrl)) ))
             (web-mouse tab "mousePressed" cx cy :button "left" :buttons 1 :count count :mods mods)
             (if (fb-var "*BUTTON-DOWN*")
                 (setf (hand-press) (list :web :tab tab :cx cx :cy cy :count count :mods mods))
                 (web-mouse tab "mouseReleased" cx cy :button "left" :count count :mods mods)))))))))

(defun web-drag (x y)
  "The button is held and the pointer moved: a drag in the page (selecting text,
   moving a slider). -> NIL: the page answers with a frame, not us with a redraw."
  (let ((tab (getf (web-press) :tab)))
    (when tab
      (when (web-tab-page-rect tab)
        (destructuring-bind (px py pw ph) (web-tab-page-rect tab)
          (let ((cx (/ (- (max px (min x (+ px pw -1))) px) *web-zoom* 1.0))
                (cy (/ (- (max py (min y (+ py ph -1))) py) *web-zoom* 1.0)))
            (web-mouse tab "mouseMoved" cx cy :button "left" :buttons 1))))))
  nil)

(defun web-release ()
  "The button came up: finish the click — and, as X does, a drag that selected text
   puts it in PRIMARY (lol.clipboard:clip-select), for a middle-click elsewhere."
  (let ((p (take-web-press)))
    (when p
     (destructuring-bind (&key tab cx cy count mods) p
      (multiple-value-bind (rx ry) (multiple-value-call #'web-page-point tab
                                     (screen-to-pane (current-pane) (or (fb-var "*CURSOR-X*") -1)
                                                     (or (fb-var "*CURSOR-Y*") -1)))
        (web-mouse tab "mouseReleased" (or rx cx) (or ry cy) :button "left" :count count :mods mods))
      (web-send tab "Runtime.evaluate"
                (jobj "expression" +web-selection-js+ "returnByValue" t)
                (lambda (r)
                  (let ((s (jget r "result" "result" "value")))
                    (when (and (stringp s) (plusp (length (string-trim " " s))))
                      (lol.clipboard:clip-select s)))))))))

(defun web-hover (x y)
  "A bare pointer move to the screen point X,Y: over the page, Chrome needs it for
   hover menus, tooltips and cursors-that-highlight — wherever the pane is, a window
   included. The frame that may follow redraws; we do not."
  (let ((tab (web-visible-tab)))
    (when (and tab (null (web-press)))
      (multiple-value-bind (cx cy) (multiple-value-call #'web-page-point tab
                                     (screen-to-pane (current-pane) x y))
        (when cx (web-mouse tab "mouseMoved" cx cy))))))

(defun web-wheel (lines)
  "The wheel over a web pane: LINES as the shell counts them (+ = down), sent as a
   mouseWheel at the pointer — the page scrolls whatever is under it, as on a desktop.
   One notch (+WHEEL-LINES+ lines) is 100 CSS pixels, Chrome's own step on Linux."
  (let ((tab (web-visible-tab)))
    (when tab
      (multiple-value-bind (cx cy) (multiple-value-call #'web-page-point tab
                                     (screen-to-pane (current-pane) (or (fb-var "*CURSOR-X*") 0)
                                                     (or (fb-var "*CURSOR-Y*") 0)))
        (unless cx (setf cx 10.0 cy 10.0))
        (web-mouse tab "mouseWheel" cx cy :dy (round (* lines 100) (max 1 +wheel-lines+)))))))

;;; ---- input: keys -----------------------------------------------------------------
;;; A key reaches us as the shell decodes it: a character or a keyword, plus STATE
;;; (Shift 1, Ctrl 4, Meta/Alt 8, Super 64). CDP wants a DOM KeyboardEvent: "key" (what
;;; the key means: "a", "Enter"), "code" (which physical key: "KeyA"), the legacy
;;; windowsVirtualKeyCode most sites still read, and "text" when the key inserts
;;; something. A keyDown with text is what makes a character appear; a keyDown
;;; without (Ctrl-A, an arrow) is a command; every keyDown is followed by its keyUp.

(defparameter +web-special-keys+
  '((:return "Enter" "Enter" 13 #.(string #\Return))
    (:backspace "Backspace" "Backspace" 8 nil)
    (:tab "Tab" "Tab" 9 nil)
    (:escape "Escape" "Escape" 27 nil)
    (:delete "Delete" "Delete" 46 nil)
    (:insert "Insert" "Insert" 45 nil)
    (:left "ArrowLeft" "ArrowLeft" 37 nil) (:up "ArrowUp" "ArrowUp" 38 nil)
    (:right "ArrowRight" "ArrowRight" 39 nil) (:down "ArrowDown" "ArrowDown" 40 nil)
    (:home "Home" "Home" 36 nil) (:end "End" "End" 35 nil)
    (:page-up "PageUp" "PageUp" 33 nil) (:page-down "PageDown" "PageDown" 34 nil)
    ;; F4 opens a <select> or a date's picker on Windows and Linux; the page script
    ;; (+WEB-PICKER-JS+) listens for it
    (:f4 "F4" "F4" 115 nil)
    (:f10 "F10" "F10" 121 nil))
  "(KEY dom-key dom-code virtual-keycode text) for the keys that are not characters.")

(defun web-char-code-name (ch)
  "The DOM \"code\" and virtual keycode of the US-layout key that types CH."
  (let ((up (char-upcase ch)))
    (cond ((char<= #\A up #\Z) (values (format nil "Key~a" up) (char-code up)))
          ((char<= #\0 ch #\9) (values (format nil "Digit~a" ch) (char-code ch)))
          ((char= ch #\Space) (values "Space" 32))
          (t (let ((e (assoc ch '((#\- "Minus" 189) (#\= "Equal" 187) (#\[ "BracketLeft" 219)
                                  (#\] "BracketRight" 221) (#\\ "Backslash" 220) (#\; "Semicolon" 186)
                                  (#\' "Quote" 222) (#\, "Comma" 188) (#\. "Period" 190)
                                  (#\/ "Slash" 191) (#\` "Backquote" 192)))))
               (if e (values (second e) (third e)) (values "" 0)))))))

(defun web-dispatch-key (tab key state)
  "Send KEY (with the shell's STATE) to TAB's page as keyDown + keyUp."
  (let* ((ctrl (logbitp 2 state)) (shift (logbitp 0 state)) (alt (logbitp 3 state))
         (mods (web-modifiers :ctrl ctrl :shift shift :alt alt))
         (special (assoc key +web-special-keys+)))
    (multiple-value-bind (dkey code vk text)
        (cond (special (values (second special) (third special) (fourth special)
                               (and (not ctrl) (not alt) (fifth special))))
              ((characterp key)
               (multiple-value-bind (code vk) (web-char-code-name key)
                 (values (string key) code vk
                         ;; Ctrl-a is a COMMAND (select all), not the letter a
                         (and (not ctrl) (not alt) (string key)))))
              (t (return-from web-dispatch-key nil)))
      (web-note-input tab)
      (web-send tab "Input.dispatchKeyEvent"
                (apply #'jobj "type" (if text "keyDown" "rawKeyDown")
                       "modifiers" mods "key" dkey "code" code
                       "windowsVirtualKeyCode" vk "nativeVirtualKeyCode" vk
                       (and text (list "text" text "unmodifiedText" text))))
      (web-send tab "Input.dispatchKeyEvent"
                (jobj "type" "keyUp" "modifiers" mods "key" dkey "code" code
                      "windowsVirtualKeyCode" vk "nativeVirtualKeyCode" vk)))))

(defun web-copy (tab &optional cut)
  "C-c / Ctrl-Insert (C-x cuts): the page's selection into OUR clipboard history —
   Chrome's own clipboard lives in a headless process nobody else can paste from.
   The key is ALSO sent to the page, so a site with its own copy handler still sees it."
  (web-send tab "Runtime.evaluate" (jobj "expression" +web-selection-js+ "returnByValue" t)
            (lambda (r)
              (let ((s (jget r "result" "result" "value")))
                (if (and (stringp s) (plusp (length s)))
                    (copy-to-clipboard s)
                    (setf *browser-status* "copy: nothing selected in the page"))
                (setf *web-changed* t))))
  (when cut
    (web-send tab "Runtime.evaluate"
              (jobj "expression" "document.execCommand('delete')" "returnByValue" t))))

(defun web-paste (tab &optional (text (lol.clipboard:clip-yank)))
  "C-v / Shift-Insert: our newest clipboard entry, typed into the focused field as
   one insertion (Input.insertText: the way an input method commits text)."
  (if text
      (web-send tab "Input.insertText" (jobj "text" text))
      (setf *browser-status* "paste: the clipboard is empty")))

(defun web-prompt-address (tab)
  "C-l, or a click on the address: edit the URL in the minibuffer; Enter loads it.
   C-y / C-v pastes a copied URL, C-u or the ✕ clears it (MINIBUFFER-KEY). Above the
   line, the candidates (WEB-ADDRESS-CANDIDATES): a folder's files once the text is a
   path, else the history — Down / C-n chooses one, Tab takes it into the line."
  (minibuffer-read "url, path or search: " (lambda (text) (web-navigate tab text))
                   :initial (if (member (web-tab-url tab) (list "about:blank" (web-home-url)) :test #'equal)
                                 "" (web-tab-url tab))  ; a blank or home tab: an empty prompt
                   :completer (lambda (text)
                                (let ((q (string-trim " " text)))
                                  (if (path-query-p q) (complete-path-query q :home (home-directory)) text)))
                   :candidates (web-address-candidates tab)
                   :hint "Enter loads · / or ~/ a file · C-u clears · C-g cancels"))

(defvar *web-find* "" "The last find-in-page query.")

(defun web-find (tab)
  "C-f: find in the page as you type (window.find highlights and scrolls to it);
   Enter keeps it, C-f again with the same text finds the next one."
  (flet ((find-js (q from-top)
           (web-send tab "Runtime.evaluate"
                     (jobj "expression"
                           (format nil "~:[~;window.getSelection().removeAllRanges();~]window.find(~a,false,false,true)"
                                   from-top (js-string q))
                           "returnByValue" t)
                     (lambda (r)
                       (unless (eq (jget r "result" "result" "value") t)
                         (setf *browser-status* (format nil "find: ~s not found" q)
                               *web-changed* t))))))
    (minibuffer-read "find in page: "
                     (lambda (text) (setf *web-find* text)
                       (when (plusp (length text)) (find-js text nil)))
                     :initial *web-find*
                     :on-change (lambda (text) (when (plusp (length text)) (find-js text t)))
                     :hint "Enter: next match")))

(defun web-revive (tab)
  "r / a click on a dead or crashed tab: reopen it at its last URL."
  (setf (web-tab-status tab) nil (web-tab-open-url tab) (web-tab-url tab))
  (case *web-chrome-state*
    ((:off :failed) (setf (web-tab-state tab) :new) (web-start-chrome))
    (:ready (if (eq (web-tab-state tab) :crashed)
                (progn (setf (web-tab-state tab) :live) (web-reload tab))
                (web-create-target tab))))
  (setf *web-changed* t))

(defun web-zoom (delta)
  (setf *web-zoom* (if (zerop delta) 1 (max 1/2 (min 3 (+ *web-zoom* (* delta 1/4))))))
  (setf *browser-status* (format nil "page zoom ~d%" (round (* 100 *web-zoom*)))))

(defun web-key (pane key state)
  "A key while a web pane has focus. The environment keeps SuperL-* (handled before
   this is reached) and a handful of browser chords; everything else is the page's.

     C-l        the address          SuperL-Left/Right back / forward (M-Left/Right too,
                                     where they arrive: not on the console, see shell)
     C-r        reload               Esc (loading)     stop
     C-f        find in the page     C-c / C-x / C-v   copy / cut / paste, via OUR clipboard
     C-+ C-- C-0  zoom               Shift-Insert / Ctrl-Insert  paste / copy (X style)
     Shift-F10 / Menu  the environment's menu"
  (let* ((tab (web-tab-of pane))
         (ctrl (logbitp 2 state)) (meta (logbitp 3 state)) (shift (logbitp 0 state)))
    (cond
      ((and (eq key :f10) shift) (keyboard-context-menu))
      ((member (web-tab-state tab) '(:dead :crashed))
       (when (member key '(#\r :return)) (web-revive tab)))
      ((and ctrl (member key '(#\l #\L))) (web-prompt-address tab))
      ((and meta (eq key :left)) (web-go tab -1))
      ((and meta (eq key :right)) (web-go tab 1))
      ((and ctrl (member key '(#\r #\R))) (web-reload tab))
      ((and (eq key :escape) (web-tab-loading tab)) (web-stop tab))
      ((and ctrl (member key '(#\f #\F))) (web-find tab))
      ((and ctrl (member key '(#\+ #\=))) (web-zoom 1))
      ((and ctrl (member key '(#\_ #\-))) (web-zoom -1))
      ((and ctrl (eql key #\0)) (web-zoom 0))
      ((and ctrl (member key '(#\c #\C))) (web-copy tab) (web-dispatch-key tab key state))
      ((and ctrl (member key '(#\x #\X))) (web-copy tab t))
      ((and ctrl (member key '(#\v #\V))) (web-paste tab))
      ((and shift (eq key :insert)) (web-paste tab))
      ((and ctrl (eq key :insert)) (web-copy tab))
      (t (web-dispatch-key tab key state))))
  nil)

;;; ---- the subject's verbs, hints and the Spotter -----------------------------------

(defmethod commands-for ((s subj-web))
  (let ((tab (subj-web-tab s)))
    (list (make-command :label "Address..." :key "C-l" :surfaces '(:menu :halo)
                        :action (lambda () (web-prompt-address tab)))
          (make-command :label "Back" :key "SuperL-left" :surfaces '(:menu)
                        :action (lambda () (web-go tab -1)))
          (make-command :label "Forward" :key "SuperL-right" :surfaces '(:menu)
                        :action (lambda () (web-go tab 1)))
          (make-command :label "Reload" :key "C-r" :surfaces '(:menu :halo)
                        :action (lambda () (web-reload tab)))
          (make-command :label "Find in page..." :key "C-f" :surfaces '(:menu)
                        :action (lambda () (web-find tab)))
          (make-command :label "Copy page URL" :surfaces '(:menu :halo)
                        :action (lambda () (copy-to-clipboard (web-tab-url tab))))
          (make-command :label "Duplicate tab" :surfaces '(:menu)
                        :action (lambda () (web-new-tab (web-tab-url tab))))
          (make-command :label "New web tab" :key "SuperL-b" :surfaces '(:menu)
                        :action (lambda () (web-open)))
          (make-command :label "Downloads folder" :surfaces '(:menu)
                        :action (lambda () (funcall 'open-files (web-downloads-dir)))))))

(defmethod subject-hints ((s subj-web) pane)
  (declare (ignore pane))
  '(("C-l" . "address") ("SuperL-left/right" . "back/fwd") ("C-r" . "reload")
    ("C-f" . "find") ("C-c/C-v" . "copy/paste") ("SuperL-b" . "new tab")))

(defun web-open (&optional url)
  "SuperL-b, the Spotter, a command: a new web tab, with the address prompt open when
   no URL was given."
  (let ((tab (web-new-tab (and url (web-url-for url)))))
    (unless url (web-prompt-address tab))
    tab))

(defun web-url-like-p (q)
  "Does the Spotter query Q look like an address rather than a name to find?"
  (let ((q (string-trim " " q)))
    (and (> (length q) 3) (not (find #\Space q))
         (or (search "://" q)
             (and (find #\. q) (not (find #\( q))
                  (alpha-char-p (char q (1- (length q))))
                  (let ((dot (position #\. q :from-end t)))
                    (and (< dot (1- (length q))) (plusp dot))))
             (and (>= (length q) 9) (string-equal "localhost" q :end2 9))))))

(defstruct (subj-web-open (:constructor subj-web-open (url))) url)

(defmethod present ((s subj-web-open))
  ;; a Spotter row's subject is OPENED with NEW-TRAIL, so presenting this one IS
  ;; the act of opening the page: it becomes a real web tab in that same trail
  (let ((tab (%make-web-tab :url (web-url-for (subj-web-open-url s))
                            :open-url (web-url-for (subj-web-open-url s)))))
    (push tab *web-tabs*)
    (case *web-chrome-state*
      ((:off :failed) (web-start-chrome))
      (:ready (web-create-target tab)))
    (present (subj-web tab))))

(defun web-spotter-rows (query)
  "The Spotter's web rows for QUERY: \"open\" when it looks like an address, \"search
   the web\" for any other text of two or more characters, the open tabs whose title
   or URL contains it, and up to five pages from the history that no tab shows."
  (let ((q (string-trim " " query)) (out '()))
    (when (web-url-like-p q)
      (push (list :label (format nil "open ~a" (web-url-for q)) :detail "web page"
                  :group "web" :kind :restart :subject (subj-web-open q))
            out))
    (dolist (tab *web-tabs*)
      (when (and (plusp (length q))
                 (or (search q (web-tab-url tab) :test #'char-equal)
                     (search q (or (web-tab-title tab) "") :test #'char-equal)))
        (push (list :label (web-tab-label tab) :detail (format nil "web tab · ~a" (web-tab-url tab))
                    :group "web" :kind :restart :subject (subj-web tab))
              out)))
    ;; pages from the history that no open tab shows, the five best
    (when (and (>= (length q) 2) (not (path-query-p q)))   ; a path has rows of its own
      (loop for e in (web-history-matches q :limit 20)
            for url = (getf e :url)
            with shown = 0
            while (< shown 5)
            unless (find url *web-tabs* :key #'web-tab-url :test #'equal)
              do (incf shown)
                 (push (list :label (or (getf e :title) (web-url-sans-scheme url))
                             :detail (format nil "history · ~a" (web-url-sans-scheme url))
                             :group "web" :kind :restart :subject (subj-web-open url))
                       out)))
    (when (and (>= (length q) 2) (not (web-url-like-p q)))
      (push (list :label (format nil "search the web for ~s" q) :detail "web search"
                  :group "web" :kind :restart :subject (subj-web-open (format nil *web-search* (url-encode q))))
            out))
    (nreverse out)))

;;; ---- the event-loop service, for fb-browser.lisp ----------------------------------

(defun web-present-frame (canvas)
  "Only the page's pixels changed: blit the new frame straight into the canvas its
   pane draws into — the screen's, or a window's backing store — and hand that one
   rectangle to the world (poc/morphic.lisp, PANE-PAINTED): on the screen it
   composites whatever lies over the page (a window, an overlay's scrim) and sends
   just that rect; in a window it copies just that rect. The economy the systray and
   the pointer already use, so a scrolling page costs a JPEG decode and one rectangle,
   not a repaint. With no world (a host test), it is sent straight away."
  (let ((tab (web-visible-tab)))
    (when (and tab (web-tab-page-rect tab) (web-tab-frame-px tab))
      (if (and (fboundp 'pane-painted) (boundp '*world*) (symbol-value '*world*))
          (funcall 'pane-painted (current-pane) canvas
                   (lambda (c) (web-blit c tab) (list (web-tab-page-rect tab))))
          (destructuring-bind (x y w h) (web-tab-page-rect tab)
            (web-blit canvas tab)
            ;; by name: lol.fb (poc/fb.lisp) exists in the image, not in a host test
            (funcall (find-symbol "PRESENT-FB-RECT" "LOL.FB") canvas x y w h)
            (when (and (fb-var "*FB-MOUSE*") (not (fb-var "*CURSOR-HIDDEN*")))
              (funcall 'put-cursor canvas (fb-var "*CURSOR-X*") (fb-var "*CURSOR-Y*"))))))))

(defun web-loop-service (canvas dirty)
  "fb-browser.lisp's per-turn hook (see *FB-SERVICES*). -> :REDRAW when what the pane
   shows besides the page changed (a title, the address, a load state — the desk
   finds which morph, by its signature); a frame alone is painted here, wherever the
   pane is, and composited by the world even when other damage is pending."
  (declare (ignore dirty))
  (case (web-service)
    (:redraw :redraw)
    (:frame (web-present-frame canvas) nil)))

(defun web-shutdown ()
  "Close Chrome cleanly: Browser.close lets it flush the profile (cookies, history)
   to disk before the disk is remounted read-only at power-off."
  (web-history-save :force t)              ; before the disk goes read-only
  (when *web-chrome*
    ;; let go of it FIRST, so the browser's loop stops reading a pipe being closed
    (let ((c (shiftf *web-chrome* nil)))
      (setf *web-chrome-state* :off)
      (ignore-errors (chrome-close c)))
    (dolist (tab *web-tabs*) (setf (web-tab-state tab) :dead (web-tab-session tab) nil))
    (web-reap-orphans)))

;;; The browser's event loop (fb-browser.lisp) learns about us here, and only here:
;;; Chrome's pipe joins its poll, and each turn runs WEB-LOOP-SERVICE. In a host
;;; test there is no loop, and nothing to register with.
(when (fboundp 'fb-register-service)
  (funcall 'fb-register-service :web
           :fd 'web-wait-fd :timeout 'web-timeout :run 'web-loop-service :motion 'web-hover))
