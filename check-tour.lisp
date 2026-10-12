;;;; check-tour.lisp — assert that the guided tour still points at real code.
;;;;
;;;;     LOL_PORT=4018 sbcl --script check-tour.lisp     ; exits 0 if all pass
;;;;
;;;; initramfs/tour.lisp names its stops by STRINGS ("CL-USER" "BROWSER-KEY") and
;;;; resolves them only when the tour is shown, so that a stop never breaks the
;;;; build. The price is that a renamed function turns its stop into a polite
;;;; "not in this image" row and nothing else complains. render-frame cannot catch
;;;; that either: it loads only part of the stack, so several stops are
;;;; legitimately missing there. Only the booted image has every module, so this
;;;; asks the guest, over the net REPL (no synthetic input needed):
;;;;
;;;;   - every Lisp stop resolves to a subject, and every subject presents;
;;;;   - the Spotter offers "tour";
;;;;   - the registry shows its OWN definitions as source (registry.lisp records
;;;;     itself since 2026-10-12; before that, ACCEPT-SOURCE opened as a reference).
;;;;
;;;; The two file stops (preinit.c, build.sh) are not counted: they open only when
;;;; the repository is on the disk the guest mounted, which the stand-in
;;;; test-disk.img does not carry. Run ./build.sh first; this boots iso_root as is.

(load (merge-pathnames "host-client/guest-harness.lisp"
                       (make-pathname :directory (pathname-directory *load-truename*))))
(use-package :lol.guest)

(defun row (label value)
  (format t "~&  ~44a ~a~%" label value)
  (finish-output)
  value)

(defun checks ()
  (let ((stops (row "stops in the tour"
                    (probe-int "(length (view-items (present (subj-tour))))" 10.0)))
        (missing (row "Lisp stops that do not resolve"
                      (probe-int "(count-if (lambda (s) (and (not (eq (first (second s)) :file)) (null (ignore-errors (tour-stop-subject (second s)))))) *tour-stops*)" 10.0)))
        (broken (row "resolved stops whose view fails"
                     (probe-int "(count-if (lambda (it) (and (item-subject it) (not (view-p (ignore-errors (present (item-subject it))))))) (view-items (present (subj-tour))))" 20.0)))
        (spotter (row "the Spotter offers \"tour\" (1=yes)"
                      (probe-int "(if (find \"tour\" (spotter-candidates) :key (lambda (c) (getf c :label)) :test (function string=)) 1 0)" 10.0)))
        (own (row "accept-source opens as :source (1=yes)"
                  (probe-int "(if (eq (view-kind (present (subj-defn (quote accept-source)))) :source) 1 0)" 10.0))))
    (check "the tour has as many rows as *tour-stops*"
           (and stops (eql stops (probe-int "(length *tour-stops*)"))))
    (check "every Lisp stop names a definition in the image" (eql missing 0))
    (check "every stop's subject presents a view" (eql broken 0))
    (check "the Spotter offers the tour" (eql spotter 1))
    (check "the registry shows its own code as source" (eql own 1)))
  t)

(run-guest-checks "check-tour" #'checks :qmp nil)
