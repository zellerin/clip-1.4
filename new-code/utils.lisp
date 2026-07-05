(in-package clip)

(defun list-named-objects (class package)
  "List experiments defined in given package. This is meant for editing support."
  (let ((res))
    (do-symbols (s package res)
      (let ((a (get s 'named-object-mixin)))
        (when (and a (typep a class))
          (push (clip::name a) res))))))

(defun clip-users ()
  "List of packages using clip."
  (remove (find-package 'clip) (list-all-packages) :key 'package-use-list
                                                   :test-not  'member))

(defvar *last-real-time*)

(defclip real-time-ms ()
  "Real time in ms since enabling or reset of the clip"
  (:enable-function (setf *last-real-time* (get-internal-real-time))
   :reset-function  (setf *last-real-time* (get-internal-real-time))
   :report-key "Real time ms"
   :disable-function (fmakunbound '*last-real-time*))
  (round (- (get-internal-real-time) *last-real-time*)
         (floor internal-time-units-per-second 1000.0)))


(defun display-current-experiment-data ()
  "There is no original function using DISPLAY property of the instrumentations.

This calls display on each slot of the current experiment. Imagine a GUI that
can be moved with the change."
  (mapcar #'clip::display (slot-value  clip::*current-experiment* 'clip::instrumentation)))
