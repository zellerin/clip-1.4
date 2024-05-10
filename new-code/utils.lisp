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
