;; -*- nameless-current-name: "clip"; -*-

(defun clip-list-experiments (package)
  (sly-eval `(cl:ignore-errors
              (clip:list-named-objects 'clip::experiment ,(upcase package)))
            (upcase package)))

(defun clip-list-packages ()
  (sly-eval `(cl:ignore-errors
              (cl:mapcar 'cl:package-name (clip::clip-users)))
            "CL-USER"))

(defun clip-run-experiment-skeleton (experiment file)
  "Insert a skeleton to run an experiment."
  (interactive
   (let* ((p (completing-read "Package: " (clip-list-packages)))
          (e (completing-read "Experiment: " (clip-list-experiments p))))
     (list e (read-file-name "Output file: " "/tmp/" "foo.clasp")))
   sly-mrepl-mode)
  (insert (format "(clip:run-experiment '%s :output-file \"%s\" :args '()) "
                  experiment file)))

(bind-key "<f5>r" 'clip-run-experiment-skeleton sly-mrepl-mode-map)
