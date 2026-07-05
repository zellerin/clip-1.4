;; -*- nameless-current-name: "clip"; -*-

(defvar clip-packages-history nil)
(defvar clip-instrumentations-history nil)

(defun clip-list-named-objects (name package)
  (sly-eval `(cl:ignore-errors
              (clip:list-named-objects ',name ,(upcase package)))
            (sly-find-buffer-package)))

(defun clip-list-packages ()
  (sly-eval `(cl:ignore-errors
              (cl:mapcar 'cl:package-name (clip::clip-users)))
            "CL-USER"))

;;;###autoload
(defun clip-run-experiment-skeleton (experiment file)
  "Insert a skeleton to run an experiment."
  (interactive
   (let* ((p (completing-read "Package: " (clip-list-packages) nil nil (car clip-packages-history)
                              'clip-packages-history))
          (e (completing-read "Experiment: " (clip-list-named-objects 'clip::experiment p))))
     (list e (read-file-name "Output file: " "/tmp/" "foo.clasp")))
   sly-mrepl-mode)
  (insert (format "(clip:run-experiment '%s :output-file \"%s\" :args '()) "
                  experiment file)))

(define-derived-mode clasp-mode lisp-mode "Clasp"
  "Mode for CLASP files. Handles experiment as a page and trial as a paragraph."
  (setq-local page-delimiter "^\"$"
              paragraph-start "("
              paragraph-separate "^")
  (font-lock-add-keywords nil '(("\\*\\{4\\} [A-Za-z]*: \\(.*\\)\s *\\*\\{4\\}" 1 '(bold t)))
                  'set)
  (setq truncate-lines t)
  (font-lock-mode))


(bind-key "<f5>r" 'clip-run-experiment-skeleton sly-mrepl-mode-map)

(define-skeleton clip-insert-define-experiment-skeleton
  "Insert define-experiment skeleton"
  "Experiment name: "
  "(define-experiment " str " ()" \n
  "\"" _  "\"" \n
  ":system-version (format nil \"\")" \n
  ":simulator "
  (insert (completing-read "Simulator: "
                           (clip-list-named-objects 'clip::simulator (sly-find-buffer-package)))) \n
  ":ivs (" ("Independent variable: " "(" str " '())")  ")

  :timestamp get-internal-real-time
  :schedule-function nil
  :deactivate-scheduled-function nil

  :locals ()
  :instrumentation (" ((completing-read "Clip: " (clip-list-named-objects 'clip::instrumentation (sly-find-buffer-package))) str) ")
  :after-trial (write-current-experiment-data)
  :after-experiment nil)")

;;; (bind-key "<f5>r" 'clip-run-experiment-skeleton sly-mrepl-mode-map)
