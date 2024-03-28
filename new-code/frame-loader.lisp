(in-package #:clip/loader)

(defun load-clasp-frame (name stream)
  "Load data from a CLIP experiment to variable and package NAME.

NAME is upcased and turned to a variable that contains the data frame.

Aditionally, package of same name is created and a symbol in this package is created for each columnt of the experiment.

As second value, return package description"
  (let* ((symbol (intern (string-upcase name)))
         (package (or (find-package (string-upcase name))
                      (make-package (string-upcase name))))
         (note (the string (read stream)))
         (columns (loop while (eql (peek-char t stream) #\")
                        collect (string-upcase (read stream))))
         (data (loop while (eql (peek-char t stream nil) #\()
                     collect (read stream)))
         (frame (data-frame:alist-df
                 (mapcar (lambda (column-key data-column)
                           (set (intern column-key package) data-column)
                           (proclaim `(special ,(intern column-key package)))
                           (cons (intern column-key package) data-column))
                         columns
                         (apply 'mapcar 'vector data)))))
    (set symbol frame)
    (setf (lisp-stat:name frame) symbol)
    (heuristicate-types frame)
    (values frame note)))

(defun load-measurements (file &rest names)
  "Load frames from a single CLASP file to variables and packages in NAMES."
  (with-open-file (in file)
    (dolist (name names)
      (load-clasp-frame name in))))
