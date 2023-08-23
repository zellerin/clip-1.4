(asdf:defsystem clip/extended-lisp
  :depends-on ("closer-mop")
  :pathname "extended-lisp"
  :components ((:file "el-package")
               (:file "lisp-extensions")
               (:file "time-parse")))

;;;; list other implementations that would use cl-advice
;;;; actually, sbcl might better use trace macro
#+(or sbcl) (push :use-cl-advice *features*)

(asdf:defsystem clip
  :depends-on ("closer-mop" "clip/extended-lisp"
                            #+use-cl-advice "cl-advice")
  :pathname"clip-1.4"
  :version "1.4"
  :components ((:file "packages")
               (:file "utilities")
               (:file "super-intrinsic-mixins")
               (:file "time-definitions")
               (:file "macros")
               (:file "parameters")
               (:file "intrinsic-mixins")
               (:file "class-defs")
               (:file "instrumentation")
               (:file "defclip")
               (:file "simulator")
               (:file "experiment-runner")
               (:file "standard-clips")
               (:file "define-experiment")))
