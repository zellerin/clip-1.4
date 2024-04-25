(asdf:defsystem clip-1994/extended-lisp
  :depends-on ("closer-mop")
  :pathname "extended-lisp"
  :components ((:file "el-package")
               (:file "lisp-extensions")
               (:file "time-parse")))

;;;; list other implementations that would use cl-advice
;;;; actually, sbcl might better use trace macro
#+(or sbcl) (push :use-cl-advice *features*)

(asdf:defsystem clip-1994
  :depends-on ("closer-mop" "clip-1994/extended-lisp"
                            #+use-cl-advice "cl-advice")
  :pathname"clip-1.4"
  :version "1.4.1"
  :description "Original parts of the CLIP/SYSTEM. The name is suffixed with the original
release year to prevent collision with another unrelated asdf system that is in
the Quicklisp."
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
               (:file "define-experiment")
               (:file "../new-code/utils")))

(asdf:defsystem clip-1994/loader
  :depends-on ("lisp-stat" "mgl-pax")
  :description "Auxiliary functions for loading of the data in CLASP format."
  :pathname"new-code"
  :version "1.4.1"
  :components ((:file "package")
               (:file "frame-loader")))

(asdf:defsystem clip-1994/demo
  :depends-on ("clip-1994")
  :pathname "demos/agent-simulator"
  :version "1.4.1"
  :description "Original demos for the \\CLIP system. They show different ways to use clips."
  :components (
               (:file "generic-simulator")
               (:file "agent-simulator")
               (:file "simple-agent-experiment")
               (:file "super-agent-experiment")
               (:file "agent-experiment")))

(asdf:defsystem clip-1994/doc
  :depends-on ("clip-1994" "dref" "mgl-pax")
  :pathname "new-code"
  :version "1.4.1"
  :components ((:file "pax-doc")))
