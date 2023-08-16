;;; -*- Mode:Common-Lisp; Package:USER; Base:10 -*-
;;;; *-* Last-edit: Tuesday, October 26, 1993  16:27:20; Edited-By: Westy *-* 


(in-package :USER)

#-Explorer
(defun do-compile-and-load ()
  (flet ((do-file (name)
	    (let ((source (merge-pathnames name (clip-load-pathname)))
		  (binary (make-pathname 
			   :directory `(,@(or (pathname-directory (clip-load-pathname)) '(:RELATIVE))
					  #+lispworks3.1 "bin-lispworks3.1"
					  #+lispworks3.2 "bin-lispworks3.2"
					  #+lucid "bin-lucid"
					  #+(and allegro sparc) "bin-allegro-sparc"
					  #+(and allegro mips)  "bin-allegro-mips")
			    :defaults (merge-pathnames name (clip-load-pathname))
			    :type #+(and lispworks sun4)   "wfasl"
			          #+(and lispworks alpha) "afasl"
                                  #+allegro "fasl"
                                  #+(and lucid sparc) "sbin"
                                  #+(and lucid mips) "mbin")))
	      (compile-file source :output-file binary)
	      (load binary))))
    (do-file "generic-simulator")
    (do-file "agent-simulator")
    (do-file "simple-agent-experiment")
    (do-file "super-agent-experiment")
    (do-file "agent-experiment")
    ))

#-Explorer
(do-compile-and-load)

#+Explorer
(ticl::defsystem agent-experiment-demo
  (:pathname-default "clip:top.demos.agent-simulator;")
  (:default-output-directory "clip:top.demos.agent-simulator.bin-explorer;")
  (:compile-load-modules
    ("generic-simulator"
     "agent-simulator"
     "simple-agent-experiment"
     "super-agent-experiment"
     "agent-experiment")))

#+Explorer 
(make-system :AGENT-EXPERIMENT-DEMO :compile :noconfirm)

