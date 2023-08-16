;;;; -*- Mode:Common-Lisp; Package:CL-USER; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/systems/clip/development/defsys.lisp *-*
;;;; *-* Last-edit: Wednesday, January 5, 1994  14:31:52; Edited-By: Westy *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                       CLIP System Definition                           *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Copyright (c) 1994 University of Massachusetts
;;; Department of Computer Science
;;; Experimental Knowledge Systems Laboratory
;;; Professor Paul Cohen, Director.
;;; All rights reserved.

;;; Permission to use, copy, modify and distribute this software and its
;;; documentation is hereby granted without fee for non-commercial uses
;;; only (not for resale), provided that the above copyright notice of EKSL,
;;; this paragraph and the one following appear in all copies and in
;;; supporting documentation.

;;; EKSL makes no representation about the suitability of this software for any
;;; purposes.  It is provided "AS IS", without express or implied warranties
;;; including (but not limited to) all implied warranties of merchantability
;;; and fitness for a particular purpose, and notwithstanding any other
;;; provision contained herein.  In no event shall EKSL be liable for any
;;; special, indirect or consequential damages whatsoever resulting from loss
;;; of use, data or profits, whether in an action of contract, negligence or
;;; other tortuous action, arising out of or in connection with the use or
;;; performance of this software, even if EKSL is advised of the possiblity
;;; of such damages.

;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  01-19-93 File Created.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package #+CLTL2 COMMON-LISP-USER #-CLTL2 'USER)

;;; --*--
;;; ***************************************************************************

#+(or Allegro Lucid MCL)
(assert (or (find-package 'make)
	    (find-package 'portable-defsystem))
	nil
	"~%I really need the Public Domain DEFSYSTEM-Facility in Package MAKE"
	)

(load #-(or allegro lucid lispworks) "clip:extended-lisp;defsys"
      #+(or allegro lucid lispworks)
      (make-pathname
       :directory (pathname-directory *extended-lisp-root-pathname*)
       :name "defsys"
       #+lispworks :type #+lispworks "lisp"))

;;;----------------------------------------------------------------------------

#+Defsystem
(defun merge-binary-defaults (pathname)
  (make-pathname :defaults 
                 (merge-pathnames *default-binary-path* 
                                  pathname)
		 :host "EKSL"))

#+Defsystem
(portable-defsystem:defsystem CLIP
    (:default-pathname 
      #-(or allegro lucid lispworks)
      "clip:source;"
      #+(or allegro lucid lispworks)
      (make-pathname
       :directory `(,@(pathname-directory *clip-root-pathname*)
		      ,*clip-source-directory*))
      :default-binary-pathname
      #-(or allegro lucid lispworks)
      (make-pathname :host "CLIP" :defaults (merge-pathnames *default-binary-path* 
							     "clip:source;"))
      #+(or lucid allegro lispworks)
      (make-pathname
       :directory `(,@(pathname-directory *clip-root-pathname*)
		      ,*clip-source-directory*
		      #+(and allegro mips)
		      "bin-allegro-mips"
		      #+(and allegro sparc)
		      "bin-allegro-sparc"
		      #+lucid
		      "bin-lucid"
                      #+lispworks3.2
		      "bin-lispworks3.2"
		      #+lispworks3.1
		      "bin-lispworks3.1"
		      ))
      :needed-systems (extended-lisp)
      :load-before-compile (extended-lisp))

  ("packages")
  #+(or allegro lucid)
  ("patches")
  ("utilities")
  ("super-intrinsic-mixins")
  ("time-definitions")
  ("macros")
  ("parameters")
  ("intrinsic-mixins")
  ("class-defs")
  ("instrumentation")
  ("defclip")
  ("simulator")
  ("experiment-runner")
  ("standard-clips")
  ("define-experiment"))
  
;;;----------------------------------------------------------------------------

#+Explorer
(ticl::defsystem COMMON-LISP-INSTRUMENTATION-PACKAGE
  (:name "Common Lisp Instrumentation Package")
  (:short-name "CLIP")
  (:pathname-default "clip:source;")
  (:default-output-directory "clip:binary;")
  (:component-systems
    :extended-lisp)
  (:do-components)
  (:compile-load-modules
    ("PACKAGES"
     "UTILITIES"
     "SUPER-INTRINSIC-MIXINS"
     "TIME-DEFINITIONS"
     "MACROS"
     "PARAMETERS"
     "INTRINSIC-MIXINS"
     "CLASS-DEFS"
     "INSTRUMENTATION"
     "DEFCLIP"
     "SIMULATOR"
     "EXPERIMENT-RUNNER"
     "STANDARD-CLIPS"
     "DEFINE-EXPERIMENT")))

;;; ***************************************************************************
;;; EOF
