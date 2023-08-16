;;;; -*- Mode:Common-Lisp; Package:CL-USER; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/mac-files/extended-lisp/defsys.lisp *-*
;;;; *-* Last-edit: Monday, June 28, 1993  11:13:18; Edited-By: StAmant *-* 
;;;; *-* Machine: Miles (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                            System Definition                           *
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
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  01-19-93 File Created.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :USER)

;;; --*--
;;; ***************************************************************************

#+MCL
(assert (find-package 'portable-defsystem)
	nil
	"~%I really need the Public Domain DEFSYSTEM-Facility"
	)

(defun el-load-pathname ()
  #+lucid lcl:*source-pathname*
  #+allegro excl:*source-pathname*
  #+(or Genera Explorer) (ticl::translated-pathname sys:fdefine-file-pathname)
  #-(or lucid allegro Genera Explorer) *load-pathname*)

;; Define some stubs so things do not blow up.
#+GBB-DEFMODULE
(unless (find-package 'portable-defsystem)
	(make-package 'PORTABLE-DEFSYSTEM))

#+GBB-DEFMODULE
(unless (fboundp 'portable-defsystem::defsystem)
  (defmacro portable-defsystem::defsystem (&rest args)))

#+Defsystem
(portable-defsystem::defsystem Extended-Lisp
    (:default-pathname (el-load-pathname)
     :default-binary-pathname
     (make-pathname
      :directory `(,@(pathname-directory (el-load-pathname))
		     #+(and allegro mips)
		     "bin-allegro-mips"
		     #+(and allegro sparc)
		     "bin-allegro-sparc"
		     #+lispworks3.2
		     "bin-lispworks3.2"
		     #+lispworks3.1
		     "bin-lispworks3.1"
		     #+lucid
		     "bin-lucid")))
  
  ("el-package")
  ("lisp-extensions")
  #-Explorer
  ("time-parse"))

;;;----------------------------------------------------------------------------

#+Explorer
(fs:add-logical-pathname-host
    "extended-lisp" (pathname-host (el-load-pathname))
    `(("source" "" ,(pathname-directory (el-load-pathname)))
      ("binary" "" ,(append (pathname-directory (el-load-pathname))
                            '("BIN-EXPLORER")))))

#+Explorer
(ticl::defsystem EXTENDED-LISP
  (:name "EXTENDED LISP")
  (:short-name "EL")
  (:pathname-default "extended-lisp:source;")
  (:default-output-directory "extended-lisp:binary;")

  (:compile-load-modules
    ("el-package"
     "lisp-extensions")))

#+GBB-DEFMODULE
(gbb::defdirectory el-dir
                   #+EXPLORER
                   "el:binary;" 
		   #+EXPLORER
                   "el:source;"
		   #+Lispworks
		   "/usr/users/eksl/systems/extended-lisp/bin-lispworks"
		   #+lucid
		   "/usr/users/eksl/systems/extended-lisp/bin-lucid"
		   #+allegro
		   "/usr/users/eksl/systems/extended-lisp/bin-allegro"
		   #+(or Lispworks Lucid allegro)
                   "/usr/users/eksl/systems/extended-lisp/"
		   )

#+GBB-DEFMODULE
(gbb::defmodule Extended-Lisp
  (:directory el-dir)
  (:files
    "el-package"
    #-Explorer
    "time-parse"
    "lisp-extensions"))

;;; ***************************************************************************
;;; EOF
