;;;; -*- Mode:Common-Lisp; Package:USER; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/mac-files/clip/development/packages.lisp *-*
;;;; *-* Last-edit: Wednesday, July 21, 1993  17:25:00; Edited-By: WESTY *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                           Package Definitions                          *
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

(in-package #+CLTL2 COMMON-LISP-USER #-CLTL2 'USER)

;;; --*--
;;; ***************************************************************************

(defpackage CLIP
  (:use COMMON-LISP #+(or allegro Explorer lcl4.0) CLOS)
  (:export "DEFCLIP" "DEFINE-EXPERIMENT" "DEFINE-SIMULATOR"
	   "WRITE-CURRENT-EXPERIMENT-DATA"
           "TRIAL-NUMBER" "TIMESTAMP" "RUN-EXPERIMENT"
           "SHUTDOWN-AND-RERUN-TRIAL"
           "SHUTDOWN-AND-RUN-NEXT-TRIAL"
           "SHUTDOWN-EXPERIMENT"
	   "*DATA-SEPARATOR-CHARACTER*"
	   "*OUTPUT-FORMAT*"
	   "*CURRENT-EXPERIMENT*")
  #+Explorer
  (:nicknames CLIPS)
  #+lcl4.0
  (:import-from clos validate-superclass)
  #+lcl4.1
  (:import-from lucid-common-lisp restart-case find-restart invoke-restart)
  #+MCL
  (:import-from ccl class-prototype class-direct-superclasses class-direct-subclasses
                class-precedence-list)
  #+Allegro
  (:import-from clos class-prototype class-direct-superclasses class-direct-subclasses
                class-precedence-list class-finalized-p)
  #+LISPWORKS
  (:import-from clos class-prototype class-direct-superclasses class-direct-subclasses
                class-precedence-list class-finalized-p validate-superclass)
  )

(defpackage CLIP-USER
  (:use #+CLTL2 COMMON-LISP #-CLTL2 LISP
        #+CLTL2 COMMON-LISP-USER #-CLTL2 USER
        CLIP
	#+Explorer TICL #+EXplorer TICLOS
        #+(or allegro Explorer lcl4.0) CLOS))

;;; ***************************************************************************
;;; EOF



