;;;; -*- Mode:Common-Lisp; Package:USER; Base:10 -*-

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

(in-package common-lisp)

;;; --*--
;;; ***************************************************************************

(defpackage CLIP
  (:use COMMON-LISP)
  (:export "DEFCLIP" "DEFINE-EXPERIMENT" "DEFINE-SIMULATOR"
	   "WRITE-CURRENT-EXPERIMENT-DATA"
           "TRIAL-NUMBER" "TIMESTAMP" "RUN-EXPERIMENT"
           "SHUTDOWN-AND-RERUN-TRIAL"
           "SHUTDOWN-AND-RUN-NEXT-TRIAL"
           "SHUTDOWN-EXPERIMENT"
	   "*DATA-SEPARATOR-CHARACTER*"
	   "*OUTPUT-FORMAT*"
	   "*CURRENT-EXPERIMENT*"
           ;; Introspection for editor
           "LIST-NAMED-OBJECTS" "CLIP-USERS"
           )
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
  (:use COMMON-LISP  CLIP))

;;; ***************************************************************************
;;; EOF
