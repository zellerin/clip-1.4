;;;; -*- Mode:Common-Lisp; Package:CLIP; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/mac-files/clip/super-intrinisic-mixins.lisp *-*
;;;; *-* Last-edit: Tuesday, January 19, 1993  16:37:07; Edited-By: Westy *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                          Metaclass definitions                         *
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

(in-package #+CLTL2 CLIP #-CLTL2 'CLIP)

;;; --*--
;;; ***************************************************************************

;; define a metaclass that automatically defines a method for accessing
;; objects by their name.

(defclass our-standard-class (standard-class)
  ())

(defvar *all-class-names* nil)

(defmethod initialize-instance :after ((the-class our-standard-class) &key)
  (pushnew (class-name the-class) *all-class-names*))

(defclass named-class (our-standard-class)
  ())

(defmethod initialize-instance :after ((the-named-class named-class) &key)
  #-lcl4.0 ; punt in lucid since the night grows long
  (eval `(defmethod ,(class-name the-named-class) ((name symbol))
	   (get name ',(class-name the-named-class)))))

;; An alternative to `named-class' that can be used to build classes
;; that can be mixed in with classes built with `named-class'.

(defclass basic-class (our-standard-class)
  ())

;; This indicates that it is OK to build a class that has `named-class' as its
;; meta-class and includes classes built on `basic-class' in its superclasses.
(defmethod validate-superclass
	   ((class-prototype named-class) (superclass basic-class))
  t)

(defmethod validate-superclass
	   ((class-prototype named-class) (superclass standard-class))
  t)

(defmethod validate-superclass
	   ((class-prototype basic-class) (superclass standard-class))
  t)

;;; ***************************************************************************
;;; EOF
