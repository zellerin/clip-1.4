;;;;-*- Mode:Common-Lisp; Package:CLIP; Base:10 -*-
;;;; *-* Last-edit: Tuesday, October 5, 1993  16:32:26; Edited-By: Westy *-* 

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                        Define-Simulator Macro                          *
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
;;;  11-22-93 File Created.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :clip)

;;; --*--
;;; ***************************************************************************

(defvar *last-simulator-loaded* nil)

(defclass simulator (named-object-mixin remember-instances object-with-properties)
  ((system-name         :initarg :system-name)
   (system-version-hook :initarg :system-version-hook)
   (reset-system-hook   :initarg :reset-system-hook)
   (start-system-hook   :initarg :start-system-hook)
   (stop-system-hook    :initarg :stop-system-hook)
   (schedule-function-hook :initarg :schedule-function-hook)
   (deactivate-scheduled-function-hook :initarg :deactivate-scheduled-function-hook)
   (seconds-per-time-unit :initarg :seconds-per-time-unit)
   (timestamp-function :initform nil :initarg :timestamp-function))
  (:metaclass named-class))

(defmacro define-simulator (name &key 
		     system-name  
		     system-version 
		     reset-system   
		     start-system   
		     stop-system   
		     schedule-function 
		     deactivate-scheduled-function 
		     seconds-per-time-unit
		     timestamp)
  "Define the interface to a simulation. The following args are recognized:
:SYSTEM-NAME system-name
:SYSTEM-VERSION function or form that handles same args as :BEFORE-EXPERIMENT 
:START-SYSTEM   function or form that handles same args as :BEFORE-TRIAL
:RESET-SYSTEM   function or form that handles same args as :BEFORE-TRIAL
:STOP-SYSTEM    function or form that handles same args as :BEFORE-TRIAL
:SECONDS-PER-TIME-UNIT value (default is 1)

:SCHEDULE-FUNCTION (lambda (function time period name) . body) - interface to
  system event scheduling mechanism
:DEACTIVATE-SCHEDULED-FUNCTION (lambda (scheduled-function-object) . body) - interface
  to system event unscheduling mechanism
:TIMESTAMP a function that returns the current simulation time
"
  `(eval-when (load compile eval)
     ,@(when seconds-per-time-unit
	 `((when ',seconds-per-time-unit
	     (setf (seconds-per-time-unit) ',seconds-per-time-unit))))
     (setf *last-simulator-loaded*
           (make-instance 'simulator
             :name ',name
             :system-name ',(if system-name system-name name)
             :system-version-hook ',system-version 
             :reset-system-hook ',reset-system   
             :start-system-hook ',start-system   
             :stop-system-hook ',stop-system   
             :schedule-function-hook ',schedule-function
             :deactivate-scheduled-function-hook ',deactivate-scheduled-function 
             :seconds-per-time-unit ',seconds-per-time-unit 
             :timestamp-function ',timestamp))
     ',name))

;;; ***************************************************************************
;;; EOF
