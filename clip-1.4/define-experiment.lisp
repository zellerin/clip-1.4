;;;-*- Mode:Zetalisp; Package:CLIP; Base:10 -*-
;;;; *-* Last-edit: Tuesday, October 26, 1993  17:23:58; Edited-By: Westy *-* 

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                       Define-Experiment Macro                          *
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

(defmacro define-experiment (name arguments &body body &aux documentation)
  "Defines an experiment named `name' that can be run using the
`run-experiment' function.

:SIMULATOR should be the name used in a previous define-simulation form.

:BEFORE-TRIAL and :AFTER-TRIAL are called with the current values of the
independent variables and the arguments.  :BEFORE-EXPERIMENT and
:AFTER-EXPERIMENT are called with the just the arguments.  The value of all of
these should be a form that refers to the arguments or a function that handles
the correct number of arguments.

:INSTRUMENTATION is a list of names defined with `defclip' which will be
enabled during the experiment.  Use `write-current-experiment-data' in the    
after-trial code to write the data to the appropriate output files.

:IVS    ({(var exp) | (var <loop-for-clause>)}*) - define independent variables
  Example: ((iv1 from 1 to 6 by 5)
            (iv2 '(a b)))

:LOCALS ({(var exp) | var}*) - bind variables in the context of the experiment

:SCRIPT ({(descriptive-name initial-time [next-time] form) | script-element-name}*)
`initial-time' can be a string, a number or form to evaluate.  `next-time' is
a time interval and should be a fixnum or form.

The way things work:
 Run :BEFORE-EXPERIMENT code with args
 LOOP
   Update IVs for trial
   Run :BEFORE-TRIAL code with IV's and args
   Call :RESET-SYSTEM function with IV's and args
   Instantiate Script Events
   Reset and Enable all the instrumentation
   Call :START-SYSTEM function with IV's and args
   <system runs (possible periodic and event based collection done)>
   Run :AFTER-TRIAL code (possible post-mortem collection done)
 END LOOP when all trials completed
 Run :AFTER-EXPERIMENT code with args
"
  
  #+Explorer
  (declare (arglist name arguments [documentation]
                    &key
		    simulator
		    before-experiment
                    before-trial after-trial
                    after-experiment
                    script
                    instrumentation
                    ivs locals
                    system-name
                    system-version
                    start-system
                    reset-system
                    stop-system
                    schedule-function
                    deactivate-scheduled-function
                    seconds-per-time-unit
                    timestamp))
  (setf documentation
	(if (stringp (car body)) (pop body) "No documentation supplied."))
  (el::with-keywords-bound ((simulator
			      before-experiment
			      before-trial after-trial
			      after-experiment
			      script
			      instrumentation
			      #+IGNORE
			      end-of-trial-time
			      ((ivs :variables :experiment-variables :ivs))
			      locals
			      system-name
			      system-version
			      start-system
			      reset-system
			      stop-system
			      schedule-function
			      deactivate-scheduled-function
			      seconds-per-time-unit
                              timestamp)
			    body
			    "~s is not a valid keyword for DEFINE-EXPERIMENT.")
    (labels (#+OLD
             (build-code-call (code-name code &optional arguments)
               (etypecase code
                 (symbol   `',code)
                 (function `',code)
                 (cons
                  (if (eq (first code) 'lambda)
                  `(defun ,(el::form-symbol "%" name "-" code-name "-FUNCTION" )
                          ,@(rest code))
		  `(defun ,(el::form-symbol "%" name "-" code-name "-FUNCTION" )
			  ,arguments
		     ;; get rid of not referenced errors
		     ,.(extract-arguments-from-lambda-list arguments)
		     ,code)))))
             #+OLD
             (process-script-specs (specs)
               `(progn
                  ,@(mapcar
                      #'(lambda (element)
                          (cond ((atom element)
                                 `(schedule (find-script-element ',element)))
                                ((= (length element) 1)
                                 `(schedule (find-script-element ',(first element))))
                                ((= (length element) 3)
                                 (let* ((name (first element))
                                        (time (second element))
                                        (code (third element)))
                                   (check-type name symbol)
                                   (check-type time (or number string cons symbol))
                                   (check-type code cons)
                                   `(schedule-function
                                      ,(build-code-call name code)
                                      (parse-time-specifier ,time)
                                      :name ',name)))
                                ((= (length element) 4)
                                 (let* ((name (first element))
                                        (time (second element))
                                        (repeat-time (third element))
                                        (code (fourth element)))
                                   (check-type name symbol)
                                   (check-type time (or number string cons symbol))
                                   (check-type repeat-time (or number cons symbol))
                                   (check-type code cons)
                                   `(schedule-function
                                      ,(build-code-call name code)
                                      (parse-time-specifier ,time)
                                      :period (parse-time-specifier ,repeat-time :interval-p t)
                                      :name ',name)))
                                (t
                                 (error "improper script element specification; ~s" element))))
                      specs))))

      
      ;; Attempt to default values from simulation object
      (let ((sim (and simulator (find-instance-by-name simulator 'simulator))))
	(when sim
	  (macrolet ((get-from-sim (var slot)
				   `(setf ,var (or ,var (slot-value sim ',slot)))))
	    (get-from-sim system-name system-name)
	    (get-from-sim system-version system-version-hook)
	    (get-from-sim reset-system reset-system-hook)
	    (get-from-sim start-system start-system-hook) 
	    (get-from-sim stop-system stop-system-hook)
	    (get-from-sim schedule-function schedule-function-hook)
	    (get-from-sim deactivate-scheduled-function deactivate-scheduled-function-hook)
	    (get-from-sim seconds-per-time-unit seconds-per-time-unit)
	    (get-from-sim timestamp timestamp-function))))

      
      (let* ((ivs-vars     (mapcar #'first  ivs))
             (ivs-elements (mapcar #'rest ivs))
	     (local-symbols      (mapcar #'(lambda (item) (if (consp item) (first item) item)) locals))
	     (local-init-values  (mapcar #'(lambda (item) (when (consp item) (second item))) locals))
	     (local-values-init-function
	       (build-code-call 
                name
		 'init-local-values
		 `(progn
		    ,@(mapcar #'(lambda (symbol value)
				  `(store-local *current-experiment* ',symbol ,value))
			      local-symbols
			      local-init-values))
		 `(,@arguments)))
             (before-experiment-function 
	       (build-code-call 
                name
                'before-experiment (build-locals-accessor-code-wrapper
						     local-symbols             
						     before-experiment)
				`(,@arguments)))
             (before-trial-function      
	       (build-code-call
                 name
                'before-trial (build-locals-accessor-code-wrapper
						local-symbols             
						before-trial)
                                ;; Reversed order because of lambda-list-keywords. (Rubinstein)
				`(,@ivs-vars ,@arguments)))
	     (stop-system-function 
	       (build-code-call
                 name
                'stop-system    
				(build-locals-accessor-code-wrapper
				  local-symbols       
				  stop-system)
				`(,@ivs-vars ,@arguments)))
	     (reset-system-function 
	       (build-code-call
                 name
                'reset-system 
				(build-locals-accessor-code-wrapper
				  local-symbols       
				  reset-system)
				`(,@ivs-vars ,@arguments)))
	     (start-system-function 
	       (build-code-call 
                name
		 'start-system 
		 (build-locals-accessor-code-wrapper
		   local-symbols       
		   start-system)
		 `(,@ivs-vars ,@arguments)))
	     (system-version-function 
	       (build-code-call 
                 name
		 'system-version 
		 (build-locals-accessor-code-wrapper
		   local-symbols       
		   system-version)
		 `(,@arguments)))
	     (after-trial-function       
	       (build-code-call 
                 name
                'after-trial (build-locals-accessor-code-wrapper
					       local-symbols             
					       after-trial)
                                ;; Reversed order because of lambda-list-keywords. (Rubinstein)
				`(,@ivs-vars ,@arguments)))
	     (after-experiment-function  
	       (build-code-call
                 name
                'after-experiment (build-locals-accessor-code-wrapper
						    local-symbols             
						    after-experiment)
				`(,@arguments)))
	     (ivs-elements-init-function 
	       (build-code-call 
                 name
		 'init-ivs-elements
		 `(progn
		    (setf (slot-value *current-experiment* 'ivs-elements)
			  (list ,@(mapcar #'(lambda (element-list)
					      (if (intersection '(from downfrom upfrom in) element-list 
								:key #'(lambda (item)
									 (if (symbolp item)
									     (symbol-name item)
									     ""))
								:test #'string-equal)
						  `(loop for val ,@element-list
							 collect val)
						  (first element-list))
					      )
					  ivs-elements))
			  #+OLD
			  (list ,@ivs-elements)
			  ))
		 `(,@arguments)))
	     (script-setup-function
	       (build-code-call name 'script-setup (process-script-specs name script))))
	
        `(progn
           ;; Create the experiment instance.
           ,@(mapcar #'build-iv-defclip-form ivs-vars)
           ,(build-timestamp-defclip-form (if (consp timestamp) 
                                           (second timestamp)
                                           'timestamp))
           (make-instance
             'experiment
             :name ,(string name)
             :description ,(string documentation)
             :arguments ',arguments
             :ivs ',ivs-vars
             :ivs-element-init-function ,ivs-elements-init-function 
             :locals ',local-symbols
	     :locals-init-function ,local-values-init-function
             :before-experiment-function ,before-experiment-function
             :before-trial-function      ,before-trial-function
             :after-trial-function       ,after-trial-function
             :after-experiment-function  ,after-experiment-function
             :script-setup-function ,script-setup-function
             :instrumentation-names ',instrumentation
             :system-name ',system-name
             :system-version-hook ,system-version-function
             :start-system-hook   ,start-system-function
             :reset-system-hook   ,reset-system-function
             :stop-system-hook    ,stop-system-function
             :schedule-function-hook ,(build-code-call 
                                       name
                                       'schedule-function schedule-function
                                       '(function time period name))
             :deactivate-scheduled-function-hook ,(build-code-call
                                                   name
                                                   'deactivate-scheduled-function
                                                   deactivate-scheduled-function
                                                   '(event))
	     :seconds-per-time-unit ,(or seconds-per-time-unit 1)
             :timestamp-function ',(if (consp timestamp) 
                                     (first timestamp)
                                     timestamp)
             :timestamp-clip-name ',(if (consp timestamp) 
                                     (second timestamp)
                                     'timestamp)
             )
           ',name)))))

(defun build-iv-defclip-form (iv)
  `(defclip ,iv ()
     (:class simple-instrumentation)
     (assert *current-experiment* () "there is no experiment currently running.")
     (get-local *current-experiment* ',iv)))

(defun build-timestamp-defclip-form (&optional (name 'timestamp))
  `(defclip ,name ()
     ()
     (system-timestamp)))

(defun build-locals-accessor-code-wrapper (locals code)
  (when code
    (if (or (symbolp code) (null locals))
	code
	`(symbol-macrolet
	   ,(mapcar #'(lambda (local)
			`(,local (get-local *current-experiment* ',local)))
		    locals)
	   ,code))))

(defun build-code-call (experiment-name code-name code &optional arguments)
  (etypecase code
    (symbol   `',code)
    (function `',code)
    (cons
     (if (eq (first code) 'lambda)
       `(defun ,(el::form-symbol "%" experiment-name "-" code-name "-FUNCTION" )
               ,@(rest code))
       `(defun ,(el::form-symbol "%" experiment-name "-" code-name "-FUNCTION" )
               ,arguments
          ;; get rid of not referenced errors
          ,.(extract-arguments-from-lambda-list arguments)
          ,code)))))


;;; ============================================================================
;;; Changed to call `schedule-function' correctly.  This function is called in
;;; expanding the macro `define-experiment'

(defun process-script-specs (experiment-name specs)
  `(progn
     ,@(mapcar
	 #'(lambda (element)
	     (cond ((atom element)
		    `(schedule (find-script-element ',element)))
		   ((= (length element) 1)
		    `(schedule (find-script-element ',(first element))))
		   ((= (length element) 3)
		    (let* ((name (first element))
			   (time (second element))
			   (code (third element)))
		      (check-type name symbol)
		      (check-type time (or number string cons symbol))
		      (check-type code cons)
		      `(schedule-function
			 ,(build-code-call experiment-name name code)
			 (parse-time-specifier ,time)
			 ;; Changed calling form.  SDA  11/22/93
			 nil
			 ',name)))
		   ((= (length element) 4)
		    (let* ((name (first element))
			   (time (second element))
			   (repeat-time (third element))
			   (code (fourth element)))
		      (check-type name symbol)
		      (check-type time (or number string cons symbol))
		      (check-type repeat-time (or number cons symbol))
		      (check-type code cons)
		      `(schedule-function
			 ,(build-code-call experiment-name name code)
			 (parse-time-specifier ,time)
			 ;; Changed calling form.  SDA  11/22/93
			 (parse-time-specifier ,repeat-time :interval-p t)
			 ',name)))
		   (t
		    (error "improper script element specification; ~s" element))))
	 specs)))

#+OLD
(defun process-script-specs (experiment-name specs)
  `(progn
     ,@(mapcar
        #'(lambda (element)
            (cond ((atom element)
                   `(schedule (find-script-element ',element)))
                  ((= (length element) 1)
                   `(schedule (find-script-element ',(first element))))
                  ((= (length element) 3)
                   (let* ((name (first element))
                          (time (second element))
                          (code (third element)))
                     (check-type name symbol)
                     (check-type time (or number string cons symbol))
                     (check-type code cons)
                     `(schedule-function
                       ,(build-code-call experiment-name name code)
                       (parse-time-specifier ,time)
                       :name ',name)))
                  ((= (length element) 4)
                                 (let* ((name (first element))
                                        (time (second element))
                                        (repeat-time (third element))
                                        (code (fourth element)))
                                   (check-type name symbol)
                                   (check-type time (or number string cons symbol))
                                   (check-type repeat-time (or number cons symbol))
                                   (check-type code cons)
                                   `(schedule-function
                                      ,(build-code-call experiment-name name code)
                                      (parse-time-specifier ,time)
                                      :period (parse-time-specifier ,repeat-time :interval-p t)
                                      :name ',name)))
                                (t
                                 (error "improper script element specification; ~s" element))))
                      specs)))

;;; ***************************************************************************
;;; EOF
