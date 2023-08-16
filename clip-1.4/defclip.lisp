;;;; -*- Mode:Lisp; Package:CLIP; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/systems/clip/development/defclip.lisp *-*
;;;; *-* Last-edit: Wednesday, January 5, 1994  18:23:15; Edited-By: Westy *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                              Defclip Macro                             *
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
;;;  09-17-93 File Created.  (WESTY)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :clip)

;;; --*--
;;; ***************************************************************************

(defun build-standard-code-function (name form args &optional function)
  (if form
    `(defun ,name (,@args)
       ;; eliminate not-referenced errors
       ,@(extract-arguments-from-lambda-list args)
       ,form)
    (when function
      `(defun ,name (&rest args)
         (apply ',function args)))))

(defun build-report-code-function (name form args &optional build-standard composite-child-p)
  (cond (form
	 (etypecase form
           (cons (build-standard-code-function 
                  name 
                  ;; eliminate not-referenced errors
                  form
                  `(instrumentation *instrumentation-report-stream*
                                    extracter ,@args)))
           (symbol (build-standard-code-function name nil args form))))
	(t
	 (when build-standard
	   (if composite-child-p
	       ;; Children of composites do not use the arguments to index the stored values.
	       `(defun ,name (instrumentation *instrumentation-report-stream* extracter)
		  (standard-report-function instrumentation *instrumentation-report-stream*
					    extracter))
	       `(defun ,name (instrumentation *instrumentation-report-stream* extracter ,@args)
                  ,(if (member '&rest args)
		     `(apply 
                       #'standard-report-function 
                       instrumentation 
                       *instrumentation-report-stream*
                       extracter ,@(extract-arguments-from-lambda-list args))
                     `(standard-report-function 
                       instrumentation 
                       *instrumentation-report-stream*
                       extracter ,@(extract-arguments-from-lambda-list args)))))))))

(defun build-report-key (name args &optional composite-child-p)
  (if composite-child-p
    ;; Children of composites do not use the arguments to index the stored values.
    (string-capitalize name)
    (let ((clasp-format (format nil "~a~{~~@[ ~~a~~]~*~^ ~}"
				(substitute #\- #\space (string-capitalize name))
				args)))
      (if (null args)
	  clasp-format 
	  (list
	    clasp-format 
	    ;; ASCII format
	    (format nil "~a~{~~@[-~~a~~]~*~^-~}"
		    (substitute #\- #\space (string-capitalize name))
		    args))))))
      
;;;----------------------------------------------------------------------------
;;; DEFCLIP

(defvar *parent-table* (make-hash-table)
; Remember, only the children of composites use this.  They are
; the only ones with parents.
  "Stores the parents of children of composites.")

(defun make-instrumentation (class-name &rest initargs)
  (apply #'make-instance class-name initargs))

(defmacro defclip (name args &rest body &aux documentation code)
  
  "Defines a method for managing a set of functions used to collect data.

:COMPONENTS is list of clips that are associated with this clip; ie., they are
collected and reported as a group by collecting or report this clip.
:MAP-FUNCTION provides a list of items to map the `components' over.
:REPORT-KEY allows overriding of the default key output to the data stream.
:PERIOD provides the time interval between collections, if this is not specified
collection will be done immediately prior to the report is output to the data
stream.
:TRIGGER-EVENT is a function which triggers collection
:ENABLE-FUNCTION, :DISABLE-FUNCTION, :RESET-FUNCTION, :DISPLAY-FUNCTION are code
that will be called to enable, disable, reset and display the clip.
:REPORT-FUNCTION can be used to override the default report function.  

 For example:

 Another simple one with code used to report a value:
   \(defclip number-of-dead-bulldozers ()
     \(:report-key \"# of Dead bulldozers\"\)

     (length (bds-that-died)))

 An example showing one with components:
  \(defclip methods-each-bd (bulldozer)
    \"Salient info for each instance of applying a recovery method:\"
    (:components (trial-number agent-name method-type failure-type calculate-recovery-cost 
                  method-succeeded-p order-of-application)
     :map-function (gather-recovery-method-instances (name-of bulldozer)))
    ;; This code executes before the map-function is executed.    
    (send (fire-system) :set-frame-system (name-of bulldozer)))

 This one reports no values it simply provides an interface to some metering code:
   \(defclip scorched-earth-meter ()
     \"Records amount of torched terrain.\"
     \(:enable-function  (send (fire-system) :turn-on-scorched-earth-metering)
      :disable-function (send (fire-system) :turn-off-scorched-earth-metering)
      :reset-function   (send (fire-system) :reset-scorched-earth-meter)
      :display-function (send (fire-system) :display-scorched-earth-meter)))

"
  #+Explorer
  (declare (arglist name args [documentation] (&key
                                                enable-function disable-function
                                                reset-function display-function
                                                report-function report-key
                                                period
						function
                                                initial-status
                                                components map-function
                                                output-file
                                                time-series)
                    &body body))
  
  (setf documentation
	(if (stringp (car body)) (pop body) "No documentation supplied."))
  (let ((code-body (rest body)))
    (when code-body
      (when (and (consp (first code-body))
                 (eq (first (first code-body)) 'declare))
        (cerror "Ignore the declaration" "~a not allowed in body" (first code-body))
        (pop code-body)))
    (setf code `(;; eliminate not-referenced errors
                 ,@(extract-arguments-from-lambda-list args)
                 ,@(or code-body
                       `((values ,@args))))))
  (el::with-keywords-bound ((enable-function disable-function reset-function display-function
                                           report-function report-key
                                           ((period :period :schedule))
					   ((trigger-spec :function :trigger-event))
                                           args-from
                                           initial-status
                                           ((components :components :columns))
                                           ((unmapped-columns :unmapped-columns :unmapped-components))
                                           map-function
                                           (output-file nil output-file-supplied-p)
                                           (time-series nil time-series-supplied-p)
                                           (combiner nil combiner-supplied-p)
                                           (extracter nil extracter-supplied-p)
                                           class parent)
                              (first body)
                              "~s is not a valid keyword for DEFCLIP.")
    (assert (member initial-status '(nil :enabled :disabled)) ()
            "`:initial-status' should be one of :ENABLED or :DISABLED")
    
    (assert (or (null map-function) (not (null components))) ()
            "If `:map-function' is specified, `:columns' must also be.")
    (assert (not (and components map-function report-function)) ()
            "If `:components' are specified, `:report-function' should not be.")
    (let* ((parent-name (or parent (gethash name *parent-table*)))
           (enable-function-name  (el::form-uninterned-symbol name "-ENABLE-FUNCTION"))
           (disable-function-name (el::form-uninterned-symbol name "-DISABLE-FUNCTION"))
           (reset-function-name   (el::form-uninterned-symbol name "-RESET-FUNCTION"))
           (display-function-name (el::form-uninterned-symbol name "-DISPLAY-FUNCTION"))
           (report-function-name  (el::form-uninterned-symbol name "-REPORT-FUNCTION"))
           (map-function-name     (el::form-uninterned-symbol name "-MAP-FUNCTION")))

      (multiple-value-bind (class-name superclasses)
			   (derive-instrumentation-class
			    class parent-name period components map-function time-series trigger-spec)
      
      ;(spy name parent-name superclasses class-name)
      ;; Update the hash table at compile time so that components of the composite will
      ;; now how to make themselves.
      (update-parents-children name (and (not map-function) components))
      
      `(progn
         ;; Define the functional interface to this instrumentation.
         (defun ,name ,args
           ,(format nil "Functional interface to `~(~a~)' ~(~a~).~@[~%~a~]"
                    name
		    class-name
                    documentation)
           . ,code)
               
         ,(build-standard-code-function enable-function-name enable-function nil)
         ,(build-standard-code-function disable-function-name disable-function nil)
         ,(build-standard-code-function reset-function-name reset-function nil)
         ,(build-standard-code-function display-function-name display-function args)
	 
	 
	 ;; Define the report function.
	 ,(cond ((member 'mapping-instrumentation-mixin superclasses)
		 ;; For super-instrumentation the report-function is basically an init
		 ;; function that runs before all of the components.
		 (if code
		     (build-report-code-function report-function-name
                                                 `(apply ',name args)
                                                 '(&rest args))
		     (setf report-function-name nil)))
		((member 'composite-instrumentation-mixin superclasses)
		 ;; For composite-instrumentation the report function generates the values
                 ;; that are passed on to the components.
                 (build-report-code-function report-function-name 
                                             `(apply ',name args)
                                             '(&rest args)))
                                             
                                              ; `(progn
                                               ;   instrumentation stream
                                               ;   (apply ',name args))
                                               ;'(instrumentation stream &rest args)))

		((or (null superclasses)
		     (equal superclasses '(child-of-composite-instrumentation-mixin))
		     (equal superclasses '(scheduled-instrumentation-mixin))
		     (equal superclasses '(functional-instrumentation-mixin)))
			    
		 ;; For standard style instrumentation the report function is responsible
		 ;; for outputing the data to the data output stream. It can be directly
		 ;; specified by the user.
		 (if report-function
		     ;; Define a user-specifed report-function.
		     (build-report-code-function report-function-name report-function
                                                 args parent-name)
		     ;; Define a report-function which simply calls the functional interface function.
		     (when code
		       (build-report-code-function report-function-name nil args name parent-name))))
		(t
		 (error "cannot figure out how to define a report function for these superclasses; ~a"
			 superclasses)))
	 
         ;; Define map function.
         ,(build-standard-code-function map-function-name map-function args)
	 
	 ;; kill any old ones
	 (mapcar #'kill (find-all-instrumentation ',name))
         ;; Make the instance.
         (make-instrumentation
           ',class-name
           :name     ',name
           ,@(when parent-name
               `(:parent (find-instrumentation ',parent-name)))
           :status   ',(or initial-status
                           (if (or enable-function disable-function)
                             :disabled
                             :enabled))
           ,@(when enable-function
               `(:enable-function  ',enable-function-name))
           ,@(when enable-function
               `(:disable-function ',disable-function-name))
           ,@(when reset-function
               `(:reset-function   ',reset-function-name))
           ,@(when display-function              
               `(:display-function ',display-function-name))
           
           :arguments    ',args
           ;; Unless this is a mapping-instrumentation-mixin with no code define a
           ;; report function. The report function of a mapping-instrumentation-mixin
           ;; is just init code that is run before the subs.
           ,@(unless (null report-function-name)
               `(:report-function  ',report-function-name ))
           :report-key   ',(or report-key (build-report-key name args parent-name))
           :documentation ',documentation
           ,@(when output-file-supplied-p
               `(:output-file-name ',output-file))
           ,@(when time-series-supplied-p
               `(:time-series? ',time-series))
           ,@(when combiner-supplied-p
               `(:combiner ',combiner))
           ,@(when extracter-supplied-p
               `(:extracter ',extracter))
           
	   ;; Add class specific slots
	   ,@(when (member 'column-producing-instrumentation-mixin superclasses)
	       `(:components ',components
                 ,@(when unmapped-columns
                     `(:unmapped-columns ',unmapped-columns))))
               
	   ,@(when (member 'mapping-instrumentation-mixin superclasses)
	       `(:map-function ',map-function-name))
	   ,@(when (member 'scheduled-instrumentation-mixin superclasses)
	       `(:scheduler-args ',period))
			     
	   ,@(when (member 'functional-instrumentation-mixin superclasses)
	       `(:trigger-events ',(parse-trigger-spec trigger-spec args)))
	   )
	 ;; Update components to know they have a parent.
	 ,@(if (member 'composite-instrumentation-mixin superclasses)
	       (build-record-children-forms name components)
	       (build-record-children-forms name nil)))))
  ))

;; Setting this to T disables the ability to define children before their parents.
;; Setting it to NIL does not work.

(defvar *force-subclip-redefine* t)

(defun build-record-children-forms (name components)
  (append 
    (mapcan #'(lambda (kid-name)
		`((when
                      (or *force-subclip-redefine*
			  ;; Need to make `find-instrumentation' work on subclasses automagically.
			  (and (not (find-instrumentation ',kid-name t 'child-of-composite-instrumentation))
			       (not (find-instrumentation ',kid-name t 'composite-child-of-composite-instrumentation))))
                    (defclip ,kid-name (value)
                             (:parent ,name
                              :class child-of-composite-instrumentation)))
                  (update-childs-parents ',name ',kid-name)))
	    components)
    `((update-parents-children ',name ',components))))

(defun update-childs-parents (parent-name child-name &aux instr)
  (if (setf instr (or (find-instrumentation child-name t 'child-of-composite-instrumentation)
		      (find-instrumentation child-name t 'composite-child-of-composite-instrumentation)))
    (setf (parent instr) (find-instrumentation parent-name))))

(defun update-parents-children (name components)
  (if components
      (dolist (child-name  components)
	;; Stick it in the hash table so that forward references work.
	(setf (gethash child-name *parent-table*) name))
      ;; Just for sanities sake I will remove all hash table entries whose value is
      ;; this non-composite instrumentation.
      (maphash #'(lambda (key value)
		   (when (eq value name)
		     (remhash key *parent-table*)))
	       *parent-table*)))

(defun parse-trigger-spec (trigger-spec clip-args)
  ;; Convert single spec to a list of specs first...
  (cond ((or
          ;; A function name
          (atom trigger-spec)
          ;; A list of the form \(function :BEFORE\)
          (and (consp trigger-spec)
               (atom (first trigger-spec))
	       (keywordp (second trigger-spec))))
         (setf trigger-spec (list trigger-spec))))
  
  (cond
   ;; A list of function names
   ((every #'atom trigger-spec)
    (loop for spec in trigger-spec
	  collect  `(,spec :AFTER ,(if clip-args :ARGS-AND-VALUES :NONE))))
   ((consp trigger-spec)
    (loop for (function . options) in trigger-spec 
	  for when = (if (member :BEFORE options) :BEFORE :AFTER)
	  for predicate = (when (member :PREDICATE options)
				(second (member :PREDICATE options)))
	  when (eq when :BEFORE)
	  collect `(,function :BEFORE ,(if clip-args :ARGS :NONE) ,predicate)
	  else
	  collect `(,function :AFTER ,(if clip-args :ARGS-AND-VALUES :NONE) ,predicate)))))

#+EXPLORER
(zwei:define-indentation defclip (1 1 2 1 3 1 4 1))
#+EXPLORER
(setf (get :components 'zwei:lisp-indent-offset) '(1 0 2 0 3 0 4 0))

(defmethod find-subclass-if (predicate (class standard-class))
  (let ((subclasses-examined-so-far nil))
    (labels ((find-subclass-if-helper (pred class)
	       (unless (member class subclasses-examined-so-far)
		 (when (funcall pred class)
		       (return-from find-subclass-if class))
                 (let ((subclasses (class-direct-subclasses class)))
		   (push class subclasses-examined-so-far)
		   (when subclasses
			 (loop for subclass in subclasses do
			       (find-subclass-if-helper pred subclass)))))))
	    (find-subclass-if-helper predicate class))))

(defun set-eq-p (s1 s2)
  (and (subsetp s1 s2 :test #'eq)
       (subsetp s2 s1 :test #'eq)))

(defun inst-cpl (x)
  #+Explorer
  (unless (class-finalized-p *instrumentation-class*)
    (finalize-inheritance *instrumentation-class*))
  #+Explorer
  (unless (class-finalized-p x)
    (finalize-inheritance x))
  (remove x
	  (set-difference (class-precedence-list x)
			  (class-precedence-list *instrumentation-class*))))

(defun find-instrumentation-class (subclasses &optional no-error-p)
  (cond ((find-subclass-if #'(lambda (x)
			       (let ((partial-cpl (inst-cpl x)))
				 (set-eq-p subclasses partial-cpl)))
			   *instrumentation-class*))
	(no-error-p nil)
	(t 
	 (error "No instrumentation class with superclasses ~a found." subclasses))))

;; Note that this will currently not allow super-instrumentation to have parents (parents being composite, that is).
;; There is no conceptual reason for this, I have just not written the necessary code.
(defun derive-instrumentation-class (class parent-name period components map-function time-series trigger-spec
					   &aux superclasses)
  (cond (class
	 (setf superclasses
	       (mapcar #'class-name (inst-cpl (find-class class)))))
	
	(t
	 (macrolet ((pc (&rest args) `(setf superclasses (append superclasses ',args))))
	   (when parent-name
		 (pc child-of-composite-instrumentation-mixin))
	   (when period (pc scheduled-instrumentation-mixin))
	   (when (and period components)
		 (pc time-series-instrumentation-mixin))
	   (when components
		 (pc column-producing-instrumentation-mixin)
		 (if map-function
		     (pc mapping-instrumentation-mixin)
		   (pc composite-instrumentation-mixin)))
	   (when time-series
		 (pc time-series-instrumentation-mixin))
	   (when trigger-spec 
		 (pc functional-instrumentation-mixin time-series-instrumentation-mixin)))))
  (values
   (or class
       (and (null superclasses) 'instrumentation)
       (class-name (find-instrumentation-class (mapcar #'find-class superclasses))))
   superclasses))

#|
(class-name (cond (class class)
		  ((equal superclasses '(child-of-composite-instrumentation))
		   'child-of-composite-instrumentation)
		  ((equal superclasses '(super-instrumentation))
		   'super-instrumentation)
		  ((equal superclasses '(composite-instrumentation))
		   'composite-instrumentation)
		  ((equal superclasses '(periodic-instrumentation))
		   'periodic-instrumentation)
		  ((equal superclasses '(instrumentation))
		   'instrumentation)
		  ((equal superclasses '(functional-instrumentation))
		   'functional-instrumentation)
		  ((null (set-difference '(functional-instrumentation composite-instrumentation)
					 superclasses))
		   'functional-composite-instrumentation)
		  ((null (set-difference '(periodic-instrumentation super-instrumentation)
					 superclasses))
		   'periodic-super-instrumentation)
		  ((null (set-difference '(periodic-instrumentation composite-instrumentation)
					 superclasses))
		   'periodic-composite-instrumentation)
		  ((null (set-difference '(child-of-composite-instrumentation
					   composite-instrumentation)
					 superclasses))
		   'composite-child-of-composite-instrumentation)
		  ((null (set-difference '(child-of-composite-instrumentation
					   composite-instrumentation
					   functional-instrumentation)
					 superclasses))
		   'composite-child-of-functional-composite-instrumentation)
		  (t 
		   (error "invalid superclasses; ~a" superclasses)))
|#


;;; ***************************************************************************
;;; EOF