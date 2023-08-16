;;;; -*- Mode:Lisp; Package:CLIP; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/systems/clip/development/instrumentation.lisp *-*
;;;; *-* Last-edit: Tuesday, December 7, 1993  14:58:50; Edited-By: File Server *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                       Instrumentation Definitions                      *
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
;;;  10-17-89 File Created.  (Westy)
;;;  01-24-90 Converted to use CLOS.  (Westy)
;;;  07-26-90 Moved `replace-or-push' to FS;MACROS.  (Westy)
;;;  11-01-90 Added `report-form' and `report-key' slots to `instrumentation'
;;;           and added `periodic-instrumentation' defclass.
;;;           Also added `find-instrumentation' and made `execute-action'
;;;           handle arguments (sort of).
;;;           Also added :after methods for enable and disable for 
;;;           `periodic-instrumentation'.
;;;           And, last (and foremost), rewrote `defclip' to
;;;           use some of these new features.  (Westy)
;;;  11-15-90 Changed syntax of `defclip' to allow a body of
;;;           code at the end and got rid of :collection-form arg. Made other
;;;           changes to allow `super-instrumentation' class to work better. 
;;;           (Westy)
;;;  11-28-90 Made nested instrumentations print their data in one row for
;;;           each trial.  (Westy)
;;;  01-19-93 Began portable implementation.  (Westy)
;;;  03-31-93 Added `composite-instrumentation', but just barely. Things that
;;;           will not work include periodic collection of composites and
;;;           composite children that are supers.
;;;           (Westy)
;;;  07-21-93 Added code to deal with *data-separator-character* and
;;;           *output-format*. (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package #+CLTL2 CLIP #-CLTL2 'CLIP)

;;; --*--
;;; ***************************************************************************

#+UNUSED
(defvar *call-tree-print* nil)

#+UNUSED
(defmethod print-report-key-internal :around ((the-instrumentation record-call-tree-mixin) stream toplevel
				     prefix &rest arguments)
  (declare (ignore arguments))
  (let ((*call-tree-print* (cons the-instrumentation *call-tree-print*)))
    (call-next-method)))

(defvar *call-tree-collect* nil)

(defmethod collect-internal :around ((the-instrumentation record-call-tree-mixin) combiner
				     &rest arguments)
  (declare (ignore arguments))
  (let ((*call-tree-collect* (cons the-instrumentation *call-tree-collect*)))
;    (spy the-instrumentation *call-tree-collect*)
    (call-next-method)))

(defvar *call-tree-report* nil)

(defmethod report-internal :around ((the-instrumentation record-call-tree-mixin) stream
				     extracter &rest arguments)
  (declare (ignore arguments))
  (let ((*call-tree-report* (cons the-instrumentation *call-tree-report*)))
    (call-next-method)))
  
;; If this ever changes to something besides nil we will have to rewrite some code.
(defparameter *uncollected-value* nil)

;;; ---------------------------------------------------------------------------

(defvar *instrumentation-report-stream* nil
  "If non-nil this is the stream where instrumentation reports are written.")

;;;----------------------------------------------------------------------------


(defvar *instrumentation-class* (find-class 'instrumentation))

(defvar *all-instrumentation-classes*
  (list* *instrumentation-class* (class-direct-subclasses *instrumentation-class*)))

(defun find-instrumentation (instrumentation-spec &optional no-error-p type)
  "Given a instrumentation specifier this returns the instrumentation instance.
`no-error-p' if non-nil says to return nil if there is no such instrumentation,
instead of signalling an error."
  (if type
      (find-instance-by-name instrumentation-spec type t)
    (cond ((loop with inst
		 for class in *all-instrumentation-classes*
		 until (setf inst (find-instance-by-name instrumentation-spec class t))
		 finally (return inst)))
	  (no-error-p nil)
	  (t (error "no instrumentation named ~s has been defined" instrumentation-spec)))))

(defun find-all-instrumentation (instrumentation-spec)
  (delete-duplicates
    (mapcan #'(lambda (class)
		(let ((ins (find-instrumentation instrumentation-spec t class)))
		  (if ins (list ins))))
	    *all-instrumentation-classes*)))

;;;----------------------------------------------------------------------------
;;; Generic functions

(defgeneric collect (instrumentation &rest arguments)
  (:method ((the-instrumentation-name symbol) &rest arguments)
           (apply #'collect (find-instrumentation the-instrumentation-name)
                  arguments)))


(defgeneric collect-internal (instrumentation combiner &rest arguments))

;;;----------------------------------------------------------------------------

(defmacro call-safe (function &rest args)
  (el::once-only (function)
    `(when ,function
       ,@(if (null args)
           `((funcall ,function))
           `((apply ,function ,@args))))))

;;; ---------------------------------------------------------------------------
;;; Basic methods

(defmethod enable ((the-instrumentation instrumentation))
  (setf (slot-value the-instrumentation 'status) :enabled)
  (call-safe (slot-value the-instrumentation 'enable-function)))

(defmethod disable ((the-instrumentation instrumentation))
  (setf (slot-value the-instrumentation 'status) :disabled)
  (call-safe (slot-value the-instrumentation 'disable-function)))

(defmethod reset ((the-instrumentation instrumentation))
  (reset-internal the-instrumentation))

(defmethod reset-internal ((the-instrumentation instrumentation) &optional propagate)
  (declare (ignore propagate))
  (setf (slot-value the-instrumentation 'value) *uncollected-value*
        (slot-value the-instrumentation 'number-of-samples) nil)
  (call-safe (slot-value the-instrumentation 'reset-function)))

;;;----------------------------------------------------------------------------

(defmethod display ((the-instrumentation instrumentation) &rest arguments)
  (call-safe (slot-value the-instrumentation 'display-function)
             arguments))

(defmethod report ((the-instrumentation instrumentation) *instrumentation-report-stream* &rest arguments)
  (apply #'report-internal the-instrumentation *instrumentation-report-stream* nil  arguments))

(defmethod report-internal :before ((the-instrumentation instrumentation)
				    *instrumentation-report-stream* (extracter t) &rest args)
  (apply #'collect-if-necessary the-instrumentation args))

(defmethod collect-if-necessary ((the-instrumentation instrumentation) &rest args)
  (unless (previously-collected-p the-instrumentation args)
      (apply #'collect the-instrumentation args)))

(defmethod report-internal ((the-instrumentation instrumentation) *instrumentation-report-stream*
			    (extracter t) &rest arguments)
  (call-safe (slot-value the-instrumentation 'report-function)
             the-instrumentation
             *instrumentation-report-stream*
	     extracter 
             arguments))

(defmethod collect ((the-instrumentation instrumentation) &rest arguments)
  (apply #'collect-internal the-instrumentation nil arguments))

(defmethod collect-internal :after ((the-instrumentation instrumentation) (combiner t) &rest arguments)
  (update-number-of-samples the-instrumentation arguments))


;; If this is T the call tree (the sequence of clip invocations) will be prepended
;; onto the list used to assoc values from a clip. This allows clips to store data for
;; multiple parents.
;; If it is NIL only the arguments passed in will be used.

(defvar *use-new-lookup?* t)  

;; Used exclusively in collect methods
(defmethod update-number-of-samples ((the-instrumentation instrumentation) arguments)
  (update-number-of-samples-internal the-instrumentation arguments))

;; Used exclusively in report methods
(defmethod lookup-number-of-samples ((the-instrumentation instrumentation) arguments)
  (when *use-new-lookup?*
    (setf arguments (append (rest *call-tree-report*) arguments)))
  (lookup-equal (slot-value the-instrumentation 'number-of-samples) arguments))

;; Used exclusively in report methods
(defmethod lookup-value ((the-instrumentation instrumentation) arguments)
  (when *use-new-lookup?*
    (setf arguments (append (rest *call-tree-report*) arguments)))
  (lookup-equal (slot-value the-instrumentation 'value) arguments))
  
;; Used exclusively in collect methods
(defmethod update-number-of-samples-internal ((the-instrumentation instrumentation) arguments)
  (when *use-new-lookup?*
    (setf arguments (append (rest *call-tree-collect*) arguments)))
  (with-slots (number-of-samples) the-instrumentation
    (if (null (lookup-equal number-of-samples arguments))
      (setf-lookup-equal number-of-samples arguments 1)
      (setf-lookup-equal number-of-samples arguments 
                         (1+ (lookup-equal number-of-samples arguments))))))

;; Used exclusively in collect methods
(defmethod update-value (new-value (the-instrumentation instrumentation) arguments combiner)
  (when *use-new-lookup?*
    (setf arguments (append (rest *call-tree-collect*) arguments)))
  (with-slots (value (default-combiner combiner)) the-instrumentation
    (setf-lookup-equal value arguments
                       (funcall (or combiner default-combiner)
                                (lookup-equal value arguments)
                                new-value))))

(defmethod collect-internal ((the-instrumentation instrumentation) combiner &rest arguments)
  (with-slots (name) the-instrumentation
    (let* ((values (multiple-value-list (apply name arguments)))
           (new-value (first values)))
      (update-value new-value the-instrumentation arguments combiner)
      (values-list values))))

(defmethod previously-collected-p ((the-instrumentation instrumentation) args)
  (not (eq (lookup-value the-instrumentation args) *uncollected-value*)))

(defgeneric standard-report-function (instrumentation stream extracter &rest args))

(defmethod standard-report-function ((the-instrumentation instrumentation)
				     *instrumentation-report-stream* extracter &rest args)
  (with-slots ((default-extracter extracter)) the-instrumentation
    (let ((extracter (or extracter default-extracter))
          (value (lookup-value the-instrumentation args))
          (number-of-samples (lookup-number-of-samples the-instrumentation args)))
      (standard-value-printer 
       (etypecase extracter
         (number (elt value extracter))
         (function (funcall extracter value number-of-samples))
         (symbol (funcall extracter value number-of-samples)))
       *instrumentation-report-stream*)
      (write-char *data-separator-character* *instrumentation-report-stream*))
    (values)))

(defmethod standard-value-printer ((value t) stream)
  (princ value stream))

(defmethod standard-value-printer ((value ratio) stream)
  (standard-value-printer (float value) stream))

(defmethod print-report-key ((the-instrumentation instrumentation) stream &rest arguments)
  (when (eq *output-format* :CLASP)
    (fresh-line stream)
    (write-char #\" stream))
  (apply #'print-report-key-internal the-instrumentation stream nil nil arguments))

(defmethod print-report-key-internal ((the-instrumentation instrumentation) stream toplevel prefix
				      &rest arguments)
  (declare (ignore toplevel))
  (ecase *output-format*
    (:CLASP
     (format stream "~@[~a~]~?\"~%"
	     prefix
	     (instr.report-key the-instrumentation)  arguments))
    (:ASCII
     (format stream "~@[~a~]~?"
	     prefix
	     (instr.report-key the-instrumentation)  arguments)
     (write-char *data-separator-character* stream))))

(defmethod instr.report-key ((the-instrumentation instrumentation))
  (with-slots (report-key) the-instrumentation 
    (if (consp report-key)
	(ecase  *output-format*
	  (:CLASP
	   (first report-key))
	  (:ASCII
	   (second report-key)))
	 report-key)))

;;;----------------------------------------------------------------------------
;;; Specialized methods for scheduled-instrumentation-mixin.

(defmethod enable :after ((the-scheduled-instrumentation-mixin scheduled-instrumentation-mixin))
  (with-slots (scheduler-args collection-event) the-scheduled-instrumentation-mixin
    (let ((period nil)
	  (start-time nil))
      (cond ((consp scheduler-args)
	     (setf period (parse-time-specifier
			    (getf scheduler-args :period)
			    :interval-p t :zero-error-p t)
		   start-time (getf scheduler-args :start-time))
	     (when start-time
	       (setf start-time (parse-time-specifier start-time))))
	    (t
	     (setf period (parse-time-specifier
			    scheduler-args
			    :interval-p t :zero-error-p t))))
      (setf collection-event
	    (apply #'schedule-function
		   #'(lambda ()
		       (collect the-scheduled-instrumentation-mixin))
		   (or start-time (system-timestamp))
		   period
		   nil
		   (if (consp scheduler-args) scheduler-args nil))))))
       
(defmethod disable :after ((the-scheduled-instrumentation-mixin scheduled-instrumentation-mixin))
  (with-slots (collection-event) the-scheduled-instrumentation-mixin
    (when collection-event
      (deactivate collection-event))))

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; Instrumentation that produce multiple columns

(defmethod enable :after ((the-instrumentation column-producing-instrumentation-mixin))
  (dolist (component (slot-value the-instrumentation 'components))
    (enable (find-instrumentation component))))

(defmethod reset :after ((the-instrumentation column-producing-instrumentation-mixin))
  (dolist (component (slot-value the-instrumentation 'components))
    (reset (find-instrumentation component))))

(defmethod report-components ((the-instrumentation column-producing-instrumentation-mixin)
                              stream extracter
                              &rest arguments)
  (loop for component in (slot-value the-instrumentation 'components) do
        (apply #'report-internal (find-instrumentation component) stream extracter arguments)))

;;; Make them automagically write out `trial-number', the IVS and the timestamp.

(defmethod collect-internal :before ((the-instrumentation column-producing-instrumentation-mixin)
                                     (combiner t) &rest arguments)
  (declare (ignore arguments))
  ;; Do the implicit-clips...
  (when (time-series-p the-instrumentation)
    (when *current-experiment*
      (with-slots (timestamp-clip) *current-experiment* 
	(apply #'collect-internal timestamp-clip combiner nil))))
  (dolist (column (slot-value the-instrumentation 'unmapped-columns))
    (apply #'collect-internal (find-instrumentation column) combiner nil))
  )

(defmethod report-components :before ((the-instrumentation column-producing-instrumentation-mixin)
                                      stream
                                      (extracter t) &rest arguments)
  (declare (ignore arguments))
  (when (time-series-p the-instrumentation)
    (when *current-experiment* 
      (report-internal-implicit-clips *current-experiment*  stream extracter t)))
  (dolist (column (slot-value the-instrumentation 'unmapped-columns))
    (apply #'report-internal (find-instrumentation column) stream extracter nil)))

(defmethod report-internal-implicit-clips ((the-experiment experiment) stream extracter include-timestamp)
  ;; Write trial-number, IVS and timestamp
  (apply #'report-internal (find-instrumentation 'trial-number) stream extracter nil)
  (with-slots (timestamp-clip ivs) *current-experiment* 
    (dolist (iv ivs)
      (apply #'report-internal (find-instrumentation iv) stream extracter nil))
    (when (and include-timestamp timestamp-clip)
      (apply #'report-internal timestamp-clip stream extracter nil))))

(defmethod print-report-key-internal :before ((the-instrumentation column-producing-instrumentation-mixin)
                                     stream toplevel prefix &rest arguments)
  (declare (ignore toplevel prefix arguments))
  (when (time-series-p the-instrumentation)
    ;; the implicit clips are written in write-experiment-headers
    (dolist (column (slot-value the-instrumentation 'unmapped-columns))
      (apply #'print-report-key (find-instrumentation column) stream nil))))

(defmethod print-report-key-implicit-clips ((the-experiment experiment) stream include-timestamp)
  (apply #'print-report-key (find-instrumentation 'trial-number) stream t nil nil)
  (with-slots (timestamp-clip ivs) the-experiment
    (dolist (iv ivs)
      (apply #'print-report-key (find-instrumentation iv) stream nil))
    (when (and include-timestamp timestamp-clip)
      (apply #'print-report-key timestamp-clip stream nil))))


;;; ---------------------------------------------------------------------------
;;; ---------------------------------------------------------------------------
;;; Specialized methods for mapping-instrumentation-mixin

(defvar *not-top-level-in-report* nil)

(defmethod report-internal ((the-mapping-instrumentation-mixin mapping-instrumentation-mixin) stream (extracter t)
			    &rest arguments)
  (call-safe (slot-value the-mapping-instrumentation-mixin 'report-function)
             the-mapping-instrumentation-mixin stream extracter arguments)
  (apply #'report-components the-mapping-instrumentation-mixin stream extracter arguments))

(defmethod report-components ((the-mapping-instrumentation-mixin mapping-instrumentation-mixin) stream (extracter t)
                              &rest arguments)
  (with-slots (report-function (default-extracter extracter) map-function components lowest?)
	      the-mapping-instrumentation-mixin 
    (let ((map-values (apply map-function arguments)))
      (dolist (component components)
        (dolist (value map-values)
          (let ((*not-top-level-in-report* t))
            ;; ignore the passed in extracter for now 
	    ;; WE SHOULD BE PASSING THE `ARGUMENTS' TO THE LOWER LEVEL CLIP
            (report-internal (find-instrumentation component) stream
                             (or extracter default-extracter) value)))))))
  
(defmethod collect-internal ((the-mapping-instrumentation-mixin mapping-instrumentation-mixin) (combiner t) &rest arguments)
  ;; record the fact that we have collected this instrumentation
  (update-value :collected the-mapping-instrumentation-mixin arguments nil)
  (dolist (value (apply (slot-value the-mapping-instrumentation-mixin 'map-function)
                        arguments))
    (dolist (component (slot-value the-mapping-instrumentation-mixin 'components))
      ;; We ignore the `combiner' argument - this may be bad.
      ;; WE SHOULD BE PASSING THE `ARGUMENTS' TO THE LOWER LEVEL CLIP
      (collect (find-instrumentation component) value))))

(defmethod display ((the-mapping-instrumentation-mixin mapping-instrumentation-mixin) &rest arguments)
  (dolist (value (apply (slot-value the-mapping-instrumentation-mixin 'map-function)
                        arguments))
    (dolist (component (slot-value the-mapping-instrumentation-mixin 'components))
      (display (find-instrumentation component) value))))

(defmethod print-report-key-internal ((the-mapping-instrumentation-mixin mapping-instrumentation-mixin) stream
			     toplevel prefix
			     &rest arguments)
  (let ((map-values (apply (slot-value the-mapping-instrumentation-mixin 'map-function)
                           arguments)))
    (dolist (component (slot-value the-mapping-instrumentation-mixin 'components))
      (loop for value in map-values
	    with prefix = (format nil (if (eq *output-format* :CLASP) 
					  "~@[~a~]~{~a ~}" 
					  "~@[~a~]~{~a-~}")
				  prefix
				  arguments)
	    do
	    (when (and toplevel (eq *output-format* :CLASP))
	      (write-char #\" stream))
	    (print-report-key-internal (find-instrumentation component) stream nil prefix value)
	    (setf toplevel t)
	    ))))

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; Instrumentation groups AKA Composite-instrumentation.

#| Example

(defclip bunch-of-delays ()
  (:components (overall-delay number-of-delays))
  
  (loop with number-of-delays = 0
        and total-delay-time = 0
        and local-delay-time = 0
        for order in (find-units 'order (make-paths '(scheduler * order)) :ALL) do
        (setf local-delay-time (- (task-network$finish-time (order$network order))
                                  (order$due-date-time order)))
        (unless (zerop local-delay-time)
          (incf number-of-delays)
          (incf total-delay-time local-delay-time))
        finally (return (values total-delay-time number-of-delays))))

(defclip overall-delay (value))

(defclip number-of-delays (value))

;;;----------------------------------------------------------------------------

(defclip delays ()
  (:components (total-delay-time number-of-delays))
  (values 10 20))

(defclip total-delay-time (value))

(defclip number-of-delays (value))

|#

;;;----------------------------------------------------------------------------
;;; composite-instrumentation-mixin

(defmethod collect-internal ((the-instrumentation composite-instrumentation-mixin) (combiner t) &rest arguments)
  (with-slots (name (default-combiner combiner)) the-instrumentation
    (let* ((values-list (multiple-value-list
                         (call-safe (slot-value the-instrumentation 'report-function)
                                    the-instrumentation nil nil arguments))))
      ;; Store all the values.
      (update-value values-list the-instrumentation NIL combiner)
      (if values-list
          (loop for component in (slot-value the-instrumentation 'components)
	        for value in values-list do
	        ;; This calls collect internal
	        (collect-internal (find-instrumentation component)
			          (or combiner default-combiner) value))
        
        (loop for component in (slot-value the-instrumentation 'components) do
	      ;; This calls collect internal
	      (collect-internal (find-instrumentation component)
				(or combiner default-combiner)))))))

(defmethod report-internal ((the-instrumentation composite-instrumentation-mixin) stream extracter &rest arguments)
  (declare (ignore arguments))
  (with-slots (value (default-extracter extracter)) the-instrumentation
    (loop for component in (slot-value the-instrumentation 'components) do
          (report-internal (find-instrumentation component) stream (or extracter default-extracter)))))

(defmethod print-report-key-internal ((the-instrumentation composite-instrumentation-mixin) stream toplevel
			     prefix &rest arguments)
  (declare (ignore arguments))
  (loop for component in (slot-value the-instrumentation 'components)
        do
        (when (and toplevel (eq *output-format* :CLASP))
          (write-char #\" stream))
        ;; The nil for `arguments' is there since `print-report-key-internal' will try to print
        ;; out a value for the argument.
        (print-report-key-internal (find-instrumentation component) stream nil prefix nil)
        (setf toplevel t)))

(defmethod reset-internal ((the-instrumentation composite-instrumentation-mixin) &optional from-child)
  (declare (ignore from-child))
  (call-next-method)
  (dolist (component (slot-value the-instrumentation 'components))
    (reset-internal (find-instrumentation component) t)))

;;;----------------------------------------------------------------------------
;;; child-of-composite-instrumentation-mixin

;; Add child based collect.

(defmethod collect ((the-instrumentation child-of-composite-instrumentation-mixin) &rest arguments)
  (with-slots (parent) the-instrumentation
    (unless (previously-collected-p parent arguments)
      (apply #'collect parent arguments))))

;; Override default method which indexes the stored value based on the arguments.
;; Children of composites do not use the arguments to index the stored values.
(defmethod collect-internal ((the-instrumentation child-of-composite-instrumentation-mixin) (combiner t)
			     &rest arguments)
  (with-slots (name) the-instrumentation
    (let* ((values (multiple-value-list (apply name arguments)))
           (new-value (first values)))
      (update-value new-value the-instrumentation NIL combiner)
      (values-list values))))

;; Override default method which indexes the stored value based on the arguments.
;; Children of composites do not use the arguments to index the stored values.
(defmethod update-number-of-samples ((the-instrumentation child-of-composite-instrumentation-mixin) arguments)
  (declare (ignore arguments))
  (update-number-of-samples-internal the-instrumentation NIL))

;; Add reset code for children that wil propogate up to parent and back down to siblings.

(defmethod reset-internal ((the-instrumentation child-of-composite-instrumentation-mixin) &optional from-parent)
  (call-next-method)
  (unless from-parent
    (reset-internal (parent the-instrumentation) t)))

;;;----------------------------------------------------------------------------

;; Both of these are of dubious benefit.

;; Special case this method to not pass the arguments through? Is this necessary?
(defmethod collect-internal ((the-instrumentation composite-child-of-composite-instrumentation) 
                             (combiner t)
			     &rest arguments)
  (apply #'call-next-method the-instrumentation combiner nil))

;; How is this different from composite-instrumentation-mixin method?
(defmethod print-report-key-internal ((the-instrumentation composite-child-of-composite-instrumentation)
                                      stream toplevel
			              prefix &rest arguments)
  (declare (ignore arguments))
  (loop for component in (slot-value the-instrumentation 'components)
        do
	(when (and toplevel (eq *output-format* :CLASP))
          (write-char #\" stream))
        ;; The nil for `arguments' is there since `print-report-key-internal' will try to print
        ;; out a value for the argument.
        (print-report-key-internal (find-instrumentation component) stream toplevel prefix nil)
        (setf toplevel t)))


;;;----------------------------------------------------------------------------
;;; Functional instrumentation

(defmacro define-around-advice (function name &rest forms)
  #-(or Explorer MCL lispworks lucid allegro)
  (el::nyi)
  #+allegro
  `(let ((excl::*compile-advice* t))
     (excl::advise-1
      ',function :around ',name nil
	'(macrolet ((call-next-advice ()
		     :DO-IT)
		    (advice-arglist ()
		     'excl:arglist))
	  ,@forms)))
  #+Lucid
  `(macrolet ((call-next-advice ()
                '(lcl:apply-advice-continue .args.))
	      (advice-arglist ()
                 '.args.))
     (lcl::defadvice (,function ,name) (&rest .args.)
       ,@forms))
  #+lispworks
  `(macrolet ((call-next-advice ()
                '(apply #'lispworks:call-next-advice .args.))                
	     (advice-arglist ()
                '.args.))
    (lispworks:defadvice (,function ,name :around) (&rest .args.)
       ,@forms))
  #+MCL 
  `(macrolet ((call-next-advice ()
                '(:DO-IT))
	      (advice-arglist ()
                 'ccl:arglist))
       (ccl:advise ,function ,@forms
		   :when :around :name ,name))
  #+Explorer
  `(let ((ticl::compile-encapsulations-flag t))
       (sys::advise-1 ',function :around ',name nil
	  '((macrolet ((call-next-advice ()
			':DO-IT)
		      (advice-arglist ()
			'ticl:arglist))
	     ,@forms)))))

#+TEST
(defun fred (a)
  (values a :RETURN))

#+TEST
(define-around-advice fred sam
  (let ((args (advice-arglist))
	vals)
    (setf vals (multiple-value-list (call-next-advice)))
    (spy args vals)
    (values-list vals)))

(defmethod enable :after ((the-instrumentation functional-instrumentation-mixin))
  (with-slots (trigger-events) the-instrumentation 
    (loop for (function when args-from predicate) in trigger-events do
          (initialize-trigger-event the-instrumentation function when args-from predicate))))

(defmethod initialize-trigger-event  ((the-instrumentation functional-instrumentation-mixin) function
				      when args-from predicate)
  (remove-around-advice function (name the-instrumentation))
  (ecase when
    (:AFTER
     (eval
      `(define-around-advice ,function ,(name the-instrumentation)
	 (if (or (null ,predicate) (funcall ,predicate))
	     (let ((return-values (multiple-value-list (call-next-advice))))
	       (ecase ,args-from
		      (:VALUES          (apply #'collect-internal ,the-instrumentation nil return-values))
		      ;; Need to find out how to get the args in Allegro.
		      (:ARGS-AND-VALUES (apply #'collect-internal ,the-instrumentation nil
					       (append (advice-arglist) return-values)))
		      (:NONE            (collect-internal ,the-instrumentation nil)))
	       (values-list return-values))
	   (call-next-advice)))))
    (:BEFORE
     (eval
      `(define-around-advice ,function ,(name the-instrumentation)
	 (if (or (null ,predicate) (funcall ,predicate))
	     (progn
	       (ecase ,args-from
		      (:ARGS (apply #'collect-internal ,the-instrumentation nil (advice-arglist)))
		      (:NONE (collect-internal ,the-instrumentation nil)))
	       (call-next-advice))
	   (call-next-advice)))))))

(defun remove-around-advice (function name)
  #-(or Explorer MCL lispworks Lucid allegro)
  (el::nyi)
  #+allegro
  (when (member function (excl::advised-functions))
    (excl::unadvise-1 function :around name))
  #+Lucid
  (lcl::remove-advice function name)
  #+lispworks
  (eval
   `(lispworks:delete-advice ,function ,name))
  #+MCL
  (eval 
   `(ccl:unadvise ,function :when :around :name ,name))
  #+Explorer
  (eval 
   `(ticl::unadvise ,function :around ,name)))

(defmethod disable :after ((the-instrumentation functional-instrumentation-mixin))
  (with-slots (trigger-events) the-instrumentation 
    (loop for (function when args-from predicate) in trigger-events do
          (remove-around-advice function (name the-instrumentation)))))

;; Override default method which indexes the stored value based on the arguments.
;; Functional-Instrumentation-Mixin does not use the arguments to index the stored values.
(defmethod update-number-of-samples ((the-instrumentation functional-instrumentation-mixin) arguments)
  (declare (ignore arguments))
  (update-number-of-samples-internal the-instrumentation NIL))

;;;----------------------------------------------------------------------------

;; Override default method which indexes the stored value based on the arguments.
;; composite-time-series-instrumentation does not use the arguments to index the stored values.
(defmethod update-number-of-samples ((the-instrumentation composite-time-series-instrumentation) arguments)
  (declare (ignore arguments))
  (update-number-of-samples-internal the-instrumentation NIL))

;;;----------------------------------------------------------------------------
;;; Time series mixin

(defmethod collect-if-necessary ((the-instrumentation time-series-instrumentation-mixin) &rest args)
  ;; time series never do this
  (declare (ignore args))
  )
  

(defmethod report-internal ((the-instrumentation time-series-instrumentation-mixin)
                            stream extracter
                            &rest arguments)
  (declare (ignore arguments))
  (assert (null extracter)  ())
  (with-slots (number-of-samples) the-instrumentation
    (if (time-series-p the-instrumentation)
      (when number-of-samples ;; time series sometimes have no values
        (loop for cnt from (1- number-of-samples) downto 0 do
              (with-output-as-clasp-row (stream)
		;; It looks like we are assumming that all time-series clips have 
		;; components. This is probably a bad assumption.
                (report-components the-instrumentation stream cnt))))
      (call-next-method))))

(defmethod report-internal :around ((the-instrumentation time-series-instrumentation-mixin)
                            stream extracter
                            &rest arguments)
  (if (and *current-experiment* (slot-value the-instrumentation 'time-series?))
    (with-open-experiment-file
      (*current-experiment*
        the-instrumentation
        stream (or (output-pathname *current-experiment* the-instrumentation)
                   stream))
      (apply #'call-next-method the-instrumentation stream extracter arguments))
    (call-next-method)))
    
;;;----------------------------------------------------------------------------

(defun push-value-combiner (old-value  new-value)
  (push new-value old-value))

(defun average-of-values-extracter (value number-of-samples)
  (assert (plusp number-of-samples) () "no samples have been taken")
  (if (= number-of-samples 1)
      (first value)
      (/ (reduce #'+ value) number-of-samples)))

(defun last-value-combiner (old-value  new-value)
  (setf old-value new-value))

(defun last-value-extracter (value number-of-samples)
  (declare (ignore number-of-samples))
  (values value))

;;; ***************************************************************************
;;; EOF
