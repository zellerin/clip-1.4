;;;; -*- Mode:Common-Lisp; Package:CLIP; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/mac-files/clip/intrinsic-mixins.lisp *-*
;;;; *-* Last-edit: Tuesday, January 19, 1993  16:29:07; Edited-By: Westy *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                   Underlying Basic Class Mixins                        *
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

(in-package #+CLTL2 CLIP #-CLTL2 'CLIP)

;;; --*--
;;; ***************************************************************************

(defgeneric name (object))

(defmethod name ((the-object t))
  the-object)

;;;----------------------------------------------------------------------------
;;; NAMED-OBJECTS have a NAME and a DESCRIPTION slot. They store themselves on the
;;; :OBJECT property of their name so that they can be accessed by the NAME.
;;; Note that the names of named objects define a name-space. 
;;; They also store themselves under the class-name property of their name symbol.

(defclass named-object-mixin ()
     ((name :initform nil :type symbol :initarg :name :accessor name)
      (description :initform nil :initarg :description :accessor description))
  (:metaclass named-class)
  (:documentation "Allows each instance to have a name.  One is generated for it if not provided.
                   The name is always a symbol.
                   NAMED-OBJECTS store themselves, i.e., their instances, under
                   the (class-name (class-of the-object) property of their name symbol
                   as well as under the :object property of their name symbol."))

(defmethod displayed-slots append ((object named-object-mixin))
  `(,@(when (slot-value object 'description)
        `((:name "Description"   :value ,(slot-value object 'description))))))

(defmethod print-object ((object named-object-mixin) stream)
  (if (and *print-escape* (not *override-print-escape*))
    (call-next-method)
    (let ((name (name object)))
      (format stream "~:[(unnamed)~;~a~]" name name))))

(defmethod initialize-instance :after ((the-object named-object-mixin) &rest ignore)
  (declare (ignore ignore))
  (let ((name (name the-object)))
    (when (or (not name) (not (symbolp (name the-object))))
      (setf (name the-object) (or name (make-name the-object)))))

  
  (setf (get (name the-object) :object) the-object)

  ;; Put a name property on the object for each superclass whose Meta class is
  ;; named-object.
  (loop with named-class-instance = (find-class 'named-class)
	with object-name = (name the-object)
	for class in (class-precedence-list (class-of the-object)) do
	(when (eq (class-of class) named-class-instance)
	  (setf (get object-name (class-name class)) the-object))))

;;;----------------------------------------------------------------------------

(defmethod make-name ((the-object named-object-mixin))
  "Make a name for yourthe-object if necessary."
  (let ((class-name (class-name (class-of the-object))))
    (intern
      (format nil "~:@(~a-~a~)" class-name
	      (setf (get class-name 'name-index)
		    (1+ (or (get class-name 'name-index) 0))))
      (symbol-package class-name))))

(defmethod (setf name) (new-name (the-object named-object-mixin))
  "This version insures name is a symbol.  You may want something else."
  (setf (slot-value the-object 'name)
	(cond ((not new-name) (make-name the-object))
	      ((symbolp new-name) new-name)
	      ((stringp new-name) (intern new-name))
	      (t (intern (format nil "~a" new-name))))))

(defmethod name-string ((the-object t))
  "Returns name as a string."
  (let ((name (name the-object)))
    (cond ((stringp name) name)
	  ((symbolp name) (string-capitalize name))
	  (t (format nil "~a" name)))))

(defmethod kill :after ((the-object named-object-mixin) &aux (the-name (name the-object)))
  (when the-name
    (remprop the-name :object)
    ;; make sure that this removes all the heirarchical name properties - maybe
    (remprop the-name (class-name (class-of the-object)))))

;; Set the index back to zero if we are fogetting all the instances.
(defmethod forget-instances :after ((class named-class))
  (setf (get (class-name class) 'name-index) 0))

;;;----------------------------------------------------------------------------

;;; REMEMBER INSTANCES CLASS......

;;; Previously, we remembered the instances in a class slot and accessed the set by
;;; looking at the class prototype.  However, there are too many problems with
;;; accessing the prototype, especially for classes that haven't had any instances
;;; created.  So, a much simpler approach is to use a hash table.

;;; Changed this so it does do things hierarchically.  Use find-direct-instances if
;;; you need the old behavior. [dlc]

;;; Note that to get rid of an instance one must KILL IT, so that the connection,
;;; if any, between the instance and its name is broken.  If you do not
;;; kill instances properly they will come back to haunt you.

(defvar *remember-instances-table* (make-hash-table)
  "table of class names (the keys) and List of Instances of that Class.")

(defclass remember-instances ()
  ()
  (:metaclass basic-class))

(defmethod initialize-instance :after ((x remember-instances) &rest ignore)
  (declare (ignore ignore))
  (push x (gethash (class-name (class-of x))
		   *remember-instances-table*)))

(defmethod find-direct-instances ((class-name symbol))
  (values (gethash class-name *remember-instances-table*)))

;;; This returns me and all other instances of my class.
(defmethod find-direct-instances ((the-object remember-instances))
  (find-instances (class-name (class-of the-object))))

;;;This does what we normally want...return all instances of this specific class and
;;;any classes that inherit from this.

(defmethod find-instances ((class standard-class))
  (let ((subclasses-examined-so-far nil))
    (labels ((grab-instances (class)
	       (let ((subclasses (class-direct-subclasses
                                  class)))
		 (unless (member class subclasses-examined-so-far)
		   (push class subclasses-examined-so-far)
		   (cond ((null subclasses)
			  (find-direct-instances (class-name class)))
			 (t
			  (append
			    (find-direct-instances (class-name class))
			    (loop for subclass in subclasses
				  append (grab-instances subclass))))
			 )))))
      (grab-instances class))))

(defmethod find-instances ((class-name symbol))
  (find-instances (find-class class-name)))

(defmethod find-instances ((the-object remember-instances))
  (find-instances (class-of the-object)))

(defmethod kill :after ((the-object remember-instances))
  (forget-instance the-object))

(defvar *killing-everything* nil)
(defvar *killing-class* nil)

(defmethod forget-instance ((the-object remember-instances))
  (unless (or *killing-everything* 
              (class-built-on-class (class-of the-object) *killing-class*)
              ;(eq *killing-class* (class-of the-object))
            )
    (setf (gethash (class-name (class-of the-object)) *remember-instances-table*)
          (remove the-object (gethash (class-name (class-of the-object)) *remember-instances-table*)
                  ;; There was a bug (that you will not believe until you see it)
                  ;; where (eq <object-1> <object-1>) was returning nil. I didn't
                  ;; track it down, but this gets around it.
                  :test #'(lambda (a b) 
                            (eq (name a) (name b)))))))

;;; Note that we remove them one-by-one.

(defmethod forget-instances ((class-name symbol))
  (forget-instances (find-class class-name)))

(defmethod forget-instances ((class standard-class))
  (let ((*killing-class* class))
    (map nil #'kill (find-instances class))
    (remhash (class-name class) *remember-instances-table*)))

;;;----------------------------------------------------------------------------
;; Make this use a hash table if speed is an issue.

(defmethod find-instance-by-name ((object-spec symbol) (class standard-class) &optional no-error-p)
  (cond ((find object-spec (find-instances class) :key #'name :test #'eq))
        (no-error-p nil)
        (t
         (error "No object named ~s of class ~s has been defined."
                object-spec class))))

(defmethod find-instance-by-name (object-spec (class symbol) &optional no-error-p)
  (find-instance-by-name object-spec (find-class class) no-error-p))

(defmethod find-instance-by-name ((object-spec string) class &optional no-error-p)
  (find-instance-by-name (intern object-spec) class no-error-p))

(defmethod find-instance-by-name ((object-spec standard-object) class &optional no-error-p)
  (find-instance-by-name (name object-spec) class no-error-p))

;;;----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------
;;; OBJECT-WITH-PROPERTIES

(defclass object-with-properties ()
  ((property-list :initarg :property-list :initform nil))
  (:metaclass basic-class))

(defmethod initialize-instance :after ((object object-with-properties) &key property-list)
  ;; convert the supplied property list to an assoc list (for who knows what reason)
  (when property-list
    (setf (slot-value object 'property-list) nil)
    (el::doplist (key value property-list)
      (setf (get-value object key) value))))

(defmethod get-value ((the-object object-with-properties) key)
  (cdr (assoc key (slot-value the-object 'property-list))))

(defmethod (setf get-value) (val (the-object object-with-properties) key)
  (with-slots (property-list) the-object
    (let ((cell (assoc key property-list)))
      (if cell (rplacd cell val)
	  (push (cons key val) property-list))
      val)))

;;; ----------------------------------------------------------------------------

(defclass browser-mixin ()
  ()
  (:metaclass basic-class))

(defclass basic-object (browser-mixin)
  ()
  (:metaclass basic-class)
  (:documentation "All our objects are built on this."))

;;; Make class prototypes print without an error. Most of my `print-object'
;;; methods assume that an object has been created using the standard
;;; initialization methods and will have the appropriate slots filled in.

#+Explorer
(defmethod class-prototype-p ((object basic-object))
  (not (some #'(lambda (slot-object)
                 (and (eq (slot-definition-allocation slot-object) :INSTANCE)
                      (slot-boundp object (slot-definition-name slot-object))))
         (class-slots (class-of object)))))

#+Explorer
(defmethod print-object :around ((object basic-object) stream)
;; This slot-bound check is so that class prototypes print out without error.
;; All their slots are unbound (at least on the Explorer).
  (if (class-prototype-p object)
    (format stream "~a Prototype" (class-name (class-of object)))
    (call-next-method)))

;; Generate a printed representation that is readable if
;; *print-escape* is non-nil and *override-print-escape* is nil.
; Need to check out the *print-readably* (sp) variable.
(defmethod print-object :around ((object named-object-mixin) stream)
  (if (and *print-escape* (not *override-print-escape*))
    (format stream "#.(~a '~a)" (class-name (class-of object)) (name object))
    (call-next-method)))

;;;----------------------------------------------------------------------------
;;; What we need to know is how to kill efficiently.

(defgeneric kill (object))

(defmethod kill ((the-object t))
  (values nil))

;; Make sure we do not waste time killing things more than once.
(defvar *killed-objects-table* (make-hash-table :test #'eq :size 20000))

(defmethod kill :around ((the-object t))
  (when (or (not *killing-everything*)
	    (not (gethash the-object *killed-objects-table*)))
    (call-next-method)
    (setf (gethash the-object *killed-objects-table*) the-object)))

(defmethod reset :around ((the-object t))
  (unless (gethash the-object *killed-objects-table*)
    (call-next-method)))

(defun kill-everything (&key (verbose *verbose*) (report-stream *standard-output*))
  ;; When the metaobject stuff works we want to make a metaclass for all these classes
  ;; that puts them on a list of classes whose instances need to be reset.
  (let ((*killing-everything* t))
    (map-over-classes #'(lambda (class)
			  (report-if verbose report-stream "~%Killing all instances of ~a" class)
			  (report-if (not verbose) report-stream "#")
			  (forget-instances class))
		      *all-class-names*
		      :key #'find-class
		      :predicate #'(lambda (class)
				     (and (not (eq (class-name class) 'remember-instances))
					  (class-built-on-class 
					    class 
					    (find-class 'remember-instances))
					  (not
					    (class-built-on-class 
					      class 
					      (find-class 'persistent-instances))))))
    (clrhash *killed-objects-table*))
  
  (if (and verbose report-stream) (terpri report-stream)))

;;;----------------------------------------------------------------------------

;; At some point we should add a metaclass that allows a :RESET slot option to
;; indicate which slots should be reset back to their initial value when the
;; reset method is called. That would be way cool.
(defmethod reset ((object basic-object)))

;; If a class includes this its direct instances will not be killed
;; in `kill-everything'.
(defclass persistent-instances ()
  ()
  (:metaclass basic-class))

;;;----------------------------------------------------------------------------

(defclass named-object (named-object-mixin basic-object)
  ()
  (:metaclass named-class))

(defclass unnamed-object (basic-object)
  ()
  (:metaclass basic-class))

;;; ***************************************************************************
;;; EOF

