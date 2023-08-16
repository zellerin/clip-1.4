;;;; -*- Mode:Common-Lisp; Fonts:(MEDFNT); Base:10; Patch-file:T -*-
;;;; *-* File: Titanic: /usr/users/eksl/mac-files/clasp/clasp/development/patches.lisp *-*
;;;; *-* Last-edit: Wednesday, March 24, 1993  14:03:50; Edited-By: Westy *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                             Patches                                    *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: David L. Westbrook and Adam Carlson
;;;             Experimental Knowledge Systems Laboratory
;;;             Paul R. Cohen, Principal Investigator
;;;             David L. Westbrook, Systems Manager
;;;             David M. Hart, Laboratory Manager
;;;             Department of Computer Science
;;;             University of Massachusetts
;;;             Amherst, Massachusetts 01003.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  10-02-92 File Created.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package
 #+(and lucid ansi-packages)
    :common-lisp
 #+(and lucid (not ansi-packages))
    :lisp
 #+allegro :excl)

#+allegro
(excl::without-package-locks
 (shadowing-import '(reduce row-major-aref)
		   'common-lisp))

;;; --*--
;;; ***************************************************************************
;;;
;;; Patched `reduce' to incorporate X3J13 extension which adds :KEY
;;; keyword argument. - WESTY
;;;

#+(or (and lcl4.0 (not lcl4.1)) allegro)
(eval-when #+CLTL2 (:compile-toplevel :execute) #-CLTL2 (compile eval)
  (defmacro with-reduce-bindings (&rest body)
    `(let* ((start (if start (max 0 start) 0))
	    (len (length sequence))
	    (end (if end (min end len) len))
	    (actlen (max (- end start) 0)))
       . ,body))
  )

#+(or (and lcl4.0 (not lcl4.1)) allegro)
(defun reduce-vector (fct sequence &optional initial-value initial-value-p start end from-end key)
  (with-reduce-bindings
    (if key
      (if initial-value-p
        (cond ((zerop actlen) (values initial-value))
              ((= actlen 1)
               (values (if from-end
                         (funcall fct (funcall key (aref sequence (1- end))) initial-value)
                         (funcall fct initial-value (funcall key (aref sequence start))))))
              (from-end
               (do ((accum (funcall fct (funcall key (aref sequence (1- end))) initial-value)
                           (funcall fct (funcall key (aref sequence index)) accum))
                    (index (- end 2) (1- index)))
                   ((< index start) (values accum))))
              (t
               (do ((accum (funcall fct initial-value (funcall key (aref sequence start)))
                           (funcall fct accum (funcall key (aref sequence index))))
                    (index (1+ start) (1+ index)))
                   ((>= index end) (values accum)))))
        (cond ((zerop actlen) (values (funcall fct)))
              ((= actlen 1) (values (funcall key (aref sequence start))))
              (from-end
               (do ((accum (funcall fct (funcall key (aref sequence (- end 2))) (funcall key (aref sequence (1- end))))
                           (funcall fct (funcall key (aref sequence index)) accum))
                    (index (- end 3) (1- index)))
                   ((< index start) (values accum))))
              (t
               (do ((accum (funcall fct (funcall key (aref sequence start)) (funcall key (aref sequence (1+ start))))
                           (funcall fct accum (funcall key (aref sequence index))))
                    (index (+ start 2) (1+ index)))
                   ((>= index end) (values accum)))
               )))
      
      (if initial-value-p
       (cond ((zerop actlen) (values initial-value))
	     ((= actlen 1)
	      (values (if from-end
			  (funcall fct (aref sequence (1- end)) initial-value)
			  (funcall fct initial-value (aref sequence start)))))
	     (from-end
	      (do ((accum (funcall fct (aref sequence (1- end)) initial-value) 
			  (funcall fct (aref sequence index) accum))
		   (index (- end 2) (1- index)))
		  ((< index start) (values accum))))
	     (t
	      (do ((accum (funcall fct initial-value (aref sequence start))
			  (funcall fct accum (aref sequence index)))
		   (index (1+ start) (1+ index)))
		  ((>= index end) (values accum)))))
       (cond ((zerop actlen) (values (funcall fct)))
	     ((= actlen 1) (values (aref sequence start)))
	     (from-end
	      (do ((accum (funcall fct (aref sequence (- end 2)) (aref sequence (1- end))) 
			  (funcall fct (aref sequence index) accum))
		   (index (- end 3) (1- index)))
		  ((< index start) (values accum))))
	     (t
	      (do ((accum (funcall fct (aref sequence start) (aref sequence (1+ start)))
			  (funcall fct accum (aref sequence index)))
		   (index (+ start 2) (1+ index)))
		  ((>= index end) (values accum)))))))))

#+(or (and lcl4.0 (not lcl4.1)) allegro)
(defun reduce-list (fct sequence &optional initial-value initial-value-p start end from-end key)
  (with-reduce-bindings
    (if key
      (if initial-value-p
        (cond ((zerop actlen) (values initial-value))
              ((= actlen 1)
               (values (if from-end
                         (funcall fct (funcall key (nth (1- end) sequence)) initial-value)
                         (funcall fct initial-value (funcall key (nth start sequence))))))
              (from-end
               (let ((vector (coerce sequence 'vector)))  ;; convert to vector - shorter and faster than a list
                 (do ((accum (funcall fct (funcall key (aref vector (1- end))) initial-value) 
                             (funcall fct (funcall key (aref vector index)) accum))
                      (index (- end 2) (1- index)))
                     ((< index start) (values accum)))))
              (t
               (let ((list (nthcdr start sequence)))
                 (do ((accum (funcall fct initial-value (funcall key (car list)))
                             (funcall fct accum (funcall key (car rest))))
                      (rest (cdr list) (cdr rest))
                      (index 1 (1+ index)))
                     ((>= index actlen) (values accum))))))
        (cond ((zerop actlen) (values (funcall fct)))
              ((= actlen 1) (values (funcall key (nth start sequence))))
              (from-end
               (let ((vector (coerce sequence 'vector)))
                 (do ((accum (funcall fct (funcall key (aref vector (- end 2))) (funcall key (aref vector (1- end))))
                             (funcall fct (funcall key (aref vector index)) accum))
                      (index (- end 3) (1- index)))
                     ((< index start) (values accum)))))
              (t
               (let ((list (nthcdr start sequence)))
                 (do ((accum (funcall fct (funcall key (car list)) (funcall key (cadr list)))
                             (funcall fct accum (funcall key (car rest))))
                      (rest (cddr list) (cdr rest))
                      (index  2 (1+ index)))
                     ((>= index actlen) (values accum)))))))
      (if initial-value-p
        (cond ((zerop actlen) (values initial-value))
              ((= actlen 1)
               (values (if from-end
                         (funcall fct (nth (1- end) sequence) initial-value)
                         (funcall fct initial-value (nth start sequence)))))
              (from-end
               (let ((vector (coerce sequence 'vector)))  ;; convert to vector - shorter and faster than a list
                 (do ((accum (funcall fct (aref vector (1- end)) initial-value) 
                             (funcall fct (aref vector index) accum))
                      (index (- end 2) (1- index)))
                     ((< index start) (values accum)))))
              (t
               (let ((list (nthcdr start sequence)))
                 (do ((accum (funcall fct initial-value (car list))
                             (funcall fct accum (car rest)))
                      (rest (cdr list) (cdr rest))
                      (index 1 (1+ index)))
                     ((>= index actlen) (values accum))))))
        (cond ((zerop actlen) (values (funcall fct)))
              ((= actlen 1) (values (nth start sequence)))
              (from-end
               (let ((vector (coerce sequence 'vector)))
                 (do ((accum (funcall fct (aref vector (- end 2)) (aref vector (1- end))) 
                             (funcall fct (aref vector index) accum))
                      (index (- end 3) (1- index)))
                     ((< index start) (values accum)))))
              (t
               (let ((list (nthcdr start sequence)))
                 (do ((accum (funcall fct (car list) (cadr list))
                             (funcall fct accum (car rest)))
                      (rest (cddr list) (cdr rest))
                      (index  2 (1+ index)))
                     ((>= index actlen) (values accum))))))))))

#+(or (and lcl4.0 (not lcl4.1)) allegro)
(defun reduce* (fct sequence &optional initial-value initial-value-p start end from-end key)
  (if (arrayp sequence)
      (reduce-vector fct sequence initial-value initial-value-p start end from-end key)
      (reduce-list fct sequence initial-value initial-value-p start end from-end key)))

#+(or (and lcl4.0 (not lcl4.1)) allegro)
(defun reduce (function sequence &key key (initial-value nil initial-value-p) start end from-end)
  "Combine the elements of SEQUENCE using FUNCTION, a function of two args.
FUNCTION is applied to the first two elements; then to that result and the third element;
 then to that result and the fourth element; and so on.
KEY, if non-NIL, is a function to be applied to each element to get a key.
 If KEY is NIL, the element itself is used.
START and END restrict the action to a part of SEQUENCE,
 as if the rest of SEQUENCE were not there.  They default to 0 and NIL
 (NIL for END means to the end of SEQUENCE).
If FROM-END is non-NIL, FUNCTION is applied to the last two elements;
 then to the previous element and that result; then to the previous
 element and that result; and so on.
If INITIAL-VALUE is specified, it acts like an extra element of SEQUENCE
 at the end (if FROM-END is non-NIL) or the beginning, in addition to
 the actual elements of the specified part of SEQUENCE.  Then there is
 effectively one more element to be processed.  The INITIAL-VALUE is
 used in the first call to FUNCTION.
If there is only one element to be processed,
 that element is returned and FUNCTION is not called.
If there are no elements (SEQUENCE is of length zero and no INITIAL-VALUE),
 FUNCTION is called with no arguments and its value is returned."
  (reduce* function sequence initial-value initial-value-p start end from-end key))

;;;----------------------------------------------------------------------------
;;;
;;; Added 'row-major-aref function - CARLSON
;;;

#+allegro
(defvar *row-major-aref-association-list* nil)

#+allegro
(defun row-major-aref (array index)
  (let ((r-m-array (or (cdr (assoc array *row-major-aref-association-list*))
		       (cdar (setf *row-major-aref-association-list*
			       (acons array
				      (make-array (array-total-size array)
						  :displaced-to array
						  :element-type
						  (array-element-type
						   array))
				      *row-major-aref-association-list*))))))
    (aref r-m-array index)))

;;;----------------------------------------------------------------------------

;;; ----------------------
;;; Proposal PROCLAIM-ETC-IN-COMPILE-FILE:NEW-MACRO
;;; ----------------------

;;; used to be #+(or lcl4.0 lcl4.1)

#+ignore
(defmacro DECLAIM (&rest decl-specs)
  "This macro PROCLAIMs the given DECL-SPECS, which are not evaluated.
If a call to this macro appears at top-level in a file being processed by the 
file compiler, the proclamations are also made at compile-time."
  `(eval-when (compile load eval)
     . ,(loop for x in decl-specs
	      collect `(proclaim (quote ,x)))))

#+ignore
(proclaim '(inline complement))

#+ignore
(defun complement (function)
  "Returns a function whose value is the same as the NOT of the given FUNCTION
applied to the same arguments."
  #'(lambda (&rest arguments)
      (not (apply function arguments))))

#+ignore
(export '(complement declaim))


;;; ***************************************************************************


