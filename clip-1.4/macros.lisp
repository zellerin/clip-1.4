;;;; -*- Mode:Common-Lisp; Package:CLIP; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/systems/clip/development/macros.lisp *-*
;;;; *-* Last-edit: Friday, September 24, 1993  10:15:55; Edited-By: Westy *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                Transportation Simulation General Macros                *
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

#-Explorer
(defmacro spy (&rest forms &aux (stream '*trace-output*) no-newline)
  "A debugging tool: wrapping this around a form causes both the form and its
values to be printed on *trace-output*.  Like `progn,' this returns the value\(s\)
of the last form, so that it can be safely wrapped about functional code as
well.  In other words, \(spy \(foo\)\) returns the same values as \(foo\).
The first form can optionally be a list of options, eg., keyword value pairs\).
Options supported:
:STREAM <stream>      - directs output to `stream'
:NO-NEWLINE <boolean> - if non-nil suppresses insertion of newlines"
  (when (and (consp (first forms)) (keywordp (first (first forms))))
    (let ((options (pop forms)))
      (setf stream (getf options :stream stream)
            no-newline (getf options :no-newline))))
    
  (flet ((spy-1 (form)
	   `(let ((form   ',form)
		  (values  (multiple-value-list ,form)))
              ,@(unless no-newline
                  `((fresh-line ,stream)))
	      (format ,stream "~s =>~{ ~s~}" form values)
              ,@(if no-newline
                  `((write-char #\space ,stream))
                  `((terpri ,stream)))
	      (values-list values))))
    `(progn ,@(mapcar #'spy-1 forms))))

;;;----------------------------------------------------------------------------

(defmacro undefclass (name supers slots &rest options)
  (declare (ignore supers slots options))
  `(setf (find-class ',name) nil))

;;;----------------------------------------------------------------------------

(defmacro map-over-classes (function class-list &key key predicate partial-order &aux (class (gensym)))
  `(dolist (,class ,@(if partial-order `((sort-using-list-order (copy-list ,class-list)
                                                                ,partial-order))
                         `(,class-list)))
     ,@(if key `((setf ,class (funcall ,key ,class))))
     (when (funcall ,predicate ,class)
       (funcall ,function ,class))))

;;; ----------------------------------------------------------------------------

(defmacro lookup-equal (table key)
  `(if ,key
    (rest (assoc ,key ,table :test #'equal))
    ,table))

(defmacro setf-lookup-equal (table key value)
  `(if ,key
    (el::pushnew-acons ,table ,key ,value :test #'equal)
    (setf ,table ,value)))

;;; ----------------------------------------------------------------------------

(defmacro incf-safe (place &optional delta)
  `(if (numberp ,place) 
     (incf ,place ,@(if delta `(,delta)))
     ;; Non-numbers are assumed to be zero.
     (setf ,place 1)))

;;; ----------------------------------------------------------------------------

(defmacro with-open-experiment-file ((experiment instrumentations stream filename 
                                                 &rest options
                                                 &key
                                                 (direction :output)
                                                 (if-exists :append)
                                                 (if-does-not-exist :create)
                                                 &allow-other-keys) &body body)
  `(el::with-conditional-open-file (,stream ,filename 
                                            :direction ,direction
                                            :if-exists ,if-exists 
                                            :if-does-not-exist ,if-does-not-exist
                                            ,@options)
     (possibly-write-experiment-headers ,experiment ,instrumentations ,stream ,filename)
     . ,body))

(defvar *clasp-row-started-table* (make-hash-table))

(defmacro with-output-as-clasp-row ((stream) &body body)
  `(progn
     (assert (not (gethash stream *clasp-row-started-table*)) ()
             "attempt to recursively write clasp formatted row to ~a;~
              you might want to specify a separate output-file for your clip" stream)
     (unwind-protect 
	 (progn
	   (setf (gethash stream *clasp-row-started-table*) t)
	   (fresh-line ,stream)
	   (when (eq *output-format* :CLASP)
	     (write-char #\( ,stream))
	   ,@body
	   (when (eq *output-format* :CLASP)
	     (write-char #\) ,stream))
	   (terpri ,stream))
       (remhash stream *clasp-row-started-table*))))

;;; ***************************************************************************
;;; EOF




