;;;; -*- Mode:Common-Lisp; Package:CLIP; Base:10; Fonts:(MEDFNT) -*-
;;;; *-* File: Titanic: /usr/users/eksl/mac-files/clip/utilities.lisp *-*
;;;; *-* Last-edit: Tuesday, January 19, 1993  17:03:32; Edited-By: Westy *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                                Utilities                               *
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

(defun sort-using-list-order (x list &key (test #'eq) key (list-key #'identity))
  (flet ((item-lessp-function (list  test item-key list-key)
           (if item-key
             #'(lambda (item-1 item-2)
                 (setf item-2 (funcall item-key item-2)
                       item-1 (funcall item-key item-1))
                 (let ((item-2-present (member item-2 list :test test :key list-key))
                       (item-1-present (member item-1 list :test test :key list-key)))
                   (if (and item-1-present item-2-present)
                     (not (member item-1 item-2-present :test test :key list-key))
                     ;; otherwise
                     (if item-1-present t nil))))
             #'(lambda (item-1 item-2)
                 (let ((item-2-present (member item-2 list :test test :key list-key))
                       (item-1-present (member item-1 list :test test :key list-key)))
                   (if (and item-1-present item-2-present)
                     (not (member item-1 item-2-present :test test :key list-key))
                     ;; otherwise
                     (if item-1-present t nil)))))))
    (let ((lessp-function (item-lessp-function list test key list-key)))
      (stable-sort x lessp-function))))

;;;----------------------------------------------------------------------------

(defun class-built-on-class (class potential-superclass)
  (member potential-superclass
          #-MCL
	  (if #+Explorer (ticlos::class-composed-p class)
              #-Explorer (class-finalized-p class)
	      (class-precedence-list class)
	      (class-direct-superclasses class))
          #+MCL
          (class-precedence-list class)
	  :test #'eq))

;;; ----------------------------------------------------------------------------

(defun format-if (stream format-string &rest format-args)
  (when stream (apply #'format stream format-string format-args)))

(defun report-if (flag stream format-string &rest format-args)
  (when (and flag stream)
    (apply #'format stream format-string format-args)))

;;;----------------------------------------------------------------------------

(defun fill-all (array elt &key (start 0) (end (array-total-size array)))
  "Fills an array ignoring the fill pointer."
  (let ((fp (fill-pointer array)))
    (setf (fill-pointer array) end)
    (fill array elt :start start :end end)
    (setf (fill-pointer array) fp)))

;;; ----------------------------------------------------------------------------

(defun DATE-AND-TIME (&OPTIONAL (time (get-universal-time)))
  
  "DATE-AND-TIME [universal-time]

Returns formatted date/time string."
  
  (multiple-value-bind (second minute hour date month year day)
                       (decode-universal-time time)
    (format nil 
            "~a, ~a ~a, ~a  ~2,'0d:~2,'0d:~2,'0d"
            (nth day
                 '("Monday" "Tuesday" "Wednesday" "Thursday"
                   "Friday" "Saturday" "Sunday"))
            (nth (1- month)
                 '("January" "February" "March" "April" "May"
                   "June" "July" "August" "September"
                   "October" "November" "December"))
            date
            year
            hour
            minute
            second)))

;;;----------------------------------------------------------------------------

(DEFUN xor (&rest args)
  "Takes any number of arguments and returns T if an odd number of its arguments are non-NIL, 
otherwise returns NIL."
  (LET ((flag nil))
    (DOLIST (form args flag)
      (WHEN form
	(SETF flag (not flag))))))

(defun extract-arguments-from-lambda-list (lambda-list)
  (loop for arg in lambda-list
        when (not (member arg lambda-list-keywords))
        collect (if (listp arg) (first arg) arg)))

#+UNUSED
(defun extract-declarations-and-body (body)
  (if (eq (first (first body)) 'declare)
    (values (list (first body)) (rest body))
    (values nil body)))
                         
;;; ***************************************************************************
;;; EOF


