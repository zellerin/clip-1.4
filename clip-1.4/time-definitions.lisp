;;;; -*- Mode:Common-Lisp; Package:CLIP; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/mac-files/clip/development/time-definitions.lisp *-*
;;;; *-* Last-edit: Friday, July 23, 1993  15:00:10; Edited-By: WESTY *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                            Time Definitions                            *
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

(defparameter *base-time-string* "1/1/93 0:00")

(defparameter *seconds-per-time-unit* 3600)

(defparameter *time-units-per-second* (/ *seconds-per-time-unit*)
  "Number of internal time units per second.")

(defmethod (setf seconds-per-time-unit) (value)
  (setf *seconds-per-time-unit* value)
  (setf *time-units-per-second* (/ *seconds-per-time-unit*)))

(defmethod (setf time-units-per-second) (value)
  (setf *time-units-per-second* value)
  (setf *seconds-per-time-unit* (/ *time-units-per-second*)))

;;;----------------------------------------------------------------------------

(defun seconds->internal-time (seconds)
  (values (round (* seconds *time-units-per-second*))))

(defun internal-time->seconds (internal-time)
  (values (round internal-time *time-units-per-second*)))

(defun internal-time->hours (internal-time)
  (values (/ internal-time *time-units-per-second* 3600.0)))

(defun internal-time->integral-hours (internal-time)
  (values (floor internal-time (* *time-units-per-second* 3600))))

(defun hours->internal-time (hours)
  (values (round (* hours 60 60 *time-units-per-second*))))

;;;----------------------------------------------------------------------------

(defvar *base-time* (time-parser:parse-universal-time *base-time-string*)
  "The time at which we begin simulating. In universal-time units.")

(defvar *elapsed-time* 0
  "The time that has elapsed from *base-time*. In internal-time units.")

(defmethod (setf current-time) (time)
  "Sets the current-time to `time' which is specified in internal-time units."
  (assert (typep time 'fixnum) () "Your time is up! You have reached the maximum simulatable time.")
  (setf *elapsed-time* time)
  (values))

(defun base-time ()
  "Base time (in universal-time units)."
  (values *base-time*))

(defun current-time ()
  "Elapsed time (in internal time units!)."
  (values *elapsed-time*))

;;; Time is measured in seconds since base time

(defun real-time ()
  "Current time (in universal-time units)."
  (values (+ *base-time*
             (internal-time->seconds *elapsed-time*))))

(defun parse-time-specifier (time-spec
                             &key interval-p
                             (negative-error-p (not interval-p))
                             zero-error-p
                             (base-time (real-time)))
                             
                             
  "Converts a time specifier into internal time."
  (let ((parsed-spec
          (etypecase time-spec
            (fixnum time-spec)
            (string (if interval-p
                      (seconds->internal-time (time-parser:parse-interval-or-never time-spec))
                      (seconds->internal-time
                        (- (time-parser:parse-universal-time time-spec 0 nil T base-time)
                           *base-time*))))
            (cons (apply #'parse-time-specifier
                         (first time-spec)
                         (rest time-spec))))))

    (unless (plusp parsed-spec)
      (assert (or (null negative-error-p) (not (minusp parsed-spec)))
              (parsed-spec)
              "~s produces a negative time ~:[~;interval~]; ~d"
              time-spec interval-p parsed-spec)
      (assert (or (null zero-error-p) (not (zerop parsed-spec)))
              (parsed-spec)
              "~s produces a time of zero ~:[~;duration~]; ~
               the minimum time granularity is ~d second~:p, or ~d minute~:p, or ~d hour~:p"
              time-spec interval-p
              *seconds-per-time-unit*
              (/ *seconds-per-time-unit* 60)
              (/ *seconds-per-time-unit* 3600)))
    (values parsed-spec)))


#+OLD
(defun parse-to-internal-time (time-string &optional (base-time (real-time)))
  "Parses `time-string' into internal-time format. The base time defaults to
the current simulated time and must be in universal-time format (not internal time)."
  (seconds->internal-time (- (time-parser:parse-universal-time time-string 0 nil T base-time)
                             *base-time*)))

;;;----------------------------------------------------------------------------
;;; Support for old `current-date' function.

(defun seconds->date (seconds)
  "Returns an integral date."
  (values (floor seconds #.(* 24 60 60))))

(defun seconds->days (seconds)
  "Returns a non-integral date."
  (values (coerce (/ seconds #.(* 24 60 60)) 'short-float)))

(defun date->seconds (date)
  (values (* date #.(* 24 60 60))))

(defun date->internal-time (date)
  (seconds->internal-time (date->seconds date)))

(defun internal-time->date (internal-time)
  "Returns an integral date."
  (seconds->date (internal-time->seconds internal-time)))

(defun internal-time->days (internal-time)
  "Returns a non-integral date."
  (seconds->days (internal-time->seconds internal-time)))
  
(defun current-date ()
  (internal-time->date *elapsed-time*))

(defconstant %not-a-date% -1)

;;;----------------------------------------------------------------------------

(defun print-date (internal-time &key (stream *standard-output*) (show-minutes nil))
  (let* ((date-part (internal-time->date internal-time))
         (remainder-after-date (- internal-time (date->internal-time date-part)))
         (hours-part (internal-time->integral-hours remainder-after-date))
         (remainder-after-hours (- remainder-after-date (hours->internal-time hours-part)))
         (minutes-part (floor (internal-time->seconds remainder-after-hours) 60)))
    (format stream "[~dd ~2,48dh~:[~*~; ~2,48dm~]]"
            date-part hours-part show-minutes minutes-part)))

;;; ***************************************************************************
;;; EOF

