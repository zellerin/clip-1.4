;;;; -*- Mode:Common-Lisp; Package:CLIP; Base:10 -*-

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                               Parameters                               *
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
;;;  07-21-93 Added *data-separator-character* and *output-format*. (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package CLIP)

;;; --*--
;;; ***************************************************************************

(defparameter *verbose* nil)
(defparameter *override-print-escape* nil)

(defparameter *output-format* :CLASP)
(defparameter *suppress-headers* nil)

(defparameter *data-separator-character* #\space)

(defvar *current-experiment* nil
  "Bound to the experiment that is currently running.")

(defvar *debug* nil)

;;; ***************************************************************************
;;; EOF
