;;;; -*- Mode:Common-Lisp; Package:CLIP; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/systems/clip/development/standard-clips.lisp *-*
;;;; *-* Last-edit: Wednesday, January 5, 1994  15:42:10; Edited-By: Westy *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                        Built-in Clip Definitions                       *
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
;;;  01-05-94 File created.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :CLIP)

;;; --*--
;;; ***************************************************************************
 
(defclip trial-number (&rest args)
  (:class simple-instrumentation
   :report-key "Trial")
  (assert *current-experiment* () "there is no experiment currently running.")
  (slot-value *current-experiment* 'trial-number))

(defclip scenario-name (&rest args)
  (:class simple-instrumentation
   :report-key "Scenario name")
  (assert *current-experiment* () "there is no experiment currently running.")
  (name (slot-value *current-experiment* 'scenario)))

(defclip script-name (&rest args)
  (:class simple-instrumentation
   :report-key "Script name")
  (assert *current-experiment* () "there is no experiment currently running.")
  (slot-value *current-experiment* 'script-name))

;;; ***************************************************************************
;;; EOF