;;;; -*- Mode:Common-Lisp; Package:CLIP-USER; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/systems/clip/demos/agent-simulator/super-agent-experiment.lisp *-*
;;;; *-* Last-edit: Tuesday, October 26, 1993  17:23:46; Edited-By: Westy *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *               CLIPS agent Simulator Experiment Example                 *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: David L. Westbrook
;;;             Department of Computer and Information Science
;;;             University of Massachusetts
;;;             Amherst, Massachusetts 01003.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  01-19-93 File Created.  (WESTY)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package #+CLTL2 clip-user #-CLTL2 'clip-user)

;;; --*--
;;; ***************************************************************************

;; Needs simple-agent-experiment definitions, but we will not load them here.
;(load (merge-pathnames "simple-agent-experiment" (user::clip-load-pathname)))

;;;----------------------------------------------------------------------------
;;; Super clip to collect cost of each agent

(defclip all-agents-costs ()
  (:map-function (find-agents)
   :components   (each-agent-cost)))

(defclip each-agent-cost (agent)
  ()
  
  (cost agent))

;;; ***************************************************************************
;;; The Experiment Definition

(define-experiment simple-agent-experiment-2 ()
  "A test experiment."
  :simulator agent-sim-1
  :ivs ((transition-probability in '(.01 .1))
	      (cost-factor from 1 to 3))
  :instrumentation (agents-cost all-agents-costs completion-time)
  :before-trial (setf *transition-probability* transition-probability 
		      *relative-cost* cost-factor)
  :after-trial (write-current-experiment-data))

;;;----------------------------------------------------------------------------

#| Execute this to run the demo experiment. Be sure to change the output file
   to something reasonable.


(run-experiment 'simple-agent-experiment-2 :output-file #+Explorer "ed-buffer:data.clasp")

|#
  
;;; ***************************************************************************
;;; EOF






