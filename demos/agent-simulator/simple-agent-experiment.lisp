;;;; -*- Mode:Common-Lisp; Package:CLIP-USER; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/systems/clip/demos/agent-simulator/simple-agent-experiment.lisp *-*
;;;; *-* Last-edit: Tuesday, October 26, 1993  17:23:13; Edited-By: Westy *-* 
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

(in-package clip-user)

;;; --*--
;;; ***************************************************************************

(define-simulator agent-sim-1
  :start-system (run-agent-simulation :reset nil)
  :reset-system (reset-agent-simulation)
  :stop-system stop-simulation)

;;;----------------------------------------------------------------------------
;;; Clip Definitions

(defclip agents-cost ()
  ()

  (reduce #'+ (find-agents) :key #'cost))


(defclip completion-time ()
  ()
  
  (current-time))

;;; ***************************************************************************
;;; The Experiment Definition

(define-experiment simple-agent-experiment-1 ()
  "A test experiment."
  :simulator agent-sim-1
  :ivs ((transition-probability in '(.01 .1))
	      (cost-factor from 1 to 3))
  :instrumentation (agents-cost completion-time)
  :before-trial (setf *transition-probability* transition-probability 
		      *relative-cost* cost-factor)
  :after-trial (write-current-experiment-data))

;;;----------------------------------------------------------------------------

#| Execute this to run the demo experiment. Be sure to change the output file
   to something reasonable.


(run-experiment 'simple-agent-experiment-1 :output-file #+Explorer "ed-buffer:data.clasp")

|#
  
;;; ***************************************************************************
;;; EOF






