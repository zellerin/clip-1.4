;;;; -*- Mode:Common-Lisp; Package:CLIP-USER; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/systems/clip/demos/agent-simulator/agent-experiment.lisp *-*
;;;; *-* Last-edit: Friday, January 7, 1994  17:57:58; Edited-By: StAmant *-* 
;;;; *-* Machine: Lester (Explorer II, Microcode 489) *-*
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

;; Needs super-agent experiment definitions, but we will not load them here.
;(load (merge-pathnames "super-agent-experiment" (user::clip-load-pathname)))

(define-simulator agent-sim
  :start-system (run-agent-simulation :reset nil)
  :reset-system (reset-agent-simulation)
  :stop-system (stop-simulation)
  ;; a function that places functions to run on the queue of events.
  :schedule-function (lambda (function time period name &rest options)
                       (declare (ignore name options))
                       (schedule-event function nil time period))
  ;; a function that removes functions from the queue of events.
  :deactivate-scheduled-function unschedule-event
  :seconds-per-time-unit 60
  :timestamp current-time)

;;;----------------------------------------------------------------------------
;;;----------------------------------------------------------------------------
;;; Clip Definitions

;;;----------------------------------------------------------------------------
;;; Multiple columns

(defclip highest-agent-state ()
  (:components (highest-state highest-agent))
  
  (loop 
    with agents = (find-agents)
    with highest-agent = (first agents)
    with highest-state = (state highest-agent)
    for agent in (rest agents)
    for agent-state = (state agent) do
    (when (state< highest-state agent-state)
      (setf highest-state agent-state
	    highest-agent agent))
    finally (return (values highest-state highest-agent))))

;;;----------------------------------------------------------------------------
;;; Post Hoc collection

(defclip posthoc-agent-state-snapshot ()
  (:map-function (clip::find-instances 'agent)
   :components   (each-agent-state-snapshot)))

(defclip each-agent-state-snapshot (agent)
  "Record the state at an agent."
  ()
  (state agent))

;;;----------------------------------------------------------------------------
;;; Periodic collection

(defclip periodic-agent-state-snapshot ()
  (:output-file conjure-up-filename
   :schedule (:period "12 minutes")
   :map-function (clip::find-instances 'agent)
   :components   (each-agent-state-snapshot)))

;;;----------------------------------------------------------------------------
;;; Event Based Clips

(defclip change-of-state (agent-name new-state)
  (:output-file conjure-up-filename
   :trigger-event (change-of-state-event-function :BEFORE)
   :components (new-state agent-name))
  (values new-state agent-name))

;;;----------------------------------------------------------------------------

(defclip change-of-state-pred ()
  (:output-file conjure-up-filename
   :trigger-event (change-of-state-event-function :AFTER :PREDICATE #'(lambda () (evenp (trial-number))))
   :components (fred barney))
  (values :FRED :BARNEY))

;;;----------------------------------------------------------------------------
;;; Example of multiple levels of multiple-column clips.

(defclip change-of-state-3 ()
  (:output-file conjure-up-filename
   :trigger-event change-of-state-event-function
   :components (wilma betty)))

(defclip wilma ()
  (:components (pebbles bam-bam))
  (values :PEBBLES :BAM-BAM))

(defclip betty ()
  (:components (dino the-cat))
  (values :DINO :THE-CAT))

;;;----------------------------------------------------------------------------
;;; Example of component that is a mapping clip

;; NOTE: These do not work. There is a problem with the way arguments are 
;; passed to components that are both children and mappers. A workaround is
;; to define all the components as mapping clips that map over the same list.

#+THIS-DOES-NOT-WORK
(defclip posthoc-state-snapshot ()
   (:components (event-based-generic-state-snapshot))
   
   (values 'agent))

#+THIS-DOES-NOT-WORK
(defclip event-based-generic-state-snapshot (type)
  (:map-function (clip::find-instances type)
   :components   (each-agent-state-snapshot)))

;;;----------------------------------------------------------------------------
;;; Event based collection with mapping

(defclip event-based-agent-state-snapshot ()
  (:output-file conjure-up-filename
   :trigger-event (change-of-state-event-function :AFTER)
   :map-function (clip::find-instances 'agent)
   :components   (each-agent-state-snapshot)))

;;;----------------------------------------------------------------------------
;;; This examples shows the use of the `clip::collect' function to explicitly
;;; collect some values. 

(defclip self-collection (two-item-list)
  (:time-series t
   :output-file conjure-up-filename
   :components (value1 value2))

  (values (first two-item-list) (second two-item-list)))

(defun collect-self-collector ()
  (clip::collect 'self-collection '(:FIRST :SECOND)))

;;; ***************************************************************************
;;; The Experiment Definition

(define-experiment agent-experiment (&key (verbose t))
  "A test experiment."
  :simulator agent-sim
  :variables ((transition-probability in '(.01 .1))
	      (cost-factor in '(1 3 5)))
  :instrumentation (agents-cost all-agents-costs completion-time
				highest-agent-state
                                change-of-state-pred
                                change-of-state-3
				change-of-state
				periodic-agent-state-snapshot
				self-collection
                                event-based-agent-state-snapshot
                                posthoc-agent-state-snapshot
                                #+NEW
                                posthoc-state-snapshot)
  :before-experiment (setf *verbose* verbose)
  
  :before-trial (setf *transition-probability* transition-probability 
		      *relative-cost* cost-factor)

  ;; Set up a script that will call a function (in this case one that
  ;; will do a collection of the `self-collection' clip) at time 5.
  :script ((do-a-collection 5 (collect-self-collector)))

  :after-trial
    (progn
      (collect-self-collector) ; do one final collection of the `self-collection' clip
      (write-current-experiment-data))
    
  :after-experiment (setf *verbose* nil))

(define-experiment sae ()
  "A test experiment."
  :simulator agent-sim
  :variables ((transition-probability in '(.01))
	      (cost-factor in '(1)))
  :instrumentation (posthoc-agent-state-snapshot
		    periodic-agent-state-snapshot)
  :before-trial (setf *transition-probability* transition-probability 
		      *relative-cost* cost-factor)
  :after-trial
    (write-current-experiment-data))

;;;----------------------------------------------------------------------------

(defun conjure-up-filename (&optional (name ""))
  "Returns a filename the adequately captures the moment."
  (multiple-value-bind (seconds minute hour date month year)
		       (if (and *current-experiment*
				(clip::real-world-start-time *current-experiment*))
			   (decode-universal-time (clip::real-world-start-time *current-experiment*))
			 (get-decoded-time))
    (declare (ignore seconds))
    (format nil "~a-~d-~d-~d-~d-~d.clasp"
	    name
            month
            date
            (- year 1900)
            hour
            minute)))

;;;----------------------------------------------------------------------------

;; Execute this to run the demo experiment. Be sure to change the output file
;; to something reasonable.

(defun rexp ()
  (run-experiment 'agent-experiment
		  :output-file (conjure-up-filename #-Explorer "data"
                                                    #+Explorer "ed-buffer:data")
		  :length-of-trial "500 minutes"))


  
;;; ***************************************************************************
;;; EOF






