;;;-*- Mode:Common-Lisp; Package:CLIP-USER; Base:10 -*-
;;;; *-* Last-edit: Tuesday, December 7, 1993  15:47:10; Edited-By: File Server *-* 


;;; This file defines a simple agent based simulation using the generic simulator.
;;; Basically, each time step each agent changes state with some probability.

(in-package :clip-user)

;;;----------------------------------------------------------------------------
;;; The agent class

(defclass agent (clip::named-object-mixin clip::remember-instances clip::object-with-properties)
  ((state :initform 'state1 :accessor state)
   (cost :initform 0 :accessor cost))
  (:metaclass clip::named-class))

;;;----------------------------------------------------------------------------
;;; Agent methods

(defmethod (setf state) :after (new-value (the-agent agent))
  (change-of-state-event-function the-agent new-value))

(defvar *verbose* nil)

(defun change-of-state-event-function (the-agent new-state)
  (when *verbose*
    (format *standard-output* "~&~d ~a: ~a~%"
	    *current-time* (clip::name-string the-agent) new-state))
  (values new-state the-agent))

;;;----------------------------------------------------------------------------
;;; States

(defvar *state-list* '(state1 state2 state3 state4 state5 state6))

(defun state-p (state)
  (when (member state *state-list*)
    (values t)))

(deftype state ()
  '(satisfies state-p))

(defun highest-state-p (state)
  (check-type state state)
  (eq state (first (last *state-list*))))

(defun state< (statex statey)
  (check-type statex state)
  (check-type statey state)
  (when (member statey (rest (member statex *state-list*)))
    (values t)))

(defun next-state (state)
  (check-type state state)
  (first (rest (member state *state-list*))))

;;;----------------------------------------------------------------------------
;;; The agents execution function

(defparameter *transition-probability* .01)
(defparameter *relative-cost* 1)

(defmethod run ((the-agent agent))
  (with-accessors  ((state state) (cost cost)) the-agent
    (unless (highest-state-p state)
      ;; Possibly bop it up to the next state.
      (when (probability-p *transition-probability*)
	(setf state (next-state state)))
      ;; When they have reached the highest state incur a cost.
      (when (highest-state-p state)
	(setf cost (* *current-time* *relative-cost*))))))

;;;----------------------------------------------------------------------------
;;; Simulation running

(defun reset-agent-simulation ()
  (clip::forget-instances 'agent)
  (initialize-simulation))
      
(defun run-agent-simulation (&key (reset t) (number-of-agents 3) (end-time most-positive-fixnum)
			     ((:verbose *verbose*) *verbose*))
  (when reset (reset-agent-simulation))
  (loop repeat number-of-agents do
        (schedule-event #'run (list (make-instance 'agent)) 0 1))
  (schedule-event #'(lambda ()
		      (when (check-for-completion)
			(stop-simulation)))
		  nil
		  0
		  1)
  (simulate end-time))

(defun check-for-completion ()
  (every #'(lambda (agent)
	     (highest-state-p (state agent)))
	 (find-agents)))

;;;----------------------------------------------------------------------------
;;; Some utilities
  
(defun probability-p (p)
  (< (random 1.0)  p))

(defun find-agents ()
  (clip::find-instances 'agent))

;;;----------------------------------------------------------------------------
;;; EOF