;;;-*- Mode:Common-Lisp; Package:CLIP-USER; Base:10 -*-

;; This file defines a simple event-based simulator.  The events are represented by
;; functions which are kept on a time-sorted queue.

(in-package :clip-user)

(defvar *current-time* 0)
(defvar *event-queue* nil)
(defvar *stop-simulation?* nil)

(defstruct (event (:conc-name event.) (:type list))
  time
  period
  function
  args)

(defun initialize-simulation ()
  "Reset the simulator to an initial state."
  (setf *stop-simulation?* nil)
  (setf *event-queue* nil)
  (setf *current-time* 0))

(defun simulate (&optional (finish-time most-positive-fixnum))
  "Run the simulator."
  (loop until (or *stop-simulation?*
                  (>= *current-time* finish-time)
                  (null *event-queue*))
        for event = (pop *event-queue*)
        do 
        (setf *current-time* (event.time event))
        (run-event event)))
        
(defun run-event (event)
  (apply (event.function event) (event.args event))
  (when (event.period event)
    (schedule-event (event.function event) 
                    (event.args event) 
                    (+ (event.time event) (event.period event))
                    (event.period event))))

(defun schedule-event (function args time &optional period)
  "Schedule `function' to run at `time'.  If period is specified the function will be
rescheduled each time after it has run."

  (el::pushnew-ordered  
   (make-event :time time
               :function function
               :args args
               :period period)
   *event-queue*
   #'<
   :test #'eq
   :key #'event.time))

(defun unschedule-event (event)
  (when (find event *event-queue* :test #'eq)
    (setf *event-queue* (delete event *event-queue* :test #'eq)))
  ;; handle the case where a periodic event is calling unschedule as part
  ;; of its event.function code
  (setf (event.period event) nil))

(defun stop-simulation ()
  (setf *stop-simulation?* t))

(defun current-time ()
  *current-time*)

#|

(defun test-simulator ()
  (initialize-simulation)
  (schedule-event
   #'(lambda ()
       (format t "~&The time is ~a~%" *current-time*))
   nil
   0
   100)
  (simulate 1000))

|#
  