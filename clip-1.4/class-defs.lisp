;;;; -*- Mode:Common-Lisp; Package:CLIP; Base:10 -*-

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                       CLIP Class Definitions                           *
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
;;;  10-14-93 File created.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :CLIP)

;;; --*--
;;; ***************************************************************************

(defclass experiment (named-object-mixin remember-instances object-with-properties)
  ((status                :initform :idle)
   (trial-number
     :initform nil
     :documentation "In a run of trials, this is the number of the trial that is currently being run.")

   (first-trial-number
     :initarg :first-trial-number
     :initform nil
     :documentation "The starting trial number.")
   (last-trial-number
     :initarg :last-trial-number
     :initform nil
     :documentation "The number of trials in a run of trials.")
   (end-of-trial-time
     :initarg :end-of-trial-time
     :initform nil
     :accessor end-of-trial-time
     :documentation "The absolute time at which a trial ends.  At that time, the system is stopped
and the `after-experiment-run' function is executed.")
   (output-file-name
     :initarg :output-file-name
     :initform nil
     :accessor output-file-name
     :documentation "Bound by `run-experiment' to the file used for data output.")
   (error-file-name
     :initform nil
     :initarg :error-file-name
     :accessor error-file-name
     :documentation "Bound by `run-experiment' to the file used for error output.")
   (extra-header-string :initform nil :initarg :extra-header-string)

   (headers-output-already :initform nil)
   ;; Currently unused...
   (scenario
     :initarg :scenario
     :initform nil
     :accessor experiment.scenario
     :documentation "Bound to the scenario being utilitized by the current experiment.")
   (script-name
     :initarg :script-name
     :initform nil
     :accessor experiment.script-name
     :documentation "Bound to the script-name which is being used to guide the current experiment.")
   (instrumentation-names     :initform nil :initarg :instrumentation-names)
   (instrumentation           :initform nil :initarg :instrumentation)
   (before-experiment-function  :initarg :before-experiment-function)
   (before-trial-function       :initarg :before-trial-function)
   (after-trial-function        :initarg :after-trial-function)
   (after-experiment-function   :initarg :after-experiment-function)
   (ivs-element-init-function :initarg :ivs-element-init-function)
   (script-setup-function :initarg :script-setup-function)
   (arguments    :initarg :arguments)
   (argument-values)
   (ivs          :initarg :ivs)
   (ivs-elements :initarg :ivs-elements)
   (locals       :initarg :locals)
   (locals-init-function :initarg :locals-init-function)
   (system-name         :initarg :system-name)
   (system-version-hook :initarg :system-version-hook)
   (reset-system-hook   :initarg :reset-system-hook)
   (start-system-hook   :initarg :start-system-hook)
   (stop-system-hook    :initarg :stop-system-hook)
   (schedule-function-hook :initarg :schedule-function-hook)
   (deactivate-scheduled-function-hook :initarg :deactivate-scheduled-function-hook)
   (seconds-per-time-unit :initarg :seconds-per-time-unit)
   (ivs-and-args)
   (timestamp-function :initarg :timestamp-function)
   (timestamp-clip)
   (timestamp-clip-name :initarg :timestamp-clip-name)
   (real-world-start-time :initform nil :reader real-world-start-time)
   )
  (:metaclass named-class))

;;;----------------------------------------------------------------------------

(defclass record-call-tree-mixin ()
	  ())

;;;----------------------------------------------------------------------------

(defclass instrumentation (record-call-tree-mixin named-object-mixin remember-instances)
  ((status        :initarg :status       :reader status)
   (arguments     :initarg :arguments    :reader instr.arguments)
   (enable-function   :initform nil :initarg :enable-function  :reader instr.enable-function)
   (disable-function  :initform nil :initarg :disable-function :reader instr.disable-function)
   (reset-function    :initform nil :initarg :reset-function   :reader instr.reset-function)
   (display-function  :initform nil :initarg :display-function :reader instr.display-function)
   (report-function   :initform nil :initarg :report-function  :reader instr.report-function)
   (report-key    :initarg :report-key)
   (documentation :initarg :documentation)
   ;; This particular combiner assumes that *uncollected-value* is NIL. Fix this?
   (combiner      :initarg :combiner :initform 'push-value-combiner)
   (extracter     :initarg :extracter :initform 'average-of-values-extracter)
   (value         :initform *uncollected-value*)
   (number-of-samples :initform nil)
   (time-series? :initform nil :initarg :time-series? :reader time-series-p)
   (output-file-name :initarg :output-file-name :initform nil :reader output-file-name)
   )
  (:metaclass named-class)
  (:default-initargs
    :status :disabled)
  (:documentation
   "Simple clips have no components and collect data immediately prior to reporting
it to the output le at :AFTER-TRIAL time. If they are defined with a :schedule or
:trigger-event defclip option their default behavior is store all of the data
collected during a trial and report a single value which is the mean of all the
values collected."))

;;; ----------------------------------------------------------------------------
;;; Mixins

(defclass scheduled-instrumentation-mixin ()
  ((scheduler-args   :initarg :scheduler-args  :reader instr.scheduler-args)
   (collection-event :initarg :collection-event) ;; so we can deactivate it
   )
  (:metaclass named-class))

(defclass functional-instrumentation-mixin ()
  ((trigger-events :initarg :trigger-events))
  (:metaclass named-class))

(defclass column-producing-instrumentation-mixin ()
  ((components     :initarg :components :reader instr.components)
   ;; note that I decided to star calling them columns
   ;; but did not change the old names
   (unmapped-columns :initarg :unmapped-columns :initform nil))
  (:metaclass named-class))

(defclass composite-instrumentation-mixin (column-producing-instrumentation-mixin)
  ()
  (:metaclass named-class)
  (:default-initargs :combiner 'last-value-combiner
		     :extracter 'last-value-extracter))

(defclass mapping-instrumentation-mixin (column-producing-instrumentation-mixin)
  ((map-function       :initarg :map-function :reader instr.map-function)
   ;; This is used to produce parens in the data file. The lowest level
   ;; super-instrumentation produces a distinct line of data.
   (lowest?        :initform nil :accessor  instr.lowest?)
   (congruity-already-checked? :initform nil :accessor   instr.congruity-already-checked?))
  (:metaclass named-class))

;; These produce multiple lines of data for each call to report.
(defclass time-series-instrumentation-mixin ()
  ()
  (:default-initargs
    :time-series? t
    :combiner 'push-value-combiner)
  (:metaclass named-class))

(defclass child-of-composite-instrumentation-mixin ()
  ((parent        :initarg :parent       :accessor parent))
  (:metaclass named-class))

;;; Instantiable

(defclass periodic-instrumentation (scheduled-instrumentation-mixin
				    instrumentation)
  ()
  (:metaclass named-class))

#+Explorer
(finalize-inheritance (find-class 'periodic-instrumentation))

(defclass super-instrumentation (mapping-instrumentation-mixin
				 instrumentation)
  ()
  (:metaclass named-class)
  (:documentation
   "Mapping clips are specified using the :map-function and :components option to
defclip. They generate multiple columns of data each time they are reported -
they produce multiple columns per component.

All clips with components are referred to as super clips. For a good example of
clips with components and further discussion of their use, see Appendix A.1.1."))

#+Explorer
(finalize-inheritance (find-class 'super-instrumentation))

(defclass functional-instrumentation (functional-instrumentation-mixin
				      instrumentation)
  ()
  (:metaclass named-class))

#+Explorer
(finalize-inheritance (find-class 'functional-instrumentation))

(defclass periodic-super-instrumentation (time-series-instrumentation-mixin
                                          scheduled-instrumentation-mixin
                                          mapping-instrumentation-mixin
					  instrumentation)
  ()
  (:metaclass named-class)
  (:documentation
   "Clips that have COMPONENTS and SCHEDULE option are
periodic time-series clips. They generate multiple data columns in the manner of
component clips (which they are) and also multiple data rows. Each row
corresponds to a single collection activated periodically. Since time-series
clips generate multiple rows, they are generally written to a data le that is
separate from the main experiment (summary) data le. The name of the data file
associated with a time-series clip is speci ed using the OUTPUT-FILE option to
defclip.  The SCHEDULE-FUNCTION, SECONDS-PER-TIME-UNIT, and TIMESTAMP keywords
to define-simulator must be specified."))

#+Explorer
(finalize-inheritance (find-class 'periodic-super-instrumentation))

(defclass composite-instrumentation (composite-instrumentation-mixin
				     instrumentation)
  ()
  (:metaclass named-class)
  (:default-initargs :combiner 'last-value-combiner
		     :extracter 'last-value-extracter)
  (:documentation
   "Clips with components, as specified by the :components keyword, generate
multiple columns in a data le each time they are reported. Without other options
they produce one column per component (composite clips).

All clips with components are referred to as super clips. For a good example of
clips with components and further discussion of their use,"))

#+Explorer
(finalize-inheritance (find-class 'composite-instrumentation))

(defclass composite-time-series-instrumentation (time-series-instrumentation-mixin
						 composite-instrumentation-mixin
						 instrumentation)
  ()
  (:metaclass named-class)
  (:documentation
   "Time series clip that uses explicit CLIP::COLLECT to collect the data."))

#+Explorer
(finalize-inheritance (find-class 'composite-time-series-instrumentation))

(defclass periodic-composite-instrumentation (time-series-instrumentation-mixin
                                               scheduled-instrumentation-mixin
                                               composite-instrumentation-mixin
					       instrumentation)
  ()
  (:metaclass named-class))

#+Explorer
(finalize-inheritance (find-class 'periodic-composite-instrumentation))

(defclass functional-composite-instrumentation (time-series-instrumentation-mixin
                                                 functional-instrumentation-mixin
                                                 composite-instrumentation-mixin
						 instrumentation)
  ()
  (:metaclass named-class)
  (:documentation
   "Clips that have COMPONENTS and TRIGGER-EVENT option are
event based time-series clips. They generate multiple data columns in the manner
of component clips (which they are) and also multiple data rows. Each row
corresponds to a single collection and is triggered by a particular event. Since
time-series clips generate multiple rows, they are generally written to a data
file that is separate from the main experiment (summary) data file. The name of
the data le associated with a time-series clip is specified using the
OUTPUT-FILE option to defclip. Event-based time-series clips require that the
Common Lisp implementation provide some mechanism similar to the advise
function."))

#+Explorer
(finalize-inheritance (find-class 'functional-composite-instrumentation))

(defclass child-of-composite-instrumentation (child-of-composite-instrumentation-mixin
					      instrumentation)
  ()
  (:metaclass named-class))

#+Explorer
(finalize-inheritance (find-class 'child-of-composite-instrumentation))

(defclass composite-child-of-composite-instrumentation (composite-instrumentation-mixin
							child-of-composite-instrumentation-mixin
							instrumentation)
  ()
  (:metaclass named-class))

#+Explorer
(finalize-inheritance (find-class 'composite-child-of-composite-instrumentation))

(defclass composite-child-of-functional-composite-instrumentation
     (time-series-instrumentation-mixin
      functional-instrumentation-mixin
      composite-instrumentation-mixin
      child-of-composite-instrumentation-mixin
      instrumentation)
  ()
  (:metaclass named-class))

#+Explorer
(finalize-inheritance (find-class 'composite-child-of-functional-composite-instrumentation))

;; NOTE: These do not work. There is a problem with the way arguments are
;; passed to components that are both children and mappers. A workaround is
;; to define all the components as mapping clips that map over the same list.

;(defclass mapping-child-of-composite-instrumentation (mapping-instrumentation-mixin
;						      child-of-composite-instrumentation-mixin
;						      instrumentation)
;  ()
; (:metaclass named-class))

;#+Explorer
;(finalize-inheritance (find-class 'mapping-child-of-composite-instrumentation))

(defclass functional-mapping-instrumentation (time-series-instrumentation-mixin
					      functional-instrumentation-mixin
					      mapping-instrumentation-mixin
					      instrumentation)
  ()
  (:metaclass named-class)
  (:documentation
   "Event based collection with mapping."))

#+Explorer
(finalize-inheritance (find-class 'functional-mapping-instrumentation))

;;;----------------------------------------------------------------------------

(defclass simple-instrumentation (instrumentation)
  ()
  (:metaclass named-class))

#+Explorer
(finalize-inheritance (find-class 'simple-instrumentation))

;;;----------------------------------------------------------------------------
