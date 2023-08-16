;;;; -*- Mode:Common-Lisp; Package:CLIP; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/systems/clip/development/experiment-runner.lisp *-*
;;;; *-* Last-edit: Monday, January 31, 1994  10:22:37; Edited-By: Westy *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                 Functions to Set Up and Run Experiments                *
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
;;;  10-17-89 Original Phoenix instrumentation package started.  (Westy)
;;;  11-01-90 Added instrumentation features.  (Westy)
;;;  ...      Loads of undocumented changes.  (Westy)
;;;  02-05-91 Added or altered `shutdown-experiment', 
;;;           `shutdown-and-go-to-next-trial' and `shutdown-and-rerun-trial
;;;           to provide a more consistent interface. A command level
;;;           interface is being considered.  (Westy)
;;;  02-06-91 Migrated all of the globals into the `experiment' instance.
;;;           It is now much less hacker friendly, but a lot cleaner.  (Westy)
;;;  02-26-91 Added `starting-trial-number' argument to `run-experiment'.
;;;           (Westy)
;;;  01-19-93 Began portable implementation (CLIP). (Westy)
;;;  01-29-93 Added iv and local support and finally used cross-product code.
;;;           (Westy)
;;;  02-19-93 Added support for lambda-list-keywords.  
;;;           Modified:  define-experiment, run, startup-trial, shutdown-trial
;;;           (Rubinstein)
;;;  07-21-93 Added code to deal with *data-separator-character* and
;;;           *output-format*. (Westy)
;;;  12-06-93 Added Scott's changes to make it work with Phoenix again. The major
;;;           change is the use of the `status' slot to indicate what action
;;;           to take after a trial.  (Westy)
;;;  01-13-94 Rewrote `shutdown-trial-of-current-experiment' and friends 
;;;           to use restarts. (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :CLIP)

;;; --*--
;;; ***************************************************************************

(defmethod store-ivs-elements ((the-experiment experiment) arg-list)
  (apply (slot-value the-experiment 'ivs-element-init-function) arg-list))

(defmethod store-arg-values ((the-experiment experiment) arg-list)
  (setf (slot-value the-experiment 'argument-values) arg-list))

(defmethod get-arg-values ((the-experiment experiment))
  (slot-value the-experiment 'argument-values))

(defmethod store-local ((the-experiment experiment) symbol value)
  (setf (get-value the-experiment symbol) value))

(defmethod get-local ((the-experiment experiment) symbol)
  (get-value the-experiment symbol))

(defsetf get-local store-local)
         
(defmethod init-ivs-values-for-trial ((the-experiment experiment))
  (with-slots (trial-number ivs ivs-elements) the-experiment
    (when ivs
      (mapc #'store-local 
            (el::circular-list the-experiment)
            ivs
            (apply #'nth-elt-of-cross-product (1- trial-number) ivs-elements))))
  (values))

(defmethod compute-ending-trial-number ((the-experiment experiment) repetitions)
  (with-slots (ivs-elements first-trial-number) the-experiment 
    (* (if ivs-elements 
	   (reduce #'* ivs-elements  :key #'length)
	   1)
       repetitions)))

(defmethod get-ivs-values ((the-experiment experiment))
  (when (slot-value the-experiment 'ivs)
    (mapcar #'get-local 
            (el::circular-list the-experiment)
            (slot-value the-experiment 'ivs))))

(defmethod init-local-values ((the-experiment experiment)  arg-list)
  (apply (slot-value the-experiment 'locals-init-function) arg-list))

;;;----------------------------------------------------------------------------

(defun find-experiment (experiment-spec &optional no-error-p)
  "Given a experiment specifier this returns the experiment instance. `no-error-p' if non-nil says to
return nil if there is no such experiment, instead of signalling an error."
  (find-instance-by-name experiment-spec 'experiment no-error-p))

;;;----------------------------------------------------------------------------

;;; Modified the debugging stuff because I needed to use w:notify, and I think
;;; it's better to be able to redefine a function than a macro, because you
;;; don't have to recompile all the callers.

(defun when-debugging-format* (format-string &rest format-args)
  "Outputs debugging information about CLIP.  Defaults to `format,' but you can change this function if you
need to direct debugging info to another stream, or use ``notifications'' or some such."
  (progn (fresh-line *standard-output*)
	 (apply #'format *standard-output* format-string format-args)
	 (terpri *standard-output*))
  #+ignore
  (apply #'w:notify nil format-string format-args))

(defmacro when-debugging-format (tag format-string &rest format-args)
  (declare (ignore tag))
  `(when *debug* (when-debugging-format* ,format-string ,@format-args)))

;;; ============================================================================
;;; Put a when-debugging-format in here.  Otherwise, this is unchanged. SDA

(defmethod startup-experiment ((the-experiment experiment) arg-list repetitions)
  (when-debugging-format experiment-runner "Startup experiment: ~s ~s ~s" the-experiment arg-list repetitions)
  (with-slots (before-experiment-function status last-trial-number seconds-per-time-unit
	       real-world-start-time) the-experiment
    (when-debugging-format experiment-runner "Running :BEFORE-EXPERIMENT code")
    (setf status :initializing-experiment)
    (setf real-world-start-time (get-universal-time))
    (setf (seconds-per-time-unit) seconds-per-time-unit)
    (store-arg-values the-experiment arg-list)
    (store-ivs-elements the-experiment arg-list)
    (init-local-values the-experiment arg-list)
    (unless last-trial-number 
      (setf last-trial-number 
	    (or (compute-ending-trial-number the-experiment repetitions)
		(error "you have to supply :NUMBER-OF-TRIALS"))))
    (when before-experiment-function
      (apply before-experiment-function arg-list))))

;;; ============================================================================
;;; Removed form that set `status' to :running-trial, since we have to do other
;;; stuff first. SDA

(defmethod startup-trial ((the-experiment experiment))
  (when-debugging-format experiment-runner "Loop:  Startup trial")
  (with-slots (before-trial-function trial-number
				     last-trial-number status ivs-and-args) the-experiment
    (format *standard-output* "~&TRIAL NUMBER ~d OF ~d~%" trial-number last-trial-number)
    (when-debugging-format experiment-runner "Running :BEFORE-TRIAL code")
    (setf status :initializing-trial)
    (init-ivs-values-for-trial the-experiment)
    (setf ivs-and-args (append (get-ivs-values the-experiment) 
			       (get-arg-values the-experiment)))
    (when before-trial-function
      (apply before-trial-function ivs-and-args))
    (reset-system ivs-and-args)))

;; For use by hooks and other things that need a function with no args.
(defun startup-trial-of-current-experiment ()
  (assert *current-experiment* () "there is no experiment currently running.")
  (startup-trial *current-experiment*))


;;; ============================================================================
;;; Interface for users to explicitly terminate trials.


(defun shutdown-trial-of-current-experiment (&optional (action :run-next-trial))
  (assert *current-experiment* () "there is no experiment currently running.")
  (check-type action (member :rerun-trial :run-next-trial :abort-experiment))
  (when-debugging-format experiment-runner "Early Trial Shutdown; ~a" action)
  (with-slots (status stop-system-hook ivs-and-args) *current-experiment*
    ;; Setting the status slot is how we tell the `run' function what to do when we return.
    (setf status action)
    ;; First we attempt to run the the stop-system code. If this is a multi-processing system
    ;; like Phoenix this code should shut down all of the other processes.
    (when stop-system-hook
      (apply stop-system-hook ivs-and-args))
    ;; If this is a single process thread system the this should throw us back to the
    ;; `run' function.
    (when (find-restart 'shutdown-trial)
      (invoke-restart 'shutdown-trial))))

;;; These are all user-level functions for use by hooks and other things
;;; that need a function with no args.

(defun shutdown-and-rerun-trial ()
  "This will cause the current trial to be aborted (no data written) and
restarted.  This is a function that users should call when they have detected
an error condition of some sort that renders the trial worthless, but
rerunning the trial may work.  This is a no-arg function that Phoenix users
can use for *phoenix-error-restart-hook* and *task-error-hook*."
  (shutdown-trial-of-current-experiment :rerun-trial))

(defun shutdown-and-run-next-trial ()
  "This will cause the current trial to be stopped (data will be written) and
the next trial started.  This is a function that users should call when want
to normally shutdown a trial and collect and report the data from that trial."
  (shutdown-trial-of-current-experiment :run-next-trial))

(defun shutdown-experiment ()
  "This will cause the current trial to be aborted (no data will be written)
and will return the system to the state it was before the experiment began (by
running the after-experiment code)."
  (shutdown-trial-of-current-experiment :abort-experiment))

#+OLD
(defun rerun-trial ()
  "Obsolete; use `shutdown-and-rerun-trial'"
  (shutdown-and-rerun-trial))

;;;----------------------------------------------------------------------------

(defmethod process-instrumentation-names ((the-experiment experiment)) ()
  (when-debugging-format experiment-runner "Processing instrumentation names")
  (with-slots (instrumentation instrumentation-names) the-experiment 
    ;; Insure that the latest version of each instrumentation is used.
    (setf instrumentation
          (mapcar #'find-instrumentation instrumentation-names))
    ; Obsolete
    ;(check-instrumentation-component-congruity instrumentation)
    ))

;;; ============================================================================
;;; Moved the instrumentation stuff out of the `run' method.  I haven't defined
;;; a defgeneric for this. SDA

(defmethod reset-instrumentation ((the-experiment experiment))
  "Resets and enables all instrumentation.  Called in the main loop of the `run' method of experiments."
  (when-debugging-format experiment-runner "Loop:  reset instrumentation")
  (with-slots (status ivs timestamp-clip instrumentation) the-experiment
    (setf status :reset-instrumentation)
    (reset (find-instrumentation 'trial-number))
    (dolist (iv ivs)
      (reset (find-instrumentation iv))
      (enable (find-instrumentation iv)))
    (when timestamp-clip
      (reset timestamp-clip)
      (enable timestamp-clip))
    (dolist (in instrumentation)
      (reset in)
      (enable in))))  

; From the users point of view this is how things work.
; Run :BEFORE-EXPERIMENT code
; LOOP
;   Run :BEFORE-TRIAL code
;   Call :RESET-SYSTEM function	
;   Reset and Enable all the instrumentation
;   Call :START-SYSTEM function
;   Run :AFTER-TRIAL code
; END LOOP when all trials completed
; Run :AFTER-EXPERIMENT code

;;; ============================================================================
;;; Changed how trials end under user control.  Now, the user just tells how and
;;; when to stop the system, and this function schedules `stop-system' to run,
;;; instead of scheduling `shutdown-trial-of-current-experiment.' That function
;;; is no longer used; we do everything here, modifying the `status' variable so
;;; that we can tell where things died if they do.  The rule is to modify
;;; `status' before each call to user code, since it's mostly in the user code
;;; that we're likely to die. SDA

(defmethod run ((the-experiment experiment) args
		 ending-trial-number
		 repetitions end-time
		 output-file error-stream error-file
		 extra-header
		 *suppress-headers*
		 *output-format*
		 starting-trial-number)
  
  (declare (ignore error-stream))
  
  (with-slots (status first-trial-number last-trial-number trial-number
		      end-of-trial-time script-setup-function scenario
		      script-name output-file-name error-file-name
		      extra-header-string headers-output-already instrumentation
		      timestamp-clip timestamp-clip-name arguments ivs ivs-and-args
		      after-trial-function after-experiment-function)
	      the-experiment
    #+IGNORE ; this does not work with &key args
    ;; Check for correct number of arguments in `args'.
    (assert (= (length args) (length arguments)) (arguments)
	    "too ~:[few~;many~] arguments given.  There are ~s:  ~s"
	    (> (length args) (length arguments))
	    (length arguments) arguments)
    
    (setf first-trial-number starting-trial-number
          last-trial-number ending-trial-number 
          end-of-trial-time end-time
          trial-number starting-trial-number
          output-file-name output-file
          error-file-name error-file
          extra-header-string extra-header
          headers-output-already nil)
    
    (process-instrumentation-names the-experiment)
    ;; Explicitly set timestamp-clip to nil, otherwise it's unbound.  SDA
    (if timestamp-clip-name
	(setf timestamp-clip (find-instrumentation timestamp-clip-name))
	(setf timestamp-clip nil))
    (startup-experiment the-experiment args repetitions)
    
    (loop 
      (startup-trial the-experiment)
      (when script-setup-function
	(funcall script-setup-function))
      (when end-of-trial-time
	(when-debugging-format experiment-runner "Loop:  Schedule end-of-trial")
	(schedule-function 'shutdown-and-run-next-trial end-of-trial-time nil 'end-of-trial))
      (reset-instrumentation the-experiment)
      (restart-case (start-system ivs-and-args)
        (shutdown-trial ()
          (when-debugging-format experiment-runner "Loop:  Handling Restart")))
      (when-debugging-format experiment-runner "Loop:  Returned from system")
      (unless (member status '(:running-trial :run-next-trial :rerun-trial :abort-experiment))
	(when-debugging-format experiment-runner "Weird< status after return:  ~s" status))
      ;; If status is :abort-experiment, stop looping
      (when (eq status :abort-experiment) (return))
      ;; If status is :rerun-trial, this will go to the top of the loop
      (unless (eq status :rerun-trial)
	;; Status must be :running-trial or :run-next-trial, which is normal, so run :after-trial code.
	(when after-trial-function
	  (setf status :after-trial)
	  (when-debugging-format experiment-runner "Running :AFTER-TRIAL code")
	  (apply after-trial-function ivs-and-args))
	;; Stop looping unless more trials
	(unless (<= (incf trial-number) last-trial-number)
	  (return))))
    
    ;; After experiment code is after the loop
    (when-debugging-format experiment-runner "Running :AFTER-EXPERIMENT code")
    (setf status :after-experiment)
    (when after-experiment-function
      (apply after-experiment-function (get-arg-values the-experiment)))
    ;; ...disable the instrumentation.
    (setf status :disable-instrumentation)
    (dolist (instrumentation instrumentation)
      (disable instrumentation))
    (close-error-file the-experiment)
    (setf status :done)))

;;;----------------------------------------------------------------------------
;;; The top-level experiment interface function.

(defun run-experiment (experiment-name
                         &key 
                         args
			 (repetitions 1)
			 number-of-trials length-of-trial
                         output-file (error-stream *error-output*)
                         error-file extra-header
			 (suppress-headers *suppress-headers*)
			 (output-format *output-format*)
                         (starting-trial-number 1))
                         
  "Run the experiment named `experiment-name'.  `output-file' is
optional, but must be specified if `write-current-experiment-data' is
called from within your experiment.  `error-stream' can be used to direct
output to an open stream.  It defaults to the *error-output* which sends
error/debug output to the appropriate place.  `error-file' can also be
used to direct the error/debug output to a file.  `extra-header' is
written at the end of the header of the output file.
`starting-trial-number' can be used to change this value to something
other than one (1).  If `number-of-trials' is not specified it will be
calculated so as to vary all the independent variables across all their
values `repetitions' (default 1) times.
"

  (run (setf *current-experiment* (find-experiment experiment-name))     
       args
       (when number-of-trials
         (1- (+ number-of-trials starting-trial-number)))
       repetitions
       (when length-of-trial
         (parse-time-specifier length-of-trial :interval-p t))
       output-file error-stream error-file
       extra-header
       suppress-headers
       output-format
       starting-trial-number))
          
;;;----------------------------------------------------------------------------

(defun schedule-function (function time period name &rest options)
  (assert *current-experiment* () "there is no experiment currently running.")
  (with-slots (schedule-function-hook) *current-experiment*
    (assert schedule-function-hook () "you need to supply :SCHEDULE-FUNCTION")
    (values (apply schedule-function-hook function time period name options))))

(defun deactivate (scheduled-function)
  (assert *current-experiment* () "there is no experiment currently running.")
  (with-slots (deactivate-scheduled-function-hook) *current-experiment*
    (when deactivate-scheduled-function-hook
      (funcall deactivate-scheduled-function-hook scheduled-function))))
  
(defun system-version (args)
  (assert *current-experiment* () "there is no experiment currently running.")
  (with-slots (system-version-hook) *current-experiment*
    (if system-version-hook
      (apply system-version-hook args)
      "Unknown")))

(defun reset-system (args)
  (assert *current-experiment* () "there is no experiment currently running.")
  (with-slots (reset-system-hook) *current-experiment*
    (when reset-system-hook
      (apply reset-system-hook args))))


;;; ============================================================================
;;; Changed to pass the correct args and take no args.  SDA

(defun stop-system ()
  (assert *current-experiment* () "there is no experiment currently running.")
  (with-slots (stop-system-hook ivs-and-args) *current-experiment*
    (when stop-system-hook
      (apply stop-system-hook ivs-and-args))))

;;; ============================================================================
;;; Changed to set the status slot to :running-trial.  I think this should be a
;;; method instead.

(defun start-system (args)
  (when-debugging-format experiment-runner "Loop:  Starting system")
  (assert *current-experiment* () "there is no experiment currently running.")
  (with-slots (status start-system-hook) *current-experiment*
    (assert start-system-hook () "you need to supply :START-SYSTEM")
    (setf status :running-trial)
    ;; Can't do anything after this, because it may not return for a while!
    (apply start-system-hook args)))

;;;----------------------------------------------------------------------------

(defmethod close-error-file ((the-experiment experiment))
;  (with-slots (error-file-name previous-*phoenix-error-output*) the-experiment
;    (when error-file-name
;      (close *phoenix-error-output*)
;      (setf *phoenix-error-output* previous-*phoenix-error-output*)))
  )

;;;----------------------------------------------------------------------------
;;; Functions for collecting and writing out data.

(defmethod output-pathname ((the-experiment experiment) (the-instrumentation instrumentation))
  (when (slot-value the-instrumentation 'output-file-name)
    (let* ((instrumentation-output-file (slot-value the-instrumentation 'output-file-name))
	   (instrumentation-output-file-name
	    (etypecase instrumentation-output-file
	      (string instrumentation-output-file)
	      (cons (eval instrumentation-output-file))
	      (function (funcall instrumentation-output-file (name-string the-instrumentation)))
	      (symbol (funcall instrumentation-output-file (name-string the-instrumentation))))))
      (merge-pathnames
       instrumentation-output-file-name
       (slot-value the-experiment 'output-file-name)))))

;;; ============================================================================
;;; Changed to write out the `extra-header' information.  The user is
;;; responsible for formatting.


(defun possibly-write-experiment-headers (experiment instrumentations stream filename)
  (with-slots (headers-output-already) experiment
    (unless
     (or *suppress-headers*
	 (eq *output-format* :DATA-ONLY)
	 (assoc (namestring filename) headers-output-already :test #'string=))
     (write-experiment-headers experiment stream instrumentations)
     (el::pushnew-acons headers-output-already (namestring filename) t :test #'string=))))

(defmethod write-experiment-headers ((the-experiment experiment) stream &optional specific-instrumentations)
  (with-slots (name description scenario script-name system-name
		    first-trial-number last-trial-number end-of-trial-time
		    extra-header-string instrumentation) the-experiment
    (multiple-value-bind (second min hour date month year)
        (get-decoded-time)
      (declare (ignore second))
      #+IGNORE
      (when-debugging-format write-experiment
			     "extra-header-string = ~s~%*output-format* = ~s"
			     extra-header-string *output-format*)
      (when (eq *output-format* :CLASP)
;                             ~@[~:{~4,,,'*<~>~1@T~99@<~a~>~1@T~4,,,'*<~>~%~}~]~
;                     ~@[~:{~4,,,'*<~>~1@T~99@<~a~>~1@T~4,,,'*<~>~%~}~]~
	;; Added the ~a at the end of this, for the extra-header-string.  SDA 12/2/93
	(format stream "~%\"~%~109,,,'*<~>~%~
                     ~4,,,'*<~>~1@T~99@T~1@T~4,,,'*<~>~%~
                     ~4,,,'*<~>~1@T~99@<Experiment: ~:(~a~)~>~1@T~4,,,'*<~>~%~
                     ~4,,,'*<~>~1@T~99@<Machine: ~a~>~1@T~4,,,'*<~>~%~
                     ~4,,,'*<~>~1@T~99@<~a version: ~a~>~1@T~4,,,'*<~>~%~
                     ~4,,,'*<~>~1@T~99@<Date: ~d/~d/~d ~d:~d~>~1@T~4,,,'*<~>~%~
                     ~4,,,'*<~>~1@T~99@<Scenario: ~:(~a~)~>~1@T~4,,,'*<~>~%~
                     ~4,,,'*<~>~1@T~99@<Script-name: ~:(~a~)~>~1@T~4,,,'*<~>~%~
                     ~4,,,'*<~>~1@T~99@<First trial number: ~d~>~1@T~4,,,'*<~>~%~
                     ~4,,,'*<~>~1@T~99@<Last trial number: ~d~>~1@T~4,,,'*<~>~%~
                     ~4,,,'*<~>~1@T~99@<Number of trials: ~d~>~1@T~4,,,'*<~>~%~
                     ~4,,,'*<~>~1@T~99@<Max trial length: ~d hours~>~1@T~4,,,'*<~>~%~
                      ~109,,,'*<~>~%~
                                 ~a
                     ~%The key follows:\""
		#+BAD
		(and description
		     #+Explorer
		     (tv:break-string-into-lines description)
		     #-Explorer
		     (list description))
		name
		#+Explorer
		net:local-pretty-host-name
		#-Explorer
		"Unknown"
		system-name
		(system-version (slot-value the-experiment 'argument-values))
		month date (- year 1900) hour min
		(if scenario (name scenario) "None")
		(or script-name "None")
		first-trial-number
		last-trial-number
		(1+ (- last-trial-number first-trial-number))
		(if end-of-trial-time (internal-time->hours end-of-trial-time) "Unknown")
		;; Removed this, since scenario is given above.  SDA 12/2/93
		#+BAD
		scenario
;              (when scenario
;                (mapcar #'(lambda (agent) (list (ad.number agent) (ad.type agent))) 
;                        (get-agents-from-scenario scenario)))
		;; added this arg, to go with the ~a.  SDA 12/2/93
		(or extra-header-string "")
		#+BAD
		(and extra-header-string
		     #+Explorer
		     (tv:break-string-into-lines extra-header-string)
		     #-Explorer
		     (list extra-header-string))))
      
      (let ((instrumentations (or (parse-instrumentation specific-instrumentations nil) instrumentation)))
	(print-report-key-implicit-clips the-experiment stream (some #'time-series-p instrumentations))
	(dolist (inst instrumentations)
	  (print-report-key inst stream))
	(when (eq *output-format* :CLASP)
	  (terpri stream))))))

;;; Added the following method so that it's easier to add some more text to the
;;; header string from other packages.  It also exists to hide the
;;; implementation of header strings.

(defmethod append-extra-header ((the-experiment experiment) string)
  (with-slots (extra-header-string) the-experiment
    (setf extra-header-string
	  (concatenate 'string
		       extra-header-string
		       string))))

(defun write-current-experiment-data (&key 
				      (separator *data-separator-character*)
				      (format *output-format*)
				      instrumentation
				      stream)
  "Causes each experiment instrumentation to write its data to `filename'.
`separator' should be a character which will be used to separate fields.  It
defaults to the value of *data-separator-character*.  `format' should be one of
:CLASP which means write a clasp native format data file, :ASCII which means
write a standard `separator' delimited data file including column names or
:DATA-ONLY which is the same as :ASCII except no column names are included.  It
defaults to the value of *output-format*.  `instrumentation' can be used to
specify a subset of the experiments instrumentation to write to the data file."

  (assert *current-experiment* () "there is no experiment currently running.")
  (check-type format (member :CLASP :DATA-ONLY :ASCII))
  (when (slot-value *current-experiment* 'output-file-name)
    (write-experiment-data *current-experiment* separator format instrumentation stream)))

(defmethod write-experiment-data ((the-experiment experiment)
				  *data-separator-character*
				  *output-format*
				  specific-instrumentations
				  stream)
  (with-slots (instrumentation output-file-name) the-experiment
    (let ((stream-or-file (or stream output-file-name))
          (instrumentations (or (parse-instrumentation specific-instrumentations t)
                                instrumentation)))
      (when instrumentations
	(assert output-file-name () "filename not specified")
        (let* ((instrumentations-to-other-files
                (remove-if-not #'output-file-name instrumentations))
	       (instrumentations-to-main-file 
                (remove-if #'output-file-name instrumentations))
	       (time-series-instrumentations
                (remove-if-not #'time-series-p instrumentations)))
	(with-open-experiment-file (the-experiment instrumentations-to-main-file stream stream-or-file)
          ;; Make sure that multiple time-series instrumentations are not
          ;; tryting to write to the main output-file
          (cond ((null (set-difference time-series-instrumentations
				       instrumentations-to-other-files))
		 (with-output-as-clasp-row (stream)
		   (report-internal-implicit-clips the-experiment stream nil nil)
		   (dolist (inst instrumentations)
			   (report inst stream))))
		(t
		 ;; otherwise there can be only one and it will call
		 ;; `with-output-as-clasp-row' itself
		 (assert (el::length-1-list-p instrumentations) ()
                    "multiple clips cannot write to the single output file if any are time series;~%~
                     file is ~a; clips are ~{~a ~^~}"
                    output-file-name instrumentations)
		 (report (first instrumentations) stream)))))))))

(defun parse-instrumentation (instrumentation &optional (error-p *current-experiment*))
  (let ((parsed-instrumentation
	  (mapcar #'find-instrumentation
		  (if (listp instrumentation)
		      instrumentation
		      (list instrumentation)))))
    (when error-p 
      (assert *current-experiment* () "there is no experiment currently running.")
      (assert (not (set-difference parsed-instrumentation 
				   (slot-value *current-experiment* 'instrumentation)))))
    
    (values parsed-instrumentation)))

(defun test-instrumentation-report (instrumentation-name[s] &key (stream *standard-output*) map-list)
  "A test function for `write-current-experiment-data'."
  (let ((instrumentations (parse-instrumentation instrumentation-name[s])))
    ;(check-instrumentation-component-congruity instrumentations)
    (unless (or *suppress-headers*
		(eq *output-format* :DATA-ONLY))
      (dolist (instrumentation instrumentations)
        (cond (map-list
	       (dolist (item map-list)
		 (print-report-key instrumentation stream item)))
	      (t
	       (print-report-key instrumentation stream)))
        (if map-list
	  (dolist (item map-list)
	    (report instrumentation stream item))
	  (report instrumentation stream))))))


;;;----------------------------------------------------------------------------

#|

(defun decode-row-major-index (index &rest dimensions)
  (assert (every #'integerp dimensions))
  (let ((remainder index)
	index-in-this-dimension)
    (utils:with-collection
      (dotimes (d (- (length dimensions) 1))
	(multiple-value-setq (index-in-this-dimension remainder)
	  (truncate remainder (reduce #'* dimensions :start (1+ d))))
	(utils:collect index-in-this-dimension))
      (utils:collect remainder))))

(defun test-decode-row-major-index (&rest dimensions)
  (let ((array (make-array dimensions)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref array i) (random)))
    (dotimes (i (array-total-size array))
      (let ((indices (apply #'decode-row-major-index i dimensions)))
	(assert (= (row-major-aref array i)
		   (apply #'aref array indices))
		()
		"the decoding of ~d is incorrect:  ~s" i (decode-row-major-index i dimensions))))))
|#
;; This code courtesy of Scott Anderson

(defun nth-elt-of-cross-product (n &rest sets)
  "Suppose we created a multidimensional array, with as many dimensions as
we have sets, and in which the size of each dimension were equal to the size of
the corresponding set.  That array would have exactly as many elements as the
cross product of all these sets, and we could easily put the elements of the
cross product into the array by stepping the index for each dimension through
each of the sets.  But this array notion also gives a unique index to every
element of the cross product.  This function works as if we created such an
array and then did an `array-row-major-aref' into it."
  ;; The code looks just like `decode-row-major-index'
  (let ((remainder (mod n (reduce #'* sets :key #'length)))
	index-in-this-dimension)
    (append
     (loop repeat (- (length sets) 1)
           for d from 0 do
           (multiple-value-setq (index-in-this-dimension remainder)
	     (truncate remainder (reduce #'* sets :start (1+ d) :key #'length)))
           collect (elt (elt sets d) index-in-this-dimension))
     (list (elt (elt sets (- (length sets) 1)) remainder)))))

(defun nth-elt-of-cross-product-as-multiple-values (n &rest sets)
  "Returns list of values of `nth-elt-of-cross-product' as multiple values.
See it for documentation."
  (values-list (apply #'nth-elt-of-cross-product n sets)))

#|
(defun test-nth-elt-of-cross-product ()
  (dotimes (i 27)
    (print (nth-elt-of-cross-product i '(a b c) '(1 2 3) '(x y z))))
  #+Explorer
  (timeit (:cpu :repeat 100)
    (nth-elt-of-cross-product (random 27) '(a b c) '(1 2 3) '(x y z))))
|#
;;;----------------------------------------------------------------------------

(defclass script-element (named-object-mixin remember-instances)
  ((time     :initarg :time)
   (interval :initarg :interval)
   (code     :initarg :code))
  (:metaclass named-class))

(defun find-script-element (script-element-spec &optional no-error-p)
  "Given a experiment specifier this returns the experiment instance. `no-error-p' if non-nil says to
return nil if there is no such experiment, instead of signalling an error."
  (find-instance-by-name script-element-spec 'script-element no-error-p))

(defmacro define-script-element (name (time &key interval) &rest body &aux documentation)
  (declare (arglist name (time &key interval) [documentation] &rest body))
  (setf documentation
	(if (stringp (car body)) (pop body) "No documentation supplied."))
  `(progn
     (make-instance 'script-element
                    :name ,(string name)
                    :description ,(string documentation)
                    :time (parse-time-specifier ,time)
                    :interval ,(when interval `(parse-time-specifier ,interval :interval-p t))
                    :code (compile
                            ',(el::form-symbol "%" name "-SCRIPT-ELEMENT-FUNCTION")
                            '(lambda nil ,@body)))
     ',name))

;;; ============================================================================
;;; Error in usage of schedule-function.  Sometimes it's called with 3 or 4
;;; positional arguments, others with keyword args.  I'm going with positional
;;; args.

(defmethod schedule ((the-script-element script-element) &optional ignore)
  (declare (ignore ignore))
  (with-slots (name time interval code) the-script-element
    (if interval
	(schedule-function code time interval name)
	(schedule-function code time NIL name))))

;;;----------------------------------------------------------------------------

(defvar *collecting-simple-instrumentation-ok* nil)

(defmethod update-value (new-value (the-instrumentation simple-instrumentation) arguments combiner)
  (declare (ignore arguments))
  (unless  *collecting-simple-instrumentation-ok*
    (error "should not be called"))
  (with-slots (value (default-combiner combiner)) the-instrumentation
    (setf-lookup-equal value NIL
                       (funcall (or combiner default-combiner)
                                (lookup-equal value NIL)
                                new-value))))

(defmethod update-number-of-samples ((the-instrumentation simple-instrumentation) arguments)
  (declare (ignore arguments))
  (unless *collecting-simple-instrumentation-ok*
    (error "should not be called"))
  (let ((*use-new-lookup?* nil))
    (update-number-of-samples-internal the-instrumentation NIL)))

(defmethod collect-internal :around ((the-instrumentation simple-instrumentation) (combiner t)
                                     &rest arguments)
  (declare (ignore arguments))
  (when *collecting-simple-instrumentation-ok*
    (call-next-method))
  )

(defmethod report-internal :around ((the-instrumentation simple-instrumentation) stream 
                                    (extracter t) &rest arguments)
  (declare (ignore arguments))
  (let ((*collecting-simple-instrumentation-ok* t))
    (call-next-method))
  )

(defmethod lookup-number-of-samples ((the-instrumentation simple-instrumentation) arguments)
  (declare (ignore arguments))
  (lookup-equal (slot-value the-instrumentation 'number-of-samples) NIL))

(defmethod lookup-value ((the-instrumentation simple-instrumentation) arguments)
  (declare (ignore arguments))
  (lookup-equal (slot-value the-instrumentation 'value) NIL))

(defmethod standard-report-function ((the-instrumentation simple-instrumentation)
				   stream extracter &rest args)
  (declare (ignore extracter args))
  (with-slots (value number-of-samples) the-instrumentation 
    (assert (= number-of-samples 1))
    (standard-value-printer (first value) stream)
    (write-char *data-separator-character* stream))
  (values))

(defun system-timestamp ()
  (assert *current-experiment* () "there is no experiment currently running")
  (let ((function (slot-value *current-experiment* 'timestamp-function)))
    (if function
      (funcall function)
      (error "~a does not have a timestamp function defined" *current-experiment*))))

;;;----------------------------------------------------------------------------

;; A simple example that does nothing
#|
(define-experiment test-experiment ()
  "This is the test experiment documentation-string."
  :start-system (ccl::beep)
  :before-experiment (progn
                       (setf *screen-saver-time-delay*                         nil
                             *reset-random-state-during-scenario-initialization?* nil))
  :before-trial      (format t "Before Trial #~d" (trial-number))
  :after-trial       (format t "After Trial #~d" (trial-number))
  :after-experiment finish-it ; just to show an alternative style
  :instrumentation (trial-number))


(defun finish-it ()
  (print "Finishing up"))
|#

;;;----------------------------------------------------------------------------

;(DEFVAR LOGOUT-LIST NIL "List of forms to evaluate on logout, to undo effects of init file.")

;(MAPC 'GLOBAL:EVAL LOGOUT-LIST)

;(DEFMACRO LOGIN-FORMS (&BODY FORMS)
;  "Execute FORMS, arranging to undo them at logout."
;  `(UNDOABLE-FORMS-1 'LOGOUT-LIST ',FORMS "at logout"))

;(DEFUN UNDOABLE-FORMS-1 (UNDO-LIST-NAME FORMS &OPTIONAL (COMPLAINT-STRING ""))
;  (DOLIST (FORM FORMS)
;    (LET ((U (UNDOABLE-EVAL FORM)))
;      (IF (EQ U T)
;	  (FORMAT T "~&[A ~S form is supposed to be undone ~A~% but this is not implemented.
;The form's effects will be permanent.]~%"
;		  (CAR FORM)
;		  COMPLAINT-STRING)
;	(AND U (PUSH U (SYMBOL-VALUE UNDO-LIST-NAME)))))))


;;; ***************************************************************************
;;; EOF


