(mgl-pax:define-package #:clip/doc
  (:use #:cl #:clip #:mgl-pax)
  (:documentation "Additional locatives for clip components."))

(in-package #:clip/doc)


(defsection clip::@index
    (:Title "MGL-PAX documentation for clips")
  (@api section)
  (@clip-classes section)
  (@locatives section)
  (@demos section))

(defsection @api
    (:title "Documented interface")
  (defclip macro)
  (define-simulator macro)
  (define-experiment macro)
  (run-experiment function)
  (write-current-experiment-data function)
  (clip::collect function))

(defsection @clip-classes
    (:title "Clip classes")
  "This is for a reference from clips."
  (clip::instrumentation class)
  (clip::composite-instrumentation class)
  (clip::super-instrumentation class)
  (clip::periodic-super-instrumentation class)
  (clip::functional-composite-instrumentation class)
  (CLIP::COMPOSITE-CHILD-OF-COMPOSITE-INSTRUMENTATION class)
  (CLIP::COMPOSITE-TIME-SERIES-INSTRUMENTATION class)
  (CLIP::FUNCTIONAL-MAPPING-INSTRUMENTATION class))

(defsection @locatives
    (:title "New locatives")
  (clip/doc package)
  (clip locative)
  (experiment locative)
  (simulator locative))

(defsection @demos
    (:title "Demos")
  (clip-user::@simple-agent-experiment section)
  (clip-user::@agent-experiment section)
  (clip-user::@super-agent-experiment section))

;;;; Document clips
(defun dref-to-clip (dref)
  (clip::find-instrumentation (dref-ext:dref-name dref) t))

(dref-ext:define-locative-type clip ()
  "An instrumentation (clip, alligator clip). Documentation is taken from the docstring in the DEFCLIP.")

(dref-ext:define-definition-class clip clip-dref)

(defmethod dref-ext:locate* ((object clip::instrumentation))
  (make-instance 'clip-dref :name (clip::name object) :locative 'clip))

(defmethod dref-ext:dref* (symbol (locative-type (eql 'clip)) locative-args)
  (unless (and (symbolp symbol)
               (clip::find-instrumentation symbol t))
    (dref-ext:locate-error "~S does not name a clip" symbol))
  (make-instance 'clip-dref :name symbol :locative 'clip))

(defmethod dref-ext:resolve* ((dref clip-dref))
  (dref-to-clip dref))

(defmethod dref-ext:docstring* ((clip clip-dref))
  (format nil "~a~2%Class: ~s"
          (slot-value (dref-to-clip clip) 'documentation)
          (class-name (class-of (dref-to-clip clip)))))

(defmethod dref-ext:arglist* ((clip clip-dref))
  (clip::instr.arguments (dref-to-clip clip)))

;;;; Document experiments
(defun dref-to-experiment (dref)
  (clip::find-experiment (dref-ext:dref-name dref) t))

(dref-ext:define-locative-type experiment ()
  "An experiment. Documentation is taken from :DESCRIPTION of the DEFINE-EXPERIMENT.")

(dref-ext:define-definition-class experiment experiment-dref)

(defmethod dref-ext:locate* ((object clip::experiment))
  (make-instance 'experiment-dref :name (clip::name object) :locative 'experiment))

(defmethod dref-ext:dref* (symbol (locative-type (eql 'experiment)) locative-args)
  (unless (and (symbolp symbol)
               (clip::find-experiment symbol t))
    (dref-ext:locate-error "~S does not name a experiment" symbol))
  (make-instance 'experiment-dref :name symbol :locative 'experiment))

(defmethod dref-ext:resolve* ((dref experiment-dref))
  (dref-to-experiment dref))

(defmethod dref-ext:docstring* ((experiment experiment-dref))
  (format nil "~a~2%Instrumentation: ~s"
          (slot-value (dref-to-experiment experiment) 'clip::description)
          (slot-value (dref-to-experiment experiment) 'clip::instrumentation-names)))

(defmethod dref-ext:arglist* ((experiment experiment-dref))
  (slot-value  (dref-to-experiment experiment) 'clip::arguments))

;;;; Document simulators
(defun dref-to-simulator (dref)
  (clip::simulator (dref-ext:dref-name dref)))

(dref-ext:define-locative-type simulator ()
  "An instrumentation (simulator, alligator simulator)")

(dref-ext:define-definition-class simulator simulator-dref)

(defmethod dref-ext:locate* ((object clip::simulator))
  (make-instance 'simulator-dref :name (clip::name object) :locative 'simulator))

(defmethod dref-ext:dref* (symbol (locative-type (eql 'simulator)) locative-args)
  (unless (and (symbolp symbol)
               (clip::simulator symbol))
    (dref-ext:locate-error "~S does not name a simulator" symbol))
  (make-instance 'simulator-dref :name symbol :locative 'simulator))

(defmethod dref-ext:resolve* ((dref simulator-dref))
  (dref-to-simulator dref))

(defmethod dref-ext:docstring* ((simulator simulator-dref))
  (slot-value (dref-to-simulator simulator) 'clip::description))

(defmethod dref-ext:arglist* ((simulator simulator-dref))
  nil)


(in-package #:clip-user)
(mgl-pax:defsection @simple-agent-experiment
    (:title "Documentation of the simple agent experiment")
  "This example collects trial summary data about overall agent-cost and task
completion-time. Collection occurs at the end of each trial and is written out
to a summary fole in Clasp format."
  (agent-sim-1 clip/doc:simulator)
  (agents-cost clip/doc:clip)
  (completion-time clip/doc:clip)
  (simple-agent-experiment-1 clip/doc:experiment))

(mgl-pax:defsection @agent-experiment
    (:title "Documentation of the agent experiment")
  "This example collects trial summary data about overall agent-cost and task
completion-time. Collection occurs at the end of each trial and is written out
to a summary fole in Clasp format."
  (agent-sim clip/doc:simulator)
  (highest-agent-state clip/doc:clip)
  (posthoc-agent-state-snapshot clip/doc:clip)
  (each-agent-state-snapshot clip/doc:clip)
  (periodic-agent-state-snapshot clip/doc:clip)
  (change-of-state-pred clip/doc:clip)
  (change-of-state clip/doc:clip)
  (change-of-state-3 clip/doc:clip)
  (wilma clip/doc:clip)
  (betty clip/doc:clip)
  (event-based-agent-state-snapshot clip/doc:clip)
  (self-collection clip/doc:clip)
  (agent-experiment clip/doc:experiment)
  (sae clip/doc:experiment))

(mgl-pax:defsection @super-agent-experiment
    (:title "Documentation of the super agent experiment")
  "This example collects trial summary data about overall agent-cost and task
completion-time. Collection occurs at the end of each trial and is written out
to a summary fole in Clasp format."
  ; (agent-sim clip/doc:simulator)
  (all-agents-costs clip/doc:clip)
  (each-agent-cost clip/doc:clip)
  (simple-agent-experiment-2 clip/doc:experiment))

; (mgl-pax:update-asdf-system-html-docs clip::@index "clip-1994")
