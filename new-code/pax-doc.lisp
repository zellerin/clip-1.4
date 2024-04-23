(in-package clip-user)



;;;; Document clips
(defun dref-to-clip (dref)
  (clip::find-instrumentation (dref-ext:dref-name dref) t))

(dref-ext:define-locative-type clip ()
  "An instrumentation (clip, alligator clip)")

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
  (slot-value (dref-to-clip clip) 'documentation))

(defmethod dref-ext:arglist* ((clip clip-dref))
  (clip::instr.arguments (dref-to-clip clip)))

;;;; Document experiments
(defun dref-to-experiment (dref)
  (clip::find-experiment (dref-ext:dref-name dref) t))

(dref-ext:define-locative-type experiment ()
  "An instrumentation (experiment, alligator experiment)")

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
