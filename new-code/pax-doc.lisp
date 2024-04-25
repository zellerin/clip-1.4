(mgl-pax:define-package #:clip/doc
  (:use #:cl #:clip #:mgl-pax)
  (:documentation "Additional locatives for clip components."))

(in-package #:clip/doc)


(defsection clip::@index
    (:Title "MGL-PAX documentation for clips")
  (@concepts section)
  (@api section)
  (@loader section)
  (@clip-classes section)
  (@locatives section)
  (@demos section)
  (@background section)
#+nil  (@extended-lisp section))

(defsection @concepts
    (:title "Motivation and concept")
  "From the original manual:"
  "We collect information from software systems for many reasons. Sometimes, the
very purpose of the system is to produce information. Other times, we collect
data for debugging, feedback to the user - in general, for understanding the
system's behavior. Unfortunately, there seem to be few general tools available
for this task. Instead, we often find ad hoc, domain-specific code, spread
throughout a system, varying from component to component, despite strong
similarity in the requirements for any data collection code.

Clip, the Common Lisp Instrumentation Package, standardizes data collection. It
is named by analogy with the \"alligator clips\" that connect diagnostic
equipment to electrical components. By standardizing data collection, we can
more easily add and delete instrumentation from our systems. We can reuse a
piece of data collection code, called a CLIP in other systems we are
studying. We can use general tools to analyze the collected data.
Instrumentation in a system becomes more easily used and understood, because the
basic functionality of the system is separated from the functionality of the
instrumentation.  We designed Clip to be used for scientific experiments, in
which a system is run under a number of different conditions to see whether and
how the behavior changes as a function of the condition. Consequently, we view
an experiment as a series of trials, varying the settings of experiment
variables and collecting information. Generally speaking, an experiment
comprises the following steps:

1. Creating clips and inserting them into the system to be studied. Generally,
clip measures one particular aspect of a system, but you can also define clips
that combine many measurements, including other clips. Often, the clips you need
will already be defined.

2. Define the experiment, in terms of what system is to be run, any necessary
initialization code, and the conditions under which it is to run (different
input parameters or environmental conditions).

3. Run a series of trials, saving the output into a Clasp format data file. This format
is described in the Clasp manual section Data Manipulation Functions, although it
isn't necessary to understand the file format. Clasp can read what Clip writes.

Clip can also write data files in standard tab or space delimited format used by
most other statistical packages, databases and spreadsheets."

  "Instead of CLASP, CLIP/LOADER::LOAD-MEASUREMENTS can be used to load the data as lisp-utils frames."
  (clip/system glossary-term)
  (clip glossary-term))

(define-glossary-term clip/system
    (:title "CLIP (Common Lisp Instrumentation Package)")
    "A tool for automating experimentation and
data collection. Clip was originally designed to be used in combination with the
Common Lisp Analytical Statistics Package (CLASP), though it can be used as a
standalone instrumentation package for Common Lisp applications.

The original 1.4 version was released in Jan 1994. This version was modified to
run on Common Lisp systems in 2023.

Clasp is no longer easy to obtain and run in 2023, so a loader was also added.")

(define-glossary-term clip
    (:title "CLIP")
    "There are basically only a small number of ways to instrument a software
system. These are: adding special purpose code to collect data, interrogating
global (usually objects with state) data structures or using advice.

The first way is to build in special purpose code to collect data about the
state of the system while the system is running. We used this technique in the
Phoenix testbed to keep information about the execution time of timeline entries
and also message traffic patterns.  The Transsim simulator also uses this
technique when it keeps track of daily costs, ship locations, demon ring
intervals. The key point here is that the only reason for adding the piece of
code was to allow an experimenter to analyze the behavior of the system. The
simulation itself does not use the information. This method increases the
complexity and reduces the readability and maintainability of the software
system. In code that is highly instrumented it is often difficult to determine
what is intrinsic to the running of the system and what is used to instrument.

Another method involves interrogating global data structures to determine the
state post hoc. Examples include most everything we collected about
Phoenix (i.e., fireline built, first plan tried, bulldozer digging times,
etc.). This technique is fine if the information is hanging around after you
have finished running the simulation, but this is not always the case. Also,
there is some information that is time-sensitive, and therefore must be
collected on a periodic or event-driven basis. Collecting this type of
information often involves resorting to the first method -- altering the code
itself.

Alternatively, one can use the advise facility available in many lisp
environments to non- intrusively collect information when functions are
called. This is a nice modular approach, but requires a good deal of knowledge
about the advise facility and the software system itself. Unfortunately, the
advise facility is not standardized across Lisp implementations.  The DEFCLIP
macro encapsulates the code necessary to instrument a software system and
separates it from the system itself. It avoids the pitfalls of adding special
purpose code directly to the software system while at the same time allowing
periodic and event-driven data collection. It also allows collection to be done
by perusing global data structures.")

(define-glossary-term clasp/format
    (:title "CLASP format")
    "Format of the data used by clasp and documented in CLASP manual.")

(define-glossary-term clasp
    (:title "CLASP")
    "Common Lisp Analytical Statistics Package - another tool of same origin as CLIP.")

(defsection @api
    (:title "Documented interface")
  (clip package)
  (defclip macro)
  (define-simulator macro)
  (define-experiment macro)
  (run-experiment function)
  (write-current-experiment-data function)
  (clip::collect function)
  (clip-1994 asdf:system))

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
  (simulator locative)
  (clip-1994/doc asdf:system))

(defsection @loader
    (:title "Loading CLASP format")
  (clip/loader package)
  (clip/loader::load-measurements function)
  (clip/loader::load-clasp-frame function)
  (clip-1994/loader asdf:system))

(defsection @demos
    (:title "Demos")
  (clip-1994/demo asdf:system)
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

(mgl-pax:defsection @background
    (:title "Background")
  (clasp glossary-term)
  (clasp/format glossary-term)
  (phoenix glossary-term))

(define-glossary-term phoenix
    (:title "Phoenix system")
    "Phoenix was a multi-agent planning system that fights simulated
forest-fires. The simulation used terrain, elevation, and feature data from
Yellowstone.

It is used for the demos. See original manual for more specifics.")

(defsection @extended-lisp
    (:title "Lisp extension")
  (extended-lisp package)
  (clip::named-class class)
  (clip::basic-class class))


(in-package #:clip-user)
(mgl-pax:defsection @simple-agent-experiment
    (:title "Documentation of the simple agent experiment")
  "This example collects trial summary data about overall agent-cost and task
completion-time. Collection occurs at the end of each trial and is written out
to a summary file in Clasp format."
  (agent-sim-1 clip/doc:simulator)
  (agents-cost clip/doc:clip)
  (completion-time clip/doc:clip)
  (simple-agent-experiment-1 clip/doc:experiment))

(mgl-pax:defsection @agent-experiment
    (:title "Documentation of the agent experiment")
  "This example collects trial summary data about overall agent-cost and task
completion-time. Collection occurs at the end of each trial and is written out
to a summary file in Clasp format."
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
