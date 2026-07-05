;;;; -*- Mode:Common-Lisp; Package:EXTENDED-LISP; Base:10; Syntax:COMMON-LISP; Fonts:(MEDFNT) -*-
;;;; *-* File: Titanic: /usr/users/eksl/systems/extended-lisp/lisp-extensions.lisp *-*
;;;; *-* Last-edit: Thursday, September 23, 1993  19:59:53; Edited-By: Westy *-*
;;;; *-* Edited-By: Westy *-*
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *                  Standard EKSL Extended Lisp
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Kevin Gallagher, Philip Johnson, Daniel Corkill, Kelly Murray,
;;;             David Westbrook, Marty Humphrey, Mike Greenberg, Scott Anderson

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
;;;  03-01-90 File Created.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :extended-lisp)

;;; --*--
;;; ***************************************************************************


#-Explorer
(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro once-only (variable-list &body body)
  "Generate code that evaluates certain expressions only once.
This is used in macros, for computing expansions.
VARIABLE-LIST is a list of symbols, whose values are subexpressions
to be substituted into a larger expression.  BODY is what uses those
symbols' values and constructs the larger expression.

ONCE-ONLY modifies BODY so that it constructs a different expression,
which when run will evaluate the subsexpressions only once, save the
values in temporary variables, and use those from then on.
Example:
\(DEFMACRO DOUBLE (ARG) `(+ ,ARG ,ARG)) expands into code that computes ARG twice.
\(DEFMACRO DOUBLE (ARG) (ONCE-ONLY (ARG) `(+ ,ARG ,ARG))) will not."

  (dolist (variable variable-list)
    (if (not (symbolp variable))
	(error "~S is not a variable" variable)))
  (let ((bind-vars (gensym))
	(bind-vals (gensym))
	(tem (gensym)))
    `(let ((,bind-vars nil)
	   (,bind-vals nil))
       (let ((result ((lambda ,variable-list . ,body)
		      . ,(loop for variable in variable-list
			       collect `(if (let ((variable ,variable))
					      (loop
						(when (atom variable) (return t))
						(when (or (eq (car variable) 'quote)
							  (eq (car variable) 'function))
						  (return t))
						(if  (eq (car variable) 'the)
						     (setf variable (cadr (cdr variable)))
						     (return nil))))
					    ,variable
					    (let ((,tem (gensym)))
					      (push ,tem ,bind-vars)
					      (push ,variable ,bind-vals)
					      ,tem))))))
	 (if (null ,bind-vars)
	     result
	     `((lambda
		,(nreverse ,bind-vars) ,result) . ,(nreverse ,bind-vals))))))))

(defmacro NYI (&optional function-string)
  `(error "~@[~a ~]not yet written for ~A ~A on ~A."
          ,function-string
          (lisp-implementation-type) (lisp-implementation-version) (machine-type)))

;;;----------------------------------------------------------------------------

;; Much better than doing (= (length x) 1).
(defun length-1-list-p (x)
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

;;;----------------------------------------------------------------------------

(defun mapappend (fun &rest args)
  (if (some #'null args)
      ()
      (append (apply fun (mapcar #'car args))
	      (apply #'mapappend  fun (mapcar #'cdr args)))))

;;;----------------------------------------------------------------------------

(defmacro find-all-if (predicate sequence &key key)
  `(mapcan #'(lambda (item)
               (when (funcall ,predicate ,(if key `(funcall ,key item) `item))
                 (list item)))
           ,sequence))

(defmacro find-all-if-not (predicate sequence &key key)
  `(mapcan #'(lambda (item)
               (unless (funcall ,predicate ,(if key `(funcall ,key item) `item))
                 (list item)))
           ,sequence))

;;;----------------------------------------------------------------------------

(defmacro pushnew-ordered (value place lessp-predicate
                            &rest nordered-adjoin-keywords)
  "Like PUSHNEW, except it calls nordered-adjoin instead of adjoin.
Arguments are passed through to nordered-adjoin."
  `(setf ,place (nordered-adjoin ,value ,place ,lessp-predicate
                                 ,@nordered-adjoin-keywords)))

;;;----------------------------------------------------------------------------

#-Explorer
(defmacro push-end (value place)
  "Like PUSH, except that the value goes on the end of the PLACE list.
If PLACE is (), then (value) is returned."
  (let ((place-to-setf place))
    (once-only (place value)
      `(if ,place
           (progn
             (setf (cdr (last ,place)) (cons ,value nil))
             ,place)
           (setf ,place-to-setf (cons ,value nil))))))

(defmacro pushnew-end (value place &rest test-keywords)
  "Like PUSHNEW, except that the value goes on the end of the PLACE list.
If PLACE is (), then (value) is returned.  The normal test keywords are
allowed: :TEST :TEST-NOT and :KEY, as in PUSHNEW."
  (let ((place-to-setf place))
    (once-only (place value)
      `(if ,place
           (progn
             (or (member ,value ,place ,@test-keywords)
                 (setf (cdr (last ,place)) (cons ,value nil)))
             ,place)
           (setf ,place-to-setf (cons ,value nil))))))

;;;----------------------------------------------------------------------------

(defmacro REPLACE-OR-PUSH (new-item sequence &key key (test '#'eq))
  "NEW-ITEM is used to replace any (all) elements it matches.
If there are no matches then NEW-ITEM is PUSH'ed on the front of SEQUENCE.
SEQUENCE can be a list or an array.
TEST is a function of two args to use to compare NEW-ITEM against an element (or key).
 An element matches when the TEST function returns non-NIL. TEST must be non-nil
 and defaults to 'EQ.
KEY, if non-NIL, is a function to be applied to each element to get a key,
 which is passed to TEST.  If KEY is NIL, the element itself is used.
This macro modifies SEQUENCE (like PUSH)."
  (once-only (new-item)
  `(if (zerop (count ,(if key `(funcall ,key ,new-item) new-item)
                         ,sequence :test ,test ,@(if key `(:key ,key))))
     (push ,new-item ,sequence)
     (setf ,sequence
           ,(if key
              `(nsubstitute-if ,new-item #'(lambda (old-item)
                                             (funcall ,test (funcall ,key ,new-item) old-item))
                               ,sequence :key ,key)
              `(nsubstitute-if ,new-item #'(lambda (old-item)
                                             (funcall ,test ,new-item old-item))
                               ,sequence))))))

;; We should add this COUNT stuff... (Doc line follows)
;; If COUNT is non-NIL, it is the number of such elements to replace.
(defmacro REPLACE-OR-PUSH-END (new-item sequence &key key (test ''eq))
  "NEW-ITEM is used to replace any (all) elements it matches.
If there are no matches then NEW-ITEM is placed on the end of SEQUENCE.
SEQUENCE can be a list or an array.
TEST is a function of two args to use to compare NEW-ITEM against an element (or key).
 An element matches when the TEST function returns non-NIL. TEST must be non-nil
 and defaults to 'EQ.
KEY, if non-NIL, is a function to be applied to each element to get a key,
 which is passed to TEST.  If KEY is NIL, the element itself is used.
This macro modifies SEQUENCE (like PUSH-END)."
  (once-only (new-item)
  `(if (zerop (count ,(if key `(funcall ,key ,new-item) new-item)
                         ,sequence :test ,test ,@(if key `(:key ,key))))
         (push-end ,new-item ,sequence)
         ;; else, replace the little bugger...
         (setf ,sequence
               ,(if key
                  `(nsubstitute-if ,new-item #'(lambda (old-item)
                                                 (funcall ,test (funcall ,key ,new-item) old-item))
                                   ,sequence :key ,key)
                  `(nsubstitute-if ,new-item #'(lambda (old-item)
                                                 (funcall ,test ,new-item old-item))
                                   ,sequence))))))


;;;; --------------------------------------------------------------------------
;;;;   Association List Functions
;;;; --------------------------------------------------------------------------

(define-modify-macro PUSH-ACONS (key datum)
                     (lambda (place key datum)
                       (acons key datum place))

  "PUSH-ACONS place key datum

Pushes an ACONS of key and datum onto the place alist (whether or not
a matching key exists in the place alist.  Returns the updated alist.")


(define-modify-macro PUSHNEW-ACONS (key datum &REST keys)
                     (lambda (place key datum &REST keys)
                       (let ((assoc-result (apply 'assoc key place keys)))
                         (cond (assoc-result
                                   (setf (rest assoc-result) datum)
                                   place)
                               (t (push-acons place key datum)))))

  "PUSHNEW-ACONS place key datum  &KEYS :TEST :TEST-NOT

Performs an PUSH-ACONS of place, key, and datum only if ASSOC of
key and place returns NIL.  Otherwise, datum replaces the old
datum of key.  In either case, PUSHNEW-ACONS returns the modified
alist.")

;;;----------------------------------------------------------------------------

#-Explorer
(defun circular-list (element)
  "Return a circular list of one element"
  (let ((cell (cons element nil)))
    (setf (cdr cell) cell)
    cell))

#+Explorer
(import 'ticl::circular-list)

;;;----------------------------------------------------------------------------

(defmacro docount ((count &optional (result nil resultp)) &body body)

  "DOCOUNT (countform [resultform]) {declaration}* {tag | statement}*

This macro provides a simple iteration over a sequence of integers. First the
countform is evaluated to produce an integer.  The body is then executed
countform times.  Finally, the resultform is evaluated and returned (defaulting
to NIL).  The entire construct is surrounded by an implicit block
named NIL, so that RETURN may be used to exit the loop at any point.

The internal counter can be declared using the THE special form in
countform."

  (let ((var-sym (gensym)))
    `(dotimes (,var-sym ,count ,@(when resultp `(,result)))
       ,@(when (and (consp count)
                    (eq (first count) 'the))
          `((declare (,(second count) ,var-sym))))
       ,@body)))

(defmacro doplist ((key value plist) &body body)
  "Iterate over each key/value pair in a plist.  Key and Value are
bound to successive pairs in Plist."
  (let ((atvar (gensym "PLIST")))
    `(do* ((,atvar ,plist (cddr ,atvar))
	   (,key (car ,atvar) (car ,atvar))
	   (,value (second ,atvar) (second ,atvar)))
	  ((null ,atvar) nil)
       ,@body)))

(defmacro doalist ((key value alist) &body body)
	  "Macro to iterate over an a-list:  Elements in the alist are bound
to key and value"
  (let ((atvar (gensym)))
    `(dolist (,atvar ,alist)
       (let ((,key (first ,atvar))
	     (,value (rest ,atvar)))
	     ,@body))))

(defmacro dolist-or-atom ((var possible-list &optional test-form) &body body)

  "DOLIST-OR-ATOM (var possible-list &optional test-form) &body body

   This macro handles those cases where a variable may be a single object or
   a list of objects.  If `Possible-List' is a list then DOLIST-OR-ATOM will
   act like DOLIST; if `Possible-List' is not a list then this will act like
   LET.  The optional `Test-Form' is a form that should return true if you
   want the DOLIST behavior.  It defaults to the form (LISTP Possible-List).

   Note that `Possible-List' may be evaluated twice so it should not be a
   form with side-effects or one that is expensive to compute."

  ;; This could be coded as follows but if body is large the
  ;; space penalty is significant.
  ;;
  ;; (if ,test-form
  ;;     (dolist (,var ,possilble-list) ,@body)
  ;;     (let ((,var ,possible-list)) ,@body))

  (let ((test-result (gensym))
	(remaining (gensym)))
    (when (null test-form)
      (setf test-form `(listp ,possible-list)))
    `(do* ((,test-result ,test-form)
	   (,remaining ,possible-list (cdr ,remaining))
	   (,var (if ,test-result (first ,remaining) ,remaining)
	         (first ,remaining)))
	  ((when ,test-result
	     (endp ,remaining)))
       ,@body
       (unless ,test-result (return nil)))))

;;;; --------------------------------------------------------------------------
;;;;   Functions used in the following macros
;;;; --------------------------------------------------------------------------

;;; Sometimes it's nice to have your gensyms mean something when
;;; you're reading the macroexpansion of some form.  The problem
;;; is that if you give a prefix to GENSYM it remains the prefix
;;; until you change it.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *newsym-counter* 0
  "Counter used by NEWSYM for generating print names.")

(defun newsym (&optional (prefix "X"))
  "Create a new uninterned symbol whose print name begins with PREFIX.
   This differs from GENSYM in that the prefix is not sticky."
  (unless (stringp prefix)
    (setf prefix (string prefix)))
  (make-symbol (format nil "~a~4,'0d" prefix (incf *newsym-counter*))))

) ;; End of Eval-When


(defmacro with-variables (symbols &body body)
  "Using gensyms/gentemps is necessary to prevent variables produced by macro expansions from interfering
with user variables, and naming them mnemonically helps make macro expansions and compiled code easier to
read, but it's a pain to create them properly.  This macro creates them for you, which makes writing nice
macros easier.  For example, if you are writing a macro to iterate over an array, you used to have to write:

 (defmacro do-2d-array ((elt array) &body body)
   (let ((row (gentemp \"ROW\"))
         (col (gentemp \"COL\")))
     `(dotimes (,row (array-dimension 0))
        (dotimes (,col ,(array-dimension 1))
           (let ((,elt (aref ,array ,row ,col)))
               . ,body)))))

Now you can just write the following, which eliminates the need to laboriously create the mnemonic
gensyms/gentemps.

 (defmacro do-2d-array ((elt array) &body body)
   (with-variables (row col)
      `(dotimes (,row ,(array-dimension 0))
          (dotimes (,col ,(array-dimension 1))
             (let ((,elt (aref ,array ,row ,col)))
                 . ,body))))
"
  `(let ,(mapcar #'(lambda (sym)
		     `(,sym (newsym ,(symbol-name sym))))
		 symbols)
     . ,body))


;;;; --------------------------------------------------------------------------
;;;;   Creating symbols
;;;; --------------------------------------------------------------------------

(defmacro form-symbol-in-package (pkg &rest names)
  "FORM-SYMBOL-IN-PACKAGE pkg &rest names
   Return a symbol interned in PKG whose print name is the concatenation
   of NAMES.  Each name must be acceptable to the string function."
  `(intern (concatenate
	     'simple-string
	     ,@(mapcar #'(lambda (name)
			   (if (stringp name) name `(string ,name)))
		       names))
	   ,pkg))

(defmacro form-symbol (&rest names)
  "FORM-SYMBOL &rest names
   Return a symbol interned in the current package whose print name is the
   concatenation of NAMES.  Each name must be acceptable to the string function."
  `(form-symbol-in-package *package* ,@names))

(defmacro form-gbb-symbol (&rest names)
  "FORM-GBB-SYMBOL &rest names
   Return a symbol interned in the GBB package whose print name is the
   concatenation of NAMES.  Each name must be acceptable to the string function."
  `(form-symbol-in-package "GBB" ,@names))

(defmacro form-keyword (&rest names)
  "FORM-KEYWORD &rest names
   Return a symbol interned in the keyword package whose print name is the
   concatenation of NAMES.  Each name must be acceptable to the string function."
  `(form-symbol-in-package "KEYWORD" ,@names))

(defmacro form-uninterned-symbol (&rest names)
  "FORM-UNINTERNED-SYMBOL &rest names
   Return an uninterned symbol whose print name is the concatenation of NAMES.
   Each name must be acceptable to the string function."
  `(make-symbol (concatenate
		  'simple-string
		  ,@(mapcar #'(lambda (name)
				(if (stringp name) name `(string ,name)))
			    names))))
;;;----------------------------------------------------------------------------

(defmacro with-conditional-open-file ((stream filename &rest options)
                                         &body body)

  "A slightly modified version of the Common Lisp standard.

Executes `form's with the all `stream' variables bound to a stream for the
corresponding file `filename'.  `Filename' is opened using `options', which
are the same as for the OPEN function.  If `filename' is nil, the OPEN is
bypassed and nil is bound to `stream'."

  (once-only (filename)
    (let ((close? (newsym 'close?)))
      `(let* ((,close? nil)
	      (,stream (cond
			 ;; Nil simply return nil ::
			 ((null ,filename) nil)
			 ;; Streams return themselves ::
			 ((streamp ,filename) ,filename)
			 ;; Anything else attempts an open ::
			 (t (setf ,close? t)
			    (open ,filename ,@options)))))
	 (unwind-protect
	     (progn ,@body)
	   (when (and ,close? ,stream)
	     (close ,stream)))))))

(defmacro with-conditional-open-files (streams &body body)

  "A handy version of WITH-CONDITIONAL-OPEN-FILE for nested opens.

Executes `form's with the all `stream' variables bound to a stream for the
corresponding file `filename'.  `Filename' is opened using `options', which
are the same as for the OPEN function.  If `filename' is nil, the OPEN is
bypassed and nil is bound to `stream'."

  (cond ((endp streams) `(progn ,@body))
        (t `(with-conditional-open-file ,(first streams)
              (with-conditional-open-files ,(rest streams) ,@body)))))

;;;----------------------------------------------------------------------------

;;; WITH-KEYWORDS-BOUND:
;;;
;;; (with-keywords-bound (((key1 default1)
;;;                        (key2 default2)
;;;                        ...)
;;;                       arglist)
;;;   <body>)

(defmacro with-keywords-bound ((key-specs arg-list &rest args)
			       &body body)

  "WITH-KEYWORDS-BOUND (key-specs arg-list [error-msg | keyvar] &rest otherwise)
                 &body body)

   Look through ARG-LIST for keywords and bind them, similar to the
   processing of lambda-list keywords.  ARG-LIST is evaluated and should
   be a list of keywords and values.  KEY-SPECS describes the keywords
   to check for.  Each element should be a symbol that will be bound to
   the keyword value from ARG-LIST or a list of the symbol and a default
   value.

   If the third argument is a string it is used as the format string for
   the error message when an unknown keyword is found.

   If the third argument is a symbol then it is used as the variable
   which is used internally to hold the remaining part of the list during
   processing.  OTHERWISE is code that will be executed (in an implicit
   progn) if any keyword from ARG-LIST is not a recognized keyword.
   This code can refer to KEYVAR."

  (let* ((key-symbols (mapcar #'(lambda (key)
                                  (cond ((symbolp key) key)
                                        ((symbolp (first key)) (first key))
                                        ((symbolp (first (first key))) (first (first key)))
                                        (t (error "ill-formed key spec; ~a" key)))
                                  #+OLD
				  (if (symbolp key) key (first key))
                                  )
			      key-specs))
	 (error-msg (if (stringp (first args)) (first args) nil))
	 (keyvar (if (and (symbolp (first args))
			  (not (null (first args))))
		     (first args)
		     (newsym "KEY")))
	 (otherwise (if error-msg nil (cdr args)))
         (key-spec-bindings
           (mapcar #'(lambda (key-spec)
                       (cond ((symbolp key-spec) key-spec)
                             ((symbolp (first key-spec)) `(,(first key-spec) ,(second key-spec)))
                             ((symbolp (first (first key-spec))) `(,(first (first key-spec))
                                                               ,(second key-spec)))
                             (t (error "ill-formed key spec; ~a" key-spec)))
                       #+OLD
                       (if (consp key-spec)
                         `(,(first key-spec) ,(second key-spec))
                         key-spec)
                       )
                   key-specs))
         (supplied-p-bindings
           (mapcan #'(lambda (key-spec)
                       (when (and (consp key-spec) (third key-spec))
                         `((,(third key-spec) nil))))
                   key-specs))
         (all-bindings (append key-spec-bindings supplied-p-bindings))
         )
    (or otherwise
	error-msg
	(setf error-msg (format nil "~~s is not one of (~{:~a~^ ~})."
				key-symbols)))
    `(let ,all-bindings
       (when (consp ,arg-list)
	 (do ((,keyvar ,arg-list (cdr ,keyvar)))
	     ((null ,keyvar))
	   (case (first ,keyvar)
	     ,@(mapcar #'(lambda (key-spec)
                           (multiple-value-bind
                             (variable key)
                             (cond ((symbolp key-spec)
                                    (values key-spec
                                            (form-keyword key-spec)))
                                   ((symbolp (first key-spec))
                                    (values (first key-spec)
                                            (form-keyword (first key-spec))))
                                   ((every #'keywordp (rest (first key-spec)))
                                    (values (first (first key-spec))
                                            (mapcar #'(lambda (k) (form-keyword k))
                                                    (rest (first key-spec))))))
                             #+OLD
                             (if (symbolp key-spec) key-spec (first key-spec))
                             (let ((supplied-p-symbol (when (consp key-spec) (third key-spec))))
                               `(,key
                                 (setf ,variable (first (setf ,keyvar (cdr ,keyvar))))
                                 ,(when supplied-p-symbol
                                    `(setf ,supplied-p-symbol t))))))
                       key-specs)
             (otherwise
              ,@(or otherwise
                    `((error ,error-msg (first ,keyvar))))))))
       ,@body)))

;;;----------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;;				  End of File
;;; ---------------------------------------------------------------------------
