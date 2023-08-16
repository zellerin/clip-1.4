;;;; -*- Mode:Common-Lisp; Package:TIME-PARSER; Base:10 -*-
;;;; *-* Last-edit: Monday, January 25, 1993  12:05:35; Edited-By: Westy *-* 


;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (c)(1)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.

;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151

;;; Copyright (C) 1985-1989 Texas Instruments Incorporated. All rights reserved.

;;; Convert Date To Binary for the Lisp Machine    DLW 9/14/80
;;; Take a description of a time (including a date) in any of many commonly
;;; understood formats, parse it and return it in a machine-usable form.

;;  We want to be able to manipulate times in a manner where
;; we can compute how long one hour and 4 minutes is, and
;; print it out in a variety of formats.

;; add and subtract times, as well as multiply them
  ;; ie.  6 units of one month and one day

;; different calanders (seperate file)
;;  default =  Christian Era   (A. C. E.) (A. D.) (Anno Domina) ?
;;  we can have long dates printed out like that.
;; if we store the UT in long format we can hack BC etc.
;; don't forget about lost days in 1580 or so.

;;;----------------------------------------------------------------------------
;;; 
;;; 12-20-88 DAB Take care of 10:10:20-GMT etc, in decode-time-zone.
;;; 12-13-88 DAB Added support to parse +xxxx and -xxxx timezones.

;;;----------------------------------------------------------------------------

(in-package :TIME-PARSER)

(DEFVAR *TIME-PARSER-PACKAGE* (FIND-PACKAGE "TIME-PARSER"))

;;;----------------------------------------------------------------------------

;;;----------------------------------------------------------------------------

(defun nyi () 
  (cerror "implement it and go on" "NYI"))

#-Explorer
(defvar user-id nil)

;;; ----------------------------------------------------------------------------
;;; Some extensions used in this file.

(defmacro defprop (symbol value property)
  "Make the value of SYMBOL's PROPERTY property be VALUE."
  ;;(putprop symbol value property)	; jlm 4/11/89
  `(progn (setf (get ',symbol ',property) ',value)
          ',symbol))

(defun parse-number (string &optional (from 0) to radix fail-if-not-whole-string)
  "Return a number parsed from the contents of STRING, or a part of it.
FROM and TO specify the part of the string; TO = NIL means the end of it.
RADIX defaults to decimal.
If the string or part doesn't start with a number, NIL is returned.
The second value is the index in STRING of the first non-digit, or NIL if none.
FAIL-IF-NOT-WHOLE-STRING means return NIL and 0 unless the whole string or
specified part can be parsed as a number."
  (let ((real-end  (or to (length string))))
    (multiple-value-bind (num index)
	(parse-integer string :start from :end real-end :radix (or radix 10.)
		       :junk-allowed t )
      (if fail-if-not-whole-string
	  (if  (= index real-end)
	       (values num index)
	       (values nil 0))
	  (values num (and num index))))))

(defun STRING-SEARCH-CHAR (char string &optional (from 0) to)
  "Returns the index in STRING of the first occurrence of CHAR past FROM, or NIL if none.
If TO is non-NIL, the search stops there, and the value is NIL
if CHAR is not found before there."
  (or to (setq to (length string)))
  (position char string :start from :end to))

(defun string-search-set (char-set string &optional (from 0) to)
  "Returns the index in STRING of the first char past FROM that's in CHAR-SET, or NIL.
CHAR-SET can be a list of characters or a string.
If TO is non-NIL, the search stops there, and the value is NIL
if no occurrence of a char in CHAR-SET is found before there."
  (if (null char-set)
    (return-from string-search-set ()))
  (or to (setq to (length string)))
  (typecase char-set
    (list
     (let (best
           this)
       (dolist (char char-set best)
         (when (setq this (string-search-char char string from to))
           (when (= this from)
             (return  from));fast return
           (setq best (if best
                        (min best this)
                        this)
                 to best)))))
    (array
     (let ((set-len (length char-set)))
       (do ((i from (1+ i)))
           ((>= i to)
            nil)
         (when (string-search-char (aref string i) char-set 0 set-len)
           (return i)))))
    ((or fixnum character) (string-search-char char-set string from to))
    (t (error "CHAR-SET ~s must be a character, string, or list of characters" char-set))))

(defun string-search-not-set (char-set string &optional (from 0) to)
  "Returns the index in STRING of the first char past FROM that's NOT in CHAR-SET, or NIL.
CHAR-SET can be a list of characters or a string.
If TO is non-NIL, the search stops there, and the value is NIL
if no occurrence of a char not in CHAR-SET is found before there."
  (if (null char-set)
    (return-from string-search-not-set ()))
  (or to (setq to (length string)))
  (let ((compare-fct #'char-equal))
    (etypecase char-set
      (array
       (do ((i from (1+ i)))
	   ((>= i to)
	    nil)
	 (unless (string-search-char  (aref string i) char-set 0 (length char-set))
	   (return-from string-search-not-set i))))
      (list
       (do ((i from (1+ i)))
	   ((>= i to)
	    nil)
         (unless (dolist (e char-set)
                   (when (char-equal  (aref string i) e)
                     (return e)))
	   (return-from string-search-not-set i))))
      ((or character fixnum)
       (do ((i from (1+ i)))
	   ((>= i to)
	    nil)
	 (unless (funcall compare-fct char-set (aref string i))
	   (return-from string-search-not-set i)))))))

;;; ----------------------------------------------------------------------------

(defun daylight-savings-p ()
  (multiple-value-bind (second minutes hour day month weekday) (get-decoded-time)
    (declare (ignore second minutes))
    #+MCL
    (ccl::DAYLIGHT-SAVING-TIME-P hour day month weekday)
    #+allegro
    EXCL::*DAYLIGHT-SAVINGS-TIME-OBSERVED-P*
    #+lispworks
    t
    #-(or MCL allegro lispworks)
    (nyi)))

;;;----------------------------------------------------------------------------

;;; Some useful strings and accessing functions.

;;; Days of the week.  Elements must be (in order):
;;; (1) Three-letter form.
;;; (2) Full spelling.
;;; (3) Middle-length form if any, else NIL.
;;; (4) Francais.
;;; (5) Deutsch.
;;; (6) Italian.  ; How do you say that in Italian ?

(DEFVAR *DAYS-OF-THE-WEEK* '(("Mon" "Monday" NIL "Lundi" "Montag" "Lunedi")
			     ("Tue" "Tuesday" "Tues" "Mardi" "Dienstag" "Martedi")
			     ("Wed" "Wednesday" NIL "Mercredi" "Mittwoch" "Mercoledi")
			     ("Thu" "Thursday" "Thurs" "Jeudi" "Donnerstag" "Giovedi")
			     ("Fri" "Friday" NIL "Vendredi" "Freitag" "Venerdi")
			     ("Sat" "Saturday" NIL "Samedi" "Samstag" "Sabato")
			     ("Sun" "Sunday" NIL "Dimanche" "Sonntag" "Domenica")))

(DEFUN DAY-OF-THE-WEEK-STRING (DAY-OF-THE-WEEK &OPTIONAL (MODE :LONG) &AUX STRINGS)
  (SETQ STRINGS (NTH DAY-OF-THE-WEEK *DAYS-OF-THE-WEEK*))
  (CASE MODE
    (:SHORT (FIRST STRINGS))
    (:LONG(SECOND STRINGS))
    (:MEDIUM (OR (THIRD STRINGS) (FIRST STRINGS)))
    (:FRENCH (FOURTH STRINGS))
    (:GERMAN (FIFTH STRINGS))
    (:ITALIAN (SIXTH STRINGS))    ;; After this, perhaps NDOWSS ?
    (OTHERWISE (error () "~S is not a known day-of-the-week mode" MODE))))  


;;; Months of the year:  Elements must be (in order):
;;; (1) Three-letter form.
;;; (2) Full spelling.
;;; (3) Middle-length form if any, else NIL.
;;; (4) Francais.
;;; (5) Roman numerals (used in Europe).
;;; (6) Deutsch.
;;; (7) Italian.

(DEFVAR *MONTHS* '(("Jan" "January" NIL "Janvier" "I" "Januar" "Genniao")
		   ("Feb" "February" NIL "Fevrier" "II" "Februar" "Febbraio")
		   ("Mar" "March" NIL "Mars" "III" "Maerz" "Marzo")
		   ("Apr" "April" NIL "Avril" "IV" "April" "Aprile")
		   ("May" "May" NIL "Mai" "V" "Mai" "Maggio")
		   ("Jun" "June" NIL "Juin" "VI" "Juni" "Giugno")
		   ("Jul" "July" NIL "Juillet" "VII" "Juli" "Luglio")
		   ("Aug" "August" NIL "Aout" "VIII" "August" "Agosto")
		   ("Sep" "September" "Sept" "Septembre" "IX" "September" "Settembre")
		   ("Oct" "October" NIL "Octobre" "X" "Oktober" "Ottobre")
		   ("Nov" "November" "Novem" "Novembre" "XI" "November" "Novembre")
		   ("Dec" "December" "Decem" "Decembre" "XII" "Dezember" "Dicembre")))

(DEFUN MONTH-STRING (MONTH &OPTIONAL (MODE :LONG) &AUX STRINGS)
  (SETQ STRINGS (NTH (1- MONTH) *MONTHS*))
  (CASE MODE
    (:SHORT (FIRST STRINGS))
    (:LONG (SECOND STRINGS))
    (:MEDIUM (OR (THIRD STRINGS) (FIRST STRINGS)))
    (:FRENCH (FOURTH STRINGS))
    (:ROMAN (FIFTH STRINGS))
    (:GERMAN (SIXTH STRINGS))
    (:ITALIAN (SEVENTH STRINGS))
    (OTHERWISE (error "~S is not a known month mode" MODE)))) 

;;; One-based lengths of months
(DEFVAR *MONTH-LENGTHS* '(0 31. 28. 31. 30. 31. 30. 31. 31. 30. 31. 30. 31.)
  "One-based list of lengths of months.")

(DEFUN MONTH-LENGTH (MONTH YEAR)
  "Return the number of days in month MONTH in year YEAR.
Knows about leap years.  January is month 1."
  (IF (= MONTH 2)
      (IF (LEAP-YEAR-P YEAR) 29. 28.)
      (NTH MONTH *MONTH-LENGTHS*)))

(DEFUN LEAP-YEAR-P (YEAR)           ;;2000 is a leap year.  2100 is not.
  "T if YEAR is a leap year."
  (IF (< YEAR 100.)
    (SETQ YEAR (+ 1900. YEAR)))
  (AND (ZEROP (REM YEAR 4))
     (OR (NOT (ZEROP (REM YEAR 100.)))
	 (ZEROP (REM YEAR 400.)))))

;;AB 8/5/87.  Fix to work as documented regarding YEAR.  [SPR 5382]
;;; Check that a date is ok: day is within month; and day-of-week, if specified, is valid
(DEFUN VERIFY-DATE (DAY MONTH YEAR DAY-OF-THE-WEEK)
  "If the day of the week of the date specified by DATE, MONTH, and YEAR
is the same as DAY-OF-THE-WEEK, return NIL; otherwise, return a string that
contains a suitable error message. If YEAR is less than 100, it is shifted
by centuries until it is within 50 years of the present."
  (COND
    ((> DAY (MONTH-LENGTH MONTH YEAR))
     (FORMAT () "~A only has ~D day~:P" (MONTH-STRING MONTH) (MONTH-LENGTH MONTH YEAR)))
    (DAY-OF-THE-WEEK
     (LET ((UT (encode-UNIVERSAL-TIME 0 0 0 DAY MONTH YEAR)))
       (MULTIPLE-VALUE-BIND (ignore1 ignore2 ignore3 ignore4 ignore5 year CORRECT-DAY-OF-THE-WEEK)
                            (DECODE-UNIVERSAL-TIME UT)
         (declare (ignore ignore1 ignore2 ignore3 ignore4 ignore5))
	 (AND (/= DAY-OF-THE-WEEK CORRECT-DAY-OF-THE-WEEK)
	    (FORMAT () "The ~:R of ~A, ~D is a ~A, not a ~A" DAY (MONTH-STRING MONTH) YEAR
		    (DAY-OF-THE-WEEK-STRING CORRECT-DAY-OF-THE-WEEK)
		    (DAY-OF-THE-WEEK-STRING DAY-OF-THE-WEEK))))))
    (T NIL)))

(DEFVAR *TIMEZONES*
      '((12.    nil     nil  #.(char-code #\M))
        (11.   "NT"     nil  -1)			; Nome Time
        (11.   "BST"    nil  #.(char-code #\L))		; Bering
        (10.   "HST"   "BDT" #.(char-code #\K))		; Hawaiian
        (10.   "AHST"   nil  -1)			; Alaska-Hawaii Standard Time
        (9.    "YST"   "HDT" #.(char-code #\I))		; Yukon Standard Time
        (8.    "PST"   "YDT" #.(char-code #\H))		; Pacific Standard Time
        (7.    "MST"   "PDT" #.(char-code #\G))		; Mountain Standard Time
        (6.    "CST"   "MDT" #.(char-code #\F))		; Central Standard Time
        (5.    "EST"   "CDT" #.(char-code #\E))		; Eastern Standard Time
        (4.    "AST"   "EDT" #.(char-code #\D))		; Atlantic Standard Time
        (3.5   "NST"    nil  -1)			; Newfoundland Standard Time
        (3.    "GST"   "ADT" #.(char-code #\C))		; Greenland  Standard Time
        (2.    "AT"     nil  #.(char-code #\B))		; Azores Time
        (1.    "WAT"    nil  #.(char-code #\A))		; West Africa Time
        (0.    "UT"     nil  -1)			; Universal Time
        (0.    "Z"      nil  -1)			; Universal Time
        (0.    "GMT"    nil  #.(char-code #\Z))		; Greenwich Mean Time
        (-1.   "CET"    nil  #.(char-code #\N))		; Central European Time
        (-1.   "MET"    nil  -1)			; Middle Europe Time
        (-1.   "MEWT"   nil  -1)			; Middle Europe Winter Time
        (-1.   "BST"    nil  -1)			; British Summer Time
        (-1.   "SWT"    nil  -1)			; Swedish Winter Time
        (-1.   "FWT"    nil  -1)			; French Winter Time
        (-2.   "MEST"   nil  -1)			; Middle Europe Summer Time
        (-2.   "EET"    nil  #.(char-code #\O))		; Eastern European Time
        (-2.   "SST"    nil  -1)			; Swedish Summer Time
        (-2.   "FST"    nil  -1)			; French Summer Time
        (-3.   "BT"     nil  #.(char-code #\P))		; Baghdad Time
        (-4.   "ZP4"    nil  #.(char-code #\Q))		; GMT +4  hours.
        (-5.   "ZP5"    nil  #.(char-code #\R))		; GMT +5  hours.
        (-5.5  "IST"    nil  -1)			; Indian Standard Time
        (-6.   "ZP6"    nil  -1)			; GMT +6  hours.
        (-7.   "WAST"   nil  #.(char-code #\T))		; West Australian Standard Time
        (-7.5  "JT"     nil  -1)			; Java Time
        (-8.   "WADT"   nil  #.(char-code #\U))		; West Australian Daylight Time
        (-8.   "CCT"    nil  -1)			; China Coast Time
        (-9.   "JST"    nil  #.(char-code #\V))		; Japan Standard Time
        (-9.5  "CAST"   nil  -1)			; Central Australian Standard Time
        (-9.5  "SAST"   nil  -1)			; South Australian Standard Time
        (-10.  "EAST"   nil  #.(char-code #\W))		; East Australian Standard Time
        (-10.5 "CADT"   nil  -1)			; Central Australian Daylight Time
        (-10.5 "SADT"   nil  -1)			; South Australian Daylight Time
        (-11.  "EADT"   nil  #.(char-code #\X))		; East Australian Daylight Time
        (-12.  "NZT"    nil  -1)))

(defvar *TIMEZONE* 5)

(DEFUN TIMEZONE-STRING (&OPTIONAL (TIMEZONE *TIMEZONE*) (DAYLIGHT-SAVINGS-P (DAYLIGHT-SAVINGS-P)))
  "Return a string describing timezone TIMEZONE, optionally for daylight savings time.
Defaults are our own timezone, and DST if it is now in effect."
  (IF DAYLIGHT-SAVINGS-P
    (THIRD (ASSOC (1- TIMEZONE) *TIMEZONES* :TEST #'EQUAL))
    (SECOND (ASSOC TIMEZONE *TIMEZONES* :TEST #'EQUAL))))

;;;----------------------------------------------------------------------------

(DEFVAR *DEFAULT-LANGUAGE* "English")  ;belongs in site info
  ;;so put these things there anyway!!
;; where are latitude and longitude anyway???
;; they should be in site info.
 
;;; Character manipulation primitives that ought to be installed
;;; for general use, possibly not under these names.

(DEFUN DIGIT-CHARACTER-P (CHAR)
  "T if CHAR is a digit."
  (digit-char-p char))

(DEFUN LETTER-CHARACTER-P (CHAR)
  "T if CHAR is a letter."
  (<= (char-code #\A) (char-code (CHAR-UPCASE CHAR)) (char-code #\Z))) 


(DEFUN DELQ-ALL (LIST SYMBOLS)
  "Delete all occurrences of all elements of SYMBOLS from LIST."
  (DOLIST (SYMBOL SYMBOLS)
    (SETQ LIST (DELETE SYMBOL (THE LIST LIST) :TEST #'EQ)))
  LIST) 


;;; Lexical analyzer.


(DEFPARAMETER *SPECIAL-CHAR-SYMBOLS*
              '((#\- . -) (#\+ . +) (#\/ . /) (#\: . |:|) (#\, . |,|) (#\. . |.|)
               (#\; . |;|))) 

;;; This function is the lexical analyzer of the parser; it splits a string
;;; up into tokens.  It takes a string, and optionally the starting and finishing
;;; indexes of a substring within the string to use.  It returns a list.
;;; Each element of the list corresponds to a token.  Numbers, interpreted
;;; in decimal, appear in the list as two-lists; the first element is a fixnum
;;; giving the value of the number, and the second is the number of digits
;;; (including leading zeroes!).  Any other token appears as
;;; a symbol (interned in the Time package).

(DEFUN LEXICALLY-ANALYZE (STRING &OPTIONAL (START 0) (END (LENGTH STRING)));; Each time around this loop we either skip over some uninteresting text,
 ;; or we cons another token onto the list of tokens.
 
  (DO ((INDEX START)
       (RESULT NIL))
      ((>= INDEX END) (NREVERSE RESULT))
    (LET ((CHAR (AREF STRING INDEX)))
      (COND
        ((MEMBER CHAR '(#\SPACE #\TAB #\' #\NEWLINE) :TEST #'char=);; This is a whitespace character, ignore it.
          (SETQ INDEX (1+ INDEX)))
        ((ASSOC CHAR *SPECIAL-CHAR-SYMBOLS* :TEST #'char=);; This is a special character.  Make a new token which is is
         ;; symbol whose name is that character.
         
         (PUSH (CDR (ASSOC CHAR *SPECIAL-CHAR-SYMBOLS* :TEST #'EQ)) RESULT)
         (SETQ INDEX (1+ INDEX)))
        ((DIGIT-CHARACTER-P CHAR);; This is the beginning of a number in decimal.  Make a new token
         ;; which is a fixnum of this number's value.
         
         (DO ((I (1+ INDEX) (1+ I))
              (DIGITS 1 (1+ DIGITS))
              (N 0))
             (NIL)
           (SETQ N (+ (* N 10) (- (char-code CHAR) (char-code #\0))))
           (COND
             ((OR (>= I END) (NOT (DIGIT-CHARACTER-P (SETQ CHAR (AREF STRING I)))))
              (PUSH (LIST N DIGITS) RESULT) (SETQ INDEX I) (RETURN ())))))
        ((LETTER-CHARACTER-P CHAR);; This is the beginning of an alphabetic token.  Scan over all contiguous
         ;; letters, upcasing them, and make a new token which is a symbol.
         
         (DO ((I INDEX (1+ I)))
             ((OR (>= I END)
               (LET ((CHAR (AREF STRING I)))
                 (AND
                  (NOT (OR (char= CHAR #\@) (LETTER-CHARACTER-P CHAR)));; This clause is to make "A.M." and "P.M." work, but note
                  ;; that trailing dots are ignored (see below).
                  
                  (NOT (char= CHAR #\.));; If we are inside an alphabetic token and see a hypen followed
                  ;; by a letter, accept this; e.g. "twenty-third".
                  
                  (NOT
                   (AND (char= CHAR #\-) (< (1+ I) END)
                        (LETTER-CHARACTER-P (AREF STRING (1+ I)));; Special kludge: if it is AM- or PM-, break
                        ;; here; some hosts send AM-EDT and such.
                        
                        (NOT
                         (MEMBER (SUBSEQ STRING (MAX 0 (- I 2)) I)
				 '("AM" "PM" "am" "pm")
                                 :TEST #'EQUAL)))))))
           (PUSH
            (MONTH-INTERN
             (STRING-UPCASE
              (#+Portable NSUBSTRING #-Portable subseq STRING INDEX;; Strip trailing dots, for "Wed." and "Oct.".
                                 ;; Unfortunately also for "A.M.", see way below.
                                 
                          (IF (CHAR-EQUAL (AREF STRING (1- I)) #\.) (1- I) I)))
             *TIME-PARSER-PACKAGE*)
            RESULT)
           (SETQ INDEX I))))
        ((char= CHAR #\();; This is the beginning of a parenthesized string.  RFC733 defines such
         ;; to be equivalent to whitespace, so we ignore this string.  The "Laurel"
         ;; program puts days of the week in parens, but these are redundant so it
         ;; is OK to ignore them.
         
         (DO ((I INDEX (1+ I)))
             ((OR (>= I END) (= (AREF STRING I) #\))) (SETQ INDEX (1+ I)))))
        (T (BARF "Unknown character ~C" CHAR)))))) 


(DEFPARAMETER *MONTH-SYMBOLS*
              '(("JAN" . JAN) ("FEB" . FEB) ("MAR" . MAR) ("APR" . APR) ("MAY" . MAY)
               ("JUN" . JUN) ("JUL" . JUL) ("AUG" . AUG) ("SEP" . SEP) ("OCT" . OCT)
               ("NOV" . NOV) ("DEC" . DEC))) 


(DEFUN MONTH-INTERN (STRING *PACKAGE*)
  "The same as INTERN, but faster if STRING is a 3-character month name.
Assumes that the package is the TIME package for them."
  (OR (AND (= (LENGTH STRING) 3) (CDR (ASSOC STRING *MONTH-SYMBOLS* :TEST 'EQUALP)))
      (INTERN STRING *PACKAGE*))) 

;;; Defining patterns.

;;; A pattern is a list of N pattern elements, which are compared one-for-one
;;; with the first N elements of the token list we are analyzing.  Each
;;; pattern element may be:
;;; (a) A symbol or fixnum, which matches exactly that symbol or fixnum.
;;; (b) A list whose CAR is a "special pattern" symbol, in which case a special
;;;        function associated with that symbol is invoked.
;;; (c) A list of one symbol, which is interpreted as an arbitrary predicate
;;;        of one argument, which is applied to the token and should return
;;;        true if the token "matches".
;;;
;;; Note: symbols with FIXNUM-STRING properties are treated as if they
;;; were fixnums, rather than symbols.  This is for English cardinal and
;;; ordinal numbers.
;;;
;;; The following special pattern symbols exist, with the following special "forms":
;;;  (FIXP), which matches any fixnum.
;;;  (FIXP <n>), which matches any fixnum with exactly n digits.
;;;  (FIXP <m> <n>), which matches any fixnum with between m and n digits inclusive.
;;;  (FRACTION), which matches any fraction, with articles before or after.
;;;    These fractions are phrases like "a half", not numbers and slashes.
;;;  (FRACTION T), which matches any fraction, not including articles after.
;;;  (ANY-OF <pattern-element> <pattern-element> ...), which matches if any of the
;;;    pattern elements match.
;;;  (GET <property-indicator>), which matches if the token is a symbol with a
;;;    non-NIL <property-indicator> property.
;;;  (ANY), which matches any single token.

;;; Examples:
;;;   ((FIXP 1 2) /: (FIXP 2) /: (FIXP 2))    Matches 11:30:15
;;;   ((GET MONTH) (FIXP 1 2) /, (FIXP 4))    Matches Jan 23, 1980
;;;   (12. (GET MERIDIAN))                    Matches 12 pm or 12 am
;;;   ()                                      Matches anything
;;;   ((ANY))                                 Matches anything except no tokens.

;;; The special form DEFPATTERN defines a pattern, information about when to try to
;;; match it, and what to do if it is matched.  The form looks like this:
;;; (DEFPATTERN <from-state> <pattern> <to-state> <lambda-list> . <body>)
;;; The parser has a state, represented by a symbol.
;;; It finds all the DEFPATTERNs for its current
;;; state by finding all those with <from-state> EQ to the current state.
;;; It applies each pattern in succession.  When it finds a pattern that
;;; matches, it invokes the associated function (defined by the <lambda-list>
;;; and <body> of the DEFPATTERN) and sets the state to <to-state>; it also
;;; CDRs off the matched portion of the token list, proceeding with the rest
;;; of the list.  The argument to the function defined by DEFPATTERN are
;;; all those tokens in the token list that matched the pattern, except
;;; those that were expressed in the pattern as symbols or fixnums (since
;;; these are just constants, the function is not interested in them).
;;; (Those that were expressed as ANY-OF pattern elements ARE passed
;;; to the function, even if the token is a constant, just in case the
;;; function cares which of the choices was taken.
;;;
;;; The parse proceeds until there are no tokens left and the state
;;; has a FINAL-STATE property of T.
;;;
;;; There is another version of DEFPATTERN called DEFPATTERN-PEEK, which
;;; is the same except that it "looks ahead" at the tokens, without
;;; passing over them.  Also, the tokens are not passed to the function;
;;; the function must take zero arguments.
;;;
;;; NOTE that the order of DEFPATTERNs in this file is significant, and
;;; defines the order in which patterns are checked.
;;;
;;; A data structure that allows the parser to find all the patterns is
;;; constructed at LOAD-time.  A list of known states is maintained as
;;; the value of *STATES*.  Each state is given a PATTERNS property,
;;; consisting of a list of elements of the form:
;;;  (<pattern> <to-state> <function-to-call> <skip-tokens>)
;;; These lists are CONSed up in reverse order, and NREVERSEd at the end.
;;; <skip-tokens> is true for DEFPATTERN and false for DEFPATTERN-PEEK.


(DEFMACRO DEFPATTERN (FROM-STATE PATTERN TO-STATE LAMBDA-LIST . BODY)
  (LET ((FUNCTION-NAME (GENSYM)))
    `(PROGN
      'COMPILE
      (DEFUN ,FUNCTION-NAME ,LAMBDA-LIST
        . ,BODY)
      (DEFINE-PATTERN ',FROM-STATE ',PATTERN ',TO-STATE ',FUNCTION-NAME T)))) 


(DEFMACRO DEFPATTERN-PEEK (FROM-STATE PATTERN TO-STATE LAMBDA-LIST . BODY)
  (LET ((FUNCTION-NAME (GENSYM)))
    `(PROGN
      'COMPILE
      (DEFUN ,FUNCTION-NAME ,LAMBDA-LIST
        . ,BODY)
      (DEFINE-PATTERN ',FROM-STATE ',PATTERN ',TO-STATE ',FUNCTION-NAME ())))) 

(DEFVAR *STATES*)  ;Need to check if unbound.

;;; This function gets invoked once at load-time before any of the patters
;;; are defined.  There must be exactly one top-level call to this
;;; function in the file, and it must be before all the DEFPATTERNs.

(DEFUN START-PATTERNS ()
  (IF (BOUNDP '*STATES*);; We are reloading.
       (DOLIST (STATE *STATES*)
                            (REMPROP STATE 'PATTERNS)))
  (SETQ *STATES* ())) 

;;; This function runs once at load-time for each DEFPATTERN.  This DEFUN must
;;; appear before any calls to DEFPATTERN in this file.

(DEFUN DEFINE-PATTERN (FROM-STATE PATTERN TO-STATE FUNCTION-NAME SKIP-PATTERNS)
  (OR (MEMBER FROM-STATE *STATES* :TEST #'EQ) (PUSH FROM-STATE *STATES*))
  (PUSH (LIST PATTERN TO-STATE FUNCTION-NAME SKIP-PATTERNS) (GET FROM-STATE 'PATTERNS))) 

;;; This function gets invoked once at load-time after all the patterns
;;; are defined.  There must be exactly one top-level call to this
;;; function in this file, and it must be after all the DEFPATTERNs.

(DEFUN FINISH-PATTERNS ()
  (DOLIST (STATE *STATES*)
    (SETF (GET STATE 'PATTERNS) (NREVERSE (GET STATE 'PATTERNS))))) 

;;; Parser.

;;; This is the function that interprets patterns according to the algorithm
;;; described above.  It returns the final value of STATE.


(DEFUN PARSE-1 (TOKEN-LIST INITIAL-STATE)
  (DO ((STATE INITIAL-STATE)
       (TOKENS TOKEN-LIST))
      ((AND (NULL TOKENS) (GET STATE 'FINAL-STATE)) STATE);; Try matching the first tokens of TOKENS against all the patterns
   ;; associated with STATE.
   
    (DO ((TRIES (GET STATE 'PATTERNS) (CDR TRIES)))
        ((NULL TRIES) (BARF "No pattern matches the tokens ~S in state ~S" TOKENS STATE))
      (LET ((TRY (CAR TRIES)) ignore MATCHED-TOKENS);; TRY represents one of the patterns associated with STATE; it looks
            ;; like (<pattern> <to-state> <function-name> <skip-tokens>).
            
        (COND
          ((MULTIPLE-VALUE-SETQ (ignore MATCHED-TOKENS)
             (PATTERN-MATCH (FIRST TRY) TOKENS));; Found it!  Run the function, advance over the matched tokens,
           ;; go to the new state and continue.
           
           (LET ((RESULT (PATTERN-INVOKE (FIRST TRY) MATCHED-TOKENS (THIRD TRY) (FOURTH TRY))))
             (IF (FOURTH TRY) (SETQ TOKENS RESULT)))
           (SETQ STATE (SECOND TRY)) (RETURN ()))))))) 

;;; Try to match PATTERN against the beginning of TOKEN-LIST.
;;; Return possibly altered token list if they match, else NIL.

(DEFUN PATTERN-MATCH (PATTERN TOKEN-LIST)
#+nil  (DECLARE (VALUES MATCHP EDITED-TOKEN-LIST))
  ;; Check specially for two possible first elements of the pattern
  ;; that are the ones we check for in parsing dates from file servers.
  
  (COND
    ((AND (EQUAL (CAR PATTERN) '(FIXP 1 2)) (CADR PATTERN) (SYMBOLP (CADR PATTERN))
          (not (eq (CADR TOKEN-LIST) (CADR PATTERN))))
     NIL)
    ((AND (EQUAL (CAR PATTERN) '(ANY-OF |:| |.|))
          (NOT (MEMBER (CAR TOKEN-LIST) '(|:| |.|) :TEST #'EQ)))
     NIL)
    (T
     (DO ((PATTERN PATTERN (CDR PATTERN))
          (ENTIRE-TOKEN-LIST TOKEN-LIST)
          (EDITED-TOKEN-LIST)
          (TOKEN-LIST TOKEN-LIST)
          ignore)
         (NIL)
       (COND
         ((NULL PATTERN);; We've matched each element.  Matches!
           (RETURN (VALUES T ENTIRE-TOKEN-LIST)))
         ((NULL TOKEN-LIST);; There is more pattern, but no more tokens.  No match.
           (RETURN ()))
         ((NOT
           (MULTIPLE-VALUE-SETQ (ignore TOKEN-LIST EDITED-TOKEN-LIST)
             (MATCH-ELEMENT (CAR PATTERN) TOKEN-LIST ENTIRE-TOKEN-LIST)));; This element does not match, lose.
          
          (RETURN ()))
         (EDITED-TOKEN-LIST (SETQ ENTIRE-TOKEN-LIST EDITED-TOKEN-LIST))))))) 

;;; Predicate: Does PATTERN-ELEMENT match the first TOKEN(s) of TOKEN-LIST?
;;; Second value the remaining tokens not matched.

(DEFUN MATCH-ELEMENT (PATTERN-ELEMENT TOKEN-LIST ENTIRE-TOKEN-LIST &AUX (TOKEN (CAR TOKEN-LIST))
  (REMAINING-TOKEN-LIST (CDR TOKEN-LIST)) EDITED-TOKEN-LIST MATCHP)
#+nil  (DECLARE (VALUES MATCHP REMAINING-TOKENS EDITED-TOKEN-LIST))
  (SETQ MATCHP
        (COND
          ((SYMBOLP PATTERN-ELEMENT);; The pattern element is a symbol; matching is judged by EQness.
            (EQ PATTERN-ELEMENT TOKEN))
          ((INTEGERP PATTERN-ELEMENT);; Match any fixnum of this value, no matter what its length.
           
           (OR;; Detect multi-token fractions of all sorts, plus noise words.
            
            (MULTIPLE-VALUE-BIND (FLAG REMAINING EDITED-LIST) (MATCH-FRACTION PATTERN-ELEMENT TOKEN-LIST ENTIRE-TOKEN-LIST)
              (IF FLAG (SETQ REMAINING-TOKEN-LIST REMAINING EDITED-TOKEN-LIST EDITED-LIST))
              FLAG);; Next possibility: a number made of digits.
            
            (AND (CONSP TOKEN) (INTEGERP (FIRST TOKEN)) (= (FIRST TOKEN) PATTERN-ELEMENT));; Other possibility: a string number.
            
            (AND (SYMBOLP TOKEN) (GET TOKEN 'FIXNUM-STRING)
                 (= (GET TOKEN 'VALUE) PATTERN-ELEMENT))))
          ((EQ (FIRST PATTERN-ELEMENT) 'FRACTION)
           (MULTIPLE-VALUE-BIND (FLAG REMAINING EDITED-LIST) (MATCH-FRACTION PATTERN-ELEMENT TOKEN-LIST ENTIRE-TOKEN-LIST
                            (SECOND PATTERN-ELEMENT))
             (IF FLAG (SETQ REMAINING-TOKEN-LIST REMAINING EDITED-TOKEN-LIST EDITED-LIST))
             FLAG))
          ((EQ (FIRST PATTERN-ELEMENT) 'FIXP);; Match certain fixnums.
           
           (OR;; Detect multi-token fractions of all sorts, plus noise words.
            
            (AND (EQUAL PATTERN-ELEMENT '(FIXP))
                 (MULTIPLE-VALUE-BIND (FLAG REMAINING EDITED-LIST) (MATCH-FRACTION PATTERN-ELEMENT TOKEN-LIST ENTIRE-TOKEN-LIST)
                   (IF FLAG (SETQ REMAINING-TOKEN-LIST REMAINING EDITED-TOKEN-LIST EDITED-LIST))
                   FLAG));; Next possibility: a number made of digits.
            
            (AND (CONSP TOKEN) (INTEGERP (FIRST TOKEN))
                 (MATCH-NUMBER PATTERN-ELEMENT (SECOND TOKEN)));; Other possibility: a string number.
            
            (AND (SYMBOLP TOKEN) (GET TOKEN 'FIXNUM-STRING)
                 (MATCH-NUMBER PATTERN-ELEMENT (IF (> (GET TOKEN 'VALUE) 9) 2 1)))))
          ((EQ (FIRST PATTERN-ELEMENT) 'ANY);; Match any token.
            T)
          ((EQ (FIRST PATTERN-ELEMENT) 'ANY-OF);; If the TOKEN is any of these things, match.
           
           (MEMBER TOKEN (CDR PATTERN-ELEMENT) :TEST #'EQ))
          ((EQ (FIRST PATTERN-ELEMENT) 'GET);; If TOKEN is a symbol with this property, match.
           
           (AND (SYMBOLP TOKEN) (GET TOKEN (SECOND PATTERN-ELEMENT))))
          (T;; Not a "special" form.  This is a predicate to apply.
            (FUNCALL (FIRST PATTERN-ELEMENT) TOKEN))))
  (VALUES MATCHP REMAINING-TOKEN-LIST EDITED-TOKEN-LIST)) 


(DEFUN MATCH-FRACTION (PATTERN-ELEMENT TOKEN-LIST ENTIRE-TOKEN-LIST &OPTIONAL DONT-INCLUDE-FOLLOWING-ARTICLES &AUX
  (TOKEN (CAR TOKEN-LIST)) (REMAINING-TOKEN-LIST (CDR TOKEN-LIST)) EDITED-TOKEN-LIST MATCHP
  (NUMBER-OF-TOKENS 1))
#+nil  (DECLARE (VALUES MATCHP REMAINING-TOKEN-LIST EDITED-TOKEN-LIST))
  (OR;; "2.5"
   
   (AND (CONSP TOKEN) (INTEGERP (CAR TOKEN)) (EQ (SECOND TOKEN-LIST) '|.|)
        (CONSP (THIRD TOKEN-LIST)) (INTEGERP (CAR (THIRD TOKEN-LIST)))
        (SETQ MATCHP
              (READ-FROM-STRING (FORMAT () "~D.~D" (CAR TOKEN) (CAR (THIRD TOKEN-LIST))) T))
        (SETQ NUMBER-OF-TOKENS 3));; ".5"
   
   (AND (EQ TOKEN '|.|) (CONSP (SECOND TOKEN-LIST)) (INTEGERP (CAR (SECOND TOKEN-LIST)))
        (SETQ MATCHP (READ-FROM-STRING (FORMAT () "~D.~D" 0 (CAR (SECOND TOKEN-LIST))) T))
        (SETQ NUMBER-OF-TOKENS 2));; "2 a half", which is what we get from "2 and a half"
   ;; since "and" is a noise word.
   
   (AND (CONSP TOKEN) (INTEGERP (CAR TOKEN))
        (MULTIPLE-VALUE-BIND (FRACTION REMAINING) (MATCH-FRACTION '(FIXP) (CDR TOKEN-LIST) ENTIRE-TOKEN-LIST
                         DONT-INCLUDE-FOLLOWING-ARTICLES)
          (IF FRACTION
              (SETQ MATCHP (+ (CAR TOKEN) FRACTION) NUMBER-OF-TOKENS
                    (LENGTH (LDIFF TOKEN-LIST REMAINING)) DONT-INCLUDE-FOLLOWING-ARTICLES T))
          FRACTION));; "A half", etc.
   
   (AND (SYMBOLP TOKEN) (GET TOKEN 'ARTICLE) (SYMBOLP (CADR TOKEN-LIST))
        (GET (CADR TOKEN-LIST) 'FRACTION) (SETQ MATCHP (GET (CADR TOKEN-LIST) 'VALUE))
        (SETQ NUMBER-OF-TOKENS 2));; just "Half".
   
   (AND (SYMBOLP TOKEN) (GET TOKEN 'FRACTION) (SETQ MATCHP (GET TOKEN 'VALUE))))
  (AND (INTEGERP PATTERN-ELEMENT) MATCHP (/= PATTERN-ELEMENT MATCHP) (SETQ MATCHP ()));; Now discard an article or proposition following the fraction, if any.
  ;; "half a", etc.
  
  (COND
    ((NOT DONT-INCLUDE-FOLLOWING-ARTICLES)
     (LET ((TOKEN-AFTER (NTH NUMBER-OF-TOKENS TOKEN-LIST)))
       (AND (EQ TOKEN-AFTER 'OF) (INCF NUMBER-OF-TOKENS)))
     (LET ((TOKEN-AFTER (NTH NUMBER-OF-TOKENS TOKEN-LIST)))
       (AND (SYMBOLP TOKEN-AFTER) (GET TOKEN-AFTER 'ARTICLE) (INCF NUMBER-OF-TOKENS)))));; Now edit out the tokens we want to replace, if more than one.
  
  (IF (/= NUMBER-OF-TOKENS 1)
      (SETQ REMAINING-TOKEN-LIST (NTHCDR NUMBER-OF-TOKENS TOKEN-LIST) EDITED-TOKEN-LIST
            (APPEND (LDIFF ENTIRE-TOKEN-LIST TOKEN-LIST) (LIST (LIST MATCHP))
                    REMAINING-TOKEN-LIST)))
  (VALUES MATCHP REMAINING-TOKEN-LIST EDITED-TOKEN-LIST)) 


;;; Internal function of MATCH-ELEMENT for matching numbers.

(DEFUN MATCH-NUMBER (PATTERN-ELEMENT LENGTH)
  (CASE (LENGTH PATTERN-ELEMENT)
    (1 T)
    (2 (= (SECOND PATTERN-ELEMENT) LENGTH))
    (3 (AND (<= (SECOND PATTERN-ELEMENT) LENGTH) (>= (THIRD PATTERN-ELEMENT) LENGTH))))) 

;;; Call FUNCTION, passing it all the tokens of TOKEN-LIST that were
;;; matched by PATTERN, except the constants.

(DEFUN PATTERN-INVOKE (PATTERN TOKEN-LIST FUNCTION PASS-ARGUMENTS
                               &aux #+Explorer (arg-count 0)
                               #-Explorer arguments)
  (PROG ()
    (IF (NOT PASS-ARGUMENTS) (GO END-LOOP)); Don't give it arguments.
    
    #+Explorer
    (sys:%ASSURE-PDL-ROOM (+ 4 (LENGTH PATTERN))); (Conservative.)
    
    LOOP
    (COND
      ((NULL PATTERN) (GO END-LOOP)))
    (COND
      ((NOT (ATOM (CAR PATTERN)))
       #+Explorer
       (incf arg-count)
       #+Explorer
       (sys:%PUSH (CAR TOKEN-LIST))
       #-Explorer 
       (push (CAR TOKEN-LIST) arguments)))

    (SETQ PATTERN (CDR PATTERN))
    (SETQ TOKEN-LIST (CDR TOKEN-LIST))
    (GO LOOP)
    END-LOOP
    #+Explorer
    (sys:%call function arg-count)
    #-Explorer
    (apply function (reverse arguments))
    (RETURN TOKEN-LIST))
  #+FUCKED
  (cond ((not pass-arguments)
         (funcall function)
         (values token-list))
        (t
         (loop for token-list on token-list
               for token = (first token-list)
               for pattern on pattern
               for pattern-item = (first pattern)
               when (not (atom pattern-item)) collect token into arguments
               finally 
               (format t "~&Calling ~a with ~a" function arguments)
               (apply function arguments)
               (return token-list))))
  )

;;; Given a token that represents a number, return the number's value.

(DEFUN NUMBER-VALUE (TOKEN)
  (COND
    ((AND (CONSP TOKEN) (NUMBERP (FIRST TOKEN)));; This is a number token made of digits.
      (FIRST TOKEN))
    ((AND (SYMBOLP TOKEN) (GET TOKEN 'FIXNUM-STRING));; This is an English ordinal or cardinal.
      (GET TOKEN 'VALUE))
    (T (error "The token ~S is not a number at all." TOKEN)))) 


;;; Keywords.

;;; This stuff runs at LOAD time.  It sets up properties on various interesting
;;; keyword symbols, so that patterns can check for these properties.

;;; The argument is a list of lists.  Each list is a bunch of spellings
;;; of a value, each of which is a string; successive lists have successive values,
;;; starting at FIRST-VALUE.  Each spelling is turned into a symbol, which gets
;;; a VALUE property of the fixnum value, and a <TYPE> property of T.

(DEFUN ASSIGN-TYPE-AND-VALUES (TYPE LIST-OF-LISTS FIRST-VALUE)
  (DO ((REST LIST-OF-LISTS (CDR REST))
       (I FIRST-VALUE (1+ I)))
      ((NULL REST))
    (DOLIST (STRING (CAR REST))
      (IF (STRINGP STRING); Don't bash plist of NIL.
          
          (LET ((SYMBOL (INTERN (STRING-UPCASE STRING) *TIME-PARSER-PACKAGE*)))
            (SETF (GET SYMBOL 'VALUE) I)
            (SETF (GET SYMBOL TYPE) T)))))) 

;;; NOTE: This file must be loaded after the TIME file.

(ASSIGN-TYPE-AND-VALUES 'DAY-OF-THE-WEEK *DAYS-OF-THE-WEEK* 0) 

(ASSIGN-TYPE-AND-VALUES 'MONTH *MONTHS* 1) 

;;; Take a list of lists of symbols.  Every symbol gets a <type> property
;;; of T and a VALUE property of the first symbol of the list.

(DEFUN ASSIGN-TYPE-AND-VALUES-SYMBOLS (TYPE VALUE-PROP-NAME LIST-OF-LISTS)
  (DOLIST (LIST-OF-SYMBOLS LIST-OF-LISTS)
    (LET ((FIRST-SYMBOL (FIRST LIST-OF-SYMBOLS)))
      (DOLIST (SYMBOL LIST-OF-SYMBOLS)
        (SETF (GET SYMBOL VALUE-PROP-NAME) FIRST-SYMBOL)
        (SETF (GET SYMBOL TYPE) T))))) 


(ASSIGN-TYPE-AND-VALUES-SYMBOLS 'HALF-DAY 'VALUE '((NOON N) (MIDNIGHT M))) 


(ASSIGN-TYPE-AND-VALUES-SYMBOLS 'OFFSET 'OFFSET-VALUE
                                '((YEARS YEAR YR Y) (MONTHS MONTH MO) (WEEKS WEEK WK)
                                 (DAYS DAY DA DY D) (HOURS HOUR HR H)
                                 (MINUTES MINUTE MINS MIN MN M) (SECONDS SECOND SECS SEC SC S))) 


(ASSIGN-TYPE-AND-VALUES-SYMBOLS 'MERIDIAN 'VALUE '((AM A.M. A.M) (PM P.M. P.M))) 

;;6/1/88 CLM - changed to call STRING instead of the DEFF'ed STRING-CHAR (SPR 8136).
(DEFUN ASSIGN-TIME-ZONES ()
  (DOLIST (ZONE-SPEC *TIMEZONES*)
    (LET ((VALUE (FIRST ZONE-SPEC)))
      (IF (NOT (NULL (SECOND ZONE-SPEC))) (ASSIGN-ZONE (SECOND ZONE-SPEC) VALUE))
      (IF (NOT (NULL (THIRD ZONE-SPEC))) (ASSIGN-ZONE (THIRD ZONE-SPEC) VALUE))
      (IF (PLUSP (FOURTH ZONE-SPEC)) (ASSIGN-ZONE (STRING (code-char (FOURTH ZONE-SPEC))) VALUE)))))

(DEFUN ASSIGN-ZONE (STRING VALUE)
  (IF (STRINGP STRING); Don't bash plist of NIL.
      
      (LET ((SYMBOL (INTERN STRING *TIME-PARSER-PACKAGE*)))
        (SETF (GET SYMBOL 'ZONE-VALUE) VALUE); Can't use VALUE: N and M are half-days too!
        
        (SETF (GET SYMBOL 'TIME-ZONE) T)))) 


(ASSIGN-TIME-ZONES) 


(SETF (GET '- T) 'SIGN) 

(SETF (GET '+ T) 'SIGN) 

;;; Cardinal and ordinal numbers.

(DEFUN ASSIGN-NUMBERS ()
  (DOTIMES (I 31)
    (ASSIGN-NUMBER I (INTERN (STRING-UPCASE (FORMAT () "~:R" I)) *TIME-PARSER-PACKAGE*))
    (ASSIGN-NUMBER I (INTERN (STRING-UPCASE (FORMAT () "~R" I)) *TIME-PARSER-PACKAGE*)))) 


(DEFUN ASSIGN-NUMBER (NUMBER SYMBOL)
  (SETF (GET SYMBOL 'FIXNUM-STRING) T)
  (SETF (GET SYMBOL 'VALUE) NUMBER)) 


(ASSIGN-NUMBERS) 

;;; Make indefinite articles work, so that "a minute" and "an hour" will be accepted.

(ASSIGN-NUMBER 1 'A) 

(ASSIGN-NUMBER 1 'AN) 

;;; Make "a half" and "half a" work in MATCH-ELEMENT.

(DEFPROP A T ARTICLE) 

(DEFPROP AN T ARTICLE) 

(DEFPROP ONE T ARTICLE) 

(DEFPROP HALF T FRACTION) 

(DEFPROP QUARTER T FRACTION) 

(ASSIGN-NUMBER 0.5 'HALF) 

(ASSIGN-NUMBER 0.25 'QUARTER) 

;;; German numbers.

#+Explorer
(IF (GET :GERMAN 'SI:PRINC-FUNCTION) (ASSIGN-GERMAN-NUMBERS)) 


(DEFUN ASSIGN-GERMAN-NUMBERS ()
  (LET ((*PRINT-BASE* :GERMAN))
    (DOTIMES (I 31)
      (ASSIGN-NUMBER I (INTERN (STRING-UPCASE (FORMAT () "~S" I)) *TIME-PARSER-PACKAGE*))))) 

;;; The patterns.

(defvar *last-offset-direction* '- "Set in SET-OFFSET and used in `THE HOUR' parsing.")

;;; Handle a general time followed by FROM (or something like it) and another time.
                        
(defprop from + direction) 

(defprop after + direction)

(defprop past + direction) 

(defprop before - direction) 

(defprop till - direction) 

(defprop to - direction) 

(defprop of - direction)

(DEFPROP OF T OF-OR-TO) 

(DEFPROP TO T OF-OR-TO) 

(START-PATTERNS)


;;; Buzz words.  And, Of, 1st, 2nd, 3rd, 4th.

(defparameter *noise-words* '(of day and at st nd rd th |;|)) ;; Remove the THE - Westy.

;;; 3-Jan-80 means Jan 3, 1980.
;;; Put this first so Twenex file servers run fast.
;;; This pattern is so specific that nothing that
;;; follows ought to override it.

(defpattern main ((fixp 1 2) - (get month) - (fixp 2)) main (date month year) (set-date date)
            (set-month-from-name month) (set-year-of-century year)) 

;;; 3/15/80 means March 15, 1980.  15/3/80 means March 15, 1980 to a European.
;;; If both the numbers are small, an ambuguity must be dealt with.
;;; Put this here so that ITS file servers run fast.

(defpattern main ((fixp 1 2) / (fixp 1 2) / (fixp 2)) main (month date year-of-century)
            (set-month-and-date month date) (set-year-of-century year-of-century)) 

;;; 11:30 means 11 hours and 30 minutes, go look for seconds.
;;; This is also used by ITS and Twenex file servers.

(defpattern main ((fixp 1 2) |:| (fixp 2)) second (hour minute) (set-hour hour)
            (set-minute minute)) 

;;; March 15 means March 15; go look for a year preceeded by a comma.

(defpattern main ((get month) (fixp 1 2)) year-comma (month date) (set-month-from-name month)
            (set-date date)) 

;;; 15 March means March 15; go look for a year.

(defpattern main ((fixp 1 2) (get month)) year-comma (date month) (set-date date)
            (set-month-from-name month)) 

;;; 3/15/1980 means March 15, 1980.  Same European problem.

(defpattern main ((fixp 1 2) / (fixp 1 2) / (fixp 4)) main (month date year)
            (set-month-and-date month date) (set-year year)) 

;;; 3/15 means March 15, year defaults.  Same European problem.

(defpattern main ((fixp 1 2) / (fixp 1 2)) main (month date) (set-month-and-date month date)) 

;;; Note: GDixon's convert_date_to_binary_.rd believes in YY-MM-DD; the code
;;; below believes in MM-DD-YY.  RFC733 does not allow numeric months at all.

;;; 3-15-80 means March 15, 1980.  Same European problem.

(defpattern main ((fixp 1 2) - (fixp 1 2) - (fixp 2)) main (month date year-of-century)
            (set-month-and-date month date) (set-year-of-century year-of-century)) 

;;; 3-15-1980 means March 15, 1980.  Same European problem.

(defpattern main ((fixp 1 2) - (fixp 1 2) - (fixp 4)) main (month date year)
            (set-month-and-date month date) (set-year year)) 

;;; 3-15 means March 15, year defaults.  Same European problem.

(defpattern main ((fixp 1 2) - (fixp 1 2)) main (month date) (set-month-and-date month date)) 

;;; 3-Jan-1980 means Jan 3, 1980.

(defpattern main ((fixp 1 2) - (get month) - (fixp 4)) main (date month year) (set-date date)
            (set-month-from-name month) (set-year year)) 

;;; Jan-3-80 means Jan 3, 1980.

(defpattern main ((get month) - (fixp 1 2) - (fixp 2)) main (month date year)
            (set-month-from-name month) (set-date date) (set-year-of-century year)) 

;;; Jan-3-1980 means Jan 3, 1980.

(defpattern main ((get month) - (fixp 1 2) - (fixp 4)) main (month date year)
            (set-month-from-name month) (set-date date) (set-year year)) 

;;; 1130.4 means 11 hours and 30.4 minutes, in Multics internal headers,
;;; which Zmail sometimes actually sees.  (I think this happens when
;;; a QSEND from Multics turns into mail.)

(defpattern main ((fixp 4) |.| (fixp 1)) main (hhmm tenths-of-minutes) (set-hhmm hhmm)
            (set-tenths-of-minute tenths-of-minutes)) 

;;; 1130. means 11 hours and 30 minutes and zero seconds.

(defpattern main ((fixp 4) |.|) main (hhmm) (set-hhmm hhmm)) 

;;; 1130 means 11 hours and 30 minutes and zero seconds.

(defpattern main ((fixp 4)) main (hhmm) (set-hhmm hhmm)) 

;;; 113015 means 11 hours, 30 minutes and 15 seconds.

(defpattern main ((fixp 6)) main (hhmmss) (set-hhmmss hhmmss)) 

;;; Allow the format 11:12:03 1982 which UNIX seems to put in messages.

(defpattern second ((any-of |:| |.|) (fixp 1 2) (fixp 4)) main (ignore second year)
            ignore (set-year year) (set-second second)) 

;;; Looking for seconds, :23 means 23 seconds, look for AM/PM.
;;; .23 works too; this is a European form.

(defpattern second ((any-of |:| |.|) (fixp 1 2)) meridian (ignore second) 
  ignore (set-second second)) 

;;; Looking for seconds, not finding them, look for AM/PM.

(defpattern second () meridian () (set-second '(0 2))) 

;;; Looking for meridian, AM means AM and PM means PM, go back to main state.

(defpattern meridian ((get meridian)) main (meridian) (set-meridian meridian)) 

;;; Looking for meridian, not finding it, go back to main state.

(defpattern meridian () main ()) 

;;; 4 PM means what you would think.

(defpattern main ((fixp 1 2) (get meridian)) main (hour meridian) (set-hour hour)
            (set-meridian meridian) (set-minute '(0 2)) (set-second '(0 2))) 

;;; Day of the week, as in "Friday, Jan 5"

(defpattern main ((get day-of-the-week) |,|) main (day-of-the-week)
            (set-day-of-the-week day-of-the-week)) 

;;; Day of the week.

(defpattern main ((get day-of-the-week)) main (day-of-the-week)
            (set-day-of-the-week day-of-the-week)) 

;;; These patterns inserted by CAL 10/24/80

;;; "today"

(defpattern main (today) main () (set-today)) 

;;; "yesterday"

(defpattern main (yesterday) main () (set-yesterday)) 

;;; "tomorrow"

(defpattern main (tomorrow) main () (set-tomorrow)) 

;;; "now"

(defpattern main (now) main () (set-now))

;; "half past the hour" or "20 minutes before the hour" - WESTY
(defpattern main (the hour) main ()
            (if (eq *last-offset-direction* '+)
              (set-past-hour)
              (set-next-hour)))

;;; "2 days before jan 30"

(defpattern main ((fixp) (get offset) before) main (offset-value offset-units)
            (set-offset '- offset-value offset-units)) 

;;;PHD 2/19/87 Added pattern for "2 days <direction>. jan 30"
;;;(fixes a bug where 2 days till christmas is parsed as 27-dec)
(defpattern main ((fixp) (get offset) (get direction)) main (offset-value offset-units direction)
		  (set-offset (get direction 'direction) offset-value offset-units))
;;; "2 minutes past 3 pm"

(defpattern main ((fixp) (get offset) past) main (offset-value offset-units)
            (set-offset '+ offset-value offset-units)) 

;;; "half past 3pm"

(defpattern main ((fraction) past) main (hour-value) (set-offset '+ hour-value 'hours)) 

;;; "20 past 3 pm"

(defpattern main ((fixp) past) main (minute-value) (set-offset '+ minute-value 'minutes)) 

;;; "2 minutes of 3 pm"

(defpattern main ((fixp) (get offset) (get direction)) main (offset-value ignore offset-units)
            ignore (set-offset '- offset-value offset-units)) 

;;; "a quarter of 3pm"

(defpattern main ((fraction t) (get direction)) main (hour-value ignore)
            ignore (set-offset '- hour-value 'hours)) 

;;; "20 of 3 pm"

(defpattern main ((fixp) (get direction)) main (minute-value ignore)
            ignore (set-offset '- minute-value 'minutes)) 

;;; "The day before yesterday" or "day before yesterday"

(defpattern main ((get offset) before) main (offset-units) (set-offset  '- '(1 1) offset-units))

;;; "2 days after jan 15"

(defpattern main ((fixp) (get offset) after) main (offset-value offset-units)
            (set-offset '+ offset-value offset-units)) 

;;; "The day after jan 15", "day after tomorrow"

(defpattern main ((get offset) after) main (offset-units) (set-offset '+ '(1 1) offset-units)) 

;;; "5 minutes from now"

(defpattern main ((fixp) (get offset) from) main (offset-value offset-units)
            (set-offset '+ offset-value offset-units)) 

;;; "3 days ago"

(defpattern main ((fixp) (get offset) ago) main (offset-value offset-units) (set-now)
            (set-offset '- offset-value offset-units)) 

;;; "dlw's birthday"

(defpattern main ((any) s birthday) main (name) (set-birthday name)) 

;;; "my birthday"

(defpattern main (my birthday) main () (set-birthday user-id)) 

;;; 11.30 works like 11:30; this is a European form.

(defpattern main ((fixp 1 2) |.| (fixp 2)) second (hour minute) (set-hour hour)
            (set-minute minute)) 

;;; Ed says that Agatha Christie books use 11.3 to mean 11:30:00, also.

(defpattern main ((fixp 1 2) |.| (fixp 1)) second (hour tens-of-minutes) (set-hour hour)
            (set-tens-of-minutes tens-of-minutes)) 

;;; 12 Noon and friends.
;;; This must follow "3 minutes from ...", which includes "12 m from ...".

(defpattern main (12 (get half-day)) main (half-day) (set-half-day half-day)) 

;;; Noon and friends.

(defpattern main ((get half-day)) main (half-day) (set-half-day half-day)) 

;;; - 3 minutes

(defpattern main ((get sign) (fixp) (get offset)) main (sign offset-value offset-units)
            (set-offset sign offset-value offset-units)) 

;;; 3 minutes

(defpattern main ((fixp) (get offset)) main (offset-value offset-units)
            (set-offset '+ offset-value offset-units)) 

;;; Time zones

(defpattern main ((get time-zone)) main (time-zone) (set-time-zone time-zone)) 

;;; Time zones preceeded by a hyphen

(defpattern main (- (get time-zone)) main (time-zone) (set-time-zone time-zone)) 

;; These patterns added by Hdt 8/23/82


(defpattern main (christmas) main () (set-christmas)) 


(defpattern main (halloween) main () (set-halloween)) 


(defpattern main (new years) main () (set-new-years)) 


(defpattern main (new years day) main () (set-new-years)) 

;;; If we encounter random commas in MAIN state, we have to just ignore them
;;; in order to win in such cases as "Thursday, 21 May 1981, 00:27-EDT"

(defpattern main (|,|) main ()) 

(defpattern main ((get direction)) main (direction)
            (move-abs-to-offset (get direction 'direction))) 

;;; Time zones preceeded by a + ,which means offset relative to GMT.

(defpattern main (+ (decode-time-zone time-zone)) main (time-zone) (set-encoded-time-zone time-zone))

;;; Time zones preceeded by a - ,which means offset relative to GMT.

(defpattern main (- (decode-time-zone time-zone t)) main (time-zone) (set-encoded-time-zone time-zone  t ))

;;; Anything else

;; Random THE's (instead of considering them to be noise (using *NOISE-WORDS*)). - WESTY

(defpattern main (the) main ())

(defpattern main ((any)) main (token)
            (barf "Unrecognized date/time format, starting with token ~A." token)) 

;;; If nothing is left and we are in MAIN state, that is the end.

(setf (get 'main 'final-state) 't) 

;;; We just saw "Jan 23", look for a comma followed by a year, e.g. "Jan 23, 80"

(defpattern year-comma (|,| (fixp 2)) main (year-of-century)
            (set-year-of-century year-of-century)) 

;;; We just saw "Jan 23", look for a comma followed by a year, e.g. "Jan 23, 1980"

(defpattern year-comma (|,| (fixp 4)) main (year) (set-year year)) 

;;; If there isn't a comma, go look for the regular kinds of years.

(defpattern year-comma () year ()) 

;many of the fixed dates would best be implimented setting up
;an array to be searched...

;;; We are now in the state of looking for a year.  If we see a number,
;;; that may be a year or it may be the start of something else.  For
;;; example, "6 Jan 59" versus "6 Jan 59 minutes" or "6 Jan 3:23:12".
;;; So we have to look ahead for various possibilities and return to
;;; the main state if any of them are happening.  Otherwise, a number
;;; gets interpreted as a year in this context.

(defpattern-peek year ((fixp) |.|) main ()) 


(defpattern-peek year ((fixp) /) main ()) 


(defpattern-peek year ((fixp) |:|) main ()) 


(defpattern-peek year ((fixp) (get meridian)) main ()) 


(defpattern-peek year (12 (get half-day)) main ()) 


(defpattern-peek year ((get sign) (fixp) (get offset)) main ()) 


(defpattern-peek year ((fixp) (get offset)) main ()) 


(defpattern-peek year ((fixp) (get month)) main ()
                 (barf "Date and month seen where year expected.")) 



;;; Finally, there is no other way to interpret the number.  If there
;;; is a number it must be a year.

(defpattern year ((fixp)) main (year) (set-year year)) 

;;; Not a number at all.

(defpattern year () main ()) 

;;; This is the end of the patterns.  Don't add new ones after this!

(finish-patterns)

;;;----------------------------------------------------------------------------

;;; Special variables.

;;; These variables hold the time values found in the string.  NIL means
;;; that no such value has been seen yet.

;;; Absolute values.

(DEFVAR *ABS-YEAR*) 

(DEFVAR *ABS-MONTH*) 

(DEFVAR *ABS-DATE*) 

(DEFVAR *ABS-HOUR*) 

(DEFVAR *ABS-MINUTE*) 

(DEFVAR *ABS-SECOND*) 

(DEFVAR *ABS-DAY-OF-THE-WEEK*) 

(DEFVAR *ABS-TIME-ZONE*) 

;;; Relative values, from offsets.

(DEFVAR *REL-YEAR*) 

(DEFVAR *REL-MONTH*) 

(DEFVAR *REL-DATE*) 

(DEFVAR *REL-HOUR*) 

(DEFVAR *REL-MINUTE*) 

(DEFVAR *REL-SECOND*) 

(DEFVAR *REL-DAY-OF-THE-WEEK*) 
;(DEFVAR *REL-TIME-ZONE*)

;;; Values of the "base" time.

(DEFVAR *BASE-YEAR*) 

(DEFVAR *BASE-MONTH*) 

(DEFVAR *BASE-DATE*) 

(DEFVAR *BASE-HOUR*) 

(DEFVAR *BASE-MINUTE*) 

(DEFVAR *BASE-SECOND*) 


(DEFVAR *RELATIVE-P*) 

;;; Action functions.

;;; These are the functions invoked by the bodies of the DEFPATTERNs.

(defun set-encoded-time-zone (time-zone &optional negative)   ;time zone will be a number or a list like (100 4) from parse-1.
  (IF (NOT (NULL *ABS-TIME-ZONE*)) (BARF "Time zone specified twice."))
  (SETQ *ABS-TIME-ZONE*  (if (realp TIME-ZONE) time-zone (decode-time-zone time-zone  negative)))
  )

(Defun decode-time-zone (time-zone &optional negative)      ;new function 12-05-88 DAB
  "Decode a time-zone specified as an offset of GMT, i.e +0100, -0300, etc."
  (cond ((symbolp time-zone)  ;12-20-88 DAB Take care of 10:10:20-GMT etc.
	    (LET ((SYMBOL (INTERN (string time-zone) *TIME-PARSER-PACKAGE*)))
	       (GET SYMBOL 'ZONE-VALUE)))
	(T
	 (let ((rt (car time-zone))
	       tz)
	   (when (integerp rt)
	     (setq tz (truncate rt 100.))
	     (when negative (setq tz (* -1 tz)))
	     (when (> (abs tz) 12.)  ;if it is greater than 12, we need to translated it backforwards after subtracting 12 hrs.
	       (if (plusp tz)
		   (setf tz (* -1 (- 12. (- tz 12.))))
		   (setf tz (- 12. (- (abs tz) 12.)))
		   ))
	     (* -1 tz) ;we need to inverse the number because of the way *timezones* is built.
	     )))))


(DEFUN SET-MONTH-FROM-NAME (MONTH)
  (IF (NOT (NULL *ABS-MONTH*)) (BARF "Month specified twice."))
  (SETQ *ABS-MONTH* (GET MONTH 'VALUE))) 


(DEFUN SET-MONTH (MONTH)
  (IF (NOT (NULL *ABS-MONTH*)) (BARF "Month specified twice."))
  (SETQ *ABS-MONTH* (NUMBER-VALUE MONTH))) 


(DEFUN SET-DATE (DATE)
  (IF (NOT (NULL *ABS-DATE*)) (BARF "Date specified twice."))
  (SETQ *ABS-DATE* (NUMBER-VALUE DATE))) 

;;; Here we have to deal with the incompatibility betweeen U.S. and European
;;; date format.  If either number is greater than 12., then that number
;;; cannot be the month and so must be the date.  Otherwise, default based
;;; on the location of the machine.

(DEFUN SET-MONTH-AND-DATE (FIRST SECOND)
  (SETQ FIRST (NUMBER-VALUE FIRST) SECOND (NUMBER-VALUE SECOND))
  (COND
    ((> FIRST 12) (SETQ *ABS-MONTH* SECOND *ABS-DATE* FIRST))
    ((> SECOND 12) (SETQ *ABS-MONTH* FIRST *ABS-DATE* SECOND))
    ((MEMBER *TIMEZONE* '(1 0 -1 -2 -3 -4 -5) :TEST #'EQ);; Europe, kind of.  (Soneone should check a map, and find out
     ;; how the Soviet write dates, when we enter that market...)
     
     (SETQ *ABS-MONTH* SECOND *ABS-DATE* FIRST))
    (T;; Patriotic American date format.
      (SETQ *ABS-MONTH* FIRST *ABS-DATE* SECOND)))) 

;;; This version takes a fixnum, rather than a two-list.

(DEFUN SET-YEAR-INTERNAL (YEAR)
  (IF (NOT (NULL *ABS-YEAR*)) (BARF "Year specified twice."))
  (SETQ *ABS-YEAR* YEAR)) 


(DEFUN SET-YEAR (YEAR)
  (SET-YEAR-INTERNAL (NUMBER-VALUE YEAR))) 


(DEFUN SET-YEAR-OF-CENTURY (YEAR-OF-CENTURY)
  (SET-YEAR-INTERNAL (+ (NUMBER-VALUE YEAR-OF-CENTURY) (* 100 (FLOOR *BASE-YEAR* 100))))) 	; Multics crockishly assumes 1900.


(DEFUN SET-HHMM (TIME)
  (IF (NOT (NULL *ABS-HOUR*)) (BARF "Hour specified twice."))
  (IF (NOT (NULL *ABS-MINUTE*)) (BARF "Minute specified twice."))
  (SETQ TIME (NUMBER-VALUE TIME) *ABS-HOUR* (FLOOR TIME 100) *ABS-MINUTE*
        (REM (VALUES (FLOOR TIME)) 100))) 


(DEFUN SET-HHMMSS (TIME)
  (IF (NOT (NULL *ABS-HOUR*)) (BARF "Hour specified twice."))
  (IF (NOT (NULL *ABS-MINUTE*)) (BARF "Minute specified twice."))
  (IF (NOT (NULL *ABS-SECOND*)) (BARF "Second specified twice."))
  (SETQ TIME (NUMBER-VALUE TIME) *ABS-HOUR* (FLOOR TIME 10000) TIME
        (- TIME (* *ABS-HOUR* 10000)) *ABS-MINUTE* (FLOOR TIME 100) *ABS-SECOND*
        (REM (VALUES (FLOOR TIME)) 100))) 


(DEFUN SET-HOUR (HOUR)
  (IF (NOT (NULL *ABS-HOUR*)) (BARF "Hour specified twice."))
  (SETQ *ABS-HOUR* (NUMBER-VALUE HOUR))) 


(DEFUN SET-MINUTE (MINUTE)
  (IF (NOT (NULL *ABS-MINUTE*)) (BARF "Minute specified twice."))
  (SETQ *ABS-MINUTE* (NUMBER-VALUE MINUTE))) 


(DEFUN SET-TENS-OF-MINUTES (TENS-OF-MINUTES)
  (IF (NOT (NULL *ABS-MINUTE*)) (BARF "Minute specified twice."))
  (SETQ *ABS-MINUTE* (* 10 (NUMBER-VALUE TENS-OF-MINUTES)))) 


(DEFUN SET-SECOND (SECOND)
  (IF (NOT (NULL *ABS-SECOND*)) (BARF "Second specified twice."))
  (SETQ *ABS-SECOND* (NUMBER-VALUE SECOND))) 


(DEFUN SET-TENTHS-OF-MINUTE (TENTHS)
  (IF (NOT (NULL *ABS-SECOND*)) (BARF "Second specified twice."))
  (SETQ *ABS-SECOND* (* 6 (NUMBER-VALUE TENTHS)))) 


(DEFUN SET-MERIDIAN (MERIDIAN)
  (IF (OR (NOT (NUMBERP *ABS-HOUR*)) (< *ABS-HOUR* 0) (> *ABS-HOUR* 12))
      (BARF "Meridian value ~A seen in bad context." MERIDIAN))
  (SETQ *ABS-HOUR*
        (IF (EQ (GET MERIDIAN 'VALUE) 'PM) (IF (= *ABS-HOUR* 12) 12 (+ *ABS-HOUR* 12))
            (IF (= *ABS-HOUR* 12) 0 *ABS-HOUR*)))) 


(DEFUN SET-HALF-DAY (HALF-DAY)
  (IF (NOT (NULL *ABS-SECOND*))
      (BARF "Second specified twice, by the half-day value \"~A\"." HALF-DAY))
  (IF (NOT (NULL *ABS-HOUR*))
      (BARF "Hour specified twice, by the half-day value \"~A\"." HALF-DAY))
  (IF (NOT (NULL *ABS-MINUTE*))
      (BARF "Minute specified twice, by the half-day value \"~A\"." HALF-DAY))
  (SETQ *ABS-HOUR* (IF (EQ (GET HALF-DAY 'VALUE) 'NOON) 12 0) *ABS-MINUTE* 0 *ABS-SECOND* 0)) 


(DEFUN SET-DAY-OF-THE-WEEK (DAY-OF-THE-WEEK)
  (IF (NOT (NULL *ABS-DAY-OF-THE-WEEK*)) (BARF "Day of the week specified twice."))
  (SETQ *ABS-DAY-OF-THE-WEEK* (GET DAY-OF-THE-WEEK 'VALUE))) 


(DEFUN SET-TIME-ZONE (TIME-ZONE)
  (IF (NOT (NULL *ABS-TIME-ZONE*)) (BARF "Time zone specified twice."))
  (SETQ *ABS-TIME-ZONE* (GET TIME-ZONE 'ZONE-VALUE))) 

(defun set-offset (sign value units)
  (setf *last-offset-direction* sign) ;; Added this to help in `THE HOUR' parsing. - WESTY
  (let ((value (* (number-value value) (if (eq sign '+) 1 -1))))
    (case (get units 'offset-value)
      (years (setq *rel-year* (+ *rel-year* value)))
      (months (setq *rel-month* (+ *rel-month* value)))
      (weeks (setq *rel-date* (+ *rel-date* (* 7 value))))
      (days (setq *rel-date* (+ *rel-date* value)))
      (hours (setq *rel-hour* (+ *rel-hour* value)))
      (minutes (setq *rel-minute* (+ *rel-minute* value)))
      (seconds (setq *rel-second* (+ *rel-second* value)))
      (otherwise (barf "Bad units" units)))))

;(DEFUN SET-OFFSET (SIGN VALUE UNITS)
;  (LET ((VALUE (* (NUMBER-VALUE VALUE) (IF (EQ SIGN '+) 1 -1))))
;    (CASE (GET UNITS 'OFFSET-VALUE)
;      (YEARS (SETQ *REL-YEAR* (+ *REL-YEAR* VALUE)))
;      (MONTHS (SETQ *REL-MONTH* (+ *REL-MONTH* VALUE)))
;      (WEEKS (SETQ *REL-DATE* (+ *REL-DATE* (* 7 VALUE))))
;      (DAYS (SETQ *REL-DATE* (+ *REL-DATE* VALUE)))
;      (HOURS (SETQ *REL-HOUR* (+ *REL-HOUR* VALUE)))
;      (MINUTES (SETQ *REL-MINUTE* (+ *REL-MINUTE* VALUE)))
;      (SECONDS (SETQ *REL-SECOND* (+ *REL-SECOND* VALUE)))
;      (OTHERWISE (BARF "Bad units" UNITS))))) 

;; Used in "half past the hour" or "20 minutes before the hour" - WESTY
(defun set-past-hour ()
  (setq *abs-hour* *base-hour*)
  (setq *abs-date* *base-date*)
  (setq *abs-month* *base-month*)
  (setq *abs-year* *base-year*)
  (setq *relative-p* :relative))

;; Used in "half past the hour" or "20 minutes before the hour" - WESTY
(defun set-next-hour ()
  (setq *abs-hour* (1+ *base-hour*))
  (setq *abs-date* *base-date*)
  (setq *abs-month* *base-month*)
  (setq *abs-year* *base-year*)
  (setq *relative-p* :relative))

;Used in handling "2:30 from now".
;Turn the time we have so far into an offset,
;and clear out the absolute time.

(DEFUN MOVE-ABS-TO-OFFSET (SIGN)
  (AND *ABS-YEAR* (SETQ *REL-YEAR* (FUNCALL SIGN (OR *REL-YEAR* 0) *ABS-YEAR*)))
  (AND *ABS-MONTH* (SETQ *REL-MONTH* (FUNCALL SIGN (OR *REL-MONTH* 0) *ABS-MONTH*)))
  (AND *ABS-DATE* (SETQ *REL-DATE* (FUNCALL SIGN (OR *REL-DATE* 0) *ABS-DATE*)))
  (AND *ABS-HOUR* (SETQ *REL-HOUR* (FUNCALL SIGN (OR *REL-HOUR* 0) *ABS-HOUR*)))
  (AND *ABS-MINUTE* (SETQ *REL-MINUTE* (FUNCALL SIGN (OR *REL-MINUTE* 0) *ABS-MINUTE*)))
  (AND *ABS-SECOND* (SETQ *REL-SECOND* (FUNCALL SIGN (OR *REL-SECOND* 0) *ABS-SECOND*)))
  (SETQ *ABS-YEAR* () *ABS-MONTH* () *ABS-DATE* () *ABS-HOUR* () *ABS-MINUTE* () *ABS-SECOND* ())
  (SETQ *RELATIVE-P* :RELAVTIVE)) 


(DEFUN SET-TODAY ()
  (SETQ *ABS-DATE* *BASE-DATE*)
  (SETQ *ABS-MONTH* *BASE-MONTH*)
  (SETQ *ABS-YEAR* *BASE-YEAR*)
  (SETQ *RELATIVE-P* :RELATIVE)) 


(DEFUN SET-YESTERDAY ()
  (SETQ *ABS-DATE* *BASE-DATE*)
  (SETQ *REL-DATE* (1- *REL-DATE*))
  (SETQ *ABS-MONTH* *BASE-MONTH*)
  (SETQ *ABS-YEAR* *BASE-YEAR*)
  (SETQ *RELATIVE-P* :RELATIVE)) 


(DEFUN SET-TOMORROW ()
  (SETQ *ABS-DATE* *BASE-DATE*)
  (SETQ *REL-DATE* (1+ *REL-DATE*))
  (SETQ *ABS-MONTH* *BASE-MONTH*)
  (SETQ *ABS-YEAR* *BASE-YEAR*)
  (SETQ *RELATIVE-P* :RELATIVE)) 



(DEFUN SET-NOW ()
  (SETQ *ABS-SECOND* *BASE-SECOND*)
  (SETQ *ABS-MINUTE* *BASE-MINUTE*)
  (SETQ *ABS-HOUR* *BASE-HOUR*)
  (SETQ *ABS-DATE* *BASE-DATE*)
  (SETQ *ABS-MONTH* *BASE-MONTH*)
  (SETQ *ABS-YEAR* *BASE-YEAR*)
  (SETQ *RELATIVE-P* :RELATIVE)) 

(DEFUN SET-CHRISTMAS ()
  (SETQ *ABS-SECOND* 0)
  (SETQ *ABS-MINUTE* 0)
  (SETQ *ABS-HOUR* 0)
  (SETQ *ABS-DATE* 25)
  (SETQ *ABS-MONTH* 12)
  (SETQ *ABS-YEAR*
        (COND
          ((AND (= *BASE-MONTH* 12) (> *BASE-DATE* 25)) (1+ *BASE-YEAR*))
          (T *BASE-YEAR*)));if after December 25, then next year
  ) 

(defun set-halloween ()
  (setq *abs-second* 0)
  (setq *abs-minute* 0)
  (setq *abs-hour* 0)
  (setq *abs-date* 31)
  (setq *abs-month* 10)
  ;; Fixed dumb copy-edit bug. -Westy
;  (setq *abs-year*
;        (cond
;          ((and (= *base-month* 12) (> *base-date* 25)) (1+ *base-year*))
;          (t *base-year*)));if after December 25, then next year
  (setq *abs-year*
        ;; If after halloween this year then next year.
        (if (and (= *base-month* 11) (>= *base-date* 0)) (1+ *base-year*) *base-year*)))

(DEFUN SET-NEW-YEARS ()
  (SETQ *ABS-SECOND* *BASE-SECOND*)
  (SETQ *ABS-MINUTE* *BASE-MINUTE*)
  (SETQ *ABS-HOUR* *BASE-HOUR*)
  (SETQ *ABS-DATE* *BASE-DATE*)
  (SETQ *ABS-MONTH* *BASE-MONTH*)
  (SETQ *ABS-YEAR* *BASE-YEAR*)
  (SETQ *RELATIVE-P* :RELATIVE)) 

(DEFUN SET-BIRTHDAY (USER-ID)
  (PARSE-1
   (LEXICALLY-ANALYZE
    "7/1/64")
   'MAIN))
  

;;; Top level.

;;; These are the top level functions and external entrypoints that call
;;; the lexical analyzer and parser; the parser calls the action routines.
;;; Any of these callees may call BARF to report an error; BARF is guaranteed
;;; to THROW out, and therefore not return to its caller.

;;; Documented functions.

(export '(parse parse-universal-time print-interval-or-never parse-interval-or-never read-interval-or-never))

(DEFMACRO CHECK-RANGE (VARIABLE LOWER UPPER STRING)
  `(IF (OR (< ,VARIABLE ,LOWER) (> ,VARIABLE ,UPPER))
     (BARF "~D is ~:[more~;less~] than the number of ~A." ,VARIABLE (< ,VARIABLE ,LOWER) ,STRING))) 


(DEFUN PARSE (STRING &OPTIONAL (START 0) END (FUTUREP T) BASE-TIME MUST-HAVE-TIME DATE-MUST-HAVE-YEAR
         TIME-MUST-HAVE-SECOND (DAY-MUST-BE-VALID T))
  (DECLARE
   (VALUES SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-P RELATIVE-P))
  (MULTIPLE-VALUE-BIND (ANSWER RELATIVE-P) (PARSE-UNIVERSAL-TIME STRING START END FUTUREP BASE-TIME MUST-HAVE-TIME DATE-MUST-HAVE-YEAR
                         TIME-MUST-HAVE-SECOND DAY-MUST-BE-VALID)
    (MULTIPLE-VALUE-BIND (SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-P) (DECODE-UNIVERSAL-TIME ANSWER)
      (VALUES SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-P RELATIVE-P)))) 

;; Copied from SYS:PATCH.SYSTEM;PATCH-6-45

(DEFUN PARSE-UNIVERSAL-TIME (STRING &OPTIONAL (START 0) END (FUTUREP T) BASE-TIME MUST-HAVE-TIME DATE-MUST-HAVE-YEAR
         TIME-MUST-HAVE-SECOND (DAY-MUST-BE-VALID T))
  "Return a universal-time parsed from STRING, or the part from START to END.
FUTUREP controls the interpretation if there is just a day-of-the-week:
 T means use the next such day, NIL means use the previous.
BASE-TIME is used if the string is a relative time.
 It is what the relative time is relative to.  Default is now.
MUST-HAVE-TIME if T means error if the string is empty.
DATE-MUST-HAVE-YEAR if T means error if no year number.
TIME-MUST-HAVE-SECOND if T means error if time doesn't
 include a number of seconds.
DAY-MUST-BE-VALID if NIL means allow things like February 29
 (which equals March 1 or March 2)."
#+nil  (DECLARE (VALUES UNIVERSAL-TIME RELATIVE-P))
  (BLOCK ()
    (BLOCK KLUDGE
      (IF (AND MUST-HAVE-TIME (EQUAL STRING ""))
          (ERROR "PARSE-ERROR: The supplied time string is empty."))
      (IF (NULL END) (SETQ END (LENGTH STRING)))
      (LET ((TEM (PARSE-TWENEX-TIME STRING START END)))
        (IF TEM (RETURN (VALUES TEM ()))))
      (LET (*ABS-YEAR* *ABS-MONTH* *ABS-DATE* *ABS-HOUR* *ABS-MINUTE* *ABS-SECOND*
            *ABS-DAY-OF-THE-WEEK* *ABS-TIME-ZONE* (*REL-YEAR* 0) (*REL-MONTH* 0) (*REL-DATE* 0)
            (*REL-HOUR* 0) (*REL-MINUTE* 0) (*REL-SECOND* 0) *REL-DAY-OF-THE-WEEK*;	     *REL-TIME-ZONE*
                                                              *BASE-YEAR*
            *BASE-MONTH* *BASE-DATE* *BASE-HOUR* *BASE-MINUTE* *BASE-SECOND* *RELATIVE-P*
            );; Compute the "base" time: the time to which the string is relative.
            
        (COND
          ((NULL BASE-TIME);; Time is relative to right now.
           
           (MULTIPLE-VALUE-SETQ (*BASE-SECOND* *BASE-MINUTE* *BASE-HOUR* *BASE-DATE* *BASE-MONTH* *BASE-YEAR*)
             (get-decoded-time));; If the time is not known, assume a default base time so that we
           ;; can still parse fully-specified date/times (e.g. in the file system)
           
           (IF (NULL *BASE-SECOND*)
               (SETQ *BASE-SECOND* 0 *BASE-MINUTE* 0 *BASE-HOUR* 0 *BASE-DATE* 1 *BASE-MONTH* 1
                     *BASE-YEAR* 0)))
          (T;; Time is relative to a specified time.
           
           (MULTIPLE-VALUE-SETQ (*BASE-SECOND* *BASE-MINUTE* *BASE-HOUR* *BASE-DATE* *BASE-MONTH* *BASE-YEAR*)
             (DECODE-UNIVERSAL-TIME BASE-TIME))));; Do the parse, calling the action routines, which work by setting the
        ;; ABS and REL special variables bound above.
        
        (PARSE-1 (DELQ-ALL (LEXICALLY-ANALYZE STRING START END) *NOISE-WORDS*) 'MAIN)
        (IF (AND DATE-MUST-HAVE-YEAR (NULL *ABS-YEAR*)) (BARF "no year supplied"))
        (IF (AND TIME-MUST-HAVE-SECOND (NULL *ABS-SECOND*)) (BARF "no seconds supplied"));; Now apply lots of defaults.
        ;; There are many terms, from the lowest order (seconds) to the highest
        ;; order (years).  A legal date must specify some contiguous subsequence
        ;; of these.  The low unspecified ones get zeroed; the high unspecified
        ;; ones are either the next in the future or the previous in the past.
        ;; Time zones and days of the week are handled specially.
        ;; First, the following code allows a day of the week to be used to
        ;; specify a year, month, and date, when it is supposed to.
        
        (IF
         (AND (NULL *ABS-YEAR*) (NULL *ABS-MONTH*) (NULL *ABS-DATE*)
              (NOT (NULL *ABS-DAY-OF-THE-WEEK*)));; Day of week specified the year, month, and date.
         
         (LET ((UT (ENCODE-UNIVERSAL-TIME 0 0 0 *BASE-DATE* *BASE-MONTH* *BASE-YEAR*)))
           (MULTIPLE-VALUE-BIND (ignore1 ignore2 ignore3 ignore4 ignore5 ignore6 BASE-DAY-OF-THE-WEEK)
                                (DECODE-UNIVERSAL-TIME UT)
             (declare (ignore ignore1 ignore2 ignore3 ignore4 ignore5 ignore6))
             (LET ((DELTA-DAYS (- *ABS-DAY-OF-THE-WEEK* BASE-DAY-OF-THE-WEEK))
                   ignore-s ignore-m ignore-h)
               (IF FUTUREP (DO ()
                               ((> DELTA-DAYS 0))
                             (SETQ DELTA-DAYS (+ DELTA-DAYS 7)))
                   (DO ()
                       ((< DELTA-DAYS 0))
                     (SETQ DELTA-DAYS (- DELTA-DAYS 7))))
               (MULTIPLE-VALUE-SETQ (ignore-s ignore-m ignore-h *ABS-DATE* *ABS-MONTH* *ABS-YEAR*)
                 (COMPUTE-RELATIVE 0 0 0 (+ *BASE-DATE* DELTA-DAYS) *BASE-MONTH* *BASE-YEAR*))
               ; since I can not get declarations to work.
               ignore-h ignore-m ignore-s))));; If everything was specified (as in a date read from a file server)
        ;; then skip worrying about defaulting.
        
        (OR
         (AND *ABS-YEAR* *ABS-MONTH* *ABS-DATE* *ABS-HOUR* *ABS-MINUTE* *ABS-SECOND*);; Non-specified low-order terms get set to zero (or the moral equivalent
         ;; of zero), up to the first speicified term.
         
         (DO ((TERMS '(*ABS-SECOND* *ABS-MINUTE* *ABS-HOUR* *ABS-DATE* *ABS-MONTH* *ABS-YEAR*)
               (CDR TERMS))
              (BASE-TERMS
               '(*BASE-SECOND* *BASE-MINUTE* *BASE-HOUR* *BASE-DATE* *BASE-MONTH* *BASE-YEAR*)
               (CDR BASE-TERMS))
              (LOWEST '(0 0 0 1 1 -100000000) (CDR LOWEST))
              (HIGHEST '(59 59 23 NIL 12 100000000) (CDR HIGHEST))
              (STATE 'DEFAULT-LOW-TERMS)
              (COMPARISON 'EQUAL)
              (OPERATION NIL))
             ((NULL TERMS) (IF (EQ STATE 'DEFAULT-LOW-TERMS) (BARF "No time was specified.")))
           RESTART
           (LET ((TERM-VALUE (SYMBOL-VALUE (CAR TERMS)))
                 (BASE-TERM-VALUE (SYMBOL-VALUE (CAR BASE-TERMS))))
             (CASE STATE
               (DEFAULT-LOW-TERMS;; Non-specified low-order terms get set to default values, which
                ;; are zero or one depending on whether the quantity is zero-based
                ;; or one-based.
                
                (COND
                  ((NULL TERM-VALUE);; Term is non-specified, default it.
                    (SET (CAR TERMS) (CAR LOWEST)))
                  (T;; Term is specified: go to the next state and try again.
                    (SETQ STATE 'SKIP-OVER-SPECIFIED) (GO RESTART))))
               (SKIP-OVER-SPECIFIED;; Now we are moving over the contiguous subsequence of values
                ;; specified by the user.
                
                (COND
                  ((NOT (NULL TERM-VALUE));; This value was specified by the user.
                   
                   (COND
                     ((> TERM-VALUE BASE-TERM-VALUE);; Specified time is later than the base time.
                       (SETQ COMPARISON 'LATER))
                     ((< TERM-VALUE BASE-TERM-VALUE);; Specified time is earlier than the base time.
                       (SETQ COMPARISON 'EARLIER));; If these terms are equal, use the old value of
                     ;;   COMPARISON based on the lower order terms.
                     ))
                  (T;; Term is not specified; go to the next state and try again.
                   ;; This SETQ is documented at the next state.
                   
                   (SETQ OPERATION
                         (CASE COMPARISON
                           (EQUAL;; The specified and base times are equal, do nothing.
                             'EQUAL)
                           (LATER;; Specified time is later than base time.
                             (IF FUTUREP 'EQUAL 'SUB1))
                           (EARLIER;; Specified time is earlier than base time.
                             (IF FUTUREP 'ADD1 'EQUAL))))
                   (SETQ STATE 'DEFAULT-HIGH-TERMS) (GO RESTART))))
               (DEFAULT-HIGH-TERMS;; Non-specified high-order terms come from the base time.  The
                ;; tricky thing is that we may have to add or subtract one, depending
                ;; on FUTUREP and COMPARISON, which requires propagating carry or
                ;; borrow.  This information is encoded in OPERATION, which is SETQed
                ;; above (so that we don't do it each time around the loop!).
                
                (IF (NOT (NULL TERM-VALUE));; Foo, the rest of the high-order terms have to be unspecified.
                     (BARF "Unrecognized pattern of defaulting."))
                (CASE OPERATION
                  (EQUAL;; We are just copying base time into abs time.  Keep doing it.
                    (SET (CAR TERMS) BASE-TERM-VALUE))
                  (ADD1;; Set this term one higher than it is in the base time.
                   
                   (LET ((HIGHEST-VALUE;; Compute the highest legal value for this term.
                          
                          (IF (EQ (CAR TERMS) '*ABS-DATE*);; Highest possible value for dates depends on
                              ;; which month this is.
			      (MONTH-LENGTH *BASE-MONTH* *BASE-YEAR*);; Other highest values are just constants.
			      (CAR HIGHEST))))
                     (COND
                       ((< BASE-TERM-VALUE HIGHEST-VALUE);; No carry.  Just add one, and copy the rest.
                        
                        (SET (CAR TERMS) (1+ BASE-TERM-VALUE)) (SETQ OPERATION 'EQUAL))
                       (T;; Carry into next term.
                         (SET (CAR TERMS) (CAR LOWEST))))))
                  (SUB1;; Set this term one lower than it is in the base time.
                   
                   (COND
                     ((> BASE-TERM-VALUE (CAR LOWEST));; No borrow.  Just subtract one, and copy the rest.
                       (SET (CAR TERMS) (1- BASE-TERM-VALUE))
                      (SETQ OPERATION 'EQUAL))
                     (T;; Borrow from the next term.
                      
                      (SET (CAR TERMS)
                           (IF (EQ (CAR TERMS) '*ABS-DATE*);; Highest possible value for dates depends on
                               ;; which month this is.
                               
                               (MONTH-LENGTH
				 (if (eq *base-date* 1)
				     ;;DAB 12-01-89 This case occurs when time is in previous day. The Month-lenght
                                     ;;needs to depend upon last month. [10827] 
				     (let ((val (1- *BASE-MONTH*)))	;; may 01/04/91 wrap 0 to 12 after subtraction
				       (if (zerop val) 12. val))
				     *BASE-MONTH*)
				 *BASE-YEAR*);; Other highest values are just constants.
                                (CAR HIGHEST))))))
                  (OTHERWISE (error "Bad value of OPERATION ~S" OPERATION))))
               (OTHERWISE (error "Bad value of STATE ~S" STATE))))));; Now hack other random defaults.
        ;(IF (NULL *ABS-TIME-ZONE*)
        ;  (SETQ *ABS-TIME-ZONE* 0
        ;        ))
        ;(SETQ *REL-TIME-ZONE* *ABS-TIME-ZONE*)
        ;; Check ranges.
        
        (CHECK-RANGE *ABS-SECOND* 0 59 "seconds in a minute")
        (CHECK-RANGE *ABS-MINUTE* 0 59 "minutes in an hour")
        (CHECK-RANGE *ABS-HOUR* 0 23 "hours in a day");Check this before MONTH-STRING call!
        
        (CHECK-RANGE *ABS-MONTH* 1 12 "months in a year")
        (CHECK-RANGE *ABS-DATE* 1 (MONTH-LENGTH *ABS-MONTH* *ABS-YEAR*)
                     (FORMAT () "days in ~A" (MONTH-STRING *ABS-MONTH*)))
        (IF (AND DAY-MUST-BE-VALID (NOT (NULL *ABS-DAY-OF-THE-WEEK*)))
            (VERIFY-DATE *ABS-DATE* *ABS-MONTH* *ABS-YEAR* *ABS-DAY-OF-THE-WEEK*));; Now put it together.
        
        (MULTIPLE-VALUE-SETQ (*ABS-SECOND* *ABS-MINUTE* *ABS-HOUR* *ABS-DATE* *ABS-MONTH* *ABS-YEAR*)
          (COMPUTE-RELATIVE (+ *ABS-SECOND* *REL-SECOND*) (+ *ABS-MINUTE* *REL-MINUTE*)
                            (+ *ABS-HOUR* *REL-HOUR*) (+ *ABS-DATE* *REL-DATE*)
                            (+ *ABS-MONTH* *REL-MONTH*) (+ *ABS-YEAR* *REL-YEAR*)))
        (RETURN
         (VALUES
          (if *ABS-TIME-ZONE*
            (ENCODE-UNIVERSAL-TIME *ABS-SECOND* *ABS-MINUTE* *ABS-HOUR* *ABS-DATE* *ABS-MONTH*
                                   *ABS-YEAR* *ABS-TIME-ZONE*)
            (ENCODE-UNIVERSAL-TIME *ABS-SECOND* *ABS-MINUTE* *ABS-HOUR* *ABS-DATE* *ABS-MONTH*
                                   *ABS-YEAR*))
          *RELATIVE-P*)))
      ());This is needed because multiple values
    ))

;;; This function will accept dates such as -1,March 1980 and return 28,Febuary 1980
;;; CAL 10/24/80


(DEFUN COMPUTE-RELATIVE (SECOND MINUTE HOUR DATE MONTH YEAR)
  (PROG (M)
    (SETQ SECOND (+ SECOND (* 60 (+ MINUTE (* 60 (+ HOUR (* 24 DATE)))))))
    (SETQ DATE (FLOOR SECOND 86400))
    (SETQ SECOND (- SECOND (* DATE 86400)))
    (SETQ HOUR (FLOOR SECOND 3600))
    (SETQ SECOND (REM (VALUES (FLOOR SECOND)) 3600))
    (SETQ MINUTE (FLOOR SECOND 60))
    (SETQ SECOND (REM SECOND 60))
    (SETQ YEAR (+ YEAR (FLOOR (1- MONTH) 12)))
    (SETQ MONTH (1+ (REM (+ 12 (REM (1- MONTH) 12)) 12)))
    L1
    (SETQ M (MONTH-LENGTH MONTH YEAR))
    (COND
      ((> DATE M) (SETQ DATE (- DATE M)) (SETQ MONTH (1+ MONTH))
       (COND
         ((> MONTH 12) (SETQ MONTH 1) (SETQ YEAR (1+ YEAR)))) (GO L1))
      ((< DATE 1) (SETQ MONTH (1- MONTH))
       (COND
         ((= MONTH 0) (SETQ MONTH 12) (SETQ YEAR (1- YEAR))))
       (SETQ DATE (+ (MONTH-LENGTH MONTH YEAR) DATE)) (GO L1)))
    (RETURN (VALUES SECOND MINUTE HOUR DATE MONTH YEAR)))) 



(DEFUN PARSE-TWENEX-TIME (STRING START END)
  "If STRING (between START and END) is a Twenex file server format date,
return the universal time for it.  Otherwise, return NIL."
  (PROG (IDX
         YEAR
         MONTH
         DATE
         HOUR
         MINUTE
         SECOND
         SUBSTRING)
    (IF (AND (> END START) (eql (AREF STRING START) #\SPACE)) (INCF START))
    (SETQ IDX
          (SEARCH (THE STRING (STRING #\-)) (THE STRING (STRING STRING)) :START2 START :END2 END
                  :TEST #'CHAR-EQUAL))
    (OR IDX (RETURN ()))
    (OR (EQ IDX (STRING-SEARCH-NOT-SET "0123456789" STRING START END)) (RETURN ()))
    (SETQ DATE (PARSE-NUMBER STRING START IDX))
    (SETQ START (1+ IDX));; Now the month name.
    
    (SETQ IDX
          (SEARCH (THE STRING (STRING #\-)) (THE STRING (STRING STRING)) :START2 START :END2 END
                  :TEST #'CHAR-EQUAL))
    (SETQ SUBSTRING (SUBSEQ STRING START IDX))
    (OR (EQ IDX (STRING-SEARCH-SET "0123456789" SUBSTRING)) (RETURN ()))
    (SETQ MONTH
          (1+
           (POSITION (ASSOC SUBSTRING *MONTH-SYMBOLS* :TEST 'EQUALP) (THE LIST *MONTH-SYMBOLS*)
                     :TEST #'EQ)))
    (OR MONTH (RETURN ()))
    (SETQ START (1+ IDX));; Now the year.
    
    (SETQ IDX
          (SEARCH (THE STRING (STRING #\SPACE)) (THE STRING (STRING STRING)) :START2 START :END2
                  END :TEST #'CHAR-EQUAL))
    (OR IDX (RETURN ()))
    (OR (EQ IDX (STRING-SEARCH-NOT-SET "0123456789" STRING START END)) (RETURN ()))
    (SETQ YEAR (PARSE-NUMBER STRING START IDX))
    (SETQ START (1+ IDX));; Now the hour
    
    (SETQ IDX
          (SEARCH (THE STRING (STRING #\:)) (THE STRING (STRING STRING)) :START2 START :END2 END
                  :TEST #'CHAR-EQUAL))
    (OR IDX (RETURN ()))
    (OR (EQ IDX (STRING-SEARCH-NOT-SET "0123456789" STRING START END)) (RETURN ()))
    (SETQ HOUR (PARSE-NUMBER STRING START IDX))
    (SETQ START (1+ IDX));; Now the minute
    
    (SETQ IDX
          (SEARCH (THE STRING (STRING #\:)) (THE STRING (STRING STRING)) :START2 START :END2 END
                  :TEST #'CHAR-EQUAL))
    (OR IDX (RETURN ()))
    (OR (EQ IDX (STRING-SEARCH-NOT-SET "0123456789" STRING START END)) (RETURN ()))
    (SETQ MINUTE (PARSE-NUMBER STRING START IDX))
    (SETQ START (1+ IDX));; Now the second
    
    (OR (EQ END (OR (STRING-SEARCH-NOT-SET "0123456789" STRING START END) END)) (RETURN ()))
    (SETQ SECOND (PARSE-NUMBER STRING START END))
    (RETURN (ENCODE-UNIVERSAL-TIME SECOND MINUTE HOUR DATE MONTH YEAR *TIMEZONE*)))) 


(DEFUN FIND-BIRTHDAY (STRING &AUX X)
  (SETQ X
        (SEARCH (THE STRING (STRING "birthday")) (THE STRING (STRING STRING)) :TEST #'CHAR-EQUAL))
  (IF (NULL X) (BARF "Cannot find \"BIRTHDAY\"."))
  (SUBSEQ STRING (+ 9 X)
             (SEARCH (THE STRING (STRING ";")) (THE STRING (STRING STRING)) :START2 (+ 9 X)
                     :TEST #'CHAR-EQUAL))) 


#+Explorer
(DEFPROP BARF T :ERROR-REPORTER) 

#+Explorer
(DEFPROP BARF T :ERROR-REPORTER) 

(DEFUN BARF (STRING &REST ARGS)
  (error "PARSE-ERROR: ~?" STRING ARGS))

(DEFUN TEST ()
  (DO ((S (READ-LINE) (READ-LINE))
       (NOW (GET-UNIVERSAL-TIME)))
      ((EQUAL S ""))
      #+Explorer
      (CONDITION-CASE (VAL RELATIVE-P) (PARSE-UNIVERSAL-TIME S 0 () T NOW) (ERROR (PRINC VAL))
		      (:NO-ERROR (FORMAT T "~15A" (OR RELATIVE-P "Absolute"))
				 (PRINT-UNIVERSAL-TIME VAL)))
      #-Explorer
      (multiple-value-bind (VAL RELATIVE-P)
	  (PARSE-UNIVERSAL-TIME S 0 () T NOW)
	(FORMAT T "~15A ~A" 
		(OR RELATIVE-P "Absolute") 
		(print-universal-date val nil)))
    (TERPRI)
    (TERPRI))) 

;;; This function should be run whenever you make a major change to the
;;; parser.  It has an exhaustive set of test cases, all of which should
;;; be verified.


(defparameter *test-cases*
              '("March 15, 1985" "15 March 1985" "3/15/85" "15/3/85" "3/15/1985" "3-15-85"
               "15-3-1985" "3-15" "3-March-85" "3-Mar-85" "March-3-85" "1130." "11:30"
               "11:30:17" "11:30 pm" "11:30 AM" "1130" "113000" "11.30" "11.30.00" "11.3"
               "11 pm" "12 noon" "midnight" "m" "Friday, March 15, 1980" "6:00 gmt" "3:00 pdt"
               ;; Added these to test DAB patches of 12/88. - Westy
               "Fri, 5 Apr 91 15:18:28 -0500" "10:10:20-GMT" "10:10:20+0000" 
               "15 March 85" "15 march 85 seconds" "Fifteen March 85"
               "The Fifteenth of March, 1985;" "Thursday, 21 May 1981, 00:27-EDT"
               "One minute after March 3, 1985" "Three days ago" "5 hours ago"
               "Two days after March 3, 1985" "Three minutes after 23:59:59 Dec 31, 1984" "Now"
               "Today" "Yesterday" "two days after tomorrow" "one day before yesterday"
               "the day after tomorrow" "half past noon" "half a minute past noon"
               "20 past noon" "a quarter of an hour from now" "2.5 days from now"
               "2.5 hours after tomorrow" ".5 days from now" "2 and a half days from now"
               "2 hours and 20 minutes from tomorrow" "5h3m from tomorrow"
               ;; Added these to test my additions. - Westy
               "the hour" "half past the hour" "20 minutes till the hour"
               ;; Added these to test the goofy stuff additions. - Westy
               "halloween" "christmas"
               ;; Leave these last in case server is down!
               "my birthday"
               "the day before my birthday" "1 hour before dlw's birthday"))

(DEFUN TEST-PARSER ()
  (TERPRI)
  (DOLIST (CASE *TEST-CASES*)
	  (FORMAT T "~40A   " CASE)
	  #+Explorer
	  (CONDITION-CASE (VAL RELATIVE-P) (PARSE-UNIVERSAL-TIME CASE) (ERROR (PRINC VAL))
			  (:NO-ERROR (FORMAT T "~15A" (OR RELATIVE-P "Absolute"))
				     (PRINT-UNIVERSAL-TIME VAL)))
	  #-Explorer
	  (multiple-value-bind (VAL RELATIVE-P)
	      (PARSE-UNIVERSAL-TIME CASE)
	    (FORMAT T "~15A ~A" 
		    (OR RELATIVE-P "Absolute") 
		    (print-universal-date val nil)))
	  (TERPRI)))
      


;;; Time interval stuff.

(DEFVAR TIME-INTERVAL-ARRAY (MAKE-ARRAY '(50 2))) 


(DEFVAR TIME-INTERVAL-UNIT-TYPES 0) 


(DEFUN TIME-INTERVAL-TO-SECONDS (STRING &AUX (TOTAL 0))
  "Return a number of seconds parsed from STRING.
If the string cannot be parsed, the first value is NIL
and the second is a string describing the problem."
  (IF (NUMBERP STRING) STRING
      (DO ((IX 0)
           (L (LENGTH STRING)))
          ((OR (NULL IX) (>= IX L)) TOTAL)
        (LET ((TOKEN-START
               (POSITION #\SPACE (THE STRING (STRING STRING)) :START IX :TEST-NOT #'CHAR-EQUAL)))
          (IF (NULL TOKEN-START) (RETURN TOTAL))
          (LET* ((TOKEN-END
                  (POSITION #\SPACE (THE STRING (STRING STRING)) :START TOKEN-START :TEST
                            #'CHAR-EQUAL));;works even if end nil!
                 
                 (UNITS (PARSE-NUMBER STRING TOKEN-START TOKEN-END)))
            (IF (NULL UNITS)
                (RETURN
                 (VALUES ()
                         (FORMAT () "Invalid number: ~A"
                                 (SUBSEQ STRING TOKEN-START TOKEN-END)))))
            (LET ((TOKEN-START
                   (POSITION #\SPACE (THE STRING (STRING STRING)) :START TOKEN-END :TEST-NOT
                             #'CHAR-EQUAL)))
              (IF (NULL TOKEN-START)
                  (RETURN (VALUES () "Units specification missing from time string")))
              (SETQ IX
                    (POSITION #\SPACE (THE STRING (STRING STRING)) :START TOKEN-START :TEST
                              #'CHAR-EQUAL))
              (LET ((UVAL
                     (LOOP FOR I FROM 0 BELOW TIME-INTERVAL-UNIT-TYPES FINALLY (RETURN ()) DO
                           (IF
                            (STRING-EQUAL (AREF TIME-INTERVAL-ARRAY I 0) STRING :start1 0 :start2 TOKEN-START
					  :end1 () :end2 IX)
                            (RETURN (AREF TIME-INTERVAL-ARRAY I 1))))))
                (IF UVAL
                    (PROGN
                      (IF (CHAR-EQUAL #\y (AREF STRING TOKEN-START));years?
                          
                          (IF (> UNITS 3);good till 1999.
                              
                              (INCF TOTAL (* (FLOOR UNITS 4) (TIME-INTERVAL-TO-SECONDS "1 day")))))
                      (INCF TOTAL (* UVAL UNITS)))
                    (RETURN
                     (VALUES ()
                             (FORMAT () "Unknown time spec: ~A"
                                     (SUBSEQ STRING TOKEN-START IX)))))))))))) 


(DEFUN INIT-TIME-INTERVAL-ARRAY ()
  (SETF (AREF TIME-INTERVAL-ARRAY 0 0) "second")
  (SETF (AREF TIME-INTERVAL-ARRAY 0 1) 1)
  (SETQ TIME-INTERVAL-UNIT-TYPES 1)
  (DOLIST (L
    '(("1 second" "seconds" "s" "sec" "secs") ("60 seconds" "minute" "minutes" "min" "mins" "m")
     ("60 minutes" "hour" "hours" "hr" "hrs" "h") ("24 hours" "day" "days")
     ("7 days" "week" "weeks" "wk" "wks") ("365 days" "year" "years" "yr" "yrs")))
    (LET ((VALUE (TIME-INTERVAL-TO-SECONDS (CAR L))))
      (DOLIST (NEWNAME (CDR L))
        (SETF (AREF TIME-INTERVAL-ARRAY TIME-INTERVAL-UNIT-TYPES 0) NEWNAME)
        (SETF (AREF TIME-INTERVAL-ARRAY TIME-INTERVAL-UNIT-TYPES 1) VALUE)
        (INCF TIME-INTERVAL-UNIT-TYPES))))) 


(INIT-TIME-INTERVAL-ARRAY) 


(DEFUN SECONDS-TO-INTERVAL-STRING (SECS)
  "Return a string describing a time interval SECS in seconds."
  (IF (ZEROP SECS) "0 seconds"
      (DO ((I 0 (1+ I))
           (LAST ()))
          ((>= I TIME-INTERVAL-UNIT-TYPES) (SECONDS-TO-INTERVAL-STRING-1 LAST SECS))
        (IF (> (AREF TIME-INTERVAL-ARRAY I 1) SECS)
            (RETURN (SECONDS-TO-INTERVAL-STRING-1 LAST SECS))
            (IF
             (OR (NULL LAST)
                 (NOT (= (AREF TIME-INTERVAL-ARRAY I 1) (AREF TIME-INTERVAL-ARRAY LAST 1))))
             (SETQ LAST I)))))) 


(DEFVAR *FOUR-YEAR-CYCLE* (TIME-INTERVAL-TO-SECONDS "4 Years")) 

(DEFVAR *SECONDS-IN-DAY* (TIME-INTERVAL-TO-SECONDS "1 day")) 


(DEFUN SECONDS-TO-INTERVAL-STRING-1 (INDEX SECS)
  (IF (NOT (ZEROP (FLOOR SECS *FOUR-YEAR-CYCLE*)))
      (DECF SECS (* (FLOOR SECS *FOUR-YEAR-CYCLE*) *SECONDS-IN-DAY*)))
  (LET ((QUO (FLOOR SECS (AREF TIME-INTERVAL-ARRAY INDEX 1)))
        (REM (REM SECS (AREF TIME-INTERVAL-ARRAY INDEX 1))))
    (IF (ZEROP REM) (FORMAT () "~D ~A~P" QUO (AREF TIME-INTERVAL-ARRAY INDEX 0) QUO)
        (FORMAT () "~D ~A~P ~A" QUO (AREF TIME-INTERVAL-ARRAY INDEX 0) QUO
                (SECONDS-TO-INTERVAL-STRING REM))))) 


;; Maybe add a CLIM equivalent here.
#+Explorer
(DEFPROP :TIME-INTERVAL-OR-NEVER
         (PRINT-INTERVAL-OR-NEVER READ-INTERVAL-OR-NEVER () () ()
                                  "Click left to input a time interval or \"never\".")
         TV:CHOOSE-VARIABLE-VALUES-KEYWORD) 


(DEFUN PARSE-INTERVAL-OR-NEVER (STRING &OPTIONAL FROM TO)
  "Parse a string either describing a time interval or \"never\".
For a time interval, the number of seconds is returned.
For \"never\" or variations, NIL is returned."
  (IF (NUMBERP STRING) STRING
      (PROGN
        (SETQ STRING
              (STRING-TRIM '(#\SPACE #\TAB)
                           (IF (NULL (OR FROM TO)) STRING (SUBSEQ STRING FROM TO))))
        (IF (MEMBER STRING '("none" "no" "" "never" "not ever" "nil" "()") :TEST 'EQUALP) ()
            (MULTIPLE-VALUE-BIND (VAL ERR) (TIME-INTERVAL-TO-SECONDS STRING)
              (IF ERR (error "~A: ~A" STRING ERR) VAL)))))) 


(DEFUN READ-INTERVAL-OR-NEVER (&OPTIONAL (STREAM *STANDARD-INPUT*))
  "Read a line from STREAM and parse into time interval or NIL for never."
  (PARSE-INTERVAL-OR-NEVER (READ-LINE STREAM))) 

(DEFUN PRINT-INTERVAL-OR-NEVER (VAL &OPTIONAL (STREAM T))
  "Print the interval-or-never VAL on STREAM.
VAL can be a number of seconds, or NIL for never."
  (IF (NULL VAL) (FORMAT STREAM "Never") (FORMAT STREAM "~a" (SECONDS-TO-INTERVAL-STRING VAL))))

(DEFUN PRINT-UNIVERSAL-DATE (UT &OPTIONAL (STREAM *STANDARD-OUTPUT*) (TIMEZONE (if (daylight-savings-p) (1- *TIMEZONE*) *TIMEZONE*)))
  "Print the universal-time UT in verbose form on STREAM, decoding for TIMEZONE.
If STREAM is NIL, construct and return a string."
  (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
      (DECODE-UNIVERSAL-TIME UT TIMEZONE)
    (PRINT-DATE SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK STREAM)))

(DEFUN PRINT-DATE (SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK
		   &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Print the date and time in verbose form on STREAM.
If STREAM is NIL, construct and return a string."
  (SETQ MONTH (MONTH-STRING MONTH)
	DAY-OF-THE-WEEK (DAY-OF-THE-WEEK-STRING DAY-OF-THE-WEEK))
  (FORMAT STREAM
	  "~A the ~:R of ~A, ~D; ~D:~2,'0D:~2,'0D ~A"
	  DAY-OF-THE-WEEK DAY MONTH YEAR (1+ (REM (+ HOURS 11.) 12.)) MINUTES SECONDS
	  (COND ((AND (ZEROP SECONDS)
		      (ZEROP MINUTES)
		      (MEMBER HOURS '(0 12.) :TEST #'EQ))
		 (IF (= HOURS 0) "midnight" "noon"))
		((>= HOURS 12.) "pm")
		(T "am"))))


;;;(setf (symbol-function 'RTC-GET-UNIVERSAL-TIME) #'FALSE)  ; ***temp to keep from using SDU clock
  

;;AB 8/5/87.  Remove this.  INIT-THE-TIME (defined in MICRO-TIME) does the same thing.
;;(ADD-INITIALIZATION "Initialize Timebase"
;;                    '(PROGN
;;                      (SETQ LAST-BOOT-TIME (TIME) WAS-NEGATIVE () HIGH-TIME-BITS 0)
;;                      (INITIALIZE-TIMEBASE))
;;                    '(:WARM :NOW)) 

;;; Now that the time parser is loaded, we can fix up times remembered as strings by
;;; the system generator.
#+Explorer
(DEFUN CANONICALIZE-COLD-LOADED-TIMES ()
  (MAPHASH
   #'(LAMBDA (IGNORE VAL &AUX ALIST)
       (declare (ignore ignore))
       (AND (SETQ ALIST (GETF (FS:PATHNAME-PROPERTY-LIST VAL) :FILE-ID-PACKAGE-ALIST))
	  (DOLIST (ID ALIST)
	    (LET ((INFO (CADR ID)))
	      (AND (STRINGP (CDR INFO))
		 (RPLACD INFO (FS:PARSE-DIRECTORY-DATE-PROPERTY (CDR INFO) 0.)))))))
   FS:*PATHNAME-HASH-TABLE*))

#+Explorer
(ADD-INITIALIZATION "TIME-PARSER-LOADED" '(CANONICALIZE-COLD-LOADED-TIMES) '(:ONCE)) 

