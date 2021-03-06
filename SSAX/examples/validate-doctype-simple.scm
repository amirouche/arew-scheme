;		SSAX parsing with limited XML doctype validation
;			and datatype conversion
;
; XML Schema introduces a number of atomic XML datatypes and the
; corresponding validation rules. SXML likewise supports boolean,
; numerical, and other Scheme values [1] in the "character" content of
; SXML elements and attributes. As we parse the document, we can
; validate and de-serialize character data from the source XML
; document into the corresponding numerical etc. Scheme values. The
; present code shows an example of such datatype conversion and
; validation. This code also demonstrates element content validation:
; making sure that the sequence of child elements matches a
; user-specified template.
;
; The present code instantiates the SSAX parser framework to support a
; limited document type validation and datatype conversion. The only
; changes from ssax:xml->sxml concern procedures UNDECL-ROOT,
; NEW-LEVEL-SEED and FINISH-ELEMENT. We also add a function
; validate-content. The tests at the end of the file demonstrate the
; datatype conversion and the detection of invalid atomic and
; structural XML fragments.
;
; [1] One may use any Scheme value in the "character" content of an
; SXML element or attribute as long as this value is not a list or a
; symbol. If we need to insert a list or a symbol in the "character"
; content, we should wrap them in a vector or a procedure.

;
; $Id: validate-doctype-simple.scm,v 1.4 2004/08/06 23:03:20 oleg Exp $

; IMPORT
; The following is a Bigloo-specific module declaration.
; Other Scheme systems have something similar.
;
; See the Makefile for the rules to run this example on Bigloo, SCM
; and other systems
; (module validate-xml
; 	(include "myenv-bigloo.scm")
; 	(include "srfi-13-local.scm") ; or import from SRFI-13 if available
; 	(include "char-encoding.scm")
; 	(include "util.scm")
; 	(include "look-for-str.scm")
; 	(include "input-parse.scm")
; 	(include "catch-error.scm")
; 	(include "SSAX-code.scm")
; 	)

; procedure: validate-content EXPECTED-CONTENT SEED -> CONVERTED-SEED

; Verify the content represented by the SEED against the EXPECTED-CONTENT.
; If the content verifies, we optionally do a type-specific transformation:
; de-serialization. Verification failure is an error.
;
; At present, we only check for the following EXPECTED-CONTENT:
; 'bool: SEED must be a one-element list containing a
;	 case-insensitive string "T", "F", "Y" or "N" 
;        The string is converted to a boolean in the obvious way.
; 'int:  SEED must be a one-element list containing a string
;        convertible to an integer. We do the conversion.
; (seq tag1 tag2 ...)
;        SEED must be a sequence of elements tag1 tag2 ...
;
; We accept all other content.

(define (validate-content expected-content seed)
  (cout "validating seed: " seed " against the expected content "
        expected-content nl)
  (case expected-content
    ((bool)
      (assert (pair? seed) (null? (cdr seed)))
      (cond
	((string-ci=? "T" (car seed)) (list #t))
	((string-ci=? "Y" (car seed)) (list #t))
	((string-ci=? "F" (car seed)) (list #f))
	((string-ci=? "N" (car seed)) (list #f))
	(else (error "Wrong content for the type bool: " seed))))
    ((int)
      (assert (pair? seed) (null? (cdr seed)))
      (let ((val (string->integer (car seed) 0 (string-length (car seed)))))
	(or val
	  (error "Wrong content for the type int: " seed))
	(list val)))
    (else
      (cond
	((and (pair? expected-content) 
	   (eq? 'seq (car expected-content)))
	  ; validate (seq tag1 ...)
	  (or (equal?
		(cdr expected-content)
		(map (lambda (elem)
		       (and (pair? elem) (car elem))) seed))
	    (error "Wrong content for the type " expected-content 
	      ": " seed))
	  seed)
	; accept everything else
	(else seed)))))


; procedure: ssax:typedxml->sxml PORT NAMESPACE-PREFIX-ASSIG ELEMS -> SXML
;
; An instance of a SSAX parser that reads an XML document from PORT
; and returns the corresponding SXML tree. On return, PORT will point
; to the first character after the root element.
; NAMESPACE-PREFIX-ASSIG is a list of (USER-PREFIX . URI-STRING)
; that assigns USER-PREFIXes to certain namespaces identified by
; particular URI-STRINGs. The argument may be an empty list.
;
; ELEMS is either #f or a list of constraints to validate the
; input XML document against. Each item of ELEMS has the form:
;	(elem-name elem-content decl-attrs)
;	elem-name is an UNRES-NAME for the element.
;	elem-content is an ELEM-CONTENT-MODEL.
;	decl-attrs is an ATTLIST, of (ATTR-NAME . VALUE) associations
;
; If ELEMS is non-#f, each element in the input XML document must
; be described by the corresponding constraint.
; ELEM-CONTENT-MODEL specifies the expected type of the element content.
; In the present code, ELEM-CONTENT-MODEL is one of the following:
;    A symbol:
;	ANY	  - anything goes
;	EMPTY-TAG - no content
;	EMPTY	  - no content
;	PCDATA    - expect character data only, and no children elements
;       bool      - expect a character string representing a boolean
;       int       - expect a character string representing an integer
;    A list:
;       (seq tag1 tag2 ...) - expect a sequence of child elements
;                    tag1 tag2 ...
;
; See the function validate-content above for more details on the
; last three choices for ELEM-CONTENT-MODEL. See the description
; of the ELEM-CONTENT-MODEL datatype in SSAX.scm.

(define (ssax:typedxml->sxml port namespace-prefix-assig elem-types)
  (letrec
      ((namespaces
	(map (lambda (el)
	       (cons* #f (car el) (ssax:uri-string->symbol (cdr el))))
	     namespace-prefix-assig))

       (res-name->sxml
	(lambda (res-name)
	  (string->symbol
	   (string-append
	    (symbol->string (car res-name))
	    ":"
	    (symbol->string (cdr res-name))))))
       )
    (let ((result
	   (reverse
	    ((ssax:make-parser
	     NEW-LEVEL-SEED 
	     (lambda (elem-gi attributes namespaces
			      expected-content seed)
	       (list (cons '*expected-type* expected-content)))
   
	     FINISH-ELEMENT
	     (lambda (elem-gi attributes namespaces parent-seed seed)
	       (let* 
		 ((seed-raw (ssax:reverse-collect-str-drop-ws seed))
		   (attrs
		     (attlist-fold
		       (lambda (attr accum)
			 (cons (list 
				 (if (symbol? (car attr)) (car attr)
				   (res-name->sxml (car attr)))
				 (cdr attr)) accum))
		       '() attributes))
		   (seed
		     (if (and (pair? seed-raw) (pair? (car seed-raw))
			   (eq? '*expected-type* (caar seed-raw)))
		       (validate-content (cdar seed-raw) (cdr seed-raw))
		       seed-raw)))
		 (cons
		   (cons 
		     (if (symbol? elem-gi) elem-gi
		       (res-name->sxml elem-gi))
		     (if (null? attrs) seed
		       (cons (cons '@ attrs) seed)))
		   parent-seed)))

	     CHAR-DATA-HANDLER
	     (lambda (string1 string2 seed)
	       (if (string-null? string2) (cons string1 seed)
		   (cons* string2 string1 seed)))

	     DOCTYPE
	     (lambda (port docname systemid internal-subset? seed)
	       (when internal-subset?
		     (SSAX:warn port
			   "Internal DTD subset is not currently handled ")
		     (SSAX:skip-internal-dtd port))
	       (SSAX:warn port "DOCTYPE DECL " docname " "
		     systemid " found and skipped")
	       (values #f '() namespaces seed))

	     UNDECL-ROOT
	     (lambda (elem-gi seed)
	       (values elem-types '() namespaces seed))

	     PI
	     ((*DEFAULT* .
		(lambda (port pi-tag seed)
		  (cons
		   (list '*PI* pi-tag (ssax:read-pi-body-as-string port))
		   seed))))
	     )
	    port '()))))
      (cons '*TOP*
	    (if (null? namespace-prefix-assig) result
		(cons
		 (list '@ (cons '*NAMESPACES* 
				 (map (lambda (ns) (list (car ns) (cdr ns)))
				      namespace-prefix-assig)))
		      result)))
)))


(define (test strs elem-types expected-result)
  ; This function is a standard equal? predicate with one exception.
  ; On Scheme systems where (string->symbol "A") and a symbol A
  ; are the same, equal_? is precisely equal?
  ; On other Scheme systems, we compare symbols disregarding their case.
  ; Since this function is used only in tests, we don't have to
  ; strive to make it efficient.
  (define (equal_? e1 e2)
    (if (eq? 'A (string->symbol "A")) (equal? e1 e2)
      (cond
	((symbol? e1)
	  (and (symbol? e2) 
	    (string-ci=? (symbol->string e1) (symbol->string e2))))
	((pair? e1)
	  (and (pair? e2)
	    (equal_? (car e1) (car e2)) (equal_? (cdr e1) (cdr e2))))
	((vector? e1)
	  (and (vector? e2) (equal_? (vector->list e1) (vector->list e2))))
	(else
	  (equal? e1 e2)))))

  (let ((str (string-concatenate/shared strs)))
    (newline) (display "input: ") (write str) (newline)
    (let ((result 
	    (call-with-input-string str
	      (lambda (port)
		(ssax:typedxml->sxml port '() elem-types)))))
      (display "Result: ")
      (pp result)
      (assert (equal_? result expected-result))
      (newline))))


; This silly functions makes symbols case-sensitive
(define (symbolize exp)
  (cond
    ((string? exp) (string->symbol exp))
    ((pair? exp) (cons (symbolize (car exp)) (symbolize (cdr exp))))
    (else exp)))

; Simple test, no validation
(test '("<BR></BR>") #f
  (symbolize '("*TOP*" ("BR"))))

(test 
  `("<FLAGS>"
      "<FLAG>T</FLAG><FLAG>n</FLAG>" ,nl
    "</FLAGS>")
  ; The doctype
  (symbolize '(("FLAGS" "ANY" ()) ("FLAG" "bool" ())))
  ; The result
  (symbolize '("*TOP*" ("FLAGS" ("FLAG" #t) ("FLAG" #f)))))

; The content of an element FLAG is of a wrong type
(assert 
  (failed?
    (test 
      `("<FLAGS>"
	  ; we see bad content: xxx is not a boolean
	  "<FLAG>T</FLAG><FLAG>xxx</FLAG>" ,nl
	"</FLAGS>")
      (symbolize '(("FLAGS" "ANY" ()) ("FLAG" "bool" ())))
      '())))


; Test for boolean and int datatype conversions
; Note that the resulting SXML contains a boolean #t and an
; integer number 10, rather than corresponding strings.
(test 
  `("<T>"
      "<FLAG>y</FLAG>"
      "<AMOUNT>10</AMOUNT>" ,nl
    "</T>")
  ; The doctype
  (symbolize '(("T" "ANY" ()) ("FLAG" "bool" ()) ("AMOUNT" "int" ())))
  ; The result
  (symbolize '("*TOP*" ("T" ("FLAG" #t) ("AMOUNT" 10)))))


; check that the error is generated given an improper content for AMOUNT
(assert 
  (failed?
    (test 
      `("<T>"
	  "<FLAG>y</FLAG>"
	  "<AMOUNT>10xx</AMOUNT>" ,nl
	"</T>")
     (symbolize '(("T" "ANY" ()) ("FLAG" "bool" ()) ("AMOUNT" "int" ())))
     '())))

; Test for the proper sequence. Doctype specifies that element T must
; first contain AMOUNT and then FLAG. In the actual document, the child
; elements are switched. This is an error.
(assert 
  (failed?
    (test 
      `("<T>"
	  "<FLAG>T</FLAG>"
	  "<AMOUNT>10</AMOUNT>" ,nl
	"</T>")
      (symbolize '(("T" ("seq" "AMOUNT" "FLAG") ())
		   ("FLAG" "bool" ()) ("AMOUNT" "int" ())))
      '())))

; Test for the an undeclared element: the doctype below does not
; define the FLAG element.
(assert 
  (failed?
    (test 
      `("<T>"
	  "<FLAG>T</FLAG>"
	  "<AMOUNT>10</AMOUNT>" ,nl
	"</T>")
      ; The doctype
      (symbolize '(("T" ("seq" "AMOUNT" "FLAG") ()) ("AMOUNT" "int" ())))
      '())))

; The successful test: validation of both structural and datatype
; constraints.
(test 
  `("<T>"
       "<FLAG>T</FLAG>"
       "<AMOUNT>10</AMOUNT>" ,nl
    "</T>")
  ; The doctype
  (symbolize '(("T" ("seq" "FLAG" "AMOUNT") ())
	       ("FLAG" "bool" ()) ("AMOUNT" "int" ())))
  ; The result
  (symbolize '("*TOP*" ("T" ("FLAG" #t) ("AMOUNT" 10)))))

(cout nl "All tests passed" nl)

