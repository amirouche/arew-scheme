; Parse a DAML ontology into SXML and pretty-print it back into XML
;
; This code is an example of parsing and _unparsing_ of a notably
; namespace-heavy XML/SXML. The XML document in question also
; makes an extensive use of named parsed entities.
;
; This code verifies that 
;     PARSE . UNPARSE . PARSE === PARSE
; In the first step, the code parses a DAML document and prints the
; resulting SXML. In the second step, we unparse the SXML back into
; XML. Finally, we parse that XML again and compare the result with
; that of the first step.
;
; A Bigloo Scheme system is assumed. We extensively rely on the
; match-case form provided by Bigloo.
; $Id: daml-parse-unparse.scm,v 1.7 2004/08/06 23:04:10 oleg Exp $

; IMPORTS
; (module xml-to-sxml-to-xml
; 	(include "myenv-bigloo.scm")
;  	(include "srfi-13-local.scm") ; or import from SRFI-13 if available
; 	(include "char-encoding.scm")
; 	(include "util.scm")
; 	(include "look-for-str.scm")
; 	(include "input-parse.scm")
; 	(include "SSAX-code.scm")
; 	(include "SXML-tree-trans.scm")
; 	)


; A simple linear pattern matcher
; It is efficient (generates code at macro-expansion time) and simple:
; it should work on any R5RS Scheme system.


; (match-case-simple exp <clause> ...[<else-clause>])
; <clause> ::= (<pattern> <guard> exp ...)
; <else-clause> ::= (else exp ...)
; <guard> ::= boolean exp | ()
; <pattern> :: =
;         var  -- matches always and binds the var
;                 pattern must be linear! No check is done
;         _    -- matches always
;        'exp  -- matches exp (using equal?)
;        (<pattern1> <pattern2> ...) -- matches the list of patterns
;        (<pattern1> . <pattern2>)  -- ditto
;        ()    -- matches the empty list

(define-syntax match-case-simple
  (syntax-rules ()
    ((match-case-simple exp clause ...)
      (let ((val-to-match exp))
	(match-case-simple* val-to-match clause ...)))))

(define (<match-failure> val)
  (assert #f "failed match" val))

(define-syntax match-case-simple*
  (syntax-rules (else)
    ((match-case-simple* val (else exp ...))
      (let () exp ...))
    ((match-case-simple* val)
      (<match-failure> val))
    ((match-case-simple* val (pattern () exp ...) . clauses)
      (let ((fail (lambda () (match-case-simple* val . clauses))))
	  ; note that match-pattern may do binding. Here,
	  ; other clauses are outside of these binding
	(match-pattern val pattern (let () exp ...) (fail))))
    ((match-case-simple* val (pattern guard exp ...) . clauses)
      (let ((fail (lambda () (match-case-simple* val . clauses))))
	(match-pattern val pattern
	  (if guard (let () exp ...) (fail))
	  (fail))))))


; (match-pattern val pattern kt kf)
(define-syntax match-pattern
  (syntax-rules (_ quote)
    ((match-pattern val _ kt kf) kt)
    ((match-pattern val () kt kf)
      (if (null? val) kt kf))
    ((match-pattern val (quote lit) kt kf)
      (if (equal? val (quote lit)) kt kf))
    ((match-pattern val (x . y) kt kf)
      (if (pair? val)
	(let ((valx (car val))
	      (valy (cdr val)))
	  (match-pattern valx x
	    (match-pattern valy y kt kf)
	    kf))
	kf))
    ((match-pattern val var kt kf)
      (let ((var val)) kt))))


; The file is downloaded from
; http://www.daml.org/services/daml-s/0.7/CongoProfile.daml
(define daml-file "xml/CongoProfile.daml")
; A temp file to store the result of unparsing
(define unparsed-file "/tmp/a.xml") 

; Internal entities declared in the CongoProfile.daml file
(define DAML-entities
  (map
    (lambda (ass)
      (cons (string->symbol (car ass)) (cdr ass)))
  '(("rdf" . "http://www.w3.org/1999/02/22-rdf-syntax-ns")
    ("rdfs" . "http://www.w3.org/2000/01/rdf-schema")
    ("daml" . "http://www.daml.org/2001/03/daml+oil")
    ("process" . "http://www.daml.org/services/daml-s/0.7/Process.daml")
    ("service" . "http://www.daml.org/services/daml-s/0.7/Service.daml")
    ("profile" . "http://www.daml.org/services/daml-s/0.7/Profile.daml")
    ("profileTaxonomy" . 
	"http://www.daml.org/services/daml-s/0.7/ProfileTaxonomy.daml")
    ("country" . "http://www.daml.ri.cmu.edu/ont/Country.daml")
    ("concepts" . "http://www.daml.ri.cmu.edu/ont/DAML-S/concepts.daml")
    ("congoService" . 
		  "http://www.daml.org/services/daml-s/0.7/CongoService.daml")
    ("congoProcess" . 
		  "http://www.daml.org/services/daml-s/0.7/CongoProcess.daml")
    ("time" . "http://www.ai.sri.com/daml/ontologies/time/Time.daml")
    ("xsd" . "http://www.w3.org/2000/10/XMLSchema.xsd")
    ("DEFAULT" . "http://www.daml.org/services/daml-s/0.7/CongoProfile.daml")
    )))

;------------------------------------------------------------------------
;			Parsing of DAML

; The following is an instantiation variation of the SSAX parser
; to handle specified internal parsed entities.
; This code is identical to ssax:xml->sxml except one line in
; the DOCTYPE handler:
;   (values #f DAML-entities namespaces seed)
; (The original ssax:xml->sxml had '() in place of DAML-entities)
;
; We also create (@ (*NAMESPACES* . ns-assocs)) for each element
; with a non-local element or attribute name. These local ns-assocs
; describe only the namespaces of the element-gi and its attributes.
; Sharing should be improved!

(define (ssax:daml->sxml port namespace-prefix-assig)
  (letrec
      ((namespaces
	(map (lambda (el)
	       (cons* #f (car el) (ssax:uri-string->symbol (cdr el))))
	     namespace-prefix-assig))

       (RES-NAME->SXML
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
	       '())
   
	     FINISH-ELEMENT
	     (lambda (elem-gi attributes namespaces parent-seed seed)
	       (let* ((seed (ssax:reverse-collect-str-drop-ws seed))
		      (attrs
		       (attlist-fold
			(lambda (attr accum)
			  (cons (list 
				 (if (symbol? (car attr)) (car attr)
				     (RES-NAME->SXML (car attr)))
				 (cdr attr)) accum))
			'() attributes))
		      (ns-id-used	; namespace prefixes used by the elem
		       (attlist-fold
			(lambda (attr accum)
			  (if (symbol? (car attr)) accum
			      (cons  (caar attr) accum)))
			(if (symbol? elem-gi) '()
			    (list (car elem-gi))) ; ns-id of the element name
			attributes))
		      (local-namespaces
		       (map
			(lambda (ns-id)
			  (let ((ns-elem 
				 ; locate the ns-id among the namespaces
				 (let loop ((namespaces namespaces))
				   (cond
				    ((null? namespaces)
				     (assert #f ns-id))  ; can't happen
				    ((eq? (cadar namespaces) ns-id)
				     (car namespaces))
				    (else (loop (cdr namespaces)))))))
			    (list (cadr ns-elem) ; user prefix or URI-symbol
				  (symbol->string (cddr ns-elem)) ; URI string
				  (car ns-elem)) ; original prefix
			    ))
			ns-id-used))
		      (attrs
		       (if (null? local-namespaces) attrs
			   (cons (list '@
				   (cons '*NAMESPACES* local-namespaces))
				  attrs)))
		      (sxml-element	; newly created element
		       (cons 
			(if (symbol? elem-gi) elem-gi
			    (RES-NAME->SXML elem-gi))
			(if (null? attrs) seed
			    (cons (cons '@ attrs) seed))))
		      )
		 (cons
		  sxml-element parent-seed)
		 ))

	     CHAR-DATA-HANDLER
	     (lambda (string1 string2 seed)
	       (if (string-null? string2) (cons string1 seed)
		   (cons* string2 string1 seed)))

	     DOCTYPE
	     (lambda (port docname systemid internal-subset? seed)
	       (when internal-subset?
		     (ssax:warn port
			   "Internal DTD subset is not currently handled ")
		     (ssax:skip-internal-dtd port))
	       (ssax:warn port "DOCTYPE DECL " docname " "
		     systemid " found and skipped")
	       (values #f DAML-entities namespaces seed))

	     UNDECL-ROOT
	     (lambda (elem-gi seed)
	       (values #f '() namespaces seed))

	     PI
	     ((*DEFAULT* .
		(lambda (port pi-tag seed)
		  (cons
		   (list '*PI* pi-tag (ssax:read-pi-body-as-string port))
		   seed))))
	     )
	    port '()))))
      (cons '*TOP* result)
)))

; Parse a DAML file
(define (parse-daml daml-file)
  (call-with-input-file daml-file
     (lambda (port) (ssax:daml->sxml port 
	; Define the following user namespace shortcuts
	 '((rdfs . "http://www.w3.org/2000/01/rdf-schema#")
	  (rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
	  (daml . "http://www.daml.org/2001/03/daml+oil#")
	  (dc . "http://purl.org/dc/elements/1.1/")
	  (log . "http://www.w3.org/2000/10/swap/log#")
	  (service . "http://www.daml.org/services/daml-s/0.7/Service.daml")
	  (grounding . "http://www.daml.org/services/daml-s/0.7/Grounding.daml")
	  (congo-service . "http://www.daml.org/services/daml-s/0.7/CongoService.daml")
	  (congo-profile . "http://www.daml.org/services/daml-s/0.7/CongoProfile.daml")
	  (congo-process . "http://www.daml.org/services/daml-s/0.7/CongoProcess.daml")
	  (congo-grounding . "http://www.daml.org/services/daml-s/0.7/CongoGrounding.daml")
	  (congo-wsdl-grounding . "http://www.daml.org/services/daml-s/0.7/CongoGrounding.wsdl"))
	 ))))


;------------------------------------------------------------------------
;		Unparsing of an SXML file back into XML

; pretty-print the resulting SXML into XML, emitting the appropriate
; xmlns:xxx attributes
(define (SXML->XML sxml)
  ; Split the GE (entity name) into the namespace part and the local part
  ; Return both as strings
  ; If GE has no namespace part, return #f for the namespace part
  (define (split-ge ge)
    (let* ((ge-str (symbol->string ge))
	   (last-colon (string-rindex ge-str #\:))
	   )
      (if last-colon ; GE is indeed an extended name
	  (values
	   (substring ge-str 0 last-colon)
	   (substring ge-str (inc last-colon) (string-length ge-str)))
	  (values #f ge-str))))

  (define (make-symbol . pieces)
    (string->symbol (apply string-append pieces)))

  ; Find out the list of all namespaces and
  ; design the translation from SXML names to XML names
  ; Return both values.
  ;
  ; The list of all namespaces is a list of
  ;	(ns-id NS-URI prefix-str)
  ; The translation is an assoc list, a list of
  ;     (sxml-name prefix-str:local-name-str)
  ; A namespace association is converted into (ns-id NS-URI prefix)
  ; by setting prefix to be ns-id, if it wasn't specified explicitly.
  ; If we come across an sxml name of the form ns-id1:local-name
  ; we check if we already know the translation for the name,
  ; if ns-id1 exists in the current list of namespaces,
  ; if ns-id1 is a NS-URI in the current list of namespaces.
  ; Otherwise, we add a new association to the list of namespaces
  ;    (ns-id1 ns-id1-string prefix1)
  ; where prefix1 is derived from ns-id1. We make it unique.

  (define (ns-normalize sxml)
    ; check to see if annotations contains a namespace association
    ; If so, process it and add to the 'namespaces'
    (define (add-local-namespaces annotations namespaces)
      (let ((ns (assq '*NAMESPACES* annotations)))
	(if (not ns) namespaces
	    (let loop ((nss (cdr ns)) (namespaces namespaces))
	      (match-case-simple nss
		(((ns-id uri . prefix-opt) . ns-rest) ()
		 (if (assq ns-id namespaces) ; seen already. Check prefixes!
		     (loop ns-rest namespaces)
		     (loop ns-rest
			   (cons
			    (list ns-id uri
				  (symbol->string
				   (if (null? prefix-opt) ns-id
				       (car prefix-opt))))
			    namespaces))))
		(else namespaces))))))

    (let loop ((node sxml) (todo '())
	       (namespaces '()) (translation '()))
      (match-case-simple node
        (() ()
	 (if (null? todo)
	     (values namespaces translation)  ; we're done
	     (loop (car todo) (cdr todo) namespaces translation)))
	(('*PI* . _) () (loop '() todo namespaces translation))
	(('*NAMESPACES* . _) () (loop '() todo namespaces translation))
	((ge . children)  (symbol? ge) ; regular node
	 (let ((namespaces
		(match-case-simple children
		 ((('@ . attrs) . _) ()
		  (cond
		    ((assq '@ attrs) =>
		      (lambda (annot-assoc)
			(add-local-namespaces (cdr annot-assoc) namespaces)))
		    (else namespaces)))
		 (else namespaces))))
	   (if (assq ge translation)  ; if we have seen ge
	     (loop children todo namespaces translation)
	     (let*-values
	      (((ns-part local-name) (split-ge ge))
	       ((ns-symbol) (and ns-part (string->symbol ns-part))))
	      (cond
	       ((not ns-part) ; ge is a local, non-expanded name
		(loop children todo namespaces translation))
	       ((let find-loop ((ns-assocs namespaces))
		  (cond       ; check if we have seen a namespace with ns-part
		   ((null? ns-assocs) #f)
		   ((eq? (caar ns-assocs) ns-symbol) (car ns-assocs))
		   ((equal? (cadar ns-assocs) ns-part) (car ns-assocs))
		   (else (find-loop (cdr ns-assocs)))))
		=>
		(lambda (seen-assoc) ; add a new translation
		  (loop children todo
			namespaces
			(cons
			 (list ge (make-symbol
				   (caddr seen-assoc) ":" local-name))
			 translation))))
	       (else    ; come across a new namespace. If we used 
		(let    ; ssax:daml->sxml above, this should not happen
		    ((prefix (symbol->string (gensym)))) ; choose gensym
		  (cerr nl "Namespace Previously not seen: " ns-symbol nl)
		  (loop 
		   children todo
		   (cons
		    (list ns-symbol ns-part prefix) namespaces)
		   (cons
		    (list ge (make-symbol prefix ":" local-name))
		    translation)))))))))
	((hd . tl) ()	; list of nodes, break them up
	 (loop hd (append tl todo) namespaces translation))
	(else		; atomic, primitive node. Ignore
	 (loop '() todo namespaces translation))
	)))

  ; The stylesheet to do the pretty-printing of SXML
  (define (this-ss namespaces translation)
    (define (translate name)
      (cond
       ((assq name translation) => cadr)
       (else name)))
    (define ns-attrs	; make xmlns: attributes
      (map
       (lambda (ns-assoc)
	 (list (make-symbol "xmlns:" (caddr ns-assoc))
	       (cadr ns-assoc)))
       namespaces))
;     (cerr namespaces nl)
;     (cerr translation nl)
    `((@
      ((*default*       ; local override for attributes
        . ,(lambda (attr-key . value) (enattr (translate attr-key) value)))
       (@ *preorder* . ,(lambda _ '()))) ; annotations handled already
      . ,(lambda (trigger . value) (cons '@ value)))
     (*default* . ,(lambda (tag . elems) (entag (translate tag) elems)))
     (*text* . ,(lambda (trigger str) 
		  (if (string? str) (string->goodXML str) str)))
     (*PI*
      *preorder*
      . ,(lambda (tag target body)
	   (list "<?" target " " body "?>")))
     (*TOP*       ; check for the namespaces and add xmlns:xxx attributes
      *preorder*  ; to the root element
      . ,(lambda (tag . elems)
	   (let*-values
	    (((pis root-elem)
	      (let ((elems-rev (reverse elems)))
		(values (reverse (cdr elems-rev)) (car elems-rev))))
	     )
	    (pre-post-order
	     (match-case-simple root-elem
			; the root element had its own attributes, add xmlns:
	      ((rootname ('@ . attrs) . children) ()
	       (list pis
		 `(,rootname 
		   (@ ,@ns-attrs . ,attrs) . ,children)))
			; the root element had no attr list: make one
	      ((rootname . children) ()
	       (list pis
		     `(,rootname
		       (@ . ,ns-attrs) . ,children)))
	      (else (error "shouldn't happen")))
	    (this-ss namespaces translation)))))
     ))
 (SRV:send-reply
  (pre-post-order sxml
		  (call-with-values (lambda () (ns-normalize sxml)) this-ss)
  )))

(define (entag tag elems)
  (match-case-simple elems
    ((('@ . attrs) . children) ()
      (list #\< tag attrs 
	(if (null? children) "/>"
	  (list #\> children "</" tag #\>))))
    (() () (list #\< tag "/>"))
    (else
      (list #\< tag #\> elems "</" tag #\>))))

(define (enattr attr-key value)
  (if (null? value) (list #\space attr-key "='" attr-key "'")
    (list #\space attr-key "=\'" value #\')))


; Given a string, check to make sure it does not contain characters
; such as '<' or '&' that require encoding. Return either the original
; string, or a list of string fragments with special characters
; replaced by appropriate character entities.

(define string->goodXML
  (make-char-quotator
   '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;")
     (#\' . "&apos;"))))


;------------------------------------------------------------------------
;				Main body

(cout nl ">>>Step 1. Parsing the DAML file " daml-file " ..." nl)
(define daml-sxml (parse-daml daml-file))
(cout nl "Parsed result" nl)
(pp daml-sxml)
(newline)

(cout nl ">>>Step 2. Parsing the resulting sxml into " unparsed-file " ..." nl)
(with-output-to-file unparsed-file
  (lambda ()
    (SXML->XML daml-sxml)))
(cout nl "Unparsing done. Check the file for the result." nl)

(cout nl ">>>Step 3. Parsing the unparsed file " unparsed-file " ..." nl)
(define daml-sxml-again (parse-daml unparsed-file))
(cout nl "Parsed result" nl)
(pp daml-sxml-again)
(newline)

(cout nl "verifying that PARSE . UNPARSE . PARSE === PARSE" nl)
(cout (equal? daml-sxml-again daml-sxml) nl)

(cout nl nl "Done" nl)


