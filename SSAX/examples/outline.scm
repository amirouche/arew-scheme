; Pretty-print the structure of an XML document, disregarding the
; character data.
; This example corresponds to outline.c of the Expat distribution.
; The example demonstrates how to transform an XML document on the
; fly, as we parse it.
;
; $Id: outline.scm,v 1.2 2002/12/10 22:28:14 oleg Exp $

(define (outline xml-port)
  ; The seed describes the depth of an element relative to the root of the tree
  ; To be more precise, the seed is the string of space characters
  ; to output to indent the current element. The indent increases by two
  ; space characters for the next nested element.
  ((ssax:make-parser
	   NEW-LEVEL-SEED 
	   (lambda (elem-gi attributes namespaces
			      expected-content seed)
	     (display seed)		; indent the element name
	     (display elem-gi)		; print the name of the element
	     (newline)
	     (string-append "  " seed))	; advance the indent level
   
	     FINISH-ELEMENT
	     (lambda (elem-gi attributes namespaces parent-seed seed)
	       parent-seed)			; restore the indent level

	     CHAR-DATA-HANDLER
	     (lambda (string1 string2 seed)
	       seed)
	     )
	    xml-port ""))
