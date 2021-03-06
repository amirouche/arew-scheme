; Given an XML document, remove all markup
; In other words, compute the string-value of a DOM tree of the document
;
; $Id: remove-markup.scm,v 1.3 2003/04/09 21:27:53 oleg Exp $



; Given a port referring to an xml document, this procedure returns
; a string that contains all the character data from the document:
; that is, the body of the document without the markup.
; This function deliberately does not print anything. Because the function
; is free from a difficult-to-estimate overhead of writing data, the
; function is rather suitable for benchmarking of the SSAX parser.
;
; On the other hand, the file outline.scm in the present directory
; shows how to do an XML transformation on the fly, as we parse.

(define (remove-markup xml-port)
    ; Accumulate the text values of leaves in a seed, in reverse order
    (let ((result
	   ((ssax:make-parser
	     NEW-LEVEL-SEED 
	     (lambda (elem-gi attributes namespaces
			      expected-content seed)
	       seed)
   
	     FINISH-ELEMENT
	     (lambda (elem-gi attributes namespaces parent-seed seed)
	       seed)

	     CHAR-DATA-HANDLER
	     (lambda (string1 string2 seed)
	       (let* ((seed (cons string1 seed)))
		 (if (string-null? string2) seed
		     (cons string2 seed))))
	     )
	    xml-port '())))
      (string-concatenate-reverse/shared result)
    ))

