; Pretty-print the structure of an XML document
; Bigloo module declaration
;
; $Id: run-outline-bigloo.scm,v 1.2 2003/04/09 21:27:53 oleg Exp $

(module outline
	(include "myenv-bigloo.scm")
 	(include "srfi-13-local.scm") ; or import from SRFI-13 if available
        (include "char-encoding.scm")
	(include "util.scm")
	(include "look-for-str.scm")
	(include "input-parse.scm")
	(include "SSAX-code.scm")
	(include "outline.scm")
	(include "run-outline.scm")
	(main main))
