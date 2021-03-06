;
; $Id: run-outline.scm,v 1.2 2002/12/10 22:28:14 oleg Exp $

(define docstrings
 '(
 " Pretty-print the structure of an XML document, disregarding the "
 " character data."
 " This example corresponds to outline.c of the Expat distribution"
 ""
 " Usage"
 "	outline xml-file-name"
))

(define (parser-error port message . specialising-msgs)
  (apply cerr (cons message specialising-msgs))
  (cerr nl)
  (exit 4))
(define (ssax:warn port message . specialising-msgs)
  (apply cerr (cons message specialising-msgs))
  (cerr nl))


(define (main argv)

  (define (help)
    (for-each
     (lambda (docstring) (cerr docstring nl))
     docstrings)
    (exit 4))

  (if (not (= 2 (length argv)))
      (help))		; at least one argument, besides argv[0], is expected
  (display 
   (call-with-input-file (cadr argv)
     outline))
)

