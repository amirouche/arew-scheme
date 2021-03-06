;
; $Id: run-sxml.scm,v 1.2 2002/12/10 22:28:14 oleg Exp $

(define docstrings
 '(
 " Transform an XML document into SXML. See ../docs/SXML.html for description."
 ""
 " Usage"
 "	xml-to-sxml xml-file-name"
 " The translated SXML document is printed to the standard output."
))

; The XML->SXML function is a part of the SSAX distribution

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
  (pp 
   (call-with-input-file (cadr argv)
     (lambda (port) (ssax:xml->sxml port '()))))
)

