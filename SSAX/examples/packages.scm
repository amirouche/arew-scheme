(define-structure remove-markup (export remove-markup)
  (open scheme
	ssax-vanilla
	(subset srfi-13 (string-null?
			 string-concatenate-reverse/shared)))
  (files remove-markup))

(define-structure outline (export outline)
  (open scheme
	ssax-vanilla)
  (files outline))

(define-structure apply-templates (export apply-templates)
  (open scheme
	assertions
	ppretty-prints
	coutputs
	oleg-string-ports
	srfi-23 ; ERROR
	sxml-to-html
	sxpath)
  (files apply-templates))

(define-structure sxml-db-conv (export convert-db SXML->XML)
  (open scheme
	coutputs
	ppretty-prints
	oleg-string-ports
	oleg-utils
	(subset srfi-1 (cons*))
	sxml-tree-trans
	ssax-vanilla)
  (files sxml-db-conv))

(define-structure pull-punct-sxml (export match-tree classify-ss transform-ss)
  (open scheme
	assertions
	srfi-2 ; AND-LET*
	oleg-string-ports
	coutputs
	ppretty-prints
	sxml-tree-trans
	sxml-to-html
	ssax-vanilla)
  (files pull-punct-sxml))

(define-structure sxml-nesting-depth-label  (export my-ss)
  (open scheme
	ppretty-prints
	sxml-tree-trans)
  (files sxml-nesting-depth-label))

(define-structure sxml-to-sxml (export map-node-concat)
  (open scheme
	ppretty-prints
	assertions
	crementing
	srfi-23)
  (begin
    (define gensym
      (let ((count 0))
	(lambda ()
	  (set! count (+ 1 count))
	  (string->symbol (string-append "foo-bar-"
					 (number->string count)))))))
  (files sxml-to-sxml))

;; daml-parse-unparse.scm is very Bigloo-specific

(define-structure sxslt-advanced (export number-sections
					 make-toc-entries
					 main-ss)
  (open scheme
	oleg-utils
	(subset srfi-1 (cons*))
	sxml-tree-trans
	sxml-to-html
	sxml-to-html-ext)
  (files sxslt-advanced))

(define-structure parent-pointers (export print-ancestors
					  get-parent-from-true-pt
					  add-thunk-parent-pt
					  make-get-parent-by-search
					  xml->dict-parent-sxml
					  xml->true-parent-sxml)
  (open scheme
	assertions
	ppretty-prints
	(subset srfi-1 (cons*))
	srfi-11 ; LET*-VALUES
	(subset srfi-13 (string-null?))
	srfi-23 ; ERROR
	coutputs
	oleg-utilso
	oleg-string-ports
	sxml-tree-trans
	ssax-vanilla
	sxpath)
  (begin
    (define gensym
      (let ((count 0))
	(lambda ()
	  (set! count (+ 1 count))
	  (string->symbol (string-append "foo-bar-"
					 (number->string count))))))
    (define (display-circle x)
      (display "Cannot safely display circular datastructures. Use SRFI-38")
      (newline)))
  (files parent-pointers))

(define-structure validate-doctype-simple (export validate-content
						   ssax:typedxml->sxml)
  (open scheme
	assertions
	coutputs
	control-flows
	oleg-utils
	oleg-string-ports
	catch-errors
	ppretty-prints
	(subset srfi-1 (cons*))
	(subset srfi-13 (string-null?
			 string-concatenate/shared
			 string-concatenate-reverse/shared))
	srfi-23
	ssax-warnings-vanilla
	ssax-vanilla)
  (files validate-doctype-simple))
						   