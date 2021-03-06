; Create an XML document for benchmark1
;
; The document is a full binary tree expressed in XML, such as
;
; <node><node><node><node><leaf>0</leaf><leaf>1</leaf></node>
;                   <node><leaf>2</leaf><leaf>3</leaf></node></node>
;             <node><node><leaf>4</leaf><leaf>5</leaf></node>
;                   <node><leaf>6</leaf><leaf>7</leaf></node></node></node>
;       <node><node><node><leaf>8</leaf><leaf>9</leaf></node>
; 	            <node><leaf>0</leaf><leaf>1</leaf></node></node>
; 	      <node><node><leaf>2</leaf><leaf>3</leaf></node>
; 	            <node><leaf>4</leaf><leaf>5</leaf></node></node>
; </node></node>
; The content of leaf nodes is a single-digit string.
;
; The only command-line parameter of the script is an S-expression. The
; script produces an XML document on the standard output.
; The argument S-expression specifies the parameters of the document.
; The S-expression must be an associative list of the following pairs:
;   (depth . <depth-of-the-tree>)
;   (doctype . <#t or #f>) 
; The last pair is optional. It determines if a DOCTYPE declaration is
; to be created.  The latter is necessary for a validating parser but
; optional otherwise.  By default, no DOCTYPE declaration is produced.
; 
; Example:
; ./make-file-bench1.scm "((depth . 14))" > bench-file.xml
; 
; $Id: make-file-bench1.scm,v 1.1.1.1 2001/11/01 19:40:18 oleg Exp $

(module make-file-bench1
	(include "myenv-bigloo.scm") ; My standard prelude is assumed
	(include "SXML-tree-trans.scm")
	(main main))

(define Doctype-decl
  "<!DOCTYPE node [
     <!ELEMENT leaf (#PCDATA)>
     <!ELEMENT node (leaf | (node, node))>
   ]>")

; Make a full binary SXML tree of depth 'depth'; for use in benchmarking
; Each leaf is a one-digit string.

(define (make-simple-sample-tree depth)
  (define (make-tree depth seqno)
    (if (zero? depth) (list 'leaf (remainder seqno 10))
	(let* ((new-depth (-- depth))
	       (kid-left (make-tree new-depth (+ seqno seqno)))
	       (kid-right (make-tree new-depth (+ seqno seqno 1))))
	  (list 'node kid-left kid-right))))
  (make-tree depth 0))

(define (entag tag)
  (lambda elems
    (if (and (pair? elems) (pair? (car elems)) (eq? '@ (caar elems)))
	(list #\< tag (cdar elems) #\> (cdr elems) "</" tag #\>)
	(list #\< tag #\> elems "</" tag #\>))))
(define (enattr attr-key)
  (lambda (value)
    (if (eq? value #t) (list #\space attr-key)
	(list #\space attr-key "=\"" value #\"))))

(define (send-simple-sample-xml-doc depth)
  (SRV:send-reply
   (post-order (make-simple-sample-tree depth)
                ; Universal transformation rules. Work for every XML,
                ; present and future
	  `((@
	    ((*default*       ; local override for attributes
	      . ,(lambda (attr-key . value) ((enattr attr-key) value))))
	    . ,(lambda (trigger . value) (list '@ value)))
	   (*default* . ,(lambda (tag . elems) (apply (entag tag) elems)))
	   (*text* . ,(lambda (trigger str) 
			str))
 
	   )
    )))



; The root module

(define (main argv)
  ;(if (not (= (length argv) (+ 1 1)))
  ;    (error "One argument required: an S-expression\n"))
  (let*
      ((params (call-with-input-string (list-ref argv (-- (length argv))) 
				       read))
       (dummy (assert (pair? params) (pair? (car params))))
       (depth
	(cond
	 ((assq 'depth params) => cdr)
	 (else (error "depth parameter is needed!"))))
       (dummy (assert (number? depth) (positive? depth)))
       (include-doctype?
	(cond
	 ((assq 'doctype params) => cdr)
	 (else #f)))
       )

    (if include-doctype?
	(cout Doctype-decl nl))
    
    (send-simple-sample-xml-doc depth)))

;(main)
