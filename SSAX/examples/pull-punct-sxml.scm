;      An example of an interesting (S)XML transformation
;
; The example has been suggested by Dr. David Durand, a member of
; Brown University's Scholarly Technology Group (STG).
;
; Originally, the example intended to move punctuation inside a tag,
; that is, convert
;       Click <a href='url'>here</a>! ==> Click <a href='url'>here!</a>
;
; Even in this simple formulation, XSLT solution is painful. We should
; note a few desired complications. For one thing, we definitely don't
; want to pull in the punctuation in the following context:
;        <br></br>;some scheme comment
;
; Furthermore, we may need to avoid moving punctuation into the content
; of certain elements, for example:
;   <p>For more details, see the paper <cite>Krishnamurthi2001</cite>.</p>
; 
; The content of the 'cite' element is typically a bibliographic key. It
; is not appropriate to add any punctuation to it.
;
; Lastly, we do want to pull in the punctuation recursively, as in
;        <p>This is <strong><em>awesome</em></strong>!</p>
; We would like to see the explanation mark inside the innermost
; appropriate element (<em> in our case). 
;
; The desired transformation is done by the pre-post-order iterator
; with the help of two stylesheets. One of them specifies which
; elements can accept punctuations and which should not. The second
; stylesheet is generic. Both stylesheets instruct pre-post-order to do
; *preorder*, that is, call-by-name evaluations. The reason is that
; the transformation is a mix of breadth-first and depth-first
; traversals.
;
; IMPORT
; The following is a Bigloo-specific module declaration.
; Other Scheme systems have something similar.
; (module pull-punct-sxml
; 	(include "myenv-bigloo.scm")
; 	(include "srfi-13-local.scm") ; or import from SRFI-13 if available
;	(include "char-encoding.scm")
; 	(include "util.scm")
; 	(include "look-for-str.scm")
; 	(include "input-parse.scm")
; 	(include "SSAX-code.scm")
; 	(include "SXML-tree-trans.scm")
; 	(include "SXML-to-HTML.scm")
; 	)
;
; $Id: pull-punct-sxml.scm,v 1.7 2004/08/06 23:03:20 oleg Exp $

;++
; A non-linear pattern matcher

(define (pattern-var? x)
  (and (symbol? x) (char=? #\_ (string-ref (symbol->string x) 0))))

; A dumb match of two ordered trees, one of which may contain variables
;
; Match two trees. tree1 may contain variables 
; (as decided by the pattern-var? predicate above)
; variables match the corresponding branch in tree2.
; Env (bindings) contains associations of variables to values.
; tree1 may contain several occurrences of the same variable.
; All these occurrences must match the same value.
; A variable match is entered into the binding. A variable _ is an
; exception: its match is never entered into the binding.
; The function returns the resulting binding or #f if the match fails.

(define (match-tree tree1 tree2 env)
  (cond
   ((pair? tree1)			; Recursively match pairs
    (and-let* (
	    ((pair? tree2))
	    (env-new (match-tree (car tree1) (car tree2) env)))
	   (match-tree (cdr tree1) (cdr tree2) env-new)))
   ((null? tree1)
    (and (null? tree2) env))
   ((eq? '_ tree1) env)			; _ matches everything
   ((pattern-var? tree1)
    (cond 
     ((assq tree1 env) => 
      (lambda (prev-binding)		; variable occurred before
	(and (equal? (cdr prev-binding) tree2) env)))
     (else
      (cons (cons tree1 tree2) env))	; new variable, enter fresh binding
      ))
   (else
    (and (equal? tree1 tree2) env))))

;(run-test
 (let ((test 
	(lambda (tree1 tree2 expected)
	(assert (equal? expected (match-tree tree1 tree2 '()))))))
   (test '_x '(seq 1 (seq-empty)) '((_x seq 1 (seq-empty))))
   (test '(seq-empty) '(seq 1 (seq-empty)) #f)
   (test '(seq 1 (seq-empty)) '(seq 1 (seq-empty)) '())
   (test '(seq _x (seq-empty)) '(seq 1 (seq-empty)) '((_x . 1)))
   (test '(seq _x _y) '(seq 1 (seq-empty)) '((_y seq-empty) (_x . 1)))
   (test '(seq _x _y _z) '(seq 1 (seq-empty)) #f)
   (test '(seq _x (seq _y _z)) '(seq 1 (seq 2 (seq-empty)))
	 '((_z seq-empty) (_y . 2) (_x . 1)))
   (test '(seq _x (seq _x _z)) '(seq 1 (seq 2 (seq-empty))) #f)
   (test '(seq _x (seq _x _z)) '(seq 1 (seq 1 (seq-empty)))
	 '((_z seq-empty) (_x . 1)))
)
;)
; end of the non-linear pattern matcher
;--


; A sample document
(define doc
  "<div><p>some text <strong><em>awesome</em></strong>!</p><br></br>.
This is a <cite>citation</cite>. Move <a href='url'>in</a>.text</div>"
)

; Punctuation is defined as a member of the following charset
(define punctuation '(#\. #\, #\? #\! #\; #\:))

; The first step of the transformation is parsing the source
; document into an abstract syntax tree (SXML) form. Using a SSAX XML
; parser [SSAX], this step can be accomplished as follows:
(define doc-sxml
  (call-with-input-string doc
     (lambda (port) (ssax:xml->sxml port '()))))

(cout nl 
  ">>>The following is the sample document to transform, in its SXML form" nl)
(pp doc-sxml)
(newline)
(newline)

; The identity function for use in a SXSLT stylesheet
(define sxslt-id (lambda x x))

; The algorithm consists of two distinct phases, which are pushed from the
; root to the leaves. The first phase is a classification, performed
; by a stylesheet classify-ss below. Given an SXML node, the
; stylesheet transforms it into the following node:
; If the source SXML node is a string:
;    if the string starts with a 'punctuation', remove the punctuation and
;    return a node: 
;      (*move-me* punctuation-char-string orig-string-with-punctuation-removed)
;    Otherwise, return the string unchanged.
; If the source node is an attribute list, a 'cite' node, or a
; node with an empty content, we return the node as it is.
; Any other source SXML node is transformed into
;      (*can-accept* original-node)
; That is, nodes that can accept punctuation or can yield punctuation to
; move are wrapped into identifying SXML nodes. Nodes that can't accept
; the punctuation are left unwrapped.

; Note, that any XML name can be a valid Scheme identifier. However, not
; every Scheme identifier can be a name of an XML element. This property
; lets us introduce administrative names such as *move-me* and
; *can-accept* without the fear of name clashes.

(define classify-ss
  `((*text*
     . ,(lambda (trigger str)
	  (cond
	   ((equal? str "") str)
	   ((memv (string-ref str 0) punctuation)
	    (list '*move-me*
		  (string (string-ref str 0))
		  (substring str 1 (string-length str))))
	   (else str))))

    ; the following nodes never accept the punctuation
    (@ *preorder* . ,sxslt-id)
    (cite *preorder* . ,sxslt-id)

    (*default*
     *preorder*
     . ,(lambda (tag . elems)
	  (cond
	   ((null? elems) (cons tag elems)) ; no children, won't accept
	   ((match-tree '((@ . _)) elems '()) ; no children but attributes
	    (cons tag elems))	              ;  ... won't accept
	   (else
	    (list '*can-accept*
		 (cons tag elems)))))
     )
    ))

	   
; The following macro does sort of a 'destructuring-bind'

(define-syntax match-bind
  (syntax-rules ()
    ((match-bind (var ...) body ...)
     (lambda (env)
       (let ((var (cdr 
		   (assq (string->symbol (string-append "_" (symbol->string 'var))) env)))
	     ...)
	 body ...)))))

; The transformation phase looks at the classified children of the node.
; When we see a (*can-accept* original-node) immediately followed by a
; (*move-me* punctuation-char-string string) node, we move the
; punctuation inside the original-node. We then apply the algorithm
; recursively to the children. 
; This phase of the algorithm is generic.

(define transform-ss
  `(
    (*text* . ,(lambda (trigger str) str))

    (@ *preorder* . ,sxslt-id)  ; Don't mess with attributes, leave them

    (*move-me*			; remove the wrapper
     *preorder* 
     . ,(lambda (trigger str1 str2)
	  (string-append str1 str2)))

    (*can-accept*		; remove the wrapper and transform
     *preorder*			; the node (recursively)
     . ,(lambda (trigger node) 
	  (pre-post-order node transform-ss)))

    (*default*
     *preorder*
     . ,(lambda (tag . elems)
	  (cout nl "on entry: " (cons tag elems) nl)
	  (let ((classified
		 (pre-post-order elems classify-ss)))
	    
	    ;(define (handle-one elem)
	    ;  (cout "handle-one:" elem nl)
	    ; (pre-post-order elem transform-ss))
	    
	    (cout nl "classified" nl)
	    (pp classified)
	    (cons tag
	      (pre-post-order
	       (let loop ((classified classified))
		 (cond
		  ((null? classified) classified)
					;((null? (cdr classified)) 
					; (map handle-one classified))
		  ((match-tree '((*can-accept* _node)
				 (*move-me*  _punctuation _str)
				 . _rest)
			       classified
			       '())
		   =>
		   (match-bind (node punctuation str rest)
		     (cout nl "pull in: " (list node punctuation str rest) nl)
		     (cons (append node (list punctuation))
			   (cons str (loop rest)))))
		  (else
		   (cons (car classified)
			 (loop (cdr classified))))
		  ))
	       transform-ss)
	   ))))))

(define result
 (pre-post-order
  (cadr doc-sxml)
 transform-ss)
)

(cout nl ">>>The transformed SXML tree is as follows" nl)
(pp result)
(newline)
(cout nl ">>>Here is the result of pretty printing of the transformed SXML 
tree into HTML" nl)
(SXML->HTML result)
(newline)
