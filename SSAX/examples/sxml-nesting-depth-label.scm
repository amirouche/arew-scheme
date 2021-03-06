; The present code shows a solution to a problem of labeling nested
; sections of an SXML document by their nesting depth. The problem was
; posed by T. Kurt Bond on the SSAX-SXML mailing list on Oct 27, 2003.
; T. Kurt Bond wrote:
; <blockquote>
; I have a document that looks something like this:
;
;     (article
;       (section (title "T1")
; 	(para "text")
; 	(section (title "T1.1")
; 	  (para "text")
; 	  (section (title "T1.1.1")
; 	    (para "text"))
; 	  (section (title "T1.1.2")
; 	    (para "text")))
; 	(section (title "T1.2")
; 	  (para "text")))
;       (section (title "T2")
; 	  (para "text")))
;
; In the general case the sections can be nested even more deeply.  To
; convert this to the troff/groff MM I need to know the depth of each
; section to generate the appropriate call to the H (numbered section
; heading) macro. As a first step I thought I'd rewrite the section
; nodes using pre-post-order so they each have the depth following the
; tag, like this:
;
;     (*section 1 (title "T1")
;       ...
;       (*section 2 (title "T1.1")
;         ...
;         (*section 3 (title "T1.1.1")
;           ...
;           )))
; </blockquote>

; He then gave two partial and one full solution. The following is yet
; another -- simpler solution. Unlike the full section numbering
; problem, sxslt-advanced.scm, here we do not need to label across
; sibling nodes. All children sections of the same parent receive the
; same label, which is the label of their parent incremented by
; one. When information flows only up-and-down the branches but not
; from a node to its sibling, simple solutions are indeed possible.
;
; The following code demonstrates traversing an SXML document with a
; stylesheet that differs from node to node. The 'section' handler in
; the stylesheet my-ss below is a pre-order handler. The depth level
; counter is wired in the handler's code.  The handler receives the
; current 'section', and invokes pre-post-order to traverse section's
; children -- but with a "modified" stylesheet. We prepend to my-ss a
; stylesheet rule with a new section handler. The new rule effectively
; overrides the old one (without destructively mutating the
; stylesheet). The new section handler incorporates the incremented
; depth level counter.
;
; We should note the fix-point-like nature of the code, hinted to by
; 'letrec'. The section labeling process works like a `viral
; infection': each infected 'section' mutates into a labeled
; '*section', and spreads the mutated virus among its children.
;

; IMPORT
; The following is a Bigloo-specific module declaration.
; Other Scheme systems have something similar.
; (module sxslt-depth-numbering
; 	(include "myenv-bigloo.scm")
; 	(include "srfi-13-local.scm") ; or import from SRFI-13 if available
; 	(include "util.scm")
; 	(include "SXML-tree-trans.scm")
; 	)
;
; $Id: sxml-nesting-depth-label.scm,v 1.2 2004/07/07 16:02:30 sperber Exp $

; T. Kurt Bond's sample document
(define doc2
  '(article
     (section (title "T1") (para "text")
       (section (title "T1.1") (para "text")
	 (section (title "T1.1.1") (para "text"))
	 (section (title "T1.1.2") (para "text")))
       (section (title "T1.2") (para "text")))
     (section (title "T2") (para "text"))))

; The stylesheet
(define my-ss
  (letrec
    ((make-section-handler
       (lambda (level)
	 (lambda (tag . elems)
	   (pre-post-order 
	     `(*section ,level ,@elems)
	     (cons `(section *preorder* . 
		      ,(make-section-handler (+ 1 level)))
	       my-ss))))))
    `((article *macro*
	. ,(lambda (tag . elems)
	     `(*article ,@elems)))
     (section *preorder* . ,(make-section-handler 1))
     (*default* . ,(lambda x x))
     (*text* . ,(lambda (tag str) str)))))

(pp (pre-post-order doc2 my-ss))
 
(newline)


; expected result
;    (*article
;       (*section 1 (title "T1") (para "text")
; 	(*section 2 (title "T1.1") (para "text")
; 	  (*section 3 (title "T1.1.1") (para "text"))
; 	  (*section 3 (title "T1.1.2") (para "text")))
; 	(*section 2 (title "T1.2") (para "text")))
;       (*section 1 (title "T2") (para "text")))
