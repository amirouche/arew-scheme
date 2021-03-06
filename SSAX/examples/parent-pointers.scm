;		Parent pointers in SXML trees
;
; This present code illustrates several ways of determining the parent
; of an element in an SXML tree. Some of the approaches rely on
; "pointers" of various kinds from an element to its parent. The
; "pointer" and similar auxiliary data are maintained in annotations
; of an SXML element. We illustrate how to build these auxiliary data
; structures -- "indices" -- as we parse an XML document into SXML.
;
; To avoid clutter, the custom XML parsers below specifically do not
; handle attributes and namespaces. This assumption makes the code
; simpler and more lucid. See the ssax:sxml->xml function in
; ../lib/SSAX.scm for a production example, which deals with both
; attributes and namespaces.
;
; This code accompanies an article "On parent pointers in SXML trees".
; The article is posted on the SSAX-SXML mailing list and available as
; parent-pointers.txt
;
; $Id: parent-pointers.scm,v 1.4 2004/07/07 16:02:30 sperber Exp $

; IMPORT
; The following is a Bigloo-specific module declaration.
; Other Scheme systems have something similar.
;
; See the Makefile for the rules to run this example on Bigloo, SCM
; and other systems
; (module parent-pointers
; 	(include "myenv-bigloo.scm")
; 	(include "srfi-13-local.scm") ; or import from SRFI-13 if available
;	(include "char-encoding.scm")
; 	(include "util.scm")
; 	(include "look-for-str.scm")
; 	(include "input-parse.scm")
; 	(include "SSAX-code.scm")
; 	(include "SXML-tree-trans.scm")
; 	(include "SXPath-old.scm")
; 	)

; If the pretty-printer is available, use it. Otherwise, use 'display'
; If display-circle is available or a regular pretty-printer can handle
; circular lists, use them. Otherwise, refuse to display circular data
; structures
;; (cond-expand
;;   (bigloo
;;     #f)					; pp and display-circle are natively
;; 					; available
;;   ((or scm gambit)
;;     					; pp is natively available
;;     (define (display-circle x)          ; display-circle is not
;;       (display "Cannot safely display circular datastructures. Use SRFI-38")
;;       (newline)))
;;   ((or petite-chez)
;;     (define pp pretty-print))
;;   (else
;;     (define pp display)		         ; Fall-back to display
;;     (define (display-circle x)
;;       (display "Cannot safely display circular datastructures. Use SRFI-38")
;;       (newline))))

; A sample document used throughout the example
; It is an RSS description of the latest news from a fictious
; source. No element in the example has any attributes: for
; clarity, the parsers below skip the attributes anyway.
(define doc
  "<RDF>
   <channel>
     <title>The source</title>
     <link>http://www.origin/</link>
     <description>All the news that fit</description>
   </channel>
   <item>
    <title>Parent pointer</title>
    <link>http://www.origin/1</link>
   </item>
   <item>
    <title>Better solution</title>
    <link>http://www.origin/7</link>
   </item>
  </RDF>")

;-------------------------
; Auxiliary functions 

(define (string-whitespace? str)
  (let ((len (string-length str)))
    (cond
     ((zero? len) #t)
     ((= 1 len) (char-whitespace? (string-ref str 0)))
     ((= 2 len) (and (char-whitespace? (string-ref str 0))
		     (char-whitespace? (string-ref str 1))))
     (else
      (let loop ((i 0))
	(or (>= i len)
	    (and (char-whitespace? (string-ref str i))
		 (loop (+ 1 i)))))))))

; Given the list of fragments (some of which are text strings) reverse
; the list and concatenate adjacent text strings. We also drop
; "insignificant" whitespace, that is, the whitespace in front of, 
; behind and between elements. The whitespace that is included in
; the character data is not affected.

(define (reverse-collect-str-drop-ws fragments)
  (cond 
    ((null? fragments) '())		; a shortcut
    ((and (string? (car fragments))	; another shortcut
       (null? (cdr fragments))	        ; remove trailing ws
       (string-whitespace? (car fragments))) '())
    (else
      (let loop ((fragments fragments) (result '()) (strs '())
		 (all-whitespace? #t))
	(cond
	  ((null? fragments)
	   (if all-whitespace? result	; remove leading ws
	     (cons (apply string-append strs) result)))
	  ((string? (car fragments))
	   (loop (cdr fragments) result (cons (car fragments) strs)
	     (and all-whitespace?
	       (string-whitespace? (car fragments)))))
	  (else
	    (loop (cdr fragments)
	      (cons
		(car fragments)
		(if all-whitespace? result
		  (cons (apply string-append strs) result)))
	      '() #t)))))))



; Useful functions to access an SXML tree and its parts

; The name of a proper SXML node
(define node-name car)

; The children of a proper SXML node (some of which may be annotations!)
(define node-children cdr)

; Check if a SXML node is a proper SXML node: it's not a text string,
; it's not a nodelist, it's not an attr list
(define (node-proper? sxml)
  (and (pair? sxml)
       (symbol? (node-name sxml))
       (not (eq? (node-name sxml) '@))))

; Check if a SXML node is the top node
(define (node-top? sxml)
  (and (pair? sxml) (eq? '*TOP* (node-name sxml))))

; Traverse the tree and execute f (pre-order) on all proper
; elements of the sxml tree except the *TOP*
(define (for-all-elems f sxml)
  (cond
    ((nodeset? sxml) (for-each (lambda (el) (for-all-elems f el)) sxml))
    ((not (node-proper? sxml)) #f)      ; skip non-proper SXML elements
    (else
      (if (not (node-top? sxml)) (f sxml))
      (for-all-elems f (node-children sxml)))))

; Print the ancestors of every element in the SXML tree but the *TOP* one.
; We rely on a user-supplied procedure 'get-parent'
(define (print-ancestors get-parent sxml)
  ; Compute the transitive closure of 'get-parent' with respect to
  ; elem
  ; Return the list of ancestors of 'elem', from the immediate parent upwards.
  (define (get-ancestors elem)
    (if (node-top? elem) '()
      (let ((parent (get-parent elem)))
	(cons (node-name parent)
	  (get-ancestors parent)))))
  (cout "Printing the ancestors of every element" nl)
  (for-all-elems
    (lambda (elem)
      (cout
	"Ancestors of " (node-name elem) " are: "
	(list-intersperse (get-ancestors elem) "-") nl))
    sxml)
  (newline)
)

; Obtain the value associated with a given key from annotations of a node

(define (aux-assq key node)
  (let*
    ((_ (assert (pair? node) (pair? (cdr node))))
     (attr-list (cadr node))
     (_ (assert (pair? attr-list) (eq? '@ (car attr-list))))
     (annotations (assq '@ (cdr attr-list)))
     (needed-assoc (assq key (cdr annotations)))
     (_ (assert key needed-assoc)))
    (cadr needed-assoc)))

; Obtain the value associated with a given key from attributes of a node

(define (attr-assq key node)
  (let*
    ((_ (assert (pair? node) (pair? (cdr node))))
     (attr-list (cadr node))
     (_ (assert (pair? attr-list) (eq? '@ (car attr-list))))
     (needed-assoc (assq key (cdr attr-list)))
     (_ (assert key needed-assoc)))
    (cadr needed-assoc)))

;------------------------------------------------------------------------
; Solution 1
; Find the parent of an node by searching the SXML tree

; The following procedure takes a rootnode (the top node or an
; appropriate branch node of a SXML tree) and returns a get-parent
; procedure.  The procedure get-parent takes a node and searches for
; the parent of the node.
; The rootnode does not have to be the root node of the whole
; SXML tree -- it may be a root node of a branch of interest.
; Given the notation of Philip Wadler's paper on semantics of XSLT,
;      parent(x) = { y | y=subnode*(root), x=subnode(y) }
; If no parent is found, we return #f.
;
; For a "production" version of this function, see node-parent in
; SXPath.scm. See also sxp:parent in Kirill Lisovsky's SXPath-ext
; http://cvs.sf.net/cgi-bin/viewcvs.cgi/*checkout*/ssax/sxml-tools/sxpasxpath-ext.scm
; sxp:parent relies on a more efficient breadth-first search for parents.
;
; The present approach to locating parents does not need any
; annotations on SXML nodes. We process SXML tree as it is constructed
; by ssax:xml->sxml.

(define (make-get-parent-by-search rootnode)
  (assert (node-proper? rootnode))	; it should look like an SXML node
  (lambda (elem)			; the returned get-parent procedure
    (let loop ((parent rootnode))
      (cond
	((not (pair? parent)) #f)
	((memq elem (node-children parent)) => (lambda (_) parent))
	(else (any? loop (node-children parent)))))))

(cout nl "Test 1. Determining parents by searching the SXML tree" nl)
(let ((sxml (call-with-input-string doc
	      (lambda (port) (ssax:xml->sxml port '())))))
  (pp sxml) (newline)
  (print-ancestors (make-get-parent-by-search sxml) sxml)
)


;------------------------------------------------------------------------
; Solution 2
; Real parent pointers
;
; The following approach to locating node parents relies on a SXML "tree"
; whose nodes are annotated with real parent pointers. To be more precise,
; each node except the *TOP* should have annotations with the following
; association
;   `(parent ,ptr-to-parent)
;
; where ptr-to-parent is a reference to the parent of that node.
;
; An SXML data structure with true parent pointers is no longer a
; tree. Rather, it is a general, cyclic graph. Care must be taken when
; displaying, comparing or processing such a data structure. In
; particular, we should use display-circle or SRFI-38 but never a
; naive display to output this SXML data structure. Otherwise, we fall
; into an infinite loop.
;
; We can construct the annotated SXML as we parse the corresponding
; XML document. The following custom XML parser xml->true-parent-sxml
; (an instantiation of SSAX) does that. The time overhead of adding
; annotations is therefore negligible. Locating a parent is also very
; fast: we merely need to extract the reference to the parent from
; annotations of a node.
;
; To create real parent pointers, we must do destructive updates.  It
; is not possible to build cyclic data structures in a strict language
; without mutations. The seed of a custom SSAX parser below is a pair
;	(growth-pt . parent-cell)
; where growth-pt is a cell that is the growth point of the SXML tree
; under construction. When we create a new cell for the tree,
; we add the cell to the cdr of the growth-pt. 
; Value 'parent-cell' is the pointer to the parent of the SXML element
; being constructed.
;
; Due to imperativity, the code is notably trickier.

(define (xml->true-parent-sxml doc)
  (call-with-input-string doc
    (lambda (port)
      (let* ((top (list '*TOP*))
	     (growth-pt top))
	((ssax:make-parser
	   ; Entering new element. Construct an SXML structure with
	   ; what's known so far: the element name and the annotations.
	   ; The element will be extended as the parser reads the element
	   ; content. The constructed SXML structure becomes the parent
	   ; for all XML elements that might follow. The new growth
	   ; point is established at the constructed element.
	   NEW-LEVEL-SEED 
	   (lambda (elem-gi attributes namespaces
		     expected-content seed)
	     (let* ((growth-pt (car seed))
		    (parent-el (cdr seed))
		    (new-elem		; newly constructed element
		      `(,elem-gi (@ (@ (parent ,parent-el)))))
		    (new-growth-pt (cdr new-elem)))
	       (cons new-growth-pt new-elem)))
   

	   ; Finished the element. Add the current element (which is
	   ; now fully constructed) to the growth point of the parent
	   ; element.
	   FINISH-ELEMENT
	   (lambda (elem-gi attributes namespaces parent-seed seed)
	     (let*
	       ((parent-growth-pt (car parent-seed))
		(this-elem (cdr seed))
		(cell (list this-elem)))   ; cell becomes the new growth point
	       (set-cdr! parent-growth-pt cell)
	       (cons cell (cdr parent-seed))))

           ; Add character data to the growth point of the current element
	   ; Return the seed with the advanced growth point.
	   ; If the character data is insignificant white space, disregard
	   ; it.
	   CHAR-DATA-HANDLER 
	   (lambda (string1 string2 seed)
	     (let* ((growth-pt (car seed))
		    (parent-el (cdr seed))
		    (char-data
		      (if (string-null? string2) string1
			(string-append string1 string2))))
	       (if (and (string-whitespace? char-data)
		     (not (string? (car growth-pt))))
		 seed  ; skip insignificant whitespace
		 (let ((cell (list char-data)))
		   (set-cdr! growth-pt cell)  ; cell becomes new growth pt
		   (cons cell parent-el)))))
	   )
	  port
	  (cons growth-pt top) ; initial seed
	  )
	top				; the result
	))))

; Determining the parent of a node in an SXML tree with true
; parent pointers
; We expect that each SXML element has the form
;	(name (@ (@ (parent parent-pt))) ...)
; that is, contains annotations with the 'parent' association.
; We extract the parent pointer and return it.

(define (get-parent-from-true-pt elem)
  (aux-assq 'parent elem))

; Note. the following expression relies on a Bigloo-specific
; procedure 'display-circle'. We can substitute SRFI-38 instead.
; The regular 'display' will loop forever.

(cout nl "Test 2. The SXML tree with true parent pointers" nl)
(let ((sxml (xml->true-parent-sxml doc)))
  (display-circle sxml)(newline)
  (print-ancestors get-parent-from-true-pt sxml)
)


;------------------------------------------------------------------------
; Solution 3
; Building a genuine SXML _tree_ with thunked parent pointers,
; in a pure-functional way
;
; The present solution also relies on a SXML tree whose nodes are
; annotated with parent pointers. To be more precise, each node except
; the *TOP* should have annotations with the following association
;   `(parent ,thunk-ptr-to-parent)
;
; In contrast to Solution 2 however, thunk-ptr-to-parent is not the
; reference to the parent of the node. Rather, it is a nullary
; procedure, a thunk, which, when applied, yields an SXML node that is
; the parent of the current node.
;
; There is another difference from the true parent pointers approach.
; An SXML tree with thunked parent pointers can be built without any
; mutations whatsoever, in a pure functional way. Furthermore, the
; SXML datastructure with thunked parent annotations is _truly_ a
; tree.  We must incur however a separate tree traversal pass -- which
; takes an SXML tree without parent annotations (e.g., the output of the
; ordinary ssax:xml->sxml) and returns an SXML tree with annotations.
;
; There is a subtlety in the current approach. Indeed, it sounds
; contradictory: how can we add backward pointers to a tree in a pure
; functional way and still maintain the tree property (there is only
; one path between any two nodes)? If we look carefully, we note an
; odd feature: if 'node' is a proper SXML node other than the *TOP*,
; then
;   (memq node (node-children (get-parent-from-thunk-pt node)))
; is actually #f. However
;   (member* node (node-children (get-parent-from-thunk-pt node)))
; is #t
; where member* is the ordinary procedure member that uses a special
; equal*? procedure to compare SXML nodes modulo their annotations.
;
; The odd feature arises from the fact that the double connected graph
; with forward and backward node pointers is "virtual". The graph is
; computed on demand as a *fixpoint* of an expression (elem-add-parent
; original-tree).  Because our parent pointers are thunks, we delay a
; fixpoint iteration until it is demanded. We therefore avoid the
; divergence of the fixpoint, notwithstanding the strict nature of our
; host language. The fact that a node is not a child of its parent (if
; we use eq? to compare nodes) -- but it is a child of its parent if
; we use equal*? -- should not matter in a pure functional
; context. Indeed, the notion of an extensional equality, eq?, does
; not exist in a pure functional context, as Henry Baker pointed
; out. In fact, pure functional languages like Haskell do not provide
; the eq? equality predicate.
;
; There is one advantage of such a construction: despite the thunked
; parent pointers, our tree remains a tree at all times. Therefore, we
; can safely print it out using 'display' and traverse it, without
; worrying about falling into an infinite loop.
;
; The present approach is perhaps more academic than practical. It
; does however make us think what a parent pointer is or could be. We
; thus come to the dictionary approach (Solution 4), which seems to
; give the best overall trade-off.


(define (add-thunk-parent-pt sxml)
  
  ; add the parent association to the list of SXML elements
  (define (nodelist-add-parent parent nlist)
    (map
      (lambda (elem)
	(cond
	 ((node-proper? elem)	; it's a true SXML node
	  (elem-add-parent parent elem))
	  (else elem)))
      nlist))

  ; add parent to the proper SXML node
  (define (elem-add-parent parent elem)
    (let*
      ((name (node-name elem))		
       (children (node-children elem))	; Here, we assume elem has no attrs
       (new-elem
	 (cons* name 
	   `(@ (@ (parent ,parent)))
	   (nodelist-add-parent		; Note an implicit fixpoint!
	     (lambda () (elem-add-parent parent elem))
	     children))))
      new-elem))

  (elem-add-parent (lambda () '()) sxml))


; Determining the parent of a node in an SXML tree with thunked
; parent pointers
; We expect that each SXML element has the form
;	(name (@ (@ (parent thunk-parent-pt))) ...)
; that is, contains annotations with the 'parent' association.
; We extract the thunk-parent-pt and invoke it to yield the reference
; to the parent.

(define (get-parent-from-thunk-pt elem)
  ((aux-assq 'parent elem)))

(cout nl "Test 3. The SXML tree with 'functional' parent pointers" nl)
(let ((sxml (add-thunk-parent-pt
	(call-with-input-string doc
	  (lambda (port) (ssax:xml->sxml port '()))))))
  (pp sxml) (newline)
  (print-ancestors get-parent-from-thunk-pt sxml)
)

;------------------------------------------------------------------------
; Solution 4
; Building an SXML tree with parent-pseudo-pointers
;
; The present solution also locates the parent of a node with the
; help of annotations attached to SXML tree nodes. To be more precise,
; each node except the *TOP* should have annotations with the
; following association
;  `(parent ,node-id-of-the-parent)
; 
; Here node-id-of-the-parent is a pseudo-pointer to the parent of the
; current node. A pseudo-pointer is an node-id, a symbol, which is a
; key in a dictionary of node-ids. The *TOP* element of our annotated
; SXML tree must have annotations with an association
;  `(node-id-dict ,dictionary-of-node-ids)
; where dictionary-of-node-ids is, in the present case, an assoc list of
; (node-id pointer). The performance of the get-parent procedure improves
; if we use a more advanced data structure for the dictionary of node ids,
; for example, a balanced tree or a hash table.
;
; We can construct the annotated SXML as we parse the corresponding
; XML document. The following custom XML parser xml->dict-parent-sxml
; (an instantiation of SSAX) does that. The time overhead of adding
; annotations is therefore negligible. To determine the parent of a
; node, we extract the id of the parent from annotations of the node,
; and locate the parent itself through the dictionary-of-node-ids. If
; the latter is implemented as a balanced tree or a hash table, the
; lookup should be reasonably fast.
;
; In contrast to the true-parent-pointer approach above (Solution 2),
; parent-pseudo-pointers annotations can be added to an SXML tree in a
; pure functional way. Absolutely no mutations to SXML nodes are
; necessary.  There is also another important difference between the
; two approaches.  An SXML data structure with true parent pointers is
; not a tree and is not a DAG. It contains directed cycles. The double
; connectivity means that if any node is reachable, all of the data
; structure is reachable. This fact presents a grave problem for the
; garbage collector and leads to a significant space leak. In
; contrast, an SXML data structure with parent-pseudo-pointers is a
; DAG. Each node but the *TOP* one is referenced from its parent and
; from the dictionary of node-ids. However, there are no directed
; cycles. If we retain a leaf of such a tree, the rest of the tree can
; be garbage collected.
;
; Again, we construct the annotated SXML DAG, in a pure functional way,
; with the help of a custom XML parser. The latter is an instantiation
; of the SSAX framework. Its seed is a list
;   (sxml-tree-so-far parent-node-id node-id-dict-so-far)

; The ID of the *TOP* node in the dictionary of node ids.
(define dict-top-node-id '*top-ref*)

(define (xml->dict-parent-sxml doc)
  (call-with-input-string doc
    (lambda (port)
   (let* ((resulting-seed
	    ((ssax:make-parser

	       NEW-LEVEL-SEED 
	       (lambda (elem-gi attributes namespaces
			 expected-content seed)
		 (let*-values
		   (((parent-sxml-tree parent-id dict)
		      (apply values seed)))
		   (list
		     '()	; tree so far on the current level
		     (gensym)   ; ID for this node, which is the parent
		                ; of the children to follow
		     dict)))
		 
	       FINISH-ELEMENT
	       (lambda (elem-gi attributes namespaces parent-seed seed)
		 (let*-values
		   (((parent-sxml-tree parent-id parent-dict)
		      (apply values parent-seed))
		    ((curr-sxml-tree current-id current-dict)
		      (apply values seed))
		    ((curr-sxml-tree)
		      (reverse-collect-str-drop-ws curr-sxml-tree))
		    ((new-sxml-elem)
		      (cons* elem-gi 
			`(@ (@ (parent ,parent-id)))
			curr-sxml-tree))
		    ((new-dict)
		      (cons (list current-id new-sxml-elem) current-dict))
		     )
		   (list
		     (cons new-sxml-elem parent-sxml-tree)
		     parent-id
		     new-dict)))

	       CHAR-DATA-HANDLER
	       (lambda (string1 string2 seed)
		 (let*-values
		   (((curr-sxml-tree parent-id dict)
		      (apply values seed)))
		   (list
		       ; grow the tree (in reverse order):
		       ; add the char data
		     (if (string-null? string2) (cons string1 curr-sxml-tree)
		       (cons* string2 string1 curr-sxml-tree))
		     parent-id
		     dict)))
	       )
	      port
	      (list		; initial seed
		'()
		dict-top-node-id
		'()		; empty dict
		))))

     ; Build the *TOP* node 
     (let*-values
       (((top-node-content id dict)
	 (apply values resulting-seed))
	((top-node-content) (reverse top-node-content))
	 )
     (cons* '*TOP*
       (list '@ 
	 (list 'node-id-dict dict))
       top-node-content))
     ))))

; The following procedure takes the *TOP* node of the annotated SXML
; tree above and extracts the node-dictionary from annotations of the
; top node.

; We return a get-parent procedure that searches for and locates the
; parent of a node it is applied to.

; If the parent of the current node has an id dict-top-node-id, we
; return the top node. Thus we _effectively_ (but not actually) "tie
; the knot"

(define (make-get-parent-from-dict topnode)
  (let ((dict (attr-assq 'node-id-dict topnode)))
    (lambda (elem)
      (let ((parent-id (aux-assq 'parent elem)))
	(cond
	  ((assq parent-id dict) => cadr)
	  ((eq? parent-id dict-top-node-id) topnode)
	  (else (error "Dictionary lookup failure for: " elem)))))))

(cout nl "Test 4. The SXML tree with id parent pointers" nl)
(let ((sxml (xml->dict-parent-sxml doc)))
  (pp sxml)(newline)
  (print-ancestors (make-get-parent-from-dict sxml) sxml)
)
