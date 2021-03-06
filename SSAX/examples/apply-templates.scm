; A simple example of XSLT-like 'apply-templates' form.
; For more advanced and real-life examples, see Kirill Lisovsky's STX.
;
; IMPORT
; The following is a Bigloo-specific module declaration.
; Other Scheme systems have something similar.
; (module apply-templates-simple
; 	(include "myenv-bigloo.scm")
; 	(include "srfi-13-local.scm") ; or import from SRFI-13 if available
;	(include "char-encoding.scm")
; 	(include "util.scm")
; 	(include "look-for-str.scm")
; 	(include "input-parse.scm")
; 	(include "SSAX-code.scm")
; 	(include "SXML-tree-trans.scm")
; 	(include "SXML-to-HTML.scm")
; 	(include "SXPath-old.scm")
; 	)
;
; $Id: apply-templates.scm,v 1.3 2003/04/09 21:27:52 oleg Exp $


; Running examples: SXML data structures

(define tree1 
  '(html
    (head (title "Slides"))
    (body
     (p (@ (align "center"))
	(table (@ (style "font-size: x-large"))
	       (tr
		(td (@ (align "right")) "Talks ")
		(td (@ (align "center")) " = ")
		(td " slides + transition"))
	       (tr (td)
		   (td (@ (align "center")) " = ")
		   (td " data + control"))
	       (tr (td)
		   (td (@ (align "center")) " = ")
		   (td " programs"))))
     (ul
      (li (a (@ (href "slides/slide0001.gif")) "Introduction"))
      (li (a (@ (href "slides/slide0010.gif")) "Summary")))
     )))


(define tree2
  '(Forecasts (@ (TStamp "958082142"))
     (TAF (@ (TStamp "958066200") (LatLon "36.583, -121.850")
	     (BId "724915") (SName "KMRY, MONTEREY PENINSULA"))
	  (VALID (@ (TRange "958068000, 958154400")) "111730Z 111818")
	  (PERIOD (@ (TRange "958068000, 958078800"))
		  (PREVAILING "31010KT P6SM FEW030"))
	  (PERIOD (@ (TRange "958078800, 958104000") (Title "FM2100"))
		  (PREVAILING "29016KT P6SM FEW040"))
	  (PERIOD (@ (TRange "958104000, 958154400") (Title "FM0400"))
		  (PREVAILING "29010KT P6SM SCT200")
		  (VAR (@ (Title "BECMG 0708")
			  (TRange "958114800, 958118400"))
		       "VRB05KT"))
)))



; Pre-order traversal of a tree and creation of a new tree:
;	apply-templates:: tree x <templates> -> <new-tree>
; where
; <templates> ::= (<template> ...)
; <template>  ::= (<node-test> <node-test> ... <node-test> . <handler>)
; <node-test> ::= an argument to node-typeof? above
; <handler>   ::= <tree> -> <new-tree>
;
; This procedure does a _normal_, pre-order traversal of an SXML
; tree.  It walks the tree, checking at each node against the list of
; matching templates.
; If the match is found (which must be unique, i.e., unambiguous), the
; corresponding handler is invoked and given the current node as an
; argument.  The result from the handler, which must be a <tree>,
; takes place of the current node in the resulting tree.
; The name of the function is not accidental: it resembles rather closely
; an 'apply-templates' function of XSLT.

(define (apply-templates tree templates)

		; Filter the list of templates. If a template does not
		; contradict the given node (that is, its head matches
		; the type of the node), chop off the head and keep the
		; rest as the result. All contradicting templates are removed.
  (define (filter-templates node templates)
    (cond
     ((null? templates) templates)
     ((not (pair? (car templates)))  ; A good template must be a list
      (filter-templates node (cdr templates)))
     (((node-typeof? (caar templates)) node)
      (cons (cdar templates) (filter-templates node (cdr templates))))
     (else
      (filter-templates node (cdr templates)))))

		; Here <templates> ::= [<template> | <handler>]
		; If there is a <handler> in the above list, it must
		; be only one. If found, return it; otherwise, return #f
  (define (find-handler templates)
    (and (pair? templates)
	 (cond
	  ((procedure? (car templates))
	   (if (find-handler (cdr templates))
	       (error "ambiguous template match"))
	   (car templates))
	  (else (find-handler (cdr templates))))))

  (let loop ((tree tree) (active-templates '()))
   ;(cout "active-templates: " active-templates nl "tree: " tree nl)
    (if (nodeset? tree)
	(map-union (lambda (a-tree) (loop a-tree active-templates)) tree)
	(let ((still-active-templates 
	       (append 
		(filter-templates tree active-templates)
		(filter-templates tree templates))))
	  (cond 
	   ;((null? still-active-templates) '())
	   ((find-handler still-active-templates) =>
	    (lambda (handler) (handler tree)))
	   ((not (pair? tree)) '())
	   (else
	    (loop (cdr tree) still-active-templates))))))
)


(cout nl "Give the list of all 'td' elements in tree1" nl)
; Compare with XSLT:
; <xsl:template match="td"><xsl:value-of/></xsl:template>
(pp
 (apply-templates tree1
		  `((td . ,(lambda (node) node)))
))
(newline)

(cout nl "Give the list of all 'href' attributes in tree1 that are children"
      nl "of 'a' elements" nl)
; Compare with XSLT:
; <xsl:template match="a/@href"><xsl:value-of/></xsl:template>
(pp
 (apply-templates tree1
		  `((a @ href . ,(lambda (node) node)))
))
(newline)

(cout nl "Give the list of all 'align' attributes in tree1 that are children"
      nl "of 'td' elements" nl)
; Compare with XSLT:
; <xsl:template match="td/@align"><xsl:value-of/></xsl:template>
(pp
 (apply-templates tree1
		  `((td @ align . ,(lambda (node) node)))
))
(newline)

(cout nl "Give the list of all 'Title' attributes in tree2 that are children"
      nl "of 'PERIOD' and 'VAR' elements" nl)
; Compare with XSLT:
; <xsl:template match="PERIOD/@Title/text()">Title-PERIOD <xsl:value-of/>
; </xsl:template>
; <xsl:template match="VAR/@Title/text()">Title-VAR <xsl:value-of/>
; </xsl:template>
(pp
 (apply-templates tree2
  `((PERIOD @ Title *text* . ,(lambda (node) (list 'Title-PERIOD node)))
    (VAR @ Title *text* . ,(lambda (node) (list 'Title-VAR node))))
  ))
(newline)


; XSLT processing in Scheme
; The following Scheme code transforms the tree2 above the same way
; as an XSL stylesheet
;	http://zowie.metnet.navy.mil/~spawar/JMV-TNG/XML/TC/taf.xsl
; does regarding the corresponding OMF XML document.
;
; This XSLT stylesheet is commented in part below, for easy reference.

; <xsl:template match="TAF">
;   <P><BR/></P>
;   <TABLE BGCOLOR="#CCCCCC" width="100%" cellpadding="3">
;   <TR>
;     <TD><xsl:value-of select="@SName"/></TD>
;     <TD>Id: <xsl:value-of select="@BId"/></TD>
;     <TD>[<xsl:value-of select="@LatLon"/>]</TD>
;   </TR></TABLE>

;   <TABLE width="100%" cellpadding="1" BORDER="1" RULES="none" FRAME="hsides">
;   <TR>
;   <TD WIDTH="20%">
;     <xsl:eval>conv_date(this.selectSingleNode('@TStamp').text)</xsl:eval>
;     <BR/>
;     <xsl:eval>conv_trange(this.selectSingleNode('VALID/@TRange').text)</xsl:eval>
;   </TD>
;   <TD>
;      <xsl:value-of select="VALID"/>
;      <xsl:apply-templates select="PERIOD/*"/>
;   </TD>
;   </TR>
;   </TABLE>
; </xsl:template>

; <xsl:template match="PERIOD/PREVAILING">
;    <DIV>
;    <xsl:attribute name="class">
;        <xsl:eval>period_class(this.selectSingleNode('../@TRange').text)</xsl:eval>
;    </xsl:attribute>
;    <xsl:if test="../@Title"><xsl:value-of select="../@Title"/> </xsl:if>
;    <xsl:value-of />
;    </DIV>
; </xsl:template>

; <xsl:template match="PERIOD/VAR">
;    <DIV class="var">
;    <xsl:value-of select="@Title"/> <xsl:value-of />
;    </DIV>
; </xsl:template>

; Scheme XSLT code follows. It does not need to resort to xsl:eval :
; regular Scheme backquote suffices. The Scheme code is also shorter,
; and well integrated.

; A rotten skeleton implementation of SRFI-19, just to make
; the following example run

(define (make-date nanosecond second minute hour day month year zone-offset)
  (assert (zero? zone-offset))
  ;        0     1    2   3    4      5      6
  (vector year month day hour minute second nanosecond))

(define (date-minute date) (vector-ref date 4))
(define (date-hour date)   (vector-ref date 3))
(define (date-day date)    (vector-ref date 2))
(define (date-month date)  (vector-ref date 1))
(define (date-year date)   (vector-ref date 0))

; works only for time within May 11, 2000. Enough for our example
(define (time-utc->date time . tz-offset)
  (assert (equal? tz-offset '(0)))
  ;(assert (<= 958003200 time (- (+ 958003200 (* 24 3600)) 1)))
  (let* ((sec-of-day (- time 958003200))
	 (min-of-day (quotient sec-of-day 60))
	 (hr-of-day (quotient min-of-day 60)))
  (make-date 0 (modulo sec-of-day 60)
	     (modulo min-of-day 60)
	     (modulo hr-of-day 24)
	     11
	     5
	     2000
	     0)))

		; trange-str is a string of two numbers (epoch secs) 
		; separated by a ", "
		; The following function returns the numbers as a pair
(define (conv-TRange trange-str)
  (call-with-input-string trange-str
    (lambda (port)
      (let* ((tstart (read port))
	     (delim (read-char port))
	     (tend (read port)))
	(if (and (eq? delim #\,) (number? tstart) (number? tend))
	    (cons tstart tend)
	    (error "Invalid Trange string: " trange-str))))))

 
(define (handle-TAF TAF-elem)
  (define time-current 958078810)
		; Handle PREVAILING grandchild of a TAF-elem
  (define (handle-Prevailing elem)
    (let ((trange (conv-TRange (car 
	     ((sxpath `((PERIOD  ((eq? ,elem))) @ TRange *text*))
	      TAF-elem)))))
      `(div (@ (class ,(if (<= (car trange) time-current (cdr trange))
			   "per_c" "per_nc")))
	    ,((sxpath `((PERIOD ((eq? ,elem))) @ Title *text*)) TAF-elem)
	    " "
	    ,((sxpath '(*text*)) elem))))

  `((p (br))
    (table (@ (bgcolor "#CCCCCC") (width "100%") (cellpadding "3"))
     (tr
      (td ,((sxpath '(@ SName *text*)) TAF-elem))
      (td "Id: " ,((sxpath '(@ BId *text*)) TAF-elem))
      (td "[" ,((sxpath '(@ LatLon *text*)) TAF-elem) "]")
      ))
    (table 
     (@ (width "100%") (cellpadding "1") (BORDER "1") (RULES "none")
	(FRAME "hsides"))
     (tr (td (@ (width "20%"))
	     ,(let ((date (time-utc->date 
			   (string->number 
			    (car ((sxpath '(@ TStamp *text*)) TAF-elem)))
			   0)))
		(list (date-month date) "-" (date-day date) " "
		      (date-hour date) ":" (date-minute date)))
	     (br)
	     ,(let ((trange
		     (conv-TRange
		      (car ((sxpath '(VALID @ TRange *text*)) TAF-elem))))
		    (format-date
		     (lambda (tstamp)
		       (let ((date (time-utc->date  tstamp 0)))
			 (list (date-day date) " "
			       (date-hour date) ":" (date-minute date))))))
		(list (format-date (car trange)) " - "
		      (format-date (cdr trange))))
	     )
	 (td ,((sxpath '(VALID *text*)) TAF-elem)
	     ,(apply-templates TAF-elem
		 `((PERIOD PREVAILING . ,handle-Prevailing)
		   (PERIOD VAR . ,(lambda (elem)
				    `(div (@ (class "var"))
					  ,((sxpath '(@ Title *text*)) elem)
					  " "
					  ,((sxpath '(*text*)) elem))))
		   ))
	     )
     )))
)


(define tree2-result 
  (apply-templates tree2
		   `((TAF . ,handle-TAF))
		   ))


(cout nl nl "The transformation of tree2 produced the following result:" nl)
(pp tree2-result)
(newline)

(cout nl "Here is the result of pretty printing of the transformed SXML"
      nl "tree into HTML:" nl)
(SXML->HTML tree2-result)
