; Converting SXML.scm into a SIGPLAN paper submission
; Evaluation of this file yields TeX data for an article

; IMPORT
; SXML-to-HTML-ext.scm and all of its imports
;
; $Id: SXML-paper.scm,v 3.1 2004/03/12 22:53:10 oleg Exp $

; Extract 'Content' from SXML.scm
; It is idefined in SXML.scm as
; (define Content '(...))
(define Content-raw
  (call-with-input-file "SXML.scm"
    (lambda (port)
      (let loop ()
	(let ((form (read port)))
	  (assert (not (eof-object? form)))
	  (if (and
	       (pair? form)
	       (eq? (car form) 'define)
	       (pair? (cdr form))
	       (eq? (cadr form) 'Content)
	       (pair? (cddr form)))
	      ; Found our content
	      (if (and 
		   (pair? (caddr form))
		   (eq? (caaddr form) 'quote))
		  (car (cdaddr form))
		  (caddr form))
	      (loop)		; else keep looking
	      ))))))


	    
; Transform the raw content a little bit
; We replace the SXML code between (Section _ "Acknowledgment")
; and the next Section or References with the new code.

(define Content
  (let* ((acknowledgment
	  '((Acknowledgment "Acknowledgments")
	    (p "I am indebted to Kirill Lisovsky for insightful
discussions and suggestions. This work has been supported in part by
the National Research Council Research Associateship Program and Naval
Postgraduate School.")))

	(step1
	 (replace-range
	  (lambda (node)
	    (and (pair? node) (eq? 'Section (car node)) 
		 (equal? '("Acknowledgment") (cddr node))
		 acknowledgment))
	  (lambda (node)
	    (and (pair? node) (or (eq? 'Section (car node)) 
				  (eq? 'References (car node)))
		 (list node)))
	  (list Content-raw)))
	)
    (car step1)
))

;(pp Content ##stderr)
;(exit)


; Given a string, check to make sure it does not contain characters
; such as '_' or '&' that require encoding. Return either the original
; string, or a list of string fragments with special characters
; replaced by appropriate "escape sequences"

(define string->goodTeX
  (make-char-quotator
   '((#\# . "\\#") (#\$ . "\\$") (#\% . "\\%") (#\& . "\\&")
     (#\~ . "\\textasciitilde{}") (#\_ . "\\_") (#\^ . "\\^")
     (#\\ . "$\\backslash$") (#\{ . "\\{") (#\} . "\\}"))))


; Place the 'body' within the LaTeX environment named 'tex-env-name'
; options is a string or a list of strings that specify optional or
; mandatory parameters for the environment
; Return the list of fragments (suitable for SRV:send-reply)
(define (in-tex-env tex-env-name options body)
  (list "\\begin{" tex-env-name "}" options nl
	body
	"\\end{" tex-env-name "}" nl))


(define (generate-TEX Content)

 (SRV:send-reply
  (pre-post-order Content
   `(
			; General converion rules
     (@
      ((*default*       ; local override for attributes
	. ,(lambda (attr-key . value) (cons attr-key value))))
      . ,(lambda (trigger . value) (list '@ value)))
     (*default* . ,(lambda (tag . elems)
		       (cerr "Tag: " tag " ignored" nl)
		       '()))
     (*text* . ,(lambda (trigger str) 
		  (if (string? str) (string->goodTeX str) str)))
     (n_		; a non-breaking space
      . ,(lambda (tag . elems)
	   (list "~" elems)))

     (html:begin
      . ,(lambda (tag . elems)
	   (list
	    "% SXML->TEX created this file" nl
	    "\\documentclass[10pt]{article}" nl
	    "\\usepackage[T1]{fontenc}" nl
	    "\\usepackage[latin1]{inputenc}" nl
	    "\\usepackage{times}" nl
	    "\\pagestyle{empty}" nl
	    "\\makeatletter" nl
	    "\\makeatother" nl
	    "\\sloppy" nl
	    "\\newcommand{\\minitab}[2][l]{\\begin{tabular}{#1}#2\\end{tabular}}" nl
	    "% The standard article.cls leaves out too big margins" nl
	    "\\setlength\\topmargin{-10pt}" nl
	    "\\setlength\\textwidth{504pt}" nl
	    "\\setlength\\textheight{53\\baselineskip}" nl
	    "\\setlength\\oddsidemargin{-0.3in}" nl
	    "\\setlength\\evensidemargin{-0.3in}" nl
	    nl
	    elems
	    )))

     (Header		; It is just the placeholder for the meta-information
      *preorder*
      . ,(lambda (tag . headers) '()))

     (body
      . ,(lambda (tag . elems)
	   (in-tex-env "document" '() elems)))

     

     (page-title	
      . ,(lambda (tag) 
	   (list "\\title{" "SXML Specification" "}" nl
		 "\\author{Oleg Kiselyov\\\\"
		 "FNMOC, Monterey, CA 93943" "\\\\" "oleg@pobox.com, oleg@acm.org"
		 "}" nl
		 "\\maketitle" nl)))

     ; Standard typography
     (em
      . ,(lambda (tag . elems)
	   (list "\\emph{" elems "}")))

     (p
      . ,(lambda (tag . elems)
	   (list elems nl nl)))

     (div
      . ,(lambda (tag . elems)
	   (list elems nl nl)))

     (strong
       . ,(lambda (tag . elems)
	    (list "\\textbf{" elems "}")))

     (br
      . ,(lambda (tag)
	   (list "\\\\ ")))

     (code
      . ,(lambda (tag . elems)
	   ;(in-tex-env "ttfamily" '() elems) ;
	   (list "\\texttt{" elems "}")
	   ))

     (cite
      . ,(lambda (tag key)
	   (list "\\cite{" key "}")))

     (ul			; Unnumbered lists
      . ,(lambda (tag . elems)
	   (in-tex-env "itemize" '() elems)))

     (ol			; Numbered lists
      . ,(lambda (tag . elems)
	   (in-tex-env "enumerate" '() elems)))

     (li
      . ,(lambda (tag . elems)
	   (list "\\item " elems nl)))

     (dl			; Definition list
				; deal with the sequences of dt/dd in CPS

      ; dl and dt are translated to procedures that take one argument:
      ; previously set label: list of fragments or #f if none
      ; The procedure returns a pair: (new-label . generate-fragments)
      ; Initially, label is #f

      ((dt			; The item title
	. ,(lambda (tag . elems)
	    (lambda (label)
	      (cons elems	; elems become the new label
		    (if label	; the label was set: we've seen dt without dd
			(list "\\item [" label "]" nl) ; empty body
			'())))))
       (dd			; The item body
	. ,(lambda (tag . elems)
	    (lambda (label)
	      (cons #f		; consume the existing label
		    (list "\\item [" (or label "") "] " elems nl)))))
       )
      . ,(lambda (tag . procs)	; execute provs generated by dt/dd
	  (let loop ((procs procs) (label #f) (accum '()))
	    (if (null? procs) (in-tex-env "description" '() (reverse accum))
		(let ((result ((car procs) label)))
		  (loop (cdr procs) (car result) (cons (cdr result) accum))))))
      )
	
     (URL 
      . ,(lambda (tag url) (list " \\texttt{" url "} ")))

     (LaTeX
      . ,(lambda (tag) "\\LaTeX{} "))

     (blockquote
      . ,(lambda (tag . elems)
	   (in-tex-env "quote" '() elems)))

     (Section	; (Section level "content ...")
      . ,(lambda (tag level head-word . elems)
	   (list #\\
		 (case level
		   ((1 2) "section")
		   ((3) "subsection")
		   ((4) "subsubsection")
		   (else (error "unsupported section level: " level)))
		 "{" head-word elems "}" nl)))

     (abstract			; The abstract of the document
      ((Revision
	. ,(lambda (tag)	; Find the Header in the Content
				; and create the revision record
	     (let ((header-parms (find-Header Content)))
	       (list "The present article specifies revision "
		     (lookup-def 'Revision header-parms #f) " of SXML. "))))
       (prod-note
	. ,(lambda (tag . master-url)
	     (list
	      "\\\\The master SXML specification file is written in SXML itself. The present paper is the result of translating that SXML code into \\LaTeX, using an appropriate \"stylesheet\". A different stylesheet converted the specification to HTML. The master file, the stylesheets and the corresponding HTML and \\LaTeX{} documents are available at "  master-url ".")))
       (keywords
	. ,(lambda (tag)
	     (let ((header-parms (find-Header Content)))
	       (list "\\\\Keywords: " 
		     (lookup-def 'keywords header-parms #f) "."))))

       )
      . ,(lambda (tag . abstract-body)
	   (in-tex-env "abstract" '() abstract-body))
      )

     (Acknowledgment
      . ,(lambda (tag title)
	   (list "\\subsection*{" title "}" nl)))

;      (References	; (References bibitem ...)
;       *preorder*	; (bibitem label key . text)
;       . ,(lambda (tag . bib-items)
; 	   (let
; 	       ((widest-label ; first search for the widest label
; 			      ; among the bibitems
; 		 (let loop ((items bib-items) (widest ""))
; 		   (if (null? items) widest
; 		       (loop (cdr items)
; 			     (let ((label (cadar items)))
; 			       (if (> (string-length label)
; 				      (string-length widest))
; 				   label widest))))))
; 		(processed-items
; 		 (map
; 		  (lambda (bib-item)
; 		    (apply
; 		     (lambda (tag label key . text)
; 		       (list "\\bibitem[" label "]{" key "} " text nl))
; 		     bib-item))
; 		  bib-items)))
; 	     (in-tex-env "thebibliography" (list "{" "" "}")
; 			 processed-items))))

      (References	; (References bibitem ...)
       ((bibitem
	 . ,(lambda (tag label key . text)
	      (list "\\bibitem{" key "} " text nl)))
	)
       . ,(lambda (tag . bib-items)
	    (in-tex-env "thebibliography" "{}"
			bib-items)))

     (verbatim	; set off pieces of code: one or several lines
      ((*text* . ; Different quotation rules apply within a verbatim block
	       ,(let ((string->goodTeX-in-verbatim
		      (make-char-quotator
		       '((#\# . "\\#") (#\$ . "\\$") (#\% . "\\%")
			 (#\& . "\\&") (#\~ . "\\textasciitilde{}")
			 (#\_ . "\\_")
			 (#\^ . "\\^")
			 (#\\ . "$\\backslash$")))))
		  (lambda (trigger str) 
		    (if (string? str) (string->goodTeX-in-verbatim str) str)))
	       ))
      . ,(lambda (tag . lines)
	   (in-tex-env "verbatim" '()
		       (map (lambda (line) (list "     " line nl))
			    lines))))

     (table
		; verbatim mode does not work in tabular <deep sigh> ...
		; we have to emulate
      ((verbatim	
	((*text* . ; Different quotation rules apply within a "verbatim" block
	       ,(let ((string->goodTeX-in-verbatim
		      (make-char-quotator
		       '((#\space "~")	; All spaces are "hard"
			 (#\# . "\\#") (#\$ . "\\$") (#\% . "\\%")
			 (#\& . "\\&") (#\~ . "\\textasciitilde{}")
			 (#\_ . "\\_") (#\^ . "\\^")
			 (#\\ . "$\\backslash$") (#\{ . "\\{")
			 (#\} . "\\}")))))
		  (lambda (trigger str) 
		    (if (string? str) (string->goodTeX-in-verbatim str) str)))
	       ))
	. ,(lambda (tag . lines)	
	     (map (lambda (line) (list "\\texttt{\\small " line "}\\\\"))
		  lines)))
       (tr		; elems ::= [(@ attrib ...)] td ...
			; we disregard all attributes of a row
			; The result is (td ...)
	. ,(lambda (tag . elems)
	     (if (and (pair? elems) (pair? (car elems))
		      (eq? '@ (caar elems)))
		 (cdr elems)
		 elems)))

       (td		; elems ::= [(@ attrib ...)] body ...
			; we're only interested in (align "alignment") attr
			; and the (colspan "number") attr
			; The result is ("alignment" colspan body ...)
			; where "alignment" will be #\l, #\c, #\r
			; (#\l if not given); colspan is the integer
	. ,(lambda (tag . elems)
	     (define (get-alignment attrs)
	       (cond
		((assq 'align attrs) =>
		 (lambda (attr)
		   ;(cerr "align attr: " attr nl)
		   (cond
		    ((string-ci=? (cadr attr) "left") #\l)
		    ((string-ci=? (cadr attr) "right") #\r)
		    ((string-ci=? (cadr attr) "center") #\c)
		    (else (error "wrong alignment attribute: " attr)))))
		(else #\l)))
	     (define (get-colspan attrs)
	       (cond
		((assq 'colspan attrs) =>
		 (lambda (attr)
		   (let ((val (string->number (cadr attr))))
		     (assert val)
		     val)))
		(else 1)))
	     (if (and (pair? elems) (pair? (car elems))
		      (eq? '@ (caar elems)))
		 (cons (get-alignment (cadar elems))
		   (cons (get-colspan (cadar elems))
			 (cdr elems)))
		 (cons (get-alignment '())
		   (cons (get-colspan '())
			elems)))))

       (th		; elems ::= [(@ attrib ...)] body ...
			; we're only interested in (align "alignment") attr
			; and the (colspan "number") attr
			; The result is ("alignment" colspan body ...)
			; where "alignment" will be #\l, #\c, #\r
			; (#\c if not given); colspan is the integer
	. ,(lambda (tag . elems)
	     (define (get-alignment attrs)
	       (cond
		((assq 'align attrs) =>
		 (lambda (attr)
		   ;(cerr "align attr: " attr nl)
		   (cond
		    ((string-ci=? (cadr attr) "left") #\l)
		    ((string-ci=? (cadr attr) "right") #\r)
		    ((string-ci=? (cadr attr) "center") #\c)
		    (else (error "wrong alignment attribute: " attr)))))
		(else #\c)))
	     (define (get-colspan attrs)
	       (cond
		((assq 'colspan attrs) =>
		 (lambda (attr)
		   (let ((val (string->number (cadr attr))))
		     (assert val)
		     val)))
		(else 1)))
	     (if (and (pair? elems) (pair? (car elems))
		      (eq? '@ (caar elems)))
		 (cons (get-alignment (cadar elems))
		   (cons (get-colspan (cadar elems))
			 (cdr elems)))
		 (cons (get-alignment '())
		   (cons (get-colspan '())
			elems)))))
       )
			; (table [(@ attrib ...)] tr ...
      . ,(lambda (tag row . rows)
	   (let*-values
	    (((attrs rows)
	      (if (and (pair? row) (eq? '@ (car row)))
		  (values (cadr row) rows)
		  (values '() (cons row rows))))
	     ((border?)
	      (cond
	       ((assq 'border attrs) => 
		(lambda (border-attr) (not (equal? "0" (cadr border-attr)))))
	       (else #f)))
	     ((dummy) (assert (pair? rows))) ; at least one row must be given
	     ((ncols) (length (car rows)))
	     ((tex-cols)
	      (let ((col-codes
		     (map (lambda (_) (if border? "l|" "l")) (car rows))))
		(if border?
		    (apply string-append 
			   (cons "|" col-codes))
		    (apply string-append col-codes))))
	     )
	    (list
	     (in-tex-env "table" "[ht]"
	      (in-tex-env "tabular" (list "{" "@{\\extracolsep{-25pt}}" 
					  tex-cols "}")
	       (list (and border? "\\hline\n")
	       (map
		(lambda (row)
		  (list
		   (list-intersperse
		    (map
		     (lambda (col)
		       (apply
			(lambda (alignment span . data)
			  (list "\\multicolumn{" span "}{" alignment "}{"
				"\\minitab[" alignment "]{"
				data "}}")
			  )
			col))
		     row)
		    " & ")
		   "\\\\" (and border? "\\hline") nl))
		rows))))
	     nl)
	    )))
	     
     ; Grammatical terms
     (nonterm		; Non-terminal of a grammar
      . ,(lambda (tag term)
	   (list "<" term ">")))

     (term-id		; terminal that is a Scheme id
      . ,(lambda (tag term)
	   (list term )))

     (term-str		; terminal that is a Scheme string
      . ,(lambda (tag term)
	   (list "\"" term "\"")))

     (term-lit		; a literal Scheme symbol
      . ,(lambda (tag term)
	   (list "{\\itshape " term "}")))

     (ebnf-opt		; An optional term
      . ,(lambda (tag term)
	   (list term "?")))

     (ebnf-*		; Zero or more repetitions
      . ,(lambda (tag term)
	   (list term "*")))

     (ebnf-+		; One or more repetitions
      . ,(lambda (tag term)
	   (list term "+")))

     (ebnf-choice	; Choice of terms
      . ,(lambda (tag . terms)
	   (list-intersperse terms " | ")))

     (sexp		; S-expression constructor
      . ,(lambda (tag . terms)
	   (list "\\textbf{(} " (list-intersperse terms " ") 
		 " \\textbf{)}")))

     (sexp-cons		; S-expression constructor, like cons
      . ,(lambda (tag car cdr)
	   (list "\\textbf{(} " car "\\textbf{ . }" cdr  
		 " \\textbf{)}")))

     (sexp-symb		; Symbol constructor
      . ,(lambda (tag . terms)
	   (list "\\textbf{$MAKE{-}SYMBOL$(}" 
		 terms "\\textbf{)}")))

     (sset		; A tagged unordered S-expression (i.e., a set)
      *macro*
      . ,(lambda (tag set-tag . terms)
	   `((code (strong "{")) ,(list-intersperse
					(cons set-tag terms) " ")
	      (code " " (strong "}")))))

     (production
      . ,(lambda (tag number lhs rhs . comment)
	   (list "{[}" number "{]} & \\texttt{" lhs "} & "
		 " $::=$ &"
		 " \\texttt{"
		 (if (pair? rhs)
		     (list-intersperse rhs " ")
		     rsh)
		 " } " comment)))

     (productions
      . ,(lambda (tag . prods)
	   (list
	    (in-tex-env "tabular"
			'("{"
			  "r" "r" "c" "p{2.8in}" "}")
			(map (lambda (item)
			       (list item "\\\\" nl))
			    prods))
	    "\\\\" nl)))
     )))
 )

(generate-TEX Content)

