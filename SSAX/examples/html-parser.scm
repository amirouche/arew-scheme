; An example of using the SSAX library: a permissive HTML parser
;
; IMPORT
; The following is a Bigloo-specific module declaration.
; Other Scheme systems have something similar.
; (module permissive-HTML-parser
; 	(include "myenv-bigloo.scm")
; 	(include "srfi-13-local.scm") ; or import from SRFI-13 if available
;	(include "char-encoding.scm")
; 	(include "util.scm")
; 	(include "look-for-str.scm")
; 	(include "input-parse.scm")
; 	(include "SSAX-code.scm"))
;
; $Id: html-parser.scm,v 1.4 2003/04/09 21:27:52 oleg Exp $

; To satisfy SSAX imports

(define parser-error error)
(define ssax:warn cerr)

(define doc
"<html> <head> <title> </title> <title> whatever </title> </head>
  <body> <a href=\"url\">link</a> <p align=center> <ul compact style='aa'>
  <p> BLah <!-- comment <comment> --> <i> italic <b> bold <tt> ened </i> still  &lt; bold </b>
  </body>
  <P> But not done yet...")

       ; given the list of fragments (some of which are text strings)
       ; reverse the list and concatenate adjacent text strings
(define (reverse-collect-str fragments)
  (if (null? fragments) '()	; a shortcut
      (let loop ((fragments fragments) (result '()) (strs '()))
	(cond
	 ((null? fragments)
	  (if (null? strs) result
	      (cons (apply string-append strs) result)))
	 ((string? (car fragments))
	  (loop (cdr fragments) result (cons (car fragments) strs)))
	 (else
	  (loop (cdr fragments)
		(cons
		 (car fragments)
		 (if (null? strs) result
		     (cons (apply string-append strs) result)))
		'()))))))


(define (make-start-tag name attrs) (vector 'START name attrs))
(define (make-end-tag name) (vector 'END name))

; Override SSAX function, to make it more permissive and to support
; "unquoted" attributes like <a href=url>link</a>
; and singular attributes like <ul compact>


(define ssax:read-attributes  ; ssax:read-attributes port entities
 (let ()
		; Read the AttValue from the PORT up to the delimiter
		; (which can be a single or double-quote character,
		; or even a symbol *eof*)
		; 'prev-fragments' is the list of string fragments, accumulated
		; so far, in reverse order.
		; Return the list of fragments with newly read fragments
		; prepended.
  (define (read-attrib-value delimiter port entities prev-fragments)
    (let* ((new-fragments
	    (cons
	     (next-token '() (cons delimiter
				   '(#\newline #\return #\space #\tab #\< #\&))
			 "XML [10]" port)
	     prev-fragments))
	   (cterm (read-char port)))
      (if (or (eof-object? cterm) (eqv? cterm delimiter))
	  new-fragments
          (case cterm
            ((#\newline #\space #\tab)
              (read-attrib-value delimiter port entities
				 (cons " " new-fragments)))
            ((#\return)
              (if (eqv? (peek-char port) #\newline) (read-char port))
              (read-attrib-value delimiter port entities
				 (cons " " new-fragments)))
            ((#\&)
              (cond
                ((eqv? (peek-char port) #\#)
                  (read-char port)
                  (read-attrib-value delimiter port entities
		     (cons (string (ssax:read-char-ref port)) new-fragments)))
                (else
		 (read-attrib-value delimiter port entities
		     (read-named-entity port entities new-fragments)))))
            ((#\<) (parser-error port "[CleanAttrVals] broken"))
            (else (parser-error port "Can't happen"))))))

		; we have read "&" that introduces a named entity reference.
		; read this reference and return the result of
		; normalizing of the corresponding string
		; (that is, read-attrib-value is applied to the replacement
		; text of the entity)
		; The current position will be after ";" that terminates
		; the entity reference
  (define (read-named-entity port entities fragments)
    (let ((name (ssax:read-NCName port)))
      (assert-curr-char '(#\;) "XML [68]" port)
      (ssax:handle-parsed-entity port name entities
	(lambda (port entities fragments)
	  (read-attrib-value '*eof* port entities fragments))
	(lambda (str1 str2 fragments)
	  (if (equal? "" str2) (cons str1 fragments)
	      (cons* str2 str1 fragments)))
	fragments)))

  (lambda (port entities)
    (let loop ((attr-list (make-empty-attlist)))
      (if (not (ssax:ncname-starting-char? (ssax:skip-S port))) attr-list
	  (let ((name (ssax:read-QName port)))
	    (if (eqv? #\= (ssax:skip-S port)) ; name=value
		(let ((delimiter (begin (read-char port) (ssax:skip-S port))))
		  (case delimiter
		    ((#\' #\") (read-char port) ; name="val" or name='val'
		     (loop 
		      (attlist-add attr-list 
			 (cons name 
			       (apply string-append
				   (reverse
				     (read-attrib-value delimiter port entities
						      '())))))))
		    (else			; name=val
		     (loop 
		      (attlist-add attr-list 
			 (cons name 
			       (next-token-of
				(lambda (c)
				  (cond
				   ((eof-object? c) #f)
				   ((char-alphabetic? c) c)
				   ((string-index "0123456789.-_" c) c)
				   (else #f)))
				port)))))))
		; name without any value, assume "name"
		(loop 
		 (attlist-add attr-list 
			      (cons name (symbol->string name))))
		)))))
))

(define (lex-html port)

  (define (str-handler fragment foll-fragment seed)
    (if (string-null? foll-fragment) (cons fragment seed)
	(cons* foll-fragment fragment seed)))

  (define (new-level-seed elem-gi attributes namespaces
			  expected-content seed)
    (cons (make-start-tag elem-gi attributes) seed))

  (define (finish-element elem-gi attributes namespaces parent-seed seed)
    ; Note, elem-gi is not guaranteed to be the same as the corresponding
    ; argument in new-level-seed.
    ; This is because we are very permissive and allow documents like
    ; <b><i>text</b></i>
    (cons (make-end-tag elem-gi) seed))

  (define (read-content port entities seed parent-seed attributes)
    (let*-values
     (((expect-eof?) #t)
      ((seed term-token)
       (ssax:read-char-data port expect-eof? str-handler seed)))
     (if (eof-object? term-token)
	 seed
	 (case (xml-token-kind term-token)
	   ((END)
	    (finish-element (xml-token-head term-token)
			    attributes '() parent-seed seed))
	   ((ENTITY-REF)
	    (let ((seed
		   (cond
		    ((assq (xml-token-head term-token)
			   ssax:predefined-parsed-entities) =>
			   (lambda (entity-text-assoc)
			     (str-handler (cdr entity-text-assoc) "" seed)))
		    (else seed)		; Ignore an unknown entity
		    )))
	      (read-content port entities seed parent-seed attributes)))
	   ((START)		; Start of a child element
	    (let ((seed
		   (handle-start-tag
		    (xml-token-head term-token)
		    port entities seed)))
	      (read-content port entities seed parent-seed attributes)))
	   (else
	    (error "Can't happen: " term-token))))))

  (define (handle-start-tag start-tag-head port entities parent-seed)
    (let*-values
     (((elem-gi attributes namespaces expected-content)
       (ssax:complete-start-tag start-tag-head port #f
				entities '()))
      ((seed)
       (new-level-seed elem-gi attributes
		       namespaces expected-content parent-seed)))
     (read-content port entities seed parent-seed attributes)))

  (reverse-collect-str (read-content port '() '() '() '()))
)


(cout nl "Permissive parsing of a not-very-valid HTML document" nl)
(cout "The source document:" nl doc nl "---End---" nl
      nl "The document is parsed to the following flat list:" nl)
(write
 (call-with-input-string doc lex-html))
(cout nl "---Done---")


