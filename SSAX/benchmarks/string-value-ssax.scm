;*****************************************************************
; string-value-ssax
;
; A SSAX benchmark test
; Computing the string value of the root node of the DOM tree
; of the XML document.
;
; An XML document is loaded into memory first (from a file whose
; name is the single argument to this program). We then pass the
; content of that buffer one character at a time to SSAX (we let
; SSAX to read the document from a string port, to be precise).
;
; The computed string value is disregarded.
;
; $Id: string-value-ssax.scm,v 1.2 2003/04/29 01:40:35 oleg Exp $

(module string-value-ssax
	(include "myenv-bigloo.scm")
	(include "srfi-13-local.scm")
	(include "char-encoding.scm")
	(include "util.scm")
	(include "look-for-str.scm")
	(include "input-parse.scm")
	(include "SSAX-code.scm")
	(extern
	 (rusage-start::void () "rusage_start")
	 (rusage-report::void (string) "rusage_report")
	 )
	(main main))


(define parser-error error)
(define ssax:warn cerr)

(cond-expand
 (bigloo
  (define-macro (time x)
    `(begin
       (rusage-start)
       (begin0 ,x
	       (rusage-report "Timing report"))))
  ;(define OS:file-length file-size) Now in myenv-bigloo.scm
  )
 (else #f))

(define (load-file-to-string fname)
  (let ((size (OS:file-length fname)))
    (assert (positive? size))
    (cerr "Loading file " fname " of size " size " into memory" nl)
    (call-with-input-file fname
	(lambda (port)
	  (read-string size port)))))


; Parse the xml-doc and return the string-value of its root element
; (as a string)

(define (sax-root-string-value xml-doc)
  (call-with-input-string xml-doc
    (lambda (xml-port)
    ; Accumulate the text values of leaves in a seed, in reverse order
    (let ((result
	   ((ssax:make-parser
	     NEW-LEVEL-SEED 
	     (lambda (elem-gi attributes namespaces
			      expected-content seed)
	       seed)
   
	     FINISH-ELEMENT
	     (lambda (elem-gi attributes namespaces parent-seed seed)
	       seed)

	     CHAR-DATA-HANDLER
	     (lambda (string1 string2 seed)
	       (let* ((seed (cons string1 seed)))
		 (if (string-null? string2) seed
		     (cons string2 seed))))
	     )
	    xml-port '())))
      (string-concatenate-reverse/shared result)
    ))))


; The root module
(define (main argv)
  (if (not (= (length argv) (+ 1 1)))
      (error "One argument required: the name of the file to parse\n"))
  (let* ((content (load-file-to-string (cadr argv)))
	 (string-value (time (sax-root-string-value content))))
    (if (> (string-length string-value) 15)
	(cerr "The first 15 (out of " (string-length string-value)
	      " bytes of string-value: '" (substring string-value 0 15)
	      "'" nl))))

