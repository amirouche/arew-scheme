;	Create a dependency list for SSAX/SXML examples
; to guide the Makefile in building the examples for all supported platforms
;
; We read the file packages.scm that describes the examples and
; their imports.
; We read the file ../lib/packages.scm that describes library files
; that provide the imports.
; We then generate the file target-dependencies in the Makefile format
; that lists targets for the examples and their dependencies.
; The main Makefile file includes that target-dependencies file and uses
; the list of pre-requisites when building a target. Please refer to the 
; Makefile in this directory for details.
;
; The target-dependencies file has the following format.
; It is made of a sequence of lines optionally interspersed with
; empty lines. Each non-empty-line has the format
;	<depend-label>=<filelist>
;
; where <depend-label> is the name of a SSAX/SXML example with the
; suffix "-depend" added to it. The names of the examples are the
; names of the modules (`structures') found in ./packages.scm.
; <filelist> is a space-separated list of files that are to be
; included into the build of the corresponding SSAX/SXML
; example. Those files are to be found in the ../lib directory.
;
; Sample line:
;	sxml-nesting-depth-label-depend= SXML-tree-trans.scm
;
;
; A file packages.scm contains one or several module expressions.
; Of only interest to us here are 'define-structure' expressions
; of the following format
;
; (define-structure <structure-name> <export-spec>
;   [<open-spec>]
;   [(begin ...)]
;   [(files <file-spec>...)])
;
; where <structure-name> is a symbol
; <open-spec> is (open <import-structure-spec> ...)
; <import-structure-spec> is either a structure name or
;	(subset <structure-name> (<function-name> ...))
; <file-spec> is either a string or a symbol. The latter represents
; the file name without the .scm extension.
;
; For example:
;
; (define-structure sxml-tree-trans sxml-tree-trans-interface
;   (open scheme
; 	assertions
; 	srfi-11 ; LET*-VALUES
; 	srfi-23) ; ERROR
;   (files "SXML-tree-trans.scm"))
;
;
; The file ./packages.scm describes SSAX/SXML examples and their dependencies.
; The file ../lib/packages.scm describes structures that provide
; the above dependencies. The exported structures may be associated with
; a file, in the ../lib directory. The exported structures may have their
; own dependencies, provides by other structures in the ../lib/packages.scm
; file. Some dependencies are `standard' and so are not defined anywhere.
; Their existence is silently assumed.
;
; To create the complete list of all library files that support imports
; for an SSAX/SXML example, we build the transitive closure of
; ../lib/packages.scm structures and the corresponding files.
; In other words, we effectively `chase the imports'.
; We must ensure that the order of library files is the same as the order
; of imports in import lists. The order of imports, and the order
; of loading the files matter!
;
; $Id: build-dependencies.scm,v 1.1 2004/08/06 23:02:41 oleg Exp $


(define example-structures-fname  "packages.scm")
(define library-structures-fname  "../lib/packages.scm")
(define target-dependencies-fname "target-dependencies")

; `Structures', that is, module expressions, are internally represented
; by the following `record' sd (short for `structure-definition'):
;	(structure-name (file-name ...) import-structure-name ...)
; where file-name is a string 


; Constructor for the `record' of the structure-definition 
(define make-sd cons*)

; Accessors for the structure-definition
(define (sd-name sd)    (car sd))
(define (sd-files sd)   (cadr sd))
(define (sd-imports sd) (cddr sd))

;-------------------------------------
; `Patches' to ../lib/packages.scm
; They account for functors (which we do not handle at present)
; and other structures that require a supporting file
; for Scheme systems other than Scheme48.

(define extra-lib-structures
  (list
    (make-sd
      'input-parses-vanilla
      '("input-parse.scm")
      '(scheme
	 ascii
	 (subset srfi-13 (string-concatenate-reverse))
	 define-opt
	 crementing
	 char-encodings
	 parser-errors-vanilla))
    (make-sd
      'ssax-vanilla
      '("SSAX-code.scm")
      '(scheme
	 oleg-utils control-flows find-strings
	 ascii
	 assertions
	 coutputs catch-errors
	 oleg-string-ports
	 input-parses-vanilla
	 ssax-warnings-vanilla
	 char-encodings
	 crementing
	 (subset srfi-1 (cons*))
	 srfi-6 ; OPEN-INPUT-STRING
	 srfi-11 ; LET-VALUES
	 (subset srfi-13 (string-index
			   string-null?
			   string-concatenate-reverse/shared
			   string-concatenate/shared))
	 srfi-23
	 ppretty-prints))
    (make-sd
      'char-encodings
      '("char-encoding.scm")
      '())
    (make-sd 
      'srfi-13
      '("srfi-13-local.scm")
      '())
    (make-sd
      'ppretty-prints
      '("ppretty-prints.scm")
      '())
    (make-sd
      'parser-errors-vanilla
      '("parser-errors-vanilla.scm")
      '())
    (make-sd
      'catch-errors
      '("catch-error.scm")
      '())
))


;-------------------------------------
; Utilities

; remove the first occurrence of an element from a list
(define (remq e lst)
  (cond 
    ((null? lst) lst)
    ((eq? (car lst) e) (cdr lst))
    (else (cons (car lst) (remq e (cdr lst))))))

; Left fold combinator for a single list
(define (fold kons knil lis1)
  (let lp ((lis lis1) (ans knil))
    (if (null? lis) ans
      (lp (cdr lis) (kons (car lis) ans)))))

(define (fold-right kons knil lis1)
    (let recur ((lis lis1))
       (if (null? lis) knil
	    (let ((head (car lis)))
	      (kons head (recur (cdr lis)))))))

; union of two lists. Try to keep the same order
; That is, we remove from l2 elements that are present in l1,
; and concatenate the two lists
(define (union l1 l2)
  (append l1
    (fold-right
      (lambda (e seed)
	(if (memq e l1) seed (cons e seed)))
      '()
      l2)))

; union of two associative lists
; If l1 has a key that is also present in l2, the l2's association
; is preferred.
(define (aunion l1 l2)
  (cond
    ((null? l1) l2)
    ((assq (caar l1) l2)  (aunion (cdr l1) l2))
    (else (cons (car l1)  (aunion (cdr l1) l2)))))



;-------------------------------------
; Given the port to the (usually open for a file packages.scm)
; read the structure definitions and return them in internalized format,
; as an (associative) list of structure-definition.

(define (read-structure-definitions port)
  (let loop ((sd-list '()))
    (let ((item (read port)))
      (cond
	((eof-object? item) sd-list)
	((not (pair? item)) (assert #f item)) ; shouldn't happen, probably
	((not (eq? (car item) 'define-structure))
	  (loop sd-list))
	(else
	  (let* ((sd-name  (cadr item))
		 (sd-attrs (cdddr item))
		 (files
		   (cond
		     ((assq 'files sd-attrs) => cdr)
		     (else '())))
		 (imports
		   (cond
		     ((assq 'open sd-attrs) => cdr)
		     (else '()))))
	    (loop
	      (cons
		(make-sd
		  sd-name
		  (map
		    (lambda (fname)
		      (if (string? fname) fname
			(string-append (symbol->string fname) ".scm")))
		    files)
		  (map
		    (lambda (import)
		      (cond
			((symbol? import) import)
			((and (pair? import) (eq? 'subset (car import)))
			  (cadr import))
			(else (assert #f import report: "bad import"))))
		    imports))
		sd-list))))))))


(define example-structures 
  (call-with-input-file example-structures-fname 
    read-structure-definitions))
(define library-structures
  (aunion
    (call-with-input-file library-structures-fname 
      read-structure-definitions)
    extra-lib-structures))

(cout nl "example-structures" nl)
(pp example-structures)
(cout nl "library-structures" nl)
(pp library-structures)


; Given the structure-definition list, convert it into the normal form.
; At each reduction step, we chose one definition of the form
;	(structure-name (file-name ...) 
;            import-structure-name1 import-structure-name ...)
; look up import-structure-name1, add the corresponding file names 
; to the file-name list and replace import-structure-name1 with
; import structures of import-structure-name1, minding the duplicates.
; Chances are, import-structure-name1 may be absent. That is not
; an error.
; We finish when no reduction applies, that is, all import-structure
; names have been resolved.
; Hopefully the import list is acyclic, so we terminate.


(define (transitive-closure-of-imports sd-list)
  ; Choose one sd to work on, or #f
  (define (choose-sd sd-list)
    (cond
      ((null? sd-list) #f)
      ((null? (sd-imports (car sd-list))) (choose-sd (cdr sd-list)))
      (else (car sd-list))))
      
  ; One reduction step
  (define (elaborate sd sd-list)
    (let* ((imports (sd-imports sd))
	   (target-import (car imports))
	   (imports (cdr imports))
	   (resolved-sd (assq target-import sd-list))
	   (files 
	     (if resolved-sd (union (sd-files resolved-sd)
			       (sd-files sd))
	       (sd-files sd)))
	   (imports
	     (if resolved-sd
	       (union (sd-imports resolved-sd) imports)
	       imports)))
      (make-sd (sd-name sd) files imports)))


  ; main fix-point loop
  (let fix ((sd-list sd-list))
    (let ((target-sd (choose-sd sd-list)))
      (if (not target-sd) sd-list
	(let ((sd-rem (remq target-sd sd-list)))
	  (fix
	    (cons
	      (elaborate target-sd sd-rem)
	      sd-rem))))))
)


(define resolved-library-structures
  (transitive-closure-of-imports library-structures))

(cout nl "resolved-library-structures" nl)
(pp resolved-library-structures)


; create the depenencies and write them out
; Note that the order of files corresponds the order of imports!
(define (create-dependency-list port)
  (cerr nl "creating dependecies" nl)
  (for-each
    (lambda (sd)
      (if (not (= 1 (length (sd-files sd))))
	(cerr nl "skipping weird dependency: " sd)
	(begin
	  (display (sd-name sd) port)
	  (display "-depend=" port)
	  (for-each
	    (lambda (fname)
	      (display fname port) (display #\space port))
	    (fold
	      (lambda (import seed)
		(let ((resolved-import 
			(assq import resolved-library-structures)))
		  (if resolved-import
		    ; keep the order of files matching the order of imports
		    ; that is, the deepest imports first
		    (union seed (sd-files resolved-import))
		    seed)))
	      '()
	      (sd-imports sd)))
	  (display #\newline port)
	  (display #\newline port))))
    example-structures))

(call-with-output-file target-dependencies-fname
  create-dependency-list)

(cerr nl "Done" nl)
