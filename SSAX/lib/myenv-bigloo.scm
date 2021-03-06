; 			My Standard Scheme "Prelude"
; $Id: myenv-bigloo.scm,v 1.9 2004/07/08 21:53:32 oleg Exp $

; assert the truth of an expression (or of a sequence of expressions)
;
; syntax: assert ?expr ?expr ... [report: ?r-exp ?r-exp ...]
;
; If (and ?expr ?expr ...) evaluates to anything but #f, the result
; is the value of that expression.
; If (and ?expr ?expr ...) evaluates to #f, an error is reported.
; The error message will show the failed expressions, as well
; as the values of selected variables (or expressions, in general).
; The user may explicitly specify the expressions whose
; values are to be printed upon assertion failure -- as ?r-exp that
; follow the identifier 'report:'
; Typically, ?r-exp is either a variable or a string constant.
; If the user specified no ?r-exp, the values of variables that are
; referenced in ?expr will be printed upon the assertion failure.

(define-macro (assert expr . others)
			; given the list of expressions or vars,
			; make the list appropriate for cerr
  (define (make-print-list prefix lst)
    (cond
     ((null? lst) '())
     ((symbol? (car lst))
      (cons #\newline
	(cons (list 'quote (car lst))
	  (cons ": " (cons (car lst) (make-print-list #\newline (cdr lst)))))))
     (else 
      (cons prefix (cons (car lst) (make-print-list "" (cdr lst)))))))

			; return the list of all unique "interesting"
			; variables in the expr. Variables that are certain
			; to be bound to procedures are not interesting.
			; We perform macro-expansion first to avoid tripping
			; on user-defined macros
  (define (vars-of expr)
    (let loop ((expr (expand expr)) (vars '()))
      (cond
       ((not (pair? expr)) vars)	; not an application -- ignore
       ((memq (car expr) 
	      '(quote let let* letrec lambda cond quasiquote case define do))
	vars)				; won't go there
       (else				; ignore the head of the application
	(let inner ((expr (cdr expr)) (vars vars))
	  (cond 
	   ((null? expr) vars)
	   ((symbol? (car expr))
	    (inner (cdr expr)
		   (if (memq (car expr) vars) vars (cons (car expr) vars))))
	   (else
	    (inner (cdr expr) (loop (car expr) vars)))))))))

  (cond
   ((null? others)		; the most common case
    `(or ,expr (begin (cerr "failed assertion: " ',expr nl "bindings"
			    ,@(make-print-list #\newline (vars-of expr)) nl)
		      (error "assertion failure"))))
   ((eq? (car others) 'report:) ; another common case
    `(or ,expr (begin (cerr "failed assertion: " ',expr
			    ,@(make-print-list #\newline (cdr others)) nl)
		      (error "assertion failure"))))
   ((not (memq 'report: others))
    `(or (and ,expr ,@others)
	 (begin (cerr "failed assertion: " '(,expr ,@others) nl "bindings"
		      ,@(make-print-list #\newline
			 (vars-of (cons 'and (cons expr others)))) nl)
		      (error "assertion failure"))))
   (else			; report: occurs somewhere in 'others'
    (let loop ((exprs (list expr)) (reported others))
      (cond
       ((eq? (car reported) 'report:)
	`(or (and ,@(reverse exprs))
	     (begin (cerr "failed assertion: " ',(reverse exprs)
			  ,@(make-print-list #\newline (cdr reported)) nl)
		    (error "assertion failure"))))
       (else (loop (cons (car reported) exprs) (cdr reported)))))))
)
    
(define-macro (assure exp error-msg) `(assert ,exp report: ,error-msg))

(define (identify-error msg args . disposition-msgs)
  (let ((port (current-error-port)))
    (newline port)
    (display "ERROR" port)
    (display msg port)
    (for-each (lambda (msg) (display msg port))
	      (append args disposition-msgs))
    (newline port)))

(define bigloo-error error)
(define error
  (lambda (msg . args)
    (bigloo-error "myerror" msg args)))

; Bigloo has an advanced module system
(define include (lambda (_) #f))
(define-macro (declare . x) '(begin #f)) ; Gambit-specific compiler-decl

; A few convenient functions that are not in Bigloo
(define (call-with-input-string str proc)
    (proc (open-input-string str)))
(define (call-with-output-string proc)
  (let ((port (open-output-string)))
    (proc port)
    (close-output-port port)))


; like cout << arguments << args
; where argument can be any Scheme object. If it's a procedure
; (without args) it's executed rather than printed (like newline)

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x) (x (current-error-port)) (display x (current-error-port))))
            args))

;(define-macro (nl) '(newline))
(define nl (string #\newline))

; Legacy re-direction
(define-macro (read-line . x) `(read-text-line . ,x))

; Some useful increment/decrement operators
; Note, fixnum prefix is Gambit-specific, it means that the
; operands assumed FIXNUM (as they ought to be anyway).
; This perfix could be safely removed: it'll leave the code just as
; correct, but more portable (and less efficient)

				; Mutable increment
(define-macro (inc! x) `(set! ,x (+fx 1 ,x)))
(define-macro (++! x) `(set! ,x (+fx 1 ,x)))

				; Read-only increment
(define-macro (inc x) `(+fx 1 ,x))
(define-macro (++ x) `(+fx 1 ,x))

				; Mutable decrement
(define-macro (dec! x) `(set! ,x (-fx ,x 1)))
(define-macro (--! x) `(set! ,x (-fx ,x 1)))

				; Read-only decrement
(define-macro (dec x) `(-fx ,x 1))
(define-macro (-- x) `(-fx ,x 1))

; Some useful control operators

			; if condition is true, execute stmts in turn
			; and return the result of the last statement
			; otherwise, return unspecified.
; (define-macro (when condition . stmts)
;   `(and ,condition (begin ,@stmts)))
  

			; if condition is false execute stmts in turn
			; and return the result of the last statement
			; otherwise, return unspecified.
			; This primitive is often called 'unless'
(define-macro (whennot condition . stmts)
  `(or ,condition (begin ,@stmts)))


			; Execute a sequence of forms and return the
			; result of the _first_ one. Like PROG1 in Lisp.
			; Typically used to evaluate one or more forms with
			; side effects and return a value that must be
			; computed before some or all of the side effects happen.
(define-macro (begin0 form . forms)
  (let ((var (gensym)))
    `(let ((,var ,form)) ,@forms ,var)))

			; Prepend an ITEM to a LIST, like a Lisp macro PUSH
			; an ITEM can be an expression, but ls must be a VAR
(define-macro (push! item ls)
  `(set! ,ls (cons ,item ,ls)))

			; Is str the empty string?
			; string-null? str -> bool
			; See Olin Shiver's Underground String functions
(define-macro (string-null? str) `(zero? (string-length ,str)))

; Often-used OS-specific function
(define OS:file-length file-size)

; Support for let*-values form: SRFI-11
; We rely on a 'receive' form in the general case. In Bigloo,
; receive can handle both proper and improper 'arglists'

(define-macro (let*-values bindings . body)
  (if (null? bindings) (cons 'begin body)
      (apply (lambda (vars initializer)
	 (let ((cont 
		(cons 'let*-values
		      (cons (cdr bindings) body))))
	   (cond
	    ((and (pair? vars) (null? (cdr vars))) ; a single var optimization
	     `(let ((,(car vars) ,initializer)) ,cont))
	   (else			; the most generic case
	    `(receive ,vars ,initializer ,cont)))))
       (car bindings))))

; Look up a value associated with a symbolic key in alist 
; ((key value) ...) or ((key . value) ...)
; and return the associated value.
; If the association has the form
;   (key . value) where value is not a pair --> return value
;   (key   value) where value is not a pair --> return value
;   (key value1 value2 value3 ...) -> return (value1 value2 value3 ...)
; that is, the procedure tries to do the right thing for
; both kinds of associative lists. The downside is that the result
; is never the list of one element (that is, the (list value) is
; synonymous with value for this procedure).
; We use a pseudo-keyword argument warn: as a modifier.
; These are not really keyword arguments (although they may be,
; if the Scheme system turns out DSSSL-compatible)
; 
; (lookup key alist)  -- lookup the key in the alist and return the
;                        associated value. Raise an error if key is not
;                        found.
; (lookup key alist default-exp)
;                     -- lookup the key in the alist and return the associated
;                        value. If the key is not found, evaluate
;                        the default-exp and return its result.
; (lookup key alist warn: default-exp)
;                     -- the same as above. In addition, write a warning
;                        (using cerr above) if the key is not found.


; assoc-primitives with a default clause
;
; If the search in the assoc list fails, the default action argument
; is returned. If this default action turns out to be a thunk,
; the result of its evaluation is returned.
; If the default action is not given, an error is signaled

(define-macro (assq-def key alist . default-action-arg)
  (let ((default-action
        (if (null? default-action-arg)
          `(error "failed to assq key '" ,key "' in a list " ,alist)
          (let ((defact-symb (gensym)))
	    `(let ((,defact-symb ,(car default-action-arg)))
               (if (procedure? ,defact-symb) (,defact-symb) ,defact-symb))))))
    `(or (assq ,key ,alist) ,default-action)))

(define-macro (assv-def key alist . default-action-arg)
  (let ((default-action
        (if (null? default-action-arg)
          `(error "failed to assv key '" ,key "' in a list " ,alist)
          (let ((defact-symb (gensym)))
	    `(let ((,defact-symb ,(car default-action-arg)))
               (if (procedure? ,defact-symb) (,defact-symb) ,defact-symb))))))
    `(or (assv ,key ,alist) ,default-action)))

(define-macro (assoc-def key alist . default-action-arg)
  (let ((default-action
        (if (null? default-action-arg)
          `(error "failed to assoc key '" ,key "' in a list " ,alist)
          (let ((defact-symb (gensym)))
	    `(let ((,defact-symb ,(car default-action-arg)))
               (if (procedure? ,defact-symb) (,defact-symb) ,defact-symb))))))
    `(or (assoc ,key ,alist) ,default-action)))


			; Convenience macros to avoid quoting of symbols
			; being deposited/looked up in the environment
(define-macro (env.find key) `(%%env.find ',key))
(define-macro (env.demand key) `(%%env.demand ',key))
(define-macro (env.bind key value) `(%%env.bind ',key ,value))

; define-opt: A concise definition allowing optional arguments.
; Example:
;
; (define-opt (foo arg1 arg2 (optional arg3 (arg4 init4))) body)
;
; The form define-opt is designed to be as compatible with DSSSL's
; extended define as possible -- while avoiding the non-standard
; lexical token #!optional. On systems that do support DSSSL (e.g.,
; Gambit, Bigloo, Kawa) our define-opt expands into DSSSL's extended
; define, which is implemented efficiently on these systems.
;
; Here's the relevant part of the DSSSL specification, lifted
; from Gambit's online documentation:

;   define-formals = formal-argument-list | r4rs-define-formals
;   formal-argument-list = reqs opts rest keys
;   reqs = required-formal-argument*
;   required-formal-argument = variable
;   opts = #!optional optional-formal-argument* | empty
;   optional-formal-argument = variable | ( variable initializer )
;   rest = #!rest rest-formal-argument | empty
;   rest-formal-argument = variable
;   keys = #!key keyword-formal-argument* | empty
;   keyword-formal-argument = variable | ( variable initializer )
;   initializer = expression
;   r4rs-lambda-formals = ( variable* ) | ( variable+ . variable ) | variable
;   r4rs-define-formals = variable* | variable* . variable
;
;   1. Variables in required-formal-arguments are bound to successive actual
;      arguments starting with the first actual argument. It shall be an error
;      if there are fewer actual arguments than required-formal-arguments.
;   2. Next variables in optional-formal-arguments are bound to remaining
;      actual arguments. If there are fewer remaining actual arguments than
;      optional-formal-arguments, then the variables are bound to the result
;      of evaluating initializer, if one was specified, and otherwise to #f.
;      The initializer is evaluated in an environment in which all previous
;      formal arguments have been bound.
;   It shall be an error for a variable to appear more than once in a
;   formal-argument-list.
;   It is unspecified whether variables receive their value by binding or by
;   assignment.
;
; Our define-opt does not currently support rest and keys arguments.
; Also, instead of #optional optional-formal-argument ...
; we write (optional optional-formal-argument ...)
; 
; Our define-opt is similar to PLT Scheme's opt-lambda. However, 
; the syntax of define-opt guarantees that optional arguments are 
; really at the very end of the arg list.


; Bigloo supports DSSSL extended defines and lambdas, Therefore
; define-opt expands to that.

(define-macro (define-opt bindings body . body-rest)
  (let loop ((curr bindings) (reqd '()))
    (cond
      ((not (pair? curr))			; No optional bindings,
	`(define ,bindings ,body ,@body-rest))  ; regular define
      ((and (pair? (car curr)) (eq? 'optional (caar curr)))
	`(define ,(append (reverse (cons #!optional reqd))
		    (cdar curr) (cdr curr))
	   ,body ,@body-rest))
      (else (loop (cdr curr) (cons (car curr) reqd))))))

; Implementation of HANDLE-EXCEPTIONS of SRFI-12.
;
; Syntax: handle-exceptions VAR HANDLE-EXPR EXPR1 EXPR2 ...
; 
; Evaluates the body expressions EXPR1, EXPR2, ... in sequence with an
; exception handler constructed from VAR and HANDLE-EXPR. Assuming no
; exception is raised, the result(s) of the last body expression
; is(are) the result(s) of the handle-exceptions expression.
;
; The exception handler created by handle-exceptions restores the
; dynamic context (continuation, exception handler, etc.) of the
; handle-exceptions expression, and then evaluates HANDLE-EXPR with
; VAR bound to the value provided to the handler.

; Bigloo's native 'try' form already restores the dynamic
; environment before running the handler

(define-macro (handle-exceptions var handle-expr body . bodies)
  (let ((escape (gensym)) (proc (gensym)) (mes (gensym)) (obj (gensym)))
  `(try 
    (begin ,body ,@bodies)
			; If we raised the condition explicitly, the proc
			; is a pair, whose car is the
			; argument that was passed to 'abort' or 'signal'.
			; The cdr part of the pair is the
			; continuation (for a continuable exception)
    (lambda (,escape ,proc ,mes ,obj)
      (,escape			; Continue after 'try'
       (if (pair? ,proc)	; We've caught the exception thrown
	   (let ((,var (car ,proc)))   ; by abort or signal
	     ,handle-expr)
				; Otherwise, this is a native Bigloo error
	   (let ((,var
		  (make-property-condition
		   'exn		; condition kind required by SRFI-12
		   'message
		   (list ,proc ,mes ,obj))))
	     ,handle-expr))))
    )))

;      AND-LET* -- an AND with local bindings, a guarded LET* special form
;
; AND-LET* (formerly know as LAND*) is a generalized AND: it evaluates
; a sequence of forms one after another till the first one that yields
; #f; the non-#f result of a form can be bound to a fresh variable and
; used in the subsequent forms.
; It is defined in SRFI-2 <http://srfi.schemers.org/srfi-2/>
; This macro re-writes the and-let* form into a combination of
; 'and' and 'let'.
; See vland.scm for the denotational semantics and
; extensive validation tests.
; Bigloo has its own and-let* now -- alas, that and-let* fails
; many vland tests.
(define-macro (and-let* claws . body)
  (if (null? body)
      (cond 
       ((null? claws) #t)		; (and-let* () )
       ((not (pair? claws)) `(error "claws must be a list" ',claws))
       ; re-write (and-let* ((claw ... last-claw)) ) into
       ; (and-let* ((claw ...)) body) with 'body' derived from the last-claw
       (else
	(let* ((claws-rev (reverse claws))
	       (last-claw (car claws-rev))
	       (first-claws (reverse (cdr claws-rev)))
	       (new-body
		(if (pair? last-claw)
		    (if (null? (cdr last-claw))	; (and-let* (... (exp)) )
			(car last-claw)
			(cadr last-claw))	; (and-let* (... (var exp)) )
		    last-claw			; (and-let* (... var) )
		    )))
	  (list 'and-let* first-claws new-body))))
  ; generic case, 'body' is present
  (let loop ((claws claws))
    (cond
     ((null? claws)			; (and-let* () form ...)
      (cons 'begin body))
     ((not (pair? claws))
      `(error "claws must be a list" ',claws))
     ((not (pair? (car claws)))		; (and-let* ( var claw ...) body ...)
      (if (symbol? (car claws))
	  (list 'and (car claws) (loop (cdr claws)))
	  `(error "wrong claw:" ',(car claws))))
     ((null? (cdar claws))		; (and-let* ( (exp) claw... ) body ...)
      (list 'and (caar claws) (loop (cdr claws))))
     (else				; (and-let* ((var exp) claw...)body...)
      (list 'let (list (car claws))
	    (list 'and (caar claws) (loop (cdr claws)))))))
))
