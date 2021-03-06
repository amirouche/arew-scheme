; 			My Standard Scheme "Prelude"
; Version for Petite Chez Scheme 6.0a
; $Id: myenv-chez.scm,v 1.2 2004/07/08 21:53:32 oleg Exp $

(define-syntax include
  (syntax-rules ()
    ((include "myenv.scm") (begin #f))
    ((include file) (load file))))

(define pp pretty-print)
;(define gensym gentemp)


(define-syntax declare	; Gambit-specific compiler-decl
  (syntax-rules () ((declare . x) (begin #f))))

; A few convenient functions that are not Chez
(define (call-with-input-string str proc)
    (proc (open-input-string str)))
(define (call-with-output-string proc)
  (let ((port (open-output-string)))
    (proc port)
    (get-output-string port)))
(define (with-input-from-string str thunk)
  (parameterize ((current-input-port (open-input-string str))) (thunk)))
(define (with-output-to-string thunk)
  (let ((port (open-output-string)))
    (parameterize ((current-output-port port)) (thunk))
    (get-output-string port)))


; Frequently-occurring syntax-rule macros

; A symbol? predicate at the macro-expand time
;	symbol?? FORM KT KF
; FORM is an arbitrary form or datum
; expands in KT if FORM is a symbol (identifier), Otherwise, expands in KF

(define-syntax symbol??
  (syntax-rules ()
    ((symbol?? (x . y) kt kf) kf)	; It's a pair, not a symbol
    ((symbol?? #(x ...) kt kf) kf)	; It's a vector, not a symbol
    ((symbol?? maybe-symbol kt kf)
      (let-syntax
	((test
	   (syntax-rules ()
	     ((test maybe-symbol t f) t)
	     ((test x t f) f))))
	(test abracadabra kt kf)))))

; A macro-expand-time memv function for identifiers
;	id-memv?? FORM (ID ...) KT KF
; FORM is an arbitrary form or datum, ID is an identifier.
; The macro expands into KT if FORM is an identifier, which occurs
; in the list of identifiers supplied by the second argument.
; All the identifiers in that list must be unique.
; Otherwise, id-memv?? expands to KF.
; Two identifiers match if both refer to the same binding occurrence, or
; (both are undefined and have the same spelling).

; (id-memv??			; old code. 
;   (syntax-rules ()
;     ((_ x () kt kf) kf)
;     ((_ x (y . rest) kt kf)
;       (let-syntax
; 	((test 
; 	   (syntax-rules (y)
; 	     ((test y _x _rest _kt _kf) _kt)
; 	     ((test any _x _rest _kt _kf)
; 	       (id-memv?? _x _rest _kt _kf)))))
; 	(test x x rest kt kf)))))


(define-syntax id-memv??
  (syntax-rules ()
    ((id-memv?? form (id ...) kt kf)
      (let-syntax
	((test
	   (syntax-rules (id ...)
	     ((test id _kt _kf) _kt) ...
	     ((test otherwise _kt _kf) _kf))))
	(test form kt kf)))))

; Test cases
; (id-memv?? x (a b c) #t #f)
; (id-memv?? a (a b c) 'OK #f)
; (id-memv?? () (a b c) #t #f)
; (id-memv?? (x ...) (a b c) #t #f)
; (id-memv?? "abc" (a b c) #t #f)
; (id-memv?? x () #t #f)
; (let ((x 1))
;   (id-memv?? x (a b x) 'OK #f))
; (let ((x 1))
;   (id-memv?? x (a x b) 'OK #f))
; (let ((x 1))
;   (id-memv?? x (x a b) 'OK #f))

; Commonly-used CPS macros
; The following macros follow the convention that a continuation argument
; has the form (k-head ! args ...)
; where ! is a dedicated symbol (placeholder).
; When a CPS macro invokes its continuation, it expands into
; (k-head value args ...)
; To distinguish such calling conventions, we prefix the names of
; such macros with k!

(define-syntax k!id			; Just the identity. Useful in CPS
  (syntax-rules ()
    ((k!id x) x)))

; k!reverse ACC (FORM ...) K
; reverses the second argument, appends it to the first and passes
; the result to K

(define-syntax k!reverse
  (syntax-rules (!)
    ((k!reverse acc () (k-head ! . k-args))
      (k-head acc . k-args))
    ((k!reverse acc (x . rest) k)
      (k!reverse (x . acc) rest k))))


; (k!reverse () (1 2 () (4 5)) '!) ;==> '((4 5) () 2 1)
; (k!reverse (x) (1 2 () (4 5)) '!) ;==> '((4 5) () 2 1 x)
; (k!reverse (x) () '!) ;==> '(x)


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


(define-syntax assert
  (syntax-rules ()
    ((assert _expr . _others)
     (letrec-syntax
       ((write-report
	  (syntax-rules ()
			; given the list of expressions or vars,
			; create a cerr form
	    ((_ exprs prologue)
	      (k!reverse () (cerr . prologue)
		(write-report* ! exprs #\newline)))))
	 (write-report*
	   (syntax-rules ()
	     ((_ rev-prologue () prefix)
	       (k!reverse () (nl . rev-prologue) (k!id !)))
	     ((_ rev-prologue (x . rest) prefix)
	       (symbol?? x
		 (write-report* (x ": " 'x #\newline . rev-prologue) 
		   rest #\newline)
		 (write-report* (x prefix . rev-prologue) rest "")))))
	  
			; return the list of all unique "interesting"
			; variables in the expr. Variables that are certain
			; to be bound to procedures are not interesting.
	 (vars-of 
	   (syntax-rules (!)
	     ((_ vars (op . args) (k-head ! . k-args))
	       (id-memv?? op 
		 (quote let let* letrec let*-values lambda cond quasiquote
		   case define do assert)
		 (k-head vars . k-args) ; won't go inside
				; ignore the head of the application
		 (vars-of* vars args (k-head ! . k-args))))
		  ; not an application -- ignore
	     ((_ vars non-app (k-head ! . k-args)) (k-head vars . k-args))
	     ))
	 (vars-of*
	   (syntax-rules (!)
	     ((_ vars () (k-head ! . k-args)) (k-head vars . k-args))
	     ((_ vars (x . rest) k)
	       (symbol?? x
		 (id-memv?? x vars
		   (vars-of* vars rest k)
		   (vars-of* (x . vars) rest k))
		 (vars-of vars x (vars-of* ! rest k))))))

	 (do-assert
	   (syntax-rules (report:)
	     ((_ () expr)			; the most common case
	       (do-assert-c expr))
	     ((_ () expr report: . others) ; another common case
	       (do-assert-c expr others))
	     ((_ () expr . others) (do-assert (expr and) . others))
	     ((_ exprs)
	       (k!reverse () exprs (do-assert-c !)))
	     ((_ exprs report: . others)
	       (k!reverse () exprs (do-assert-c ! others)))
	     ((_ exprs x . others) (do-assert (x . exprs) . others))))

	 (do-assert-c
	   (syntax-rules ()
	     ((_ exprs)
	       (or exprs
		 (begin (vars-of () exprs
			  (write-report ! 
			    ("failed assertion: " 'exprs nl "bindings")))
		   (error "assertion failure"))))
	     ((_ exprs others)
	       (or exprs
		 (begin (write-report others
			  ("failed assertion: " 'exprs))
		   (error "assertion failure"))))))
	 )
       (do-assert () _expr . _others)
       ))))


(define-syntax assure
  (syntax-rules ()
    ((assure exp error-msg) (assert exp report: error-msg))))

(define (identify-error msg args . disposition-msgs)
  (let ((port (console-output-port)))
    (newline port)
    (display "ERROR" port)
    (display msg port)
    (for-each (lambda (msg) (display msg port))
	      (append args disposition-msgs))
    (newline port)))

; (define-syntax assert
;   (syntax-rules ()
;     ((_ expr ...)
;      (or (and expr ...)
;        (begin (error "failed assertion: " '(expr ...)))))))
    

(define chez-error error)
(define error
  (lambda (msg . args)
    (chez-error 'runtime-error "~a~%" (cons msg args))))

; like cout << arguments << args
; where argument can be any Scheme object. If it's a procedure
; (without args) it's executed rather than printed (like newline)

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

;(define cerr cout)
(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x) (x (console-output-port))
		(display x (console-output-port))))
            args))

(define nl (string #\newline))

; Some useful increment/decrement operators

(define-syntax inc!		; Mutable increment
  (syntax-rules ()
    ((inc! x) (set! x (+ 1 x)))))
(define-syntax inc               ; Read-only increment
  (syntax-rules ()
    ((inc x) (+ 1 x))))

(define-syntax dec!		; Mutable decrement
  (syntax-rules ()
    ((dec! x) (set! x (- x 1)))))
(define-syntax dec		; Read-only decrement
  (syntax-rules ()
    ((dec x) (- x 1))))

; Some useful control operators

			; if condition is true, execute stmts in turn
			; and return the result of the last statement
			; otherwise, return unspecified.
			; Native in Petite
; (define-syntax when
;   (syntax-rules ()
;     ((when condition . stmts)
;       (and condition (begin . stmts)))))
  

			; if condition is false execute stmts in turn
			; and return the result of the last statement
			; otherwise, return unspecified.
			; This primitive is often called 'unless'
(define-syntax whennot
  (syntax-rules ()
    ((whennot condition . stmts)
      (or condition (begin . stmts)))))


			; Execute a sequence of forms and return the
			; result of the _first_ one. Like PROG1 in Lisp.
			; Typically used to evaluate one or more forms with
			; side effects and return a value that must be
			; computed before some or all of the side effects happen.
(define-syntax begin0
  (syntax-rules ()
    ((begin0 form form1 ... ) 
      (let ((val form)) form1 ... val))))

			; Prepend an ITEM to a LIST, like a Lisp macro PUSH
			; an ITEM can be an expression, but ls must be a VAR
(define-syntax push!
  (syntax-rules ()
    ((push! item ls)
      (set! ls (cons item ls)))))

			; Is str the empty string?
			; string-null? str -> bool
			; See Olin Shiver's Underground String functions
(define-syntax string-null?
  (syntax-rules ()
    ((string-null? str) (zero? (string-length str)))))


; A rather useful utility from SRFI-1
; cons* elt1 elt2 ... -> object
;    Like LIST, but the last argument provides the tail of the constructed
;    list -- i.e., (cons* a1 a2 ... an) = (cons a1 (cons a2 (cons ... an))).
;
;   (cons* 1 2 3 4) => (1 2 3 . 4)
;   (cons* 1) => 1
(define (cons* first . rest)
  (let recur ((x first) (rest rest))
    (if (pair? rest)
	(cons x (recur (car rest) (cdr rest)))
	x)))

; Support for let*-values form: SRFI-11

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () . bodies) (begin . bodies))
    ((let*-values (((var) initializer) . rest) . bodies)
      (let ((var initializer))		; a single var optimization
	(let*-values rest . bodies)))
    ((let*-values ((vars initializer) . rest) . bodies)
      (call-with-values (lambda () initializer) ; the most generic case
	(lambda vars (let*-values rest . bodies))))))

			; assoc-primitives with a default clause
			; If the search in the assoc list fails, the
			; default action argument is returned. If this
			; default action turns out to be a thunk,
			; the result of its evaluation is returned.
			; If the default action is not given, an error
			; is signaled

(define-syntax assq-def
  (syntax-rules ()
    ((assq-def key alist)
      (or (assq key alist)
	(error "failed to assq key '" key "' in a list " alist)))
    ((assq-def key alist #f)
      (assq key alist))
    ((assq-def key alist default)
      (or (assq key alist) (if (procedure? default) (default) default)))))

(define-syntax assv-def
  (syntax-rules ()
    ((assv-def key alist)
      (or (assv key alist)
	(error "failed to assv key '" key "' in a list " alist)))
    ((assv-def key alist #f)
      (assv key alist))
    ((assv-def key alist default)
      (or (assv key alist) (if (procedure? default) (default) default)))))

(define-syntax assoc-def
  (syntax-rules ()
    ((assoc-def key alist)
      (or (assoc key alist)
	(error "failed to assoc key '" key "' in a list " alist)))
    ((assoc-def key alist #f)
      (assoc key alist))
    ((assoc-def key alist default)
      (or (assoc key alist) (if (procedure? default) (default) default)))))

			; Convenience macros to avoid quoting of symbols
			; being deposited/looked up in the environment
(define-syntax env.find
  (syntax-rules () ((env.find key) (%%env.find 'key))))
(define-syntax env.demand
  (syntax-rules () ((env.demand key) (%%env.demand 'key))))
(define-syntax env.bind
  (syntax-rules () ((env.bind key value) (%%env.bind 'key value))))

			; Implementation of SRFI-0
			; Only feature-identifiers srfi-0, chez, and
			; petite-chez are assumed predefined.
			; See below why this
			; syntax-rule may NOT use an let-syntax.
(define-syntax cond-expand
  (syntax-rules (else chez petite-chez srfi-0 and or not)
    ((cond-expand)
      (error "Unfulfilled cond-expand"))
    ((cond-expand (else . cmd-or-defs*))
      (begin . cmd-or-defs*))
    ((cond-expand "feature-id" chez kt kf) kt)
    ((cond-expand "feature-id" petite-chez kt kf) kt)
    ((cond-expand "feature-id" srfi-0 kt kf) kt)
    ((cond-expand "feature-id" x kt kf) kf)
    ((cond-expand "satisfies?" (and) kt kf) kt)
    ((cond-expand "satisfies?" (and clause) kt kf)
      (cond-expand "satisfies?" clause kt kf))
    ((cond-expand "satisfies?" (and clause . rest) kt kf)
      (cond-expand "satisfies?" clause
	(cond-expand "satisfies?" (and . rest) kt kf) kf))
    ((cond-expand "satisfies?" (or) kt kf) kf)
    ((cond-expand "satisfies?" (or clause) kt kf)
      (cond-expand "satisfies?" clause kt kf))
    ((cond-expand "satisfies?" (or clause . rest) kt kf)
      (cond-expand "satisfies?" clause kt
	(cond-expand "satisfies?" (or . rest) kt kf)))
    ((cond-expand "satisfies?" (not clause) kt kf)
      (cond-expand "satisfies?" clause kf kt))
    ((cond-expand "satisfies?" x kt kf)
      (cond-expand "feature-id" x kt kf))

    ((cond-expand (feature-req . cmd-or-defs*) . rest-clauses)
      (cond-expand "satisfies?" feature-req
	  (begin . cmd-or-defs*)
	  (cond-expand . rest-clauses)))))


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


; Chez does not support DSSSL extended defines and lambdas.
; Caveat: (define-opt name-bindings body) cannot expand into
; (let-syntax ((helper-macro ...)) (helper-macro name-bindings body))
; where helper-macro will generate the valid define.
; The mere appearance of (let-syntax ...) tells the Scheme system
; that whatever define will be generated, it is meant for the _internal_
; context. For example, the following code
;
; (define-syntax tdefine
;   (syntax-rules ()
;     ((tdefine _args . _bodies)
;       (letrec-syntax
; 	((helper
; 	   (syntax-rules ()
; 	     ((helper args bodies) (define args . bodies)))))
; 	(helper _args _bodies)))))
; (tdefine (foo x) (display "OK") (display x) (newline))
; (foo 42)
;
; runs OK on Petite Chez but gives an error "definition in expression context"
; on Scheme48 and SCM (and, consequently, the binding to foo does not occur).


(define-syntax define-opt
  (syntax-rules (optional)
    ((define-opt (name . bindings) . bodies)
      (define-opt "seek-optional" bindings () ((name . bindings) . bodies)))

    ((define-opt "seek-optional" ((optional . _opt-bindings))
       (reqd ...) ((name . _bindings) . _bodies))
      (define (name reqd ... . _rest)
	(letrec-syntax
	  ((handle-opts
	     (syntax-rules ()
	       ((_ rest bodies (var init))
		 (let ((var (if (null? rest) init
			      (if (null? (cdr rest)) (car rest)
				(error "extra rest" rest)))))
		   . bodies))
	       ((_ rest bodies var) (handle-opts rest bodies (var #f)))
	       ((_ rest bodies (var init) . other-vars)
		 (let ((var (if (null? rest) init (car rest)))
		       (new-rest (if (null? rest) '() (cdr rest))))
		   (handle-opts new-rest bodies . other-vars)))
	       ((_ rest bodies var . other-vars)
		 (handle-opts rest bodies (var #f) . other-vars))
	       ((_ rest bodies)		; no optional args, unlikely
		 (let ((_ (or (null? rest) (error "extra rest" rest))))
		   . bodies)))))
	  (handle-opts _rest _bodies . _opt-bindings))))

    ((define-opt "seek-optional" (x . rest) (reqd ...) form)
      (define-opt "seek-optional" rest (reqd ... x) form))

    ((define-opt "seek-optional" not-a-pair reqd form)
      (define . form))			; No optional found, regular define

    ((define-opt name body)		; Just the definition for 'name',
      (define name body))		; for compatibilibility with define
))


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

(define-syntax and-let*
  (syntax-rules ()
    ((_ ()) #t)
    ((_ claws)    ; no body
       ; re-write (and-let* ((claw ... last-claw)) ) into
       ; (and-let* ((claw ...)) body) with 'body' derived from the last-claw
     (and-let* "search-last-claw" () claws))
    ((_ "search-last-claw" first-claws ((exp)))
     (and-let* first-claws exp))	; (and-let* (... (exp)) )
    ((_ "search-last-claw" first-claws ((var exp)))
     (and-let* first-claws exp))	; (and-let* (... (var exp)) )
    ((_ "search-last-claw" first-claws (var))
     (and-let* first-claws var))	; (and-let* (... var) )
    ((_ "search-last-claw" (first-claw ...) (claw . rest))
     (and-let* "search-last-claw" (first-claw ... claw) rest))
    
    ; now 'body' is present
    ((_ () . body) (begin . body))	; (and-let* () form ...)
    ((_ ((exp) . claws) . body)		; (and-let* ( (exp) claw... ) body ...)
     (and exp (and-let* claws . body)))
    ((_ ((var exp) . claws) . body)	; (and-let* ((var exp) claw...)body...)
     (let ((var exp)) (and var (and-let* claws . body))))
    ((_ (var . claws) . body)		; (and-let* ( var claw... ) body ...)
     (and var (and-let* claws . body)))
))

