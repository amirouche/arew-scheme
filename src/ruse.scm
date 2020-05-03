(import (chezscheme)
        (nanopass))

(define unique-var
  (let ((count 0))
    (lambda (name)
      (let ([count* count])
        (set! count (+ count 1))
        (string->symbol
         (string-append (symbol->string name) "." (number->string count*)))))))

(define make-tmp (lambda () (unique-var 't)))



(define-language Lsrc
  (terminals
   (symbol (x))
   (primitive (pr))
   (constant (c))
   (datum (d)))
  (Expr (e body)
        pr
        x
        c
        (quote d)
        (if e0 e1)
        (if e0 e1 e2)
        (or e* ...)
        (and e* ...)
        (not e)
        (begin e* ... e)
        (lambda (x* ...) body* ... body)
        (let ([x* e*] ...) body* ... body)
        (letrec ([x* e*] ...) body* ... body)
        (set! x e)
        (e e* ...)))

(define primitives
  '((- . 2)
    (+ . 2)
    (= . 2)
    (* . 2)))

(define constant? number?)

(define primitive? (lambda (name) (assq name primitives)))

(define-pass parse-and-rename : * (e) -> Lsrc ()
  ;; Helper functions for this pass.
  (definitions

    ;; process-body - used to process the body of begin, let, letrec, and
    ;; lambda expressions.  since all four of these have the same pattern in
    ;; the body.
    (define process-body
      (lambda (who env body* f)
        (when (null? body*) (error who "invalid empty body"))
        (let loop ([body (car body*)] [body* (cdr body*)] [rbody* '()])
          (if (null? body*)
              (f (reverse rbody*) (Expr body env))
              (loop (car body*) (cdr body*)
                    (cons (Expr body env) rbody*))))))

    ;; vars-unique? - processes the list of bindings to make sure all of the
    ;; variable names are different (i.e. we don't want to allow
    ;; (lambda (x x) x), since we would not know which x is which).
    (define vars-unique?
      (lambda (fmls)
        (let loop ([fmls fmls])
          (or (null? fmls)
              (and (not (memq (car fmls) (cdr fmls)))
                   (loop (cdr fmls)))))))
    ;; unique-vars - builds a list of unique variables based on a set of
    ;; formals and extends the environment.  it takes a function as an
    ;; argument (effectively a continuation), and passes it the updated
    ;; environment and a list of unique variables.
    (define unique-vars
      (lambda (env fmls f)
        (unless (vars-unique? fmls)
          (error 'unique-vars "invalid formals" fmls))
        (let loop ([fmls fmls] [env env] [rufmls '()])
          (if (null? fmls)
              (f env (reverse rufmls))
              (let* ([fml (car fmls)] [ufml (unique-var fml)])
                (loop (cdr fmls) (cons (cons fml ufml) env)
                      (cons ufml rufmls)))))))

    ;; process-bindings - processes the bindings of a let or letrec and
    ;; produces bindings for unique variables for each of the original
    ;; variables.  it also processes the right-hand sides of the variable
    ;; bindings and selects either the original environment (for let) or the
    ;; updated environment (for letrec).
    (define process-bindings
      (lambda (rec? env bindings f)
        (let loop ([bindings bindings] [rfml* '()] [re* '()])
          (if (null? bindings)
              (unique-vars env rfml*
                           (lambda (new-env rufml*)
                             (let ([env (if rec? new-env env)])
                               (let loop ([rufml* rufml*]
                                          [re* re*]
                                          [ufml* '()]
                                          [e* '()])
                                 (if (null? rufml*)
                                     (f new-env ufml* e*)
                                     (loop (cdr rufml*) (cdr re*)
                                           (cons (car rufml*) ufml*)
                                           (cons (Expr (car re*) env) e*)))))))
              (let ([binding (car bindings)])
                (loop (cdr bindings) (cons (car binding) rfml*)
                      (cons (cadr binding) re*)))))))

    ;; Expr* - helper to process a list of expressions.
    (define Expr*
      (lambda (e* env)
        (map (lambda (e) (Expr e env)) e*)))
    ;; with-output-language rebinds quasiquote so that it will build
    ;; language records.

    (with-output-language (Lsrc Expr)
      ;; build-primitive - this is a helper function to build entries in the
      ;; initial environment for our user primitives.  the initial
      ;; enviornment contains a mapping of keywords and primitives to
      ;; processing functions that check their arity (in the case of
      ;; primitives) or their forms (in the case of keywords).  These are
      ;; put into an environment, because keywords and primitives can be
      ;; rebound.  (i.e. (lambda (lambda) (lambda lambda)) is a perfectly
      ;; valid function in Scheme that takes a function as an argument and
      ;; applies the argument to itself).
      (define build-primitive
        (lambda (as)
          (let ([name (car as)] [argc (cdr as)])
            (cons name
                  (if (< argc 0)
                      (error who
                             "primitives with arbitrary counts are not currently supported"
                             name)
                      ;; we'd love to support arbitrary argument lists, but we'd
                      ;; need to either:
                      ;;   1. get rid of raw primitives, or
                      ;;   2. add function versions of our raw primitives with
                      ;;      arbitrary arguments, or (possibly and)
                      ;;   3. add general handling for functions with arbitrary
                      ;;      arguments. (i.e. support for (lambda args <body>)
                      ;;      or (lambda (x y . args) <body>), which we don't
                      ;;      currently support.
                      #;(let ([argc (bitwise-not argc)])
                      (lambda (env . e*)
                      (if (>= (length e*) argc)
                      `(,name ,(Expr* e* env) ...)
                      (error name "invalid argument count"
                      (cons name e*)))))
                      (lambda (env . e*)
                        (if (= (length e*) argc)
                            `(,name ,(Expr* e* env) ...)
                            (error name "invalid argument count"
                                   (cons name e*)))))))))
      ;; initial-env - this is our initial environment, expressed as an
      ;; association list of keywords and primitives (represented as
      ;; symbols) to procedure handlers (represented as procedures).  As the
      ;; program is processed through this pass, it will be extended with
      ;; local bidings from variables (represented as symbols) to unique
      ;; variables (represented as symbols with a format of symbol.number).
      (define initial-env
        (cons*
         (cons 'quote (lambda (env d)
                        (unless (datum? d)
                          (error 'quote "invalid datum" d))
                        `(quote ,d)))
         (cons 'if (case-lambda
                    [(env e0 e1) `(if ,(Expr e0 env) ,(Expr e1 env))]
                    [(env e0 e1 e2)
                     `(if ,(Expr e0 env) ,(Expr e1 env) ,(Expr e2 env))]
                    [x (error 'if (if (< (length x) 3)
                                      "too few arguments"
                                      "too many arguments")
                              x)]))
         (cons 'or (lambda (env . e*) `(or ,(Expr* e* env) ...)))
         (cons 'and (lambda (env . e*) `(and ,(Expr* e* env) ...)))
         (cons 'not (lambda (env e) `(not ,(Expr e env))))
         (cons 'begin (lambda (env . e*)
                        (process-body 'begin env e*
                                      (lambda (e* e)
                                        `(begin ,e* ... ,e)))))
         (cons 'lambda (lambda (env fmls . body*)
                         (unique-vars env fmls
                                      (lambda (env fmls)
                                        (process-body 'lambda env body*
                                                      (lambda (body* body)
                                                        `(lambda (,fmls ...)
                                                           ,body* ... ,body)))))))
         (cons 'let (lambda (env bindings . body*)
                      (process-bindings #f env bindings
                                        (lambda (env x* e*)
                                          (process-body 'let env body*
                                                        (lambda (body* body)
                                                          `(let ([,x* ,e*] ...) ,body* ... ,body)))))))
         (cons 'letrec (lambda (env bindings . body*)
                         (process-bindings #t env bindings
                                           (lambda (env x* e*)
                                             (process-body 'letrec env body*
                                                           (lambda (body* body)
                                                             `(letrec ([,x* ,e*] ...)
                                                                ,body* ... ,body)))))))
         (cons 'set! (lambda (env x e)
                       (cond
                        [(assq x env) =>
                         (lambda (as)
                           (let ([v (cdr as)])
                             (if (symbol? v)
                                 `(set! ,v ,(Expr e env))
                                 (error 'set! "invalid syntax"
                                        (list 'set! x e)))))]
                        [else (error 'set! "set to unbound variable"
                                     (list 'set! x e))])))
         (map build-primitive primitives)))
      ;; App - helper for handling applications.
      (define App
        (lambda (e env)
          (let ([e (car e)] [e* (cdr e)])
            `(,(Expr e env) ,(Expr* e* env) ...))))))
  ;; transformer: Expr: S-expression -> LSrc:Expr (or error)
  ;;
  ;; parses an S-expression, looking for a pair (which indicates, a
  ;; keyword use, a primitive call, or a normal function call), a symbol
  ;; (which indicates a variable reference or a primitive reference), or one of
  ;; our constants (which indicates a raw constant).
  (Expr : * (e env) -> Expr ()
        (cond
         [(pair? e)
          (cond
           [(assq (car e) env) =>
            (lambda (as)
              (let ([v (cdr as)])
                (if (procedure? v)
                    (apply v env (cdr e))
                    (App e env))))]
           [else (App e env)])]
         [(symbol? e)
          (cond
           [(assq e env) =>
            (lambda (as)
              (let ([v (cdr as)])
                (cond
                 [(symbol? v) v]
                 [(primitive? e) e]
                 [else (error who "invalid syntax" e)])))]
           [else (error who "unbound variable" e)])]
         [(constant? e) e]
         [else (error who "invalid expression" e)]))
  ;; kick off processing the S-expression by handing Expr our initial
  ;; S-expression and the initial environment.
  (Expr e initial-env))

(define (pk . args)
  (for-each pretty-print args) (newline)
  (car (reverse args)))

(define-pass lift-expression : Lsrc (e) -> Lsrc ()
  (Expr : Expr (e) -> Expr ()

        (definitions

          (define (build-let t0 e* e)
            (with-output-language (Lsrc Expr)
              (let f ((e* e*)
                      (v* '()))
                (if (null? e*)
                    (nanopass-case (Lsrc Expr) e
                      (,c (let ((v* (reverse (cons e v*)))
                                (out (unique-var 'out)))
                            `(let ((,out (,t0 ,v* ...))
                                   ,out))))
                      (,x (let ((v* (reverse (cons e v*)))
                                (out (unique-var 'out)))
                            `(let ((,out (,t0 ,v* ...)))
                               ,out)))
                      (else
                       (let ((t (make-tmp))
                             (out (unique-var 'out)))
                         `(let ((,t ,e))
                            (let ((,out (,t0 ,(reverse v*) ... ,t)))
                              ,out)))))
                    (let ((head (car e*)))
                      (nanopass-case (Lsrc Expr) head
                        (,c (f (cdr e*) (cons head v*)))
                        (,x (f (cdr e*) (cons head v*)))
                        (else (let ((tmp (make-tmp)))
                                `(let ((,tmp ,(car e*)))
                                   ,(f (cdr e*) (cons tmp v*))))))))))))

        ((if ,[e0] ,[e1] ,[e2])
         `(if ,e0 ,e1 ,e2))

        ((,pr ,[e*] ... ,[e])
         (build-let pr e* e))))

(define-pass flatten-let : Lsrc (e) -> Lsrc ()

  (Expr : Expr (e) -> Expr ()
        (definitions

          (define (flatten e)
            (nanopass-case (Lsrc Expr) e
              ((let ((,x0* ,e0*) ...) ,e1)
               (let loop ((x0* x0*)
                          (e0* e0*)
                          (x** '())
                          (e** '()))
                 (if (null? x0*)
                     (values x** e** e1)
                     (let ((head (car e0*)))
                       (nanopass-case (Lsrc Expr) head
                         ((let ((,x1* ,e1*) ...) ,e2 ...)
                          (call-with-values (lambda () (flatten head))
                            (lambda (x2 e2 e3)
                              (loop (cdr x0*)
                                    (cdr e0*)
                                    (append x** x2 (list (car x0*)))
                                    (append e** e2 (list e3))))))
                         (else (loop (cdr x0*)
                                     (cdr e0*)
                                     (append x** (list (car x0*)))
                                     (append e** (list (car e0*)))))))))))))

        ((lambda (,x* ...) ,e)
         (call-with-values (lambda () (flatten e))
           (lambda (x0* e0* out)
             `(lambda (,x* ...) (letrec ((,x0* ,e0*) ...) ,out)))))))

(define program '(lambda (n)
                   (if n
                       1
                       (* (+ n 1) 2))))

(define program '(lambda (n) (* (+ n 1) 2)))

(define program '(lambda (n) (let ((a (let ((b n)) b))) a)))

;; (define program '(letrec ((fact (lambda (n) (if (= n 0) 1 (* (fact (- n 1)) n)))))
;;                    (fact 5)))

(pretty-print (unparse-Lsrc (flatten-let (values (parse-and-rename program)))))

(lambda (n.0)
  (+ 42 1)
  (+ n.0 1))
