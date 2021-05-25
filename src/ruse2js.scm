(import (only (chezscheme) import pretty-print open-process-ports current-transcoder))

(import (scheme base))
(import (scheme list))
(import (scheme file))
(import (scheme read))
(import (scheme eval))
(import (scheme write))
(import (scheme process-context))

(import (arew matchable))


;; helper

(define (make-unique-var)
  (let ()
    (define count 0)
    (lambda (name)
      (let ((c count))
        (set! count (+ count 1))
        (string->symbol
         (string-append (symbol->string name) "." (number->string c)))))))

(define unique-var (make-unique-var))

;; nanosteps

(define-record-type <step>
  (make-step name parent reader compiler evaler)
  step?
  (name step-name)
  (parent step-parent)
  (reader step-reader)
  (compiler step-compiler)
  (evaler step-evaler))

(define STEPS '())

(define (make-step! name parent reader compiler evaler)
  (define step (make-step name parent reader compiler evaler))
  (set! STEPS (cons step STEPS))
  step)

(define (nanosteps source target filename)

  (define (->steps source target)
    (let loop ((steps STEPS))
      (if (null? steps)
          (error 'ruse2wasm "no step found" target)
          (if (string=? (step-name (car steps)) target)
              (let loop ((step (car steps))
                         (out '()))
                (unless step
                  (error 'ruse2wasm "no step found" source))
                (if (string=? (step-name step) source)
                    (cons step out)
                    (loop (step-parent step) (cons step out))))
              (loop (cdr steps))))))

  (define steps (->steps source target))
  (define exp ((step-reader (car steps)) filename))

  (let loop ((steps steps)
             (exp exp))
    (newline)
    (pk "exec step: ")
    (pk (step-name (car steps)))
    (if (null? (cdr steps))
        (let ((evaler (step-evaler (car steps)))
              (exp ((step-compiler (car steps)) exp)))
          (if evaler (evaler exp) (pretty-print exp)))
        (let ((exp ((step-compiler (car steps)) exp)))
          ;; (pretty-print exp)
          (loop (cdr steps) exp)))))

(define (reader filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((out '()))
        (let ((exp (read port)))
          (if (eof-object? exp)
              `(lambda () ,@(reverse out))
              (loop (cons exp out))))))))

(define (evaler exp)
  (define exp*
    `(begin
       (define (pk . args) (display ";;; ") (write args) (newline) (car (reverse args)))
       (define void (lambda () (when #f #f)))
       (define (make-box v) (vector v))
       (define (unbox b) (vector-ref b 0))
       (define (box! b a) (vector-set! b 0 a))
       (define time current-jiffy)
       (define (print . args)
         (write args)(newline))
       (define (primcall proc . args) (apply proc args))
       (define (zero? x)
         (= x 0))
       (define (add a b)
         (+ a b))
       (define (sub a b)
         (- a b))

       ,exp))

  (pretty-print exp*)
  (pk ((eval exp* (environment '(scheme base) '(scheme write) '(scheme time))))))

;; set as lists helpers

(define (set-cdr items tail)
  (let f ((items items))
    (if (null? items)
        tail
        (cons (car items) (f (cdr items))))))

(define set-cons
  (lambda (x set)
    (if (memq x set)
        set
        (cons x set))))

;; construct the intersection of 0 to n sets
(define intersect
  (lambda set*
    (if (null? set*)
        '()
        (fold (lambda (seta setb)
                     (let loop ((seta seta) (fset '()))
                       (if (null? seta)
                           fset
                           (let ((a (car seta)))
                             (if (memq a setb)
                                 (loop (cdr seta) (cons a fset))
                                 (loop (cdr seta) fset))))))
                   (car set*) (cdr set*)))))

;; construct the difference of 0 to n sets
(define difference
  (lambda set*
    (if (null? set*)
        '()
        (fold-right (lambda (setb seta)
                      (let loop ((seta seta) (final '()))
                        (if (null? seta)
                            final
                            (let ((a (car seta)))
                              (if (memq a setb)
                                  (loop (cdr seta) final)
                                  (loop (cdr seta) (cons a final)))))))
                    (car set*) (cdr set*)))))

(define union
  (lambda set*
    (if (null? set*)
        '()
        (fold (lambda (seta setb)
                     (let loop ((setb setb) (seta seta))
                       (if (null? setb)
                           seta
                           (loop (cdr setb) (set-cons (car setb) seta)))))
                   (car set*) (cdr set*)))))

;; make-letrec*-explicit

(define (make-letrec*-explicit exp)
  (match exp
    (('lambda () e* ...)
     (let loop ((e* e*)
                (bindings '()))
       (if (null? e*)
           (error 'ruse2wasm "invalid program")
           (match (car e*)
             (('define n e0)
              (loop (cdr e*) (cons (list n e0) bindings)))
             (_ `(lambda () (letrec* ,(reverse bindings) ,@e*)))))))))

(define make-letrec*-explicit-step (make-step! "make-letrec*-explicit"
                                               #f
                                               reader
                                               make-letrec*-explicit
                                               evaler))


;; identify

(define primitives
  '((zero? . 1)
    (- . 2)
    (+ . 2)
    (* . 2)
    (make-box . 1)
    (unbox . 1)
    (box! . 2)
    (void . 0)
    (time . 0)
    (print . 2)
    (make-closure . 2)
    (closure-ref . 2)))

(define (primitive? s)
  (memq s (map car primitives)))

(define (parse-and-rename exp)
  ;; XXX: primtives are not first-class
  (define (do exp env)

    (match exp
      (('lambda (n* ...) e* ...)
       (let* ((rn* (map unique-var n*))
              (env* (append (map cons n* rn*) env)))
         `(lambda ,rn* ,@(map (lambda (e) (do e env*)) e*))))

      (('letrec* ((a* b*) ...) e* ...)
       (let* ((ra* (map unique-var a*))
              (env* (append (map cons a* ra*) env)))
         `(letrec* ,(map (lambda (ra b) (list ra (do b env*))) ra* b*)
            ,@(map (lambda (e) (do e env*)) e*))))

      (('let* ((a* b*) ...) e* ...)
       (let loop ((a* a*)
                  (b* b*)
                  (bindings '())
                  (env* env))
         (if (null? a*)
             `(let* ,(reverse bindings) ,@(map (lambda (e) (do e env*)) e*))
             (let* ((a (car a*))
                    (ra (unique-var a))
                    (rb (do (car b*) env*))
                    (env* (cons (cons ra a) env*)))
               (loop (cdr a*)
                     (cdr b*)
                     (cons (list ra rb) bindings)
                     (cons (cons a ra) env*))))))

      (('let ((a* b*) ...) e* ...)
       (let* ((ra* (map unique-var a*))
              (env* (append (map cons a* ra*) env)))
         `(let ,(map (lambda (ra b) (list ra (do b env))) ra* b*)
            ,@(map (lambda (e) (do e env*)) e*))))

      (('if e0 e1 e2) `(if ,(do e0 env) ,(do e1 env) ,(do e2 env)))

      (((? primitive? pr) e* ...) `(primcall ,pr ,@(map (lambda (e) (do e env)) e*)))
      ((e* ...) (map (lambda (e) (do e env)) e*))
      ((? symbol? s) (cdr (assoc s env)))
      (e e)))

  (do exp '()))


(define parse-and-rename-step (make-step! "parse-and-rename"
                                          make-letrec*-explicit-step
                                          reader
                                          parse-and-rename
                                          evaler))

;; convert-letrec*-naive

(define (convert-letrec*-naive exp)

  (define (convert-reference-to-unbox v* exp)
    (if (memq exp v*)
        `(primcall unbox ,exp)
        (match exp
          ((e* ...) (map (lambda (e) (convert-reference-to-unbox v* e)) e*))
          (e e))))

  (match exp
    (('letrec* ((a* v*) ...) e)
     `(let* ,(map (lambda (a) (list a '(primcall make-box (primcall void)))) a*)
        ,@(map (lambda (a v) `(primcall box! ,a ,(convert-reference-to-unbox a* v))) a* v*)
        ,(convert-reference-to-unbox a* e)))
    ((e* ...) (map convert-letrec*-naive e*))
    (e e)))

(define convert-letrec*-naive-step (make-step! "convert-letrec*-naive"
                                               parse-and-rename-step
                                               reader
                                               convert-letrec*-naive
                                               evaler))

;; convert-exprs-as-let*

(define (convert-exprs-as-let* exp)
  (match exp
    (('let* bindings e* ... en)
     `(let* ,bindings
        (let* ,(map (lambda (e) (list (unique-var 'i) (convert-exprs-as-let* e))) e*)
          ,(convert-exprs-as-let* en))))
    (('let bindings e* ... en)
     `(let ,bindings
        (let* ,(map (lambda (e) (list (unique-var 'i) (convert-exprs-as-let* e))) e*)
          ,(convert-exprs-as-let* en))))
    ((e* ...) (map convert-exprs-as-let* e*))
    (e e)))

(define convert-exprs-as-let*-step (make-step! "convert-exprs-as-let*"
                                               convert-letrec*-naive-step
                                               reader
                                               convert-exprs-as-let*
                                               evaler))

;; convert-let*-as-let

(define (convert-let*-as-let exp)
  (match exp
    (('let* ((a* v*) ...) e)
     (let f ((a* a*)
             (v* v*))
       (if (null? a*)
           (convert-let*-as-let e)
           `(let ((,(car a*) ,(convert-let*-as-let (car v*))))
              ,(f (cdr a*) (cdr v*))))))
    ((e* ...) (map convert-let*-as-let e*))
    (e e)))

(define convert-let*-as-let-step (make-step! "convert-let*-as-let"
                                               convert-exprs-as-let*-step
                                               reader
                                               convert-let*-as-let
                                               evaler))
;; convert-let-as-lambda

(define (convert-let-as-lambda exp)
  (match exp
    (('let ((a v)) e)
     `((lambda (,a) ,(convert-let-as-lambda e)) ,(convert-let-as-lambda v)))
    ((e* ...) (map convert-let-as-lambda e*))
    (e e)))

(define convert-let-as-lambda-step (make-step! "convert-let-as-lambda"
                                          convert-let*-as-let-step
                                          reader
                                          convert-let-as-lambda
                                          evaler))
;; cps

(define (cps exp)
  (match exp
    ((? integer? i)
     (let ((r (unique-var 'r)))
       `(lambda (,r) (,r ,i))))

    ((? string? i)
     (let ((r (unique-var 'r)))
       `(lambda (,r) (,r ,i))))

    ((? boolean? b)
     (let ((r (unique-var 'r)))
       `(lambda (,r) (,r ,b))))

    (('if e0 e1 e2)
     (let ((k (unique-var 'k))
           (v (unique-var 'v)))
       `(lambda (,k)
          (,(cps e0)
           (lambda (,v)
             (if ,v
                 (,(cps e1) ,k)
                 (,(cps e2) ,k)))))))

    (('primcall pr)
     `(lambda (r) (r (primcall ,pr))))

    (('primcall pr e* ... en)
     (let ((r0 (unique-var 'r)))
       `(lambda (,r0)
          ,(let f ((e* e*)
                   (v* '()))
             (if (null? e*)
                 (let ((v (unique-var 'v)))
                   `(,(cps en)
                     (lambda (,v)
                       (,r0 (primcall ,pr ,@(reverse v*) ,v)))))
                 (let ((v (unique-var 'v)))
                   `(,(cps (car e*))
                     (lambda (,v)
                       ,(f (cdr e*) (cons v v*))))))))))

    (('lambda args body)
     (let ((k (unique-var 'k))
           (r (unique-var 'r)))
       `(lambda (,r)
          (,r (lambda ,(cons k args) (,(cps body) ,k))))))
    ((e)
     (let ((p (unique-var 'p))
           (k (unique-var 'k)))
       `(lambda (,k)
          (,(cps e)
           (lambda (,p)
             (,p ,k))))))
    ((e e* ... en)
     (let ((p (unique-var 'p))
           (k (unique-var 'k)))
       `(lambda (,k)
          (,(cps e)
           (lambda (,p)
             ,(let f ((e* e*)
                      (v* '()))
                (if (null? e*)
                    (let ((v (unique-var 'v))
                          (r (unique-var 'r)))
                      `(,(cps en)
                        (lambda (,v)
                          (,p ,k
                              ,@(map (lambda (v) `(lambda (r) (r ,v))) (reverse v*))
                              (lambda (r) (r ,v))))))
                    (let ((v (unique-var 'v)))
                      `(,(cps (car e*))
                        (lambda (,v)
                          ,(f (cdr e*) (cons v v*))))))))))))

    (x x)))

(define (cps-evaler exp)
  (define exp* `(begin
                  (define (pk . args) (write ";;; ") (write args) (newline) (car (reverse args)))
                  (define void (lambda () (when #f #f)))
                  (define (make-box v) (vector v))
                  (define (unbox b) (vector-ref b 0))
                  (define (box! b a) (vector-set! b 0 a))
                  (define time current-jiffy)
                  (define (print . args)
                    (write args)(newline))
                  (define (primcall proc . args) (apply proc args))
                  (define (make-closure s a) (vector s a))
                  (define (closure-ref cl i) (vector-ref (vector-ref cl 1) i))
                  (define (zero? x)
                    (= x 0))
                  (define (add a b)
                    (+ a b))
                  (define (sub a b)
                    (- a b))

                  (,exp (lambda (r) (r (lambda args (apply pk 'out args)))))))

  (define env (environment '(scheme base) '(scheme write) '(scheme time)))
  (pretty-print exp*)

  (let loop ((exp* (cdr exp*)))
    (unless (null? exp*)
      (eval (car exp*) env)
      (loop (cdr exp*)))))

(define cps-step (make-step! "cps"
                             convert-let-as-lambda-step
                             reader
                             cps
                             cps-evaler))

;; uncover free-variables

(define (uncover-free-variables exp)

  (define (do exp)
    (match exp
      (('primcall pr e* ...)
       (let loop ((e* e*) (o* '()) (free* '()))
         (if (null? e*)
             (values `(primcall ,pr ,@(reverse o*)) free*)
             (call-with-values (lambda () (do (car e*)))
               (lambda (o free)
                 (loop (cdr e*) (cons o o*) (union free free*)))))))

      (('lambda args e* ...)
       (let loop ((e* e*)
                  (out* '())
                  (free* '()))
         (if (null? e*)
           (let ((free** (difference free* args)))
             (values `(lambda ,args (values ,@(map (lambda (f) `',f) free**)) ,@(reverse out*))
                     free**))
           (call-with-values (lambda () (do (car e*)))
             (lambda (out free)
               (loop (cdr e*)
                     (cons out out*)
                     (append free free*)))))))

      (('if e0 e1 e2)
       (call-with-values (lambda () (do e0))
         (lambda (e0 free0)
           (call-with-values (lambda () (do e1))
             (lambda (e1 free1)
               (call-with-values (lambda () (do e2))
                 (lambda (e2 free2)
                   (values `(if ,e0 ,e1 ,e2) (union free0 free1 free2)))))))))

      ((e* ...)
       (let loop ((e* e*) (o* '()) (free* '()))
         (if (null? e*)
             (values (reverse o*) free*)
             (call-with-values (lambda () (do (car e*)))
               (lambda (o free)
                 (loop (cdr e*) (cons o o*) (union free free*)))))))
      ((? integer? c) (values c '()))
      (e (values e (list e)))))

  (call-with-values (lambda () (do exp))
    (lambda (exp free)
      (unless (null? free)
        (error 'ruse2wasm "unbound variables" free))
      exp)))

(define uncover-free-variables-step
  (make-step! "uncover-free-variables"
              cps-step
              reader
              uncover-free-variables
              cps-evaler))

(define (make-closures exp)
  (match exp
    (('lambda args ('values f* ...) e* ...)
     (let ((cl (unique-var 'cl)))
       `(primcall make-closure (list ,@f*)
                      (lambda ,(cons cl args)
                        ,@(map (lambda (f i) `(define ,(cadr f) (primcall closure-ref ,cl ,i)))
                               f*
                               (iota (length f*)))
                        ,@(map make-closures e*)))))
    ((e* ...) (map make-closures e*))
    (e e)))


(define make-closures-step
  (make-step! "make-closures"
              uncover-free-variables-step
              reader
              make-closures
              #f))

;; closure finish

(define (closure-finish exp)
  (match exp
    (('define name value)
     `(define ,name ,value))
    (('primcall 'make-closure free ('lambda args body ...))
     `(primcall make-closure ,free (lambda ,args ,@(map closure-finish body))))
    (('primcall pr e* ...)
     `(primcall ,pr ,@e*))
    (('if e0 e1 e2) `(if ,(closure-finish e0) ,(closure-finish e1) ,(closure-finish e2)))
    (((? symbol? s) e* ...)
     `(,s ,s ,@(map closure-finish e*)))
    ((e e* ...)
     (let ((cl (unique-var 'cl)))
       `(let ()
          (define ,cl ,(closure-finish e))
          (,cl ,cl ,@(map closure-finish e*)))))
    (e e)))

(define closure-finish-step
  (make-step! "closure-finish"
              make-closures-step
              reader
              closure-finish
              #f))

(define (lift-lambda exp)

  (define (named-lambda exp)
    (match exp
      (('lambda args e* ...)
       `(lambda ,(unique-var 'a) ,args ,@(map named-lambda e*)))
      ((e* ...) (map named-lambda e*))
      (e e)))

  (define lambdas '())

  (define (do exp)
    (match exp
      (('lambda name args e* ...)
       (set! lambdas (cons (cons name `(lambda ,args ,@(map do e*))) lambdas))
       name)
      ((e* ...) (map do e*))
      (e e)))

  (define program (do (named-lambda exp)))

  `(begin
     ,@(map (lambda (name+lambda) `(define ,(car name+lambda) ,(cdr name+lambda)))
            lambdas)

     (define main ,program)))


(define lift-lambda-step (make-step! "lift-lambda"
                                     closure-finish-step
                                     reader
                                     lift-lambda
                                     #f))

(define (trampolinize exp)
  (define (dodo exp)
    (match exp
      (('if e0 e1 e2) `(if ,e0 ,(dodo e1) ,(dodo e2)))
      (('let '() define (e* ...))
       `(let () ,define (values ,@e*)))
      ((e* ...) `(values ,@e*))
      (e e)))

  (define (do exp)
    (match exp
      (('define name ('lambda vars e* ... en))
       `(define ,name (lambda ,vars ,@e* ,(dodo en))))

      (('define name value)
       `(define ,name ,value))))

  `(begin ,@(map do (cdr exp))))

(define trampolinize-step (make-step! "trampolinize"
                                     lift-lambda-step
                                     reader
                                     trampolinize
                                     #f))

(define (mess exp)

  (define (map-lambdas proc exp)
    (let loop ((exp (cdr exp))
               (out '()))
      (if (null? exp)
          `(begin ,@(reverse out))
          (match (car exp)
            (('define name ('lambda args e* ...))
             (loop (cdr exp)
                   (cons `(define ,name (lambda ,args ,@(proc e*)))
                         out)))
            (('define 'main value)
             (loop (cdr exp)
                   (cons `(define main ,value) out)))))))


  (define (hoist-simple-let exp)
    (define (do exp)
      (match exp
        (('let () e* ...) e*)
        (('if e0 e1 e2) (list `(if ,e0 (begin ,@(do e1)) (begin ,@(do e2)))))
        (e (list e))))

    (map-lambdas (lambda (e) (apply append (map do e))) exp))

  (define (simplify-make-closure exp)

    (define (do e)
      (match e
        (('primcall 'make-closure ('list f* ...) a)
         (let ((cl (unique-var 'cl)))
           `(let ()
              (define ,cl (primcall make-closure ,a))
              ,@(map (lambda (f) `(primcall closure-push ,cl ,(cadr f))) f*)
              ,cl)))
        (('define name v)
         `(define ,name ,(do v)))
        ((e* ...)
         (map do e*))
        (e e)))


    (map-lambdas (lambda (e*) (map do e*)) exp))

  (define (simple-values exp)

    (define (dodo e)
      (match e
        ((? symbol? s) s)
        ((? integer? c) c)
        (e (let ((t (unique-var 't)))
             `(let ()
                (define ,t ,e)
                ,t)))))

    (define (do e)
      (match e
        (('values e* ...)
         `(values ,@(map dodo e*)))
        (e e)))

    (map-lambdas (lambda (e*) (map do e*)) exp))

  (define (hoist-take-n+1 exp)

    (define (dovalues e*)
      (let loop ((e* e*)
                 (v* '())
                 (out '()))
        (if (null? e*)
            (append (reverse out) (list `(values ,@(reverse v*))))
            (match (car e*)
              ((? symbol? s) (loop (cdr e*) (cons s v*) out))
              ((? integer? s) (loop (cdr e*) (cons s v*) out))
              (('let () x* ... xn)
               (loop (cdr e*) (cons xn v*) (append x* out)))))))


    (define (do exp)
      (match exp
        (('values e* ...) (dovalues e*))
        (('define x v) (list `(define ,x ,v)))
        (e (list e))))

    (map-lambdas (lambda (e) (apply append (map do e))) exp))

  (define (hoist-take-n+2 exp)

    (define (do exp)
      (match exp
        (('define x ('let () e* ... en)) (append e* (list `(define ,x ,en))))
        (('if e0 e1 e2) (list `(if ,e0
                                   (begin ,@(apply append (map do (cdr e1))))
                                   (begin ,@(apply append (map do (cdr e2)))))))

        (e (list e))))

    (map-lambdas (lambda (e) (apply append (map do e))) exp))

  (define (hoist-take-n+3 exp)

    (define (do exp)
      (match exp
        (('define x ('primcall pr x* ...))
         (let ((out (map (lambda (x) (cons (unique-var 't) x)) x*)))
           (append (map (lambda (x) `(define ,(car x) ,(cdr x))) out)
                   (list `(define ,x (primcall ,pr ,@(map car out)))))))
        (e (list e))))

    (map-lambdas (lambda (e) (apply append (map do e))) exp))

  (define (define+set! exp)

    (define (do exp)
      (define vars '())

      (define (dodo e)
        (match e
          (('define x v)
           (set! vars (cons x vars))
           `(set! ,x ,v))
          (('if e0 e1 e2)
           `(if ,e0
                (begin ,@(map dodo (cdr e1)))
                (begin ,@(map dodo (cdr e2)))))
          (('let () e* ...) (map dodo e*)
           `(let () ,@e*))
          (e e)))

      (define e* (map dodo exp))
      (define v* (map (lambda (v) `(define ,v)) vars))
      (cons `(begin ,@v*) e*))

    (map-lambdas do exp))

  (define (deduplicate-closure-continuation exp)

    (define (do exp)
      (match exp
        (('values cl cl e* ...)
         `(values ,cl ,@e*))
        (e e)))

    (map-lambdas (lambda (e*) (map do e*)) exp))

  (define steps (list hoist-simple-let
                      simplify-make-closure
                      simple-values
                      hoist-take-n+1
                      hoist-take-n+2
                      hoist-take-n+3
                      define+set!
                      deduplicate-closure-continuation))

  (let loop ((exp exp)
             (steps steps))
    (if (null? steps)
        exp
        (loop ((car steps) exp) (cdr steps)))))


(define mess-step (make-step! "mess"
                              trampolinize-step
                              reader
                              mess
                              #f))



(define step-javascripter (make-step! "javascripter" mess-step #f javascripter display))

(nanosteps "make-letrec*-explicit" "mess" (cadr (command-line)))
