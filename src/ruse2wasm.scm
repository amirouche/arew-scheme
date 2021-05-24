(import (only (chezscheme) import pretty-print))

(import (scheme base))
(import (scheme list))
(import (scheme file))
(import (scheme read))
(import (scheme eval))
(import (scheme write))
(import (scheme process-context))

(import (arew matchable))


;; helper

(define unique-var
  (let ()
    (define count 0)
    (lambda (name)
      (let ((c count))
        (set! count (+ count 1))
        (string->symbol
         (string-append (symbol->string name) "." (number->string c)))))))

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
    (pretty-print exp)
    (pretty-print (step-name (car steps)))
    (if (null? (cdr steps))
        ((step-evaler (car steps)) ((step-compiler (car steps)) exp))
        (let ((exp ((step-compiler (car steps)) exp)))
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
       (define (box-set! b a) (vector-set! b 0 a))
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
        ,@(map (lambda (a v) `(primcall box-set! ,a ,(convert-reference-to-unbox a* v))) a* v*)
        ,(convert-reference-to-unbox a* e)))
    ((e* ...) (map convert-letrec*-naive e*))
    (e e)))

(define convert-letrec*-naive-step (make-step! "convert-letrec*-naive"
                                               make-letrec*-explicit-step
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
                  (define (box-set! b a) (vector-set! b 0 a))
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


(nanosteps "make-letrec*-explicit" "cps" (cadr (command-line)))
;;(nanosteps "make-letrec*-explicit" "convert-let-as-lambda" (caddr (command-line)))
