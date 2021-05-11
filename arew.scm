(import (chezscheme))
(import (only (rnrs) (rename define-record-type define-record-type*)))


;; srfi-9 define-record-type

(define-syntax define-record-type
  (lambda (stx)
    (syntax-case stx ()
      ((_ type (constructor constructor-tag ...)
          predicate
          (field-tag accessor setter ...) ...)
       (and (for-all identifier?
                     #'(type constructor constructor-tag ... predicate
                             field-tag ... accessor ... setter ... ...))
            (for-all (lambda (s) (<= 0 (length s) 1))
                     #'((setter ...) ...))
            (for-all (lambda (ct)
                       (memp (lambda (ft) (bound-identifier=? ct ft))
                             #'(field-tag ...)))
                     #'(constructor-tag ...)))
       (with-syntax (((field-clause ...)
                      (map (lambda (clause)
                             (if (= 2 (length clause))
                                 #`(immutable . #,clause)
                                 #`(mutable . #,clause)))
                           #'((field-tag accessor setter ...) ...)))
                     ((unspec-tag ...)
                      (remp (lambda (ft)
                              (memp (lambda (ct) (bound-identifier=? ft ct))
                                    #'(constructor-tag ...)))
                            #'(field-tag ...))))
                    #'(define-record-type* (type constructor predicate)
                        (protocol (lambda (ctor)
                                    (lambda (constructor-tag ...)
                                      (define unspec-tag) ...
                                      (ctor field-tag ...))))
                        (fields field-clause ...)))))))

;; hints, https://github.com/pre-srfi/hints

(define any-of
  (lambda predicates
    (lambda (obj)
      (let loop ((predicates predicates))
        (if (null? predicates)
            #f
            (if ((car predicates) obj)
                #t
                (loop (cdr predicates))))))))

(define every-of
  (lambda predicates
    (lambda (obj)
      (let loop ((predicates predicates))
        (if (null? predicates)
            #t
            (if ((car predicates) obj)
                (loop (cdr predicates))
                #f))))))

(define each-of
  (lambda predicates
    (lambda objs
      (let loop ((predicates predicates)
                 (objs objs))

        (cond
         ;; 1) if there is no more predicates and objs is empty return #t
         ((and (null? predicates) (null? objs)) #t)
         ;; 2) if it is the last predicate, pass what remains of OBJS
         ((and (pair? predicates) (null? (cdr predicates)))
          ((car predicates) objs))
         ;; 3) if there is no more objects and there is still at least
         ;; two predicates because of 2), return #f
         ((and (null? objs) (not (null? predicates))) #f)
         ;; 4) Should be handled by 2)
         ((and (not (null? objs)) (null? predicates)) (error 'hint "Oops"))
         ;; 5) the first obj, is valid according to the first
         ;; predicate, continue
         (((car predicates) (car objs)) (loop (cdr predicates)
                                              (cdr objs)))
         ;; 6) Otherwise, it is invalid.
         (else #f))))))

(define anything?
  (lambda args
    #t))

(define false? not)

(define-record-type <hint>
  (make-hint% documentation arguments values raise)
  hint?
  (documentation hint-documentation)
  (arguments hint-arguments)
  (values hint-values)
  (raise hint-raise))

(define hint-error
  (lambda args
    (error 'hint "Oops again!")))

(define make-hint
  (case-lambda
   ((documentation) (make-hint% documentation
                                hint-error
                                hint-error
                                hint-error))
   ((documentation arguments)
    (make-hint% documentation
                arguments
                hint-error
                hint-error))
   ((documentation arguments values)
    (make-hint% documentation
                arguments
                values
                hint-error))
   ((documentation arguments values raise)
    (make-hint% documentation
                arguments
                values
                raise))))

(define hint-arguments?
  (lambda (hint objs)
    (apply (hint-arguments hint) objs)))

(define hint-values?
  (lambda (hint objs)
    (apply (hint-values hint) objs)))

(define hint-raise?
  (lambda (hint obj)
    ((hint-raise hint) obj)))

(define generic-method-tag '(generic-method-tag))

(define make-generic-method-procedure
  (lambda (hint proc)
    ;; TODO: when in debug, wrap and check return values and raise +
    ;; warning.
    proc))

(define (generic-method-call name methods args)
  (let loop ((methods methods))
    (if (null? methods)
        (error 'generic "No matching generic method..." name)
        (let ((hint (caar methods)))
          (if (hint-arguments? hint args)
              (apply (cdar methods) args)
              (loop (cdr methods)))))))

(define make-generic
  (lambda (name)
    (let ((name name)
          (methods '()))
      (lambda args
        (if (and (not (null? args)) (eq? (car args) generic-method-tag))
            (let* ((hint (cadr args))
                   (proc (caddr args))
                   (proc* (make-generic-method-procedure hint proc)))
              (set! methods (cons (cons hint proc*) methods))
              proc*)
            (generic-method-call name methods args))))))

(define (make-generic-method generic hint proc)
  (generic generic-method-tag hint proc))

(define-syntax define*
  (syntax-rules ()
    ;; create a generic
    ((define* identifier)
     (define identifier (make-generic 'identifier)))
    ;; create a generic method
    ((define* identifier generic hint (lambda args body ...))
     (define identifier (make-generic-method generic hint (lambda args body ...))))))
