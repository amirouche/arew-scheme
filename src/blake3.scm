#!chezscheme
(library (blake3)
  (export blake3 make-blake3 blake3-update! blake3-finalize!)
  (import (chezscheme))

  (define libblake3 (load-shared-object "local/lib/libblake3.so"))


  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((define-syntax-rule (keyword args ...) body)
       (define-syntax keyword
         (syntax-rules ()
           ((keyword args ...) body))))))

  (define (bytevector->pointer bv)
    (#%$object-address bv (+ (foreign-sizeof 'void*) 1)))

  (define-syntax-rule (foreign-procedure* return ptr args ...)
    (foreign-procedure __collect_safe ptr (args ...) return))


  (define-syntax-rule (with-lock obj body ...)
    (begin (lock-object obj)
           (call-with-values (lambda () body ...)
             (lambda out
               (unlock-object obj)
               (apply values out)))))

    (define-ftype %keyvalue
      (packed (struct
               (key void*)
               (key-length int)
               (value void*)
               (value-length int))))

    (define blake3-hasher-init
      (let ((func (foreign-procedure* void "blake3_hasher_init" void*)))
        (lambda (blake3)
          (func blake3))))

    (define (make-blake3)
      (define bv (make-bytevector 1912)) ;; sizeof blake3_hasher
      (with-lock bv
                 (blake3-hasher-init (bytevector->pointer bv)))
      (bytevector->pointer bv))

    (define blake3-update!
      (let ((func (foreign-procedure* void "blake3_hasher_update" void* void* size_t)))
        (lambda (blake3 bytevector)
          (with-lock bytevector
                     (func blake3
                           (bytevector->pointer bytevector)
                           (bytevector-length bytevector))))))

    (define blake3-finalize!
      (let ((func (foreign-procedure* void "blake3_hasher_finalize" void* void* size_t)))
        (lambda (blake3)
          (define bytevector (make-bytevector 32))
          (with-lock bytevector
                     (func blake3 (bytevector->pointer bytevector) 32))
          bytevector)))

    (define (blake3 bytevector)
      (define hasher (make-blake3))
      (blake3-update! hasher bytevector)
      (blake3-finalize! hasher)))
