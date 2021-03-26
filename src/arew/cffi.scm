;; Copyright 2021 (c) Amirouche (amirouche@hyper.dev)
#!chezscheme
(library (arew cffi)

  (export bytevector-pointer
          foreign-procedure*
          with-lock
          make-double-pointer
          pointer-dereference
          errno
          strerror)
  (import (chezscheme))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((define-syntax-rule (keyword args ...) body)
       (define-syntax keyword
         (syntax-rules ()
           ((keyword args ...) body))))))

  (define-syntax-rule (bytevector-pointer bv)
    (#%$object-address bv (+ (foreign-sizeof 'void*) 1)))

  (define-syntax-rule (foreign-procedure* return ptr args ...)
    (foreign-procedure __collect_safe ptr (args ...) return))

  (define-syntax-rule (with-lock obj body ...)
    (begin
      (lock-object obj)
      (call-with-values (lambda () body ...)
        (lambda out
          (unlock-object obj)
          (apply values out)))))

  (define-syntax-rule (make-double-pointer)
    (bytevector-pointer (make-bytevector 8)))

  (define-syntax-rule (pointer-dereference  pointer)
    (foreign-ref 'void* pointer 0))

  (define errno #%$errno)

  (define stdlib (load-shared-object #f))

  (define strerror
    (let ((func (foreign-procedure "strerror" (int) string)))
      (lambda (code)
        (func code)))))
