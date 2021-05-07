;; Copyright 2021 (c) Amirouche (amirouche@hyper.dev)
#!chezscheme
(library (arew cffi)

  (export bytevector-pointer
          foreign-procedure*
          with-lock
          make-double-pointer
          pointer-dereference
          errno
          strerror
          check
          ftype-alloc
          with-foreign-free)
  (import (chezscheme))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((define-syntax-rule (keyword args ...) body)
       (define-syntax keyword
         (syntax-rules ()
           ((keyword args ...) body))))))

  (define-syntax-rule (bytevector-pointer bv)
    (#%$object-address bv (foreign-sizeof 'void*)))

  (define-syntax-rule (foreign-procedure* return ptr args ...)
    (foreign-procedure __collect_safe ptr (args ...) return))

  (define-syntax-rule (with-lock objs body ...)
    (begin
      (for-each lock-object objs)
      (call-with-values (lambda () body ...)
        (lambda args
          (for-each unlock-object objs)
          (apply values args)))))

  (define-syntax-rule (make-double-pointer)
    (bytevector-pointer (make-bytevector 8)))

  (define-syntax-rule (pointer-dereference  pointer)
    (foreign-ref 'void* pointer 0))

  (define errno #%$errno)

  (define stdlib (load-shared-object #f))

  (define strerror
    (let ((func (foreign-procedure "strerror" (int) string)))
      (lambda (code)
        (func code))))

  (define-syntax-rule (check who v)
    (let ((v* v))
      (if (fx=? v* -1)
          (let ((code (errno)))
            (error who (strerror code) code))
          v*)))

  (define-syntax-rule (ftype-alloc ftype)
    (make-ftype-pointer ftype (foreign-alloc (ftype-sizeof ftype))))

  (define-syntax-rule (with-foreign-free pointer proc)
    (let ((pointer* pointer))
      (call-with-values (lambda () (proc pointer*))
        (lambda args
          (foreign-free pointer*)
          (apply values args))))))
