(library (srfi srfi-4-tests)

  (export test-1
          test-2
          test-3
          test-4
          test-5
          test-6
          test-7
          test-8
          test-9
          test-10)

  (import (chezscheme)
          (tests)
          (srfi srfi-4)
          (srfi srfi-2))

  (begin

    ;;
    ;; Adapted from John Cowan contrib in SRFI-4 see:
    ;;
    ;;   https://github.com/scheme-requests-for-implementation/srfi-4/
    ;;
    ;; TODO: maybe improve this tests, they are as of right now
    ;; difficult to debug since they don't compare values using `test`
    ;; macro but inside `and-let*`
    ;;

    (define relerr (expt 2 -24))

    (define (same? result expected)
      ;; super `equal?` that does also approximation between numbers
      (cond
       ((and (number? result) (number? expected) (inexact? result) (inexact? expected))
        (let ((abserr (abs (* expected relerr))))
          (<= (- expected abserr) result (+ expected abserr))))
       ((and (number? result) (number? expected))
        (= result expected))
       ((and (pair? result) (pair? expected))
        (list-same? result expected))
       (else
        (equal? result expected))))

    (define (list-same? result expected)
      (cond
       ((and (null? result) (null? expected))
        #t)
       ((and (pair? result) (pair? expected))
        (and (same? (car result) (car expected)) (list-same? (cdr result) (cdr expected))))
       (else
        #f)))

    (define (create label value)
      value)

    (define (ok? tag
                 cast
                 make-homogeneous-vector
                 homogeneous-vector
                 homogeneous-vector?
                 homogeneous-vector-length
                 homogeneous-vector-ref
                 homogeneous-vector-set!
                 homogeneous-vector->list
                 list->homogeneous-vector
                 low
                 high)
      (let* ((vec0 (create "make" (make-homogeneous-vector 3)))
             (vec1 (create "make-fill" (make-homogeneous-vector 3 (cast 3))))
             (vec2 (create "vec" (homogeneous-vector (cast 1) (cast 2) (cast 3))))
             (vec3 (create "from list" (list->homogeneous-vector (map cast '(3 2 1)))))
             (float (or (eq? tag 'f32) (eq? tag 'f64)))
             (zero (if float 0.0 0)))
        (and-let* ((v1  (same? (homogeneous-vector? vec0) #t))
                   (v2 (same? (homogeneous-vector? vec1) #t))
                   (v3 (same? (homogeneous-vector? vec2) #t))
                   (v4 (same? (homogeneous-vector? vec3) #t))
                   (v5 (same? (homogeneous-vector-length vec0) 3))
                   (v6 (same? (homogeneous-vector-length vec1) 3))
                   (v7 (same? (homogeneous-vector-length vec2) 3))
                   (v8 (same? (homogeneous-vector-length vec3) 3))
                   (v9 (homogeneous-vector-set! vec0 0 low))
                   (v10 (homogeneous-vector-set! vec0 1 zero))
                   (v11 (homogeneous-vector-set! vec0 2 high))
                   (v12 (same? (homogeneous-vector-ref vec0 0) low))
                   (v13 (same? (homogeneous-vector-ref vec0 1) zero))
                   (v14 (same? (homogeneous-vector-ref vec0 2) high))
                   (v15 (same? (homogeneous-vector-ref vec1 0) (cast 3)))
                   (v16 (same? (homogeneous-vector-ref vec1 1) (cast 3)))
                   (v17 (same? (homogeneous-vector-ref vec1 2) (cast 3)))
                   (v18 (same? (homogeneous-vector-ref vec2 0) (cast 1)))
                   (v19 (same? (homogeneous-vector-ref vec2 1) (cast 2)))
                   (v20 (same? (homogeneous-vector-ref vec2 2) (cast 3)))
                   (v21 (same? (homogeneous-vector-ref vec3 0) (cast 3)))
                   (v22 (same? (homogeneous-vector-ref vec3 1) (cast 2)))
                   (v23 (same? (homogeneous-vector-ref vec3 2) (cast 1)))
                   (v24 (same? (homogeneous-vector->list vec0) (list low zero high)))
                   (v25 (same? (homogeneous-vector->list vec1) (map cast '(3 3 3))))
                   (v26 (same? (homogeneous-vector->list vec2) (map cast '(1 2 3))))
                   (v27 (same? (homogeneous-vector->list vec3) (map cast '(3 2 1)))))
          #t)))

        (define test-1
          (test #t
                (ok? 'u8 exact make-u8vector u8vector u8vector? u8vector-length
                     u8vector-ref u8vector-set! u8vector->list list->u8vector
                     0 255)))

        (define test-2
          (test #t
                (ok? 's8 exact make-s8vector s8vector s8vector? s8vector-length
                     s8vector-ref s8vector-set! s8vector->list list->s8vector
                     -128 127)))

        (define test-3
          (test #t
                (ok? 'u16 exact make-u16vector u16vector u16vector? u16vector-length
                     u16vector-ref u16vector-set! u16vector->list list->u16vector
                     0 65535)))

        (define test-4
          (test #t
                (ok? 's16 exact make-s16vector s16vector s16vector? s16vector-length
                     s16vector-ref s16vector-set! s16vector->list list->s16vector
                     -32768 32767)))

        (define test-5
          (test #t
                (ok? 'u32 exact make-u32vector u32vector u32vector? u32vector-length
                     u32vector-ref u32vector-set! u32vector->list list->u32vector
                     0 4294967295)))

        (define test-6
          (test #t
                (ok? 's32 exact make-s32vector s32vector s32vector? s32vector-length
                     s32vector-ref s32vector-set! s32vector->list list->s32vector
                     -2147483648 2147483647)))

        (define test-7
          (test #t
                (ok? 'u64 exact make-u64vector u64vector u64vector? u64vector-length
                     u64vector-ref u64vector-set! u64vector->list list->u64vector
                     0 18446744073709551615)))

        (define test-8
          (test #t
                (ok? 's64 exact make-s64vector s64vector s64vector? s64vector-length
                     s64vector-ref s64vector-set! s64vector->list list->s64vector
                     -9223372036854775808 9223372036854775807)))

        (define test-9
          (test #t
                (ok? 'f32 inexact make-f32vector f32vector f32vector? f32vector-length
                     f32vector-ref f32vector-set! f32vector->list list->f32vector
                     -1e38 1e38)))

        (define test-10
          (test #t (ok? 'f64 inexact make-f64vector f64vector f64vector? f64vector-length
                        f64vector-ref f64vector-set! f64vector->list list->f64vector
                        -1e308 1e308)))))
