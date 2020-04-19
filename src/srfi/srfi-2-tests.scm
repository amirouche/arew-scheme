(library (srfi srfi-2-tests)

  (export test-01
          test-02
          test-03
          test-04
          test-05
          test-06
          test-07
          test-08
          test-09
          test-10
          test-11
          test-12
          test-13
          test-14
          test-15
          test-16
          test-17
          test-18
          test-19
          test-20
          test-21
          test-22
          test-23
          test-24
          test-25
          test-26
          test-27
          test-28)

  (import (chezscheme)
          (tests)
          (srfi srfi-2))

  (begin

    (define test-01
      (test (and-let* () 1) 1))

    (define test-02
      (test (and-let* () 1 2) 2))

    (define test-03
      (test (and-let* () ) #t))

    (define test-04
      (test (let ((x #f)) (and-let* (x))) #f))

    (define test-05
      (test (let ((x 1)) (and-let* (x))) 1))

    (define test-06
      (test (and-let* ((x #f)) ) #f))

    (define test-07
      (test (and-let* ((x 1)) ) 1))

    ;; TODO:
    ;; (must-be-a-syntax-error (and-let* ( #f (x 1))) )

    (define test-08
      (test (and-let* ( (#f) (x 1)) ) #f))

    ;; TODO
    ;; (must-be-a-syntax-error (and-let* (2 (x 1))) )

    (define test-09
      (test (and-let* ( (2) (x 1)) ) 1))

    (define test-10
      (test (and-let* ( (x 1) (2)) ) 2))

    (define test-11
      (test (let ((x #f)) (and-let* (x) x)) #f))

    (define test-12
      (test (let ((x "")) (and-let* (x) x)) ""))

    (define test-13
      (test (let ((x "")) (and-let* (x)  )) ""))

    (define test-14
      (test (let ((x 1)) (and-let* (x) (+ x 1))) 2))

    (define test-15
      (test (let ((x #f)) (and-let* (x) (+ x 1))) #f))

    (define test-16
      (test (let ((x 1)) (and-let* (((positive? x))) (+ x 1))) 2))

    (define test-17
      (test (let ((x 1)) (and-let* (((positive? x))) )) #t))

    (define test-18
      (test (let ((x 0)) (and-let* (((positive? x))) (+ x 1))) #f))

    (define test-19
      (test (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1))) (+ x 1)))  3))

    ;; TODO
    ;; (must-be-a-syntax-error
    ;;  (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1)))
    ;;  )

    (define test-20
      (test (let ((x 1)) (and-let* (x ((positive? x))) (+ x 1))) 2))

    (define test-21
      (test (let ((x 1)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))) 2))

    (define test-22
      (test (let ((x 0)) (and-let* (x ((positive? x))) (+ x 1))) #f))

    (define test-23
      (test (let ((x #f)) (and-let* (x ((positive? x))) (+ x 1))) #f))

    (define test-24
      (test (let ((x #f)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))) #f))

    (define test-25
      (test (let ((x 1)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f))

    (define test-26
      (test (let ((x 0)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f))

    (define test-27
      (test (let ((x #f)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) #f))

    (define test-28
      (test (let ((x 3)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))) 3/2))))
