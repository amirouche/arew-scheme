(library (srfi srfi-16-tests)

  (export test-1
          test-2)

  (import (except (chezscheme) case-lambda add1)
          (tests)
          (srfi srfi-16))

  (begin

    (define add1
      (case-lambda
        ((a) (add1 a 0))
        ((a b) (+ 1 a b))))

    (define test-1
      (test 2 (add1 1)))

    (define test-2
      (test 4 (add1 1 2)))))
