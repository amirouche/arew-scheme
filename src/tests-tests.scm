(library (tests-tests)

  (export test-0
          test-1)

  (import (chezscheme)
          (tests))

  (define test-0
    (test #t #t))

  (define test-1
    (test #f #f))

  )
