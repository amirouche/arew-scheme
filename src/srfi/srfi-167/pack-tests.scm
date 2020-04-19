(library (srfi srfi-167 pack-tests)

  (export test-0
          )

  (import (scheme base)
          (tests)
          (srfi srfi-167 pack))

  (define expected
    (list *null*
          #t
          #f
          0
          #vu8(42 101 255)
          "hello world"
          'symbol
          42
          (expt 2 64)
          -42
          (- (expt 2 64))))

  (define test-0
    (test expected (unpack (apply pack expected)))))
