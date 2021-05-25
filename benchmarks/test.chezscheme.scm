(import (only (chezscheme) import current-time time-second time-nanosecond format load set-box! unbox box))
(import (scheme base))
(import (scheme fixnum))
(import (scheme time))

(define make-box box)

(define box! set-box!)


(define (time)
  (current-jiffy))

(define (print out delta)
  (format #t "out: ~a nanoseconds: ~a\n" out (inexact (/ delta (jiffies-per-second)))))


(include "benchmarks/test.scm")
