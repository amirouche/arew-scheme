(import (scheme base))
(import (scheme write))
(import (scheme fixnum))
(import (scheme time))
(import (scheme box))

(define make-box box)

(define box! set-box!)


(define (time)
  (current-jiffy))

(define (print out delta)
  (display (string-append "out: "
                          (number->string out)
                          " nanoseconds: "
                          (number->string (inexact (/ delta (jiffies-per-second)))))))


(include "benchmarks/test.scm")
