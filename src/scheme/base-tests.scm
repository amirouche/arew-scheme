(library (scheme base-tests)

  (export test-0001
          test-0002
          test-0003
          test-0004
          test-0005
          test-0006
          test-0007
          test-0008
          test-0009
          test-0010
          test-0011
          test-0012
          test-0013
          test-0014
          test-0015
          test-0016
          test-0017
          test-0018
          test-0019
          test-0020
          test-0021
          test-0022
          test-0023
          test-0024
          test-0025
          test-0026
          test-0027
          test-0028
          test-0029
          test-0030
          test-0031
          test-0032
          test-0033
          test-0034
          test-0035
          test-0036
          test-0037
          test-0038
          test-0039
          test-0040
          test-0041
          test-0042
          test-0043
          test-0044
          test-0045
          test-0046
          test-0047
          test-0048
          test-0049
          test-0050)

  (import (tests)
          (scheme base))

  (define test-0001
    (test (+ 1 2 3 4) 10))

  (define test-0002
    (test (+ 1) 1))

  (define test-0003
    (test (* 1 2 3 4) 24))

  (define test-0004
    (test (* 1) 1))

  (define test-0005
    (test (- 1) -1))

  (define test-0006
    (test (- 1 1) 0))

  (define test-0007
    (test (/ 1 2) 1/2))

  ;; propertly guard the exception 'numerical-overflow
  (define test-0008
    (test (guard (ex (else #t)) (/ 1 0)) #t))

  (define test-0009
    (test (/ 1 2 3 4 5) 1/120))

  (define test-0010
    (test (< 1 2 3 4 5) #t))

  (define test-0011
    (test (< 1 2 3 4 5 5) #f))

  (define test-0012
    (test (< 10 5) #f))

  (define test-0013
    (test (<= 1 2 3 4 5 5) #t))

  (define test-0014
    (test (<= 1 2 3 4 5) #t))

  (define test-0015
    (test (<= 10 5) #f))

  (define test-0016
    (test (= 10 5) #f))

  (define test-0017
    (test (= 10 10 10 10) #t))

  (define test-0018
    (test (> 10111 1011 11 1) #t))

  (define test-0019
    (test (> 10 100) #f))

  (define test-0020
    (test (>= 10111 10111 1011 11 1) #t))

  (define test-0021
    (test (>= 10 100) #f))

  (define test-0022
    (test (abs -10) 10))

  (define test-0023
    (test (abs 10) 10))

  (define test-0024
    (test (and 1 2 3) 3))

  (define test-0025
    (test (and 1 #f 3) #f))

  (define test-0026
    (test (append '(1 2 3)) '(1 2 3)))

  (define test-0027
    (test (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6)))

  (define test-0028
    (test (apply + '(1 2 3)) 6))

  (define test-0029
    (test (apply + 1 2 3 '(4 5 6)) 21))

  (define test-0030
    (test (assoc 1 '((1 . 2) (3 . 4))) '(1 . 2)))

  (define test-0031
    (test (assoc 42 '((1 . 2) (3 . 4))) #f))

  (define test-0032
    (test (assq 1 '((1 . 2) (3 . 4))) '(1 . 2)))

  (define test-0033
    (test (assq 42 '((1 . 2) (3 . 4))) #f))

  (define test-0034
    (test (assv 1 '((1 . 2) (3 . 4))) '(1 . 2)))

  (define test-0035
    (test (assv 42 '((1 . 2) (3 . 4))) #f))

  (define test-0036
    (test (boolean=? #t #f) #f))

  (define test-0037 (test (boolean=? #t #t) #t))

  (define test-0038
    (test (boolean? #t) #t))

  (define test-0039
    (test (boolean? 23) #f))

  (define test-0040
    (test (bytevector 0 1 2 3 4 5) #vu8(0 1 2 3 4 5)))

  (define test-0041
    (test (bytevector-append #vu8(0 1 2) #vu8(3 4 5)) #vu8(0 1 2 3 4 5)))

  (define test-0042
    (test (bytevector-copy #vu8(0 1 2)) #vu8(0 1 2)))

  (define test-0043
    (test (bytevector-copy #vu8(0 1 2) 1) #vu8(1 2)))

  (define test-0044
    (test (bytevector-copy #vu8(0 1 2) 1 2) #vu8(1)))

  (define test-0045
    (test
     (let ((bv (make-bytevector 4 0)))
       (bytevector-copy! bv 1 #vu8(0 1 2 3 4) 1 3)
       bv)
     #vu8(0 1 2 0)))

  (define test-0046
    (test (bytevector-length #vu8(0 1 2)) 3))

  (define test-0047
    (test (bytevector-u8-ref #vu8(0 1 2) 2) 2))

  (define test-0048
    (test
     (let ((bv (make-bytevector 4 0)))
       (bytevector-u8-set! bv 1 42)
       bv)
     #vu8(0 42 0 0)))

  (define test-0049
    (test (bytevector? #vu8(0 1 2)) #t))

  (define test-0050 (test (bytevector? 123456) #f)))
