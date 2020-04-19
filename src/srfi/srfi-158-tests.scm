(library (srfi srfi-158-tests)
  (export
   test-000 test-001 test-002 test-003 test-004 test-005 test-006 test-007 test-008 test-009
   test-010 test-011 test-012 test-013 test-014 test-015 test-016 test-017 test-018 test-019
   test-020 test-021 test-022 test-023 test-024 test-025 test-026 test-027 test-028 test-029
   test-030 test-031 test-032 test-033 test-034 test-035 test-036 test-037 test-038 test-039
   test-040 test-041 test-042 test-043 test-044 test-045 test-046 test-047 test-048 test-049
   test-050 test-051 test-052 test-053 test-054 test-055 test-056 test-057 test-058 test-059
   test-060 test-061 test-062 test-063 test-064 test-065 test-066 test-067 test-068 test-069
   test-070 test-071)

  (import (scheme base)
          (tests)
          (srfi srfi-1)
          (srfi srfi-4)
          (srfi srfi-158))

  ;; generators/constructors

  (define test-000
    (test '() (generator->list (generator))))

  (define test-001
    (test '(1 2 3) (generator->list (generator 1 2 3))))

    ;; TODO: FIXME
    ;; (test-equal '(1 2 3 1 2) (generator->list (circular-generator 1 2 3) 5))

  (define test-002
    (test '(8 9 10) (generator->list (make-iota-generator 3 8))))

  (define test-003
    (test '(8 10 12) (generator->list (make-iota-generator 3 8 2))))

  (define test-004
    (test '(3 4 5 6) (generator->list (make-range-generator 3) 4)))

  (define test-005
    (test '(3 4 5 6 7) (generator->list (make-range-generator 3 8))))

  (define test-006
    (test '(3 5 7) (generator->list (make-range-generator 3 8 2))))

  (define test-007
    (let ()
      (define g
        (make-coroutine-generator
         (lambda (yield) (let loop ((i 0))
                           (when (< i 3) (yield i) (loop (+ i 1)))))))
      (test '(0 1 2) (generator->list g))))

  (define test-008
    (test '(1 2 3 4 5) (generator->list (list->generator '(1 2 3 4 5)))))

  (define test-009
    (test '(1 2 3 4 5) (generator->list (vector->generator '#(1 2 3 4 5)))))

  (define test-010
    (test '#(0 0 1 2 4)
          (let ((v (make-vector 5 0)))
            (generator->vector! v 2 (generator 1 2 4))
            v)))

  (define test-011
    (test '(5 4 3 2 1) (generator->list (reverse-vector->generator '#(1 2 3 4 5)))))

  (define test-012
    (test '(#\a #\b #\c #\d #\e) (generator->list (string->generator "abcde"))))

  (define test-013
    (test '(10 20 30) (generator->list (bytevector->generator (bytevector 10 20 30)))))

  (define test-014
    (let ()
      (define (for-each-digit proc n)
        (when (> n 0)
          (let-values (((div rem) (truncate/ n 10)))
            (proc rem)
            (for-each-digit proc div))))
      (test '(5 4 3 2 1) (generator->list
                                (make-for-each-generator for-each-digit
                                                         12345)))))

  (define test-015
    (test '(0 2 4 6 8 10) (generator->list
                           (make-unfold-generator
                            (lambda (s) (> s 5))
                            (lambda (s) (* s 2))
                            (lambda (s) (+ s 1))
                            0))))

  ;; generators/operators

  (define test-016
    (test '(a b 0 1) (generator->list (gcons* 'a 'b (make-range-generator 0 2)))))

  (define test-017
    (test '(0 1 2 0 1) (generator->list (gappend (make-range-generator 0 3)
                                                 (make-range-generator 0 2)))))

  (define test-018
    (test '() (generator->list (gappend))))

  (define test-019
    (let ()
      (define g1 (generator 1 2 3))
      (define g2 (generator 4 5 6 7))
      (define (proc . args) (values (apply + args) (apply + args)))
      (test '(15 22 31) (generator->list (gcombine proc 10 g1 g2)))))

  (define test-020
    (test '(1 3 5 7 9) (generator->list (gfilter
                                         odd?
                                         (make-range-generator 1 11)))))

  (define test-021
    (test '(2 4 6 8 10) (generator->list (gremove
                                          odd?
                                          (make-range-generator 1 11)))))

  (define g (make-range-generator 1 5))

  (define test-022
    (test '(1 2 3) (generator->list (gtake g 3))))

  (define test-023
    (test '(4) (generator->list g)))

  (define test-024
    (test '(1 2) (generator->list (gtake (make-range-generator 1 3) 3))))

  (define test-025
    (test '(1 2 0) (generator->list (gtake (make-range-generator 1 3) 3 0))))

  (define test-026
    (test '(3 4) (generator->list (gdrop (make-range-generator 1 5) 2))))

  (define g2 (make-range-generator 1 5))

  (define (small? x) (< x 3))

  (define test-027
    (test '(1 2) (generator->list (gtake-while small? g2))))

  (define g3 (make-range-generator 1 5))

  (define test-028
    (test '(3 4) (generator->list (gdrop-while small? g3))))

  (define test-029
    (test '() (generator->list (gdrop-while (lambda args #t) (generator 1 2 3)))))

  (define test-030
    (test '(0.0 1.0 0 2) (generator->list (gdelete 1
                                                   (generator 0.0 1.0 0 1 2)))))

  (define test-031
    (test '(0.0 0 2) (generator->list (gdelete 1
                                               (generator 0.0 1.0 0 1 2)
                                               =))))

  (define test-032
    (test '(a c e) (generator->list (gindex (list->generator '(a b c d e f))
                                            (list->generator '(0 2 4))))))

  (define test-033
    (test '(a d e) (generator->list (gselect (list->generator '(a b c d e f))
                                             (list->generator '(#t #f #f #t #t #f))))))

  (define test-034
    (test '(1 2 3) (generator->list (gdelete-neighbor-dups
                                     (generator 1 1 2 3 3 3)
                                     =))))

  (define test-035
    (test '(1) (generator->list (gdelete-neighbor-dups
                                 (generator 1 2 3)
                                 (lambda args #t)))))

  (define test-036
    (test '(1 2 3 a b c)
          (generator->list
           (gflatten (generator '(1 2 3) '(a b c))))))

  (define test-037
    (test '((1 2 3) (4 5 6) (7 8))
          (generator->list (ggroup (generator 1 2 3 4 5 6 7 8) 3))))

  (define test-038
    (test '((1 2 3) (4 5 6) (7 8 0))
          (generator->list (ggroup (generator 1 2 3 4 5 6 7 8) 3 0))))

  (define test-039
    (test '(1 2 3)
          (generator->list (gmerge < (generator 1 2 3)))))

  (define test-040
    (test '(1 2 3 4 5 6)
          (generator->list (gmerge < (generator 1 2 3) (generator 4 5 6)))))

  (define test-041
    (test '(1 2 3 4 4 5 6)
      (generator->list (gmerge <
                               (generator 1 2 4 6)
                               (generator)
                               (generator 3 4 5)))))

  (define test-042
    (test '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          (generator->list (gmerge <
                                   (generator 1 10 11)
                                   (generator 2 9 12)
                                   (generator 3 8 13)
                                   (generator 4 7 14)
                                   (generator 5 6 15)))))

  (define test-043
    ;; check the tie-break rule
    (test '((1 a) (1 e) (1 b) (1 c) (1 d))
          (generator->list (gmerge (lambda (x y) (< (car x) (car y)))
                                   (generator '(1 a) '(1 e))
                                   (generator '(1 b))
                                   (generator '(1 c) '(1 d))))))

  (define test-044
    (test '(-1 -2 -3 -4 -5)
          (generator->list (gmap - (generator 1 2 3 4 5)))))

  (define test-045
    (test '(7 9 11 13)
          (generator->list (gmap +
                                 (generator 1 2 3 4 5)
                                 (generator 6 7 8 9)))))

  (define test-046
    (test '(54 140 264)
          (generator->list (gmap *
                                 (generator 1 2 3 4 5)
                                 (generator 6 7 8)
                                 (generator 9 10 11 12 13)))))

  (define test-047
    (test '(a c e g i)
          (generator->list
           (gstate-filter
            (lambda (item state) (values (even? state) (+ 1 state)))
            0
            (generator 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))))

  ;; generators/consumers

  (define test-048
    ;; no test-equal for plain generator->list (used throughout)
    (test '(1 2 3) (generator->list (generator 1 2 3 4 5) 3)))

  (define test-049
    (test '(5 4 3 2 1) (generator->reverse-list (generator 1 2 3 4 5))))

  (define test-050
    (test '#(1 2 3 4 5) (generator->vector (generator 1 2 3 4 5))))

  (define test-051
    (test '#(1 2 3) (generator->vector (generator 1 2 3 4 5) 3)))

  (define test-052
    (test "abc" (generator->string (generator #\a #\b #\c))))

  ;; TODO: FIXME
  ;; (test-equal '(e d c b a . z) (with-input-from-string "a b c d e"
  ;;                                (lambda () (generator-fold cons 'z read))))

  (define test-053
    (test 6 (let ()
              (define n 0)
              (generator-for-each (lambda values (set! n (apply + values)))
                                  (generator 1) (generator 2) (generator 3))
              n)))

  (define test-054
    (test '(6 15)
          (generator-map->list (lambda values (apply + values))
                               (generator 1 4) (generator 2 5) (generator 3 6))))

  (define test-055
    (test 3 (generator-find (lambda (x) (> x 2)) (make-range-generator 1 5))))

  (define test-056
    (test 2 (generator-count odd? (make-range-generator 1 5))))

  (define test-057
    (test #t
          (let ()
            (define g (make-range-generator 2 5))
            (and
             (equal? #t (generator-any odd? g))
             (equal? '(4) (generator->list g))))))

  (define test-058
    (test #t
          (let ()
            (define g (make-range-generator 2 5))
            (and
             (equal? #f (generator-every odd? g))
             (equal? '(3 4) (generator->list g))))))

  (define test-059
    (test '(#\a #\b #\c)
          (generator-unfold (make-for-each-generator string-for-each "abc") unfold)))

  ;; accumulators

  (define test-060
    (test -8
          (let ((a (make-accumulator * 1 -)))
            (a 1)
            (a 2)
            (a 4)
            (a (eof-object)))))

  (define test-061
    (test 3
          (let ((a (count-accumulator)))
            (a 1)
            (a 2)
            (a 4)
            (a (eof-object)))))

  (define test-062
    (test '(1 2 4)
          (let ((a (list-accumulator)))
            (a 1)
            (a 2)
            (a 4)
            (a (eof-object)))))

  (define test-063
    (test '(4 2 1)
          (let ((a (reverse-list-accumulator)))
            (a 1)
            (a 2)
            (a 4)
            (a (eof-object)))))

  (define test-064
    (test '#(1 2 4)
          (let ((a (vector-accumulator)))
            (a 1)
            (a 2)
            (a 4)
            (a (eof-object)))))

  (define test-065
    (test '#(0 0 1 2 4)
          (let* ((v (vector 0 0 0 0 0))
                 (a (vector-accumulator! v 2)))
            (a 1)
            (a 2)
            (a 4)
            (a (eof-object)))))

  (define test-066
    (test '#vu8(0 0 1 2 4)
          (let* ((v (bytevector 0 0 0 0 0))
                 (a (bytevector-accumulator! v 2)))
            (a 1)
            (a 2)
            (a 4)
            (a (eof-object)))))

  (define test-067
    (test '#(4 2 1)
          (let ((a (reverse-vector-accumulator)))
            (a 1)
            (a 2)
            (a 4)
            (a (eof-object)))))

  (define test-068
    (test "abc"
          (let ((a (string-accumulator)))
            (a #\a)
            (a #\b)
            (a #\c)
            (a (eof-object)))))

  (define test-069
    (test #vu8(1 2 4)
          (let ((a (bytevector-accumulator)))
            (a 1)
            (a 2)
            (a 4)
            (a (eof-object)))))

  (define test-070
    (test 7
          (let ((a (sum-accumulator)))
            (a 1)
            (a 2)
            (a 4)
            (a (eof-object)))))

  (define test-071
    (test 8
          (let ((a (product-accumulator)))
            (a 1)
            (a 2)
            (a 4)
            (a (eof-object))))))
