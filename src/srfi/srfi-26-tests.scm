(library (srfi srfi-26-tests)

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
          test-26)

  (import (chezscheme)
          (tests)
          (srfi srfi-26))


  ;; CONFIDENCE TEST FOR IMPLEMENTATION OF SRFI-26
  ;; =============================================
  ;;
  ;; Sebastian.Egner@philips.com, 3-Jun-2002.
  ;;
  ;; This file checks a few assertions about the implementation.
  ;; If you run it and no error message is issued, the implementation
  ;; is correct on the cases that have been tested.
  ;;
  ;; compliance:
  ;;   Scheme R5RS with
  ;;     SRFI-23: error
  ;;
  ;; loading this file into Scheme 48 0.57 after 'cut.scm' has been loaded:
  ;;   ,open srfi-23
  ;;   ,load check.scm

  ;; (check expr)
  ;;    evals expr and issues an error if it is not #t.

  (begin

    (define env (environment '(chezscheme) '(srfi srfi-26)))

    ;; cut

    (define test-01
      (test (eval '(equal? ((cut list)) '()) env) #t))

    (define test-02
      (test (eval '(equal? ((cut list <...>)) '()) env) #t))

    (define test-03
      (test (eval '(equal? ((cut list 1)) '(1)) env) #t))

    (define test-04
      (test (eval '(equal? ((cut list <>) 1) '(1)) env) #t))

    (define test-05
      (test (eval '(equal? ((cut list <...>) 1) '(1)) env) #t))

    (define test-06
      (test (eval '(equal? ((cut list 1 2)) '(1 2)) env) #t))

    (define test-07
      (test (eval '(equal? ((cut list 1 <>) 2) '(1 2)) env) #t))

    (define test-08
      (test (eval '(equal? ((cut list 1 <...>) 2) '(1 2)) env) #t))

    (define test-09
      (test (eval '(equal? ((cut list 1 <...>) 2 3 4) '(1 2 3 4)) env) #t))

    (define test-10
      (test (eval '(equal? ((cut list 1 <> 3 <>) 2 4) '(1 2 3 4)) env) #t))

    (define test-11
      (test (eval '(equal? ((cut list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6)) env) #t))

    (define test-12
      (test (eval '(equal? (let* ((x 'wrong) (y (cut list x))) (set! x 'ok) (y)) '(ok)) env)
            #t))

    (define test-13
      (test (eval '(equal?
                    (let ((a 0))
                      (map (cut + (begin (set! a (+ a 1)) a) <>)
                           '(1 2))
                      a)
                    2)
                  env)
            #t))

    ;; cute

    (define test-14
      (test (eval '(equal? ((cute list)) '()) env) #t))

    (define test-15
      (test (eval '(equal? ((cute list <...>)) '()) env) #t))

    (define test-16
      (test (eval '(equal? ((cute list 1)) '(1)) env) #t))

    (define test-17
      (test (eval '(equal? ((cute list <>) 1) '(1)) env) #t))

    (define test-18
      (test (eval '(equal? ((cute list <...>) 1) '(1)) env) #t))

    (define test-19
      (test (eval '(equal? ((cute list 1 2)) '(1 2)) env) #t))

    (define test-20
      (test (eval '(equal? ((cute list 1 <>) 2) '(1 2)) env) #t))

    (define test-21
      (test (eval '(equal? ((cute list 1 <...>) 2) '(1 2)) env) #t))

    (define test-22
      (test (eval '(equal? ((cute list 1 <...>) 2 3 4) '(1 2 3 4)) env) #t))

    (define test-23
      (test (eval '(equal? ((cute list 1 <> 3 <>) 2 4) '(1 2 3 4)) env) #t))

    (define test-24
      (test (eval '(equal? ((cute list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6)) env) #t))

    (define test-25
      (test (eval '(equal? (let* ((x 'ok) (y (cute list x))) (set! x 'wrong) (y)) '(ok))
                  env)
            #t))

    (define test-26
      (test (eval '(equal?
                    (let ((a 0))
                      (map (cute + (begin (set! a (+ a 1)) a) <>)
                           '(1 2))
                      a)
                    1)
                  env)
            #t))

    ))
