#!r6rs
;; Test suite for SRFI-1
;; 2003-12-29 / lth
;;
;; $Id: srfi-1-test.sps 5842 2008-12-11 23:04:51Z will $
;;
;; Note: In Larceny, we require that the procedures designated as
;; "linear update" variants in the spec (eg append!) side-effect their
;; arguments, and there are tests here that check that side-effecting
;; occurs.
;;
;; For linear update we only require that the cells of the result are
;; taken from the cells of the input.  We could be stricter and require
;; that the cells of the results are the cells of the input with only
;; the CDR changed, ie, values are never moved from one cell to another.
;;
;; Comments:
;;
;; - Test cases are ordered as in the spec.  R5RS procedures are left
;;   out.
;;
;; - Adapted to arew scheme tests
;;

(library (srfi srfi-1-tests)

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
          test-0050
          test-0051
          test-0052
          test-0053
          test-0054
          test-0055
          test-0056
          test-0057
          test-0058
          test-0059
          test-0060
          test-0061
          test-0062
          test-0063
          test-0064
          test-0065
          test-0066
          test-0067
          test-0068
          test-0069
          test-0070
          test-0071
          test-0072
          test-0073
          test-0074
          test-0075
          test-0076
          test-0077
          test-0078
          test-0079
          test-0080
          test-0081
          test-0082
          test-0083
          test-0084
          test-0085
          test-0086
          test-0087
          test-0088
          test-0089
          test-0090
          test-0091
          test-0092
          test-0093
          test-0094
          test-0095
          test-0096
          test-0097
          test-0098
          test-0099
          test-0100
          test-0101
          test-0102
          test-0103
          test-0104
          test-0105
          test-0106
          test-0107
          test-0108
          test-0109
          test-0110
          test-0111
          test-0112
          test-0113
          test-0114
          test-0115
          test-0116
          test-0117
          test-0118
          test-0119
          test-0120
          test-0121
          test-0122
          test-0123
          test-0124
          test-0125
          test-0126
          test-0127
          test-0128
          test-0129
          test-0130
          test-0131
          test-0132
          test-0133
          test-0134
          test-0135
          test-0136
          test-0137
          test-0138
          test-0139
          test-0140
          test-0141
          test-0142
          test-0143
          test-0144
          test-0145
          test-0146
          test-0147
          test-0148
          test-0149
          test-0150
          test-0151
          test-0152
          test-0153
          test-0154
          test-0155
          test-0156
          test-0157
          test-0158
          test-0159
          test-0160
          test-0161
          test-0162
          test-0163
          test-0164
          test-0165
          test-0166
          test-0167
          test-0168
          test-0169
          test-0170
          test-0171)

  (import (only (chezscheme)
                if define begin quote lambda error
                call-with-current-continuation
                not eq? eqv? list? let let* or and equal?
                = let-values quasiquote even? < max + > * -
                zero? set! number? symbol? integer? remainder
                unquote)
          (tests)
          (srfi srfi-1))

  (begin

    (define test-0001
      (test '(2 . 1) (xcons 1 2)))

    (define test-0002
      (test 1 (cons* 1)))

    (define test-0003
      (test '(1 2 3 4 . 5) (cons* 1 2 3 4 5)))

    (define test-0004
      (test '(#t #t #t #t #t) (make-list 5 #t)))

    (define test-0005
      (test '() (make-list 0 #f)))

    (define test-0006
      (test 3 (length (make-list 3))))

    (define test-0007
      (test '(0 1 2 3 4) (list-tabulate 5 (lambda (x) x))))

    (define test-0008
      (test '() (list-tabulate 0 (lambda (x) (error 'srfi-1-tests "tests")))))

    (define test-0009
      (test #t
            (call-with-current-continuation
             (lambda (abort)
               (let* ((c  (list 1 2 3 4 5))
	              (cp (list-copy c)))
	         (or (equal? c cp)
	             (abort #f))
	         (let loop ((c c) (cp cp))
	           (if (not (null? c))
	               (begin
		         (or (not (eq? c cp))
		             (abort #f))
		         (loop (cdr c) (cdr cp)))))
	         #t)))))

    (define test-0010
      (test '(1 2 3 . 4) (list-copy '(1 2 3 . 4))))

    (define test-0011
      (test #t (not (list? (circular-list 1 2 3)))))

    (define test-0012
      (test #t
            (let* ((a (list 'a))
	           (b (list 'b))
	           (c (list 'c))
	           (x (circular-list a b c)))
              (and (eq? a (car x))
	           (eq? b (cadr x))
	           (eq? c (caddr x))
	           (eq? a (cadddr x))))))

    (define test-0013
      (test '() (iota 0)))

    (define test-0014
      (test '(2 5 8 11 14) (iota 5 2 3)))

    (define test-0015
      (test '(2 3 4 5 6) (iota 5 2)))

    (define test-0016
      (test #t (proper-list? '(1 2 3 4 5))))

    (define test-0017
      (test #t (proper-list? '())))

    (define test-0018
      (test #f (proper-list? '(1 2 . 3))))

    (define test-0019
      (test #f (proper-list? (circular-list 1 2 3))))

    (define test-0020
      (test #f (circular-list? '(1 2 3 4 5))))

    (define test-0021
      (test #f (circular-list? '())))

    (define test-0022
      (test #f (circular-list? '(1 2 . 3))))

    (define test-0023
      (test #t (circular-list? (circular-list 1 2 3))))

    (define test-0024
      (test #f (dotted-list? '(1 2 3))))

    (define test-0025
      (test #f (dotted-list? '())))

    (define test-0026
      (test #t (dotted-list? '(1 2 . 3))))

    (define test-0027
      (test #f (dotted-list? (circular-list 1 2 3))))

    (define test-0028
      (test #t (null-list? '())))

    (define test-0029
      (test #f (null-list? '(1 2))))

    (define test-0030
      (test #f (null-list? (circular-list 1 2))))

    (define test-0031
      (test #t (not-pair? 1)))

    (define test-0032
      (test #f (not-pair? (cons 1 2))))

    (define test-0033
      (test #t (list= = '(1 2 3) '(1 2 3) '(1 2 3))))

    (define test-0034
      (test #f (list= = '(1 2 3) '(1 2 3) '(4 5 6))))

    (define test-0035
      ;; Checks that l0 is not being used when testing l2, cf spec
      (test #t (list= (lambda (a b) (not (eq? a b))) '(#f #f #f) '(#t #t #t) '(#f #f #f))))

    (define test-0036
      (test 1 (first '(1 2 3 4 5 6 7 8 9 10))))

    (define test-0037
      (test 2 (second '(1 2 3 4 5 6 7 8 9 10))))

    (define test-0038
      (test 3 (third '(1 2 3 4 5 6 7 8 9 10))))

    (define test-0039
      (test 4 (fourth '(1 2 3 4 5 6 7 8 9 10))))

    (define test-0040
      (test 5 (fifth '(1 2 3 4 5 6 7 8 9 10))))

    (define test-0041
      (test 6 (sixth '(1 2 3 4 5 6 7 8 9 10))))

    (define test-0042
      (test 7 (seventh '(1 2 3 4 5 6 7 8 9 10))))

    (define test-0043
      (test 8 (eighth '(1 2 3 4 5 6 7 8 9 10))))

    (define test-0044
      (test 9 (ninth '(1 2 3 4 5 6 7 8 9 10))))

    (define test-0045
      (test 10 (tenth '(1 2 3 4 5 6 7 8 9 10))))

    (define test-0046
      (test #t (let-values (((a b) (car+cdr (cons 1 2))))
                 (and (= a 1) (= b 2)))))

    (define test-0047
      (test '(1 2 3) (take '(1 2 3 4 5 6) 3)))

    (define test-0048
      (test '(1) (take '(1) 1)))

    (define test-0049
      (let ((x (list 1 2 3 4 5 6)))
        (test (cdddr x) (drop x 3))))

    (define test-0050
      (let ((x (list 1 2 3)))
        (test #t (eq? x (drop x 0)))))

    (define test-0051
      (test '(4 5 6) (take-right '(1 2 3 4 5 6) 3)))

    (define test-0052
      (test '() (take-right '(1 2 3 4 5 6) 0)))

    (define test-0053
      (test '(2 3 . 4) (take-right '(1 2 3 . 4) 2)))

    (define test-0054
      (test 4 (take-right '(1 2 3 . 4) 0)))

    (define test-0055
      (test '(1 2 3) (drop-right '(1 2 3 4 5 6) 3)))

    (define test-0056
      (test '(1 2 3) (drop-right '(1 2 3) 0)))

    (define test-0057
      (test '(1 2 3) (drop-right '(1 2 3 . 4) 0)))

    (define test-0058
      (test #t (let ((x (list 1 2 3 4 5 6)))
                 (let ((y (take! x 3)))
	           (and (eq? x y)
	                (eq? (cdr x) (cdr y))
	                (eq? (cddr x) (cddr y))
	                (equal? y '(1 2 3)))))))

    (define test-0059
      (test #t (let ((x (list 1 2 3 4 5 6)))
                 (let ((y (drop-right! x 3)))
	           (and (eq? x y)
	                (eq? (cdr x) (cdr y))
	                (eq? (cddr x) (cddr y))
	                (equal? y '(1 2 3)))))))

    (define test-0060
      (test #t (let-values (((a b) (split-at '(1 2 3 4 5 6) 2)))
                 (and (equal? a '(1 2))
	              (equal? b '(3 4 5 6))))))

    (define test-0061
      (test #t (let* ((x (list 1 2 3 4 5 6))
	              (y (cddr x)))
                 (let-values (((a b) (split-at! x 2)))
                   (and (equal? a '(1 2))
	                (eq? a x)
	                (equal? b '(3 4 5 6))
	                (eq? b y))))))

    (define test-0062
      (test #t (= 37 (last '(1 2 3 37)))))

    (define test-0063
      (test #f (length+ (circular-list 1 2 3))))

    (define test-0064
      (test 4 (length+ '(1 2 3 4))))

    (define test-0065
      (test #t (let ((x (list 1 2))
	             (y (list 3 4))
	             (z (list 5 6)))
                 (let ((r (append! x y '() z)))
	           (and (equal? r '(1 2 3 4 5 6))
	                (eq? r x)
	                (eq? (cdr r) (cdr x))
	                (eq? (cddr r) y)
	                (eq? (cdddr r) (cdr y))
	                (eq? (cddddr r) z)
	                (eq? (cdr (cddddr r)) (cdr z)))))))

    (define test-0066
      (test '(1 2 3 4 5 6 7 8 9) (concatenate '((1 2 3) (4 5 6) () (7 8 9))) ))

    (define test-0067
      (test '(1 2 3 4 5 6 7 8 9)
            (concatenate! `(,(list 1 2 3) ,(list 4 5 6) () ,(list 7 8 9)))))

    (define test-0068
      (test '(1 2 3 4 5 6) (append-reverse '(3 2 1) '(4 5 6))))

    (define test-0069
      (test '(1 2 3 4 5 6) (append-reverse! '(3 2 1) '(4 5 6))))

    (define test-0070
      (test  '((1 4) (2 5) (3 6)) (zip '(1 2 3) '(4 5 6))))

    (define test-0071
      (test '() (zip '() '() '() '())))

    (define test-0072
      (test '((1 1)) (zip '(1) (circular-list 1 2))))

    (define test-0073
      (test '(1 2 3 4 5) (unzip1 '((1) (2) (3) (4) (5)))))

    (define test-0074
      (test #t
            (let-values (((a b) (unzip2 '((10 11) (20 21) (30 31)))))
              (and (equal? a '(10 20 30))
               	   (equal? b '(11 21 31))))))

    (define test-0075
      (test #t (let-values (((a b c) (unzip3 '((10 11 12) (20 21 22) (30 31 32)))))
                 (and (equal? a '(10 20 30))
	              (equal? b '(11 21 31))
	              (equal? c '(12 22 32))))))

    (define test-0076
      (test #t (let-values (((a b c d) (unzip4 '((10 11 12 13)
				                 (20 21 22 23)
				                 (30 31 32 33)))))
                 (and (equal? a '(10 20 30))
	              (equal? b '(11 21 31))
	              (equal? c '(12 22 32))
	              (equal? d '(13 23 33))))))

    (define test-0077
      (test #t (let-values (((a b c d e) (unzip5 '((10 11 12 13 14)
					           (20 21 22 23 24)
					           (30 31 32 33 34)))))
                 (and (equal? a '(10 20 30))
	              (equal? b '(11 21 31))
	              (equal? c '(12 22 32))
	              (equal? d '(13 23 33))
	              (equal? e '(14 24 34))))))

    (define test-0078
      (test 3 (count even? '(3 1 4 1 5 9 2 5 6))))

    (define test-0079
      (test 3 (count < '(1 2 4 8) '(2 4 6 8 10 12 14 16))))

    (define test-0080
      (test 2 (count < '(3 1 4 1) (circular-list 1 10))))

    (define test-0081
      (test '(c 3 b 2 a 1) (fold cons* '() '(a b c) '(1 2 3 4 5))))

    (define test-0082
      (test '(a 1 b 2 c 3) (fold-right cons* '() '(a b c) '(1 2 3 4 5))))

    (define test-0083
      (test #t (let* ((x (list 1 2 3))
	              (r (list x (cdr x) (cddr x)))
	              (y (pair-fold (lambda (pair tail)
			              (set-cdr! pair tail) pair)
			            '()
			            x)))
                 (and (equal? y '(3 2 1))
	              (every (lambda (c) (memq c r)) (list y (cdr y) (cddr y)))
                      #t))))

    (define test-0084
      (test '((a b c) (b c) (c)) (pair-fold-right cons '() '(a b c))))

    (define test-0085
      (test 5 (reduce max 'illegal '(1 2 3 4 5))))

    (define test-0086
      (test 0 (reduce max 0 '())))

    (define test-0087
      (test '(1 2 3 4 5) (reduce-right append 'illegal '((1 2) () (3 4 5)))))

    (define test-0088
      (test '(1 4 9 16 25 36 49 64 81 100)
	    (unfold (lambda (x) (> x 10))
		    (lambda (x) (* x x))
		    (lambda (x) (+ x 1))
		    1)))

    (define test-0089
      (test '(1 4 9 16 25 36 49 64 81 100)
	    (unfold-right zero?
			  (lambda (x) (* x x))
			  (lambda (x) (- x 1))
			  10)))

    (define test-0090
      (test '(4 1 5 1)
	    (map + '(3 1 4 1) (circular-list 1 0))))

    (define test-0091
      (test '(5 4 3 2 1)
	    (let ((v 1)
		  (l '()))
	      (for-each (lambda (x y)
			  (let ((n v))
			    (set! v (+ v 1))
			    (set! l (cons n l))))
			'(0 0 0 0 0)
			(circular-list 1 2))
	      l)))

    (define test-0092
      (test '(1 -1 3 -3 8 -8)
	    (append-map (lambda (x) (list x (- x))) '(1 3 8))))

    (define test-0093
      (test '(1 -1 3 -3 8 -8)
	    (append-map! (lambda (x) (list x (- x))) '(1 3 8))))

    (define test-0094
      (test #t (let* ((l (list 1 2 3))
	              (m (map! (lambda (x) (* x x)) l)))
                 (and (equal? m '(1 4 9))
	              (equal? l '(1 4 9))))))

    (define test-0095
      (test '(1 2 3 4 5)
	    (let ((v 1))
	      (map-in-order (lambda (x)
			      (let ((n v))
				(set! v (+ v 1))
				n))
			    '(0 0 0 0 0)))))

    (define test-0096
      (test '((3) (2 3) (1 2 3))
	    (let ((xs (list 1 2 3))
		  (l '()))
	      (pair-for-each (lambda (x) (set! l (cons x l))) xs)
	      l)))

    (define test-0097
      (test '(1 9 49)
            (filter-map (lambda (x y) (and (number? x) (* x x)))
			'(a 1 b 3 c 7)
			(circular-list 1 2))))

    (define test-0098
      (test '(0 8 8 -4) (filter even? '(0 7 8 8 43 -4))))

    (define test-0099
      (test #t (let-values (((a b) (partition symbol? '(one 2 3 four five 6))))
                 (and (equal? a '(one four five))
	              (equal? b '(2 3 6))))))

    (define test-0100
      (test '(7 43) (remove even? '(0 7 8 8 43 -4))))

    (define test-0101
      (test #t (let* ((x (list 0 7 8 8 43 -4))
	              (y (pair-fold cons '() x))
	              (r (filter! even? x)))
                 (and (equal? '(0 8 8 -4) r)
	              (every (lambda (c) (memq c y)) (pair-fold cons '() r))
                      #t))))

    (define test-0102
      (test #t (let* ((x (list 'one 2 3 'four 'five 6))
	              (y (pair-fold cons '() x)))
                 (let-values (((a b) (partition! symbol? x)))
	           (and (equal? a '(one four five))
	                (equal? b '(2 3 6))
	                (every (lambda (c) (memq c y)) (pair-fold cons '() a))
	                (every (lambda (c) (memq c y)) (pair-fold cons '() b))
                        #t)))))

    (define test-0103
      (test #t (let* ((x (list 0 7 8 8 43 -4))
	              (y (pair-fold cons '() x))
	              (r (remove! even? x)))
                 (and (equal? '(7 43) r)
	              (every (lambda (c) (memq c y)) (pair-fold cons '() r))
                      #t))))

    (define test-0104
      (test 4 (find even? '(3 1 4 1 5 9 8))))

    (define test-0105
      (test '(4 1 5 9 8) (find-tail even? '(3 1 4 1 5 9 8))))

    (define test-0106
      (test '#f (find-tail even? '(1 3 5 7))))

    (define test-0107
      (test '(2 18) (take-while even? '(2 18 3 10 22 9))))

    (define test-0108
      (test #t (let* ((x (list 2 18 3 10 22 9))
	              (r (take-while! even? x)))
                 (and (equal? r '(2 18))
	              (eq? r x)
	              (eq? (cdr r) (cdr x))))))

    (define test-0109
      (test '(3 10 22 9) (drop-while even? '(2 18 3 10 22 9))))

    (define test-0110
      (test #t (let-values (((a b) (span even? '(2 18 3 10 22 9))))
                 (and (equal? a '(2 18))
	              (equal? b '(3 10 22 9))))))

    (define test-0111
      (test #t (let-values (((a b) (break even? '(3 1 4 1 5 9))))
                 (and (equal? a '(3 1))
	              (equal? b '(4 1 5 9))))))

    (define test-0112
      (test #t (let* ((x (list 2 18 3 10 22 9))
	              (cells (pair-fold cons '() x)))
                 (let-values (((a b) (span! even? x)))
                   (and (equal? a '(2 18))
	                (equal? b '(3 10 22 9))
	                (every (lambda (x) (memq x cells)) (pair-fold cons '() a))
	                (every (lambda (x) (memq x cells)) (pair-fold cons '() b))
                        #t)))))

    (define test-0113
      (test #t (let* ((x     (list 3 1 4 1 5 9))
	              (cells (pair-fold cons '() x)))
                 (let-values (((a b) (break! even? x)))
                   (and (equal? a '(3 1))
	                (equal? b '(4 1 5 9))
	                (every (lambda (x) (memq x cells)) (pair-fold cons '() a))
	                (every (lambda (x) (memq x cells)) (pair-fold cons '() b))
                        #t)))))

    (define test-0114
      (test #t (any integer? '(a 3 b 2.7))))

    (define test-0115
      (test #f (any integer? '(a 3.1 b 2.7))))

    (define test-0116
      (test #t (any < '(3 1 4 1 5) (circular-list 2 7 1 8 2))))

    (define test-0117
      (test 'yes (any (lambda (a b) (if (< a b) 'yes #f))
		      '(1 2 3) '(0 1 4))))

    (define test-0118
      (test #t (every integer? '(1 2 3))))

    (define test-0119
      (test #f (every integer? '(3 4 5.1))))

    (define test-0120
      (test #t (every < '(1 2 3) (circular-list 2 3 4))))

    (define test-0121
      (test 2 (list-index even? '(3 1 4 1 5 9))))

    (define test-0122
      (test 1 (list-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))))

    (define test-0123
      (test #f (list-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))))

    (define test-0124
      (test '(37 48) (member 5 '(1 2 5 37 48) <)))

    (define test-0125
      (test '(1 2 5) (delete 5 '(1 48 2 5 37) <)))

    (define test-0126
      (test '(1 2 7) (delete 5 '(1 5 2 5 7))))

    (define test-0127
      (test #t (let* ((x (list 1 48 2 5 37))
	              (cells (pair-fold cons '() x))
	              (r (delete! 5 x <)))
                 (and (equal? r '(1 2 5))
	              (every (lambda (x) (memq x cells)) (pair-fold cons '() r))
                      #t))))

    (define test-0128
      (test '((a . 3) (b . 7) (c . 1))
	    (delete-duplicates '((a . 3) (b . 7) (a . 9) (c . 1))
			       (lambda (x y) (eq? (car x) (car y))))))

    (define test-0129
      (test '(a b c z) (delete-duplicates '(a b a c a b c z) eq?)))

    (define test-0130
      (test '((a b c z))
            (let* ((x     (list 'a 'b 'a 'c 'a 'b 'c 'z))
	           (cells (pair-fold cons '() x))
	           (r     (delete-duplicates! x)))
              (and (equal? '(a b c z) r)
	           (every (lambda (x) (memq x cells)) (pair-fold cons '() r))))))

    (define test-0131
      (test '(3 . #t) (assoc 6
			     '((4 . #t) (3 . #t) (5 . #t))
			     (lambda (x y)
			       (zero? (remainder x y))))))

    (define test-0132
      (test '((1 . #t) (2 . #f)) (alist-cons 1 #t '((2 . #f)))))

    (define test-0133
      (test #t (let* ((a (list (cons 1 2) (cons 3 4)))
	              (b (alist-copy a)))
                 (and (equal? a b)
	              (every (lambda (x) (not (memq x b))) a)
	              (every (lambda (x) (not (memq x a))) b)))))

    (define test-0134
      (test '((1 . #t) (2 . #t) (4 . #t))
	    (alist-delete 5 '((1 . #t) (2 . #t) (37 . #t) (4 . #t) (48 #t)) <)))

    (define test-0135
      (test '((1 . #t) (2 . #t) (4 . #t))
	    (alist-delete 7 '((1 . #t) (2 . #t) (7 . #t) (4 . #t) (7 #t)))))

    (define test-0136
      (test '((4 . #t) (7 . #t))
            (let* ((x (list-copy '((1 . #t) (2 . #t) (7 . #t) (4 . #t) (7 . #t))))
	           (y (list-copy x))
	           (cells (pair-fold cons '() x))
	           (r (alist-delete! 7 x)))
              (and (equal? r '((1 . #t) (2 . #t) (4 . #t)))
	           (every (lambda (x) (memq x cells)) (pair-fold cons '() r))
	           (every (lambda (x) (memq x y)) r)))))

    (define test-0137
      (test #t (lset<= eq? '(a) '(a b a) '(a b c c))))

    (define test-0138
      (test #f (lset<= eq? '(a) '(a b a) '(a))))

    (define test-0139
      (test #t (lset<= eq?)))

    (define test-0140
      (test #t (lset<= eq? '(a))))

    (define test-0141
      (test #t (lset= eq? '(b e a) '(a e b) '(e e b a))))

    (define test-0142
      (test #f (lset= eq? '(b e a) '(a e b) '(e e b a c))))

    (define test-0143
      (test #t (lset= eq?)))

    (define test-0144
      (test #t (lset= eq? '(a))))

    (define test-0145
      (test '(u o i a b c d c e)
	    (lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o 'u)))

    (define test-0146
      (test '(u o i a b c d e)
	    (lset-union eq? '(a b c d e) '(a e i o u))))

    (define test-0147
      (test '(x a a c) (lset-union eq? '(a a c) '(x a x))))

    (define test-0148
      (test #t (null? (lset-union eq?))))

    (define test-0149
      (test '(a b c) (lset-union eq? '(a b c))))

    (define test-0150
      (test '(a e) (lset-intersection eq? '(a b c d e) '(a e i o u))))

    (define test-0151
      (test '(a x a) (lset-intersection eq? '(a x y a) '(x a x z))))

    (define test-0152
      (test '(a b c) (lset-intersection eq? '(a b c))))

    (define test-0153
      (test '(b c d) (lset-difference eq? '(a b c d e) '(a e i o u))))

    (define test-0154
      (test '(a b c) (lset-difference eq? '(a b c))))

    (define test-0155
      (let ((x '(d c b i o u)))
        (test x (lset= eq? x (lset-xor eq? '(a b c d e) '(a e i o u))))))

    (define test-0156
      (test #t (lset= eq? '() (lset-xor eq?))))

    (define test-0157
      (let ((x '(a b c d e)))
        (test '(e) (lset= eq? x (lset-xor eq? '(a b c d e))))))

    (define test-0158
      (test #t (let-values (((d i) (lset-diff+intersection eq? '(a b c d e) '(c d f))))
                 (and (equal? d '(a b e))
	              (equal? i '(c d))))))

    ;; FIXME: For the following five procedures, need to check that
    ;; cells returned are from the arguments.

    (define test-0159
      (test '(u o i a b c d e)
	    (lset-union! eq? (list 'a 'b 'c 'd 'e) (list 'a 'e 'i 'o 'u))))

    (define test-0160
      (test '(x a a c) (lset-union! eq? (list 'a 'a 'c) (list 'x 'a 'x))))

    (define test-0161
      (test #t (null? (lset-union! eq?))))

    (define test-0162
      (test '(a b c) (lset-union! eq? (list 'a 'b 'c))))

    (define test-0163
      (test '(a e) (lset-intersection! eq?
                                       (list 'a 'b 'c 'd 'e)
				       (list 'a 'e 'i 'o 'u))))

    (define test-0164
      (test '(a x a) (lset-intersection! eq?
                                         (list 'a 'x 'y 'a)
					 (list 'x 'a 'x 'z))))
    (define test-0165
      (test '(a b c) (lset-intersection! eq? (list 'a 'b 'c))))

    (define test-0166
      (test '(b c d) (lset-difference! eq?
                                       (list 'a 'b 'c 'd 'e)
				       (list 'a 'e 'i 'o 'u))))

    (define test-0167
      (test '(a b c) (lset-difference! eq? (list 'a 'b 'c))))

    (define test-0168
      (let ((x '(d c b i o u)))
        (test x (lset= eq? x
                       (lset-xor! eq?
                                  (list 'a 'b 'c 'd 'e)
				  (list 'a 'e 'i 'o 'u))))))

    (define test-0169
      (test #t (lset= eq? '() (lset-xor! eq?))))

    (define test-0170
      (test '(e) (lset= eq? '(a b c d e) (lset-xor! eq? (list 'a 'b 'c 'd 'e)))))

    (define test-0171
      (test #t (let-values (((d i) (lset-diff+intersection! eq?
                                                            (list 'a 'b 'c 'd 'e)
						            (list 'c 'd 'f))))
                 (and (equal? d '(a b e))
	              (equal? i '(c d))))))))
