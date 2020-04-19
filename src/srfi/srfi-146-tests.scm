;; Copyright (C) Marc Nieper-Wißkirchen (2016, 2017).  All Rights
;; Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
(library (srfi srfi-146-tests)
  (export
   test-000 test-001 test-002 test-003 test-004 test-005 test-006 test-007 test-008 test-009
   test-010 test-011 test-012 test-013 test-014 test-015 test-016 test-017 test-018 test-019
   test-020 test-021 test-022 test-023 test-024 test-025 test-026 test-027 test-028 test-029
   test-030 test-031 test-032 test-033 test-034 test-035 test-036 test-037 test-038 test-039
   test-040 test-041 test-042 test-043 test-044 test-045 test-046 test-047 test-048 test-049
   test-050 test-051 test-052 test-053 test-054 test-055 test-056 test-057 test-058 test-059
   test-060 test-061 test-062 test-063 test-064 test-065 test-066 test-067 test-068 test-069
   test-070 test-071 test-072 test-073 test-074 test-075 test-076 test-077 test-078 test-079
   test-080 test-081 test-082 test-083 test-084 test-085 test-086 test-087 test-088 test-089
   test-090 test-091 test-092 test-093 test-094 test-095)

  (import (scheme base)
          (tests)
	  (srfi srfi-1)
	  (srfi srfi-8)
	  (srfi srfi-128)
	  (srfi srfi-146))

  (define comparator (make-default-comparator))

  (define mapping0 (mapping comparator))
  (define mapping1 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping2 (mapping comparator 'c 1 'd 2 'e 3))
  (define mapping3 (mapping comparator 'd 1 'e 2 'f 3))

  (define test-000
    (test #t (mapping? (mapping comparator))))

  (define test-001
    (test #t (not (mapping? (list 1 2 3)))))

  (define test-002
    (test #t (mapping-empty? mapping0)))

  (define test-003
    (test #t (not (mapping-empty? mapping1))))

  (define test-004
    (test #t (mapping-contains? mapping1 'b)))

  (define test-005
    (test #t (not (mapping-contains? mapping1 '2))))

  (define test-006
    (test #t (mapping-disjoint? mapping1 mapping3)))

  (define test-007
    (test #t (not (mapping-disjoint? mapping1 mapping2))))

  ;; Accessors
  (define mapping11 (mapping comparator 'a 1 'b 2 'c 3))

  (define test-008
    (test 2 (mapping-ref mapping11 'b)))

  (define test-009
    (test
     42
     (mapping-ref mapping11 'd (lambda () 42))))

  ;; TODO: make it a test
  ;; (test-error "mapping-ref: key not found/without failure"
  ;;   (mapping-ref mapping11 'd))

  (define test-010
    (test
     (* 2 2)
     (mapping-ref mapping11 'b (lambda () #f) (lambda (x) (* x x)))))

  (define test-011
    (test
     3
     (mapping-ref/default mapping11 'c 42)))

  (define test-012
    (test
     42
     (mapping-ref/default mapping11 'd 42)))

  (define test-013
    (test
     comparator
     (mapping-key-comparator mapping11)))

  ;; Updaters
  (define mapping21 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping22 (mapping-set mapping1 'c 4 'd 4 'd 5))
  (define mapping23 (mapping-update mapping1 'b (lambda (x) (* x x))))
  (define mapping24 (mapping-update/default mapping1 'd (lambda (x) (* x x)) 4))
  (define mapping25 (mapping-adjoin mapping1 'c 4 'd 4 'd 5))
  (define mapping20 (mapping comparator))

  (define test-014
    (test
	  3
	  (mapping-ref mapping25 'c)))

  (define test-015
    (test
     4
     (mapping-ref mapping25 'd)))

  (define test-016
    (test
     4
     (mapping-ref mapping22 'c)))

  (define test-017
    (test
     5
     (mapping-ref mapping22 'd)))

  (define test-018
    (test
     #f
     (mapping-ref/default (mapping-replace mapping21 'd 4) 'd #f)))

  (define test-019
    (test
     6
     (mapping-ref (mapping-replace mapping21 'c 6) 'c)))

  (define test-020
    (test
     42
     (mapping-ref/default (mapping-delete mapping21 'b) 'b 42)))

  (define test-021
    (test
     42
     (mapping-ref/default (mapping-delete-all mapping21 '(a b)) 'b 42)))

  (define test-022
    (test
     (list mapping21 2)
     (receive result
	 (mapping-intern mapping21 'b (lambda () (error "should not have been invoked")))
       result)))

  (define test-023
    (test
     (list 42 42)
     (receive (mapping value)
	 (mapping-intern mapping21 'd (lambda () 42))
       (list value (mapping-ref mapping 'd)))))

  (define test-024
    (test
     4
     (mapping-ref mapping23 'b)))

  (define test-025
    (test
     16
     (mapping-ref mapping24 'd)))

  (define test-026
    (test
     'empty
     (mapping-pop mapping20 (lambda () 'empty))))

  (define test-027
    (test
     (list 2 'a 1)
     (receive (mapping key value)
	 (mapping-pop mapping21)
       (list (mapping-size mapping) key value))))

  (define test-028
    (test
     '("success updated"
       "failure ignored"
       ((0 . "zero") (1 . "one") (2 . "two [seen]") (3 . "three")
	(4 . "four") (5 . "five")))
     (let ((m1 (mapping (make-default-comparator)
                        1 "one"
                        3 "three"
                        0 "zero"
                        4 "four"
                        2 "two"
                        5 "five")))
       (define (f/ignore insert ignore)
         (ignore "failure ignored"))
       (define (s/update key val update remove)
         (update key
                 (string-append val " [seen]")
                 "success updated"))
       (let*-values (((m2 v2) (mapping-search m1 2 f/ignore s/update))
                     ((m3 v3) (mapping-search m2 42 f/ignore s/update)))
         (list v2 v3 (mapping->alist m3))))))

  ;; The whole mapping

  (define mapping30 (mapping comparator))
  (define mapping31 (mapping comparator 'a 1 'b 2 'c 3))

  (define test-029
    (test
	  0
	  (mapping-size mapping30)))

  (define test-030
    (test
     3
     (mapping-size mapping31)))

  (define test-031
    (test
     (list 'b 2)
     (receive result
	 (mapping-find (lambda (key value)
			 (and (eq? key 'b)
			      (= value 2)))
		       mapping31
		       (lambda () (error "should not have been called")))
       result)))

  (define test-032
    (test
     (list 42)
     (receive result
	 (mapping-find (lambda (key value)
			 (eq? key 'd))
		       mapping31
		       (lambda ()
			 42))
       result)))

  (define test-033
    (test
     2
     (mapping-count (lambda (key value)
		      (>= value 2))
		    mapping31)))

  (define test-034
    (test #t
	  (mapping-any? (lambda (key value)
		          (= value 3))
		        mapping31)))

  (define test-035
    (test #t
	  (not (mapping-any? (lambda (key value)
			       (= value 4))
			     mapping31))))

  (define test-036
    (test #t
          (mapping-every? (lambda (key value)
		            (<= value 3))
		          mapping31)))

  (define test-037
    (test #t
	  (not (mapping-every? (lambda (key value)
			         (<= value 2))
			       mapping31))))

  (define test-038
    (test
     3
     (length (mapping-keys mapping31))))

  (define test-039
    (test
     6
     (fold + 0 (mapping-values mapping31))))

  (define test-040
    (test
     (list 3 6)
     (receive (keys values)
	 (mapping-entries mapping31)
       (list (length keys) (fold + 0 values)))))

  (define mapping41 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping42 (mapping-map (lambda (key value)
				   (values (symbol->string key)
					   (* 10 value)))
			         comparator
			         mapping1))
  (define test-041
    (test
	  20
	  (mapping-ref mapping42 "b")))

  (define test-042
    (test
	  6
	  (let ((counter 0))
	    (mapping-for-each (lambda (key value)
			        (set! counter (+ counter value)))
			      mapping41)
	    counter)))

  (define test-043
    (test
     6
     (mapping-fold (lambda (key value acc)
		     (+ value acc))
		   0
		   mapping41)))

  (define test-044
    (test
     (+ (* 1 1) (* 2 2) (* 3 3))
     (fold + 0 (mapping-map->list (lambda (key value)
				    (* value value))
				  mapping41))))

  (define test-045
    (test
     2
     (mapping-size (mapping-filter (lambda (key value)
				     (<= value 2))
				   mapping41))))

  (define test-046
    (test
     1
     (mapping-size (mapping-remove (lambda (key value)
				     (<= value 2))
				   mapping41))))

  (define test-047
    (test
     (list 1 2)
     (receive result
	 (mapping-partition (lambda (key value)
			      (eq? 'b key))
			    mapping41)
       (map mapping-size result))))

  ;; Copying and conversion
  (define mapping51 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping52 (alist->mapping comparator '((a . 1) (b . 2) (c . 3))))
  (define mapping53 (alist->mapping! (mapping-copy mapping1) '((d . 4) '(c . 5))))

  (define test-048
    (test
     3
     (mapping-size (mapping-copy mapping51))))

  (define test-049
    (test
     comparator
     (mapping-key-comparator (mapping-copy mapping51))))

  (define test-050
    (test
     (cons 'b 2)
     (assq 'b (mapping->alist mapping51))))

  (define test-051
    (test
     2
     (mapping-ref mapping52 'b)))

  (define test-052
    (test
     4
     (mapping-ref mapping53 'd)))

  (define test-053
    (test
     3
     (mapping-ref mapping53 'c)))

  ;; Submappings
  (define mapping61 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping62 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping63 (mapping comparator 'a 1 'c 3))
  (define mapping64 (mapping comparator 'a 1 'c 3 'd 4))
  (define mapping65 (mapping comparator 'a 1 'b 2 'c 6))
  (define mapping66 (mapping (make-comparator (comparator-type-test-predicate comparator)
					      (comparator-equality-predicate comparator)
					      (comparator-ordering-predicate comparator)
					      (comparator-hash-function comparator))
			     'a 1 'b 2 'c 3))


  (define test-054
    (test #t
          (mapping=? comparator mapping61 mapping62)))

  (define test-055
    (test #t
          (not (mapping=? comparator mapping61 mapping64))))

  (define test-056
    (test #t
	  (not (mapping=? comparator mapping61 mapping66))))

  (define test-057
    (test #t
          (mapping<? comparator mapping63 mapping61)))

  (define test-058
    (test #t
	  (not (mapping<? comparator mapping3 mapping61 mapping62))))

  (define test-059
    (test #t
	  (mapping>? comparator mapping62 mapping63)))

  (define test-060
    (test #t
	  (not (mapping>? comparator mapping1 mapping62 mapping63))))

  (define test-061
    (test #t
	  (mapping<=? comparator mapping63 mapping62 mapping61)))

  (define test-062
    (test #t
          (not (mapping<=? comparator mapping63 mapping65))))

  (define test-063
    (test #t
          (not (mapping<=? comparator mapping62 mapping64))))

  (define test-064
    (test #t
	  (mapping>=? comparator mapping64 mapping63)))

  (define test-065
    (test #t
	  (not (mapping>=? comparator mapping65 mapping63))))

  ;; Set theory operations
  (define mapping71 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping72 (mapping comparator 'a 1 'b 2 'd 4))
  (define mapping73 (mapping comparator 'a 1 'b 2))
  (define mapping74 (mapping comparator 'a 1 'b 2 'c 4))
  (define mapping75 (mapping comparator 'a 1 'c 3))
  (define mapping76 (mapping comparator 'd 4 'e 5 'f 6))

  (define test-066
    (test
     4
     (mapping-ref (mapping-union mapping71 mapping72) 'd)))

  (define test-067
    (test
     3
     (mapping-ref (mapping-union mapping71 mapping74) 'c)))

  (define test-068
    (test
     6
     (mapping-size (mapping-union mapping71 mapping72 mapping76))))

  (define test-069
    (test
     3
     (mapping-ref (mapping-intersection mapping71 mapping74) 'c)))

  (define test-070
    (test
     42
     (mapping-ref/default (mapping-intersection mapping71 mapping75) 'b 42)))

  (define test-071
    (test
     2
     (mapping-size (mapping-difference mapping72 mapping76))))

  (define test-072
    (test
     4
     (mapping-size (mapping-xor mapping72 mapping76))))

  ;; Additional procedures for mappings with ordered keys

  (define mapping81 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping82 (mapping comparator 'a 1 'b 2 'c 3 'd 4))
  (define mapping83 (mapping comparator 'a 1 'b 2 'c 3 'd 4 'e 5))
  (define mapping84 (mapping comparator 'a 1 'b 2 'c 3 'd 4 'e 5 'f 6))
  (define mapping85 (mapping comparator 'f 6 'g 7 'h 8))

  (define test-073
    (test
     '(a a a a)
     (map mapping-min-key (list mapping81 mapping82 mapping83 mapping84))))

  (define test-074
    (test
     '(c d e f)
     (map mapping-max-key (list mapping81 mapping82 mapping83 mapping84))))

  (define test-075
    (test
     '(1 1 1 1)
     (map mapping-min-value (list mapping81 mapping82 mapping83 mapping84))))

  (define test-076
    (test
     '(3 4 5 6)
     (map mapping-max-value (list mapping81 mapping82 mapping83 mapping84))))

  (define test-077
    (test
     '(c d d d)
     (map (lambda (mapping)
	    (mapping-key-predecessor mapping 'e (lambda () #f)))
	  (list mapping81 mapping82 mapping83 mapping84))))

  (define test-078
    (test
     '(#f #f e e)
     (map (lambda (mapping)
	    (mapping-key-successor mapping 'd (lambda () #f)))
	  (list mapping81 mapping82 mapping83 mapping84))))

  (define test-079
    (test '(4)
	  (mapping-values (mapping-range= mapping84 'd))))

  (define test-080
    (test
     '()
     (mapping-values (mapping-range= mapping84 'z))))

  (define test-081
    (test '(1 2 3)
	  (mapping-values (mapping-range< mapping84 'd))))

  (define test-082
    (test
     '(1 2 3 4)
     (mapping-values (mapping-range<= mapping84 'd))))

  (define test-083
    (test
     '(5 6)
     (mapping-values (mapping-range> mapping84 'd))))

  (define test-084
    (test
     '(4 5 6)
     (mapping-values (mapping-range>= mapping84 'd))))

  (define test-085
    (test
     '((1 2 3) (1 2 3 4) (4) (4 5 6) (5 6))
     (receive mappings
	 (mapping-split mapping84 'd)
       (map mapping-values mappings))))

  (define test-086
    (test
     '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6) (g . 7) (h . 8))
     (mapping->alist (mapping-catenate comparator mapping82 'e 5 mapping85))))

  (define test-087
    (test
     '((1 . 1) (2 . 4) (3 . 9))
     (mapping->alist
      (mapping-map/monotone (lambda (key value)
			      (values value (* value value)))
			    comparator
			    mapping81))))

  (define test-088
    (test
     '(1 2 3)
     (mapping-fold/reverse (lambda (key value acc)
			     (cons value acc))
			   '() mapping81)))

  ;; Comparators

  (define mapping91 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping92 (mapping comparator 'a 1 'b 2 'c 3))
  (define mapping93 (mapping comparator 'a 1 'b 2))
  (define mapping94 (mapping comparator 'a 1 'b 2 'c 4))
  (define mapping95 (mapping comparator 'a 1 'c 3))
  (define mapping90 (mapping comparator mapping91 "a" mapping92 "b" mapping93 "c" mapping94 "d" mapping95 "e"))

  (define test-089
    (test #t
          (comparator? mapping-comparator)))

  (define test-090
    (test
     (list "a" "a" "c" "d" "e")
     (list (mapping-ref mapping90 mapping91)
	   (mapping-ref mapping90 mapping92)
	   (mapping-ref mapping90 mapping93)
	   (mapping-ref mapping90 mapping94)
	   (mapping-ref mapping90 mapping95))))

  ;; Ordering comparators
  (define test-091
    (test #t
          (=? comparator mapping91 mapping92)))

  (define test-092
    (test #t
	  (not (=? comparator mapping91 mapping94))))

  (define test-093
    (test #t
	  (<? comparator mapping93 mapping94)))

  (define test-094
    (test #t
          (<? comparator mapping91 mapping94)))

  (define test-095
    (test #t
          (<? comparator mapping91 mapping95))))
