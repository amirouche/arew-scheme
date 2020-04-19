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
(library (srfi srfi-146 hash-tests)
  (export
   test-000 test-001 test-002 test-003 test-004 test-005 test-006 test-007 test-008 test-009
   test-010 test-011 test-012 test-013 test-014 test-015 test-016 test-017 test-018 test-019
   test-020 test-021 test-022 test-023 test-024 test-025 test-026 test-027 test-028 test-029
   test-030 test-031 test-032 test-033 test-034 test-035 test-036 test-037 test-038 test-039
   test-040 test-041 test-042 test-043 test-044 test-045 test-046 test-047 test-048 test-049
   test-050 test-051 test-052 test-053 test-054 test-055 test-056 test-057 test-058 test-059
   test-060 test-061 test-062 test-063 test-064 test-065 test-066 test-067 test-068 test-069
   test-070 test-071 test-072 test-073 test-074)

  (import (scheme base)
          (tests)
	  (srfi srfi-1)
	  (srfi srfi-8)
	  (srfi srfi-128)
	  (srfi srfi-146 hash))

  (define comparator (make-default-comparator))

  ;; SRFI 146: Hashmaps

  ;; Predicates
  (define hashmap0 (hashmap comparator))
  (define hashmap1 (hashmap comparator 'a 1 'b 2 'c 3))
  (define hashmap2 (hashmap comparator 'c 1 'd 2 'e 3))
  (define hashmap3 (hashmap comparator 'd 1 'e 2 'f 3))

  (define test-000
    (test #t
	  (hashmap? (hashmap comparator))))

  (define test-001
    (test #t
	  (not (hashmap? (list 1 2 3)))))

  (define test-002
    (test #t
	  (hashmap-empty? hashmap0)))

  (define test-003
    (test #t
	  (not (hashmap-empty? hashmap1))))

  (define test-004
    (test #t
	  (hashmap-contains? hashmap1 'b)))

  (define test-005
    (test #t
          (not (hashmap-contains? hashmap1 '2))))

  (define test-006
    (test #t
          (hashmap-disjoint? hashmap1 hashmap3)))

  (define test-007
    (test #t
          (not (hashmap-disjoint? hashmap1 hashmap2))))

  ;; Accessors
  (define hashmap11 (hashmap comparator 'a 1 'b 2 'c 3))

  (define test-008
    (test
     2
     (hashmap-ref hashmap11 'b)))

  (define test-009
    (test
     42
     (hashmap-ref hashmap11 'd (lambda () 42))))

  ;; TODO: convert to a test
  ;; (test-error "hashmap-ref: key not found/without failure"
  ;;             (hashmap-ref hashmap1 'd))

  (define test-010
    (test
     (* 2 2)
     (hashmap-ref hashmap11 'b (lambda () #f) (lambda (x) (* x x)))))

  (define test-011
    (test
     3
     (hashmap-ref/default hashmap11 'c 42)))

  (define test-012
    (test
     42
     (hashmap-ref/default hashmap11 'd 42)))

  (define test-013
    (test
     comparator
     (hashmap-key-comparator hashmap11)))

  ;; Updaters
  (define hashmap21 (hashmap comparator 'a 1 'b 2 'c 3))
  (define hashmap22 (hashmap-set hashmap21 'c 4 'd 4 'd 5))
  (define hashmap23 (hashmap-update hashmap21 'b (lambda (x) (* x x))))
  (define hashmap24 (hashmap-update/default hashmap21 'd (lambda (x) (* x x)) 4))
  (define hashmap25 (hashmap-adjoin hashmap21 'c 4 'd 4 'd 5))
  (define hashmap20 (hashmap comparator))

  (define test-014
    (test
     3
     (hashmap-ref hashmap25 'c)))

  (define test-015
    (test
     4
     (hashmap-ref hashmap25 'd)))

  (define test-016
    (test
     4
     (hashmap-ref hashmap22 'c)))

  (define test-017
    (test
     5
     (hashmap-ref hashmap22 'd)))

  (define test-018
    (test
     #f
     (hashmap-ref/default (hashmap-replace hashmap21 'd 4) 'd #f)))

  (define test-019
    (test
     6
     (hashmap-ref (hashmap-replace hashmap21 'c 6) 'c)))

  (define test-020
    (test
     42
     (hashmap-ref/default (hashmap-delete hashmap21 'b) 'b 42)))

  (define test-021
    (test
     42
     (hashmap-ref/default (hashmap-delete-all hashmap21 '(a b)) 'b 42)))

  (define test-022
    (test
     (list hashmap21 2)
     (receive result
	 (hashmap-intern hashmap21 'b (lambda () (error "should not have been invoked")))
       result)))

  (define test-023
    (test
     (list 42 42)
     (receive (hashmap value)
	 (hashmap-intern hashmap21 'd (lambda () 42))
       (list value (hashmap-ref hashmap 'd)))))

  (define test-024
    (test
     4
     (hashmap-ref hashmap23 'b)))

  (define test-025
    (test
     16
     (hashmap-ref hashmap24 'd)))

  (define test-026
    (test
     'empty
     (hashmap-pop hashmap20 (lambda () 'empty))))

  ;; TODO: convert to a test
  ;; (test-assert "hashmap-pop: non-empty hashmap"
  ;;   (member
  ;;    (receive (hashmap key value)
  ;;        (hashmap-pop hashmap1)
  ;;      (list (hashmap-size hashmap) key value))
  ;;    '((2 a 1) (2 b 2) (2 c 3)))))

  ;; The whole hashmap
  (define hashmap30 (hashmap comparator))
  (define hashmap31 (hashmap comparator 'a 1 'b 2 'c 3))

  (define test-027
    (test
     0
     (hashmap-size hashmap30)))

  (define test-028
    (test
     3
     (hashmap-size hashmap31)))

  (define test-029
    (test
     (list 'b 2)
     (receive result
	 (hashmap-find (lambda (key value)
			 (and (eq? key 'b)
			      (= value 2)))
		       hashmap31
		       (lambda () (error "should not have been called")))
       result)))

  (define test-030
    (test
     (list 42)
     (receive result
	 (hashmap-find (lambda (key value)
			 (eq? key 'd))
		       hashmap31
		       (lambda ()
			 42))
       result)))

  (define test-031
    (test
     2
     (hashmap-count (lambda (key value)
		      (>= value 2))
		    hashmap31)))

  (define test-032
    (test #t
          (hashmap-any? (lambda (key value)
		          (= value 3))
		        hashmap31)))

  (define test-033
    (test #t
          (not (hashmap-any? (lambda (key value)
			       (= value 4))
			     hashmap31))))

  (define test-034
    (test #t
          (hashmap-every? (lambda (key value)
		            (<= value 3))
		          hashmap31)))

  (define test-035
    (test #t
	  (not (hashmap-every? (lambda (key value)
			         (<= value 2))
			       hashmap31))))

  (define test-036
    (test
     3
     (length (hashmap-keys hashmap31))))

  (define test-037
    (test
     6
     (fold + 0 (hashmap-values hashmap31))))

  (define test-038
    (test
     (list 3 6)
     (receive (keys values)
	 (hashmap-entries hashmap31)
       (list (length keys) (fold + 0 values)))))

  ;; Hashmap and folding
  (define hashmap41 (hashmap comparator 'a 1 'b 2 'c 3))
  (define hashmap42 (hashmap-map (lambda (key value)
				   (values (symbol->string key)
					   (* 10 value)))
			         comparator
			         hashmap41))

  (define test-039
    (test
     20
     (hashmap-ref hashmap42 "b")))

  (define test-040
    (test
     6
     (let ((counter 0))
       (hashmap-for-each (lambda (key value)
			   (set! counter (+ counter value)))
			 hashmap41)
       counter)))

  (define test-041
    (test
     6
     (hashmap-fold (lambda (key value acc)
		     (+ value acc))
		   0
		   hashmap41)))

  (define test-042
    (test
     (+ (* 1 1) (* 2 2) (* 3 3))
     (fold + 0 (hashmap-map->list (lambda (key value)
				    (* value value))
				  hashmap41))))

  (define test-043
    (test
     2
     (hashmap-size (hashmap-filter (lambda (key value)
				     (<= value 2))
				   hashmap41))))

  (define test-044
    (test
     1
     (hashmap-size (hashmap-remove (lambda (key value)
				     (<= value 2))
				   hashmap41))))

  (define test-045
    (test
     (list 1 2)
     (receive result
	 (hashmap-partition (lambda (key value)
			      (eq? 'b key))
			    hashmap41)
       (map hashmap-size result))))

  ;; Copying and conversion
  (define hashmap51 (hashmap comparator 'a 1 'b 2 'c 3))
  (define hashmap52 (alist->hashmap comparator '((a . 1) (b . 2) (c . 3))))
  (define hashmap53 (alist->hashmap! (hashmap-copy hashmap51) '((d . 4) '(c . 5))))

  (define test-046
    (test
     3
     (hashmap-size (hashmap-copy hashmap51))))

  (define test-047
    (test
     comparator
     (hashmap-key-comparator (hashmap-copy hashmap51))))

  (define test-048
    (test
     (cons 'b 2)
     (assq 'b (hashmap->alist hashmap51))))

  (define test-049
    (test
     2
     (hashmap-ref hashmap52 'b)))

  (define test-050
    (test
     4
     (hashmap-ref hashmap53 'd)))

  (define test-051
    (test
     3
     (hashmap-ref hashmap53 'c)))

  ;; Subhashmaps

  (define hashmap61 (hashmap comparator 'a 1 'b 2 'c 3))
  (define hashmap62 (hashmap comparator 'a 1 'b 2 'c 3))
  (define hashmap63 (hashmap comparator 'a 1 'c 3))
  (define hashmap64 (hashmap comparator 'a 1 'c 3 'd 4))
  (define hashmap65 (hashmap comparator 'a 1 'b 2 'c 6))
  (define hashmap66 (hashmap (make-comparator (comparator-type-test-predicate comparator)
					      (comparator-equality-predicate comparator)
					      (comparator-ordering-predicate comparator)
					      (comparator-hash-function comparator))
			     'a 1 'b 2 'c 3))

  (define test-052
    (test #t
          (hashmap=? comparator hashmap61 hashmap62)))

  (define test-053
    (test #t
          (not (hashmap=? comparator hashmap61 hashmap64))))

  (define test-054
    (test #t
          (not (hashmap=? comparator hashmap61 hashmap66))))

  (define test-055
    (test #t
          (hashmap<? comparator hashmap63 hashmap61)))

  (define test-056
    (test #t
          (not (hashmap<? comparator hashmap63 hashmap61 hashmap62))))

  (define test-057
    (test #t
          (hashmap>? comparator hashmap62 hashmap63)))

  (define test-058
    (test #t
          (not (hashmap>? comparator hashmap61 hashmap62 hashmap63))))

  (define test-059
    (test #t
          (hashmap<=? comparator hashmap63 hashmap62 hashmap61)))

  (define test-060
    (test #t
          (not (hashmap<=? comparator hashmap63 hashmap65))))

  (define test-061
    (test #t
          (not (hashmap<=? comparator hashmap62 hashmap64))))

  (define test-062
    (test #t
          (hashmap>=? comparator hashmap64 hashmap63)))

  (define test-063
    (test #t
          (not (hashmap>=? comparator hashmap65 hashmap63))))

  ;; Set theory operations
  (define hashmap71 (hashmap comparator 'a 1 'b 2 'c 3))
  (define hashmap72 (hashmap comparator 'a 1 'b 2 'd 4))
  (define hashmap73 (hashmap comparator 'a 1 'b 2))
  (define hashmap74 (hashmap comparator 'a 1 'b 2 'c 4))
  (define hashmap75 (hashmap comparator 'a 1 'c 3))
  (define hashmap76 (hashmap comparator 'd 4 'e 5 'f 6))

  (define test-064
    (test
     4
     (hashmap-ref (hashmap-union hashmap71 hashmap72) 'd)))

  (define test-065
    (test
     3
     (hashmap-ref (hashmap-union hashmap71 hashmap74) 'c)))

  (define test-066
    (test
     6
     (hashmap-size (hashmap-union hashmap71 hashmap72 hashmap76))))

  (define test-067
    (test
     3
     (hashmap-ref (hashmap-intersection hashmap71 hashmap74) 'c)))

  (define test-068
    (test
     42
     (hashmap-ref/default (hashmap-intersection hashmap71 hashmap75) 'b 42)))

  (define test-069
    (test
     2
     (hashmap-size (hashmap-difference hashmap72 hashmap76))))

  (define test-070
    (test
     4
     (hashmap-size (hashmap-xor hashmap72 hashmap76))))

  ;; Comparators

  (define hashmap81 (hashmap comparator 'a 1 'b 2 'c 3))
  (define hashmap82 (hashmap comparator 'a 1 'b 2 'c 3))
  (define hashmap83 (hashmap comparator 'a 1 'b 2))
  (define hashmap84 (hashmap comparator 'a 1 'b 2 'c 4))
  (define hashmap85 (hashmap comparator 'a 1 'c 3))
  (define hashmap80 (hashmap comparator
			     hashmap81 "a"
			     hashmap82 "b"
			     hashmap83 "c"
			     hashmap84 "d"
			     hashmap85 "e"))

  (define test-071
    (test #t
          (comparator? hashmap-comparator)))

  (define test-072
    (test
     (list "a" "a" "c" "d" "e")
     (list (hashmap-ref hashmap80 hashmap81)
	   (hashmap-ref hashmap80 hashmap82)
	   (hashmap-ref hashmap80 hashmap83)
	   (hashmap-ref hashmap80 hashmap84)
	   (hashmap-ref hashmap80 hashmap85)
	   )))

  ;; Ordering comparators
  (define test-073
    (test #t
          (=? comparator hashmap81 hashmap82)))

  (define test-074
    (test #t
          (not (=? comparator hashmap81 hashmap84)))))
