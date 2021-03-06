(library (nstore2)
  (export make-nstore
          nstore
          nstore-ask?
          nstore-add!
          nstore-delete!
          nstore-var
          nstore-var?
          nstore-var-name
          nstore-from
          nstore-where
          nstore-query)

  (import (scheme base)
          (scheme list)
          (scheme generator)
          (scheme mapping hash)
          (db))

  ;; combinatorics helpers

  (define (permutations s)
    ;; http://rosettacode.org/wiki/Permutations#Scheme
    (cond
     ((null? s) '(()))
     ((null? (cdr s)) (list s))
     (else ;; extract each item in list in turn and permutations the rest
      (let splice ((l '()) (m (car s)) (r (cdr s)))
        (append
         (map (lambda (x) (cons m x)) (permutations (append l r)))
         (if (null? r) '()
             (splice (cons m l) (car r) (cdr r))))))))

  (define (combination k lst)
    (cond
     ((= k 0) '(()))
     ((null? lst) '())
     (else
      (let ((head (car lst))
            (tail (cdr lst)))
        (append (map (lambda (y) (cons head y)) (combination (- k 1) tail))
                (combination k tail))))))

  (define (combinations lst)
    (if (null? lst) '(())
        (let* ((head (car lst))
               (tail (cdr lst))
               (s (combinations tail))
               (v (map (lambda (x) (cons head x)) s)))
          (append s v))))

  ;; make-indices will compute smallest set of
  ;; indices/tables/subspaces required to bind any pattern in one
  ;; hop. The math behind this computation is explained at:
  ;;
  ;;   https://math.stackexchange.com/q/3146568/23663
  ;;
  ;; make-indices will return the smallest set of permutations in
  ;; lexicographic order of the base index ie. the output of (iota
  ;; n) where n is the length of ITEMS ie. the n in nstore.

  (define (prefix? lst other)
    "Return #t if LST is prefix of OTHER"
    (let loop ((lst lst)
               (other other))
      (if (null? lst)
          #t
          (if (fx=? (car lst) (car other))
              (loop (cdr lst) (cdr other))
              #f))))

  ;; TODO: memoize
  (define (permutation-prefix? c o)
    (any (lambda (p) (prefix? p o)) (permutations c)))

  (define (ok? combinations candidate)
    (every (lambda (c) (any (lambda (p) (permutation-prefix? c p)) candidate)) combinations))

  (define (findij L)
    (let loop3 ((x L)
                (y '()))
      (if (or (null? x) (null? (cdr x)))
          (values #f (append (reverse y) x) #f #f)
          (if (and (not (cdr (list-ref x 0))) (cdr (list-ref x 1)))
              (values #t
                      (append (cddr x) (reverse y))
                      (car (list-ref x 0))
                      (car (list-ref x 1)))
              (loop3 (cdr x) (cons (car x) y))))))

  (define (lex< a b)
    (let loop ((a a)
               (b b))
      (if (null? a)
          #t
          (if (not (= (car a) (car b)))
              (< (car a) (car b))
              (loop (cdr a) (cdr b))))))

  (define (make-indices n)
    ;; This is based on:
    ;;
    ;;   https://math.stackexchange.com/a/3146793/23663
    ;;
    (let* ((tab (iota n))
           (cx (combination (floor (/ n 2)) tab)))
      (let loop1 ((cx cx)
                  (out '()))
        (if (null? cx)
            (list-sort lex< out)
            (let loop2 ((L (map (lambda (i) (cons i (not (not (memv i (car cx)))))) tab))
                        (a '())
                        (b '()))
              (call-with-values (lambda () (findij L))
                (lambda (continue? L i j)
                  (if continue?
                      (loop2 L (cons j a) (cons i b))
                      (loop1 (cdr cx)
                             (cons (append (reverse a) (map car L) (reverse b))
                                   out))))))))))

  (define-record-type <nstore>
    (make-nstore indices n)
    nstore?
    (indices nstore-indices)
    (n nstore-n))

  (define (nstore engine prefix items)
    (make-nstor (make-indices (length items))
                (length items)))

  (define nstore-ask?
    (lambda (db nstore items)
      ;; indices are sorted in lexicographic order, that is the
      ;; first index is always (iota n) (also known as the base
      ;; index). So that there is no need to permute ITEMS.  zero in
      ;; the following `list` is the index of the base subspace in
      ;; nstore-indices
      (not (not (db-ref db (pack (cons 0 items)))))))

  (define (make-tuple list permutation)
    ;; Construct a permutation of LIST based on PERMUTATION
    (let ((tuple (make-vector (length permutation))))
      (for-each (lambda (index value) (vector-set! tuple index value)) permutation list)
      (vector->list tuple)))

  (define (permute items index)
    ;; inverse of `make-tuple`
    (let ((items (list->vector items)))
      (let loop ((index index)
                 (out '()))
        (if (null? index)
            (reverse out)
            (loop (cdr index)
                  (cons (vector-ref items (car index)) out))))))

  (define nstore-add!
    (lambda (db nstore items)
      ;; add ITEMS into the okvs and prefix each of the permutation
      ;; of ITEMS with the nstore-prefix and the index of the
      ;; permutation inside the list INDICES called SUBSPACE.
      (let loop ((indices (nstore-indices nstore))
                 (subspace 0))
        (unless (null? indices)
          (let ((key (pack (cons subspace (permute items (car indices))))))
              (db-set! db key #vu8(0))
              (loop (cdr indices) (+ subspace 1)))))))

  (define nstore-delete!
    (lambda (db nstore items)
      ;; Similar to the above but remove ITEMS
      (let loop ((indices (nstore-indices nstore))
                 (subspace 0))
        (unless (null? indices)
          (let ((key (pack (cons subspace (permute items (car indices))))))
            (db-delete! db key)
            (loop (cdr indices) (+ subspace 1)))))))

  (define-record-type <nstore-var>
    (nstore-var name)
    nstore-var?
    (name nstore-var-name))

  (define (bind* pattern tuple seed)
    ;; Associate variables of PATTERN to value of TUPLE with SEED.
    (let loop ((tuple tuple)
               (pattern pattern)
               (out seed))
      (if (null? tuple)
          out
          (if (nstore-var? (car pattern)) ;; only bind variables
              (loop (cdr tuple)
                    (cdr pattern)
                    (hashmap-set out
                                 (nstore-var-name (car pattern))
                                 (car tuple)))
              (loop (cdr tuple) (cdr pattern) out)))))

  (define (pattern->combination pattern)
    (let loop ((pattern pattern)
               (index 0)
               (out '()))
      (if (null? pattern)
          (reverse out)
          (loop (cdr pattern)
                (+ 1 index)
                (if (nstore-var? (car pattern))
                    out
                    (cons index out))))))

  (define (pattern->index pattern indices)
    ;; Retrieve the index and subspace that will allow to bind
    ;; PATTERN in one hop. This is done by getting all non-variable
    ;; items of PATTERN and looking up the first index that is
    ;; permutation-prefix...
    (let ((combination (pattern->combination pattern)))
      (let loop ((indices indices)
                 (subspace 0))
        (if (null? indices)
            (error 'nstore "oops!")
            (if (permutation-prefix? combination (car indices))
                (values (car indices) subspace)
                (loop (cdr indices) (+ subspace 1)))))))

  (define (pattern->prefix pattern index)
    ;; Return the list that correspond to INDEX, that is the items
    ;; of PATTERN that are not variables. This is used as the prefix
    ;; for the range query done later.
    (let loop ((index index)
               (out '()))
      (let ((v (list-ref pattern (car index))))
        (if (nstore-var? v)
            (reverse out)
            (loop (cdr index) (cons v out))))))

  (define (%from db nstore pattern seed)
    (call-with-values (lambda () (pattern->index pattern (nstore-indices nstore)))
      (lambda (index subspace)
        (let ((prefix (cons subspace pattern->prefix pattern index)))
          (gmap (lambda (pair)
                  (bind* pattern
                         (make-tuple (unpack (car pair)) index)
                         seed))
                (db-query (pack prefix)))))))

  (define comparator (make-eq-comparator))

  (define nstore-from
    (case-lambda
     ((db nstore pattern)
      (%from db nstore pattern (hashmap comparator) '()))
     ((db nstore pattern config)
      (%from db nstore pattern (hashmap comparator) config))))

  (define (pattern-bind pattern seed)
    ;; Return a pattern where variables that have a binding in SEED
    ;; are replaced with the associated value. In practice, most of
    ;; the time, it is the same pattern with less variables.
    (map (lambda (item) (or (and (nstore-var? item)
                                 (hashmap-ref/default seed
                                                      (nstore-var-name item)
                                                      #f))
                            item))
         pattern))

  (define (gconcatenate generator)
    ;; Return a generator that yields the elements of the generators
    ;; produced by the given GENERATOR. Similar to gflatten but
    ;; GENERATOR contains other generators instead of lists.
    (let ((state eof-object))
      (lambda ()
        (let ((value (state)))
          (if (eof-object? value)
              (let loop ((new (generator)))
                (if (eof-object? new)
                    new
                    (let ((value (new)))
                      (if (eof-object? value)
                          (loop (generator))
                          (begin (set! state new)
                                 value)))))
              value)))))

  (define nstore-where
    (lambda (db nstore pattern)
      (lambda (from)
        (gconcatenate
         (gmap (lambda (bindings) (%from db
                                         nstore
                                         (pattern-bind pattern bindings)
                                         bindings
                                         '()))
               from)))))

  (define-syntax nstore-query
    (syntax-rules ()
      ((_ value) value)
      ((_ value f rest ...)
       (nstore-query (f value) rest ...)))))
