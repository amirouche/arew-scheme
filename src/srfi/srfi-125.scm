;;; Copyright 2015 William D Clinger.
;;; Copyright 2019 Amirouche Boubekki.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in
;;; full.
;;;
;;; I also request that you send me a copy of any improvements that
;;; you make to this software so that they may be incorporated within
;;; it to the benefit of the Scheme community.
;;;
(library (srfi srfi-125)

  (export
   make-hash-table
   hash-table
   hash-table-unfold
   alist->hash-table

   hash-table?
   hash-table-contains?
   hash-table-empty?
   hash-table=?
   hash-table-mutable?

   hash-table-ref
   hash-table-ref/default

   hash-table-set!
   hash-table-delete!
   hash-table-intern!
   hash-table-update!
   hash-table-update!/default
   hash-table-pop!
   hash-table-clear!

   hash-table-size
   hash-table-keys
   hash-table-values
   hash-table-entries
   hash-table-find
   hash-table-count

   hash-table-map
   hash-table-for-each
   hash-table-map!
   hash-table-map->list
   hash-table-fold
   hash-table-prune!

   hash-table-copy
   hash-table-empty-copy
   hash-table->alist

   hash-table-union!
   hash-table-intersection!
   hash-table-difference!
   hash-table-xor!)

  (import (scheme base)
          (scheme case-lambda)
          (scheme comparator)
          (prefix (srfi srfi-69) s69:))


  (begin

    ;; A unique (in the sense of eq?) value that will never be found
    ;; within a hash-table.

    (define %not-found (list '%not-found))

    ;; A unique (in the sense of eq?) value that escapes only as an irritant
    ;; when a hash-table key is not found.

    (define %not-found-irritant (list 'not-found))

    ;; The error message used when a hash-table key is not found.

    (define %not-found-message "hash-table key not found")

    (define (make-hash-table comparator . args)
      (let ((hash (comparator-hash-function comparator))
            (eqv? (comparator-equality-predicate comparator)))
        (s69:make-hash-table eqv? hash)))

    (define (hash-table comparator . rest)
      (let ((ht (make-hash-table comparator)))
        (let loop ((kvs rest))
          (cond
           ((null? kvs) #f)
           ((null? (cdr kvs)) (error "hash-table: wrong number of arguments"))
           ((hash-table-contains? ht (car kvs))
            (error "hash-table: two equivalent keys were provided" (car kvs)))
           (else (hash-table-set! ht (car kvs) (cadr kvs))
                 (loop (cddr kvs)))))
        ht))

    (define (hash-table-unfold stop? mapper successor seed comparator . rest)
      (let ((ht (apply make-hash-table comparator rest)))
        (let loop ((seed seed))
          (if (stop? seed)
              ht
              (call-with-values
                  (lambda () (mapper seed))
                (lambda (key val)
                  (hash-table-set! ht key val)
                  (loop (successor seed))))))))

    (define (alist->hash-table alist comparator . rest)
      (let ((ht (apply make-hash-table comparator rest))
            (entries (reverse alist)))
        (for-each (lambda (entry) (hash-table-set! ht (car entry) (cdr entry)))
                  entries)
        ht))

    ;; Predicates.

    (define hash-table? s69:hash-table?)

    (define (hash-table-contains? ht key)
      (s69:hash-table-exists? ht key))

    (define (hash-table-empty? ht)
      (= 0 (hash-table-size ht)))

    (define (hash-table=? value-comparator ht1 ht2)
      ;; FIXME: walks both hash tables because their key comparators might
      ;; be different
      (let ((val=? (comparator-equality-predicate value-comparator))
            (n1 (hash-table-size ht1))
            (n2 (hash-table-size ht2)))
        (and (= n1 n2)
             (call/cc
              (lambda (return)
                (hash-table-for-each (lambda (key val1)
                                       (or (and (hash-table-contains? ht2 key)
                                                (val=? val1
                                                       (hash-table-ref ht2 key 'ignored)))
                                           (return #f)))
                                     ht1)
                (hash-table-for-each (lambda (key val2)
                                       (or (and (hash-table-contains? ht1 key)
                                                (val=? val2
                                                       (hash-table-ref ht1 key 'ignored)))
                                           (return #f)))
                                     ht2)
                (return #t))))))

    (define (hash-table-mutable? ht)
      #t)

    (define (%hash-table-ref ht key failure success)
      (let ((val (hash-table-ref ht key %not-found)))
        (cond ((eq? val %not-found)
               (if (and failure (procedure? failure))
                   (failure)
                   (error %not-found-message ht key %not-found-irritant)))
              (success
               (success val))
              (else
               val))))

    (define (ref-on-failure ht key)
      (error %not-found-message ht key %not-found-irritant))

    (define hash-table-ref
      (case-lambda
        ((ht key) (hash-table-ref ht key (lambda () (ref-on-failure ht key)) values))
        ((ht key failure) (hash-table-ref ht key failure values))
        ((ht key failure success)
         (success (s69:hash-table-ref ht key failure)))))

    (define (hash-table-ref/default ht key default)
      (hash-table-ref ht key (const default)))

    (define (hash-table-set! ht key value . rest)
      (s69:hash-table-set! ht key value)
      (unless (null? rest)
        (let loop ((kvs rest))
          (cond
           ((and (not (null? kvs))
                 (not (null? (cdr kvs))))
            (hash-table-set! ht (car kvs) (cadr kvs))
            (loop (cddr kvs)))
           ((not (null? kvs))
            (error "hash-table-set!: wrong number of arguments"
                   (cons ht rest)))))))

    (define (hash-table-delete! ht . keys)
      (let loop ((keys keys)
                 (cnt 0))
        (cond ((null? keys) cnt)
              ((hash-table-contains? ht (car keys))
               (s69:hash-table-delete! ht (car keys))
               (loop (cdr keys) (+ cnt 1)))
              (else
               (loop (cdr keys) cnt)))))

    (define (hash-table-intern! ht key failure)
      (if (hash-table-contains? ht key)
          (hash-table-ref ht key)
          (let ((val (failure)))
            (hash-table-set! ht key val)
            val)))

    (define (hash-table-update! ht key updater . rest)
      (hash-table-set! ht
                       key
                       (updater (apply hash-table-ref ht key rest))))

    (define (hash-table-update!/default ht key updater default)
      (hash-table-set! ht key (updater (hash-table-ref/default ht key default))))

    (define (hash-table-pop! ht)
      (call/cc
       (lambda (return)
         (hash-table-for-each
          (lambda (key value)

            (hash-table-delete! ht key)
            (return key value))
          ht)
         (error "hash-table-pop!: hash table is empty" ht))))

    (define (hash-table-clear! ht)
      (for-each (lambda (key) (hash-table-delete! ht key)) (hash-table-keys ht)))

    ;; The whole hash table.

    (define hash-table-size s69:hash-table-size)

    (define hash-table-keys s69:hash-table-keys)

    (define hash-table-values s69:hash-table-values)

    (define (hash-table-entries ht)
      (apply values
             (hash-table-fold (lambda (key value seed) (list (cons key (car seed))
                                                             (cons value (cadr seed))))
                              '(() ())
                              ht)))

    (define (hash-table-find proc ht failure)
      (call-with-values (lambda () (hash-table-entries ht))
        (lambda (keys vals)
          (let loop ((keys keys)
                     (vals vals))
            (if (null? keys)
                (failure)
                (let* ((key (car keys))
                       (val (car vals))
                       (x   (proc key val)))
                  (or x
                      (loop (cdr keys)
                            (cdr vals)))))))))

    (define (hash-table-count pred ht)
      (call-with-values (lambda () (hash-table-entries ht))
        (lambda (keys vals)
          (let loop ((keys keys)
                     (vals vals)
                     (n 0))
            (if (null? keys)
                n
                (let* ((key (car keys))
                       (val (car vals))
                       (x   (pred key val)))
                  (loop (cdr keys)
                        (cdr vals)
                        (if x (+ n 1) n))))))))

    ;; Mapping and folding.

    (define (hash-table-map proc comparator ht)
      (let ((result (make-hash-table comparator)))
        (hash-table-for-each
         (lambda (key val)
           (hash-table-set! result key (proc val)))
         ht)
        result))

    (define (hash-table-map->list proc ht)
      (call-with-values (lambda () (hash-table-entries ht))
        (lambda (keys vals)
          (map proc keys vals))))


    (define (hash-table-for-each proc ht)
      ;; XXX: With this particular implementation, the proc can safely
      ;; mutate ht.  That property is not guaranteed by the specification,
      ;; but can be relied upon by procedures defined in this file.
      (call-with-values (lambda () (hash-table-entries ht))
        (lambda (keys vals)
          (for-each proc keys vals))))

    (define (hash-table-map! proc ht)
      (hash-table-for-each (lambda (key val)
                             (hash-table-set! ht key (proc key val)))
                           ht))

    (define (hash-table-fold proc seed ht)
      (s69:hash-table-fold ht proc seed))

    (define (hash-table-prune! proc ht)
      (hash-table-for-each (lambda (key val)
                             (if (proc key val)
                                 (hash-table-delete! ht key)))
                           ht))

    ;; Copying and conversion.

    (define (hash-table-copy ht . rest)
      (s69:hash-table-copy ht))

    (define (hash-table-empty-copy ht)
      (let* ((ht2 (hash-table-copy ht #t))
             (ignored (hash-table-clear! ht2)))
        ht2))

    (define (hash-table->alist ht)
      (call-with-values
          (lambda () (hash-table-entries ht))
        (lambda (keys vals)
          (map cons keys vals))))

    ;; Hash tables as sets.

    (define (hash-table-union! ht1 ht2)
      (hash-table-for-each
       (lambda (key2 val2)
         (if (not (hash-table-contains? ht1 key2))
             (hash-table-set! ht1 key2 val2)))
       ht2)
      ht1)

    (define (hash-table-intersection! ht1 ht2)
      (hash-table-for-each
       (lambda (key1 val1)
         (if (not (hash-table-contains? ht2 key1))
             (hash-table-delete! ht1 key1)))
       ht1)
      ht1)

    (define (hash-table-difference! ht1 ht2)
      (hash-table-for-each
       (lambda (key1 val1)
         (if (hash-table-contains? ht2 key1)
             (hash-table-delete! ht1 key1)))
       ht1)
      ht1)

    (define (hash-table-xor! ht1 ht2)
      (hash-table-for-each
       (lambda (key2 val2)
         (if (hash-table-contains? ht1 key2)
             (hash-table-delete! ht1 key2)
             (hash-table-set! ht1 key2 val2)))
       ht2)
      ht1)))
