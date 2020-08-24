(library (arew data siset)

  (export make-siset siset-size siset-empty? siset-pop! siset-add! siset-contains?)

  (import (scheme base)
          (scheme list)
          (scheme fixnum)
          (srfi srfi-145)
          (scheme comparator))

  (define-record-type <siset>
    (%make-siset comparator vector size)
    siset?
    (comparator siset-comparator)
    (vector siset-vector)
    (size siset-size siset-size!))

  (define (make-siset comparator size . args)
    (let ((siset (%make-siset comparator (make-vector size '()) 0)))
      (let loop ((args args))
        (unless (null? args)
          (siset-add! siset (car args))
          (loop (cdr args))))
      siset))
  
  (define (siset-empty? siset)
    (fx=? (siset-size siset) 0))
  
  (define (siset-pop! siset)
    (define vector (siset-vector siset))

    (when (siset-empty? siset)
      (error 'siset "siset is empty!"))
       
    (siset-size! siset (fx- (siset-size siset) 1))
    
    (let loop ((index (vector-length vector)))
      (let ((bucket (vector-ref vector (fx- index 1))))
        (if (null? bucket)
            (loop (fx- index 1))
            (let* ((out (caar bucket))
                   (rest (cdr bucket)))
              (vector-set! vector (fx- index 1) rest)
              out)))))
  
  (define (siset-add! siset key)
    (define comparator (siset-comparator siset))
    
    (assume (siset? siset))
    (comparator-check-type comparator key)

    (let* ((hash (comparator-hash comparator key))
           (hash2 (modulo hash (vector-length (siset-vector siset))))
           (bucket (vector-ref (siset-vector siset) hash2))
           (obj (find (lambda (v)
                        ((comparator-equality-predicate comparator) key (car v)))
                      bucket)))
      (unless obj
        (siset-size! siset (fx+ (siset-size siset) 1))
        (vector-set! (siset-vector siset) hash2 (cons (cons key #t) bucket)))))

  (define (siset-contains? siset key)
    (define comparator (siset-comparator siset))
    
    (assume (siset? siset))
    (comparator-check-type comparator key)

    (let* ((hash ((comparator-hash-function comparator) key))
           (hash2 (modulo hash (vector-length (siset-vector siset))))
           (bucket (vector-ref (siset-vector siset) hash2)))
      (find (lambda (v)
              ((comparator-equality-predicate comparator) key (car v)))
            bucket))))

