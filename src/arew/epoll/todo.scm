(library (arew epoll todo)

  (export make-todo
          todo-empty?
          todo-add!
          todo-min)

  (import (scheme base)
          (scheme fixnum)
          (scheme comparator)
          (scheme mapping))

  ;; TODO: replace with a mutable priority queue

  ;; TODO: add a box
  
  (define (item? object)
    (and (integer? (car object))
         (procedure? (cdr object))))
  
  (define (item<? a b)
    (or (fx<? (car a) (car b))
        (fx<? (equal-hash (cdr a))
              (equal-hash (cdr b)))))
  
  (define (make-item-comparator)
    (make-comparator item? equal? item<? equal-hash))
  
  (define (make-todo)
    (mapping (make-item-comparator)))

  (define todo-empty mapping-empty?)

  (define (todo-add! todo integer proc)
    (mapping-set! todo (cons integer proc) #t))

  (define (todo-min todo)
    (raise 'not-implemented))
  
  (define (todo-pop! todo)
    (raise 'not-implemented)))
