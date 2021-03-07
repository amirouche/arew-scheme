(library (db-check)

  (export check-000
          check-001
          check-002
          check-003
          check-004
          check-005
          check-006
          check-007
          check-008
          check-009
          check-010
          )

  (import (chezscheme)
          (db)
          (check))

  (define (pk . args)
    (write args)
    (newline)
    (car (reverse args)))

  (define mkdtemp
    (foreign-procedure "mkdtemp" (string) string))

  (define (make-temporary-directory prefix)
    (let ((input (string-append prefix "-XXXXXX")))
      (mkdtemp input)))

  (define (call-with-db proc)
    (define directory (make-temporary-directory "/tmp/babelialite-db"))
    (define filename (string-append directory "/db.sqlite"))

    (define db (db-open filename))
    (call-with-values (lambda () (proc db))
      (lambda args
        (db-close db)
        (apply values args))))

  (define check-000
    (check #t (call-with-db (lambda (db) #t))))

  (define check-001
    (check #vu8(42)
           (call-with-db
            (lambda (db)
              (db-set! db #vu8(42) #vu8(42))
              (db-ref db #vu8(42))))))

  (define check-002
    (check #f
           (call-with-db
            (lambda (db)
              (db-ref db #vu8(42))))))

   (define (generator->list generator)
    (let loop ((out '()))
      (let ((item (generator)))
        (if (eof-object? item)
            (reverse out)
            (loop (cons item out))))))

   (define check-003
     (check (list (cons #vu8(20 16) #vu8(2)) (cons #vu8(20 17) #vu8(3)))
            (call-with-db
             (lambda (db)
               ;; given
               (db-set! db #vu8(20 18) #vu8(4))
               (db-set! db #vu8(20 16) #vu8(2))
               (db-set! db #vu8(20 15) #vu8(1))
               (db-set! db #vu8(20 19) #vu8(5))
               (db-set! db #vu8(20 17) #vu8(3))

               ;; then
               (generator->list (db-query db #vu8(20 16) #vu8(20 18)))))))

   (define check-004
     (check (list (cons #vu8(20 16) #vu8(2)) (cons #vu8(20 17 01) #vu8(3)))
            (call-with-db
             (lambda (db)
               (db-set! db #vu8(20 18) #vu8(4))
               (db-set! db #vu8(20 16) #vu8(2))
               (db-set! db #vu8(20 15) #vu8(1))
               (db-set! db #vu8(20 19) #vu8(5))
               ;; #vu8(20 17 01) lexicographically less than #vu8(20 18)
               (db-set! db #vu8(20 17 01) #vu8(3))
               (generator->list
                (db-query db #vu8(20 16) #vu8(20 18)))))))

   (define check-005
     (check
      '((#vu8(01 02) . #vu8(1))
        (#vu8(20 16) . #vu8(2))
        (#vu8(20 16 1) . #vu8(2))
        (#vu8(20 17) . #vu8(3))
        (#vu8(20 17 1) . #vu8(2))
        (#vu8(42 42) . #vu8(5)))
      (call-with-db
       (lambda (db)
         (db-set! db #vu8(20 17 01) #vu8(2))
         (db-set! db #vu8(20 17) #vu8(3))
         (db-set! db #vu8(42 42) #vu8(5))
         (db-set! db #vu8(01 02) #vu8(1))
         (db-set! db #vu8(20 16) #vu8(2))
         (db-set! db #vu8(20 16 01) #vu8(2))
         (generator->list (db-query db #vu8() #vu8(255)))))))

   (define check-006
     (check
      '((#vu8(20 16) . #vu8(2))
        (#vu8(20 16 1) . #vu8(2))
        (#vu8(20 17) . #vu8(3))
        (#vu8(20 17 1) . #vu8(2)))
      (call-with-db
       (lambda (db)
          (db-set! db #vu8(20 17 01) #vu8(2))
          (db-set! db #vu8(20 17) #vu8(3))
          (db-set! db #vu8(42 42) #vu8(5))
          (db-set! db #vu8(01 02) #vu8(1))
          (db-set! db #vu8(20 16) #vu8(2))
          (db-set! db #vu8(20 16 01) #vu8(2))
          (generator->list (db-query db #vu8(20) #vu8(21)))))))

   (define check-007
     (check '((#vu8(20 16 1) . #vu8(2))
              (#vu8(20 17) . #vu8(3)))
            (call-with-db
             (lambda (db)
               (db-set! db #vu8(01 02) #vu8(1))
               (db-set! db #vu8(20 16 01) #vu8(2))
               (db-set! db #vu8(20 16) #vu8(2))
               (db-set! db #vu8(20 17 01) #vu8(2))
               (db-set! db #vu8(20 17) #vu8(3))
               (db-set! db #vu8(42 42) #vu8(5))
               ;; get
               (generator->list (db-query db
                                          #vu8(20)
                                          #vu8(21)
                                          1
                                          2))))))

   (define check-008
     (check '((#vu8(20 17) . #vu8(3))
              (#vu8(20 16 1) . #vu8(2)))
            (call-with-db
             (lambda (db)
               (db-set! db #vu8(01 02) #vu8(1))
               (db-set! db #vu8(20 16 01) #vu8(2))
               (db-set! db #vu8(20 16) #vu8(2))
               (db-set! db #vu8(20 17 01) #vu8(2))
               (db-set! db #vu8(20 17) #vu8(3))
               (db-set! db #vu8(42 42) #vu8(5))
               ;; get
               (generator->list (db-query db
                                          #vu8(21)
                                          #vu8(20)
                                          1
                                          2))))))

   (define check-009
     (check '((#vu8(20 16 01) . #vu8(2))
              (#vu8(20 17 01) . #vu8(2)))
            (call-with-db
             (lambda (db)
               (db-set! db #vu8(20 17 01) #vu8(2))
               (db-set! db #vu8(20 16 01) #vu8(2))
               (generator->list (db-query db
                                          #vu8(20)
                                          #vu8(21)))))))

   (define check-010
     (check '()
            (call-with-db
             (lambda (db)
               (db-set! db #vu8(20 17 01) #vu8(2))
               (db-set! db #vu8(20 16 01) #vu8(2))
               (generator->list (db-query db
                                          #vu8(20)
                                          #vu8(21)
                                          3
                                          #f))))))
   )
