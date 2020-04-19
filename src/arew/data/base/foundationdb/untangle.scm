(define-library (arew data base foundationdb untangle)

  (export make-fdb
          fdb-close
          fdb-in-transaction
          fdb-ref
          fdb-set
          fdb-delete
          fdb-range
          strinc)

  (import (scheme base))
  (import (scheme bytevector))
  (import (arew data base foundationdb))
  (import (arew network untangle))
  (import (only (chezscheme) fork-thread))

  (begin

    (define-record-type <fdb>
      (%make-fdb database)
      fdb?
      (database fdb-database))

    (define-record-type <transaction>
      (make-transaction pointer)
      okvs-transaction?
      (pointer transaction-pointer))

    (define *network-thread* #f)

    (define (make-fdb config)
      ;; setup network thread
      (fdb-setup-network)
      (set! *network-thread* (fork-thread fdb-setup-network))
      ;; create cluster and database
      (%make-fdb (fdb-create-database 0)))

    (define (fdb-close fdb)
      (fdb-database-destroy (fdb-database fdb)))

    (define (fdb-transaction-begin fdb)
      (make-transaction (fdb-database-create-transaction (fdb-database fdb))))

    (define (fdb-transaction-commit-callback future*)
      (fdb-future-callback
       (lambda (future)
         (let ((error (fdb-future-get-error future)))
           (fdb-future-destroy future)
           (future-continue-with-lock future* (list error))))))

    (define (fdb-transaction-commit transaction)
      (let ((future (fdb-transaction-commit (transaction-pointer transaction)))
            (future* (make-future)))
        (fdb-future-set-callback future (fdb-transaction-commit-callback future*))
        (let ((error (await future*)))
          (fdb-transaction-destroy (transaction-pointer transaction))
          (unless (zero? error)
            (raise (cons 'foundationdb error))))))

    (define (fdb-transaction-rollback transaction)
      (fdb-transaction-cancel (transaction-pointer transaction))
      (fdb-transaction-destroy (transaction-pointer transaction)))

    (define (fdb-on-error-callback future*)
      (fdb-future-callback
       (lambda (future)
         (let ((error (fdb-future-get-error future)))
           (fdb-future-destroy future)
           (future-continue-with-lock future* (list error))))))

    (define (fdb-in-transaction fdb proc)
      (let ((tx (fdb-transaction-begin fdb)))
        (let loop ()
          (guard (ex
                  ((and (pair? ex)
                        (eq? (car ex) 'foundationdb))
                   ;; https://apple.github.io/foundationdb/api-c.html#c.fdb_transaction_on_error
                   (let ((future (fdb-transaction-on-error (transaction-pointer tx)
                                                           (cdr ex)))
                         (future* (make-future)))
                     (fdb-future-set-callback future (fdb-on-error-callback future*))
                     (let ((code (await future*)))
                       (if (zero? error)
                           (loop)
                           (begin
                             (fdb-transaction-rollback (transaction-pointer tx))
                             (raise (cons 'foundationdb error)))))))
                  (else (fdb-transaction-rollback tx)
                        (raise ex)))
            (call-with-values (lambda () (proc tx))
              (lambda out
                (okvs-transaction-commit tx)
                (apply values out)))))))

    (define (fdb-ref-callback future*)
      (fdb-future-callback
       (lambda (future)
         (let ((error (fdb-future-get-error future)))
           (if (zero? error)
               (let ((value (fdb-future-get-value future)))
                 (fdb-future-destroy future)
                 (future-continue-with-lock future* (list 0 value)))
               (begin
                 (fdb-future-destroy future)
                 (future-continue-with-lock future* (list error #f))))))))

    (define (fdb-ref tx key)
      (let ((future (fdb-transaction-get (transaction-pointer tx)
                                         key
                                         #f))
            (future* (make-future)))
        (fdb-future-set-callback future (fdb-ref-callback future*))
        (call-with-values (lambda () (await future*))
          (lambda (error value)
            (unless (zero? error)
              (raise (cons 'foundationdb error)))
            value))))

    (define (fdb-set tx key value)
      (fdb-transaction-set (transaction-pointer tx)
                           key
                           value))

    (define (fdb-delete transaction key)
      (fdb-transaction-clear (transaction-pointer transaction)
                             key))

    (define (fdb-range-callback future*)
      (fdb-future-callback
       (lambda (future)
         (let ((error (fdb-future-get-error future)))
           (if (zero? error)
               (let ((range (fdb-future-get-range future)))
                 (fdb-future-destroy future)
                 (future-continue-with-lock future* (list 0 range)))
               (begin
                 (fdb-future-destroy future)
                 (future-continue-with-lock future* (list error #f))))))))

    (define (fdb-range transaction
                       start-key
                       start-include?
                       end-key
                       end-include?
                       limit
                       reverse?)
      (let ((future (fdb-transaction-get-range (transaction-pointer transaction)
                                               start-key
                                               start-include?
                                               0
                                               end-key
                                               end-include?
                                               0
                                               (or limit 0)
                                               0
                                               (if limit 0 -2) ;; EXACT or WANT_ALL
                                               0
                                               #f
                                               reverse?))
            (future* (make-future)))
        (fdb-future-set-callback future (fdb-range-callback future*))
        (call-with-values (lambda () (await future*))
          (lambda (error range)
            (if (zero? error)
                range
                (raise (cons 'foundationdb error)))))))

    (define (strinc bytevector)
      "Return the first bytevector that is not prefix of BYTEVECTOR"
      ;; See https://git.io/fj34F, TODO: OPTIMIZE
      (let ((bytes (reverse (bytevector->u8-list bytevector))))
        ;; strip #xFF
        (let loop ((out bytes))
          (when (null? out)
            (error 'foundationdb
                   "BYTEVECTOR must contain at least one byte not equal to #xFF."
                   bytevector))
          (if (= (car out) #xFF)
              (loop (cdr out))
              (set! bytes out)))
        ;; increment first byte, reverse and return the bytevector
        (u8-list->bytevector (reverse (cons (+ 1 (car bytes)) (cdr bytes))))))
