;; foundationdb

;; Copyright © 2019-2020 Amirouche BOUBEKKI <amirouche at hyper dev>

;;; Comment:
;;
;; - 2019/05: initial version
;;
;; - 2020/02: port to arew
;;
(define-library (arew data base foundationdb)

  (export
   fdb-error
   fdb-setup-network
   fdb-run-network
   fdb-stop-network
   fdb-future-cancel
   fdb-future-release-memory
   fdb-future-destroy
   fdb-future-block-until-ready
   fdb-future-ready?
   fdb-future-callback
   fdb-future-set-callback
   fdb-future-get-error
   fdb-future-get-key
   fdb-future-get-cluster
   fdb-future-get-database
   fdb-future-get-value
   fdb-future-get-range
   fdb-create-database
   fdb-database-destroy
   fdb-database-create-transaction
   fdb-transaction-destroy
   fdb-transaction-cancel
   fdb-transaction-get
   fdb-transaction-get-range
   fdb-transaction-set
   fdb-transaction-atomic-op
   fdb-transaction-clear
   fdb-transaction-clear-range
   fdb-transaction-commit
   fdb-transaction-on-error
   )

  (import (chezscheme))

  (begin

    (define-syntax define-syntax-rule
      (syntax-rules ()
        ((define-syntax-rule (keyword args ...) body)
         (define-syntax keyword
           (syntax-rules ()
             ((keyword args ...) body))))))

    (define (bytevector->pointer bv)
      (#%$object-address bv (+ (foreign-sizeof 'void*) 1)))

    (define-syntax-rule (with-lock obj body ...)
      (lock-object obj)
      (call-with-values (lambda () body ...)
        (lambda out
          (unlock-object obj)
          (apply values out))))

    ;; ffi helpers

    (define (make-double-pointer)
      (foreign-alloc 8))

    (define-syntax-rule (dereference  pointer)
      (foreign-ref 'void* pointer 0))

    (define-syntax-rule (ftype->pointer ftype)
      (ftype-pointer-address ftype))

    (define-syntax-rule (pointer->ftype ftype pointer)
      (make-ftype-pointer ftype pointer))

    (define-syntax-rule (foreign-procedure* return ptr args ...)
      (foreign-procedure __collect_safe ptr (args ...) return))

    ;; foundationdb bindings

    (define foundationdb (load-shared-object "/usr/lib/libfdb_c.so"))

    ;;
    ;; foundationdb enums
    ;;

    ;; TODO

    ;;
    ;; foundationdb bindings
    ;;

    (define fdb-error
      (let ((func (foreign-procedure* string "fdb_get_error" int)))
        (lambda (code)
          (func code))))

    (define-syntax-rule (check code)
      (unless (= code 0)
        (raise (cons 'foundationdb code))))

    ;; (define fdb-error-predicate
    ;;   (let ((func (foreign-procedure* int "fdb_error_predicate" ffi:int error)))
    ;;     (lambda (predicate-test code)
    ;;       (= 1 (func predicate-test code)))))

    ;; (define fdb-network-set-option
    ;;   (let ((func (fdb error "fdb_network_set_option" enum POINTER ffi:int)))
    ;;     (lambda (option value length)
    ;;       (check (func option value length)))))

    (define fdb-setup-network
      (let ((func (foreign-procedure* int "fdb_setup_network")))
        (lambda ()
          (check (func)))))

    (define fdb-run-network
      (let ((func (foreign-procedure* int "fdb_run_network")))
        (lambda ()
          (check (func)))))

    (define fdb-stop-network
      (let ((func (foreign-procedure* int "fdb_stop_network")))
        (lambda ()
          (check (func)))))

    ;; (define fdb-add-network-thread-completion-hook
    ;;   (let ((func (foreign-procedure* int "fdb_add_network_thread_completion_hook" void*)))
    ;;     (lambda (thunk)
    ;;       (check (func (ffi:procedure->pointer ffi:void thunk (list ffi:void)))))))


    (define-ftype %keyvalue
      (struct
       (key void*)
       (key-length int)
       (value void*)
       (value-length int)))

    (define fdb-future-cancel
      (let ((func (foreign-procedure* void "fdb_future_cancel" void*)))
        (lambda (future)
          (func future))))

    (define fdb-future-release-memory
      (let ((func (foreign-procedure* void "fdb_future_release_memory" void*)))
        (lambda (future)
          (func future))))

    (define fdb-future-destroy
      (let ((func (foreign-procedure* void "fdb_future_destroy" void*)))
        (lambda (future)
          (func future))))

    (define fdb-future-block-until-ready
      (let ((func (foreign-procedure* int "fdb_future_block_until_ready" void*)))
        (lambda (future)
          (check (func future)))))

    (define fdb-future-ready?
      (let ((func (foreign-procedure* int "fdb_future_is_ready" void*)))
        (lambda (future)
          (= 1 (func future)))))

    (define (fdb-future-callback proc)
      (let ((code (foreign-callable __collect_safe
                                    (lambda (a b) (proc a)) (void* void*) void)))
        (lock-object code)
        (foreign-callable-entry-point code)))

    (define fdb-future-set-callback
      (let ((func (foreign-procedure* int "fdb_future_set_callback" void* void* void*)))
        (lambda (future code)
          (check (func future code 0)))))

    (define fdb-future-get-error
      (let ((func (foreign-procedure* int "fdb_future_get_error" void*)))
        (lambda (future)
          (func future))))

    (define fdb-future-get-key
      (let ((func (foreign-procedure* int "fdb_future_get_key" void* void* int)))
        (lambda (future)
          (let ((key (make-double-pointer))
                (length (make-double-pointer)))
            (check (func future key length))
            (let* ((length* (foreign-ref int length 0))
                   (out (make-bytevector length*)))
              (let loop ((index 0))
                (unless (= index length*)
                  (bytevector-u8-set! out index (foreign-ref unsigned-8 key index))
                  (loop (+ index 1))))
              (foreign-free key)
              (foreign-free length)
              out)))))

    (define fdb-future-get-value
      (let ((func (foreign-procedure* int "fdb_future_get_value" void* void* void* void*)))
        (lambda (future)
          (let ((present (make-double-pointer))
                (value (make-double-pointer))
                (length (make-double-pointer)))
            (check (func future present value length))
            (if (= 0 (foreign-ref integer present 0))
                (begin
                  (foreign-free present)
                  (foreign-free value)
                  (foreign-free length)
                  #f)
                (let* ((length* (foreign-ref int length 0))
                       (out (make-bytevector length*)))
                  (let loop ((index 0))
                    (unless (= index length*)
                      (bytevector-u8-set! out index (foreign-ref unsigned-8 value index))
                      (loop (+ index 1))))
                  (foreign-free present)
                  (foreign-free value)
                  (foreign-free length)
                  out))))))

    (define (key-value->cons pointer)
      (let* ((kv (pointer->ftype %keyvalue pointer))
             (key-length (ftype-ref %keyvalue (key-length) kv))
             (key (make-bytevector key-length))
             (value-length (ftype-ref %keyvalue (value-length) kv))
             (value (make-bytevector value-length)))
        ;; set bytevector key
        (let loop ((index 0))
          (unless (= index key-length)
            (bytevector-u8-set! key (ftype-ref %keyvalue (key) kv index))
            (loop (+ index 1))))
        ;; set bytevector value
        (let loop ((index 0))
          (unless (= index value-length)
            (bytevector-u8-set! value (ftype-ref %keyvalue (value) kv index))
            (loop (+ index 1))))
        (cons key value)))

    (define fdb-future-get-range
      (let ((func (foreign-procedure* "fdb_future_get_keyvalue_array" void* void* void* void*)))
        (lambda (future)
          (let ((out (make-double-pointer))
                (count (make-double-pointer))
                 ;; TODO: support different streaming mode
                (more (make-double-pointer)))
            (check (func future out count more))
            (let ((count* (foreign-ref int count 0))
                  (pointer (dereference out)))
              (let loop ((index (- count 1))
                         (out '()))
                (if (= index -1)
                    out
                    (loop (- index 1)
                          (cons (key-value->cons (foreign-ref void* out index))
                                out)))))))))

    (define fdb-create-database
      (let ((func (foreign-procedure "fdb_create_database" (string void*) int)))
        (lambda (cluster-file)
          (let ((out* (make-double-pointer)))
            (func cluster-file out)
            (dereference out)))))

    (define fdb-database-destroy
      (let ((func (foreign-procedure* void "fdb_database_destroy" void*)))
        (lambda (database)
          (func database))))

    (define fdb-database-create-transaction
      (let ((func (foreign-procedure* int "fdb_database_create_transaction" void* void*)))
        (lambda (database)
          (let ((out (make-double-pointer)))
            (check (func database out))
            (dereference out)))))

    (define fdb-transaction-destroy
      (let ((func (foreign-procedure* void "fdb_transaction_destroy" void*)))
        (lambda (transaction)
          (func transaction))))

    (define fdb-transaction-cancel
      (let ((func (foreign-procedure* void "fdb_transaction_cancel" void*)))
        (lambda (transaction)
          (func transaction))))

    (define fdb-transaction-get
      (let ((func (foreign-procedure* void* "fdb_transaction_get" void* void* int int)))
        (lambda (transaction key snapshot?)
          (with-lock key
            (func transaction
                  (bytevector->pointer key)
                  (bytevector-length key)
                  (if snapshot? 1 0))))))

    (define fdb-transaction-get-range
      ;; https://apple.github.io/foundationdb/api-c.html#c.fdb_transaction_get_range
      (let ((func (foreign-procedure* void*
                                      "fdb_transaction_get_range"
                                      void* ;; tr
                                      void* ;; begin key name
                                      int ;; begin key name length
                                      int ;; begin or equal
                                      int ;; begin offset
                                      void* ;; end key name
                                      int ;; end key name length
                                      int ;; end or equal
                                      int ;; end offset
                                      int ;; limit
                                      int ;; target bytes
                                      int ;; mode
                                      int ;; iteration
                                      int ;; snapshot
                                      int ;; reverse
                                      )))
        (lambda (transaction
                 begin-key
                 begin-or-equal?
                 begin-offset
                 end-key
                 end-or-equal?
                 end-offset
                 limit
                 target-bytes
                 mode
                 iteration
                 snapshot?
                 reverse?)
          (with-lock begin-key
            (with-lock end-key
              (func transaction
                    (bytevector->pointer begin-key)
                    (bytevector-length begin-key)
                    (if begin-or-equal? 1 0)
                    begin-offset
                    (bytevector->pointer end-key)
                    (bytevector-length end-key)
                    (if end-or-equal? 1 0)
                    end-offset
                    limit
                    target-bytes
                    mode
                    iteration
                    (if snapshot? 1 0)
                    (if reverse? 1 0)))))))

    (define fdb-transaction-set
      (let ((func (foreign-procedure* void
                                      "fdb_transaction_set"
                                      void* ;; tr
                                      void* ;; key-name
                                      int ;; key-name length
                                      void* ;; value
                                      int ;; value length
                                      )))
        (lambda (transaction key value)
          (with-lock key
            (with-lock value
              (func transaction
                    (bytevector->pointer key)
                    (bytevector-length key)
                    (bytevector->pointer value)
                    (bytevector-length value)))))))

    (define fdb-transaction-atomic-op
      (let ((func (foreign-procedure* void
                                      "fdb_transaction_atomic_op"
                                      void* ;; tr
                                      void* ;; key-name
                                      int ;; key-name length
                                      void* ;; param
                                      int ;; param length
                                      int ;; operation type
                                      )))
        (lambda (transaction key param operation-type)
          (with-lock key
            (with-lock param
              (func transaction
                    (bytevector->pointer key)
                    (bytevector-length key)
                    (bytevector->pointer param)
                    (bytevector-length param)
                    operation-type))))))

    (define fdb-transaction-clear
      (let ((func (foreign-procedure* void
                                      "fdb_transaction_clear"
                                      void* ;; tr
                                      void* ;; key-name
                                      int ;; key-name length
                                      )))
        (lambda (transaction key)
          (with-lock key
            (func transaction
                  (bytevector->pointer key)
                  (bytevector-length key))))))

    (define fdb-transaction-clear-range
      (let ((func (foreign-procedure* void
                                      "fdb_transaction_clear_range"
                                      void*
                                      void*
                                      int
                                      void*
                                      int)))
        (lambda (transaction begin end)
          (with-lock begin
            (with-lock end
              (func transaction
                    (bytevector->pointer begin)
                    (bytevector-length begin)
                    (bytevector->pointer end)
                    (bytevector-length end)))))))

    (define fdb-transaction-commit
      (let ((func (foreign-procedure* void*
                                      "fdb_transaction_commit"
                                      void*)))
        (lambda (transaction)
          (func transaction))))

    (define fdb-transaction-on-error
      (let ((func (foreign-procedure* void*
                                      "fdb_transaction_on_error"
                                      void*
                                      int)))
        (lambda (transaction error)
          (func transaction error))))))
