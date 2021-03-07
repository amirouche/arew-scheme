#!chezscheme
(import (chezscheme))


(define (pk . args)
  (write args)
  (newline)
  (car (reverse args)))

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((define-syntax-rule (keyword args ...) body)
     (define-syntax keyword
       (syntax-rules ()
         ((keyword args ...) body))))))

(define (bytevector->pointer bv)
  (#%$object-address bv (+ (foreign-sizeof 'void*) 1)))

;; TODO: take a list of objects, use a macro, possibly with a guard
(define (call-with-lock obj thunk)
  (lock-object obj)
  (call-with-values thunk
    (lambda out
      (unlock-object obj)
      (apply values out))))

;; ffi helpers

(define (make-double-pointer)
  ;; TODO: replace with (make-bytevector 8)
  (foreign-alloc 8))

(define-syntax-rule (dereference  pointer)
  (foreign-ref 'void* pointer 0))

(define-syntax-rule (ftype->pointer ftype)
  (ftype-pointer-address ftype))

(define-syntax-rule (foreign-procedure* return ptr args ...)
  (foreign-procedure __collect_safe ptr (args ...) return))

;; sqlite lsm extension bindings

(define okvslite (load-shared-object "./local/lib/lsm.so"))

(define (error->message code)
  (case code
    ((1) "ERROR")
    ((5) "BUSY")
    ((7) "NO MEMORY")
    ((8) "READ ONLY")
    ((10) "IO ERROR")
    ((11) "CORRUPT")
    ((13) "FULL")
    ((14) "CAN NOT OPEN")
    ((15) "PROTOCOL")
    ((21) "MISUSE")
    ((50) "MISMATCH")
    (else "UNKNOWN ERROR")))

(define-syntax-rule (check sym code)
  (let ((code* code))
    (unless (zero? code*)
      (error 'okvslite (error->message code*) code* sym))))

(define okvslite-new
  (let ((proc (foreign-procedure* int "lsm_new" void* void*)))
    (lambda ()
      (let ((out (make-double-pointer)))
        (check 'okvslite-new (proc 0 out))
        (dereference out)))))

(define okvslite-close
  (let ((proc (foreign-procedure* int "lsm_close" void*)))
    (lambda (db)
      (check 'okvslite-close (proc db)))))

;; TODO: replace with symbols
(define okvslite-config
  (let ((proc (foreign-procedure* int "lsm_config" void* int void*)))
    (lambda (db config value)
      (let ((pointer (make-double-pointer)))
        (foreign-set! 'int pointer 0 value)
        (check 'okvslite-config (proc db config pointer))))))

(define okvslite-open
  (let ((proc (foreign-procedure "lsm_open" (void* string) int)))
    (lambda (db filename)
      (check 'okvslite-open (proc db filename)))))

(define okvslite-begin
  (let ((proc (foreign-procedure* int "lsm_begin" void* int)))
    (lambda (db level)
      (check 'okvslite-begin (proc db level)))))

(define okvslite-commit
  (let ((proc (foreign-procedure* int "lsm_commit" void* int)))
    (lambda (db level)
      (check 'okvslite-commit (proc db level)))))

(define okvslite-rollback
  (let ((proc (foreign-procedure* int "lsm_rollback" void* int)))
    (lambda (db level)
      (check 'okvslite-rollback (proc db level)))))

(define okvslite-insert
  (let ((proc (foreign-procedure* int "lsm_insert" void* void* int void* int)))
    (lambda (db key value)
      (call-with-lock key
        (lambda ()
          (call-with-lock value
            (lambda ()
              (check 'okvslite-insert
                     (proc db
                           (bytevector->pointer key)
                           (bytevector-length key)
                           (bytevector->pointer value)
                           (bytevector-length value))))))))))

(define okvslite-delete
  (let ((proc (foreign-procedure* int "lsm_delete" void* void* int)))
    (lambda (db key)
      (call-with-lock key
        (lambda ()
          (check 'okvslite-delete
                 (proc db
                       (bytevector->pointer key)
                       (bytevector-length key))))))))

(define okvslite-cursor-open
  (let ((proc (foreign-procedure* int "lsm_csr_open" void* void*)))
    (lambda (db)
      (let ((out (make-double-pointer)))
        (check 'okvslite-cursor-open
               (proc db out))
        (dereference out)))))

(define okvslite-cursor-close
  (let ((proc (foreign-procedure* int "lsm_csr_close" void*)))
    (lambda (cursor)
      ;; TODO: XXX: maybe free cursor, followup on the above
      ;; make-double-pointer
      (check 'okvslite-cursor-close (proc cursor)))))

(define (->seek symbol)
  (case symbol
    ((less-than-or-equal-fast) -2)
    ((less-than-or-equal) -1)
    ((equal) 0)
    ((greater-than-or-equal) 1)
    (else (error 'okvslite "unknown seek strategy"))))

(define okvslite-cursor-seek
  (let ((proc (foreign-procedure* int "lsm_csr_seek" void* void* int int)))
    (lambda (cursor key strategy)
      (call-with-lock key
        (lambda ()
          (check 'okvslite-cursor-seek
                 (proc cursor
                       (bytevector->pointer key)
                       (bytevector-length key)
                       (->seek strategy))))))))

(define okvslite-cursor-first
  (let ((proc (foreign-procedure* int "lsm_csr_first" void*)))
    (lambda (cursor)
      (check 'okvslite-cursor-first (proc cursor)))))

(define okvslite-cursor-last
  (let ((proc (foreign-procedure* int "lsm_csr_last" void*)))
    (lambda (cursor)
      (check 'okvslite-cursor-last (proc cursor)))))

(define okvslite-cursor-next
  (let ((proc (foreign-procedure* int "lsm_csr_next" void*)))
    (lambda (cursor)
      (check 'okvslite-cursor-next (proc cursor)))))

(define okvslite-cursor-prev
  (let ((proc (foreign-procedure* int "lsm_csr_prev" void*)))
    (lambda (cursor)
      (check 'okvslite-cursor-prev (proc cursor)))))

(define okvslite-cursor-valid?
  (let ((proc (foreign-procedure* int "lsm_csr_valid" void*)))
    (lambda (cursor)
      (= (proc cursor) 1))))

(define okvslite-cursor-key
  (let ((proc (foreign-procedure* int "lsm_csr_key" void* void* void*)))
    (lambda (cursor)
      (let ((data* (make-double-pointer))
            (length* (make-double-pointer)))
        (check 'okvslite-cursor-key (proc cursor data* length*))
        ;; copy the data into a scheme bytevector
        (let* ((data (dereference data*))
               (length (foreign-ref 'int length* 0))
               (bytevector (make-bytevector length)))
          (let loop ((index (- length 1)))
            (unless (< index 0)
              (let ((value (foreign-ref 'unsigned-8 data index)))
                (bytevector-u8-set! bytevector index value)
                (loop (- index 1)))))
          bytevector)))))

(define okvslite-cursor-value
  (let ((proc (foreign-procedure* int "lsm_csr_value" void* void* void*)))
    (lambda (cursor)
      (let ((data* (make-double-pointer))
            (length* (make-double-pointer)))
        (check 'okvslite-cursor-value (proc cursor data* length*))
        ;; copy the data into a scheme bytevector
        (let* ((data (dereference data*))
               (length (foreign-ref 'int length* 0))
               (bytevector (make-bytevector length)))
          (let loop ((index (- length 1)))
            (unless (< index 0)
              (let ((value (foreign-ref 'unsigned-8 data index)))
                (bytevector-u8-set! bytevector index value)
                (loop (- index 1)))))
          bytevector)))))

(define (okvs-open filename)
  (define okvs (okvslite-new))
  (okvslite-open okvs filename)
  okvs)

(define (failure-default)
  (error 'okvs "transaction failed"))

(define okvs-in-transaction
  (case-lambda
   ((okvs proc)
    (okvs-in-transaction okvs proc failure-default values))
   ((okvs proc failure) (okvs-in-transaction okvs proc failure values))
   ((okvs proc failure success) (okvs-in-transaction% okvs proc failure values))))

(define (okvs-in-transaction% okvs proc failure values)
  (let loop ((try 5) ;; magic number
             (ex #f))
    (if (zero? try)
        (raise ex)
        (guard (ex
                (else
                 (okvslite-rollback okvs 0)
                 (loop (fx- try 1) ex)))
          (okvslite-begin okvs 0)
          (call-with-values (lambda () (proc okvs))
            (lambda args
              (okvslite-commit okvs 0)
              (apply values args)))))))

(define (call-with-cursor okvs proc)
  (define cursor (okvslite-cursor-open okvs))
  (guard (ex (else (okvslite-cursor-close cursor) (raise ex)))
    (call-with-values (lambda ()
                        (proc cursor))
      (lambda args
        (okvslite-cursor-close okvs)
        (apply values args)))))

(define (okvs-ref okvs key)
  (call-with-cursor okvs
    (lambda (cursor)
      (okvslite-cursor-seek cursor key 'equal)
      (if (okvslite-cursor-valid? cursor)
          (okvslite-cursor-value cursor)
          #f))))

(define (compare bytevector other)
  ;; lexicographic comparison

  ;; TODO: add a few fixnum calls
  ;; TODO: add a 3-way-if macro

  ;; If BYTEVECTOR is before OTHER return -1, if equal return 0,
  ;; otherwise if BYTEVECTOR is after OTHER return 1
  (let ((end (min (bytevector-length bytevector)
                  (bytevector-length other))))
    (let loop ((index 0))
      (if (zero? (- end index))
          (if (= (bytevector-length bytevector)
                 (bytevector-length other))
              0
              (if (< (bytevector-length bytevector)
                     (bytevector-length other))
                  -1
                  1))
          (let ((delta (- (bytevector-u8-ref bytevector index)
                          (bytevector-u8-ref other index))))
            (if (zero? delta)
                (loop (+ 1 index))
                (if (negative? delta)
                    -1
                    1)))))))

;; TODO: go through the range in other order!!
(define (generator-range okvs key other)
  (define cursor (okvslite-cursor-open okvs))

  (define (fini!) (okvslite-cursor-close cursor) (set! yield eof-object) (eof-object))

  (define (init)
    ;; go through the range in reverse order
    (okvslite-cursor-seek other 'less-than-or-equal)
    (if (okvslite-cursor-valid? cursor)
        (fini!)
        (let* ((key* (okvslite-cursor-key cursor))
               (shift (compare key* other)))
          (when (fx=? shift 0)
            ;; the end of the range is excluded.
            (okvslite-cursor-prev cursor))
          (if (not (okvslite-cursor-valid? cursor))
              (fini!)
              (let* ((key* (okvslite-cursor-key cursor))
                     (shift (compare key* other)))
                (if (fx=? shift 1 key)
                    (fini!)
                    (begin
                      ;; if shift equals zero, continue will run once.
                      (set! yield continue)
                      key)))))))

  (define (continue)
    ;; go through the range in reverse order
    (okvslite-cursor-prev cursor)
    (if (not (okvslite-cursor-valid? cursor))
        (fini!)
        (let* ((key* (okvslite-cursor-key cursor))
               ;; comparison is done against KEY.
               (shift (compare key key*)))
          (cond
           ((fx=? shift 1) key*)
           ((fx=? shift 0) key*)
           ((fx=? shift -1) (fini!))))))

  (define yield init)

  (lambda ()
    (yield)))


(define (generator->list generator)
  (let loop ((out '()))
    (let ((item (generator)))
      (if (eof-object? item)
          (reverse out)
          (loop (cons item out))))))

(define (generator-map proc generator)
  (lambda ()
    (proc (generator))))

(define (okvs-range okvs key other)
  (define (make-key-value key)
    (cons key (okvs-ref okvs key)))

  (generator->list (generator-map make-key-value (generator-range okvs key other))))

(define (okvs-delete! okvs key)
  (okvslite-delete okvs key))

(define (okvs-remove! okvs key other)
  (define (delete! key)
    (okvs-delete! okvs key))

  (generator->list (generator-map delete! (generator-range okvs key other))))


(define (file-char-generator filename)
  (define port (open-input-file filename))

  (define (continue)
    (let ((out (read-char port)))
      (if (eof-object? out)
          (begin (set! yield eof-object) (eof-object))
          out)))

  (define yield continue)

  (lambda ()
    (yield)))


(define (cleanize char)
  (case char
    ((#\# #\. #\( #\[ #\) #\] #\` #\newline) #\space)
    (else char)))

(define (tokenize chars)
  (define token '())

  (define (continue)
    (let loop ((char (chars)))
      (if (eof-object? char)
          (if (null? token)
              (begin
                (set! yield eof-object)
                (eof-object))
              (let ((out (list->string (reverse token))))
                (set! yield eof-object)
                out))
          (if (char=? char #\space)
              (if (null? token)
                  (loop (chars))
                  (let ((out (list->string (reverse token))))
                    (set! token '())
                    out))
              (begin
                (set! token (cons char token))
                (loop (chars)))))))

  (define yield continue)

  (lambda ()
    (yield)))

(define (generator-string-uniquify generator)

  (define set (make-hashtable string-hash string=?))

  (define (continue)
    (let loop ((out (generator)))
      (if (eof-object? out)
          (begin
            (set! yield eof-object)
            (eof-object))
          (let ((generated? (hashtable-ref set out #f)))
            (if generated?
                (loop (generator))
                (begin
                  (hashtable-set! set out #t)
                  out))))))

  (define yield continue)

  (lambda ()
    (yield)))

(define tokens (tokenize (generator-map cleanize (file-char-generator "forms-index.txt"))))

(for-each pk (generator->list (generator-string-uniquify tokens)))
