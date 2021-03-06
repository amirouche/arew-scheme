(import (scheme base))
(import (scheme case-lambda))
(import (scheme fixnum))


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
  (guard (ex (else (cursor-close cursor) (raise ex)))
    (call-with-values (lambda ()
                        (proc cursor))
      (lambda args
        (cusor-close okvs)
        (apply values args)))))

(define (okvs-ref okvs key)
  (call-with-cursor okvs
    (lambda (cursor)
      (okvslite-cursor-seek cursor key 'equal)
      (if (okvslite-cursor-valid? cursor)
          (cursor-value-ref cursor)
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
  (define cursor (okvs-cursor-open okvs))

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
