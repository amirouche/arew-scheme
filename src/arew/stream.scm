;;; Copyright © 2018-2019 Amirouche Boubekki <amirouche@hyper.dev>
;;

;;; Comments:
;;
;; - 2018/02/25: Replace the use of 'throw' and 'cons' with 'values'
;;               because it is faster
;;
(library (arew stream)
  (export list->stream
          stream->list
          stream-null
          stream-empty?
          stream-car
          stream-map
          stream-for-each
          stream-filter
          stream-append
          stream-concatenate
          stream-length
          ;; stream-group
          ;; stream-sort
          )
  (import
    (scheme base))

  (define (list->stream lst)
    (let loop ((lst lst))
      (lambda ()
        (if (null? lst)
            (values #f #f)
            (values (car lst) (loop (cdr lst)))))))

  (define (stream->list stream)
    (let loop ((stream stream)
               (out '()))
      (call-with-values stream
        (lambda (value next)
          (if next
              (loop next (cons value out))
              (reverse out))))))

  (define (stream-null)
    (lambda ()
      (values #f #f)))

  (define (stream-empty? stream)
    (call-with-values stream
      (lambda (_ next?)
        (not next?))))

  (define (stream-car stream)
    (call-with-values stream (lambda (item _) item)))

  (define (stream-map proc stream)
    (let loop ((stream stream))
      (lambda ()
        (call-with-values stream
          (lambda (value next)
            (if next
                (values (proc value) (loop next))
                (values #f #f)))))))

  (define (stream-for-each proc stream)
    (let loop ((stream stream))
      (call-with-values stream
        (lambda (value next)
          (when next
            (proc value)
            (loop next))))))

  (define (stream-filter predicate? stream)
    (let loop1 ((stream stream))
      (lambda ()
        (let loop2 ((stream stream))
          (call-with-values stream
            (lambda (value next)
              (if next
                  (if (predicate? value)
                      (values value (loop1 next))
                      (loop2 next))
                  (values #f #f))))))))

  (define (stream-append . streams)
    (cond
     ((null? streams) (lambda () (values #f #f)))
     ;; wanna be fast path for the common case, if there is single
     ;; stream, return the first stream
     ((null? (cdr streams)) (car streams))
     ;; otherwise, unroll each stream...
     (else (let loop1 ((streams streams))
             (if (null? streams)
                 (lambda () (values #f #f))
                 (let loop2 ((stream (car streams)))
                   (call-with-values stream
                     (lambda (value next)
                       (if next
                           (lambda () (values value (loop2 next)))
                           (loop1 (cdr streams)))))))))))


  ;; (define (stream-take count stream)
  ;;   (let loop ((stream stream)
  ;;              (count count))
  ;;     (lambda ()
  ;;       (if (eq? count 0)
  ;;           '()
  ;;           (match (stream)
  ;;             ('() '())
  ;;             ((item . next) (cons item (loop next (1- count)))))))))

  ;; (define (stream-drop count stream)
  ;;   (let loop ((stream stream)
  ;;              (count count))
  ;;     (lambda ()
  ;;       (match (stream)
  ;;         ('() '())
  ;;         ((item . next) (if (eq? count 0)
  ;;                            (cons item (loop next 0))
  ;;                            ((loop next (1- count)))))))))


  ;; (define (stream-paginator count stream)
  ;;   (throw 'stream "not implemented error"))

  (define (stream-length stream)
    (let loop ((stream stream)
               (count 0))
      (call-with-values stream
        (lambda (value next)
          (if next
              (loop next (+ 1 count))
              count)))))

  (define (stream-concatenate stream)
    (let loop1 ((stream stream))
      (call-with-values stream
        (lambda (substream next)
          (if (not next)
              stream-null
              (let loop2 ((substream substream))
                (call-with-values substream
                  (lambda (item subnext)
                    (if (not subnext)
                        (loop1 next)
                        (lambda () (values item (loop2 subnext))))))))))))

  ;; TODO: re-implement when needed
  ;;
  ;; (define (stream-unique stream)
  ;;   (let ((seen '()))  ;; TODO: replace with a hash table or (scheme set)
  ;;     (let loop1 ((stream stream))
  ;;       (lambda ()
  ;;         (let loop2 ((stream stream))
  ;;           (match (stream)
  ;;             ('() '())
  ;;             ((item . next) (if (list-index (cut equal? <> (car item)) seen)
  ;;                                (loop2 next)
  ;;                                (begin (set! seen (cons (car item) seen))
  ;;                                       (cons item (loop1 next)))))))))))

  ;; XXX: This is too complex
  ;;
  ;;   (define (stream-group predicate? proc stream)
  ;;     "Return a new stream of stream values from STREAM. STREAM must be sorted.
  ;; Values from STREAM are grouped according to PROC. The value returned
  ;; by PROC must be comparable with PREDICATE?."
  ;;     (let* ((%stream-next
  ;;             (lambda (stream key)
  ;;               ;; TODO: maybe memoize that procedure, because if the underlying
  ;;               ;; stream is a cursor stream (see cursor->stream) it leads to
  ;;               ;; multiple cursor-key-set + cursor-search which can be expensive.

  ;;               ;; XXX: This only called in the case where the previous stream was
  ;;               ;; not fully consumed ie. next-group-callback is replaced in most
  ;;               ;; cases by a lambda returning a value without computation, see
  ;;               ;; %stream-group procedure.
  ;;               (let loop ((stream stream))
  ;;                 (call-with-values stream
  ;;                   (lambda (value next)
  ;;                     (if next
  ;;                         (if (predicate? (proc value) key)
  ;;                             (loop next)
  ;;                             (lambda () (values value next))) ;; next-group
  ;;                         (lambda () (values #f #f)))))))) ;; end of stream
  ;;            (%stream-group
  ;;              (lambda (stream key)
  ;;                ;; worst case scenario, stream was not consumed, but the user
  ;;                ;; request the next group

  ;;                ;; TODO: use make-paramater instead of set!
  ;;                (let ((next-group-callback (lambda () (%stream-next stream key))))
  ;;                  (values (let loop ((stream stream))
  ;;                            (lambda ()
  ;;                              (call-with-values stream
  ;;                                (lambda (value next)
  ;;                                  (if next
  ;;                                      (if (predicate? (proc value) key)
  ;;                                          ;; save advance stream
  ;;                                          (begin (set! next-group-callback (lambda () (%stream-next next key)))
  ;;                                                 (values value (loop next)))
  ;;                                          (and (set! next-group-callback (lambda () stream)) ;; next group
  ;;                                               (values #f #f))) ;; end of group stream
  ;;                                      (and (set! next-group-callback (lambda () (lambda () (values #f #f)))) ;; end of stream
  ;;                                           (values #f #f))))))) ;; end of group
  ;;                          (lambda () (next-group-callback)))))))

  ;;       (let loop ((stream (lambda () stream)))
  ;;         (lambda ()
  ;;           ;; the whole thing must appear pure, but depending on whether a
  ;;           ;; group is consumed, the next stream code path changes. That's
  ;;           ;; why, the loop's STREAM is wrapped in lambda as a thunk, to
  ;;           ;; allow 'next-group-stream' callback returned by %stream-group,
  ;;           ;; to return the correct/current next-group-callback depending
  ;;           ;; on whether the next group stream was computed or not and
  ;;           ;; compute it if it wasn't computed. TBH I am not sure this is
  ;;           ;; the right level of lambda nesting. It seems like there is too
  ;;           ;; much callback.
  ;;           (call-with-values (stream)
  ;;             (lambda (value next)
  ;;               (if next
  ;;                   (call-with-values (lambda () (%stream-group (stream) (proc value)))
  ;;                     (lambda (group next-group-stream)
  ;;                       (values group (loop next-group-stream))))
  ;;                   (values #f #f))))))))


  ;; (define (hash-increment ht key)
  ;;   (let ((value (hash-ref ht key)))
  ;;     (if (not value)
  ;;         (hash-set! ht key 1)
  ;;         (hash-set! ht key (1+ value)))))

  ;; TODO: re-implement when neeeded, maybe with a bag from (scheme set)
  ;;
  ;; (define (stream-group-count stream)
  ;;   (let ((groups (make-hash-table)))
  ;;     (let loop ((stream stream))
  ;;       (match (stream)
  ;;         ('() (sort (hash-map->list cons groups) (lambda (a b) (> (cdr a) (cdr b)))))
  ;;         ((item . next)
  ;;          (hash-increment groups (car item))
  ;;          (loop next))))))

  ;; TODO: improve preformance with a tree
  ;;
  ;; (define (stream-sort stream less?)
  ;;   (list->stream (sort (stream->list stream) less?)))
  )
