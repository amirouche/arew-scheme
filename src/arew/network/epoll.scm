(library (arew network epoll)
  (export make-epoll
          epoll-watch
          epoll-unwatch
          epoll-generator)

  (import (chezscheme))

  (begin

    (define-syntax define-syntax-rule
      (syntax-rules ()
        ((define-syntax-rule (keyword args ...) body)
         (define-syntax keyword
           (syntax-rules ()
             ((keyword args ...) body))))))

    (define ior bitwise-ior)
   
    (define stdlib (load-shared-object #f))

    (define errno
      (let ((entry (foreign-entry "errno")))
        (lambda ()
          (foreign-ref 'int entry 0))))

    (define (ref map code)
      (let loop ((map map))
        (if (null? map)
            (error 'check "unknown code" code)
            (if (fx=? (caar map) code)
                (cdar map)
                (loop (cdr map))))))

    (define-syntax-rule (check map expr)
      (let ((code expr))
        (if (fx=? code -1)
            (values #t code)
            (values #f (ref map (errno))))))

    ;; high level interface

    (define (make-epoll)
      (call-with-values epoll-create
        (lambda (ok? code)
          (unless ok?
            (error 'epoll "make-epoll failed" code)))))

    (define (symbol->epoll symbol)
      (case symbol
        ((write) EPOLLIN)
        ((read) EPOLLOUT)
        (else (error 'epoll "unknown symbol" symbol))))

    (define (epoll-watch! epfd fd read-or-write)
      (let ((code (symbol->epoll read-or-write))
            (event (make-epoll-events 1)))
        (event-fd-set! event fd)
        ;; always watch for close event
        (event-event-set! event (ior code EPOLLRDHUP))
        (call-with-values (lambda () (epoll-ctl! epfd EPOLL_CTL_ADD fd event))
          (lambda (ok? code)
            (unless ok?
              (case code
                ((exist)
                 ;; If the fd is already in the epoll, it means we
                 ;; need to watch for reads and writes.
                 (event-event-set! event (ior EPOLLIN EPOLLOUT EPOLLRDHUP))
                 (call-with-values (lambda ()
                                     (epoll-ctl! epfd EPOLL_CTL_MOD fd event))
                   (lambda (ok? code)
                     (unless ok?
                       (error 'epoll "epoll-watch! failed" code)))))
                (else (error 'epoll "epoll-watch! failed" code))))))))

    (define (epoll-unwatch epfd fd)
      (call-with-values (lambda () (epoll-ctl! epfd EPOLL_CTL_DEL fd 0))
        (lambda (ok? code)
          (unless ok?
            (error 'epoll "epoll-unwatch failed" code)))))

    (define (epoll-wait* epfd max-events timeout)

      (define events (make-epoll-events max-events))
      (define count #f)

      (define (continue/write fd)
        (set! continue event-read)
        (cons 'write fd))

      (define (continue/both fd)
        (set! continue/write fd)
        (cons 'read fd))

      (define (event-read)
        (if (zero? count)
            (begin
              (foreign-free (ftype-pointer-address events))
              (eof-object))
            (begin
              (set! count (fx- count 1))
              (let ((events (epoll-event-events-ref events count))
                    (fd (epoll-event-fd-ref events count)))
                (cond
                 ((not (zero? (ior EPOLLHUP events)))
                  (cons 'error fd))
                 ((not (zero? (ior EPOLLRDHUP events)))
                  (cons 'close fd))
                 (else (let ((in? (ior EPOLLIN events))
                             (out? (ior EPOLLOUT events)))
                         (if (and in? out?)
                             (continue/both fd)
                             (if in?
                                 (cons 'read fd)
                                 (cons 'write fd))))))))))

      (define continue event-read)

      (call-with-values (lambda () (epoll-wait epfd events max-events timeout))
        (lambda (ok? code-or-count)
          (if ok?
              (set! count code-or-count)
              (error 'epoll "epoll-wait failed" code-or-count))))

      (lambda ()
        (continue)))


    ;; low level interface

    (define EPOLL_CTL_ADD 1)
    (define EPOLL_CTL_DEL 2)
    (define EPOLL_CTL_MOD 3)

    ;; available for read
    (define EPOLLIN #x01)
    ;; available for write
    (define EPOLLOUT #x04)
    ;; signals an unexpected close of the socket, i.e. usually an
    ;; internal error
    (define EPOLLHUP #x10)
    ;; detect peer shutdown
    (define EPOLLRDHUP #x2000)

    (define-ftype %epoll-data
      (union (ptr void*)
             (fd int)
             (u32 unsigned-32)
             (u64 unsigned-64)))

    (define-ftype %epoll-event
      (struct (events unsigned-32)
              (data %epoll-data)))

    (define (make-events count)
      (make-ftype-pointer %epoll-event
                          (foreign-alloc (* count (ftype-sizeof %epoll-event)))))

    (define (event-fd-ref events index)
      (ftype-ref %epoll-event (data fd) events index))

    (define (event-event-ref events index)
      (ftype-ref %epoll-event (events) events index))

    (define (event-event-set! event flag)
      (ftype-set! %epoll-event (events) event flag))

    (define (event-fd-set! event fd)
      (ftype-set! %epoll-event (data fd) event fd))

    (define epoll-create-map
      '((22 . invalid-size-or-flag)
        (24 . limit)
        (12 . no-memory)))

    (define epoll-create
      (let ((func (foreign-procedure "epoll_create1" (int) int)))
        (lambda ()
          (check epoll-create-map (func 1)))))

    (define epoll-ctl-map
      '((9 . invalid-epfd)
        (17 . exist)
        (22 . invalid)
        (40 . loop)
        (2 . invalid-operation)
        (12 . no-memory)
        (28 . limit)
        (1 . unsupported)))

    (define epoll-ctl!
      (let ((func (foreign-procedure "epoll_ctl" (int int int void*) int)))
        (lambda (epoll op fd event)
          (check epoll-ctl-map (func epoll op fd (ftype-pointer-address event))))))

    (define epoll-wait-map
      '((77 . invalid-epfd)
        (14 . invalid-memory)
        (4 . interurupted)
        (22 . invalid)))

    (define epoll-wait
      (let ([func (foreign-procedure "epoll_wait" (int void* int int) int)])
        (lambda (epoll events max-events timeout)
          (check epoll-wait-map (func epoll (ftype-pointer-address events) max-events timeout)))))))
