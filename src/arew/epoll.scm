#!chezscheme
(library (arew epoll)
  (export make-epoll
          epoll-register-read!
          epoll-register-write!
          epoll-wait-all)

  (import (chezscheme))

  (begin

    (define errno #%$errno)

    (define stdlib (load-shared-object #f))

    (define EPOLLIN #x001)
    (define EPOLLOUT #x004)
    (define EPOLL_CTL_ADD 1)
    (define EPOLL_CTL_DEL 2)

    (define-ftype epoll-data
      (union (ptr void*)
             (fd int)
             (u32 unsigned-32)
             (u64 unsigned-64)))

    (define-ftype epoll-event%
      (struct (events unsigned-32)
              (data epoll-data)))

    (define (epoll-event-fd event index)
      (ftype-ref epoll-event% (data fd) event index))

    (define (epoll-event-type event index)
      (define type (ftype-ref epoll-event% (data events) event index))
      (cond
       ((fx=? type EPOLLIN) 'read)
       ((fx=? type EPOLLOUT) 'write)
       (else (error 'epoll "Unknown event type"))))

    (define (make-epoll-event fd type)
      (define event (make-ftype-pointer epoll-event%
                                        (foreign-alloc
                                         (ftype-sizeof epoll-event%))))
      (ftype-set! epoll-event% (events) fptr type)
      (ftype-set! epoll-event% (data fd) fptr fd)
      event)

    (define (make-epoll-read-event fd)
      (make-epoll-event fd EPOLLIN))

    (define (make-epoll-write-event fd)
      (make-epoll-event fd EPOLLOUT))

    (define epoll-event
      (case-lambda
       ((event) (epoll-event event 0))
       ((events index)
        (cons (epoll-event-type event index)
              (epoll-event-fd event index)))))

    (define make-epoll
      (let ((func (foreign-procedure "epoll_create1" (int) int)))
        (lambda (flags)
          (define fd (func flags))
          (define code (errno)
          (unless (fxzero? code)
            (error 'epoll
                   "Failed to create epoll (epoll_create1)"
                   code))
          fd))))

    (define epoll-ctl
      (let ((func (foreign-procedure "epoll_ctl" (int int int void*) int)))
        (lambda (epoll op fd event)
          (func epoll op fd (ftype-pointer-address event)))))

    (define (epoll-register-read! epoll fd)
      (epoll-ctl epoll EPOLL_CTL_ADD fd (make-epoll-read-event)))

    (define (epoll-register-write! epoll fd)
      (epoll-ctl epoll EPOLL_CTL_ADD fd (make-epoll-write-event)))

    (define (epoll-unregister! epoll fd)
      (epoll-ctl epoll EPOLL_CTL_ADD fd 0))

    (define epoll-wait%
      (let ([func (foreign-procedure "epoll_wait" (int void* int int) int)])
        (lambda (epoll events max-events timeout)
          (func epoll
                (ftype-pointer-address events)
                max-events
                timeout))))

    (define (epoll-wait-all epoll timeout)
      (define events (foreign-alloc
                      (fx* (ftype-sizeof epoll-event%) 1024)))
      (let loop0 ((out '()))
        (let ((count (epoll-wait% epoll events 1024 timeout)))
          (when (fx=? count -1)
            (error 'epoll "epoll-wait failed"))
          (if (fxzero? count)
              (begin
                (foreign-free events)
                out)
              (let loop1 ((index 0)
                          (out out))
                (if (fx=? index count)
                    (loop0 out)
                    (loop1 (fx+ index 1)
                           (cons (epoll-event events index) out))))))))))
