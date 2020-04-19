(library (arew network epoll)
  (export epoll-create1
          make-epoll-event
          make-epoll-event-in
          make-epoll-event-out
          make-epoll-event-in-out
          epoll-ctl
          epoll-wait
          event-fd)

  (import (chezscheme))

  (begin

    (define stdlib (load-shared-object #f))

    (define EPOLLIN #x001)
    (define EPOLLOUT #x004)

    (define-ftype %epoll-data
      (union (ptr void*)
             (fd int)
             (u32 unsigned-32)
             (u64 unsigned-64)))

    (define-ftype %epoll-event
      (struct (events unsigned-32)
              (data %epoll-data)))

    (define (make-epoll-event)
      ;; TODO: free
      (make-ftype-pointer %epoll-event (foreign-alloc (ftype-sizeof %epoll-event))))

    (define make-epoll-event-in-out
      (lambda (fd)
        (let ([fptr (make-ftype-pointer %epoll-event
                                        (foreign-alloc (ftype-sizeof %epoll-event)))])
          (ftype-set! %epoll-event (events) fptr (logior EPOLLIN EPOLLOUT))
          (ftype-set! %epoll-event (data fd) fptr fd)
          fptr)))

    (define make-epoll-event-in
      (lambda (fd)
        (let ([fptr (make-ftype-pointer %epoll-event
                                        (foreign-alloc (ftype-sizeof %epoll-event)))])
          (ftype-set! %epoll-event (events) fptr EPOLLIN)
          (ftype-set! %epoll-event (data fd) fptr fd)
          fptr)))

    (define make-epoll-event-out
      (lambda (fd)
        (let ([fptr (make-ftype-pointer %epoll-event
                                        (foreign-alloc (ftype-sizeof %epoll-event)))])
          (ftype-set! %epoll-event (events) fptr EPOLLOUT)
          (ftype-set! %epoll-event (data fd) fptr fd)
          fptr)))

    (define (event-fd event)
      (ftype-ref %epoll-event (data fd) event))

    (define epoll-create1
      (let ((func (foreign-procedure "epoll_create1" (int) int)))
        (lambda (flags)
          (func flags))))

    (define epoll-ctl
      (let ((func (foreign-procedure "epoll_ctl" (int int int void*) int)))
        (lambda (epoll op fd event)
          (func epoll op fd (ftype-pointer-address event)))))

    (define epoll-wait
      (let ([func (foreign-procedure "epoll_wait" (int void* int int) int)])
        (lambda (epoll events max-events timeout)
          (func epoll (ftype-pointer-address events) max-events timeout))))

    ))
