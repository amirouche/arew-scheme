#!chezscheme
(library (arew epoll)
  (export make-epoll
          epoll?
          epoll-fd
          epoll-register-read!
          epoll-unregister-read!
          epoll-register-write!
          epoll-unregister-write!
          epoll-wait-all
          epoll-close!)

  (import (only (chezscheme) load-shared-object)
          (scheme base)
          (prefix (scheme hash-table) scheme-)
          (scheme comparator)
          (srfi srfi-145))

  (define errno #%$errno)

  (define stdlib (load-shared-object #f))

  (define close
    (let ((func (foreign-procedure __collect_safe "close" (int) int)))
      (lambda (fd)
        (define code (func fd))
        (when (fx=? code -1)
          (error 'epoll "close failed" (errno))))))

  (define (epoll-close! epoll)
    (close (epoll-fd epoll)))

  (define-record-type <epoll>
    (make-epoll% fd ks)
    epoll?
    (fd epoll-fd)
    ;; hash-table association events with continuations.
    (ks epoll-ks))

  (define EPOLLIN #x001)
  (define EPOLLOUT #x004)
  (define EPOLL_CTL_ADD 1)
  (define EPOLL_CTL_DEL 2)
  (define EPOLL_CTL_MOD 3)

  (define-ftype epoll-data
    (union (ptr void*)
           (fd int)
           (u32 unsigned-32)
           (u64 unsigned-64)))

  (define-ftype epoll-event
    (struct (events unsigned-32)
            (data epoll-data)))

  (define (epoll-event-fd event index)
    (ftype-ref epoll-event (data fd) event index))

  (define types% (list (cons EPOLLIN 'read)
                       (cons EPOLLOUT 'write)))

  (define (epoll-event-types event index)
    (define type (ftype-ref epoll-event (data events) event index))
    (let loop ((types types%)
               (out '()))
      (if (null? types)
          out
          (if (fx=? (fxand type (caar types)) (caar types))
              (loop (cdr types) (cons (cdar types) out))
              (loop (cdr types) out)))))

  (define (make-epoll-event fd type)
    (define event (make-ftype-pointer epoll-event
                                      (foreign-alloc
                                       (ftype-sizeof epoll-event))))
    (ftype-set! epoll-event (events) fptr type)
    (ftype-set! epoll-event (data fd) fptr fd)
    event)

  (define (make-epoll-read-event fd)
    (make-epoll-event fd EPOLLIN))

  (define (make-epoll-write-event fd)
    (make-epoll-event fd EPOLLOUT))

  (define (make-epoll-read-and-write event fd)
    (make-epoll-event fd (fxor EPOLLIN EPOLLOUT)))

  (define epoll-create1
    (let ((func (foreign-procedure __collect_safe "epoll_create1" (int) int)))
      (lambda (flags)
        (define fd (func flags))
        (define code (errno))
        (unless (fxzero? code)
          (error 'epoll
                 "Failed to create epoll (epoll_create1)"
                 code))
        fd)))

  (define epoll-ctl
    (let ((func (foreign-procedure __collect_safe "epoll_ctl" (int int int void*) int)))
      (lambda (epoll op fd event)
        (func epoll op fd (ftype-pointer-address event)))))

  (define epoll-wait ;; TOOD: look into epoll_pwait
    (let ([func (foreign-procedure __collect_safe "epoll_wait" (int void* int int) int)])
      (lambda (epoll events max-events timeout)
        (func epoll
              (ftype-pointer-address events)
              max-events
              timeout))))

  (define (make-epoll)
    (define comparator (make-comparator integer? =? #f values))
    (make-epoll% (epoll-create1 0)
                 ;; TODO: replace with ad-hoc comparator
                 (scheme-make-hash-table (make-default-comprator))))

  (define (epoll-register-read! epoll fd k)
    (assume
     (not (scheme-hash-table-contains? (epoll-events epoll)
                                       (cons fd 'read))))
    (scheme-hash-table-set! (epoll-events epoll)
                            (cons fd 'read)
                            k)

    (if (scheme-hash-table-contains? (epoll-events epoll)
                                     (cons fd 'write))
        ;; modify existing event
        (epoll-ctl (epoll-fd epoll)
                   EPOLL_CTL_MOD
                   fd
                   (make-epoll-read-and-write-event fd))
        ;; add as new event
        (epoll-ctl (epoll-fd epoll)
                   EPOLL_CTL_ADD
                   fd
                   (make-epoll-read-event fd)))

  (define (epoll-register-write! epoll fd k)
    (assume
     (not (scheme-hash-table-contains? (epoll-events epoll)
                                       (cons fd 'write))))
    (scheme-hash-table-set! (epoll-events epoll)
                            (cons fd 'write)
                            k)
    (if (scheme-hash-table-contains? (epoll-events epoll)
                                     (cons fd 'read))
        ;; modify existing event
        (epoll-ctl (epoll-fd epoll)
                   EPOLL_CTL_MOD
                   fd
                   (make-epoll-read-and-write-event fd))
        ;; add as a new event
        (epoll-ctl (epoll-fd epoll)
                   EPOLL_CTL_ADD
                   fd
                   (make-epoll-write-event fd)))


  (define (epoll-unregister-read! epoll fd)
    (assume (scheme-hash-table-contains? (epoll-events epoll)
                                         (cons fd 'read)))
    (if (scheme-hash-table-contains? (epoll-events epoll)
                                     (cons fd 'write))
        ;; modify existing event
        (epoll-ctl (epoll-fd epoll)
                   EPOLL_CTL_MOD
                   fd
                   (make-epoll-write-event fd))
        ;; delete event
        (epoll-ctl (epoll-fd epoll)
                   EPOLL_CTL_DEL
                   fd
                   0)))


  (define (epoll-unregister-write! epoll fd)
    (assume (scheme-hash-table-contains? (epoll-events epoll)
                                         (cons fd 'read)))
    (if (scheme-hash-table-contains? (epoll-events epoll)
                                     (cons fd 'read))
        ;; modify existing event
        (epoll-ctl (epoll-fd epoll)
                   EPOLL_CTL_MOD
                   fd
                   (make-epoll-read-event fd))
        ;; delete event
        (epoll-ctl (epoll-fd epoll)
                   EPOLL_CTL_DEL
                   fd
                   0)))


  (define (epoll-wait-all epoll timeout)
    (define events
      (foreign-alloc (fx* (ftype-sizeof epoll-event) 1024)))

    (define (call-continuation events count index)
      (define fd (epoll-event-fd events index))
      (let loop ((types (epoll-event-types events index)))
        (if (null? types)
            (let ((index (fx+ index 1)))
              (unless (fx=? index count)
                (call-continuation events count index)))
            (let ((k (ref (epoll-ks epoll) (cons fd (car types)))))
              (k) ;; call continuation
              (loop (cdr types))))))

    (let loop0 ()
      (define count (epoll-wait epoll events 1024 timeout))
      (when (fx=? count -1)
        (error 'epoll "epoll-wait failed"))
      (if (fxzero? count)
          (foreign-free events)
          (begin
            (call-continuation events count 0)
            (loop)))))
