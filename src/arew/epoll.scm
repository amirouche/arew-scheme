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

  (define dup
    (let ((func (foreign-procedure __collect_safe "dup" (int) int)))
      (lambda (fd)
        (define out (func fd))
        (when (fx=? out -1)
          (error 'epoll "dup failed" (errno)))
        out)))

  (define close
    (let ((func (foreign-procedure __collect_safe "close" (int) int)))
      (lambda (fd)
        (define code (func fd))
        (when (fx=? code -1)
          (error 'epoll "close failed" (errno))))))

  (define (epoll-close! epoll)
    (close (epoll-fd epoll)))

  (define-record-type <epoll>
    (make-epoll% fd fds events)
    epoll?
    (fd epoll-fd)
    ;; fds is hash-table that associates internal fd and user fd
    ;; with the desired event type: read or write. This is required
    ;; because Linux epoll can only register read, write, or both
    ;; together, yielding the same event for both reads and writes
    ;; that makes it impossible for user code to sort out whether
    ;; the fd is ready for writes or reads.
    (fds epoll-fds)
    (events epoll-events))

  (define EPOLLIN #x001)
  (define EPOLLOUT #x004)
  (define EPOLL_CTL_ADD 1)
  (define EPOLL_CTL_DEL 2)
  (define EPOLL_CTL_MOD 2)

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

  (define (epoll-event-type event index)
    (define type (ftype-ref epoll-event (data events) event index))
    (cond
     ((fx=? type EPOLLIN) 'read)
     ((fx=? type EPOLLOUT) 'write)
     (else (error 'epoll "Unknown event type"))))

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

  (define epoll-wait
    (let ([func (foreign-procedure __collect_safe "epoll_wait" (int void* int int) int)])
      (lambda (epoll events max-events timeout)
        (func epoll
              (ftype-pointer-address events)
              max-events
              timeout))))

  (define (make-epoll)
    (define comparator (make-comparator integer? =? #f values))

    (make-epoll% (epoll-create1 0)
                 (scheme-make-hash-table comparator)
                 ;; TODO: replace with ad-hoc comparator
                 (scheme-make-hash-table (make-default-comprator))))

  (define (epoll-register-read! epoll fd)
    (assume
     (not (scheme-hash-table-contains? (epoll-events epoll)
                                       (cons fd 'read))))
    (define ifd (dup fd))
    (scheme-hash-table-set! (epoll-events epoll)
                            (cons fd 'read)
                            ifd)
    (scheme-hash-table-set! (epoll-fds epoll)
                            ifd
                            (cons fd 'read))

    (epoll-ctl (epoll-fd epoll)
               EPOLL_CTL_ADD
               ifd
               (make-epoll-read-event ifd)))

  (define (epoll-register-write! epoll fd)
    (assume
     (not (scheme-hash-table-contains? (epoll-events epoll)
                                       (cons fd 'write))))
    (define ifd (dup fd))
    (scheme-hash-table-set! (epoll-events epoll)
                            (cons fd 'write)
                            ifd)
    (scheme-hash-table-set! (epoll-fds epoll)
                            ifd
                            (cons fd 'write))

    (epoll-ctl (epoll-fd epoll)
               EPOLL_CTL_ADD
               ifd
               (make-epoll-write-event ifd)))

  (define (epoll-unregister-read! epoll fd)
    (define ifd (scheme-hash-table-ref (epoll-events epoll)
                                       (cons fd 'read)))
    (scheme-hash-table-delete! (epoll-events epoll)
                               (cons fd 'read))

    (scheme-hash-table-delete! (epoll-fds epoll)
                               ifd)

    (epoll-ctl epoll EPOLL_CTL_DEL ifd 0))

  (define (epoll-unregister-write! epoll fd)
    (define ifd (scheme-hash-table-ref (epoll-events epoll)
                                       (cons fd 'write)))
    (scheme-hash-table-delete! (epoll-events epoll)
                               (cons fd 'write))

    (scheme-hash-table-delete! (epoll-fds epoll)
                               ifd)

    (epoll-ctl epoll EPOLL_CTL_DEL ifd 0))

  (define (epoll-wait-all epoll timeout)
    (define events
      (foreign-alloc (fx* (ftype-sizeof epoll-event) 1024)))
    (let loop0 ((out '()))
      (define count (epoll-wait epoll events 1024 timeout))
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
                (let* ((fd (epoll-event-fd events index))
                       (event (scheme-hash-table-ref (epoll-fds epoll)
                                                     fd)))
                  (loop1 (fx+ index 1)
                         (cons event out)))))))))
