#!chezscheme
(library (arew entangle)
  (export make-entangle
          entangle?
          entangle-empty?
          entangle-epoll
          entangle-register-read!
          entangle-unregister-read!
          entangle-register-write!
          entangle-unregister-write!
          entangle-continue
          entangle-close!)

  (import (only (chezscheme) load-shared-object)
          (scheme base)
          (prefix (scheme hash-table) scheme-)
          (scheme comparator)
          (srfi srfi-145))

  ;; TODO: extract epoll bindings
  ;; TODO: use (arew cffi), use foreign-procedure*

  (define errno #%$errno)

  (define stdlib (load-shared-object #f))

  (define close
    (let ((func (foreign-procedure __collect_safe "close" (int) int)))
      (lambda (fd)
        (define code (func fd))
        (when (fx=? code -1)
          (error 'epoll "close failed" (errno))))))

  (define (entangle-close! entangle)
    (close (entangle-epoll entangle)))

  (define-record-type <entangle>
    (make-entangle% epoll thunks)
    entangle?
    (epoll entangle-epoll)
    ;; hash-table association events with continuations.
    (thunks entangle-thunks))

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
        (define count (func epoll
                            (ftype-pointer-address events)
                            max-events
                            timeout))
        (when (fx=? count -1)
          (error 'epoll "epoll-wait failed"))
        count)))

  (define (make-entangle)
    (define comparator (make-comparator integer? =? #f values))
    (make-entangle% (epoll-create1 0)
                    ;; TODO: replace with ad-hoc comparator
                    (scheme-make-hash-table (make-default-comprator))))

  (define (entangle-empty? entangle)
    (scheme-hash-table-empty? (entangle-thunks entangle)))

  (define (entangle-register-read! entangle fd thunk)
    (assume
     (not (scheme-hash-table-contains? (entangle-thunks entangle)
                                       (cons fd 'read))))
    (scheme-hash-table-set! (entangle-thunks entangle)
                            (cons fd 'read)
                            thunk)

    (if (scheme-hash-table-contains? (entangle-thunks entangle)
                                     (cons fd 'write))
        ;; modify existing event
        (epoll-ctl (entangle-epoll entangle)
                   EPOLL_CTL_MOD
                   fd
                   (make-epoll-read-and-write-event fd))
        ;; add as new event
        (epoll-ctl (entangle-epoll entangle)
                   EPOLL_CTL_ADD
                   fd
                   (make-epoll-read-event fd))))

  (define (entangle-register-write! entangle fd thunk)
    (assume
     (not (scheme-hash-table-contains? (entangle-thunks entangle)
                                       (cons fd 'write))))
    (scheme-hash-table-set! (entangle-thunks entangle)
                            (cons fd 'write)
                            thunk)
    (if (scheme-hash-table-contains? (entangle-thunks entangle)
                                     (cons fd 'read))
        ;; modify existing event
        (epoll-ctl (entangle-epoll entangle)
                   EPOLL_CTL_MOD
                   fd
                   (make-epoll-read-and-write-event fd))
        ;; add as a new event
        (epoll-ctl (entangle-epoll entangle)
                   EPOLL_CTL_ADD
                   fd
                   (make-epoll-write-event fd)))


    (define (entangle-unregister-read! entangle fd)
      (assume (scheme-hash-table-contains? (entangle-thunks entangle)
                                           (cons fd 'read)))
      (if (scheme-hash-table-contains? (entangle-thunks entangle)
                                       (cons fd 'write))
          ;; modify existing event
          (epoll-ctl (entangle-epoll entangle)
                     EPOLL_CTL_MOD
                     fd
                     (make-epoll-write-event fd))
          ;; delete event
          (epoll-ctl (entangle-epoll entangle)
                     EPOLL_CTL_DEL
                     fd
                     0)))


    (define (entangle-unregister-write! entangle fd)
      (assume (scheme-hash-table-contains? (entangle-thunks entangle)
                                           (cons fd 'read)))
      (if (scheme-hash-table-contains? (entangle-thunks entangle)
                                       (cons fd 'read))
          ;; modify existing event
          (epoll-ctl (entangle-epoll entangle)
                     EPOLL_CTL_MOD
                     fd
                     (make-epoll-read-event fd))
          ;; delete event
          (epoll-ctl (entangle-epoll entangle)
                     EPOLL_CTL_DEL
                     fd
                     0)))


    (define (entangle-continue entangle timeout)
      (define maxevents 1024) ;; magic
      (define events
        (foreign-alloc (fx* (ftype-sizeof epoll-event) maxevents)))

      (define (call-continuations events count index)
        (define ref scheme-hash-table-ref)
        (define fd (epoll-event-fd events index))
        (let loop ((types (epoll-event-types events index)))
          (if (null? types)
              (let ((index (fx+ index 1)))
                (unless (fx=? index count)
                  (call-continuations events count index)))
              (let ((thunk (ref (entangle-thunks entangle)
                                (cons fd (car types)))))
                (thunk) ;; Call associated continuation.
                (loop (cdr types))))))

      (let loop0 ()
        (define count (epoll-wait (entangle-epoll entangle)
                                  events
                                  maxevents
                                  timeout))
        (if (fxzero? count)
            (foreign-free events)
            (begin
              (call-continuations events count 0)
              (loop)))))))
