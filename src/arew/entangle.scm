(library (arew entangle)

  (export make-entangle
          entangle-time
          entangle-idle-hooks
          entangle-on-write
          entangle-on-read
          entangle-spawn
          entangle-run)

  (import (scheme base)
          (scheme hash-table)
          (scheme comparator)
          (srfi srfi-173))

  (define-record-type <entangle>
    (%make-entangle time epfd max-events on-write on-read idle-hooks tasks)
    entangle?
    (time entangle-time)
    (epfd entangle-epfd)
    (max-events entangle-max-events)
    (on-write %entangle-on-write)
    (on-read %entangle-on-read)
    (idle-hooks entangle-idle-hooks)
    (tasks entangle-tasks))

  (define (make-integer-comparator)
    (make-comparator integer? = < number-hash))

  (define (make-hash-table*)
    (make-hash-table (make-integer-comparator)))

  (define (make-entangle max-events)
    (%make-entangle 0
                    (make-epoll)
                    max-events
                    (make-hash-table*)
                    (make-hash-table*)
                    (make-hooks)
                    (make-tasks)))

  (define (entangle-on-write entangle fd thunk)
    (when (hash-table-ref/default (%entangle-on-write entangle) fd #f)
      (error 'entangle "file descriptor is already registred for write" fd))
    (if (hash-table-ref/default (%entangle-on-read entangle) fd)
        (epoll-modify! (entangle-epfd) fd 'write)
        (epoll-add! (entangle-epfd) fd 'write))
    (hash-table-set! (%entangle-on-write entangle) fd thunk))

  (define (entangle-on-read entangle fd thunk)
    (when (hash-table-ref/default (%entangle-on-read entangle) fd #f)
      (error 'entangle "file descriptor is already registred for write" fd))
    (if (hash-table-ref/default (%entangle-on-write entangle) fd)
        (epoll-modify! (entangle-epfd) fd 'read)
        (epoll-add! (entangle-epfd) fd 'read))
    (hash-table-set! (%entangle-on-read entangle) fd thunk))

  (define (entangle-spawn entangle thunk delta)
    (tasks-add (entangle-tasks entangle)
               (fx+ (entangle-time entangle) delta)
               thunk))

  (define (entangle-wait entangle)
    (epoll-wait* (entangle-epfds entangle)
                 (entangle-max-events entangle)
                 (tasks-timeout (entangle-tasks entangle))))

  (define (entangle-on-write-callback entangle fd)
    ((hash-table-ref (%entangle-on-write entangle) fd)))

  (define (entangle-on-read-callback entangle fd)
    ((hash-table-ref (%entangle-on-read entangle) fd)))
  
  (define (entangle-run-once entangle)
    (define generator (entangle-wait entangle))
    (let loop ((idle? #t)
               (object (generator)))
      (if (eof-object? object)
          idle?
          (let ((write-or-read (car object))
                (fd (cdr object)))
            (case write-or-read
              ((write) (entangle-on-write-callback entangle fd))
              ((read) (entangle-on-read-callback entangle fd))
              (else (error 'entangle "unknown event" write-or-read)))
            (loop #f (generator))))))

  (define (entangle-finished? entangle)
    (and (tasks-empty? (entangle-tasks entangle))
         (hash-table-empty? (entangle-on-write entangle))
         (hash-table-empty? (entangle-on-write entangle))))

  (define (entangle-idle-callback entangle)
    (raise 'not-implemented))
  
  (define (entangle-run entangle)
    (let loop ()
      (unless (entangle-finished? entangle)
        (unless (entangle-run-once entangle)
          (entangle-idle-callback entangle))
        (loop))))
            
