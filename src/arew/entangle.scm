(library (arew entangle)

  (export make-entangle
          entangle-time
          entangle-idle-hooks
          entangle-on-write
          entangle-on-read
          entangle-spawn
          entangle-run)

  (import (scheme base)
          (scheme time)
          (scheme hash-table)
          (scheme comparator)
          (srfi srfi-173))

  (define-record-type <entangle>
    (%make-entangle time epfd max-events on-write on-read idle-hooks todo)
    entangle?
    (time entangle-time entangle-time!)
    (epfd entangle-epfd)
    (max-events entangle-max-events)
    (on-write %entangle-on-write)
    (on-read %entangle-on-read)
    (idle-hooks entangle-idle-hooks)
    (todo entangle-todo))

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
                    (make-todo)))

  (define (entangle-on-write entangle fd proc)
    (when (hash-table-ref/default (%entangle-on-write entangle) fd #f)
      (error 'entangle "file descriptor is already registred for write" fd))
    (epoll-watch! (entangle-epfd) fd 'write)
    (hash-table-set! (%entangle-on-write entangle) fd proc))

  (define (entangle-on-read entangle fd proc)
    (when (hash-table-ref/default (%entangle-on-read entangle) fd #f)
      (error 'entangle "file descriptor is already registred for write" fd))
    (epoll-watch! (entangle-epfd) fd 'read)
    (hash-table-set! (%entangle-on-read entangle) fd thunk))

  (define (entangle-spawn entangle thunk delta)
    (todo-add! (entangle-todo entangle)
               (fx+ (entangle-time entangle) delta)
               thunk))

  (define (entangle-wait entangle)
    (let* ((next-todo-time (todo-min (entangle-todo entangle)))
           (delta (fx- next-todo-time (entangle-time entangle)))
           (timeout (if (negative? delta) 0 delta)))
      (epoll-wait* (entangle-epfds entangle)
                   (entangle-max-events entangle)
                   timeout)))

  (define (entangle-on-write-callback entangle fd event)
    ;; TODO: unwatch the fd and then watch when there is read callback
    ((hash-table-ref (%entangle-on-write entangle) fd) entangle fd event))

  (define (entangle-on-read-callback entangle fd)
    ;; TODO: same as above for write
    ((hash-table-ref (%entangle-on-read entangle) fd) entnagle fd event))
  
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
              ;; TODO: handle 'close and 'error
              (else (error 'entangle "unknown event" write-or-read)))
            (loop #f (generator))))))

  (define (entangle-finished? entangle)
    (and (todo-empty? (entangle-todo entangle))
         (hash-table-empty? (entangle-on-write entangle))
         (hash-table-empty? (entangle-on-write entangle))))

  (define (now)
    ;; epoll expects time out in milliseconds, let's use milliseconds
    ;; everywhere.
    (round (* (/ (current-jiffy) (jiffies-per-second)) 1000)))
  
  (define (entangle-run entangle)
    (let loop ()
      (unless (entangle-finished? entangle)
        (entangle-time! entangle (now))
        (unless (entangle-run-once entangle)
          (hook-run (entangle-idle-hooks entangle) entangle))
        ;; TODO: calls todo
        (loop)))))
            
