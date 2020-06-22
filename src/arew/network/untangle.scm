#!chezscheme
(define-library (arew network untangle)

  (export untangle
          spawn
          make-future
          await
          future-continue-with-lock
          socket
          accept
          fd->port
          bind
          listen
          close
          socket-generator
          socket-accumulator)

  (import (except (chezscheme) read write define-record-type)
          (only (arew scheme base) pk define-record-type)
          (prefix (arew scheme hash-table) scheme:)
          (arew scheme comparator)
          (arew network epoll)
          (prefix (arew network socket) socket:)
          (arew scheme bytevector))
  ;; inspired from https://stackoverflow.com/a/51777980/140837
  ;;
  ;; single thread, single event-loop
  ;;
  (begin

    (alias bind socket:bind)
    (alias listen socket:listen)
    (alias close socket:close)

    ;; event loop

    (define %mutex (make-mutex))
    (define %prompt #f)
    (define %queue '())
    (define %wouldblock (list 'wouldblock))

    (define EWOULDBLOCK 11)

    (define (call-with-prompt thunk handler)
      (call-with-values (lambda ()
                          (call/1cc
                           (lambda (k)
                             ;; XXX: The continuation K also called
                             ;; %prompt may be called in THUNK during
                             ;; the extent of this lambda.
                             (set! %prompt k)
                             (thunk))))
        (lambda out
          (cond
           ((and (pair? out) (eq? (car out) %wouldblock))
            (apply handler (cdr out)))
           (else (apply values out))))))

    (define (abort-to-prompt . args)
      (call/cc
       (lambda (k)
         ;; XXX: Capture the continuation and call it later, hence
         ;; call/cc instead of call/1cc.
         (let ((prompt %prompt))
           (set! %prompt #f)
           (apply prompt (cons %wouldblock (cons k args)))))))

    (define-record-type <future>
      ;; The <future> is meant to support parallel work.  The primary
      ;; use case is to `make-future` in a green thread of the main
      ;; thread, then use the returned value in a parallel thread to
      ;; resolve the future using `future-continue-with-lock`.  The
      ;; main thread is expected to call `await` on the future to
      ;; pause the current green thread, until a parallel thread
      ;; resolve the future.  In other words, a future shared between
      ;; parallel thread is not meant to be supported.  A future
      ;; shared between green threads might be supported.
      (%make-future mutex continuation values)
      future?
      ;; The mutex is used to atomically wait or atomically continue a
      ;; future.  Without the mutex, and in a context where there is
      ;; multiple POSIX threads on multiple cpu cores, it would be
      ;; possible for a thread to await the future and another thread
      ;; to set the values: the future will leak ie. there will be no
      ;; other chances to resolve the future.  With the mutex, it is
      ;; possible that deterministically serialize the order in which
      ;; `await` and `future-continue` happen and handle all the
      ;; cases.
      (mutex future-mutex)
      (continuation future-continuation future-continuation!)
      (values future-values future-values!))

    (define (make-future)
      (%make-future (make-mutex) #f #f))

    (define (await future)
      ;; It is not clear to me what will happen with the abort, hence
      ;; the mutex is acquired and released explicitly instead of
      ;; using with-mutex.  It is possible that with-mutex does not
      ;; work in this case, because, in the second branch, the mutex
      ;; is released in the procedure exec after abort-to-prompt.
      (mutex-acquire (future-mutex future))
      (if (future-values future)
          (begin
            (mutex-release (future-mutex future))
            ;; future-values was set concurrently, that means that the
            ;; future is resolved no need to abort.
            (apply values (future-values future)))
          (begin
            ;; In that case, the future is still pending, abort.  The
            ;; future is released by the abort handler in the
            ;; procedure exec.
            (abort-to-prompt future 'future))))

    (define (future-continue future values)
      ;; future-continue is not thread-safe, see
      ;; future-continue-with-lock.
      (with-mutex (future-mutex future)
        (if (future-continue future)
            ;; Some code already called `await` on the future ie. they
            ;; are waiting for the values, call the continuation.
            (spawn (lambda () (apply (future-continuation future) values)))
            ;; Concurrently, the values are set before the future is
            ;; awaited.  Set the values to allow, await to return
            ;; immediatly.
            (future-values! future values))))

    (define (future-continue-with-lock future values)
      ;; The same as future-continue, except it is thread-safe, it can
      ;; be called in a parallel thread.
      (with-mutex (future-mutex future)
        (if (future-continue future)
            ;; XXX: Use spawn-with-lock instead of spawn.
            (spawn-with-lock (lambda () (apply (future-continuation future) values)))
            (future-values! future values))))

    (define make-event cons)
    (define event-continuation car)
    (define event-mode cdr)

    (define (exec epoll thunk waiting)
      (call-with-prompt
       thunk
       (lambda (k fd mode)
         (case mode
           ;; If the thunk aborts, then mark FD as waiting output or input.
           ((write) (epoll-ctl epoll 1 fd (make-epoll-event-out fd)))
           ((read) (epoll-ctl epoll 1 fd (make-epoll-event-in fd)))
           ;; Prepare for the future, in that case, FD is a <future>.
           ((future)
            (future-continuation! fd k)
            (mutex-release (future-mutex fd)))
           (else (error 'untangle "mode not supported" mode)))
         (scheme:hash-table-set! waiting fd (make-event k mode)))))

    (define (run-once epoll waiting)
      ;; Execute every callback waiting in queue.
      (let loop ()
        (mutex-acquire %mutex)
        (if (null? %queue)
            (mutex-release %mutex)
            (let ((head (car %queue))
                  (tail (cdr %queue)))
              (set! %queue tail)
              (mutex-release %mutex)
              (exec epoll head waiting)
              (loop))))
      (unless (scheme:hash-table-empty? waiting)
        ;; Wait for ONE event.
        (let* ((event (make-epoll-event))
               (count (epoll-wait epoll event 1 -1))
               (fd (event-fd event))
               (event (scheme:hash-table-ref waiting fd)))
          (scheme:hash-table-delete! waiting fd)
          (case (event-mode event)
            ((write) (epoll-ctl epoll 2 fd (make-epoll-event-out fd)))
            ((read) (epoll-ctl epoll 2 fd (make-epoll-event-in fd))))
          (spawn (event-continuation event)))))

    (define (spawn thunk)
      (set! %queue (cons thunk %queue)))

    (define (spawn-with-lock thunk)
      (with-mutex %mutex
        (spawn thunk)))

    (define integer-comparator
      (make-comparator integer? = < number-hash))

    (define (untangle thunk)
      (let ((epoll (epoll-create1 0))
            (waiting (scheme:make-hash-table integer-comparator)))
        (spawn thunk)
        (let loop ()
          (unless (and (null? %queue) (scheme:hash-table-empty? waiting))
            (run-once epoll waiting)
            (loop)))))

    ;; async sockets

    (define (socket domain type protocol)
      (let ((fd (socket:socket domain type protocol)))
        ;; TODO: logior 2048 with existing flags
        (socket:fcntl! fd 4 2048) ;; SOCK_NONBLOCK
        fd))

    (define (accept fd)
      (let ((out (socket:%accept fd 0 0)))
        (if (= out -1)
            (let ((code (socket:errno)))
              (if (= code EWOULDBLOCK)
                  (begin
                    (abort-to-prompt fd 'read)
                    (accept fd))
                  (error 'accept (socket:strerror code))))
            out)))

    (define (read fd bv start n)
      (lock-object bv)
      (let ((pointer (#%$object-address bv (+ (foreign-sizeof 'ptr) 1 start))))
        (let loop ()
          (let ((out (socket:%recv fd pointer n 0)))
            (if (= out -1)
                (let ((code (socket:errno)))
                  (if (= code EWOULDBLOCK)
                      (begin
                        (abort-to-prompt fd 'read)
                        (loop))
                      (error 'socket (socket:strerror code))))
                (begin
                  (unlock-object bv)
                  out))))))

    (define (socket-generator fd)
      ;; XXX: This is a generator, hence it is stateful.  It will keep
      ;; trying to read until an error is raised.
      (let* ((bv (make-bytevector 1024))
             (index 0)
             (count (read fd bv 0 1024)))
        (lambda ()
          (if (= index count)
              ;; That is the end of the previous bytevector, read
              ;; something and return the first byte.
              (let* ((bv* (make-bytevector 1024))
                     (count* (read fd bv 0 1024)))
                (set! bv bv*)
                (set! index 1)
                (set! count count*)
                (bytevector-u8-ref bv 0))
              ;; The bytevector is not finished, increment and return
              ;; a byte.
              (let ((byte (bytevector-u8-ref bv index)))
                (set! index (+ index 1))
                byte)))))

    (define (write fd bv start n)
      (lock-object bv)
      (let ((pointer (#%$object-address bv (+ (foreign-sizeof 'ptr) 1 start))))
        (let loop ()
          (let ((out (socket:%send fd pointer n 0)))
            (if (= out -1)
                (let ((code (socket:errno)))
                  (if (= code EWOULDBLOCK)
                      (begin
                        (abort-to-prompt fd 'read)
                        (loop))
                      (error 'socket (socket:strerror code))))
                (begin
                  (unlock-object bv)
                  out))))))

    (define (socket-accumulator fd)
      (lambda (bv)
        (write fd bv 0 (bytevector-length bv))))

    (define (fd->port fd)
      ;; TODO: logior 2048 with existing flags
      (socket:fcntl! fd 4 2048) ;; nonblocking
      (make-custom-binary-input/output-port
       (format "socket ~a" fd)
       (lambda (bv start n) (read fd bv start n))
       (lambda (bv start n) (write fd bv start n))
       #f ;; get-position
       #f ;; set-position
       (lambda ()
         (socket:close fd))))

    ))
