#!chezscheme
(library (arew untangle)

  (export make-untangle
          make-untangle-channel
          untangle-accept
          untangle-bind
          untangle-channel?
          untangle-channel-recv
          untangle-channel-recv*
          untangle-channel-send
          untangle-closing?
          untangle-listen
          untangle-wrap
          untangle-choice
          untangle-perform
          untangle?
          untangle-sleep
          untangle-status
          untangle-socket-accumulator
          untangle-socket-generator
          untangle-spawn
          untangle-stop
          untangle-time)

  (import (only (chezscheme)
                box unbox set-box! box-cas!
                make-mutex with-mutex
                current-time make-time add-duration time<=?
                make-condition condition-wait condition-signal
                make-thread-parameter)
          (scheme base)
          (srfi srfi-145)
          (arew cffi)
          (arew entangle)
          (arew socket))

  ;; magic number that should be replaced by an ad-hoc value, every
  ;; time it is used.
  (define magic 1024)
  (define magic* 42)

  ;;; box helpers that use box-cas!
  ;;
  ;; TODO: try to box-cas! a magic N number of times and raise an
  ;; exception.
  (define (box-cons! box item)
    ;; Add ITEM to the front of the list that is in BOX
    (unless (box-cas! box lst (cons item (unbox box)))
      (box-cons! box item)))

  (define (box-adjoin! box lst)
    ;; Add the elements of LST of the list that is in BOX without
    ;; preserving the order.
    (let loop ((lst lst))
      (unless (null? lst)
        (box-cons! box (car lst))
        (loop (cdr lst)))))

  (define (box-uncons! box default)
    ;; Remove the first item from the front of the list that is in BOX
    ;; and return it. If the list is empty, return DEFAULT.
    (define lst (unbox box))
    (if (null? lst)
        default
        (let ((item (car lst))
              (tail (cdr lst)))
          (if (box-cas! box lst tail)
              item
              (box-uncons! box default)))))

  (define (unbox-and-swap box new)
    ;; Retrieve the value in the BOX and replace it with NEW.
    (define old (unbox box))
    (if (box-cas! box old new)
        old
        (unbox-and-swap box new)))

  ;;; Cogspace
  ;;
  ;; Cogspace is introduced to avoid many `if` in the code related to
  ;; whether the current POSIX thread is running an untangled
  ;; cooperative event-loop or it is a bare POSIX thread.  Each case
  ;; has two procedures: `pause`, and `resume`. The latter procedure
  ;; is passed to `pause` handler, to be able to resume the
  ;; continuation. In total, that is four procedures that may be
  ;; called by any of the two: green thread inside untangle, or a bare
  ;; thread. This is done like some to be DRY, and hopefully fast.

  (define-record-type <cogspace>
    (make-cogspace pause data)
    cogspace?
    (pause cogspace-pause-ref)
    (data cogspace-data))

  (define (cogspace-pause on-pause)
    (unless (cogspace)
      ;; If there is no registred cogspace, then it is necessarly a
      ;; bare cogspace, because untangle procedures are called by
      ;; untangle-start that setups the appropriate untangle cogspace.
      (cogspace (make-bare-cogspace)))
    ((cogspace-pause-ref (cogspace)) on-pause))

  (define (make-bare-cogspace)
    ;; Also know as bare POSIX thread without untangle
    (define mutex (make-mutex))
    (define condition (make-condition))
    (define thunk)

    (define (pause on-pause)
      (on-pause (lambda (thunk*)
                  (set! thunk thunk*)
                  (condition-signal condition)))
      (with-mutex mutex
        (condition-wait condition mutex))
      (apply values (thunk)))

    (make-cogspace pause #f))

  (define (make-untangle-cogspace untangle)

    (define (pause-with-untangle on-pause)
      (pause untangle
             (lambda (k)
               (on-pause
                (lambda (thunk)
                  (untangle-spawn untangle
                                  (lambda () (apply k (thunk)))))))))

    (make-cogspace pause-with-untangle untangle))

  ;; When the cogspace is first accessed, if the value is #f it is
  ;; setup correctly.
  (define cogspace (make-thread-parameter #f))

  ;;
  ;; XXX: Inside call/pause, escapade-singleton allows to tell how
  ;; call/pause thunk continuation is called:
  ;;
  ;; - nominal case: thunk returned.
  ;;
  ;; - thunk called untangle-escapade, in that case escapade-singleton
  ;; is the first argument of the continuation of thunk inside
  ;; call/pause.
  ;;
  ;; escapade-singleton is a singleton, user code can not create it.
  ;;

  (define escapade-singleton '(escapade-singleton))

  ;; TODO: probably replace untangle with a box that contains the
  ;; escapade.
  (define (call/pause untangle thunk)
    (call-with-values (lambda ()
                        (call/1cc
                         (lambda (escape)
                           (assume (not (untangle-escapade untangle)))
                           (untangle-escapade! untangle escape)
                           (thunk))))
      (lambda args
        ;; TODO: use chibi's match
        ;; args may be the empty list if THUNK returns nothing.
        (if (and (pair? args) (eq? (car args) escapade-singleton))
            ;; XXX: The following code is the escapade handler! That
            ;; is always a proc and a continuation, because of how
            ;; pause is implemented. Racket call/ec has an optional
            ;; argument called handler that allows to change that
            ;; behavior.
            (let ((proc (cadr args))
                  (k (caddr args)))
              ;; call the procedure proc passed to untangle-escapade
              ;; with its continuation called k. That is, k, is what
              ;; follow escapade call inside THUNK. K will allow to
              ;; resume THUNK.
              (proc k))
            (apply values args)))))

  (define (pause untangle proc)
    (assume? untangle)
    ;; XXX: Capture the continuation and call it later, that is why it
    ;; use call/cc instead of call/1cc.
    (call/cc
     (lambda (k)
       ;; save escapade
       (define escapade (untangle-escapade untangle))
       ;; The escapade is a call/1cc continuation, no need to keep it
       ;; around, and it might lead to strange bugs.
       (untangle-escapade! untangle #f)
       ;; XXX: escapade is the continuation of a thunk inside
       ;; call/pause whereas k is the continuation of the caller of
       ;; pause inside thunk.

       ;; XXX: Continue with thunk's continuation inside call/pause as
       ;; known as escapade. Inside call/pause, proc and k are used to
       ;; build the pause handler.
       (escapade escapade-singleton proc k))))

  ;;; Untangle

  (define-record-type <untangle>
    (make-untangle% status escapade time queue entangle)
    untangle?
    (status untangle-status untangle-status!)
    (escapade untangle-escapade untangle-escapade!)
    (time untangle-time untangle-time!)
    (queue untangle-queue untangle-queue!)
    (entangle untangle-entangle))

  (define (untangled)
    (cogspace-data (cogspace)))

  (define (make-untangle)
    (make-untangle% 'init
                    #f
                    0
                    (box '())
                    (make-entangle)))

  (define (untangle-spawn% untangle thunk)
    ;; THUNK will be executed at the next tick. See
    ;; untangle-exec-expired-continuations.
    (define when (untangle-time untangle))
    ;; Since there is no fast lock-free or thread-safe priority queue,
    ;; the code rely on a list of pairs. At least, with Babelia, the
    ;; lock-free or thread-safe priority queue, MIGHT NOT be worth the
    ;; effort. TODO: explain why.
    (define item (cons when thunk))
    (box-cons! (untangle-queue untangle) item))

  (define untangle-spawn
    (case-lambda
     ((thunk) (untangle-spawn% (untangled) thunk))
     ((untangle thunk) (untangle-spawn% untangle thunk))))

  (define (untangle-stop% untangle)
    (assume untangle)
    (untangle-status! untangle 'stopping))

  (define untangle-stop
    (case-lambda
     (() (untangle-stop% (untangled)))
     ((untangle) (untangle-stop% untangle))))

  (define untangle-stopping?
    (case-lambda
     (() (untangle-stopping? (untangled)))
     ((obj)
      (or (eq? obj 'stopping-singleton)
          (and (untangle? obj)
               (eq? (untangle-status untangle) 'stopping))))))

  (define (untangle-start untangle)
    ;; Only one instance of <untangle> can be active per POSIX thread.
    ;; untangle-start can not be called from a green thread, or a
    ;; POSIX thread forked in green thread. POSIX thread be must setup
    ;; before calling untangle-start in the main thread.
    (assume? (not (untangled)))
    (cogspace (make-untangle-cogspace untangle))
    (untangle-status! untangle 'running)
    (let loop ()
      (when (untangle-continue? untangle)
        (untangle-tick untangle)
        (loop)))
    (untangle-status! untangle 'stopped)
    (cogspace #f))

  (define (untangle-continue? untangle)
    (not (untangle-stopping? untangle)))

  (define (untangle-tick untangle)
    (untangle-time! untangle (current-time 'time-monotonic))
    (untangle-exec-expired-continuations untangle)
    (untangle-exec-network-continuations untangle))

  (define (untangle-exec-expired-continuations untangle)

    (define time (untangle-time untangle))

    (define (call-or-keep time+thunk)
      (if (time<=? (car time+thunk) time)
          ;; A green thread wants to wake up!
          (begin
            ;; Call the registred thunk
            (call/pause untangle (cdr time+thunk))
            ;; There is no need to call this thunk in the future,
            ;; because it was already called.
            #f)
          time+thunk))

    (define (filter-map proc lst)
      (let loop ((lst lst)
                 (out '()))
        (if (null? lst)
            out
            (let ((keep? (proc (car lst))))
              (if keep?
                  (loop (cdr lst) (cons (car lst) out))
                  (loop (cdr lst) out))))))

    (define waiting (unbox-and-swap (untangle-queue untangle) '()))
    (define pending (filter-map call-or-keep waiting))

    (box-adjoin! (untangle-queue untangle) pending))

  (define (untangle-exec-network-continuations untangle)
    ;; TODO: wake up at most for the next expiration, possibly 0 since
    ;; the sleep precision is nanoseconds whether epoll is
    ;; milliseconds.
    (entangle-continue (untangle-entangle untangle TODO)))

  ;;; Sleep

  (define (untangle-sleep nanoseconds seconds)

    (define untangle (untangled))

    (define (on-sleep resume)
      ;; RESUME is untangle-sleep continuation.
      (define item (list (cons when resume)))
      (box-cons! (untangle-queue untangle) item))

    ;; untangle-sleep is necessarily called while untangle is running,
    ;; where untangled is true, otherwise user code need to call POSIX
    ;; sleep.
    (define delta (make-time 'time-duration nanoseconds seconds))
    (define when (add-duration (untangle-time untangle) delta))

    ;; XXX: This does not make sense with POSIX threads.
    (cogspace-pause on-sleep))

  ;;; Coop
  ;;
  ;; Coop is short name for cooperation, is what guile-fibers calls
  ;; operation and what CML calls event.

  (define-record-type <untangle-coop-base>
    (make-coop-base wrap try block)
    coop-base?
    (wrap coop-base-wrap)
    (try coop-base-try)
    (block coop-base-block))

  (define-record-type <untangle-coop>
    (make-coop% type data wrap)
    untangle-coop?
    (type coop-type)
    (data coop-data)
    (wrap coop-wrap)
    (perform coop-perform%))

  (define (untangle-wrap coop proc)
    ((coop-wrap coop) coop proc))

  (define (untangle-perform coop)
    ((coop-perform coop) coop))

  (define (make-coop wrap try block)

    (define (coop-wrap coop proc)

      (define (wrapped . args)
        (call-with-values (lambda args (apply wrap args) proc)))

      (make-coop 'base
                 (make-coop-base wrapped try block)
                 coop-wrap
                 coop-try
                 coop-block))

    (define (on-pause resume)
      (block (box 'waiting)
             (lambda (thunk)
               (resume (lambda ()
                         (call-with-values thunk wrap))))))

    (define (coop-perform)
      (let ((maybe-thunk (try)))
        (if (not maybe-thunk)
            (cogspace-pause on-pause))
            (call-with-values maybe-thunk wrap)))

    (make-coop% 'base
                (make-coop-base wrap try block)
                coop-wrap
                coop-perform))

  (define (untangle-choice coops)
    (define (adjoin vector lst)
      (raise 'not-implemented)

    (define (flatten coops)
      (if (null? coops)
          '()
          (if (eq? (coop-type (car coops)) 'base)
              (cons (car coops) (flatten (cdr coops)))
              (adjoin (coop-data (car coops))
                      (flatten (cdr coops))))))

    (define (make-choice bases)

      (define (coop-choice-wrap coop proc)

        (define (vector-map proc vector)
          (define vector* (make-vector (vector-length vector)))
          (let loop ((index 0))
            (if ((fx=? index (vector-length vector)))
                vector* ;; return
                (begin
                  (vector-set! vector* index (proc (vector-ref vector index)))
                  (loop (fx+ index 1))))))

        (make-choice (vector-map (lambda (base) (untangle-coop-wrap base proc))
                                 (coop-data coop))))

      (define (coop-choice-perform)

        (define (on-pause resume)
          ;; state is shared with all base coop of the choice.
          (define state (box 'waiting))
          (vector-for-each (lambda (base)
                             (define block (coop-base-block base))
                             (define wrap (coop-base-wrap base))
                             (block state
                                    (lambda (thunk)
                                      (resume (lambda ()
                                                (call-with-values thunk wrap))))))

                           bases))

        (let* ((count (vector-length coops))
               (offset (random count)))
          (let loop ((index 0))
            (if (fx=? index count)
                (cogspace-pause on-pause))
                (let* ((base (vector-ref bases (modulo (fx+ index offset) count)))
                       (maybe-thunk ((coop-base-try (coop-data base)))))
                  (if (not maybe-thunk)
                      (loop (fx+ index 1))
                      (call-with-values thunk (coop-wrap base)))))))

      (make-coop 'choice
                 bases
                 coop-choice-wrap
                 coop-choice-perform))

    (define bases (flatten coops))

    (when (null? bases)
      (raise 'untangle "coop-choice requires a non empty list"))

    (if (null? (cdr bases))
        (car bases)
        (make-choice (list->vector bases))))

  ;;; Channel

  (define-record-type <untangle-channel>
    (make-untangle-channel% pops puts)
    untangle-channel?
    (pops channel-pops)
    (puts channel-puts))

  (define-record-type <queue>
    (make-queue% data gc-counter)
    queue?
    (data queue-data)
    (gc-counter queue-gc-counter))

  (define (make-queue)
    (make-queue% (box '()) (box magic)))

  (define (untangle-make-channel)
    (make-untangle-channel% (make-queue) (make-queue)))

  (define (untangle-put channel object)

    (define (try)
      (let loop0 ((pops (unbox (channel-pops channel))))
        (if (null? pops)
            ;; No or no more pop coop.
            #f
            (let ((state (caar pops))
                  (resume (cdar pops)))
              (let loop1 ((count magic*))
                (if (fxzero? count)
                    (loop0 (cdr pops)) ;; give up!
                    (if (eq? (unbox state) 'synched)
                        (loop0 (cdr pops))
                        (if (box-cas! state 'waiting 'synched)
                            ;; Success: acquired the state.
                            (begin
                              ;; Resume the thread that was waiting.
                              (resume (lambda () object))
                              ;; Current thread can continue without
                              ;; pausing. The default return value of
                              ;; performing a put is nothing, hence:
                              values)
                            ;; Otherwise, the state is claimed, try
                            ;; again...
                            (loop1 (fx- count 1))))))))))

    (define (block state-put resume-put)
      (define item (cons state-put resume-put))
      (queue-cons! (channel-puts channel) item)
      ;; Since the above call to try, items might have been added
      ;; concurrently to channel-pops by a thread that could not see
      ;; the current item since it was not in channel-puts. Hence, go
      ;; again through all channel-pops *except* items that share the
      ;; same STATE, because of untangle-choice. Otherwise said, a
      ;; choice coop can not be performed by itself

      ;; TODO: check that a choice does not have the same channel with
      ;; untangle-put and untangle-pop, otherwise that will dead-lock.
      (let loop0 ((pops (unbox (channel-pops channel))))
        (unless (null? pops)
          (let ((state-pop (caar pops))
                (resume-pop (cdar pops)))
            (if (eq? state-pop state-put)
                (loop0 (cdr pops))
                (let loop1 ((count magic*))
                  (if (fxzero? count)
                      (loop0 (cdr pops)) ;; give up!
                      ;; TODO: do case after box-cas! because synched
                      ;; and claimed can wait.
                      (case (unbox state-pop)
                        ((synched) (loop0 (cdr pops)))
                        ((claimed) (loop1 (fx- count 1)))
                        ((waiting)
                         ;; Try to rendez-vous, two phase locking?!
                         ;; If current thread can not claim its own
                         ;; state, there is two cases: 1) it was
                         ;; synched, that means the current thread has
                         ;; nothing to do 2) it was claimed, TODO: it
                         ;; means the current thread could retry with
                         ;; loop1... but guile-fibers does not. Note:
                         ;; there is no return value.
                         (when (box-cas! state-put 'waiting 'claimed)
                           (if (box-cas! state-pop 'waiting 'synched)
                               (begin ;; rendez vous!
                                 (set-box! state-put 'synched)
                                 (resume-pop (lambda () object))
                                 (resume-put values))
                               (if (eq? (unbox state-pop) 'claimed)
                                   (begin
                                     (set-box! state-put 'waiting)
                                     (loop1 (fx- count 1)))
                                   (begin
                                     (set-box! state-put 'waiting)
                                     (loop0 (cdr pops))))))

    (make-coop values try block))

  (define (untangle-channel-send channel obj)

    (define resumer (box-uncons! (channel-resumers channel) #f))

    (if resumer
        (resumer obj)
        ;; TODO: That might overflow memory.
        (box-cons! (channel-inbox channel) obj)))

  (define inbox-empty '(inbox-empty))

  (define (untangle-channel-recv untangle channel)

    (define (on-pause resume)
      (define (resumer obj)
        ;; When box-remove! find resumer in channel-resumers, it
        ;; returns true. Otherwise, it means some other POSIX thread
        ;; acquired the resumer and already resumed it.
        (when (box-remove! (channel-resumers channel) resumer)
          ;; Resume the green thread with K in the POSIX thread that
          ;; is receiving and not in the POSIX thread that created
          ;; CHANNEL.
          (if (untangle-stopping? untangle)
              (resume (lambda () stopping-singleton))
              (resume (lambda () obj)))))

      (box-cons! (channel-resumers channel) resumer))

    (define (pause)
      ;; Mind the fact that the escapade is bound to the calling
      ;; <untangle> instance that is UNTANGLE.
      (cogspace-pause on-pause))

    (if (untangle-stopping? untangle)
        stopping-singleton
        (let ((obj (box-uncons! (channel-inbox channel)
                                obj
                                inbox-empty)))
          (if (eq? obj inbox-empty)
              (pause)
              obj))))

  (define (list->generator lst)
    (lambda ()
      (if (null? lst)
          (eof-object)
          (let ((head (car lst)))
            (set! lst (cdr lst))
            head))))

  (define (generator-any? generator)
    (let ((item (generator)))
      (if (eof-object? item)
          #f
          (if item
              #t
              (generator-any? generator)))))

  (define (generator-map proc generator)
    (lambda ()
      (let ((item (generator)))
        (if (eof-object? item)
            item
            (proc item)))))

  (define (maybe-for-each? proc a b)
    ;; similar to for-each, except it stops as soon as PROC returns
    ;; #f, and returns #f. Otherwise, if it finish it returns #t.
    (let loop ((a a)
               (b b))
      (if (null? a)
          #t
          (if (proc (car a) (car b))
              (loop (cdr a) (cdr b))
              #f))))

  (define (channel-stopping? channel)
    (untangle-stopping? (channel-untangle channel)))

  (define (untangle-channel-recv* untangle channels)

    (define (inboxes-pop!)
      (let loop ((channels channels))
        (if (null? channels)
            (values #f #f)
            (let* ((channel (car channels))
                   (out (box-uncons! (channel-inbox channel)
                                     inbox-empty)))
              (if (eq? out inbox-empty)
                  (loop (cdr channels))
                  (values channel out))))))

    (define (make-resumer channel k)

      (define (maybe-remove!? channel resumer)

        (define box (channel-resumers channel))
        (define resumers (unbox-and-swap box '()))

        (define (adjoin! out)
          (channel-resumers! channel
                             (box-adjoin! box out)))

        (let loop ((resumers resumers)
                   (out '()))
          (if (null? resumers)
              #f ;; RESUMER was not found channel-resumers
              (if (eq? (car resumers) resumer)
                  (begin
                    (adjoin! out)
                    #t)
                  (loop (cdr resumers)
                        (cons (car resumers) out))))))

      (define (resumer obj)
        ;; first try to remove all resumers from the pool, to
        ;; implement only-once delivery.
        (with-mutex mutex
          (let ((continue? (apply maybe-for-each?
                                  maybe-remove!?
                                  poll)))
            ;; When maybe-for-each? returns #t, it means all the
            ;; resumers were found in the registred channels in POOL.
            ;; Otherwise, one was not found, hence none is present.
            ;; That is none, because the expression is protected with
            ;; a mutex, hence if the code went through it, it removed
            ;; everything ie. it can not be the partial view of a
            ;; concurrent excecution of the procedure resumer.

            ;; When two POSIX threads, race to send on CHANNELS,
            ;; resume might be called twice, but only one will win,
            ;; because the resumer should only be resumed once.

            ;; At this point the mutex has done its job. The code
            ;; is wrapped inside with-mutex to avoid a set!...
            (when continue?
              ;; continuation is the same whatever the resumer, since
              ;; they represent the same recv call.
              (if (or (untangle-stopping? untangle)
                      (untangle-stopping? (channel-untangle channel)))
                  (untangle-spawn untangle
                                  (lambda () (k channel
                                                stopping-singleton)))
                  (untangle-spawn untangle
                                  (lambda () (k channel obj))))))))

      (set! pool (cons (list channel resumer) pool))

      resumer))

  (define (on-pause k)
    ;; TODO: fail! cogspace needs a cogspace-with-mutex that is nop in
    ;; the case of untangle.
    (with-mutex mutex
      (for-each (lambda (c)
                  (box-cons! (channel-resumers c) (make-resumer c k)))
                channels)))

  (define (pause)
    (pause untangle on-pause))

  (define (stopping?)
    (or (untangle-stopping untangle)
        (generator-any?
         (generator-map channel-stopping?
                        (list->generator channels)))))

  (if (stopping?)
      stopping-singleton
      (call-with-values inboxes-pop!
        (lambda (channel obj)
          (if channel
              (values channel obj)
              (pause)))))

  ;;; sockets

  (define-record-type <socket>
    (make-socket type fd)
    untangle-socket?
    (type socket-type)
    (fd socket-fd))

  (define SOCK_NONBLOCK 2048)

  ;; XXX: benchmark? make it configureable?
  (define backlog magic)

  (define (make-untangle-tcp-server-socket ip port)
    (define domain=inet 2)
    (define type=non-blocking-stream (fxlogior SOCK_NONBLOCK 1))
    (define protocol=ip 0)
    (define fd (check 'untangle
                      (socket-socket domain=inet
                                     type=non-blocking-stream
                                     protocol=ip)))

    (with-foreign-free (make-socket-inet-address ip port)
      (lambda (address)
        ;; where 16 is (ftype-sizeof struct-sockaddr-in)
        (check 'untangle (socket-bind fd address 16))))

    (check 'untangle (socket-listen fd backlog))

    (make-socket 'tcp-server-socket fd))

  (define (untangle-socket-close socket)
    (check 'untangle (socket-close (socket-fd socket))))

  (define EWOULDBLOCK 11)
  (define EAGAIN 11)

  (define (untangle-socket-connections socket)
    ;; XXX: Returns an non-blocking generator of connections.
    (define untangle (untangled))

    (define accepted-fds '())

    (define (on-pause k)
      (entangle-register-read! (untangle-entangle untangle)
                               (socket-fd socket)
                               (lambda () (call/pause untangle k))))


    (define (accept!)
      (assume (null? accepted-fds))
      (pause untangle on-pause)
      (entangle-unregister-read! (untangle-entangle untangle)
                                 (socket-fd socket))

      ;; Accept some connections.
      (let loop ()
        (let ((out (socket-accept4 (socket-fd socket)
                                   0 0 SOCK_NONBLOCK)))
          (if (fx=? out -1)
              (let ((code (errno)))
                ;; If code is EWOULDBLOCK or EAGAIN exit the loop
                (unless (or (fx=? code EWOULDBLOCK)
                            (fx=? code EAGAIN))
                  (error 'untangle (strerror code) code)))
              (set! accepted-fds (cons out accepted-fds))
              ;; Try to accept more file descriptors
              (loop)))))

    (assume (eq? (socket-type socket) 'tcp-server-socket))
    (assume (untangled))

    (lambda ()
      (when (null? accepted-fds)
        (accept!))
      (let ((connection (make-socket 'connection (car accepted-fds))))
        (set! accepted-fds (cdr accepted-fds))
        connection)))

  (define (untangle-socket-generator socket)
    ;; Returns a non-blocking generator of byte

    (define untangle (untangled))

    (define (connection-generator fd)
      ;; TODO: The value magic should be benchmarked somehow.
      (define bytevector (make-bytevector magic))
      (define index -1)
      (define count -1)
      (define untangle (untangled))

      (define (on-pause k)
        (entangle-register-read! (untangle-entangle untangle)
                                 fd
                                 (lambda () (call/pause untangle k))))

      (define (read!)
        (pause untangle on-pause)
        (entangle-unregister-read! (untangle-entangle untangle)
                                   fd)
        (set! count
              (check 'untangle
               (with-lock (list bytevector)
                 (socket-recv fd
                              (bytevector-pointer bytevector)
                              (bytevector-length bytevector)
                              0))))
        (set! index 0))

      (lambda ()
        (when (fx=? index count)
          (read!))
        (let ((byte (bytevector-ref buffer index)))
          (set! index (fx+ index 1))
          byte)))

    (assume untangle)

    (case (socket-type socket)
      ((connection) (connection-generator (socket-fd socket)))
      (else (raise 'not-implemented))))

  (define (untangle-socket-accumulator socket)
    ;; Returns a non-blocking accumulator of bytevector

    (define untangle (untangled))

    (define (connection-accumulator socket)
      (define untangle (untangled))

      (lambda (bytevector)
        (define (on-pause k)
          (entangle-register-write! (untangle-entangle untangle)
                                    fd
                                    (lambda () (call/pause untangle k))))

        (pause untangle on-pause)
        (entangle-unregister-write! (untangle-entangle untangle)
                                    fd)
        (with-lock (list bytevector)
          (let loop ((index 0))
            ;; pointers can be bignum, so use + instead of fx+
            (define pointer (+ (bytevector-pointer bytevector)
                               (fx* index 8)))
            (define length (fx- (bytevector-length bytevector)
                                (fx* index 8)))
            (define count (check 'untangle
                                 (socket-send fd
                                              pointer
                                              length
                                              0)))
            (unless (fx=? count length)
              (loop (fx+ index count)))))))

    (assume untangle)

    (case (socket-type socket)
      ((connection) (connection-accumulator (socket-fd socket)))
      (else (raise 'not-implemented)))))
