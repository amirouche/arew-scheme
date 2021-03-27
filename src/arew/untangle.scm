#!chezscheme
(library (arew untangle)

  (export make-untangle
          make-untangle-channel
          untangle-accept
          untangle-bind
          untangle-channel?
          untangle-channel-recv
          untangle-channel-select
          untangle-channel-send
          untangle-closing?
          untangle-listen
          untangle?
          untangle-sleep
          untangle-status
          untangle-socket-accumulator
          untangle-socket-generator
          untangle-spawn
          untangle-stop
          untangle-time)

  (import (only (chezscheme)
                make-mutex with-mutex
                current-time make-time add-duration time<=?
                make-condition condition-wait condition-signal)
          (scheme base)
          (srfi srfi-145)
          (arew entangle)
          (arew socket))

  ;;
  ;; XXX: Inside untangle-call/ec, escapade-singleton allows to tell
  ;; how untangle-call/ec thunk continuation is called:
  ;;
  ;; - nominal case: thunk returned.
  ;;
  ;; - thunk called untangle-escapade, in that case escapade-singleton
  ;; is the first argument of the continuation of thunk inside
  ;; untangle-call/ec.
  ;;
  ;; escapade-singleton is a singleton, user code can not create it.
  ;;

  (define escapade-singleton '(escapade-singleton))

  ;; call/ec = call-with-escape-continuation
  (define (untangle-call/ec untangle thunk)
    (call-with-values (lambda ()
                        (call/1cc
                         (lambda (k)
                           ;; K will allow to abort THUNK.
                           (assume (not (untangle-escapade untangle)))
                           (untangle-escapade! untangle k)
                           (thunk))))
      (lambda args
        ;; args may be the empty list if thunk returns nothing.
        (if (and (pair? args) (eq? (car args) escapade-singleton))
            ;; XXX: escapade handler! That is always a proc and a
            ;; continuation, because of untangle-escape.
            (let ((proc (cadr args))
                  (k (caddr args)))
              ;; call the procedure proc passed to untangle-escapade
              ;; with its continuation called k. That is, k, is what
              ;; follow untangle-escapade call inside THUNK. K will
              ;; allow to resume THUNK.
              (proc k))
            (apply values args)))))

  (define (untangle-escape untangle proc)
    ;; XXX: Capture the continuation and call it later, that is why it
    ;; is a call/cc and not call/1cc.
    (call/cc
     (lambda (k)
       ;; save escapade
       (define escapade (untangle-escapade untangle))
       ;; The escapade is a call/1cc continuation, no need to keep
       ;; it around, and it might lead to strange bugs.
       (untangle-escapade! untangle #f)
       ;; XXX: escapade is the continuation of thunk inside
       ;; untangle-call/ec whereas k is the continuation of the caller
       ;; of untangle-escapade, inside thunk.

       ;; XXX: Continue with thunk's continuation inside
       ;; untangle-call/ec as known as escapade. Inside
       ;; untangle-call/ec, proc and k are used to build the escape
       ;; handler.
       (escapade escapade-singleton proc k))))

  (define-record-type <untangle>
    (make-untangle% status escape time mutex queue entangle)
    untangle?
    (status untangle-status untangle-status!)
    (escape untangle-escapade untangle-escapade!)
    (time untangle-time untangle-time!)
    (mutex untangle-mutex untangle-mutex!)
    (queue untangle-queue untangle-queue!)
    (entangle untangle-entangle))

  (define untangled? (make-parameter #f))

  (define (make-untangle)
    (make-untangle% 'init (make-mutex) #f 0 '() (make-entangle)))

  (define (untangle-spawn untangle thunk)
    ;; TODO: use box-cas!
    (with-mutex (untangle-mutex untangle)
      (untangle-queue! untangle
                       (cons (cons (untangle-time untangle)
                                   thunk)
                             (untangle-queue untangle)))))

  (define (untangle-stop untangle)
    (untangle-status! untangle 'stopping))

  (define (untangle-start untangle)
    (untangled? #t)
    (untangle-status! untangle 'running)
    (let loop ()
      (when (and (untangle-busy? untangle)
                 (not (eq? (untangle-status untangle)
                           'stopping)))
        (untangle-tick untangle)
        (loop)))
    (untangle-status! untangle 'stopped)
    (untangled? #f))

  (define (untangle-busy? untangle)
    (raise 'not-implemented))

  (define (untangle-tick untangle)
    (untangle-time! untangle (current-time 'time-monotonic))
    (untangle-exec-expired-continuation untangle)
    (untangle-exec-network-continuation untangle))

  (define (untangle-sleep untangle nanoseconds seconds)
    (define delta (make-time 'time-duration nanoseconds seconds))
    (define when (add-duration (untangle-time untangle) delta))

    (define (handler resume)
      ;; RESUME is untangle-sleep continuation.
      ;; TODO: use box-cas!
      (with-mutex (untangle-mutex untangle)
        (untangle-queue! untangle (cons (cons when resume)
                                        (untangle-queue untangle)))))

    ;; untangle-sleep is necessarily called while untangle is running,
    ;; where untangled? returns #t, otherwise user code need to call
    ;; POSIX sleep.
    (assume (untangled?))
    (untangle-escape untangle handler))

  (define (untangle-exec-expired-continuation untangle)
    (define time (untangle-time untangle))

    (define (queue-snapshot-and-nullify)
      ;; TODO: use box-cas!
      (with-mutex (untangle-mutex untangle)
        (let ((queue (untangle-queue untangle)))
          (untangle-queue! untangle '())
          queue)))

    (define (call-or-keep time+thunk)
      (if (time<=? (car time) time)
          ;; A green thread wants to wake up!
          (begin
            ;; Call the registred thunk
            ((cdr time+thunk))
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

    (define waiting (queue-snapshot-and-nullify))
    (define pending (filter-map call-or-keep waiting))

    ;; TODO: use box-cas!
    (with-mutex (untangle-mutex untangle)
      (let ((queue (untangle-queue untangle)))
        (untangle-queue! untangle
                         (append pending (untangle-queue queue))))))

  (define (untangle-exec-network-continuation untangle)
    (entangle-continue (untangle-entangle untangle)))

  ;; channel

  (define-record-type <untangle-channel>
    (make-untangle-channel% untangle mutex inbox subscribers condition-mutex condition)
    untangle-channel?
    (untangle channel-untangle)
    (mutex channel-mutex)
    (inbox channel-inbox channel-inbox!)
    (subscribers channel-subscribers channel-subscribers!)
    (condition-mutex channel-condition-mutex)
    (condition channel-condition))

  (define (untangle-make-channel untangle)
    (make-untangle-channel% untangle
                            (make-mutex)
                            '()
                            '()
                            (make-mutex)
                            (make-condition)))

  (define untangle-closing-singleton '(untangle-closing-singleton))

  (define (untangle-channel-send channel obj)
    (define (subscribers-pop!)
      ;; TODO: use box-cas!
      (with-mutex (channel-mutex channel)
        (let ((subscribers (channel-subscribers channel)))
          (if (null? subscribers)
              #f
              (let ((subscriber (car subscribers)))
                (channel-subscribers! channel (cdr subscribers))
                subscriber)))))

    (define (inbox-cons!)
      ;; TODO: use box-cas!
      (with-mutex (channel-mutex channel)
        (channel-inbox! channel
                        (cons obj
                              (channel-inbox channel)))))

    (define subscriber (subscribers-pop!))

    (if subscriber
        (subscriber obj)
        (inbox-cons!)))

  (define (untangle-channel-recv untangle channel obj)
    (if (eq? (untangle-status (channel-untangle)) 'stopping)
        untangle-closing-singleton
        (if (untangled?)
            (channel-recv-with-untangle untangle channel obj)
            (channel-recv-without-untangle channel obj))))

  (define inbox-empty '(inbox-empty))

  (define (channel-recv-with-untangle untangle channel obj)
    (define (inbox-pop!)
      ;; TODO: use box-cas!
      (with-mutex (channel-mutex channel)
        (let ((inbox (channel-inbox channel)))
          (if (null? inbox)
              inbox-empty
              (let ((obj (car inbox)))
                (channel-inbox! channel (cdr inbox))
                obj)))))

    (define (spawn subscriber)
      (lambda (obj)
        (untangle-spawn untangle (lambda () (subscriber obj)))))

    (define (subscribe! subscriber)
      ;; TODO: use box-cas!
      ;; subscriber is the continuation of subscribe-and-pause!
      (with-mutex (channel-mutex channel)
        (channel-subscribers! channel
                              (cons (spawn subscriber)
                                    (channel-subscribers channel)))))

    (define (subscribe-and-pause!)
      ;; subscribe! is the escape handler, it is passed the
      ;; continuation of subscribe-and-pause! which eventually produce
      ;; the return value of untangle-channel-recv.
      (untangle-escape untangle subscribe!))

    (define obj (inbox-pop!))

    (if (eq? obj inbox-empty)
        (subscribe-and-pause!)
        obj))

  (define (channel-recv-without-untangle channel obj)

    (define (inbox-pop!)
      ;; TODO: use box-cas!
      (with-mutex (channel-mutex channel)
        (let ((inbox (channel-inbox channel)))
          (if (null? inbox)
              inbox-empty
              (let ((obj (car inbox)))
                (channel-inbox! channel (cdr inbox))
                obj)))))

    (define (wait-and-return!)
      (with-mutex (channel-condition-mutex channel)
        (condition-wait (channel-condition channel)
                        (channel-condition-mutex channel))
        (inbox-pop!)))

    (define obj (inbox-pop!))

    (if (eq? obj inbox-empty)
        (wait-and-return!)
        obj))

  (define (list->generator lst)
    (lambda ()
      (if (null? lst)
          (eof-object)
          (let ((head (lst)))
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

  (define (channel-select untangle channels)

    (define (channel-stopping? channel)
      (eq? (untangle-status (channel-untangle)) 'stopping))

    (define (stopping?)
      (generator-any?
       (generator-map channel-stopping?
                      (list->generator channels))))

    (if (stopping?)
        untangle-closing-singleton
        (if (untangled?)
            (select-with-untangle untangle channels)
            (select-without-untangle channels))))

  (define (select-with-untangle untangle channels)

    (define (inboxes-pop!)
      (let loop ((channels channels))
        (if (null? channels)
            (values #f #f)
            (let ((channel (car channels)))
              (with-mutex (channel-mutex channel)
                (if (null? (channel-inbox channel))
                    (loop (cdr channels))
                    (let ((obj (car (channel-inbox channel))))
                      (channel-inbox! channel
                                      (cdr (channel-inbox channel)))
                      (values channel obj))))))))

    (define (unsub-and-continue channel obj subscriber)
      (lambda ()
        ;; XXX: TODO: unsubscribe in other channels
        (untangle-spawn untangle
                        (lambda () (subscriber channel obj)))))

    (define (subscribe!! channel subscriber)
      (with-mutex (channel-mutex channel)
        (channel-subscribers! channel
                              (cons (lambda (obj)
                                      (unsub-and-continue channel
                                                          obj
                                                          subscriber))
                                    (channel-subscribers channel)))))

    (define (subscribe! subscriber)
      (for-each (lambda (channel) (subscribe!! channel subscriber))
                channels))

    (define (subscribe-and-pause!)
      (untangle-escape untangle subscribe!))

    (call-with-values inboxes-pop!
      (lambda (channel obj)
        (if channel
            (values channel obj)
            (subscribe-and-pause!)))))

  (define (select-without-untangle channels)

    (define (inboxes-pop!)
      (let loop ((channels channels))
        (if (null? channels)
            (values #f #f)
            (let ((channel (car channels)))
              (with-mutex (channel-mutex channel)
                (if (null? (channel-inbox channel))
                    (loop (cdr channels))
                    (let ((obj (car (channel-inbox channel))))
                      (channel-inbox! channel
                                      (cdr (channel-inbox channel)))
                      (values channel obj))))))))

    (call-with-values inboxes-pop!
      (lambda (channel obj)
        (if channel
            (values channel obj)
            (wait-and-return!))))))
