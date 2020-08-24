(library (arew data aho-corasick)

  (export make-aho-corasick
          aho-corasick-add!
          aho-corasick-finalize!
          aho-corasick-debug
          aho-corasick-match)

  (import (scheme base)
          (scheme char)
          (scheme set)
          (scheme write)
          (scheme time)
          (scheme process-context)
          (scheme comparator)
          (scheme hash-table)
          (scheme fixnum)
          (arew data siset)
          (only (chezscheme) eval))

  ;; transitions hash-table helpers

  (define transition-comparator (make-comparator char? char=? #f char-hash))

  (define (make-transitions)
    (make-hash-table transition-comparator))

  (define uid-generator
    (let ((uid -1))
      (lambda ()
        (set! uid (fx+ uid 1))
        uid)))

  (define-record-type <state>
    (%make-state uid char success? transitions parent match longest-strict-suffix next)
    state?
    (uid state-uid)
    (char state-char)
    (success? state-success? state-success?!)
    (transitions state-transitions)
    (parent state-parent)
    (match state-match state-match!)
    (longest-strict-suffix state-longest-strict-suffix state-longest-strict-suffix!)
    (next state-next state-next!))

  (define (make-state char parent)
    (%make-state (uid-generator)
                 char
                 #f
                 (make-transitions)
                 parent
                 #f
                 #f
                 #f))

  (define-record-type <aho-corasick>
    (%make-aho-corasick root size states finalized?)
    aho-corasick?
    (root aho-corasick-root)
    (size aho-corasick-size aho-corasick-size!)
    (states aho-corasick-states aho-corasick-states!)
    (finalized? aho-corasick-finalized? aho-corasick-finalized?!))

  (define (make-aho-corasick)
    (%make-aho-corasick (make-state #f #f) 1 #f #f))

  (define(aho-corasick-add! aho-corasick word)

    (define (make-new-state char parent)
      (define new (make-state char parent))
      (aho-corasick-size! aho-corasick
                          (fx+ (aho-corasick-size aho-corasick) 1))

      (hash-table-set! (state-transitions parent) char new)

      new)

    (when (zero? (string-length word))
      (error 'aho-corasick "word can not be the empty string"))

    (let loop ((chars (string->list word))
               (state (aho-corasick-root aho-corasick)))
      (if (null? chars)
          (begin
            (state-success?! state #t)
            (state-match! state word))
          (let ((state (hash-table-ref (state-transitions state)
                                       (car chars)
                                       (lambda () (make-new-state (car chars) state)))))
            (loop (cdr chars) state)))))

  (define (aho-corasick-debug aho-corasick)
    (define root (aho-corasick-root aho-corasick))

    (define todo (make-hash-table (make-eq-comparator)))
    (define done (make-hash-table (make-eq-comparator)))

    (hash-table-set! todo root #t)

    (display "digraph g {
 graph [fontname = \"Noto Sans\"];
 node [fontname = \"Noto Sans\"];
 edge [fontname = \"Noto Sans\"];
")

    (let loop ()
      (unless (hash-table-empty? todo)
        (call-with-values (lambda () (hash-table-pop! todo))
          (lambda (state _)
            (unless (hash-table-contains? done state)
              (hash-table-set! done state #t)
              (call-with-values (lambda () (hash-table-entries (state-transitions state)))
                (lambda (keys values)
                  (let loop0 ((entries (map cons keys values)))
                    (unless (null? entries)
                      (let* ((entry (car entries))
                             (char (car entry))
                             (child (cdr entry)))
                        (display "\t") (display (state-uid state)) (display " -> ") (display (state-uid child)) (display " [label=\"") (display char) (display "\"]")(newline)
                        (hash-table-set! todo child #t)
                        (loop0 (cdr entries)))))))
              ;; render success
              (when (state-success? state)
                (display "\t") (display (state-uid state)) (display " -> \"") (display (state-match state)) (display "\" [color=\"green\"]") (newline))
              (display "\t") (display (state-uid state)) (display " -> ") (display (state-uid (state-longest-strict-suffix state))) (display " [color=\"red\"]") (newline))))
          (loop)))

    (display "\n}")
    (newline))

  (define (aho-corasick-finalize-step-1! aho-corasick)
    (define root (aho-corasick-root aho-corasick))

    (define (finalize! state char next-state)
      (unless (hash-table-contains? (state-transitions state) char)
        (hash-table-set! (state-transitions state) char next-state)))

    (define (search-longest-strict-suffix! state)
      (unless (state-longest-strict-suffix state)
        (let loop ((traversed (state-longest-strict-suffix (state-parent state))))
          (cond
           ((hash-table-ref (state-transitions traversed)
                            (state-char state)
                            (lambda () #f)
                            (lambda (obj) (and (not (eq? obj state)) obj)))
            => (lambda (obj) (state-longest-strict-suffix! state obj)))
           ((eq? traversed root) (state-longest-strict-suffix! state root))
           (else (loop (state-longest-strict-suffix traversed)))))

        (let ((suffix (state-longest-strict-suffix state)))
          (unless (eq? suffix root)
            (search-longest-strict-suffix! suffix)
            (hash-table-for-each (lambda (char next)
                                   (finalize! state char next))
                                 (state-transitions suffix))))))

    (define (make-fixnum-comparator)
      (make-comparator fixnum? fx=? fx<? number-hash))

    (define (make-state-comparator)
      (make-comparator state? eq? #f (lambda (x) (number-hash (state-uid x)))))

    (define todo (make-siset (make-state-comparator)
                             ;; XXX: magic number ahead?
                             ;; TODO: document magic number!
                             (max (round (/ (aho-corasick-size aho-corasick) 30)) 1)
                             root))

    (define done (make-siset (make-fixnum-comparator)
                             (max (aho-corasick-size aho-corasick) 1)))

    (define (proc char child)
      (unless (siset-contains? done (state-uid child))
        (siset-add! todo child)
        (search-longest-strict-suffix! child)))

    (when (aho-corasick-finalized? aho-corasick)
      (error 'aho-corasick "aho-corasick is already finalized!"))

    (state-longest-strict-suffix! root root)

    (let loop ()
      (unless (siset-empty? todo)
        (let ((state (siset-pop! todo)))
          (unless (siset-contains? done (state-uid state))
            (siset-add! done (state-uid state))
            (hash-table-for-each proc
                                  (state-transitions state))))
        (loop))))

  (define (aho-corasick-finalize-step-2! aho-corasick)
    (define root (aho-corasick-root aho-corasick))

    (define (make-state-comparator)
      (make-comparator state? eq? #f (lambda (x) (number-hash (state-uid x)))))

    (define todo (make-siset (make-state-comparator)
                             ;; XXX: magic number ahead?
                             ;; TODO: document magic number!
                             (max (round (/ (aho-corasick-size aho-corasick) 30)) 1)
                             root))

    (define (add! state)
      (unless (vector-ref (aho-corasick-states aho-corasick) (state-uid state))
        (siset-add! todo state)))

    (define (next-add! state)
      (unless (state-next state)
        (siset-add! todo state)))

    (define (compute-next state)
      `(lambda (char fallback)
         (case char
           ,@(map (lambda (char* state*) `((,char*) ,(state-uid state*)))
                  (hash-table-keys (state-transitions state))
                  (hash-table-values (state-transitions state)))
           (else (fallback char #f)))))

    (aho-corasick-states! aho-corasick
                          (make-vector
                           (aho-corasick-size aho-corasick) #f))

    (let loop ()
      (unless (siset-empty? todo)
        (let ((state (siset-pop! todo)))
          (vector-set! (aho-corasick-states aho-corasick) (state-uid state) state)
          (hash-table-for-each (lambda (_ child) (add! child))
                               (state-transitions state)))
        (loop)))

    (set! todo (make-siset (make-state-comparator)
                           ;; XXX: magic number ahead?
                           ;; TODO: document magic number!
                           (max (round (/ (aho-corasick-size aho-corasick) 30)) 1)))

    (state-next! root
                 (eval `(lambda (char _)
                          (case char
                            ,@(map (lambda (char* state*)
                                     `((,char*) ,(state-uid state*)))
                                   (hash-table-keys (state-transitions root))
                                   (hash-table-values (state-transitions root)))
                            (else 0)))))

    (for-each next-add!
              (hash-table-values (state-transitions root)))

    (let loop ()
      (unless (siset-empty? todo)
        (let ((state (siset-pop! todo)))
          (unless (state-next state)
            (state-next! state (eval (compute-next state)))
            (for-each next-add!
                      (hash-table-values (state-transitions state)))))
        (loop))))

  (define (aho-corasick-finalize! aho-corasick)
    (aho-corasick-finalize-step-1! aho-corasick)
    (aho-corasick-finalize-step-2! aho-corasick)
    (aho-corasick-finalized?! aho-corasick #t))

  (define (aho-corasick-match aho-corasick generator)
    (define (lookup-matches state)
      (let loop0 ((state state)
                  (out '()))
        (if (eq? state root)
            out
            (if (state-success? state)
                (loop0 (state-longest-strict-suffix state)
                       (cons (state-match state) out))
                (loop0 (state-longest-strict-suffix state)
                       out)))))

    (define root (aho-corasick-root aho-corasick))

    (define fallback (state-next root))

    (unless (aho-corasick-finalized? aho-corasick)
      (error 'aho-corasick "aho-corasick is not finalized"))

    (let loop ((state root)
               (char (generator))
               (out '()))
      (if (eof-object? char)
          out
          (let* ((uid ((state-next state) char fallback))
                 (next (vector-ref (aho-corasick-states aho-corasick) uid)))
            (loop next
                  (generator)
                  (append (lookup-matches next) out)))))))
