(define-library (arew data base fstore)

  (export fstore)
  (export fstore-branch-create)
  (export fstore-branch-create-orphan)
  (export fstore-branch-create-orphan-with-snapshot)
  (export fstore-branch-ref)
  (export fstore-branch-change-id-ref)
  (export fstore-merge-with-source-of-truth)
  (export fstore-ask?)
  (export fstore-add)
  (export fstore-delete)
  (export fstore-var)
  (export fstore-var?)
  (export fstore-var-name)
  (export fstore-from)
  (export fstore-where)
  (export fstore-select)

  (import (only (chezscheme) assert))

  (import (scheme base))
  (import (scheme case-lambda))
  (import (only (scheme list) drop iota reverse!))
  (import (scheme comparator))
  (import (scheme generator))
  (import (scheme hash-table))
  (import (scheme mapping hash))

  (import (cffi wiredtiger okvs))
  (import (arew data pack))
  (import (arew data base nstore))

  (import (uuid))

  (begin

    ;; fstore

    (define-record-type <fstore>
      (make-fstore engine prefix prefix-length metadata changes n)
      fstore?
      (engine fstore-engine)
      (prefix fstore-prefix)
      (prefix-length fstore-prefix-length)
      (metadata fstore-metadata)
      (changes fstore-changes)
      (n fstore-n))

    (define %quad '(graph subject predicate object))

    (define %change-extra-items '(alive? change-id uid))

    ;; XXX: zero has the shortest lexicographic representation and
    ;; changes has the most number tuples it will be the subpsace that
    ;; benefits the most from it.
    (define %prefix-change 0)
    (define %prefix-metadata 1)
    (define %prefix-snapshot 2)
    (define %prefix-history-significance 3)

    (define (fstore engine prefix items)
      (make-fstore engine
                   prefix
                   (length prefix)
                   ;; metadata stores branch history, branch snapshot
                   ;; identifier and tags.
                   (nstore engine
                           (append prefix (list %prefix-metadata))
                           %quad)
                   ;; change stores added / deleted tuples, it adds
                   ;; alive, change-id and uid tuple items via
                   ;; %change-extra-items to the set of items the
                   ;; application want to store to be able to tell
                   ;; whether a given tuple exists (alive), which
                   ;; change in branch history introduced the tuple
                   ;; (change-id), also each "tuple" in change store
                   ;; has a unique identifier, for dubious reaons...
                   (nstore engine
                           (append prefix (list %prefix-change))
                           (append items %change-extra-items))
                   ;; this is the length of the tuples that can be
                   ;; stored in this fstore.
                   (length items)))

    (define %bigbang (nil-uuid))

    (define %only-one '((limit . 1)))

    ;; XXX: the generator returned by nstore-from, nstore-where and
    ;; nstore-select must be consumed to close the cursor. Otherwise
    ;; there will be a resource leak.
    (define consume generator->list)

    ;; history

    (define-record-type <history>
      (%make-history max significance)
      history?
      (max history-max history-max!)
      (significance history-significance))

    (define %history-significance-comparator
      (make-vector-comparator (make-default-comparator)
                              bytevector?
                              bytevector-length
                              bytevector-u8-ref))

    (define (%make-empty-history)
      (make-hash-table %history-significance-comparator))

    (define (make-empty-history transaction fstore branch-id)
      (let ((history (%make-history 0 (%make-empty-history))))
        (hash-table-set! (okvs-transaction-metadata transaction)
                         (list 'fstore-history-significance
                               (fstore-prefix fstore)
                               branch-id)
                         history)
        history))

    (define (massage n x)
      ;; keep only the key and drop the prefix
      (apply cons (drop (unpack (car x)) n)))

    (define (query-history-significance transaction fstore branch-id)
      ;; fetch the history significance of BRANCH-ID as a generator
      (gmap (lambda (x) (massage (+ (fstore-prefix-length fstore) 2) x))
            (okvs-prefix transaction
                         (apply pack (append (fstore-prefix fstore)
                                             (list %prefix-history-significance
                                                   branch-id))))))

    (define (make-history transaction fstore branch-id)
      ;; fetch full history significance and cache it in transaction
      (let ((history (make-empty-history transaction fstore branch-id))
            (query (query-history-significance transaction fstore branch-id)))
        (let loop ()
          (let ((maybe-pair (query)))
            (unless (eof-object? maybe-pair)
              (hash-table-set! (history-significance history)
                               (car maybe-pair)
                               (cdr maybe-pair))
              (loop))))
        (history-max! history (hash-table-size (history-significance history)))
        history))

    (define (history-ref transaction fstore branch-id)
      ;; fetch history significance from cache if it is there
      ;; otherwise build it using make-history.
      (let ((out (hash-table-ref/default (okvs-transaction-metadata transaction)
                                         (list 'fstore-history-significance
                                               (fstore-prefix fstore)
                                               branch-id)
                                         #f)))
        (if out out (make-history transaction fstore branch-id))))

    (define %filler (pack 0))

    (define (history-significance-append! transaction fstore branch-id new-change)
      ;; Append NEW-CHANGE in BRANCH-ID history significance
      (let ((history (history-ref transaction fstore branch-id)))
        (let ((max (+ (history-max history) 1)))
          (history-max! history max)
          (hash-table-set! (history-significance history)
                           new-change
                           max)
          (okvs-set! transaction
                     (apply pack (append (fstore-prefix fstore)
                                         (list %prefix-history-significance
                                               branch-id
                                               new-change
                                               max)))
                     %filler))))

    ;; branch

    (define fstore-branch-ref
      ;; Fetch the identifier of branch NAME
      (lambda (transaction fstore name)
        (let ((out (consume
                    (nstore-from transaction
                                 (fstore-metadata fstore)
                                 (list 'branch
                                       (nstore-var 'uid)
                                       'branch/name
                                       name)
                                 %only-one))))
          (if (null? out)
              #f
              (hashmap-ref (car out) 'uid)))))

    (define fstore-branch-create
      ;; create a branch NAME starting at CHANGE
      (lambda (transaction fstore name branch-id)
        (when (fstore-branch-ref transaction fstore name)
          (error 'fstore "a branch with that name already exists" name))
        (let ((new-branch-id (random-uuid))
              (change-id (fstore-branch-change-id-ref transaction fstore branch-id)))
          ;; create branch
          (nstore-add! transaction
                       (fstore-metadata fstore)
                       (list 'branch
                             new-branch-id
                             'branch/name
                             name))
          ;; set change-id
          (nstore-add! transaction
                       (fstore-metadata fstore)
                       (list 'branch
                             new-branch-id
                             'branch/change-id
                             change-id))
          ;; the branch has no snapshot
          (nstore-add! transaction
                       (fstore-metadata fstore)
                       (list 'branch
                             new-branch-id
                             'branch/snapshot?
                             #f))
          ;; copy history significance to new branch
          (let ((significance (query-history-significance transaction fstore branch-id)))
            (let loop ((pair (significance)))
              (unless (eof-object? pair)
                (okvs-set! transaction
                           (apply pack (append (fstore-prefix fstore)
                                               (list %prefix-history-significance
                                                     new-branch-id
                                                     (car pair)
                                                     (cdr pair))))
                           %filler)
                (loop (significance))))
            new-branch-id))))

    (define (counter-increment transaction fstore symbol)
      ;; increment counter named SYMBOL and return its value.
      ;; Initialize the counter at 1 if it doesn't exist.
      (let ((out (consume (nstore-from transaction
                                       (fstore-metadata fstore)
                                       (list 'branch
                                             symbol
                                             symbol
                                             (nstore-var 'count))))))
        (if (null? out)
            (let ((count 1))
              (nstore-add! transaction
                           (fstore-metadata fstore)
                           (list 'branch
                                 symbol
                                 symbol
                                 count))
              count)
            (let ((count (hashmap-ref (car out) 'count)))
              ;; update count
              (nstore-delete! transaction
                              (fstore-metadata fstore)
                              (list 'branch
                                    symbol
                                    symbol
                                    count))
              (nstore-add! transaction
                           (fstore-metadata fstore)
                           (list 'branch
                                 symbol
                                 symbol
                                 (+ count 1)))
              (+ count 1)))))

    (define fstore-branch-create-orphan
      ;; create an orphan branch NAME
      (lambda (transaction fstore name)
        (when (fstore-branch-ref transaction fstore name)
          (error 'fstore "a branch with that name already exists" name))
        (let ((uid (random-uuid)))
          (nstore-add! transaction
                       (fstore-metadata fstore)
                       (list 'branch
                             uid
                             'branch/name
                             name))
          (nstore-add! transaction
                       (fstore-metadata fstore)
                       (list 'branch
                             uid
                             'branch/change-id
                             %bigbang))
          (nstore-add! transaction
                       (fstore-metadata fstore)
                       (list 'branch
                             uid
                             'branch/snapshot?
                             #f))
          ;; empty history
          uid)))

    (define fstore-branch-create-orphan-with-snapshot
      ;; create an orphan branch NAME with snapshot
      (lambda (transaction fstore name)
        (when (fstore-branch-ref transaction fstore name)
          (error 'fstore "a branch with that name already exists" name))
        (let ((uid (random-uuid)))
          (nstore-add! transaction
                       (fstore-metadata fstore)
                       (list 'branch
                             uid
                             'branch/name
                             name))
          (nstore-add! transaction
                       (fstore-metadata fstore)
                       (list 'branch
                             uid
                             'branch/change-id
                             %bigbang))
          (nstore-add! transaction
                       (fstore-metadata fstore)
                       (list 'branch
                             uid
                             'branch/snapshot?
                             #t))
          ;; allocate a snapshot identifier
          (nstore-add! transaction
                       (fstore-metadata fstore)
                       (list 'branch
                             uid
                             'branch/snapshot-id
                             (counter-increment transaction fstore 'branch/snapshot-count)))
          uid)))

    (define fstore-branch-change-id-ref
      ;; Return the identifier of the change pointed to by BRANCH
      (lambda (transaction fstore branch-id)
        (let ((out (consume
                    (nstore-from transaction
                                 (fstore-metadata fstore)
                                 (list 'branch
                                       branch-id
                                       'branch/change-id
                                       (nstore-var 'change-id))
                                 %only-one))))
          (if (null? out)
              ;; by construction, should not happen.
              (error 'fstore "oops")
              (hashmap-ref (car out) 'change-id)))))

    ;; merge

    (define (maybe-snapshot-ref transaction fstore branch-id)
      ;; TODO: re-use in fstore-ask, fstore-add and fstore-delete
      (let ((snapshot-id (branch-snapshot-ref transaction fstore branch-id)))
        (if (not snapshot-id)
            #f
            (nstore (fstore-engine fstore)
                    (append (fstore-prefix fstore)
                            (list %prefix-snapshot snapshot-id))
                    ;; the snapshot stores exactly fstore-n
                    ;; items in tuples
                    (iota (fstore-n fstore))))))

    (define (item-symbol index)
      (string->symbol (string-append "item-" (number->string index))))

    (define (make-pattern fstore change-id)
      (let loop ((index (- (fstore-n fstore) 1))
                 (out (list (nstore-var 'alive?)
                            change-id
                            (nstore-var 'uid))))
        (if (negative? index)
            out
            (loop (- index 1) (cons (nstore-var (item-symbol index)) out)))))

    (define (changes-ref transaction fstore change-id)
      (nstore-from transaction
                   (fstore-changes fstore)
                   (make-pattern fstore change-id)))

    (define (make-tuple fstore change-id change)
      (let loop ((index (- (fstore-n fstore) 1))
                 (out '()))
        (if (negative? index)
            (values (hashmap-ref change 'alive?) out)
            (loop (- index 1)
                  (cons (hashmap-ref change (item-symbol index)) out)))))

    (define (apply-changes transaction fstore snapshot change-id)
      (let ((changes (changes-ref transaction fstore change-id)))
        (let loop ((change (changes)))
          (unless (eof-object? change)
            (call-with-values (lambda () (make-tuple fstore change-id change))
              (lambda (alive? tuple)
                (if alive?
                    (nstore-add! transaction snapshot tuple)
                    (nstore-delete! transaction snapshot tuple))))
            (loop (changes))))))

    (define (fstore-merge-with-source-of-truth transaction fstore branch-id other-id)
      (unless (transaction-dirty? transaction fstore branch-id)
        (make-change transaction fstore branch-id))
      ;; mark OTHER-ID as parent of the current change
      (nstore-add! transaction
                   (fstore-metadata fstore)
                   (list 'change
                         (fstore-branch-change-id-ref transaction fstore branch-id)
                         'change/parent-id
                         other-id))
      ;; append history significance from OTHER-ID in BRANCH-ID and
      ;; copy tuples in the case of snapshoted branch.
      (let* ((branch-history (history-significance
                              (make-history transaction fstore branch-id)))
             (significances (generator->list
                             (query-history-significance transaction fstore other-id)))
             (significances (sort! (lambda (a b) (< (cdr a) (cdr b))) significances))
             (significances (map car significances))
             (snapshot (maybe-snapshot-ref transaction fstore branch-id)))
          (for-each
           (lambda (change-id)
             (unless (hash-table-ref/default branch-history change-id #f)
               (history-significance-append! transaction fstore branch-id change-id)
               (when snapshot
                 (copy-changes transaction fstore snapshot change-id))))
           significances)))

    ;; branch-snapshot

    (define (branch-snapshot-ref transaction fstore branch-id)
      ;; Return the identifier of the snapshot of `BRANCH-ID` if any
      (let ((out (consume
                  (nstore-from transaction
                               (fstore-metadata fstore)
                               (list 'branch
                                     branch-id
                                     'branch/snapshot-id
                                     (nstore-var 'uid))
                               %only-one))))
        (if (null? out)
            #f
            (hashmap-ref (car out) 'uid))))

    ;; fstore-ask

    (define (ask-with-snapshot? transaction fstore branch-id items snapshot-id)
      (let ((snapshot (nstore (fstore-engine fstore)
                              (append (fstore-prefix fstore)
                                      (list %prefix-snapshot snapshot-id))
                              ;; the snapshot stores exactly fstore-n
                              ;; items in tuples
                              (iota (fstore-n fstore)))))
        (nstore-ask? transaction snapshot items)))

    (define (candidate-is-from-branch candidate history)
      (hash-table-ref/default history (hashmap-ref candidate 'change-id) #f))

    (define (most-significant-change-alive? history candidates)
      ;; Select the most significant change, and extract its alive
      ;; value. CANDIDATES is a generator. HISTORY is hash-table
      ;; mapping change-id to history significance.
      (let loop ((out #f))
        (let ((candidate (candidates)))
          (if (eof-object? candidate)
              (if out
                  (hashmap-ref out 'alive?)
                  #f)
              (if (not (candidate-is-from-branch candidate history))
                  (loop out)
                  (if (not out)
                      (loop candidate)
                      (if (< (hash-table-ref history (hashmap-ref out 'change-id))
                             (hash-table-ref history (hashmap-ref candidate 'change-id)))
                          (loop candidate)
                          (loop out))))))))

    (define (ask? transaction fstore branch-id items)
      (let loop ((history (history-ref transaction fstore branch-id)))
        (let ((candidates (nstore-from transaction
                                       (fstore-changes fstore)
                                       (append items
                                               (list (nstore-var 'alive?)
                                                     (nstore-var 'change-id)
                                                     (nstore-var 'uid))))))
          (most-significant-change-alive? (history-significance history) candidates))))

    (define fstore-ask?
      (lambda (transaction fstore branch-id items)
        (assert (= (fstore-n fstore) (length items)))
        (let ((snapshot (branch-snapshot-ref transaction fstore branch-id)))
          (if snapshot
              (ask-with-snapshot? transaction fstore branch-id items snapshot)
              (ask? transaction fstore branch-id items)))))

  ;; transaction helpers

  (define (transaction-dirty? transaction fstore branch-id)
    (let* ((prefix (fstore-prefix fstore))
           (key (list 'fstore-dirty prefix branch-id)))
      (hash-table-ref/default (okvs-transaction-metadata transaction) key #f)))

  (define (transaction-dirty! transaction fstore branch-id)
    (let* ((prefix (fstore-prefix fstore))
           (key (list 'fstore-dirty prefix branch-id)))
      (hash-table-set! (okvs-transaction-metadata transaction) key #t)))

  (define (make-change transaction fstore branch-id)
    ;; This procedure is called when a transaction try to write for
    ;; the first time to the database. It will add the change to the
    ;; history of the given BRANCH-ID and return its identifier. It
    ;; will also mark the transaction as dirty, to avoid to create
    ;; multiple changes per transaction. In fact, a change (commit in
    ;; git parlance) maps one-to-one with database transactions that
    ;; do writes (except in the case of merge).
    (let ((new-change (random-uuid))
          (old-change (fstore-branch-change-id-ref transaction fstore branch-id)))
      ;; add new-change to the list of changes as a successor of
      ;; old-change
      (nstore-add! transaction
                   (fstore-metadata fstore)
                   (list 'change
                         new-change
                         'change/parent-id
                         old-change))
      ;; point branch to the new change
      (nstore-delete! transaction
                      (fstore-metadata fstore)
                      (list 'branch
                            branch-id
                            'branch/change-id
                            old-change))
      (nstore-add! transaction
                   (fstore-metadata fstore)
                   (list 'branch
                         branch-id
                         'branch/change-id
                         new-change))
      ;; mark the transaction as dirty, so that future writes don't
      ;; create another change for the same transaction.
      (transaction-dirty! transaction fstore branch-id)
      ;; add to history significance
      (history-significance-append! transaction fstore branch-id new-change)
      new-change))

  ;; fstore-add

  (define (add-with-snapshot transaction fstore branch-id items snapshot)
    (let ((snapshot (nstore (fstore-engine fstore)
                            (append (fstore-prefix fstore) (list %prefix-snapshot snapshot))
                            (iota (fstore-n fstore)))))
      ;; add ITEMS only if it is not present.
      (if (nstore-ask? transaction snapshot items)
          #f
          ;; mark as dirty, add to changes and snapshot
          (begin
            (unless (transaction-dirty? transaction fstore branch-id)
              (make-change transaction fstore branch-id))
            (nstore-add! transaction
                         (fstore-changes fstore)
                         (append items
                                 (list #t ;; alive?
                                       (fstore-branch-change-id-ref transaction fstore branch-id)
                                       (random-uuid))))
            (nstore-add! transaction snapshot items)
            #t))))

  (define (add transaction fstore branch-id items)
    ;; add without snapshot
    (if (ask? transaction fstore branch-id items)
        #f ;; already in database
        (begin
          ;; mark as dirty and add to changes
          (unless (transaction-dirty? transaction fstore branch-id)
            (make-change transaction fstore branch-id))
          (nstore-add! transaction
                       (fstore-changes fstore)
                       (append items
                               (list #t ;; alive?
                                     (fstore-branch-change-id-ref transaction fstore branch-id)
                                     (random-uuid))))
          #t)))

  (define fstore-add
    (lambda (transaction fstore branch-id items)
      (assert (= (fstore-n fstore) (length items)))
      (let ((snapshot (branch-snapshot-ref transaction fstore branch-id)))
        (if snapshot
            (add-with-snapshot transaction fstore branch-id items snapshot)
            (add transaction fstore branch-id items)))))

  ;; fstore-remove

  ;; TODO: factor with fstore-add, because only nstore-add! differs
  ;; with nstore-delete!

  (define (delete-with-snapshot transaction fstore branch-id items snapshot)
    (let ((snapshot (nstore (fstore-engine fstore)
                            (append (fstore-prefix fstore) (list %prefix-snapshot snapshot))
                            (iota (fstore-n fstore)))))
      ;; remove ITEMS only if it is present.
      (if (not (nstore-ask? transaction snapshot items))
          #f
          (begin
            ;; mark as dirty, add to changes and snapshot
            (unless (transaction-dirty? transaction fstore branch-id)
              (make-change transaction fstore branch-id))
            (nstore-add! transaction
                         (fstore-changes fstore)
                         (append items
                                 (list #f ;; alive?
                                       (fstore-branch-change-id-ref transaction fstore branch-id)
                                       (random-uuid))))
            (nstore-delete! transaction snapshot items)
            #t))))

  (define (delete transaction fstore branch-id items)
    (if (not (ask? transaction fstore branch-id items))
        #f ;; not in database, nothing do to
        (begin
          ;; mark as dirty and add to changes
          (unless (transaction-dirty? transaction fstore branch-id)
            (make-change transaction fstore branch-id))
          (nstore-add! transaction
                       (fstore-changes fstore)
                       (append items
                               (list #f ;; alive?
                                     (fstore-branch-change-id-ref transaction fstore branch-id)
                                     (random-uuid))))
          #t)))

  (define fstore-delete
    (lambda (transaction fstore branch-id items)
      (assert (= (length items) (fstore-n fstore)))
      (let ((snapshot (branch-snapshot-ref transaction fstore branch-id)))
        (if snapshot
            (delete-with-snapshot transaction fstore branch-id items snapshot)
            (delete transaction fstore branch-id items)))))

  ;; fstore-from

  (define-record-type <fstore-var>
    (fstore-var name)
    fstore-var?
    (name fstore-var-name))

  (define symbol-comparator (make-eq-comparator))

  (define (fstore->nstore-var maybe-variable)
    (if (fstore-var? maybe-variable)
        (nstore-var (fstore-var-name maybe-variable))
        maybe-variable))

  (define (query-with-snapshot transaction fstore pattern seed config snapshot)
    (let ((snapshot (nstore (fstore-engine fstore)
                            (append (fstore-prefix fstore) (list %prefix-snapshot snapshot))
                            (iota (fstore-n fstore))))
          (pattern (map fstore->nstore-var pattern)))
      (gmap (lambda (binding) (hashmap-union seed binding))
            (nstore-from transaction snapshot pattern config))))

  (define (query-config config)
    (let ((limit #f)
          (offset #f))
      (let loop ((config config))
        (if (null? config)
            (values limit offset)
            (begin (case (caar config)
                     ((limit) (set! limit (cdar config)))
                     ((offset) (set! offset (cdar config))))
                   (loop (cdr config)))))))

  (define (bind pattern binding)
    ;; Return a tuple where variable of PATTERN are bound using
    ;; BINDING.
    (let loop ((pattern pattern)
               (out '()))
      (if (null? pattern)
          (reverse! out)
          (loop (cdr pattern)
                (let ((head (car pattern)))
                  (if (fstore-var? head)
                      (cons (hashmap-ref binding (fstore-var-name head)) out)
                      (cons head out)))))))

  (define (nstore->fstore binding)
    ;; convert nstore BINDING to a fstore binding
    (let ((out (hashmap-delete binding '%fstore-change-id)))
      (hashmap-delete out '%fstore-uid)))

  (define (query transaction fstore branch-id pattern seed config)
    ;; This is it! The time travelling query.
    (let* ((changes (fstore-changes fstore))
           ;; convert PATTERN to an nstore pattern and add alive?,
           ;; change-id and uid items
           (pattern* (append (map fstore->nstore-var pattern)
                             (list #t ;; alive?
                                   (nstore-var '%fstore-change-id)
                                   (nstore-var '%fstore-uid))))
           (out (nstore-from transaction changes pattern*))
           ;; XXX: Filter out actually dead items at this point in
           ;; time. gfilter will consume the whole generator, so there
           ;; is no cursor bookeeping to do.
           (out (gfilter (lambda (x) (ask? transaction fstore branch-id (bind pattern x)))
                         out))
           (out (gmap nstore->fstore out)))
      (call-with-values (lambda () (query-config config))
        (lambda (limit offset)
          (when offset
            (set! out (gdrop out offset)))
          (when limit
            (set! out (gtake out limit)))
          ;; That is all folks!
          (gmap (lambda (binding) (hashmap-union seed binding)) out)))))

  (define fstore-from
    ;; similar to nstore-from...
    (case-lambda
      ((transaction fstore branch-id pattern)
       (assert (= (length pattern) (fstore-n fstore)))
       (fstore-from transaction fstore branch-id pattern '()))
      ((transaction fstore branch-id pattern config)
       (assert (= (length pattern) (fstore-n fstore)))
       (let ((snapshot (branch-snapshot-ref transaction fstore branch-id))
             ;; empty seed
             (seed (hashmap symbol-comparator)))
         (if snapshot
             (query-with-snapshot transaction fstore pattern seed config snapshot)
             (query transaction fstore branch-id pattern seed config))))))

  ;; fstore-where

  (define (gscatter generator)
    ;; Return a generator that yields the elements of the generators
    ;; produced by the given generator. Same as gflatten but the
    ;; generator contains other generators instead of lists.
    (let ((state eof-object))
      (lambda ()
        (let ((value (state)))
          (if (eof-object? value)
              (let loop ((new (generator)))
                (if (eof-object? new)
                    new
                    (let ((value (new)))
                      (if (eof-object? value)
                          (loop (generator))
                          (begin (set! state new)
                                 value)))))
              value)))))

  (define (maybe-bind pattern binding)
    ;; Same as the above bind procedure, but variables in PATTERN
    ;; are not necessarly in BINDING.
    (let loop ((pattern pattern)
               (out '()))
      (if (null? pattern)
          (reverse! out)
          (loop (cdr pattern)
                (let ((head (car pattern)))
                  (if (fstore-var? head)
                      (cons (hashmap-ref/default binding (fstore-var-name head) head) out)
                      (cons head out)))))))

  (define (fstore-where transaction fstore branch-id pattern)
    ;; similar to nstore-from...
    (lambda (from)
      (let ((snapshot (branch-snapshot-ref transaction fstore branch-id))
            (seed (hashmap symbol-comparator)))
        (gscatter
         (gmap
          (lambda (binding)
            (if snapshot
                (query-with-snapshot transaction
                                     fstore
                                     (maybe-bind pattern binding)
                                     binding
                                     '()
                                     snapshot)
                (query transaction fstore branch-id (maybe-bind pattern binding) binding '())))
          from)))))

  (define-syntax fstore-select
    ;; exactly the same as nstore-select...
    (syntax-rules ()
      ((_ value) value)
      ((_ value f rest ...)
       (fstore-select (f value) rest ...))))

  ))
