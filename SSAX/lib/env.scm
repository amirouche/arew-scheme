;				The Environment
;
; The environment is like a "database" (or soup in NewtonScript).
; It does *not* follow any static scope of Scheme expressions, nor a
; dynamic scope of Scheme procedure activations. It's just a
; (hierarchical) repository that any procedure may deposit things
; into, or request values associated with a name (a key-symbol)
;
; The values associated with names are generally non-mutable. If a
; procedure wants to change a binding (an association between a key and
; a value), it should extend the environment by another binding of the same
; name.
; Name lookup always proceeds from the most recently added entries
; backwards. It is possible to temporarily extend the environment with a
; private environment, which will get deleted later on. These private
; environments are created by env.capture!, which "privatizes" a part of
; the environment, from the current point to a previously put "mark"
;
; syntax:	env.find KEY
; Returns the value (property) associtated with the given key (symbol) in the
; environment, or #f if not found. Note, since env.find is a macro, the symbol
; KEY doesn't have to be quoted
;
; syntax:	env.demand KEY
; The same as above, but an error is signaled is KEY wasn't defined in the
; environment
;
; procedure:	env.print
; Prints the environment
;
; syntax:	env.bind KEY VALUE
; Binds the VALUE to a KEY, shadowing the previous binding for the KEY if
; existed. Since env.bind is a macro, the symbol KEY doesn't have to be quoted
;
; procedure:	env.bind* ASSOC-LIST
; Extends the environment with bindings specified by the ASSOC-LIST
;
; procedure:	env.mark
; Places a mark into the environment, and returns the mark
;
; procedure:	env.flush! MARK
; Flushes the environment through the given mark. An error is reported if
; the MARK does not exist in the environment
;
; procedure:	env.capture! MARK NAME
; Captures the environment through the MARK, creating a captured environment
; with a given NAME (a string). The captured portion is removed from the
; environment. An error is reported if the MARK does not exist in the
; environment.
; The captured environment is an opaque object: a PAIR, actually, whose
; CAR is a special magic symbol, and whose CDR is the captured environment
; (with its name on the bottom of it). The captured environment can be put
; back to the environment via env.extend, env.with and env.with-exclusive
;
; procedure:	env.extend CAPTURED-ENV
; Extends the environment with the CAPTURED-ENV (which was previously
; captured by env.capture!). An error is reported if the argument isn't
; a true captured env
;
; procedure:	env.with CAPTURED-ENV THUNK
; Executes the THUNK in an environment extended by the CAPTURED-ENV.
; After the chunk is finished, the original environment (the one existed
; before the application of this procedure) is restored. The result of
; THUNK is returned
;
; procedure:	env.with-exclusive CAPTURED-ENV THUNK
; The same is above but the CAPTURED-ENV (temporarily) replaces the current
; environment rather than extends it. Again, after the THUNK is over, the
; original environment is restored
;
; procedure:	env.->alist
; Export the environment into a (freshly allocated) associative list,
; skipping special slots (which carry environments' names, for example).
; The list will maintain the same order of bindings as was in the
; original environment.
;
; future extensions
; - make "virtual" slots, that is, slots whose value is computed/recomputed
; first time/every time the slots is accessed (vie env.find or env.demand),
; like in Dylan
; - env.with and env.with-exclusive ought to use dynamic-wind
; - env.find with a default clause? (in the manner of assq-def, etc)
; then env.demand may be unnecessary. Also document a procedural
; version of env.demand (%%env.demand), etc. in the case a key
; is computed.
; - iteration procedures to scan the environment, like
; env.any? (lambda (key value) ...)
; or similar iterators over containers in Dylan...
;
; myenv.scm, myenv-bigloo.scm or similar prelude is assumed.
;
; $Id: env.scm,v 1.2 2002/11/14 23:57:37 oleg Exp $

(declare			; (Gambit) Compiler options
 (block)
 (standard-bindings)
 (fixnum)               ; all arithmetics is FIXED
)

(define %%env.find #f)
(define %%env.demand #f)
(define env.print #f)
(define %%env.bind #f)
(define env.bind* #f)
(define env.mark #f)
(define env.flush! #f)
(define env.capture! #f)
(define env.extend #f)
(define env.with #f)
(define env.with-exclusive #f)
(define env.->alist #f)

; At present, the "environment" is implemented as an associative list. It can
; be a hash table (well, symbols - keys in the environment are hashed
; by Gambit anyway).
; Environment's name is an association with a special env-name-key

(let*
  ((env-name-key (string->symbol "Env Name Key!"))
   (env-capture-magic (string->symbol "Captured Env!"))
   (env-alist `((,env-name-key . "*** initial env ***")))
  )
  
  			; Print names of the environments that have extended
  			; the current environment
  			; Note, every captured env that extends the environment
  			; has its name at its bottom, associated with
  			; a special env-name-key
  (define (print-names)
    (cerr "The environment: ")
    (do ((item env-alist (cdr item)))
        ((null? item) "the above environment")
        (if (eq? env-name-key (caar item))
          (cerr (cdar item) "; ")))
      )
    
  (define (print . msg)
    (newline)
    (apply cout msg)
    (cout nl print-names nl (lambda () (pp env-alist)) nl))
    
  (define (env.error . msg)
    (and (apply error `(,@msg " within " ,(print-names)
          " enter #t to print the environment"))
      (print)))

  				; Make sure the argument is truly a
  				; captured env
  (define (env.assure-captured arg)
    (assure (and (pair? arg) (eq? (car arg) env-capture-magic))
      "The argument isn't a captured env"))
  
  (set! %%env.find
    (lambda (key)
      (cdr (or (assq key env-alist) '(#f . #f)))))
  
  (set! %%env.demand
    (lambda (key)
      (cdr (or (assq key env-alist)
          (env.error "Failed to find property " key)))))
  
  (set! env.print print)
  
  (set! %%env.bind
    (lambda (key value)
      (set! env-alist (cons (cons key value) env-alist))))
  
  (set! env.bind*
    (lambda (assoc-list)
      (set! env-alist (append assoc-list env-alist))))
      
      
      			; Place a mark into the env, and return it
  (set! env.mark
    (lambda ()
      (let ((mark (gensym)))
        (set! env-alist (cons (cons mark '()) env-alist))
        mark)))

			; Flush through the mark
  (set! env.flush!
    (lambda (mark)
      (let loop ((pos env-alist))
        (cond
          ((null? pos) (env.error "couldn't locate mark " mark))
          ((eq? mark (caar pos))
            (set! env-alist (cdr pos)))
          (else (loop (cdr pos)))))))

			; Capture the environment through the mark
			; (into a special data structure)
			; and flush through the mark
  (set! env.capture!
    (lambda (mark name)
      (let ((captured-env env-alist))
        (let loop ((pos env-alist))
          (cond
            ((null? pos) (env.error "couldn't locate mark " mark))
            ((eq? mark (caar pos))
              (set! env-alist (cdr pos))
              (set-cdr! pos '())
              (set-car! pos (cons env-name-key name)))
            (else (loop (cdr pos)))))
            			; make the capture object and return it
        (cons env-capture-magic captured-env))))
       
 
			; Extend the env with a previously captured-env
  (set! env.extend
    (lambda (captured-env)
      (env.assure-captured captured-env)
      (set! env-alist (append (cdr captured-env) env-alist))))
      
      			; Execute the THUNK in an environment extended with
      			; a previously captured-env
  (set! env.with
    (lambda (captured-env thunk)
      (env.assure-captured captured-env)
      (let ((orig-env env-alist) (result #f))
        (set! env-alist (append (cdr captured-env) env-alist))
        (set! result (thunk))
        (set! env-alist orig-env)
        result)))
      
      			; Execute the THUNK in the captured-env
  (set! env.with-exclusive
    (lambda (captured-env thunk)
      (env.assure-captured captured-env)
      (let ((orig-env env-alist) (result #f))
        (set! env-alist (cdr captured-env))
        (set! result (thunk))
        (set! env-alist orig-env)
        result)))

       			; Export the current env into an associative list
  (set! env.->alist
    (lambda ()
      (let loop ((pos env-alist))
        (cond
          ((null? pos) '())
          ((eq? env-name-key (caar pos)) (loop (cdr pos)))
          (else (cons (car pos) (loop (cdr pos))))))))
     
)
