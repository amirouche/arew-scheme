; 			Catcher of errors
; catches an error when it occurs, preventing an abort that would otherwise
; follow.
; It is very useful in the validation code to make sure a test case that
; was supposed to fail fails indeed. An expected error would not
; cause the inferior REPL be entered, nor will it abort the execution,
; letting the validation process continue.
;
; $Id: catch-error.scm,v 1.2 2003/10/30 21:33:10 oleg Exp $

	; Try to execute the thunk, and return #f if execution succeeded
	; If an error occurred during the execution, it is caught, and
	; the thunk-failed? procedure returns #t
(define (thunk-failed? thunk)
  (let
    ((orig-error error)		; save the original 'error' primitive
     (caught #f))
    (call-with-current-continuation 
     (lambda (cont)
       (set! error		; redefine the error primitive
             (lambda (msg . args)
               (identify-error msg args "The error above has been caught")
               ;(cerr "caught error: " msg)
               ;(for-each (lambda (arg) (cerr arg)) args) 
               ;(cerr nl)
               (set! caught #t)
               (cont #t)))
       (thunk)
       #f))
    (set! error orig-error)
    caught))

(cond-expand
  ((or bigloo gambit)
    (define-macro (failed? . stmts)
      `(thunk-failed? (lambda () ,@stmts))))
  (else
    (define-syntax failed?
      (syntax-rules ()
	((failed? . stmts)
	  (thunk-failed? (lambda () . stmts)))))))


