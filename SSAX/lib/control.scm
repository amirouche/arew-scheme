(define-syntax when
  (syntax-rules ()
    ((when condition stuff ...)
     (if condition
	 (begin stuff ...)))))

(define-syntax begin0
  (syntax-rules ()
    ((begin0 form0 form1 ...)
     (let ((result form0))
       (begin form1 ... result)))))
