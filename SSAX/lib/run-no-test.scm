; The following macro DOES not run built-in test cases
(define-macro (run-test . body)
  '(begin #f))

