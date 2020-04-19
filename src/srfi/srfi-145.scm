(library (srfi srfi-145)
  (export assume)

  (import (chezscheme))

  (define-syntax assume
    (syntax-rules ()
      [(_ expression messages ...)
       (meta-cond
         [(fx=? (optimize-level) 3) (void)]
         [else (unless expression
                 (let ([msgs (list messages ...)])
                   (errorf #f "invalid assumption ~s~@[ with message~p ~{~a~#[~;, and ~:;, ~]~}~]"
                           'expression (and (not (null? msgs)) (length msgs)) msgs)))])])))
