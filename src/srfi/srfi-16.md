
## `(arew srfi srfi-16)`

### `(case-lambda clause1 clause2 ...)` syntax

Each clause is of the form `(formals body)`, where `formals` and
`body` have the same syntax as in a lambda expression.

A case-lambda expression evaluates to a procedure that accepts a
variable number of arguments and is lexically scoped in the same
manner as a procedure resulting from a lambda expression. When the
procedure is called, the first clause for which the arguments agree
with `formals` is selected, where agreement is specified as for the
`formals` of a lambda expression. The variables of `formals` are bound
to fresh locations, the values of the arguments are stored in those
locations, the `body` is evaluated in the extended environment, and
the results of `body` are returned as the results of the procedure
call.

It is an error for the arguments not to agree with the `formals` of
any clause`.

**Example:**

```scheme
(define add1
  (case-lambda
    ((a) (add1 a 0))
    ((a b) (+ 1 a b))))

(add1 1) ;; => 2
(add1 1 2) ;; => 4
```
