
## `(arew srfi srfi-9)`

This is based on [SRFI-9](https://srfi.schemers.org/srfi-9/).

### Abstract

Syntax for creating new data types, called record types. A predicate,
constructor, and field accessors and modifiers are defined for each
record type. Each new record type is distinct from all existing types,
including other record types and Scheme's predefined types.

### Reference

#### `(define-record-type ...)` syntax

The following:

```scheme
  (define-record-type <pare>
    (kons x y)
    pare?
    (x kar set-kar!)
    (y kdr))
```

Defines `KONS` to be a constructor, `KAR` and `KDR` to be accessors,
`SET-KAR!` to be a modifier, and `PARE?` to be a predicate for
`<PAREs>`.

```scheme
  (pare? (kons 1 2))        --> #t
  (pare? (cons 1 2))        --> #f
  (kar (kons 1 2))          --> 1
  (kdr (kons 1 2))          --> 2
  (let ((k (kons 1 2)))
    (set-kar! k 3)
    (kar k))                --> 3
```
