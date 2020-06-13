
## `(arew srfi srfi-17)`

This is based on [SRFI-17](https://srfi.schemers.org/srfi-17/).

### Abstract

Allow procedure calls that evaluate to the "value of a location" to be
used to set the value of the location, when used as the first operand
of `set!`. For example:

```scheme
(set! (car x) (car y))
```

becomes equivalent to

```scheme
(set-car! x (car y))
```

### Reference

```scheme
(set! (car x) v) == (set-car! x v)
(set! (cdr x) v) == (set-cdr! x v)
(set! (caar x) v) == (set-car! (car x) v)
(set! (cadr x) v) == (set-car! (cdr x) v)
....
(set! (caXXr x) v) == (set-car! (cXXr x) v)
(set! (cdXXr x) v) == (set-cdr! (cXXr x) v)
(set! (string-ref x i) v) == (string-set! x i v)
(set! (vector-ref x i) v) == (vector-set! x i v)
```
