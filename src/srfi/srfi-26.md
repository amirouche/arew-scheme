
## `(arew srfi srfi-26)`

This is based on [SRFI-26](https://srfi.schemers.org/srfi-26/).

### Abstract

When programming in functional style, it is frequently necessary to
specialize some of the parameters of a multi-parameter procedure. For
example, from the binary operation cons one might want to obtain the
unary operation (lambda (x) (cons 1 x)). This specialization of
parameters is also known as "partial application", "operator section"
or "projection".

### Reference

#### `(cut ...)` syntax

#### `(cute ...)` syntax
