
## `(arew srfi srfi-8)`

This is based on [SRFI-8](https://srfi.schemers.org/srfi-8/).

### Abstract

The only mechanism that R5RS provides for binding identifiers to the
values of a multiple-valued expression is the primitive
`call-with-values`. This SRFI proposes a more concise, more readable
syntax for creating such bindings.

### Reference

#### `(receive <formals> <expression> <body>)` syntax
