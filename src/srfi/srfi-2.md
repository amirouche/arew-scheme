
## `(arew srfi srfi-2)`

This is based on [SRFI-2](https://srfi.schemers.org/srfi-2/).

### Abstract

Like an ordinary `and`, an `and-let*` special form evaluates its
arguments -- expressions -- one after another in order, till the first
one that yields `#f`. Unlike `and`, however, a non-`#f` result of one
expression can be bound to a fresh variable and used in the subsequent
expressions. `and-let*` is a cross-breed between `let*` and `and`.

### Reference

#### `and-let*`

`and-let*` is a generalized `and`: it evaluates a sequence of forms
one after another till the first one that yields `#f`; the non-`#f`
result of a form can be bound to a fresh variable and used in the
subsequent forms.
