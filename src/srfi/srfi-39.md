
## `(arew srfi srfi-39)`

This is based on [SRFI-39](https://srfi.schemers.org/srfi-39/).

### Abstract

This SRFI defines parameter objects, the procedure make-parameter to
create parameter objects and the parameterize special form to
dynamically bind parameter objects. In the dynamic environment, each
parameter object is bound to a cell containing the value of the
parameter. When a procedure is called the called procedure inherits
the dynamic environment from the caller. The parameterize special form
allows the binding of a parameter object to be changed for the dynamic
extent of its body.

### Reference

#### `make-parameter`

#### `parameterize`
