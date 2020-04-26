
## `(arew srfi srfi-111)`

This library is based on [SRFI-111](https://srfi.schemers.org/srfi-111/).

### Abstract

Boxes are objects with a single mutable state. Several Schemes have
them, sometimes called cells. A constructor, predicate, accessor, and
mutator are provided.

### Reference

#### `(box value)`

Constructor. Returns a newly allocated box initialized to value.

#### `(box? object)`

Predicate. Returns `#t` if object is a box, and `#f` otherwise.

#### `(unbox box)`

Accessor. Returns the current value of box.

#### `(set-box! box value)`

Mutator. Changes box to hold value.
