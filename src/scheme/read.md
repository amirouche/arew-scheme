
## `(arew scheme read)`

### `(read [port])`

The `read` procedure converts external representations of Scheme
objects into the objects themselves. That is, it is a parser for the
non-terminal datum. It returns the next object parsable from the given
textual input port, updating port to point to the first character past
the end of the external representation of the object.

The current implementation is not fully compatible with R7RS.
