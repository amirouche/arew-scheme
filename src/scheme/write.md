
## `(arew scheme write)`

### `(display obj [port])`

Writes a representation of obj to the given textual output
port. Strings that appear in the written representation are output as
if by write-string instead of by write. Symbols are not
escaped. Character objects appear in the representation as if written
by write-char instead of by write.

### `(write obj [port])`

Writes a representation of obj to the given textual output
port. Strings that appear in the written representation are enclosed
in quotation marks, and within those strings backslash and quotation
mark characters are escaped by backslashes. Symbols that contain
non-ASCII characters are escaped with vertical lines. Character
objects are written using the #\ notation.

If obj contains cycles which would cause an infinite loop using the
normal written representation, then at least the objects that form
part of the cycle must be represented using datum labels as described
in section 2.4. Datum labels must not be used if there are no cycles.

### `(write-simple obj [port])`

The write-simple procedure is the same as write, except that shared
structure is never represented using datum labels. This can cause
write-simple not to terminate if obj contains circular structure.

### `(write-shared obj [port])`

The write-shared procedure is the same as write, except that shared
structure must be represented using datum labels for all pairs and
vectors that appear more than once in the output.
