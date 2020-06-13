
## `(arew scheme eval)`

### `(environment list1 ...)`

This procedure returns a specifier for the environment that results by
starting with an empty environment and then importing each list,
considered as an import set, into it. The bindings of the environment
represented by the specifier are immutable, as is the environment
itself.

### `(eval expr-or-def environment-specifier)`

If `expr-or-def` is an expression, it is evaluated in the specified
environment and its values are returned. If it is a definition, the
specified identifier(s) are defined in the specified environment,
provided the environment is not immutable. Implementations may extend
`eval` to allow other objects.
