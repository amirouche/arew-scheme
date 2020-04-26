
## `(arew scheme load)`

### `(load filename [environment])`

It is an error if `filename` is not a string.

An implementation-dependent operation is used to transform `filename`
into the name of an existing file containing Scheme source code. The
`load` procedure reads expressions and definitions from the file and
evaluates them sequentially in the environment specified by
`environment`. If `environment` is omitted,
`(interaction-environment)` is assumed.

It is unspecified whether the results of the expressions are
printed. The `load` procedure does not affect the values returned by
`current-input-port` and `current-output-port`. It returns an
unspecified value.
