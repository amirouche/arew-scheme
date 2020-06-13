
## `(arew srfi srfi-28)`

This is based on [SRFI-28](https://srfi.schemers.org/srfi-28/).

### Abstract

A method of interpreting a Scheme string which contains a number of
escape sequences that are replaced with other string data according to
the semantics of each sequence. Also called string interpolation.

### Reference

#### `(format format-string [obj ...])`

Accepts a message template (a Scheme string), and processes it,
replacing any escape sequences in order with one or more characters,
the characters themselves dependent on the semantics of the escape
sequence encountered.

An escape sequence is a two character sequence in the string where the
first character is a tilde '~'. Each escape code's meaning is as
follows:

- `~a` The corresponding value is inserted into the string as if
  printed with display.

- `~s` The corresponding value is inserted into the string as if
  printed with write.

- `~%` A newline is inserted.

- `~~` A tilde `~` is inserted.

`~a` and `~s`, when encountered, require a corresponding Scheme value
to be present after the format string. The values provided as operands
are used by the escape sequences in order. It is an error if fewer
values are provided than escape sequences that require them.

`~%` and `~~` require no corresponding value.

**Examples:**

```scheme
(format "Hello, ~a" "World!")
;; => "Hello, World!"

(format "Error, list is too short: ~s~%" '(one "two" 3))
;; => "Error, list is too short: (one \"two\" 3))"
```
