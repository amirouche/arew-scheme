
## `(arew srfi srfi-6)`

This is based on [SRFI-6](https://srfi.schemers.org/srfi-6/).

### Abstract

Scheme's i/o primitives are extended by adding three new procedures that

- create an input port from a string,

- create an output port whose contents are accumulated in Scheme's
  working memory instead of an external file, and

- extract the accumulated contents of an in-memory output port and
  return them in the form of a string.

### Reference

#### `(open-input-string string)`

Takes a string and returns an input port that delivers characters from
the string. The port can be closed by `close-input-port`, though its
storage will be reclaimed by the garbage collector if it becomes
inaccessible.

```scheme
(define p
  (open-input-string "(a . (b . (c . ()))) 34"))

(input-port? p) ;; =>  #t
(read p) ;; => (a b c)
(read p) ;; => 34
(eof-object? (peek-char p)) ;; => #t
```
#### `(open-output-string)`

Returns an output port that will accumulate characters for retrieval
by `get-output-string`. The port can be closed by the procedure
`close-output-port`, though its storage will be reclaimed by the
garbage collector if it becomes inaccessible.

```scheme
(let ((q (open-output-string))
      (x '(a b c)))
  (write (car x) q)
  (write (cdr x) q)
  (get-output-string q)) ;; => "a(b c)"
```

#### `(get-output-string output-port)`

Given an output port created by `open-output-string`, returns a string
consisting of the characters that have been output to the port so far.
