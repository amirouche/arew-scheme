
## `(arew untangle)`

### Abstract

`untangle` is a framework for building asynchronous application using
code that looks like synchronous, that is, the code looks sequential
without callback.  That is inspired from Python's `asyncio`,
guile-fibers and scheme-cml.

### Reference

#### `(untangle thunk) `

Create an event-loop and bind it to the current POSIX threads and
execute `THUNK`.  `THUNK` can call `spawn`, `make-future` and `await`.
The event-loop use cooperative multithreading.  The threads running
inside the event-loop are called greenthreads.

#### `(untangle-spawn thunk)`

Create a new greenthread in the current event-loop. An event-loop must
be running in the current POSIX threads to be able to call `spawn`.
`THUNK` will be executed as-soon-as-possible, in the same event-loop.

#### `(untangle-make-future)`

`untangle-make-future` returns a handle that can be passed to another
POSIX thread.  It must be used with `untangle-future-continue` and
`await`.  This is mostly useful for doing CPU intensive operation
without blocking the event-loop.  It can also be useful to turn a
blocking operation into asynchronous operation, if the cost of
creating a POSIX thread does not matter (or it is not possible to do
otherwise).

#### `(untangle-await future)`

`untangle-await` will await for `FUTURE` to be continued.  It must be
called while an event-loop is running in the current POSIX thread.
When `untangle-await` is called, the current greenthread will pause
and wait that some other POSIX thread call `untangle-future-continue`.
Only one greenthread can await `FUTURE`, otherwise the behavior is
unspecified.

#### `(untangle-future-continue future values)`

Continue `FUTURE` with `VALUES`. The greenthread that is awaiting
`FUTURE` will resume and `await` will return multiple values as passed
as `VALUES`. Only one POSIX thread can continue `FUTURE`, otherwise
the behavior is unspecified.

#### `(untangle-socket domain type protocol)`

Return a handle over a socket for the given `DOMAIN`, `TYPE` and
`PROTOCOL`.

#### `(untangle-accept fd)`

Accept a connection over the socket handle `FD`.

#### `(untangle-generator fd)`

Generates `bytevector` without blocking the current greenthread.

#### `(untangle-accumulator fd)`

Accumulates `bytevector` without blocking the current greenthread.

#### `(untangle-make-channel)`

#### `(untangle-operation-put channel values)`

#### `(untangle-operation-get channel values)`

#### `(untangle-operation-sleep timeout values)`

#### `(untangle-choose operations)`

#### `(untangle-do operation)`

Execute `OPERATION` without blocking. Return multiple values.
