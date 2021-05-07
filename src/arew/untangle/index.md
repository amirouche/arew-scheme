
## `(arew untangle)`

`untangle` is cooperative event-loop that does not expose
continuations (aka. callbacks) and where procedures are [not
colored](https://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/). That
can be achieved thanks to `call/cc`.

> **Note:** Understanding what is `call/cc` and how to use it is not
> necessary to use this library.

Until things like
[`io_uring`](https://thenewstack.io/how-io_uring-and-ebpf-will-revolutionize-programming-in-linux/)
are mainstream, `untangle` only expose the asynchronous support of BSD
sockets from the kernel Linux.

Both TCP and UDP sockets are supported. In particular, Scheme's port
interface is not supported by this library. `untangle` high-level
interface expose generators and accumulators to respectively read from
and write into sockets.

`untangle` can interop with POSIX threads. You may execute of
CPU-bound procedures or other blocking procedure in POSIX threads,
possibly relying on a pool of threads.

`untangle` use channels to allow unidirectional communication between
green threads, between green threads and POSIX threads, and among
POSIX threads.

On top of `untangle` is implemented the HTTP library `hyperserver`.

This library exposes six new types with constructors and predicates:

- `make-untangle`, and `untangle?`
- `make-untangle-channel` and `untangle-channel?`
- `make-untangle-tcp-client-socket` and `untangle-tcp-client-socket?`
- `make-untangle-tcp-server-socket` and `untangle-tcp-server-socket?`
- `make-untangle-udp-unicast` and `untangle-udp-unicast?`
- `make-untangle-udp-broadcast` and `untangle-udp-broadcast?`

Also, `untangle-accept` will produce an object that satisfy the
predicate `untangle-connection-socket?`.

- related: http://cml.cs.uchicago.edu/pages/cml.html
- related: http://mumble.net/~campbell/darcs/scheme-cml/
- related: https://docs.python.org/fr/3.9/library/asyncio.html
- related: https://github.com/polytypic/poc.cml
- related: https://github.com/wingo/fibers
- related: https://gitlab.com/weinholt/loko/-/blob/master/runtime/fibers.sls
- related: https://www.microsoft.com/en-us/research/wp-content/uploads/2009/09/Parallel-Concurrent-ML.pdf

### `(untangle-accept socket)`

need non-blocking.

### `(untangle-bind socket)`

### `(untangle-listen socket)`

### `(untangle-socket-generator socket)`

need non-blocking.

### `(untangle-socket-accumulator socket)`

need non-blocking.
