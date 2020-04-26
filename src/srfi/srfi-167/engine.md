
## `(arew srfi srfi-167 engine)`

This is based on [SRFI-167](https://srfi.schemers.org/srfi-167/).

### Abstract

This library describe a typeclass object for ordered key-value stores.
It allows database abstraction to switch between ordered key-value
store implementation easily.

### Reference

#### `(make-engine ref set delete range-remove range prefix-range hook-on-transaction-begin hook-on-transaction-commit pack unpack)`

Return an engine record instance.

#### `(engine? obj)`

Return `#t` if `OBJ` is an engine record instance. Otherwise, it returns `#f`.

#### `(engine-ref engine)`

Return the procedure `okvs-ref`.

#### `(engine-set engine)`

Return the procedure `okvs-set!`.

#### `(engine-delete engine)`

Return the procedure `okvs-delete!`.

#### `(engine-range-remove engine)`

Return the procedure `okvs-range-remove!`.

#### `(engine-range engine)`

Return the procedure `okvs-range`.

#### `(engine-prefix-range engine)`

Return the procedure `okvs-prefix-range`.

#### `(engine-hook-on-transaction-begin engine)`

Return the procedure `okvs-hook-on-transaction-begin`.

#### `(engine-hook-on-transaction-commit engine)`

Return the procedure okvs-hook-on-transaction-commit.

#### `(engine-pack engine)`

Return a packing procedure that allows to encode some scheme types
into bytevectors preserving their natural order. The supported Scheme
types is implementation dependent.

#### `(engine-unpack engine)`

Return an unpacking procedure that will decode a bytevector encoded
with the above `pack` procedure into a Scheme object.
