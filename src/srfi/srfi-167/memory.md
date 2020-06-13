
## `(arew srfi srfi-167 memory)`

This is based on [SRFI-167](https://srfi.schemers.org/srfi-167/).

### Abstract

TODO

### Reference

#### `(okvs-open home [config])`

TODO

#### `(okvs? obj)`

TODO

#### `(okvs-close okvs [config])`

TODO

#### `(make-default-state)`

TODO

#### `(okvs-transaction? obj)`

TODO

#### `(okvs-transaction-state transaction)`

TODO

#### `(okvs-in-transaction okvs proc [failure [success [make-sate [config]]]])`

TODO

#### `(okvs-ref okvs-or-transaction key)`

TODO

#### `(okvs-set! okvs-or-transaction key value)`

TODO

#### `(okvs-delete! okvs-or-transaction key)`

TODO

#### `(okvs-range-remove! okvs-or-transaction start-key start-include? end-key end-include?)`

TODO

#### `(okvs-range okvs-or-transaction start-key start-include? end-key end-include? [CONFIG])

TODO

#### `(okvs-prefix-range okvs-or-transaction prefix [config])`

TODO

#### `(okvs-hook-on-transaction-begin okvs)`

TODO

#### `(okvs-hook-on-transaction-commit okvs)`

TODO

#### `(make-default-engine)`

Return an engine for the current okvs implementation.
