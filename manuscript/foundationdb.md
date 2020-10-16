# `(foundationdb)`

This library expose low-level procedures to interact with the official
FoundationDB client library.

## Ordered Key-Value Store

## Subspace

## `(fdb-error code)`

Return a string explaning the error associated with `CODE`.

## `(fdb-setup-network)`

## `(fdb-run-network)`

## `(fdb-stop-network)`

## `(fdb-future-cancel future)`

## `(fdb-future-release-memory future)`

## `(fdb-future-destroy future)`

## `(fdb-future-block-until-ready future)`

## `(fdb-future-ready? future)`

## `(fdb-future-callback proc)`

Create a future's callback from `PROC`. The return object can be
passed as second argument to `fdb-future-set-callback`.

## `(fdb-future-set-callback future proc)`

## `(fdb-future-get-error future)`

## `(fdb-future-get-key future)`

## `(fdb-future-get-value future)`

## `(fdb-future-get-range future)`

Returns a list.

## `(fdb-create-database cluster-file)`

## `(fdb-database-destroy database)`

## `(fdb-database-create-transaction database)`

## `(fdb-transaction-destroy transaction)`

## `(fdb-transaction-cancel transaction)`

## `(fdb-transaction-get transaction key snapshot?)`

## `(fdb-transaction-get-range transaction begin-key begin-or-equal? begin-offset end-key end-or-equal? end-offset limit target-bytes mode iteration snapshot? reverse?)`

## `(fdb-transaction-set transaction key value)`

## `(fdb-transaction-atomic-op transaction key param operation-type)`

## `(fdb-transaction-clear transaction key)`

## `(fdb-transaction-clear-range transaction start end)`

## `(fdb-transaction-commit transaction)`

## `(fdb-transaction-on-error transaction)`

# `(foundationdb pack)`

## `(pack object) → bytevector`

Serialize `OBJECT` into a bytevector. `OBJECT` can be:

- a boolean
- an integer
- an inexact real number
- a symbol
- a string
- a bytevector

Or a composition of those types inside lists or vectors.

The `pack` procedure will preserve natural ordering between objects of
the same type. The order between objects of different types is
determinist but unspecified. Inexact real numbers are always encoded
as double precision floats.

## `(unpack bytevector) → object`

This the the inverse operation of `pack`. It will deserialize
`bytevector` into a scheme object.

# `(foundationdb entangle)`

# `(foundationdb blob-store)`

# `(foundationdb rstore)`

# `(foundationdb bstore)`

# `(foundationdb nstore)`

## `(make-nstore subspace n)`

## `(nstore-add! nstore txn vector)`

## `(nstore-delete! nstore txn vector)`

## `(nstore-ask? nstore txn vector)`

## `(nstore-query nstore txn pattern seed)`

# `(foundationdb vnstore)`

# `(foundationdb xzstore)`

# `(foundationdb gstore)`

A property graphdb can be built on top of `nstore` prolly with
`n=3`. This implementation rely on another approach that should be
faster when you do not need to query properties, and only need to
traverse the graph.

## `(make-gstore subspace)`

## `(gstore-make-vertex gstore txn properties)`

## `(gstore-vertex? obj)`

## `(gstore-vertex-uid vertex)`

## `(gstore-vertex-properties vertex)`

## `(gstore-make-edge gstore txn start label end properties)`

## `(gstore-edge? obj)`

## `(gstore-edge-uid edge)`

## `(gstore-edge-start edge)`

## `(gstore-edge-end edge)`

## `(gstore-edge-properties edge)`

# `(foundationdb hstore)`

# `(foundationdb ixstore)`
