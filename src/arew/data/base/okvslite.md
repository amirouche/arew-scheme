
## `(arew data base okvslite)`

okvslite is an embedded database library for key-value data, roughly
similar in scope to Berkeley DB, LevelDB or KyotoCabinet. Both keys
and values are specified and stored as byte arrays. Duplicate keys are
not supported. Keys are always sorted in `memcmp()` order. okvslite supports
the following operations for the manipulation and query of database
data:

- Writing a new key and value into the database.

- Deleting an existing key from the database.

- Deleting a range of keys from the database.

- Querying the database for a specific key.

- Iterating through a range of database keys (either forwards or backwards).

Other salient features are:

- A single-writer/multiple-reader MVCC based transactional concurrency
  model. SQL style nested sub-transactions are supported. Clients may
  concurrently access a single okvslite database from within a single
  process or multiple application processes.

- An entire database is stored in a single file on disk.

- Data durability in the face of application or power
  failure. okvslite may optionally use a write-ahead log file when
  writing to the database to ensure committed transactions are not
  lost if an application or power failure occurs.

- An API that allows external data compression and/or encryption
  routines to be used to create and access compressed and/or encrypted
  databases

The above documentation was taken and adapted from the [original
sqlite4 documentation about LSM (dubbed
okvslite)](https://sqlite.org/src4/doc/trunk/www/lsmusr.wiki).

### `(okvs-begin db level)`

`okvs-begin` will start transactions or sub-transactions.  `LEVEL`
must be a strictly positive number (that is greater than `1`).  To
open a top-level transaction, pass `1` as level, pass `2` to open a
subtransaction inside the top-level transaction. `0` is no-op.

### `(okvslite-close db)`

Close database `DB`.

### `(okvslite-commit db level)`

Commit transactions.  A successful call to `okvslite-commit` will
ensure that there are at most `LEVEL` nested transactions open.  To
commit a top-level transaction, pass `0`. To commit all
sub-transactions inside the top-level transaction, pass `1`.

### `(okvslite-config db config value)`

Configure the database.

### `(okvslite-cursor-close cursor)`

Close a cursor.

### `(okvslite-cursor-first cursor)`

...

### `(okvslite-cursor-key cursor)`

...

### `(okvslite-cursor-last cursor)`

...

### `(okvslite-cursor-next cursor)`

...

### `(okvslite-cursor-open db)`

...

### `(okvslite-cursor-prev cursor)`

...

### `(okvslite-cursor-seek cursor key strategy)`

...

### `(okvslite-cursor-valid? cursor)`

...

### `(okvslite-cursor-value cursor)`

...

### `(okvslite-delete db key)`

...

### `(okvslite-insert db key value)`

...

### `(okvslite-new)`

Create a new database connection handle.

### `(okvslite-open db filename)`

Connecting to a database at `FILENAME`.

### `(okvslite-rollback db level)`

Rollback transaction.
