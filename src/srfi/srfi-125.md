
## `(arew srfi srfi-125)`

This library is based on
[srfi-125](https://srfi.schemers.org/srfi-125/).

The library doesn't implement deprecated features. Application must
rely on `(scheme comparator)` to specify equal predicate and hash
function.

### Abstract

This SRFI defines an interface to hash tables, which are widely
recognized as a fundamental data structure for a wide variety of
applications. A hash table is a data structure that:

- Is disjoint from all other types.
- Provides a mapping from objects known as keys to corresponding
  objects known as values.

  - Keys may be any Scheme objects in some kinds of hash tables, but
    are restricted in other kinds.
  - Values may be any Scheme objects.

- Has no intrinsic order for the key-value associations it contains.
- Provides an equality predicate which defines when a proposed key is
  the same as an existing key. No table may contain more than one
  value for a given key.
- Provides a hash function which maps a candidate key into a
  non-negative exact integer.
- Supports mutation as the primary means of setting the contents of a
  table.
- Provides key lookup and destructive update in (expected) amortized
  constant time, provided a satisfactory hash function is available.
- Does not guarantee that whole-table operations work in the presence
  of concurrent mutation of the whole hash table (values may be safely
  mutated).

### Reference

#### Constructors

##### `(make-hash-table comparator . args)`

Returns a newly allocated hash table using `(scheme comparator)`
object `COMPARATOR`. For the time being, `ARGS` is ignored.

##### `(hash-table comparator [key value] ...)`

Returns a newly allocated hash table using `(scheme comparator)`
object `COMPARATOR`. For each pair of arguments, an association is
added to the new hash table with key as its key and value as its
value. If the same key (in the sense of the equality predicate) is
specified more than once, it is an error.

##### `(hash-table-unfold stop? mapper successor seed comparator args ...)`

Create a new hash table as if by `make-hash-table` using `comparator`
and the `args`. If the result of applying the predicate `stop?` to
`seed` is true, return the hash table. Otherwise, apply the procedure
`mapper` to `seed`. `mapper` returns two values, which are inserted
into the hash table as the key and the value respectively. Then get a
new `seed` by applying the procedure `successor` to `seed`, and repeat
this algorithm.

##### `(alist->hash-table alist comparator arg ...)`

Returns a newly allocated hash-table as if by `make-hash-table` using
`comparator` and the `args`. It is then initialized from the
associations of `alist`. Associations earlier in the list take
precedence over those that come later.

#### Predicates

##### `(hash-table? obj)`

Returns #t if obj is a hash table, and #f otherwise

##### `(hash-table-contains? hash-table key)`

Returns #t if there is any association to key in hash-table, and #f
otherwise.

##### `(hash-table-empty? hash-table)`

Returns #t if hash-table contains no associations, and #f otherwise.

##### `(hash-table=? value-comparator hash-table1 hash-table2)`

Returns #t if hash-table1 and hash-table2 have the same keys (in the
sense of their common equality predicate) and each key has the same
value (in the sense of value-comparator), and #f otherwise.

##### `(hash-table-mutable? hash-table)`

Returns #t if the hash table is mutable.

#### Accessors

The following procedures, given a key, return the corresponding value.

##### `(hash-table-ref hash-table key [failure [success]])`

Extracts the value associated to key in hash-table, invokes the
procedure success on it, and returns its result; if success is not
provided, then the value itself is returned. If key is not contained
in hash-table and failure is supplied, then failure is invoked on no
arguments and its result is returned.

##### `(hash-table-ref/default hash-table key default)`

TODO

#### Mutators

The following procedures alter the associations in a hash table either
unconditionally, or conditionally on the presence or absence of a
specified key. It is an error to add an association to a hash table
whose key does not satisfy the type test predicate of the comparator
used to create the hash table.

##### `(hash-table-set! hash-table key value ...)`

Repeatedly mutates hash-table, creating new associations in it by
processing the arguments from left to right. The args alternate
between keys and values. Whenever there is a previous association for
a key, it is deleted. It is an error if the type check procedure of
the comparator of hash-table, when invoked on a key, does not return
#t. Likewise, it is an error if a key is not a valid argument to the
equality predicate of hash-table. Returns an unspecified value.

##### `(hash-table-delete! hash-table key ...)`

Deletes any association to each key in hash-table and returns the
number of keys that had associations.

##### `(hash-table-intern! hash-table key failure)`

Effectively invokes hash-table-ref with the given arguments and
returns what it returns. If key was not found in hash-table, its value
is set to the result of calling failure.

##### `(hash-table-update! hash-table key updater [failure [success]])`

TODO:

##### `(hash-table-pop! hash-table)`

Chooses an arbitrary association from hash-table and removes it,
returning the key and value as two values.

It is an error if hash-table is empty.

##### `(hash-table-clear! hash-table)`

Delete all the associations from hash-table.

#### The whole hash table

These procedures process the associations of the hash table in an
unspecified order.

##### `(hash-table-size hash-table)`

Returns the number of associations in hash-table as an exact integer.

##### `(hash-table-keys hash-table)`

Returns a newly allocated list of all the keys in hash-table.

##### `(hash-table-values hash-table)`

Returns a newly allocated list of all the keys in hash-table.

##### `(hash-table-entries hash-table)`

Returns two values, a newly allocated list of all the keys in
hash-table and a newly allocated list of all the values in hash-table
in the corresponding order.

##### `(hash-table-find proc hash-table failure)`

For each association of hash-table, invoke proc on its key and
value. If proc returns true, then hash-table-find returns what proc
returns. If all the calls to proc return #f, return the result of
invoking the thunk failure.

##### `(hash-table-count pred hash-table)`

For each association of hash-table, invoke pred on its key and
value. Return the number of calls to pred which returned true.

#### Mapping and folding

These procedures process the associations of the hash table in an
unspecified order.

##### `(hash-table-map proc comparator hash-table)`

Returns a newly allocated hash table as if by `(make-hash-table
comparator)`. Calls `PROC` for every association in `hash-table` with
the value of the association. The key of the association and the
result of invoking `proc` are entered into the new hash table. Note
that this is not the result of lifting mapping over the domain of hash
tables, but it is considered more useful.

If comparator recognizes multiple keys in the hash-table as
equivalent, any one of such associations is taken.

##### `(hash-table-for-each proc hash-table)`

Calls proc for every association in hash-table with two arguments: the
key of the association and the value of the association. The value
returned by proc is discarded. Returns an unspecified value.

##### `(hash-table-map! proc hash-table)`

Calls proc for every association in hash-table with two arguments: the
key of the association and the value of the association. The value
returned by proc is used to update the value of the
association. Returns an unspecified value.

##### `(hash-table-map->list proc hash-table)`

Calls proc for every association in hash-table with two arguments: the
key of the association and the value of the association. The values
returned by the invocations of proc are accumulated into a list, which
is returned.

##### `(hash-table-fold proc seed hash-table)`

Calls proc for every association in hash-table with three arguments:
the key of the association, the value of the association, and an
accumulated value val. Val is seed for the first invocation of
procedure, and for subsequent invocations of proc, the returned value
of the previous invocation. The value returned by hash-table-fold is
the return value of the last invocation of proc.

##### `(hash-table-prune! proc hash-table)`

Calls proc for every association in hash-table with two arguments, the
key and the value of the association, and removes all associations
from hash-table for which proc returns true. Returns an unspecified
value.

#### Copying and conversion

##### `(hash-table-copy hash-table [mutable?])`

Returns a newly allocated hash table with the same properties and
associations as hash-table. If the second argument is present and is
true, the new hash table is mutable. Otherwise it is immutable
provided that the implementation supports immutable hash tables.

##### `(hash-table-empty-copy hash-table)`

Returns a newly allocated mutable hash table with the same properties
as hash-table, but with no associations.

##### `(hash-table->alist hash-table)`

Returns an alist with the same associations as hash-table in an
unspecified order.

#### Hash tables as sets

##### `(hash-table-union! hash-table1 hash-table2)`

Adds the associations of hash-table2 to hash-table1 and returns
hash-table1. If a key appears in both hash tables, its value is set to
the value appearing in hash-table1. Returns hash-table1.

##### `(hash-table-intersection! hash-table1 hash-table2)`

Deletes the associations from hash-table1 whose keys don't also appear
in hash-table2 and returns hash-table1.

##### `(hash-table-difference! hash-table1 hash-table2)`

Deletes the associations of hash-table1 whose keys are also present in
hash-table2 and returns hash-table1.

##### `(hash-table-xor! hash-table1 hash-table2)`

Deletes the associations of hash-table1 whose keys are also present in
hash-table2, and then adds the associations of hash-table2 whose keys
are not present in hash-table1 to hash-table1. Returns hash-table1.
