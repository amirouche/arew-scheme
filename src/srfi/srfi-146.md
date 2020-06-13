
## `(arew srfi srfi-146)`

This library is based on [SRFI-146](https://srfi.schemers.org/srfi-146/).

### Abstract

Mappings are finite sets of associations, where each association is a
pair consisting of a key and an arbitrary Scheme value. The keys are
elements of a suitable domain. Each mapping holds no more than one
association with the same key. The fundamental mapping operation is
retrieving the value of an association stored in the mapping when the
key is given.

### Reference

#### mapping

##### Construtors

###### `(mapping comparator [key value] ...)`

Returns a newly allocated mapping. The comparator argument is used to
control and distinguish the keys of the mapping. The args alternate
between keys and values and are used to initialize the mapping. In
particular, the number of args has to be even. Earlier associations
with equal keys take precedence over later arguments.

###### `(mapping-unfold stop? mapper successor seed comparator)`

Create a newly allocated mapping as if by mapping using comparator. If
the result of applying the predicate stop? to seed is true, return the
mapping. Otherwise, apply the procedure mapper to seed. Mapper returns
two values which are added to the mapping as the key and the value,
respectively. Then get a new seed by applying the procedure successor
to seed, and repeat this algorithm. Associations earlier in the list
take precedence over those that come later.

###### `(mapping/ordered)`

###### `(mapping-unfold/ordered`

These are the same as mapping and mapping-unfold, except that it is an
 error if the keys are not in order, and they may be more efficient.

##### Predicates

###### `(mapping? obj)`

Returns #t if obj is a mapping, and #f otherwise.

###### `(mapping-contains? mapping key)`

Returns #t if key is the key of an association of mapping and #f otherwise.

###### `(mapping-empty? mapping)`

Returns #t if mapping has no associations and #f otherwise.

###### `(mapping-disjoint? mapping1 mapping2)`

Returns #t if mapping1 and mapping2 have no keys in common and #f otherwise.

##### Accessors

The following three procedures, given a key, return the corresponding value.

###### `(mapping-ref mapping key [failure [success]])`

Extracts the value associated to key in the mapping mapping, invokes
the procedure success in tail context on it, and returns its result;
if success is not provided, then the value itself is returned. If key
is not contained in mapping and failure is supplied, then failure is
invoked in tail context on no arguments and its values are
returned. Otherwise, it is an error.

###### `(mapping-ref/default mapping key default)`



###### `(mapping-key-comparator mapping)`

Returns the comparator used to compare the keys of the mapping mapping.

##### Updaters

###### `(mapping-adjoin mapping arg ...)`

The mapping-adjoin procedure returns a newly allocated mapping that
uses the same comparator as the mapping mapping and contains all the
associations of mapping, and in addition new associations by
processing the arguments from left to right. The args alternate
between keys and values. Whenever there is a previous association for
a key, the previous association prevails and the new association is
skipped. It is an error to add an association to mapping whose key
that does not return #t when passed to the type test procedure of the
comparator.

###### `(mapping-adjoin! mapping arg ...)`

The mapping-adjoin! procedure is the same as mapping-adjoin, except
that it is permitted to mutate and return the mapping argument rather
than allocating a new mapping.

###### `(mapping-set mapping arg ...)`

The mapping-set procedure returns a newly allocated mapping that uses
the same comparator as the mapping mapping and contains all the
associations of mapping, and in addition new associations by
processing the arguments from left to right. The args alternate
between keys and values. Whenever there is a previous association for
a key, it is deleted. It is an error to add an association to mapping
whose key that does not return #t when passed to the type test
procedure of the comparator.

###### `(mapping-set! mapping arg ...)`

The mapping-set! procedure is the same as mapping-set, except that it
is permitted to mutate and return the mapping argument rather than
allocating a new mapping.

###### `(mapping-replace mapping key value)`

The mapping-replace procedure returns a newly allocated mapping that
uses the same comparator as the mapping mapping and contains all the
associations of mapping except as follows: If key is equal (in the
sense of mapping's comparator) to an existing key of mapping, then the
association for that key is omitted and replaced the association
defined by the pair key and value. If there is no such key in mapping,
then mapping is returned unchanged.

###### `(mapping-replace! mapping key value)`

The mapping-replace! procedure is the same as mapping-replace, except
that it is permitted to mutate and return the mapping argument rather
than allocating a new mapping.

###### `(mapping-delete mapping key ...)`

###### `(mapping-delete! mapping key ...)`

###### `(mapping-delete-all mapping key-list)`

###### `(mapping-delete-all! mapping key-list)`

The mapping-delete procedure returns a newly allocated mapping
containing all the associations of the mapping mapping except for any
whose keys are equal (in the sense of mapping's comparator) to one or
more of the keys. Any key that is not equal to some key of the mapping
is ignored.

The mapping-delete! procedure is the same as mapping-delete, except
that it is permitted to mutate and return the mapping argument rather
than allocating a new mapping.

The mapping-delete-all and mapping-delete-all! procedures are the same
as mapping-delete and mapping-delete!, respectively, except that they
accept a single argument which is a list of keys whose associations
are to be deleted.

###### `(mapping-intern mapping key failure)`

Extracts the value associated to key in the mapping mapping, and
returns mapping and the value as two values. If key is not contained
in mapping, failure is invoked on no arguments. The procedure then
returns two values, a newly allocated mapping that uses the same
comparator as the mapping and contains all the associations of
mapping, and in addition a new association mapping key to the result
of invoking failure, and the result of invoking failure.

###### `(mapping-intern! mapping key failure)`

The mapping-intern! procedure is the same as mapping-intern, except
that it is permitted to mutate and return the mapping argument as its
first value rather than allocating a new mapping.

###### `(mapping-update mapping key updater [failure [success]])`

TODO

###### `(mapping-update! mapping key updater [failure [success]])`

The mapping-update! procedure is the same as mapping-update, except
that it is permitted to mutate and return the mapping argument rather
than allocating a new mapping.

###### `(mapping-update/default mapping key updater default)`

TODO

###### `(mapping-update!/default mapping key updater default)`

The mapping-update!/default procedure is the same as
mapping-update/default, except that it is permitted to mutate and
return the mapping argument rather than allocating a new mapping.

###### `(mapping-pop mapping [failure])`

The mapping-pop procedure exported from (srfi 146) chooses the
association with the least key from mapping and returns three values,
a newly allocated mapping that uses the same comparator as mapping and
contains all associations of mapping except the chosen one, and the
key and the value of the chosen association. If mapping contains no
association and failure is supplied, then failure is invoked in tail
context on no arguments and its values returned. Otherwise, it is an
error.

###### `(mapping-pop! mapping [failure])`

The mapping-pop! procedure is the same as mapping-pop, except that it
is permitted to mutate and return the mapping argument rather than
allocating a new mapping.

###### `(mapping-search mapping key failure success)`

The mapping mapping is searched in order (that is in the order of the
stored keys) for an association with key key. If it is not found, then
the failure procedure is tail-called with two continuation arguments,
insert and ignore, and is expected to tail-call one of them. If an
association with key key is found, then the success procedure is
tail-called with the matching key of mapping, the associated value,
and two continuations, update and remove, and is expected to tail-call
one of them.

It is an error if the continuation arguments are invoked, but not in
tail position in the failure and success procedures. It is also an
error if the failure and success procedures return to their implicit
continuation without invoking one of their continuation arguments.

The effects of the continuations are as follows (where obj is any
Scheme object):

- Invoking (insert value obj) causes a mapping to be newly allocated
  that uses the same comparator as the mapping mapping and contains
  all the associations of mapping, and in addition a new association
  mapping key to value.

- Invoking (ignore obj) has no effects; in particular, no new mapping
  is allocated (but see below).

- Invoking (update new-key new-value obj) causes a mapping to be newly
  allocated that uses the same comparator as the mapping and contains
  all the associations of mapping, except for the association with key
  key, which is replaced by a new association mapping new-key to
  new-value.

- Invoking (remove obj) causes a mapping to be newly allocated that
  uses the same comparator as the mapping and contains all the
  associations of mapping, except for the association with key key.

In all cases, two values are returned: the possibly newly allocated
mapping and obj.

###### `(mapping-search! mapping key failure success)`

The mapping-search! procedure is the same as mapping-search, except
that it is permitted to mutate and return the mapping argument rather
than allocating a new mapping.

##### The whole mapping

###### `(mapping-size mapping)`

Returns the number of associations in mapping as an exact integer.

###### `(mapping-find predicate mapping failure)`

Returns the association with the least key of the mapping mapping
consisting of a key and value as two values such that predicate
returns a true value when invoked with key and value as arguments, or
the result of tail-calling failure with no arguments if there is
none. There are no guarantees how many times and with which keys and
values predicate is invoked.

###### `(mapping-count predicate mapping)`

Returns the number of associations of the mapping mapping that satisfy
predicate (in the sense of mapping-find) as an exact integer. There
are no guarantees how many times and with which keys and values
predicate is invoked.

###### `(mapping-any? predicate mapping)`

Returns #t if any association of the mapping mapping satisfies
predicate (in the sense of mapping-find), or #f otherwise. There are
no guarantees how many times and with which keys and values predicate
is invoked.

###### `(mapping-every? predicate mapping)`

Returns #t if every association of the mapping mapping satisfies
predicate (in the sense of mapping-find), or #f otherwise. There are
no guarantees how many times and with which keys and values predicate
is invoked.

###### `(mapping-keys mapping)`

Returns a newly allocated list of all the keys in increasing order in
the mapping mapping.

###### `(mapping-values mapping)`

Returns a newly allocated list of all the values in increasing order of the keys in the mapping mapping.

###### `(mapping-entries mapping)`

Returns two values, a newly allocated list of all the keys in the
mapping mapping, and a newly allocated list of all the values in the
mapping mapping in increasing order of the keys.

##### Mapping and folding

###### `(mapping-map proc comparator mapping)`

Applies proc, which returns two values, on two arguments, the key and
value of each association of mapping in increasing order of the keys
and returns a newly allocated mapping that uses the comparator
comparator, and which contains the results of the applications
inserted as keys and values.

###### `(mapping-map->list proc mapping)`

Calls proc for every association in increasing order of the keys in
the mapping mapping with two arguments: the key of the association and
the value of the association. The values returned by the invocations
of proc are accumulated into a list, which is returned.

###### `(mapping-for-each proc mapping)`

Invokes proc for every association in the mapping mapping in
increasing order of the keys, discarding the returned values, with two
arguments: the key of the association and the value of the
association. Returns an unspecified value.

###### `(mapping-fold proc nil mapping)`

Invokes proc for each association of the mapping mapping in increasing
order of the keys with three arguments: the key of the association,
the value of the association, and an accumulated result of the
previous invocation. For the first invocation, nil is used as the
third argument. Returns the result of the last invocation, or nil if
there was no invocation.

###### `(mapping-filter predicate mapping)`

Returns a newly allocated mapping with the same comparator as the
mapping mapping, containing just the associations of mapping that
satisfy predicate (in the sense of mapping-find).

###### `(mapping-filter! predicate mapping)`

A linear update procedure that returns a mapping containing just the
associations of mapping that satisfy predicate.

###### `(mapping-remove predicate mapping)`

Returns a newly allocated mapping with the same comparator as the
mapping mapping, containing just the associations of mapping that do
not satisfy predicate (in the sense of mapping-find).

###### `(mapping-remove! predicate mapping)`

A linear update procedure that returns a mapping containing just the
associations of mapping that do not satisfy predicate.

###### `(mapping-partition predicate mapping)`

Returns two values: a newly allocated mapping with the same comparator
as the mapping mapping that contains just the associations of mapping
that satisfy predicate (in the sense of mapping-find), and another
newly allocated mapping, also with the same comparator, that contains
just the associations of mapping that do not satisfy predicate.

###### `(mapping-partition! predicate mapping)`

A linear update procedure that returns two mappings containing the
associations of mapping that do and do not, respectively, satisfy
predicate.

##### Copying and conversion

###### `(mapping-copy mapping)`

Returns a newly allocated mapping containing the associations of the
mapping mapping, and using the same comparator.

###### `(mapping->alist mapping)`

Returns a newly allocated association list containing the associations
of the mapping in increasing order of the keys. Each association in
the list is a pair whose car is the key and whose cdr is the
associated value.

###### `(alist->mapping comparator alist)`

Returns a newly allocated mapping, created as if by mapping using the
comparator comparator, that contains the associations in the list,
which consist of a pair whose car is the key and whose cdr is the
value. Associations earlier in the list take precedence over those
that come later.

###### `(alist->mapping! mapping alist)`

A linear update procedure that returns a mapping that contains the
associations of both mapping and alist. Associations in the mapping
and those earlier in the list take precedence over those that come
later.

##### Submappings

All predicates in this subsection take a comparator argument, which is
a comparator used to compare the values of the associations stored in
the mappings. Associations in the mappings are equal if their keys are
equal with mappings' key comparator and their values are equal with
the given comparator. Two mappings are equal if and only if their
associations are equal, respectively.

Note: None of these five predicates produces a total order on
mappings. In particular, mapping=?, mapping<?, and mapping>? do not
obey the trichotomy law.

###### `(mapping=? comparator mapping1 mapping2 ...)`

Returns #t if each mapping mapping contains the same associations, and
#f otherwise.

Furthermore, it is explicitly not an error if mapping=? is invoked on
mappings that do not share the same (key) comparator. In that case, #f
is returned.

###### `(mapping<? comparator mapping1 mapping2 ...)`

Returns #t if the set of associations of each mapping mapping other
than the last is a proper subset of the following mapping, and #f
otherwise.

###### `(mapping>? comparator mapping1 mapping2 ...)`

Returns #t if each mapping mapping contains the same associations, and
#f otherwise.

Furthermore, it is explicitly not an error if mapping=? is invoked on
mappings that do not share the same (key) comparator. In that case, #f
is returned.

###### `(mapping<=? comparator mapping1 mapping2 ...)`

Returns #t if the set of associations of each mapping mapping other
than the last is a subset of the following mapping, and #f otherwise.

###### `(mapping>=? comparator mapping1 mapping2 ...)`

Returns #t if the set of associations of each mapping mapping other
than the last is a superset of the following mapping, and #f
otherwise.

##### Set theory operations

###### `(mapping-union mapping1 mapping2 ...)`

###### `(mapping-intersection mapping1 mapping2 ...)`

###### `(mapping-difference mapping1 mapping2 ...)`

###### `(mapping-xor mapping1 mapping2 ...)`

Return a newly allocated mapping whose set of associations is the
union, intersection, asymmetric difference, or symmetric difference of
the sets of associations of the mappings mappings. Asymmetric
difference is extended to more than two mappings by taking the
difference between the first mapping and the union of the
others. Symmetric difference is not extended beyond two mappings. When
comparing associations, only the keys are compared. In case of
duplicate keys (in the sense of the mappings comparators),
associations in the result mapping are drawn from the first mapping in
which they appear.

###### `(mapping-union! mapping1 mapping2 ...)`

###### `(mapping-intersection! mapping1 mapping2 ...)`

###### `(mapping-difference! mapping1 mapping2 ...)`

###### `(mapping-xor! mapping1 mapping2 ...)`

These procedures are the linear update analogs of the corresponding
pure functional procedures above.

##### Additional procedures

###### `(mapping-min-key mapping)`

###### `(mapping-max-key mapping)`

Returns the least/greatest key contained in the mapping mapping. It is
an error for mapping to be empty.

###### `(mapping-min-value mapping)`

###### `(mapping-max-value mapping)`

Returns the value associated with the least/greatest key contained in
the mapping mapping. It is an error for mapping to be empty.

###### `(mapping-min-entry mapping)`

###### `(mapping-max-entry mapping)`

Returns the entry associated with the least/greatest key contained in
the mapping mapping as two values, the key and its associated
value. It is an error for mapping to be empty.

###### `(mapping-key-predecessor mapping obj failure)`

###### `(mapping-key-successor mapping obj failure)`

Returns the key contained in the mapping mapping that immediately
precedes/succeeds obj in the mapping's order of keys. If no such key
is contained in mapping (because obj is the minimum/maximum key, or
because mapping is empty), returns the result of tail-calling the
thunk failure.

###### `(mapping-range= mapping obj)`

###### `(mapping-range< mapping obj)`

###### `(mapping-range> mapping obj)`

###### `(mapping-range<= mapping obj)`

###### `(mapping-range>= mapping obj)`

Returns a mapping containing only the associations of the mapping
whose keys are equal to, less than, greater than, less than or equal
to, or greater than or equal to obj.

###### `(mapping-range=! mapping obj)`

###### `(mapping-range<! mapping obj)`

###### `(mapping-range>! mapping obj)`

###### `(mapping-range<=! mapping obj)`

###### `(mapping-range>=! mapping obj)`

Linear update procedures returning a mapping containing only the
associations of the mapping whose keys are equal to, less than,
greater than, less than or equal to, or greater than or equal to obj.

###### `(mapping-split mapping obj)`

Returns five values, equivalent to the results of invoking
(mapping-range< mapping obj), (mapping-range<= mapping obj),
(mapping-range= mapping obj), (mapping-range>= mapping obj), and
(mapping-range> mapping obj), but may be more efficient.

###### `(mapping-split! mapping obj)`

The mapping-split! procedure is the same as mapping-split, except that
it is permitted to mutate and return the mapping rather than
allocating a new mapping.

###### `(mapping-catenate comparator mapping1 key value mapping2)`

Returns a newly allocated mapping using the comparator comparator
whose set of associations is the union of the sets of associations of
the mapping mapping1, the association mapping key to value, and the
associations of mapping2. It is an error if the keys contained in
mapping1 in their natural order, the key key, and the keys contained
in mapping2 in their natural order (in that order) do not form a
strictly monotone sequence with respect to the ordering of comparator.

###### `(mapping-catenate! comparator mapping1 key value mapping2)`

Returns a newly allocated mapping using the comparator comparator
whose set of associations is the union of the sets of associations of
the mapping mapping1, the association mapping key to value, and the
associations of mapping2. It is an error if the keys contained in
mapping1 in their natural order, the key key, and the keys contained
in mapping2 in their natural order (in that order) do not form a
strictly monotone sequence with respect to the ordering of comparator.

###### `(mapping-map/monotone proc comparator mapping)`

Equivalent to (mapping-map proc comparator mapping), but it is an
error if proc does not induce a strictly monotone mapping between the
keys with respect to the ordering of the comparator of mapping and the
ordering of comparator. Maybe be implemented more efficiently than
mapping-map.

###### `(mapping-map/monotone! proc comparator mapping)`

The mapping-map/monotone! procedure is the same as
mapping-map/monotone, except that it is permitted to mutate and return
the mapping argument rather than allocating a new mapping.

###### `(mapping-fold/reverse proc nil mapping)`

Equivalent to (mapping-fold proc nil mapping) except that the
associations are processed in reverse order with respect to the
natural ordering of the keys.

##### Comparators

###### `(comparator? obj)`

Type predicate for comparators as exported by `(scheme comparator)`.

###### `mapping-comparator`

mapping-comparator is constructed by invoking make-mapping-comparator
on (make-default-comparator).

###### `(make-mapping-comparator comparator)`

Returns a comparator for mappings that is compatible with the equality
predicate (mapping=? comparator mapping1 mapping2). If
make-mapping-comparator is imported from (srfi 146), it provides a
(partial) ordering predicate that is applicable to pairs of mappings
with the same (key) comparator. If (make-hashmap-comparator) is
imported from (srfi 146 hash), it provides an implementation-dependent
hash function.

If make-mapping-comparator is imported from (srfi 146), the
lexicographic ordering with respect to the keys (and, in case a
tiebreak is necessary, with respect to the ordering of the values) is
used for mappings sharing a comparator.

The existence of comparators returned by make-mapping-comparator
allows mappings whose keys are mappings themselves, and it allows to
compare mappings whose values are mappings.

#### hash mapping

#### Construtors

##### `(hashmap comparator [key value] ...)`

Returns a newly allocated hashmap. The comparator argument is used to
control and distinguish the keys of the hashmap. The args alternate
between keys and values and are used to initialize the hashmap. In
particular, the number of args has to be even. Earlier associations
with equal keys take precedence over later arguments.

##### `(hashmap-unfold stop? mapper successor seed comparator)`

Create a newly allocated hashmap as if by hashmap using comparator. If
the result of applying the predicate stop? to seed is true, return the
hashmap. Otherwise, apply the procedure mapper to seed. Mapper returns
two values which are added to the hashmap as the key and the value,
respectively. Then get a new seed by applying the procedure successor
to seed, and repeat this algorithm. Associations earlier in the list
take precedence over those that come later.

#### Predicates

##### `(hashmap? obj)`

Returns #t if obj is a hashmap, and #f otherwise.

##### `(hashmap-contains? hashmap key)`

Returns #t if key is the key of an association of hashmap and #f otherwise.

##### `(hashmap-empty? hashmap)`

Returns #t if hashmap has no associations and #f otherwise.

##### `(hashmap-disjoint? hashmap1 hashmap2)`

Returns #t if hashmap1 and hashmap2 have no keys in common and #f otherwise.

#### Accessors

The following three procedures, given a key, return the corresponding value.

##### `(hashmap-ref hashmap key [failure [success]])`

Extracts the value associated to key in the hashmap hashmap, invokes
the procedure success in tail context on it, and returns its result;
if success is not provided, then the value itself is returned. If key
is not contained in hashmap and failure is supplied, then failure is
invoked in tail context on no arguments and its values are
returned. Otherwise, it is an error.

##### `(hashmap-ref/default hashmap key default)`

##### `(hashmap-key-comparator hashmap)`

Returns the comparator used to compare the keys of the hashmap hashmap.

#### Updaters

##### `(hashmap-adjoin hashmap arg ...)`

The hashmap-adjoin procedure returns a newly allocated hashmap that
uses the same comparator as the hashmap hashmap and contains all the
associations of hashmap, and in addition new associations by
processing the arguments from left to right. The args alternate
between keys and values. Whenever there is a previous association for
a key, the previous association prevails and the new association is
skipped. It is an error to add an association to hashmap whose key
that does not return #t when passed to the type test procedure of the
comparator.

##### `(hashmap-adjoin! hashmap arg ...)`

The hashmap-adjoin! procedure is the same as hashmap-adjoin, except
that it is permitted to mutate and return the hashmap argument rather
than allocating a new hashmap.

##### `(hashmap-set hashmap arg ...)`

The hashmap-set procedure returns a newly allocated hashmap that uses
the same comparator as the hashmap hashmap and contains all the
associations of hashmap, and in addition new associations by
processing the arguments from left to right. The args alternate
between keys and values. Whenever there is a previous association for
a key, it is deleted. It is an error to add an association to hashmap
whose key that does not return #t when passed to the type test
procedure of the comparator.

##### `(hashmap-set! hashmap arg ...)`

The hashmap-set! procedure is the same as hashmap-set, except that it
is permitted to mutate and return the hashmap argument rather than
allocating a new hashmap.

##### `(hashmap-replace hashmap key value)`

The hashmap-replace procedure returns a newly allocated hashmap that
uses the same comparator as the hashmap hashmap and contains all the
associations of hashmap except as follows: If key is equal (in the
sense of hashmap's comparator) to an existing key of hashmap, then the
association for that key is omitted and replaced the association
defined by the pair key and value. If there is no such key in hashmap,
then hashmap is returned unchanged.

##### `(hashmap-replace! hashmap key value)`

The hashmap-replace! procedure is the same as hashmap-replace, except
that it is permitted to mutate and return the hashmap argument rather
than allocating a new hashmap.

##### `(hashmap-delete hashmap key ...)`

##### `(hashmap-delete! hashmap key ...)`

##### `(hashmap-delete-all hashmap key-list)`

##### `(hashmap-delete-all! hashmap key-list)`

The hashmap-delete procedure returns a newly allocated hashmap
containing all the associations of the hashmap hashmap except for any
whose keys are equal (in the sense of hashmap's comparator) to one or
more of the keys. Any key that is not equal to some key of the hashmap
is ignored.

The hashmap-delete! procedure is the same as hashmap-delete, except
that it is permitted to mutate and return the hashmap argument rather
than allocating a new hashmap.

The hashmap-delete-all and hashmap-delete-all! procedures are the same
as hashmap-delete and hashmap-delete!, respectively, except that they
accept a single argument which is a list of keys whose associations
are to be deleted.

##### `(hashmap-intern hashmap key failure)`

Extracts the value associated to key in the hashmap hashmap, and
returns hashmap and the value as two values. If key is not contained
in hashmap, failure is invoked on no arguments. The procedure then
returns two values, a newly allocated hashmap that uses the same
comparator as the hashmap and contains all the associations of
hashmap, and in addition a new association hashmap key to the result
of invoking failure, and the result of invoking failure.

##### `(hashmap-intern! hashmap key failure)`

The hashmap-intern! procedure is the same as hashmap-intern, except
that it is permitted to mutate and return the hashmap argument as its
first value rather than allocating a new hashmap.

##### `(hashmap-update hashmap key updater [failure [success]])`

TODO

##### `(hashmap-update! hashmap key updater [failure [success]])`

The hashmap-update! procedure is the same as hashmap-update, except
that it is permitted to mutate and return the hashmap argument rather
than allocating a new hashmap.

##### `(hashmap-update/default hashmap key updater default)`

TODO

##### `(hashmap-update!/default hashmap key updater default)`

The hashmap-update!/default procedure is the same as
hashmap-update/default, except that it is permitted to mutate and
return the hashmap argument rather than allocating a new hashmap.

##### `(hashmap-pop hashmap [failure])`

The hashmap-pop procedure exported from (srfi 146) chooses the
association with the least key from hashmap and returns three values,
a newly allocated hashmap that uses the same comparator as hashmap and
contains all associations of hashmap except the chosen one, and the
key and the value of the chosen association. If hashmap contains no
association and failure is supplied, then failure is invoked in tail
context on no arguments and its values returned. Otherwise, it is an
error.

##### `(hashmap-pop! hashmap [failure])`

The hashmap-pop! procedure is the same as hashmap-pop, except that it
is permitted to mutate and return the hashmap argument rather than
allocating a new hashmap.

##### `(hashmap-search hashmap key failure success)`

The hashmap hashmap is searched in order (that is in the order of the
stored keys) for an association with key key. If it is not found, then
the failure procedure is tail-called with two continuation arguments,
insert and ignore, and is expected to tail-call one of them. If an
association with key key is found, then the success procedure is
tail-called with the matching key of hashmap, the associated value,
and two continuations, update and remove, and is expected to tail-call
one of them.

It is an error if the continuation arguments are invoked, but not in
tail position in the failure and success procedures. It is also an
error if the failure and success procedures return to their implicit
continuation without invoking one of their continuation arguments.

The effects of the continuations are as follows (where obj is any
Scheme object):

- Invoking (insert value obj) causes a hashmap to be newly allocated
  that uses the same comparator as the hashmap hashmap and contains
  all the associations of hashmap, and in addition a new association
  hashmap key to value.

- Invoking (ignore obj) has no effects; in particular, no new hashmap
  is allocated (but see below).

- Invoking (update new-key new-value obj) causes a hashmap to be newly
  allocated that uses the same comparator as the hashmap and contains
  all the associations of hashmap, except for the association with key
  key, which is replaced by a new association hashmap new-key to
  new-value.

- Invoking (remove obj) causes a hashmap to be newly allocated that
  uses the same comparator as the hashmap and contains all the
  associations of hashmap, except for the association with key key.

In all cases, two values are returned: the possibly newly allocated
hashmap and obj.

##### `(hashmap-search! hashmap key failure success)`

The hashmap-search! procedure is the same as hashmap-search, except
that it is permitted to mutate and return the hashmap argument rather
than allocating a new hashmap.

#### The whole hashmap

##### `(hashmap-size hashmap)`

Returns the number of associations in hashmap as an exact integer.

##### `(hashmap-find predicate hashmap failure)`

Returns the association with the least key of the hashmap hashmap
consisting of a key and value as two values such that predicate
returns a true value when invoked with key and value as arguments, or
the result of tail-calling failure with no arguments if there is
none. There are no guarantees how many times and with which keys and
values predicate is invoked.

##### `(hashmap-count predicate hashmap)`

Returns the number of associations of the hashmap hashmap that satisfy
predicate (in the sense of hashmap-find) as an exact integer. There
are no guarantees how many times and with which keys and values
predicate is invoked.

##### `(hashmap-any? predicate hashmap)`

Returns #t if any association of the hashmap hashmap satisfies
predicate (in the sense of hashmap-find), or #f otherwise. There are
no guarantees how many times and with which keys and values predicate
is invoked.

##### `(hashmap-every? predicate hashmap)`

Returns #t if every association of the hashmap hashmap satisfies
predicate (in the sense of hashmap-find), or #f otherwise. There are
no guarantees how many times and with which keys and values predicate
is invoked.

##### `(hashmap-keys hashmap)`

Returns a newly allocated list of all the keys in increasing order in
the hashmap hashmap.

##### `(hashmap-values hashmap)`

Returns a newly allocated list of all the values in increasing order of the keys in the hashmap hashmap.

##### `(hashmap-entries hashmap)`

Returns two values, a newly allocated list of all the keys in the
hashmap hashmap, and a newly allocated list of all the values in the
hashmap hashmap in increasing order of the keys.

#### Hashmap and folding

##### `(hashmap-map proc comparator hashmap)`

Applies proc, which returns two values, on two arguments, the key and
value of each association of hashmap in increasing order of the keys
and returns a newly allocated hashmap that uses the comparator
comparator, and which contains the results of the applications
inserted as keys and values.

##### `(hashmap-map->list proc hashmap)`

Calls proc for every association in increasing order of the keys in
the hashmap hashmap with two arguments: the key of the association and
the value of the association. The values returned by the invocations
of proc are accumulated into a list, which is returned.

##### `(hashmap-for-each proc hashmap)`

Invokes proc for every association in the hashmap hashmap in
increasing order of the keys, discarding the returned values, with two
arguments: the key of the association and the value of the
association. Returns an unspecified value.

##### `(hashmap-fold proc nil hashmap)`

Invokes proc for each association of the hashmap hashmap in increasing
order of the keys with three arguments: the key of the association,
the value of the association, and an accumulated result of the
previous invocation. For the first invocation, nil is used as the
third argument. Returns the result of the last invocation, or nil if
there was no invocation.

##### `(hashmap-filter predicate hashmap)`

Returns a newly allocated hashmap with the same comparator as the
hashmap hashmap, containing just the associations of hashmap that
satisfy predicate (in the sense of hashmap-find).

##### `(hashmap-filter! predicate hashmap)`

A linear update procedure that returns a hashmap containing just the
associations of hashmap that satisfy predicate.

##### `(hashmap-remove predicate hashmap)`

Returns a newly allocated hashmap with the same comparator as the
hashmap hashmap, containing just the associations of hashmap that do
not satisfy predicate (in the sense of hashmap-find).

##### `(hashmap-remove! predicate hashmap)`

A linear update procedure that returns a hashmap containing just the
associations of hashmap that do not satisfy predicate.

##### `(hashmap-partition predicate hashmap)`

Returns two values: a newly allocated hashmap with the same comparator
as the hashmap hashmap that contains just the associations of hashmap
that satisfy predicate (in the sense of hashmap-find), and another
newly allocated hashmap, also with the same comparator, that contains
just the associations of hashmap that do not satisfy predicate.

##### `(hashmap-partition! predicate hashmap)`

A linear update procedure that returns two hashmaps containing the
associations of hashmap that do and do not, respectively, satisfy
predicate.

#### Copying and conversion

##### `(hashmap-copy hashmap)`

Returns a newly allocated hashmap containing the associations of the
hashmap hashmap, and using the same comparator.

##### `(hashmap->alist hashmap)`

Returns a newly allocated association list containing the associations
of the hashmap in increasing order of the keys. Each association in
the list is a pair whose car is the key and whose cdr is the
associated value.

##### `(alist->hashmap comparator alist)`

Returns a newly allocated hashmap, created as if by hashmap using the
comparator comparator, that contains the associations in the list,
which consist of a pair whose car is the key and whose cdr is the
value. Associations earlier in the list take precedence over those
that come later.

##### `(alist->hashmap! hashmap alist)`

A linear update procedure that returns a hashmap that contains the
associations of both hashmap and alist. Associations in the hashmap
and those earlier in the list take precedence over those that come
later.

#### Set theory operations

##### `(hashmap-union hashmap1 hashmap2 ...)`

##### `(hashmap-intersection hashmap1 hashmap2 ...)`

##### `(hashmap-difference hashmap1 hashmap2 ...)`

##### `(hashmap-xor hashmap1 hashmap2 ...)`

Return a newly allocated hashmap whose set of associations is the
union, intersection, asymmetric difference, or symmetric difference of
the sets of associations of the hashmaps hashmaps. Asymmetric
difference is extended to more than two hashmaps by taking the
difference between the first hashmap and the union of the
others. Symmetric difference is not extended beyond two hashmaps. When
comparing associations, only the keys are compared. In case of
duplicate keys (in the sense of the hashmaps comparators),
associations in the result hashmap are drawn from the first hashmap in
which they appear.

##### `(hashmap-union! hashmap1 hashmap2 ...)`

##### `(hashmap-intersection! hashmap1 hashmap2 ...)`

##### `(hashmap-difference! hashmap1 hashmap2 ...)`

##### `(hashmap-xor! hashmap1 hashmap2 ...)`

These procedures are the linear update analogs of the corresponding
pure functional procedures above.

#### Comparators

##### `(comparator? obj)`

Type predicate for comparators as exported by `(scheme comparator)`.

##### `hashmap-comparator`

hashmap-comparator is constructed by invoking make-hashmap-comparator
on (make-default-comparator).

##### `(make-hashmap-comparator comparator)`

Returns a comparator for hashmaps that is compatible with the equality
predicate (hashmap=? comparator hashmap1 hashmap2). If
make-hashmap-comparator is imported from (srfi 146), it provides a
(partial) ordering predicate that is applicable to pairs of hashmaps
with the same (key) comparator. If (make-hashmap-comparator) is
imported from (srfi 146 hash), it provides an implementation-dependent
hash function.

If make-hashmap-comparator is imported from (srfi 146), the
lexicographic ordering with respect to the keys (and, in case a
tiebreak is necessary, with respect to the ordering of the values) is
used for hashmaps sharing a comparator.

The existence of comparators returned by make-hashmap-comparator
allows hashmaps whose keys are hashmaps themselves, and it allows to
compare hashmaps whose values are hashmaps.
