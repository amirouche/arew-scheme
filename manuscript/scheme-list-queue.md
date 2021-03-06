# `(scheme list-queue)`

This library is based on [SRFI-117](https://srfi.schemers.org/srfi-117/).

List queues are mutable ordered collections that can contain any
Scheme object. Each list queue is based on an ordinary Scheme list
containing the elements of the list queue by maintaining pointers to
the first and last pairs of the list. It's cheap to add or remove
elements from the front of the list or to add elements to the back,
but not to remove elements from the back. List queues are disjoint
from other types of Scheme objects.

## `(make-list-queue list [ last ])`

Returns a newly allocated list queue containing the elements of list
in order. The result shares storage with list. If the last argument is
not provided, this operation is O(n) where n is the length of list.

However, if last is provided, make-list-queue returns a newly
allocated list queue containing the elements of the list whose first
pair is first and whose last pair is last. It is an error if the pairs
do not belong to the same list. Alternatively, both first and last can
be the empty list. In either case, the operation is O(1).

Note: To apply a non-destructive list procedure to a list queue and
return a new list queue, use (make-list-queue (proc (list-queue-list
list-queue))).

## `(list-queue element ...)`

Returns a newly allocated list queue containing the elements. This
operation is O(n) where n is the number of elements.

## `(list-queue-copy list-queue)`

Returns a newly allocated list queue containing the elements of
list-queue. This operation is O(n) where n is the length of list-queue

## `(list-queue-unfold stop? mapper successor seed [ queue ])`

Performs the following algorithm:

If the result of applying the predicate stop? to seed is true, return
queue. Otherwise, apply the procedure mapper to seed, returning a
value which is added to the front of queue. Then get a new seed by
applying the procedure successor to seed, and repeat this algorithm.

If queue is omitted, a newly allocated list queue is used.

## `(list-queue-unfold-right stop? mapper successor seed [ queue ])`

Performs the following algorithm:

If the result of applying the predicate stop? to seed is true, return
the list queue. Otherwise, apply the procedure mapper to seed,
returning a value which is added to the back of the list queue. Then
get a new seed by applying the procedure successor to seed, and repeat
this algorithm.

If queue is omitted, a newly allocated list queue is used.

## `(list-queue? obj)`

Returns #t if obj is a list queue, and #f otherwise. This operation is
O(1).

## `(list-queue-empty? list-queue)`

Returns #t if list-queue has no elements, and #f otherwise. This
operation is O(1).

## `(list-queue-front list-queue)`

Returns the first element of list-queue. If the list queue is empty,
it is an error. This operation is O(1).

## `(list-queue-back list-queue)`

Returns the last element of list-queue. If the list queue is empty, it
is an error. This operation is O(1).

## `(list-queue-list list-queue)`

Returns the list that contains the members of list-queue in order. The
result shares storage with list-queue. This operation is O(1).

## `(list-queue-first-last list-queue)`

Returns two values, the first and last pairs of the list that contains
the members of list-queue in order. If list-queue is empty, returns
two empty lists. The results share storage with list-queue. This
operation is O(1).

## `(list-queue-add-front! list-queue element)`

Adds element to the beginning of list-queue. Returns an unspecified
value. This operation is O(1).

## `(list-queue-add-back! list-queue element)`

Adds element to the end of list-queue. Returns an unspecified
value. This operation is O(1).

## `(list-queue-remove-front! list-queue)`

Removes the first element of list-queue and returns it. If the list
queue is empty, it is an error. This operation is O(1).

## `(list-queue-remove-back! list-queue)`

Removes the last element of list-queue and returns it. If the list
queue is empty, it is an error. This operation is O(n) where n is the
length of list-queue, because queues do not not have backward links.

## `(list-queue-remove-all! list-queue)`

Removes all the elements of list-queue and returns them in order as a
list. This operation is O(1).

## `(list-queue-set-list! list-queue list [ last ])`

Replaces the list associated with list-queue with list, effectively
discarding all the elements of list-queue in favor of those in
list. Returns an unspecified value. This operation is O(n) where n is
the length of list. If last is provided, it is treated in the same way
as in make-list-queue, and the operation is O(1).

Note: To apply a destructive list procedure to a list queue, use
(list-queue-set-list! (proc (list-queue-list list-queue))).

## `(list-queue-append list-queue ...)`

Returns a list queue which contains all the elements in front-to-back
order from all the list-queues in front-to-back order. The result does
not share storage with any of the arguments. This operation is O(n) in
the total number of elements in all queues.

## `(list-queue-append! list-queue ...)`

Returns a list queue which contains all the elements in front-to-back
order from all the list-queues in front-to-back order. It is an error
to assume anything about the contents of the list-queues after the
procedure returns. This operation is O(n) in the total number of
queues, not elements. It is not part of the R7RS-small list API, but
is included here for efficiency when pure functional append is not
required.

## `(list-queue-concatenate list-of-list-queues)`

Returns a list queue which contains all the elements in front-to-back
order from all the list queues which are members of
list-of-list-queues in front-to-back order. The result does not share
storage with any of the arguments. This operation is O(n) in the total
number of elements in all queues. It is not part of the R7RS-small
list API, but is included here to make appending a large number of
queues possible in Schemes that limit the number of arguments to
apply.

## `(list-queue-map proc list-queue)`

Applies proc to each element of list-queue in unspecified order and
returns a newly allocated list queue containing the results. This
operation is O(n) where n is the length of list-queue.

## `(list-queue-map! proc list-queue)`

Applies proc to each element of list-queue in front-to-back order and
modifies list-queue to contain the results. This operation is O(n) in
the length of list-queue. It is not part of the R7RS-small list API,
but is included here to make transformation of a list queue by
mutation more efficient.

## `(list-queue-for-each proc list-queue)`

Applies proc to each element of list-queue in front-to-back order,
discarding the returned values. Returns an unspecified value. This
operation is O(n) where n is the length of list-queue.
