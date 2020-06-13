
## `(arew srfi srfi-128)`

This library is based on [SRFI-128](https://srfi.schemers.org/srfi-128/).

### Abstract

A comparator is an object of a disjoint type. It is a bundle of
procedures that are useful for comparing two objects either for
equality or for ordering. There are four procedures in the bundle:

- The type test predicate returns #t if its argument has the correct
  type to be passed as an argument to the other three procedures, and
  #f otherwise.

- The equality predicate returns #t if the two objects are the same in
  the sense of the comparator, and #f otherwise. It is the
  programmer's responsibility to ensure that it is reflexive,
  symmetric, transitive, and can handle any arguments that satisfy the
  type test predicate.

- The comparison procedure returns -1, 0, or 1 if the first object
  precedes the second, is equal to the second, or follows the second,
  respectively, in a total order defined by the comparator. It is the
  programmer's responsibility to ensure that it is reflexive, weakly
  antisymmetric, transitive, can handle any arguments that satisfy the
  type test predicate, and returns 0 iff the equality predicate
  returns #t.

- The hash function takes one argument, and returns an exact
  non-negative integer. It is the programmer's responsibility to
  ensure that it can handle any argument that satisfies the type test
  predicate, and that it returns the same value on two objects if the
  equality predicate says they are the same (but not necessarily the
  converse).

It is also the programmer's responsibility to ensure that all four
procedures provide the same result whenever they are applied to the
same object(s) (in the sense of eqv?), unless the object(s) have been
mutated since the last invocation. In particular, they must not depend
in any way on memory addresses in implementations where the garbage
collector can move objects in memory.

**Limitations**

The comparator objects defined in this library are not applicable to
circular structure or to NaNs or objects containing them. Attempts to
pass any such objects to any procedure defined here, or to any
procedure that is part of a comparator defined here, is an error
except as otherwise noted.

### Reference

#### Predicates

##### `(comparator? obj)`

Returns #t if obj is a comparator, and #f otherwise.

##### `(comparator-comparison-procedure? comparator)`

Returns #t if comparator has a supplied comparison procedure, and #f
otherwise.

##### `(comparator-hash-function? comparator)`

Returns #t if comparator has a supplied hash function, and #f otherwise.

#### Standard comparators

##### `boolean-comparator`

Compares booleans using the total order #f < #t.

##### `char-comparator`

Compares characters using the total order implied by char<?. On R6RS
and R7RS systems, this is Unicode codepoint order.

##### `char-ci-comparator`

Compares characters using the total order implied by char-ci<? On R6RS
and R7RS systems, this is Unicode codepoint order after the characters
have been folded to lower case.

##### `string-comparator`

Compares strings using the total order implied by string<?. Note that
this order is implementation-dependent.

##### `string-ci-comparator`

Compares strings using the total order implied by string-ci<?. Note
that this order is implementation-dependent.

##### `symbol-comparator`

Compares symbols using the total order implied by applying
symbol->string to the symbols and comparing them using the total order
implied by string<?. It is not a requirement that the hash function of
symbol-comparator be consistent with the hash function of
string-comparator, however.

##### `exact-integer-comparator`

##### `integer-comparator`

##### `rational-comparator`

##### `real-comparator`

##### `complex-comparator`

##### `number-comparator`

These comparators compare exact integers, integers, rational numbers,
real numbers, complex numbers, and any numbers using the total order
implied by <. They must be compatible with the R5RS numerical tower in
the following sense: If S is a subtype of the numerical type T and the
two objects are members of S , then the equality predicate and
comparison procedures (but not necessarily the hash function) of
S-comparator and T-comparator compute the same results on those
objects.

Since non-real numbers cannot be compared with <, the following
least-surprising ordering is defined: If the real parts are < or >, so
are the numbers; otherwise, the numbers are ordered by their imaginary
parts. This can still produce surprising results if one real part is
exact and the other is inexact.

##### `pair-comparator`

This comparator compares pairs using default-comparator (see below) on
their cars. If the cars are not equal, that value is returned. If they
are equal, default-comparator is used on their cdrs and that value is
returned.

##### `list-comparator`

This comparator compares lists lexicographically, as follows:

- The empty list compares equal to itself.
- The empty list compares less than any non-empty list.
- Two non-empty lists are compared by comparing their cars. If the
  cars are not equal when compared using default-comparator (see
  below), then the result is the result of that comparison. Otherwise,
  the cdrs are compared using list-comparator.

##### `vector-comparator`

##### `bytevector-comparator`

These comparators compare vectors and bytevectors by comparing their
lengths. A shorter argument is always less than a longer one. If the
lengths are equal, then each element is compared in turn using
default-comparator (see below) until a pair of unequal elements is
found, in which case the result is the result of that comparison. If
all elements are equal, the arguments are equal.

If the implementation does not support bytevectors,
bytevector-comparator has a type testing procedure that always returns
#f.

#### The default comparator

##### `default-comparator`

This is a comparator that accepts any two Scheme values (with the
exceptions listed in the Limitations section) and orders them in some
implementation-defined way, subject to the following conditions:

- The following ordering between types must hold: the empty list
  precedes pairs, which precede booleans, which precede characters,
  which precede strings, which precede symbols, which precede numbers,
  which precede vectors, which precede bytevectors, which precede all
  other objects.

- When applied to pairs, booleans, characters, strings, symbols,
  numbers, vectors, or bytevectors, its behavior must be the same as
  pair-comparator, boolean-comparator, character-comparator,
  string-comparator, symbol-comparator, number-comparator,
  vector-comparator, and bytevector-comparator respectively. The same
  should be true when applied to an object or objects of a type for
  which a standard comparator is defined elsewhere.

- Given disjoint types a and b, one of three conditions must hold:

  - All objects of type a compare less than all objects of type b.
  - All objects of type a compare greater than all objects of type b.
  - All objects of either type a or type b compare equal to each
    other. This is not permitted for any of the standard types
    mentioned above.

#### Comparator constructors

##### `(make-comparator type-test equality compare hash)`

Returns a comparator which bundles the type-test, equality, compare,
and hash procedures provided. As a convenience, the following
additional values are accepted:

- If type-test is #t, a type-test procedure that accepts any arguments
  is provided.
- If equality is #t, an equality predicate is provided that returns #t
  iff compare returns 0.
- If compare or hash is #f, a procedure is provided that signals an
  error on application. The predicates
  comparator-comparison-procedure? and/or comparator-hash-function?,
  respectively, will return #f in these cases.

##### `(make-inexact-real-comparator epsilon rounding nan-handling)`

Returns a comparator that compares inexact real numbers including NaNs
as follows: if after rounding to the nearest epsilon they are the
same, they compare equal; otherwise they compare as specified by
<. The direction of rounding is specified by the rounding argument,
which is either a procedure accepting two arguments (the number and
epsilon, or else one of the symbols floor, ceiling, truncate, or
round.

The argument nan-handling specifies how to compare NaN arguments to
non-NaN arguments. If it is a procedure, the procedure is invoked on
the other argument if either argument is a NaN. If it is the symbol
min, NaN values precede all other values; if it is the symbol max,
they follow all other values, and if it is the symbol error, an error
is signaled if a NaN value is compared. If both arguments are NaNs,
however, they always compare as equal.

##### `(make-list-comparator element-comparator)`

##### `(make-vector-comparator element-comparator)`

##### `(make-bytevector-comparator element-comparator)`

These procedures return comparators which compare two lists, vectors,
or bytevectors in the same way as list-comparator, vector-comparator,
and bytevector-comparator respectively, but using element-comparator
rather than default-comparator.

If the implementation does not support bytevectors, the result of
invoking make-bytevector-comparator is a comparator whose type testing
procedure always returns #f.

##### `(make-listwise-comparator type-test element-comparator empty? head tail)`

Returns a comparator which compares two objects that satisfy type-test
as if they were lists, using the empty? procedure to determine if an
object is empty, and the head and tail procedures to access particular
elements.

##### `(make-vectorwise-comparator type-test element-comparator length ref)`

Returns a comparator which compares two objects that satisfy type-test
as if they were vectors, using the length procedure to determine the
length of the object, and the ref procedure to access a particular
element.

##### `(make-car-comparator comparator)`

Returns a comparator that compares pairs on their cars alone using
comparator.

##### `(make-cdr-comparator comparator)`

Returns a comparator that compares pairs on their cdrs alone using
comparator.

##### `(make-pair-comparator car-comparator cdr-comparator)`

Returns a comparator that compares pairs first on their cars using
car-comparator. If the cars are equal, it compares the cdrs using
cdr-comparator.

##### `(make-improper-list-comparator element-comparator)`

Returns a comparator that compares arbitrary objects as follows: the
empty list precedes all pairs, which precede all other objects. Pairs
are compared as if with (make-pair-comparator element-comparator
element-comparator). All other objects are compared using
element-comparator.

##### `(make-selecting-comparator comparator1 comparator2 ...)`

Returns a comparator whose procedures make use of the comparators as
follows:

The type test predicate passes its argument to the type test
predicates of comparators in the sequence given. If any of them
returns #t, so does the type test predicate; otherwise, it returns #f.

The arguments of the equality, compare, and hash functions are passed
to the type test predicate of each comparator in sequence. The first
comparator whose type test predicate is satisfied on all the arguments
is used when comparing those arguments. All other comparators are
ignored. If no type test predicate is satisfied, an error is signaled.


##### `(make-refining-comparator comparator1 comparator2 ...)`

Returns a comparator that makes use of the comparators in the same way
as make-selecting-comparator, except that its procedures can look past
the first comparator whose type test predicate is satisfied. If the
comparison procedure of that comparator returns zero, then the next
comparator whose type test predicate is satisfied is tried in place of
it until one returns a non-zero value. If there are no more such
comparators, then the comparison procedure returns zero. The equality
predicate is defined in the same way. If no type test predicate is
satisfied, an error is signaled.

The hash function of the result returns a value which depends, in an
implementation-defined way, on the results of invoking the hash
functions of the comparators whose type test predicates are satisfied
on its argument. In particular, it may depend solely on the first or
last such hash function. If no type test predicate is satisfied, an
error is signaled.

This procedure is analogous to the expression type refine-compare from
SRFI 67.

##### `(make-reverse-comparator comparator)`

Returns a comparator that behaves like comparator, except that the
compare procedure returns 1, 0, and -1 instead of -1, 0, and 1
respectively. This allows ordering in reverse.

##### `(make-debug-comparator comparator)`

Returns a comparator that behaves exactly like comparator, except that
whenever any of its procedures are invoked, it verifies all the
programmer responsibilities (except stability), and an error is
signaled if any of them are violated. Because it requires three
arguments, transitivity is not tested on the first call to a debug
comparator; it is tested on all future calls using an arbitrarily
chosen argument from the previous invocation. Note that this may cause
unexpected storage leaks.

#### Wrapped equality predicates

##### `eq-comparator`

##### `eqv-comparator`

##### `equal-comparator`

The equality predicates of these comparators are eq?, eqv?, and equal?
respectively. When their comparison procedures are applied to
non-equal objects, their behavior is implementation-defined. The type
test predicates always return #t.

These comparators accept circular structure (in the case of
equal-comparator, provided the implementation's equal does so) and
NaNs.

#### Accessors

##### `(comparator-type-test-procedure comparator)`

Returns the type test predicate of comparator.

##### `(comparator-equality-predicate comparator)`

Returns the equality predicate of comparator.

##### `(comparator-comparison-procedure comparator)`

Returns the comparison procedure of comparator.

##### `(comparator-hash-function comparator)`

Returns the hash function of comparator.

#### Primitive applicators

##### `(comparator-test-type comparator obj)`

Invokes the type test predicate of comparator on obj and returns what
it returns.

##### `(comparator-check-type comparator obj)`

Invokes the type test predicate of comparator on obj and returns true
if it returns true and signals an error otherwise.

##### `(comparator-equal? comparator obj1 obj2)`

Invokes the equality predicate of comparator on obj1 and obj2 and
returns what it returns.

##### `(comparator-compare comparator obj1 obj2)`

Invokes the comparison procedure of comparator on obj1 and obj2 and
returns what it returns.

##### `(comparator-hash comparator obj)`

Invokes the hash function of comparator on obj and returns what it
returns.

#### Comparison procedure constructors

##### `(make-comparison< lt-pred)`

##### `(make-comparison> gt-pred)`

##### `(make-comparison<= le-pred)`

##### `(make-comparison>= ge-pred)`

##### `(make-comparison=/< eq-pred lt-pred)`

##### `(make-comparison=/> eq-pred gt-pred)`

These procedures return a comparison procedure, given a less-than
predicate, a greater-than predicate, a less-than-or-equal-to
predicate, a greater-than-or-equal-to predicate, or the combination of
an equality predicate and either a less-than or a greater-than
predicate.

#### Comparison syntax

The following expression types allow the convenient use of comparison
procedures.

##### `(if3 <expr> <less> <equal> <greater>)`

The expression `<expr>` is evaluated; it will typically, but not
necessarily, be a call on a comparison procedure. If the result is -1,
`<less>` is evaluated and its value(s) are returned; if the result is
0, `<equal>` is evaluated and its value(s) are returned; if the result
is 1, `<greater>` is evaluated and its value(s) are
returned. Otherwise an error is signaled.

##### `(if=? <expr> <consequent> [ <alternate> ])`

##### `(if<? <expr> <consequent> [ <alternate> ])`

##### `(if>? <expr> <consequent> [ <alternate> ])`

##### `(if<=? <expr> <consequent> [ <alternate> ])`

##### `(if>=? <expr> <consequent> [ <alternate> ])`

##### `(if-not=? <expr> <consequent> [ <alternate> ])`

The expression `<expr>` is evaluated; it will typically, but not
necessarily, be a call on a comparison procedure. It is an error if
its value is not -1, 0, or 1. If the value is consistent with the
specified relation, `<consequent>` is evaluated and its value(s) are
returned. Otherwise, if `<alternate>` is present, it is evaluated and
its value(s) are returned; if it is absent, an unspecified value is
returned.

#### Comparison predicates

##### `(=? comparator object1 object2 object3 ...)`

##### `(<? comparator object1 object2 object3 ...)`

##### `(>? comparator object1 object2 object3 ...)`

##### `(<=? comparator object1 object2 object3 ...)`

##### `(>=? comparator object1 object2 object3 ...)`

These procedures are analogous to the number, character, and string
comparison predicates of Scheme. They allow the convenient use of
comparators in situations where the expression types are not
usable. They are also analogous to the similarly named procedures SRFI
67, but handle arbitrary numbers of arguments, which in SRFI 67
requires the use of the variants whose names begin with chain.

These procedures apply the comparison procedure of comparator to the
objects as follows. If the specified relation returns #t for all
objecti and objectj where n is the number of objects and 1 <= i < j <=
n, then the procedures return #t, but otherwise #f.

The order in which the values are compared is unspecified. Because the
relations are transitive, it suffices to compare each object with its
successor.

#### Comparison predicate constructors

##### `(make=? comparator)`

##### `(make<? comparator)`

##### `(make>? comparator)`

##### `(make<=? comparator)`

##### `(make>=? comparator)`

These procedures return predicates which, when applied to two or more
arguments, return #t if comparing obj1 and obj2 using the equality or
comparison procedures of comparator shows that the objects bear the
specified relation to one another. Such predicates can be used in
contexts that do not understand or expect comparators.

#### Interval (ternary) comparison predicates

These procedures return true or false depending on whether an object
is contained in an open, closed, or half-open interval. All
comparisons are done in the sense of comparator, which is
default-comparator if omitted.

##### `(in-open-interval? [comparator] obj1 obj2 obj3)`

Return #t if obj1 is less than obj2, which is less thanobj3, and #f
otherwise.

##### `(in-closed-interval? [comparator] obj1 obj2 obj3)`

Returns #t if obj1 is less than or equal to obj2, which is less than
or equal to obj3, and #f otherwise.

##### `(in-open-closed-interval? [comparator] obj1 obj2 obj3)`

Returns #t if obj1 is less than obj2, which is less than or equal to
obj3, and #f otherwise.

##### `(in-closed-open-interval? [comparator] obj1 obj2 obj3)`

Returns #t if obj1 is less than or equal to obj2, which is less than
obj3, and #f otherwise.

#### Min/max comparison procedures

##### `(comparator-min comparator object1 object2 ...)`

##### `(comparator-max comparator object1 object2 ...)`

These procedures are analogous to min and max respectively. They apply
the comparison procedure of comparator to the objects to find and
return a minimal (or maximal) object. The order in which the values
are compared is unspecified.
