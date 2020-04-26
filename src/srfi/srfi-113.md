
## `(arew srfi srfi-113)`

This library is based on [SRFI-113](https://srfi.schemers.org/srfi-113/).

### Abstract

Sets and bags (also known as multisets) are unordered collections that
can contain any Scheme object. Sets enforce the constraint that no two
elements can be the same in the sense of the set's associated equality
predicate; bags do not.

### Reference

TODO

#### Set

##### set
##### set-unfold
##### set?
##### set-contains?
##### set-empty?
##### set-disjoint?
##### set-member
##### set-element-comparator
##### set-adjoin
##### set-adjoin!
##### set-replace
##### set-replace!
##### set-delete
##### set-delete!
##### set-delete-all
##### set-delete-all!
##### set-search!
##### set-size
##### set-find
##### set-count
##### set-any?
##### set-every?
##### set-map
##### set-for-each
##### set-fold
##### set-filter
##### set-remove
##### set-remove
##### set-partition
##### set-filter!
##### set-remove!
##### set-partition!
##### set-copy
##### set->list
##### list->set
##### list->set!
##### set=?
##### set<?
##### set>?
##### set<=?
##### set>=?
##### set-union
##### set-intersection
##### set-difference
##### set-xor
##### set-union!
##### set-intersection!
##### set-difference!
##### set-xor!
##### set-comparator

#### Bag
##### bag
##### bag-unfold
##### bag?
##### bag-contains?
##### bag-empty?
##### bag-disjoint?
##### bag-member
##### bag-element-comparator
##### bag-adjoin
##### bag-adjoin!
##### bag-replace
##### bag-replace!
##### bag-delete
##### bag-delete!
##### bag-delete-all
##### bag-delete-all!
##### bag-search!
##### bag-size
##### bag-find
##### bag-count
##### bag-any?
##### bag-every?
##### bag-map
##### bag-for-each
##### bag-fold
##### bag-filter
##### bag-remove
##### bag-partition
##### bag-filter!
##### bag-remove!
##### bag-partition!
##### bag-copy
##### bag->list
##### list->bag
##### list->bag!
##### bag=?
##### bag<?
##### bag>?
##### bag<=?
##### bag>=?
##### bag-union
##### bag-intersection
##### bag-difference
##### bag-xor
##### bag-union!
##### bag-intersection!
##### bag-difference!
##### bag-xor!
##### bag-comparator
##### bag-sum
##### bag-sum!
##### bag-product
##### bag-product!
##### bag-unique-size
##### bag-element-count
##### bag-for-each-unique
##### bag-fold-unique
##### bag-increment!
##### bag-decrement!
##### bag->set
##### set->bag
##### set->bag!
##### bag->alist
##### alist->bag
