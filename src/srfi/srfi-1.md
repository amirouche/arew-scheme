
## `(arew srfi srfi-1)`

This is based on [SRFI-1](https://srfi.schemers.org/srfi-1/).

### Abstract

TODO

### Reference

#### Constructors

##### `(cons a d)`

The primitive constructor. Returns a newly allocated pair whose `car`
is `a` and whose `cdr` is `d`. The pair is guaranteed to be different
(in the sense of `eqv?`) from every existing object.

##### `(list object ...)`

Returns a newly allocated list of its arguments.

##### `(xcons d a)`

```scheme
(lambda (d a) (cons a d))
```

Of utility only as a value to be conveniently passed to higher-order
procedures.

```scheme
(xcons '(b c) 'a) ;; => (a b c)
```

The name stands for "eXchanged CONS."

##### `(cons* obj ... tail)`

Like list, but the last argument provides the tail of the constructed
list.

##### `(make-list n [fill])`

Returns an n-element list, whose elements are all the value fill. If
the fill argument is not given, the elements of the list may be
arbitrary values.

```scheme
(make-list 4 'c) => (c c c c)
```
##### `(list-tabulate n init-proc)`

Returns an n-element list. Element i of the list, where 0 <= i < n, is
produced by (init-proc i). No guarantee is made about the dynamic
order in which init-proc is applied to these indices.

```scheme
(list-tabulate 4 values) => (0 1 2 3)
```
##### `(list-copy flist)`

Copies the spine of the argument.

##### `(circular-list elt1 elt2 ...)`

Constructs a circular list of the elements.

```scheme
(circular-list 'z 'q) => (z q z q z q ...)
```

##### `(iota count [start step])`

Returns a list containing the elements:

```scheme
(start start+step ... start+(count-1)*step)
```

The start and step parameters default to 0 and 1, respectively.

```scheme
(iota 5) => (0 1 2 3 4)
(iota 5 0 -0.1) => (0 -0.1 -0.2 -0.3 -0.4)
```

#### Predicates

##### `(proper-list? x)`

Returns true iff x is a proper list -- a finite, nil-terminated list.

More carefully: The empty list is a proper list. A pair whose cdr is a
proper list is also a proper list. The opposite of proper is improper.

##### `(circular-list? x)`

True if x is a circular list. A circular list is a value such that for
every n >= 0, cdrn(x) is a pair.

Terminology: The opposite of circular is finite.

##### `(dotted-list? x)

True if x is a finite, non-nil-terminated list. That is, there exists
an n >= 0 such that cdrn(x) is neither a pair nor (). This includes
non-pair, non-() values (e.g. symbols, numbers), which are considered
to be dotted lists of length 0.

##### `(pair? obj)`

Returns #t if object is a pair; otherwise, #f.

##### `(null? obj)`

Returns #t if object is the empty list; otherwise, #f.

##### `(null-list? list)`

List is a proper or circular list. This procedure returns true if the
argument is the empty list (), and false otherwise. It is an error to
pass this procedure a value which is not a proper or circular
list. This procedure is recommended as the termination condition for
list-processing procedures that are not defined on dotted lists.

##### `(not-pair? x)

Provided as a procedure as it can be useful as the termination
condition for list-processing procedures that wish to handle all
finite lists, both proper and dotted.

##### `(list= elt= list1 ...)`

Determines list equality, given an element-equality procedure.

#### Selectors

##### (car pair)
##### (cdr pair)

These functions return the contents of the car and cdr field of their
argument, respectively. Note that it is an error to apply them to the
empty list.

Also the following selectors are defined:

- `caar`
- `cadr`
- `cdar`
- `cddr`
- `caaar`
- `caadr`
- `cadar`
- `caddr`
- `cdaar`
- `cdadr`
- `cddar`
- `cdddr`
- `caaaar`
- `caaadr`
- `caadar`
- `caaddr`
- `cadaar`
- `cadadr`
- `caddar`
- `cadddr`
- `cdaaar`
- `cdaadr`
- `cdadar`
- `cdaddr`
- `cddaar`
- `cddadr`
- `cdddar`
- `cddddr`

##### `(list-ref clist i)`

Returns the ith element of clist. (This is the same as the car of
(drop clist i).) It is an error if i >= n, where n is the length of
clist.

```scheme
(list-ref '(a b c d) 2) => c
```

##### `(first pair)`
##### `(second pair)`
##### `(third pair)`
##### `(fourth pair)`
##### `(fifth pair)`
##### `(sixth pair)`
##### `(seventh pair)`
##### `(eighth pair)`
##### `(ninth pair)`
##### `(tenth pair)`

Synonyms for `car`, `cadr`, `caddr`, ...

##### `(car+cdr pair)`

The fundamental pair deconstructor:

```
(lambda (p) (values (car p) (cdr p)))
```

This can, of course, be implemented more efficiently by a compiler.

##### `(take lst i)`
##### `(drop lst i)`

`take` returns the first `I` elements of list `LST`.
`drop` returns all but the first i elements of list `LST`.

```scheme
(take '(a b c d e) 2) ;; => (a b)
(drop '(a b c d e) 2) ;; => (c d e)
```

`LST` may be any value -- a proper, circular, or dotted list:

```scheme
(take '(1 2 3 . d) 2) ;; => (1 2)
(drop '(1 2 3 . d) 2) ;; => (3 . d)
(take '(1 2 3 . d) 3) ;; => (1 2 3)
(drop '(1 2 3 . d) 3) ;; => d
```

For a legal `I`, `take` and `drop` partition the list in a manner
which can be inverted with append:

```scheme
(equal? (append (take lst i) (drop x i)) lst)
```

`drop` is exactly equivalent to performing `i` `cdr` operations on
`LST`; the returned value shares a common tail with `LST`. If the
argument is a list of non-zero length, `take` is guaranteed to return
a freshly-allocated list, even in the case where the entire list is
taken, e.g. `(take lst (length lst))`.

##### `(take-right flist i)`
##### `(drop-right flist i)`

`take-right` returns the last `I` elements of `FLIST`.  `drop-right`
returns all but the last `I` elements of `FLIST`.

```scheme
(take-right '(a b c d e) 2) => (d e)
(drop-right '(a b c d e) 2) => (a b c)
```

The returned list may share a common tail with the argument list.

`FLIST` may be any finite list, either proper or dotted:

```scheme
(take-right '(1 2 3 . d) 2) => (2 3 . d)
(drop-right '(1 2 3 . d) 2) => (1)
(take-right '(1 2 3 . d) 0) => d
(drop-right '(1 2 3 . d) 0) => (1 2 3)
```

For a legal `I`, `take-right` and `drop-right` partition the list in a
manner which can be inverted with `append`:

```scheme
(equal? (append (take flist i) (drop flist i)) flist)
```

`take-right`'s return value is guaranteed to share a common tail with
`FLIST`. If the argument is a list of non-zero length, `drop-right` is
guaranteed to return a freshly-allocated list, even in the case where
nothing is dropped, e.g. `(drop-right flist 0)`.

##### `(take! x i)`
##### `(drop-right! flist i)`

`take!` and `drop-right!` are "linear-update" variants of `take` and
`drop-right`: the procedure is allowed, but not required, to alter the
argument list to produce the result.

If `x` is circular, `take!` may return a shorter-than-expected list:

```scheme
(take! (circular-list 1 3 5) 8) => (1 3)
(take! (circular-list 1 3 5) 8) => (1 3 5 1 3 5 1 3)
```

##### `(split-at  x i)`
##### `(split-at! x i)`

`split-at` splits the list `x` at index `i`, returning a list of the first `i`
elements, and the remaining tail. It is equivalent to:

```scheme
(values (take x i) (drop x i))
```

`split-at!` is the linear-update variant. It is allowed, but not
required, to alter the argument list to produce the result.

```scheme
(split-at '(a b c d e f g h) 3) ;; => (a b c) and (d e f g h)
```
##### `(last pair)`
##### `(last-pair pair)`

`last` returns the last element of the non-empty, finite list
`pair`. `last-pair` returns the last pair in the non-empty, finite list
`pair`:

```scheme
(last '(a b c)) ;; => c
(last-pair '(a b c)) ;; => (c)
```

#### Miscellaneous

##### `(length list)`
##### `(length+ clist)`

Both `length` and `length+` return the length of the argument. It is an
error to pass a value to length which is not a proper list (finite and
nil-terminated). In particular, this means an implementation may
diverge or signal an error when length is applied to a circular list.

`length+`, on the other hand, returns `#f` when applied to a circular
list.

The length of a proper list is a non-negative integer `n` such that
`cdr` applied `n` times to the list produces the empty list.

##### `(append  list1 ...)`
##### `(append! list1 ...)`

`append` returns a list consisting of the elements of `list1` followed by
the elements of the other list parameters.

```scheme
(append '(x) '(y))        =>  (x y)
(append '(a) '(b c d))    =>  (a b c d)
(append '(a (b)) '((c)))  =>  (a (b) (c))
```

The resulting list is always newly allocated, except that it shares
structure with the final list argument. This last argument may be any
value at all; an improper list results if it is not a proper list. All
other arguments must be proper lists.

```scheme
(append '(a b) '(c . d))  =>  (a b c . d)
(append '() 'a)           =>  a
(append '(x y))           =>  (x y)
(append)                  =>  ()
```

`append!` is the "linear-update" variant of append -- it is allowed, but
not required, to alter `cons` cells in the argument lists to construct
the result list. The last argument is never altered; the result list
shares structure with this parameter.

##### `(concatenate  list-of-lists)`
##### `(concatenate! list-of-lists)`

These functions append the elements of their argument together. That
is, `concatenate` returns:

```scheme
(apply append list-of-lists)
```

Or, equivalently,

```scheme
(reduce-right append '() list-of-lists)
```

`concatenate!` is the linear-update variant, defined in terms of `append!`
instead of `append`.

As with `append` and `append!`, the last element of the input list may be
any value at all.

##### `(reverse list)`
##### `(reverse! list)`

`reverse` returns a newly allocated list consisting of the elements of
list in reverse order.

```scheme
(reverse '(a b c)) ;; =>  (c b a)
(reverse '(a (b c) d (e (f)))) ;; =>  ((e (f)) d (b c) a)
```

`reverse!` is the linear-update variant of reverse. It is permitted,
but not required, to alter the argument's `cons` cells to produce the
reversed list.

##### `(append-reverse rev-head tail)`
##### `(append-reverse! rev-head tail)`

`append-reverse` returns `(append (reverse rev-head) tail)`. It is
provided because it is a common operation -- a common list-processing
style calls for this exact operation to transfer values accumulated in
reverse order onto the front of another list, and because the
implementation is significantly more efficient than the simple
composition it replaces. (But note that this pattern of iterative
computation followed by a `reverse` can frequently be rewritten as a
recursion, dispensing with the `reverse` and `append-reverse` steps,
and shifting temporary, intermediate storage from the heap to the
stack, which is typically a win for reasons of cache locality and
eager storage reclamation.)

`append-reverse!` is just the linear-update variant -- it is allowed,
but not required, to alter rev-head's `cons` cells to construct the
result.

##### `(zip clist1 clist2 ...)`

```scheme
(lambda lists (apply map list lists))
```

If `zip` is passed n lists, it returns a list as long as the shortest of
these lists, each element of which is an n-element list comprised of
the corresponding elements from the parameter lists.

```scheme
(zip '(one two three)
     '(1 2 3)
     '(odd even odd even odd even odd even))
     ;; => ((one 1 odd) (two 2 even) (three 3 odd))

(zip '(1 2 3)) => ((1) (2) (3))
```

##### `(unzip1 list)`
##### `(unzip2 list)`
##### `(unzip3 list)`
##### `(unzip4 list)`
##### `(unzip5 list)`

`unzip1` takes a list of lists, where every list must contain at least
one element, and returns a list containing the initial element of each
such list. That is, it returns `(map car lists)`. `unzip2` takes a
list of lists, where every list must contain at least two elements,
and returns two values: a list of the first elements, and a list of
the second elements. `unzip3` does the same for the first three
elements of the lists, and so forth.

```scheme
(unzip2 '((1 one) (2 two) (3 three))) ;; => '((1 2 3) (one two three))
```
##### `(count pred clist1 ...)`

`pred` is a procedure taking as many arguments as there are lists and
returning a single value. It is applied element-wise to the elements
of the lists, and a count is tallied of the number of elements that
produce a true value. This count is returned. `count` is "iterative"
in that it is guaranteed to apply `pred` to the list elements in a
left-to-right order. The counting stops when the shortest list
expires.

```scheme
(count even? '(3 1 4 1 5 9 2 5 6)) => 3
(count < '(1 2 4 8) '(2 4 6 8 10 12 14 16)) => 3
```

At least one of the argument lists must be finite:

```
(count < '(3 1 4 1) (circular-list 1 10)) => 2
```

#### Fold, unfold & map

##### `(fold kons knil list1 ...)`

TODO

##### `(fold-right kons knil list1 ...)`

TODO

##### `(pair-fold kons knil list1 ...)`

TODO

##### `(pair-fold-right kons knil list1 ...)`

TODO

##### `(reduce f ridentity list)`

`reduce` is a variant of `fold`.

`ridentity` should be a "right identity" of the procedure `f` -- that is,
for any value `x` acceptable to `f`:

```scheme
(f x ridentity) ;; => x
```

Note: that `ridentity` is used only in the empty-list case. You
typically use reduce when applying `f` is expensive and you'd like to
avoid the extra application incurred when fold applies `f` to the head
of list and the identity value, redundantly producing the same value
passed in to `f`. For example, if `f` involves searching a file
directory or performing a database query, this can be significant. In
general, however, `fold` is useful in many contexts where `reduce` is
not (consider the examples given in the `fold` definition -- only one
of the five folds uses a function with a right identity. The other
four may not be performed with reduce).

##### `(reducse-right f ridentity list)`

`reduce-right` is the `fold-right` variant of reduce. It obeys the
following definition:

```scheme
(reduce-right f ridentity '()) = ridentity
(reduce-right f ridentity '(e1)) = (f e1 ridentity) = e1
(reduce-right f ridentity '(e1 e2 ...)) =
    (f e1 (reduce f ridentity (e2 ...)))
```

... in other words, we compute `(fold-right f ridentity list)`.

```scheme
;; Append a bunch of lists together.
;; I.e., (apply append list-of-lists)
(reduce-right append '() list-of-lists)
```

##### `(unfold p f g seed [tail-gen])`

TODO

##### `(unfold-right p f g seed [tail-gen])`

TODO

##### `(map proc list1 ...)`

`proc` is a procedure taking as many arguments as there are list
arguments and returning a single value. map applies `proc` element-wise
to the elements of the lists and returns a list of the results, in
order. The dynamic order in which proc is applied to the elements of
the lists is unspecified.

```scheme
(map cadr '((a b) (d e) (g h))) =>  (b e h)

(map (lambda (n) (expt n n))
     '(1 2 3 4 5))
    =>  (1 4 27 256 3125)

(map + '(1 2 3) '(4 5 6)) =>  (5 7 9)

(let ((count 0))
  (map (lambda (ignored)
         (set! count (+ count 1))
         count)
       '(a b))) =>  (1 2) or (2 1)
```

At least one of the argument lists must be finite:

```scheme
(map + '(3 1 4 1) (circular-list 1 0)) ;; => (4 1 5 1)
```

##### `(for-each proc clist1 ...)`

The arguments to `for-each` are like the arguments to `map`, but
`for-each` calls `proc` for its side effects rather than for its
values. Unlike `map`, `for-each` is guaranteed to call `proc` on the
elements of the lists in order from the first element(s) to the last,
and the value returned by for-each is unspecified.

```scheme
(let ((v (make-vector 5)))
  (for-each (lambda (i)
              (vector-set! v i (* i i)))
            '(0 1 2 3 4))
  v)  =>  #(0 1 4 9 16)
```

At least one of the argument lists must be finite.

##### `(append-map f list ...)`
##### `(append-map! f list ...)`

Equivalent to:

```scheme
(apply append (map f clist1 clist2 ...))
```

And:

```scheme
(apply append! (map f clist1 clist2 ...))
```

Map `f` over the elements of the lists, just as in the `map`
function. However, the results of the applications are appended
together to make the final result. `append-map` uses `append` to
append the results together; `append-map!` uses `append!`.

The dynamic order in which the various applications of `f` are made is
not specified.

Example:

```scheme
(append-map! (lambda (x) (list x (- x))) '(1 3 8)) ;; => (1 -1 3 -3 8 -8)
```

At least one of the list arguments must be finite.

##### `(map! f list1 ...)`

Linear-update variant of `map` -- `map!` is allowed, but not required,
to alter the cons cells of `list1` to construct the result list.

The dynamic order in which the various applications of `f` are made is
not specified. In the n-ary case, `clist2`, `clist3`, ... must have at
least as many elements as `list1`.

##### `(map-in-order f clist1 ...)`

A variant of the map procedure that guarantees to apply `f` across the
elements of the `clisti` arguments in a left-to-right order. This is
useful for mapping procedures that both have side effects and return
useful values.

At least one of the list arguments must be finite.

##### `(pair-for-each f clist1 ...)`

Like for-each, but `f` is applied to successive sublists of the argument
lists. That is, `f` is applied to the `cons` cells of the lists, rather
than the lists' elements. These applications occur in left-to-right
order.

The f procedure may reliably apply `set-cdr!` to the pairs it is given
without altering the sequence of execution.

```scheme
(pair-for-each (lambda (pair) (display pair) (newline)) '(a b c))
 ;; => (a b c)
 ;; => (b c)
 ;; => (c)
```

At least one of the list arguments must be finite.

##### `(filter-map f clist1 ...)`

Like `map`, but only true values are saved.

```scheme
(filter-map (lambda (x) (and (number? x) (* x x))) '(a 1 b 3 c 7))
    ;; => (1 9 49)
```

The dynamic order in which the various applications of `f` are made is
not specified.

At least one of the list arguments must be finite.

#### Filtering & partitioning

##### `(filter pred list)`

Return all the elements of list that satisfy predicate `pred`. The list
is not disordered -- elements that appear in the result list occur in
the same order as they occur in the argument list. The returned list
may share a common tail with the argument list. The dynamic order in
which the various applications of `pred` are made is not specified.

```scheme
(filter even? '(0 7 8 8 43 -4)) => (0 8 8 -4)
```

##### `(partition pred list)`

Partitions the elements of list with predicate `pred`, and returns two
values: the list of in-elements and the list of out-elements. The list
is not disordered -- elements occur in the result lists in the same
order as they occur in the argument list. The dynamic order in which
the various applications of pred are made is not specified. One of the
returned lists may share a common tail with the argument list.

```scheme
(partition symbol? '(one 2 3 four five 6)) =>
    (one four five)
    (2 3 6)
```

##### `(remove pred list)`

Returns `list` without the elements that satisfy predicate `pred`:

```scheme
(lambda (pred list) (filter (lambda (x) (not (pred x))) list))
```

The `list` is not disordered -- elements that appear in the result
list occur in the same order as they occur in the argument list. The
returned list may share a common tail with the argument list. The
dynamic order in which the various applications of `pred` are made is
not specified.

```scheme
(remove even? '(0 7 8 8 43 -4)) => (7 43)
```

##### `(filter! pred list)`

##### `(partition! pred list)`

##### `(remove! pred list)`

Linear-update variants of `filter`, `partition` and `remove`. These
procedures are allowed, but not required, to alter the cons cells in
the argument list to construct the result lists.

#### Searching

##### `(find pred clist)`

Return the first element of clist that satisfies predicate `pred`; false
if no element does.

```scheme
(find even? '(3 1 4 1 5 9)) => 4
```

Note that `find` has an ambiguity in its lookup semantics -- if `find`
returns `#f`, you cannot tell (in general) if it found a #f element
that satisfied `pred`, or if it did not find any element at all. In
many situations, this ambiguity cannot arise -- either the list being
searched is known not to contain any `#f` elements, or the list is
guaranteed to have an element satisfying `pred`. However, in cases
where this ambiguity can arise, you should use `find-tail` instead of
find -- `find-tail` has no such ambiguity.

##### `(find-tail pred clist)`

Return the first pair of `clist` whose `car` satisfies `pred`. If no
pair does, return false.

`find-tail` can be viewed as a general-predicate variant of the member
function.

Examples:

```scheme
(find-tail even? '(3 1 37 -8 -5 0 0)) => (-8 -5 0 0)
(find-tail even? '(3 1 37 -5)) => #f

;; MEMBER X LIS:
(find-tail (lambda (elt) (equal? x elt)) lis)
```

In the circular-list case, this procedure "rotates" the list.

`find-tail` is essentially drop-while, where the sense of the
predicate is inverted: `find-tail` searches until it finds an element
satisfying the predicate; drop-while searches until it finds an
element that doesn't satisfy the predicate.

##### `(take-while  pred clist)`

##### `(take-while! pred clist)`

Returns the longest initial prefix of `clist` whose elements all
satisfy the predicate `pred`.

`take-while!` is the linear-update variant. It is allowed, but not
required, to alter the argument list to produce the result.

```scheme
(take-while even? '(2 18 3 10 22 9)) => (2 18)
```

##### `(drop-while pred clist)`

Drops the longest initial prefix of `clist` whose elements all satisfy
the predicate `pred`, and returns the rest of the list.

```scheme
(drop-while even? '(2 18 3 10 22 9)) => (3 10 22 9)
```

The circular-list case may be viewed as "rotating" the list.

##### `(span pred clist)`
##### `(span!  pred list)`
##### `(break  pred clist)`
##### `(break! pred list)

Span splits the list into the longest initial prefix whose elements
all satisfy `pred`, and the remaining tail. Break inverts the sense of
the predicate: the tail commences with the first element of the input
list that satisfies the predicate.

In other words: `span` finds the intial span of elements satisfying
`pred`, and break breaks the list at the first element satisfying `pred`.

Span is equivalent to

```scheme
(values (take-while pred clist)
        (drop-while pred clist))
```

`span!` and `break!` are the linear-update variants. They are allowed,
but not required, to alter the argument list to produce the result.

```scheme
(span even? '(2 18 3 10 22 9)) =>
      (2 18)
      (3 10 22 9)

(break even? '(3 1 4 1 5 9)) =>
      (3 1)
      (4 1 5 9)
```

##### `(any pred clist1 ...)`

Applies the predicate across the lists, returning true if the
predicate returns true on any application.

If there are n list arguments `clist1 ... clistn`, then `pred` must be
a procedure taking n arguments and returning a single value,
interpreted as a boolean (that is, `#f` means false, and any other
value means true).

`any` applies `pred` to the first elements of the `clisti` parameters. If
this application returns a true value, any immediately returns that
value. Otherwise, it iterates, applying `pred` to the second elements of
the `clisti` parameters, then the third, and so forth. The iteration
stops when a true value is produced or one of the lists runs out of
values; in the latter case, any returns `#f`. The application of pred to
the last element of the lists is a tail call.

Note the difference between `find` and `any` -- `find` returns the
element that satisfied the predicate; `any` returns the true value
that the predicate produced.

Like `every`, `any`'s name does not end with a question mark -- this
is to indicate that it does not return a simple boolean (#t or #f),
but a general value.

```scheme
(any integer? '(a 3 b 2.7))   => #t
(any integer? '(a 3.1 b 2.7)) => #f
(any < '(3 1 4 1 5)
           '(2 7 1 8 2)) => #t
```

##### `(every pred clist1 ...)`

Applies the predicate across the lists, returning true if the
predicate returns true on every application.

If there are n list arguments `clist1 ... clistn`, then `pred` must be a
procedure taking n arguments and returning a single value, interpreted
as a boolean (that is, #f means false, and any other value means
true).

`every` applies `pred` to the first elements of the `clisti`
parameters. If this application returns false, every immediately
returns false. Otherwise, it iterates, applying `pred` to the second
elements of the `clisti` parameters, then the third, and so forth. The
iteration stops when a false value is produced or one of the lists
runs out of values. In the latter case, `every` returns the true value
produced by its final application of pred. The application of `pred`
to the last element of the lists is a tail call.

If one of the `clisti` has no elements, `every` simply returns #t.

Like `any`, `every`'s name does not end with a question mark -- this
is to indicate that it does not return a simple boolean (`#t` or
`#f`), but a general value.

##### `(list-index pred clist1 ...)`

Return the index of the leftmost element that satisfies `pred`.

If there are n list arguments `clist1 ... clistn`, then `pred` must be
a function taking n arguments and returning a single value,
interpreted as a boolean (that is, `#f` means false, and any other value
means true).

`list-index` applies `pred` to the first elements of the `clisti`
parameters. If this application returns true, `list-index` immediately
returns zero. Otherwise, it iterates, applying `pred` to the second
elements of the `clisti` parameters, then the third, and so forth. When
it finds a tuple of list elements that cause `pred` to return true, it
stops and returns the zero-based index of that position in the lists.

The iteration stops when one of the lists runs out of values; in this
case, `list-index` returns `#f`.

```scheme
(list-index even? '(3 1 4 1 5 9)) => 2
(list-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)) => 1
(list-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)) => #f
```

##### `(member x list [=])`

##### `(memq x list)`

##### `(memv x list)`

These procedures return the first sublist of `list` whose `car` is
`x`, where the sublists of `list` are the non-empty lists returned by
`(drop list i)` for `i` less than the length of list. If `x` does not
occur in list, then `#f` is returned. `memq` uses `eq?` to compare `x`
with the elements of list, while `memv` uses `eqv?`, and `member` uses
`equal?`.

```scheme
(memq 'a '(a b c))          =>  (a b c)
(memq 'b '(a b c))          =>  (b c)
(memq 'a '(b c d))          =>  #f
(memq (list 'a) '(b (a) c)) =>  #f
(member (list 'a) '(b (a) c))         =>  ((a) c)
(memq 101 '(100 101 102))   =>  *unspecified*
(memv 101 '(100 101 102))   =>  (101 102)
```

The comparison procedure is used to compare the elements `ei` of list
to the key `x` in this way:

```scheme
(= x ei) ; list is (e1 ... en)
```

That is, the first argument is always `x`, and the second argument is
one of the list elements. Thus one can reliably find the first element
of list that is greater than five with `(member 5 list <)`

Note that fully general list searching may be performed with the
`find-tail` and `find` procedures, e.g.

```scheme
(find-tail even? list) ; Find the first elt with an even key.
```

#### Deleting

##### `(delete x list)`

##### `(delete! x list)`

`delete` uses the comparison procedure `=`, which defaults to
`equal?`, to find all elements of list that are equal to `x`, and
deletes them from list. The dynamic order in which the various
applications of `=` are made is not specified.

The list is not disordered -- elements that appear in the result list
occur in the same order as they occur in the argument list. The result
may share a common tail with the argument list.

Note that fully general element deletion can be performed with the
`remove` and `remove!` procedures, e.g.:

```scheme
;; Delete all the even elements from LIS:
(remove even? lis)
```

The comparison procedure is used in this way: `(= x ei)`. That is, `x`
is always the first argument, and a list element is always the second
argument. The comparison procedure will be used to compare each
element of list exactly once; the order in which it is applied to the
various `ei` is not specified. Thus, one can reliably remove all the
numbers greater than five from a list with `(delete 5 list <)`.

`delete!` is the linear-update variant of `delete`. It is allowed, but
not required, to alter the cons cells in its argument list to
construct the result.

##### `(delete-duplicates  list [=])`

##### `(delete-duplicates! list [=])`

`delete-duplicates` removes duplicate elements from the list
argument. If there are multiple equal elements in the argument list,
the result list only contains the first or leftmost of these elements
in the result. The order of these surviving elements is the same as in
the original list -- delete-duplicates does not disorder the list
(hence it is useful for "cleaning up" association lists).

The `=` parameter is used to compare the elements of the list; it
defaults to equal?. If x comes before y in list, then the comparison
is performed (= x y). The comparison procedure will be used to compare
each pair of elements in list no more than once; the order in which it
is applied to the various pairs is not specified.

Implementations of `delete-duplicates` are allowed to share common tails
between argument and result lists -- for example, if the list argument
contains only unique elements, it may simply return exactly this list.

Be aware that, in general, delete-duplicates runs in time O(n2) for
n-element lists. Uniquifying long lists can be accomplished in O(n lg
n) time by sorting the list to bring equal elements together, then
using a linear-time algorithm to remove equal elements. Alternatively,
one can use algorithms based on element-marking, with linear-time
results.

`delete-duplicates!` is the linear-update variant of
`delete-duplicates`; it is allowed, but not required, to alter the cons
cells in its argument list to construct the result.

```scheme
(delete-duplicates '(a b a c a b c z)) => (a b c z)

;; Clean up an alist:
(delete-duplicates '((a . 3) (b . 7) (a . 9) (c . 1))
                   (lambda (x y) (eq? (car x) (car y))))
;;  => ((a . 3) (b . 7) (c . 1))
```

#### Association lists

An "association list" (or "alist") is a list of pairs. The car of each
pair contains a key value, and the cdr contains the associated data
value. They can be used to construct simple look-up tables in
Scheme. Note that association lists are probably inappropriate for
performance-critical use on large data; in these cases, hash tables or
some other alternative should be employed.

##### `(assoc key alist [=])`

##### `(assq key alist)`

##### `(assv key alist)`

`alist` must be an association list -- a list of pairs. These
procedures find the first pair in `alist` whose `car` field is `key`,
and returns that pair. If no pair in `alist` has `key` as its `car`,
then `#f` is returned. `assq` uses `eq?` to compare `key` with the
`car` fields of the pairs in `alist`, while `assv` uses `eqv?` and
`assoc` uses `equal?`.

```scheme
(define e '((a 1) (b 2) (c 3)))
(assq 'a e)                            =>  (a 1)
(assq 'b e)                            =>  (b 2)
(assq 'd e)                            =>  #f
(assq (list 'a) '(((a)) ((b)) ((c))))  =>  #f
(assoc (list 'a) '(((a)) ((b)) ((c)))) =>  ((a))
(assq 5 '((2 3) (5 7) (11 13)))	   =>  *unspecified*
(assv 5 '((2 3) (5 7) (11 13)))	   =>  (5 7)
```

The comparison procedure is used to compare the elements `ei` of list to
the `key` parameter in this way:

```scheme
(= key (car ei)) ; list is (E1 ... En)
```

That is, the first argument is always `key`, and the second argument is
one of the list elements. Thus one can reliably find the first entry
of alist whose key is greater than five with `(assoc 5 alist <)`

Note that fully general `alist` searching may be performed with the
`find-tail` and `find` procedures, e.g.

```scheme
;; Look up the first association in alist with an even key:
(find (lambda (a) (even? (car a))) alist)
```

##### `(alist-cons key datum alist)`

```scheme
(lambda (key datum alist) (cons (cons key datum) alist))
```

`cons` a new entry mapping `key` to `datum` onto `alist`.

##### `(alist-copy alist)`

Make a fresh copy of `alist`. This means copying each pair that forms
an association as well as the spine of the list, i.e.

```scheme
(lambda (a) (map (lambda (elt) (cons (car elt) (cdr elt))) a))
```

##### `(alist-delete  key alist [=])`

##### `(alist-delete! key alist [=])`

`alist-delete` deletes all associations from alist with the given key,
using key-comparison procedure `=`, which defaults to equal?. The
dynamic order in which the various applications of = are made is not
specified.

Return values may share common tails with the alist argument. The
alist is not disordered -- elements that appear in the result alist
occur in the same order as they occur in the argument alist.

The comparison procedure is used to compare the element keys ki of
alist's entries to the key parameter in this way: (= key ki). Thus,
one can reliably remove all entries of alist whose key is greater than
five with (alist-delete 5 alist <)

alist-delete! is the linear-update variant of alist-delete. It is
allowed, but not required, to alter cons cells from the alist
parameter to construct the result.

#### Set operations on lists

These procedures implement operations on sets represented as lists of
elements. They all take an = argument used to compare elements of
lists. This equality procedure is required to be consistent with
eq?. That is, it must be the case that `(eq? x y) => (= x y)`.

Note that this implies, in turn, that two lists that are eq? are also
set-equal by any legal comparison procedure. This allows for
constant-time determination of set operations on eq? lists.

Be aware that these procedures typically run in time O(n * m) for n-
and m-element list arguments. Performance-critical applications
operating upon large sets will probably wish to use other data
structures and algorithms.

##### `(lset<= = list1 ...)`

Returns true iff every listi is a subset of listi+1, using = for the
element-equality procedure. List A is a subset of list B if every
element in A is equal to some element of B. When performing an element
comparison, the = procedure's first argument is an element of A; its
second, an element of B.

```scheme
(lset<= eq? '(a) '(a b a) '(a b c c)) => #t

(lset<= eq?) => #t             ; Trivial cases
(lset<= eq? '(a)) => #t
```

##### `(lset= = list1 ...)`

Returns true iff every listi is set-equal to listi+1, using = for the
element-equality procedure. "Set-equal" simply means that listi is a
subset of listi+1, and listi+1 is a subset of listi. The = procedure's
first argument is an element of listi; its second is an element of
listi+1.

```scheme
(lset= eq? '(b e a) '(a e b) '(e e b a)) => #t

(lset= eq?) => #t               ; Trivial cases
(lset= eq? '(a)) => #t
```

##### `(lset-adjoin = list elt1 ...)`

Adds the elti elements not already in the list parameter to the result
list. The result shares a common tail with the list parameter. The new
elements are added to the front of the list, but no guarantees are
made about their order. The = parameter is an equality procedure used
to determine if an elti is already a member of list. Its first
argument is an element of list; its second is one of the elti.

The list parameter is always a suffix of the result -- even if the
list parameter contains repeated elements, these are not reduced.

```scheme
(lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o 'u) => (u o i a b c d c e)
```

##### `(lset-union = list1 ...)`

Returns the union of the lists, using = for the element-equality procedure.

The union of lists A and B is constructed as follows:

- If A is the empty list, the answer is B (or a copy of B).
- Otherwise, the result is initialised to be list A (or a copy of A).
- Proceed through the elements of list B in a left-to-right order. If
  b is such an element of B, compare every element r of the current
  result list to b: (= r b). If all comparisons fail, b is consed onto
  the front of the result.

However, there is no guarantee that = will be applied to every pair of arguments from A and B. In particular, if A is eq? to B, the operation may immediately terminate.

In the n-ary case, the two-argument list-union operation is simply folded across the argument lists.

```scheme
(lset-union eq? '(a b c d e) '(a e i o u)) =>
        (u o i a b c d e)

;; Repeated elements in LIST1 are preserved.
(lset-union eq? '(a a c) '(x a x)) => (x a a c)

;; Trivial cases
(lset-union eq?) => ()
(lset-union eq? '(a b c)) => (a b c)
```

##### `(lset-intersection = list1 list2 ...)`

Returns the intersection of the lists, using = for the element-equality procedure.

The intersection of lists A and B is comprised of every element of A
that is = to some element of B: (= a b), for a in A, and b in B. Note
this implies that an element which appears in B and multiple times in
list A will also appear multiple times in the result.

The order in which elements appear in the result is the same as they
appear in list1 -- that is, lset-intersection essentially filters
list1, without disarranging element order. The result may share a
common tail with list1.

In the n-ary case, the two-argument list-intersection operation is
simply folded across the argument lists. However, the dynamic order in
which the applications of = are made is not specified. The procedure
may check an element of list1 for membership in every other list
before proceeding to consider the next element of list1, or it may
completely intersect list1 and list2 before proceeding to list3, or it
may go about its work in some third order.

```scheme
(lset-intersection eq? '(a b c d e) '(a e i o u)) => (a e)

;; Repeated elements in LIST1 are preserved.
(lset-intersection eq? '(a x y a) '(x a x z)) => '(a x a)

(lset-intersection eq? '(a b c)) => (a b c)     ; Trivial case
```

##### `(lset-difference = list1 list2 ...)`

Returns the difference of the lists, using = for the element-equality
procedure -- all the elements of list1 that are not = to any element
from one of the other listi parameters.

The = procedure's first argument is always an element of list1; its
second is an element of one of the other listi. Elements that are
repeated multiple times in the list1 parameter will occur multiple
times in the result. The order in which elements appear in the result
is the same as they appear in list1 -- that is, lset-difference
essentially filters list1, without disarranging element order. The
result may share a common tail with list1. The dynamic order in which
the applications of = are made is not specified. The procedure may
check an element of list1 for membership in every other list before
proceeding to consider the next element of list1, or it may completely
compute the difference of list1 and list2 before proceeding to list3,
or it may go about its work in some third order.

```scheme
(lset-difference eq? '(a b c d e) '(a e i o u)) => (b c d)

(lset-difference eq? '(a b c)) => (a b c) ; Trivial case
```

##### `(lset-xor = list1 ...)

Returns the exclusive-or of the sets, using = for the element-equality
procedure. If there are exactly two lists, this is all the elements
that appear in exactly one of the two lists. The operation is
associative, and thus extends to the n-ary case -- the elements that
appear in an odd number of the lists. The result may share a common
tail with any of the listi parameters.

More precisely, for two lists A and B, A xor B is a list of

- every element a of A such that there is no element b of B such that (= a b), and
- every element b of B such that there is no element a of A such that (= b a).

However, an implementation is allowed to assume that = is symmetric -- that is, that

```scheme
(= a b) => (= b a).
```

This means, for example, that if a comparison (= a b) produces true
for some a in A and b in B, both a and b may be removed from inclusion
in the result.

In the n-ary case, the binary-xor operation is simply folded across
the lists.

```scheme
(lset-xor eq? '(a b c d e) '(a e i o u)) => (d c b i o u)

;; Trivial cases.
(lset-xor eq?) => ()
(lset-xor eq? '(a b c d e)) => (a b c d e)
```

##### `(lset-diff+intersection = list1 list2 ...)`

Returns two values -- the difference and the intersection of the
lists. Is equivalent to:

```
(values (lset-difference = list1 list2 ...)
        (lset-intersection = list1
                             (lset-union = list2 ...)))
```

But can be implemented more efficiently.

The = procedure's first argument is an element of list1; its second is
an element of one of the other listi.

Either of the answer lists may share a common tail with list1. This
operation essentially partitions list1.

##### `(lset-union! list1 ...)`

##### `(lset-intersection! list1 ...)`

##### `(lset-difference! list1 ...)`

##### `(lset-xor! list1 ...)`

##### `(lset-diff+intersection! list1 ...)`

These are linear-update variants. They are allowed, but not required,
to use the cons cells in their first list parameter to construct their
answer. lset-union! is permitted to recycle cons cells from any of its
list arguments.

#### Primitive side-effects

##### `(set-car! pair object)`

##### `(set-cdr! pair object)`

These procedures store object in the car and cdr field of pair,
respectively. The value returned is unspecified.

```scheme
(define (f) (list 'not-a-constant-list))
(define (g) '(constant-list))
(set-car! (f) 3) =>  *unspecified*
(set-car! (g) 3) =>  *error*
```
