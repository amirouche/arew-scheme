# `(scheme lseq)`

This library is based on
[SRFI-127](https://srfi.schemers.org/srfi-127/).

Lazy sequences (or lseqs, pronounced "ell-seeks") are a generalization
of lists. In particular, an lseq is either a proper list or a dotted
list whose last cdr is a SRFI 121 generator. A generator is a
procedure that can be invoked with no arguments in order to lazily
supply additional elements of the lseq. When a generator has no more
elements to return, it returns an end-of-file object. Consequently,
lazy sequences cannot reliably contain end-of-file objects.

This SRFI provides a set of procedures suitable for operating on lazy
sequences based on SRFI 1.

## `(generator->lseq generator)`

Returns an lseq whose elements are the values generated by
generator. The exact behavior is as follows:

- Generator is invoked with no arguments to produce an object obj.

- If obj is an end-of-file object, the empty list is returned.

- Otherwise, a newly allocated pair whose car is obj and whose cdr is
  generator is returned.

```scheme
(generator->lseq (make-iota-generator +inf.0 1))
```

## `(lseq? x)`

Returns #t if x is an lseq. This procedure may also return #t if x is
an improper list whose last cdr is a procedure that requires
arguments, since there is no portable way to examine a procedure to
determine how many arguments it requires. Otherwise it returns #f.

## `(lseq=? elt=? lseq1 lseq2)`

Determines lseq equality, given an element-equality procedure. Two
lseqs are equal if they are of the same length, and their
corresponding elements are equal, as determined by elt=?. When elt=?
is called, its first argument is always from lseq1 and its second
argument is from lseq2.

The dynamic order in which the elt=? procedure is applied to pairs of
elements is not specified.

The elt=? procedure must be consistent with eq?. This implies that two
lseqs which are eq? are always lseq=?, as well; implementations may
exploit this fact to "short-cut" the element-by-element equality
tests.

## `(lseq-car lseq)`

## `(lseq-first lseq)`

These procedures are synonymous. They return the first element of
lseq. They are included for completeness, as they are the same as
car. It is an error to apply them to an empty lseq.

## `(lseq-cdr lseq)`

## `(lseq-rest lseq)`

These procedures are synonymous. They return an lseq with the contents
of lseq except for the first element. The exact behavior is as
follows:

- If lseq is a pair whose cdr is a procedure, then the procedure is
  invoked with no arguments to produce an object obj.

- If obj is an end-of-file object, then the cdr of lseq is set to the
  empty list, which is returned.

- If obj is any other object, then a new pair is allocated whose car
  is obj and whose cdr is the cdr of lseq (i.e. the procedure). The
  cdr of lseq is set to the newly allocated pair, which is returned.

- If lseq is a pair whose cdr is not a procedure, then the cdr is
  returned.

- If lseq is not a pair, it is an error.

Implementations that inline cdr are advised to inline lseq-cdr if
possible.

## `(lseq-ref lseq i)`

Returns the ith element of lseq. (This is the same as (lseq-first
(lseq-drop lseq i)).) It is an error if i >= n, where n is the length
of lseq.

```scheme
(lseq-ref '(a b c d) 2) => c
```

## `(lseq-take lseq i)`

## `(lseq-drop lseq i)`

lseq-take lazily returns the first i elements of lseq. lseq-drop
returns all but the first i elements of lseq.

```scheme
(lseq-take '(a b c d e)  2) => (a b)
(lseq-drop '(a b c d e)  2) => (c d e)
```

lseq-drop is exactly equivalent to performing i lseq-rest operations
on lseq.

## `(lseq-realize lseq)`

Repeatedly applies lseq-cdr to lseq until its generator (if there is
one) has been exhausted, and returns lseq, which is now guaranteed to
be a proper list. This procedure can be called on an arbitrary lseq
before passing it to a procedure which only accepts lists. However, if
the generator never returns an end-of-file object, lseq-realize will
never return.

## `(lseq->generator lseq)`

Returns a generator which when invoked will return all the elements of
lseq, including any that have not yet been realized.

## `(lseq-length lseq)`

Returns the length of its argument, which is the non-negative integer
n such that lseq-rest applied n times to the lseq produces an empty
lseq. lseq must be finite, or this procedure will not return.

## `(lseq-append lseq ...)`

Returns an lseq that lazily contains all the elements of all the lseqs
in order.

## `(lseq-zip lseq1 lseq2 ...)`

If lseq-zip is passed n lseqs, it lazily returns an lseq each element
of which is an n-element list comprised of the corresponding elements
from the lseqs. If any of the lseqs are finite in length, the result
is as long as the shortest lseq.

```scheme
    (lseq-zip '(one two three)
         (generator->lseq (make-iota-generator +inf.0 1 1))
         (generator->lseq (make-repeating-generator) '(odd even))))
        => ((one 1 odd) (two 2 even) (three 3 odd))

    (lseq-zip '(1 2 3)) => ((1) (2) (3))
```
## `(lseq-map proc lseq1 lseq2 ...)`

The lseq-map procedure lazily applies proc element-wise to the
corresponding elements of the lseqs, where proc is a procedure taking
as many arguments as there are lseqs and returning a single value, and
returns an lseq of the results in order. The dynamic order in which
proc is applied to the elements of the lseqs is unspecified.

```scheme
    (lseq-map
      (lambda (x) (lseq-car (lseq-cdr x)))
      '((a b) (d e) (g h))) =>  (b e h)

    (lseq-map (lambda (n) (expt n n))
         (make-iota-generator +inf.0 1 1)
        =>  (1 4 27 256 3125 ...)

    (lseq-map + '(1 2 3) '(4 5 6)) =>  (5 7 9)

    (let ((count 0))
      (lseq-map (lambda (ignored)
             (set! count (+ count 1))
             count)
           '(a b))) =>  (1 2) or (2 1)
```

## `(lseq-for-each proc lseq1 lseq2 ...)

The arguments to lseq-for-each are like the arguments to lseq-map, but
lseq-for-each calls proc for its side effects rather than for its
values. Unlike lseq-map, lseq-for-each is guaranteed to call proc on
the elements of the lseqs in order from the first element(s) to the
last, and the value returned by lseq-for-each is unspecified.

If none of the lseqs are finite, lseq-for-each never returns.

```scheme
    (let ((v (make-vector 5)))
      (lseq-for-each (let ((count 0))
                       (lambda (i)
                         (vector-set! v count (* i i))
                         (set! count (+ count 1))))
                     '(0 1 2 3 4))
      v)
      =>  (#0 1 2 3 4)
```

## `(lseq-filter pred lseq)`
## `(lseq-remove pred lseq)`

The procedure lseq-filter lazily returns an lseq that contains only
the elements of lseq that satisfy pred.

The procedure lseq-remove is the same as lseq-filter, except that it
returns elements that do not satisfy pred. These procedures are
guaranteed to call pred on the elements of the lseqs in sequence
order.

```scheme
(lseq-filter odd? (generator->lseq (make-range-generator 1 5)))
;; =>  (1 3)

(lseq-remove odd? (generator->lseq (make-range-generator 1 5)))
;; =>  (2 4)
```

## `(lseq-find-tail pred lseq)`

Returns the longest tail of lseq whose first element satisfies pred,
or #f if no element does. The predicate is guaranteed to be evaluated
on the elements of lseq in sequence order, and only as often as
necessary.

lseq-find-tail can be viewed as a general-predicate variant of the
lseq-member function.

Examples:

```scheme
(lseq-find-tail even? '(3 1 37 -8 -5 0 0)) => (-8 -5 0 0)
(lseq-find-tail even? '(3 1 37 -5)) => #f

;; equivalent to (lseq-member elt lseq)
(lseq-find-tail (lambda (elt) (equal? x elt)) lseq)
```

## `(lseq-take-while pred lseq)`

Lazily returns the longest initial prefix of lseq whose elements all
satisfy the predicate pred.

```scheme
(lseq-take-while even? '(2 18 3 10 22 9)) => (2 18)
```

## `(lseq-drop-while pred lseq)`

Drops the longest initial prefix of lseq whose elements all satisfy
the predicate pred, and returns the rest of the lseq.

```scheme
(lseq-drop-while even? '(2 18 3 10 22 9)) => (3 10 22 9)
```

Note that lseq-drop-while is essentially lseq-find-tail where the
sense of the predicate is inverted: lseq-find-tail searches until it
finds an element satisfying the predicate; lseq-drop-while searches
until it finds an element that doesn't satisfy the predicate.

## `(lseq-any pred lseq1 lseq2 ...)`

Applies pred to successive elements of the lseqs, returning true if
pred returns true on any application. If an application returns a true
value, lseq-any immediately returns that value. Otherwise, it iterates
until a true value is produced or one of the lseqs runs out of values;
in the latter case, lseq-any returns #f. It is an error if pred does
not accept the same number of arguments as there are lseqs and return
a boolean result.

Note the difference between lseq-find and lseq-any — lseq-find returns
the element that satisfied the predicate; lseq-any returns the true
value that the predicate produced.

Like lseq-every, lseq-any's name does not end with a question mark —
this is to indicate that it does not return a simple boolean (#t or
#f), but a general value.

```scheme
    (lseq-any integer? '(a 3 b 2.7))   => #t
    (lseq-any integer? '(a 3.1 b 2.7)) => #f
    (lseq-any < '(3 1 4 1 5)
           '(2 7 1 8 2)) => #t

    (define (factorial n)
      (cond
        ((< n 0) #f)
        ((= n 0) 1)
        (else (* n (factorial (- n 1))))))
    (lseq-any factorial '(-1 -2 3 4))
            => 6
```

## `(lseq-every pred lseq1 lseq2 ...)`

Applies pred to successive elements of the lseqs, returning true if
the predicate returns true on every application. If an application
returns a false value, lseq-every immediately returns that
value. Otherwise, it iterates until a false value is produced or one
of the lseqs runs out of values; in the latter case, lseq-every
returns the last value returned by pred, or #t if pred was never
invoked. It is an error if pred does not accept the same number of
arguments as there are lseqs and return a boolean result.

Like lseq-any, lseq-every's name does not end with a question mark —
this is to indicate that it does not return a simple boolean (#t or
#f), but a general value.

```scheme
    (lseq-every factorial '(1 2 3 4))
            => 24
```

## `(lseq-index pred lseq1 lseq2 ...)`

Return the index of the leftmost element that satisfies pred.

Applies pred to successive elements of the lseqs, returning an index
usable with lseq-ref if the predicate returns true. Otherwise, it
iterates until one of the lseqs runs out of values, in which case #f
is returned.

It is an error if pred does not accept the same number of arguments as
there are lseqs and return a boolean result.

The iteration stops when one of the lseqs runs out of values; in this
case, lseq-index returns #f.

```scheme
(lseq-index even? '(3 1 4 1 5 9)) => 2
(lseq-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)) => 1
(lseq-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)) => #f
```

## `(lseq-member x lseq [ pred ])`

## `(lseq-memq x lseq)`

## `(lseq-memv x lseq)`

These procedures return the longest tail of lseq whose first element
is x, where the tails of lseq are the non-empty lseqs returned by
(lseq-drop lseq i) for i less than the length of lseq. If x does not
occur in lseq, then #f is returned. lseq-memq uses eq? to compare x
with the elements of lseq, while lseq-memv uses eqv?, and lseq-member
uses pred, which defaults to equal?.

```scheme
(lseq-memq 'a '(a b c))           =>  (a b c)
(lseq-memq 'b '(a b c))           =>  (b c)
(lseq-memq 'a '(b c d))           =>  #f
(lseq-memq (list 'a) '(b (a) c)) =>  #f
(lseq-member (list 'a)
'(b (a) c))           =>  ((a) c)
(lseq-memq 101 '(100 101 102))    =>  *unspecified*
(lseq-memv 101 '(100 101 102))    =>  (101 102)
```

The equality procedure is used to compare the elements ei of lseq to
the key x in this way: the first argument is always x, and the second
argument is one of the lseq elements. Thus one can reliably find the
first element of lseq that is greater than five with (lseq-member 5
lseq <)

Note that fully general lseq searching may be performed with the
lseq-find-tail procedure, e.g.

```scheme
(lseq-find-tail even? lseq) ; Find the first elt with an even key.
```