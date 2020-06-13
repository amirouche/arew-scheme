
## `(arew srfi srfi-158)`

This is based on [SRFI-158](https://srfi.schemers.org/srfi-158/srfi-158.html)

### Abstract

This SRFI defines utility procedures that create, transform, and
consume generators. A generator is simply a procedure with no
arguments that works as a source of values. Every time it is called,
it yields a value. Generators may be finite or infinite; a finite
generator returns an end-of-file object to indicate that it is
exhausted. For example, read-char, read-line, and read are generators
that generate characters, lines, and objects from the current input
port. Generators provide lightweight laziness.

This SRFI also defines procedures that return accumulators. An
accumulator is the inverse of a generator: it is a procedure of one
argument that works as a sink of values.

### Reference

#### Generator

##### `(generator arg ...)`

The simplest finite generator. Generates each of its arguments in
turn. When no arguments are provided, it returns an empty generator
that generates no values.

##### `(circular-generator arg ...)`

The simplest infinite generator. Generates each of its arguments in
turn, then generates them again in turn, and so on forever.

##### `(make-iota-generator count [start [step]])`

Creates a finite generator of a sequence of count numbers. The
sequence begins with start (which defaults to 0) and increases by step
(which defaults to 1). If both start and step are exact, it generates
exact numbers; otherwise it generates inexact numbers. The exactness
of count doesn't affect the exactness of the results.

##### `(make-range-generator start [end [step]])`

Creates a generator of a sequence of numbers. The sequence begins with
start, increases by step (default 1), and continues while the number
is less than end, or forever if end is omitted. If both start and step
are exact, it generates exact numbers; otherwise it generates inexact
numbers. The exactness of end doesn't affect the exactness of the
results.

##### `(make-coroutine-generator proc)`

Creates a generator from a coroutine.

The proc argument is a procedure that takes one argument, yield. When
called, make-coroutine-generator immediately returns a generator
g. When g is called, proc runs until it calls yield. Calling yield
causes the execution of proc to be suspended, and g returns the value
passed to yield.

Whether this generator is finite or infinite depends on the behavior
of proc. If proc returns, it is the end of the sequence — g returns an
end-of-file object from then on. The return value of proc is ignored.

The following code creates a generator that produces a series 0, 1,
and 2 (effectively the same as (make-range-generator 0 3)) and binds
it to g.

```scheme
(define g
  (make-coroutine-generator
   (lambda (yield) (let loop ((i 0))
               (when (< i 3) (yield i) (loop (+ i 1)))))))

(generator->list g) ;; => (0 1 2)
```

##### `(list->generator list)`

Convert `LIST` into a generator.

##### `(vector->generator vector [start [end]])`

##### `(reverse-vector->generator vector [start [end]])`

##### `(string->generator string [start [end]])`

##### `(bytevector->generator bytevector [start [end]])`

These procedures return generators that yield each element of the
given argument. Mutating the underlying object will affect the results
of the generator.

```scheme
(generator->list (list->generator '(1 2 3 4 5)))
  ;; => (1 2 3 4 5)
(generator->list (vector->generator '#(1 2 3 4 5)))
  ;; => (1 2 3 4 5)
(generator->list (reverse-vector->generator '#(1 2 3 4 5)))
  ;; => (5 4 3 2 1)
(generator->list (string->generator "abcde"))
  ;; => (#\a #\b #\c #\d #\e)
```

The generators returned by the constructors are exhausted once all
elements are retrieved; the optional start-th and end-th arguments can
limit the range the generator walks across.

For reverse-vector->generator, the first value is the element right
before the end-th element, and the last value is the start-th
element. For all the other constructors, the first value the generator
yields is the start-th element, and it ends right before the end-th
element.

```scheme
(generator->list (vector->generator '#(a b c d e) 2))
  ;; => (c d e)
(generator->list (vector->generator '#(a b c d e) 2 4))
  ;; => (c d)
(generator->list (reverse-vector->generator '#(a b c d e) 2))
  ;; => (e d c)
(generator->list (reverse-vector->generator '#(a b c d e) 2 4))
  ;; => (d c)
(generator->list (reverse-vector->generator '#(a b c d e) 0 2))
  ;; => (b a)
```

##### `(make-for-each-generator for-each obj)`

A generator constructor that converts any collection obj to a
generator that returns its elements using a for-each procedure
appropriate for obj. This must be a procedure that when called as
(for-each proc obj) calls proc on each element of obj. Examples of
such procedures are for-each, string-for-each, and vector-for-each
from R7RS. The value returned by for-each is ignored. The generator is
finite if the collection is finite, which would typically be the case.

The collections need not be conventional ones (lists, strings, etc.)
as long as for-each can invoke a procedure on everything that counts
as a member. For example, the following procedure allows
for-each-generator to generate the digits of an integer from least to
most significant:

```scheme
(define (for-each-digit proc n)
  (when (> n 0)
    (let-values (((div rem) (truncate/ n 10)))
      (proc rem)
      (for-each-digit proc div))))
```

##### `(make-unfold-generator stop? mapper successor seed)`

A generator constructor similar to `(scheme list)` unfold.

The stop? predicate takes a seed value and determines whether to
stop. The mapper procedure calculates a value to be returned by the
generator from a seed value. The successor procedure calculates the
next seed value from the current seed value.

For each call of the resulting generator, stop? is called with the
current seed value. If it returns true, then the generator returns an
end-of-file object. Otherwise, it applies mapper to the current seed
value to get the value to return, and uses successor to update the
seed value.

This generator is finite unless stop? never returns true.

```scheme
(generator->list (make-unfold-generator
                      (lambda (s) (> s 5))
                      (lambda (s) (* s 2))
                      (lambda (s) (+ s 1))
                      0))
  ;; => (0 2 4 6 8 10)
```

##### `(gcons* item ... generator)`

Returns a generator that adds items in front of gen. Once the items
have been consumed, the generator is guaranteed to tail-call gen.

```scheme
(generator->list (gcons* 'a 'b (make-range-generator 0 2)))
 ;; => (a b 0 1)
```

##### `(gappend generator ...)`

Returns a generator that yields the items from the first given
generator, and once it is exhausted, from the second generator, and so
on.

```scheme
(generator->list (gappend (make-range-generator 0 3) (make-range-generator 0 2)))
 ;; => (0 1 2 0 1)

(generator->list (gappend))
 ;; => ()
```

##### `(gflatten generator)`

Returns a generator that yields the elements of the lists produced by
the given generator.

##### `(ggroup generator k [padding])`

Returns a generator that yields lists of k items from the given
generator. If fewer than k elements are available for the last list,
and padding is absent, the short list is returned; otherwise, it is
padded by padding to length k.

##### `(gmerge less-than generator1 ...)`

Returns a generator that yields the items from the given generators in
the order dictated by less-than. If the items are equal, the leftmost
item is used first. When all of given generators are exhausted, the
returned generator is exhausted also.

As a special case, if only one generator is given, it is returned.

##### `(gmap proc generator ...)`

When only one generator is given, returns a generator that yields the
items from the given generator after invoking proc on them.

When more than one generator is given, each item of the resulting
generator is a result of applying proc to the items from each
generator. If any of input generator is exhausted, the resulting
generator is also exhausted.

Note: This differs from generator-map->list, which consumes all values
at once and returns the results as a list, while gmap returns a
generator immediately without consuming input.

```scheme
(generator->list (gmap - (make-range-generator 0 3)))
 ;; => (0 -1 -2)

(generator->list (gmap cons (generator 1 2 3) (generator 4 5)))
 ;; => ((1 . 4) (2 . 5))
```

##### `(gcombine proc seed generator generator2)`

A generator for mapping with state. It yields a sequence of sub-folds
over proc.

The proc argument is a procedure that takes as many arguments as the
input generators plus one. It is called as (proc v1 v2 … seed), where
v1, v2, … are the values yielded from the input generators, and seed
is the current seed value. It must return two values, the yielding
value and the next seed. The result generator is exhausted when any of
the genn generators is exhausted, at which time all the others are in
an undefined state.

##### `(gfilter predicate generator)`

##### `(gremove predicate generator)`

Returns generators that yield the items from the source generator,
except those on which pred answers false or true respectively.

##### `(gstate-filter proc seed generator)`

Returns a generator that obtains items from the source generator and
passes an item and a state (whose initial value is seed) as arguments
to proc. Proc in turn returns two values, a boolean and a new value of
the state. If the boolean is true, the item is returned; otherwise,
this algorithm is repeated until gen is exhausted, at which point the
returned generator is also exhausted. The final value of the state is
discarded.

##### `(gtake gen k [padding])`
##### `(gdrop gen k)`

These are generator analogues of SRFI 1 take and drop. Gtake returns a
generator that yields (at most) the first k items of the source
generator, while gdrop returns a generator that skips the first k
items of the source generator.

These won't complain if the source generator is exhausted before
generating k items. By default, the generator returned by gtake
terminates when the source generator does, but if you provide the
padding argument, then the returned generator will yield exactly k
items, using the padding value as needed to provide sufficient
additional values.

##### `gtake-while pred gen`
##### `gdrop-while pred gen`

The generator analogues of SRFI-1 take-while and drop-while. The
generator returned from gtake-while yields items from the source
generator as long as pred returns true for each. The generator
returned from gdrop-while first reads and discards values from the
source generator while pred returns true for them, then starts
yielding items returned by the source.

##### `(gdelete item gen [=])`

Creates a generator that returns whatever gen returns, except for any
items that are the same as item in the sense of =, which defaults to
equal?. The = predicate is passed exactly two arguments, of which the
first was generated by gen before the second.

```scheme
(generator->list (gdelete 3 (generator 1 2 3 4 5 3 6 7)))
  ;; => (1 2 4 5 6 7)
```

##### `(gdelete-neighbor-dups gen [=])`

Creates a generator that returns whatever gen returns, except for any
items that are equal to the preceding item in the sense of =, which
defaults to equal?. The = predicate is passed exactly two arguments,
of which the first was generated by gen before the second.

```scheme
(generator->list (gdelete-neighbor-dups (list->generator '(a a b c a a a d c))))
  ;; => (a b c a d c)
```

##### `(gindex value-gen index-gen)`

Creates a  generator that returns  elements of value-gen  specified by
the indices  (non-negative exact integers) generated  by index-gen. It
is an  error if  the indices  are not strictly  increasing, or  if any
index  exceeds the  number  of elements  generated  by value-gen.  The
result generator is  exhausted when either generator  is exhausted, at
which time the other is in an undefined state.

```scheme
(generator->list (gindex (list->generator '(a b c d e f))
                         (list->generator '(0 2 4))))
  ;; => (a c e)
```

##### `(gselect value-gen truth-gen)`

Creates a generator that returns elements of value-gen that correspond
to the values generated by truth-gen. If the current value of
truth-gen is true, the current value of value-gen is generated, but
otherwise not. The result generator is exhausted when either generator
is exhausted, at which time the other is in an undefined state.

```scheme
(generator->list (gselect (list->generator '(a b c d e f))
                          (list->generator '(#t #f #f #t #t #f))))
  ;; => (a d e)
```

##### `(generator->list generator [k])`

Reads items from generator and returns a newly allocated list of
them. By default, it reads until the generator is exhausted.

If an optional argument k is given, it must be a non-negative integer,
and the list ends when either k items are consumed, or generator is
exhausted; therefore generator can be infinite in this case.

##### `(generator->reverse-list generator [k])`

Reads items from generator and returns a newly allocated list of them
in reverse order. By default, this reads until the generator is
exhausted.

If an optional argument k is given, it must be a non-negative integer,
and the list ends when either k items are read, or generator is
exhausted; therefore generator can be infinite in this case.

##### `(generator->vector generator [k])`

Reads items from generator and returns a newly allocated vector of
them. By default, it reads until the generator is exhausted.

If an optional argument k is given, it must be a non-negative integer,
and the list ends when either k items are consumed, or generator is
exhausted; therefore generator can be infinite in this case.

##### `(generator->vector! vector at generator)`

Reads items from generator and puts them into vector starting at index
at, until vector is full or generator is exhausted. Generator can be
infinite. The number of elements generated is returned.

##### `(generator->string generator [k])`

Reads items from generator and returns a newly allocated string of
them. It is an error if the items are not characters. By default, it
reads until the generator is exhausted.

If an optional argument k is given, it must be a non-negative integer,
and the string ends when either k items are consumed, or generator is
exhausted; therefore generator can be infinite in this case.

##### `(generator-fold proc seed generator ...)`

Works like `(scheme list)` fold on the values generated by the
generator arguments.

When one generator is given, for each value v generated by gen, proc
is called as (proc v r), where r is the current accumulated result;
the initial value of the accumulated result is seed, and the return
value from proc becomes the next accumulated result. When gen is
exhausted, the accumulated result at that time is returned from
generator-fold.

When more than one generator is given, proc is invoked on the values
returned by all the generator arguments followed by the current
accumulated result. The procedure terminates when any of the genn
generators is exhausted, at which time all the others are in an
undefined state.

```scheme
(with-input-from-string "a b c d e"
  (lambda () (generator-fold cons 'z read)))
  ;; => (e d c b a . z)
```

##### `(generator-for-each proc generator ...)`

A generator analogue of for-each that consumes generated values using
side effects. Repeatedly applies proc on the values yielded by gen,
gen2 … until any one of the generators is exhausted, at which time all
the others are in an undefined state. The values returned from proc
are discarded. Returns an unspecified value.

##### `(generator-map->list proc generator ...)`

A generator analogue of map that consumes generated values, processes
them through a mapping function, and returns a list of the mapped
values. Repeatedly applies proc on the values yielded by gen, gen2 …
until any one of the generators is exhausted, at which time all the
others are in an undefined state. The values returned from proc are
accumulated into a list, which is returned.

##### `(generator-find predicate generator)`

Returns the first item from the generator gen that satisfies the
predicate pred, or #f if no such item is found before gen is
exhausted. If gen is infinite, generator-find will not return if it
cannot find an appropriate item.

##### `(generator-count predicate generator)`

Returns the number of items available from the generator gen that
satisfy the predicate pred.

##### `(generator-any predicate generator)`

Applies predicate to each item from gen. As soon as it yields a true value,
the value is returned without consuming the rest of gen. If gen is
exhausted, returns #f.

##### `(generator-every predicate generator)`

Applies pred to each item from gen. As soon as it yields a false
value, the value is returned without consuming the rest of gen. If gen
is exhausted, returns the last value returned by pred, or #t if pred
was never called.

##### `(generator-unfold gen unfold arg ...)`

Equivalent to `(unfold eof-object? (lambda (x) x) (lambda (x) (gen))
(gen) arg ...)`. The values of gen are unfolded into the collection
that unfold creates.

The signature of the unfold procedure is (unfold stop? mapper
successor seed args ...). Note that the vector-unfold and
vector-unfold-right of SRFI 43 and SRFI 133 do not have this signature
and cannot be used with this procedure. To unfold into a vector, use
SRFI 1's unfold and then apply list->vector to the result.

```scheme
;; Iterates over string and unfolds into a list using SRFI 1 unfold
(generator-unfold (make-for-each-generator string-for-each "abc") unfold)
;; => (#\a #\b #\c)
```

#### Accumulator

##### `(make-accumulator kons knil finalizer)`

Returns an accumulator that, when invoked on an object other than an
end-of-file object, invokes kons on its argument and the accumulator's
current state, using the same order as a function passed to fold. It
then sets the accumulator's state to the value returned by kons and
returns an unspecified value. The initial state of the accumulator is
set to knil. However, if an end-of-file object is passed to the
accumulator, it returns the result of tail-calling the procedure
finalizer on the state. Repeated calls with an end-of-file object will
reinvoke finalizer.

##### `(count-accumulator)`

Returns an accumulator that, when invoked on an object, adds 1 to a
count inside the accumulator and returns an unspecified
value. However, if an end-of-file object is passed, the accumulator
returns the count.

##### `(list-accumulator)`

Returns an accumulator that, when invoked on an object, adds that
object to a list inside the accumulator in order of accumulation and
returns an unspecified value. However, if an end-of-file object is
passed, the accumulator returns the list.

##### `(reverse-list-accumulator)`

Returns an accumulator that, when invoked on an object, adds that
object to a list inside the accumulator in reverse order of
accumulation and returns an unspecified value. However, if an
end-of-file object is passed, the accumulator returns the list.

##### `(vector-accumulator)`

Returns an accumulator that, when invoked on an object, adds that
object to a vector inside the accumulator in order of accumulation and
returns an unspecified value. However, if an end-of-file object is
passed, the accumulator returns the vector.

##### `(reverse-vector-accumulator)`

Returns an accumulator that, when invoked on an object, adds that
object to a vector inside the accumulator in reverse order of
accumulation and returns an unspecified value. However, if an
end-of-file object is passed, the accumulator returns the vector.

##### `(vector-accumulator! vector at)`

Returns an accumulator that, when invoked on an object, adds that
object to consecutive positions of vector starting at at in order of
accumulation. It is an error to try to accumulate more objects than
vector will hold. An unspecified value is returned. However, if an
end-of-file object is passed, the accumulator returns vector.

##### `(string-accumulator)`

Returns an accumulator that, when invoked on a character, adds that
character to a string inside the accumulator in order of accumulation
and returns an unspecified value. However, if an end-of-file object is
passed, the accumulator returns the string.

##### `(bytevector-accumulator)`

Returns an accumulator that, when invoked on a byte, adds that integer
to a bytevector inside the accumulator in order of accumulation and
returns an unspecified value. However, if an end-of-file object is
passed, the accumulator returns the bytevector.

##### `(bytevector-accumulator! bytevector at)`

Returns an accumulator that, when invoked on a byte, adds that byte to
consecutive positions of bytevector starting at at in order of
accumulation. It is an error to try to accumulate more bytes than
vector will hold. An unspecified value is returned. However, if an
end-of-file object is passed, the accumulator returns bytevector.

##### `(sum-accumulator)`

Returns an accumulator that, when invoked on a number, adds that
number to a sum inside the accumulator in order of accumulation and
returns an unspecified value. However, if an end-of-file object is
passed, the accumulator returns the sum.

##### `(product-accumulator)

Returns an accumulator that, when invoked on a number, multiplies that
number to a product inside the accumulator in order of accumulation
and returns an unspecified value. However, if an end-of-file object is
passed, the accumulator returns the product.
