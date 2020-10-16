# `(scheme charset)`

This library is based on
[SRFI-14](https://srfi.schemers.org/srfi-14/).

The ability to efficiently represent and manipulate sets of characters
is an unglamorous but very useful capability for text-processing code
-- one that tends to pop up in the definitions of other libraries.

## `(char-set? obj)`

Is the object obj a character set?


## `(char-set= cs1 ...)`

Are the character sets equal?

Boundary cases:

```scheme
(char-set=) => true
(char-set= cs) => true
```

Rationale: transitive binary relations are generally extended to n-ary
relations in Scheme, which enables clearer, more concise code to be
written. While the zero-argument and one-argument cases will almost
certainly not arise in first-order uses of such relations, they may
well arise in higher-order cases or macro-generated code. E.g.,
consider

```scheme
(apply char-set= cset-list)
```

This is well-defined if the list is empty or a singleton list. Hence
we extend these relations to any number of arguments. Implementors
have reported actual uses of n-ary relations in higher-order cases
allowing for fewer than two arguments. The way of Scheme is to handle
the general case; we provide the fully general extension.

A counter-argument to this extension is that R5RS's transitive binary
arithmetic relations (=, <, etc.) require at least two arguments,
hence this decision is a break with the prior convention -- although
it is at least one that is backwards-compatible.

## `(char-set<= cs1 ...)`

Returns true if every character set csi is a subset of character set csi+1.

Boundary cases:

```scheme
(char-set<=) => true
(char-set<= cs) => true
```

Rationale: See char-set= for discussion of zero- and one-argument
applications. Consider testing a list of char-sets for monotonicity
with

```scheme
(apply char-set<= cset-list)
```

## `(char-set-hash cs [bound])`

Compute a hash value for the character set cs. Bound is a non-negative
exact integer specifying the range of the hash function. A positive
value restricts the return value to the range [0,bound).

If bound is either zero or not given, the implementation may use an
implementation-specific default value, chosen to be as large as is
efficiently practical. For instance, the default range might be chosen
for a given implementation to map all strings into the range of
integers that can be represented with a single machine word.

Invariant:

```scheme
(char-set= cs1 cs2) => (= (char-set-hash cs1 b) (char-set-hash cs2 b))
```

A legal but nonetheless discouraged implementation:

```scheme
(define (char-set-hash cs . maybe-bound) 1)
```

Rationale: allowing the user to specify an explicit bound simplifies
user code by removing the mod operation that typically accompanies
every hash computation, and also may allow the implementation of the
hash function to exploit a reduced range to efficiently compute the
hash value. E.g., for small bounds, the hash function may be computed
in a fashion such that intermediate values never overflow into bignum
integers, allowing the implementor to provide a fixnum-specific "fast
path" for computing the common cases very rapidly.

## `(char-set-cursor cset)`

## `(char-set-ref cset cursor)`

## `(char-set-cursor-next cset cursor)`

## `(end-of-char-set? cursor)`

Cursors are a low-level facility for iterating over the characters in
a set. A cursor is a value that indexes a character in a char
set. char-set-cursor produces a new cursor for a given char set. The
set element indexed by the cursor is fetched with char-set-ref. A
cursor index is incremented with char-set-cursor-next; in this way,
code can step through every character in a char set. Stepping a cursor
"past the end" of a char set produces a cursor that answers true to
end-of-char-set?. It is an error to pass such a cursor to char-set-ref
or to char-set-cursor-next.

A cursor value may not be used in conjunction with a different
character set; if it is passed to char-set-ref or char-set-cursor-next
with a character set other than the one used to create it, the results
and effects are undefined.

Cursor values are not necessarily distinct from other types. They may
be integers, linked lists, records, procedures or other values. This
license is granted to allow cursors to be very "lightweight" values
suitable for tight iteration, even in fairly simple implementations.

Note that these primitives are necessary to export an iteration facility for char sets to loop macros.

Example:

```scheme
    (define cs (char-set #\G #\a #\T #\e #\c #\h))

    ;; Collect elts of CS into a list.
    (let lp ((cur (char-set-cursor cs)) (ans '()))
      (if (end-of-char-set? cur) ans
          (lp (char-set-cursor-next cs cur)
              (cons (char-set-ref cs cur) ans))))
      => (#\G #\T #\a #\c #\e #\h)

    ;; Equivalently, using a list unfold (from SRFI 1):
    (unfold-right end-of-char-set?
                  (curry char-set-ref cs)
    	      (curry char-set-cursor-next cs)
    	      (char-set-cursor cs))
      => (#\G #\T #\a #\c #\e #\h)
```

Rationale: Note that the cursor API's four functions "fit" the
functional protocol used by the unfolders provided by the list, string
and char-set SRFIs (see the example above). By way of contrast, here
is a simpler, two-function API that was rejected for failing this
criterion. Besides char-set-cursor, it provided a single function that
mapped a cursor and a character set to two values, the indexed
character and the next cursor. If the cursor had exhausted the
character set, then this function returned false instead of the
character value, and another end-of-char-set cursor. In this way, the
other three functions of the current API were combined together.

## `(char-set-fold kons knil cs)`

This is the fundamental iterator for character sets. Applies the
function kons across the character set cs using initial state value
knil. That is, if cs is the empty set, the procedure returns
knil. Otherwise, some element c of cs is chosen; let cs' be the
remaining, unchosen characters. The procedure returns

```scheme
    (char-set-fold kons (kons c knil) cs')

    Examples:

    ;; CHAR-SET-MEMBERS
    (lambda (cs) (char-set-fold cons '() cs))

    ;; CHAR-SET-SIZE
    (lambda (cs) (char-set-fold (lambda (c i) (+ i 1)) 0 cs))

    ;; How many vowels in the char set?
    (lambda (cs)
      (char-set-fold (lambda (c i) (if (vowel? c) (+ i 1) i))
                     0 cs))
```

## `(char-set-unfold f p g seed [base-cs])`

## `(char-set-unfold! f p g seed base-cs)`

This is a fundamental constructor for char-sets.

- G is used to generate a series of "seed" values from the initial
  seed: seed, (g seed), (g2 seed), (g3 seed), ...

- P tells us when to stop -- when it returns true when applied to one
  of these seed values.

- F maps each seed value to a character. These characters are added to
  the base character set base-cs to form the result; base-cs defaults
  to the empty set. char-set-unfold! adds the characters to base-cs in
  a linear-update -- it is allowed, but not required, to side-effect
  and use base-cs's storage to construct the result.

More precisely, the following definitions hold, ignoring the optional-argument issues:

```scheme
    (define (char-set-unfold p f g seed base-cs)
      (char-set-unfold! p f g seed (char-set-copy base-cs)))

    (define (char-set-unfold! p f g seed base-cs)
      (let lp ((seed seed) (cs base-cs))
            (if (p seed) cs                                 ; P says we are done.
                (lp (g seed)                                ; Loop on (G SEED).
                    (char-set-adjoin! cs (f seed))))))      ; Add (F SEED) to set.

    (Note that the actual implementation may be more efficient.)
```

Examples:

```scheme
    (port->char-set p) = (char-set-unfold eof-object? values
                                          (lambda (x) (read-char p))
                                          (read-char p))

    (list->char-set lis) = (char-set-unfold null? car cdr lis)
```

## `(char-set-for-each proc cs)`

Apply procedure proc to each character in the character set cs. Note
that the order in which proc is applied to the characters in the set
is not specified, and may even change from one procedure application
to another.

Nothing at all is specified about the value returned by this
procedure; it is not even required to be consistent from call to
call. It is simply required to be a value (or values) that may be
passed to a command continuation, e.g. as the value of an expression
appearing as a non-terminal subform of a begin expression. Note that
in R5RS, this restricts the procedure to returning a single value;
non-R5RS systems may not even provide this restriction.  char-set-map
proc cs -> char-set proc is a char->char procedure. Apply it to all
the characters in the char-set cs, and collect the results into a new
character set.

Essentially lifts proc from a char->char procedure to a char-set -> char-set procedure.

Example:

```scheme
(char-set-map char-downcase cset)
```

## `(char-set-copy cs)`

Returns a copy of the character set cs. "Copy" means that if either
the input parameter or the result value of this procedure is passed to
one of the linear-update procedures described below, the other
character set is guaranteed not to be altered.

A system that provides pure-functional implementations of the
linear-operator suite could implement this procedure as the identity
function -- so copies are not guaranteed to be distinct by eq?.

## `(char-set char1 ...)`

Return a character set containing the given characters.

## `(list->char-set  char-list [base-cs])`

## `(list->char-set! char-list base-cs)`

Return a character set containing the characters in the list of
characters char-list.

If character set base-cs is provided, the characters from char-list
are added to it. list->char-set! is allowed, but not required, to
side-effect and reuse the storage in base-cs; list->char-set produces
a fresh character set.

## `(string->char-set  s [base-cs])`

## `(string->char-set! s base-cs)`

Return a character set containing the characters in the string s.

If character set base-cs is provided, the characters from s are added
to it. string->char-set! is allowed, but not required, to side-effect
and reuse the storage in base-cs; string->char-set produces a fresh
character set.

## `(char-set-filter  pred cs [base-cs])`

## `(char-set-filter! pred cs base-cs)`

Returns a character set containing every character c in cs such that
(pred c) returns true.

If character set base-cs is provided, the characters specified by pred
are added to it. char-set-filter! is allowed, but not required, to
side-effect and reuse the storage in base-cs; char-set-filter produces
a fresh character set.

An implementation may not save away a reference to pred and invoke it
after char-set-filter or char-set-filter! returns -- that is, "lazy,"
on-demand implementations are not allowed, as pred may have external
dependencies on mutable data or have other side-effects.

Rationale: This procedure provides a means of converting a character
predicate into its equivalent character set; the cs parameter allows
the programmer to bound the predicate's domain. Programmers should be
aware that filtering a character set such as char-set:full could be a
very expensive operation in an implementation that provided an
extremely large character type, such as 32-bit Unicode. An earlier
draft of this library provided a simple predicate->char-set procedure,
which was rejected in favor of char-set-filter for this reason.

## `(ucs-range->char-set  lower upper [error? base-cs])`

## `(ucs-range->char-set! lower upper error? base-cs)`

Lower and upper are exact non-negative integers; lower <= upper.

Returns a character set containing every character whose ISO/IEC 10646
UCS-4 code lies in the half-open range [lower,upper).

If the requested range includes unassigned UCS values, these are
silently ignored (the current UCS specification has "holes" in the
space of assigned codes).

If the requested range includes "private" or "user space" codes, these
are handled in an implementation-specific manner; however, a UCS- or
Unicode-based Scheme implementation should pass them through
transparently.

If any code from the requested range specifies a valid, assigned UCS
character that has no corresponding representative in the
implementation's character type, then (1) an error is raised if error?
is true, and (2) the code is ignored if error? is false (the
default). This might happen, for example, if the implementation uses
ASCII characters, and the requested range includes non-ASCII
characters.

If character set base-cs is provided, the characters specified by the
range are added to it. ucs-range->char-set! is allowed, but not
required, to side-effect and reuse the storage in base-cs;
ucs-range->char-set produces a fresh character set.

Note that ASCII codes are a subset of the Latin-1 codes, which are in
turn a subset of the 16-bit Unicode codes, which are themselves a
subset of the 32-bit UCS-4 codes. We commit to a specific encoding in
this routine, regardless of the underlying representation of
characters, so that client code using this library will be
portable. I.e., a conformant Scheme implementation may use EBCDIC or
SHIFT-JIS to encode characters; it must simply map the UCS characters
from the given range into the native representation when possible, and
report errors when not possible.

## `(->char-set x)`

Coerces x into a char-set. X may be a string, character or char-set. A
string is converted to the set of its constituent characters; a
character is converted to a singleton set; a char-set is returned
as-is. This procedure is intended for use by other procedures that
want to provide "user-friendly," wide-spectrum interfaces to their
clients.

## `(char-set-size cs)`

Returns the number of elements in character set cs.

## `(char-set-count pred cs)`

Apply pred to the chars of character set cs, and return the number of
chars that caused the predicate to return true.

## `(char-set->list cs)`

This procedure returns a list of the members of character set cs. The
order in which cs's characters appear in the list is not defined, and
may be different from one call to another.

## `(char-set->string cs)`

This procedure returns a string containing the members of character
set cs. The order in which cs's characters appear in the string is not
defined, and may be different from one call to another.

## `(char-set-contains? cs char)`

This procedure tests char for membership in character set cs.

The MIT Scheme character-set package called this procedure
char-set-member?, but the argument order isn't consistent with the
name.

## `(char-set-every pred cs)`

## `(char-set-any   pred cs)`

The char-set-every procedure returns true if predicate pred returns
true of every character in the character set cs. Likewise,
char-set-any applies pred to every character in character set cs, and
returns the first true value it finds. If no character produces a true
value, it returns false. The order in which these procedures sequence
through the elements of cs is not specified.

Note that if you need to determine the actual character on which a
predicate returns true, use char-set-any and arrange for the predicate
to return the character parameter as its true value, e.g.

```scheme
    (char-set-any (lambda (c) (and (char-upper-case? c) c))
                  cs)
```

## `(char-set-adjoin cs char1 ...)`

## `(char-set-delete cs char1 ...)`

Add/delete the chari characters to/from character set cs.

## `(char-set-adjoin! cs char1 ...)`

## `(char-set-delete! cs char1 ...)`

Linear-update variants. These procedures are allowed, but not
required, to side-effect their first parameter.

## `(char-set-complement cs)`

## `(char-set-union cs1 ...)`

## `(char-set-intersection cs1 ...)`

## `(char-set-difference cs1 cs2 ...)`

## `(char-set-xor cs1 ...)`

## `(char-set-diff+intersection cs1 cs2 ...)`

These procedures implement set complement, union, intersection,
difference, and exclusive-or for character sets. The union,
intersection and xor operations are n-ary. The difference function is
also n-ary, associates to the left (that is, it computes the
difference between its first argument and the union of all the other
arguments), and requires at least one argument.

Boundary cases:

```scheme
(char-set-union) => char-set:empty
(char-set-intersection) => char-set:full
(char-set-xor) => char-set:empty
(char-set-difference cs) => cs
```

char-set-diff+intersection returns both the difference and the
intersection of the arguments -- it partitions its first parameter. It
is equivalent to

```scheme
(values (char-set-difference cs1 cs2 ...)
        (char-set-intersection cs1 (char-set-union cs2 ...)))
```

but can be implemented more efficiently.

Programmers should be aware that char-set-complement could potentially
be a very expensive operation in Scheme implementations that provide a
very large character type, such as 32-bit Unicode. If this is a
possibility, sets can be complimented with respect to a smaller
universe using char-set-difference.

## `(char-set-complement! cs)`

## `(char-set-union! cs1 cs2 ...)`

## `(char-set-intersection! cs1 cs2 ...)`

## `(char-set-difference! cs1 cs2 ...)`

## `(char-set-xor! cs1 cs2 ...)`

## `(char-set-diff+intersection! cs1 cs2 cs3 ...)`

These are linear-update variants of the set-algebra functions. They
are allowed, but not required, to side-effect their first (required)
parameter.

char-set-diff+intersection! is allowed to side-effect both of its two
required parameters, cs1 and cs2.

## `char-set:lower-case`

Lower-case letters

## `char-set:upper-case`

Upper-case letters

## `char-set:title-case`

Title-case letters


## `char-set:letter`

Letters

## `char-set:digit`

Digits

##  `char-set:letter+digit`

Letters and digits

## `char-set:graphic`

Printing characters except spaces

## `char-set:printing`

Printing characters including spaces

## `char-set:whitespace`

Whitespace characters

## `char-set:iso-control`

The ISO control characters

## `char-set:punctuation`

Punctuation characters

## `char-set:symbol`

Symbol characters

## `char-set:hex-digit`

A hexadecimal digit: 0-9, A-F, a-f

## `char-set:blank`

Blank characters -- horizontal whitespace

## `char-set:ascii`

All characters in the ASCII set.

## `char-set:empty`

Empty set

## `char-set:full`

All characters
