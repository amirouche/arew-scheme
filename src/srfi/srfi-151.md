
## `(arew srfi srfi-151)`

This library is based on [SRFI-151](https://srfi.schemers.org/srfi-151/).

### Abstract

This library offers a coherent and comprehensive set of procedures for
performing bitwise logical operations on integers.

### Reference

#### `(bitwise-not i)`

Returns the bitwise complement of i; that is, all 1 bits are changed
to 0 bits and all 0 bits to 1 bits.

```scheme
(bitwise-not 10) ;; => -11
(bitwise-not -37) ;; => 36
```

The following ten procedures correspond to the useful set of
non-trivial two-argument boolean functions. For each such function,
the corresponding bitwise operator maps that function across a pair of
bitstrings in a bit-wise fashion. The core idea of this group of
functions is this bitwise "lifting" of the set of dyadic boolean
functions to bitstring parameters.

#### `(bitwise-and i ...)`
#### `(bitwise-ior i ...)`
#### `(bitwise-xor i ...)`
#### `(bitwise-eqv i ...)`

These operations are associative. When passed no arguments, the
procedures return the identity values -1, 0, 0, and -1 respectively.

The bitwise-eqv procedure produces the complement of the bitwise-xor
procedure. When applied to three arguments, it does not produce a 1
bit everywhere that a, b and c all agree. That is, it does not produce

```scheme
     (bitwise-ior (bitwise-and a b c)
                  (bitwise-and (bitwise-not a)
                               (bitwise-not b)
                               (bitwise-not c)))
```

Rather, it produces (bitwise-eqv a (bitwise-eqv b c)) or the
equivalent (bitwise-eqv (bitwise-eqv a b) c).

```scheme
(bitwise-ior 3  10)     =>  11
(bitwise-and 11 26)     =>  10
(bitwise-xor 3 10)      =>   9
(bitwise-eqv 37 12)     => -42
(bitwise-and 37 12)     =>   4
```

#### `(bitwise-nand i j)`
#### `(bitwise-nor i j)`
#### `(bitwise-andc1 i j)`
#### `(bitwise-andc2 i j)`
#### `(bitwise-orc1 i j)`
#### `(bitwise-orc2 i j)`

These operations are not associative.

```scheme
(bitwise-nand 11 26) =>  -11
(bitwise-nor  11 26) => -28
(bitwise-andc1 11 26) => 16
(bitwise-andc2 11 26) => 1
(bitwise-orc1 11 26) => -2
(bitwise-orc2 11 26) => -17
```

#### `(arithmetic-shift i count)`

Returns the arithmetic left shift when count>0; right shift when
count<0.

```scheme
(arithmetic-shift 8 2) => 32
(arithmetic-shift 4 0) => 4
(arithmetic-shift 8 -1) => 4
(arithmetic-shift -100000000000000000000000000000000 -100) => -79
```

#### `(bit-count i)`

Returns the population count of 1's (i >= 0) or 0's (i < 0). The
result is always non-negative.

Compatibility note: The R6RS analogue bitwise-bit-count applies
bitwise-not to the population count before returning it if i is
negative.

```scheme
(bit-count 0) =>  0
(bit-count -1) =>  0
(bit-count 7) =>  3
(bit-count  13) =>  3 ;Two's-complement binary: ...0001101
(bit-count -13) =>  2 ;Two's-complement binary: ...1110011
(bit-count  30) =>  4 ;Two's-complement binary: ...0011110
(bit-count -30) =>  4 ;Two's-complement binary: ...1100010
(bit-count (expt 2 100)) =>  1
(bit-count (- (expt 2 100))) =>  100
(bit-count (- (1+ (expt 2 100)))) =>  1
```

#### `(integer-length i)`

The number of bits needed to represent i, i.e.

```scheme
(ceiling (/ (log (if (negative? integer)
                     (- integer)
			         (+ 1 integer)))
		    (log 2)))
```

The result is always non-negative. For non-negative i, this is the
number of bits needed to represent i in an unsigned binary
representation. For all i, (+ 1 (integer-length i)) is the number of
bits needed to represent i in a signed twos-complement representation.

```scheme
(integer-length  0) => 0
(integer-length  1) => 1
(integer-length -1) => 0
(integer-length  7) => 3
(integer-length -7) => 3
(integer-length  8) => 4
(integer-length -8) => 3
```

#### `(bitwise-if mask i j)`

Merge the bitstrings i and j, with bitstring mask determining from
which string to take each bit. That is, if the kth bit of mask is 1,
then the kth bit of the result is the kth bit of i, otherwise the kth
bit of j.

```scheme
(bitwise-if 3 1 8) => 9
(bitwise-if 3 8 1) => 0
(bitwise-if 1 1 2) => 3
(bitwise-if #b00111100 #b11110000 #b00001111) => #b00110011
```

#### `(bit-set? index i)`

Is bit index set in bitstring i (where index is a non-negative exact
integer)?

Compatibility note: The R6RS analogue bitwise-bit-set? accepts its
arguments in the opposite order.

```scheme
(bit-set? 1 1) =>  false
(bit-set? 0 1) =>  true
(bit-set? 3 10) =>  true
(bit-set? 1000000 -1) =>  true
(bit-set? 2 6) =>  true
(bit-set? 0 6) =>  false
```

#### `(copy-bit index i boolean)`

Returns an integer the same as i except in the indexth bit, which is 1
if boolean is #t and 0 if boolean is #f.

Compatibility note: The R6RS analogue bitwise-copy-bit as originally
documented has a completely different interface. (bitwise-copy-bit
dest index source) replaces the index'th bit of dest with the index'th
bit of source. It is equivalent to (bit-field-replace-same dest source
index (+ index 1)). However, an erratum made a silent breaking change
to interpret the third argument as 0 for a false bit and 1 for a true
bit. Some R6RS implementations applied this erratum but others did
not.

```scheme
(copy-bit 0 0 #t) => #b1
(copy-bit 2 0 #t) => #b100
(copy-bit 2 #b1111 #f) => #b1011
```

#### `(bit-swap index1 index2 i)`

Returns an integer the same as i except that the index1th bit and the
index2th bit have been exchanged.

```scheme
(bit-swap 0 2 4) => #b1
```

#### `(any-bit-set? test-bits i)`
#### `(every-bit-set? test-bits i)`

Determines if any/all of the bits set in bitstring test-bits are set
in bitstring i. I.e., returns (not (zero? (bitwise-and test-bits i)))
and (= test-bits (bitwise-and test-bits i))) respectively.

```scheme
(any-bit-set? 3 6) => #t
(any-bit-set? 3 12) => #f
(every-bit-set? 4 6) => #t
(every-bit-set? 7 6) => #f
```

#### `(first-set-bit i)`

Return the index of the first (smallest index) 1 bit in bitstring
i. Return -1 if i contains no 1 bits (i.e., if i is zero).

```scheme
(first-set-bit 1) => 0
(first-set-bit 2) => 1
(first-set-bit 0) => -1
(first-set-bit 40) => 3
(first-set-bit -28) => 2
(first-set-bit (expt  2 99)) => 99
(first-set-bit (expt -2 99)) => 99
```

#### `(bit-field i start end)

Returns the field from i, shifted down to the least-significant
position in the result.

```scheme
(bit-field #b1101101010 0 4) => #b1010
(bit-field #b1101101010 3 9) => #b101101
(bit-field #b1101101010 4 9) => #b10110
(bit-field #b1101101010 4 10) => #b110110
(bit-field 6 0 1) => 0
(bit-field 6 1 3) => 3
(bit-field 6 2 999) => 1
(bit-field #x100000000000000000000000000000000 128 129) => 1
```

#### `(bit-field-any? i start end)`

Returns true if any of the field's bits are set in bitstring i, and false otherwise.

```scheme
(bit-field-any? #b1001001 1 6) => #t
(bit-field-any? #b1000001 1 6) => #f
```

#### `(bit-field-every? i start end)`

Returns false if any of the field's bits are not set in bitstring i, and true otherwise.

```scheme
(bit-field-every? #b1011110 1 5) => #t
(bit-field-every? #b1011010 1 5) => #f
```

#### `(bit-field-clear i start end)`
#### `(bit-field-set i start end)`

Returns i with the field's bits set to all 0s/1s.

```scheme
(bit-field-clear #b101010 1 4) => #b100000
(bit-field-set #b101010 1 4) => #b101110
```

#### `(bit-field-replace dest source start end)`

Returns dest with the field replaced by the least-significant
end-start bits in source.

```scheme
(bit-field-replace #b101010 #b010 1 4) => #b100100
(bit-field-replace #b110 1 0 1) => #b111
(bit-field-replace #b110 1 1 2) => #b110
```

#### `(bit-field-replace-same dest source start end)`

Returns dest with its field replaced by the corresponding field in source.

```scheme
(bit-field-replace-same #b1111 #b0000 1 3) => #b1001
```

#### `(bit-field-rotate i count start end)`

Returns i with the field cyclically permuted by count bits towards
high-order.

Compatibility note: The R6RS analogue bitwise-rotate-bit-field uses
the argument ordering i start end count.

```scheme
(bit-field-rotate #b110 0 0 10) => #b110
(bit-field-rotate #b110 0 0 256) => #b110
(bit-field-rotate #x100000000000000000000000000000000 1 0 129) => 1
(bit-field-rotate #b110 1 1 2) => #b110
(bit-field-rotate #b110 1 2 4) => #b1010
(bit-field-rotate #b0111 -1 1 4) => #b1011
```

#### `(bit-field-reverse i start end)`

Returns i with the order of the bits in the field reversed.

```scheme
(bit-field-reverse 6 1 3) => 6
(bit-field-reverse 6 1 4) => 12
(bit-field-reverse 1 0 32) => #x80000000
(bit-field-reverse 1 0 31) => #x40000000
(bit-field-reverse 1 0 30) => #x20000000
(bit-field-reverse #x140000000000000000000000000000000 0 129) => 5
```


#### `(bits->list i [ len ])`
#### `(bits->vector i [ len ])`

Returns a list/vector of len booleans corresponding to each bit of the
non-negative integer i, returning bit #0 as the first element, bit #1
as the second, and so on. #t is returned for each 1; #f for 0.

```scheme
(bits->list #b1110101)) => (#t #f #t #f #t #t #t)
(bits->list 3 5)) => (#t #t #f #f #f)
(bits->list 6 4)) => (#f #t #t #f)

(bits->vector #b1110101)) => #(#t #f #t #f #t #t #t)
```

#### `(list->bits list)`
#### `(vector->bits vector)`

Returns an integer formed from the booleans in list/vector, using the
first element as bit #0, the second element as bit #1, and so on. It
is an error if list/vector contains non-booleans. A 1 bit is coded for
each #t; a 0 bit for #f. Note that the result is never a negative
integer.

```scheme
(list->bits '(#t #f #t #f #t #t #t)) => #b1110101
(list->bits '(#f #f #t #f #t #f #t #t #t)) => #b111010100
(list->bits '(#f #t #t)) => 6
(list->bits '(#f #t #t #f)) => 6
(list->bits '(#f #f #t #t)) => 12

(vector->bits '#(#t #f #t #f #t #t #t)) => #b1110101
(vector->bits '#(#f #f #t #f #t #f #t #t #t)) => #b111010100
(vector->bits '#(#f #t #t)) => 6
(vector->bits '#(#f #t #t #f)) => 6
(vector->bits '#(#f #f #t #t)) => 12
```

For positive integers, bits->list and list->bits are inverses in the
sense of equal?, and so are bits->vector and vector->bits.

#### `(bits bool ...)`

Returns the integer coded by the bool arguments. The first argument is
bit #0, the second argument is bit #1, and so on. Note that the result
is never a negative integer.

```scheme
(bits #t #f #t #f #t #t #t) => #b1110101
(bits #f #f #t #f #t #f #t #t #t) => #b111010100
```

#### `(bitwise-fold proc seed i)`

For each bit b of i from bit #0 (inclusive) to bit (integer-length i)
(exclusive), proc is called as (proc b r), where r is the current
accumulated result. The initial value of r is seed, and the value
returned by proc becomes the next accumulated result. When the last
bit has been processed, the final accumulated result becomes the
result of bitwise-fold.

```scheme
(bitwise-fold cons '() #b1010111) => (#t #f #t #f #t #t #t)
```

#### `(bitwise-for-each proc i)`

Repeatedly applies proc to the bits of i starting with bit #0
(inclusive) and ending with bit (integer-length i) (exclusive). The
values returned by proc are discarded. Returns an unspecified value.

```scheme
      (let ((count 0))
        (bitwise-for-each (lambda (b) (if b (set! count (+ count 1))))
                          #b1010111)
       count)
```

#### `(bitwise-unfold stop? mapper successor seed)`

Generates a non-negative integer bit by bit, starting with bit 0. If
the result of applying stop? to the current state (whose initial value
is seed) is true, return the currently accumulated bits as an
integer. Otherwise, apply mapper to the current state to obtain the
next bit of the result by interpreting a true value as a 1 bit and a
false value as a 0 bit. Then get a new state by applying successor to
the current state, and repeat this algorithm.

```scheme
  (bitwise-unfold (lambda (i) (= i 10))
                  even?
                  (lambda (i) (+ i 1))
                  0)) => #b101010101
```

#### `(make-bitwise-generator i)`

Returns a SRFI 121 generator that generates all the bits of i starting
with bit #0. Note that the generator is infinite.

```scheme
(let ((g (make-bitwise-generator #b110)))
  (test #f (g))
  (test #t (g))
  (test #t (g))
  (test #f (g)))
```
