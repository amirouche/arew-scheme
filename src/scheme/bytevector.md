## `(arew scheme bytevector)`

This is based on [R6RS
bytevectors](http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-3.html#node_chap_2)

### Abstract

TODO

### Reference

#### General operations

##### `(endianness <endianess symbol>)` syntax

##### `(native-endianness)`

Returns the endianness symbol associated implementation's preferred
endianness (usually that of the underlying machine architecture). This
may be any <endianness symbol>, including a symbol other than big and
little.

##### `(bytevector? obj)`

Returns #t if obj is a bytevector, otherwise returns #f.

##### `(make-bytevector k [fill])`

Returns a newly allocated bytevector of `K` bytes.

If the `FILL` argument is missing, the initial contents of the
returned bytevector are unspecified.

If the `FILL` argument is present, it must be an exact integer object
in the interval {-128, ... 255} that specifies the initial value for
the bytes of the bytevector: If `FILL` is positive, it is interpreted
as an octet; if it is negative, it is interpreted as a byte.

##### `(bytevector-length bytevector)`

Returns, as an exact integer object, the number of bytes in
bytevector.

##### `(bytevector=? bytevector1 bytevector2)`

Returns #t if bytevector1 and bytevector2 are equal-that is, if they
have the same length and equal bytes at all valid indices. It returns
#f otherwise.

##### `(bytevector-fill! bytevector fill)`

The fill argument is as in the description of the make-bytevector
procedure. The bytevector-fill!  procedure stores fill in every
element of bytevector and returns unspecified values. Analogous to
vector-fill!.

##### `(bytevector-copy! source source-start‌‌ target target-start k)`

##### `(bytevector-copy bytevector)‌‌`

Returns a newly allocated copy of bytevector.

#### Operations on bytes and octets

##### `(bytevector-u8-ref bytevector k)‌‌`

The bytevector-u8-ref procedure returns the byte at index k of
bytevector, as an octet.

##### `(bytevector-s8-ref bytevector k)‌‌`

The bytevector-s8-ref procedure returns the byte at index k of
bytevector, as a (signed) byte.

##### `(bytevector-u8-set! bytevector k octet)‌‌`

The bytevector-u8-set! procedure stores octet in element k of
bytevector.

##### `(bytevector-s8-set! bytevector k byte)‌‌`

The bytevector-s8-set! procedure stores the two's-complement
representation of byte in element k of bytevector.

##### `(bytevector->u8-list bytevector)‌‌`

The bytevector->u8-list procedure returns a newly allocated list of
the octets of bytevector in the same order.

##### `(u8-list->bytevector list)‌‌`

The u8-list->bytevector procedure returns a newly allocated bytevector
whose elements are the elements of list list, in the same order. It is
analogous to list->vector.

#### Operations on integers of arbitrary size

##### `(bytevector-uint-ref bytevector k endianness size)‌‌`

##### `(bytevector-sint-ref bytevector k endianness size)‌‌`

##### `(bytevector-uint-set! bytevector k n endianness size)‌‌`

##### `(bytevector-sint-set! bytevector k n endianness size)‌‌`

##### `(bytevector->uint-list bytevector endianness size)‌‌`

##### `(bytevector->sint-list bytevector endianness sizee‌‌`

##### `(uint-list->bytevector list endianness size)‌‌`

##### `(sint-list->bytevector list endianness size)‌‌`

#### Operations on 16-bit integers

##### `(bytevector-u16-ref bytevector k endianness)‌‌`

##### `(bytevector-s16-ref bytevector k endianness)‌‌`

##### `(bytevector-u16-native-ref bytevector k)‌‌`

##### `(bytevector-s16-native-ref bytevector k)‌‌`

##### `(bytevector-u16-set! bytevector k n endianness)‌‌`

##### `(bytevector-s16-set! bytevector k n endianness)‌‌`

##### `(bytevector-u16-native-set! bytevector k n)‌‌`

##### `(bytevector-s16-native-set! bytevector k n)‌‌`

#### Operations on 32-bit integers

##### `(bytevector-u32-ref bytevector k endianness)‌‌`

##### `(bytevector-s32-ref bytevector k endianness)‌‌`

##### `(bytevector-u32-native-ref bytevector k)‌‌`

##### `(bytevector-s32-native-ref bytevector k)‌‌`

##### `(bytevector-u32-set! bytevector k n endianness)‌‌`

##### `(bytevector-s32-set! bytevector k n endianness)‌‌`

##### `(bytevector-u32-native-set! bytevector k n)‌‌`

##### `(bytevector-s32-native-set! bytevector k n)‌‌`

#### Operations on 64-bit integers

##### `(bytevector-u64-ref bytevector k endianness)‌‌`

##### `(bytevector-s64-ref bytevector k endianness)‌‌`

##### `(bytevector-u64-native-ref bytevector k)‌‌`

##### `(bytevector-s64-native-ref bytevector k)‌‌`

##### `(bytevector-u64-set! bytevector k n endianness)‌‌`

##### `(bytevector-s64-set! bytevector k n endianness)‌‌`

##### `(bytevector-u64-native-set! bytevector k n)‌‌`

##### `(bytevector-s64-native-set! bytevector k n)‌‌`

#### Operations on IEEE-754 representations

##### `(bytevector-ieee-single-native-ref bytevector k)‌‌`

##### `(bytevector-ieee-single-ref bytevector k endianness)‌‌`

##### `(bytevector-ieee-double-native-ref bytevector k)‌‌`

##### `(bytevector-ieee-double-ref bytevector k endianness)‌‌`

##### `(bytevector-ieee-single-native-set! bytevector k x)‌‌`

##### `(bytevector-ieee-single-set! bytevector ‌k x endianness)`

##### `(bytevector-ieee-double-native-set! bytevector k x)‌‌`

##### `(bytevector-ieee-double-set! bytevector k x endianness)‌`

#### Operations on strings

##### `(string->utf8 string)‌‌`

##### `(string->utf16 string)‌‌`

##### `(string->utf16 string endianness)‌‌`

##### `(string->utf32 string)‌‌`

##### `(string->utf32 string endianness)‌‌`

##### `(utf8->string bytevector)‌‌`

##### `(utf16->string bytevector endianness)‌‌`

##### `(utf16->string bytevector‌ endianness endianness-mandatory)`

##### `(utf32->string bytevector endianness)‌‌`

##### `(utf32->string bytevector‌ endianness endianness-mandatory)`
