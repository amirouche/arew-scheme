
## `(arew srfi srfi-4)`

This is based on [SRFI-4](https://srfi.schemers.org/srfi-4/).

### Abstract

This SRFI describes a set of datatypes for vectors whose elements are
of the same numeric type (signed or unsigned exact integer or inexact
real of a given precision). These datatypes support operations
analogous to the Scheme vector type, but they are distinct
datatypes.

### Reference

#### Signed 8 bits integer

##### s8vector?
##### make-s8vector
##### s8vector
##### s8vector-length
##### s8vector-ref
##### s8vector-set!
##### s8vector->list
##### list->s8vector

#### Signed 16 bits integer

##### s16vector?
##### make-s16vector
##### s16vector
##### s16vector-length
##### s16vector-ref
##### s16vector-set!
##### s16vector->list
##### list->s16vector

#### Signed 32 bits integer

##### s32vector?
##### make-s32vector
##### s32vector
##### s32vector-length
##### s32vector-ref
##### s32vector-set!
##### s32vector->list
##### list->s32vector

#### Signed 64 bits integer

##### s64vector?
##### make-s64vector
##### s64vector
##### s64vector-length
##### s64vector-ref
##### s64vector-set!
##### s64vector->list
##### list->s64vector

#### Unsigned 8 bits integer

##### u8vector?
##### make-u8vector
##### u8vector
##### u8vector-length
##### u8vector-ref
##### u8vector-set!
##### u8vector->list
##### list->u8vector

#### Unsigned 16 bits integer

##### u16vector?
##### make-u16vector
##### u16vector
##### u16vector-length
##### u16vector-ref
##### u16vector-set!
##### u16vector->list
##### list->u16vector

#### Unsigned 32 bits integer

##### u32vector?
##### make-u32vector
##### u32vector
##### u32vector-length
##### u32vector-ref
##### u32vector-set!
##### u32vector->list
##### list->u32vector

#### Unsigned 64 bits integer

##### u64vector?
##### make-u64vector
##### u64vector
##### u64vector-length
##### u64vector-ref
##### u64vector-set!
##### u64vector->list
##### list->u64vector

#### 32 bits float

##### f32vector?
##### make-f32vector
##### f32vector
##### f32vector-length
##### f32vector-ref
##### f32vector-set!
##### f32vector->list
##### list->f32vector

#### 64 bits float

##### f64vector?
##### make-f64vector
##### f64vector
##### f64vector-length
##### f64vector-ref
##### f64vector-set!
##### f64vector->list
##### list->f64vector
