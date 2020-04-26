
## `(arew srfi srfi-173)`

This is based on [SRFI-173](https://srfi.schemers.org/srfi-173/).

### Abstract

This library describes a mechanism known as hooks. Hooks are a certain
kind of extension point in a program that allows interleaving the
execution of arbitrary code with the execution of the program without
introducing any coupling between the two.

### Reference

### `(make-hook arity)`

Create a hook object for storing procedures of ARITY. The return value
is a hook object.

### `(hook? obj)`

Return #t if obj is a hook. Otherwise, it returns #f.

### `(list->hook arity lst)`

Create a hook with the given procedures LST that must have an arity
equal to ARITY. The return value is a hook object.

### `(list->hook! hook lst)`

Replace procedures in HOOK by the procedures in LST. The return value is unspecified

### `(hook-add! hook proc)`

Add the procedure PROC to the HOOK object. The return value is not
specified. An implementation may check that the arity of PROC is equal
to the arity of the HOOK.

### `(hook-delete! hook proc)`

Delete the procedure PROC from the HOOK object. The return value is
not specified.

### `(hook-reset! hook)`

Remove all procedures from the HOOK object. The return value is not
specified.

### `(hook->list hook)`

Convert the list of procedures of HOOK object to a list.

### `(hook-run hook . args)`

Apply all procedures from HOOK to the arguments ARGS. The order of the
procedure application is not specified. The return value is not
specified. The length of ARGS must be equal to the arity of the HOOK
object.
