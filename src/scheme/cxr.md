
## `(arew scheme cxr)`

Exports the following procedure which are the compositions of from
three to four `car` and `cdr` operations. For example `caddar` could
be defined:

```scheme
(define caddar
  (lambda (x) (car (cdr (cdr (car x))))))
```

Here is the full list:

- `caaaar`
- `caaadr`
- `caaar`
- `caadar`
- `caaddr`
- `caadr`
- `cadaar`
- `cadadr`
- `cadar`
- `caddar`
- `cadddr`
- `caddr`
- `cdaaar`
- `cdaadr`
- `cdaar`
- `cdadar`
- `cdaddr`
- `cdadr`
- `cddaar`
- `cddadr`
- `cddar`
- `cdddar`
- `cddddr`
- `cdddr`
