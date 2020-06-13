
## `(arew data lux)`

### Abstract

lux is markup language similar in principle to html inspired from
[skribe](https://www-sop.inria.fr/members/Manuel.Serrano/publi/jfp05/article.html).
The syntax is scheme expressions (s-expr) with the addition of square
brackets as a way to delimit strings that can be escaped.

### Rational

There is no markup language for scheme, even if they were several
experiments. Chief among them is racket's scribble which syntax looks
alien.

### `(lux-read [generator-or-port])

Read lux markup from `GENERATOR-OR-PORT`, by default
`(current-input-port)`.

The lux markup looks like a regular s-expr where square brackets
delimit strings that can be escapd. For example:

```scheme
(call-with-input-string "(p [hello world])" lux-read)
```

Will return:

```scheme
(p "hello world")
```

Whereas when the square brackets are escaped somewhat like
`unquote-splice`, what is escaped follow the previous string. For
example:

```scheme
(call-with-input-string "(p [hello ,(bold [world])])" lux-read)
```

Will return

```scheme
(p "hello " (bold "world"))
```

Mind the space that follows `hello`.

Similarly:

```scheme
(call-with-input-string "(p [bonjour ,(bold [le monde]) !])" lux-read)
```

Will return:

```scheme
(p "hello " (bold "le monde") " !")
```

Mind the space after `hello` and before the exclamation mark.
