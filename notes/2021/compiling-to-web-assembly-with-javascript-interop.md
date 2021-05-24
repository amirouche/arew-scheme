# 2021-05-23 - Compiling to Web Assembly with JavaScript interop

I will try to describe a compilation strategy inspired from [0][1][2]
that is adapted to work with Web Assembly (wasm). Unlike Schism [3],
and Cyclone [4], it is possible to interop a wasm module that is
restricted to a single exported function, and JavaScript to handle
HTML DOM (that includes the canvas) and XHR events (io events)
**without polling**.

[0] [An Incremental Approach to Compiler Construction, A. Ghuloum](http://schemeworkshop.org/2006/11-ghuloum.pdf)

[1] [Nanopass framework, A. Keep et al.](https://nanopass.org/)

[2] [scheme-to-c, A. Keep](https://github.com/akeep/scheme-to-c)

[3] [google/schism, E. Holk et al.](https://github.com/google/schism/)

[4] [Cyclone Scheme, J. Ethier](https://github.com/justinethier/cyclone/)

With the following compilation strategy it is possible to implement a
function similar to POSIX `select` [5] and `epoll` [6] that is more
efficient and more reactive than polling.

[5] [select(2)](https://manpages.debian.org/buster/manpages-dev/select.2.en.html)

[6] [epoll(7)](https://manpages.debian.org/stretch/manpages/epoll.7.en.html)

My approach is not an alternative to emscripten asyncify [7][8].

[7] [Asyncify](https://emscripten.org/docs/porting/asyncify.html)

[8] [Using asynchronous web APIs from WebAssembly](https://web.dev/asyncify/)

The strategy that will described may be useful beyond Scheme, and LISP.

## What is `select`, `epoll` or `event-wait`?

`select`, `epoll` and even `io_uring` are Linux machinery that will
pause the application with a timeout, until an event occurs, which
ever comes first. Those are the hearth of event loops such as libuv.
they are mostly used in networked application, including web browsers.
That machinery will *pause* the application, in the sense it will make
a minimal use of cpu and memory until something happens.

To ease the explanation, I will forgo `select` and `epoll`, instead I
will introduce `(event-wait events)` procedure. So, `event-wait` is a
procedure (or if you prefer a function) that will pause the current
application, until one or more events from `EVENTS` are complete. In
the context of POSIX, other threads or processus may proceed as usual,
they will even have more room to do their work. In the context of a
wasm module inside a web browser, it means that wasm will yield
control back to JavaScript.  An event in `EVENTS` may be a timeout, a
network or disk operation, or like in the case of the web browser and
more generally any interactive user interface, the application might
wait for user input to proceed.

At this stage, most people targeting wasm in the web browser will call
the equivalent of `event-wait` once per frame. That strategy makes
sense for games where JavaScript only needs to forward canvas click or
move events at a predictable frame rate. What if, the application is
mostly sleepy, such as the case of single page applications. In that
case JavaScript can keep the control and call wasm only when needed.
There is fatal flow with the latter approach, wasm code may need to do
a network call, in that case the wasm function will not return the
requested result such as the description of the new dom, but one or
more HTTP requets that must be done by JavaScript.

Web Assembly needs a way to pause and JavaScript needs a way to resume.

My informal proposal was to add `pause` to web assembly standard [9],
but it does not work in the general case [10].

[9] [Ability to pause wasm execution](https://github.com/WebAssembly/design/issues/1294)

[10] [Web Assembly stack subgroup meetings](https://github.com/WebAssembly/meetings/tree/main/stack)

What works is restricted wasm module with a single entry-point, that
start the application the first time, wasm can pause itself, then
JavaScript can resume wasm by calling the single entry point.

Given a wasm `pause`, and a JavaScript `resume` it is possible to
implement `event-wait`, `epoll` or `select`.

## Know your target!

In this note, I will not dive much into wasm language. Suffice to say,
you really need to start with learning web assembly and even write
web assembly yourself before trying to create a compiler.

The gist of the compilation strategy rely on a global
Continuation-Passing-Style transformation [11] and a trampoline [12].

[11] https://en.wikipedia.org/wiki/Continuation-passing_style

[12] https://en.wikipedia.org/wiki/Trampoline_(computing)

A consequence of those two requirements, tail calls do not grow the
stack, and call/cc is trivial to implement hence exceptions,
generators, coroutines, amb... [13]

[13] https://en.wikipedia.org/wiki/Call-with-current-continuation

In terms of wasm, it is possible to translate a standard wasm module
with the addition of the wasm function `(pause/continuation (func.ref
$myfunc))`, given function references, and either mutable globals or
multiple value returns. The generated wasm code can run with nodejs 14
and the v8 flags ` --experimental-wasm-anyref --experimental-wasm-mv`.
And, it can run with spidermonkey's jsshell nightly without flags.

I will describe further down how to translate with Scheme
source-to-source transformation in the spirit of nanopass compiler.

Here is the pseudo-code of the only wasm function that is exported
by the web assembly module aka. single entry-point:

```python
resume = False

def trampoline():
    global resume
    continuation = resume if resume else main
    while True:
        pause, continuation = continuation()
        if pause:
            resume = continuation
            return
```

Here is the same function using Scheme:

```scheme
(define resume #f)

(define (trampoline)
  (let loop ((continuation (if resume resume main)))
    (call-with-values continuation
      (lambda (pause? continuation)
        (if pause?
            (set! resume continuation)
            (loop continuation))))))
```

Given the following frontend Scheme code:


```scheme
(define (frob a b c)
  (values (+ a b) c))
```

It is translated to a subset of Scheme that is eventually translated
to wasm, that looks like the following:

```scheme
(define (frob cl k)
  (define a (stack-ref 0))
  (define b (stack-ref 1))
  (define c (stack-ref 2))
  (define out (+ a b))
  (stack-set! 0 out)
  (stack-set! 1 c)
  (values #f k))
```

Where `cl` is the closure of `frob`, in that case it is empty. And `k`
is the continuation of the caller. Arguments and return values are
passed with a stack that is in web assembly a table of `externref`.
The boolean `#f` in `values` correspond to `pause?` in `trampoline`.

To pause Scheme, there is a form `(pause/continuation proc)` that is
translated into `(values #t proc-closure)`.

In the following sections I will describe all the steps that are
sufficent to compile the following code that computes fibonacci in a
loop:

```scheme
(define (fibonacci a b n)
  (if (fxzero? n)
      a
      (fibonacci b (fx+ a b) (fx- n 1))))

(define (generate-positive-integers-from n)
  ;; it will generate all integers from `n` to `0` both are excluded.
  (lambda ()
    (if (fxzero? n)
        #f
        (begin
          (set! n (fx- n 1))
          n))))

(define (run start generator n)
  (let* ((start (time))
         (iteration (generator)))
    (if iteration
        (let* ((start (time))
               (out (fibonacci 0 1 n))
               (delta (fx- (time) start)))
          (print out delta)
          (run start generator n))
        (print "done" (fx- (time) start)))))

(define (main)
  (define n 80)
  (define m 1000)
  (run (time) (generate-positive-integers-from m) n))
```

And the same program that rely on `call/cc` to implement
`generate-positive-integers-from`. Those benchmark programs will not
use `pause/continuation` because I have no clue what a good benchmark
for `pause/continuation` looks like.

## Nanostep framework

nanostep framework is inspired from nanopass framework. The former
should be easier to port to any Scheme that has something like Chibi
Scheme pattern matching macro called `match`.

A step is constructed as follow:

```scheme
(define my-step (make-step! "my-step"
                            my-parent-step
                            reader-for-my-step
                            my-step-procedure
                            my-step-evaler))
```

Most of the arguments are explicit:

- `my-parent-step` is the step that comes before;
- `reader-for-my-step` is a procedure that will take a filename as
  input and produce a Scheme expression (s-exp), and most likely but not
  necessarly rely on Scheme `read`;
- `my-step-procedure` is a source-to-source translator powered by
  match. It takes as argument a Scheme expression and produce another
  Scheme expression;
- `my-step-evaler` is most of the time a thin wrapper around Scheme
  `eval`;

I will only describe the procedures that are `my-step-procedure` ie.
the source-to-source transformer.

There is also a helper procedure:

```scheme
(define (nanosteps step-source-name step-target-name filename)
  ...)
```

That will read and evaluate the content of `filename` after going
through all the steps from `step-source-name` until
`step-target-name`. It will use the reader of `step-source-name` to
read, and the evaler of `step-target-name` to evaluate.

Mind the fact, that I wrote source-to-source because Scheme provides
the necessary forms that allows to keep the program semantic, reduce
complexity without inventing new forms (if you prefer: keywords). In
other words, intermediate representations can be easily compiled or
interpreted with any Scheme. Along the compilation process, `(primcall
proc obj ...)` is used to describe procedures `proc` that should be
present in the evaluation environment. `primcall` can be defined in
Scheme with the following code:

```scheme
(define primcall apply)
```

So if `(primcall add a b)` appears in the intermediate source, the
following code must be added as prelude inside the associated evaler:

```scheme
(define (add a b)
  (+ a b))
```

Similarly, the last step, called `wasmic`, produce the necessary wasm
imports.

## Wrap with `lambda` and print the last expression

It is important to note that a step reader must wrap the program
with a `begin`. So that given the following source file:

```scheme
(define (frob a b) (+ a b 40))

(frob 1 1)
```

Will be read as:

```scheme
(lambda ()
  (define (frob a b) (+ a b 40))

  (frob 1 1))
```

Eventually, `nanosteps` will print the result of the last expression
`(frob 1 1)`, that is:

```scheme
42
```

The program is wrapped with a `lambda` otherwise what `read` produce
is a list of expression such as `(e0 en ...)` which means that `e0`
takes as argument `en` that does not make a Scheme sense! That is a
required treatment to top-level, so that steps do not need to special
case top level: top level is a lambda.

## make `letrec*` explicit

Inside `lambda`, `let` and other similar bindings forms, the body may
start with a set of `define`. `make-letrec*-explicit` will grab all
`define` and create a `letrec*`.

##
