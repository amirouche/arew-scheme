# Babelia Code Manual

## Introduction

Babelia is a privacy friendly, decentralized, open source, and
accessible search engine.

Search has been an essential part of knowledge acquisition from the
dawn of time, whether it is antique lexicographically ordered filing
cabinets or nowadays computer-based wonders such as Google, Bing or
Baidu. From casual search to help achieve common tasks such as
cooking, keeping up with the news, a regular dose of cat memes or
professional search such as science research. Search is, and will
remain, an essential daily-use tool, that stires human progress
forward.

Babelia aims to replace the use of privateer search engines with a
search engine that is open, hence under the control of the commons.

Eventually, Babelia may offer features commonly associated with
existing search engines, such as maps, mail accounts, calendars, image
and video galleries, and a feed reader inside a single software.
Babelia wants to be an easy to install, easy to use, easy to maintain,
no-code, personal search engine that can scale to billions of
documents, beyond a terabyte of text data, for under €100 a month per
Babelia instance.

## Abstract

This document wants to be full story about the code of babelia.

## `(arew untangle)`

`untangle` is cooperative event-loop that does not expose
continuations (aka. callbacks) and where procedures are [not
colored](https://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/). That can be achieved thanks to `call/cc`.

> **Note:** Understanding what is `call/cc` and how to use it is not
> necessary to use this library.

Until things like
[`io_uring`](https://thenewstack.io/how-io_uring-and-ebpf-will-revolutionize-programming-in-linux/)
are mainstream, `untangle` only expose the asynchronous support of BSD
sockets from the kernel.

`untangle` high-level interface expose generators and accumulators to
respectively read from and write into sockets. Both TCP and UDP
sockets are supported. In particular, Scheme's port interface is not
supported by this library.

`untangle` support the execution of CPU-bound procedures or other
blocking procedure in POSIX threads, possibly relying on a pool of
threads.

On top of `untangle` is implemented the HTTP library `hyperserver`.

### `(make-untangle)`

### `(untangle? obj)`

### `(untangle-spawn untangle thunk)`

### `(untangle-time untangle)`

### `(untangle-sleep untangle nanoseconds)`

### `(untangle-start untangle)`

### `(make-untangle-channel untangle)`

### `(untangle-channel? obj)`

### `(untangle-channel-recv channel)`

### `(untangle-channel-send channel obj)`

### `(untangle-channel-select channels)`

### `(make-untangle-socket domain type protocol)`

### `(untangle-accept socket)`

### `(untangle-bind socket)`

### `(untangle-listen socket)`

### `(untangle-close socket)`

### `(untangle-socket-generator socket)`

### `(untangle-socket-accumulator socket)`
