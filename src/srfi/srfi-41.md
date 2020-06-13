
## `(arew srfi srfi-41)`

This is based on [SRFI-41](https://srfi.schemers.org/srfi-41/).

### Abstract

Streams, sometimes called lazy lists, are a sequential data structure
containing elements computed only on demand. A stream is either null
or is a pair with a stream in its cdr. Since elements of a stream are
computed only when accessed, streams can be infinite. Once computed,
the value of a stream element is cached in case it is needed again.

### Reference

##### `stream-null`

##### `stream-cons`

##### `stream?`

##### `stream-null?`

##### `stream-pair?`

##### `stream-car`

##### `stream-cdr`

##### `stream-lambda`

##### `define-stream`

##### `list->stream`

##### `port->stream`

##### `stream`

##### `stream->list`

##### `stream-append`

##### `stream-concat`

##### `stream-constant`

##### `stream-drop`

##### `stream-drop-while`

##### `stream-filter`

##### `stream-fold`

##### `stream-for-each`

##### `stream-from`

##### `stream-iterate`

##### `stream-length`

##### `stream-let`

##### `stream-map`

##### `stream-match`

##### `stream-of`

##### `stream-range`

##### `stream-ref`

##### `stream-reverse`

##### `stream-scan`

##### `stream-take`

##### `stream-take-while`

##### `stream-unfold`

##### `stream-unfolds`

##### `stream-zip`
