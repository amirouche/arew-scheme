
## `(arew srfi srfi-29)`

This is based on [SRFI-29](https://srfi.schemers.org/srfi-29/).

### Abstract

An interface to retrieving and displaying locale sensitive messages. A
Scheme program can register one or more translations of templated
messages, and then write Scheme code that can transparently retrieve
the appropriate message for the locale under which the Scheme system
is running.

### Reference

#### `current-language`
#### `current-country`
#### `current-locale-details`
#### `declare-bundle!`
#### `store-bundle`
#### `store-bundle!`
#### `load-bundle!`
#### `localized-template`
