
## `(arew socket)`

Low-level BSD socket bindings.

related: https://srfi.schemers.org/srfi-106/srfi-106.html
related: https://practical-scheme.net/gauche/man/gauche-refe/Networking.html#Networking
related: http://wiki.call-cc.org/eggref/5/socket

### `struct-sockaddr`

### `struct-sockaddr-in`

### `struct-sockaddr-in6`

### `struct-sockaddr-un`

### `(address-family->integer symbol)`

### `(integer->address-family integer)`

### `(socket-type->integer symbol)`

### `(integer->socket-type)`

### `(protocol->integer symbol)`

### `(socket-socket domain type protocol)`

ref: https://manpages.debian.org/buster/manpages-dev/socket.2.en.html

### `(socket-connect fd address length)`

ref: https://manpages.debian.org/buster/manpages-dev/connect.2.en.html

### `(address-info-flags->integer symbol)`

### `(integer->address-info-flags integer)`

### `(msg-flag->integer symbol)`

### `(socket-recv fd buffer length flags)`

ref: https://manpages.debian.org/buster/manpages-dev/recv.2.en.html

### `(socket-recvfrom fd buffer length flags address address-length)`

ref: https://manpages.debian.org/buster/manpages-dev/recv.2.en.html

### `(socket-send fd buffer length flags)`

ref: https://manpages.debian.org/buster/manpages-dev/send.2.en.html

### `(socket-sendto fd buffer length flags address address-length)`

ref: https://manpages.debian.org/buster/manpages-dev/send.2.en.html

### `(socket-accept fd address length)`

ref: https://manpages.debian.org/buster/manpages-dev/accept.2.en.html

### `(socket-bind fd address length)`

ref: https://manpages.debian.org/buster/manpages-dev/bind.2.en.html

### `(socket-fcntl fd key)`

ref: https://manpages.debian.org/buster/manpages-dev/fcntl.2.en.html

### `(socket-fcntl! fd key value)`

ref: https://manpages.debian.org/buster/manpages-dev/fcntl.2.en.html

### `(socket-getsockopt fd level name value length)`

ref: https://manpages.debian.org/buster/manpages-dev/getsockopt.2.en.html

### `(socket-getsockname fd address length)`

ref: https://manpages.debian.org/buster/manpages-dev/getsockname.2.en.html

### `(socket-listen fd backlog)`

ref: https://manpages.debian.org/buster/manpages-dev/listen.2.en.html
