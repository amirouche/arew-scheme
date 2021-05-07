;; Copyright 2019-2021 (c) Amirouche (amirouche@hyper.dev)
#!chezscheme
(library (arew socket)

  (export
   address-family->integer
   integer->address-family
   socket-type->integer
   integer->socket-type
   protocol->integer
   address-info-flags->integer
   integer->address-info-flags
   msg-flag->integer
   socket-accept4
   socket-bind
   socket-close
   socket-connect
   socket-fcntl
   socket-listen
   socket-recv
   socket-recvfrom
   socket-send
   socket-sendto
   socket-setsockopt
   socket-getsockname
   socket-socket
   make-socket-inet-address)

  (import (chezscheme) (arew cffi))

  (define socket-close (foreign-procedure* int "close" (int)))

  ;; taken from /usr/include/bits/socket.h
  (define (address-family->integer symbol)
    (case symbol
      ((unspec unspecified unspecific) 0)
      ((unix local) 1)
      ((inet) 2)
      ((inet6) 10)
      (else (error 'socket "Unknown address family symbol" symbol))))

  (define (integer->address-family int)
    (case int
      ((0) 'unspec)
      ((1) 'unix)
      ((2) 'inet)
      ((10) 'inet6)
      (else (error 'socket "Unknown address family integer" int))))

  ;; taken from /usr/include/bits/socket_type.h
  (define (socket-type->integer symbol)
    (if (pair? symbol)
        (apply fxior (map socket-type->integer symbol))
        (case symbol
          ((stream) 1)
          ((dgram datagram) 2)
          ((raw) 3)
          ((rdm) 4)
          ((seqpacket) 5)
          ((dccp) 6)
          ((packet) 10)
          ((cloexec close-on-exec) #x80000)
          ((nonblock non-blocking) #x800)
          (else (error 'socket "Unknown socket type symbol" symbol)))))

  (define (integer->socket-type int)
    (case int
      ((1) 'stream)
      ((2) 'datagram)
      ((3) 'raw)
      ((4) 'rdm)
      ((5) 'seqpacket)
      ((6) 'dccp)
      ((10) 'packet)
      (else (error 'socket "Unknown socket type integer" int))))

  ;; taken from /usr/include/linux/in.h
  (define (protocol->integer symbol)
    (case symbol
      ((ip ip4 ipv4 #f) 0)
      ((icmp) 1)
      ((igmp) 2)
      ((ipip) 4)
      ((tcp) 6)
      ((egp) 8)
      ((pup) 12)
      ((udp) 17)
      ((idp) 22)
      ((tp) 29)
      ((dccp) 33)
      ((ipv6 ip6) 41)
      ((rsvp) 46)
      ((gre) 47)
      ((esp) 50)
      ((ah) 51)
      ((mtp) 92)
      ((beetph) 94)
      ((encap) 98)
      ((pim) 103)
      ((comp) 108)
      ((sctp) 132)
      ((udplite) 136)
      ((mpls) 137)
      ((raw) 255)
      (else (error 'socket "Unknown protocol symbol" symbol))))

  (define socket-socket
    (foreign-procedure* (int) "socket" (int int int)))

  (define-ftype struct-sockaddr
    (struct (family unsigned-short)))

  (define-ftype struct-sockaddr-in
    (struct (family unsigned-short)
            (port (endian big unsigned-16))
            (address (endian big unsigned-32))
            (padding (array 8 char))))

  (define-ftype struct-sockaddr-in6
    (struct (family unsigned-short)
            (port (endian big unsigned-16))
            (flow-info unsigned-32)
            (address-0 (endian big unsigned-32))
            (address-1 (endian big unsigned-32))
            (address-2 (endian big unsigned-32))
            (address-3 (endian big unsigned-32))
            (scope-id unsigned-32)))

  (define-ftype struct-sockaddr-un
    (struct (family unsigned-short)
            (path (array 108 char))))

  (define (make-socket-inet-address ip port)

    (define (massage* one two three four)
      (fx+ (fxarithmetic-shift-left one 24)
           (fxarithmetic-shift-left two 16)
           (fxarithmetic-shift-left three 8)
           four))

    (define (massage string)
      ;; convert 4.3.2.1 into the big endian integer representation.
      (define count (string-length string))
      (let loop ((index index)
                 (out '(())))
        (if (fx=? index count)
            (apply massage* out)
            (let ((char (string-ref string index)))
              (if (char=? char #\.)
                  (loop (fx+ index 1)
                        (cons (string->number
                               (list->string (car out)))
                              (cdr out)))
                  (loop (fx+ index 1)
                        (cons (cons char (car out))
                              (cdr out))))))))

    (let ((out (ftype-alloc struct-sockaddr-in)))
      (ftype-set! struct-sockaddr-in (family) out 2) ;; family=inet
      (ftype-set! struct-sockaddr-in (port) out port)
      (ftype-set! struct-sockaddr-in (address) out (massage ip))
      out))

  (define socket-connect
    (foreign-procedure* int "connect" (int void* int)))

  ;; taken from /usr/include/netdb.h
  (define (address-info-flags->integer symbol)
    (if (pair? symbol)
        (apply fxior (map address-info-flags->integer symbol))
        (case symbol
          ((passive) 1)
          ((canonname) 2)
          ((numerichost) 4)
          ((v4mapped) 8)
          ((all) #x10)
          ((addrconfig) #x20)
          ((idn) #x40)
          ((canonidn) #x80)
          ((idn-allow-unassigned) #x100)
          ((idn-use-std3-ascii-rules) #x200)
          (else
           (error 'socket "Unknown address-info flag symbol" symbol)))))

  (define (integer->address-info-flags int)
    ;; Return a list with the symbol flags that are set.
    ;; filter map with fold
    (fold (lambda (item out)
            (define symbol (car item))
            (define flag (cadr item))
            (if (fxzero? (fxand flag integer))
                out
                (cons symbol out)))
          '()
          '((passive 1)
            (canonname 2)
            (numerichost 4)
            (v4mapped 8)
            (all #x10)
            (addrconfig #x20)
            (idn #x40)
            (canonidn #x80)
            (idn-allow-unassigned #x100)
            (idn-use-std3-ascii-rules #x200))))

  (define (msg-flag->integer symbol)
    (if (pair? symbol)
        (apply fxior (map msg-flag->integer symbol))
        (case symbol
          ((#f) 0)
          ((oob) #x01)
          ((peek) #x02)
          ((dontroute tryhard) #x04)
          ((ctrunc) #x08)
          ((proxy) #x10)
          ((trunc) #x20)
          ((dontwait) #x40)
          ((eor) #x80)
          ((waitall) #x100)
          ((fin) #x200)
          ((syn) #x400)
          ((confirm) #x800)
          ((rst) #x1000)
          ((errqueue) #x2000)
          ((nosignal) #x4000)
          ((more) #x8000)
          ((waitforone) #x10000)
          ((batch) #x40000)
          ((fastopen) #x20000000)
          ((cmsg-cloexec) #x4000000)
          (else (error 'socket "Unknown message flag" symbol)))))

  (define socket-recv
    (foreign-procedure* ssize_t "recv" (int void* size_t int)))

  (define socket-recvfrom
    (foreign-procedure* ssize_t "recvfrom" (int void* size_t int void* void*)))

  (define socket-send
    (foreign-procedure* ssize_t "send" (int void* size_t int)))

  (define socket-sendto
    (foreign-procedure* ssize_t "sendto" (int void* size_t int void* int)))

  (define socket-accept4
    (foreign-procedure int "accept" (int void* void* int) int))

  (define socket-bind
    (foreign-procedure* int "bind" (int void* int)))

  (define socket-fcntl
    (foreign-procedure* int "fcntl" (int int)))

  (define socket-fcntl!
    (foreign-procedure* int "fcntl" (int int int)))

  (define socket-setsockopt
    (foreign-procedure* int "setsockopt" (int int int void* int)))

  (define socket-getsockname
    (foreign-procedure* int "getsockopt" (int void* void*)))

  (define socket-listen
    (foreign-procedure* int "listen" (int int))))
