;; Copyright 2019 (c) Amirouche (amirouche@hyper.dev)
#!chezscheme
(library (arew network socket)

  (export
   errno
   strerror
   check
   check*
   close
   socket
   %socket
   connect
   %connect
   getaddrinfo
   recv
   %recv
   recvfrom
   send
   %send
   sendto
   accept
   %accept
   bind
   fcntl
   fcntl!
   setsockopt
   listen
   fd->port)

  (import (chezscheme))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((define-syntax-rule (keyword args ...) body)
       (define-syntax keyword
         (syntax-rules ()
           ((keyword args ...) body))))))

  ;; ffi helpers

  (define (make-double-pointer)
      (foreign-alloc 8))

  (define-syntax-rule (alloc ftype)
    (make-ftype-pointer ftype (foreign-alloc (ftype-sizeof ftype))))

  (define (bytevector->pointer bv)
    (#%$object-address bv (+ (foreign-sizeof 'void*) 1)))

  (define (call-with-lock obj proc)
    (lock-object obj)
    (call-with-values (lambda () (proc))
      (lambda out
        (unlock-object obj)
        (apply values out))))

  ;; bindings

  (define stdlib (load-shared-object #f))

  (define strerror
    (let ((func (foreign-procedure "strerror" (int) string)))
      (lambda (code)
        (func code))))

  (define errno
    (let ((entry (foreign-entry "errno")))
      (lambda ()
        (foreign-ref 'int entry 0))))

  (define-syntax-rule (check* who v)
    (when (= v -1)
      (error who (strerror (errno)))))

  (define-syntax-rule (check who v)
    (if (= v -1)
        (error who (strerror (errno)))
        v))

  (define close
    (let ((func (foreign-procedure __collect_safe "close" (int) int)))
      (lambda (fd)
        (check* 'close (func fd)))))

  ;; taken from /usr/include/bits/socket.h
  (define (address-family->int symbol)
    (case symbol
      ((unspec unspecified unspecific) 0)
      ((unix local) 1)
      ((inet) 2)
      ((inet6) 10)
      (else (error 'socket "Unknown address family" symbol))))

  (define (int->address-family int)
    (case int
      ((0) 'unspec)
      ((1) 'unix)
      ((2) 'inet)
      ((10) 'inet6)
      (else (error 'socket "Unknown address family integer" int))))

  ;; taken from /usr/include/bits/socket_type.h
  (define (socket-type->int symbol)
    (if (list? symbol)
        (apply bitwise-ior (map socket-type->int symbol))
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
          (else (error 'socket "Unknown socket type" symbol)))))

  (define (int->socket-type int)
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
  (define (protocol->int symbol)
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
      (else (error 'socket "Unknown protocol" symbol))))

  (define %socket
    (let ((func (foreign-procedure __collect_safe "socket" (int int int) int)))
      (lambda (domain type protocol)
        (func domain type protocol))))

  (define (socket domain type protocol)
    (check 'socket (%socket (address-family->int domain)
                            (socket-type->int type)
                            (protocol->int protocol))))

  (define-ftype %sockaddr
    (struct (family unsigned-short)))

  (define-ftype %sockaddr-in
    (struct (family unsigned-short)
            (port (endian big unsigned-16))
            (address (endian big unsigned-32))
            (padding (array 8 char))))

  (define-ftype %sockaddr-in6
    (struct (family unsigned-short)
            (port (endian big unsigned-16))
            (flow-info unsigned-32)
            (address-0 (endian big unsigned-32))
            (address-1 (endian big unsigned-32))
            (address-2 (endian big unsigned-32))
            (address-3 (endian big unsigned-32))
            (scope-id unsigned-32)))

  (define-ftype sockaddr-un
    (struct (family unsigned-short)
            (path (array 108 char))))

  (define (string-join list string)
    (let loop ((list list)
               (out '()))
      (if (null? list)
          (apply string-append (reverse (cdr out)))
          (loop (cdr list)
                (cons string (cons (car list) out))))))

  (define (ipv4->string ip)
    (string-join (map number->string (list (bitwise-arithmetic-shift-right ip 24)
                              (bitwise-and (bitwise-arithmetic-shift-right ip 16) #xff)
                              (bitwise-and (bitwise-arithmetic-shift-right ip 8) #xff)
                              (bitwise-and ip #xff)))
                 "."))

  (define (sockaddr-in->alist pointer)
    (let ((pointer (make-ftype-pointer %sockaddr-in (ftype-pointer-address pointer))))
      (let ((family 'inet)
            (port (ftype-ref %sockaddr-in (port) pointer))
            (ip (ipv4->string (ftype-ref %sockaddr-in (address) pointer))))
        `((family . ,family)
          (port . ,port)
          (ip . ,ip)))))

 (define (sockaddr->alist pointer)
   (let ((family (int->address-family
                  (ftype-ref %sockaddr
                             (family)
                             (make-ftype-pointer %sockaddr (ftype-pointer-address pointer))))))
      (case family
        ((inet)
         (sockaddr-in->alist pointer))
        (else #f))))

  (define %connect
    (let ((func (foreign-procedure __collect_safe "connect" (int void* int) int)))
      (lambda (sockfd addr addrlen)
        (func sockfd (ftype-pointer-address addr) addrlen))))

  (define (%explode-inet-address alist)
    (let ((ip #f)
          (port #f))
      (let loop ((alist alist))
        (if (null? alist)
            (values ip port)
            (begin
              (case (caar alist)
                ((ip) (set! ip (cdar alist)))
                ((port) (set! port (cdar alist))))
              (loop (cdr alist)))))))

  (define (ipv4 one two three four)
    (+ (* one 256 256 256)
       (* two 256 256)
       (* three 256)
       four))

  (define (tokenize l char)
    (let loop ((t '())
               (l l))
      (if (pair? l)
          (let ((c (car l)))
            (if (char=? c char)
                (cons (reverse t) (loop '() (cdr l)))
                (loop (cons (car l) t) (cdr l))))
          (if (null? t)
              '()
              (list (reverse t))))))

  (define (string-split s char)
    (map list->string (tokenize (string->list s) char)))

  (define (string->ipv4 address)
    (let* ((components (map string->number (string-split address #\.))))
      (apply ipv4 components)))

  (define (%make-inet-address address)
    (call-with-values (lambda () (%explode-inet-address address))
      (lambda (ip port)
        (assert (string? ip))
        (assert (integer? port))
        (let ((out (alloc %sockaddr-in)))
          (ftype-set! %sockaddr-in (family) out (address-family->int 'inet))
          (ftype-set! %sockaddr-in (port) out port)
          (ftype-set! %sockaddr-in (address) out (string->ipv4 ip))
          out))))

  (define (%make-address address)
    (let ((family (assq 'family address)))
      (if (not family)
          (error 'socket "no family")
          (case (cdr family)
            ((inet) (values (%make-inet-address address) (ftype-sizeof %sockaddr-in)))
            (else (error 'socket "unsupported family"))))))

  (define (connect sockfd address)
    (call-with-values (lambda () (%make-address address))
      (lambda (addr addrlen)
        (check* 'connect (%connect sockfd addr addrlen)))))

  (define gai-strerror
    (let ((func (foreign-procedure "gai_strerror" (int) string)))
      (lambda (error)
        (func error))))

  (define-ftype %addrinfo
    (struct (flags int)
            (family int)
            (socktype int)
            (protocol int)
            (addrlen int)
            (addr (* %sockaddr))
            (canonname (* char))
            (next (* %addrinfo))))

  (define %getaddrinfo
    (let ((func (foreign-procedure "getaddrinfo" (string string void* void*) int)))
      (lambda (node service hints res)
        (func node service hints res))))

  ;; taken from /usr/include/netdb.h
  (define (address-info-flags->int symbol)
    (if (list? symbol)
        (apply bitwise-ior (map address-info-flags->int symbol))
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
           (error 'socket "Unknown address-info flag" symbol)))))

  (define (int->address-info-flags int)
    (filter (lambda (x) x)
            (map (lambda (el)
                   (let ((sym (car el))
                         (n (cadr el)))
                     (if (not (zero? (bitwise-and n int)))
                         sym
                         #f)))
                 '((passive 1)
                   (canonname 2)
                   (numerichost 4)
                   (v4mapped 8)
                   (all #x10)
                   (addrconfig #x20)
                   (idn #x40)
                   (canonidn #x80)
                   (idn-allow-unassigned #x100)
                   (idn-use-std3-ascii-rules #x200)))))

  (define (explode-hints hints)
    (let ((flags #f)
          (family #f)
          (type #f)
          (protocol #f))
      (let loop ((hints hints))
        (if (null? hints)
            (values flags family type protocol)
            (begin
              (case (caar hints)
                ((flags) (set! flags (cdar hints)))
                ((family) (set! family (cdar hints)))
                ((type) (set! type (cdar hints)))
                ((protocol) (set! protocol (cdar hints)))
                (else (error 'socket "Unknown hint")))
              (loop (cdr hints)))))))

  (define (alist->address-info hints)
    (call-with-values (lambda () (explode-hints hints))
      (lambda (flags family type protocol)
        (assert (symbol? flags))
        (assert (symbol? family))
        (assert (symbol? type))
        (assert (symbol? protocol))
        (let ((out (alloc %addrinfo)))
          (ftype-set! %addrinfo (flags) out (address-info-flags->int flags))
          (ftype-set! %addrinfo (family) out (address-family->int family))
          (ftype-set! %addrinfo (socktype) out (socket-type->int type))
          (ftype-set! %addrinfo (protocol) out (protocol->int protocol))
          (ftype-set! %addrinfo (addrlen) out 0)
          (ftype-set! %addrinfo (addr) out 0)
          (ftype-set! %addrinfo (canonname) out 0)
          (ftype-set! %addrinfo (next) out 0)
          out))))

  (define freeaddrinfo
    (let ((func (foreign-procedure __collect_safe "freeaddrinfo" (void*) void*)))
      (lambda (addrinfo)
        (func addrinfo))))

  (define (extract-string p)
    (define soc (foreign-sizeof 'char))
    (if (zero? p)
        #f
        (let loop ((i 0)
                   (lst '()))
          (let ((c (foreign-ref 'char 0 (+ p (* i soc)))))
            (if (= c 0)
                (utf8->string (apply bytevector (reverse lst)))
                (loop (+ i 1)
                      (cons c lst)))))))

  (define (addrinfo->alist pointer)
    (let* ((flags (int->address-info-flags (ftype-ref %addrinfo (flags) pointer)))
           (family (int->address-family (ftype-ref %addrinfo (family) pointer)))
           (socket-type (int->socket-type (ftype-ref %addrinfo (socktype) pointer)))
           (address (sockaddr->alist (ftype-ref %addrinfo (addr) pointer)))
           (canonical-name (if (memq 'canonname flags)
                               (extract-string (ftype-pointer-address (ftype-ref %addrinfo (canonname) pointer)))
                               #f)))
      `((flags . ,flags)
        (family . ,family)
        (type . ,socket-type)
        (address . ,address)
        (canonical-name . ,canonical-name))))

  (define (getaddrinfo node service hints)
    ;; TODO: implement non-blocking domain name resolution (DNS)
    (let ((node (if node node 0))
          (service (if service service 0))
          (hints (if hints (alist->address-info hints) 0))
          (out (make-double-pointer)))
      (let ((code (%getaddrinfo node service hints out)))
        (if (not (= code 0))
            (values #f code)
            (let ((out (foreign-ref 'void* out 0)))
              (let loop ((out* '())
                         (pointer out))

                (if (= pointer 0)
                    (begin
                      (freeaddrinfo out)
                      (values #t (reverse out*)))
                    (let ((pointer (make-ftype-pointer %addrinfo pointer)))
                      (loop (cons (addrinfo->alist pointer) out*)
                            (ftype-pointer-address (ftype-ref %addrinfo (next) pointer)))))))))))

  (define (msg-flag->int symbol)
    (if (list? symbol)
        (apply bitwise-ior (map msg-flag->int symbol))
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

  (define %recv
    (let ((func (foreign-procedure __collect_safe "recv" (int void* size_t int) ssize_t)))
      (lambda (fd buffer length flags)
        (func fd buffer length flags))))

  (define (recv fd bytevector flags)
    (call-with-lock bytevector
      (lambda ()
        (%recv fd
               (bytevector->pointer bytevector)
               (bytevector-length bytevector)
               (msg-flag->int flags)))))

  (define %recvfrom
    (let ((func (foreign-procedure __collect_safe "recvfrom" (int void* size_t int void* void*) ssize_t)))
      (lambda (fd buffer length flags addr addrlen)
        (func fd buffer length flags addr addrlen))))

  (define (recvfrom fd bytevector flags)
    (let ((addr (make-double-pointer))
          (addrlen (make-double-pointer)))
      (call-with-lock bytevector
        (lambda ()
          (let ((out (%recvfrom fd
                                (bytevector->pointer bytevector)
                                (bytevector-length bytevector)
                                (msg-flag->int flags)
                                addr
                                addrlen)))
            (if (= out -1)
                (values #f #f)
                (values out (sockaddr->alist addr))))))))

  (define %send
    (let ((func (foreign-procedure __collect_safe "send" (int void* size_t int) ssize_t)))
      (lambda (fd buffer length flags)
        (func fd buffer length flags))))

  (define (send fd bytevector flags)
    (call-with-lock bytevector
      (lambda ()
        (%send fd
               (bytevector->pointer bytevector)
               (bytevector-length bytevector)
               (msg-flag->int flags)))))

  (define %sendto
    (let ((func (foreign-procedure __collect_safe "sendto" (int void* size_t int void* int) ssize_t)))
      (lambda (fd buffer length flags addr addrlen)
        (func fd buffer length flags addr addrlen))))

  (define (sendto fd bytevector flags address)
    (call-with-lock bytevector
                    (lambda ()
                      (call-with-values (lambda () (%make-address address))
                        (lambda (addr addrlen)
                          (%sendto fd
                                   (bytevector->pointer bytevector)
                                   (bytevector-length bytevector)
                                   (msg-flag->int flags)
                                   addr
                                   addrlen))))))

  (define %accept
    (let ((func (foreign-procedure __collect_safe "accept" (int void* void*) int)))
      (lambda (sock addr length)
        (func sock addr length))))

  (define (accept sock)
    (let ((out (%accept sock 0 0)))
      (check 'accept out)))

  (define %bind
    (let ((func (foreign-procedure __collect_safe "bind" (int void* int) int)))
      (lambda (sock addr length)
        (func sock (ftype-pointer-address addr) length))))

  (define (bind sock address)
    (call-with-values (lambda () (%make-address address))
      (lambda (address length)
        (check* 'bind (%bind sock address length)))))

  (define fcntl
    (let ((func (foreign-procedure __collect_safe "fcntl" (int int) int)))
      (lambda (socket command)
        (func socket command))))

  (define fcntl!
    (let ((func (foreign-procedure __collect_safe "fcntl" (int int int) int)))
      (lambda (socket command flags)
        (func socket command flags))))

  (define %setsockopt
    (let ((func (foreign-procedure __collect_safe "setsockopt" (int int int void* int) int)))
      (lambda (socket level optname optval optlen)
        (check* 'setsockopt (func socket level optname optval optlen)))))

  (define (setsockopt socket level optname optval)
    (let ((value (ftype-pointer-address (alloc int))))
      (foreign-set! 'int value 0 optval)
      (%setsockopt socket level optname value (foreign-sizeof 'int))))

  (define listen
    (let ((func (foreign-procedure __collect_safe "listen" (int int) int)))
      (lambda (socket backlog)
        (check* 'listen (func socket backlog)))))

  (define (fd->port fd)
    (make-custom-binary-input/output-port
     (format "socket ~a" fd)
     (lambda (bv start n) ;; read
       (let ((pointer (#%$object-address bv (+ (foreign-sizeof 'ptr) 1 start))))
         (let ((out (%recv fd pointer n 0)))
           (if (= out -1)
               (error 'socket (strerror (errno)))
               out))))
     (lambda (bv start n) ;; write
       (let ((pointer (#%$object-address bv (+ (foreign-sizeof 'ptr) 1 start))))
         (let ((out (%send fd pointer n 0)))
           (if (= out -1)
               (error 'socket (strerror (errno)))
               out))))
     #f ;; get-position
     #f ;; set-position
     (lambda ()
       (close fd))))

  )
