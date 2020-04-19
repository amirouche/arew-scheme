(import (scheme base))
(import (scheme cxr))
(import (scheme write))
(import (scheme process-context))
(import (arew network untangle))
(import (arew matchable))


(define port (string->number (caddr (command-line))))

(define (read-lines generator)
  (let ((count 0))
    (lambda ()
      (if (>= count 1024)
          ;; They can be at most 1024 lines including the request
          ;; line.
          (eof-object)
          (let loop ((byte (generator))
                     (length 0)
                     (out '()))
            (if (= length 4096)
                ;; A line can be at most 4096 bytes big.
                (begin
                  (set! count 1024) ;; Stay safe!
                  (eof-object))
                (if (= byte 13)
                    (let ((other (generator)))
                      (if (= other 10)
                          (begin
                            (set! count (+ count 1))
                            (list->string (reverse (map integer->char out))))
                          (loop (generator) (+ length 2) (cons other (cons byte out)))))
                    (loop (generator) (+ length 1) (cons byte out)))))))))

(define (string-split string char)
  (let ((len (string-length string)))
    (let split ((a 0)
                (b 0))
      (cond
       ((>= b len) (if (= a b) '() (cons (substring string a b) '())))
       ((char=? char (string-ref string b)) (if (= a b)
                                                (split (+ 1 a) (+ 1 b))
                                                (cons (substring string a b) (split b b))))
       (else (split a (+ 1 b)))))))

(define (router sfd method path version)
  (let ((accumulator (socket-accumulator sfd)))
    (accumulator (string->utf8 "HTTP/1.0 200 OK\r\n\r\n"))
    (close sfd)))

(define (handle sfd)
  (let* ((generator (socket-generator sfd))
         (lines (read-lines generator))
         (request-line (lines)))
    (if (eof-object? request-line)
        (close sfd) ;; TODO: return 400
        (match (string-split request-line #\space)
          ((method path version) (router sfd method path version))
          (else (close sfd)))))) ;; TODO: return 400

(define (server)
  (pk "starting the server at PORT" port)
  (let ((sfd (socket 'inet 'stream 'ip)))
    (bind sfd `((family . inet)
                (ip . "127.0.0.1")
                (port . ,port)))
    (listen sfd 50)
    (let loop ((client (accept sfd)))
      (handle client)
      (loop (accept sfd)))))

(untangle server)
