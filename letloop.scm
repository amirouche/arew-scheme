
(define (file-char-generator filename)
  (define port (open-input-file filename))

  (define (continue)
    (let ((out (read-char port)))
      (if (eof-object? out)
          (begin (set! yield eof-object) (eof-object))
          out)))

  (define yield continue)

  (lambda ()
    (yield)))

(define (cleanize char)
  (case char
    ((#\# #\. #\( #\[ #\) #\] #\` #\newline) #\space)
    (else char)))

(define (tokenize chars)
  (define token '())

  (define (continue)
    (let loop ((char (chars)))
      (if (eof-object? char)
          (if (null? token)
              (begin
                (set! yield eof-object)
                (eof-object))
              (let ((out (list->string (reverse token))))
                (set! yield eof-object)
                out))
          (if (char=? char #\space)
              (if (null? token)
                  (loop (chars))
                  (let ((out (list->string (reverse token))))
                    (set! token '())
                    out))
              (begin
                (set! token (cons char token))
                (loop (chars)))))))

  (define yield continue)

  (lambda ()
    (yield)))

(define (generator-string-uniquify generator)

  (define set (make-hashtable string-hash string=?))

  (define (continue)
    (let loop ((out (generator)))
      (if (eof-object? out)
          (begin
            (set! yield eof-object)
            (eof-object))
          (let ((generated? (hashtable-ref set out #f)))
            (if generated?
                (loop (generator))
                (begin
                  (hashtable-set! set out #t)
                  out))))))

  (define yield continue)

  (lambda ()
    (yield)))

(define tokens (tokenize (generator-map cleanize (file-char-generator "forms-index.txt"))))

(for-each pk (generator->list (generator-string-uniquify tokens)))
