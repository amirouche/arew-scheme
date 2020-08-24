(import (only (chezscheme) fxsrl fxlogor fxsll fxlogand))
(import (scheme base))
(import (scheme list))
(import (scheme fixnum))
(import (scheme char))
(import (scheme file))
(import (scheme write))
(import (scheme generator))
(import (scheme comparator))
(import (scheme process-context))
(import (arew network http))
(import (arew data aho-corasick))


(define (generator-consume generator)
  (let loop ((byte (generator)))
    (unless (eof-object? byte)
      (loop (generator)))))

(define (file-port-generator port)
  (lambda ()
    (read-u8 port)))

(define (generator-consume-line generator)
  (let loop ((byte (generator)))
    (unless (fx=? byte 10)
      (loop (generator)))))

(define (generator->line generator)
  (let loop ((byte (generator))
             (out '()))
    (if (fx=? byte 10)
        (cdr out)
        (loop (generator) (cons byte out)))))

(define (gtake* generator count)
  (lambda ()
    (if (zero? count)
        (eof-object)
        (begin (set! count (fx- count 1)) (generator)))))

;; WARC-Target-URI

(define (warc-record-read generator)
  ;; consume WARC/1.0
  (generator-consume-line generator)

  (let ((headers (http-headers-read generator)))
    (let loop ((headers headers)
               (uri #f)
               (content-length #f))
      (if (null? headers)
          (begin 'null (values uri
                               (gtake* generator content-length)))
          (let ((key (string-downcase (caar headers))))
            (cond
             ((string=? key "warc-target-uri") (loop (cdr headers)
                                                     (cdar headers)
                                                     content-length))
             ((string=? key "content-length")
              (loop (cdr headers)
                    uri
                    (string->number (cdar headers))))
             (else (loop (cdr headers) uri content-length))))))))

(define (utf8->char generator)
  (define end? #f)
  (define (fini)
    (set! end? #t)
    #xfffd)
  (lambda ()
    (if end?
        (eof-object)
        (let ((b1 (generator)))
          (if (eof-object? b1)
              (begin (set! end? #t) (eof-object))
              (cond
               ((fx<=? b1 #x7f) ;; one-byte encoding
                (integer->char b1))
               ((fx<=? #xc0 b1 #xdf) ;; two-byte encoding
                (let ((b2 (generator)))
                  (if (not (eof-object? b2))
                      (if (fx=? (fxsrl b2 6) #b10) ;; second byte a continuation byte?
                          (let ((x (fxlogor (fxsll (fxlogand b1 #b11111) 6) (fxlogand b2 #b111111))))
                            (if (fx<=? x #x7f) #\xfffd (integer->char x)))
                          ;; second byte is not a continuation byte
                          #\xfffd)
                      ;; have only one byte
                      (fini))))
               ((fx<=? #xe0 b1 #xef) ;; three-byte encoding
                (let ((b2 (generator)))
                  (if (not (eof-object? b2))
                      (if (fx=? (fxsrl b2 6) #b10) ;; second byte a continuation byte?
                          (let ((b3 (generator)))
                            (if (not (eof-object? b3))
                                (if (fx=? (fxsrl b3 6) #b10) ;; third byte a continuation byte?
                                    (let ((x (fxlogor
                                              (fxsll (fxlogand b1 #b1111) 12)
                                              (fxsll (fxlogand b2 #b111111) 6)
                                              (fxlogand b3 #b111111))))
                                      (if (and (fx>=? x #x800) (not (fx<=? #xd800 x #xdfff)))
                                          (integer->char x)
                                          #\xfffd))
                                    ;; third byte is not a continuation byte
                                    #\xfffd)
                                ;; have only two bytes
                                (fini)))
                          ;; second byte is not a continuation byte
                          #\xfff)
                      ;; have only one byte
                      (fini))))
               ((fx<=? #xf0 b1 #xf4) ;; four-byte encoding
                (let ((b2 (generator)))
                  (if (not (eof-object? b2))
                      (if (fx=? (fxsrl b2 6) #b10) ;; second byte a continuation byte?
                          (let ((b3 (generator)))
                            (if (not (eof-object? b3))
                                (if (fx=? (fxsrl b3 6) #b10) ;; third byte a continuation byte?
                                    (let ((b4 (generator)))
                                      (if (not (eof-object? b4))
                                          (if (fx=? (fxsrl b4 6) #b10) ;; fourth byte a continuation byte?
                                              (let ((x (fxlogor
                                                        (fxsll (fxlogand b1 #b111) 18)
                                                        (fxsll (fxlogand b2 #b111111) 12)
                                                        (fxsll (fxlogand b3 #b111111) 6)
                                                        (fxlogand b4 #b111111))))
                                                (if (fx<=? #x10000 x #x10ffff)
                                                    (integer->char x)
                                                    #\xfffd))
                                              ;; fourth byte is not a continuation byte
                                              #\xfffd)
                                          (fini)))
                                      #\xfffd)
                                (fini)))
                          ;; second byte is not a continuation byte
                           #\xfffd)
                      (fini))))
               (else
                 #\xfffd)))))))

(define (warc-record-generator generator)
  (call-with-values (lambda () (warc-record-read wet-generator))
    (lambda (_ body)
      (generator-consume body)))

  ;; consume some CRLF
  (generator) (generator) (generator) (generator)
  
  (lambda ()
    (call-with-values (lambda () (warc-record-read wet-generator))
      (lambda (uri body)
        (values uri (gmap char-downcase (utf8->char body )))))))

(define wet-generator
  (file-port-generator (open-binary-input-file (cadr (command-line)))))

(define warc-record-reader (warc-record-generator wet-generator))

(define ac (make-aho-corasick))

(define count* 0)

(define expected-length (length (cddr (command-line))))

(let loop ((keywords (cddr (command-line))))
  (unless (null? keywords)
    (aho-corasick-add! ac (car keywords))
    (loop (cdr keywords))))

(aho-corasick-finalize! ac)


(let loop ()
  (call-with-values warc-record-reader
    (lambda (uri body)
      (when (fx=? (length (delete-duplicates (aho-corasick-match ac body)))
                  expected-length)
        (display (string-append uri "\n"))
        (set! count* (fx+ count* 1)))))
  (wet-generator)
  (wet-generator)
  (wet-generator)
  (wet-generator)
  (unless (eof-object? (wet-generator))
    (loop)))

(pk count*)
