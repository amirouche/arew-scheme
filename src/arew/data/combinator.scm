;;; Copyright © 2018-2019 Amirouche Boubekki <amirouche@hyper.dev>
;;
;;; Commentary:
;;
;; Parser combinators.
;;
;; XXX: Use (arew stream) because the parser combinator require a
;;      somewhat functional stream to be able to backtrack.  SRFI-41
;;      work, see guile-parser-combinators, but SRFI-41 is much
;;      slower.
;;
;; TODO: improve error handling
;; TODO: add compatible pk procedure
;;
;; Also see:
;;
;; - https://epsil.github.io/gll/
;; - https://docs.racket-lang.org/parsack/index.html
;; - https://docs.racket-lang.org/megaparsack/
;; - https://git.dthompson.us/guile-parser-combinators.git
;; - https://gitlab.com/tampe/stis-parser
;;
;;; Code:
;;
(library (arew data combinator)
  (export
   ;; port->stream
   make-pseudo-xchar
   parse
   parse-any
   parse-char
   parse-char-set
   parse-each
   parse-either
   parse-lift
   parse-maybe
   parse-one-or-more
   parse-only
   parse-return
   parse-xstring
   parse-unless
   parse-unless*
   parse-when
   parse-when*
   parse-xchar
   parse-zero-or-more
   string->stream
   xchar-char
   xchar?
   )

  (import
   (scheme base)
   (scheme charset)
   (only (chezscheme) format)
   (arew stream))

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((define-syntax-rule (keyword args ...) body)
       (define-syntax keyword
         (syntax-rules ()
           ((keyword args ...) body))))))

  (define-record-type <result>
    (make-result value stream)
    result?
    (value result-value)
    (stream result-stream))

  (define-record-type <failure>
    (make-failure value parser args)
    failure?
    (value failure-value)
    (parser failure-parser)
    (args failure-args))

  (define continue make-result)

  (define (fail value parser . args)
    (make-failure value parser args))

  (define-record-type <xchar>
    ;; extended char with line, column and offset information
    (make-xchar char line column offset)
    xchar?
    (char xchar-char)
    (line xchar-line)
    (column xchar-column)
    (offset xchar-offset))

  (define (make-pseudo-xchar char)
    (make-xchar char #f #f #f))

  (define (parse-lift proc parser)
    "Apply PROC to the result of PARSER"
    (lambda (stream)
      (let ((out (parser stream)))
        (if (failure? out)
            out
            (continue (proc (result-value out)) (result-stream out))))))

  (define (xchar-format xchar port)
    ;; TODO: replace with (scheme show)
    (format port "<xchar ~s [~a, ~a] @ ~a>"
            (xchar-char xchar)
            (xchar-line xchar)
            (xchar-column xchar)
            (xchar-offset xchar)))

  (define (string->stream string)
    ;; TODO: optimize
    (let loop ((chars (string->list string))
               (line 1)
               (column 1)
               (offset 0)
               (out '()))
      (if (null? chars)
          (list->stream (reverse out))
          (if (eq? (car chars) #\newline)
              (loop (cdr chars)
                    (+ 1 line)
                    1
                    (+ 1 offset)
                    (cons (make-xchar #\newline line column offset) out))
              (loop (cdr chars)
                    line
                    (+ 1 column)
                    (+ 1 offset)
                    (cons (make-xchar (car chars) line column offset) out))))))

  (define (parse parser stream)
    (let ((out (parser stream)))
      (if (failure? out)
          (error 'combinatorix "parse failed" out)
          (if (stream-empty? (result-stream out))
              (result-value out)
              (error 'combinatorix
                     "stream was not fully consumed"
                     (stream-car (result-stream out)))))))

  (define (parse-char char)
    (lambda (stream)
      (call-with-values stream
        (lambda (value next)
          (if next
              (if (char=? value char)
                  (continue value next)
                  (fail value parse-char char))
              (fail (eof-object) parse-char char))))))

  (define (parse-xchar char)
    (lambda (stream)
      (call-with-values stream
        (lambda (value next)
          (if next
              (if (char=? (xchar-char value) char)
                  (continue value next)
                  (fail value parse-xchar char))
              (fail (eof-object) parse-xchar char))))))

  (define (%either . args)
    (lambda (stream)
      (let loop ((parsers args))
        (if (null? parsers)
            (apply fail (stream-car stream) %either args)
            (let ((out (((car parsers)) stream)))
              (if (failure? out)
                  (loop (cdr parsers))
                  out))))))

  (define-syntax-rule (parse-either parser ...)
    (%either (lambda () parser) ...))

  (define (%each . args)
    (lambda (stream)
      (let loop ((parsers args)
                 (stream stream)
                 (out '()))
        (if (null? parsers)
            (continue (reverse out) stream)
            (let ((out* (((car parsers)) stream)))
              (if (failure? out*)
                  out*
                  (loop (cdr parsers)
                        (result-stream out*)
                        (cons (result-value out*) out))))))))

  (define-syntax-rule (parse-each parser ...)
    (%each (lambda () parser) ...))

  (define (parse-zero-or-more parser)
    (lambda (stream)
      (let loop ((stream stream)
                 (out '()))
        (let ((out* (parser stream)))
          (if (failure? out*)
              (continue (reverse out) stream)
              (loop (result-stream out*) (cons (result-value out*) out)))))))

  (define (parse-one-or-more parser)
    (parse-lift (lambda (x) (apply cons x)) (parse-each parser (parse-zero-or-more parser))))

  (define (parse-when predicate? parser)
    (lambda (stream)
      (call-with-values stream
        (lambda (value next)
          (if next
              (if (predicate? value)
                  (parser stream)
                  (fail value parse-when predicate?))
              (fail (eof-object) parse-when predicate?))))))

  (define (parse-when* parser other)
    ;; more general than parse-when
    (lambda (stream)
      (let ((out (parser stream)))
        (if (failure? out)
            out
            (other stream)))))

  (define (parse-unless predicate? parser)
    (parse-when (lambda (value) (not (predicate? value))) parser))

  (define (parse-unless* parser other)
    ;; more general than parse-unless
    (lambda (stream)
      (let ((out (parser stream)))
        (if (failure? out)
            (other stream)
            (fail (stream-car stream) parse-unless* parser other)))))

  (define (parse-only predicate? parser)
    ;; PARSER succeed only if its result passed to PREDICATE? return
    ;; true.
    (lambda (stream)
      (let ((out (parser stream)))
        (if (failure? out)
            out
            (if (predicate? (result-value out))
                out
                (fail (stream-car stream) parse-only predicate? parser))))))

  (define (parse-char-set char-set)
    (lambda (stream)
      (call-with-values stream
        (lambda (value next)
          (if next
              (if (char-set-contains? char-set (xchar-char value))
                  (continue value next)
                  (fail value parse-char-set char-set))
              (fail (eof-object) parse-char-set char-set))))))

  (define parse-any
    (lambda (stream)
      (call-with-values stream
        (lambda (value next)
          (if next
              (continue value next)
              (fail (eof-object) parse-any #f))))))

  ;;
  ;; TODO: maybe dangerous
  ;;
  ;; (define (port->stream port)
  ;;   (let ((buffer (make-hash-table)))
  ;;     (let loop ((index 0))
  ;;       (lambda ()
  ;;         (let ((char (hashtable-ref buffer index #f)))
  ;;           (cond
  ;;            ((eof-object? char) (values #f #f))
  ;;            ((char? char) (values char (loop (+ 1 index))))
  ;;            (else (let ((char (get-char port)))
  ;;                    (hashtable-set! buffer index char)
  ;;                    (if (eof-object? char)
  ;;                        (values #f #f)
  ;;                        (values char (loop (+ 1 index))))))))))))

  (define (parse-return value)
    (lambda (stream)
      (continue value stream)))

  (define (parse-maybe parser)
    (parse-either parser (parse-return #f)))

  (define (parse-xstring string)
    (apply %each (map (lambda (char) (lambda () (parse-xchar char))) (string->list string))))

  )
