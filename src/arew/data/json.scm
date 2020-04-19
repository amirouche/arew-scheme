;;; Copyright © 2019 Amirouche Boubekki <amirouche@hypermove.net>
;;;
;;; This module is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This module is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this module.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Comments:
;;
;; - 2019/02/05: initial version, https://www.json.org/
;;
(library (arew data json)

  (export
   json-null?
   json->scm
   )
  (import
   (scheme base)
   (scheme list)
   (scheme cxr)
   (scheme charset)
   (only (chezscheme) logior ash)
   (arew data parser combinator)
   (arew stream))

  (define %json-null '(null))

  (define (json-null)
    %json-null)

  (define (json-null? object)
    (eq? object json-null))

  (define (%json->scm string)
    ;; Algorithm:
    ;;
    ;; 1) Convert string to a stream of extended char
    ;; 2) Parse stream of xchar, to a stream of tokens useless space.
    ;; 3) Parse tokens into a scheme object
    (parse json (list->stream (parse tokens (string->stream string)))))

  (define (json->scm string)
    (guard (ex (else #f))
      (%json->scm string)))

  (define (->string value)
    (list->string (map xchar-char value)))

  (define (%parse-escaped input output)
    (parse-lift (const (make-pseudo-xchar output)) (parse-xstring input)))

  (define (hexcode->integer v)
    (string->number
     (list->string
      (list #\# #\x
            (xchar-char (car v))
            (xchar-char (cadr v))
            (xchar-char (caddr v))
            (xchar-char (cadddr v))))))

  (define parse-json-unicode
    (parse-lift cddr (parse-each (parse-xchar #\\)
                                 (parse-xchar #\u)
                                 parse-any
                                 parse-any
                                 parse-any
                                 parse-any)))

  (define parse-simple-unicode
    (parse-lift hexcode->integer parse-json-unicode))

  ;; taken from decode.py of cpython

  (define parse-out-of-range-unicode
    (parse-only (lambda (v) (<= #xd800 v #xdfff))
                parse-simple-unicode))

  (define (massage v)
    (+ #x10000 (logior (ash (- (car v) #xd800) 10) (- (cadr v) #xdc00))))

  (define parse-complex-unicode
    (parse-when*
     parse-out-of-range-unicode
     (parse-lift massage
                 (parse-each
                  parse-simple-unicode
                  parse-simple-unicode))))

  (define %parse-unicode
    (parse-lift
     (lambda (v) (make-pseudo-xchar (integer->char v)))
     (parse-either parse-complex-unicode
                   parse-simple-unicode)))


  (define parse-escaped
    (parse-either (%parse-escaped "\\\"" #\")
                  (%parse-escaped "\\\\" #\\)
                  (%parse-escaped "\\/" #\/)
                  (%parse-escaped "\\b" #\backspace)
                  ;; (%parse-escaped "\\f" #\formfeed) ;; TODO: FIXME
                  (%parse-escaped "\\n" #\newline)
                  (%parse-escaped "\\r" #\return)
                  (%parse-escaped "\\t" #\tab)
                  %parse-unicode
                  ))

  (define parse-string-token
    (parse-lift cadr
                (parse-each (parse-xchar #\")
                            (parse-lift
                             ->string
                             (parse-zero-or-more
                              (parse-unless
                               (lambda (v) (char=? #\" (xchar-char v)))
                               (parse-either parse-escaped
                                             parse-any))))
                            (parse-xchar #\"))))

  (define %parse-integer
    (parse-lift ->string
                (parse-one-or-more
                 (parse-char-set char-set:digit))))

  (define %parse-float
    (parse-lift (lambda (args) (apply string-append args))
                (parse-each
                 %parse-integer
                 (parse-lift (const ".") (parse-xchar #\.))
                 %parse-integer)))

  (define parse-number-token
    (parse-lift string->number
                (parse-either %parse-float
                              %parse-integer)))

  (define parse-number-special-token ;; TODO: give its proper name
    (parse-lift (lambda (x) (string->number (apply string-append x)))
                (parse-each
                 (parse-lift
                  number->string ;; oops!
                  (parse-either parse-number-token
                                parse-negative-number-token))
                 (parse-lift (lambda (v) (list->string (list (xchar-char v))))
                             (parse-either (parse-xchar #\e)
                                           (parse-xchar #\E)))
                 (parse-either %parse-integer
                               (parse-lift (lambda (x) (string-append "-" (cadr x)))
                                           (parse-each (parse-xchar #\-)
                                                       %parse-integer))))))

  (define parse-negative-number-token
    (parse-lift (lambda (x) (- (cadr x)))
                (parse-each (parse-xchar #\-)
                            parse-number-token)))

  (define parse-true-token (parse-lift (const #t) (parse-xstring "true")))
  (define parse-false-token (parse-lift (const #f) (parse-xstring "false")))
  (define parse-null-token (parse-lift (const json-null) (parse-xstring "null")))

  (define (parse-control-token char symbol)
    (parse-lift (const symbol) (parse-xchar char)))

  (define (cleanup lst)
    (filter (lambda (x) (not (null? x))) lst))

  (define tokens
    ;; parse character stream into tokens ignoring whitespace between tokens
    (parse-lift
     cleanup
     (parse-one-or-more
      (parse-either parse-string-token
                    parse-number-special-token
                    parse-number-token
                    parse-negative-number-token
                    parse-true-token
                    parse-false-token
                    parse-null-token
                    (parse-control-token #\, 'comma)
                    (parse-control-token #\: 'colon)
                    (parse-control-token #\{ 'brace-open)
                    (parse-control-token #\} 'brace-close)
                    (parse-control-token #\[ 'bracket-open)
                    (parse-control-token #\] 'bracket-close)
                    (parse-lift (const '()) (parse-char-set char-set:whitespace))))))

  (define parse-json-string (parse-when string? parse-any))

  (define parse-number (parse-when number? parse-any))

  (define (parse-symbol symbol)
    (parse-when (lambda (x) (eq? x symbol)) parse-any))

  (define %parse-object-item
    (parse-lift (lambda (v) (cons (string->symbol (car v)) (caddr v)))
                (parse-each parse-json-string
                            (parse-symbol 'colon)
                            json)))

  (define parse-object
    (parse-either
     (parse-lift (const '(*))
                 (parse-each (parse-symbol 'brace-open)
                             (parse-symbol 'brace-close)))
     (parse-lift (lambda (x) (append '(*) (cadr x) (list (caddr x))))
                 (parse-each
                  (parse-symbol 'brace-open)
                  (parse-zero-or-more (parse-lift car
                                                  (parse-each %parse-object-item
                                                              (parse-symbol 'comma))))
                  %parse-object-item
                  (parse-symbol 'brace-close)))))

  (define parse-array
    (parse-either
     (parse-lift (const '())
                 (parse-each (parse-symbol 'bracket-open)
                             (parse-symbol 'bracket-close)))
     (parse-lift (lambda (x) (append (cadr x) (list (caddr x))))
                 (parse-each
                  (parse-symbol 'bracket-open)
                  (parse-zero-or-more (parse-lift car
                                                  (parse-each json
                                                              (parse-symbol 'comma))))
                  json
                  (parse-symbol 'bracket-close)))))

  (define parse-true (parse-when (lambda (x) (eq? x #t)) parse-any))

  (define parse-false (parse-when (lambda (x) (eq? x #f)) parse-any))

  (define parse-null (parse-when (lambda (x) (json-null? x)) parse-any))

  (define json
     (parse-either parse-json-string
                   parse-number
                   parse-object
                   parse-array
                   parse-true
                   parse-false
                   parse-null
                   ))
  )
