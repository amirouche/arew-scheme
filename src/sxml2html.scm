(import (only (chezscheme) import))
(import (scheme base))
(import (scheme case-lambda))
(import (scheme file))
(import (scheme read))
(import (scheme cxr))
(import (scheme write))


(define (html-char char)
  (case char
    ((#\") "&quot;")
    ((#\&) "&amp;")
    ((#\') "&apos;")
    ((#\<) "&lt;")
    ((#\>) "&gt;")
    (else (list->string (list char)))))

(define (html-write-element sexp port)
  (case (car sexp)
    ((area
      base
      br
      col
      embed
      hr
      img
      input
      link
      meta
      param
      source
      track
      br)
     (if (and (not (null? (cdr sexp))) (pair? (cadr sexp)) (eq? (caadr sexp) '*))
         (begin
           (write-string (string-append "<" (symbol->string (car sexp))) port)
           (html-write-attributes (cadr sexp) port)
           (display "/>" port))
         (write-string (string-append "<" (symbol->string (car sexp)) "/>") port)))
    (else
     (if (and (pair? (cadr sexp)) (eq? (caadr sexp) '*))
         (begin
           (write-string (string-append "<" (symbol->string (car sexp))) port)
           (html-write-attributes (cadr sexp) port)
           (display ">" port)
           (for-each (lambda (sexp) (%%html-write sexp port)) (cddr sexp)))
         (begin
           (write-string (string-append "<" (symbol->string (car sexp)) ">") port)
           (for-each (lambda (sexp) (%%html-write sexp port)) (cdr sexp))))
     (write-string (string-append "</" (symbol->string (car sexp)) ">") port))))


(define (html-write-string string port)
  (write-string (apply string-append (map html-char (string->list string))) port))

(define (html-write-attribute pair port)
  (write-string
   (string-append " "
                  (symbol->string (car pair))
                  "=\""
                  (cadr pair)
                  "\"")))

(define (html-write-attributes sexp port)
  (for-each (lambda (pair) (html-write-attribute pair port)) (cdr sexp)))

(define (%%html-write sexp port)
  (cond
   ((string? sexp) (html-write-string sexp port))
   ((and (pair? sexp) (symbol? (car sexp)))
    (html-write-element sexp port))
   (else (error "object not supported" sexp))))

(define (%html-write sexp port)
  (write-string "<!DOCTYPE html>" port)
  (%%html-write sexp port))

(define html-write
  (case-lambda
   ((sexp) (html-write sexp (current-output-port)))
   ((sexp port) (%html-write sexp port))))

(call-with-input-file "index.scm"
  (lambda (port)
    (html-write (read port))))
