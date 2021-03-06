;*****************************************************************
; read-NCName-ss
; A version of SSAX:read-NCName that uses next-token-of-as-symb
;
; $Id: read-NCName-ss.scm,v 1.4 2003/04/29 01:40:35 oleg Exp $


(directives
	; Adding substring->symbol implemented in substring-symbol.c
	(extern
	 (macro substring->symbol::symbol (::bstring ::int ::int)    
				    "substring_to_symbol")
	 (include "substring-symbol.c"))
	(pragma
	 (substring->symbol no-cfa-top nesting))
)

; The same as next-token-of with the exception that it returns its result
; as a symbol
(define (next-token-of-as-symb incl-list/pred port)
  (let* ((buffer (input-parse:init-buffer))
	 (curr-buf-len (string-length buffer)))
  (if (procedure? incl-list/pred)
    (let outer ((buffer buffer) (filled-buffer-l '()))
      (let loop ((i 0))
	(if (>= i curr-buf-len)		; make sure we have space
	  (outer (make-string curr-buf-len) (cons buffer filled-buffer-l))
	  (let ((c (incl-list/pred (peek-char port))))
	    (if c
	      (begin
		(string-set! buffer i c)
		(read-char port)			; move to the next char
		(loop (++ i)))
	      ; incl-list/pred decided it had had enough
	      (if (null? filled-buffer-l) (substring->symbol buffer 0 i)
		(string->symbol
		  (string-concatenate-reverse filled-buffer-l buffer i))))))))

    ; incl-list/pred is a list of allowed characters
    (let outer ((buffer buffer) (filled-buffer-l '()))
      (let loop ((i 0))
	(if (>= i curr-buf-len)		; make sure we have space
	  (outer (make-string curr-buf-len) (cons buffer filled-buffer-l))
	  (let ((c (peek-char port)))
	    (cond
	      ((not (memv c incl-list/pred))
		(if (null? filled-buffer-l) (substring->symbol buffer 0 i)
		  (string->symbol 
		    (string-concatenate-reverse filled-buffer-l buffer i))))
	      (else
		(string-set! buffer i c)
		(read-char port)			; move to the next char
		(loop (++ i))))))))
    )))

; A version of SSAX:read-NCName that uses next-token-of-as-symb

; Read a NCName starting from the current position in the PORT and
; return it as a symbol.
(define (ssax:read-NCName port)
  (let ((first-char (peek-char port)))
    (or (ssax:ncname-starting-char? first-char)
      (parser-error port "XMLNS [4] for '" first-char "'")))
    (next-token-of-as-symb
      (lambda (c)
        (cond
          ((eof-object? c) #f)
          ((char-alphabetic? c) c)
          ((string-index "0123456789.-_" c) c)
          (else #f)))
      port))


