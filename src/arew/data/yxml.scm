(library (arew data yxml)
  (export yxml-element-name
          yxml-attribute
          yxml-data
          ;; yxml-pi
          ;; yxml-byte
          ;; yxml-total
          ;; yxml-line
          yxml-init
          yxml-parse
          yxml-eof)
  (import (chezscheme))

  ;; helpers

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((define-syntax-rule (keyword args ...) body)
       (define-syntax keyword
         (syntax-rules ()
           ((keyword args ...) body))))))

  (define-syntax-rule (foreign-procedure* return ptr args ...)
    (foreign-procedure ptr (args ...) return))

  (define-syntax-rule (ftype->pointer ftype)
    (ftype-pointer-address ftype))

  ;; bindings

  ;; https://dev.yorhel.nl/yxml/man

  (define libyxml.so (load-shared-object "./local/lib/libyxml.so"))

  (define-ftype %yxml
    (struct
     ;; public
     (element-name void*)
     (data (array 4 unsigned-8))
     (attribute void*)
     (pi void*)
     (byte unsigned-64)
     (total unsigned-64)
     (line unsigned-32)
     ;; private
     (state unsigned-64)
     (stack unsigned-8)
     (stacksize size_t)
     (stacklen size_t)
     (reflen unsigned-64)
     (quote unsigned-64)
     (nextstate unsigned-64)
     (ignore unsigned-64)
     (string void*)))

  (define %yxml-init (foreign-procedure* void* "yxml_init" void* void* size_t))

  (define yxml-init
    (case-lambda
      (() (yxml-init 4096))
      ((buffer-size)
       (let ((buffer (foreign-alloc buffer-size))
             (yxml (make-ftype-pointer %yxml (foreign-alloc (ftype-sizeof %yxml)))))
         (%yxml-init (ftype->pointer yxml) buffer buffer-size)
         yxml))))

  (define %yxml-parse (foreign-procedure* int "yxml_parse" void* int))

  (define (yxml-parse yxml integer)
    (%yxml-parse (ftype->pointer yxml) integer))

  (define %yxml-eof (foreign-procedure* int "yxml_eof" void*))

  (define (yxml-eof yxml)
    (%yxml-eof (ftype->pointer yxml)))

  (define (yxml-element-name yxml)
    ;; look into %yxml->element-name
    (let ((pointer (ftype-ref %yxml (element-name) yxml)))
      (let loop ((index 0)
                 (out '()))
        (let ((value (foreign-ref 'unsigned-8 pointer index)))
          (if (zero? value)
              ;; return a string
              (utf8->string (u8-list->bytevector (reverse out)))
              (loop (+ index 1) (cons value out)))))))

  (define (yxml-attribute yxml)
    ;; look into %yxml->attribute
    (let ((pointer (ftype-ref %yxml (attribute) yxml)))
      (let loop ((index 0)
                 (out '()))
        (let ((value (foreign-ref 'unsigned-8 pointer index)))
          (if (zero? value)
              ;; return a string
              (utf8->string (u8-list->bytevector (reverse out)))
              (loop (+ index 1) (cons value out)))))))

  (define (yxml-data yxml)
    ;; look into %yxml->data
    (let loop ((index 0)
               (out '()))
      (if (= index 4)
          (apply bytevector (reverse out))
          (let ((data (ftype-ref %yxml (data index) yxml)))
            (if (zero? data)
                (apply bytevector (reverse out))
                (loop (+ index 1) (cons data out)))))))
  )
