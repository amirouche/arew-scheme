
(define scheme2js
  '((+ . "add")
    (- . "minus")
    (* . "times")
    (void . "voidp")))

(define (symbol->string* s)
  (if (assoc s scheme2js)
      (cdr (assoc s scheme2js))
      (let ((s (symbol->string s)))
        (list->string
         (map (lambda (char)
                (case char
                  ((#\* #\. #\- #\? #\!) #\x)
                  (else char)))
              (string->list s))))))

(define (string-join strings delimiter)
  (if (null? strings)
      ""
      (fold (lambda (s so-far) (string-append so-far delimiter s))
            (car strings)
            (cdr strings))))

(define (javascripter sexp)
  (match sexp
    ((? boolean? b) (if b "true" "false"))
    ((? symbol? s) (symbol->string* s))
    ((? number? n) (number->string n))
    ((? string? s) (string-append "\"" s "\""))

    (('begin magic ...) (string-join (map javascripter magic) ";\n"))

    (('set! target value) (string-append (symbol->string* target)
                                         " = "
                                         (javascripter value)))

    (('define target) (string-append "let " (symbol->string* target)))

    (('define target value) (string-append "let "
                                           (symbol->string* target)
                                           " = " (javascripter value)))
    (('values e e e* ...) (string-append "return [" (string-join (map javascripter (cons e e*)) ", ") "]"))
    (('values e* ...) (string-append "return [" (string-join (map javascripter e*) ", ") "]"))
    (('lambda (args ...) ('begin exprs ... last))
     (string-append "(function("
                    (string-join (map javascripter args) ", ")
                    ") {\n"
                    (string-join (map javascripter exprs) ";\n")
                    ";\n"
                    (javascripter last)
                    ";\n})"))

    (('lambda (args ...) expr)
     (string-append "(function("
                    (string-join (map javascripter args) ", ")
                    ") { "
                    (javascripter expr)
                    "; })"))

    (('lambda (args ...) exprs ... last)
     (string-append "(function("
                    (string-join (map javascripter args) ", ")
                    ") {"
                    (string-join (map javascripter exprs) ";")
                    ";"
                    " "
                    (javascripter last)
                    ";})"))

    (('if a b c) (string-append "if ("
                                (javascripter a)
                                ") { "
                                (javascripter b)
                                " } else { "
                                (javascripter c)
                                "}"))

    (('primcall proc args ...)
     (javascripter (cons proc args)))

    ((proc args ...)
     (string-append (javascripter proc)
                    "("
                    (string-join (map javascripter args) ", ")
                    ")"))))

(define (nodejs string)
  (call-with-values (lambda () (open-process-ports "nodejs -p -"
                                                   'block
                                                   (current-transcoder)))
    (lambda (stdin stdout stderr id)
      (display string stdin)
      (close-port stdin)
      (string-append (read-string stdout) (read-string stderr)))))
