(import (only (chezscheme)
              compile-profile
              profile-dump-html
              expand
              annotation?
              pretty-print
              import
              copy-environment
              environment
              eval
              make-source-file-descriptor
              open-source-file
              get-datum/annotations
              open-file-input-port
              annotation-expression
              source-directories
              with-source-path))
(import (scheme base))
(import (scheme list))
(import (scheme file))
(import (scheme read))
(import (scheme process-context))
(import (scheme write))
(import (scheme hash-table))
(import (scheme comparator))
(import (srfi srfi-145))
(import (arew matchable))

;; helpers

(define (topological-sort dependencies)

  (define (remove-self-dependency pair)
    (let ((key (car pair))
          (value (cdr pair)))
      (cons key (delete key value equal?))))

  (define (remove-self-dependencies alist)
    (map remove-self-dependency alist))

  (define (add-missing-items dependencies)
    (let loop ((items (delete-duplicates (append-map cdr dependencies) equal?))
               (out dependencies))
      (if (null? items)
          out
          (let ((item (car items)))
            (if (assoc item out)
                (loop (cdr items) out)
                (loop (cdr items) (cons (cons item '()) out)))))))

  (define (lift dependencies batch)
    (let loop ((dependencies dependencies)
               (out '()))
      (if (null? dependencies)
          out
          (let ((key (caar dependencies))
                (value (cdar dependencies)))
            (if (null? value)
                (loop (cdr dependencies) out)
                (loop (cdr dependencies)
                      (cons (cons key (lset-difference equal? value batch))
                            out)))))))


  (let* ((dependencies (remove-self-dependencies dependencies))
         (dependencies (add-missing-items dependencies)))
    (let loop ((out '())
               (dependencies dependencies))
      (if (null? dependencies)
          (reverse (apply append out))
          (let ((batch (map car (filter (lambda (pair) (null? (cdr pair))) dependencies))))
            (if (null? batch)
                #f
                (loop (cons batch out) (lift dependencies batch))))))))

(define string-join
  (lambda (str* jstr)
    (cond
     [(null? str*) ""]
     [(null? (cdr str*)) (car str*)]
     [else (string-append (car str*) jstr (string-join (cdr str*) jstr))])))


(define %arew-path
  (get-environment-variable "AREW_PATH"))

(source-directories (list %arew-path "."))

(define find-file
  (lambda (fn)
    (with-source-path 'find-file fn values)))

(define %arew-library-extensions
  '(".arew.sld" ".arew.sls" ".arew.scm"
    ".chezscheme.sld" ".chezscheme.sls" ".chezscheme.scm" ".chezscheme.ss"
    ".sld" ".sls" ".scm" ".ss"))

(define (annotations->datum obj)
  (cond
   ((pair? obj) (cons (annotations->datum (car obj))
                      (annotations->datum (cdr obj))))
   ((annotation? obj) (annotations->datum (annotation-expression obj)))
   (else obj)))

(define (pack filename)

  (define (and=> v proc)
    (if v (proc v) #f))

  (define unique-var
    (let ((count 0))
      (lambda (name)
        (let ([count* count])
          (set! count (+ count 1))
          (string->symbol
           (string-append (symbol->string name) "." (number->string count*)))))))

  (define (memoize proc)
    (let ((memory '()))
      (lambda (name)
        (or (and=> (assoc name memory) cdr)
            (let ((new-name (proc name)))
              (set! memory (cons (cons name new-name)
                                 memory))
              new-name)))))

  (define rename
    (memoize
     (lambda (name)
       (case (car name)
         ((chezscheme rnrs) name)
         (else (list (unique-var 'library)))))))

  (define (import-rename spec)
    (let ((spec (annotation-expression spec)))
      (case (annotation-expression (car spec))
        ((prefix rename for except only)
         (cons* (car spec) (import-rename (cadr spec)) (cddr spec)))
        (else (rename (map annotation-expression spec))))))

  (define (read-filename filename)
    (define port (open-file-input-port filename))
    (define sfd (make-source-file-descriptor filename port))
    (define source (open-source-file sfd))
    (let loop ((out '()))
      (call-with-values (lambda () (get-datum/annotations source sfd 0))
        (lambda (object _)
          (if (eof-object? object)
              (reverse out)
              (loop (cons object out)))))))

  (define (library-filepath name)
    (let ((name* (string-join (map symbol->string name) "/")))
      (let loop ((extensions %arew-library-extensions))
        (if (null? extensions)
            (error "Library not found" name)
            (guard (ex (else (loop (cdr extensions))))
              (find-file (string-append name* (car extensions))))))))

  (define (primitive-import? name)
    (case (car name)
      ((chezscheme rnrs) #t)
      (else #f)))

  (define (read-program filename)
    (let loop ((imports '())
               (sexp (read-filename filename)))
      (if (null? sexp)
          (values '() imports)
          (let ((head (car sexp)))
            (if (and (pair? (annotation-expression head))
                     (eq? (annotation-expression (car (annotation-expression head)))
                          'import))
                (loop (append (cdr (annotation-expression head)) imports) (cdr sexp))
                (values (reverse imports) sexp))))))

  (define (read-library name)
    (let ((sexp (car (read-filename (library-filepath name)))))
      (let loop ((body (cddr (annotation-expression sexp)))
                 (exports '())
                 (imports '()))
        (if (null? body)
            (values (reverse exports) (reverse imports) '())
            (let ((head (annotation-expression (car body))))
              (case (annotation-expression (car head))
                ((export) (loop (cdr body) (append (cdr head) exports) imports))
                ((import) (loop (cdr body) exports (append (cdr head) imports)))
                (else (values (reverse exports) (reverse imports) body))))))))

  (define (import->name spec)
    (let ((spec (annotations->datum spec)))
      (case (car spec)
        ((rename only prefix except for) (import->name (cadr spec)))
        (else spec))))

  (define (library-imports name)
    (if (primitive-import? name)
        '()
        (call-with-values (lambda () (read-library name))
          (lambda (exports imports body)
            (map import->name imports)))))

  (define (make-dependencies imports)
    (let loop ((imports imports)
               (out '()))
      (if (null? imports)
          out
          (if (assoc (car imports) out)
              (loop (cdr imports) out)
              (let ((imports* (library-imports (car imports))))
                (loop (append (cdr imports) imports*)
                      (cons (cons (car imports) imports*)
                            out)))))))

  (define (libraries-pack name)
    (call-with-values (lambda () (read-library name))
      (lambda (exports imports body)
        `($library ,(rename name)
                   (export ,@exports)
                   (import ,@(map import-rename imports))
                   ,@body))))

  (call-with-values (lambda () (read-program filename))
    (lambda (imports body)
      (when (null? imports)
        (error "No imports" filename))
      (when (null? body)
        (error "No expression" filename))

      (let* ((imports* (map import->name imports))
             (dependencies (make-dependencies imports*))
             (libraries (remove primitive-import? (topological-sort dependencies))))

        `($begin
          ,@(map libraries-pack libraries)
          ($import ,@(map import-rename imports))
          ,@body)))))

(define (print filename)
  (pretty-print (annotations->datum (pack filename))))

(define (eval* filename)
  (let ((env (copy-environment (environment '(prefix (arew r7rs) $)))))
    (eval (pack filename) env)))

(define (expand* filename)
  (let ((env (copy-environment (environment '(prefix (arew r7rs) $)))))
    (pretty-print (expand (pack filename) env))))

(define (test filename)

  (define (read-filename filename)
    (define port (open-file-input-port filename))
    (define sfd (make-source-file-descriptor filename port))
    (define source (open-source-file sfd))
    (let loop ((out '()))
      (call-with-values (lambda () (get-datum/annotations source sfd 0))
        (lambda (object _)
          (if (eof-object? object)
              (reverse out)
              (loop (cons object out)))))))

  (define (library-filepath name)
    (let ((name* (string-join (map symbol->string name) "/")))
      (let loop ((extensions %arew-library-extensions))
        (if (null? extensions)
            (error "Library not found" name)
            (guard (ex (else (loop (cdr extensions))))
              (find-file (string-append name* (car extensions))))))))

  (define (read-library name)
    (let ((sexp (car (read-filename (library-filepath name)))))
      (let loop ((body (cddr (annotation-expression sexp)))
                 (exports '())
                 (imports '()))
        (if (null? body)
            (values (reverse exports) (reverse imports) '())
            (let ((head (annotation-expression (car body))))
              (case (annotation-expression (car head))
                ((export) (loop (cdr body) (append (cdr head) exports) imports))
                ((import) (loop (cdr body) exports (append (cdr head) imports)))
                (else (values (reverse exports) (reverse imports) body))))))))

  (define (make-program library test)
    `((import ,library) (,test)))

  (define (run-one library test)
    ;; Create temporary program
    (when (file-exists? "test.scm")
      (delete-file "test.scm"))
    (let ((program (make-program library test)))
      (call-with-output-file "test.scm"
        (lambda (port) (for-each (lambda (sexp) (write sexp port)) program))))
    ;; execute the program
    (eval* "test.scm"))

  (define (format-output library out tests)
    (let ((error? #f))
      (let ((out (map cons out tests)))
        (let loop ((out out))
          (unless (null? out)
            (unless (caaar out)
              (display library)
              (display " ")
              (display (cdar out))
              (display ": failed")
              (newline)
              (set! error? #t)))))))

  (let* ((name (cadr (annotations->datum (car (read-filename filename))))))
    (call-with-values (lambda () (read-library name))
      (lambda (exports _0 _1)
        (let ((exports (annotations->datum exports)))
          (parameterize ([compile-profile 'source])
            (let ((error? (format-output exports
                                         (map-in-order (lambda (test) (run-one name test))
                                                       exports)
                                         exports)))
              (profile-dump-html "profile/")
              (exit (if error? 1 0)))))))))

(match (cdr (command-line))
  (("eval" filename) (eval* filename))
  (("expand" filename) (expand* filename))
  (("print" filename) (print filename))
  (("test" filename) (test filename))
  (else (display "unknown subcommand.\n")))
