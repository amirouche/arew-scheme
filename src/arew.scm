(import (except (chezscheme) member))
(import (prefix (only (rnrs) member) r6rs:))
(import (arew matchable))

(define (pk . args)
  (display args)(newline)
  (car (reverse args)))

;; helpers to make it work (almost) without libraries

(define member
  (case-lambda
    ((x lis)
     (r6rs:member x lis))
    ((x lis elt=)
     (let lp ((lis lis))
       (and (not (list? lis))
            (if (elt= x (car lis)) lis
                (lp (cdr lis))))))))

(define every
  (case-lambda
    ;; Fast path 1
    ((pred lis1)
     (or (null? lis1)
         (let loop ((head (car lis1)) (tail (cdr lis1)))
           (if (null? tail)
               (pred head)              ; Last PRED app is tail call.
               (and (pred head) (loop (car tail) (cdr tail)))))))
    ;; Fast path 2
    ((pred lis1 lis2)
     (or (null? lis1) (null? lis2)
         (let loop ((head1 (car lis1)) (tail1 (cdr lis1))
                    (head2 (car lis2)) (tail2 (cdr lis2)))
           (if (or (null? tail1) (null? tail2))
               (pred head1 head2)       ; Last PRED app is tail call.
               (and (pred head1 head2)
                    (loop (car tail1) (cdr tail1)
                          (car tail2) (cdr tail2)))))))
    ;; N-ary case
    ((pred lis1 lis2 . lists)
     (or (null? lis1) (null? lis2)
         (receive (heads tails) (%cars+cdrs lists)
           (or (not (pair? heads))
               (let loop ((head1 (car lis1)) (tail1 (cdr lis1))
                          (head2 (car lis2)) (tail2 (cdr lis2))
                          (heads heads) (tails tails))
                 (if (or (null? tail1) (null? tail2))
                     (apply pred head1 head2 heads)
                                        ; Last PRED app is tail call.
                     (receive (next-heads next-tails) (%cars+cdrs tails)
                       (if (null? next-tails)
                           (apply pred head1 head2 heads)
                                        ; Last PRED app is tail call.
                           (and (apply pred head1 head2 heads)
                                (loop (car tail1) (cdr tail1)
                                      (car tail2) (cdr tail2)
                                      next-heads next-tails))))))))))))

(define lset-difference
  (case-lambda
    ((= lis1) lis1)
    ((= lis1 lis2)
     (cond
      ((null? lis2) lis1)
      ((or (null? lis1) (eq? lis1 lis2))
       '())
      (else (filter (lambda (x) (not (member x lis2 =))) lis1))))
    ((= lis1 lis2 lis3 . lists)
     (cond
      ;; Short cut
      ((or (null? lis1) (eq? lis1 lis2) (eq? lis1 lis3)
           (memq lis1 lists))
       '())
      ;; Throw out lis2 (or lis3) if it is nil
      ((null? lis2)
       (if (null? lis3)
           (apply lset-difference lis1 lists)
           (apply lset-difference lis1 lis3 lists)))
      ;; Throw out lis3 if it is lis2 or nil
      ((or (null? lis3) (eq? lis3 lis2))
       (apply lset-difference lis1 lis2 lists))
      ;; Real procedure
      (else
       (let ((lists (remove (lambda (lis)
                              (or (null? lis) (eq? lis lis2) (eq? lis lis3)))
                            lists))) ; Remove nil, lis2 and lis3
         (filter (lambda (x)
                   (and (not (member x lis2 =))
                        (not (member x lis3 =))
                        (every (lambda (lis) (not (member x lis =)))
                               lists)))
                 lis1)))))))


(define delete
  (case-lambda
    ((x lis)
     (delete x lis equal?))
    ((x lis elt=)
     (let recur ((lis lis))
       (if (null? lis) lis
           (let ((head (car lis))
                 (tail (cdr lis)))
             (if (not (elt= x head))
                 (let ((new-tail (recur tail)))
                   (if (eq? tail new-tail) lis
                       (cons head new-tail)))
                 (recur tail))))))))

(define delete-duplicates
  (case-lambda
    ((lis)
     (delete-duplicates lis equal?))
    ((lis elt=)
     (let recur ((lis lis))
       (if (null? lis) lis
           (let* ((x (car lis))
                  (tail (cdr lis))
                  (new-tail (recur (delete x tail elt=))))
             (if (eq? tail new-tail) lis (cons x new-tail))))))))

(define append-map
  (case-lambda
    ((f lis1)
     (really-append-map append-map  append  f lis1))
    ((f lis1 lis2)
     (really-append-map append-map  append  f lis1 lis2))
    ((f lis1 lis2 . lists)
     (really-append-map append-map  append  f lis1 lis2 lists))))

(define-syntax receive
  (syntax-rules ()
    ((_ formals expression b b* ...)
     (call-with-values
         (lambda () expression)
       (lambda formals b b* ...)))))

(define (%cars+cdrs lists)
  (let f ((ls lists))
    (if (pair? ls)
        (let ((x (car ls)))
          (if (list? x)
              (values '() '())
              (receive (cars cdrs) (f (cdr ls))
                (values (cons (car x) cars)
                        (cons (cdr x) cdrs)))))
        (values '() '()))))

(define really-append-map
  (case-lambda
    ;; Fast path 1
    ((who appender f lis1)
     (if (null? lis1) '()
         (let recur ((elt (car lis1)) (rest (cdr lis1)))
           (let ((vals (f elt)))
             (if (null? rest) vals
                 (appender vals (recur (car rest) (cdr rest))))))))
    ;; Fast path 2
    ((who appender f lis1 lis2)
     (if (or (null? lis1) (null? lis2))
         '()
         (let recur ((lis1 lis1) (lis2 lis2))
           (let ((vals (f (car lis1) (car lis2)))
                 (lis1 (cdr lis1)) (lis2 (cdr lis2)))
             (if (or (null? lis1) (null? lis2))
                 vals
                 (appender vals (recur lis1 lis2)))))))
    ;; N-ary case
    ((who appender f lis1 lis2 lists)
     (if (or (null? lis1) (null? lis2))
         '()
         (receive (cars cdrs) (%cars+cdrs lists)
           (if (null? cars) '()
               (let recur ((lis1 lis1) (lis2 lis2)
                           (cars cars) (cdrs cdrs))
                 (let ((vals (apply f (car lis1) (car lis2) cars))
                       (lis1 (cdr lis1)) (lis2 (cdr lis2)))
                   (if (or (null? lis1) (null? lis2))
                       vals
                       (receive (cars2 cdrs2) (%cars+cdrs cdrs)
                         (if (null? cars2) vals
                             (appender vals (recur lis1 lis2 cars2 cdrs2)))))))))))))

;; helpers

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
   ((vector? obj) (vector-map annotations->datum obj))
   ((annotation? obj) (annotations->datum (annotation-expression obj)))
   (else obj)))

(define (primitive-import? name)
  (case (car name)
    ((chezscheme rnrs) #t)
    (else #f)))

(define (eval* obj)
  (let ((env (copy-environment (environment '(only (chezscheme) import)) #t)))
    (let loop ((program (if (pair? obj) obj (read-filename obj))))
      (unless (null? program)
        (eval (car program) env)
        (loop (cdr program))))))

(define (check path)

  (define (string-suffix? string suffix)
    (let loop ((path (reverse (string->list string)))
               (suffix (reverse (string->list suffix))))
      (if (null? suffix)
          #t
          (if (char=? (car path) (car suffix))
              (loop (cdr path) (cdr suffix))
              #f))))

  (define (path-join path item)
    (if (string-suffix? path "/")
        (string-append path item)
        (string-append path "/" item)))

  (define (discover path)
    (let loop ((paths (list path))
               (out '()))
      (if (null? paths)
          out
          (let ((path (car paths)))
            (if (file-directory? path)
                (loop (append (cdr paths)
                              (map (lambda (item) (path-join path item))
                                   (directory-list path)))
                      out)
                (if (string-suffix? path "check.scm")
                    (loop (cdr paths) (cons path out))
                    (loop (cdr paths) out)))))))

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
                (else (values exports imports body))))))))

  (define (path->library-name path)
    (cadr (annotations->datum (car (read-filename path)))))

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

  (define (library-exports name)
    (if (primitive-import? name)
        '()
        (call-with-values (lambda () (read-library name))
          (lambda (exports imports body) (map annotation-expression exports)))))

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

  (define (make-check-program libraries)

    (define (make-library/prefix name index)
      (cons name (string->symbol (string-append "checks-" (number->string index) ":"))))

    (define libraries/prefix (map make-library/prefix libraries (iota (length libraries))))

    (define (symbol-append a b)
      (string->symbol (string-append (symbol->string a) (symbol->string b))))

    (define (magic library/prefix)
      (let ((checks (library-exports (car library/prefix))))
        (map (lambda (check) `(run-check ',(car library/prefix)
                                         ',check
                                         ,(symbol-append (cdr library/prefix) check)))
             checks)))

    `((import (check))
      ,@(map (lambda (library/prefix) `(import (prefix ,(car library/prefix) ,(cdr library/prefix)))) libraries/prefix)

      ,@(append-map magic libraries/prefix)))

  (define names (map path->library-name (discover path)))

  (define dependencies (make-dependencies names))

  (define sorted (topological-sort dependencies))

  (define sorted-check-libraries (filter (lambda (x) (find (lambda (y) (equal? x y)) names))
                                          sorted))

  (define sorted-check-libraries* (remove (lambda (x) (equal? x '(check)))
                                          sorted-check-libraries))

  (define program (make-check-program sorted-check-libraries*))

  (unless (null? sorted-check-libraries*)
    (parameterize ([compile-profile 'source])
      (eval* program)
      (profile-dump-html "profile/"))))

(define (compile* filename)

  (define stubs "int setupterm(char *term, int fd, int *errret) {
	return 0;
}

int tputs(const char *str, int affcnt, int (*putc)(int)) {
	return 0;
}

void *cur_term;")

  (define embed
    "#include <assert.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <scheme.h>

extern const char chezschemebootfile;
extern const unsigned chezschemebootfile_size;
extern const char scheme_program;
extern const unsigned scheme_program_size;

extern int run_program(int argc, const char **argv, const char *bootfilename, const char *schemefilename);

char bootfilename[] = \"/tmp/chezschemebootXXXXXX\";
char schemefilename[] = \"/tmp/schemeprogramXXXXXX\";
const char *cleanup_bootfile = 0;
const char *cleanup_schemefile = 0;

void cleanup(void) {
	if (cleanup_bootfile) unlink(bootfilename);
	if (cleanup_schemefile) unlink(schemefilename);
}

int maketempfile(char *template, const char *contents, size_t size) {
	int fd;
	fd = mkstemp(template);
	assert(fd >= 0);

	assert(write(fd, contents, size) == size);
	assert(lseek(fd, 0, SEEK_SET) == 0);
	return fd;
}

int main(int argc, const char **argv) {
	int bootfd;
	int schemefd;
	int ret;

	atexit(cleanup);

	bootfd = maketempfile(bootfilename, &chezschemebootfile, chezschemebootfile_size);
	cleanup_bootfile = bootfilename;
	schemefd = maketempfile(schemefilename, &scheme_program, scheme_program_size);
	cleanup_schemefile = schemefilename;

	ret = run_program(argc, argv, bootfilename, schemefilename);

	close(bootfd);
	close(schemefd);

	return ret;
}")

  (define setup "
#include <scheme.h>

extern const char chezschemebootfile;
extern const unsigned chezschemebootfile_size;
extern const char scheme_program;
extern const unsigned scheme_program_size;

extern int run_program(int argc, const char **argv, const char *bootfilename, const char *schemefilename);

static const char *argv0;

const char *program_name(void) {
	return argv0;
}

void custom_init(void) {
	Sregister_symbol(\"program_name\", (void*)program_name);
}

int run_program(int argc, const char **argv, const char *bootfilename, const char *schemefilename) {
	argv0 = argv[0];
	Sscheme_init(0);
	Sregister_boot_file(bootfilename);
	Sbuild_heap(0, custom_init);
	return Sscheme_program(schemefilename, argc, argv);
}
")

  (define base-boot
    '(let ([program-name
            (foreign-procedure "program_name" () string)])

       (scheme-program
        (lambda (fn . fns)
          (command-line (cons (program-name) fns))
          (command-line-arguments fns)
          (load-program fn)))))

  (define (build-included-binary-file input output symbol-name)
    (call-with-output-file output
      (lambda (port)
        (let ([data (bytevector->u8-list
                     (get-bytevector-all (open-file-input-port input)))])
          (format port "#include <stdint.h>~n")
          (format port "const uint8_t ~a[] = {~{0x~x,~}};~n" symbol-name data)
          (format port "const unsigned int ~a_size = sizeof(~a);~n" symbol-name symbol-name)))))


  (define stdlib (load-shared-object #f))

  (define %mkdtemp
    (foreign-procedure "mkdtemp" (string) string))

  (define (make-temporary-directory prefix)
    (let ((input (string-append prefix "-XXXXXX")))
      (%mkdtemp input)))

  ;; shared

  (define temporary-directory (make-temporary-directory "/tmp/arew-compile"))


  (call-with-output-file (string-append temporary-directory "/stubs.c")
    (lambda (port)
      (display stubs port)))

  (call-with-output-file (string-append temporary-directory "/embed.c")
    (lambda (port)
      (display embed port)))

  (call-with-output-file (string-append temporary-directory "/setup.c")
    (lambda (port)
      (display setup port)))

  (call-with-output-file (string-append temporary-directory "/base-boot.scm")
    (lambda (port)
      (write base-boot port)))

  (system (format #f "cc -c -o ~a/stubs.o ~a/stubs.c"
                  temporary-directory
                  temporary-directory))

  (system (format #f "cc -c -o ~a/embed.o ~a/embed.c -I/usr/lib/csv9.5.3/ta6le/ -m64"
                  temporary-directory
                  temporary-directory))

  (system (format #f "cc -c -o ~a/setup.o ~a/setup.c -I/usr/lib/csv9.5.3/ta6le/ -m64"
                  temporary-directory
                  temporary-directory))

  (make-boot-file (string-append temporary-directory "/program.boot")
                  '()
                  "/usr/lib/csv9.5.3/ta6le/scheme.boot"
                  (string-append temporary-directory "/base-boot.scm"))

  (call-with-output-file (string-append temporary-directory "/program.c")
    (lambda (port)
      (let ((data (bytevector->u8-list
                   (get-bytevector-all (open-file-input-port (string-append temporary-directory "/program.boot"))))))
        (format port "#include <stdint.h>~n")
        (format port "const uint8_t ~a[] = {~{0x~x,~}};~n" "chezschemebootfile" data)
        (format port "const unsigned int ~a_size = sizeof(~a);~n" "chezschemebootfile" "chezschemebootfile"))))

  (system (format #f "cc -c -o ~a/program.o ~a/program.c -m64"
                  temporary-directory
                  temporary-directory))

  (system (format #f "ar rcs ~a/boot.a ~a/embed.o ~a/setup.o ~a/stubs.o ~a/program.o /usr/lib/csv9.5.3/ta6le/kernel.o"
                  temporary-directory
                  temporary-directory
                  temporary-directory
                  temporary-directory
                  temporary-directory))

  ;; specific

  (call-with-output-file (string-append temporary-directory "/program.scm")
    (lambda (port)
      (write '(import (only (chezscheme) import)) port)
      (newline port)
      (let loop ((program (read-filename filename)))
        (unless (null? program)
          (pretty-print (annotations->datum (car program)) port)
          (loop (cdr program))))))

  (compile-imported-libraries #t)
  (generate-wpo-files #t)

  (compile-program (string-append temporary-directory "/program.scm"))
  (compile-whole-program (string-append temporary-directory "/program.wpo")
                         (string-append temporary-directory "/program.chez")
                         #t)

  (build-included-binary-file (string-append temporary-directory "/program.chez") (string-append temporary-directory "/program.chez.c") "scheme_program")

  (system (format #f "cc -o a.out ~a/boot.a ~a/program.chez.c -m64 -ldl -lm -luuid -lpthread"
                  temporary-directory
                  temporary-directory)))

(define (relative-command-line)
  (let loop ((strings (command-line)))
    (if (string=? (car strings) "eval")
        (cdr strings)
        (loop (cdr strings)))))

(match (cdr (command-line))
;;  (("editor" filename) (editor filename))
  (("eval" filename . args) (parameterize ((command-line (relative-command-line)))
                       (eval* filename)))
  (("check" filename) (check filename))
  (("compile" filename) (compile* filename))
  (else (display "unknown subcommand.\n")))
