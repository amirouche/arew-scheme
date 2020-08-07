(import (only (chezscheme)
              file-directory?
              directory-list
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
              with-source-path
              system
              make-boot-file
              format
              bytevector->u8-list
              get-bytevector-all
              compile-imported-libraries
              generate-wpo-files
              compile-program
              load-shared-object
              foreign-procedure
              compile-whole-program))

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
;; (import (arew editor))

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
