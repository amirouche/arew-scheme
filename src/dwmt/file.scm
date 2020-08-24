(library (dwmt file)
  (export file-generator file-full-generator file-bytevector
file-open file-read file-close file-size file-generator/mmap)
  (import (chezscheme))

  #!chezscheme
 
  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((define-syntax-rule (keyword args ...) body)
       (define-syntax keyword
         (syntax-rules ()
           ((keyword args ...) body))))))

  (define-syntax-rule (ftype->pointer ftype)
    (ftype-pointer-address ftype))
  
  (define stdlib (load-shared-object #f))

  (define strerror
    (let ((func (foreign-procedure "strerror" (int) string)))
      (lambda (code)
        (func code))))

  (define errno
    (lambda ()
      (let ((entry (foreign-entry "errno")))
        (foreign-ref 'int entry 0))))
  
  (define file-open
    (let ((proc (foreign-procedure "open" (string int) int)))
      (lambda (pathname flag)
        (proc pathname flag))))
  
  (define (bytevector->pointer bv)
    (#%$object-address bv (+ (foreign-sizeof 'void*) 1)))

  (define (call-with-lock obj thunk)
    (lock-object obj)
    (call-with-values thunk
      (lambda out
        (unlock-object obj)
        (apply values out))))

  (define file-read
    (let ((proc (foreign-procedure __collect_safe
                                   "read" (int void* size_t) ssize_t)))
      (lambda (fd bv size)
        (lock-object bv)
        (let ((out (proc fd (bytevector->pointer bv) size)))
          (unlock-object bv)
          out))))

  (define file-close
    (let ((proc (foreign-procedure __collect_safe
                                   "read" (int) int)))
      (lambda (fd)
        (proc fd))))

(define (file-generator filename)
  (define fd (file-open filename 0))
  (define bv (make-bytevector 4096))
  (define count #f)
  (define index #f)
  
  (define (read-bytevector)
    (set! count (file-read fd bv 4096))
    (if (= count 0)
        (begin (file-close fd) (eof-object))
        (begin
          (set! index 0)
          (set! continue yield)
          (bytevector-u8-ref bv 0))))

  (define (yield)
    (set! index (fx+ index 1))
    (if (or (fx=? index 4096) (fx=? index count))
        (read-bytevector)
        (bytevector-u8-ref bv index)))

  (define continue read-bytevector)
  
  (lambda ()
    (continue)))

;; sizeof(dev_t) == 8
;; sizeof(ino_t) == 8
;; sizeof(mode_t) == 4
;; sizeof(nlink_t) == 8
;; sizeof(uid_t) == 4
;; sizeof(gid_t) == 4
;; sizeof(dev_t) == 8
;; sizeof(off_t) == 8
;; sizeof(blksize_t) == 8
;; sizeof(blkcnt_t) == 8
;; sizeof(struct timespec) == 16

(define-ftype %stat
  (struct
   (dev long)
   (ino long)
   (mode int)
   (nlink long)
   (uid int)
   (gid int)
   (rdev long)
   (off long)
   (blksize long)
   (blkcnt long)
   (atim (array 2 long))
   (mtim (array 2 long))
   (ctim (array 2 long))))

(define (make-statbuf)
  (make-ftype-pointer %stat (foreign-alloc (ftype-sizeof %stat))))

(define file-size
  (let ((proc (foreign-procedure __collect_safe
                                 "fstat" (int void*) int)))
    (lambda (fd)
      (let* ((out (make-statbuf))
             (code (proc fd (ftype-pointer-address out))))
        (unless (zero? code)
          (error "fstat failed" code))
        (let ((size (ftype-ref %stat (off) out)))
          (foreign-free (ftype-pointer-address out))
          size)))))

(define MMAP-FAILED #xffffffffffffffff)

(define MMAP-FLAGS (logior #x02 #x8000))

(define file-mmap
  (let ((proc (foreign-procedure __collect_safe
                                 "mmap"
                                 (void* size_t int int int long)
                                 void*)))
    (lambda (fd size)
      (let ((out (proc 0 size 1 MMAP-FLAGS fd 0)))
        (if (= out MMAP-FAILED)
            (error 'mmap "mmap failed")
            out)))))

(define file-munmap
  (let ((proc (foreign-procedure __collect_safe
                                 "munmap" (void* size_t) int)))
    (lambda (address size)
      (proc address size))))

(define (file-generator/mmap filename)
  (define fd (file-open filename 0))
  (define size (file-size fd))
  (define address (file-mmap fd size))
  (define index 0)
  
  (define (continue)
    (if (fx=? index size)
        (begin (file-munmap address size)
               (eof-object))
        (let ((byte (foreign-ref 'unsigned-8 address index)))
          (set! index (fx+ index 1))
          byte)))

  continue)

(define (file-full-generator filename)
  (define fd (file-open filename 0))
  (define size (file-size fd))
  (define bv (make-bytevector size))
  (define index -1)
  
  (define (yield)
    (set! index (fx+ index 1))
    (if (fx=? index size)
        (eof-object)
        (bytevector-u8-ref bv index)))

  (time (file-read fd bv size))
  (file-close fd)
  
  yield)
             
(define (file-bytevector filename)
  (define fd (file-open filename 0))
  (define size (file-size fd))
  (define bv (make-bytevector size))
  (define index -1)
  
  (file-read fd bv size)
  
  bv))
  
           

