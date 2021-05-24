(import (scheme base))
(import (scheme fixnum))
(import (scheme time))

(define (time)
  (let ((now (current-time 'time-monotonic)))
    (fx+ (fx* (time-second now) (expt 10 9))
         (time-nanosecond now))))

(define (print out delta)
  (format #t "out: ~a nanoseconds: ~a\n" out delta))

(define (fibonacci a b n)
  (if (fxzero? n)
      a
      (fibonacci b (fx+ a b) (fx- n 1))))

(define (generate-positive-integers-from n)
  ;; it will generate all integers from `n` to `0` both are excluded.
  (lambda ()
    (if (fxzero? n)
        #f
        (begin
          (set! n (fx- n 1))
          n))))

(define (run start generator n)
  (let* ((start (time))
         (iteration (generator)))
    (if iteration
        (let* ((start (time))
               (out (fibonacci 0 1 n))
               (delta (fx- (time) start)))
          (print out delta)
          (run start generator n))
        (print "done" (fx- (time) start)))))

(run (time) (generate-positive-integers-from 10000) 80)
