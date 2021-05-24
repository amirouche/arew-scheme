(import (scheme base))
(import (scheme time))
(import (scheme write))

(define (time)
  (current-jiffy))

(define (print out delta)
  (display (string-append "out: "
                          (number->string out)
                          " nanoseconds: "
                          (number->string delta)
                          "\n")))

(define (fibonacci a b n)
  (if (zero? n)
      a
      (fibonacci b (+ a b) (- n 1))))

(define (generate-positive-integers-from n)
  ;; it will generate all integers from `n` to `0` both are excluded.
  (lambda ()
    (if (zero? n)
        #f
        (begin
          (set! n (- n 1))
          n))))

(define (run start generator n)
    (if (generator)
        (let* ((start* (time))
               (out (fibonacci 0 1 n))
               (delta (- (time) start*)))
          (print out delta)
          (run start generator n))
        (print -1 (- (time) start))))

(run (time) (generate-positive-integers-from 100) 2000)
