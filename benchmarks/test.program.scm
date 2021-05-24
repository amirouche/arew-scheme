(define fibonacci
  (lambda (a b n)
    (if (zero? n)
        a
        (fibonacci b (+ a b) (- n 1)))))

(define generate-positive-integers-from
  (lambda (n)
    ;; it will generate all integers from `n` to `0` excluded.
    (lambda ()
      (if (zero? n)
          #f
          (let ((nbis n))
            (set! n (- n 1))
            nbis)))))

(define run
  (lambda (start generator n)
    (if (generator)
        (let* ((startbis (time))
               (out (fibonacci 0 1 n))
               (delta (- (time) startbis)))
          (print out delta)
          (run start generator n))
        (print -1 (- (time) start)))))

(run (time) (generate-positive-integers-from 100) 2000)
