;; (define magic
;;   (lambda (x) (if (zero? x) 42 1337)))

;; (magic 0)

;; (define fact
;;   (lambda (i n)
;;     (if (zero? i)
;;         n
;;         (fact (- i 1) (* n i)))))


;; (fact 5 1)

;; (define magic
;;   (lambda (x) (+ 40 x)))

;; (magic 2)

(define fibonacci
  (lambda (a b n)
    (if (zero? n)
        a
        (let* ((n* (- n 1))
               (b* (+ a b))
               (a* b))
           (fibonacci a* b* n*)))))

(define generate-positive-integers-from
  (lambda (n)
    (let ((nbox (make-box n)))
      (lambda ()
        (let ((nbis (unbox nbox)))
          (let ((nbis* (- nbis 1)))
            (let ((ignore (box! nbox nbis*)))
              nbis)))))))

(define run
  (lambda (start generator n out)
    (if (zero? (generator))
        (let ((ignore (print -1 (- (time) start))))
          out)
        (let ((out (fibonacci 0 1 n)))
          (run start generator n out)))))


(run (time) (generate-positive-integers-from 100000) 16 0)

;; (define frob
;;   (lambda (x)
;;     (call/cc (lambda (k) (k 42) x))))

;; (frob 1337)
