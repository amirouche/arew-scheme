;; (define magic
;;   (lambda (x) (if (zero? x) 42 1337)))

;; (magic 0)

(define fact
  (lambda (i n)
    (if (zero? i)
        n
        (fact (- i 1) (* n i)))))


(fact 5 1)


;; (define fibonacci
;;   (lambda (a b n)
;;     (if (zero? n)
;;         a
;;         (let* ((n* (- n 1))
;;                (b* (+ a b))
;;                (a* b))
;;            (fibonacci a* b* n*)))))

;; (fibonacci 0 1 11)

;; (define wrap
;;   (lambda ()
;;     (let ((value "test"))
;;       (lambda ()
;;         value))))

;; ((wrap))

;; (define generate-positive-integers-from
;;   (lambda (n)
;;     (let ((nbox (make-box n)))
;;       (lambda ()
;;         (let ((nbis (unbox nbox)))
;;           (if (zero? nbis)
;;               #f
;;               (let ((nbis* (- nbis 1)))
;;                 (let ((ignore (box! nbox nbis*)))
;;                   nbis))))))))

;; (define generator (generate-positive-integers-from 100))

;; (generator)

;; (define run
;;   (lambda (start generator n)
;;     (if (generator)
;;         (let* ((startbis (time))
;;                (out (fibonacci 0 1 n))
;;                (delta (- (time) startbis)))
;;           (print out delta)
;;           (run start generator n)
;;           out)
;;         (print -1 (- (time) start)))))

;; (run (time) (generate-positive-integers-from 10) 10)
