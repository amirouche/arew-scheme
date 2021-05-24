;; (define magic
;;   (lambda (x) (if (primcall zero? x) 42 1337)))

;; (magic 0)

;; (define fact
;;   (lambda (i n)
;;     (if (primcall zero? i)
;;         n
;;         (fact (primcall - i 1) (primcall * n i)))))


;; (fact 5 1)


(define fibonacci
  (lambda (a b n)
    (if (primcall zero? n)
        a
        (let* ((n* (primcall sub n 1))
               (b* (primcall add a b))
               (a* b))
           (fibonacci a* b* n*)))))

;; (fibonacci 0 1 11)

;; (define wrap
;;   (lambda ()
;;     (let ((value "test"))
;;       (lambda ()
;;         value))))

;; ((wrap))

(define generate-positive-integers-from
  (lambda (n)
    (let ((nbox (primcall make-box n)))
      (lambda ()
        (let ((nbis (primcall unbox nbox)))
          (if (primcall zero? nbis)
              #f
              (let ((nbis* (primcall - nbis 1)))
                (let ((ignore (primcall box-set! nbox nbis*)))
                  nbis))))))))

;; (define generator (generate-positive-integers-from 100))

;; (generator)

(define run
  (lambda (start generator n)
    (if (generator)
        (let* ((startbis (primcall time))
               (out (fibonacci 0 1 n))
               (delta (primcall - (primcall time) startbis)))
          (primcall print out delta)
          (run start generator n)
          out)
        (primcall print -1 (primcall - (primcall time) start)))))

(run (primcall time) (generate-positive-integers-from 10) 10)
