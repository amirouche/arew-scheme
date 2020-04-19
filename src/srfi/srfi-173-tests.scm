(library (srfi srfi-173-tests)

  (export test-0
          test-1
          test-2
          test-3
          test-4
          test-5
          test-6
          )

  (import (scheme base)
          (tests)
          (srfi srfi-173))

  (define test-0
    ;; test hook->list and make-hook
    (test 0 (length (hook->list (make-hook 0)))))

  (define test-1
    ;; test hook-add!
    (test 1 (let ((hook (make-hook 0))
                  (counter 0))
              (hook-add! hook (lambda () (set! counter (+ counter 1))))
              (length (hook->list hook)))))

  (define test-2
    ;; test hook-delete!
    (test 0 (let ((hook (make-hook 0))
                  (counter 0))
              (let ((increment (lambda () (set! counter (+ counter 1)))))
                (hook-add! hook increment)
                (hook-delete! hook increment)
                (length (hook->list hook))))))

  (define test-3
    ;; test hook-reset!
    (test 0 (let ((hook (make-hook 0))
                  (counter 0))
              (let ((increment (lambda () (set! counter (+ counter 1))))
                    (decrement (lambda () (set! counter (- counter 1)))))
                (hook-add! hook increment)
                (hook-reset! hook)
                (length (hook->list hook))))))

  (define test-4
    ;; test hook-run
    (test 0 (let ((hook (make-hook 0))
                  (counter 0))
              (let ((increment (lambda () (set! counter (+ counter 1))))
                    (decrement (lambda () (set! counter (- counter 1)))))
                (hook-add! hook increment)
                (hook-add! hook decrement)
                (hook-run hook)
                counter))))

  (define test-5
    ;; test list->hook
    (test 0 (let* ((counter 0)
                   (increment (lambda () (set! counter (+ counter 1))))
                   (decrement (lambda () (set! counter (- counter 1)))))
              (let ((hook (list->hook 0 (list increment decrement))))
                (hook-add! hook increment)
                (hook-add! hook decrement)
                (hook-run hook)
                counter))))

  (define test-6
    ;; test list->hook!
    (test 0 (let* ((counter 0)
                   (increment (lambda () (set! counter (+ counter 1))))
                   (decrement (lambda () (set! counter (- counter 1))))
                   (hook (make-hook 0)))
              (list->hook! hook (list increment decrement))
              (hook-add! hook increment)
              (hook-add! hook decrement)
              (hook-run hook)
              counter))))
