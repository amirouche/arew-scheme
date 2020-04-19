(library (srfi srfi-124)
  (export ephemeron?
          make-ephemeron
          ephemeron-broken?
          ephemeron-key
          ephemeron-value)

  (import (chezscheme))

  (begin

    (define make-ephemeron ephemeron-cons)

    (define ephemeron? ephemeron-pair?)

    (define ephemeron-key
      (lambda (ephemeron)
        (if (bwp-object? ephemeron)
            #f
            (car ephemeron))))

    (define ephemeron-datum
      (lambda (ephemeron)
        (if (bwp-object? ephemeron)
            #f
            (cdr ephemeron))))

    (define ephemeron-broken? bwp-object?)))
