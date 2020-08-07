;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: CC0-1.0
#!r6rs

(library (scheme process-context)
  (export
   command-line emergency-exit (rename (r7rs-exit exit)) get-environment-variable
   get-environment-variables)
  (import
   (except (rnrs) command-line)
   (only (chezscheme) command-line foreign-procedure)
   (srfi srfi-98))

  (define (translate-status status)
    (case status
      ((#t) 0)
      ((#f) 1)
      (else status)))

  (define r7rs-exit
    (case-lambda
      (()
       (exit))
      ((status)
       (exit (translate-status status)))))

  (define native-emergency-exit
    (let ((c-exit (foreign-procedure "(cs)c_exit" (integer-32) void)))
      (case-lambda
        (()
         (c-exit 0))
        ((status)
         (c-exit status)))))

  (define emergency-exit
    (case-lambda
      (()
       (native-emergency-exit))
      ((status)
       (native-emergency-exit (translate-status status))))))
