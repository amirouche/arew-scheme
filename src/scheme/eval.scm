;; -*- mode: scheme; coding: utf-8 -*-
;; SPDX-License-Identifier: CC0-1.0
#!r6rs

(library (scheme eval)
  (export environment eval)
  (import (scheme base)
          (prefix (only (chezscheme) eval environment copy-environment)
                  chez-))

  (define eval chez-eval)

  (define (environment . args)
    (define env (apply chez-environment args))
    (chez-copy-environment env #t)))
