#!r6rs
;; Copyright 2010 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (srfi srfi-6)
  (export
    (rename (open-string-input-port open-input-string))
    open-output-string
    get-output-string)
  (import
    (only (rnrs io ports) open-string-input-port)
    (only (chezscheme) open-output-string get-output-string)))
