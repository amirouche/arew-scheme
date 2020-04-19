#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (srfi srfi-11)
  (export
    let-values
    let*-values)
  (import
    (only (rnrs) let-values let*-values))
)
