(library (arew data json-tests)
  (export
   test-000
   test-001
   test-002
   test-003
   test-004
   test-005
   test-006
   test-007
   test-008
   test-009
   test-010
   test-011
   test-012
   )
  (import (scheme base)
          (tests)
          (arew data json))

  (define test-000
    (test #t (json->scm "true")))

  (define test-001
    (test #f (json->scm "false")))

  (define test-002
    (test "hello" (json->scm "\"hello\"")))

  (define test-003
    (test 3.14 (json->scm "3.14")))

  (define test-004
    (test -3.14 (json->scm "-3.14")))

  (define test-005
    (test 42 (json->scm "42")))

  (define test-006
    (test -42 (json->scm "-42")))

  (define test-007
    (test '(#t #f "hello" 3.14 -3.14 42 -42)
          (json->scm "[true, false, \"hello\", 3.14, -3.14, 42, -42]")))

  (define test-008
    (test '(* (float . 3.14) (integer . 42) (boolean . #t) (string . "hello"))
          (json->scm
           "{\"float\": 3.14, \"integer\": 42, \"boolean\": true, \"string\": \"hello\"}")))

  (define test-009
    (test '(*) (json->scm "{}")))

  (define test-010
    (test '() (json->scm "[]")))

  (define test-011
    (test '(#t #f "hello" 3.14 -3.14 42 -42 (* (float . 3.14) (integer . 42) (boolean . #t) (string . "hello")))
          (json->scm "[true, false, \"hello\", 3.14, -3.14, 42, -42, {\"float\": 3.14, \"integer\": 42, \"boolean\": true, \"string\": \"hello\"}]")))

  (define test-012
    (test #f (json->scm "[")))

  )
