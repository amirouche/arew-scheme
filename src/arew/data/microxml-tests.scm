(library (arew data microxml-tests)
  (export
   test-000
   test-001
   test-002
   test-003
   )
  (import (scheme base)
          (tests)
          (arew data microxml))

  (define test-000
    (test '(microxml) (microxml->sxml "<microxml></microxml>")))

  (define test-001
    (test '(microxml "via yxml") (microxml->sxml "<microxml>via yxml</microxml>")))

  (define test-002
    (test '(microxml (* (version "0")) "via yxml")
          (microxml->sxml "<microxml version=\"0\">via yxml</microxml>")))

  (define test-003
    (test '(microxml (* (test-name "test-003") (version "0")) (scheme "chez") (version (major "9")))
          (microxml->sxml "<microxml version=\"0\" test-name=\"test-003\" ><scheme>chez</scheme><version><major>9</major></version></microxml>")))


  )
