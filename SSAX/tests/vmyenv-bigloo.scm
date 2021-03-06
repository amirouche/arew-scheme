; Validation of my standard prelude under Bigloo
; Module declaration

(module vmyenv
    (include "myenv-bigloo.scm")
    (include "catch-error.scm")
    (include "env.scm")
    (include "util.scm")
    (include "vmyenv.scm"))

