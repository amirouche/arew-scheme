(import (scheme base))
(import (scheme write))
(import (arew data base okvslite))
(import (arew matchable))
(import (scheme process-context))


(define (set filepath key value)
  (define db (okvslite-new))
  (okvslite-open db filepath)
  (okvslite-insert db (string->utf8 key) (string->utf8 value))
  (okvslite-close db))

(define (get filepath key)
  (define db (okvslite-new))
  (okvslite-open db filepath)
  (let ((cursor (okvslite-cursor-open db))
        (key (string->utf8 key)))
    (okvslite-cursor-seek cursor key 'equal)
    ;; TODO: maybe when strategy is equal the following is not what I
    ;; expected it is.
    (if (okvslite-cursor-valid? cursor)
        (begin (display (utf8->string (okvslite-cursor-value cursor)))
               (newline)
               (okvslite-cursor-close cursor)
               (okvslite-close db))
        (begin (okvslite-cursor-close cursor)
               (okvslite-close db)
               (exit 1)))))

(define (help)
  (display "Usage:\n")
  (display "\n")
  (display "  set DB KEY VALUE\n")
  (display "  get DB KEY\n"))

(match (cdr (command-line))
  (("set" db key value) (set db key value))
  (("get" db key) (get db key))
  (else (help)))
   
