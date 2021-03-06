(define (SSAX:warn port msg . other-msg)
  (apply cerr (cons (string-append (string #\newline) "Warning: ")
		    (cons msg
			  other-msg))))
