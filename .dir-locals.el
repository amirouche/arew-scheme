;; The 'nil' configuration applies to all modes.
((scheme-mode . ((indent-tabs-mode . nil)
		 (tab-width . 2)
		 (eval . (progn
                           ;; okvs
                           (put 'with-output-language 'scheme-indent-function 1)
                           (put 'nanopass-case 'scheme-indent-function 2)
                           (put 'okvs-in-transaction 'scheme-indent-function 2)
                           (put 'engine-in-transaction 'scheme-indent-function 2)
			   ;; sfx
			   (put 'section 'scheme-indent-function 1)
			   ;; (put 'library 'scheme-indent-function 1)
			   (put 'procedure 'scheme-indent-function 1)
			   ;; scheme
			   (put 'test-group 'scheme-indent-function 1)
			   (put 'switch 'scheme-indent-function 1)
			   (put 'test-equal 'scheme-indent-function 1)
			   (put 'call-with-input-string 'scheme-indent-function 1)
			   (put 'call-with-values 'scheme-indent-function 1)
			   ;; minikanren stuff
			   (put 'run* 'scheme-indent-function 1)
			   (put 'fresh 'scheme-indent-function 1)
			   (put 'conde 'scheme-indent-function nil)
			   (put 'run** 'scheme-indent-function 1)
			   ;; fs
			   (put 'fs:query 'scheme-indent-function 1)
			   ;; others
			   (put 'search-address-info 'scheme-indent-function 3)
			   (put 'call-with-lock 'scheme-indent-function 1)
			   (put 'call-with-port 'scheme-indent-function 1)
			   ;; wiredtigerz
			   (put 'with-cursor 'scheme-indent-function 1)
			   (put 'with-directory 'scheme-indent-function 1)
			   (put 'with-env 'scheme-indent-function 1)
			   (put 'with-context 'scheme-indent-function 1)
			   (put 'match 'scheme-indent-function 1)
			   (put 'with-transaction 'scheme-indent-function 1)
			   (put 'test-check 'scheme-indent-function 1)
			   (put 'call-with-cursor 'scheme-indent-function 3)
			   (put 'call-with-session 'scheme-indent-function 2)
			   (put 'call-with-context 'scheme-indent-function 2)
			   (put 'call-with-transaction 'scheme-indent-function 1)
			   (put 'with-cnx 'scheme-indent-function 1))))))
