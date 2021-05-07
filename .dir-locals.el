;; The 'nil' configuration applies to all modes.
((scheme-mode . ((indent-tabs-mode . nil)
		 (tab-width . 2)
		 (eval . (progn
                           (put 'guard 'scheme-indent-function 1)
			   ;; scheme
			   (put 'switch 'scheme-indent-function 1)
			   (put 'with-foreign-free 'scheme-indent-function 1)
			   (put 'if3 'scheme-indent-function 2)
			   (put 'call-with-input-string 'scheme-indent-function 1)
			   (put 'call-with-values 'scheme-indent-function 1)
			   (put 'with-lock 'scheme-indent-function 1)
			   (put 'with-mutex 'scheme-indent-function 1)
			   (put 'call-with-port 'scheme-indent-function 1)
			   (put 'with-cursor 'scheme-indent-function 1)
			   (put 'match 'scheme-indent-function 1))))))
