(define _eqv?_7 eqv?)
(define _cons_23 cons)
(define _append_24 append)
(define _list_25 list)
(define _vector_26 vector)
(define _list->vector_27 list->vector)
(define make-xml-token (lambda (_kind_38 _head_39) (cons _kind_38 _head_39)))
(define xml-token? pair?)
(define ssax:warn
  (lambda (_port_38 _msg_39 . _other-msg_40)
    (apply cerr (cons* nl "Warning: " _msg_39 _other-msg_40))))
(define parser-error
  (lambda (_port_38 _msg_39 . _specializing-msgs_40)
    (apply error (cons _msg_39 _specializing-msgs_40))))
(define equal_?
  (lambda (_e1_38 _e2_39)
    (if (eq? (if (string? 'A) (string->symbol 'A) 'A) (string->symbol "A"))
      (equal? _e1_38 _e2_39)
      (if (symbol? _e1_38)
        (if (symbol? _e2_39)
          (string-ci=? (symbol->string _e1_38) (symbol->string _e2_39))
          #f)
        (if (pair? _e1_38)
          (if (pair? _e2_39)
            (if (equal_? (car _e1_38) (car _e2_39))
              (equal_? (cdr _e1_38) (cdr _e2_39))
              #f)
            #f)
          (if (vector? _e1_38)
            (if (vector? _e2_39)
              (equal_? (vector->list _e1_38) (vector->list _e2_39))
              #f)
            (equal? _e1_38 _e2_39)))))))
(define unesc-string
  (lambda (_str_38)
    (call-with-input-string
      _str_38
      (lambda (_port_45)
        ((letrec ((_loop_46
                    (lambda (_frags_47)
                      ((lambda (_token_48)
                         ((lambda (_cterm_49)
                            ((lambda (_frags_50)
                               (if (eof-object? _cterm_49)
                                 (string-concatenate-reverse/shared _frags_50)
                                 ((lambda (_cchar_51)
                                    (if (eof-object? _cchar_51)
                                      (error "unexpected EOF after reading % in unesc-string:"
                                             _str_38)
                                      (_loop_46
                                        (cons ((lambda (_key_52)
                                                 (if (if (_eqv?_7 _key_52 '#\n)
                                                       #t
                                                       #f)
                                                   (string #\newline)
                                                   (if (if (_eqv?_7 _key_52
                                                                    '#\r)
                                                         #t
                                                         #f)
                                                     (string char-return)
                                                     (if (if (_eqv?_7 _key_52
                                                                      '#\t)
                                                           #t
                                                           #f)
                                                       (string char-tab)
                                                       (if (if (_eqv?_7 _key_52
                                                                        '#\%)
                                                             #t
                                                             #f)
                                                         "%"
                                                         (error "bad %-char in unesc-string:"
                                                                _cchar_51))))))
                                               _cchar_51)
                                              _frags_50))))
                                  (read-char _port_45))))
                             (cons _token_48 _frags_47)))
                          (read-char _port_45)))
                       (next-token
                         '()
                         '(#\% *eof*)
                         "unesc-string"
                         _port_45)))))
           _loop_46)
         '())))))
(define string-whitespace?
  (lambda (_str_38)
    ((lambda (_len_39)
       (if (zero? _len_39)
         #t
         (if (= 1 _len_39)
           (char-whitespace? (string-ref _str_38 0))
           (if (= 2 _len_39)
             (if (char-whitespace? (string-ref _str_38 0))
               (char-whitespace? (string-ref _str_38 1))
               #f)
             ((letrec ((_loop_40
                         (lambda (_i_41)
                           ((lambda (_x_42)
                              (if _x_42
                                _x_42
                                (if (char-whitespace?
                                      (string-ref _str_38 _i_41))
                                  (_loop_40 (+ 1 _i_41))
                                  #f)))
                            (>= _i_41 _len_39)))))
                _loop_40)
              0)))))
     (string-length _str_38))))
(define assq-values
  (lambda (_val_38 _alist_39)
    ((letrec ((_loop_40
                (lambda (_alist_41 _scanned_42)
                  (if (null? _alist_41)
                    (values #f _scanned_42)
                    (if (equal? _val_38 (caar _alist_41))
                      (values (car _alist_41)
                              (append _scanned_42 (cdr _alist_41)))
                      (_loop_40
                        (cdr _alist_41)
                        (cons (car _alist_41) _scanned_42)))))))
       _loop_40)
     _alist_39
     '())))
(define fold-right
  (lambda (_kons_38 _knil_39 _lis1_40)
    ((letrec ((_recur_41
                (lambda (_lis_42)
                  (if (null? _lis_42)
                    _knil_39
                    ((lambda (_head_43)
                       (_kons_38 _head_43 (_recur_41 (cdr _lis_42))))
                     (car _lis_42))))))
       _recur_41)
     _lis1_40)))
(define fold
  (lambda (_kons_38 _knil_39 _lis1_40)
    ((letrec ((_lp_41 (lambda (_lis_42 _ans_43)
                        (if (null? _lis_42)
                          _ans_43
                          (_lp_41 (cdr _lis_42)
                                  (_kons_38 (car _lis_42) _ans_43))))))
       _lp_41)
     _lis1_40
     _knil_39)))
(define ssax:S-chars (map ascii->char '(32 10 9 13)))
(define ssax:skip-S (lambda (_port_38) (skip-while ssax:S-chars _port_38)))
(define ssax:ncname-starting-char?
  (lambda (_a-char_38)
    (if (char? _a-char_38)
      ((lambda (_x_39) (if _x_39 _x_39 (char=? #\_ _a-char_38)))
       (char-alphabetic? _a-char_38))
      #f)))
(define ssax:read-NCName
  (lambda (_port_38)
    (begin
      ((lambda (_first-char_39)
         ((lambda (_x_40)
            (if _x_40
              _x_40
              (parser-error _port_38 "XMLNS [4] for '" _first-char_39 "'")))
          (ssax:ncname-starting-char? _first-char_39)))
       (peek-char _port_38))
      (string->symbol
        (next-token-of
          (lambda (_c_39)
            (if (eof-object? _c_39)
              #f
              (if (char-alphabetic? _c_39)
                _c_39
                (if (string-index "0123456789.-_" _c_39) _c_39 #f))))
          _port_38)))))
(define ssax:read-QName
  (lambda (_port_38)
    ((lambda (_prefix-or-localpart_39)
       ((lambda (_key_40)
          (if (if (_eqv?_7 _key_40 '#\:) #t #f)
            (begin
              (read-char _port_38)
              (cons _prefix-or-localpart_39 (ssax:read-NCName _port_38)))
            _prefix-or-localpart_39))
        (peek-char _port_38)))
     (ssax:read-NCName _port_38))))
(define ssax:Prefix-XML (string->symbol "xml"))
(assert (eq? (if (string? '_) (string->symbol '_) '_)
             (call-with-input-string "_" ssax:read-NCName)))
(assert (eq? (if (string? '_) (string->symbol '_) '_)
             (call-with-input-string "_" ssax:read-QName)))
(assert (eq? (string->symbol "_abc_")
             (call-with-input-string "_abc_;" ssax:read-NCName)))
(assert (eq? (string->symbol "_abc_")
             (call-with-input-string "_abc_;" ssax:read-QName)))
(assert (eq? (string->symbol "_a.b")
             (call-with-input-string "_a.b " ssax:read-QName)))
(assert (equal? (cons (string->symbol "_a.b") (string->symbol "d.1-ef-"))
                (call-with-input-string "_a.b:d.1-ef-;" ssax:read-QName)))
(assert (equal? (cons (string->symbol "a") (string->symbol "b"))
                (call-with-input-string "a:b:c" ssax:read-QName)))
(assert (failed? (call-with-input-string ":abc" ssax:read-NCName)))
(assert (failed? (call-with-input-string "1:bc" ssax:read-NCName)))
(define name-compare
  (letrec ((_symbol-compare_38
             (lambda (_symb1_39 _symb2_40)
               (if (eq? _symb1_39 _symb2_40)
                 '=
                 (if (string<?
                       (symbol->string _symb1_39)
                       (symbol->string _symb2_40))
                   '<
                   '>)))))
    (lambda (_name1_39 _name2_40)
      (if (symbol? _name1_39)
        (if (symbol? _name2_40) (_symbol-compare_38 _name1_39 _name2_40) '<)
        (if (symbol? _name2_40)
          '>
          (if (eq? _name2_40 ssax:largest-unres-name)
            '<
            (if (eq? _name1_39 ssax:largest-unres-name)
              '>
              (if (eq? (car _name1_39) (car _name2_40))
                (_symbol-compare_38 (cdr _name1_39) (cdr _name2_40))
                (_symbol-compare_38 (car _name1_39) (car _name2_40))))))))))
(define ssax:largest-unres-name
  (cons (string->symbol "#LARGEST-SYMBOL") (string->symbol "#LARGEST-SYMBOL")))
(assert (eq? (if (string? '=) (string->symbol '=) '=)
             (name-compare
               (if (string? 'ABC) (string->symbol 'ABC) 'ABC)
               (if (string? 'ABC) (string->symbol 'ABC) 'ABC))))
(assert (eq? (if (string? '<) (string->symbol '<) '<)
             (name-compare
               (if (string? 'ABC) (string->symbol 'ABC) 'ABC)
               (if (string? 'ABCD) (string->symbol 'ABCD) 'ABCD))))
(assert (eq? (if (string? '>) (string->symbol '>) '>)
             (name-compare
               (if (string? 'XB) (string->symbol 'XB) 'XB)
               (if (string? 'ABCD) (string->symbol 'ABCD) 'ABCD))))
(assert (eq? (if (string? '>) (string->symbol '>) '>)
             (name-compare
               '(HTML . PRE)
               (if (string? 'PRE) (string->symbol 'PRE) 'PRE))))
(assert (eq? (if (string? '<) (string->symbol '<) '<)
             (name-compare
               (if (string? 'HTML) (string->symbol 'HTML) 'HTML)
               '(HTML . PRE))))
(assert (eq? (if (string? '=) (string->symbol '=) '=)
             (name-compare '(HTML . PRE) '(HTML . PRE))))
(assert (eq? (if (string? '<) (string->symbol '<) '<)
             (name-compare '(HTML . PRE) '(XML . PRE))))
(assert (eq? (if (string? '>) (string->symbol '>) '>)
             (name-compare '(HTML . PRE) '(HTML . P))))
(assert (eq? (if (string? '<) (string->symbol '<) '<)
             (name-compare '(HTML . PRE) ssax:largest-unres-name)))
(assert (eq? (if (string? '<) (string->symbol '<) '<)
             (name-compare '(ZZZZ . ZZZ) ssax:largest-unres-name)))
(assert (eq? (if (string? '>) (string->symbol '>) '>)
             (name-compare ssax:largest-unres-name '(ZZZZ . ZZZ))))
(define ssax:read-markup-token
  (letrec ((_read-cdata_38
             (lambda (_port_40)
               (begin
                 (assert (string=? "CDATA[" (read-string 6 _port_40)))
                 (make-xml-token 'CDSECT #f))))
           (_skip-comment_39
             (lambda (_port_40)
               (begin
                 (assert-curr-char '(#\-) "XML [15], second dash" _port_40)
                 (if (not (find-string-from-port? "-->" _port_40))
                   (parser-error _port_40 "XML [15], no -->"))
                 (make-xml-token 'COMMENT #f)))))
    (lambda (_port_40)
      (begin
        (assert-curr-char '(#\<) "start of the token" _port_40)
        ((lambda (_key_41)
           (if (if (_eqv?_7 _key_41 '#\/) #t #f)
             (begin
               (read-char _port_40)
               ((lambda (_val_42)
                  (begin
                    (ssax:skip-S _port_40)
                    (assert-curr-char '(#\>) "XML [42]" _port_40)
                    _val_42))
                (make-xml-token 'END (ssax:read-QName _port_40))))
             (if (if (_eqv?_7 _key_41 '#\?) #t #f)
               (begin
                 (read-char _port_40)
                 (make-xml-token 'PI (ssax:read-NCName _port_40)))
               (if (if (_eqv?_7 _key_41 '#\!) #t #f)
                 ((lambda (_key_42)
                    (if (if (_eqv?_7 _key_42 '#\-) #t #f)
                      (begin (read-char _port_40) (_skip-comment_39 _port_40))
                      (if (if (_eqv?_7 _key_42 '#\[) #t #f)
                        (begin (read-char _port_40) (_read-cdata_38 _port_40))
                        (make-xml-token 'DECL (ssax:read-NCName _port_40)))))
                  (peek-next-char _port_40))
                 (make-xml-token 'START (ssax:read-QName _port_40))))))
         (peek-char _port_40))))))
(define ssax:skip-pi
  (lambda (_port_38)
    (if (not (find-string-from-port? "?>" _port_38))
      (parser-error _port_38 "Failed to find ?> terminating the PI"))))
(define ssax:read-pi-body-as-string
  (lambda (_port_38)
    (begin
      (ssax:skip-S _port_38)
      (string-concatenate/shared
        ((letrec ((_loop_39
                    (lambda ()
                      ((lambda (_pi-fragment_40)
                         (if (eqv? #\> (peek-next-char _port_38))
                           (begin
                             (read-char _port_38)
                             (cons _pi-fragment_40 '()))
                           (cons* _pi-fragment_40 "?" (_loop_39))))
                       (next-token
                         '()
                         '(#\?)
                         "reading PI content"
                         _port_38)))))
           _loop_39))))))
(assert (equal? "p1 content "
                (call-with-input-string
                  "<?pi1  p1 content ?>"
                  (lambda (_port_44)
                    (begin
                      (ssax:read-markup-token _port_44)
                      (ssax:read-pi-body-as-string _port_44))))))
(assert (equal? "pi2? content? ?"
                (call-with-input-string
                  "<?pi2 pi2? content? ??>"
                  (lambda (_port_44)
                    (begin
                      (ssax:read-markup-token _port_44)
                      (ssax:read-pi-body-as-string _port_44))))))
(define ssax:skip-internal-dtd
  (lambda (_port_38)
    (if (not (find-string-from-port? "]>" _port_38))
      (parser-error
        _port_38
        "Failed to find ]> terminating the internal DTD subset"))))
(define ssax:read-cdata-body
  ((lambda (_cdata-delimiters_38)
     (lambda (_port_39 _str-handler_40 _seed_41)
       ((letrec ((_loop_42
                   (lambda (_seed_43)
                     ((lambda (_fragment_44)
                        ((lambda (_key_45)
                           (if (if (_eqv?_7 _key_45 '#\newline) #t #f)
                             (_loop_42
                               (_str-handler_40 _fragment_44 nl _seed_43))
                             (if (if (_eqv?_7 _key_45 '#\]) #t #f)
                               (if (not (eqv? (peek-char _port_39) #\]))
                                 (_loop_42
                                   (_str-handler_40 _fragment_44 "]" _seed_43))
                                 ((letrec ((_check-after-second-braket_46
                                             (lambda (_seed_47)
                                               ((lambda (_key_48)
                                                  (if (if (_eqv?_7 _key_48
                                                                   '#\>)
                                                        #t
                                                        #f)
                                                    (begin
                                                      (read-char _port_39)
                                                      _seed_47)
                                                    (if (if (_eqv?_7 _key_48
                                                                     '#\])
                                                          #t
                                                          #f)
                                                      (_check-after-second-braket_46
                                                        (_str-handler_40
                                                          "]"
                                                          ""
                                                          _seed_47))
                                                      (_loop_42
                                                        (_str-handler_40
                                                          "]]"
                                                          ""
                                                          _seed_47)))))
                                                (peek-next-char _port_39)))))
                                    _check-after-second-braket_46)
                                  (if (zero? (string-length _fragment_44))
                                    _seed_43
                                    (_str-handler_40
                                      _fragment_44
                                      ""
                                      _seed_43))))
                               (if (if (_eqv?_7 _key_45 '#\&) #t #f)
                                 ((lambda (_ent-ref_46)
                                    (if (if (string=? "gt" _ent-ref_46)
                                          (eqv? (peek-char _port_39) #\;)
                                          #f)
                                      (begin
                                        (read-char _port_39)
                                        (_loop_42
                                          (_str-handler_40
                                            _fragment_44
                                            ">"
                                            _seed_43)))
                                      (_loop_42
                                        (_str-handler_40
                                          _ent-ref_46
                                          ""
                                          (_str-handler_40
                                            _fragment_44
                                            "&"
                                            _seed_43)))))
                                  (next-token-of
                                    (lambda (_c_46)
                                      (if (not (eof-object? _c_46))
                                        (if (char-alphabetic? _c_46) _c_46 #f)
                                        #f))
                                    _port_39))
                                 (begin
                                   (if (eqv? (peek-char _port_39) #\newline)
                                     (read-char _port_39))
                                   (_loop_42
                                     (_str-handler_40
                                       _fragment_44
                                       nl
                                       _seed_43)))))))
                         (read-char _port_39)))
                      (next-token
                        '()
                        _cdata-delimiters_38
                        "reading CDATA"
                        _port_39)))))
          _loop_42)
        _seed_41)))
   (list char-return #\newline #\] #\&)))
(letrec ((_test_44
           (lambda (_str_46 _expected-result_47)
             (begin
               (newline)
               (display "body: ")
               (write _str_46)
               (newline)
               (display "Result: ")
               ((lambda (_result_48)
                  (begin
                    (write _result_48)
                    (assert (equal? _result_48 _expected-result_47))))
                (reverse (call-with-input-string
                           (unesc-string _str_46)
                           (lambda (_port_48)
                             (ssax:read-cdata-body
                               _port_48
                               _consumer_45
                               '()))))))))
         (_consumer_45
           (lambda (_fragment_46 _foll-fragment_47 _seed_48)
             (cons* (if (equal? _foll-fragment_47 (string #\newline))
                      " NL"
                      _foll-fragment_47)
                    _fragment_46
                    _seed_48))))
  (begin
    (_test_44 "]]>" '())
    (_test_44 "abcd]]>" '("abcd" ""))
    (_test_44 "abcd]]]>" '("abcd" "" "]" ""))
    (_test_44 "abcd]]]]>" '("abcd" "" "]" "" "]" ""))
    (_test_44 "abcd]]]]]>" '("abcd" "" "]" "" "]" "" "]" ""))
    (_test_44 "abcd]]]a]]>" '("abcd" "" "]" "" "]]" "" "a" ""))
    (_test_44 "abc%r%ndef%n]]>" '("abc" " NL" "def" " NL"))
    (_test_44 "%r%n%r%n]]>" '("" " NL" "" " NL"))
    (_test_44 "%r%n%r%na]]>" '("" " NL" "" " NL" "a" ""))
    (_test_44 "%r%r%r%na]]>" '("" " NL" "" " NL" "" " NL" "a" ""))
    (_test_44 "abc&!!!]]>" '("abc" "&" "" "" "!!!" ""))
    (_test_44
      "abc]]&gt;&gt&amp;]]]&gt;and]]>"
      '("abc"
        ""
        "]]"
        ""
        ""
        ">"
        ""
        "&"
        "gt"
        ""
        ""
        "&"
        "amp"
        ""
        ";"
        ""
        "]"
        ""
        "]]"
        ""
        ""
        ">"
        "and"
        ""))))
(define ssax:read-char-ref
  (lambda (_port_38)
    ((lambda (_base_39)
       ((lambda (_name_40)
          ((lambda (_char-code_41)
             (begin
               (read-char _port_38)
               (if (integer? _char-code_41)
                 (ucscode->char _char-code_41)
                 (parser-error
                   _port_38
                   "[wf-Legalchar] broken for '"
                   _name_40
                   "'"))))
           (string->number _name_40 _base_39)))
        (next-token '() '(#\;) "XML [66]" _port_38)))
     (if (eqv? (peek-char _port_38) #\x) (begin (read-char _port_38) 16) 10))))
(define ssax:predefined-parsed-entities
  (_list_25
    (_cons_23 (string->symbol "amp") '"&")
    (_cons_23 (string->symbol "lt") '"<")
    (_cons_23 (string->symbol "gt") '">")
    (_cons_23 (string->symbol "apos") '"'")
    (_cons_23 (string->symbol "quot") '"\"")))
(define ssax:handle-parsed-entity
  (lambda (_port_38
           _name_39
           _entities_40
           _content-handler_41
           _str-handler_42
           _seed_43)
    ((lambda (_tmp_44)
       (if _tmp_44
         ((lambda (_decl-entity_45)
            ((lambda (_ent-body_46 _new-entities_47)
               (if (string? _ent-body_46)
                 (call-with-input-string
                   _ent-body_46
                   (lambda (_port_48)
                     (_content-handler_41 _port_48 _new-entities_47 _seed_43)))
                 (if (procedure? _ent-body_46)
                   ((lambda (_port_48)
                      ((lambda (_val_49)
                         (begin (close-input-port _port_48) _val_49))
                       (_content-handler_41
                         _port_48
                         _new-entities_47
                         _seed_43)))
                    (_ent-body_46))
                   (parser-error
                     _port_38
                     "[norecursion] broken for "
                     _name_39))))
             (cdr _decl-entity_45)
             (cons (cons _name_39 #f) _entities_40)))
          _tmp_44)
         ((lambda (_tmp_45)
            (if _tmp_45
              ((lambda (_decl-entity_46)
                 (_str-handler_42 (cdr _decl-entity_46) "" _seed_43))
               _tmp_45)
              (parser-error _port_38 "[wf-entdeclared] broken for " _name_39)))
          (assq _name_39 ssax:predefined-parsed-entities))))
     (assq _name_39 _entities_40))))
(define make-empty-attlist (lambda () '()))
(define attlist-add
  (lambda (_attlist_38 _name-value_39)
    (if (null? _attlist_38)
      (cons _name-value_39 _attlist_38)
      ((lambda (_key_40)
         (if (if (_eqv?_7 _key_40 '=) #t #f)
           #f
           (if (if (_eqv?_7 _key_40 '<) #t #f)
             (cons _name-value_39 _attlist_38)
             (cons (car _attlist_38)
                   (attlist-add (cdr _attlist_38) _name-value_39)))))
       (name-compare (car _name-value_39) (caar _attlist_38))))))
(define attlist-null? null?)
(define attlist-remove-top
  (lambda (_attlist_38) (values (car _attlist_38) (cdr _attlist_38))))
(define attlist->alist (lambda (_attlist_38) _attlist_38))
(define attlist-fold fold)
(define ssax:read-attributes
  ((lambda (_value-delimeters_38)
     (letrec ((_read-named-entity_39
                (lambda (_port_41 _entities_42 _fragments_43)
                  ((lambda (_name_44)
                     (begin
                       (assert-curr-char '(#\;) "XML [68]" _port_41)
                       (ssax:handle-parsed-entity
                         _port_41
                         _name_44
                         _entities_42
                         (lambda (_port_45 _entities_46 _fragments_47)
                           (_read-attrib-value_40
                             '*eof*
                             _port_45
                             _entities_46
                             _fragments_47))
                         (lambda (_str1_45 _str2_46 _fragments_47)
                           (if (equal? "" _str2_46)
                             (cons _str1_45 _fragments_47)
                             (cons* _str2_46 _str1_45 _fragments_47)))
                         _fragments_43)))
                   (ssax:read-NCName _port_41))))
              (_read-attrib-value_40
                (lambda (_delimiter_41
                         _port_42
                         _entities_43
                         _prev-fragments_44)
                  ((lambda (_new-fragments_45)
                     ((lambda (_cterm_46)
                        (if ((lambda (_x_47)
                               (if _x_47 _x_47 (eqv? _cterm_46 _delimiter_41)))
                             (eof-object? _cterm_46))
                          _new-fragments_45
                          (if (eqv? _cterm_46 char-return)
                            (begin
                              (if (eqv? (peek-char _port_42) #\newline)
                                (read-char _port_42))
                              (_read-attrib-value_40
                                _delimiter_41
                                _port_42
                                _entities_43
                                (cons " " _new-fragments_45)))
                            (if (memv _cterm_46 ssax:S-chars)
                              (_read-attrib-value_40
                                _delimiter_41
                                _port_42
                                _entities_43
                                (cons " " _new-fragments_45))
                              (if (eqv? _cterm_46 #\&)
                                (if (eqv? (peek-char _port_42) #\#)
                                  (begin
                                    (read-char _port_42)
                                    (_read-attrib-value_40
                                      _delimiter_41
                                      _port_42
                                      _entities_43
                                      (cons (string (ssax:read-char-ref
                                                      _port_42))
                                            _new-fragments_45)))
                                  (_read-attrib-value_40
                                    _delimiter_41
                                    _port_42
                                    _entities_43
                                    (_read-named-entity_39
                                      _port_42
                                      _entities_43
                                      _new-fragments_45)))
                                (parser-error
                                  _port_42
                                  "[CleanAttrVals] broken"))))))
                      (read-char _port_42)))
                   (cons (next-token
                           '()
                           (cons _delimiter_41 _value-delimeters_38)
                           "XML [10]"
                           _port_42)
                         _prev-fragments_44)))))
       (lambda (_port_41 _entities_42)
         ((letrec ((_loop_43
                     (lambda (_attr-list_44)
                       (if (not (ssax:ncname-starting-char?
                                  (ssax:skip-S _port_41)))
                         _attr-list_44
                         ((lambda (_name_45)
                            (begin
                              (ssax:skip-S _port_41)
                              (assert-curr-char '(#\=) "XML [25]" _port_41)
                              (ssax:skip-S _port_41)
                              ((lambda (_delimiter_46)
                                 (_loop_43
                                   ((lambda (_x_47)
                                      (if _x_47
                                        _x_47
                                        (parser-error
                                          _port_41
                                          "[uniqattspec] broken for "
                                          _name_45)))
                                    (attlist-add
                                      _attr-list_44
                                      (cons _name_45
                                            (string-concatenate-reverse/shared
                                              (_read-attrib-value_40
                                                _delimiter_46
                                                _port_41
                                                _entities_42
                                                '())))))))
                               (assert-curr-char
                                 '(#\' #\")
                                 "XML [10]"
                                 _port_41))))
                          (ssax:read-QName _port_41))))))
            _loop_43)
          (make-empty-attlist)))))
   (append ssax:S-chars '(#\< #\&))))
(letrec ((_test_44
           (lambda (_str_45 _decl-entities_46 _expected-res_47)
             (begin
               (newline)
               (display "input: ")
               (write _str_45)
               (newline)
               (display "Result: ")
               ((lambda (_result_48)
                  (begin
                    (write _result_48)
                    (newline)
                    (assert (equal? _result_48 _expected-res_47))))
                (call-with-input-string
                  (unesc-string _str_45)
                  (lambda (_port_48)
                    (ssax:read-attributes _port_48 _decl-entities_46))))))))
  (begin
    (_test_44 "" '() '())
    (_test_44
      "href='http://a%tb%r%n%r%n%nc'"
      '()
      (_list_25 (_cons_23 (string->symbol "href") '"http://a b   c")))
    (_test_44
      "href='http://a%tb%r%r%n%rc'"
      '()
      (_list_25 (_cons_23 (string->symbol "href") '"http://a b   c")))
    (_test_44
      "_1 ='12&amp;' _2= \"%r%n%t12&#10;3\">"
      '()
      (_list_25 '(_1 . "12&") (_cons_23 '_2 (unesc-string "  12%n3"))))
    (_test_44
      "%tAbc='&lt;&amp;&gt;&#x0A;'%nNext='12&ent;34' />"
      '((ent . "&lt;xx&gt;"))
      (_list_25
        (_cons_23 (string->symbol "Abc") (unesc-string "<&>%n"))
        (_cons_23 (string->symbol "Next") '"12<xx>34")))
    (_test_44
      "%tAbc='&lt;&amp;&gt;&#x0d;'%nNext='12&ent;34' />"
      '((ent . "&lt;xx&gt;"))
      (_list_25
        (_cons_23 (string->symbol "Abc") (unesc-string "<&>%r"))
        (_cons_23 (string->symbol "Next") '"12<xx>34")))
    (_test_44
      "%tAbc='&lt;&amp;&gt;&#x0A;'%nNext='12&en;34' />"
      (_list_25
        (_cons_23 'en (lambda () (open-input-string "&quot;xx&apos;"))))
      (_list_25
        (_cons_23 (string->symbol "Abc") (unesc-string "<&>%n"))
        (_cons_23 (string->symbol "Next") '"12\"xx'34")))
    (_test_44
      "%tAbc='&lt;&amp;&gt;&#x0A;'%nNext='12&ent;34' />"
      '((ent . "&lt;&ent1;T;&gt;") (ent1 . "&amp;"))
      (_list_25
        (_cons_23 (string->symbol "Abc") (unesc-string "<&>%n"))
        (_cons_23 (string->symbol "Next") '"12<&T;>34")))
    (assert (failed? (_test_44
                       "%tAbc='&lt;&amp;&gt;&#x0A;'%nNext='12&ent;34' />"
                       '((ent . "<&ent1;T;&gt;") (ent1 . "&amp;"))
                       '())))
    (assert (failed? (_test_44
                       "%tAbc='&lt;&amp;&gt;&#x0A;'%nNext='12&ent;34' />"
                       '((ent . "&lt;&ent;T;&gt;") (ent1 . "&amp;"))
                       '())))
    (assert (failed? (_test_44
                       "%tAbc='&lt;&amp;&gt;&#x0A;'%nNext='12&ent;34' />"
                       '((ent . "&lt;&ent1;T;&gt;") (ent1 . "&ent;"))
                       '())))
    (_test_44
      "html:href='http://a%tb%r%n%r%n%nc'"
      '()
      (_list_25
        (_cons_23
          (_cons_23 (string->symbol "html") (string->symbol "href"))
          '"http://a b   c")))
    (_test_44
      "html:href='ref1' html:src='ref2'"
      '()
      (_list_25
        (_cons_23
          (_cons_23 (string->symbol "html") (string->symbol "href"))
          '"ref1")
        (_cons_23
          (_cons_23 (string->symbol "html") (string->symbol "src"))
          '"ref2")))
    (_test_44
      "html:href='ref1' xml:html='ref2'"
      '()
      (_list_25
        (_cons_23
          (_cons_23 (string->symbol "html") (string->symbol "href"))
          '"ref1")
        (_cons_23 (_cons_23 ssax:Prefix-XML (string->symbol "html")) '"ref2")))
    (assert (failed? (_test_44 "html:href='ref1' html:href='ref2'" '() '())))
    (assert (failed? (_test_44 "html:href='<' html:href='ref2'" '() '())))
    (assert (failed? (_test_44
                       "html:href='ref1' html:href='&ref2;'"
                       '()
                       '())))))
(define ssax:resolve-name
  (lambda (_port_38 _unres-name_39 _namespaces_40 _apply-default-ns?_41)
    (if (pair? _unres-name_39)
      (cons ((lambda (_tmp_42)
               (if _tmp_42
                 (cadr _tmp_42)
                 (if (eq? (car _unres-name_39) ssax:Prefix-XML)
                   ssax:Prefix-XML
                   (parser-error
                     _port_38
                     "[nsc-NSDeclared] broken; prefix "
                     (car _unres-name_39)))))
             (assq (car _unres-name_39) _namespaces_40))
            (cdr _unres-name_39))
      (if _apply-default-ns?_41
        ((lambda (_default-ns_42)
           (if (if _default-ns_42 (cadr _default-ns_42) #f)
             (cons (cadr _default-ns_42) _unres-name_39)
             _unres-name_39))
         (assq '*DEFAULT* _namespaces_40))
        _unres-name_39))))
((lambda (_namespaces_44)
   ((lambda (_namespaces-def_45)
      ((lambda (_namespaces-undef_46)
         ((lambda (_port_47)
            (begin
              (assert (equal? (if (string? 'ABC) (string->symbol 'ABC) 'ABC)
                              (ssax:resolve-name
                                _port_47
                                (if (string? 'ABC) (string->symbol 'ABC) 'ABC)
                                _namespaces_44
                                #t)))
              (assert (equal? '(DEF . ABC)
                              (ssax:resolve-name
                                _port_47
                                (if (string? 'ABC) (string->symbol 'ABC) 'ABC)
                                _namespaces-def_45
                                #t)))
              (assert (equal? (if (string? 'ABC) (string->symbol 'ABC) 'ABC)
                              (ssax:resolve-name
                                _port_47
                                (if (string? 'ABC) (string->symbol 'ABC) 'ABC)
                                _namespaces-def_45
                                #f)))
              (assert (equal? (if (string? 'ABC) (string->symbol 'ABC) 'ABC)
                              (ssax:resolve-name
                                _port_47
                                (if (string? 'ABC) (string->symbol 'ABC) 'ABC)
                                _namespaces-undef_46
                                #t)))
              (assert (equal? '(UHTML . ABC)
                              (ssax:resolve-name
                                _port_47
                                '(HTML . ABC)
                                _namespaces-def_45
                                #t)))
              (assert (equal? '(UHTML . ABC)
                              (ssax:resolve-name
                                _port_47
                                '(HTML . ABC)
                                _namespaces-def_45
                                #f)))
              (assert (equal? (_cons_23 ssax:Prefix-XML 'space)
                              (ssax:resolve-name
                                _port_47
                                (_cons_23 (string->symbol "xml") 'space)
                                _namespaces-def_45
                                #f)))
              (assert (failed? (ssax:resolve-name
                                 _port_47
                                 '(XXX . ABC)
                                 _namespaces-def_45
                                 #f)))))
          (current-input-port)))
       (cons '(*DEFAULT* #f . #f) _namespaces-def_45)))
    (cons '(*DEFAULT* DEF . URN-DEF) _namespaces_44)))
 '((HTML UHTML . URN-HTML) (HTML UHTML-1 . URN-HTML) (A UHTML . URN-HTML)))
(define ssax:uri-string->symbol
  (lambda (_uri-str_38) (string->symbol _uri-str_38)))
(define ssax:complete-start-tag
  ((lambda (_xmlns_38 _largest-dummy-decl-attr_39)
     (letrec ((_adjust-namespace-decl_40
                (lambda (_port_43 _attrs_44 _namespaces_45)
                  ((letrec ((_loop_46
                              (lambda (_attrs_47
                                       _proper-attrs_48
                                       _namespaces_49)
                                (if (null? _attrs_47)
                                  (values _proper-attrs_48 _namespaces_49)
                                  (if (eq? _xmlns_38 (caar _attrs_47))
                                    (_loop_46
                                      (cdr _attrs_47)
                                      _proper-attrs_48
                                      (if (equal? "" (cdar _attrs_47))
                                        (cons (cons* '*DEFAULT* #f #f)
                                              _namespaces_49)
                                        (_add-ns_41
                                          _port_43
                                          '*DEFAULT*
                                          (cdar _attrs_47)
                                          _namespaces_49)))
                                    (if (if (pair? (caar _attrs_47))
                                          (eq? _xmlns_38 (caaar _attrs_47))
                                          #f)
                                      (_loop_46
                                        (cdr _attrs_47)
                                        _proper-attrs_48
                                        (_add-ns_41
                                          _port_43
                                          (cdaar _attrs_47)
                                          (cdar _attrs_47)
                                          _namespaces_49))
                                      (_loop_46
                                        (cdr _attrs_47)
                                        (cons (car _attrs_47) _proper-attrs_48)
                                        _namespaces_49)))))))
                     _loop_46)
                   _attrs_44
                   '()
                   _namespaces_45)))
              (_add-ns_41
                (lambda (_port_43 _prefix_44 _uri-str_45 _namespaces_46)
                  (begin
                    (if (equal? "" _uri-str_45)
                      (parser-error
                        _port_43
                        "[dt-NSName] broken for "
                        _prefix_44)
                      #f)
                    ((lambda (_uri-symbol_47)
                       ((letrec ((_loop_48
                                   (lambda (_nss_49)
                                     (if (null? _nss_49)
                                       (cons (cons* _prefix_44
                                                    _uri-symbol_47
                                                    _uri-symbol_47)
                                             _namespaces_46)
                                       (if (eq? _uri-symbol_47 (cddar _nss_49))
                                         (cons (cons* _prefix_44
                                                      (cadar _nss_49)
                                                      _uri-symbol_47)
                                               _namespaces_46)
                                         (_loop_48 (cdr _nss_49)))))))
                          _loop_48)
                        _namespaces_46))
                     (ssax:uri-string->symbol _uri-str_45)))))
              (_validate-attrs_42
                (lambda (_port_43 _attlist_44 _decl-attrs_45)
                  (letrec ((_add-default-decl_46
                             (lambda (_decl-attr_47 _result_48)
                               (call-with-values
                                 (lambda () (apply values _decl-attr_47))
                                 (lambda (_attr-name_49
                                          _content-type_50
                                          _use-type_51
                                          _default-value_52)
                                   (begin
                                     (if (eq? _use-type_51 'REQUIRED)
                                       (parser-error
                                         _port_43
                                         "[RequiredAttr] broken for"
                                         _attr-name_49)
                                       #f)
                                     (if _default-value_52
                                       (cons (cons _attr-name_49
                                                   _default-value_52)
                                             _result_48)
                                       _result_48)))))))
                    ((letrec ((_loop_47
                                (lambda (_attlist_48 _decl-attrs_49 _result_50)
                                  (if (attlist-null? _attlist_48)
                                    (attlist-fold
                                      _add-default-decl_46
                                      _result_50
                                      _decl-attrs_49)
                                    (call-with-values
                                      (lambda ()
                                        (attlist-remove-top _attlist_48))
                                      (lambda (_attr_51 _attr-others_52)
                                        (call-with-values
                                          (lambda ()
                                            (if (attlist-null? _decl-attrs_49)
                                              (values _largest-dummy-decl-attr_39
                                                      _decl-attrs_49)
                                              (attlist-remove-top
                                                _decl-attrs_49)))
                                          (lambda (_decl-attr_53
                                                   _other-decls_54)
                                            ((lambda (_key_55)
                                               (if (if (_eqv?_7 _key_55 '<)
                                                     #t
                                                     #f)
                                                 (if ((lambda (_x_56)
                                                        (if _x_56
                                                          _x_56
                                                          (if (pair? (car _attr_51))
                                                            (eq? _xmlns_38
                                                                 (caar _attr_51))
                                                            #f)))
                                                      (eq? _xmlns_38
                                                           (car _attr_51)))
                                                   (_loop_47
                                                     _attr-others_52
                                                     _decl-attrs_49
                                                     (cons _attr_51
                                                           _result_50))
                                                   (parser-error
                                                     _port_43
                                                     "[ValueType] broken for "
                                                     _attr_51))
                                                 (if (if (_eqv?_7 _key_55 '>)
                                                       #t
                                                       #f)
                                                   (_loop_47
                                                     _attlist_48
                                                     _other-decls_54
                                                     (_add-default-decl_46
                                                       _decl-attr_53
                                                       _result_50))
                                                   (call-with-values
                                                     (lambda ()
                                                       (apply values
                                                              _decl-attr_53))
                                                     (lambda (_attr-name_56
                                                              _content-type_57
                                                              _use-type_58
                                                              _default-value_59)
                                                       (begin
                                                         (if (eq? _use-type_58
                                                                  'FIXED)
                                                           ((lambda (_x_60)
                                                              (if _x_60
                                                                _x_60
                                                                (parser-error
                                                                  _port_43
                                                                  "[FixedAttr] broken for "
                                                                  _attr-name_56)))
                                                            (equal? (cdr _attr_51)
                                                                    _default-value_59))
                                                           (if (eq? _content-type_57
                                                                    'CDATA)
                                                             #t
                                                             (if (pair? _content-type_57)
                                                               ((lambda (_x_60)
                                                                  (if _x_60
                                                                    _x_60
                                                                    (parser-error
                                                                      _port_43
                                                                      "[enum] broken for "
                                                                      _attr-name_56
                                                                      "="
                                                                      (cdr _attr_51))))
                                                                (member (cdr _attr_51)
                                                                        _content-type_57))
                                                               (ssax:warn
                                                                 _port_43
                                                                 "declared content type "
                                                                 _content-type_57
                                                                 " not verified yet"))))
                                                         (_loop_47
                                                           _attr-others_52
                                                           _other-decls_54
                                                           (cons _attr_51
                                                                 _result_50))))))))
                                             (name-compare
                                               (car _attr_51)
                                               (car _decl-attr_53)))))))))))
                       _loop_47)
                     _attlist_44
                     _decl-attrs_45
                     '())))))
       (lambda (_tag-head_43 _port_44 _elems_45 _entities_46 _namespaces_47)
         ((lambda (_attlist_48)
            ((lambda (_empty-el-tag?_49)
               (call-with-values
                 (lambda ()
                   (if _elems_45
                     ((lambda (_tmp_50)
                        (if _tmp_50
                          ((lambda (_decl-elem_51)
                             (values (if _empty-el-tag?_49
                                       'EMPTY-TAG
                                       (cadr _decl-elem_51))
                                     (caddr _decl-elem_51)))
                           _tmp_50)
                          (parser-error
                            _port_44
                            "[elementvalid] broken, no decl for "
                            _tag-head_43)))
                      (assoc _tag-head_43 _elems_45))
                     (values (if _empty-el-tag?_49 'EMPTY-TAG 'ANY) #f)))
                 (lambda (_elem-content_50 _decl-attrs_51)
                   ((lambda (_merged-attrs_52)
                      (call-with-values
                        (lambda ()
                          (_adjust-namespace-decl_40
                            _port_44
                            _merged-attrs_52
                            _namespaces_47))
                        (lambda (_proper-attrs_53 _namespaces_54)
                          (values (ssax:resolve-name
                                    _port_44
                                    _tag-head_43
                                    _namespaces_54
                                    #t)
                                  (fold-right
                                    (lambda (_name-value_55 _attlist_56)
                                      ((lambda (_x_57)
                                         (if _x_57
                                           _x_57
                                           (parser-error
                                             _port_44
                                             "[uniqattspec] after NS expansion broken for "
                                             _name-value_55)))
                                       (attlist-add
                                         _attlist_56
                                         (cons (ssax:resolve-name
                                                 _port_44
                                                 (car _name-value_55)
                                                 _namespaces_54
                                                 #f)
                                               (cdr _name-value_55)))))
                                    (make-empty-attlist)
                                    _proper-attrs_53)
                                  _namespaces_54
                                  _elem-content_50))))
                    (if _decl-attrs_51
                      (_validate-attrs_42 _port_44 _attlist_48 _decl-attrs_51)
                      (attlist->alist _attlist_48))))))
             (begin
               (ssax:skip-S _port_44)
               (if (eqv? #\/
                         (assert-curr-char
                           '(#\> #\/)
                           "XML [40], XML [44], no '>'"
                           _port_44))
                 (assert-curr-char '(#\>) "XML [44], no '>'" _port_44)
                 #f))))
          (ssax:read-attributes _port_44 _entities_46)))))
   (string->symbol "xmlns")
   (list ssax:largest-unres-name #f #f #f)))
((lambda (_urn-a_44)
   ((lambda (_urn-b_45)
      ((lambda (_urn-html_46)
         ((lambda (_namespaces_47)
            ((lambda (_test_48)
               (begin
                 (assert (equal? (_cons_23
                                   (if (string? '"TAG1")
                                     (string->symbol '"TAG1")
                                     '"TAG1")
                                   (_cons_23
                                     '()
                                     (_cons_23 _namespaces_47 '(ANY))))
                                 (_test_48 "TAG1" #f ">")))
                 (assert (equal? (_cons_23
                                   (if (string? '"TAG1")
                                     (string->symbol '"TAG1")
                                     '"TAG1")
                                   (_cons_23
                                     '()
                                     (_cons_23 _namespaces_47 '(EMPTY-TAG))))
                                 (_test_48 "TAG1" #f "/>")))
                 (assert (equal? (_cons_23
                                   (if (string? '"TAG1")
                                     (string->symbol '"TAG1")
                                     '"TAG1")
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"a"))
                                     (_cons_23 _namespaces_47 '(EMPTY-TAG))))
                                 (_test_48 "TAG1" #f "HREF='a'/>")))
                 (assert (equal? (_cons_23
                                   (_cons_23
                                     (if (string? '"UA")
                                       (string->symbol '"UA")
                                       '"UA")
                                     (if (string? '"TAG1")
                                       (string->symbol '"TAG1")
                                       '"TAG1"))
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"a"))
                                     (_cons_23
                                       (cons (_cons_23
                                               '*DEFAULT*
                                               (_cons_23
                                                 (if (string? '"UA")
                                                   (string->symbol '"UA")
                                                   '"UA")
                                                 _urn-a_44))
                                             _namespaces_47)
                                       '(ANY))))
                                 (_test_48
                                   "TAG1"
                                   #f
                                   "HREF='a' xmlns='urn:a'>")))
                 (assert (equal? (_cons_23
                                   (if (string? '"TAG1")
                                     (string->symbol '"TAG1")
                                     '"TAG1")
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"a"))
                                     (_cons_23
                                       (cons '(*DEFAULT* #f . #f)
                                             _namespaces_47)
                                       '(ANY))))
                                 (_test_48 "TAG1" #f "HREF='a' xmlns=''>")))
                 (assert (failed? (_test_48
                                    "UA:TAG1"
                                    #f
                                    "HREF='a' xmlns=''/>")))
                 (assert (equal? (_cons_23
                                   (_cons_23
                                     (if (string? '"UA")
                                       (string->symbol '"UA")
                                       '"UA")
                                     (if (string? '"TAG1")
                                       (string->symbol '"TAG1")
                                       '"TAG1"))
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (_cons_23
                                           (if (string? '"UA")
                                             (string->symbol '"UA")
                                             '"UA")
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF"))
                                         '"a"))
                                     (_cons_23
                                       (cons '(*DEFAULT* #f . #f)
                                             _namespaces_47)
                                       '(ANY))))
                                 (_test_48
                                   "A:TAG1"
                                   #f
                                   "A:HREF='a' xmlns=''>")))
                 (assert (equal? (_cons_23
                                   (_cons_23
                                     (if (string? '"UA")
                                       (string->symbol '"UA")
                                       '"UA")
                                     (if (string? '"TAG1")
                                       (string->symbol '"TAG1")
                                       '"TAG1"))
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (_cons_23
                                           (if (string? '"UA")
                                             (string->symbol '"UA")
                                             '"UA")
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF"))
                                         '"a"))
                                     (_cons_23
                                       (cons (_cons_23
                                               '*DEFAULT*
                                               (_cons_23 _urn-b_45 _urn-b_45))
                                             _namespaces_47)
                                       '(ANY))))
                                 (_test_48
                                   "A:TAG1"
                                   #f
                                   "A:HREF='a' xmlns='urn:b'>")))
                 (assert (failed? (_test_48
                                    "B:TAG1"
                                    #f
                                    "A:HREF='a' xmlns:b=''/>")))
                 (assert (equal? (_cons_23
                                   (_cons_23
                                     _urn-b_45
                                     (if (string? '"TAG1")
                                       (string->symbol '"TAG1")
                                       '"TAG1"))
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (_cons_23
                                           (if (string? '"UA")
                                             (string->symbol '"UA")
                                             '"UA")
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF"))
                                         '"a"))
                                     (_cons_23
                                       (cons (_cons_23
                                               (if (string? '"B")
                                                 (string->symbol '"B")
                                                 '"B")
                                               (_cons_23 _urn-b_45 _urn-b_45))
                                             _namespaces_47)
                                       '(ANY))))
                                 (_test_48
                                   "B:TAG1"
                                   #f
                                   "A:HREF='a' xmlns:B='urn:b'>")))
                 (assert (equal? (_cons_23
                                   (_cons_23
                                     _urn-b_45
                                     (if (string? '"TAG1")
                                       (string->symbol '"TAG1")
                                       '"TAG1"))
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (_cons_23
                                           (if (string? '"UA")
                                             (string->symbol '"UA")
                                             '"UA")
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF"))
                                         '"a")
                                       (_cons_23
                                         (_cons_23
                                           _urn-b_45
                                           (if (string? '"SRC")
                                             (string->symbol '"SRC")
                                             '"SRC"))
                                         '"b"))
                                     (_cons_23
                                       (cons (_cons_23
                                               (if (string? '"B")
                                                 (string->symbol '"B")
                                                 '"B")
                                               (_cons_23 _urn-b_45 _urn-b_45))
                                             _namespaces_47)
                                       '(ANY))))
                                 (_test_48
                                   "B:TAG1"
                                   #f
                                   "B:SRC='b' A:HREF='a' xmlns:B='urn:b'>")))
                 (assert (equal? (_cons_23
                                   (_cons_23
                                     _urn-b_45
                                     (if (string? '"TAG1")
                                       (string->symbol '"TAG1")
                                       '"TAG1"))
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (_cons_23
                                           (if (string? '"UA")
                                             (string->symbol '"UA")
                                             '"UA")
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF"))
                                         '"a")
                                       (_cons_23
                                         (_cons_23
                                           _urn-b_45
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF"))
                                         '"b"))
                                     (_cons_23
                                       (cons (_cons_23
                                               (if (string? '"B")
                                                 (string->symbol '"B")
                                                 '"B")
                                               (_cons_23 _urn-b_45 _urn-b_45))
                                             _namespaces_47)
                                       '(ANY))))
                                 (_test_48
                                   "B:TAG1"
                                   #f
                                   "B:HREF=\"b\" A:HREF='a' xmlns:B='urn:b'>")))
                 (assert (failed? (_test_48
                                    "B:TAG1"
                                    #f
                                    "HREF=\"b\" HREF='a' xmlns:B='urn:a'/>")))
                 (assert (failed? (_test_48
                                    "B:TAG1"
                                    #f
                                    "B:HREF=\"b\" A:HREF='a' xmlns:B='urn:a'/>")))
                 (assert (equal? (_cons_23
                                   (_cons_23
                                     (if (string? '"UA")
                                       (string->symbol '"UA")
                                       '"UA")
                                     (if (string? '"TAG1")
                                       (string->symbol '"TAG1")
                                       '"TAG1"))
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"a")
                                       (_cons_23
                                         (_cons_23
                                           (if (string? '"UA")
                                             (string->symbol '"UA")
                                             '"UA")
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF"))
                                         '"b"))
                                     (_cons_23
                                       (cons (_cons_23
                                               '*DEFAULT*
                                               (_cons_23
                                                 (if (string? '"UA")
                                                   (string->symbol '"UA")
                                                   '"UA")
                                                 _urn-a_44))
                                             _namespaces_47)
                                       '(ANY))))
                                 (_test_48
                                   "TAG1"
                                   #f
                                   "A:HREF=\"b\" HREF='a' xmlns='urn:a'>")))
                 (assert (equal? (_cons_23
                                   (if (string? '"TAG1")
                                     (string->symbol '"TAG1")
                                     '"TAG1")
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (_cons_23
                                           (if (string? '"UHTML")
                                             (string->symbol '"UHTML")
                                             '"UHTML")
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF"))
                                         '"a")
                                       (_cons_23
                                         (_cons_23
                                           _urn-b_45
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF"))
                                         '"b"))
                                     (_cons_23
                                       (append (_list_25
                                                 (_cons_23
                                                   (if (string? '"HTML")
                                                     (string->symbol '"HTML")
                                                     '"HTML")
                                                   (_cons_23
                                                     (if (string? '"UHTML")
                                                       (string->symbol
                                                         '"UHTML")
                                                       '"UHTML")
                                                     _urn-html_46))
                                                 (_cons_23
                                                   (if (string? '"B")
                                                     (string->symbol '"B")
                                                     '"B")
                                                   (_cons_23
                                                     _urn-b_45
                                                     _urn-b_45)))
                                               _namespaces_47)
                                       '(ANY))))
                                 (_test_48
                                   "TAG1"
                                   #f
                                   "B:HREF=\"b\" xmlns:B='urn:b' xmlns:HTML='http://w3c.org/html' HTML:HREF='a' >")))
                 (assert (failed? (_test_48
                                    "TAG1"
                                    '((TAG2 ANY ()))
                                    "B:HREF='b' xmlns:B='urn:b'>")))
                 (assert (failed? (_test_48
                                    "TAG1"
                                    (_list_25
                                      (_cons_23
                                        (if (string? '"TAG1")
                                          (string->symbol '"TAG1")
                                          '"TAG1")
                                        '(ANY ())))
                                    "B:HREF='b' xmlns:B='urn:b'>")))
                 (assert (failed? (_test_48
                                    "TAG1"
                                    (_list_25
                                      (_list_25
                                        (if (string? '"TAG1")
                                          (string->symbol '"TAG1")
                                          '"TAG1")
                                        'ANY
                                        (_list_25
                                          (_cons_23
                                            (if (string? '"HREF1")
                                              (string->symbol '"HREF1")
                                              '"HREF1")
                                            '(CDATA IMPLIED #f)))))
                                    "B:HREF='b' xmlns:B='urn:b'>")))
                 (assert (equal? (_cons_23
                                   (if (string? '"TAG1")
                                     (string->symbol '"TAG1")
                                     '"TAG1")
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"b"))
                                     (_cons_23 _namespaces_47 '(EMPTY-TAG))))
                                 (_test_48
                                   "TAG1"
                                   (_list_25
                                     (_list_25
                                       (if (string? '"TAG1")
                                         (string->symbol '"TAG1")
                                         '"TAG1")
                                       'PCDATA
                                       (_list_25
                                         (_cons_23
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF")
                                           '(CDATA REQUIRED #f)))))
                                   "HREF='b'/>")))
                 (assert (equal? (_cons_23
                                   (if (string? '"TAG1")
                                     (string->symbol '"TAG1")
                                     '"TAG1")
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"b"))
                                     (_cons_23 _namespaces_47 '(PCDATA))))
                                 (_test_48
                                   "TAG1"
                                   (_list_25
                                     (_list_25
                                       (if (string? '"TAG1")
                                         (string->symbol '"TAG1")
                                         '"TAG1")
                                       'PCDATA
                                       (_list_25
                                         (_cons_23
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF")
                                           '(CDATA REQUIRED #f)))))
                                   "HREF='b'>")))
                 (assert (failed? (_test_48
                                    "TAG1"
                                    (_list_25
                                      (_list_25
                                        (if (string? '"TAG1")
                                          (string->symbol '"TAG1")
                                          '"TAG1")
                                        'PCDATA
                                        (_list_25
                                          (_cons_23
                                            (if (string? '"HREF")
                                              (string->symbol '"HREF")
                                              '"HREF")
                                            '(CDATA REQUIRED #f)))))
                                    ">")))
                 (assert (failed? (_test_48
                                    "TAG1"
                                    (_list_25
                                      (_list_25
                                        (if (string? '"TAG1")
                                          (string->symbol '"TAG1")
                                          '"TAG1")
                                        'PCDATA
                                        (_list_25
                                          (_cons_23
                                            (if (string? '"HREF")
                                              (string->symbol '"HREF")
                                              '"HREF")
                                            '(("c") REQUIRED #f)))))
                                    "HREF='b'>")))
                 (assert (equal? (_cons_23
                                   (if (string? '"TAG1")
                                     (string->symbol '"TAG1")
                                     '"TAG1")
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"b"))
                                     (_cons_23 _namespaces_47 '(PCDATA))))
                                 (_test_48
                                   "TAG1"
                                   (_list_25
                                     (_list_25
                                       (if (string? '"TAG1")
                                         (string->symbol '"TAG1")
                                         '"TAG1")
                                       'PCDATA
                                       (_list_25
                                         (_cons_23
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF")
                                           '(("c" "b") IMPLIED #f)))))
                                   "HREF='b'>")))
                 (assert (equal? (_cons_23
                                   (if (string? '"TAG1")
                                     (string->symbol '"TAG1")
                                     '"TAG1")
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"b"))
                                     (_cons_23 _namespaces_47 '(PCDATA))))
                                 (_test_48
                                   "TAG1"
                                   (_list_25
                                     (_list_25
                                       (if (string? '"TAG1")
                                         (string->symbol '"TAG1")
                                         '"TAG1")
                                       'PCDATA
                                       (_list_25
                                         (_cons_23
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF")
                                           '(CDATA IMPLIED "c")))))
                                   "HREF='b'>")))
                 (assert (failed? (_test_48
                                    "TAG1"
                                    (_list_25
                                      (_list_25
                                        (if (string? '"TAG1")
                                          (string->symbol '"TAG1")
                                          '"TAG1")
                                        'PCDATA
                                        (_list_25
                                          (_cons_23
                                            (if (string? '"HREF")
                                              (string->symbol '"HREF")
                                              '"HREF")
                                            '(CDATA FIXED "c")))))
                                    "HREF='b'>")))
                 (assert (equal? (_cons_23
                                   (if (string? '"TAG1")
                                     (string->symbol '"TAG1")
                                     '"TAG1")
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"b"))
                                     (_cons_23 _namespaces_47 '(PCDATA))))
                                 (_test_48
                                   "TAG1"
                                   (_list_25
                                     (_list_25
                                       (if (string? '"TAG1")
                                         (string->symbol '"TAG1")
                                         '"TAG1")
                                       'PCDATA
                                       (_list_25
                                         (_cons_23
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF")
                                           '(CDATA FIXED "b")))))
                                   "HREF='b'>")))
                 (assert (equal? (_cons_23
                                   (if (string? '"TAG1")
                                     (string->symbol '"TAG1")
                                     '"TAG1")
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"b"))
                                     (_cons_23 _namespaces_47 '(PCDATA))))
                                 (_test_48
                                   "TAG1"
                                   (_list_25
                                     (_list_25
                                       (if (string? '"TAG1")
                                         (string->symbol '"TAG1")
                                         '"TAG1")
                                       'PCDATA
                                       (_list_25
                                         (_cons_23
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF")
                                           '(CDATA FIXED "b")))))
                                   ">")))
                 (assert (equal? (_cons_23
                                   (if (string? '"TAG1")
                                     (string->symbol '"TAG1")
                                     '"TAG1")
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"b"))
                                     (_cons_23 _namespaces_47 '(PCDATA))))
                                 (_test_48
                                   "TAG1"
                                   (_list_25
                                     (_list_25
                                       (if (string? '"TAG1")
                                         (string->symbol '"TAG1")
                                         '"TAG1")
                                       'PCDATA
                                       (_list_25
                                         (_cons_23
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF")
                                           '(CDATA IMPLIED "b")))))
                                   ">")))
                 (assert (equal? (_cons_23
                                   (if (string? '"TAG1")
                                     (string->symbol '"TAG1")
                                     '"TAG1")
                                   (_cons_23
                                     '()
                                     (_cons_23 _namespaces_47 '(PCDATA))))
                                 (_test_48
                                   "TAG1"
                                   (_list_25
                                     (_list_25
                                       (if (string? '"TAG1")
                                         (string->symbol '"TAG1")
                                         '"TAG1")
                                       'PCDATA
                                       (_list_25
                                         (_cons_23
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF")
                                           '(CDATA IMPLIED #f)))))
                                   ">")))
                 (assert (failed? (_test_48
                                    "TAG1"
                                    (_list_25
                                      (_list_25
                                        (if (string? '"TAG1")
                                          (string->symbol '"TAG1")
                                          '"TAG1")
                                        'PCDATA
                                        (_list_25
                                          (_cons_23
                                            (_cons_23
                                              (if (string? '"A")
                                                (string->symbol '"A")
                                                '"A")
                                              (if (string? '"HREF")
                                                (string->symbol '"HREF")
                                                '"HREF"))
                                            '(CDATA IMPLIED "c")))))
                                    "HREF='b'>")))
                 (assert (equal? (_cons_23
                                   (if (string? '"TAG1")
                                     (string->symbol '"TAG1")
                                     '"TAG1")
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"b")
                                       (_cons_23
                                         (_cons_23
                                           (if (string? '"UA")
                                             (string->symbol '"UA")
                                             '"UA")
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF"))
                                         '"c"))
                                     (_cons_23 _namespaces_47 '(PCDATA))))
                                 (_test_48
                                   "TAG1"
                                   (_list_25
                                     (_list_25
                                       (if (string? '"TAG1")
                                         (string->symbol '"TAG1")
                                         '"TAG1")
                                       'PCDATA
                                       (_list_25
                                         (_cons_23
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF")
                                           '(CDATA REQUIRED #f))
                                         (_cons_23
                                           (_cons_23
                                             (if (string? '"A")
                                               (string->symbol '"A")
                                               '"A")
                                             (if (string? '"HREF")
                                               (string->symbol '"HREF")
                                               '"HREF"))
                                           '(CDATA IMPLIED "c")))))
                                   "HREF='b'>")))
                 (assert (equal? (_cons_23
                                   (_cons_23
                                     (if (string? '"UA")
                                       (string->symbol '"UA")
                                       '"UA")
                                     (if (string? '"TAG1")
                                       (string->symbol '"TAG1")
                                       '"TAG1"))
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"b")
                                       (_cons_23
                                         (_cons_23
                                           (if (string? '"UA")
                                             (string->symbol '"UA")
                                             '"UA")
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF"))
                                         '"c"))
                                     (_cons_23 _namespaces_47 '(PCDATA))))
                                 (_test_48
                                   "A:TAG1"
                                   (_list_25
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"A")
                                           (string->symbol '"A")
                                           '"A")
                                         (if (string? '"TAG1")
                                           (string->symbol '"TAG1")
                                           '"TAG1"))
                                       'PCDATA
                                       (_list_25
                                         (_cons_23
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF")
                                           '(NMTOKEN REQUIRED #f))
                                         (_cons_23
                                           (_cons_23
                                             (if (string? '"A")
                                               (string->symbol '"A")
                                               '"A")
                                             (if (string? '"HREF")
                                               (string->symbol '"HREF")
                                               '"HREF"))
                                           '(CDATA IMPLIED "c")))))
                                   "HREF='b'>")))
                 (assert (equal? (_cons_23
                                   (_cons_23
                                     _urn-b_45
                                     (if (string? '"TAG1")
                                       (string->symbol '"TAG1")
                                       '"TAG1"))
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"b"))
                                     (_cons_23
                                       (cons (_cons_23
                                               (if (string? '"B")
                                                 (string->symbol '"B")
                                                 '"B")
                                               (_cons_23 _urn-b_45 _urn-b_45))
                                             _namespaces_47)
                                       '(PCDATA))))
                                 (_test_48
                                   "B:TAG1"
                                   (_list_25
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"B")
                                           (string->symbol '"B")
                                           '"B")
                                         (if (string? '"TAG1")
                                           (string->symbol '"TAG1")
                                           '"TAG1"))
                                       'PCDATA
                                       (_list_25
                                         (_cons_23
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF")
                                           '(CDATA REQUIRED #f))
                                         (_cons_23
                                           (_cons_23
                                             (if (string? '"xmlns")
                                               (string->symbol '"xmlns")
                                               '"xmlns")
                                             (if (string? '"B")
                                               (string->symbol '"B")
                                               '"B"))
                                           '(CDATA IMPLIED "urn:b")))))
                                   "HREF='b'>")))
                 (assert (equal? (_cons_23
                                   (_cons_23
                                     _urn-b_45
                                     (if (string? '"TAG1")
                                       (string->symbol '"TAG1")
                                       '"TAG1"))
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (_cons_23
                                           _urn-b_45
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF"))
                                         '"b"))
                                     (_cons_23
                                       (cons (_cons_23
                                               (if (string? '"B")
                                                 (string->symbol '"B")
                                                 '"B")
                                               (_cons_23 _urn-b_45 _urn-b_45))
                                             _namespaces_47)
                                       '(PCDATA))))
                                 (_test_48
                                   "B:TAG1"
                                   (_list_25
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"B")
                                           (string->symbol '"B")
                                           '"B")
                                         (if (string? '"TAG1")
                                           (string->symbol '"TAG1")
                                           '"TAG1"))
                                       'PCDATA
                                       (_list_25
                                         (_cons_23
                                           (_cons_23
                                             (if (string? '"B")
                                               (string->symbol '"B")
                                               '"B")
                                             (if (string? '"HREF")
                                               (string->symbol '"HREF")
                                               '"HREF"))
                                           '(CDATA REQUIRED #f))
                                         (_cons_23
                                           (_cons_23
                                             (if (string? '"xmlns")
                                               (string->symbol '"xmlns")
                                               '"xmlns")
                                             (if (string? '"B")
                                               (string->symbol '"B")
                                               '"B"))
                                           '(CDATA IMPLIED "urn:b")))))
                                   "B:HREF='b'>")))
                 (assert (equal? (_cons_23
                                   (_cons_23
                                     _urn-b_45
                                     (if (string? '"TAG1")
                                       (string->symbol '"TAG1")
                                       '"TAG1"))
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"b"))
                                     (_cons_23
                                       (cons (_cons_23
                                               '*DEFAULT*
                                               (_cons_23 _urn-b_45 _urn-b_45))
                                             _namespaces_47)
                                       '(PCDATA))))
                                 (_test_48
                                   "TAG1"
                                   (_list_25
                                     (_list_25
                                       (if (string? '"TAG1")
                                         (string->symbol '"TAG1")
                                         '"TAG1")
                                       'PCDATA
                                       (_list_25
                                         (_cons_23
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF")
                                           '(CDATA REQUIRED #f))
                                         (_cons_23
                                           (if (string? '"xmlns")
                                             (string->symbol '"xmlns")
                                             '"xmlns")
                                           '(CDATA IMPLIED "urn:b")))))
                                   "HREF='b'>")))
                 (assert (equal? (_cons_23
                                   (_cons_23
                                     _urn-b_45
                                     (if (string? '"TAG1")
                                       (string->symbol '"TAG1")
                                       '"TAG1"))
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"HREF")
                                           (string->symbol '"HREF")
                                           '"HREF")
                                         '"b"))
                                     (_cons_23
                                       (cons (_cons_23
                                               '*DEFAULT*
                                               (_cons_23 _urn-b_45 _urn-b_45))
                                             _namespaces_47)
                                       '(PCDATA))))
                                 (_test_48
                                   "TAG1"
                                   (_list_25
                                     (_list_25
                                       (if (string? '"TAG1")
                                         (string->symbol '"TAG1")
                                         '"TAG1")
                                       'PCDATA
                                       (_list_25
                                         (_cons_23
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF")
                                           '(CDATA REQUIRED #f)))))
                                   "HREF='b' xmlns='urn:b'>")))
                 (assert (equal? (_cons_23
                                   (_cons_23
                                     _urn-b_45
                                     (if (string? '"TAG1")
                                       (string->symbol '"TAG1")
                                       '"TAG1"))
                                   (_cons_23
                                     (_list_25
                                       (_cons_23
                                         (_cons_23
                                           _urn-b_45
                                           (if (string? '"HREF")
                                             (string->symbol '"HREF")
                                             '"HREF"))
                                         '"b"))
                                     (_cons_23
                                       (cons (_cons_23
                                               (if (string? '"B")
                                                 (string->symbol '"B")
                                                 '"B")
                                               (_cons_23 _urn-b_45 _urn-b_45))
                                             _namespaces_47)
                                       '(PCDATA))))
                                 (_test_48
                                   "B:TAG1"
                                   (_list_25
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"B")
                                           (string->symbol '"B")
                                           '"B")
                                         (if (string? '"TAG1")
                                           (string->symbol '"TAG1")
                                           '"TAG1"))
                                       'PCDATA
                                       (_list_25
                                         (_cons_23
                                           (_cons_23
                                             (if (string? '"B")
                                               (string->symbol '"B")
                                               '"B")
                                             (if (string? '"HREF")
                                               (string->symbol '"HREF")
                                               '"HREF"))
                                           '(CDATA REQUIRED #f)))))
                                   "B:HREF='b' xmlns:B='urn:b'>")))))
             (lambda (_tag-head-name_48 _elems_49 _str_50)
               (call-with-input-string
                 _str_50
                 (lambda (_port_51)
                   (call-with-values
                     (lambda ()
                       (ssax:complete-start-tag
                         (call-with-input-string
                           _tag-head-name_48
                           (lambda (_port_52) (ssax:read-QName _port_52)))
                         _port_51
                         _elems_49
                         '()
                         _namespaces_47))
                     list))))))
          (_list_25
            (_cons_23
              '#f
              (_cons_23
                (if (string? '"UHTML") (string->symbol '"UHTML") '"UHTML")
                _urn-html_46))
            (_cons_23
              (if (string? '"A") (string->symbol '"A") '"A")
              (_cons_23
                (if (string? '"UA") (string->symbol '"UA") '"UA")
                _urn-a_44)))))
       (string->symbol "http://w3c.org/html")))
    (string->symbol "urn:b")))
 (string->symbol "urn:a"))
(define ssax:read-external-id
  (lambda (_port_38)
    ((lambda (_discriminator_39)
       (begin
         (assert-curr-char
           ssax:S-chars
           "space after SYSTEM or PUBLIC"
           _port_38)
         (ssax:skip-S _port_38)
         ((lambda (_delimiter_40)
            (if (eq? _discriminator_39 (string->symbol "SYSTEM"))
              ((lambda (_val_41) (begin (read-char _port_38) _val_41))
               (next-token '() (list _delimiter_40) "XML [11]" _port_38))
              (if (eq? _discriminator_39 (string->symbol "PUBLIC"))
                (begin
                  (skip-until (list _delimiter_40) _port_38)
                  (assert-curr-char
                    ssax:S-chars
                    "space after PubidLiteral"
                    _port_38)
                  (ssax:skip-S _port_38)
                  ((lambda (_delimiter_41)
                     ((lambda (_systemid_42)
                        (begin (read-char _port_38) _systemid_42))
                      (next-token
                        '()
                        (list _delimiter_41)
                        "XML [11]"
                        _port_38)))
                   (assert-curr-char '(#\' #\") "XML [11]" _port_38)))
                (parser-error
                  _port_38
                  "XML [75], "
                  _discriminator_39
                  " rather than SYSTEM or PUBLIC"))))
          (assert-curr-char '(#\' #\") "XML [11], XML [12]" _port_38))))
     (ssax:read-NCName _port_38))))
(define ssax:scan-Misc
  (lambda (_port_38)
    ((letrec ((_loop_39
                (lambda (_c_40)
                  (if (eof-object? _c_40)
                    _c_40
                    (if (not (char=? _c_40 #\<))
                      (parser-error
                        _port_38
                        "XML [22], char '"
                        _c_40
                        "' unexpected")
                      ((lambda (_token_41)
                         ((lambda (_key_42)
                            (if (if (_eqv?_7 _key_42 'COMMENT) #t #f)
                              (_loop_39 (ssax:skip-S _port_38))
                              (if (if (_eqv?_7 _key_42 'PI)
                                    #t
                                    (if (_eqv?_7 _key_42 'DECL)
                                      #t
                                      (if (_eqv?_7 _key_42 'START) #t #f)))
                                _token_41
                                (parser-error
                                  _port_38
                                  "XML [22], unexpected token of kind "
                                  (car _token_41)))))
                          (car _token_41)))
                       (ssax:read-markup-token _port_38)))))))
       _loop_39)
     (ssax:skip-S _port_38))))
(define ssax:read-char-data
  ((lambda (_terminators-usual_38
            _terminators-usual-eof_39
            _handle-fragment_40)
     (lambda (_port_41 _expect-eof?_42 _str-handler_43 _seed_44)
       (if (eqv? #\< (peek-char _port_41))
         ((lambda (_token_45)
            ((lambda (_key_46)
               (if (if (_eqv?_7 _key_46 'START)
                     #t
                     (if (_eqv?_7 _key_46 'END) #t #f))
                 (values _seed_44 _token_45)
                 (if (if (_eqv?_7 _key_46 'CDSECT) #t #f)
                   ((lambda (_seed_47)
                      (ssax:read-char-data
                        _port_41
                        _expect-eof?_42
                        _str-handler_43
                        _seed_47))
                    (ssax:read-cdata-body _port_41 _str-handler_43 _seed_44))
                   (if (if (_eqv?_7 _key_46 'COMMENT) #t #f)
                     (ssax:read-char-data
                       _port_41
                       _expect-eof?_42
                       _str-handler_43
                       _seed_44)
                     (values _seed_44 _token_45)))))
             (car _token_45)))
          (ssax:read-markup-token _port_41))
         ((lambda (_char-data-terminators_45)
            ((letrec ((_loop_46
                        (lambda (_seed_47)
                          ((lambda (_fragment_48)
                             ((lambda (_term-char_49)
                                (if (eof-object? _term-char_49)
                                  (values (_handle-fragment_40
                                            _fragment_48
                                            _str-handler_43
                                            _seed_47)
                                          _term-char_49)
                                  ((lambda (_key_50)
                                     (if (if (_eqv?_7 _key_50 '#\<) #t #f)
                                       ((lambda (_token_51)
                                          ((lambda (_key_52)
                                             (if (if (_eqv?_7 _key_52 'CDSECT)
                                                   #t
                                                   #f)
                                               (_loop_46
                                                 (ssax:read-cdata-body
                                                   _port_41
                                                   _str-handler_43
                                                   (_handle-fragment_40
                                                     _fragment_48
                                                     _str-handler_43
                                                     _seed_47)))
                                               (if (if (_eqv?_7 _key_52
                                                                'COMMENT)
                                                     #t
                                                     #f)
                                                 (_loop_46
                                                   (_handle-fragment_40
                                                     _fragment_48
                                                     _str-handler_43
                                                     _seed_47))
                                                 (values (_handle-fragment_40
                                                           _fragment_48
                                                           _str-handler_43
                                                           _seed_47)
                                                         _token_51))))
                                           (car _token_51)))
                                        (ssax:read-markup-token _port_41))
                                       (if (if (_eqv?_7 _key_50 '#\&) #t #f)
                                         ((lambda (_key_51)
                                            (if (if (_eqv?_7 _key_51 '#\#)
                                                  #t
                                                  #f)
                                              (begin
                                                (read-char _port_41)
                                                (_loop_46
                                                  (_str-handler_43
                                                    _fragment_48
                                                    (string (ssax:read-char-ref
                                                              _port_41))
                                                    _seed_47)))
                                              ((lambda (_name_52)
                                                 (begin
                                                   (assert-curr-char
                                                     '(#\;)
                                                     "XML [68]"
                                                     _port_41)
                                                   (values (_handle-fragment_40
                                                             _fragment_48
                                                             _str-handler_43
                                                             _seed_47)
                                                           (make-xml-token
                                                             'ENTITY-REF
                                                             _name_52))))
                                               (ssax:read-NCName _port_41))))
                                          (peek-next-char _port_41))
                                         (begin
                                           (if (eqv? (peek-next-char _port_41)
                                                     #\newline)
                                             (read-char _port_41))
                                           (_loop_46
                                             (_str-handler_43
                                               _fragment_48
                                               (string #\newline)
                                               _seed_47))))))
                                   _term-char_49)))
                              (peek-char _port_41)))
                           (next-token
                             '()
                             _char-data-terminators_45
                             "reading char data"
                             _port_41)))))
               _loop_46)
             _seed_44))
          (if _expect-eof?_42
            _terminators-usual-eof_39
            _terminators-usual_38)))))
   (list #\< #\& char-return)
   (list #\< '*eof* #\& char-return)
   (lambda (_fragment_38 _str-handler_39 _seed_40)
     (if (zero? (string-length _fragment_38))
       _seed_40
       (_str-handler_39 _fragment_38 "" _seed_40)))))
(letrec ((_test_44
           (lambda (_str_49
                    _expect-eof?_50
                    _expected-data_51
                    _expected-token_52)
             (begin
               (newline)
               (display "body: ")
               (write _str_49)
               (newline)
               (display "Result: ")
               (call-with-values
                 (lambda ()
                   (call-with-input-string
                     (unesc-string _str_49)
                     (lambda (_port_53)
                       (ssax:read-char-data
                         _port_53
                         _expect-eof?_50
                         _str-handler_45
                         '()))))
                 (lambda (_seed_53 _token_54)
                   ((lambda (_result_55)
                      (begin
                        (write _result_55)
                        (display " ")
                        (display _token_54)
                        (assert (equal? _result_55
                                        (map unesc-string _expected-data_51))
                                (if (eq? _expected-token_52 _eof-object_46)
                                  (eof-object? _token_54)
                                  (equal? _token_54 _expected-token_52)))))
                    (reverse _seed_53)))))))
         (_str-handler_45
           (lambda (_fragment_49 _foll-fragment_50 _seed_51)
             (if (zero? (string-length _foll-fragment_50))
               (cons _fragment_49 _seed_51)
               (cons* _foll-fragment_50 _fragment_49 _seed_51))))
         (_eof-object_46 (lambda () _eof-object_46))
         (_a-ref_47
           (make-xml-token
             (if (string? 'ENTITY-REF)
               (string->symbol 'ENTITY-REF)
               'ENTITY-REF)
             (string->symbol "lt")))
         (_a-tag_48
           (make-xml-token
             (if (string? 'START) (string->symbol 'START) 'START)
             (string->symbol "BR"))))
  (begin
    (_test_44 "" #t '() _eof-object_46)
    (assert (failed? (_test_44 "" #f '() _eof-object_46)))
    (_test_44 "  " #t '("  ") _eof-object_46)
    (_test_44 "<BR/>" #f '() _a-tag_48)
    (_test_44 " <BR  />" #f '(" ") _a-tag_48)
    (_test_44 " &lt;" #f '(" ") _a-ref_47)
    (_test_44 " a&lt;" #f '(" a") _a-ref_47)
    (_test_44 " a &lt;" #f '(" a ") _a-ref_47)
    (_test_44 " <!-- comment--> a  a<BR/>" #f '(" " " a  a") _a-tag_48)
    (_test_44 " <!-- comment-->%ra  a<BR/>" #f '(" " "" "%n" "a  a") _a-tag_48)
    (_test_44
      " <!-- comment-->%r%na  a<BR/>"
      #f
      '(" " "" "%n" "a  a")
      _a-tag_48)
    (_test_44
      " <!-- comment-->%r%na%t%r%r%na<BR/>"
      #f
      '(" " "" "%n" "a%t" "%n" "" "%n" "a")
      _a-tag_48)
    (_test_44 "a<!-- comment--> a  a<BR/>" #f '("a" " a  a") _a-tag_48)
    (_test_44 "&#x21;<BR/>" #f '("" "!") _a-tag_48)
    (_test_44 "&#x21;%n<BR/>" #f '("" "!" "%n") _a-tag_48)
    (_test_44 "%t&#x21;%n<BR/>" #f '("%t" "!" "%n") _a-tag_48)
    (_test_44 "%t&#x21;%na a<BR/>" #f '("%t" "!" "%na a") _a-tag_48)
    (_test_44 "%t&#x21;%ra a<BR/>" #f '("%t" "!" "" "%n" "a a") _a-tag_48)
    (_test_44 "%t&#x21;%r%na a<BR/>" #f '("%t" "!" "" "%n" "a a") _a-tag_48)
    (_test_44 " %ta &#x21;   b <BR/>" #f '(" %ta " "!" "   b ") _a-tag_48)
    (_test_44 " %ta &#x20;   b <BR/>" #f '(" %ta " " " "   b ") _a-tag_48)
    (_test_44 "<![CDATA[<]]><BR/>" #f '("<") _a-tag_48)
    (_test_44 "<![CDATA[]]]><BR/>" #f '("]") _a-tag_48)
    (_test_44 "%t<![CDATA[<]]><BR/>" #f '("%t" "<") _a-tag_48)
    (_test_44 "%t<![CDATA[<]]>a b<BR/>" #f '("%t" "<" "a b") _a-tag_48)
    (_test_44 "%t<![CDATA[<]]>  a b<BR/>" #f '("%t" "<" "  a b") _a-tag_48)
    (_test_44
      "%td <![CDATA[  <%r%r%n]]>  a b<BR/>"
      #f
      '("%td " "  <" "%n" "" "%n" "  a b")
      _a-tag_48)))
(define ssax:assert-token
  (lambda (_token_38 _kind_39 _gi_40 _error-cont_41)
    ((lambda (_x_42)
       (if _x_42 _x_42 (_error-cont_41 _token_38 _kind_39 _gi_40)))
     (if (xml-token? _token_38)
       (if (eq? _kind_39 (car _token_38)) (equal? _gi_40 (cdr _token_38)) #f)
       #f))))
(pp (lambda (_port_46 _target_47 _seed_48)
      ((lambda (_key_49)
         (begin
           (ssax:warn _port_46 "Skipping PI: " _target_47 nl)
           (ssax:skip-pi _port_46)
           _seed_48))
       _target_47)))
(pp (lambda (_port_46 _target_47 _seed_48)
      ((lambda (_key_49)
         (if (if (_eqv?_7 _key_49 'xml) #t #f)
           ((lambda (_port_50 _target_51 _seed_52) _seed_52)
            _port_46
            _target_47
            _seed_48)
           (begin
             (ssax:warn _port_46 "Skipping PI: " _target_47 nl)
             (ssax:skip-pi _port_46)
             _seed_48)))
       _target_47)))
(pp (lambda (_port_46 _target_47 _seed_48)
      ((lambda (_key_49)
         (if (if (_eqv?_7 _key_49 'xml) #t #f)
           ((lambda (_port_50 _target_51 _seed_52) _seed_52)
            _port_46
            _target_47
            _seed_48)
           (if (if (_eqv?_7 _key_49 'html) #t #f)
             (list _port_46 _target_47 _seed_48)
             (ssax:warn _port_46 _target_47 _seed_48))))
       _target_47)))
(letrec ((_test_44
           (lambda (_str_47 _doctype-fn_48 _expected_49)
             (begin
               (cout nl "Parsing: " _str_47 nl)
               ((lambda (_result_50)
                  (begin
                    (write _result_50)
                    (assert (equal? _result_50 _expected_49))))
                (_simple-parser_46 (unesc-string _str_47) _doctype-fn_48)))))
         (_dummy-doctype-fn_45
           (lambda (_elem-gi_47 _seed_48) (values #f '() '() _seed_48)))
         (_simple-parser_46
           (lambda (_str_47 _doctype-fn_48)
             (call-with-input-string
               _str_47
               (lambda (_port_49)
                 ((lambda (_port_53 _seed_54)
                    (letrec ((_element-parser_55
                               (lambda (_start-tag-head_59
                                        _port_60
                                        _elems_61
                                        _entities_62
                                        _namespaces_63
                                        _preserve-ws?_64
                                        _seed_65)
                                 (letrec ((_xml-space-gi_66
                                            (cons ssax:Prefix-XML
                                                  (string->symbol "space"))))
                                   ((letrec ((_handle-start-tag_67
                                               (lambda (_start-tag-head_68
                                                        _port_69
                                                        _entities_70
                                                        _namespaces_71
                                                        _preserve-ws?_72
                                                        _parent-seed_73)
                                                 (call-with-values
                                                   (lambda ()
                                                     (ssax:complete-start-tag
                                                       _start-tag-head_68
                                                       _port_69
                                                       _elems_61
                                                       _entities_70
                                                       _namespaces_71))
                                                   (lambda (_elem-gi_74
                                                            _attributes_75
                                                            _namespaces_76
                                                            _expected-content_77)
                                                     ((lambda (_seed_78)
                                                        ((lambda (_key_79)
                                                           (if (if (_eqv?_7 _key_79
                                                                            'EMPTY-TAG)
                                                                 #t
                                                                 #f)
                                                             ((lambda (_elem-gi_80
                                                                       _attributes_81
                                                                       _namespaces_82
                                                                       _parent-seed_83
                                                                       _seed_84)
                                                                ((lambda (_seed_85)
                                                                   ((lambda (_seed_86)
                                                                      (cons (cons _elem-gi_80
                                                                                  _seed_86)
                                                                            _parent-seed_83))
                                                                    (if (attlist-null?
                                                                          _attributes_81)
                                                                      _seed_85
                                                                      (cons (cons (if (string? '@)
                                                                                    (string->symbol
                                                                                      '@)
                                                                                    '@)
                                                                                  (map (lambda (_attr_86)
                                                                                         (list (car _attr_86)
                                                                                               (cdr _attr_86)))
                                                                                       (attlist->alist
                                                                                         _attributes_81)))
                                                                            _seed_85))))
                                                                 (if (null? _namespaces_82)
                                                                   (reverse _seed_84)
                                                                   (cons (list (if (string? '*NAMESPACES*)
                                                                                 (string->symbol
                                                                                   '*NAMESPACES*)
                                                                                 '*NAMESPACES*)
                                                                               _namespaces_82)
                                                                         (reverse _seed_84)))))
                                                              _elem-gi_74
                                                              _attributes_75
                                                              _namespaces_76
                                                              _parent-seed_73
                                                              _seed_78)
                                                             (if (if (_eqv?_7 _key_79
                                                                              'EMPTY)
                                                                   #t
                                                                   #f)
                                                               (begin
                                                                 (ssax:assert-token
                                                                   (if (eqv? #\<
                                                                             (ssax:skip-S
                                                                               _port_69))
                                                                     (ssax:read-markup-token
                                                                       _port_69)
                                                                     #f)
                                                                   'END
                                                                   _start-tag-head_68
                                                                   (lambda (_token_80
                                                                            _exp-kind_81
                                                                            _exp-head_82)
                                                                     (parser-error
                                                                       _port_69
                                                                       "[elementvalid] broken for "
                                                                       _token_80
                                                                       " while expecting "
                                                                       _exp-kind_81
                                                                       _exp-head_82)))
                                                                 ((lambda (_elem-gi_80
                                                                           _attributes_81
                                                                           _namespaces_82
                                                                           _parent-seed_83
                                                                           _seed_84)
                                                                    ((lambda (_seed_85)
                                                                       ((lambda (_seed_86)
                                                                          (cons (cons _elem-gi_80
                                                                                      _seed_86)
                                                                                _parent-seed_83))
                                                                        (if (attlist-null?
                                                                              _attributes_81)
                                                                          _seed_85
                                                                          (cons (cons (if (string? '@)
                                                                                        (string->symbol
                                                                                          '@)
                                                                                        '@)
                                                                                      (map (lambda (_attr_86)
                                                                                             (list (car _attr_86)
                                                                                                   (cdr _attr_86)))
                                                                                           (attlist->alist
                                                                                             _attributes_81)))
                                                                                _seed_85))))
                                                                     (if (null? _namespaces_82)
                                                                       (reverse _seed_84)
                                                                       (cons (list (if (string? '*NAMESPACES*)
                                                                                     (string->symbol
                                                                                       '*NAMESPACES*)
                                                                                     '*NAMESPACES*)
                                                                                   _namespaces_82)
                                                                             (reverse _seed_84)))))
                                                                  _elem-gi_74
                                                                  _attributes_75
                                                                  _namespaces_76
                                                                  _parent-seed_73
                                                                  _seed_78))
                                                               ((lambda (_preserve-ws?_80)
                                                                  ((letrec ((_loop_81
                                                                              (lambda (_port_82
                                                                                       _entities_83
                                                                                       _expect-eof?_84
                                                                                       _seed_85)
                                                                                (call-with-values
                                                                                  (lambda ()
                                                                                    (ssax:read-char-data
                                                                                      _port_82
                                                                                      _expect-eof?_84
                                                                                      (lambda (_string1_86
                                                                                               _string2_87
                                                                                               _seed_88)
                                                                                        (if (zero? (string-length
                                                                                                     _string2_87))
                                                                                          (cons _string1_86
                                                                                                _seed_88)
                                                                                          (cons* _string2_87
                                                                                                 _string1_86
                                                                                                 _seed_88)))
                                                                                      _seed_85))
                                                                                  (lambda (_seed_86
                                                                                           _term-token_87)
                                                                                    (if (eof-object?
                                                                                          _term-token_87)
                                                                                      _seed_86
                                                                                      ((lambda (_key_88)
                                                                                         (if (if (_eqv?_7 _key_88
                                                                                                          'END)
                                                                                               #t
                                                                                               #f)
                                                                                           (begin
                                                                                             (ssax:assert-token
                                                                                               _term-token_87
                                                                                               'END
                                                                                               _start-tag-head_68
                                                                                               (lambda (_token_89
                                                                                                        _exp-kind_90
                                                                                                        _exp-head_91)
                                                                                                 (parser-error
                                                                                                   _port_82
                                                                                                   "[GIMatch] broken for "
                                                                                                   _term-token_87
                                                                                                   " while expecting "
                                                                                                   _exp-kind_90
                                                                                                   _exp-head_91)))
                                                                                             ((lambda (_elem-gi_89
                                                                                                       _attributes_90
                                                                                                       _namespaces_91
                                                                                                       _parent-seed_92
                                                                                                       _seed_93)
                                                                                                ((lambda (_seed_94)
                                                                                                   ((lambda (_seed_95)
                                                                                                      (cons (cons _elem-gi_89
                                                                                                                  _seed_95)
                                                                                                            _parent-seed_92))
                                                                                                    (if (attlist-null?
                                                                                                          _attributes_90)
                                                                                                      _seed_94
                                                                                                      (cons (cons (if (string? '@)
                                                                                                                    (string->symbol
                                                                                                                      '@)
                                                                                                                    '@)
                                                                                                                  (map (lambda (_attr_95)
                                                                                                                         (list (car _attr_95)
                                                                                                                               (cdr _attr_95)))
                                                                                                                       (attlist->alist
                                                                                                                         _attributes_90)))
                                                                                                            _seed_94))))
                                                                                                 (if (null? _namespaces_91)
                                                                                                   (reverse _seed_93)
                                                                                                   (cons (list (if (string? '*NAMESPACES*)
                                                                                                                 (string->symbol
                                                                                                                   '*NAMESPACES*)
                                                                                                                 '*NAMESPACES*)
                                                                                                               _namespaces_91)
                                                                                                         (reverse _seed_93)))))
                                                                                              _elem-gi_74
                                                                                              _attributes_75
                                                                                              _namespaces_76
                                                                                              _parent-seed_73
                                                                                              _seed_86))
                                                                                           (if (if (_eqv?_7 _key_88
                                                                                                            'PI)
                                                                                                 #t
                                                                                                 #f)
                                                                                             ((lambda (_seed_89)
                                                                                                (_loop_81
                                                                                                  _port_82
                                                                                                  _entities_83
                                                                                                  _expect-eof?_84
                                                                                                  _seed_89))
                                                                                              ((lambda (_port_91
                                                                                                        _target_92
                                                                                                        _seed_93)
                                                                                                 ((lambda (_key_94)
                                                                                                    (begin
                                                                                                      (ssax:warn
                                                                                                        _port_91
                                                                                                        "Skipping PI: "
                                                                                                        _target_92
                                                                                                        nl)
                                                                                                      (ssax:skip-pi
                                                                                                        _port_91)
                                                                                                      _seed_93))
                                                                                                  _target_92))
                                                                                               _port_82
                                                                                               (cdr _term-token_87)
                                                                                               _seed_86))
                                                                                             (if (if (_eqv?_7 _key_88
                                                                                                              'ENTITY-REF)
                                                                                                   #t
                                                                                                   #f)
                                                                                               ((lambda (_seed_89)
                                                                                                  (_loop_81
                                                                                                    _port_82
                                                                                                    _entities_83
                                                                                                    _expect-eof?_84
                                                                                                    _seed_89))
                                                                                                (ssax:handle-parsed-entity
                                                                                                  _port_82
                                                                                                  (cdr _term-token_87)
                                                                                                  _entities_83
                                                                                                  (lambda (_port_89
                                                                                                           _entities_90
                                                                                                           _seed_91)
                                                                                                    (_loop_81
                                                                                                      _port_89
                                                                                                      _entities_90
                                                                                                      #t
                                                                                                      _seed_91))
                                                                                                  (lambda (_string1_89
                                                                                                           _string2_90
                                                                                                           _seed_91)
                                                                                                    (if (zero? (string-length
                                                                                                                 _string2_90))
                                                                                                      (cons _string1_89
                                                                                                            _seed_91)
                                                                                                      (cons* _string2_90
                                                                                                             _string1_89
                                                                                                             _seed_91)))
                                                                                                  _seed_86))
                                                                                               (if (if (_eqv?_7 _key_88
                                                                                                                'START)
                                                                                                     #t
                                                                                                     #f)
                                                                                                 (begin
                                                                                                   (if (eq? _expected-content_77
                                                                                                            'PCDATA)
                                                                                                     (parser-error
                                                                                                       _port_82
                                                                                                       "[elementvalid] broken for "
                                                                                                       _elem-gi_74
                                                                                                       " with char content only; unexpected token "
                                                                                                       _term-token_87))
                                                                                                   ((lambda (_seed_89)
                                                                                                      (_loop_81
                                                                                                        _port_82
                                                                                                        _entities_83
                                                                                                        _expect-eof?_84
                                                                                                        _seed_89))
                                                                                                    (_handle-start-tag_67
                                                                                                      (cdr _term-token_87)
                                                                                                      _port_82
                                                                                                      _entities_83
                                                                                                      _namespaces_76
                                                                                                      _preserve-ws?_80
                                                                                                      _seed_86)))
                                                                                                 (parser-error
                                                                                                   _port_82
                                                                                                   "XML [43] broken for "
                                                                                                   _term-token_87))))))
                                                                                       (car _term-token_87))))))))
                                                                     _loop_81)
                                                                   _port_69
                                                                   _entities_70
                                                                   #f
                                                                   _seed_78))
                                                                ((lambda (_tmp_80)
                                                                   (if _tmp_80
                                                                     ((lambda (_name-value_81)
                                                                        (equal? "preserve"
                                                                                (cdr _name-value_81)))
                                                                      _tmp_80)
                                                                     _preserve-ws?_72))
                                                                 (assoc _xml-space-gi_66
                                                                        _attributes_75))))))
                                                         _expected-content_77))
                                                      ((lambda (_elem-gi_78
                                                                _attributes_79
                                                                _namespaces_80
                                                                _expected-content_81
                                                                _seed_82)
                                                         '())
                                                       _elem-gi_74
                                                       _attributes_75
                                                       _namespaces_76
                                                       _expected-content_77
                                                       _parent-seed_73)))))))
                                      _handle-start-tag_67)
                                    _start-tag-head_59
                                    _port_60
                                    _entities_62
                                    _namespaces_63
                                    _preserve-ws?_64
                                    _seed_65))))
                             (_scan-for-significant-prolog-token-2_56
                               (lambda (_port_59
                                        _elems_60
                                        _entities_61
                                        _namespaces_62
                                        _seed_63)
                                 ((lambda (_token_64)
                                    (if (eof-object? _token_64)
                                      (parser-error
                                        _port_59
                                        "XML [22], unexpected EOF")
                                      ((lambda (_key_65)
                                         (if (if (_eqv?_7 _key_65 'PI) #t #f)
                                           ((lambda (_seed_66)
                                              (_scan-for-significant-prolog-token-2_56
                                                _port_59
                                                _elems_60
                                                _entities_61
                                                _namespaces_62
                                                _seed_66))
                                            ((lambda (_port_68
                                                      _target_69
                                                      _seed_70)
                                               ((lambda (_key_71)
                                                  (begin
                                                    (ssax:warn
                                                      _port_68
                                                      "Skipping PI: "
                                                      _target_69
                                                      nl)
                                                    (ssax:skip-pi _port_68)
                                                    _seed_70))
                                                _target_69))
                                             _port_59
                                             (cdr _token_64)
                                             _seed_63))
                                           (if (if (_eqv?_7 _key_65 'START)
                                                 #t
                                                 #f)
                                             (_element-parser_55
                                               (cdr _token_64)
                                               _port_59
                                               _elems_60
                                               _entities_61
                                               _namespaces_62
                                               #f
                                               ((lambda (_elem-gi_66 _seed_67)
                                                  _seed_67)
                                                (cdr _token_64)
                                                _seed_63))
                                             (parser-error
                                               _port_59
                                               "XML [22], unexpected markup "
                                               _token_64))))
                                       (car _token_64))))
                                  (ssax:scan-Misc _port_59))))
                             (_scan-for-significant-prolog-token-1_57
                               (lambda (_port_59 _seed_60)
                                 ((lambda (_token_61)
                                    (if (eof-object? _token_61)
                                      (parser-error
                                        _port_59
                                        "XML [22], unexpected EOF")
                                      ((lambda (_key_62)
                                         (if (if (_eqv?_7 _key_62 'PI) #t #f)
                                           ((lambda (_seed_63)
                                              (_scan-for-significant-prolog-token-1_57
                                                _port_59
                                                _seed_63))
                                            ((lambda (_port_65
                                                      _target_66
                                                      _seed_67)
                                               ((lambda (_key_68)
                                                  (begin
                                                    (ssax:warn
                                                      _port_65
                                                      "Skipping PI: "
                                                      _target_66
                                                      nl)
                                                    (ssax:skip-pi _port_65)
                                                    _seed_67))
                                                _target_66))
                                             _port_59
                                             (cdr _token_61)
                                             _seed_60))
                                           (if (if (_eqv?_7 _key_62 'DECL)
                                                 #t
                                                 #f)
                                             (_handle-decl_58
                                               _port_59
                                               (cdr _token_61)
                                               _seed_60)
                                             (if (if (_eqv?_7 _key_62 'START)
                                                   #t
                                                   #f)
                                               (call-with-values
                                                 (lambda ()
                                                   ((lambda (_elem-gi_63
                                                             _seed_64)
                                                      (_doctype-fn_48
                                                        _elem-gi_63
                                                        _seed_64))
                                                    (cdr _token_61)
                                                    _seed_60))
                                                 (lambda (_elems_63
                                                          _entities_64
                                                          _namespaces_65
                                                          _seed_66)
                                                   (_element-parser_55
                                                     (cdr _token_61)
                                                     _port_59
                                                     _elems_63
                                                     _entities_64
                                                     _namespaces_65
                                                     #f
                                                     _seed_66)))
                                               (parser-error
                                                 _port_59
                                                 "XML [22], unexpected markup "
                                                 _token_61)))))
                                       (car _token_61))))
                                  (ssax:scan-Misc _port_59))))
                             (_handle-decl_58
                               (lambda (_port_59 _token-head_60 _seed_61)
                                 (begin
                                   ((lambda (_x_62)
                                      (if _x_62
                                        _x_62
                                        (parser-error
                                          _port_59
                                          "XML [22], expected DOCTYPE declaration, found "
                                          _token-head_60)))
                                    (eq? (string->symbol "DOCTYPE")
                                         _token-head_60))
                                   (assert-curr-char
                                     ssax:S-chars
                                     "XML [28], space after DOCTYPE"
                                     _port_59)
                                   (ssax:skip-S _port_59)
                                   ((lambda (_docname_62)
                                      ((lambda (_systemid_63)
                                         ((lambda (_internal-subset?_64)
                                            (call-with-values
                                              (lambda ()
                                                ((lambda (_port_65
                                                          _docname_66
                                                          _systemid_67
                                                          _internal-subset?_68
                                                          _seed_69)
                                                   (begin
                                                     (when _internal-subset?_68
                                                           (ssax:warn
                                                             _port_65
                                                             "Internal DTD subset is not currently handled ")
                                                           (ssax:skip-internal-dtd
                                                             _port_65))
                                                     (ssax:warn
                                                       _port_65
                                                       "DOCTYPE DECL "
                                                       _docname_66
                                                       " "
                                                       _systemid_67
                                                       " found and skipped")
                                                     (_doctype-fn_48
                                                       _docname_66
                                                       _seed_69)))
                                                 _port_59
                                                 _docname_62
                                                 _systemid_63
                                                 _internal-subset?_64
                                                 _seed_61))
                                              (lambda (_elems_65
                                                       _entities_66
                                                       _namespaces_67
                                                       _seed_68)
                                                (_scan-for-significant-prolog-token-2_56
                                                  _port_59
                                                  _elems_65
                                                  _entities_66
                                                  _namespaces_67
                                                  _seed_68))))
                                          (begin
                                            (ssax:skip-S _port_59)
                                            (eqv? #\[
                                                  (assert-curr-char
                                                    '(#\> #\[)
                                                    "XML [28], end-of-DOCTYPE"
                                                    _port_59)))))
                                       (if (ssax:ncname-starting-char?
                                             (ssax:skip-S _port_59))
                                         (ssax:read-external-id _port_59)
                                         #f)))
                                    (ssax:read-QName _port_59))))))
                      (_scan-for-significant-prolog-token-1_57
                        _port_53
                        _seed_54)))
                  _port_49
                  '()))))))
  (begin
    (_test_44
      "<BR/>"
      _dummy-doctype-fn_45
      (_list_25 (_list_25 (if (string? '"BR") (string->symbol '"BR") '"BR"))))
    (assert (failed? (_test_44 "<BR>" _dummy-doctype-fn_45 '())))
    (_test_44
      "<BR></BR>"
      _dummy-doctype-fn_45
      (_list_25 (_list_25 (if (string? '"BR") (string->symbol '"BR") '"BR"))))
    (assert (failed? (_test_44 "<BR></BB>" _dummy-doctype-fn_45 '())))
    (_test_44
      "   <A HREF='URL'> link <I>itlink </I> &amp;amp;</A>"
      _dummy-doctype-fn_45
      (_list_25
        (_cons_23
          (if (string? '"A") (string->symbol '"A") '"A")
          (_cons_23
            (_list_25
              '@
              (_cons_23
                (if (string? '"HREF") (string->symbol '"HREF") '"HREF")
                '("URL")))
            (_cons_23
              '" link "
              (_cons_23
                (_cons_23
                  (if (string? '"I") (string->symbol '"I") '"I")
                  '("itlink "))
                '(" " "&" "amp;")))))))
    (_test_44
      "   <A HREF='URL' xml:space='preserve'> link <I>itlink </I> &amp;amp;</A>"
      _dummy-doctype-fn_45
      (_list_25
        (_cons_23
          (if (string? '"A") (string->symbol '"A") '"A")
          (_cons_23
            (_list_25
              '@
              (_cons_23
                (if (string? '"HREF") (string->symbol '"HREF") '"HREF")
                '("URL"))
              (_cons_23
                (_cons_23
                  (if (string? '"xml") (string->symbol '"xml") '"xml")
                  (if (string? '"space") (string->symbol '"space") '"space"))
                '("preserve")))
            (_cons_23
              '" link "
              (_cons_23
                (_cons_23
                  (if (string? '"I") (string->symbol '"I") '"I")
                  '("itlink "))
                '(" " "&" "amp;")))))))
    (_test_44
      "   <A HREF='URL' xml:space='preserve'> link <I xml:space='default'>itlink </I> &amp;amp;</A>"
      _dummy-doctype-fn_45
      (_list_25
        (_cons_23
          (if (string? '"A") (string->symbol '"A") '"A")
          (_cons_23
            (_list_25
              '@
              (_cons_23
                (if (string? '"HREF") (string->symbol '"HREF") '"HREF")
                '("URL"))
              (_cons_23
                (_cons_23
                  (if (string? '"xml") (string->symbol '"xml") '"xml")
                  (if (string? '"space") (string->symbol '"space") '"space"))
                '("preserve")))
            (_cons_23
              '" link "
              (_cons_23
                (_cons_23
                  (if (string? '"I") (string->symbol '"I") '"I")
                  (_cons_23
                    (_list_25
                      '@
                      (_cons_23
                        (_cons_23
                          (if (string? '"xml") (string->symbol '"xml") '"xml")
                          (if (string? '"space")
                            (string->symbol '"space")
                            '"space"))
                        '("default")))
                    '("itlink ")))
                '(" " "&" "amp;")))))))
    (_test_44
      "<itemize><item>This   is item 1 </item>%n<!-- Just:a comment --><item>Item 2</item>%n </itemize>"
      _dummy-doctype-fn_45
      (_list_25
        (_list_25
          (if (string? '"itemize") (string->symbol '"itemize") '"itemize")
          (_cons_23
            (if (string? '"item") (string->symbol '"item") '"item")
            '("This   is item 1 "))
          (unesc-string "%n")
          (_cons_23
            (if (string? '"item") (string->symbol '"item") '"item")
            '("Item 2"))
          (unesc-string "%n "))))
    (_test_44
      " <P><![CDATA[<BR>%n<![CDATA[<BR>]]&gt;]]></P>"
      _dummy-doctype-fn_45
      (_list_25
        (_cons_23
          (if (string? '"P") (string->symbol '"P") '"P")
          (_cons_23 '"<BR>" (_cons_23 nl '("<![CDATA[<BR>" "]]" "" ">"))))))
    (_test_44
      " <P><![CDATA[<BR>%r<![CDATA[<BR>]]&gt;]]></P>"
      _dummy-doctype-fn_45
      (_list_25
        (_cons_23
          (if (string? '"P") (string->symbol '"P") '"P")
          (_cons_23 '"<BR>" (_cons_23 nl '("<![CDATA[<BR>" "]]" "" ">"))))))
    (_test_44
      "<?xml version='1.0'?>%n%n<Reports TStamp='1'></Reports>"
      _dummy-doctype-fn_45
      (_list_25
        (_list_25
          (if (string? '"Reports") (string->symbol '"Reports") '"Reports")
          (_list_25
            '@
            (_cons_23
              (if (string? '"TStamp") (string->symbol '"TStamp") '"TStamp")
              '("1"))))))
    (_test_44
      "%n<?PI xxx?><!-- Comment %n -%r-->%n<?PI1 zzz?><T/>"
      _dummy-doctype-fn_45
      (_list_25 (_list_25 (if (string? '"T") (string->symbol '"T") '"T"))))
    (_test_44
      "<!DOCTYPE T SYSTEM 'system1' ><!-- comment -->%n<T/>"
      (lambda (_elem-gi_47 _seed_48)
        (begin
          (assert (equal? _elem-gi_47
                          (if (string? '"T") (string->symbol '"T") '"T")))
          (values #f '() '() _seed_48)))
      (_list_25 (_list_25 (if (string? '"T") (string->symbol '"T") '"T"))))
    (_test_44
      "<!DOCTYPE T PUBLIC '//EN/T' \"system1\" [ <!ELEMENT a 'aa'> ]>%n<?pi?><T/>"
      (lambda (_elem-gi_47 _seed_48)
        (begin
          (assert (equal? _elem-gi_47
                          (if (string? '"T") (string->symbol '"T") '"T")))
          (values #f '() '() _seed_48)))
      (_list_25 (_list_25 (if (string? '"T") (string->symbol '"T") '"T"))))
    (_test_44
      "<BR/>"
      (lambda (_elem-gi_47 _seed_48)
        (values (_list_25
                  (_cons_23
                    (if (string? '"BR") (string->symbol '"BR") '"BR")
                    '(EMPTY ())))
                '()
                '()
                _seed_48))
      (_list_25 (_list_25 (if (string? '"BR") (string->symbol '"BR") '"BR"))))
    (_test_44
      "<BR></BR>"
      (lambda (_elem-gi_47 _seed_48)
        (values (_list_25
                  (_cons_23
                    (if (string? '"BR") (string->symbol '"BR") '"BR")
                    '(EMPTY ())))
                '()
                '()
                _seed_48))
      (_list_25 (_list_25 (if (string? '"BR") (string->symbol '"BR") '"BR"))))
    (assert (failed? (_test_44
                       "<BR>aa</BR>"
                       (lambda (_elem-gi_47 _seed_48)
                         (values (_list_25
                                   (_cons_23
                                     (if (string? '"BR")
                                       (string->symbol '"BR")
                                       '"BR")
                                     '(EMPTY ())))
                                 '()
                                 '()
                                 _seed_48))
                       '())))
    (_test_44
      "<BR>aa</BR>"
      (lambda (_elem-gi_47 _seed_48)
        (values (_list_25
                  (_cons_23
                    (if (string? '"BR") (string->symbol '"BR") '"BR")
                    '(PCDATA ())))
                '()
                '()
                _seed_48))
      (_list_25
        (_cons_23 (if (string? '"BR") (string->symbol '"BR") '"BR") '("aa"))))
    (assert (failed? (_test_44
                       "<BR>a<I>a</I></BR>"
                       (lambda (_elem-gi_47 _seed_48)
                         (values (_list_25
                                   (_cons_23
                                     (if (string? '"BR")
                                       (string->symbol '"BR")
                                       '"BR")
                                     '(PCDATA ())))
                                 '()
                                 '()
                                 _seed_48))
                       '())))
    (_test_44
      "<BR>a<I>a</I></BR>"
      (lambda (_elem-gi_47 _seed_48)
        (values (_list_25
                  (_cons_23
                    (if (string? '"BR") (string->symbol '"BR") '"BR")
                    '(ANY ()))
                  (_cons_23
                    (if (string? '"I") (string->symbol '"I") '"I")
                    '(PCDATA ())))
                '()
                '()
                _seed_48))
      (_list_25
        (_list_25
          (if (string? '"BR") (string->symbol '"BR") '"BR")
          '"a"
          (_cons_23 (if (string? '"I") (string->symbol '"I") '"I") '("a")))))
    (_test_44
      "<DIV>Example: \"&example;\"</DIV>"
      (lambda (_elem-gi_47 _seed_48)
        (values #f
                '((example . "<P>An    ampersand (&#38;) may   be escaped numerically (&#38;#38;) or with a general entity (&amp;amp;).</P>"))
                '()
                _seed_48))
      (_list_25
        (_cons_23
          (if (string? '"DIV") (string->symbol '"DIV") '"DIV")
          (_cons_23
            '"Example: \""
            (_cons_23
              (_cons_23
                (if (string? '"P") (string->symbol '"P") '"P")
                '("An    ampersand ("
                  "&"
                  ") may   be escaped numerically ("
                  "&"
                  "#38;) or with a general entity ("
                  "&"
                  "amp;)."))
              '("\""))))))
    (_test_44
      "<DIV>Example: \"&example;\" <P/></DIV>"
      (lambda (_elem-gi_47 _seed_48)
        (values #f
                (_list_25
                  (_cons_23
                    (if (string? '"quote") (string->symbol '"quote") '"quote")
                    '"<I>example:</I> ex")
                  (_cons_23
                    (if (string? '"example")
                      (string->symbol '"example")
                      '"example")
                    '"<Q>&quote;!</Q>?"))
                '()
                _seed_48))
      (_list_25
        (_list_25
          (if (string? '"DIV") (string->symbol '"DIV") '"DIV")
          '"Example: \""
          (_cons_23
            (if (string? '"Q") (string->symbol '"Q") '"Q")
            (_cons_23
              (_cons_23
                (if (string? '"I") (string->symbol '"I") '"I")
                '("example:"))
              '(" ex" "!")))
          '"?"
          '"\" "
          (_list_25 (if (string? '"P") (string->symbol '"P") '"P")))))
    (assert (failed? (_test_44
                       "<DIV>Example: \"&example;\" <P/></DIV>"
                       (lambda (_elem-gi_47 _seed_48)
                         (values #f
                                 (_list_25
                                   (_cons_23
                                     (if (string? '"quote")
                                       (string->symbol '"quote")
                                       '"quote")
                                     '"<I>example:")
                                   (_cons_23
                                     (if (string? '"example")
                                       (string->symbol '"example")
                                       '"example")
                                     '"<Q>&quote;</I>!</Q>?"))
                                 '()
                                 _seed_48))
                       '())))
    (_test_44
      "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
      (lambda (_elem-gi_47 _seed_48) (values #f '() '() _seed_48))
      (_list_25
        (_list_25
          (_cons_23
            (if (string? '"URI1") (string->symbol '"URI1") '"URI1")
            (if (string? '"DIV") (string->symbol '"DIV") '"DIV"))
          (_list_25
            '@
            (_cons_23 (if (string? '"B") (string->symbol '"B") '"B") '("B"))
            (_cons_23
              (_cons_23
                (if (string? '"URI1") (string->symbol '"URI1") '"URI1")
                (if (string? '"B") (string->symbol '"B") '"B"))
              '("A")))
          (_list_25
            '*NAMESPACES*
            (_list_25
              (_cons_23
                (if (string? '"A") (string->symbol '"A") '"A")
                (_cons_23
                  (if (string? '"URI1") (string->symbol '"URI1") '"URI1")
                  (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
              (_cons_23
                '*DEFAULT*
                (_cons_23
                  (if (string? '"URI1") (string->symbol '"URI1") '"URI1")
                  (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))))
          (_list_25
            (_cons_23
              (if (string? '"URI1") (string->symbol '"URI1") '"URI1")
              (if (string? '"P") (string->symbol '"P") '"P"))
            (_list_25
              '*NAMESPACES*
              (_list_25
                '(*DEFAULT* #f . #f)
                (_cons_23
                  (if (string? '"A") (string->symbol '"A") '"A")
                  (_cons_23
                    (if (string? '"URI1") (string->symbol '"URI1") '"URI1")
                    (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
                (_cons_23
                  '*DEFAULT*
                  (_cons_23
                    (if (string? '"URI1") (string->symbol '"URI1") '"URI1")
                    (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))))
            (_list_25
              (if (string? '"BR") (string->symbol '"BR") '"BR")
              (_list_25
                '*NAMESPACES*
                (_list_25
                  '(*DEFAULT* #f . #f)
                  (_cons_23
                    (if (string? '"A") (string->symbol '"A") '"A")
                    (_cons_23
                      (if (string? '"URI1") (string->symbol '"URI1") '"URI1")
                      (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
                  (_cons_23
                    '*DEFAULT*
                    (_cons_23
                      (if (string? '"URI1") (string->symbol '"URI1") '"URI1")
                      (if (string? '"URI1")
                        (string->symbol '"URI1")
                        '"URI1"))))))))))
    (_test_44
      "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
      (lambda (_elem-gi_47 _seed_48)
        (values #f
                '()
                (_list_25
                  (_cons_23
                    '#f
                    (_cons_23
                      (if (string? '"UA") (string->symbol '"UA") '"UA")
                      (if (string? '"URI1")
                        (string->symbol '"URI1")
                        '"URI1"))))
                _seed_48))
      (_list_25
        (_list_25
          (_cons_23
            (if (string? '"UA") (string->symbol '"UA") '"UA")
            (if (string? '"DIV") (string->symbol '"DIV") '"DIV"))
          (_list_25
            '@
            (_cons_23 (if (string? '"B") (string->symbol '"B") '"B") '("B"))
            (_cons_23
              (_cons_23
                (if (string? '"UA") (string->symbol '"UA") '"UA")
                (if (string? '"B") (string->symbol '"B") '"B"))
              '("A")))
          (_list_25
            '*NAMESPACES*
            (_list_25
              (_cons_23
                (if (string? '"A") (string->symbol '"A") '"A")
                (_cons_23
                  (if (string? '"UA") (string->symbol '"UA") '"UA")
                  (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
              (_cons_23
                '*DEFAULT*
                (_cons_23
                  (if (string? '"UA") (string->symbol '"UA") '"UA")
                  (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
              (_cons_23
                '#f
                (_cons_23
                  (if (string? '"UA") (string->symbol '"UA") '"UA")
                  (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))))
          (_list_25
            (_cons_23
              (if (string? '"UA") (string->symbol '"UA") '"UA")
              (if (string? '"P") (string->symbol '"P") '"P"))
            (_list_25
              '*NAMESPACES*
              (_list_25
                '(*DEFAULT* #f . #f)
                (_cons_23
                  (if (string? '"A") (string->symbol '"A") '"A")
                  (_cons_23
                    (if (string? '"UA") (string->symbol '"UA") '"UA")
                    (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
                (_cons_23
                  '*DEFAULT*
                  (_cons_23
                    (if (string? '"UA") (string->symbol '"UA") '"UA")
                    (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
                (_cons_23
                  '#f
                  (_cons_23
                    (if (string? '"UA") (string->symbol '"UA") '"UA")
                    (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))))
            (_list_25
              (if (string? '"BR") (string->symbol '"BR") '"BR")
              (_list_25
                '*NAMESPACES*
                (_list_25
                  '(*DEFAULT* #f . #f)
                  (_cons_23
                    (if (string? '"A") (string->symbol '"A") '"A")
                    (_cons_23
                      (if (string? '"UA") (string->symbol '"UA") '"UA")
                      (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
                  (_cons_23
                    '*DEFAULT*
                    (_cons_23
                      (if (string? '"UA") (string->symbol '"UA") '"UA")
                      (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
                  (_cons_23
                    '#f
                    (_cons_23
                      (if (string? '"UA") (string->symbol '"UA") '"UA")
                      (if (string? '"URI1")
                        (string->symbol '"URI1")
                        '"URI1"))))))))))
    (assert (failed? (_test_44
                       "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
                       (lambda (_elem-gi_47 _seed_48)
                         (values (_list_25
                                   (_list_25
                                     (if (string? '"DIV")
                                       (string->symbol '"DIV")
                                       '"DIV")
                                     'ANY
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"B")
                                           (string->symbol '"B")
                                           '"B")
                                         '(CDATA IMPLIED #f))
                                       (_cons_23
                                         (_cons_23
                                           (if (string? '"A")
                                             (string->symbol '"A")
                                             '"A")
                                           (if (string? '"B")
                                             (string->symbol '"B")
                                             '"B"))
                                         '(CDATA IMPLIED #f))
                                       (_cons_23
                                         (_cons_23
                                           (if (string? '"C")
                                             (string->symbol '"C")
                                             '"C")
                                           (if (string? '"B")
                                             (string->symbol '"B")
                                             '"B"))
                                         '(CDATA IMPLIED "xx"))
                                       (_cons_23
                                         (_cons_23
                                           (if (string? '"xmlns")
                                             (string->symbol '"xmlns")
                                             '"xmlns")
                                           (if (string? '"C")
                                             (string->symbol '"C")
                                             '"C"))
                                         '(CDATA IMPLIED "URI1"))))
                                   (_cons_23
                                     (_cons_23
                                       (if (string? '"A")
                                         (string->symbol '"A")
                                         '"A")
                                       (if (string? '"P")
                                         (string->symbol '"P")
                                         '"P"))
                                     '(ANY ()))
                                   (_cons_23
                                     (if (string? '"BR")
                                       (string->symbol '"BR")
                                       '"BR")
                                     (_cons_23
                                       (if (string? '"EMPTY")
                                         (string->symbol '"EMPTY")
                                         '"EMPTY")
                                       '(()))))
                                 '()
                                 (_list_25
                                   (_cons_23
                                     '#f
                                     (_cons_23
                                       (if (string? '"UA")
                                         (string->symbol '"UA")
                                         '"UA")
                                       (if (string? '"URI1")
                                         (string->symbol '"URI1")
                                         '"URI1"))))
                                 _seed_48))
                       '())))
    (assert (failed? (_test_44
                       "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
                       (lambda (_elem-gi_47 _seed_48)
                         (values (_list_25
                                   (_list_25
                                     (if (string? '"DIV")
                                       (string->symbol '"DIV")
                                       '"DIV")
                                     'ANY
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"B")
                                           (string->symbol '"B")
                                           '"B")
                                         '(CDATA IMPLIED #f))
                                       (_cons_23
                                         (if (string? '"xmlns")
                                           (string->symbol '"xmlns")
                                           '"xmlns")
                                         '(CDATA IMPLIED "URI1"))
                                       (_cons_23
                                         (_cons_23
                                           (if (string? '"A")
                                             (string->symbol '"A")
                                             '"A")
                                           (if (string? '"B")
                                             (string->symbol '"B")
                                             '"B"))
                                         '(CDATA IMPLIED #f))
                                       (_cons_23
                                         (_cons_23
                                           (if (string? '"C")
                                             (string->symbol '"C")
                                             '"C")
                                           (if (string? '"B")
                                             (string->symbol '"B")
                                             '"B"))
                                         '(CDATA IMPLIED "xx"))))
                                   (_cons_23
                                     (_cons_23
                                       (if (string? '"A")
                                         (string->symbol '"A")
                                         '"A")
                                       (if (string? '"P")
                                         (string->symbol '"P")
                                         '"P"))
                                     '(ANY ()))
                                   (_cons_23
                                     (if (string? '"BR")
                                       (string->symbol '"BR")
                                       '"BR")
                                     '(EMPTY ())))
                                 '()
                                 (_list_25
                                   (_cons_23
                                     '#f
                                     (_cons_23
                                       (if (string? '"UA")
                                         (string->symbol '"UA")
                                         '"UA")
                                       (if (string? '"URI1")
                                         (string->symbol '"URI1")
                                         '"URI1"))))
                                 _seed_48))
                       '())))
    (assert (failed? (_test_44
                       "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
                       (lambda (_elem-gi_47 _seed_48)
                         (values (_list_25
                                   (_list_25
                                     (if (string? '"DIV")
                                       (string->symbol '"DIV")
                                       '"DIV")
                                     'ANY
                                     (_list_25
                                       (_cons_23
                                         (if (string? '"B")
                                           (string->symbol '"B")
                                           '"B")
                                         '(CDATA IMPLIED #f))
                                       (_cons_23
                                         (if (string? '"xmlns")
                                           (string->symbol '"xmlns")
                                           '"xmlns")
                                         '(CDATA FIXED "URI2"))
                                       (_cons_23
                                         (_cons_23
                                           (if (string? '"A")
                                             (string->symbol '"A")
                                             '"A")
                                           (if (string? '"B")
                                             (string->symbol '"B")
                                             '"B"))
                                         '(CDATA IMPLIED #f))))
                                   (_cons_23
                                     (_cons_23
                                       (if (string? '"A")
                                         (string->symbol '"A")
                                         '"A")
                                       (if (string? '"P")
                                         (string->symbol '"P")
                                         '"P"))
                                     '(ANY ()))
                                   (_cons_23
                                     (if (string? '"BR")
                                       (string->symbol '"BR")
                                       '"BR")
                                     '(EMPTY ())))
                                 '()
                                 (_list_25
                                   (_cons_23
                                     '#f
                                     (_cons_23
                                       (if (string? '"UA")
                                         (string->symbol '"UA")
                                         '"UA")
                                       (if (string? '"URI1")
                                         (string->symbol '"URI1")
                                         '"URI1"))))
                                 _seed_48))
                       '())))
    (_test_44
      "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
      (lambda (_elem-gi_47 _seed_48)
        (values (_list_25
                  (_list_25
                    (if (string? '"DIV") (string->symbol '"DIV") '"DIV")
                    'ANY
                    (_list_25
                      (_cons_23
                        (if (string? '"B") (string->symbol '"B") '"B")
                        '(CDATA IMPLIED #f))
                      (_cons_23
                        (if (string? '"xmlns")
                          (string->symbol '"xmlns")
                          '"xmlns")
                        '(CDATA FIXED "URI1"))
                      (_cons_23
                        (_cons_23
                          (if (string? '"A") (string->symbol '"A") '"A")
                          (if (string? '"B") (string->symbol '"B") '"B"))
                        '(CDATA IMPLIED #f))))
                  (_cons_23
                    (_cons_23
                      (if (string? '"A") (string->symbol '"A") '"A")
                      (if (string? '"P") (string->symbol '"P") '"P"))
                    '(ANY ()))
                  (_cons_23
                    (if (string? '"BR") (string->symbol '"BR") '"BR")
                    '(EMPTY ())))
                '()
                (_list_25
                  (_cons_23
                    '#f
                    (_cons_23
                      (if (string? '"UA") (string->symbol '"UA") '"UA")
                      (if (string? '"URI1")
                        (string->symbol '"URI1")
                        '"URI1"))))
                _seed_48))
      (_list_25
        (_list_25
          (_cons_23
            (if (string? '"UA") (string->symbol '"UA") '"UA")
            (if (string? '"DIV") (string->symbol '"DIV") '"DIV"))
          (_list_25
            '@
            (_cons_23 (if (string? '"B") (string->symbol '"B") '"B") '("B"))
            (_cons_23
              (_cons_23
                (if (string? '"UA") (string->symbol '"UA") '"UA")
                (if (string? '"B") (string->symbol '"B") '"B"))
              '("A")))
          (_list_25
            '*NAMESPACES*
            (_list_25
              (_cons_23
                '*DEFAULT*
                (_cons_23
                  (if (string? '"UA") (string->symbol '"UA") '"UA")
                  (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
              (_cons_23
                (if (string? '"A") (string->symbol '"A") '"A")
                (_cons_23
                  (if (string? '"UA") (string->symbol '"UA") '"UA")
                  (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
              (_cons_23
                '#f
                (_cons_23
                  (if (string? '"UA") (string->symbol '"UA") '"UA")
                  (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))))
          (_list_25
            (_cons_23
              (if (string? '"UA") (string->symbol '"UA") '"UA")
              (if (string? '"P") (string->symbol '"P") '"P"))
            (_list_25
              '*NAMESPACES*
              (_list_25
                '(*DEFAULT* #f . #f)
                (_cons_23
                  '*DEFAULT*
                  (_cons_23
                    (if (string? '"UA") (string->symbol '"UA") '"UA")
                    (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
                (_cons_23
                  (if (string? '"A") (string->symbol '"A") '"A")
                  (_cons_23
                    (if (string? '"UA") (string->symbol '"UA") '"UA")
                    (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
                (_cons_23
                  '#f
                  (_cons_23
                    (if (string? '"UA") (string->symbol '"UA") '"UA")
                    (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))))
            (_list_25
              (if (string? '"BR") (string->symbol '"BR") '"BR")
              (_list_25
                '*NAMESPACES*
                (_list_25
                  '(*DEFAULT* #f . #f)
                  (_cons_23
                    '*DEFAULT*
                    (_cons_23
                      (if (string? '"UA") (string->symbol '"UA") '"UA")
                      (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
                  (_cons_23
                    (if (string? '"A") (string->symbol '"A") '"A")
                    (_cons_23
                      (if (string? '"UA") (string->symbol '"UA") '"UA")
                      (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
                  (_cons_23
                    '#f
                    (_cons_23
                      (if (string? '"UA") (string->symbol '"UA") '"UA")
                      (if (string? '"URI1")
                        (string->symbol '"URI1")
                        '"URI1"))))))))))
    (_test_44
      "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
      (lambda (_elem-gi_47 _seed_48)
        (values (_list_25
                  (_list_25
                    (if (string? '"DIV") (string->symbol '"DIV") '"DIV")
                    'ANY
                    (_list_25
                      (_cons_23
                        (if (string? '"B") (string->symbol '"B") '"B")
                        '(CDATA IMPLIED #f))
                      (_cons_23
                        (_cons_23
                          (if (string? '"A") (string->symbol '"A") '"A")
                          (if (string? '"B") (string->symbol '"B") '"B"))
                        '(CDATA IMPLIED #f))
                      (_cons_23
                        (_cons_23
                          (if (string? '"C") (string->symbol '"C") '"C")
                          (if (string? '"B") (string->symbol '"B") '"B"))
                        '(CDATA IMPLIED "xx"))
                      (_cons_23
                        (_cons_23
                          (if (string? '"xmlns")
                            (string->symbol '"xmlns")
                            '"xmlns")
                          (if (string? '"C") (string->symbol '"C") '"C"))
                        '(CDATA IMPLIED "URI2"))))
                  (_cons_23
                    (_cons_23
                      (if (string? '"A") (string->symbol '"A") '"A")
                      (if (string? '"P") (string->symbol '"P") '"P"))
                    '(ANY ()))
                  (_cons_23
                    (if (string? '"BR") (string->symbol '"BR") '"BR")
                    '(EMPTY ())))
                '()
                (_list_25
                  (_cons_23
                    '#f
                    (_cons_23
                      (if (string? '"UA") (string->symbol '"UA") '"UA")
                      (if (string? '"URI1")
                        (string->symbol '"URI1")
                        '"URI1"))))
                _seed_48))
      (_list_25
        (_list_25
          (_cons_23
            (if (string? '"UA") (string->symbol '"UA") '"UA")
            (if (string? '"DIV") (string->symbol '"DIV") '"DIV"))
          (_list_25
            '@
            (_cons_23 (if (string? '"B") (string->symbol '"B") '"B") '("B"))
            (_cons_23
              (_cons_23
                (if (string? '"UA") (string->symbol '"UA") '"UA")
                (if (string? '"B") (string->symbol '"B") '"B"))
              '("A"))
            (_cons_23
              (_cons_23
                (if (string? '"URI2") (string->symbol '"URI2") '"URI2")
                (if (string? '"B") (string->symbol '"B") '"B"))
              '("xx")))
          (_list_25
            '*NAMESPACES*
            (_list_25
              (_cons_23
                '*DEFAULT*
                (_cons_23
                  (if (string? '"UA") (string->symbol '"UA") '"UA")
                  (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
              (_cons_23
                (if (string? '"A") (string->symbol '"A") '"A")
                (_cons_23
                  (if (string? '"UA") (string->symbol '"UA") '"UA")
                  (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
              (_cons_23
                (if (string? '"C") (string->symbol '"C") '"C")
                (_cons_23
                  (if (string? '"URI2") (string->symbol '"URI2") '"URI2")
                  (if (string? '"URI2") (string->symbol '"URI2") '"URI2")))
              (_cons_23
                '#f
                (_cons_23
                  (if (string? '"UA") (string->symbol '"UA") '"UA")
                  (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))))
          (_list_25
            (_cons_23
              (if (string? '"UA") (string->symbol '"UA") '"UA")
              (if (string? '"P") (string->symbol '"P") '"P"))
            (_list_25
              '*NAMESPACES*
              (_list_25
                '(*DEFAULT* #f . #f)
                (_cons_23
                  '*DEFAULT*
                  (_cons_23
                    (if (string? '"UA") (string->symbol '"UA") '"UA")
                    (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
                (_cons_23
                  (if (string? '"A") (string->symbol '"A") '"A")
                  (_cons_23
                    (if (string? '"UA") (string->symbol '"UA") '"UA")
                    (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
                (_cons_23
                  (if (string? '"C") (string->symbol '"C") '"C")
                  (_cons_23
                    (if (string? '"URI2") (string->symbol '"URI2") '"URI2")
                    (if (string? '"URI2") (string->symbol '"URI2") '"URI2")))
                (_cons_23
                  '#f
                  (_cons_23
                    (if (string? '"UA") (string->symbol '"UA") '"UA")
                    (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))))
            (_list_25
              (if (string? '"BR") (string->symbol '"BR") '"BR")
              (_list_25
                '*NAMESPACES*
                (_list_25
                  '(*DEFAULT* #f . #f)
                  (_cons_23
                    '*DEFAULT*
                    (_cons_23
                      (if (string? '"UA") (string->symbol '"UA") '"UA")
                      (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
                  (_cons_23
                    (if (string? '"A") (string->symbol '"A") '"A")
                    (_cons_23
                      (if (string? '"UA") (string->symbol '"UA") '"UA")
                      (if (string? '"URI1") (string->symbol '"URI1") '"URI1")))
                  (_cons_23
                    (if (string? '"C") (string->symbol '"C") '"C")
                    (_cons_23
                      (if (string? '"URI2") (string->symbol '"URI2") '"URI2")
                      (if (string? '"URI2") (string->symbol '"URI2") '"URI2")))
                  (_cons_23
                    '#f
                    (_cons_23
                      (if (string? '"UA") (string->symbol '"UA") '"UA")
                      (if (string? '"URI1")
                        (string->symbol '"URI1")
                        '"URI1"))))))))))))
(define ssax:reverse-collect-str
  (lambda (_fragments_38)
    (if (null? _fragments_38)
      '()
      (if (null? (cdr _fragments_38))
        _fragments_38
        ((letrec ((_loop_39
                    (lambda (_fragments_40 _result_41 _strs_42)
                      (if (null? _fragments_40)
                        (if (null? _strs_42)
                          _result_41
                          (cons (string-concatenate/shared _strs_42)
                                _result_41))
                        (if (string? (car _fragments_40))
                          (_loop_39
                            (cdr _fragments_40)
                            _result_41
                            (cons (car _fragments_40) _strs_42))
                          (_loop_39
                            (cdr _fragments_40)
                            (cons (car _fragments_40)
                                  (if (null? _strs_42)
                                    _result_41
                                    (cons (string-concatenate/shared _strs_42)
                                          _result_41)))
                            '()))))))
           _loop_39)
         _fragments_38
         '()
         '())))))
(define ssax:reverse-collect-str-drop-ws
  (lambda (_fragments_38)
    (if (null? _fragments_38)
      '()
      (if (null? (cdr _fragments_38))
        (if (if (string? (car _fragments_38))
              (string-whitespace? (car _fragments_38))
              #f)
          '()
          _fragments_38)
        ((letrec ((_loop_39
                    (lambda (_fragments_40
                             _result_41
                             _strs_42
                             _all-whitespace?_43)
                      (if (null? _fragments_40)
                        (if _all-whitespace?_43
                          _result_41
                          (cons (string-concatenate/shared _strs_42)
                                _result_41))
                        (if (string? (car _fragments_40))
                          (_loop_39
                            (cdr _fragments_40)
                            _result_41
                            (cons (car _fragments_40) _strs_42)
                            (if _all-whitespace?_43
                              (string-whitespace? (car _fragments_40))
                              #f))
                          (_loop_39
                            (cdr _fragments_40)
                            (cons (car _fragments_40)
                                  (if _all-whitespace?_43
                                    _result_41
                                    (cons (string-concatenate/shared _strs_42)
                                          _result_41)))
                            '()
                            #t))))))
           _loop_39)
         _fragments_38
         '()
         '()
         #t)))))
(define ssax:xml->sxml
  (lambda (_port_38 _namespace-prefix-assig_39)
    (letrec ((_RES-NAME->SXML_40
               (lambda (_res-name_42)
                 (string->symbol
                   (string-append
                     (symbol->string (car _res-name_42))
                     ":"
                     (symbol->string (cdr _res-name_42))))))
             (_namespaces_41
               (map (lambda (_el_42)
                      (cons* #f
                             (car _el_42)
                             (ssax:uri-string->symbol (cdr _el_42))))
                    _namespace-prefix-assig_39)))
      ((lambda (_result_42)
         (cons '*TOP*
               (if (null? _namespace-prefix-assig_39)
                 _result_42
                 (cons (list '@
                             (cons '*NAMESPACES*
                                   (map (lambda (_ns_43)
                                          (list (car _ns_43) (cdr _ns_43)))
                                        _namespace-prefix-assig_39)))
                       _result_42))))
       (reverse ((lambda (_port_45 _seed_46)
                   (letrec ((_element-parser_47
                              (lambda (_start-tag-head_51
                                       _port_52
                                       _elems_53
                                       _entities_54
                                       _namespaces_55
                                       _preserve-ws?_56
                                       _seed_57)
                                (letrec ((_xml-space-gi_58
                                           (cons ssax:Prefix-XML
                                                 (string->symbol "space"))))
                                  ((letrec ((_handle-start-tag_59
                                              (lambda (_start-tag-head_60
                                                       _port_61
                                                       _entities_62
                                                       _namespaces_63
                                                       _preserve-ws?_64
                                                       _parent-seed_65)
                                                (call-with-values
                                                  (lambda ()
                                                    (ssax:complete-start-tag
                                                      _start-tag-head_60
                                                      _port_61
                                                      _elems_53
                                                      _entities_62
                                                      _namespaces_63))
                                                  (lambda (_elem-gi_66
                                                           _attributes_67
                                                           _namespaces_68
                                                           _expected-content_69)
                                                    ((lambda (_seed_70)
                                                       ((lambda (_key_71)
                                                          (if (if (_eqv?_7 _key_71
                                                                           'EMPTY-TAG)
                                                                #t
                                                                #f)
                                                            ((lambda (_elem-gi_72
                                                                      _attributes_73
                                                                      _namespaces_74
                                                                      _parent-seed_75
                                                                      _seed_76)
                                                               ((lambda (_seed_77
                                                                         _attrs_78)
                                                                  (cons (cons (if (symbol? _elem-gi_72)
                                                                                _elem-gi_72
                                                                                (_RES-NAME->SXML_40
                                                                                  _elem-gi_72))
                                                                              (if (null? _attrs_78)
                                                                                _seed_77
                                                                                (cons (cons '@
                                                                                            _attrs_78)
                                                                                      _seed_77)))
                                                                        _parent-seed_75))
                                                                (ssax:reverse-collect-str-drop-ws
                                                                  _seed_76)
                                                                (attlist-fold
                                                                  (lambda (_attr_77
                                                                           _accum_78)
                                                                    (cons (list (if (symbol? (car _attr_77))
                                                                                  (car _attr_77)
                                                                                  (_RES-NAME->SXML_40
                                                                                    (car _attr_77)))
                                                                                (cdr _attr_77))
                                                                          _accum_78))
                                                                  '()
                                                                  _attributes_73)))
                                                             _elem-gi_66
                                                             _attributes_67
                                                             _namespaces_68
                                                             _parent-seed_65
                                                             _seed_70)
                                                            (if (if (_eqv?_7 _key_71
                                                                             'EMPTY)
                                                                  #t
                                                                  #f)
                                                              (begin
                                                                (ssax:assert-token
                                                                  (if (eqv? #\<
                                                                            (ssax:skip-S
                                                                              _port_61))
                                                                    (ssax:read-markup-token
                                                                      _port_61)
                                                                    #f)
                                                                  'END
                                                                  _start-tag-head_60
                                                                  (lambda (_token_72
                                                                           _exp-kind_73
                                                                           _exp-head_74)
                                                                    (parser-error
                                                                      _port_61
                                                                      "[elementvalid] broken for "
                                                                      _token_72
                                                                      " while expecting "
                                                                      _exp-kind_73
                                                                      _exp-head_74)))
                                                                ((lambda (_elem-gi_72
                                                                          _attributes_73
                                                                          _namespaces_74
                                                                          _parent-seed_75
                                                                          _seed_76)
                                                                   ((lambda (_seed_77
                                                                             _attrs_78)
                                                                      (cons (cons (if (symbol? _elem-gi_72)
                                                                                    _elem-gi_72
                                                                                    (_RES-NAME->SXML_40
                                                                                      _elem-gi_72))
                                                                                  (if (null? _attrs_78)
                                                                                    _seed_77
                                                                                    (cons (cons '@
                                                                                                _attrs_78)
                                                                                          _seed_77)))
                                                                            _parent-seed_75))
                                                                    (ssax:reverse-collect-str-drop-ws
                                                                      _seed_76)
                                                                    (attlist-fold
                                                                      (lambda (_attr_77
                                                                               _accum_78)
                                                                        (cons (list (if (symbol? (car _attr_77))
                                                                                      (car _attr_77)
                                                                                      (_RES-NAME->SXML_40
                                                                                        (car _attr_77)))
                                                                                    (cdr _attr_77))
                                                                              _accum_78))
                                                                      '()
                                                                      _attributes_73)))
                                                                 _elem-gi_66
                                                                 _attributes_67
                                                                 _namespaces_68
                                                                 _parent-seed_65
                                                                 _seed_70))
                                                              ((lambda (_preserve-ws?_72)
                                                                 ((letrec ((_loop_73
                                                                             (lambda (_port_74
                                                                                      _entities_75
                                                                                      _expect-eof?_76
                                                                                      _seed_77)
                                                                               (call-with-values
                                                                                 (lambda ()
                                                                                   (ssax:read-char-data
                                                                                     _port_74
                                                                                     _expect-eof?_76
                                                                                     (lambda (_string1_78
                                                                                              _string2_79
                                                                                              _seed_80)
                                                                                       (if (zero? (string-length
                                                                                                    _string2_79))
                                                                                         (cons _string1_78
                                                                                               _seed_80)
                                                                                         (cons* _string2_79
                                                                                                _string1_78
                                                                                                _seed_80)))
                                                                                     _seed_77))
                                                                                 (lambda (_seed_78
                                                                                          _term-token_79)
                                                                                   (if (eof-object?
                                                                                         _term-token_79)
                                                                                     _seed_78
                                                                                     ((lambda (_key_80)
                                                                                        (if (if (_eqv?_7 _key_80
                                                                                                         'END)
                                                                                              #t
                                                                                              #f)
                                                                                          (begin
                                                                                            (ssax:assert-token
                                                                                              _term-token_79
                                                                                              'END
                                                                                              _start-tag-head_60
                                                                                              (lambda (_token_81
                                                                                                       _exp-kind_82
                                                                                                       _exp-head_83)
                                                                                                (parser-error
                                                                                                  _port_74
                                                                                                  "[GIMatch] broken for "
                                                                                                  _term-token_79
                                                                                                  " while expecting "
                                                                                                  _exp-kind_82
                                                                                                  _exp-head_83)))
                                                                                            ((lambda (_elem-gi_81
                                                                                                      _attributes_82
                                                                                                      _namespaces_83
                                                                                                      _parent-seed_84
                                                                                                      _seed_85)
                                                                                               ((lambda (_seed_86
                                                                                                         _attrs_87)
                                                                                                  (cons (cons (if (symbol? _elem-gi_81)
                                                                                                                _elem-gi_81
                                                                                                                (_RES-NAME->SXML_40
                                                                                                                  _elem-gi_81))
                                                                                                              (if (null? _attrs_87)
                                                                                                                _seed_86
                                                                                                                (cons (cons '@
                                                                                                                            _attrs_87)
                                                                                                                      _seed_86)))
                                                                                                        _parent-seed_84))
                                                                                                (ssax:reverse-collect-str-drop-ws
                                                                                                  _seed_85)
                                                                                                (attlist-fold
                                                                                                  (lambda (_attr_86
                                                                                                           _accum_87)
                                                                                                    (cons (list (if (symbol? (car _attr_86))
                                                                                                                  (car _attr_86)
                                                                                                                  (_RES-NAME->SXML_40
                                                                                                                    (car _attr_86)))
                                                                                                                (cdr _attr_86))
                                                                                                          _accum_87))
                                                                                                  '()
                                                                                                  _attributes_82)))
                                                                                             _elem-gi_66
                                                                                             _attributes_67
                                                                                             _namespaces_68
                                                                                             _parent-seed_65
                                                                                             _seed_78))
                                                                                          (if (if (_eqv?_7 _key_80
                                                                                                           'PI)
                                                                                                #t
                                                                                                #f)
                                                                                            ((lambda (_seed_81)
                                                                                               (_loop_73
                                                                                                 _port_74
                                                                                                 _entities_75
                                                                                                 _expect-eof?_76
                                                                                                 _seed_81))
                                                                                             ((lambda (_port_83
                                                                                                       _target_84
                                                                                                       _seed_85)
                                                                                                ((lambda (_key_86)
                                                                                                   ((lambda (_port_87
                                                                                                             _pi-tag_88
                                                                                                             _seed_89)
                                                                                                      (cons (list '*PI*
                                                                                                                  _pi-tag_88
                                                                                                                  (ssax:read-pi-body-as-string
                                                                                                                    _port_87))
                                                                                                            _seed_89))
                                                                                                    _port_83
                                                                                                    _target_84
                                                                                                    _seed_85))
                                                                                                 _target_84))
                                                                                              _port_74
                                                                                              (cdr _term-token_79)
                                                                                              _seed_78))
                                                                                            (if (if (_eqv?_7 _key_80
                                                                                                             'ENTITY-REF)
                                                                                                  #t
                                                                                                  #f)
                                                                                              ((lambda (_seed_81)
                                                                                                 (_loop_73
                                                                                                   _port_74
                                                                                                   _entities_75
                                                                                                   _expect-eof?_76
                                                                                                   _seed_81))
                                                                                               (ssax:handle-parsed-entity
                                                                                                 _port_74
                                                                                                 (cdr _term-token_79)
                                                                                                 _entities_75
                                                                                                 (lambda (_port_81
                                                                                                          _entities_82
                                                                                                          _seed_83)
                                                                                                   (_loop_73
                                                                                                     _port_81
                                                                                                     _entities_82
                                                                                                     #t
                                                                                                     _seed_83))
                                                                                                 (lambda (_string1_81
                                                                                                          _string2_82
                                                                                                          _seed_83)
                                                                                                   (if (zero? (string-length
                                                                                                                _string2_82))
                                                                                                     (cons _string1_81
                                                                                                           _seed_83)
                                                                                                     (cons* _string2_82
                                                                                                            _string1_81
                                                                                                            _seed_83)))
                                                                                                 _seed_78))
                                                                                              (if (if (_eqv?_7 _key_80
                                                                                                               'START)
                                                                                                    #t
                                                                                                    #f)
                                                                                                (begin
                                                                                                  (if (eq? _expected-content_69
                                                                                                           'PCDATA)
                                                                                                    (parser-error
                                                                                                      _port_74
                                                                                                      "[elementvalid] broken for "
                                                                                                      _elem-gi_66
                                                                                                      " with char content only; unexpected token "
                                                                                                      _term-token_79))
                                                                                                  ((lambda (_seed_81)
                                                                                                     (_loop_73
                                                                                                       _port_74
                                                                                                       _entities_75
                                                                                                       _expect-eof?_76
                                                                                                       _seed_81))
                                                                                                   (_handle-start-tag_59
                                                                                                     (cdr _term-token_79)
                                                                                                     _port_74
                                                                                                     _entities_75
                                                                                                     _namespaces_68
                                                                                                     _preserve-ws?_72
                                                                                                     _seed_78)))
                                                                                                (parser-error
                                                                                                  _port_74
                                                                                                  "XML [43] broken for "
                                                                                                  _term-token_79))))))
                                                                                      (car _term-token_79))))))))
                                                                    _loop_73)
                                                                  _port_61
                                                                  _entities_62
                                                                  #f
                                                                  _seed_70))
                                                               ((lambda (_tmp_72)
                                                                  (if _tmp_72
                                                                    ((lambda (_name-value_73)
                                                                       (equal? "preserve"
                                                                               (cdr _name-value_73)))
                                                                     _tmp_72)
                                                                    _preserve-ws?_64))
                                                                (assoc _xml-space-gi_58
                                                                       _attributes_67))))))
                                                        _expected-content_69))
                                                     ((lambda (_elem-gi_70
                                                               _attributes_71
                                                               _namespaces_72
                                                               _expected-content_73
                                                               _seed_74)
                                                        '())
                                                      _elem-gi_66
                                                      _attributes_67
                                                      _namespaces_68
                                                      _expected-content_69
                                                      _parent-seed_65)))))))
                                     _handle-start-tag_59)
                                   _start-tag-head_51
                                   _port_52
                                   _entities_54
                                   _namespaces_55
                                   _preserve-ws?_56
                                   _seed_57))))
                            (_scan-for-significant-prolog-token-2_48
                              (lambda (_port_51
                                       _elems_52
                                       _entities_53
                                       _namespaces_54
                                       _seed_55)
                                ((lambda (_token_56)
                                   (if (eof-object? _token_56)
                                     (parser-error
                                       _port_51
                                       "XML [22], unexpected EOF")
                                     ((lambda (_key_57)
                                        (if (if (_eqv?_7 _key_57 'PI) #t #f)
                                          ((lambda (_seed_58)
                                             (_scan-for-significant-prolog-token-2_48
                                               _port_51
                                               _elems_52
                                               _entities_53
                                               _namespaces_54
                                               _seed_58))
                                           ((lambda (_port_60
                                                     _target_61
                                                     _seed_62)
                                              ((lambda (_key_63)
                                                 ((lambda (_port_64
                                                           _pi-tag_65
                                                           _seed_66)
                                                    (cons (list '*PI*
                                                                _pi-tag_65
                                                                (ssax:read-pi-body-as-string
                                                                  _port_64))
                                                          _seed_66))
                                                  _port_60
                                                  _target_61
                                                  _seed_62))
                                               _target_61))
                                            _port_51
                                            (cdr _token_56)
                                            _seed_55))
                                          (if (if (_eqv?_7 _key_57 'START)
                                                #t
                                                #f)
                                            (_element-parser_47
                                              (cdr _token_56)
                                              _port_51
                                              _elems_52
                                              _entities_53
                                              _namespaces_54
                                              #f
                                              ((lambda (_elem-gi_58 _seed_59)
                                                 _seed_59)
                                               (cdr _token_56)
                                               _seed_55))
                                            (parser-error
                                              _port_51
                                              "XML [22], unexpected markup "
                                              _token_56))))
                                      (car _token_56))))
                                 (ssax:scan-Misc _port_51))))
                            (_scan-for-significant-prolog-token-1_49
                              (lambda (_port_51 _seed_52)
                                ((lambda (_token_53)
                                   (if (eof-object? _token_53)
                                     (parser-error
                                       _port_51
                                       "XML [22], unexpected EOF")
                                     ((lambda (_key_54)
                                        (if (if (_eqv?_7 _key_54 'PI) #t #f)
                                          ((lambda (_seed_55)
                                             (_scan-for-significant-prolog-token-1_49
                                               _port_51
                                               _seed_55))
                                           ((lambda (_port_57
                                                     _target_58
                                                     _seed_59)
                                              ((lambda (_key_60)
                                                 ((lambda (_port_61
                                                           _pi-tag_62
                                                           _seed_63)
                                                    (cons (list '*PI*
                                                                _pi-tag_62
                                                                (ssax:read-pi-body-as-string
                                                                  _port_61))
                                                          _seed_63))
                                                  _port_57
                                                  _target_58
                                                  _seed_59))
                                               _target_58))
                                            _port_51
                                            (cdr _token_53)
                                            _seed_52))
                                          (if (if (_eqv?_7 _key_54 'DECL)
                                                #t
                                                #f)
                                            (_handle-decl_50
                                              _port_51
                                              (cdr _token_53)
                                              _seed_52)
                                            (if (if (_eqv?_7 _key_54 'START)
                                                  #t
                                                  #f)
                                              (call-with-values
                                                (lambda ()
                                                  ((lambda (_elem-gi_55
                                                            _seed_56)
                                                     (values #f
                                                             '()
                                                             _namespaces_41
                                                             _seed_56))
                                                   (cdr _token_53)
                                                   _seed_52))
                                                (lambda (_elems_55
                                                         _entities_56
                                                         _namespaces_57
                                                         _seed_58)
                                                  (_element-parser_47
                                                    (cdr _token_53)
                                                    _port_51
                                                    _elems_55
                                                    _entities_56
                                                    _namespaces_57
                                                    #f
                                                    _seed_58)))
                                              (parser-error
                                                _port_51
                                                "XML [22], unexpected markup "
                                                _token_53)))))
                                      (car _token_53))))
                                 (ssax:scan-Misc _port_51))))
                            (_handle-decl_50
                              (lambda (_port_51 _token-head_52 _seed_53)
                                (begin
                                  ((lambda (_x_54)
                                     (if _x_54
                                       _x_54
                                       (parser-error
                                         _port_51
                                         "XML [22], expected DOCTYPE declaration, found "
                                         _token-head_52)))
                                   (eq? (string->symbol "DOCTYPE")
                                        _token-head_52))
                                  (assert-curr-char
                                    ssax:S-chars
                                    "XML [28], space after DOCTYPE"
                                    _port_51)
                                  (ssax:skip-S _port_51)
                                  ((lambda (_docname_54)
                                     ((lambda (_systemid_55)
                                        ((lambda (_internal-subset?_56)
                                           (call-with-values
                                             (lambda ()
                                               ((lambda (_port_57
                                                         _docname_58
                                                         _systemid_59
                                                         _internal-subset?_60
                                                         _seed_61)
                                                  (begin
                                                    (when _internal-subset?_60
                                                          (ssax:warn
                                                            _port_57
                                                            "Internal DTD subset is not currently handled ")
                                                          (ssax:skip-internal-dtd
                                                            _port_57))
                                                    (ssax:warn
                                                      _port_57
                                                      "DOCTYPE DECL "
                                                      _docname_58
                                                      " "
                                                      _systemid_59
                                                      " found and skipped")
                                                    (values #f
                                                            '()
                                                            _namespaces_41
                                                            _seed_61)))
                                                _port_51
                                                _docname_54
                                                _systemid_55
                                                _internal-subset?_56
                                                _seed_53))
                                             (lambda (_elems_57
                                                      _entities_58
                                                      _namespaces_59
                                                      _seed_60)
                                               (_scan-for-significant-prolog-token-2_48
                                                 _port_51
                                                 _elems_57
                                                 _entities_58
                                                 _namespaces_59
                                                 _seed_60))))
                                         (begin
                                           (ssax:skip-S _port_51)
                                           (eqv? #\[
                                                 (assert-curr-char
                                                   '(#\> #\[)
                                                   "XML [28], end-of-DOCTYPE"
                                                   _port_51)))))
                                      (if (ssax:ncname-starting-char?
                                            (ssax:skip-S _port_51))
                                        (ssax:read-external-id _port_51)
                                        #f)))
                                   (ssax:read-QName _port_51))))))
                     (_scan-for-significant-prolog-token-1_49
                       _port_45
                       _seed_46)))
                 _port_38
                 '()))))))
(define SSAX:XML->SXML ssax:xml->sxml)
(letrec ((_test_44
           (lambda (_str_45 _namespace-assig_46 _expected-res_47)
             (begin
               (newline)
               (display "input: ")
               (write (unesc-string _str_45))
               (newline)
               (display "Result: ")
               ((lambda (_result_48)
                  (begin
                    (pp _result_48)
                    (assert (equal_? _result_48 _expected-res_47))))
                (call-with-input-string
                  (unesc-string _str_45)
                  (lambda (_port_48)
                    (ssax:xml->sxml _port_48 _namespace-assig_46))))))))
  (begin
    (_test_44 " <BR/>" '() '(*TOP* (BR)))
    (_test_44 "<BR></BR>" '() '(*TOP* (BR)))
    (_test_44
      " <BR CLEAR='ALL'%nCLASS='Class1'/>"
      '()
      '(*TOP* (BR (@ (CLEAR "ALL") (CLASS "Class1")))))
    (_test_44
      "   <A HREF='URL'>  link <I>itlink </I> &amp;amp;</A>"
      '()
      '(*TOP* (A (@ (HREF "URL")) "  link " (I "itlink ") " &amp;")))
    (_test_44
      "   <A HREF='URL' xml:space='preserve'>  link <I>itlink </I> &amp;amp;</A>"
      '()
      '(*TOP* (A (@ (xml:space "preserve") (HREF "URL"))
                 "  link "
                 (I "itlink ")
                 " &amp;")))
    (_test_44
      "   <A HREF='URL' xml:space='preserve'>  link <I xml:space='default'>itlink </I> &amp;amp;</A>"
      '()
      '(*TOP* (A (@ (xml:space "preserve") (HREF "URL"))
                 "  link "
                 (I (@ (xml:space "default")) "itlink ")
                 " &amp;")))
    (_test_44
      " <P><?pi1  p1 content ?>?<?pi2 pi2? content? ??></P>"
      '()
      '(*TOP* (P (*PI* pi1 "p1 content ") "?" (*PI* pi2 "pi2? content? ?"))))
    (_test_44
      " <P>some text <![CDATA[<]]>1%n&quot;<B>strong</B>&quot;%r</P>"
      '()
      (_list_25
        '*TOP*
        (_list_25
          'P
          (unesc-string "some text <1%n\"")
          '(B "strong")
          (unesc-string "\"%n"))))
    (_test_44
      " <P><![CDATA[<BR>%n<![CDATA[<BR>]]&gt;]]></P>"
      '()
      (_list_25 '*TOP* (_list_25 'P (unesc-string "<BR>%n<![CDATA[<BR>]]>"))))
    (_test_44
      "<T1><T2>it&apos;s%r%nand   that%n</T2>%r%n%r%n%n</T1>"
      '()
      (_list_25
        '*TOP*
        (_list_25 'T1 (_list_25 'T2 (unesc-string "it's%nand   that%n")))))
    (_test_44
      "<T1><T2>it&apos;s%rand   that%n</T2>%r%n%r%n%n</T1>"
      '()
      (_list_25
        '*TOP*
        (_list_25 'T1 (_list_25 'T2 (unesc-string "it's%nand   that%n")))))
    (_test_44
      "<!DOCTYPE T SYSTEM 'system1' ><!-- comment -->%n<T/>"
      '()
      '(*TOP* (T)))
    (_test_44
      "<?xml version='1.0'?>%n<WEIGHT unit=\"pound\">%n<NET certified='certified'> 67 </NET>%n<GROSS> 95 </GROSS>%n</WEIGHT>"
      '()
      '(*TOP* (*PI* xml "version='1.0'")
              (WEIGHT (@ (unit "pound"))
                      (NET (@ (certified "certified")) " 67 ")
                      (GROSS " 95 "))))
    (_test_44
      "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
      '()
      '(*TOP* (URI1:DIV (@ (URI1:B "A") (B "B")) (URI1:P (BR)))))
    (_test_44
      "<DIV A:B='A' B='B' xmlns:A='URI1' xmlns='URI1'><A:P xmlns=''><BR/></A:P></DIV>"
      '((UA . "URI1"))
      '(*TOP* (@ (*NAMESPACES* (UA "URI1")))
              (UA:DIV (@ (UA:B "A") (B "B")) (UA:P (BR)))))
    (_test_44
      (string-append
        "<x xmlns:edi='http://ecommerce.org/schema'>"
        "<!-- the 'taxClass' attribute's  ns http://ecommerce.org/schema -->"
        "<lineItem edi:taxClass='exempt'>Baby food</lineItem>"
        nl
        "</x>")
      '()
      '(*TOP* (x (lineItem
                   (@ (http://ecommerce.org/schema:taxClass "exempt"))
                   "Baby food"))))
    (_test_44
      (string-append
        "<x xmlns:edi='http://ecommerce.org/schema'>"
        "<!-- the 'taxClass' attribute's  ns http://ecommerce.org/schema -->"
        "<lineItem edi:taxClass='exempt'>Baby food</lineItem>"
        "</x>")
      '((EDI . "http://ecommerce.org/schema"))
      '(*TOP* (@ (*NAMESPACES* (EDI "http://ecommerce.org/schema")))
              (x (lineItem (@ (EDI:taxClass "exempt")) "Baby food"))))
    (_test_44
      (string-append
        "<bk:book xmlns:bk='urn:loc.gov:books' "
        "xmlns:isbn='urn:ISBN:0-395-36341-6'>"
        "<bk:title>Cheaper by the Dozen</bk:title>"
        "<isbn:number>1568491379</isbn:number></bk:book>")
      '()
      '(*TOP* (urn:loc.gov:books:book
                (urn:loc.gov:books:title "Cheaper by the Dozen")
                (urn:ISBN:0-395-36341-6:number "1568491379"))))
    (_test_44
      (string-append
        "<!-- initially, the default namespace is 'books' -->"
        "<book xmlns='urn:loc.gov:books' "
        "xmlns:isbn='urn:ISBN:0-395-36341-6'>"
        "<title>Cheaper by the Dozen</title>"
        "<isbn:number>1568491379</isbn:number>"
        "<notes>"
        "<!-- make HTML the default namespace for some commentary -->"
        "<p xmlns='urn:w3-org-ns:HTML'>"
        "This is a <i>funny</i> book!"
        "</p>"
        "</notes>"
        "</book>")
      '()
      '(*TOP* (urn:loc.gov:books:book
                (urn:loc.gov:books:title "Cheaper by the Dozen")
                (urn:ISBN:0-395-36341-6:number "1568491379")
                (urn:loc.gov:books:notes
                  (urn:w3-org-ns:HTML:p
                    "This is a "
                    (urn:w3-org-ns:HTML:i "funny")
                    " book!")))))
    (_test_44
      (string-append
        "<Beers>"
        "<!-- the default namespace is now that of HTML -->"
        "<table xmlns='http://www.w3.org/TR/REC-html40'>"
        "<th><td>Name</td><td>Origin</td><td>Description</td></th>"
        "<tr>"
        "<!-- no default namespace inside table cells -->"
        "<td><brandName xmlns=\"\">Huntsman</brandName></td>"
        "<td><origin xmlns=''>Bath, UK</origin></td>"
        "<td>"
        "<details xmlns=''><class>Bitter</class><hop>Fuggles</hop>"
        "<pro>Wonderful hop, light alcohol, good summer beer</pro>"
        "<con>Fragile; excessive variance pub to pub</con>"
        "</details>"
        "</td>"
        "</tr>"
        "</table>"
        "</Beers>")
      '((html . "http://www.w3.org/TR/REC-html40"))
      '(*TOP* (@ (*NAMESPACES* (html "http://www.w3.org/TR/REC-html40")))
              (Beers (html:table
                       (html:th (html:td "Name")
                                (html:td "Origin")
                                (html:td "Description"))
                       (html:tr (html:td (brandName "Huntsman"))
                                (html:td (origin "Bath, UK"))
                                (html:td (details (class "Bitter")
                                                  (hop "Fuggles")
                                                  (pro "Wonderful hop, light alcohol, good summer beer")
                                                  (con "Fragile; excessive variance pub to pub"))))))))
    (_test_44
      (string-append
        "<!-- 1 --><RESERVATION xmlns:HTML='http://www.w3.org/TR/REC-html40'>"
        "<!-- 2 --><NAME HTML:CLASS=\"largeSansSerif\">Layman, A</NAME>"
        "<!-- 3 --><SEAT CLASS='Y' HTML:CLASS=\"largeMonotype\">33B</SEAT>"
        "<!-- 4 --><HTML:A HREF='/cgi-bin/ResStatus'>Check Status</HTML:A>"
        "<!-- 5 --><DEPARTURE>1997-05-24T07:55:00+1</DEPARTURE></RESERVATION>")
      '((HTML . "http://www.w3.org/TR/REC-html40"))
      '(*TOP* (@ (*NAMESPACES* (HTML "http://www.w3.org/TR/REC-html40")))
              (RESERVATION
                (NAME (@ (HTML:CLASS "largeSansSerif")) "Layman, A")
                (SEAT (@ (HTML:CLASS "largeMonotype") (CLASS "Y")) "33B")
                (HTML:A (@ (HREF "/cgi-bin/ResStatus")) "Check Status")
                (DEPARTURE "1997-05-24T07:55:00+1"))))
    (_test_44
      (string-concatenate/shared
        (list-intersperse
          '("<?xml version='1.0' encoding='utf-8' standalone='yes'?>"
            "<!-- this can be decoded as US-ASCII or iso-8859-1 as well,"
            "  since it contains no characters outside the US-ASCII repertoire -->"
            "<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'"
            "         xmlns:rdfs='http://www.w3.org/2000/01/rdf-schema#'"
            "          xmlns='http://www.w3.org/2001/02/infoset#'>"
            "<rdfs:Class ID='Boolean'/>"
            "<Boolean ID='Boolean.true'/>"
            "<Boolean ID='Boolean.false'/>"
            "<!--Info item classes-->"
            "<rdfs:Class ID='InfoItem'/>"
            "<rdfs:Class ID='Document' rdfs:subClassOf='#InfoItem'/>"
            "<rdfs:Class ID='Element' rdfs:subClassOf='#InfoItem'/>"
            "<rdfs:Class ID='Attribute' rdfs:subClassOf='#InfoItem'/>"
            "<rdfs:Class ID='InfoItemSet'\n      rdfs:subClassOf='http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag'/>"
            "<rdfs:Class ID='AttributeSet' rdfs:subClassOf='#InfoItemSet'/>"
            "<!--Info item properties-->"
            "<rdfs:Property ID='allDeclarationsProcessed'>"
            "<rdfs:domain resource='#Document'/>"
            "<rdfs:range resource='#Boolean'/></rdfs:Property>"
            "<rdfs:Property ID='attributes'>"
            "<rdfs:domain resource='#Element'/>"
            "<rdfs:range resource='#AttributeSet'/>"
            "</rdfs:Property>"
            "</rdf:RDF>")
          (string #\newline)))
      '((RDF . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        (RDFS . "http://www.w3.org/2000/01/rdf-schema#")
        (ISET . "http://www.w3.org/2001/02/infoset#"))
      '(*TOP* (@ (*NAMESPACES*
                   (RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                   (RDFS "http://www.w3.org/2000/01/rdf-schema#")
                   (ISET "http://www.w3.org/2001/02/infoset#")))
              (*PI* xml "version='1.0' encoding='utf-8' standalone='yes'")
              (RDF:RDF (RDFS:Class (@ (ID "Boolean")))
                       (ISET:Boolean (@ (ID "Boolean.true")))
                       (ISET:Boolean (@ (ID "Boolean.false")))
                       (RDFS:Class (@ (ID "InfoItem")))
                       (RDFS:Class
                         (@ (RDFS:subClassOf "#InfoItem") (ID "Document")))
                       (RDFS:Class
                         (@ (RDFS:subClassOf "#InfoItem") (ID "Element")))
                       (RDFS:Class
                         (@ (RDFS:subClassOf "#InfoItem") (ID "Attribute")))
                       (RDFS:Class
                         (@ (RDFS:subClassOf
                              "http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag")
                            (ID "InfoItemSet")))
                       (RDFS:Class
                         (@ (RDFS:subClassOf "#InfoItemSet")
                            (ID "AttributeSet")))
                       (RDFS:Property
                         (@ (ID "allDeclarationsProcessed"))
                         (RDFS:domain (@ (resource "#Document")))
                         (RDFS:range (@ (resource "#Boolean"))))
                       (RDFS:Property
                         (@ (ID "attributes"))
                         (RDFS:domain (@ (resource "#Element")))
                         (RDFS:range (@ (resource "#AttributeSet")))))))
    (_test_44
      (string-concatenate/shared
        (list-intersperse
          '("<?xml version='1.0'?><rdf:RDF "
            "xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#' "
            "xmlns='http://my.netscape.com/rdf/simple/0.9/'>"
            "<channel>"
            "<title>Daemon News Mall</title>"
            "<link>http://mall.daemonnews.org/</link>"
            "<description>Central source for all your BSD needs</description>"
            "</channel>"
            "<item>"
            "<title>Daemon News Jan/Feb Issue NOW Available! Subscribe $24.95</title>"
            "<link>http://mall.daemonnews.org/?page=shop/flypage&amp;product_id=880</link>"
            "</item>"
            "<item>"
            "<title>The Design and Implementation of the 4.4BSD Operating System $54.95</title>"
            "<link>http://mall.daemonnews.org/?page=shop/flypage&amp;product_id=912&amp;category_id=1761</link>"
            "</item>"
            "</rdf:RDF>")
          (string #\newline)))
      '((RDF . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        (RSS . "http://my.netscape.com/rdf/simple/0.9/")
        (ISET . "http://www.w3.org/2001/02/infoset#"))
      '(*TOP* (@ (*NAMESPACES*
                   (RDF "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                   (RSS "http://my.netscape.com/rdf/simple/0.9/")
                   (ISET "http://www.w3.org/2001/02/infoset#")))
              (*PI* xml "version='1.0'")
              (RDF:RDF (RSS:channel
                         (RSS:title "Daemon News Mall")
                         (RSS:link "http://mall.daemonnews.org/")
                         (RSS:description
                           "Central source for all your BSD needs"))
                       (RSS:item
                         (RSS:title
                           "Daemon News Jan/Feb Issue NOW Available! Subscribe $24.95")
                         (RSS:link
                           "http://mall.daemonnews.org/?page=shop/flypage&product_id=880"))
                       (RSS:item
                         (RSS:title
                           "The Design and Implementation of the 4.4BSD Operating System $54.95")
                         (RSS:link
                           "http://mall.daemonnews.org/?page=shop/flypage&product_id=912&category_id=1761")))))
    (_test_44
      (string-concatenate/shared
        (list-intersperse
          '("<Forecasts TStamp='958082142'>"
            "<TAF TStamp='958066200' LatLon='36.583, -121.850' BId='724915'"
            "  SName='KMRY, MONTEREY PENINSULA'>"
            "<VALID TRange='958068000, 958154400'>111730Z 111818</VALID>"
            "<PERIOD TRange='958068000, 958078800'>"
            "<PREVAILING>31010KT P6SM FEW030</PREVAILING>"
            "</PERIOD>"
            "<PERIOD TRange='958078800, 958104000' Title='FM2100'>"
            "<PREVAILING>29016KT P6SM FEW040</PREVAILING>"
            "</PERIOD>"
            "<PERIOD TRange='958104000, 958154400' Title='FM0400'>"
            "<PREVAILING>29010KT P6SM SCT200</PREVAILING>"
            "<VAR Title='BECMG 0708' TRange='958114800, 958118400'>VRB05KT</VAR>"
            "</PERIOD></TAF>"
            "</Forecasts>")
          (string #\newline)))
      '()
      '(*TOP* (Forecasts
                (@ (TStamp "958082142"))
                (TAF (@ (TStamp "958066200")
                        (SName "KMRY, MONTEREY PENINSULA")
                        (LatLon "36.583, -121.850")
                        (BId "724915"))
                     (VALID (@ (TRange "958068000, 958154400"))
                            "111730Z 111818")
                     (PERIOD (@ (TRange "958068000, 958078800"))
                             (PREVAILING "31010KT P6SM FEW030"))
                     (PERIOD (@ (Title "FM2100")
                                (TRange "958078800, 958104000"))
                             (PREVAILING "29016KT P6SM FEW040"))
                     (PERIOD (@ (Title "FM0400")
                                (TRange "958104000, 958154400"))
                             (PREVAILING "29010KT P6SM SCT200")
                             (VAR (@ (Title "BECMG 0708")
                                     (TRange "958114800, 958118400"))
                                  "VRB05KT"))))))))
(newline)
(display "All tests passed")
(newline)
