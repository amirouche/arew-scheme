;;; Copyright (C) William D Clinger (2016). All Rights Reserved.
;;; Copyright (C) Amirouche Boubekki (2020). All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
(library (srfi srfi-135)
  (export text?
          textual?
          textual-null?
          textual-every
          textual-any
          make-text
          text
          text-tabulate
          text-unfold
          text-unfold-right
          textual->text
          textual->string
          textual->vector
          textual->list
          string->text
          vector->text
          list->text
          reverse-list->text
          textual->utf8
          textual->utf16
          textual->utf16be
          textual->utf16le
          utf8->text
          utf16->text
          utf16be->text
          utf16le->text
          text-length
          text-ref
          textual-length
          textual-ref
          subtext
          subtextual
          textual-copy
          textual-take
          textual-drop
          textual-take-right
          textual-drop-right
          textual-pad
          textual-pad-right
          textual-trim
          textual-trim-right
          textual-trim-both
          textual-replace
          textual=?
          textual<?
          textual>?
          textual<=?
          textual>=?
          textual-ci=?
          textual-ci<?
          textual-ci>?
          textual-ci<=?
          textual-ci>=?
          textual-prefix-length
          textual-suffix-length
          textual-prefix?
          textual-suffix?
          textual-index
          textual-index-right
          textual-skip
          textual-skip-right
          textual-contains
          textual-contains-right
          textual-upcase
          textual-downcase
          textual-foldcase
          textual-titlecase
          textual-append
          textual-concatenate
          textual-concatenate-reverse
          textual-join
          textual-fold
          textual-fold-right
          textual-map
          textual-for-each
          textual-map-index
          textual-for-each-index
          textual-count
          textual-filter
          textual-remove
          textual-replicate
          textual-split)
  (import (chezscheme))

  (define text? immutable-text?)

  (define textual? string?)

  (define textual-null? string-null?)

  (define-syntax define-textual-start-end
    (syntax-rules ()
      ((_ (f args ... textual start end) expr1 expr2 ...)
       (define f
         ;; Don't change this to letrec or an internal definition,
         ;; because recursive calls should call the version that checks.
         (let ((f
                (lambda (args ... textual start end) expr1 expr2 ...)))
           (case-lambda
             ((args ... textual)
              (let ((text (textual->text textual f args ... textual)))
                (f args ... text 0 (%text-length text))))
             ((args ... textual start)
              (let* ((text (textual->text textual f args ... textual start))
                     (n (%text-length text)))
                (if (and (exact-integer? start)
                         (<= 0 start n))
                    (f args ... text start n)
                    (complain 'f args ... textual start))))
             ((args ... textual start end)
              (let* ((text (textual->text textual f args ... textual start end))
                     (n (%text-length text)))
                (if (and (exact-integer? start)
                         (exact-integer? end)
                         (<= 0 start end n))
                    (f args ... text start end)
                    (complain 'f args ... textual start end))))))))))

  (define-textual-start-end (textual-every pred textual start end)
    (if (= start end)
        #t
        (let ((end-1 (- end 1)))
          (let loop ((i start))
            (if (= i end-1)
                (pred (%text-ref textual i))
                (and (pred (%text-ref textual i))
                     (loop (+ i 1))))))))


  (define-textual-start-end (textual-any pred textual start end)
    (let loop ((i start))
      (if (= i end)
          #f
          (or (pred (%text-ref textual i))
              (loop (+ i 1))))))

  (define (make-text len char)
    (string->immutable-string (make-string len char)))

  (define (text . chars)
    (string->immutable-string (string chars)))

  (define %empty-string (text))

  (define (text-tabulate proc len)
    (if (= len 0)
        %empty-string
        (let loop ((index len)
                   (out '()))
          (if (zero? index)
              (text out)
              (loop (- index 1) (cons (proc (- index 1)) out))))))

  ;; TODO: replace with text-error?
  (define (%bad-final name final)
    (error (string-append (symbol->string name)
                          " : make-final returned illegal value : ")
           final))

  ;; TODO: replace with a text-error?
  (define (complain name . args)
    (apply error
           (string-append (symbol->string name) ": illegal arguments")
           args))

  (define text-unfold
    (case-lambda
      ((stop? mapper succ seed)
       (text-unfold stop? mapper succ seed %empty-string (lambda (x) %empty-string)))
      ((stop? mapper succ seed base)
       (text-unfold stop? mapper succ seed base (lambda (x) %empty-string)))
      ((stop? mapper succ seed base make-final)
       (let* ((txt (textual->text (if (char? base) (text base) base)))
              (k (text-length txt)))
         (let loop ((k k)
                    (texts (list txt))
                    (chars '())
                    (seed seed))
           (cond ((>= k N)
                  (let* ((k/N   (quotient k N))
                         (k     (- k (* k/N N)))
                         (texts (cons (reverse-list->text (list-tail chars k))
                                      texts))
                         (chars (list-take chars k)))
                    (loop k texts chars seed)))
                 ((stop? seed)
                  (let* ((texts (if (null? chars)
                                    texts
                                    (cons (reverse-list->text chars) texts)))
                         (final (make-final seed))
                         (final (cond ((char? final) (text final))
                                      ((string? final) (string->text final))
                                      ((text? final) final)
                                      (else
                                       (%bad-final 'text-unfold final)))))
                    (textual-concatenate-reverse texts final)))
                 (else
                  (let ((x (mapper seed)))
                    (cond ((char? x)
                           (loop (+ k 1)
                                 texts
                                 (cons x chars)
                                 (succ seed)))
                          ((string? x)
                           (loop (+ k (string-length x))
                                 texts
                                 (append (reverse (string->list x)) chars)
                                 (succ seed)))
                          ((text? x)
                           (loop (+ k (text-length x))
                                 texts
                                 (append (reverse (textual->list x)) chars)
                                 (succ seed)))
                          (else
                           (complain 'text-unfold
                                     stop? mapper succ seed
                                     base make-final)))))))))))

  (define text-unfold-right
    (case-lambda
      ((stop? mapper succ seed)
       (text-unfold-right stop? mapper succ seed (text) (lambda (x) (text))))
      ((stop? mapper succ seed base)
       (text-unfold-right stop? mapper succ seed base (lambda (x) (text))))
      ((stop? mapper succ seed base make-final)
       (let* ((txt (textual->text (if (char? base) (text base) base)
                                   'text-unfold-right
                                   stop? mapper succ seed base make-final))
              (k (text-length txt)))
         (let loop ((k k)
                    (texts (list txt))
                    (chars '())
                    (seed seed))
           (cond ((>= k N)
                  (let* ((k/N   (quotient k N))
                         (k     (- k (* k/N N)))
                         (texts (cons (list->text (list-tail chars k)) texts))
                         (chars (list-take chars k)))
                    (loop k texts chars seed)))
                 ((stop? seed)
                  (let* ((texts (if (null? chars)
                                    texts
                                    (cons (list->text chars) texts)))
                         (final (make-final seed))
                         (final (cond ((char? final) (text final))
                                      ((string? final) (string->text final))
                                      ((text? final) final)
                                      (else
                                       (%bad-final 'text-unfold-right
                                                   final)))))
                    (textual-concatenate (cons final texts))))
                 (else
                  (let ((x (mapper seed)))
                    (cond ((char? x)
                           (loop (+ k 1)
                                 texts
                                 (cons x chars)
                                 (succ seed)))
                          ((string? x)
                           (loop (+ k (string-length x))
                                 texts
                                 (append (string->list x) chars)
                                 (succ seed)))
                          ((text? x)
                           (loop (+ k (text-length x))
                                 texts
                                 (append (textual->list x) chars)
                                 (succ seed)))
                          (else
                           (complain 'text-unfold-right
                                     stop? mapper succ seed
                                     base make-final)))))))))))

  (define textual->text string->immutable-string)

  (define textual->string
    (case-lambda
      ((txt)
       (if (string? txt)
           txt
           (textual->string txt 0 (textual-length txt))))
      ((txt start)
       (if (string? txt)
           (substring txt start (string-length txt))
           (textual->string txt start (textual-length txt))))
      ((txt start end)
       (let* ((txt (textual->text txt))
              (n (- end start))
              (s (make-string n)))
         (do ((i start (+ i 1)))
             ((= i end)
              s)
           (string-set! s (- i start) (text-ref txt i)))))))

  (define-textual-start-end (textual->vector txt start end)
    (list->vector (string->list (textual->string (subtext txt start end)))))

  (define-textual-start-end (textual->list txt start end)
    (string->list (textual->string (subtext txt start end))))

  (define string->text
    (case-lambda
      ((s)
       (string->text s))
      ((s start)
       (string->text (substring s start (string-length s))))
      ((s start end)
       (string->text (substring s start end)))))

  (define (vector->text v . start/end)
    (string->text (list->string (apply vector->list v start/end))))

  (define (list->text chars . start/end)
    (apply string->text (list->string chars) start/end))

  (define (reverse-list->text chars)
    (string->text (list->string (reverse chars))))

  (define textual->utf8 string->utf8)

  (define textual->utf16 string->utf16)

  (define textual->utf16be string->utf16be)

  (define textual->utf16le string->utf16le)

  (define utf8->text (compose string->text utf8->string))

  (define utf16->text (compose string->text utf16->string))

  (define utf16be->text (compose string->text utf16be->string))

  (define utf16le->text (compose string->text utf16le->string))

  (define text-length string-length)

  (define text-ref string-ref)

  (define textual-length string-length)

  (define textual-ref string-ref)

  (define subtext (compose string->text substring))

  (define subtextual (compose string->text substring))

  (define textual-copy (compose string->text string-copy))

  (define (textual-take txt nchars)
    (subtextual txt 0 nchars))

  (define (textual-drop txt nchars)
    (subtextual txt nchars (text-length txt)))

  (define (textual-take-right txt nchars)
    (let ((n (%text-length txt)))
      (subtextual txt (- n nchars) n)))

  (define (textual-drop-right  txt nchars)
    (let ((n (%text-length txt)))
      (subtextual txt 0 (- n nchars))))

  (define (%text-pad txt len c start end)
    (if (and (exact-integer? len)
             (char? c)
             (exact-integer? start)
             (exact-integer? end)
             (<= 0 len)
             (<= 0 start end))
        (let* ((n (text-length txt))
               (k (- end start)))
          (cond ((not (<= end n))
                 (complain 'textual-pad txt len c start end))
                ((= n k len)
                 txt)
                ((= k len)
                 (if (= n k)
                     txt
                     (subtext txt start end)))
                ((< k len)
                 (textual-append (make-text (- len k) c)
                                 (if (= n k)
                                     txt
                                     (subtext txt start end))))
                (else
                 (subtext txt (- end len) end))))
        (complain 'textual-pad txt len c start end)))

  (define textual-pad
    (case-lambda
      ((txt len)
       (let ((txt (textual->text txt)))
         (%text-pad txt len #\space 0 (text-length txt))))
      ((txt len c)
       (let ((txt (textual->text txt)))
         (%text-pad txt len c 0 (text-length txt))))
      ((txt len c start)
       (let ((txt (textual->text txt)))
         (%text-pad txt len c start (text-length txt))))
      ((txt len c start end)
       (%text-pad (textual->text txt)
                  len c start end))))

  (define (%text-pad-right txt len c start end)
    (if (and (exact-integer? len)
             (char? c)
             (exact-integer? start)
             (exact-integer? end)
             (<= 0 len)
             (<= 0 start end))
        (let* ((n (%text-length txt))
               (k (- end start)))
          (cond ((not (<= end n))
                 (complain 'textual-pad-right txt len c start end))
                ((= n k len)
                 txt)
                ((= k len)
                 (if (= n k)
                     txt
                     (subtext txt start end)))
                ((< k len)
                 (textual-append (if (= n k)
                                     txt
                                     (subtext txt start end))
                                 (make-text (- len k) c)))
                (else
                 (subtext txt start (+ start len)))))
        (complain 'textual-pad-right txt len c start end)))

  (define textual-pad-right
    (case-lambda
      ((txt len)
       (let ((txt (textual->text txt)))
         (%text-pad-right txt len #\space 0 (text-length txt))))
      ((txt len c)
       (let ((txt (textual->text txt)))
         (%text-pad-right txt len c 0 (text-length txt))))
      ((txt len c start)
       (let ((txt (textual->text txt)))
         (%text-pad-right txt len c start (text-length txt))))
      ((txt len c start end)
       (%text-pad-right (textual->text txt)
                        len c start end))))

  (define textual-trim
    (case-lambda
      ((txt)
       (textual-trim txt char-whitespace? 0))
      ((txt pred)
       (textual-trim txt pred 0))
      ((txt pred start)
       (let ((txt (textual->text txt)))
         (%text-trim txt pred start (text-length txt))))
      ((txt pred start end)
       (let ((txt (textual->text txt)))
         (%text-trim txt pred start end)))))

  (define (%text-trim txt pred start end)
    (if (and (procedure? pred)
             (exact-integer? start)
             (exact-integer? end)
             (<= 0 start end (text-length txt)))
        (let loop ((i start))
          (cond ((= i end)
                 (text))
                ((pred (text-ref txt i))
                 (loop (+ i 1)))
                (else
                 (subtext txt i end))))
        (complain 'textual-trim txt pred start end)))


  (define textual-trim-right
    (case-lambda
      ((txt)
       (textual-trim-right txt char-whitespace? 0))
      ((txt pred)
       (textual-trim-right txt pred 0))
      ((txt pred start)
       (let ((txt (textual->text txt)))
         (%text-trim-right txt pred start (text-length txt))))
      ((txt pred start end)
       (let ((txt (textual->text txt)))
         (%text-trim-right txt pred start end)))))

  (define (%text-trim-right txt pred start end)
    (if (and (procedure? pred)
             (exact-integer? start)
             (exact-integer? end)
             (<= 0 start end (text-length txt)))
        (let loop ((i (- end 1)))
          (cond ((< i start)
                 (text))
                ((pred (text-ref txt i))
                 (loop (- i 1)))
                (else
                 (subtext txt start (+ i 1)))))
        (complain 'textual-trim-right txt pred start end)))

  (define textual-trim-both
    (case-lambda
      ((txt)
       (textual-trim-both txt char-whitespace? 0))
      ((txt pred)
       (textual-trim-both txt pred 0))
      ((txt pred start)
       (let ((txt (textual->text txt)))
         (%text-trim-both txt pred start (text-length txt))))
      ((txt pred start end)
       (let ((txt (textual->text txt)))
         (%text-trim-both txt pred start end)))))

  ;; This is efficient because subtext is fast.

  (define (%text-trim-both txt pred start end)
    (if (and (procedure? pred)
             (exact-integer? start)
             (exact-integer? end)
             (<= 0 start end (text-length txt)))
        (textual-trim (textual-trim-right txt pred start end)
                      pred)
        (complain 'textual-trim-both txt pred start end)))

  (define textual-replace
    (case-lambda
      ((txt1 txt2 start1 end1 start2 end2)
       (textual-append (subtextual txt1 0 start1)
                       (subtextual txt2 start2 end2)
                       (subtextual txt1 end1 (textual-length txt1))))
      ((txt1 txt2 start1 end1 start2)
       (textual-append (subtextual txt1 0 start1)
                       (subtextual txt2 start2 (textual-length txt2))
                       (subtextual txt1 end1 (textual-length txt1))))
      ((txt1 txt2 start1 end1)
       (textual-append (subtextual txt1 0 start1)
                       txt2
                       (subtextual txt1 end1 (textual-length txt1))))))

  (define textual=? string=?)

  (define textual<? string<?)

  (define textual>? string>?)

  (define textual<=? string<=?)

  (define textual>=? string>=?)

  (define textual-ci=? string-ci=?)

  (define textual-ci<? string-ci<?)

  (define textual-ci>? string-ci>?)

  (define textual-ci<=? string-ci<=?)

  (define textual-ci>=? string-ci>=?)

  (define (not-implemented . args)
    (raise 'not-implemented))

  (define textual-prefix-length not-implemented)

  (define textual-suffix-length not-implemented)

  (define textual-prefix? not-implemented)

  (define textual-suffix? not-implemented)

  (define textual-index not-implemented)

  (define textual-index-right not-implemented)

  (define textual-skip not-implemented)

  (define textual-skip-right not-implemented)

  (define textual-contains not-implemented)

  (define textual-contains-right not-implemented)

  (define textual-upcase not-implemented)

  (define textual-downcase not-implemented)

  (define textual-foldcase not-implemented)

  (define textual-titlecase not-implemented)

  (define textual-append (compose string->text string-append))

  (define textual-concatenate not-implemented)

  (define textual-concatenate-reverse not-implemented)

  (define textual-join not-implemented)

  (define textual-fold not-implemented)

  (define textual-fold-right not-implemented)

  (define textual-map not-implemented)

  (define textual-for-each not-implemented)

  (define textual-map-index not-implemented)

  (define textual-for-each-index not-implemented)

  (define textual-count not-implemented)

  (define textual-filter not-implemented)

  (define textual-remove not-implemented)

  (define textual-replicate not-implemented)

  (define textual-split not-implemented))
