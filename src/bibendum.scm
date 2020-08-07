(import (scheme base))
(import (scheme write))
(import (scheme list))
(import (scheme char))
(import (scheme charset))
(import (scheme file))
(import (scheme hash-table))
(import (scheme comparator))
(import (scheme time))
(import (scheme process-context))

(import (only (chezscheme) random random-seed))


(define (read-lines filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((line (read-line port))
                 (out '()))
        (if (eof-object? line)
            (reverse out)
            (loop (read-line port) (cons line out)))))))

(define (maybe-human-message line)
  (let loop ((line (drop (string->list line) 21)))
    (cond
     ((null? line) #f)
     ((char=? (car line) #\space) #f)
     ((char=? (car line) #\:) (list->string (cddr line)))
     (else (loop (cdr line))))))

(define (string-downcase string)
  (list->string (map char-downcase (string->list string))))

(define (string-replace-punctuation string)
  (define (punctuation->space char)
    (if (char-set-contains? char-set:punctuation char)
        #\space
        char))
  
  (list->string (map punctuation->space (string->list string))))

(define (string-tokenize string)
  (define (empty? string)
    (not (or (null? string)
             (zero? (string-length string)))))
  
  (let loop ((chars (string->list string))
             (out '(())))
    (if (null? chars)
        (filter empty? (reverse (cons (list->string (reverse (car out))) (cdr out))))
        (if (char=? (car chars) #\space)
            (loop (cdr chars) (cons '() (cons (list->string (reverse (car out))) (cdr out))))
            (loop (cdr chars) (cons (cons (car chars) (car out)) (cdr out)))))))

(define (ngrams tokens n)
  ;; https://stackoverflow.com/a/17532044/140837
  (let loop ((indices (iota (+ (- (length tokens) n) 1)))
             (out '()))
    (if (null? indices)
        (reverse out)
        (loop (cdr indices) (cons (take (drop tokens (car indices)) n) out)))))
        

(define (make-hash-table*)
  (make-hash-table (make-default-comparator)))


(define (kb-store kb grams)
  (let loop ((grams grams))
    (unless (null? grams)
      (let* ((gram (car grams))
             (key (cons (length gram) gram))
             (value (hash-table-ref/default kb key 0)))
        (hash-table-set! kb key (+ value 1))
        (loop (cdr grams))))))
                       
(define filename "data/scheme-2020-04.txt")

(define kb (make-hash-table*))

(let loop ((lines (read-lines filename)))
  (unless (null? lines)
    (let ((maybe (maybe-human-message (car lines))))
      (when maybe
        (let* ((tokens (string-tokenize (string-replace-punctuation (string-downcase maybe))))
               (tokens* (append (list "%") tokens (list "%")))
               (grams (append-map (lambda (n) (ngrams tokens* n)) (cddr (iota (min 5 (length tokens*)))))))
          (kb-store kb grams)))
      (loop (cdr lines)))))

(define (big pair)
  (< 1 (cdr pair)))

(define kb* (hash-table->alist kb))

(define (choice list)
  (list-ref list (random (length list))))

(define (continue* kb* lst) 
  (let loop ((kbn (filter (lambda (pair) (= (caar pair) (+ (length lst) 1))) kb*))
             (out '()))
    (if (null? kbn)
        (if (null? out)
            #f
            (choice (cons #f (map caar out))))
        (let ((head (car kbn)))
          (if (equal? (take (cdar head) (length lst)) lst)
              (loop (cdr kbn) (cons (cons (drop (cdar head) (length lst)) (cdr head)) out))
              (loop (cdr kbn) out))))))

(define (string-join strings)
  (let loop ((strings strings)
             (out '()))
    (if (null? strings)
        (apply string-append (reverse (cdr out)))
        (loop (cdr strings) (cons " " (cons (car strings) out))))))

(random-seed (modulo (exact (current-second)) (expt 2 32)))

(let loop ((sentence (list "%" "r7rs" "is"))
           (prefix 1))
  (if (zero? prefix)
      (pk (string-join sentence))
      (let ((next (continue* kb* (drop sentence (- (length sentence) prefix)))))
        (cond
         ((not next)
          (loop sentence
                (- prefix 1)))
         ((string=? next "%") (pk (string-join sentence)))
         (else (loop (append sentence (list next))
                     (+ (length sentence) 1)))))))
