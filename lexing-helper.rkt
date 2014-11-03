#lang racket

(require parser-tools/lex)

(module+ test
  (require rackunit
           parser-tools/lex))

(provide matching-balanced-pair
         position-token-name
         position-token-value
         extract-from-to
         read-until
         read-until-eof)

(define (position-token-value tok)
  (token-value (position-token-token tok)))

(define (position-token-name tok)
  (token-name (position-token-token tok)))

(define (matching-balanced-pair tokens pairs)
  ;;  (-> (listof position-token?) hash? (values/c (listof position-token?)))
  (define first-token (position-token-name (first tokens)))
  (if (hash-ref pairs first-token #f)
      (let loop ([stack (list (hash-ref pairs first-token))]
                 [result (list (first tokens))]
                 [toks (rest tokens)])
        (if (or (empty? toks) (empty? stack))
            (reverse result)
            (let* ([tok (first toks)]
                   [value (position-token-name tok)])
              (loop
               (cond
                [(hash-ref pairs value #f)
                 (cons (hash-ref pairs value) stack)]
                [(and (not (empty? stack))
                      (equal? (first stack) value))
                 (rest stack)]
                [else
                 stack])
               (cons tok result)
               (rest toks)))))
      empty))

(define (lex-all lexer input)
  (let loop ([current (lexer input)]
             [result '()])
    (case (position-token-name current)
      [(EOF)
       (reverse (cons current result))]
      [else
       (loop (lexer input)
             (cons current result))])))

(module+ test
  (define lexer
    (lexer-src-pos
     ["a" 'A]
     ["b" 'B]
     ["c" 'C]
     ["(" 'OPAREN]
     [")" 'CPAREN]
     [(eof) 'EOF]))

  (let ([data0 (lex-all lexer (open-input-string "a(ab)c"))]
        [data1 (lex-all lexer (open-input-string "(ab)"))]
        [data2 (lex-all lexer (open-input-string "()b"))]
        [data3 (lex-all lexer (open-input-string "((a)b)"))]
        [data4 (lex-all lexer (open-input-string "((ab)a"))])
    (check-equal?
     (map position-token-name
          (matching-balanced-pair data0 #hash((OPAREN . CPAREN))))
     '())
    (check-equal?
     (map position-token-name
          (matching-balanced-pair data1 #hash((OPAREN . CPAREN))))
     '(OPAREN A B CPAREN))
    (check-equal?
     (map position-token-name
          (matching-balanced-pair data2 #hash((OPAREN . CPAREN))))
     '(OPAREN CPAREN))
    (check-equal?
     (map position-token-name
          (matching-balanced-pair data3 #hash((OPAREN . CPAREN))))
     '(OPAREN OPAREN A CPAREN B CPAREN))
    (check-equal?
     (map position-token-name
          (matching-balanced-pair data4 #hash((OPAREN . CPAREN))))
     '(OPAREN OPAREN A B CPAREN A EOF))))



(define (read-until until input-port)
  (define (loop data block)
    (if (equal? until block)
        data
        (let ([read/c (read-char input-port)])
          (if (eof-object? read/c)
              data
              (loop (string-append data (string read/c))
                    (string-append
                     (substring block 1 (string-length block))
                     (string read/c)))))))
  (define block (read-until-eof input-port (string-length until)))
  (loop block block))

(define (read-until-eof input-port size)
  (define (loop data size)
    (if (<= size 0)
        data
        (let ([c (read-char input-port)])
          (if (eof-object? c)
              data
              (loop (string-append data (string c))
                    (- size 1))))))
  (loop "" size))

(define (extract-from-to from to tokens [include-from? #t] [include-to? #f])
  (define (coerce-list elem) (if (list? elem) elem (list elem)))
  (define from-list (coerce-list from))
  (define to-list (coerce-list to))
  (let loop ([tokens tokens]
             [result (list)]
             [start #f])
    (cond
     [(empty? tokens) (reverse result)]
     [(member (position-token-name (first tokens)) from-list)
      (loop (rest tokens)
            (if include-from?
                (cons (first tokens) result)
                result)
            #t)]
     [(and start (member (position-token-name (first tokens)) to-list))
      (reverse
       (if include-to?
           (cons (first tokens) result)
           result))]
     [start
      (loop (rest tokens)
            (cons (first tokens) result)
            #t)]
     [else
      (loop (rest tokens)
            result
            start)])))

(module+ test
  (let ([data (lex-all lexer (open-input-string "a(ab)c"))])
    (check-equal?
     (map position-token-name (extract-from-to '(A) '(B) data #t #t))
     '(A OPAREN A B))))
