#lang racket

;; Because it's a little stupid to use another format than SQL to store the
;; database, so use the SQL format.

(require "parser.rkt"
         "../lexing-helper.rkt"
         "struct.rkt"
         racket/generator)

(provide read-database)

(module+ test
  (require rackunit))

(define (start-of-create-table? tokens)
  (match tokens
    [(list-rest (app position-token-name 'CREATE) (app position-token-name 'TABLE) _) #t]
    [else #f]))

(define (read-until-semicolon tokens)
  (let loop ([toks tokens]
             [result empty])
    (if (empty? toks)
        (reverse result)
        (let ([tok (first toks)])
          (if (equal? (position-token-name tok) 'SEMICOLON)
              (reverse (cons tok result))
              (let ([balanced (matching-balanced-pair toks #hash((OPAREN . CPAREN)))])
                (if (empty? balanced)
                    (loop
                     (rest toks)
                     (cons tok result))
                    (loop
                     (drop toks (length balanced))
                     (append (reverse balanced) result)))))))))

;; conserv only create table statements
(define (only-created-table input-port)
  (let loop ([tokens (lex-all input-port)]
             [result empty])
    (if (empty? tokens)
        (reverse result)
        (let ([statement (read-until-semicolon tokens)])
          (loop
           (drop tokens (length statement))
           (if (start-of-create-table? tokens)
            (cons
             (read-until-semicolon tokens)
             result)
            result))))))

;; TODO: missing the alter column that add primary and the kind

(module+ test
  (check-=
   (length (only-created-table (open-input-file "test/schema.sql")))
   5
   0))

(define (parse-create-table input-port)
  (define tokens
    (begin0
        (only-created-table input-port)
      (close-input-port input-port)))
  (for/list ([toks tokens])
    (create-table-parser (sequence->generator toks))))

;; TODO: added real but does it is similar to numeric?
;; TODO: character does it similar to something? varchar

(define (read-database in)
  (Db (for/hash ([d (parse-create-table in)])
        (values (Table-name d) d))))
