#lang racket

(require "parser.rkt"
         "../config.rkt"
         racket/serialize)

(provide sql-completion)

(define current-database (make-parameter #f))

(define (get-database)
  (or (current-database)
      (begin 
        (current-database (deserialize (call-with-input-file (get-config 'sql-database-file) read)))
        (current-database))))

(define/contract (sql-completion str position)
  (-> string? (or/c 0 (and/c integer? positive?)) any/c)
  (define tokens (call-with-input-string str lex-all-without-blank))
  (cons 'sql-completion (find-candidates tokens position (get-database))))
