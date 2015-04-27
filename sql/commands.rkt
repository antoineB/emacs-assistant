#lang racket

(require "parser.rkt"
         "../config.rkt"
         "terminate.rkt"
         "struct.rkt"
         "read-database.rkt")

(provide sql-completion
         sql-terminate)

(define current-database (make-parameter #f))

(define (get-database)
  (or (current-database)
      (begin
        (current-database (call-with-input-file (get-config 'sql-database-file) read-database))
        (current-database))))

(define/contract (sql-completion str position)
  (-> string? (or/c 0 (and/c integer? positive?)) any/c)
  (define tokens (call-with-input-string str lex-all-without-blank))
  (cons 'sql-completion (find-candidates tokens position (get-database))))


(define (sql-terminate str)
  (define tokens (call-with-input-string str lex-all-without-blank))
  (cons 'sql-terminate
        (if (select-request? tokens)
             (terminate-select-request (split-select-request tokens) (get-database))
            empty)))
