#lang racket

(provide commands-ns)

(require "sql.rkt"
         "sql-struct.rkt")

(define-namespace-anchor anchor)
(define commands-ns (namespace-anchor->namespace anchor))

(define (bonjour)
  (sleep 5)
  '(bonjour))

(define test-db
  (db
   (hash
    "fireman" (table "fireman" 
                     (list (column "id" "serious")
                           (column "name" "string")
                           (column "rank" "integer")
                           (column "year" "integer")))
    "firetruck" (table "firetruck"
                       (list (column "id" "serial")
                             (column "type" "string")
                             (column "seat" "integer")))
    "firehouse" (table "firehouse" 
                       (list (column "id" "serial")
                             (column "name" "string")
                             (column "address" "string")
                             (column "workforce" "string"))))))

(define (sql-candidates sql position)
  (define tokens-reverse (reverse (lex-all (open-input-string sql))))
  (cons 'ac-complete (find-candidates tokens-reverse position test-db)))

;; add that when server is running and it will works
;; (define (voir out)
;;   (fprintf out "voir")
;;   (flush-output out))
