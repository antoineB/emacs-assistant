#lang racket

;; Have all the function to query the representation of the database

;; e.g. find all the table starting with "abc"

(require "struct.rkt")

(provide 
 match-column-candidate
 table-candidates)

(module+ test
  (require rackunit))

(define (start-with? str start)
  (define len (string-length start))
  (and (<= len (string-length str))
       (string=? start (substring str 0 len))))
  
(module+ test
  (check-equal? 
   (filter
    (lambda (x) (start-with? x "ab"))
    '("abc" "zsd" "abed"))
   '("abc" "abed")))


;; Find all the column candidate for matching types in all tables. Types can be
;; false don't cate types.
(define/contract (column-candidates db start-with tables [types #f])
  (->* (Db? string? (listof Table?)) ((or/c #f (listof symbol?))) (listof (cons/c Table? Column?)))
  (define tables (for*/list ([db-table (hash-values (Db-tables db))]
                             [table tables]
                             #:when (equal? (Table-name db-table) table))
                   db-table))
  (define columns (for*/list ([table tables]
                              [column (hash-values (Table-columns table))]
                              #:when (start-with? (Column-name column) start-with))
                    (cons table
                          column)))
  (if types
      (filter
       (lambda (column)
         (for/first ([type types]
                     #:when (equal? type (Column-type column)))
           #t))
       columns)
      columns))
 


(define/contract (table-candidates db start-with)
  (-> Db? string? (listof string?))
  (filter
   (lambda (table)
     (start-with? (Table-name table) start-with))
   (hash-values (Db-tables db))))


(define (database->from-clause database)
  (for/list ([table (hash-values (Db-tables database))])
    (From-table (Table-name table) #f #f)))

;; Match the potential candidate given a from-clause and column-start
(define/contract (match-column-candidate database from-clause column-start alias)
  (-> Db? (or/c #f (listof From-table?)) (or/c #f string?) (or/c #f string?) (listof (cons/c Table? Column?)))
  ;; TODO: treat the case where column-start could be the start of an alias
  (define tables (Db-tables database))
  (define reduced-from-clause
    (let ([from-clause (or from-clause (database->from-clause database))])
      (if alias
          (filter (lambda (x) (equal? (From-table-alias x) alias)) from-clause)
          from-clause)))
  (apply
   append
   (for/list ([clause reduced-from-clause])
     (let ([table (hash-ref tables (From-table-table clause) #f)]
           [restrict-columns (From-table-columns clause)])
       (cond [(and table restrict-columns)
              (map 
               (lambda (column)
                 (cons table column))
               (filter (lambda (column)
                         (start-with?
                          (Column-name column)
                          (or column-start "")))
                       (for/list ([column restrict-columns]
                                  #:when (hash-ref (Table-columns table) #f))
                         (hash-ref (Table-columns table)))))]
             [table
              (map (lambda (column)
                     (cons table column))
                   (filter (lambda (column)
                             (start-with?
                              (Column-name column)
                              (or column-start "")))
                           (hash-values (Table-columns table))))]
             [else empty])))))
