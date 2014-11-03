#lang racket

;; Terminate an sql statement is a way to automaticaly adding the missing parts
;; for an sql statments.

(require "struct.rkt"
         "query.rkt"
         "../utils.rkt"
         (only-in "parser.rkt" select-parser form:from Select-request->string)
         racket/generator)

(provide terminate-select-request)

(define (group-select-column-by-table candidates)
  (let loop ([grouped #hash()]
             [candidates candidates])
    (if (empty? candidates)
        grouped
        (let* ([candidate (first candidates)]
               [table-alias (or (Select-column-table-alias candidate) 'all)]
               [column-name (Select-column-column-name candidate)])
          (loop
           (hash-set grouped
                     table-alias
                     (cons column-name (hash-ref grouped table-alias empty)))
           (rest candidates))))))

(define (terminate-select-request select database)
  (match select
    [(Select-request tokens #f _ _ #f #f)
     (let* (;;[tokens (Select-request-select select-part)]
            [parsed (select-parser (sequence->generator tokens))]
            [grouped (group-select-column-by-table parsed)]
            [tables-candidates-one (for/list ([(alias columns-name) (hash-remove grouped 'all)])
                                     (find-table-with-columns database columns-name))])
       (for/list ([possibility (sort (possible-tables database (hash-ref grouped 'all empty))
                                     (lambda (a b) (> (length a) (length b))))]
                  #:when (or (empty? tables-candidates-one)
                             (ormap (lambda (subpart) (subpart? subpart possibility))
                                    (find-combination tables-candidates-one))))
         (Select-request->string
          (struct-copy Select-request select
                       [from (form:from (set->list possibility))]))))]
     [else
      empty]))
