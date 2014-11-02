#lang racket

(provide get-config
         set-config
         read-config)

(define conf (make-hash))

(define (get-config key)
  (hash-ref conf key #f))

(define (set-config key value)
  (hash-set! conf key value))

(define (read-config)
  (define data (call-with-input-file "emacs-assistant.crkt" read))
  (for ([pair data])
    (set-config
     (car pair)
     (case (car pair)
       [(sql-database-file)
       (path->complete-path (string->path (cdr pair)) (current-directory))]
       [else (cdr pair)]))))
