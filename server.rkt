#lang racket

(require racket/tcp
         racket/rerequire)

(define commands-ns (begin
                      (dynamic-rerequire "commands.rkt")
                      (dynamic-require "commands.rkt" 'commands-ns)))

(define (printf+flush msg . args)
  (apply printf (cons msg args))
  (flush-output))

(module+ main
  (define out (current-output-port))
  (define server (tcp-listen 1026 4 #t))
  (printf+flush "Server running\n")
  (define-values (client-in client-out) (tcp-accept server))
  (printf+flush "Connection accepted\n")
  (let loop ()
    (let ([data (call-with-exception-handler
                 (lambda exc #f)
                 (lambda () (read client-in)))])
      (match data
        [(list 'exit)
         (printf+flush "Ending trigged by user")
         (tcp-close server)]
        [(list 'token (? number? nb) (list-rest (? symbol? fun) args))
         (dynamic-rerequire "commands.rkt")
         (with-handlers ([(lambda _ #t)
                          (lambda x (printf+flush "You got a bad error boy\n") (printf+flush "~a\n" x))])
           (let ([data (apply (eval fun commands-ns) args)])
             (when data
               (fprintf client-out "~s" (cons 'token (cons nb data)))
               (flush-output client-out))))
         (printf+flush "i loop\n")
         (loop)]
        [(list-rest (? symbol? fun) args)
         (dynamic-rerequire "commands.rkt")
         (with-handlers ([(lambda _ #t)
                          (lambda x (printf+flush "You got a bad error boy\n") (printf+flush "~a\n" x))])
           
           (let ([data (apply (eval fun commands-ns) args)])
             (when data
               (fprintf client-out "~s" data)
               (flush-output client-out))))
         (printf+flush "i loop\n")
         (loop)]
        [_
         (printf+flush "Abnormal ending\n")
         (tcp-close server)]))))
