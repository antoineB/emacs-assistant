#lang racket

(provide commands-ns)

(require
 "sql/commands.rkt")

(define-namespace-anchor anchor)
(define commands-ns (namespace-anchor->namespace anchor))

(define (bonjour)
  (sleep 5)
  '(bonjour))

;; sql-completion
