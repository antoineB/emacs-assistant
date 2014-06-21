#lang racket

(provide
 (struct-out db)
 (struct-out table)
 (struct-out column))

(struct db (tables) #:transparent)

(struct table (name columns) #:transparent)

(struct column (name type) #:transparent)
