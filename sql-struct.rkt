#lang racket

(provide
 (struct-out db)
 (struct-out table)
 (struct-out column)
 
 (struct-out Column)
 (struct-out Table)
 (struct-out PrimaryKey)
 (struct-out Unique)
 Constraint?)

(struct db (tables) #:transparent)

(struct table (name columns) #:transparent)

(struct column (name type) #:transparent)


(struct Table (name columns primary-key) #:transparent)

(struct Column (name type nullable default) #:transparent)

(struct Constraint (name))

(struct Unique Constraint (columns))

(struct PrimaryKey Constraint (columns))
