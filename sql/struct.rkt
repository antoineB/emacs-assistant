#lang racket

(provide
 (struct-out From-table)
 (struct-out Column)
 (struct-out Table)
 (struct-out PrimaryKey)
 (struct-out Unique)
 Constraint?
 (struct-out Db)
 (struct-out Select-request)
 (struct-out Select-column))

(struct Table (name columns primary-key) #:transparent)

(struct Column (name type nullable default) #:transparent)

(struct Constraint (name))

(struct Unique Constraint (columns))

(struct PrimaryKey Constraint (columns))

(struct Db (tables) #:transparent)

;; table : the name of the table
;; alias : the alias #f if none
;; columns : the only columns assecible #f if all
(struct From-table (table alias columns) #:transparent)

(struct Select-request (select from where order-by group-by having) #:transparent)

(struct Select-column (column-name table-alias alias) #:transparent)
