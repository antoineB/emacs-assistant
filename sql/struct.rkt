#lang racket

(require racket/serialize)

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

(serializable-struct Table (name columns primary-key) #:transparent)

(serializable-struct Column (name type nullable default) #:transparent)

(serializable-struct Constraint (name))

(serializable-struct Unique Constraint (columns))

(serializable-struct PrimaryKey Constraint (columns))

(serializable-struct Db (tables) #:transparent)

;; table : the name of the table
;; alias : the alias #f if none
;; columns : the only columns assecible #f if all
(serializable-struct From-table (table alias columns) #:transparent)

(serializable-struct Select-request (select from where order-by group-by having) #:transparent)

(serializable-struct Select-column (column-name table-alias alias) #:transparent)
