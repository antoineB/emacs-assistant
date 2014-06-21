#lang racket

(require parser-tools/yacc
         parser-tools/lex
         racket/generator
         (prefix-in : parser-tools/lex-sre)
         "sql-struct.rkt")

(provide 
 find-candidates
 lex-all)

(define (lex-all input)
  (let loop ([current (sql-lexer input)]
             [result '()])
    (case (position-token-name current)
      [(EOF)
       (reverse (cons current result))]
      [(BLANKS)
       (loop (sql-lexer input)
             result)]
      [else
       (loop (sql-lexer input)
             (cons current result))])))

(module+ test
    (require rackunit)

    (define (lex-all input)
      (let loop ([current (sql-lexer input)]
                 [result '()])
        (if (equal? (position-token-name current) 'EOF)
            (reverse (cons current result))
            (loop (sql-lexer input)
                  (cons current result)))))
    
    (define (lex-all-name-only input)
      (map position-token-name (lex-all input)))
    
    (define (lex-all-without-blank input)
      (filter (lambda (x) (not (equal? (position-token-name x) 'BLANKS))) (lex-all input))))

(define-lex-trans (ignore-case stx)
  (syntax-case stx ()
    [(_ re)
     (let ([str (syntax-e #'re)])
       (when (not (string? str))
         (raise-syntax-error #f
                             "all arguments must be strings"
                             stx))
       #`(concatenation #,@(map (lambda (c) #`(union #,(char-upcase c) #,(char-downcase c))) (string->list str))))]))

(define-empty-tokens op-tokens 
  (SELECT TABLE ALTER DELETE INSERT UPDATE INTO FROM WHERE ORDER BY SET VALUES
AND OR LIKE CREATE IN AS DISTINCT ALL NATURAL OUTER INNER ON RIGHT LEFT JOIN
FULL UNION OPAREN CPAREN COMMA DOT EQUAL EOF))

(define-tokens value-tokens (IDENTIFIER INTEGER STRING))

(define (position-token-name tok)
  (token-name (position-token-token tok)))

;; table : the name of the table
;; alias : the alias #f if none
;; columns : the only columns assecible #f if all
(struct from-table (table alias columns) #:transparent)

(module+ test
  (define test-db
    (db
     (hash
      "fireman" (table "fireman" 
                       (list (column "id" "serial")
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
                               (column "workforce" "string")))))))
  
(define (all-tables db-definition)
  (hash-keys (db-tables db-definition)))

(define (all-columns db-definition table-name)
  (define table (hash-ref (db-tables db-definition) table-name #f))
  (if table
      (map column-name (table-columns table))
      empty))

(module+ test
  (check-equal? (all-columns test-db "fireman")
                '("id" "name" "rank" "year"))
  (check-equal? (all-columns test-db "firehouse")
                '("id" "name" "address" "workforce")))

;; The list of token is in reverse order
(define (find-context reverse-list)
  (if (empty? reverse-list)
      #f
      (case (position-token-token (first reverse-list))
        [(SELECT) 'SELECT]
        [(FROM JOIN) 'FROM]
        [(WHERE) 'WHERE]
        [else (find-context (rest reverse-list))])))

(module+ test
  (let ([data (reverse (lex-all-without-blank (open-input-string "SELECT maison FROM voiture, camion WHERE id = 2")))])
    (check-equal? (find-context (drop data 9)) 'SELECT)
    (check-equal? (find-context (drop data 1)) 'WHERE)
    (check-equal? (find-context (drop data 5)) 'FROM)))

(define (find-position reverse-list position)
  (if (empty? reverse-list)
      empty
      (if (> position 
             (position-offset (position-token-start-pos (first reverse-list))))
          reverse-list
          (find-position (rest reverse-list) position))))

(module+ test
  (let ([data (reverse (lex-all-without-blank (open-input-string "SELECT maison FROM voiture, camion WHERE id = 2")))])
    (check-equal? (find-position data 43) (drop data 3))))

(define (start-with str lst)
  (let ([len (string-length str)])
    (let loop ([lst lst]
               [result empty])
    (if (empty? lst)
        result
        (loop
         (rest lst)
         (let ([elem (first lst)])
           (if (and (<= len (string-length elem))
                    (string=? str (substring elem 0 len)))
               (cons elem result)
               result)))))))

(module+ test
  (check-equal? (start-with "ab" '("abc" "zsd" "abed")) '("abed" "abc")))

(define (position-token-name=? sym token)
  (equal? (position-token-name token) sym))

(define (find-candidates-from reverse-list database)
  (define tables (all-tables database))
  (define before (first reverse-list))
  (if (position-token-name=? 'IDENTIFIER before)
      (start-with (token-value (position-token-token before)) tables)
      tables))

(define list-unique (compose list->set set->list))

(define (find-candidates-select reverse-list candidates position)
  (start-with
   (match reverse-list
     [(list-rest (position-token (and (app token-name 'IDENTIFIER) table-name) start end)
                 (app position-token-name 'DOT)
                 (position-token (and (app token-name 'IDENTIFIER) alias) _ _)
                 _)
      (string-append (token-value alias) "."
                     (if (> (position-offset end) position)
                         (substring (token-value table-name) 0 (- position (position-offset start)))
                         (token-value table-name)))]
     [(list-rest (app position-token-name 'DOT)
                 (position-token (and (app token-name 'IDENTIFIER) alias) _ _)
                 _)
      (string-append (token-value alias) ".")]
     [(list-rest (position-token (and (app token-name 'IDENTIFIER) table-name) start end) _)
      (if (> (position-offset end) position)
          (substring (token-value table-name) 0 (- position (position-offset start)))
          (token-value table-name))]
     [else ""])
   candidates))

(define (parse-from reverse-list)
  (define lst
    (let loop ([reverse-list reverse-list]
               [lst empty])
      (if (empty? reverse-list)
          lst
          (let ([elem (first reverse-list)])
            (if (equal? (position-token-name elem) 'FROM)
                (cons elem lst)
                (loop
                 (rest reverse-list)
                 (cons elem lst)))))))
  (if (or (empty? lst)
          (not (equal? (position-token-name (first lst)) 'FROM)))
      empty
      (sql-parser (sequence->generator lst))))

(module+ test
  (let ([data (reverse (lex-all-without-blank (open-input-string "SELECT maison FROM voiture, camion WHERE id = 2")))])
    (check-equal? (parse-from data) (list (from-table "voiture" #f #f) (from-table "camion" #f #f)))))
                
(define (find-candidates reverse-list position database)
  (define start (find-position reverse-list position))
  (if (empty? start)
      empty
      (case (find-context start)
        [(SELECT WHERE) (find-candidates-select 
                         start 
                         (let ([candidates (parse-from reverse-list)]) 
                           (if (empty? candidates)
                               (list-unique (all-columns database)) 
                               (posible-select candidates database)))
                         position)]
        [(FROM) (find-candidates-from start database)]
        [else empty])))

(module+ test
  (let ([data0 (reverse (lex-all-without-blank (open-input-string "SELECT f.n FROM fireman AS f")))]
        [data1 (reverse (lex-all-without-blank (open-input-string "SELECT fh.a FROM fireman AS fi, firehouse AS fh")))])
    (check-equal? (find-candidates data0 11 test-db) '("f.name"))
    (check-equal? (find-candidates data0 10 test-db) '("f.year" "f.rank" "f.name" "f.id"))
    (check-equal? (find-candidates data1 12 test-db) '("fh.address"))))

(define (build-lexer-without-blanks input)
  (define (loop)
    (let ([tok (sql-lexer input)])
      (if (and (position-token? tok)
               (equal? (token-name (position-token-token tok)) 'BLANKS))
          (loop)
          tok)))
  loop)
    

(define sql-lexer
  (lexer-src-pos
   [(:or #\space #\tab #\newline "\r") 'BLANKS]
   
   [(ignore-case "select") 'SELECT]
   [(ignore-case "table") 'TABLE]
   [(ignore-case "alter") 'ALTER]
   [(ignore-case "delete") 'DELETE]
   [(ignore-case "insert") 'INSERT]
   [(ignore-case "update") 'UPDATE]
   [(ignore-case "into") 'INTO]
   [(ignore-case "from") 'FROM]
   [(ignore-case "where") 'WHERE]
   [(ignore-case "order") 'ORDER]
   [(ignore-case "by") 'BY]
   [(ignore-case "set") 'SET]
   [(ignore-case "values") 'VALUES]
   [(ignore-case "and") 'AND]
   [(ignore-case "or") 'OR]
   [(ignore-case "like") 'LIKE]
   [(ignore-case "create") 'CREATE]
   [(ignore-case "in") 'IN]
   [(ignore-case "as") 'AS]
   [(ignore-case "distinct") 'DISTINCT]
   [(ignore-case "all") 'ALL]
   [(ignore-case "natural") 'NATURAL]
   [(ignore-case "outer") 'OUTER]
   [(ignore-case "inner") 'INNER]
   [(ignore-case "on") 'ON]
   [(ignore-case "right") 'RIGHT]
   [(ignore-case "left") 'LEFT]
   [(ignore-case "join") 'JOIN]
   [(ignore-case "full") 'FULL]
   [(ignore-case "union") 'UNION]
   
   [(:+ (:/ #\a #\z #\A #\Z #\0 #\9)) (token-IDENTIFIER lexeme)]
   [(:: #\" (:+ (:or (:: #\\ any-char) (:~ #\"))) #\")
    (token-IDENTIFIER (substring lexeme 1 (sub1 (string-length lexeme))))]
   
   [(:: #\' (:+ (:or (:: #\\ any-char) (:~ #\'))) #\')
    (token-STRING lexeme)]
   
   [(:+ (:/ #\0 #\9))
    (token-INTEGER lexeme)]
   
   [#\( 'OPAREN]
   [#\) 'CPAREN]
   [#\, 'COMMA]
   [#\. 'DOT]
   [#\= 'EQUAL]
   
   [(eof) 'EOF]
))

;; from-tables : (listof from-table)
(define (posible-select from-tables db-definition)
  (let loop ([from-tables from-tables]
             [candidates (set)])
    (if (empty? from-tables)
        (set->list candidates)
        (loop (rest from-tables)
              (set-union
               candidates
               (list->set
                (let ([from (first from-tables)])
                  (cond 
                   [(from-table-columns from)
                    (if (from-table-alias from)
                        (map 
                         (curry string-append (from-table-alias from) ".")
                         (from-table-columns from))
                        (from-table-columns from))]
                   [else
                    (let ([columns
                           (all-columns db-definition 
                                        (from-table-table from))])
                      (if (from-table-alias from)
                          (map
                           (curry string-append (from-table-alias from) ".")
                           columns)
                          columns))]))))))))

(module+ test
  (check-equal? (posible-select (list (from-table "fireman" #f #f) (from-table "firehouse" #f #f)) test-db)
                '("address" "year" "rank" "workforce" "name" "id")))

(define sql-parser
  (parser
   (src-pos)
   (start from+join)
   (end EOF WHERE ORDER)
   
   (debug "debug.txt")
   
   (tokens value-tokens op-tokens)
   (error (lambda x (print x) (print "Sorry the parser error message provide no clues.")))
   
   (grammar

    ;; (select
    ;;  [(SELECT DISTINCT ...)
    ;;  [(SELECT ALL ...)
    ;;  [(SELECT ...)
    
    (from
     [(FROM from_table_list) $2])

    (from+join
     [(from join_list) (append $1 $2)]
     [(from) $1])
    
    (join_list
     [(join) (list $1)]
     [(join join_list) (cons $1 $2)])
    
    (join_typical_test
     [(IDENTIFIER EQUAL IDENTIFIER) null]
     [(IDENTIFIER DOT IDENTIFIER EQUAL IDENTIFIER DOT IDENTIFIER) null]
     [(IDENTIFIER EQUAL IDENTIFIER DOT IDENTIFIER) null]
     [(IDENTIFIER DOT IDENTIFIER EQUAL IDENTIFIER) null])
    
    (join
     [(join_keyword IDENTIFIER) (from-table $2 #f #f)]
     [(join_keyword IDENTIFIER AS IDENTIFIER) (from-table $2 $4 #f)]
     [(join_keyword IDENTIFIER IDENTIFIER) (from-table $2 $3 #f)]
     [(join_keyword IDENTIFIER ON join_typical_test) (from-table $2 #f #f)]
     [(join_keyword IDENTIFIER ON join_typical_test AS IDENTIFIER) (from-table $2 $6 #f)]
     [(join_keyword IDENTIFIER ON join_typical_test IDENTIFIER) (from-table $2 $5 #f)])
     
    
    (join_keyword
     [(NATURAL INNER JOIN) null]
     [(NATURAL UNION JOIN) null]
     [(NATURAL LEFT JOIN) null]
     [(NATURAL RIGHT JOIN) null]
     [(NATURAL FULL JOIN) null]
     [(NATURAL LEFT OUTER JOIN) null]
     [(NATURAL RIGHT OUTER JOIN) null]
     [(NATURAL FULL OUTER JOIN) null]
     [(INNER JOIN) null]
     [(UNION JOIN) null]
     [(LEFT JOIN) null]
     [(RIGHT JOIN) null]
     [(FULL JOIN) null]
     [(LEFT OUTER JOIN) null]
     [(RIGHT OUTER JOIN) null]
     [(FULL OUTER JOIN) null]
     [(JOIN) null])
    
    (from_table
     [(IDENTIFIER AS IDENTIFIER) (from-table $1 $3 #f)]
     [(IDENTIFIER IDENTIFIER) (from-table $1 $2 #f)]
     [(IDENTIFIER) (from-table $1 #f #f)])
    
    (from_table_list
     [(from_table) (list $1)]
     [(from_table COMMA from_table_list) (cons $1 $3)])
    )))
      


(module+ test
  (let ([data (open-input-string "FROM maison, voiture AS v, \"pompier\"")])
    (check-equal? (lex-all-name-only data) '(FROM BLANKS IDENTIFIER COMMA BLANKS
IDENTIFIER BLANKS AS BLANKS IDENTIFIER COMMA BLANKS IDENTIFIER EOF))))

;;"CREATE" "TABLE" y_table "(" y_columndefs ")"

;; sql= Language("SQL",
;; """
;; sql ::= y_sql
;;       | sql ";" y_sql

;; y_sql ::=
;;         y_alter
;;     |   y_create
;;     |   y_drop
;;     |   y_insert
;;     |   y_select
;;     |   y_update
;;     |   y_delete


;; y_alter ::=
;;         "ALTER" "TABLE" y_table "ADD" "COLUMN" y_columndef
;;     |   "ALTER" "TABLE" y_table "ADD" y_columndef

;; y_create ::=
;;         "CREATE" "TABLE" y_table "(" y_columndefs ")"

;; y_drop ::=
;;         "DROP" "TABLE" y_table

;; y_select ::=
;;         "SELECT" y_columns "FROM" y_table
;;     |   "SELECT" y_columns "FROM" y_table "WHERE" y_condition
;;     |   "SELECT" y_columns "FROM" y_table "ORDER" "BY" y_order
;;     |   "SELECT" y_columns "FROM" y_table "WHERE" y_condition "ORDER" "BY" y_order

;; y_delete ::=
;;         "DELETE" "FROM" y_table
;;     |   "DELETE" "FROM" y_table "WHERE" y_condition

;; y_insert ::=
;;         "INSERT" "INTO" y_table y_values
;;     |   "INSERT" "INTO" y_table "(" y_columns ")" y_values

;; y_update ::=
;;         "UPDATE" y_table "SET" y_assignments
;;     |   "UPDATE" y_table "SET" y_assignments "WHERE" y_condition


;; y_columndefs ::=
;;         y_columndef
;;     |   y_columndefs "," y_columndef


;; y_columndef ::=
;;         "NAME" "VARCHAR" "(" "INTNUM" ")"
;;     |   "NAME" "INT"
;;     |   "NAME" "INTEGER"
;;     |   "NAME" "DOUBLE"
;;     |   "NAME" "DOUBLE" "PRECISION"
;;     |   "NAME" "DATE"


;; y_columns ::=
;;         "*"
;;     |   y_column_list


;; y_column_list ::=
;;         "NAME"
;;     |   y_column_list "," "NAME"


;; y_table ::=
;;         "NAME"


;; y_values ::=
;;         "VALUES" "(" y_value_list ")"


;; y_value_list ::=
;;         "NULL_VALUE"
;;     |   "STRING"
;;     |   "INTNUM"
;;     |   "-" "INTNUM"
;;     |   "FLOATNUM"
;;     |   "-" "FLOATNUM"
;;     |   y_value_list "," "NULL_VALUE"
;;     |   y_value_list "," "STRING"
;;     |   y_value_list "," "INTNUM"
;;     |   y_value_list "," "-" "INTNUM"
;;     |   y_value_list "," "FLOATNUM"
;;     |   y_value_list "," "-" "FLOATNUM"

;; y_assignments ::=
;;         y_assignment
;;     |   y_assignments "," y_assignment


;; y_assignment ::=
;;         "NAME" "EQUAL" "NULL_VALUE"
;;     |   "NAME" "EQUAL" y_expression


;; y_condition ::=
;;         y_sub_condition


;; y_sub_condition ::=
;;         y_sub_condition2
;;     |   y_sub_condition "OR" y_sub_condition2


;; y_sub_condition2 ::=
;;         y_boolean
;;     |   y_sub_condition2 "AND" y_boolean


;; y_boolean ::=
;;         y_comparison
;;     |   "(" y_sub_condition ")"
;;     |   "NOT" y_boolean


;; y_comparison ::=
;;         y_expression "EQUAL" y_expression
;;     |   y_expression "COMPARISON_OPERATOR" y_expression
;;     |   y_expression "IS" "NULL_VALUE"
;;     |   y_expression "NOT" "NULL_VALUE"



;; y_expression ::=
;;         y_product
;;     |   y_expression "+" y_product
;;     |   y_expression "-" y_product


;; y_product ::=
;;         y_term
;;     |   y_product "*" y_term
;;     |   y_product "/" y_term


;; y_term ::=
;;         y_atom
;;     |   "-"y_term


;; y_atom ::=
;;         y_value
;;     |   y_column
;;     |   "(" y_expression ")"



;; y_value ::=
;;         "STRING"
;;     |   "INTNUM"
;;     |   "FLOATNUM"



;; y_column ::=
;;         "NAME"


;; y_order ::=
;;         "NAME"
;; """,
;; """
;; "ALL":ALL
;; "AND":AND
;; "AVG":AVG
;; "MIN":MIN
;; "MAX":MAX
;; "SUM":SUM
;; "COUNT":COUNT
;; "ANY":ANY
;; "AS":AS
;; "ASC":ASC
;; "AUTHORIZATION":AUTHORIZATION
;; "BETWEEN":BETWEEN
;; "BY":BY
;; "CHARACTER":CHARACTER
;; "CHAR":CHAR
;; "CHECK":CHECK
;; "CLOSE":CLOSE
;; "COMMIT":COMMIT
;; "CONTINUE":CONTINUE
;; "CREATE":CREATE
;; "CURRENT":CURRENT
;; "CURSOR":CURSOR
;; "DECIMAL":DECIMAL
;; "DECLARE":DECLARE
;; "DEFAULT":DEFAULT
;; "DELETE":DELETE
;; "DESC":DESC
;; "DISTINCT":DISTINCT
;; "DOUBLE":DOUBLE
;; "ESCAPE":ESCAPE
;; "EXISTS":EXISTS
;; "FETCH":FETCH
;; "FLOAT":FLOAT
;; "FOR":FOR
;; "FOREIGN":FOREIGN
;; "FOUND":FOUND
;; "FROM":FROM
;; "GOTO":GOTO
;; "GO":GO
;; "GRANT":RANT
;; "GROUP":GROUP
;; "HAVING":HAVING
;; "IN":IN
;; "INDICATOR":INDICATOR
;; "INSERT":INSERT
;; "INTEGER":INTEGER
;; "INT":INT
;; "INTO":INTO
;; "IS":IS
;; "KEY":KEY
;; "LANGUAGE":LANGUAGE
;; "LIKE":LIKE
;; "NOT":NOT
;; "NULL":NULL_VALUE
;; "NUMERIC":NUMERIC
;; "OF":OF
;; "ON":ON
;; "OPEN":OPEN
;; "OPTION":OPTION
;; "OR":OR
;; "ORDER":ORDER
;; "PRECISION":PRECISION
;; "PRIMARY":PRIMARY
;; "PRIVILEGES":PRIVILEGES
;; "PROCEDURE":PROCEDURE
;; "PUBLIC":PUBLIC
;; "REAL":REAL
;; "REFERENCES":REFERENCES
;; "ROLLBACK":ROLLBACK
;; "SCHEMA":SCHEMA
;; "SELECT":SELECT
;; "SET":SET
;; "SMALLINT":SMALLINT
;; "SOME":SOME
;; "SQLCODE":SQLCODE
;; "TABLE":TABLE
;; "TO":TO
;; "UNION":UNION
;; "UNIQUE":UNIQUE
;; "UPDATE":UPDATE
;; "USER":USER
;; "VALUES":VALUES
;; "VIEW":VIEW
;; "VARCHAR":VARCHAR
;; "WHENEVER":WHENEVER
;; "WHERE":WHERE
;; "WITH":WITH
;; "WORK":WORK
;; "\*":*
;; "[ \\t]":<ws>
;; "[\\n\\r]":<return>
;; "[A-Za-z][A-Za-z_0-9]*":NAME
;; "[0-9]+":INTNUM
;; "<>|<=|>=|<|>":COMPARISON_OPERATOR
;; "=":EQUAL
;; "\"([a-zA-Z0-9 ]|\\\\\")*\"":STRING
;; ":[A-Za-z][A-Za-z0-9_]*":PARAMETER
;; ";":;
;; "\(":(
;; "\)":)
;; """,
