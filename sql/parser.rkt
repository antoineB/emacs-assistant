#lang racket

(require parser-tools/yacc
         parser-tools/lex
         racket/generator
         (prefix-in : parser-tools/lex-sre)
         "../lexing-helper.rkt"
         "struct.rkt"
         "query.rkt")

(provide 
;; find-candidates
 find-candidates
 lex-all-without-blank
 from+join-parser
 create-table-parser
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

(define (lex-abc input)
  (define current (sql-lexer input))
  (if (equal? (position-token-name current) 'BLANKS)
      (lex-abc input)
      current))
    

(define (sql-parse str)
  (define input (open-input-string str))
  (begin0
      (create-table-parser
       (lambda ()
         (lex-abc input)))
      (close-input-port input)))

(define (lex-all-without-blank input)
  (filter (lambda (x) (not (equal? (position-token-name x) 'BLANKS))) (lex-all input)))

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
      (map position-token-name (lex-all input))))

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
FULL UNION OPAREN CPAREN COMMA DOT EQUAL EOF SEMICOLON TRANSTYPE TRUE FALSE 
UNIQUE CONSTRAINT
F_NOW 
PRIMARY KEY DEFAULT NOT NULL
STDIN
GRANT
T_TEXT T_CHARACTER T_INT T_REAL T_SERIAL T_BIGSERIAL T_BOOL T_BIGINT T_TIMEZONE T_VARCHAR T_DATE T_NUMERIC))

(define-tokens value-tokens (IDENTIFIER INTEGER STRING))

(module+ test
  (define test-db
    (Db
     (hash "fireman"
           (Table "fireman" 
                   (hash "id" (Column "id" 'int #t 'nothing)
                         "name" (Column "name" 'string #t 'nothing)
                         "rank" (Column "rank" 'int #t 'nothing)
                         "year" (Column "year" 'int #t 'nothing))
                   empty)
           "firetruck"
           (Table "firetruck"
                  (hash "id" (Column "id" 'int #t 'nothing)
                        "type" (Column "type" 'string #t 'nothing)
                        "seat" (Column "seat" 'int #t 'nothing))
                  empty)
           "firehouse" 
           (Table "firehouse" 
                   (hash "id" (Column "id" 'int #t 'nothing)
                         "name" (Column "name" 'string #t 'nothing)
                         "address" (Column "address" 'string #t 'nothing)
                         "workforce" (Column "workforce" 'string #t 'nothing))
                   empty)))))
  
(define (find-context tokens position)
  (let loop ([context #f]
             [tokens tokens])
  (if (empty? tokens)
      context
      (let ([token (first tokens)])
        (if (> (position-offset (position-token-end-pos token)) position)
            context
            (loop
             (case (position-token-token token)
               [(SELECT) 'SELECT]
               [(FROM JOIN) 'FROM]
               [(WHERE) 'WHERE]
               [else context])
             (rest tokens)))))))

(module+ test
  (let ([data (lex-all-without-blank (open-input-string "SELECT maison FROM voiture, camion WHERE id = 2"))])

    (check-equal? (find-context data 7) 'SELECT)
    (check-equal? (find-context data 11) 'SELECT)
    (check-equal? (find-context data 14) 'SELECT)
    
    (check-equal? (find-context data 19) 'FROM)
    (check-equal? (find-context data 23) 'FROM)
    (check-equal? (find-context data 30) 'FROM)

    (check-equal? (find-context data 41) 'WHERE)
    (check-equal? (find-context data 44) 'WHERE)))

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
  ;; TODO: remove table already imported, or at leat reorder it bottom
  (map
   Table-name
   (table-candidates
    database
    (let ([before (first reverse-list)])
      (if (position-token-name=? 'IDENTIFIER before)
          (position-token-value before)
          "")))))

(define list-unique (compose list->set set->list))

(define (find-candidates-select database reverse-list from-clause position)
  (define (extract-start-with token start end)
    (if (> (position-offset end) position)
        (substring (token-value token) 0 (- position (position-offset start)))
        (token-value token)))
  (define-values (column-start alias)
    (match reverse-list
      [(list-rest (position-token (and (app token-name 'IDENTIFIER) table-name) start end)
                  (app position-token-name 'DOT)
                  (position-token (and (app token-name 'IDENTIFIER) alias) _ _)
                  _)
       (values (extract-start-with table-name start end)
             (token-value alias))]
      [(list-rest (app position-token-name 'DOT)
                  (position-token (and (app token-name 'IDENTIFIER) alias) _ _)
                  _)
       (values #f (token-value alias))]
      [(list-rest (position-token (and (app token-name 'IDENTIFIER) table-name) start end) _)
       (values (extract-start-with table-name start end) #f)]
      [else (values #f #f)]))
  (map
   (lambda (p)
     (if alias
         (string-append alias "."
                        (Column-name (cdr p)))
         (Column-name (cdr p))))
   (match-column-candidate
    database
    from-clause
    column-start
    alias)))

(define (parse-from reverse-list)
  (define tokens (extract-from-to '(WHERE EOF) 'FROM reverse-list #t #t))
  (if (empty? tokens)
      empty
      (from+join-parser (sequence->generator (reverse tokens)))))

(module+ test
  (let ([data (reverse (lex-all-without-blank (open-input-string "SELECT maison FROM voiture, camion WHERE id = 2")))])
    (check-equal? (parse-from data) (list (From-table "voiture" #f #f) (From-table "camion" #f #f)))))

(define (reverse-drop-until-position tokens position)
  (let loop ([reversed (list)]
             [tokens tokens])
  (if (empty? tokens)
      reversed
      (let ([token (first tokens)])
        (if (> (position-offset (position-token-end-pos token)) position)
            reversed
            (loop (cons token reversed)
                  (rest tokens)))))))

(module+ test
  (let ([data (lex-all-without-blank (open-input-string "SELECT f.n FROM fireman AS f"))])
    (check-equal? (map position-token-name (reverse-drop-until-position data 11))
                  '(IDENTIFIER DOT IDENTIFIER SELECT))))

(define (empty-false lst)
  (if (empty? lst)
      #f
      lst))

(define (find-candidates tokens position database)
  (define start (reverse-drop-until-position tokens position))
  (case (find-context tokens position)
    [(SELECT WHERE) (find-candidates-select
                     database
                     start 
                     (empty-false (parse-from (reverse tokens)))
                     position)]
    [(FROM) (find-candidates-from start database)]
    [else empty]))

(module+ test
  (let ([data0 (lex-all-without-blank (open-input-string "SELECT f.n FROM fireman AS f"))]
        [data1 (lex-all-without-blank (open-input-string "SELECT fh.a FROM fireman AS fi, firehouse AS fh"))])
    (check-equal? (find-candidates data0 11 test-db) '("f.name"))
    (check-equal? (list->set (find-candidates data0 10 test-db)) (list->set '("f.year" "f.rank" "f.name" "f.id")))
    (check-equal? (find-candidates data1 12 test-db) '("fh.address"))))

(define sql-lexer
  (lexer-src-pos
   [(:or #\space #\tab #\newline "\r") 'BLANKS]
   [(:: #\- #\- (:* (:~ #\newline)) #\newline) 'BLANKS]

   ;; quick and dirt way to treat STDIN
   ;; ["\\." 'STDIN]
   [(:: (ignore-case "from") (:+ (:or #\space #\tab #\newline "\r")) 
	(ignore-case "stdin") (:* (:or #\space #\tab #\newline "\r")) ";")
    (begin
      (read-until "\\." input-port)
      'STDIN)]

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
   [(ignore-case "true") 'TRUE]
   [(ignore-case "false") 'FALSE]
   [(ignore-case "primary") 'PRIMARY]
   [(ignore-case "key") 'KEY]
   [(ignore-case "default") 'DEFAULT]
   [(ignore-case "not") 'NOT]
   [(ignore-case "null") 'NULL]
   [(ignore-case "unique") 'UNIQUE]
   [(ignore-case "constraint") 'CONSTRAINT]
   [(ignore-case "grant") 'GRANT]

   
   [(ignore-case "text") 'T_TEXT]
   [(ignore-case "int") 'T_INT]
   [(ignore-case "integer") 'T_INT]
   [(ignore-case "serial") 'T_SERIAL]
   [(ignore-case "bigserial") 'T_BIGSERIAL]
   [(ignore-case "boolean") 'T_BOOL]
   [(ignore-case "bigint") 'T_BIGINT]
   [(ignore-case "real") 'T_REAL]
   
   [(:: (ignore-case "timestamp") (:? (:: #\( (:/ #\0 #\9) #\))) 
        (:? (:: (:or (ignore-case " without") (ignore-case " with")) (ignore-case " time zone"))))
    'T_TIMEZONE]
   
   [(ignore-case "character varying")
    'T_VARCHAR]

   [(:: (:or (ignore-case "varchar(") (ignore-case "character varying(")) (:+ (:/ #\0 #\9)) #\))
    'T_VARCHAR]
   [(ignore-case "date") 'T_DATE]
   [(:: (ignore-case "numeric") (:? (:: #\( (:+ (:/ #\0 #\9)) #\, (:* #\space) (:+ (:/ #\0 #\9))#\))))
    'T_NUMERIC]
   
   [(:: (ignore-case "character(") (:+ (:/ #\0 #\9)) #\))
    'T_CHARACTER]
   
   [(ignore-case "now()") 'F_NOW]
      
      
   [(:+ (:or (:/ #\a #\z #\A #\Z #\0 #\9) #\_ #\-)) (token-IDENTIFIER lexeme)]
   [(:: #\" (:+ (:or (:: #\\ any-char) (:~ #\"))) #\")
    (token-IDENTIFIER (substring lexeme 1 (sub1 (string-length lexeme))))]
   
   [(:: #\' (:* (:or (:: #\\ any-char) (:~ #\'))) #\')
    (token-STRING lexeme)]

   [(:+ (:/ #\0 #\9))
    (token-INTEGER lexeme)]
   
   [(:: #\: #\:) 'TRANSTYPE]
   [#\; 'SEMICOLON]
   [#\( 'OPAREN]
   [#\) 'CPAREN]
   [#\, 'COMMA]
   [#\. 'DOT]
   [#\= 'EQUAL]
   
   [(eof) 'EOF]))

(define-values (from+join-parser
                create-table-parser)
  (let ([sql-parser 
         (parser
          (src-pos)
          (start from+join create_table)
          (end EOF WHERE ORDER SEMICOLON)
          
          ;;(debug "debug.txt")
          
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
            [(join_keyword IDENTIFIER) (From-table $2 #f #f)]
            [(join_keyword IDENTIFIER AS IDENTIFIER) (From-table $2 $4 #f)]
            [(join_keyword IDENTIFIER IDENTIFIER) (From-table $2 $3 #f)]
            [(join_keyword IDENTIFIER ON join_typical_test) (From-table $2 #f #f)]
            [(join_keyword IDENTIFIER ON join_typical_test AS IDENTIFIER) (From-table $2 $6 #f)]
            [(join_keyword IDENTIFIER ON join_typical_test IDENTIFIER) (From-table $2 $5 #f)])
           
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
           
           (identifier_list
            [(IDENTIFIER COMMA identifier_list)
             (cons $1 $3)]
            [(IDENTIFIER) (list $1)])
           
           (column_specific_opt
            [() #f]
            [(OPAREN identifier_list CPAREN) $2])
           
           (from_table
            [(IDENTIFIER AS IDENTIFIER column_specific_opt) (From-table $1 $3 $4)]
            [(IDENTIFIER IDENTIFIER column_specific_opt) (From-table $1 $2 $3)]
            [(IDENTIFIER) (From-table $1 #f #f)])
           
           (from_table_list
            [(from_table) (list $1)]
            [(from_table COMMA from_table_list) (cons $1 $3)])
           
           (create_table
            [(CREATE TABLE IDENTIFIER OPAREN columndefs CPAREN)
             (let ([constraint (filter Constraint? $5)]
                   [column (filter Column? $5)])
               (Table $3 column constraint))])
           
           (columndefs
            [(columndef) (list $1)]
            [(columndefs COMMA columndef) (append $1 (list $3))])

           (type
            [(T_TEXT) 'text]
            [(T_INT) 'int]
            [(T_REAL) 'real]
            [(T_SERIAL) 'serial] 
            [(T_BIGSERIAL) 'bigserial]
            [(T_BOOL) 'bool] 
            [(T_BIGINT) 'bigint]
            [(T_TIMEZONE) 'timezone]
            [(T_VARCHAR) 'varchar]
            [(T_CHARACTER) 'character]
            [(T_DATE) 'date]
            [(T_NUMERIC) 'numeric])
           
           (qualified_identifiers
            [(IDENTIFIER) (list $1)]
            [(identifiers DOT IDENTIFIER) (append $1 (list $3))])
           
           (identifiers
            [(IDENTIFIER) (list $1)]
            [(identifiers COMMA IDENTIFIER) (append $1 (list $3))])
           
           (constraint_name
            [(CONSTRAINT qualified_identifiers) $2]
            [() #f])

           (not_null_opt
            [() #f]
            [(NOT NULL) #t])
           
           (columndef
            [(constraint_name PRIMARY KEY OPAREN identifiers CPAREN) (PrimaryKey $1 $5)]
            [(constraint_name UNIQUE OPAREN identifiers CPAREN) (Unique $1 $4)]
            [(IDENTIFIER type) (Column $1 $2 #t 'nothing)]
            [(IDENTIFIER type DEFAULT scalar not_null_opt) (Column $1 $2 (not $5) $4)]
            [(IDENTIFIER type NOT NULL) (Column $1 $2 #f 'nothing)])

           (scalar
            [(functions) $1]
            [(NULL TRANSTYPE type) (cons null $3)]
            [(NULL) null]
            [(INTEGER) $1]
            [(STRING TRANSTYPE type) (cons $1 $3)]
            [(STRING) $1]
            [(TRUE) #f]
            [(FALSE) #f])

           (functions
            [(F_NOW) 'now])))])
    (values
     (first sql-parser)
     (second sql-parser))))


(module+ test
  (let ([data (open-input-string "FROM maison, voiture AS v, \"pompier\"")])
    (check-equal? (lex-all-name-only data) '(FROM BLANKS IDENTIFIER COMMA BLANKS
IDENTIFIER BLANKS AS BLANKS IDENTIFIER COMMA BLANKS IDENTIFIER EOF))))


   
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
;; "GRAN"TRANT
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
