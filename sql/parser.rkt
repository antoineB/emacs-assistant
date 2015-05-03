#lang racket

(require parser-tools/yacc
         parser-tools/lex
         racket/generator
         (prefix-in : parser-tools/lex-sre)
         "../utils.rkt"
         "../lexing-helper.rkt"
         "struct.rkt"
         "query.rkt")

(provide
 find-candidates
 lex-all-without-blank
 from+join-parser
 create-table-parser
 select-parser
 split-select-request
 select-request?
 lex-all
 form:from
 Select-request->string)

(define (make-token-EOF [token #f])
  (position-token
          (token-EOF)
          (and token (position-token-end-pos token))
          (and token (position-token-end-pos token))))

;; Add an EOF at end of the tokens if there is none, assume the tokens are in
;; reverse order.
(define (coerse-with-EOF-reverse tokens)
  (cond [(empty? tokens) tokens]
        [(position-token-name=? 'EOF (first tokens)) tokens]
        [else
         (cons
          (make-token-EOF (first tokens))
          tokens)]))

(define (separate-request tokens)
  (let loop ([tokens tokens]
             [result '(())])
    (cond [(empty? tokens)
           (reverse
            (map (compose reverse coerse-with-EOF-reverse)
                 (filter (compose not empty?) result)))]
          [(position-token-name=? 'SEMICOLON (first tokens))
           (loop
            (rest tokens)
            (cons empty result))]
          [else
           (loop
            (rest tokens)
            (cons
             (cons (first tokens) (first result))
             (rest result)))])))


(module+ test
  ;; TODO: check the presence of the EOF token with the right position which are
  ;; end of the last real token
  (let* ([data0 (lex-all-without-blank
                (open-input-string "SELECT abc FROM edf; SELECT why FROM wat"))]
         [data1 (separate-request data0)])
    (match (first data1)
      [(list _ abc _ edf eof0)
       (check-equal? (position-token-value abc) "abc")
       (check-equal? (position-offset (position-token-start-pos eof0)) 20)
       (check-equal? (position-offset (position-token-end-pos eof0)) 20)])
    (match (second data1)
      [(list _ why _ wat eof1)
       (check-equal? (position-token-value why) "why")
       (check-equal? (position-offset (position-token-start-pos eof1)) 41)
       (check-equal? (position-offset (position-token-end-pos eof1)) 41)])))


(define (token->string token)
  (case (position-token-name token)
    [(SELECT) "SELECT"]
    [(TABLE) "TABLE"]
    [(ALTER) "ALTER"]
    [(DELETE) "DELETE"]
    [(INSERT) "INSERT"]
    [(UPDATE) "UPDATE"]
    [(INTO) "INTO"]
    [(FROM) "FROM"]
    [(WHERE) "WHERE"]
    [(ORDER) "ORDER"]
    [(BY) "BY"]
    [(SET) "SET"]
    [(VALUES)"VALUES"]
    [(AND) "AND"]
    [(OR) "OR"]
    [(LIKE) "LIKE"]
    [(CREATE) "CREATE"]
    [(IN) "IN"]
    [(AS) "AS"]
    [(DISTINCT) "DISTINCT"]
    [(ALL) "ALL"]
    [(NATURAL) "NATURAL"]
    [(OUTER) "OUTER"]
    [(INNER) "INNER"]
    [(ON) "ON"]
    [(RIGHT) "RIGHT"]
    [(LEFT) "LEFT"]
    [(JOIN)"JOIN"]
    [(FULL) "FULL"]
    [(UNION) "UNION"]
    [(OPAREN) "("]
    [(CPAREN) ")"]
    [(COMMA) ","]
    [(DOT) "."]
    [(EQUAL) "="]
    [(EOF) ""]
    [(SEMICOLON) ";"]
    [(TRANSTYPE) "::"]
    [(TRUE) "TRUE"]
    [(FALSE) "FALSE"]
    [(ASTERISK) "*"]
    [(UNIQUE)"UNIQUE"]
    [(CONSTRAINT)"CONSTRAINT"]
    [(F_NOW) "now()"]
    [(PRIMARY)"PRIMARY"]
    [(KEY)"KEY"]
    [(DEFAULT)"DEFAULT"]
    [(NOT)"NOT"]
    [(NULL)"NULL"]
    [(GRANT)"GRANT"]
    ;;[(T_TEXT) [(T_CHARACTER) [(T_INT) [(T_REAL) [(T_SERIAL) [(T_BIGSERIAL) [(T_BOOL) [(T_BIGINT) [(T_TIMEZONE) [(T_VARCHAR) [(T_DATE) [(T_NUMERIC)))
    [(IDENTIFIER INTEGER) (position-token-value token)]
    [(STRING) (string-append "'" (position-token-value token) "'")]
    ;;SUBQUERY))
    [else
     (error "no such token")]))


(define (Select-request->string select)
  (string-join
   (append
    (map token->string (or (Select-request-select select) empty))
    (map token->string (or (Select-request-from select) empty))
    (map token->string (or (Select-request-where select) empty))
    (map token->string (or (Select-request-group-by select) empty))
    (map token->string (or (Select-request-having select) empty))
    (map token->string (or (Select-request-order-by select) empty))
    '(";"))
   " "))

(define (position-no-pos token)
  (position-token
   token
   (position #f #f #f)
   (position #f #f #f)))

(define (form:from tables-spec)
  (cons
   (position-no-pos (token-FROM))
   (rest
    (let loop ([tables-spec tables-spec]
               [result (list (token-EOF))])
      (if (empty? tables-spec)
          (reverse result)
          (loop
           (rest tables-spec)
           (append
            (match (first tables-spec)
              [(list name alias)
               (list (position-no-pos (token-IDENTIFIER alias))
                     (position-no-pos (token-AS))
                     (position-no-pos (token-IDENTIFIER name)))]
              [(? string? name)
               (list (position-no-pos (token-IDENTIFIER name)))])
            (if (empty? (rest tables-spec))
                result
                (cons (position-no-pos (token-COMMA)) result)))))))))

(define (select-request? tokens)
  (if (empty? tokens)
      #f
      (case (position-token-name (first tokens))
        [(UPDATE) #f]
        [(INSERT) #f]
        [(DELETE) #f]
        [else #t])))

(define (split-select-request tokens)
  ;; Make sure all parts of the select request will end with an eof tokens in
  ;; order to use a eof as parse ending delimiter.
  (define (end-with-eof tokens)
    (if (and (not (empty? tokens))
             (not (position-token-name=? 'EOF (first tokens))))
        (cons
         (position-token
          (token-EOF)
          (position-token-end-pos (first tokens))
          (position-token-end-pos (first tokens)))
         tokens)
        ;; remove semicolon also
        (if (and (not (empty? tokens))
                 (not (empty? (rest tokens)))
                 (position-token-name=? 'SEMICOLON (second tokens)))
            (cons (first tokens) (rest (rest tokens)))
            tokens)))

  (define (start-with? starters tokens)
    (for/and ([start starters]
              [token tokens])
     (position-token-name=? start token)))

  (define (start-with-candidates candidates tokens)
    (for/first ([candidate (in-set candidates)]
                #:when (start-with? candidate tokens))
      candidate))

  (define (put-in-select-struct select tokens)
    (cond
     [(start-with? '(FROM) tokens)
      (struct-copy Select-request select
                   [from tokens])]
     [(start-with? '(WHERE) tokens)
      (struct-copy Select-request select
                   [where tokens])]
     [(start-with? '(ORDER BY) tokens)
      (struct-copy Select-request select
                   [order-by tokens])]
     [(start-with? '(GROUP BY) tokens)
      (struct-copy Select-request select
                   [group-by tokens])]
     [(start-with? '(HAVING) tokens)
      (struct-copy Select-request select
                   [having tokens])]
     [(start-with? '(SELECT) tokens)
      (struct-copy Select-request select
                   [select tokens])]
     [else
       select]))

  (let loop ([tokens (mark-subquery tokens)]
             [stopers (set '(FROM) '(WHERE) '(ORDER BY) '(GROUP BY) '(HAVING))]
             [group-tokens empty]
             [parts (Select-request #f #f #f #f #f #f)])
    (cond
     [(empty? tokens)
      (put-in-select-struct parts (reverse (end-with-eof group-tokens)))]
     [(start-with-candidates stopers tokens) =>
      (lambda (stoper)
        (loop
         (drop tokens (length stoper))
         (set-remove stopers stoper)
         (take tokens (length stoper))
         (put-in-select-struct parts (reverse (end-with-eof group-tokens)))))]
     [else
      (loop
       (rest tokens)
       stopers
       (cons (first tokens) group-tokens)
       parts)])))

(module+ test
  (match-let* ([data (split-select-request
               (lex-all-without-blank
                (open-input-string "SELECT maison FROM voiture, camion WHERE id = 2;")))]
               [(Select-request select from where _ _ _) data])
    (check-pred list? select)
    (check-equal? (map position-token-name select)
                  '(SELECT IDENTIFIER EOF))
    (check-pred list? from)
    (check-equal? (map position-token-name from)
                  '(FROM IDENTIFIER COMMA IDENTIFIER EOF))
    (check-pred list? where)
    (check-equal? (map position-token-name where)
                  '(WHERE IDENTIFIER EQUAL IDENTIFIER EOF))))


(define (mark-subquery tokens)
  (let loop ([tokens tokens]
             [result empty])
    (cond [(empty? tokens)
           (reverse result)]
          [(empty? (rest tokens))
           (reverse (cons (first tokens) result))]
          [else
           (let ([tok (first tokens)]
                 [tokk (second tokens)])
             ;; TODO: make the subquery algorithm recognise from the second
             ;; SELECT occuring not only one preceded by an OPAREN. And stop
             ;; subquery at the second WHERE, FROM, GROUP etc...
             (if (and (position-token-name=? 'OPAREN tok)
                      (position-token-name=? 'SELECT tokk))
                 (let* ([subquery-tokens (matching-balanced-pair tokens #hash((OPAREN . CPAREN)))]
                        [subquery-rec (cons (first subquery-tokens) (mark-subquery (rest subquery-tokens)))])
                   (loop
                    (drop tokens (length subquery-tokens))
                    (cons (position-token (token-SUBQUERY subquery-rec)
                                          (position-token-start-pos (first subquery-rec))
                                          (position-token-end-pos (last subquery-rec)))
                          result)))
                 (loop (rest tokens) (cons (first tokens) result))))])))

(module+ test
  (let ([data0 (mark-subquery (lex-all-without-blank (open-input-string "SELECT * FROM papillion;")))]
        [data1 (mark-subquery (lex-all-without-blank (open-input-string "SELECT * FROM (SELECT * FROM papillion);")))]
        [data2 (mark-subquery (lex-all-without-blank (open-input-string "SELECT * FROM (SELECT * FROM (SELECT * FROM papillion));")))])
    (check-equal?
     (filter (curry position-token-name=? 'SUBQUERY) data0)
     empty)
    (let ([subqueries (filter (curry position-token-name=? 'SUBQUERY) data1)])
      (check-equal? (length subqueries) 1)
      (let ([subquery (first subqueries)])
        (check-equal? (position-offset (position-token-start-pos subquery)) 15)
        (check-equal? (position-offset (position-token-end-pos subquery)) 40)))
    (let ([subqueries (filter (curry position-token-name=? 'SUBQUERY) data2)])
      (check-equal? (length subqueries) 1)
      (let ([subquery (first subqueries)])
        (check-equal? (position-offset (position-token-start-pos subquery)) 15)
        (check-equal? (position-offset (position-token-end-pos subquery)) 56)
        (let ([subsubqueries (filter (curry position-token-name=? 'SUBQUERY) (position-token-value subquery))])
          (check-equal? (length subsubqueries) 1)
          (let ([subsubquery (first subsubqueries)])
            (check-equal? (position-offset (position-token-start-pos subsubquery)) 30)
            (check-equal? (position-offset (position-token-end-pos subsubquery)) 55)))))))

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
ASTERISK
UNIQUE CONSTRAINT
F_NOW
PRIMARY KEY DEFAULT NOT NULL
STDIN
GRANT
T_TEXT T_CHARACTER T_INT T_REAL T_SERIAL T_BIGSERIAL T_BOOL T_BIGINT T_TIMEZONE T_VARCHAR T_DATE T_NUMERIC))

(define-tokens value-tokens (IDENTIFIER INTEGER STRING SUBQUERY))

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
  (define tokens (extract-from-to '(WHERE CPAREN EOF) 'FROM reverse-list #t #t))
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

   ["*" 'ASTERISK]

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
                create-table-parser
                select-parser)
  (let ([sql-parser
         (parser
          (src-pos)
          (start from+join create_table select)
          (end EOF WHERE ORDER SEMICOLON) ; TODO: SEMICOLON, WHERE and ORDER
                                          ; should be removed cause there is a
                                          ; pre-split of the request at lexing
                                          ; time and every part is ended with a
                                          ; EOF token

          ;;(debug "debug.txt")

          (tokens value-tokens op-tokens)
          (error (lambda x (print x) (print "Sorry the parser error message provide no clues.")))

          (grammar

           (set_quantifier
            [() #f]
            [(DISTINCT) 'DISTINCT]
            [(ALL) 'ALL])

           (select_column_list
            [(qualified_identifier as_clause) (list
                                               (if (list? $1)
                                                   (Select-column (first $1) (second $1) $2)
                                                   (Select-column $1 #f $2)))]
            [(select_column_list COMMA qualified_identifier as_clause)
             (append $1 (list
                         (if (list? $3)
                             (Select-column (first $3) (second $3) $4)
                             (Select-column $3 #f $4))))])

            (select
             [(SELECT ASTERISK) 'ASTERISK]
             [(SELECT set_quantifier select_column_list) $3])

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

           (as_clause
            [() #f]
            [(AS IDENTIFIER) $2]
            [(IDENTIFIER) $1])

           (join
            [(join_keyword IDENTIFIER as_clause) (From-table $2 $3 #f)]
            [(join_keyword IDENTIFIER ON join_typical_test as_clause) (From-table $2 $5 #f)])

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
            [(identifier_list COMMA IDENTIFIER)
             (append $1 (list $3))]
            [(IDENTIFIER) (list $1)])

           (column_specific_opt
            [() #f]
            [(OPAREN identifier_list CPAREN) $2])

           (from_table
            [(IDENTIFIER as_clause column_specific_opt) (From-table $1 $2 $3)])

           (from_table_list
            [(from_table) (list $1)]
            [(from_table COMMA from_table_list) (cons $1 $3)])

           (create_table
            [(CREATE TABLE IDENTIFIER OPAREN columndefs CPAREN)
             (let ([constraint (filter Constraint? $5)]
                   [column (filter Column? $5)])
               (Table $3
                      (for/hash ([c column]) (values (Column-name c) c))
                      constraint))])

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

           (qualified_identifier
            [(IDENTIFIER) $1]
            [(IDENTIFIER DOT IDENTIFIER) (list $1 $3)])

           (qualified_identifier_list
            [(qualified_identifier) (list $1)]
            [(qualified_identifier COMMA IDENTIFIER) (append $1 (list $3))])

           (constraint_name
            [(CONSTRAINT qualified_identifier) $2]
            [() #f])

           (not_null_opt
            [() #f]
            [(NOT NULL) #t])

           (columndef
            [(constraint_name PRIMARY KEY OPAREN qualified_identifier_list CPAREN) (PrimaryKey $1 $5)]
            [(constraint_name UNIQUE OPAREN qualified_identifier_list CPAREN) (Unique $1 $4)]
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
     (second sql-parser)
     (third sql-parser))))


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
