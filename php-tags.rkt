#lang racket

(require php-parser
         (only-in parser-tools/lex
                  position-line
                  position-offset))

(module+ test
  (require rackunit))

(define (apply-to-ast ast
		      functions-pair
		      [current-namespace null]
		      [current-alias '()])
  (define (loop data)
    (define data/first (when (not (empty? data)) (first data)))
    (cond [(empty? data) (void)]
	  [(NamespaceStmt? data/first)
	   (if (not (null? (NamespaceStmt-body data/first)))
	       (apply-to-ast (NamespaceStmt-body data/first)
			     functions-pair
			     (NamespaceStmt-name data/first)
			     current-alias)
	       (set! current-namespace (NamespaceStmt-name data/first)))
	   (loop (rest data))]
	  [(UseStmt? data/first)
	   (set! current-alias (append current-alias (UseStmt-uses data/first)))
	   (loop (rest data))]
	  [else
	   (for ([p functions-pair])
	     (let ([predicate (first p)]
		   [functions (rest p)])
	       (if (predicate data/first)
		 (for ([f functions])
		   (f current-namespace current-alias data/first))
		 (when (sub-ast? data/first)
		   (loop (get-sub-ast data/first))))))
	   (loop (rest data))]))
  (loop ast))

  (define namespace-option (make-parameter #f))

;; Find the pattern from line-number and return the begin of line until pattern
;; is found
(define (get-start-line lines line-number pattern)
  (if (>= line-number (length lines))
      #f
      (let ([reg (regexp (second (regexp-match #rx"\\$?([a-z0-9A-Z_]+)" pattern)))]
            [line (list-ref lines line-number)])
        (if (regexp-match? reg line)
            (substring line 0 (cdar (regexp-match-positions reg line)))
            (get-start-line lines (add1 line-number) pattern)))))
  
;; ast         : PhpAst
;; files-lines : (VectorOf string)
(define (ast->tag-line ast files-lines)
  (define lines empty)
  (define (build-line namespace ast extractor [sep "\\"])
    (define start (Position-start ast))
       (set! lines
             (cons 
              (format
               "~a~a~a~a~a,~a\n"
               (get-start-line files-lines (sub1 (position-line start)) (extractor ast))
               (if (namespace-option) (string-join namespace "\\") "")
               (if (namespace-option) sep "")
               (extractor ast)
               (position-line start)
               (position-offset start))
              lines)))
  (apply-to-ast 
   ast
   (list 
    (list
     ClassDcl?
     (lambda (namespace _ class)
       (build-line namespace class ClassDcl-name)
       (define full-namespace (append namespace (list (ClassDcl-name class))))
       (apply-to-ast
        (ClassDcl-body class)
        (list
         (list
          MethodDcl?
          (lambda (_ __ meth) 
            (build-line full-namespace meth MethodDcl-name ":")))
         ;; (list
         ;;  ConstClassDcls?
         (list
          PropertyDcl?
          (lambda (_ __ prop) 
            (for ([name (PropertyDcl-variables prop)])
              (build-line full-namespace prop (lambda _ (if (pair? name) (car name) name)) ""))))))))
    (list
     InterfaceDcl?
     (lambda (namespace _ class)
       (build-line namespace class InterfaceDcl-name)
       (define full-namespace (append namespace (list (InterfaceDcl-name class))))
       (apply-to-ast
        (InterfaceDcl-body class)
        (list
         (list
          MethodDcl?
          (lambda (_ __ meth) (build-line full-namespace meth MethodDcl-name ":")))
         ;; (list
         ;;  ConstClassDcls?
         (list
          PropertyDcl?
          (lambda (_ __ prop) 
            (for ([name (PropertyDcl-variables prop)])
              (build-line full-namespace prop (lambda _ (if (pair? name) (car name) name)) ""))))))))
    (list
     FunctionDcl?
     (lambda (namespace _ fun) (build-line namespace fun FunctionDcl-name)))))
  (reverse lines))
(module+ test
  (define php-example 
    "<?php
namespace Racket\\Test;

class Test {
  public function methA() { return 12; }
  public $propA = null;
  public $propB;
}")
  
  (define in (open-input-string php-example))
  (define parsed (php-parse in))
  (close-input-port in)
  (define lines (string-split php-example "\n"))
  (check-equal?
   (parameterize ([namespace-option #t])
     (ast->tag-line parsed lines))
   '("class Test\u007FRacket\\Test\\Test\u00014,31\n"
     "  public function methA\u007FRacket\\Test\\Test:methA\u00015,46\n"
     "  public $propA\u007FRacket\\Test\\Test$propA\u00016,87\n"
     "  public $propB\u007FRacket\\Test\\Test$propB\u00017,111\n"))
  )

;; Return the string of a file section header. Lines correspond to a list of
;; the body of the file section.
(define (file-header abs-filename lines)
  ;;
  ;;/home/antoine/.emacs,1337\n
  ;; 1337 = the number of char for the file
  (format "\n~a,~a\n"
          abs-filename
          (foldr + 0 (map string-length lines))))

(define (file->tag filename)
  (define parsed 
    (let* ([in (open-input-file filename)]
           [parsed (php-parse in)])
      (close-input-port in)
      parsed))
  (define lines (file->lines filename))
  (define tags-line (ast->tag-line parsed lines))
  (if (empty? tags-line)
      ""
      (apply
       string-append
       (cons
        (file-header filename lines)
        tags-line))))
  

(module+ main
  (define (find directory expect)
    (define reg (and expect (if (regexp? expect) expect (regexp expect))))
    (if (directory-exists? directory)
        (parameterize ([current-directory directory])
          (foldr (lambda (v lst)
                   (define file (path->string (path->complete-path v)))
                   (cond
                    [(directory-exists? file)
                     (append (find file expect)
                             lst)]
                    [(and reg (regexp-match? reg file))
                       lst]
                    [else
                     (cons file lst)]))
                 empty
                 (directory-list)))
        empty))
    
  (define-values (directory-opt expect-opt) (values #f #f))
  (define-values (tag-file filenames)
    (command-line
     #:once-each
     [("-n" "--namespace") "Display tag with its full namespace"
      (namespace-option #t)]
     [("-e" "--expect") expect "A regexp that define the php that won't match" 
      (set! expect-opt expect)]
     [("-d" "--directory") directory "Define a directory to search for php file from" 
      (set! directory-opt directory)]
     #:args (tag-file . filenames)
     (values tag-file filenames)))
  
  (when directory-opt
    (set! filenames
          (append
           (filter
            (curry regexp-match? #rx"\\.php$")
            (find directory-opt expect-opt))
           filenames)))
  
  (when expect-opt
    (set! filenames
          (filter (compose not (curry regexp-match? expect-opt)) filenames)))
    
  (define out (open-output-file tag-file #:exists 'replace))
  (for ([filename filenames])
    (display
     (file->tag filename)
     out))
  (close-output-port out))
   
