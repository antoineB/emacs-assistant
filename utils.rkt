#lang racket

(provide list-unique
         find-combination
         subpart?)

(module+ test
  (require rackunit)
  (require (submod "..")))

(define (list-unique lst) (set->list (list->set lst)))

(define (find-combination rst)
  (define comb0
    (match-lambda*
     [(list n)
      (map list n)]
     [(list n next)
      (for*/list ([a n]
                  [b next])
        (append a (list b)))]
     [(list-rest n next rst)
      (apply comb0 (comb0 n next) rst)]))
  (if (= 1 (length rst))
      (map list (first rst))
      (apply
       comb0
       (comb0 (first rst))
       (rest rst))))

(module+ test
  (check-equal?
   (find-combination '((a b c)))
   '((a) (b) (c)))
  (check-equal?
   (find-combination '((a b c) (b c) (c d)))
   '((a b c)
     (a b d)
     (a c c)
     (a c d)
     (b b c)
     (b b d)
     (b c c)
     (b c d)
     (c b c)
     (c b d)
     (c c c)
     (c c d))))

(define (subpart? sub super)
  (define set-sub (list->set sub))
  (define set-super (list->set super))
  (= (set-count set-sub)
     (set-count (set-intersect set-sub set-super))))

(module+ test
  (check-true
   (subpart? '(1 2 3) '(1 4 5 6 2 3 4)))
  (check-false
   (subpart? '(1 2 3) '(1 4 5 6 2 4))))
