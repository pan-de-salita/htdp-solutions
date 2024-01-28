#lang racket
(require test-engine/racket-tests)

;; [Non-empty-list-of X] -> X
;; retrieves the last item on a non-empty list

(check-expect (last-item '(1)) 1)
(check-expect (last-item '(1 2 3)) 3)

(define (last-item ne-l)
  (match ne-l
    [(cons lst '()) lst]
    [(cons fst rst) (last-item rst)]))

(check-expect (last-item-using-cond '(1)) (last-item '(1)))
(check-expect (last-item-using-cond '(1 2 3)) (last-item '(1 2 3)))

(define (last-item-using-cond ne-l)
  (cond [(empty? (cdr ne-l)) (car ne-l)]
        [else (last-item-using-cond (cdr ne-l))]))

(test)
