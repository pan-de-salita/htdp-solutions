#lang racket
(require test-engine/racket-tests)

(define WAGE/H 14)
(define MESSAGE "wage*: Number less than or equal to 100 expected.")

;; List-of-numbers -> List-of-numbers
;; computes the wages for a list of workers'
;; worked hours lwh
(check-expect (wage* '()) '())
(check-expect (wage* (cons 28 '())) (cons 392 '()))
(check-expect (wage* (cons 4 (cons 2 '()))) (cons 56 (cons 28 '())))
(check-error (wage* (cons 48 (cons 101 '()))) MESSAGE)

(define (wage* lwh)
  (cond [(empty? lwh) lwh]
        [else
         (if (<= (first lwh) 100)
             (cons (wage (first lwh)) (wage* (rest lwh)))
             (error MESSAGE))]))

;; Number -> Number
;; computes the wage for h hours of work
(check-expect (wage 3) (* WAGE/H 3))

(define (wage h)
  (* WAGE/H h))

(test)
