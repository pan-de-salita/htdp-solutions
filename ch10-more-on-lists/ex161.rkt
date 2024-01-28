#lang racket
(require test-engine/racket-tests)

(define WAGE/H 14)

;; List-of-numbers -> List-of-numbers
;; computes the wages for a list of workers'
;; worked hours lwh
(check-expect (wage* '()) '())
(check-expect (wage* (cons 28 '())) (cons 392 '()))
(check-expect (wage* (cons 4 (cons 2 '()))) (cons 56 (cons 28 '())))

(define (wage* lwh)
  (if (empty? lwh)
      lwh
      (cons (wage (first lwh)) (wage* (rest lwh)))))

;; Number -> Number
;; computes the wage for h hours of work
(check-expect (wage 3) (* WAGE/H 3))

(define (wage h)
  (* WAGE/H h))

(test)
