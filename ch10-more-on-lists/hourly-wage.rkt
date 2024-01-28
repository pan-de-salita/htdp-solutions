#lang racket
(require test-engine/racket-tests)

;; Number -> Number
;; computes the wage for h hours of work
(define (wage h)
  (* 12 h))

;; List-of-numbers -> List-of-numbers
;; computes the wages for a list of workers'
;; worked hours lwh
(check-expect (wage* '()) '())
(check-expect (wage* (cons 8 (cons 8 (cons 8 '()))))
              (cons 96 (cons 96 (cons 96 '()))))

(define (wage* lwh)
  (if (empty? lwh)
      lwh
      (cons (wage (first lwh)) (wage* (rest lwh)))))

(test)
