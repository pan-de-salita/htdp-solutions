#lang racket
(require test-engine/racket-tests)

;; [List-of Number] -> Number
;; computes the sum of the numbers on l.

(check-expect (sum '()) 0)
(check-expect (sum '(1)) 1)
(check-expect (sum '(2 1)) 3)

(define (sum l)
  (cond [(empty? l) 0]
        [else (+ (car l) (sum (cdr l)))]))

;; [List-of Number] -> Number
;; computes the product of the numbers on l.

(check-expect (product '()) 1)
(check-expect (product '(1)) 1)
(check-expect (product '(2 1)) 2)

(define (product l)
  (cond [(empty? l) 1]
        [else (* (car l) (product (cdr l)))]))

;; [List-of Number] [[List-of Number] -> Number] -> Number
;; computes the R of the numbers on l.

(define (fold1 l R)
  (cond [(empty? l) (if (equal? R +) 0 1)]
        [else (R (car l) (fold1 (cdr l) R))]))

;; [List-of Number] -> Number
;; computes the sum of the numbers on l.

(check-expect (sum-from-abstract '()) 0)
(check-expect (sum-from-abstract '(1)) 1)
(check-expect (sum-from-abstract '(2 1)) 3)

(define (sum-from-abstract l)
  (fold1 l +))

;; [List-of Number] -> Number
;; computes the product of the numbers on l.

(check-expect (product-from-abstract '()) 1)
(check-expect (product-from-abstract '(1)) 1)
(check-expect (product-from-abstract '(2 1)) 2)

(define (product-from-abstract l)
  (fold1 l *))

(test)
