#lang racket
(require test-engine/racket-tests)

;; a List-of-Amounts is one of:
;; - '()
;; - (cons PositiveNumber List-of-Amounts)
;; i.e.: sequences of amounts of money
(define list-of-amounts-example-0
  '())
(define list-of-amounts-example-1
  (cons 100 '()))
(define list-of-amounts-example-2
  (cons 100
        (cons 100 '())))
(define list-of-amounts-example-3
  (cons 100
        (cons 100
              (cons 49.59 '()))))

;; List-of-Amounts -> PositiveNumber
;; computes the sum of the amounts in a List-of-Amounts
(check-expect (sum list-of-amounts-example-0) 0)
(check-expect (sum list-of-amounts-example-1) 100)
(check-expect (sum list-of-amounts-example-2) 200)
(check-within (sum list-of-amounts-example-3) 249.59 0.01)

(define (sum a-list-of-amounts)
  (cond
    [(empty? a-list-of-amounts) 0]
    [else (+ (first a-list-of-amounts)
             (sum (rest a-list-of-amounts)))]))

(test)
