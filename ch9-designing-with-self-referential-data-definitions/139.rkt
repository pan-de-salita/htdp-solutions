#lang racket
(require test-engine/racket-tests)

;; a List-of-Amounts is one of:
;; - '()
;; - (cons PositiveNumber List-of-Amounts)
;; i.e.: a sequence of amounts of money
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

;; a List-of-Numbers is one of:
;; - '()
;; - (cons Number List-of-Numbers)
;; i.e.: a sequence of numbers
(define list-of-numbers-example-0
  '())
(define list-of-numbers-example-1
  (cons -1 '()))
(define list-of-numbers-example-2
  (cons -1
        (cons 2 '())))
(define list-of-numbers-example-3
  (cons -1
        (cons 2
              (cons -30 '()))))

;; Any -> Boolean
;; checks if the input is a List-of-Amounts
(check-expect (pos? list-of-amounts-example-0) #true)
(check-expect (pos? list-of-amounts-example-1) #true)
(check-expect (pos? list-of-numbers-example-0) #true)
(check-expect (pos? list-of-numbers-example-1) #false)
(check-expect (pos? 12) #false)
(check-expect (pos? "cat") #false)
(check-expect (pos? #true) #false)

(define (pos? input)
  (if (list? input)
      (or (empty? input)
          (and (positive? (first input))
               (pos? (rest input))))
      #false))

;; List-of-Amounts -> PositiveNumber
;; computes the sum of the amounts in a List-of-Amounts
;; if a List-of-Amounts is provided
(check-expect (checked-sum list-of-amounts-example-0) 0)
(check-expect (checked-sum list-of-amounts-example-1) 100)
(check-expect (checked-sum list-of-amounts-example-2) 200)
(check-within (checked-sum list-of-amounts-example-3) 249.59 0.01)
(check-expect (checked-sum list-of-numbers-example-0) 0)
(check-error (checked-sum list-of-numbers-example-1) "checked-sum: list of amounts required")
(check-error (checked-sum list-of-numbers-example-2) "checked-sum: list of amounts required")
(check-error (checked-sum list-of-numbers-example-3) "checked-sum: list of amounts required")
(check-error (checked-sum 12) "checked-sum: list of amounts required")
(check-error (checked-sum "cat") "checked-sum: list of amounts required")
(check-error (checked-sum #true) "checked-sum: list of amounts required")

(define (checked-sum input)
  (if (pos? input)
      (cond
        [(empty? input) 0]
        [else (+ (first input)
                 (checked-sum (rest input)))])
      (error "checked-sum: list of amounts required")))

(test)
