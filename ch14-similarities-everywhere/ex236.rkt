#lang racket
(require test-engine/racket-tests)

;; List-of-Numbers -> List-of-Numbers
;; adds 1 to each item on a-list-of-numbers

(check-expect (add1* '()) '())
(check-expect (add1* '(1 0)) '(2 1))

(define (add1* a-list-of-numbers)
  (cond [(empty? a-list-of-numbers) '()]
        [else (cons (add1 (car a-list-of-numbers))
                    (add1* (cdr a-list-of-numbers)))]))

;; List-of-Numbers -> List-of-Numbers
;; adds 5 to each item on a-list-of-numbers

(check-expect (add5* '()) '())
(check-expect (add5* '(1 0)) '(6 5))

(define (add5* a-list-of-numbers)
  (cond [(empty? a-list-of-numbers) '()]
        [else (cons (+ (car a-list-of-numbers) 5)
                    (add5* (cdr a-list-of-numbers)))]))

;;;; the functions above abstracted

;; List-of-Numbers Number -> List-of-Numbers
;; adds a given Number x to each item on a-list-of-numbers

(check-expect (add-x-to '() 1) '())
(check-expect (add-x-to '(1 0) 1) (add1* '(1 0)))
(check-expect (add-x-to '(1 0) 5) (add5* '(1 0)))

(define (add-x-to a-list-of-numbers x)
  (cond [(empty? a-list-of-numbers) '()]
        [else (cons (+ (car a-list-of-numbers) x)
                    (add-x-to (cdr a-list-of-numbers) x))]))

;; List-of-Numbers -> List-of-Numbers
;; version 2 of add1* using add-x-to.

(check-expect (add1*.v2 '()) (add1* '()))
(check-expect (add1*.v2 '(1 0)) (add1* '(1 0)))

(define (add1*.v2 a-list-of-numbers)
  (add-x-to a-list-of-numbers 1))

;; List-of-Numbers -> List-of-Numbers
;; version 2 of add5* using add-x-to.

(check-expect (add5*.v2 '()) (add5* '()))
(check-expect (add5*.v2 '(1 0)) (add5* '(1 0)))

(define (add5*.v2 a-list-of-numbers)
  (add-x-to a-list-of-numbers 5))

;; List-of-Numbers -> List-of-Numbers
;; subtracts 2 from each item on a-list-of-numbers.

(check-expect (sub2 '()) '())
(check-expect (sub2 '(1 0)) '(-1 -2))

(define (sub2 a-list-of-numbers)
  (add-x-to a-list-of-numbers -2))

(test)
