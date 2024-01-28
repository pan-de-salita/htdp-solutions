#lang racket
(require test-engine/racket-tests)

;; Comparison-Operator List-of-Numbers Number -> List-of-Numbers
;; returns a List-of-Numbers with elements that hold true when compared to
;; a Number x using a-comparator.

(check-expect (extract = '() 0) '())
(check-expect (extract = '(0 1 2) 0) '(0))
(check-expect (extract < '(0 1 2) 0) '())
(check-expect (extract > '(0 1 2) 0) '(1 2))
(check-expect (extract <= '(0 1 2) 0) '(0))
(check-expect (extract >= '(0 1 2) 0) '(0 1 2))
(check-expect (extract squared>? '(3 4 5) 10) '(4 5))

(define (extract a-comparator a-list-of-numbers x)
  (cond [(empty? a-list-of-numbers) '()]
        [else (cond [(a-comparator (car a-list-of-numbers) x)
                     (cons (car a-list-of-numbers)
                           (extract a-comparator (cdr a-list-of-numbers) x))]
                    [else (extract a-comparator (cdr a-list-of-numbers) x)])]))

;; Number Number -> Boolean
;; checks if the area of a square with side x is larger then a Number x

(check-expect (squared>? 3 10) #f)
(check-expect (squared>? 4 10) #t)
(check-expect (squared>? 5 10) #t)

(define (squared>? x c)
  (> (sqr x) c))

(test)
