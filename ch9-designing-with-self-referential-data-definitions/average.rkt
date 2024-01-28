#lang racket
(require test-engine/racket-tests)

(define ABSOLUTE-0 -272)
;; A CTemperature is a Number greater than ABSOLUTE-0.

;; a List-of-Temperatures is one of:
;; - '()
;; - (cons CTemperature List-of-Temperatures)

;; List-of-Temperatures -> Number
;; computes the average temperature
(check-expect (average (cons 1 (cons 2 (cons 3 '())))) 2)

(define (average list-of-temperatures)
  (/ (sum list-of-temperatures) (how-many list-of-temperatures)))

;; List-of-Temperatures -> Number
;; adds up the temperatures on the given list
(check-expect (sum (cons 1 (cons 2 (cons 3 '())))) 6)

(define (sum list-of-temperatures)
  (cond [(empty? list-of-temperatures) 0]
        [(cons? list-of-temperatures)
         (+ (first list-of-temperatures)
            (sum (rest list-of-temperatures)))]))

;; List-of-Temperatures -> Number
;; counts the temperatures on the given list
(check-expect (how-many (cons 1 (cons 2 (cons 3 '())))) 3)

(define (how-many list-of-temperatures)
  (cond [(empty? list-of-temperatures) 0]
        [(cons? list-of-temperatures)
         (add1 (how-many (rest list-of-temperatures)))]))

(test)
