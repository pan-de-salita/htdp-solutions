#lang racket
(require test-engine/racket-tests)

(define ABSOLUTE-0 -272)
;; A CTemperature is a Number greater than ABSOLUTE-0.

;; a Non-Empty-List-of-Temperatures is one of:
;; - (cons CTemperature '())
;; - (cons CTemperature Non-Empty-List-of-Temperatures)
;; i.e.: non-empty lists of Celsius temperatures
(define NON-EMPTY-LIST-OF-TEMPERATURES-EXAMPLE-0
  (cons ABSOLUTE-0 '()))
(define NON-EMPTY-LIST-OF-TEMPERATURES-EXAMPLE-1
  (cons ABSOLUTE-0
        (cons 1 '())))
(define NON-EMPTY-LIST-OF-TEMPERATURES-EXAMPLE-2
  (cons ABSOLUTE-0
        (cons 1
              (cons 2
                    (cons 3'())))))

(define MESSAGE "average: non-empty list expected")

;; Any -> Number
;; computes the average temperature if input is
;; a non-empty list
(check-expect (checked-average (cons 1 (cons 2 (cons 3 '()))))
              (average (cons 1 (cons 2 (cons 3 '())))))
(check-error (checked-average '()) MESSAGE)

(define (checked-average input)
  (cond [(cons? input) (average input)]
        [else (error MESSAGE)]))

;; Non-Empty-List-of-Temperatures -> Number
;; computes the average temperature
(check-expect (average (cons 1 (cons 2 (cons 3 '())))) 2)

(define (average non-empty-list-of-temperatures)
  (/ (sum non-empty-list-of-temperatures)
     (how-many non-empty-list-of-temperatures)))

;; NOTE: sum and how-many below still work. reason:
;; as they recurse through their inputs, they will
;; eventually hit an empty list. but since an empty
;; list produces 0, the final return value of each
;; function is not affected.
;; NOTE: alternative solution by YE:
;; sum and how-many are designed to work with any List-of-temperatures list.
;; NEList-of-temperatures is a subset of List-of-temperatures.
;; Hence the functions sum and how-many work for NEList-of-temperatures.

;; Non-Empty-List-of-Temperatures -> Number
;; adds up the temperatures on the given list
(check-expect (sum (cons 1 (cons 2 (cons 3 '())))) 6)

(define (sum non-empty-list-of-temperatures)
  (cond [(empty? non-empty-list-of-temperatures) 0]
        [(cons? non-empty-list-of-temperatures)
         (+ (first non-empty-list-of-temperatures)
            (sum (rest non-empty-list-of-temperatures)))]))

;; Non-Empty-List-of-Temperatures -> Number
;; counts the temperatures on the given list
(check-expect (how-many (cons 1 (cons 2 (cons 3 '())))) 3)

(define (how-many non-empty-list-of-temperatures)
  (cond [(empty? non-empty-list-of-temperatures) 0]
        [(cons? non-empty-list-of-temperatures)
         (add1 (how-many (rest non-empty-list-of-temperatures)))]))

(test)
