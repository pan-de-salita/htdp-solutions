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

;; NOTE: the first clause of sum and how-many doesn't
;; contain (rest non-empty-list-of-temperatures) because
;; we do not intend to use empty lists for our computations.

;; NOTE: the second clause of sum and how-many doesn't
;; use (cons? non-empty-list-of-temperatures) not because
;; it is incorrect to do so, but because it makes the
;; functions more efficient. if (cons 1 '()) were to be
;; passed through each function, for example, both expressions
;; (cons? non-empty-list-of-temperatures) and
;; (cons? (rest non-empty-list-of-temperatures)) will have
;; the functions return the same results. the difference
;; would lie in the second expression stopping the functions'
;; operation once it registers (rest non-empty-list-of-temperatures)
;; being empty.
;;
;; also, wouldn't using (empty? (rest non-empty-list-of-temperaturs))
;; and (cons? non-empty-list-of-temperatures) be redundant since
;; it (cons 1 '()) meets both conditions simultaneously?

;; Non-Empty-List-of-Temperatures -> Number
;; adds up the temperatures on the given list
(check-expect (sum (cons 1 '())) 1)
(check-expect (sum (cons 1 (cons 2 (cons 3 '())))) 6)

(define (sum non-empty-list-of-temperatures)
  (cond [(empty? (rest non-empty-list-of-temperatures))
         (first non-empty-list-of-temperatures)]
        [(cons? (rest non-empty-list-of-temperatures))
         (+ (first non-empty-list-of-temperatures)
            (sum (rest non-empty-list-of-temperatures)))]))

;; Non-Empty-List-of-Temperatures -> Number
;; counts the temperatures on the given list
(check-expect (how-many (cons 1 '())) 1)
(check-expect (how-many (cons 1 (cons 2 (cons 3 '())))) 3)

(define (how-many non-empty-list-of-temperatures)
  (cond [(empty? (rest non-empty-list-of-temperatures)) 1]
        [(cons? (rest non-empty-list-of-temperatures))
         (add1 (how-many (rest non-empty-list-of-temperatures)))]))

(test)
