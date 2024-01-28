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

;; Non-Empty-List-of-Temperatures -> Boolean
;; checks if the temperatures in a Non-Empty-List-of-Temperatures
;; are sorted in descending order.
(check-expect (sorted>? (cons 1 '())) #true)
(check-expect (sorted>? (cons 1 (cons 2 (cons 3 '())))) #false)
(check-expect (sorted>? (cons 3 (cons 2 (cons 1 '())))) #true)
(check-expect (sorted>? (cons 3 (cons 3 (cons 3 '())))) #true)
(check-expect (sorted>? (cons 3 (cons 1 (cons 2 '())))) #false)

(define (sorted>? non-empty-list-of-temperatures)
  (cond [(empty? (rest non-empty-list-of-temperatures)) #true]
        [else
         (if (>= (first non-empty-list-of-temperatures)
                 (first (rest non-empty-list-of-temperatures)))
             (sorted>? (rest non-empty-list-of-temperatures))
             #false)]))

;; NOTE: alternative solution by YE:
;; (define (sorted>? non-empty-list-of-temperatures)
;; (cond [(empty? (rest non-empty-list-of-temperatures)) #true]
;;       [else
;;        (and (>= (first non-empty-list-of-temperatures)
;;                 (first (rest non-empty-list-of-temperatures)))
;;             (sorted>? (rest non-empty-list-of-temperatures)))]))

(test)
