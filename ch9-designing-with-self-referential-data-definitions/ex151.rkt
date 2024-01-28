#lang racket
(require test-engine/racket-tests)

;; an N is one of:
;; - 0
;; - (add1 N)
;; i.e.: counting numbers

;; N Number -> Number
;; adds N to some arbitrary number without using +.
(check-within (multiply 0 5) (* 0 5) 0.001)
(check-within (multiply 5 5) (* 5 5) 0.001)
(check-within (multiply 5 5.5) (* 5 5.5) 0.001)

(define (multiply n arbitrary-number)
  (cond [(zero? n) 0]
        [(positive? n)
         (+ arbitrary-number
            (multiply (sub1 n) arbitrary-number))]))

(test)
