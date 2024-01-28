#lang racket
(require test-engine/racket-tests)

;; an N is one of:
;; - 0
;; - (add1 N)
;; i.e.: counting numbers

;; N -> Number
;; computes (+ n pi) without using +
(check-within (add-to-pi 0) (+ 0 pi) 0.001)
(check-within (add-to-pi 3) (+ 3 pi) 0.001)
;; check-within is used because pi is an inexact number.

(define (add-to-pi n)
  (cond [(zero? n) pi]
        [(positive? n) (add1 (add-to-pi (sub1 n)))]))

;; N Number -> Number
;; computes (+ n an-arbitrary-number) without using +
(check-within (add-to-arbitrary-number 0 0.5) (+ 0 0.5) 0.001)
(check-within (add-to-arbitrary-number 5 10.3) (+ 5 10.3) 0.001)

(define (add-to-arbitrary-number n arbitrary-number)
  (cond [(zero? n) arbitrary-number]
        [(positive? n) (add1 (add-to-arbitrary-number (sub1 n) arbitrary-number))]))

(test)
