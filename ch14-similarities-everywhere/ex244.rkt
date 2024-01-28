#lang racket
(require test-engine/racket-tests)

(define (f x) x)

(define (g x) (x 10))
;; legal if x would be a function.

(define (h x) (x f))
;; legal if x would be a function.

(define (i x y) (x 'a y 'b))
;; legal if x would be a function that accepts 3 arguments.
