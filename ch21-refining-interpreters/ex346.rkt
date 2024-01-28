#lang racket
(require test-engine/racket-tests)

(define-struct add [left right])
;; an Add is a structure:
;;   (make-add Number Number)
;; i.e. the sum of two BSL expressions

(define-struct mul [left right])
;; a Mul is a structure:
;;   (make-add Number Number)
;; i.e. the product of two BSL expressions

;; a BSL-expr is one of:
;; - Number
;; - Add
;; - Mul
(define bsl-expr0 3)
(define bsl-expr1 (make-add 1 1))
(define bsl-expr2 (make-mul 3 10))
(define bsl-expr3 (make-add (* 1 1) 10))

;; a BSL-eval is a Number
;; i.e. the class of values to which a BSL-expr evaluates
(define bsl-eval0 3)
(define bsl-eval1 (+ 1 1))
(define bsl-eval2 (* 3 10))
(define bsl-eval3 (+ (* 1 1) 10))
