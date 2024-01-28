#lang racket
(require test-engine/racket-tests)

(define-struct add [left right])
;; an Add is a structure:
;;   (make-add Number Number)
;; i.e. a data representation of the expression:
;;   (+ x y)

(define-struct mul [left right])
;; a Mul (short for multiply) is a structure:
;;   (make-mul Number Number)
;; i.e. a data representation of the expression:
;;   (* x y)

;; a BSL expression is one of:
;; - Number
;; - Add
;; - Mul

#|

expression -> data
(+ 10 -10) == (make-add 10 -10)
(+ (* 20 3) 33) == (make-add (make-mul 20 3) 33)
(+ (* 3.14 (* 2 3)) (* 3.14 (* -1 -9))) == (make-add (make-mul 3.14 (make-mul 2 3))
                                                     (make-mul 3.14 (make-mul -1 -9)))
representation -> expression
(make-add -1 2) == (+ -1 2)
(make-add (make-mul -2 -3) 33) == (+ (* -2 -3) 33)
(make-mul (make-add 1 (make-mul 2 3)) 3.14) == (* (+ 1 (* 2 3)) 3.14)

|#
