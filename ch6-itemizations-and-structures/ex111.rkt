#lang racket

(define-struct vec [x y])
;; a Vec is
;;    (make-vec PositiveNumber PositiveNumber)
;; interpretation: represents a velocity vector

;; Any Any -> Vec
;; creates an instance of Vec if both inputs
;; are PositiveNumbers
(define (checked-make-vec x y)
  (if (not (and (positive? x) (positive? y)))
      (error "make-vec: 2 positive numbers expected")
      (make-vec x y)))

(checked-make-vec -9 "negative eight")
