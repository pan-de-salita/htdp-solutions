;; Exercise 11. Define a function that consumes two numbers, x and y, and that
;; computes the distance of point (x,y) to the origin.

#lang htdp/bsl
(require test-engine/racket-tests)

;; number number -> number
;; Purpose: Consumes two number, x and y, and computes the distance of point (x,y)
;;          to the origin
(define (distance-from-origin x y)
  (sqrt (+ (sqr x)
           (sqr y))))

(check-expect (distance-from-origin 3 4) 5)
(test)
