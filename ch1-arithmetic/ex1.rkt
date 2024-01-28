;; Exercise 1. Add the following definitions for x and y to DrRacket's definitions are:
;;
;; (define x 3)
;; (define y 4)
;;
;; Now imagine that x and y are the coordinates of a Cartesian point. Write down an
;; expression tht computes the distane of this point to the origin, that is, a point with
;; the coordinates (0,0).

#lang htdp/bsl
(require test-engine/racket-tests)

;; number number -> number
;; Purpose: Calculates the distance between (x,y) and (0,0)
(define (distance-from-origin x y)
  (sqrt (+ (sqr x) (sqr y))))

(check-expect (distance-from-origin 3 4) 5)
(check-expect (distance-from-origin 12 5) 13)
(test)
