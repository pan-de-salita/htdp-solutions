#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;; computes the distance of point from the origin 0
(check-within (distance-from-origin (make-posn 0 5))
              (sqrt (+ (sqr (posn-x (make-posn 0 5)))
                       (sqr (posn-y (make-posn 0 5)))))
              0.01)
(check-within (distance-from-origin (make-posn 24 56))
              (sqrt (+ (sqr (posn-x (make-posn 24 56)))
                       (sqr (posn-y (make-posn 24 56)))))
              0.01)

(define (distance-from-origin point)
  (sqrt (+ (sqr (posn-x point))
           (sqr (posn-y point)))))

(test)
