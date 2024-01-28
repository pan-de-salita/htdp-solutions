#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(define-struct r3 [x y z])
;; an R3 is a structure:
;;  (make-r3 Number Number Number)

(define ex1 (make-r3 1 2 13))
(define ex2 (make-r3 -1 0 3))

;; R3 -> Number
;; computes the distance of an object in a
;; 3D space to origin 0
(check-within (distance-to-origin ex1)
              (sqrt (+ (sqr (r3-x ex1))
                       (sqr (r3-y ex1))
                       (sqr (r3-z ex1))))
              0.01)
(check-within (distance-to-origin ex2)
              (sqrt (+ (sqr (r3-x ex2))
                       (sqr (r3-y ex2))
                       (sqr (r3-z ex2))))
              0.01)

(define (distance-to-origin object-position)
  (sqrt (+ (sqr (r3-x object-position)) ;; Number
           (sqr (r3-y object-position)) ;; Number
           (sqr (r3-z object-position))))) ;; Number

(test)
