#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(define-struct ball [location velocity])
(define-struct vel [deltax deltay])

(define ball1
  (make-ball (make-posn 30 40)
             (make-vel -10 5)))

(define-struct ballf [x y deltax deltay])
;; ==
(define solution-ex68 (make-ballf 30 40 -10 5))
