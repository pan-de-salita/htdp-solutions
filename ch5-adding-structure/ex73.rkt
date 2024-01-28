#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;; Posn -> Posn
;; takes a position and some number (called z); replaces x-coordinate of
;; positon with given number
(check-expect (posn-up-x (make-posn 10 10) 13)
              (make-posn 13 10))
(check-expect (posn-up-x (make-posn 0 0) 89)
              (make-posn 89 0))

(define (posn-up-x position z)
  (make-posn z (posn-y position)))

(test)
