#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(define SPEED 3)
;; number of pixels the ball moves per clock tick
(define-struct ball-direction [location direction])
;; structure type combining two data types:
;; - location | a number denoting the number of pixels between ball and top of canvas
;; - direction | a string denoting the direction the ball should travel ("up" or "down")
(make-ball-direction 10 "up")
;; this structure represents the ball 10 pixels from the top and moves "up"
(make-ball-direction 10 "down")
;; this sturcture represetns the ball 10 pixels from the top and moves "down"
