#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require test-engine/racket-tests)

;; Definitions
(define CAT .)

(define BACKGROUND-WIDTH (* (image-width CAT) 5))
(define BACKGROUND-HEIGHT (* (image-height CAT) 2))
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))

(define CAT-Y-COR (* (image-height CAT) 1.5))
(define CAT-STOP-POS (+ BACKGROUND-WIDTH (image-width CAT)))
(define CAT-START-POS (/ (image-width CAT) 2))

(define VELOCITY 3)

;; A WorldState is a Number.
;; Interpretation: The number of clock ticks since the
;; animation started.

;; WorldState -> WorldState
;; Purpose: Places the cat in the background according
;; to the WorldState given.
(define (render ws)
  (place-image CAT
               (position-check ws)
               CAT-Y-COR
               BACKGROUND))

;; WorldState -> WorldState
;; Purpose: Checks whether the cat is out of frame.
;; Returns the cat to the left of the background if
;; yes.
(define (position-check ws)
  (if (>= (cat-position ws) CAT-STOP-POS)
      CAT-START-POS
      (cat-position ws)))

;; WorldState -> Number
;; Purpose: Computes the number of pixels between the
;; right-most edge of the cat and the left-most edge
;; of the background according to the number of ticks
;; given.
(define (cat-position ws)
  (- (* VELOCITY ws)
     CAT-START-POS))

;; WorldState -> WorldState
;; Purpose: Starts the program from some initial state.
(define (main ws)
  (big-bang ws
    [to-draw render]
    [on-tick add1]))
