#lang racket
(require 2htdp/image)
(require 2htdp/universe)

;; Definitions
(define WHEEL-RADIUS 5) ; Single point of control over the rendered image sizes.
(define WHEEL-DIAMETER (* WHEEL-RADIUS 2))
(define VELOCITY 3)

(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define TWO-WHEELS (underlay/offset WHEEL (* WHEEL-DIAMETER 2) 0 WHEEL))

(define CAR-BODY (rectangle (* WHEEL-DIAMETER 4) WHEEL-DIAMETER "solid" "blue"))
(define CAR-BODY-TOP (rectangle (* WHEEL-DIAMETER 2) WHEEL-DIAMETER "solid" "blue"))
(define CAR
  (underlay/offset CAR-BODY-TOP
                   0 WHEEL-DIAMETER
                   (underlay/offset CAR-BODY
                                    0 WHEEL-RADIUS
                                    TWO-WHEELS)))
(define CAR-WIDTH (image-width CAR))

(define TREE
  (underlay/offset (circle WHEEL-DIAMETER "solid" "green")
               0 WHEEL-DIAMETER
               (rectangle (/ WHEEL-RADIUS 2) (* WHEEL-RADIUS 3) "solid" "brown")))

(define BACKGROUND-WIDTH 400)
(define BACKGROUND-HEIGHT (* (image-height CAR) 2))
(define BACKGROUND
  (place-image TREE
               (/ BACKGROUND-WIDTH 3)
               (- BACKGROUND-HEIGHT (/ (image-height TREE) 2))
               (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT)))

(define Y-CAR (- BACKGROUND-HEIGHT (/ (image-height CAR) 2)))
(define X-CAR-STOP (+ BACKGROUND-WIDTH CAR-WIDTH))

;; Number -> Number
;; Converts car x-coordinate
;; from right edge position to center position.
(define (car-position x)
  (- x (/ CAR-WIDTH 2)))

;; A WorldState is a Number.
;; ws (world state) is the number of pixels between
;; the left border of the scene and the right edge of the car.

;; WorldState -> Image
;; Places the car into the scene.
(define (render ws)
  (place-image CAR (car-position ws) Y-CAR BACKGROUND))

;; WorldState -> WorldState
;; Sets world state for every clock tick.
(define (tick-handler ws)
  (+ ws VELOCITY))

;; WorldState -> Number
;; Calculates when to stop the car.
(define (end? ws)
  (> ws X-CAR-STOP))

(define (main ws)
  (big-bang ws
    [to-draw render]
    [on-tick tick-handler]
    [stop-when end?]))


;; Application
;(main CAR-WIDTH)
(main 0)
