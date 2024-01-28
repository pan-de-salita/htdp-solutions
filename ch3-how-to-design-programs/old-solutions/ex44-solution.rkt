;; Exercise 43.
;; Let’s work through the same problem statement
;; with a time-based data definition:
;; An AnimationState is a Number.
;; interpretation the number of clock ticks
;; since the animation started.

#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require test-engine/racket-tests)

;; Unit tests
(check-expect (render 50) (place-image CAR (car-position 50) Y-CAR BACKGROUND))
(check-expect (render 200) (place-image CAR (car-position 200) Y-CAR BACKGROUND))

(check-expect (tick-handler 20) 21)
(check-expect (tick-handler 78) 79)

(check-expect (end? (+ X-CAR-STOP 1)) #true)
(check-expect (end? 30) #false)

(check-expect (hyper 21 10 20 "enter") 21)
(check-expect (hyper 42 10 20 "button-down") 10)
(check-expect (hyper 42 10 20 "move") 42)

;; Definitions
(define WHEEL-RADIUS 5) ; Single point of control over the rendered image sizes.
(define WHEEL-DIAMETER (* WHEEL-RADIUS 2))
(define VELOCITY (/ WHEEL-RADIUS 2))

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

(define BACKGROUND-WIDTH (* CAR-WIDTH 8))
(define BACKGROUND-HEIGHT (* (image-height CAR) 2))
(define BACKGROUND
  (place-image TREE
               (/ BACKGROUND-WIDTH 3)
               (- BACKGROUND-HEIGHT (/ (image-height TREE) 2))
               (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT)))

(define Y-CAR (- BACKGROUND-HEIGHT (/ (image-height CAR) 2)))
(define X-CAR-STOP (+ BACKGROUND-WIDTH CAR-WIDTH))

;; Number -> Number
;; Converts ticks to the car-position.
(define (car-position ticks)
  (- (* VELOCITY ticks) (/ CAR-WIDTH 2)))

;; WorldState -> Image
;; Places the car into the scene.
(define (render as)
  (place-image CAR
               (car-position as) Y-CAR
               BACKGROUND))

;; WorldState -> WorldState
;; Sets world state for every clock tick.
(define (tick-handler as)
  (+ as 1))

;; WorldState -> Number
;; Calculates when to stop the car.
(define (end? as)
  (> (car-position as) X-CAR-STOP))

; AnimationState -> AnimationState
; launches the program from some initial state
(define (main as)
  (big-bang as
            [on-tick tick-handler]
            [to-draw render]
            [on-mouse hyper]
            [stop-when end?]))

;; AnimationState Number Number String -> AnimationState
;; places the car at x-mouse if the given
;; me is "button-down"
(define (hyper x-position-of-car x-mouse y-mouse me)
  (cond [(string=? "button-down" me) x-mouse]
        [else x-position-of-car]))

;; Application
(main 0)
