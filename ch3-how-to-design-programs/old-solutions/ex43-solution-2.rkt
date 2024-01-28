#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

(define WIDTH-OF-WORLD 200)
(define HEIGHT-OF-WORLD 50)

(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 3))
; the number of pixels the car moves for every clock tick
(define SPEED 3)
(define SINE-AMPL 5)
(define SINE-FREQ 5)

(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))
(define SPACE
  (rectangle WHEEL-DISTANCE WHEEL-RADIUS "solid" "transparent"))
(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))
(define CAR-BODY
  (rectangle (* WHEEL-RADIUS 10) (* WHEEL-RADIUS 3) "solid" "red"))
(define CAR
  (underlay/align/offset "middle" "bottom"
   CAR-BODY
   0 WHEEL-RADIUS
   BOTH-WHEELS))
(define Y-CAR (- HEIGHT-OF-WORLD (/ (image-height CAR) 2)))
(define HALF-OF-CAR-WIDTH (floor (/ (image-width CAR) 2)))

(define tree
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))
(define BACKGROUND
  (overlay/align
   "middle" "bottom"
   tree
   (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD)))

; An AnimationState is a Number.
; interpretation the number of clock ticks
; since the animation started

; AnimationState -> AnimationState
; advances the AnimationState by one tick
(check-expect (tock 20) 21)
(check-expect (tock 78) 79)
(define (tock as)
  (+ as 1))

; Number -> Number
; provides a Y coordinate given an X coordinate
; for use in plotting a sine wave
(define (sine-y x)
  (+
   (floor (* SINE-AMPL (sin(/ x SINE-FREQ))))
   Y-CAR))

; AnimationState -> Image
; places the car on the BACKRGROUND SCENE
; according to the given world state
(check-expect (render 50) (place-image CAR (- (* 50 SPEED) HALF-OF-CAR-WIDTH) (sine-y (* 50 SPEED)) BACKGROUND))
(check-expect (render 100) (place-image CAR (- (* 100 SPEED) HALF-OF-CAR-WIDTH) (sine-y (* 100 SPEED)) BACKGROUND))
(check-expect (render 150) (place-image CAR (- (* 150 SPEED) HALF-OF-CAR-WIDTH) (sine-y (* 150 SPEED)) BACKGROUND))
(define (render as)
   (place-image
     CAR
     (- (* as SPEED) HALF-OF-CAR-WIDTH)
     (sine-y (* as SPEED))
     BACKGROUND))

; AnimationState -> Boolean
; stop once the car has left the scene
(check-expect (end? WHEEL-RADIUS) false)
(check-expect (end? (/ (+ (+ WIDTH-OF-WORLD (image-width CAR)) 10) SPEED)) true)
(define (end? as)
  (> (* as SPEED) (+ WIDTH-OF-WORLD (image-width CAR) 2)))

(define (main as)
  (big-bang as
    [on-tick tock]
    [to-draw render]
    [stop-when end?]))
