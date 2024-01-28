;; Sample Problem: Design a program that
;; moves a car from left to right on the
;; world canvas, three pixels per clock tick.

#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; PHYSICAL CONSTANT: car
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))

; PHYSICAL CONSTANT: background
(define WIDTH-OF-WORLD 400)
(define HEIGHT-OF-WORLD (/ WIDTH-OF-WORLD 4))

; GRAPHICAL CONSTANT: car
(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))
(define SPACE
  (rectangle (* 2 WHEEL-RADIUS) (/ WHEEL-RADIUS 2) "solid" "transparent"))
(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))
(define CAR-BODY
  (above (rectangle (* 4 WHEEL-RADIUS) WHEEL-RADIUS "solid" "red")
         (rectangle (* 8 WHEEL-RADIUS) (* 2 WHEEL-RADIUS) "solid" "red")
         (rectangle (image-width BOTH-WHEELS) WHEEL-RADIUS "solid" "transparent")))
(define CAR
  (overlay/align "middle" "bottom"
                 BOTH-WHEELS
                 CAR-BODY))

; GRAPHICAL CONSTANT: tree
(define TREE
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))

; GRAPHICAL CONSTANT: background
(define BACKGROUND
  (put-image TREE
             340 (/ (image-height TREE) 2)
             (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD)))

;; An AnimationState is a Number.
;; Interpretation: The number of clock ticks
;; since the animation started.

; AnimationState -> AnimationState
; launches the program from some initial state
(define (main as)
  (big-bang as
            [on-tick tock]
            [to-draw render]
            [stop-when end-of-the-road?]))

; AnimationState -> Image
; places the right-most edge of the car 3 pixels
; further away from the left margin of the BACKGROUND
; scene with every clock tick
(define (render as)
  (place-image CAR
               (- (* as 3) (/ (image-width CAR) 2))
               (y-cor CAR)
               BACKGROUND))

; Image -> Number
; computes the y-coordinate for the car in the background
(define (y-cor img)
  (- HEIGHT-OF-WORLD (/ (image-height img) 2)))

; AnimationState -> AnimationState
; adds 1 clock tick to the animation state
(define (tock as)
  (add1 as))

(check-expect (tock 20) 21)
(check-expect (tock 78) 79)

; AnimationState -> AnimationState
; tells the main function to stop running when
; the car is out of frame
(define (end-of-the-road? as)
  (>= as (+ WIDTH-OF-WORLD (image-width CAR) 3)))

(check-expect (end-of-the-road? 400) #f)
(check-expect (end-of-the-road? (+ 400 (image-width CAR) 3)) #t)

;; How do you think this program relates to animate
;; from Prologue: How to Program?
;; Answer: The right-most edge of the car is moved
;; 3 pixels further away from the left margin of the
;; BACKGROUND scene with every clock tick.

;; How does this program operate differently compared
;; to car-dist-based.rkt?
;; Answer: It does not. The data definitions used, however, are
;; different. In this program, we use AnimationState, which
;; denotes the number of clock ticks since the animation started.
;; In car-dist-based.rkt, on the other hand, we use WorldState, which
;; denotes the number of pixels between the left border of the
;; scene and the right-most edge of the car.
