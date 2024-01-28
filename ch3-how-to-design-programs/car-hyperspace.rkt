;; Sample Problem: Design a program that moves
;; across the world canvas, from left to right,
;; at the rate of three pixels per clock tick.
;; If the mouse is clicked anywhere on the canvas,
;; the car is placed at the x-coordinate of that
;; click.

#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

;; Definitions
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))

(define WIDTH-OF-WORLD 400)
(define HEIGHT-OF-WORLD (/ WIDTH-OF-WORLD 4))

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

(define TREE
  (overlay/xy (circle 20 "solid" "green")
              (- (/ (image-width (circle 20 "solid" "green")) 2) 5) 15
              (rectangle 10 50 "solid" "brown")))

(define BACKGROUND
  (put-image TREE
             340 (/ (image-height TREE) 2)
             (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD)))

;; A WorldState is a Number.
;; Interpretation: The number of clock ticks
;; since the animation started.

;; WorldState -> WorldState
;; launches the program from some initial state
(define (main as)
  (big-bang as
            [on-tick tock]
            [to-draw render]
            [on-mouse hyper]
            [stop-when end-of-the-road?]))

;; WorldState -> Image
;; places the car into the BACKGROUND scene according
;; to the given world state
(define (render cw)
  (place-image CAR
               (- cw (/ (image-width CAR) 2))
               (y-cor CAR)
               BACKGROUND))

;; Image -> Number
;; computes the y-coordinate for the car in the background
(define (y-cor img)
  (- HEIGHT-OF-WORLD (/ (image-height img) 2)))

;; WorldState -> WorldState
;; moves the car by 3 pixels for every clock tick
(define (tock cw)
  (+ 3 cw))

;; WorldState -> WorldState
;; tells the main function to stop running when
;; the car is out of frame
(define (end-of-the-road? cw)
  (>= cw (+ WIDTH-OF-WORLD (image-width CAR))))

;; WorldState Number Number String -> WorldState
;; places the car at x-mouse if the given
;; me is "button-down"
(define (hyper x-position-of-car x-mouse y-mouse me)
  (cond [(string=? "button-down" me) x-mouse]
        [else x-position-of-car]))

;; Unit tests
(check-expect (render 10) (place-image CAR
                                       (- 10 (/ (image-width CAR) 2))
                                       (y-cor CAR)
                                       BACKGROUND))
(check-expect (render 42) (place-image CAR
                                       (- 42 (/ (image-width CAR) 2))
                                       (y-cor CAR)
                                       BACKGROUND))

(check-expect (y-cor CAR) (- HEIGHT-OF-WORLD
                             (/ (image-height CAR) 2)))
(check-expect (y-cor TREE) (- HEIGHT-OF-WORLD
                              (/ (image-height TREE) 2)))

(check-expect (tock 20) 23)
(check-expect (tock 78) 81)

(check-expect (end-of-the-road? WIDTH-OF-WORLD) #f)
(check-expect (end-of-the-road? (+ WIDTH-OF-WORLD (image-width CAR) 3)) #t)

(check-expect (hyper 21 10 20 "enter") 21)
(check-expect (hyper 42 10 20 "button-down") 10)
(check-expect (hyper 42 10 20 "move") 42)
