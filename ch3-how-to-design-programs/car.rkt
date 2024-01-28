#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; PHYSICAL CONSTANT: car
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))

; PHYSICAL CONSTANT: background
(define WIDTH-OF-WORLD 400)

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
             (empty-scene WIDTH-OF-WORLD (+ WHEEL-RADIUS (image-height CAR)))))

; A WorldState is a Number.
; interpretation the number of pixels between
; the left border of the scene and the right-most
; edge of the car

; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
  (big-bang ws
            [on-tick tock]
            [to-draw render]
            [stop-when end-of-the-road?]))

; WorldState -> Image
; places the car into the BACKGROUND scene according
; to the given world state
(define (render cw)
  (place-image CAR
               (- cw (/ (image-width CAR) 2))
               (y-cor CAR)
               BACKGROUND))

; Image -> Number
; computes the y-coordinate for the car in the background
(define (y-cor img)
  (- (image-height img) WHEEL-RADIUS))

; WorldState -> WorldState
; moves the car by 3 pixels for every clock tick
(define (tock cw)
  (+ 3 cw))

(check-expect (tock 20) 23)
(check-expect (tock 78) 81)

; WorldState -> WorldState
; tells the main function to stop running when
; cw reaches 400
(define (end-of-the-road? cw)
  (= cw 400))

(check-expect (end-of-the-road? 399) #f)
(check-expect (end-of-the-road? 400) #t)
