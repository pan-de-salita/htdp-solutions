;; Sample Problem. Design a program that moves a car from left to right
;; on the world canvas, three pixels per clock tick.

#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require test-engine/racket-tests)

;; Constants
(define WHEEL-RADIUS 5) ;; Single point of control
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))

(define WIDTH-OF-WORLD (* 80 WHEEL-RADIUS))
(define HEIGHT-OF-WORLD (* 20 WHEEL-RADIUS))

(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define SPACE-BETWEEN-WHEELS (rectangle (* 2 WHEEL-RADIUS) WHEEL-RADIUS
                                        "solid" "transparent"))
(define BOTH-WHEELS (beside WHEEL SPACE-BETWEEN-WHEELS WHEEL))
(define CAR-BODY (above (rectangle (* 4 WHEEL-RADIUS) WHEEL-RADIUS
                                   "solid" "red")
                        (rectangle (* 8 WHEEL-RADIUS) (* 2 WHEEL-RADIUS)
                                   "solid" "red")))
(define CAR (overlay/align/offset "middle" "bottom"
                                  BOTH-WHEELS
                                  0 (- WHEEL-RADIUS)
                                  CAR-BODY))

(define BACKGROUND (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD "white"))

(define CAR-Y-COR (- HEIGHT-OF-WORLD (/ (image-height CAR) 2)))

;; A WorldState is a Number.
;; The number of pixels between the left border of the scene and the car.

;; WorldState -> Image
;; Places CAR into BACKGROUND according to the given WorldState.
(check-expect (render 50) (place-image CAR 50 CAR-Y-COR BACKGROUND))
(check-expect (render 200) (place-image CAR 200 CAR-Y-COR BACKGROUND))

(define (render cw)
  (place-image CAR
               cw
               CAR-Y-COR
               BACKGROUND))

;; WorldState -> WorldSTate
;; Moves CAR by 3 pixels for every clock tick.
(check-expect (dist-travelled 20) 23)
(check-expect (dist-travelled 78) 81)

(define (dist-travelled cw)
  (+ 3 cw))

;; WorldState -> WorldState
;; Launches the program from initial state.
(define (main ws)
  (big-bang ws
            [on-tick dist-travelled]
            [to-draw render]))

(test)
(main 0)
