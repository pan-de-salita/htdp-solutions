#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require test-engine/racket-tests)

;; Constant definitions
(define WHEEL-RADIUS 5) ;; Single point of control
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))
(define VELOCITY 3)

(define WIDTH-OF-WORLD (* 80 WHEEL-RADIUS))
(define HEIGHT-OF-WORLD (* 15 WHEEL-RADIUS))

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
(define CAR-Y-COR (- HEIGHT-OF-WORLD (/ (image-height CAR) 2)))

(define TREE (underlay/xy (circle (* 3 WHEEL-RADIUS) "solid" "green")
                          (- (* 3 WHEEL-RADIUS) 1) (* 3 WHEEL-RADIUS)
                          (rectangle 2 (* 8 WHEEL-RADIUS) "solid" "brown")))
(define TREE-Y-COR (- HEIGHT-OF-WORLD (/ (image-height TREE) 2)))

(define BACKGROUND (place-image TREE
                                250 TREE-Y-COR
                                (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD "white")))

(define STOPPING-POINT (+ WIDTH-OF-WORLD (image-width CAR) 3))

;; A WorldState is a Number.
;; The number of clock ticks since the animation started.

;; WorldState -> Number
;; Calculates the distance CAR travels within BACKGROUND according to
;; the given WorldState.
(define (dist-to-travel ws)
  (* 3 (- ws (/ (image-width CAR) 2))))

;; WorldState -> Image
;; Places CAR into BACKGROUND according to the given WorldState.
(check-expect (render 50)
              (place-image CAR
                           (dist-to-travel 50) CAR-Y-COR
                           BACKGROUND))
(check-expect (render 200)
              (place-image CAR
                           (dist-to-travel 200) CAR-Y-COR
                           BACKGROUND))

(define (render ws)
  (place-image CAR
               (dist-to-travel ws)
               CAR-Y-COR
               BACKGROUND))

;; WorldState -> Boolean
;; Stops the program when CAR has travelled out of frame.
(check-expect (end-of-road? 163) #f)
(check-expect (end-of-road? 168) #t)

(define (end-of-road? ws)
  (>= (dist-to-travel ws) STOPPING-POINT))

;; WorldState -> WorldState
;; Launches the program from initial state.
(define (main ws)
  (big-bang ws
            [on-tick add1]
            [to-draw render]
            [stop-when end-of-road?]))

(test)
(main 0)
