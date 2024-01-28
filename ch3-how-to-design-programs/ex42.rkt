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
;; The number of pixels between the left border of BACKGROUND and the
;; right-most edge of CAR.

;; WorldState -> Image
;; Places CAR into BACKGROUND according to the given WorldState.
(check-expect (render 50)
              (place-image CAR
                           (- 50 (/ (image-width CAR) 2)) CAR-Y-COR
                           BACKGROUND))
(check-expect (render 200)
              (place-image CAR
                           (- 200 (/ (image-width CAR) 2)) CAR-Y-COR
                           BACKGROUND))

(define (render ws)
  (place-image CAR
               (- ws (/ (image-width CAR) 2))
               CAR-Y-COR
               BACKGROUND))

;; WorldState -> WorldSTate
;; Moves CAR by 3 pixels for every clock tick.
(check-expect (dist-travelled 20) 23)
(check-expect (dist-travelled 78) 81)

(define (dist-travelled ws)
  (+ VELOCITY ws))

;; WorldState -> Boolean
;; Stops the program when CAR has travelled out of frame.
(check-expect (end-of-road? 420) #f)
(check-expect (end-of-road? 443) #t)

(define (end-of-road? ws)
  (>= ws STOPPING-POINT))

;; WorldState -> WorldState
;; Launches the program from initial state.
(define (main ws)
  (big-bang ws
            [on-tick dist-travelled]
            [to-draw render]
            [stop-when end-of-road?]))

(test)
(main 0)
