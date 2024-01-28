;; sample problem - design a program that simulates the descent of a UFO.

#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require test-engine/racket-tests)

;; a WorldState is a Number.
;; interpretation: number of pixels between the top and the UFO.

(define WIDTH 300)
(define HEIGHT 100)
(define CLOSE (/ HEIGHT 3))
(define MTSCN (empty-scene WIDTH HEIGHT))
(define UFO (overlay (circle (/ HEIGHT 8) "solid" "black")
                     (rectangle (/ HEIGHT 2) (/ HEIGHT 45) "solid" "black")))

;; WorldState -> WorldState
(define (main y0)
  (big-bang y0
            [on-tick nxt]
            [to-draw render]))

;; WorldState -> WorldState
;; computes next location of UFO.
(check-expect (nxt 11) 14)
(define (nxt y)
  (cond [(>= CLOSE y) (+ y 2)]
        [else (+ y 1)]))

;; WorldState -> Image
;; places UFO at given height into the center of MTSCN.
(check-expect (render 11) (place-image UFO (/ WIDTH 2) 11 MTSCN))
(define (render y)
  (place-image UFO (/ WIDTH 2) y MTSCN))

(main 0)
