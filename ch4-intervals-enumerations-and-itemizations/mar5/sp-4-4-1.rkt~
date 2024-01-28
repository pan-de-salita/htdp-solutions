;; sample problem 4.4.0 - design a program that simulates the descent of a UFO.

#lang htdp/bsl
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;; a WorldState is a Number. 
;; interpretation: number of pixels between the top and the UFO. 

(define WIDTH 300) ;; distances in terms of pixels
(define HEIGHT 100)
(define CLOSE (/ HEIGHT 3))
(define MTSCN (empty-scene WIDTH HEIGHT))
(define UFO (overlay (circle 10 "solid" "green")
                     (rectangle 40 3 "solid" "green")))
(define UFO-XPOS (* 1/2 WIDTH))
(define VELOCITY 3)

;; WorldState -> WorldState
(define (main y0)
  (big-bang y0
    [on-tick nxt]
    [to-draw render]))

;; WorldState -> WorldState
;; purpose: computes next y-location of UFO
(check-expect (nxt 11) 14)
(define (nxt y)
  (+ y VELOCITY))

;; WorldState -> Image
;; places UFO at given y-location into the center of MTSCN
(check-expect (render 11) (place-image UFO UFO-XPOS 11 MTSCN))
(define (render y)
  (place-image UFO UFO-XPOS y MTSCN))

(test)
