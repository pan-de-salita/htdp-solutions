;; sample problem 4.4.1 - add a status line. it says "descending" when the UFO's height
;; is above one third of the height of the canvas. it swithes to "closing in" below
;; that. and finally, when the UFO has reached the bottom of the canvas, the status
;; notifies the player that the UFO has "landed". you are free to use appropriate
;; colors for the status line. 

#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;; a WorldState falls into one of three intervals:
;; - between 0 and CLOSE
;; - between CLOSE and TOUCHDOWN
;; - below TOUCHDOWN (including LANDED)

(define WIDTH 200) ;; distances in terms of pixels
(define HEIGHT 200)
(define MTSCN (empty-scene WIDTH HEIGHT "navy"))

(define UFO (overlay (circle 10 "solid" "silver")
                     (rectangle 40 3 "solid" "silver")))
(define UFO-XPOS (* 1/2 WIDTH))
(define VELOCITY 1)

(define CLOSE (* 1/3 HEIGHT))
(define TOUCHDOWN (- HEIGHT (* 1/2 (image-height UFO))))
(define LANDED (+ TOUCHDOWN VELOCITY))

;; WorldState -> WorldState
(define (main y0)
  (big-bang y0
    [on-tick nxt]
    [to-draw render]
    [stop-when landed?]))

;; WorldState -> WorldState
;; purpose: computes next y-location of UFO
(check-expect (nxt 11) 12)
(define (nxt y)
  (cond ((<= y CLOSE) (+ y VELOCITY))
        (else (+ y (- VELOCITY 0.25)))))

;; WorldState -> Image
;; purpose: computes landing status of UFO
(check-expect (status CLOSE) (text "d e s c e n d i n g" 14 "green"))
(check-expect (status (/ HEIGHT 2)) (text "c l o s i n g   i n" 14 "yellow"))
(check-expect (status HEIGHT) (text "l a n d e d" 14 "red"))
(define (status y)
  (cond ((<= 0 y CLOSE)
         (text "d e s c e n d i n g" 14 "green"))
        ((<= CLOSE y TOUCHDOWN)
         (text "c l o s i n g   i n" 14 "yellow"))
        ((>= y TOUCHDOWN)
         (text "l a n d e d" 14 "red"))))

;; WorldState -> Image
;; purpose: places UFO at given y-location into the center of MTSCN
(check-expect (render 11) (place-image UFO UFO-XPOS 11
                                       (place-image (status 11)
                                                    (/ (image-width (status 11)) 2)
                                                    (/ (image-height (status 11)) 2)
                                                    MTSCN)))
(define (render y)
  (place-image UFO UFO-XPOS y (place-image (status y)
                                           (/ (image-width (status y)) 2)
                                           (/ (image-height (status y)) 2)
                                           MTSCN)))

;; WorldState -> Boolean
;; purpose: stops program when UFO has landed
(check-expect (landed? HEIGHT) #t)
(define (landed? y)
  (>= y LANDED))

(test)
(main 0)
