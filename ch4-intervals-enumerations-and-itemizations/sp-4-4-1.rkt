;; sample problem 4.4.1 - add a status line. it says "descending" when the UFO's height
;; is above one third of the height of the canvas. it swithes to "closing in" below
;; that. and finally, when the UFO has reached the bottom of the canvas, the status
;; notifies the player that the UFO has "landed". you are free to use appropriate
;; colors for the status line. 

#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;; a WorldState is a Number. 
;; interpretation: number of pixels between the top and the UFO. 

(define WIDTH 300) ;; distances in terms of pixels
(define HEIGHT 100)
(define MTSCN (empty-scene WIDTH HEIGHT "darkgrey"))

(define UFO (overlay (circle 10 "solid" "black")
                     (rectangle 40 3 "solid" "black")))
(define UFO-XPOS (* 1/2 WIDTH))
(define VELOCITY 1)

(define DESCENDING (* 1/3 HEIGHT))

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
  (+ y VELOCITY))

;; can be improved. write separate function for cond
;; WorldState -> Image
;; purpose: computes landing status of UFO
(check-expect (status DESCENDING) (text "descending" 14 "yellow"))
(check-expect (status (/ HEIGHT 2)) (text "closing in" 14 "orange"))
(check-expect (status HEIGHT) (text "landed" 14 "red"))
(define (status y)
  (text (cond ((<= y DESCENDING) "descending")
              ((and (> y DESCENDING) (< y (- HEIGHT (/ (image-height UFO) 2))) "closing in"))
              (else "landed"))
        14
        (cond ((<= y DESCENDING) "yellow")
              ((and (> y DESCENDING) (< y (- HEIGHT (/ (image-height UFO) 2))) "orange"))
              (else "red"))))

;; WorldState -> Image
;; purpose: places UFO at given y-location into the center of MTSCN
(check-expect (render 11) (place-image UFO UFO-XPOS 11
                                       (place-image (status 11)
                                                    (/ (image-width (status 11)) 2)
                                                    (/ (image-height (status 11)) 2)
                                                    MTSCN)))
(define (render y)
  (place-image UFO UFO-XPOS y  (place-image (status y)
                                            (/ (image-width (status y)) 2)
                                            (/ (image-height (status y)) 2)
                                            MTSCN)))

;; WorldState -> Boolean
;; purpose: stops program when UFO has landed
(check-expect (landed? (+ HEIGHT 1)) #t)
(define (landed? y)
  (> y (+ (- HEIGHT (/ (image-height UFO) 2)) 1)))

(test)
(main 0)