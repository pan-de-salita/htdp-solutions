;; sample problem 4.4.1 - design a program that simulates the descent of a UFO.

;; sample problem 4.4.2 - add a status line. it says "descending" when the UFO's
;; height is above one third of the height of the canvas. it switches to "closing
;; in" below that. and finally, when the UFO has reached the bottom of the canvas,
;; the status notifies the player that the UFO has "landed". you are free to use
;; appropriate colors for the status line.

#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require test-engine/racket-tests)

;; a WorldState is a Number.
;; interpretation: number of pixels between the top and the UFO.

(define WIDTH 300)
(define HEIGHT 100)
(define FONT-SIZE 15)

(define MTSCN (empty-scene WIDTH HEIGHT "navy"))
(define UFO (overlay (circle (/ HEIGHT 10) "solid" "gray")
                     (rectangle (/ HEIGHT 2) (/ HEIGHT 45) "solid" "gray")))
(define STAT-LINE (rectangle WIDTH (image-height UFO) "solid" "transparent"))

(define CLOSE (/ HEIGHT 3))
(define LANDED (- HEIGHT (/ (image-height UFO) 2)))
(define STAT-LINE-Y-COR (/ (image-height STAT-LINE) 2))

;; WorldState -> WorldState
(define (main y0)
  (big-bang y0
            [on-tick nxt]
            [to-draw render]
            [stop-when landed?]))

;; WorldState -> WorldState
;; stops program when UFO has landed.
(check-expect (landed? (add1 LANDED)) #t)
(define (landed? y)
  (>= y LANDED))

;; WorldState -> WorldState
;; computes next y-coordinate of UFO.
(check-within (nxt 11) 11.75 0.01)
(define (nxt y)
  (cond [(>= CLOSE y) (+ y 0.75)]
        [else (+ y 0.5)]))

;; WorldState -> String
;; - (< y CLOSE)
;; - (>= y LANDED)
;; - (>= y CLOSE)
;; computes descent status text text of UFO.
(check-expect (descent-status-text 40) "c l o s i n g   i n")
(define (descent-status-text y)
  (cond [(< y CLOSE) "d e s c e n d i n g"]
        [(>= y (sub1 LANDED)) "l a n d e d"]
        [else "c l o s i n g   i n"]))

;; WorldState -> String
;; - (< y CLOSE)
;; - (>= y LANDED)
;; - (>= y CLOSE)
;; computes color of descent status text of UFO.
(check-expect (descent-status-color 40) "pink")
(define (descent-status-color y)
  (cond [(< y CLOSE) "white"]
        [(>= y (sub1 LANDED)) "yellow"]
        [else "pink"]))

;; WorldState -> Image
;; displays the descent status of UFO.
(check-expect (draw-descent-status 40)
              (text "c l o s i n g   i n"
                    FONT-SIZE
                    "yellow"))
(define (draw-descent-status y)
  (text (descent-status-text y)
        FONT-SIZE
        (descent-status-color y)))

;; WorldState -> Image
;; displays the appropriate text in STAT-LINE.
(check-expect (stat-line 40)
              (place-image (underlay/align/offset "right" "bottom"
                                                  STAT-LINE
                                                  (- (/ FONT-SIZE 2)) 0
                                                  (draw-descent-status 40))
                           (/ WIDTH 2) STAT-LINE-Y-COR
                           MTSCN))
(define (stat-line y)
  (place-image (underlay/align/offset "right" "bottom"
                                      STAT-LINE
                                      (- (/ FONT-SIZE 2)) 0
                                      (draw-descent-status y))
               (/ WIDTH 2) STAT-LINE-Y-COR
               MTSCN))

;; WorldState -> Image
;; places UFO at given height into the center of MTSCN.
(check-expect (render 11)
              (place-image UFO
                           (/ WIDTH 2) 11
                           (stat-line 11)))
(define (render y)
  (place-image UFO
               (/ WIDTH 2) y
               (stat-line y)))

(test)
(main 0)
