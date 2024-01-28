;; sample problem 4.5.0
;; design a program that launches a rocket when the user of your
;; program presses the space bar. the program first displays the rocket sitting at
;; the bottom of the canvas. once launched, it moves upward at three pixels per
;; clock tick.

#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;; a RocketStatus is one of:
;; - "resting" | represents a grounded rocket
;; - NonnegativeNumber | denotes the height of a rocket in flight

(define HEIGHT 200)
(define WIDTH 300)
(define MTSCN (empty-scene WIDTH HEIGHT "navy"))

(define ROCKET (rectangle 20 60 "solid" "darkgrey"))
(define GROUNDED (- HEIGHT
                   (* 1/2 (image-height ROCKET))))

(define Y-DELTA 3)
(define BLASTOFF (- GROUNDED Y-DELTA))

;; RocketStatus -> RocketStatus
(define (main y)
  (big-bang y
            [to-draw render]
            [on-key command]
            [on-tick ascend-by]))

;; RocketStatus -> Image
;; places ROCKET into MTSCN
(check-expect (render "resting") (place-image ROCKET (* WIDTH 1/2) GROUNDED MTSCN))
(check-expect (render 10) (place-image ROCKET (* WIDTH 1/2) 10 MTSCN))
(define (render status)
  (place-image ROCKET
               (* WIDTH 1/2)
               (cond [(and (string? status) (string=? status "resting")) GROUNDED]
                     [else status])
               MTSCN))

;; RocketStatus -> RocketStatus
;; determines if ROCKET should be GROUNDED or BLASTOFF
(check-expect (command "resting" " ") BLASTOFF)
(check-expect (command "resting" "g") "resting")
(define (command status key)
  (cond [(key=? key " ") BLASTOFF]
        [else status]))

;; RocketStatus -> RocketStatus
;; determines altitude of ROCKET post-launch
(check-expect (ascend-by "resting") "resting")
(check-expect (ascend-by 15) (- 15 Y-DELTA))
(define (ascend-by status)
  (cond [(number? status) (- status Y-DELTA)]
        [else status]))

(test)
(main "resting")
