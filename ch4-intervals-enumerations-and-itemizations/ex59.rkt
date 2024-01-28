;; exercise 59.
;; finish the design of a world program that simulates the traffic light
;; FSA (finite state automata). here is the main function:
;;        ;; TrafficLight -> TrafficLight
;;        ;; simulates a clock-based American traffic light
;;        (define (traffic-light-simulation initial-state)
;;          (big-bang initial-state
;;            [to-sraw tl-render]
;;            [on-tick tl-next 1]))

#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

;; a TrafficLight in a String
;; - "red"
;; - "yellow"
;; - "green"
;; interpretation: the color of a traffic light

(define TL-BODY-WIDTH 90)
(define TL-BODY-HEIGHT 30)
(define TL-BODY (empty-scene TL-BODY-WIDTH TL-BODY-HEIGHT "black"))

(define LIGHT-RADIUS 10)

(define RED-X-POS (- (* 1/3 TL-BODY-WIDTH) LIGHT-RADIUS))
(define YELLOW-X-POS (* 1/2 TL-BODY-WIDTH))
(define GREEN-X-POS (- TL-BODY-WIDTH (* LIGHT-RADIUS 2)))
(define TL-Y-POS (* 1/2 TL-BODY-HEIGHT))

;; TrafficLight -> TrafficLight
;; yields the next state, given current state cs
(check-expect (tl-next "red") "green")
(check-expect (tl-next "yellow") "red")
(check-expect (tl-next "green") "yellow")

(define (tl-next cs)
  (cond [(string=? cs "red") "green"]
        [(string=? cs "yellow") "red"]
        [(string=? cs "green") "yellow"]))

;; TrafficLight -> Image
;; renders the current state cs as an image
(check-expect (tl-render "red")
              (place-images (list (tl-red "red")
                                  (tl-yellow "red")
                                  (tl-green "red"))
                            (list (make-posn RED-X-POS TL-Y-POS)
                                  (make-posn YELLOW-X-POS TL-Y-POS)
                                  (make-posn GREEN-X-POS TL-Y-POS))
                            TL-BODY))
(check-expect (tl-render "yellow")
              (place-images (list (tl-red "yellow")
                                  (tl-yellow "yellow")
                                  (tl-green "yellow"))
                            (list (make-posn RED-X-POS TL-Y-POS)
                                  (make-posn YELLOW-X-POS TL-Y-POS)
                                  (make-posn GREEN-X-POS TL-Y-POS))
                            TL-BODY))
(check-expect (tl-render "green")
              (place-images (list (tl-red "green")
                                  (tl-yellow "green")
                                  (tl-green "green"))
                            (list (make-posn RED-X-POS TL-Y-POS)
                                  (make-posn YELLOW-X-POS TL-Y-POS)
                                  (make-posn GREEN-X-POS TL-Y-POS))
                            TL-BODY))

(define (tl-render cs)
  (place-images (list (tl-red cs)
                      (tl-yellow cs)
                      (tl-green cs))
                (list (make-posn RED-X-POS TL-Y-POS)
                      (make-posn YELLOW-X-POS TL-Y-POS)
                      (make-posn GREEN-X-POS TL-Y-POS))
                TL-BODY))

;; TrafficLight -> Image
;; auxiliary for rendering red light
(check-expect (tl-red "red") (circle 10 "solid" "red"))
(check-expect (tl-red "green") (circle 10 "outline" "red"))

(define (tl-red cs)
  (circle 10
          (if (not (string=? cs "red"))
              "outline"
              "solid")
          "red"))

;; TrafficLight -> Image
;; auxiliary function for rendering green light
(check-expect (tl-green "green") (circle 10 "solid" "green"))
(check-expect (tl-green "red") (circle 10 "outline" "green"))

(define (tl-green cs)
  (circle 10
          (if (not (string=? cs "green"))
              "outline"
              "solid")
          "green"))

;; TrafficLight -> Image
;; auxiliary function for rendering yellow light
(check-expect (tl-yellow "yellow") (circle 10 "solid" "yellow"))
(check-expect (tl-yellow "green") (circle 10 "outline" "yellow"))

(define (tl-yellow cs)
  (circle 10
          (if (not (string=? cs "yellow"))
              "outline"
              "solid")
          "yellow"))

;; TrafficLight -> TrafficLight
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
            [to-draw tl-render]
            [on-tick tl-next 1]))

(test)
(traffic-light-simulation "red")
