#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

;; an N-TrafficLight is one of:
;; - 0 | interpretation: the traffic light shows red
;; - 1 | interpretation: the traffic light shows green
;; - 2 | interpretation: the traffic light shows yellow

(define-values
  (TL-BODY-WIDTH
   TL-BODY-HEIGHT)
  (values 90
          30))

(define TL-BODY (empty-scene TL-BODY-WIDTH TL-BODY-HEIGHT "black"))

(define LIGHT-RADIUS 10)

(define-values
  (RED-LIGHT
   GREEN-LIGHT
   YELLOW-LIGHT)
  (values 0
          1
          2))

(define-values
  (RED-X-POS
   YELLOW-X-POS
   GREEN-X-POS
   TL-Y-POS)
  (values (- (* 1/3 TL-BODY-WIDTH) LIGHT-RADIUS)
          (* 1/2 TL-BODY-WIDTH)
          (- TL-BODY-WIDTH (* LIGHT-RADIUS 2))
          (* 1/2 TL-BODY-HEIGHT)))

;; TrafficLight -> TrafficLight
;; yields the next state, given current state 
(check-expect (traffic-light-next-state-numeric 0) 1)
(check-expect (traffic-light-next-state-numeric 1) 2)
(check-expect (traffic-light-next-state-numeric 2) 0)

(define (traffic-light-next-state-numeric current-state)
  (if (not (<= current-state 2))
      0
      (modulo (+ current-state 1) 3)))

;; TrafficLight -> Image
;; renders the current state  as a traffic light image
(check-expect (traffic-light-render 0)
              (place-images (list (bulb-render 0 RED-LIGHT)
                                  (bulb-render 0 GREEN-LIGHT)
                                  (bulb-render 0 YELLOW-LIGHT))
                            (list (make-posn RED-X-POS TL-Y-POS)
                                  (make-posn GREEN-X-POS TL-Y-POS)
                                  (make-posn YELLOW-X-POS TL-Y-POS))
                            TL-BODY))
(check-expect (traffic-light-render 1)
              (place-images (list (bulb-render 1 RED-LIGHT)
                                  (bulb-render 1 GREEN-LIGHT)
                                  (bulb-render 1 YELLOW-LIGHT))
                            (list (make-posn RED-X-POS TL-Y-POS)
                                  (make-posn GREEN-X-POS TL-Y-POS)
                                  (make-posn YELLOW-X-POS TL-Y-POS))
                            TL-BODY))
(check-expect (traffic-light-render 2)
              (place-images (list (bulb-render 2 RED-LIGHT)
                                  (bulb-render 2 GREEN-LIGHT)
                                  (bulb-render 2 YELLOW-LIGHT))
                            (list (make-posn RED-X-POS TL-Y-POS)
                                  (make-posn GREEN-X-POS TL-Y-POS)
                                  (make-posn YELLOW-X-POS TL-Y-POS))
                            TL-BODY))

(define (traffic-light-render current-state)
  (place-images (list (bulb-render current-state RED-LIGHT)
                      (bulb-render current-state GREEN-LIGHT)
                      (bulb-render current-state YELLOW-LIGHT))
                (list (make-posn RED-X-POS TL-Y-POS)
                      (make-posn GREEN-X-POS TL-Y-POS)
                      (make-posn YELLOW-X-POS TL-Y-POS))
                TL-BODY))

;; TrafficLight -> Image
;; auxiliary function for rendering individual traffic light bulbs
(check-expect (bulb-render 0 RED-LIGHT) (circle 10 "solid" "red"))
(check-expect (bulb-render 1 RED-LIGHT) (circle 10 "outline" "red"))

(define (bulb-render current-state color)
  (circle 10
          (if (not (= current-state color))
              "outline"
              "solid")
          (cond [(= color 0) "red"]
                [(= color 1) "green"]
                [(= color 2) "yellow"])))

;; TrafficLight -> TrafficLight
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
            [to-draw traffic-light-render]
            [on-tick traffic-light-next-state-numeric 1]))

(test)
(traffic-light-simulation RED-LIGHT)
