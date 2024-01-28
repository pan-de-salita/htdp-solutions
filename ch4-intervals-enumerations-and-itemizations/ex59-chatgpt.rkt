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

(define-values
  (TL-BODY-WIDTH
   TL-BODY-HEIGHT)
  (values 90
          30))

(define TL-BODY (empty-scene TL-BODY-WIDTH TL-BODY-HEIGHT "black"))

(define LIGHT-RADIUS 10)

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
(check-expect (traffic-light-next-state "red") "green")
(check-expect (traffic-light-next-state "yellow") "red")
(check-expect (traffic-light-next-state "green") "yellow")

(define (traffic-light-next-state current-state)
  (cond [(string=? current-state "red") "green"]
        [(string=? current-state "yellow") "red"]
        [(string=? current-state "green") "yellow"]))

;; TrafficLight -> Image
;; renders the current state  as a traffic light image
(check-expect (traffic-light-render "red")
              (place-images (list (bulb-render "red" "red")
                                  (bulb-render "red" "yellow")
                                  (bulb-render "red" "green"))
                            (list (make-posn RED-X-POS TL-Y-POS)
                                  (make-posn YELLOW-X-POS TL-Y-POS)
                                  (make-posn GREEN-X-POS TL-Y-POS))
                            TL-BODY))
(check-expect (traffic-light-render "yellow")
              (place-images (list (bulb-render "yellow" "red")
                                  (bulb-render "yellow" "yellow")
                                  (bulb-render "yellow" "green"))
                            (list (make-posn RED-X-POS TL-Y-POS)
                                  (make-posn YELLOW-X-POS TL-Y-POS)
                                  (make-posn GREEN-X-POS TL-Y-POS))
                            TL-BODY))
(check-expect (traffic-light-render "green")
              (place-images (list (bulb-render "green" "red")
                                  (bulb-render "green" "yellow")
                                  (bulb-render "green" "green"))
                            (list (make-posn RED-X-POS TL-Y-POS)
                                  (make-posn YELLOW-X-POS TL-Y-POS)
                                  (make-posn GREEN-X-POS TL-Y-POS))
                            TL-BODY))

(define (traffic-light-render current-state)
  (place-images (list (bulb-render current-state "red")
                      (bulb-render current-state "yellow")
                      (bulb-render current-state "green"))
                (list (make-posn RED-X-POS TL-Y-POS)
                      (make-posn YELLOW-X-POS TL-Y-POS)
                      (make-posn GREEN-X-POS TL-Y-POS))
                TL-BODY))

;; TrafficLight -> Image
;; auxiliary function for rendering individual traffic light bulbs
(check-expect (bulb-render "red" "red") (circle 10 "solid" "red"))
(check-expect (bulb-render "green" "red") (circle 10 "outline" "red"))

(define (bulb-render current-state color)
  (circle 10
          (if (not (string=? current-state color))
              "outline"
              "solid")
          color))

;; TrafficLight -> TrafficLight
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
            [to-draw traffic-light-render]
            [on-tick traffic-light-next-state 1]))

(test)
(traffic-light-simulation "red")
