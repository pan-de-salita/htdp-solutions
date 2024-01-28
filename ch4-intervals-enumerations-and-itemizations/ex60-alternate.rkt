#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

;; a TrafficLight is one of:
;; - [0,2], and 9 | interpretation: the traffic light shows red
;; - [3,6] | interpretation: the traffic light shows green
;; - [7,8] | interpretation: the traffic light shows yellow

(define-values
  (TRAFFIC-LIGHT-BODY-WIDTH
   TRAFFIC-LIGHT-BODY-HEIGHT)
  (values 30
          90))

(define TRAFFIC-LIGHT-BODY (empty-scene TRAFFIC-LIGHT-BODY-WIDTH
                             TRAFFIC-LIGHT-BODY-HEIGHT
                             "black"))

(define LIGHT-RADIUS 10)

(define-values
  (RED-LIGHT
   GREEN-LIGHT
   YELLOW-LIGHT
   RESET)
  (values 0
          3
          7
          9))

(define-values
  (RED-Y-POS
   YELLOW-Y-POS
   GREEN-Y-POS
   TRAFFIC-LIGHT-X-POS)
  (values (- (* 1/3 TRAFFIC-LIGHT-BODY-HEIGHT) LIGHT-RADIUS)
          (* 1/2 TRAFFIC-LIGHT-BODY-HEIGHT)
          (- TRAFFIC-LIGHT-BODY-HEIGHT (* LIGHT-RADIUS 2))
          (* 1/2 TRAFFIC-LIGHT-BODY-WIDTH)))

;; TrafficLight -> TrafficLight
;; yields the next state, given current state 
(check-expect (traffic-light-next-state 0) 1)
(check-expect (traffic-light-next-state 7) 8)
(check-expect (traffic-light-next-state 9) 0)

(define (traffic-light-next-state current-state)
  (if (>= current-state RESET)
      RED-LIGHT
      (add1 current-state)))

;; TrafficLight -> Image
;; renders the current state  as a traffic light image
(check-expect (traffic-light-render 0)
              (place-images (list (bulb-render-red 0)
                                  (bulb-render-green 0)
                                  (bulb-render-yellow 0))
                            (list (make-posn TRAFFIC-LIGHT-X-POS RED-Y-POS)
                                  (make-posn TRAFFIC-LIGHT-X-POS GREEN-Y-POS)
                                  (make-posn TRAFFIC-LIGHT-X-POS YELLOW-Y-POS))
                            TRAFFIC-LIGHT-BODY))
(check-expect (traffic-light-render 6)
              (place-images (list (bulb-render-red 6)
                                  (bulb-render-green 6)
                                  (bulb-render-yellow 6))
                            (list (make-posn TRAFFIC-LIGHT-X-POS RED-Y-POS)
                                  (make-posn TRAFFIC-LIGHT-X-POS GREEN-Y-POS)
                                  (make-posn TRAFFIC-LIGHT-X-POS YELLOW-Y-POS))
                            TRAFFIC-LIGHT-BODY))
(check-expect (traffic-light-render 9)
              (place-images (list (bulb-render-red 9)
                                  (bulb-render-green 9)
                                  (bulb-render-yellow 9))
                            (list (make-posn TRAFFIC-LIGHT-X-POS RED-Y-POS)
                                  (make-posn TRAFFIC-LIGHT-X-POS GREEN-Y-POS)
                                  (make-posn TRAFFIC-LIGHT-X-POS YELLOW-Y-POS))
                            TRAFFIC-LIGHT-BODY))

(define (traffic-light-render current-state)
  (place-images (list (bulb-render-red current-state)
                      (bulb-render-green current-state)
                      (bulb-render-yellow current-state ))
                (list (make-posn TRAFFIC-LIGHT-X-POS RED-Y-POS)
                      (make-posn TRAFFIC-LIGHT-X-POS GREEN-Y-POS)
                      (make-posn TRAFFIC-LIGHT-X-POS YELLOW-Y-POS))
                TRAFFIC-LIGHT-BODY))

;; TrafficLight -> Image
;; auxiliary function for rendering red traffic light
(check-expect (bulb-render-red 0) (circle LIGHT-RADIUS "solid" "red"))
(check-expect (bulb-render-red 5) (circle LIGHT-RADIUS "outline" "red"))

(define (bulb-render-red current-state)
  (circle LIGHT-RADIUS
          (if (or (and (>= current-state RED-LIGHT)
                       (< current-state GREEN-LIGHT))
                  (= current-state RESET))
              "solid"
              "outline")
          "red"))

;; TrafficLight -> Image
;; auxiliary function for rendering green traffic light
(check-expect (bulb-render-green 4) (circle LIGHT-RADIUS "solid" "green"))
(check-expect (bulb-render-green 8) (circle LIGHT-RADIUS "outline" "green"))

(define (bulb-render-green current-state)
  (circle LIGHT-RADIUS
          (if (and (>= current-state GREEN-LIGHT)
                   (< current-state YELLOW-LIGHT))
              "solid"
              "outline")
          "green"))

;; TrafficLight -> Image
;; auxiliary function for rendering yellow traffic light
(check-expect (bulb-render-yellow 8) (circle LIGHT-RADIUS "solid" "yellow"))
(check-expect (bulb-render-yellow 0) (circle LIGHT-RADIUS "outline" "yellow"))

(define (bulb-render-yellow current-state)
  (circle LIGHT-RADIUS
          (if (and (>= current-state YELLOW-LIGHT)
                   (< current-state RESET))
              "solid"
              "outline")
          "yellow"))

;; TrafficLight -> TrafficLight
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
            [to-draw traffic-light-render]
            [on-tick traffic-light-next-state 1]))

(test)
(traffic-light-simulation RESET)
