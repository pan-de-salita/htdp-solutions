;; Exercise 51. Design a big-bang program that simulates a traffic light for a given duraton.
;; The program renders the state of a traffic light as a solid circle of the appropriate color,
;; and it changes state on every clock tick. Hint: Read the documentation for big-bang; there
;; is a reason all these "words" are linked to their documentation. What is the most
;; appropriate initial state? Ask your engineering friends. 

#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require test-engine/racket-tests)

;; constant definitions
(define TRAFFIC-LIGHT-RADIUS 25)

(define GO-COLOR "green")
(define CAUTION-COLOR "yellow")
(define STOP-COLOR "red")

(define GO-START-TIME 30)
(define CAUTION-START-TIME 10)
(define STOP-START-TIME 60)

;; a WorldState is a Number.
;; interpretation: the number of clock ticks since the last color switch of the traffic light.

;; WorldState -> WorldState
;; initiates the program from some state.
(define (main ws)
  (big-bang ws
    [to-draw draw-street-light]
    [on-tick clock-tick-handler 0.5]))

;; WorldState -> Image
;; renders the traffic light with the appropriate color according to time past.
(check-expect (draw-street-light 60) (circle TRAFFIC-LIGHT-RADIUS "solid" STOP-COLOR))
(check-expect (draw-street-light 29) (circle TRAFFIC-LIGHT-RADIUS "solid" GO-COLOR))
(check-expect (draw-street-light 2) (circle TRAFFIC-LIGHT-RADIUS "solid" CAUTION-COLOR))
(define (draw-street-light ws)
  (circle TRAFFIC-LIGHT-RADIUS
          "solid"
          (street-light-color ws)))

;; WorldState -> WorldState
;; counts down from 60; works with `street-light-color` to determine appropriate street
;; light color.
(check-expect (clock-tick-handler 0) 60)
(check-expect (clock-tick-handler 1) 0)
(check-expect (clock-tick-handler 25) 24)
(define (clock-tick-handler ws)
  (cond [(= ws 0) (+ ws 60)]
        [else (- ws 1)]))

;; Number -> String
;; determines appropriate street light color according to result of `clock-tick-handler`.
(check-expect (street-light-color 1) CAUTION-COLOR)
(check-expect (street-light-color 35) STOP-COLOR)
(check-expect (street-light-color 23) GO-COLOR)
(define (street-light-color ws)
  (cond [(<= (clock-tick-handler ws) CAUTION-START-TIME) CAUTION-COLOR]
        [(<= (clock-tick-handler ws) GO-START-TIME) GO-COLOR]
        [else STOP-COLOR]))

(test)
(main 60)

;; why didn't i use this?
;; a TrafficLight is one of the following Strings:
;; - "red"
;; - "green"
;; - "yellow"
;; interpretation: the three strings represent the three
;; possible states that a traffic light may assume.
