#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

;;; data definitions

;; a TrafficLightTimer is a PositiveNumber ranging from:
;; - 20 | represents a traffic light's default state; shows red
;; - [10,19] | represents the seconds at which a traffic light shows green
;; - [0,9] | represents the seconds at which a traffic light shows yellow
;; - 01 | represents a traffic light's suspended state; shows red
(define RED-LIGHT-START 20)
(define YELLOW-LIGHT-START 9)
(define GREEN-LIGHT-START 19)
(define SUSPEND -1)

;; TrafficLight is a structure:
;;      (make-traffic-light TrafficLightTimer)
;; (make-traffic-light tlt) represents the state of a traffic light.
(define-struct traffic-light [timer])

(define traffic-light-test-0 (make-traffic-light RED-LIGHT-START))
(define traffic-light-test-1 (make-traffic-light 12))
(define traffic-light-test-2 (make-traffic-light 6))
(define traffic-light-test-3 (make-traffic-light 3))

;;; constants

(define TRAFFIC-LIGHT-WIDTH 90) ;; originally 90
(define TRAFFIC-LIGHT-HEIGHT (* TRAFFIC-LIGHT-WIDTH 3))
(define TRAFFIC-LIGHT
  (empty-scene TRAFFIC-LIGHT-WIDTH
               TRAFFIC-LIGHT-HEIGHT
               "black"))

(define BULB-RADIUS (/ TRAFFIC-LIGHT-WIDTH 3))
(define RED-BULB-GRAPHIC (bitmap "./pedestrian_traffic_light_red.png"))
(define RED-BULB-OFF
  (overlay (circle BULB-RADIUS "outline" "red")
           (circle BULB-RADIUS "solid" "black")))
(define RED-BULB-ON
  (underlay RED-BULB-GRAPHIC
            (circle BULB-RADIUS "outline" "red")))
(define YELLOW-BULB-OFF
  (overlay (circle BULB-RADIUS "outline" "yellow")
           (circle BULB-RADIUS "solid" "black")))
(define YELLOW-BULB-DISPLAY-FONT 50)
(define YELLOW-BULB-DISPLAY-COLOR-0 "GREEN")
(define YELLOW-BULB-DISPLAY-COLOR-1 "ORANGE")
(define GREEN-BULB-GRAPHIC (bitmap "./pedestrian_traffic_light_green.png"))
(define GREEN-BULB-OFF
  (overlay (circle BULB-RADIUS "outline" "green")
           (circle BULB-RADIUS "solid" "black")))
(define GREEN-BULB-ON
  (underlay GREEN-BULB-GRAPHIC
            (circle BULB-RADIUS "outline" "green")))
(define BULB-X-POSN (* 1/2 TRAFFIC-LIGHT-WIDTH))
(define RED-BULB-POSN (make-posn BULB-X-POSN (- (* 1/3 TRAFFIC-LIGHT-HEIGHT) BULB-RADIUS)))
(define YELLOW-BULB-POSN (make-posn BULB-X-POSN (* 1/2 TRAFFIC-LIGHT-HEIGHT)))
(define GREEN-BULB-POSN (make-posn BULB-X-POSN (- TRAFFIC-LIGHT-HEIGHT (* BULB-RADIUS 2))))

(define ACTIVATE " ")
(define TRAFFIC-LIGHT-INITIAL-STATE (make-traffic-light RED-LIGHT-START))

;;; functions

;; TrafficLight -> Image
;; renders TrafficLight into an image.
(define (traffic-light-render traffic-light)
  (place-images
   (list (display-of-red (traffic-light-timer traffic-light))
         (display-of-yellow (traffic-light-timer traffic-light))
         (display-of-green (traffic-light-timer traffic-light)))
   (list RED-BULB-POSN
         YELLOW-BULB-POSN
         GREEN-BULB-POSN)
   TRAFFIC-LIGHT))

;; TrafficLightSignal TrafficLightTimer -> Image
;; renders a red traffic light.
(check-expect (display-of-red (traffic-light-timer traffic-light-test-0)) RED-BULB-ON)
(check-expect (display-of-red (traffic-light-timer traffic-light-test-1)) RED-BULB-OFF)
(check-expect (display-of-red (traffic-light-timer traffic-light-test-2)) RED-BULB-OFF)
(check-expect (display-of-red (traffic-light-timer traffic-light-test-3)) RED-BULB-OFF)

(define (display-of-red timer)
  (if (stop/suspend? timer)
      RED-BULB-ON
      RED-BULB-OFF))

;; TrafficLightTimer -> Boolean
;; checkes if a traffic light should show red
;; (in its default or suspended state).
(check-expect (stop/suspend? (traffic-light-timer traffic-light-test-0)) #t)
(check-expect (stop/suspend? (traffic-light-timer traffic-light-test-1)) #f)
(check-expect (stop/suspend? (traffic-light-timer traffic-light-test-2)) #f)
(check-expect (stop/suspend? (traffic-light-timer traffic-light-test-3)) #f)

(define (stop/suspend? timer)
  (or (= timer RED-LIGHT-START)
      (= timer SUSPEND)))

;; TrafficLightSignal TrafficLightTimer -> Image
;; renders a yellow traffic light.
(check-expect (display-of-yellow (traffic-light-timer traffic-light-test-0)) YELLOW-BULB-OFF)
(check-expect (display-of-yellow (traffic-light-timer traffic-light-test-1)) YELLOW-BULB-OFF)
(check-expect (display-of-yellow (traffic-light-timer traffic-light-test-2))
              (display-yellow-countdown (traffic-light-timer traffic-light-test-2)))
(check-expect (display-of-yellow (traffic-light-timer traffic-light-test-3))
              (display-yellow-countdown (traffic-light-timer traffic-light-test-3)))

(define (display-of-yellow timer)
  (if (wait? timer)
      (display-yellow-countdown timer)
      YELLOW-BULB-OFF))

;; TrafficLightTimer -> Boolean
;; checks if a traffic light should show yellow.
(check-expect (wait? (traffic-light-timer traffic-light-test-0)) #f)
(check-expect (wait? (traffic-light-timer traffic-light-test-1)) #f)
(check-expect (wait? (traffic-light-timer traffic-light-test-2)) #t)
(check-expect (wait? (traffic-light-timer traffic-light-test-3)) #t)

(define (wait? timer)
  (and (<= timer YELLOW-LIGHT-START)
       (< SUSPEND timer)))

;; TrafficLightTimer -> Image
;; displays countdown of yellow bulb.
(check-expect (display-yellow-countdown (traffic-light-timer traffic-light-test-2))
              (overlay/offset
               (text "6" YELLOW-BULB-DISPLAY-FONT
                     (yellow-display-color (traffic-light-timer traffic-light-test-2)))
               0 -5
               YELLOW-BULB-OFF))
(check-expect (display-yellow-countdown (traffic-light-timer traffic-light-test-3))
              (overlay/offset
               (text "3" YELLOW-BULB-DISPLAY-FONT
                     (yellow-display-color (traffic-light-timer traffic-light-test-3)))
               0 -5
               YELLOW-BULB-OFF))

(define (display-yellow-countdown timer)
  (overlay/offset
   (text (number->string timer)
         YELLOW-BULB-DISPLAY-FONT
         (yellow-display-color timer))
   0 -5
   YELLOW-BULB-OFF))

;; TrafficLightTimer -> String
;; returns the right color for the yellow light display text.
(check-expect (yellow-display-color (traffic-light-timer traffic-light-test-2)) YELLOW-BULB-DISPLAY-COLOR-0)
(check-expect (yellow-display-color (traffic-light-timer traffic-light-test-3)) YELLOW-BULB-DISPLAY-COLOR-1)

(define (yellow-display-color timer)
  (if (= (modulo timer 2) 0)
      YELLOW-BULB-DISPLAY-COLOR-0
      YELLOW-BULB-DISPLAY-COLOR-1))

;; TrafficLightSignal TrafficLightTimer -> Image
;; renders a green traffic light.
(check-expect (display-of-green (traffic-light-timer traffic-light-test-0)) GREEN-BULB-OFF)
(check-expect (display-of-green (traffic-light-timer traffic-light-test-1)) GREEN-BULB-ON)
(check-expect (display-of-green (traffic-light-timer traffic-light-test-2)) GREEN-BULB-OFF)
(check-expect (display-of-green (traffic-light-timer traffic-light-test-3)) GREEN-BULB-OFF)

(define (display-of-green timer)
  (if (proceed? timer)
      GREEN-BULB-ON
      GREEN-BULB-OFF))

;; TrafficLightTimer -> Boolean
;; checks if a traffic light should show green.
(check-expect (proceed? (traffic-light-timer traffic-light-test-0)) #f)
(check-expect (proceed? (traffic-light-timer traffic-light-test-1)) #t)
(check-expect (proceed? (traffic-light-timer traffic-light-test-2)) #f)
(check-expect (proceed? (traffic-light-timer traffic-light-test-3)) #f)

(define (proceed? timer)
  (and (<= timer GREEN-LIGHT-START)
       (< YELLOW-LIGHT-START timer)))

;; TrafficLight -> TrafficLight
;; changes the state of the program per clock tick.
(check-expect (traffic-light-countdown traffic-light-test-0) traffic-light-test-0)
(check-expect (traffic-light-countdown traffic-light-test-1) (make-traffic-light (sub1 12)))
(check-expect (traffic-light-countdown traffic-light-test-2) (make-traffic-light (sub1 6)))
(check-expect (traffic-light-countdown traffic-light-test-3) (make-traffic-light (sub1 3)))

(define (traffic-light-countdown traffic-light)
  (make-traffic-light
   (if (stop/suspend? (traffic-light-timer traffic-light))
       RED-LIGHT-START
       (sub1 (traffic-light-timer traffic-light)))))

;; TrafficLight KeyEvent -> TrafficLight
;; modifies the state of the program depending on KeyEvent:
;; - ACTIVATE | traffic light switches from default state
;;   to showing green
(check-expect (traffic-light-activate traffic-light-test-0 " ") (make-traffic-light GREEN-LIGHT-START))
(check-expect (traffic-light-activate traffic-light-test-0 "f") traffic-light-test-0)
(check-expect (traffic-light-activate traffic-light-test-1 " ") traffic-light-test-1)
(check-expect (traffic-light-activate traffic-light-test-1 "f") traffic-light-test-1)
(check-expect (traffic-light-activate traffic-light-test-2 " ") traffic-light-test-2)
(check-expect (traffic-light-activate traffic-light-test-2 "f") traffic-light-test-2)
(check-expect (traffic-light-activate traffic-light-test-3 " ") traffic-light-test-3)
(check-expect (traffic-light-activate traffic-light-test-3 "f") traffic-light-test-3)

(define (traffic-light-activate traffic-light key-event)
  (make-traffic-light
   (if (and (stop/suspend? (traffic-light-timer traffic-light))
            (key=? key-event ACTIVATE))
       GREEN-LIGHT-START
       RED-LIGHT-START)))

;; TrafficLight -> TrafficLight
;; main function.
(define (pedestrian-traffic-light traffic-light)
  (big-bang traffic-light
            [to-draw traffic-light-render]
            [on-tick traffic-light-countdown 1]
            [on-key traffic-light-activate]))

(test)

;;; application

;; (pedestrian-traffic-light TRAFFIC-LIGHT-INITIAL-STATE)
