#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; constant and data definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CANVAS-WIDTH 200)
(define CANVAS-HEIGHT 200)
(define CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT "maroon"))

(define UFO-WIDTH (/ CANVAS-WIDTH 6))
(define UFO-HEIGHT (/ CANVAS-HEIGHT 80))
(define UFO
  (overlay (circle (/ UFO-WIDTH 4) "solid" "navy")
           (rectangle UFO-WIDTH UFO-HEIGHT "solid" "navy")))
(define UFO-HITBOX-0 (/ UFO-WIDTH 4)) ;; work in progress
(define UFO-HITBOX-1 (rectangle (image-width UFO) (image-height UFO) "solid" "transparent")) ;; work in progress
(define UFO-HIT
  (overlay (text/font "deads" 10 "yellow"
                      #f "script" "normal" "bold" #f)
           UFO))
(define UFO-VELOCITY 1)

(define EARTH (- CANVAS-HEIGHT (/ UFO-WIDTH 2)))

(define TANK-WIDTH (/ CANVAS-WIDTH 8))
(define TANK-HEIGHT (/ CANVAS-HEIGHT 20))
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" "black"))
(define TANK-Y (- CANVAS-HEIGHT (/ TANK-HEIGHT 2)))
(define TANK-LEFT "left")
(define TANK-RIGHT "right")
(define TANK-VELOCITY (* UFO-VELOCITY 0.8))

(define MISSILE-SIDE-LENGTH (/ TANK-WIDTH 3))
(define MISSILE (isosceles-triangle MISSILE-SIDE-LENGTH 30 "solid" "yellow"))
(define MISSILE-VELOCITY (* TANK-VELOCITY 2.5))

;; a UFO-Coordinate is a Posn.
;; (make-posn x y) is the UFO's location
;; (using the topdown, left-to-right convention).

;; a Tank-Coordinate is a structure:
;;  (make-tank Number Number).
;; (make-tank x dx) specifies the position:
;; (x, TANK-Y) and the tank's speed: dx pixels/tick.
(define-struct tank [x dx])

;; a Missile-Coordinate is a Posn.
;; (make-posn x y) is the missiles location.

;; an Aim is a structure:
;;  (make-aim UFO-Coordinate Tank-Coordinate)
;; (make-aim ufo-cor tank-cor) represents the
;; locations of UFO and TANK before MISSILE is fired.
(define-struct aim [ufo-cor tank-cor])

;; a Fired is a structure:
;;  (make-fired Missile-Coordinate UFO-Coordinate Tank-Coordinate)
;; (make-fired missile-cor ufo-cor tank-cor)
;; represents the locations of MISSILE, UFO, and TANK
;; after MISSILE is fired.
(define-struct fired [missile-cor ufo-cor tank-cor])

;; a SIGS is one of:
;; - (make-aim UFO-Coordinate Tank-Coordinate)
;; - (make-fired Missile-Coordinate UFO-Coordinate Tank-Coordinate)
;; represetnts the complete state of a Space Invader game.
(define sigs-ex0
  (make-aim (make-posn 20 10)
            (make-tank 28 -3)))
(define sigs-ex1
  (make-fired (make-posn 28 (- CANVAS-HEIGHT TANK-HEIGHT))
              (make-posn 20 10)
              (make-tank 28 -3)))
(define sigs-ex2
  (make-fired (make-posn 22 103)
              (make-posn 20 100)
              (make-tank 100 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; function definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SIGS -> Image
;; renders a SIGS into an image onto CANVAS according
;; to the SIGS data given:
;; - Aim
;; - Fired
(define (sigs->image s)
  (cond [(aim? s) (aim->image s)]
        [(fired? s) (fired->image s)]))

(check-expect (sigs->image sigs-ex0) (aim->image sigs-ex0))
(check-expect (sigs->image sigs-ex1) (fired->image sigs-ex1))
(check-expect (sigs->image sigs-ex2) (fired->image sigs-ex2))

;; Aim -> Image
;; renders an Aim into an image.
(define (aim->image a)
  (place-images
          [list UFO
                TANK]
          [list (make-posn (posn-x (aim-ufo-cor a))
                           (posn-y (aim-ufo-cor a)))
                (make-posn (tank-x (aim-tank-cor a))
                           TANK-Y)]
          CANVAS))

(check-expect (aim->image sigs-ex0)
              (place-images
               [list UFO
                     TANK]
               [list (make-posn (posn-x (aim-ufo-cor sigs-ex0))
                                (posn-y (aim-ufo-cor sigs-ex0)))
                     (make-posn (tank-x (aim-tank-cor sigs-ex0))
                                TANK-Y)]
               CANVAS))

;; Fired -> Image
;; renders a Fired into an image.
(define (fired->image f)
  (place-images
          [list MISSILE
                UFO
                TANK]
          [list (make-posn (posn-x (fired-missile-cor f))
                           (posn-y (fired-missile-cor f)))
                (make-posn (posn-x (fired-ufo-cor f))
                           (posn-y (fired-ufo-cor f)))
                (make-posn (tank-x (fired-tank-cor f))
                           TANK-Y)]
          CANVAS))

(check-expect (fired->image sigs-ex1)
              (place-images
               [list MISSILE
                     UFO
                     TANK]
               [list (make-posn (posn-x (fired-missile-cor sigs-ex1))
                                (posn-y (fired-missile-cor sigs-ex1)))
                     (make-posn (posn-x (fired-ufo-cor sigs-ex1))
                                (posn-y (fired-ufo-cor sigs-ex1)))
                     (make-posn (tank-x (fired-tank-cor sigs-ex1))
                                TANK-Y)]
               CANVAS))
(check-expect (fired->image sigs-ex2)
              (place-images
               [list MISSILE
                     UFO
                     TANK]
               [list (make-posn (posn-x (fired-missile-cor sigs-ex2))
                                (posn-y (fired-missile-cor sigs-ex2)))
                     (make-posn (posn-x (fired-ufo-cor sigs-ex2))
                                (posn-y (fired-ufo-cor sigs-ex2)))
                     (make-posn (tank-x (fired-tank-cor sigs-ex2))
                                TANK-Y)]
               CANVAS))

(beside (sigs->image sigs-ex0)
        (sigs->image sigs-ex1)
        (sigs->image sigs-ex2))

(test)
