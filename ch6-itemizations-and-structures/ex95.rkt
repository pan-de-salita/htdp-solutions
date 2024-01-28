#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

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
(define-struct tank [location velocity])

;; a Missile-Coordinate is a Posn.
;; (make-posn x y) is the missiles location.

(define-struct aim [ufo-cor tank-cor])
;; an Aim is a structure:
;;  (make-aim UFO-Coordinate Tank-Coordinate)
;; (make-aim ufo-cor tank-cor) represents the
;; locations of UFO and TANK before MISSILE is fired.

(define-struct fired [ufo-cor tank-cor missile-cor])
;; A Fired is a structure:
;;  (make-fired UFO-Coordinate Tank-Coordinate Missile-Coordinate)
;; (make-fired ufo-cor tank-cor missile-cor)
;; represents the locations of UFO, TANK, and MISSILE
;; after MISSILE is fired.

;; a SIGS is one of:
;; - (make-aim UFO-Coordinate Tank-Coordinate)
;; - (make-fired UFO-Coordinate Tank-Coordinate Missile-Coordinate)
;; represetnts the complete state of a Space Invader game.

(define SIGS-ex0
  (make-aim (make-posn 20 10)
            (make-tank 28 -3)))
(define SIGS-ex1
  (make-fired (make-posn 20 10)
              (make-tank 28 -3)
              (make-posn 28 (- CANVAS-HEIGHT TANK-HEIGHT))))
(define SIGS-ex2
  (make-fired (make-posn 20 100)
              (make-tank 100 3)
              (make-posn 22 103)))

;; the first example is generated according to the first clause
;; of SIGS because it assumes that MISSILE has not been fired.
;; the second and third examples, on the other hand, assume that
;; MISSILE has been fired, and are therefore generated according
;; to the second clause of SIGS.

;;; functions
