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
(define UFO
  (overlay (circle (/ UFO-WIDTH 4) "solid" "navy")
           (rectangle UFO-WIDTH (/ CANVAS-HEIGHT 80) "solid" "navy")))
(define UFO-VELOCITY 2)
(define UFO-INIT-X (/ CANVAS-WIDTH 2))
(define UFO-INIT-Y (/ (image-height UFO) 2))
(define UFO-X-DELTA 2)
(define UFO-X-LIMIT-LEFT (/ UFO-WIDTH 2))
(define UFO-X-LIMIT-RIGHT (- CANVAS-WIDTH (/ UFO-WIDTH 2)))
(define UFO-WARP 4)

(define EARTH (- CANVAS-HEIGHT (/ (image-height UFO) 2)))

(define TANK-WIDTH (/ CANVAS-WIDTH 8))
(define TANK-HEIGHT (/ CANVAS-HEIGHT 20))
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" "black"))
(define TANK-VELOCITY-RIGHT 4)
(define TANK-VELOCITY-LEFT -4)
(define TANK-INIT-X (/ TANK-WIDTH 2))
(define TANK-Y (- CANVAS-HEIGHT (/ TANK-HEIGHT 2)))
(define TANK-X-LIMIT-LEFT (/ TANK-WIDTH 2))
(define TANK-X-LIMIT-RIGHT (- CANVAS-WIDTH (/ TANK-WIDTH 2)))

(define MISSILE-SIDE-LENGTH (/ TANK-WIDTH 3))
(define MISSILE (isosceles-triangle MISSILE-SIDE-LENGTH 30 "solid" "yellow"))
(define MISSILE-VELOCITY (* TANK-VELOCITY-RIGHT 4))
(define MISSILE-INIT-Y (- CANVAS-HEIGHT (image-height MISSILE) 10))

(define HIT-DISTANCE 20)

(define USER-WON (text "you win." 20 "black"))
(define USER-LOST (text "you lose." 20 "black"))

;; a UFOCoordinate is a Posn.
;; (make-posn x y) is the UFO's location
;; (using the topdown, left-to-right convention).

;; a TankCoordinate is a structure:
;;  (make-tank Number Number).
;; (make-tank x dx) specifies the position:
;; (x, TANK-Y) and the tank's speed: dx pixels/tick.
(define-struct tank [x dx])

;; a MissileOrNot is one of:
;; - #f
;; - Posn
;; #f means the missile is in the tank; Posn says
;; the missile is at that location.

;; a SIGS is:
;;  (make-sigs UFOCoordinate TankCoordinate MissileOrNot)
;; represents the complete state of a Space Invader game.
(define-struct sigs [ufo tank missile/#f])

(define sigs-ex0
  (make-sigs (make-posn 20 10)
             (make-tank 28 TANK-VELOCITY-RIGHT)
             #f))
(define sigs-ex1
  (make-sigs (make-posn 20 10)
             (make-tank 32 TANK-VELOCITY-RIGHT)
             (make-posn 32 MISSILE-INIT-Y)))
(define sigs-ex2
  (make-sigs (make-posn 20 100)
             (make-tank 100 TANK-VELOCITY-RIGHT)
             (make-posn 22 103)))
(define sigs-ex3
  (make-sigs (make-posn 20 EARTH)
             (make-tank 100 3)
             (make-posn 150 45)))


(define GAME-INIT-STATE
  (make-sigs (make-posn UFO-INIT-X UFO-INIT-Y)
             (make-tank TANK-INIT-X TANK-VELOCITY-RIGHT)
             #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; function definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SIGS -> image
;; renders the state of the game onto CANVAS.
(check-expect (si-render sigs-ex0)
              (place-images
               (list UFO TANK MISSILE)
               (list (sigs-ufo sigs-ex0)
                     (make-posn (tank-x (sigs-tank sigs-ex0)) TANK-Y)
                     (make-posn (tank-x (sigs-tank sigs-ex0)) TANK-Y))
               CANVAS))
(check-expect (si-render sigs-ex1)
              (place-images
               (list UFO TANK MISSILE)
               (list (sigs-ufo sigs-ex1)
                     (make-posn (tank-x (sigs-tank sigs-ex1)) TANK-Y)
                     (sigs-missile/#f sigs-ex1))
               CANVAS))

(define (si-render s)
  (place-images
   (list UFO TANK MISSILE)
   (list (sigs-ufo s)
         (make-posn (tank-x (sigs-tank s)) TANK-Y)
         (missile-placement s))
   CANVAS))

;; SIGS -> Posn
;; determines coordinates of MISSILE. if:
;; - MissileOrNot is #f -- MISSILE is not launched and stays hidden behind TANK.
;; - MissileOrNot is a Posn -- MISSILE is launched and becomes visible.
(check-expect (missile-placement sigs-ex0)
              (make-posn
               (tank-x (sigs-tank sigs-ex0))
               TANK-Y))
(check-expect (missile-placement sigs-ex1)
              (sigs-missile/#f sigs-ex1))

(define (missile-placement s)
  (cond
    [(boolean? (sigs-missile/#f s))
     (make-posn (tank-x (sigs-tank s)) TANK-Y)]
    [(posn? (sigs-missile/#f s))
     (sigs-missile/#f s)]))

(beside (si-render sigs-ex0)
        (si-render sigs-ex1)
        (si-render sigs-ex2)
        (si-render sigs-ex3))
