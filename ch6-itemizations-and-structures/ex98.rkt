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
(define UFO-HIT
  (underlay UFO
            (text/font "deads" 10 "yellow"
                       #f "script" "normal" "bold" #f)))
(define UFO-VELOCITY 1)

(define EARTH (- CANVAS-HEIGHT (/ (image-height UFO) 2)))

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

(define USER-WON (text "you won." 40 "black"))
(define USER-LOST (text "you lost." 40 "black"))

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
;;  (make-fired UFO-Coordinate Tank-Coordinate Missile-Coordinate)
;; (make-fired  ufo-cor tank-cor missile-cor)
;; represents the locations of UFO, TANK, and MISSILE
;; after MISSILE is fired.
(define-struct fired [ufo-cor tank-cor missile-cor])

;; a SIGS is one of:
;; - (make-aim UFO-Coordinate Tank-Coordinate)
;; - (make-fired UFO-Coordinate Tank-Coordinate Missile-Coordinate)
;; represetnts the complete state of a Space Invader game.
(define sigs-ex0
  (make-aim (make-posn 20 10)
            (make-tank 28 -3)))
(define sigs-ex1
  (make-fired (make-posn 20 10)
              (make-tank 28 -3)
              (make-posn 28 (- CANVAS-HEIGHT TANK-HEIGHT))))
(define sigs-ex2
  (make-fired (make-posn 20 100)
              (make-tank 100 3)
              (make-posn 22 103)))
(define sigs-ex3
  (make-aim (make-posn 20 EARTH)
              (make-tank 100 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; function definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SIGS -> Image
;; renders a SIGS into an image onto CANVAS according
;; to the SIGS data given:
;; - Aim
;; - Fired
(define (si-render s)
  (cond [(aim? s) (tank-render (aim-tank-cor s)
                               (ufo-render (aim-ufo-cor s) CANVAS))]
        [(fired? s) (tank-render (fired-tank-cor s)
                                 (ufo-render (fired-ufo-cor s)
                                             (missile-render (fired-missile-cor s)
                                                             CANVAS)))]))

(check-expect (si-render sigs-ex0)
              (tank-render (aim-tank-cor sigs-ex0)
                           (ufo-render (aim-ufo-cor sigs-ex0) CANVAS)))
(check-expect (si-render sigs-ex1)
              (tank-render (fired-tank-cor sigs-ex1)
                           (ufo-render (fired-ufo-cor sigs-ex1)
                                       (missile-render (fired-missile-cor sigs-ex1)
                                                       CANVAS))))
(check-expect (si-render sigs-ex2)
              (tank-render (fired-tank-cor sigs-ex2)
                           (ufo-render (fired-ufo-cor sigs-ex2)
                                       (missile-render (fired-missile-cor sigs-ex2)
                                                       CANVAS))))

;; SIGS -> Image
;; renders final state of the program into an image
;; onto CANVAS
(define (si-render-final s)
  (cond [(aim? s) (overlay USER-LOST
                           (tank-render (aim-tank-cor s)
                                        (ufo-render (aim-ufo-cor s) CANVAS)))]
        [(fired? s) (overlay USER-WON
                             (tank-render (fired-tank-cor s)
                                          (ufo-render (fired-ufo-cor s)
                                                      (missile-render (fired-missile-cor s)
                                                                      CANVAS))))]))

(check-expect (si-render-final sigs-ex3)
              (overlay USER-LOST
                       (tank-render (aim-tank-cor sigs-ex3)
                                    (ufo-render (aim-ufo-cor sigs-ex3) CANVAS))))
(check-expect (si-render-final sigs-ex2)
              (overlay USER-WON
                       (tank-render (fired-tank-cor sigs-ex2)
                                    (ufo-render (fired-ufo-cor sigs-ex2)
                                                (missile-render (fired-missile-cor sigs-ex2)
                                                                CANVAS)))))

;; Tank-Coordinate Image -> Image
;; renders a Tank-Coordinate into an image onto given Image.
(define (tank-render tc im)
  (place-image TANK (tank-x tc) TANK-Y im))

(check-expect (tank-render (aim-tank-cor sigs-ex0)
                           (ufo-render (aim-ufo-cor sigs-ex0) CANVAS))
              (place-image TANK (tank-x (aim-tank-cor sigs-ex0)) TANK-Y
                           (ufo-render (aim-ufo-cor sigs-ex0) CANVAS)))

;; UFO-Coordinate Image -> Image
;; renders a UFO-Coordinate into an image onto given Image.
(define (ufo-render uc im)
  (place-image UFO (posn-x uc) (posn-y uc) im))

(check-expect (ufo-render (aim-ufo-cor sigs-ex0) CANVAS)
              (place-image UFO
                           (posn-x (aim-ufo-cor sigs-ex0))
                           (posn-y (aim-ufo-cor sigs-ex0))
                           CANVAS))
(check-expect (ufo-render (fired-ufo-cor sigs-ex1)
                          (missile-render (fired-missile-cor sigs-ex1)
                                          CANVAS))
              (place-image UFO
                           (posn-x (fired-ufo-cor sigs-ex1))
                           (posn-y (fired-ufo-cor sigs-ex1))
                           (missile-render (fired-missile-cor sigs-ex1)
                                           CANVAS)))

;; Missile-Coordinate Image -> Image
;; renders a Missile-Coordinate into an image onto given Image.
(define (missile-render mc im)
  (place-image MISSILE (posn-x mc) (posn-y mc) im))

(check-expect (missile-render (fired-missile-cor sigs-ex2) CANVAS)
              (place-image MISSILE
                           (posn-x (fired-missile-cor sigs-ex2))
                           (posn-y (fired-missile-cor sigs-ex2))
                           CANVAS))

;; FIRED -> Boolean
;; stops the game if any of the following is true:
;; - UFO lands
;; - MISSILE hits UFO
(check-expect (game-over? sigs-ex0) #f)
(check-expect (game-over? sigs-ex1) #f)
(check-expect (game-over? sigs-ex2) #t)
(check-expect (game-over? (make-aim (make-posn 200 EARTH) (make-tank 50 3))) #t)

(define (game-over? s)
  (cond [(aim? s) (= (posn-y (aim-ufo-cor s)) EARTH)]
        [(fired? s) (ufo-hit? (fired-ufo-cor s)
                              (fired-missile-cor s))]))

;; UFO-Coordinate Missile-Coordinate -> Boolean
;; checks if the tip of MISSILE has hit UFO. conditions
;; for hit:
;; - x-coordinate of MISSILE falls within width of UFO
;; - tip of MISSILE falls within height of UFO
(check-expect (ufo-hit? (fired-ufo-cor sigs-ex2) (fired-missile-cor sigs-ex2)) #t)
(check-expect (ufo-hit? (fired-ufo-cor (make-fired
                                        (make-posn 20 100)
                                        (make-tank 100 3)
                                        (make-posn 40 143)))
                        (fired-missile-cor (make-fired
                                            (make-posn 20 100)
                                            (make-tank 100 3)
                                            (make-posn 40 143)))) #f)

(define (ufo-hit? uc mc)
  (and (<= (- (posn-x uc) (/ (image-width UFO) 2))
           (posn-x mc)
           (+ (posn-x uc) (/ (image-width UFO) 2)))
       (<= (- (posn-y uc) (/ (image-height UFO) 2))
           (- (posn-y mc) (/ (image-height MISSILE) 2))
           (+ (posn-y uc) (/ (image-height UFO) 2)))))

(beside (si-render sigs-ex0)
        (si-render sigs-ex1)
        (si-render sigs-ex2)
        (si-render sigs-ex3))

(test)
