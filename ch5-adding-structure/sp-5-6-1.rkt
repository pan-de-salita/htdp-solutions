#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(define SCENE-WIDTH 200)
(define SCENE-HEIGHT 100)
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT "black"))

(define UFO-WIDTH (/ SCENE-WIDTH 20))
(define UFO-HEIGHT UFO-WIDTH)
(define UFO (rectangle UFO-WIDTH UFO-HEIGHT "solid" "yellow"))

(define-struct ufo-move [position velocity])
;; a UFO-Move is a structure:
;;  (make-ufo-move Posn Vel)
;; interpretation: indicates the positon (x- and y- coordinates) of UFO
;; and its velocity

(define-struct velocity [delta-x delta-y])
;; a Velocity is a structure:
;;  (make-velocity Number Number)
;; interpretation: shows the change in x- and y-coordinates of UFO

;; test cases
(define posn-1 (make-posn 22 80))
(define posn-2 (make-posn 30 77))

(define velocity-1 (make-velocity 8 -3))
(define velocity-2 (make-velocity -5 -3))

(define ufo-1 (make-ufo-move posn-1 velocity-1))
(define ufo-2 (make-ufo-move posn-1 velocity-2))
(define ufo-3 (make-ufo-move posn-2 velocity-1))
(define ufo-4 (make-ufo-move posn-2 velocity-2))

;; UFO-Movement -> Image
;; renders the image at a given location
(define (ufo-render ufo-movement)
  (place-image UFO
               (posn-x (ufo-move-position ufo-movement))
               (posn-y (ufo-move-position ufo-movement))
               SCENE))

(check-expect (ufo-render ufo-2) (place-image UFO (posn-x posn-1) (posn-y posn-1) SCENE))
(check-expect (ufo-render ufo-4) (place-image UFO (posn-x posn-2) (posn-y posn-2) SCENE))

;; UFO-Movement -> UFO-Movement
;; determines UFO-Movement in one clock tick;
;; leaves the velocity as is
(define (ufo-to-move ufo-movement)
  (make-ufo-move (posn+ (ufo-move-position ufo-movement)
                        (ufo-move-velocity ufo-movement))
                 (ufo-move-velocity ufo-movement)))

(check-expect (ufo-to-move ufo-1) ufo-3)
(check-expect (ufo-to-move ufo-2)
              (make-ufo-move (make-posn 17 77) velocity-2))

;; Posn Velocity -> Posn
;; adds velocity to position of UFO
(define (posn+ position velocity)
  (make-posn (+ (posn-x position) (velocity-delta-x velocity))
             (+ (posn-y position) (velocity-delta-y velocity))))

(check-expect (posn+ posn-1 velocity-1) posn-2)
(check-expect (posn+ posn-1 velocity-2) (make-posn 17 77))

;; UFO-Movement -> UFO-Movement
(define (ufo-simulation ufo-movement)
  (big-bang ufo-movement
            [to-draw ufo-render]
            [on-tick ufo-to-move]))

(test)
(ufo-simulation ufo-1)
