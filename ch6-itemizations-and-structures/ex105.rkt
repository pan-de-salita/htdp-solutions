#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

;;; constant and structure definitions

;; a Coordinate is one of:
;; - a NegativeNumber | on the y axis, distance from top
;; - a PositiveNumber | on the x axis, distance from left
;; - a Posn | an ordinary Cartesian point
(define COORDINATE-TEST-0 -4)
(define COORDINATE-TEST-1 6)
(define COORDINATE-TEST-2 (make-posn 10 10))

(define CANVAS-WIDTH 50)
(define CANVAS-HEIGHT 50)
(define CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT "black"))

(define DOT (circle (/ CANVAS-WIDTH 25) "solid" "red"))
(define DOT-ORIGIN 0)

(define SKETCH-TEST-0
  (place-image DOT DOT-ORIGIN COORDINATE-TEST-0 CANVAS))
(define SKETCH-TEST-1
  (place-image DOT COORDINATE-TEST-1 DOT-ORIGIN CANVAS))
(define SKETCH-TEST-2
  (place-image DOT
               (posn-x COORDINATE-TEST-2)
               (posn-y COORDINATE-TEST-2)
               CANVAS))

;;; function definitions

;; Coordinate -> Image
;; places DOT onto CANVAS according to Coordinate given.
(check-expect (coordinate->image COORDINATE-TEST-0) SKETCH-TEST-0)
(check-expect (coordinate->image COORDINATE-TEST-1) SKETCH-TEST-1)
(check-expect (coordinate->image COORDINATE-TEST-2) SKETCH-TEST-2)

(define (coordinate->image coordinate)
  (place-image DOT
               (posn-x (pinpoint coordinate))
               (posn-y (pinpoint coordinate))
               CANVAS))

;; Coordinate -> Posn
;; converts Coordinate into a usable Posn when applicable
(check-expect (pinpoint COORDINATE-TEST-0) (make-posn DOT-ORIGIN COORDINATE-TEST-0))
(check-expect (pinpoint COORDINATE-TEST-1) (make-posn COORDINATE-TEST-1 DOT-ORIGIN))
(check-expect (pinpoint COORDINATE-TEST-2) COORDINATE-TEST-2)

(define (pinpoint coordinate)
  (cond [(posn? coordinate) coordinate]
        [(< coordinate 0) (make-posn DOT-ORIGIN coordinate)]
        [(> coordinate 0) (make-posn coordinate DOT-ORIGIN)]))

(beside SKETCH-TEST-0 SKETCH-TEST-1 SKETCH-TEST-2)
(test)
