#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;;; data definitions and constans

;; a direction is one of the following constants:
(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")
;; i.e. the direction in which the worm moves.

(struct worm [position direction] #:transparent)
;; a Worm-State is a structure:
;;   (worm-logical Posn String)
;; i.e. a (worm-logical Posn Direction) describes:
;; - the worm's position
;; - the worm's direction
;; examples:
(define worm-example-0 (worm (make-posn 12 4) LEFT))
(define worm-example-1 (worm (make-posn 3 12) RIGHT))
(define worm-example-2 (worm (make-posn 4 12) UP))
(define worm-example-3 (worm (make-posn 12 3) DOWN))

(define WORM-SEGMENT-RADIUS 5)
(define WORM-SEGMENT-DIAMETER (* WORM-SEGMENT-RADIUS 2))
(define WORM-VELOCITY WORM-SEGMENT-DIAMETER)
(define WORM-SEGMENT (circle WORM-SEGMENT-RADIUS "solid" "red"))

(define CANVAS-WIDTH (* WORM-SEGMENT-DIAMETER 15))
(define CANVAS-HEIGHT CANVAS-WIDTH)
(define CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT "dimgrey"))

;; a Game-State is a Worm-State.
;; i.e. the worm's position and direction.

(define GAME-START-X-COORDINATE WORM-SEGMENT-DIAMETER)
(define GAME-START-Y-COORDINATE (/ CANVAS-HEIGHT 2))
(define GAME-START-STATE
  (worm
   (make-posn
    GAME-START-X-COORDINATE
    GAME-START-Y-COORDINATE)
   RIGHT))

;;; functions

;; Game-State -> Game-State
;; the main function.

(define (worm-main game-state)
  (big-bang game-state
            [to-draw worm-renderer]
            [on-tick worm-tick-handler 0.75]
            [on-key worm-key-handler]))

;; Game-State -> Image
;; renders the image of the worm onto the canvas.

(check-expect
 (worm-renderer worm-example-0)
 (place-image WORM-SEGMENT
              (posn-x (worm-position worm-example-0))
              (posn-y (worm-position worm-example-0))
              CANVAS))
(check-expect
 (worm-renderer GAME-START-STATE)
 (place-image WORM-SEGMENT
              (posn-x (worm-position GAME-START-STATE))
              (posn-y (worm-position GAME-START-STATE))
              CANVAS))

(define (worm-renderer game-state)
  (place-image WORM-SEGMENT
               (posn-x (worm-position game-state))
               (posn-y (worm-position game-state))
               CANVAS))

;; Game-State -> Game-State
;; moves the worm by WORM-VELOCITY each clock tick.

(check-expect (worm-tick-handler worm-example-0) (worm-change-x-position worm-example-0))
(check-expect (worm-tick-handler worm-example-1) (worm-change-x-position worm-example-1))
(check-expect (worm-tick-handler worm-example-2) (worm-change-y-position worm-example-2))
(check-expect (worm-tick-handler worm-example-3) (worm-change-y-position worm-example-3))
(check-expect (worm-tick-handler GAME-START-STATE) (worm-change-x-position GAME-START-STATE))

(define (worm-tick-handler game-state)
  (cond [(or (string=? (worm-direction game-state) LEFT)
             (string=? (worm-direction game-state) RIGHT))
         (worm-change-x-position game-state)]
        [else (worm-change-y-position game-state)]))

;; Game-State -> Game-State
;; moves the worm to the left or to the right by WORM-VELOCITY
;; depending on given direction.

(check-expect
 (worm-change-x-position worm-example-0)
 (worm (make-posn
        (- (posn-x (worm-position worm-example-0)) WORM-VELOCITY)
        (posn-y (worm-position worm-example-0)))
       (worm-direction worm-example-0)))
(check-expect
 (worm-change-x-position worm-example-1)
 (worm (make-posn
        (+ (posn-x (worm-position worm-example-1)) WORM-VELOCITY)
        (posn-y (worm-position worm-example-1)))
       (worm-direction worm-example-1)))
(check-expect
 (worm-change-x-position GAME-START-STATE)
 (worm (make-posn
        (+ (posn-x (worm-position GAME-START-STATE)) WORM-VELOCITY)
        (posn-y (worm-position GAME-START-STATE)))
       (worm-direction GAME-START-STATE)))

(define (worm-change-x-position game-state)
  (worm (make-posn
         ((if (string=? (worm-direction game-state) LEFT) - +)
          (posn-x (worm-position game-state))
          WORM-VELOCITY)
         (posn-y (worm-position game-state)))
        (worm-direction game-state)))

;; Game-State -> Game-State
;; moves the worm upwards or downwards by WORM-VELOCITY
;; depending on given direction.

(check-expect
 (worm-change-y-position worm-example-2)
 (worm (make-posn
        (posn-x (worm-position worm-example-2))
        (- (posn-y (worm-position worm-example-2)) WORM-VELOCITY))
       (worm-direction worm-example-2)))
(check-expect
 (worm-change-y-position worm-example-3)
 (worm (make-posn
        (posn-x (worm-position worm-example-3))
        (+ (posn-y (worm-position worm-example-3)) WORM-VELOCITY))
       (worm-direction worm-example-3)))

(define (worm-change-y-position game-state)
  (worm (make-posn
         (posn-x (worm-position game-state))
         ((if (string=? (worm-direction game-state) UP) - +)
          (posn-y (worm-position game-state))
          WORM-VELOCITY))
        (worm-direction game-state)))

;; Game-State Key-Event -> Game-State
;; changes the direction of the worm depending on
;; the user's keyboard input.

(check-expect (worm-key-handler worm-example-0 "left") worm-example-0)
(check-expect
 (worm-key-handler worm-example-0 "right")
 (worm (make-posn
        (posn-x (worm-position worm-example-0))
        (posn-y (worm-position worm-example-0)))
       RIGHT))
(check-expect
 (worm-key-handler worm-example-0 "up")
 (worm (make-posn
        (posn-x (worm-position worm-example-0))
        (posn-y (worm-position worm-example-0)))
       UP))
(check-expect
 (worm-key-handler worm-example-0 "down")
 (worm (make-posn
        (posn-x (worm-position worm-example-0))
        (posn-y (worm-position worm-example-0)))
       DOWN))
(check-expect
 (worm-key-handler worm-example-0 "o")
 worm-example-0)

(define (worm-key-handler game-state key-event)
  (worm (make-posn
         (posn-x (worm-position game-state))
         (posn-y (worm-position game-state)))
        (cond [(key=? key-event "left") LEFT]
              [(key=? key-event "right") RIGHT]
              [(key=? key-event "up") UP]
              [(key=? key-event "down") DOWN]
              [else (worm-direction game-state)])))

;;; application

(test)

(worm-main GAME-START-STATE)
