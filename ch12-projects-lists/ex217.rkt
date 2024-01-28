#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;;; data definitions and constans

;; a Direction is one of the following constants:
(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")
;; i.e. the direction in which the worm moves.

(struct worm [head body direction] #:transparent)
;; a Worm is a structure:
;;   (worm Posn List-of-Posns Direction)
;; i.e. a (worm head body direction) describes:
;; - the Posn of the worm's head
;; - the Posns of each of the worm's body segments
;; - the Direction in which the worm is travelling
;; examples:
(define WORM-EXAMPLE-0
  (worm (make-posn 75 75) '() LEFT))
(define WORM-EXAMPLE-1
  (worm (make-posn 75 75) (list (make-posn 65 75)) RIGHT))
(define WORM-EXAMPLE-2
  (worm (make-posn 75 75) (list (make-posn 75 85) (make-posn 65 85)) UP))
(define WORM-EXAMPLE-3
  (worm (make-posn 75 75) (list (make-posn 75 65) (make-posn 85 65) (make-posn 95 65)) DOWN))

(define WORM-SEGMENT-RADIUS 5)
(define WORM-SEGMENT-DIAMETER (* WORM-SEGMENT-RADIUS 2))
(define WORM-VELOCITY WORM-SEGMENT-DIAMETER)
(define WORM-SEGMENT (circle WORM-SEGMENT-RADIUS "solid" "lightred"))
(define WORM-HEAD (circle WORM-SEGMENT-RADIUS "solid" "maroon"))

(define CANVAS-WIDTH (* WORM-SEGMENT-DIAMETER 15))
(define CANVAS-HEIGHT CANVAS-WIDTH)
(define CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT "dimgrey"))

(define WALL-LEFT WORM-SEGMENT-RADIUS)
(define WALL-RIGHT (- CANVAS-WIDTH WORM-SEGMENT-RADIUS))
(define WALL-UP WORM-SEGMENT-RADIUS)
(define WALL-DOWN (- CANVAS-HEIGHT WORM-SEGMENT-RADIUS))

(define GAME-OVER-TEXT "game over.")
(define GAME-OVER-FONT 12)
(define GAME-OVER-COLOR "black")
(define GAME-OVER-MESSAGE (text GAME-OVER-TEXT GAME-OVER-FONT GAME-OVER-COLOR))

;; a Game-State is a Worm.
;; i.e. the worm's position and direction.

(define GAME-START-X-COORDINATE WORM-SEGMENT-DIAMETER)
(define GAME-START-Y-COORDINATE (/ CANVAS-HEIGHT 2))
(define GAME-START-DIRECTION RIGHT)
(define GAME-START-STATE
  (worm
   (make-posn
    GAME-START-X-COORDINATE
    GAME-START-Y-COORDINATE)
   '()
   RIGHT))

;;; functions

;; Game-State -> Game-State
;; the main function.

(define (worm-main game-state)
  (big-bang game-state
            [to-draw render-game]
            [on-tick move-worm 0.5]
            [on-key worm-key-handler]))

;; Game-State -> Image
;; renders the current state of the game.

(define (render-game game-state)
  (render-worm (worm-head game-state) (worm-body game-state)))

;; Game-State -> Image
;; renders the image of the worm onto the canvas.

(check-expect
 (render-worm (worm-head WORM-EXAMPLE-0) (worm-body WORM-EXAMPLE-0))
 (place-image WORM-HEAD
              (posn-x (worm-head WORM-EXAMPLE-0))
              (posn-y (worm-head WORM-EXAMPLE-0))
              CANVAS))
(check-expect
 (render-worm (worm-head WORM-EXAMPLE-3) (worm-body WORM-EXAMPLE-3))
 (if (empty? (worm-body WORM-EXAMPLE-3))
     (place-image WORM-HEAD
                  (posn-x (worm-head WORM-EXAMPLE-3))
                  (posn-y (worm-head WORM-EXAMPLE-3))
                  CANVAS)
     (place-image WORM-SEGMENT
                  (posn-x (car (worm-body WORM-EXAMPLE-3)))
                  (posn-y (car (worm-body WORM-EXAMPLE-3)))
                  (render-worm (worm-head WORM-EXAMPLE-3) (rest (worm-body WORM-EXAMPLE-3))))))

(define (render-worm head body)
  (if (empty? body)
      (place-image WORM-HEAD (posn-x head) (posn-y head) CANVAS)
      (place-image WORM-SEGMENT (posn-x (car body)) (posn-y (car body)) (render-worm head (rest body)))))

;; Game-State -> Game-State
;; moves the worm by WORM-VELOCITY per clock tick.

(check-expect
 (move-worm WORM-EXAMPLE-0)
 (worm (adjust-head-position (worm-head WORM-EXAMPLE-0) (worm-direction WORM-EXAMPLE-0))
       (adjust-body-position (worm-head WORM-EXAMPLE-0) (worm-body WORM-EXAMPLE-0))
       (worm-direction WORM-EXAMPLE-0)))
(check-expect
 (move-worm WORM-EXAMPLE-1)
 (worm (adjust-head-position (worm-head WORM-EXAMPLE-1) (worm-direction WORM-EXAMPLE-1))
       (adjust-body-position (worm-head WORM-EXAMPLE-1) (worm-body WORM-EXAMPLE-1))
       (worm-direction WORM-EXAMPLE-1)))
(check-expect
 (move-worm WORM-EXAMPLE-2)
 (worm (adjust-head-position (worm-head WORM-EXAMPLE-2) (worm-direction WORM-EXAMPLE-2))
       (adjust-body-position (worm-head WORM-EXAMPLE-2) (worm-body WORM-EXAMPLE-2))
       (worm-direction WORM-EXAMPLE-2)))
(check-expect
 (move-worm WORM-EXAMPLE-3)
 (worm (adjust-head-position (worm-head WORM-EXAMPLE-3) (worm-direction WORM-EXAMPLE-3))
       (adjust-body-position (worm-head WORM-EXAMPLE-3) (worm-body WORM-EXAMPLE-3))
       (worm-direction WORM-EXAMPLE-3)))

(define (move-worm game-state)
  (worm (adjust-head-position (worm-head game-state) (worm-direction game-state))
        (adjust-body-position (worm-head game-state) (worm-body game-state))
        (worm-direction game-state)))

;; Posn Direction -> Posn
;; adjusts the Posn of the worm's head by WORM-VELOCITY
;; depending on the given Direction.

(check-expect (adjust-head-position (worm-head WORM-EXAMPLE-0) (worm-direction WORM-EXAMPLE-0)) (make-posn 65 75))
(check-expect (adjust-head-position (worm-head WORM-EXAMPLE-1) (worm-direction WORM-EXAMPLE-1)) (make-posn 85 75))
(check-expect (adjust-head-position (worm-head WORM-EXAMPLE-2) (worm-direction WORM-EXAMPLE-2)) (make-posn 75 65))
(check-expect (adjust-head-position (worm-head WORM-EXAMPLE-3) (worm-direction WORM-EXAMPLE-3)) (make-posn 75 85))

(define (adjust-head-position head direction)
  (cond [(string=? direction LEFT) (make-posn (- (posn-x head) WORM-VELOCITY) (posn-y head))]
        [(string=? direction RIGHT) (make-posn (+ (posn-x head) WORM-VELOCITY) (posn-y head))]
        [(string=? direction UP) (make-posn (posn-x head) (- (posn-y head) WORM-VELOCITY))]
        [(string=? direction DOWN) (make-posn (posn-x head) (+ (posn-y head) WORM-VELOCITY))]))

;; Posn List-of-Posns -> List-of-Posns
;; adjusts the Posns of the worn's body segments by WORM-VELOCITY.

(check-expect
 (adjust-body-position (worm-head WORM-EXAMPLE-0) (worm-body WORM-EXAMPLE-0))
 '())
(check-expect
 (adjust-body-position (worm-head WORM-EXAMPLE-1) (worm-body WORM-EXAMPLE-1))
 (list (make-posn 75 75)))
(check-expect
 (adjust-body-position (worm-head WORM-EXAMPLE-2) (worm-body WORM-EXAMPLE-2))
 (list (make-posn 75 75) (make-posn 75 85)))
(check-expect
 (adjust-body-position (worm-head WORM-EXAMPLE-3) (worm-body WORM-EXAMPLE-3))
 (list (make-posn 75 75) (make-posn 75 65) (make-posn 85 65)))

(define (adjust-body-position head body)
  (if (empty? body)
      '()
      (cons head (but-last body))))

;; List -> List
;; returns all but the last element of a List.

(check-expect (but-last (list 1 2 3 4 5)) (list 1 2 3 4))
(check-expect (but-last (list "a" "b" "c" "d" "e")) (list "a" "b" "c" "d"))
(check-expect (but-last (list (make-posn 0 0) (make-posn 1 1) (make-posn 2 2))) (list (make-posn 0 0) (make-posn 1 1)))

(define (but-last a-list)
  (reverse (cdr (reverse a-list))))

;; Game-State Key-Event -> Game-State
;; changes the direction of the worm depending on
;; - the amount of body segment the worm has
;; - the user's keyboard input

(check-expect (worm-key-handler WORM-EXAMPLE-0 "left") (change-worm-direction/empty-body WORM-EXAMPLE-0 "left"))
(check-expect (worm-key-handler WORM-EXAMPLE-1 "right") (change-worm-direction WORM-EXAMPLE-1 "left"))
(check-expect (worm-key-handler WORM-EXAMPLE-2 "up") (change-worm-direction WORM-EXAMPLE-2 "up"))
(check-expect (worm-key-handler WORM-EXAMPLE-3 "down") (change-worm-direction WORM-EXAMPLE-3 "down"))
(check-expect (worm-key-handler WORM-EXAMPLE-3 "o") (change-worm-direction WORM-EXAMPLE-3 "down"))

(define (worm-key-handler game-state key-event)
  (if (empty? (worm-body game-state))
      (change-worm-direction/empty-body game-state key-event)
      (change-worm-direction game-state key-event)))

;; Game-State Key-Event -> Game-State
;; changes the direction of the worm when it has no
;; body segments.

(check-expect
 (change-worm-direction/empty-body WORM-EXAMPLE-0 "left")
 WORM-EXAMPLE-0)
(check-expect
 (change-worm-direction/empty-body WORM-EXAMPLE-0 "right")
 (worm (worm-head WORM-EXAMPLE-0) (worm-body WORM-EXAMPLE-0) RIGHT))
(check-expect
 (change-worm-direction/empty-body WORM-EXAMPLE-0 "up")
 (worm (worm-head WORM-EXAMPLE-0) (worm-body WORM-EXAMPLE-0) UP))
(check-expect
 (change-worm-direction/empty-body WORM-EXAMPLE-0 "down")
 (worm (worm-head WORM-EXAMPLE-0) (worm-body WORM-EXAMPLE-0) DOWN))
(check-expect
 (change-worm-direction/empty-body WORM-EXAMPLE-0 "o")
 WORM-EXAMPLE-0)

(define (change-worm-direction/empty-body game-state key-event)
  (worm (worm-head game-state)
        (worm-body game-state)
        (cond [(key=? key-event "left") LEFT]
              [(key=? key-event "right") RIGHT]
              [(key=? key-event "up") UP]
              [(key=? key-event "down") DOWN]
              [else (worm-direction game-state)])))

;; Game-State Key-Event -> Game-State
;; changes the direction of the worm when its has
;; at least one body segment.

(check-expect (change-worm-direction WORM-EXAMPLE-1 "left") WORM-EXAMPLE-1)
(check-expect (change-worm-direction WORM-EXAMPLE-1 "right") WORM-EXAMPLE-1)
(check-expect (change-worm-direction WORM-EXAMPLE-1 "up") (worm (make-posn 75 75) (list (make-posn 65 75)) UP))
(check-expect (change-worm-direction WORM-EXAMPLE-1 "down") (worm (make-posn 75 75) (list (make-posn 65 75)) DOWN))
(check-expect (change-worm-direction WORM-EXAMPLE-1 "o") WORM-EXAMPLE-1)

(define (change-worm-direction game-state key-event)
  (worm (worm-head game-state)
        (worm-body game-state)
        (cond [(and (key=? key-event "left") (not (string=? (worm-direction game-state) RIGHT))) LEFT]
              [(and (key=? key-event "right") (not (string=? (worm-direction game-state) LEFT))) RIGHT]
              [(and (key=? key-event "up") (not (string=? (worm-direction game-state) DOWN))) UP]
              [(and (key=? key-event "down") (not (string=? (worm-direction game-state) UP))) DOWN]
              [else (worm-direction game-state)])))

;; ;;; application

(test)

;; (worm-main GAME-START-STATE)

;;; unused functions

;; Game-State -> Boolean
;; checks if the worm has hit any wall.

;; (check-expect (worm-hit-wall? worm-example-0) #f)
;; (check-expect (worm-hit-wall? worm-example-1) #f)
;; (check-expect (worm-hit-wall? worm-example-2) #t)
;; (check-expect (worm-hit-wall? worm-example-3) #t)

;; (define (worm-hit-wall? game-state)
;;   (or (<= (posn-x (worm-position game-state)) WALL-LEFT)
;;       (>= (posn-x (worm-position game-state)) WALL-RIGHT)
;;       (<= (posn-y (worm-position game-state)) WALL-UP)
;;       (>= (posn-y (worm-position game-state)) WALL-DOWN)))

;; Game-State -> Boolean
;; renders the game-over screen.

;; (check-expect (game-over-renderer worm-example-2)
;;               (overlay GAME-OVER-MESSAGE (worm-renderer worm-example-2)))

;; (define (game-over-renderer game-state)
;;   (overlay GAME-OVER-MESSAGE (worm-renderer game-state)))
