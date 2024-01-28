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

;; a Food is a Posn.

(define WORM-SEGMENT-RADIUS 5)
(define WORM-SEGMENT-DIAMETER (* WORM-SEGMENT-RADIUS 2))
(define WORM-VELOCITY WORM-SEGMENT-DIAMETER)
(define WORM-SEGMENT (circle WORM-SEGMENT-RADIUS "solid" "lightred"))
(define WORM-HEAD (circle WORM-SEGMENT-RADIUS "solid" "maroon"))

(define CANVAS-WIDTH (* WORM-SEGMENT-DIAMETER 15))
(define CANVAS-HEIGHT CANVAS-WIDTH)
(define CANVAS-COLOR "dimgrey")
(define CANVAS-COLOR/GAME-OVER "maroon")
(define CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT CANVAS-COLOR))
(define CANVAS/GAME-OVER (empty-scene CANVAS-WIDTH CANVAS-HEIGHT CANVAS-COLOR/GAME-OVER))

(define FOOD-SEGMENT-RADIUS WORM-SEGMENT-RADIUS)
(define FOOD (circle FOOD-SEGMENT-RADIUS "solid" "yellow"))
(define FOOD-MAX (+ 1 WORM-SEGMENT-DIAMETER))

(define WALL-LEFT WORM-SEGMENT-RADIUS)
(define WALL-RIGHT (- CANVAS-WIDTH WORM-SEGMENT-RADIUS))
(define WALL-UP WORM-SEGMENT-RADIUS)
(define WALL-DOWN (- CANVAS-HEIGHT WORM-SEGMENT-RADIUS))

(define GAME-OVER-FONT 12)
(define GAME-OVER-COLOR "white")
(define GAME-OVER-TEXT/HIT-WALL "you hit a wall.\ngame over.")
(define GAME-OVER-TEXT/HIT-SELF "you hit yourself.\ngame over.")
(define GAME-OVER-MESSAGE/HIT-WALL (text GAME-OVER-TEXT/HIT-WALL GAME-OVER-FONT GAME-OVER-COLOR))
(define GAME-OVER-MESSAGE/HIT-SELF (text GAME-OVER-TEXT/HIT-SELF GAME-OVER-FONT GAME-OVER-COLOR))

(struct game-state [worm food] #:transparent)
;; a Game-State is a structure:
;;   (game-state Worm Posn)
;; a (game-state worm food) describes:
;; - the overall Posn of a worm and its direction
;; - the Posn of a piece of food
;; NOTE: a Game-State is used as the World-State
;; in this program.

(define GAME-START-X-COORDINATE WORM-SEGMENT-DIAMETER)
(define GAME-START-Y-COORDINATE WORM-SEGMENT-DIAMETER)
(define GAME-START-DIRECTION RIGHT)
(define GAME-START-STATE
  (game-state
   (worm
    (make-posn
     GAME-START-X-COORDINATE
     GAME-START-Y-COORDINATE)
    '()
    RIGHT)
   (make-posn 100 100)))

;;; functions

;; World-State -> World-State
;; the main function.

(define (game-main world-state)
  (big-bang world-state
            [to-draw game-render]
            [on-tick game-move 0.75]
            [on-key game-change-direction]
            [stop-when worm-hurt? worm-game-over]))

;; World-State -> Image
;; renders the current state of the game.

(define (game-render world-state)
  (render/head-body-food (worm-head (game-state-worm world-state))
                         (worm-body (game-state-worm world-state))
                         (game-state-food world-state)))

;; Posn List-of-Posns Posn -> Image
;; renders the image of the worm onto the canvas.

(check-expect
 (render/head-body-food (worm-head WORM-EXAMPLE-0) (worm-body WORM-EXAMPLE-0) (make-posn 120 120))
 (render/head-food (worm-head WORM-EXAMPLE-0) (make-posn 120 120)))
(check-expect
 (render/head-body-food (worm-head WORM-EXAMPLE-3) (worm-body WORM-EXAMPLE-3) (make-posn 120 120))
 (if (empty? (worm-body WORM-EXAMPLE-3))
     (render/head-food (worm-head WORM-EXAMPLE-3) (make-posn 120 120))
     (place-image WORM-SEGMENT
                  (posn-x (car (worm-body WORM-EXAMPLE-3)))
                  (posn-y (car (worm-body WORM-EXAMPLE-3)))
                  (render/head-body-food (worm-head WORM-EXAMPLE-3)
                                         (rest (worm-body WORM-EXAMPLE-3))
                                         (make-posn 120 120)))))

(define (render/head-body-food head body food)
  (if (empty? body)
      (render/head-food head food)
      (place-image WORM-SEGMENT (posn-x (car body)) (posn-y (car body))
                   (render/head-body-food head (rest body) food))))

;; Posn Posn -> Image
;; renders the image of the worm's head and its food
;; onto the canvas.

(check-expect
 (render/head-food (worm-head WORM-EXAMPLE-0) (make-posn 120 120))
 (place-image FOOD (posn-x (make-posn 120 120)) (posn-y (make-posn 120 120))
              (place-image WORM-HEAD
                           (posn-x (worm-head WORM-EXAMPLE-0))
                           (posn-y (worm-head WORM-EXAMPLE-0))
                           CANVAS)))

(define (render/head-food head food)
  (place-image FOOD (posn-x food) (posn-y food)
               (place-image WORM-HEAD (posn-x head) (posn-y head) CANVAS)))

;; World-State -> World-State
;; moves the worm by WORM-VELOCITY per clock tick.

(check-expect
 (game-move (game-state WORM-EXAMPLE-0 (make-posn 145 145)))
 (make-game-state/retain-food (game-state WORM-EXAMPLE-0 (make-posn 145 145))))
(check-random
 (game-move (game-state WORM-EXAMPLE-0 (make-posn 75 75)))
 (make-game-state/add-food (game-state WORM-EXAMPLE-0 (make-posn 75 75))))

(define (game-move world-state)
  (if (food-eaten? (worm-head (game-state-worm world-state)) (game-state-food world-state))
      (make-game-state/add-food world-state)
      (make-game-state/retain-food world-state)))

;; Posn Posn -> Boolean
;; checks if the worm has eaten the piece of food.

(check-expect (food-eaten? (make-posn 15 20) (make-posn 15 20)) #t)
(check-expect (food-eaten? (make-posn 15 15) (make-posn 100 100)) #f)

(define (food-eaten? head food)
  (and (= (posn-x head) (posn-x food)) (= (posn-y head) (posn-y food))))

;; World-State -> World-State
;; moves the worm by WORM-VELOCITY and adds a new piece of
;; food for the worm.

(check-random
 (make-game-state/add-food (game-state WORM-EXAMPLE-0 (make-posn 85 85)))
 (game-state
  (worm (make-posn 85 85)
        (add-to-worm-body (make-posn 85 75) '())
        LEFT)
  (food-create (make-posn 85 85))))

(define (make-game-state/add-food world-state)
  (game-state
   (worm (game-state-food world-state)
         ;; TODO: refactor
         (add-to-worm-body
          (cond [(string=? (worm-direction (game-state-worm world-state)) LEFT)
                 (make-posn (+ (posn-x (worm-head (game-state-worm world-state))) WORM-VELOCITY)
                            (posn-y (worm-head (game-state-worm world-state))))]
                [(string=? (worm-direction (game-state-worm world-state)) RIGHT)
                 (make-posn (- (posn-x (worm-head (game-state-worm world-state))) WORM-VELOCITY)
                            (posn-y (worm-head (game-state-worm world-state))))]
                [(string=? (worm-direction (game-state-worm world-state)) UP)
                 (make-posn (posn-x (worm-head (game-state-worm world-state)))
                            (+ (posn-y (worm-head (game-state-worm world-state))) WORM-VELOCITY))]
                [(string=? (worm-direction (game-state-worm world-state)) DOWN)
                 (make-posn (posn-x (worm-head (game-state-worm world-state)))
                            (- (posn-y (worm-head (game-state-worm world-state))) WORM-VELOCITY))])
          (worm-body (game-state-worm world-state)))
         (worm-direction (game-state-worm world-state)))
   (food-create (game-state-food world-state))))

;; World-State -> World-State
;; moves the worm by WORM-VELOCITY; does not add a new
;; piece of food for the worm.

(check-expect
 (game-move (game-state WORM-EXAMPLE-0 (make-posn 145 145)))
 (game-state
  (worm (adjust-position/head (make-posn 75 75) LEFT)
        (adjust-position/body (make-posn 75 75) '())
        LEFT)
  (make-posn 145 145)))

(define (make-game-state/retain-food world-state)
  (game-state
   (worm (adjust-position/head (worm-head (game-state-worm world-state)) (worm-direction (game-state-worm world-state)))
         (adjust-position/body (worm-head (game-state-worm world-state)) (worm-body (game-state-worm world-state)))
         (worm-direction (game-state-worm world-state)))
   (game-state-food world-state)))

;; Posn -> Posn
;; creates a new Posn for a piece of food.

(check-satisfied (food-create (make-posn 10 10)) food-within-frame?)

(define (food-create old-posn)
  (food-check-create old-posn (make-posn (* (random FOOD-MAX) 10) (* (random FOOD-MAX) 10))))

;; Posn Posn -> Posn
;; generative recursion
;; creates a new Posn for a piece of food by:
;; - retaining the new-posn if the old-posn and new-posn differ
;; - else, creates a new Posn.

(check-expect (food-check-create (make-posn 15 15) (make-posn 20 20)) (make-posn 20 20))
(check-random (food-check-create (make-posn 15 15) (make-posn 15 15)) (food-create (make-posn 15 15)))

(define (food-check-create old-posn new-posn)
  (if (or (equal? old-posn new-posn) (not (food-within-frame? new-posn)))
      (food-create old-posn)
      new-posn))

;; Posn -> Boolean
;; used for testing food-create only. checks to see if the piece
;; of food is within frame.

(check-expect (food-within-frame? (make-posn 10 10)) #t)
(check-expect (food-within-frame? (make-posn -10 -10)) #f)

(define (food-within-frame? a-posn)
  (and (>= (posn-x a-posn) WALL-LEFT)
       (<= (posn-x a-posn) WALL-RIGHT)
       (>= (posn-y a-posn) WALL-UP)
       (<= (posn-y a-posn) WALL-DOWN)))

;; Posn Direction -> Posn
;; adjusts the Posn of the worm's head by WORM-VELOCITY
;; depending on the given Direction.

(check-expect (adjust-position/head (worm-head WORM-EXAMPLE-0) (worm-direction WORM-EXAMPLE-0)) (make-posn 65 75))
(check-expect (adjust-position/head (worm-head WORM-EXAMPLE-1) (worm-direction WORM-EXAMPLE-1)) (make-posn 85 75))
(check-expect (adjust-position/head (worm-head WORM-EXAMPLE-2) (worm-direction WORM-EXAMPLE-2)) (make-posn 75 65))
(check-expect (adjust-position/head (worm-head WORM-EXAMPLE-3) (worm-direction WORM-EXAMPLE-3)) (make-posn 75 85))

(define (adjust-position/head head direction)
  (cond [(string=? direction LEFT) (make-posn (- (posn-x head) WORM-VELOCITY) (posn-y head))]
        [(string=? direction RIGHT) (make-posn (+ (posn-x head) WORM-VELOCITY) (posn-y head))]
        [(string=? direction UP) (make-posn (posn-x head) (- (posn-y head) WORM-VELOCITY))]
        [(string=? direction DOWN) (make-posn (posn-x head) (+ (posn-y head) WORM-VELOCITY))]))

;; Posn List-of-Posns -> List-of-Posns
;; adds a segment to the worm's body while maintaining the current Posns
;; of each of the worm's body segments.

(check-expect
 (add-to-worm-body (worm-head WORM-EXAMPLE-0) (worm-body WORM-EXAMPLE-0))
 (cons (worm-head WORM-EXAMPLE-0) (worm-body WORM-EXAMPLE-0)))
(check-expect
 (add-to-worm-body (worm-head WORM-EXAMPLE-1) (worm-body WORM-EXAMPLE-1))
 (cons (worm-head WORM-EXAMPLE-1) (worm-body WORM-EXAMPLE-1)))
(check-expect
 (add-to-worm-body (worm-head WORM-EXAMPLE-2) (worm-body WORM-EXAMPLE-2))
 (cons (worm-head WORM-EXAMPLE-2) (worm-body WORM-EXAMPLE-2)))
(check-expect
 (add-to-worm-body (worm-head WORM-EXAMPLE-3) (worm-body WORM-EXAMPLE-3))
 (cons (worm-head WORM-EXAMPLE-3) (worm-body WORM-EXAMPLE-3)))

(define (add-to-worm-body head body)
  (cons head body))

;; Posn List-of-Posns -> List-of-Posns
;; adjusts the Posns of the worm's body segments by WORM-VELOCITY.

(check-expect
 (adjust-position/body (worm-head WORM-EXAMPLE-0) (worm-body WORM-EXAMPLE-0))
 '())
(check-expect
 (adjust-position/body (worm-head WORM-EXAMPLE-1) (worm-body WORM-EXAMPLE-1))
 (list (make-posn 75 75)))
(check-expect
 (adjust-position/body (worm-head WORM-EXAMPLE-2) (worm-body WORM-EXAMPLE-2))
 (list (make-posn 75 75) (make-posn 75 85)))
(check-expect
 (adjust-position/body (worm-head WORM-EXAMPLE-3) (worm-body WORM-EXAMPLE-3))
 (list (make-posn 75 75) (make-posn 75 65) (make-posn 85 65)))

(define (adjust-position/body head body)
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

;; World-State Key-Event -> World-State
;; changes the direction of the worm depending on
;; - the amount of body segment the worm has
;; - the user's keyboard input.

(check-expect
 (game-change-direction (game-state WORM-EXAMPLE-0 (make-posn 50 50)) "left")
 (game-state (worm-change-direction/empty-body WORM-EXAMPLE-0 "left") (make-posn 50 50)))
(check-expect
 (game-change-direction (game-state WORM-EXAMPLE-1 (make-posn 50 50)) "right")
 (game-state (worm-change-direction/body WORM-EXAMPLE-1 "left") (make-posn 50 50)))
(check-expect
 (game-change-direction (game-state WORM-EXAMPLE-2 (make-posn 50 50)) "up")
 (game-state (worm-change-direction/body WORM-EXAMPLE-2 "up") (make-posn 50 50)))
(check-expect
 (game-change-direction (game-state WORM-EXAMPLE-3 (make-posn 50 50)) "down")
 (game-state (worm-change-direction/body WORM-EXAMPLE-3 "down") (make-posn 50 50)))
(check-expect
 (game-change-direction (game-state WORM-EXAMPLE-3 (make-posn 50 50)) "o")
 (game-state (worm-change-direction/body WORM-EXAMPLE-3 "o") (make-posn 50 50)))

(define (game-change-direction  world-state key-event)
  (game-state
   (if (empty? (worm-body (game-state-worm world-state)))
       (worm-change-direction/empty-body (game-state-worm world-state) key-event)
       (worm-change-direction/body (game-state-worm world-state) key-event))
   (game-state-food world-state)))

;; Game-State Key-Event -> Game-State
;; changes the direction of the worm when it has no
;; body segments.

(check-expect
 (worm-change-direction/empty-body WORM-EXAMPLE-0 "left")
 WORM-EXAMPLE-0)
(check-expect
 (worm-change-direction/empty-body WORM-EXAMPLE-0 "right")
 (worm (worm-head WORM-EXAMPLE-0) (worm-body WORM-EXAMPLE-0) RIGHT))
(check-expect
 (worm-change-direction/empty-body WORM-EXAMPLE-0 "up")
 (worm (worm-head WORM-EXAMPLE-0) (worm-body WORM-EXAMPLE-0) UP))
(check-expect
 (worm-change-direction/empty-body WORM-EXAMPLE-0 "down")
 (worm (worm-head WORM-EXAMPLE-0) (worm-body WORM-EXAMPLE-0) DOWN))
(check-expect
 (worm-change-direction/empty-body WORM-EXAMPLE-0 "o")
 WORM-EXAMPLE-0)

(define (worm-change-direction/empty-body game-state key-event)
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

(check-expect
 (worm-change-direction/body WORM-EXAMPLE-1 "left")
 WORM-EXAMPLE-1)
(check-expect
 (worm-change-direction/body WORM-EXAMPLE-1 "right")
 WORM-EXAMPLE-1)
(check-expect
 (worm-change-direction/body WORM-EXAMPLE-1 "up")
 (worm (make-posn 75 75) (list (make-posn 65 75)) UP))
(check-expect
 (worm-change-direction/body WORM-EXAMPLE-1 "down")
 (worm (make-posn 75 75) (list (make-posn 65 75)) DOWN))
(check-expect
 (worm-change-direction/body WORM-EXAMPLE-1 "o")
 WORM-EXAMPLE-1)

(define (worm-change-direction/body game-state key-event)
  (worm (worm-head game-state)
        (worm-body game-state)
        (cond [(and (key=? key-event "left") (not (string=? (worm-direction game-state) RIGHT))) LEFT]
              [(and (key=? key-event "right") (not (string=? (worm-direction game-state) LEFT))) RIGHT]
              [(and (key=? key-event "up") (not (string=? (worm-direction game-state) DOWN))) UP]
              [(and (key=? key-event "down") (not (string=? (worm-direction game-state) UP))) DOWN]
              [else (worm-direction game-state)])))

;; World-State -> Boolean
;; checks if the worm has collided with either:
;; - a wall
;; - itself.

(check-expect
 (worm-hurt? (game-state WORM-EXAMPLE-0 (make-posn 50 50)))
 (or (worm-hit-wall? WORM-EXAMPLE-0) (worm-hit-itself? WORM-EXAMPLE-0)))
(check-expect
 (worm-hurt? (game-state WORM-EXAMPLE-1 (make-posn 50 50)))
 (or (worm-hit-wall? WORM-EXAMPLE-1) (worm-hit-itself? WORM-EXAMPLE-1)))
(check-expect
 (worm-hurt? (game-state WORM-EXAMPLE-2 (make-posn 50 50)))
 (or (worm-hit-wall? WORM-EXAMPLE-2) (worm-hit-itself? WORM-EXAMPLE-2)))
(check-expect
 (worm-hurt? (game-state WORM-EXAMPLE-3 (make-posn 50 50)))
 (or (worm-hit-wall? WORM-EXAMPLE-3) (worm-hit-itself? WORM-EXAMPLE-3)))

(define (worm-hurt? world-state)
  (or (worm-hit-wall? (game-state-worm world-state)) (worm-hit-itself? (game-state-worm world-state))))

;; Worm -> Boolean
;; checks if the worm has collided with itself.

(check-expect (worm-hit-wall? WORM-EXAMPLE-0) #f)
(check-expect (worm-hit-wall? WORM-EXAMPLE-1) #f)
(check-expect (worm-hit-wall? WORM-EXAMPLE-2) #f)
(check-expect (worm-hit-wall? WORM-EXAMPLE-3) #f)
(check-expect
 (worm-hit-wall?(worm (make-posn (random 5) 75) (list (make-posn 75 65) (make-posn 85 65) (make-posn 95 65)) DOWN))
 #t)
(check-expect
 (worm-hit-wall?
  (worm (make-posn (random 145 150) 75) (list (make-posn 75 65) (make-posn 85 65) (make-posn 95 65)) DOWN))
 #t)
(check-expect
 (worm-hit-wall?
  (worm (make-posn 10 (random 5)) (list (make-posn 75 65) (make-posn 85 65) (make-posn 95 65)) DOWN))
 #t)
(check-expect
 (worm-hit-wall?
  (worm (make-posn 10 (random 145 150)) (list (make-posn 75 65) (make-posn 85 65) (make-posn 95 65)) DOWN))
 #t)

(define (worm-hit-wall? game-state)
  (or (<= (posn-x (worm-head game-state)) WALL-LEFT)
      (>= (posn-x (worm-head game-state)) WALL-RIGHT)
      (<= (posn-y (worm-head game-state)) WALL-UP)
      (>= (posn-y (worm-head game-state)) WALL-DOWN)))

;; Worm -> Boolean
;; checks if the worm has collided with itself.

(check-expect (worm-hit-itself? WORM-EXAMPLE-0) #f)
(check-expect (worm-hit-itself? WORM-EXAMPLE-1) #f)
(check-expect (worm-hit-itself? WORM-EXAMPLE-2) #f)
(check-expect (worm-hit-itself? WORM-EXAMPLE-3) #f)
(check-expect
 (worm-hit-itself?
  (worm (make-posn 85 75) (list (make-posn 75 85) (make-posn 85 85) (make-posn 85 75)) RIGHT))
 #t)

(define (worm-hit-itself? game-state)
  (not (boolean? (member (worm-head game-state) (worm-body game-state)))))

;; World-State -> Image
;; renders the gameover screen of the game.

(check-expect
 (worm-game-over
  (game-state
   (worm (make-posn (random 5) 75) (list (make-posn 75 65) (make-posn 85 65) (make-posn 95 65)) DOWN)
   (make-posn 50 50)))
 (overlay GAME-OVER-MESSAGE/HIT-WALL CANVAS/GAME-OVER))
(check-expect
 (worm-game-over
  (game-state
   (worm (make-posn 85 75) (list (make-posn 75 85) (make-posn 85 85) (make-posn 85 75)) RIGHT)
   (make-posn 50 50)))
 (overlay GAME-OVER-MESSAGE/HIT-SELF CANVAS/GAME-OVER))

(define (worm-game-over world-state)
  (if (worm-hit-wall? (game-state-worm world-state))
      (overlay GAME-OVER-MESSAGE/HIT-WALL CANVAS/GAME-OVER)
      (overlay GAME-OVER-MESSAGE/HIT-SELF CANVAS/GAME-OVER)))

;; World-State -> Boolean
;; checks if the game should continue running.

(check-expect
 (worm-not-hurt? (game-state WORM-EXAMPLE-0 (make-posn 10 10)))
 #t)
(check-expect
 (worm-not-hurt?
  (game-state
   (worm (make-posn 10 (random 145 150)) (list (make-posn 75 65) (make-posn 85 65) (make-posn 95 65)) DOWN)
   (make-posn 50 50)))
 #f)
(check-expect
 (worm-not-hurt?
  (game-state
   (worm (make-posn 85 75) (list (make-posn 75 85) (make-posn 85 85) (make-posn 85 75)) RIGHT)
   (make-posn 50 50)))
 #f)

(define (worm-not-hurt? world-state)
  (not (worm-hurt? world-state)))

;;; application

(test)

(game-main GAME-START-STATE)
