#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

;;; data and constant definitions

(define GRID-CELL 10)
(define GRID-COLUMNS (+ (* GRID-CELL 20) GRID-CELL))
(define GRID-ROWS GRID-COLUMNS)
(define GRID (empty-scene GRID-COLUMNS GRID-ROWS "black"))

(define GRID-LIMIT-LEFT 0)
(define GRID-LIMIT-RIGHT GRID-COLUMNS)
(define GRID-LIMIT-UP 0)
(define GRID-LIMIT-DOWN GRID-ROWS)

(define WORM-RADIUS (/ GRID-CELL 2))
(define WORM-DIAMETER GRID-CELL)
(define WORM-SEGMENT (circle WORM-RADIUS "solid" "darkpink"))
(define WORM-VELOCITY WORM-DIAMETER)
(define WORM-HEAD (circle WORM-RADIUS "solid" "maroon"))

(define FOOD (circle WORM-RADIUS "solid" "lightbrown"))

(define GAME-SPEED 0.1)

(define GAME-OVER/WALL "worm hit a wall \nyour score: ")
(define GAME-OVER/BODY "worm hit its body \nyour score: ")
(define GAME-OVER-FONT-SIZE 12)
(define GAME-OVER-FONT-COLOR "white")
(define GAME-OVER-POSN (make-posn (/ GRID-COLUMNS 2) (+ (/ GRID-ROWS 2) (/ (/ GRID-ROWS 2) 2))))

;; a Direction is one of the following:
(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")

(struct worm [head body direction] #:transparent)
;; a Worm is a structure:
;;   (worm Posn List-of-Posns Direction)
;; i.e. describes:
;; - a worm's head posn
;; - the Posn of each of the worm's body segments
;; - the direction it's travelling towards.
(define WORM-INIT (worm (make-posn 50 100) '() RIGHT))

(struct frame [worm food] #:transparent)
;; a Frame is a structure:
;;   (frame Worm Posn)
;; i.e. describes:
;; - a Worm
;; - the Posn of a piece of food.
(define FRAME-INIT (frame WORM-INIT (make-posn 150 100)))

;; a Game-State is a Frame.
;; i.e. the movement of a worm and the Posn of a piece of food.

;;; functions

;; main function.

(define (game-main game-state)
  (big-bang game-state
            [to-draw game-render]
            [on-tick game-move GAME-SPEED]
            [on-key game-control]
            [stop-when worm-hurt? game-over]))
            ;; [state not-worm-hurt?]))

;; Game-State -> Image
;; renders the image of a worm and a piece of food onto GRID.

(check-expect
 (game-render FRAME-INIT)
 (place-images (list FOOD
                     WORM-HEAD)
               (list (make-posn (posn-x (frame-food FRAME-INIT))
                                (posn-y (frame-food FRAME-INIT)))
                     (make-posn (posn-x (worm-head (frame-worm FRAME-INIT)))
                                (posn-y (worm-head (frame-worm FRAME-INIT)))))
               (worm-body-render (worm-body (frame-worm FRAME-INIT)))))
(check-expect
 (game-render
  (frame (worm (make-posn 50 100)
               (list (make-posn 50 90)
                     (make-posn 60 90)
                     (make-posn 60 100)
                     (make-posn 60 110)
                     (make-posn 60 120)
                     (make-posn 60 130)
                     (make-posn 60 140))
               DOWN)
         (make-posn 150 100)))
 (place-images (list FOOD
                     WORM-HEAD)
               (list (make-posn 150 100)
                     (make-posn 50 100))
               (worm-body-render (list (make-posn 50 90)
                                       (make-posn 60 90)
                                       (make-posn 60 100)
                                       (make-posn 60 110)
                                       (make-posn 60 120)
                                       (make-posn 60 130)
                                       (make-posn 60 140)))))

(define (game-render game-state)
  (place-images (list FOOD
                      WORM-HEAD)
                (list (make-posn (posn-x (frame-food game-state))
                                 (posn-y (frame-food game-state)))
                      (make-posn (posn-x (worm-head (frame-worm game-state)))
                                 (posn-y (worm-head (frame-worm game-state)))))
                (worm-body-render (worm-body (frame-worm game-state)))))

;; List-of-Posns -> Image
;; renders the image of a worm's body onto GRID.

(check-expect (worm-body-render (worm-body (frame-worm FRAME-INIT))) GRID)
(check-expect
 (worm-body-render
  (list (make-posn 50 90)
        (make-posn 60 90)
        (make-posn 60 100)
        (make-posn 60 110)
        (make-posn 60 120)
        (make-posn 60 130)
        (make-posn 60 140)))
 (place-image WORM-SEGMENT 50 90
              (worm-body-render (cdr (list (make-posn 50 90)
                                           (make-posn 60 90)
                                           (make-posn 60 100)
                                           (make-posn 60 110)
                                           (make-posn 60 120)
                                           (make-posn 60 130)
                                           (make-posn 60 140))))))

(define (worm-body-render worm-body-posns)
  (cond [(empty? worm-body-posns) GRID]
        [else (place-image WORM-SEGMENT
                           (posn-x (car worm-body-posns))
                           (posn-y (car worm-body-posns))
                           (worm-body-render (cdr worm-body-posns)))]))

;; Game-State -> Game-State
;; does the following:
;; - moves a worm by WORM-VELOCITY per tick
;; - determines the Posn of a piece of food.

(check-expect
 (game-move FRAME-INIT)
 (game-move/old-food FRAME-INIT))
(check-random
 (game-move
  (frame (worm (make-posn 140 100)
               (list (make-posn 50 90)
                     (make-posn 60 90)
                     (make-posn 60 100)
                     (make-posn 60 110)
                     (make-posn 60 120)
                     (make-posn 60 130)
                     (make-posn 60 140))
               RIGHT)
         (make-posn 150 100)))
 (game-move/new-food
  (frame (worm (make-posn 140 100)
               (list (make-posn 50 90)
                     (make-posn 60 90)
                     (make-posn 60 100)
                     (make-posn 60 110)
                     (make-posn 60 120)
                     (make-posn 60 130)
                     (make-posn 60 140))
               RIGHT)
         (make-posn 150 100))))

(define (game-move game-state)
  (cond [(food-eaten? (worm-head (frame-worm game-state))
                      (worm-direction (frame-worm game-state))
                      (frame-food game-state))
         (game-move/new-food game-state)]
        [else (game-move/old-food game-state)]))

;; Posn Posn -> Posn
;; checks if a worm has eaten a piece of food.

(check-expect (food-eaten? (make-posn 50 100) RIGHT (make-posn 150 100)) #f)
(check-expect (food-eaten? (make-posn 140 100) RIGHT (make-posn 150 100)) #t)

(define (food-eaten? head-posn head-direction food-posn)
  (equal? (head-move head-posn head-direction) food-posn))

;; Game-State -> Game-State
;; does the following:
;; - moves a worm by WORM-VELOCITY
;; - places a new piece of food on the grid.

(check-random
 (game-move/new-food
  (frame (worm (make-posn 140 100)
               (list (make-posn 50 90)
                     (make-posn 60 90)
                     (make-posn 60 100)
                     (make-posn 60 110)
                     (make-posn 60 120)
                     (make-posn 60 130)
                     (make-posn 60 140))
               RIGHT)
         (make-posn 150 100)))
 (frame (worm (head-move (make-posn 140 100) RIGHT)
              (body-move/segment-add (make-posn 140 100)
                                     (list (make-posn 50 90)
                                           (make-posn 60 90)
                                           (make-posn 60 100)
                                           (make-posn 60 110)
                                           (make-posn 60 120)
                                           (make-posn 60 130)
                                           (make-posn 60 140)))
              RIGHT)
        (food-create (make-posn 150 100)
                     (make-posn 140 100)
                     (list (make-posn 50 90)
                           (make-posn 60 90)
                           (make-posn 60 100)
                           (make-posn 60 110)
                           (make-posn 60 120)
                           (make-posn 60 130)
                           (make-posn 60 140)))))

(define (game-move/new-food game-state)
  (frame (worm (head-move (worm-head (frame-worm game-state))
                          (worm-direction (frame-worm game-state)))
               (body-move/segment-add (worm-head (frame-worm game-state))
                                      (worm-body (frame-worm game-state)))
               (worm-direction (frame-worm game-state)))
         (food-create (frame-food game-state)
                      (worm-head (frame-worm game-state))
                      (worm-body (frame-worm game-state)))))

;; Game-State -> Game-State
;; does the following:
;; - moves a worm by WORM-VELOCITY
;; - retains the Posn of the old piece of food.

(check-expect
 (game-move/old-food FRAME-INIT)
 (frame (worm (head-move (make-posn 50 100) RIGHT)
              (body-move (make-posn 50 100) '())
              RIGHT)
        (make-posn 150 100)))
(check-expect
 (game-move/old-food
  (frame (worm (make-posn 50 100)
               (list (make-posn 50 90)
                     (make-posn 60 90)
                     (make-posn 60 100)
                     (make-posn 60 110)
                     (make-posn 60 120)
                     (make-posn 60 130)
                     (make-posn 60 140))
               DOWN)
         (make-posn 150 150)))
 (frame (worm (head-move (make-posn 50 100) DOWN)
              (body-move (make-posn 50 100)
                         (list (make-posn 50 90)
                               (make-posn 60 90)
                               (make-posn 60 100)
                               (make-posn 60 110)
                               (make-posn 60 120)
                               (make-posn 60 130)
                               (make-posn 60 140)))
              DOWN)
        (make-posn 150 150)))

(define (game-move/old-food game-state)
  (frame (worm (head-move (worm-head (frame-worm game-state))
                          (worm-direction (frame-worm game-state)))
               (body-move (worm-head (frame-worm game-state))
                          (worm-body (frame-worm game-state)))
               (worm-direction (frame-worm game-state)))
         (frame-food game-state)))

;; Posn Direction -> Posn
;; moves a worm's head in the given direction.

(check-expect
 (head-move (worm-head WORM-INIT) (worm-direction WORM-INIT))
 (make-posn 60 100))
(check-expect
 (head-move (make-posn 50 100) LEFT)
 (make-posn 40 100))
(check-expect
 (head-move (make-posn 50 100) UP)
 (make-posn 50 90))
(check-expect
 (head-move (make-posn 50 100) DOWN)
 (make-posn 50 110))

(define (head-move head-posn a-direction)
  (cond [(string=? a-direction LEFT)
         (make-posn (- (posn-x head-posn) WORM-VELOCITY) (posn-y head-posn))]
        [(string=? a-direction RIGHT)
         (make-posn (+ (posn-x head-posn) WORM-VELOCITY) (posn-y head-posn))]
        [(string=? a-direction UP)
         (make-posn (posn-x head-posn) (- (posn-y head-posn) WORM-VELOCITY))]
        [(string=? a-direction DOWN)
         (make-posn (posn-x head-posn) (+ (posn-y head-posn) WORM-VELOCITY))]))

;; Posn List-of-Posns -> List-of-Posns
;; moves a worm's head and adds a body segment according to
;; its head's movement.

(check-expect
 (body-move/segment-add
  (make-posn 50 100)
  (list (make-posn 50 90)
        (make-posn 60 90)
        (make-posn 60 100)
        (make-posn 60 110)
        (make-posn 60 120)
        (make-posn 60 130)
        (make-posn 60 140)))
 (list (make-posn 50 100)
       (make-posn 50 90)
       (make-posn 60 90)
       (make-posn 60 100)
       (make-posn 60 110)
       (make-posn 60 120)
       (make-posn 60 130)
       (make-posn 60 140)))

(define (body-move/segment-add head-posn body-posns)
  (cons head-posn body-posns))

;; List-of-Posns -> List-of-Posns
;; moves a worm's body according to its head's movement.

(check-expect (body-move (worm-head WORM-INIT) (worm-body WORM-INIT)) '())
(check-expect
 (body-move
  (make-posn 50 100)
  (list (make-posn 60 100)
        (make-posn 70 100)
        (make-posn 80 100)))
 (list (make-posn 50 100)
       (make-posn 60 100)
       (make-posn 70 100)))
(check-expect
 (body-move
  (make-posn 50 100)
  (list (make-posn 50 110)
        (make-posn 50 120)
        (make-posn 50 130)))
 (list (make-posn 50 100)
       (make-posn 50 110)
       (make-posn 50 120)))
(check-expect
 (body-move
  (make-posn 50 100)
  (list (make-posn 50 90)
        (make-posn 60 90)
        (make-posn 60 100)))
 (list (make-posn 50 100)
       (make-posn 50 90)
       (make-posn 60 90)))

(define (body-move head-posn body-posns)
  (cond [(empty? body-posns) '()]
        [else (cons head-posn (but-last body-posns))]))

;; Posn Posn List-of-Posns -> Posn
;; creates the Posn for a new piece of food.

(check-random (food-create (make-posn 150 100)
                           (make-posn 50 100)
                           (list (make-posn 50 90)
                                 (make-posn 60 90)
                                 (make-posn 60 100)))
              (food-check-create (make-posn 150 100)
                                 (posn-check-create (make-posn (x-coordinate-check-create (random (+ GRID-COLUMNS 1)))
                                                               (y-coordinate-check-create (random (+ GRID-ROWS 1))))
                                                    (make-posn 50 100)
                                                    (list (make-posn 50 90)
                                                          (make-posn 60 90)
                                                          (make-posn 60 100)))
                                 (make-posn 50 100)
                           (list (make-posn 50 90)
                                 (make-posn 60 90)
                                 (make-posn 60 100))))

(define (food-create old-food-posn head-posn body-posns)
  (food-check-create old-food-posn
                     (posn-check-create (make-posn (x-coordinate-check-create (random (+ GRID-COLUMNS 1)))
                                                   (y-coordinate-check-create (random (+ GRID-ROWS 1))))
                                        head-posn body-posns)
                     head-posn body-posns))

;; Posn Posn -> Posn
;; checks if two Posns are equal; if yes, generates a new Posn.

(check-random
 (food-check-create
  (make-posn 150 100)
  (posn-check-create (make-posn (x-coordinate-check-create (random (+ GRID-COLUMNS 1)))
                                (y-coordinate-check-create (random (+ GRID-ROWS 1))))
                     (make-posn 50 100)
                     (list (make-posn 50 90)
                           (make-posn 60 90)
                           (make-posn 60 100)))
  (make-posn 50 100)
  (list (make-posn 50 90)
        (make-posn 60 90)
        (make-posn 60 100)))
 (food-create (make-posn 150 100)
              (make-posn 50 100)
              (list (make-posn 50 90)
                    (make-posn 60 90)
                    (make-posn 60 100))))

(define (food-check-create posn-a posn-b head-posn body-posns)
  (cond [(equal? posn-a posn-b) (food-create posn-a head-posn body-posns)]
        [else posn-b]))

;; Posn Posn List-of-Posns -> Posn
;; returns a Posn different than a worm's head and body Posns.

(check-random
 (posn-check-create
  (make-posn 50 50)
  (make-posn 50 100)
  (list (make-posn 50 90)
        (make-posn 60 90)
        (make-posn 60 100)))
 (make-posn 50 50))
(check-random
 (posn-check-create
  (make-posn 50 100)
  (make-posn 50 100)
  (list (make-posn 50 90)
        (make-posn 60 90)
        (make-posn 60 100)))
 (posn-check-create (make-posn (x-coordinate-check-create (random (+ GRID-COLUMNS 1)))
                               (y-coordinate-check-create (random (+ GRID-ROWS 1))))
                    (make-posn 50 100)
                    (list (make-posn 50 90)
                          (make-posn 60 90)
                          (make-posn 60 100))))

(define (posn-check-create food-posn head-posn body-posns)
  (cond [(not (boolean? (member food-posn (cons head-posn body-posns))))
         (posn-check-create (make-posn (x-coordinate-check-create (random (+ GRID-COLUMNS 1)))
                                       (y-coordinate-check-create (random (+ GRID-ROWS 1))))
                            head-posn body-posns)]
        [else food-posn]))

;; Number -> Number
;; returns an x-coordinate that is valid. a valid x-coordinate is:
;; - a Number divisible by WORM-DIAMETER and
;; - a Number > GRID-LIMIT-LEFT and
;; - a Number < GRID-LIMIT-RIGHT.

(check-random
 (x-coordinate-check-create (random (+ GRID-COLUMNS 1)))
 (x-coordinate-check-create (random (+ GRID-COLUMNS 1))))

(define (x-coordinate-check-create x-coordinate)
  (cond [(and (= (modulo x-coordinate WORM-DIAMETER) 0)
              (> x-coordinate GRID-LIMIT-LEFT)
              (< x-coordinate GRID-LIMIT-RIGHT))
         x-coordinate]
        [else (x-coordinate-check-create (random (+ GRID-COLUMNS 1)))]))

;; Number -> Number
;; returns an y-coordinate that is valid. a valid y-coordinate is:
;; - a Number divisible by WORM-DIAMETER and
;; - a Number > GRID-LIMIT-UP and
;; - a Number < GRID-LIMIT-DOWN.

(check-random
 (y-coordinate-check-create (random (+ GRID-ROWS 1)))
 (y-coordinate-check-create (random (+ GRID-ROWS 1))))

(define (y-coordinate-check-create y-coordinate)
  (cond [(and (= (modulo y-coordinate WORM-DIAMETER) 0)
              (> y-coordinate GRID-LIMIT-UP)
              (< y-coordinate GRID-LIMIT-DOWN))
         y-coordinate]
        [else (y-coordinate-check-create (random (+ GRID-ROWS 1)))]))

;; List -> List
;; returns all but the last element of a list.

(check-expect (but-last '()) '())
(check-expect (but-last (list 1 2 3)) (list 1 2))
(check-expect (but-last (list "a" "b" "c" "d")) (list "a" "b" "c"))

(define (but-last a-list)
  (cond [(empty? a-list) '()]
        [else (reverse (cdr (reverse a-list)))]))

;; Game-State -> Game-State
;; updates a worm's direction according to given Key-Event.

(check-expect
 (game-control FRAME-INIT "down")
 (frame (worm (make-posn 50 100)
              '()
              DOWN)
        (make-posn 150 100)))
(check-expect
 (game-control
  (frame (worm (make-posn 50 100)
               (list (make-posn 60 100)
                     (make-posn 70 100)
                     (make-posn 80 100))
               LEFT)
         (make-posn 150 100))
  "up")
 (frame (worm (make-posn 50 100)
              (list (make-posn 60 100)
                    (make-posn 70 100)
                    (make-posn 80 100))
              UP)
        (make-posn 150 100)))
(check-expect
 (game-control
  (frame (worm (make-posn 50 100)
               (list (make-posn 50 110)
                     (make-posn 50 120)
                     (make-posn 50 130))
               UP)
         (make-posn 150 100))
  "right")
 (frame (worm (make-posn 50 100)
              (list (make-posn 50 110)
                    (make-posn 50 120)
                    (make-posn 50 130))
              RIGHT)
        (make-posn 150 100)))
(check-expect
 (game-control
  (frame (worm (make-posn 50 100)
               (list (make-posn 50 90)
                     (make-posn 60 90)
                     (make-posn 60 100))
               DOWN)
         (make-posn 150 100))
  "left")
 (frame (worm (make-posn 50 100)
              (list (make-posn 50 90)
                    (make-posn 60 90)
                    (make-posn 60 100))
              LEFT)
        (make-posn 150 100)))
(check-expect
 (game-control
  (frame (worm (make-posn 50 100)
               (list (make-posn 50 90)
                     (make-posn 60 90)
                     (make-posn 60 100))
               DOWN)
         (make-posn 150 100))
  "o")
 (frame (worm (make-posn 50 100)
              (list (make-posn 50 90)
                    (make-posn 60 90)
                    (make-posn 60 100))
              DOWN)
        (make-posn 150 100)))

(define (game-control game-state key-event)
  (frame (worm (worm-head (frame-worm game-state))
               (worm-body (frame-worm game-state))
               ((cond [(empty? (worm-body (frame-worm game-state))) worm-direction-change/empty]
                      [else worm-direction-change])
                (worm-direction (frame-worm game-state)) key-event))
         (frame-food game-state)))

;; Direction Key-Event -> Direction
;; changes the direction of a worm without a body.

(check-expect (worm-direction-change (worm-direction WORM-INIT) "down") DOWN)

(define (worm-direction-change/empty direction key-event)
  (cond [(string=? key-event LEFT) LEFT]
        [(string=? key-event RIGHT) RIGHT]
        [(string=? key-event UP) UP]
        [(string=? key-event DOWN) DOWN]
        [else direction]))

;; Direction Key-Event -> Direction
;; changes the direction of a worm with a body.

(check-expect (worm-direction-change LEFT "up") UP)
(check-expect (worm-direction-change UP "right") RIGHT)
(check-expect (worm-direction-change DOWN "left") LEFT)
(check-expect (worm-direction-change DOWN "o") DOWN)

(define (worm-direction-change direction key-event)
  (cond [(and (string=? key-event LEFT) (not (string=? direction RIGHT))) LEFT]
        [(and (string=? key-event RIGHT) (not (string=? direction LEFT))) RIGHT]
        [(and (string=? key-event UP) (not (string=? direction DOWN))) UP]
        [(and (string=? key-event DOWN) (not (string=? direction UP))) DOWN]
        [else direction]))

;; Game-State -> Boolean
;; stops the game when one of the following is true:
;; - the worm has run into a wall
;; - the worm has run into itself.

(check-expect (worm-hurt? (frame (worm (make-posn GRID-LIMIT-LEFT (/ GRID-ROWS 2)) '() LEFT) (make-posn 100 100))) #t)
(check-expect (worm-hurt? (frame (worm (make-posn GRID-LIMIT-RIGHT (/ GRID-ROWS 2)) '() LEFT) (make-posn 100 100))) #t)
(check-expect (worm-hurt? (frame (worm (make-posn (/ GRID-COLUMNS 2) GRID-LIMIT-UP) '() LEFT) (make-posn 100 100))) #t)
(check-expect (worm-hurt? (frame (worm (make-posn (/ GRID-COLUMNS 2) GRID-LIMIT-DOWN) '() LEFT) (make-posn 100 100))) #t)
(check-expect
 (worm-hurt?
  (frame (worm (make-posn 50 100)
               (list (make-posn 50 90)
                     (make-posn 60 90)
                     (make-posn 60 100)
                     (make-posn 60 110)
                     (make-posn 60 120))
               RIGHT)
         (make-posn 100 100)))
 #t)
(check-expect (worm-hurt? FRAME-INIT) #f)

(define (worm-hurt? game-state)
  (or (worm-hit-wall? (worm-head (frame-worm game-state)))
      (worm-hit-body? (frame-worm game-state))))

;; Posn -> Boolean
;; checks if the worm has run into a wall.

(check-expect (worm-hit-wall? (make-posn GRID-LIMIT-LEFT (/ GRID-ROWS 2))) #t)
(check-expect (worm-hit-wall? (make-posn GRID-LIMIT-RIGHT (/ GRID-ROWS 2))) #t)
(check-expect (worm-hit-wall? (make-posn (/ GRID-COLUMNS 2) GRID-LIMIT-UP)) #t)
(check-expect (worm-hit-wall? (make-posn (/ GRID-COLUMNS 2) GRID-LIMIT-DOWN)) #t)
(check-expect (worm-hit-wall? (worm-head WORM-INIT)) #f)

(define (worm-hit-wall? head)
  (or (<= (posn-x head) GRID-LIMIT-LEFT)
      (>= (posn-x head) GRID-LIMIT-RIGHT)
      (<= (posn-y head) GRID-LIMIT-UP)
      (>= (posn-y head) GRID-LIMIT-DOWN)))

;; Worm -> Boolean
;; checks if the worm has run into its own body.

(check-expect
 (worm-hit-body?
  (worm (make-posn 50 100)
        (list (make-posn 50 90)
              (make-posn 60 90)
              (make-posn 60 100)
              (make-posn 60 110)
              (make-posn 60 120))
        DOWN))
 #f)
(check-expect
 (worm-hit-body?
  (worm (make-posn 50 100)
        (list (make-posn 50 90)
              (make-posn 60 90)
              (make-posn 60 100)
              (make-posn 60 110)
              (make-posn 60 120))
        RIGHT))
 #t)

(define (worm-hit-body? worm)
  (not (boolean? (member (head-move (worm-head worm) (worm-direction worm))
                         (body-move (worm-head worm) (worm-body worm))))))

;; Game-State -> Image
;; renders the gameover screen of the game.

(check-expect
 (game-over (frame (worm (make-posn GRID-LIMIT-LEFT (/ GRID-ROWS 2)) '() LEFT) (make-posn 100 100)))
 (place-image (game-over-message (worm (make-posn GRID-LIMIT-LEFT (/ GRID-ROWS 2)) '() LEFT))
              (posn-x GAME-OVER-POSN)
              (posn-y GAME-OVER-POSN)
              (game-render (frame (worm (make-posn GRID-LIMIT-LEFT (/ GRID-ROWS 2)) '() LEFT) (make-posn 100 100)))))
(check-expect
 (game-over
  (frame (worm (make-posn 50 100)
               (list (make-posn 50 90)
                     (make-posn 60 90)
                     (make-posn 60 100)
                     (make-posn 60 110)
                     (make-posn 60 120))
               RIGHT)
         (make-posn 100 100)))
  (place-image (game-over-message
                (worm (make-posn 50 100)
                      (list (make-posn 50 90)
                            (make-posn 60 90)
                            (make-posn 60 100)
                            (make-posn 60 110)
                            (make-posn 60 120))
                      RIGHT))
               (posn-x GAME-OVER-POSN)
               (posn-y GAME-OVER-POSN)
               (game-render
                (frame (worm (make-posn 50 100)
                             (list (make-posn 50 90)
                                   (make-posn 60 90)
                                   (make-posn 60 100)
                                   (make-posn 60 110)
                                   (make-posn 60 120))
                             RIGHT)
                       (make-posn 100 100)))))

(define (game-over game-state)
  (place-image (game-over-message (frame-worm game-state))
               (posn-x GAME-OVER-POSN)
               (posn-y GAME-OVER-POSN)
               (game-render game-state)))

;; Worm -> Image
;; renders the gameover message.

(check-expect
 (game-over-message (worm (make-posn GRID-LIMIT-LEFT (/ GRID-ROWS 2)) '() LEFT))
 (text (string-append GAME-OVER/WALL "0") GAME-OVER-FONT-SIZE GAME-OVER-FONT-COLOR))
(check-expect
 (game-over-message
  (worm (make-posn 50 100)
        (list (make-posn 50 90)
              (make-posn 60 90)
              (make-posn 60 100)
              (make-posn 60 110)
              (make-posn 60 120))
        RIGHT))
 (text (string-append GAME-OVER/BODY "5") GAME-OVER-FONT-SIZE GAME-OVER-FONT-COLOR))

(define (game-over-message worm)
  (text (string-append
         (cond [(worm-hit-body? worm) GAME-OVER/BODY]
               [else GAME-OVER/WALL])
         (number->string (length (worm-body worm))))
        GAME-OVER-FONT-SIZE GAME-OVER-FONT-COLOR))

;; Game-State -> Boolean
;; used for state function.
;; checks if the user has not lost.

(check-expect (not-worm-hurt? FRAME-INIT) #t)
(check-expect
 (not-worm-hurt?
  (frame (worm (make-posn 50 100)
               (list (make-posn 50 90)
                     (make-posn 60 90)
                     (make-posn 60 100)
                     (make-posn 60 110)
                     (make-posn 60 120))
               RIGHT)
         (make-posn 100 100)))
 #f)

(define (not-worm-hurt? game-state)
  (not (worm-hurt? game-state)))

;;; application

(test)
(game-main FRAME-INIT)
