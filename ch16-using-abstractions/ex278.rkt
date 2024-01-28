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

(define GRID-LIMIT GRID-COLUMNS)

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
;; renders WORM and FOOD onto GRID

(check-expect
 (game-render FRAME-INIT)
 (place-images (list FOOD
                     WORM-HEAD)
               (list (make-posn (posn-x (frame-food FRAME-INIT))
                                (posn-y (frame-food FRAME-INIT)))
                     (make-posn (posn-x (worm-head (frame-worm FRAME-INIT)))
                                (posn-y (worm-head (frame-worm FRAME-INIT)))))
               GRID))
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
               (foldr (lambda (worm-body-posn an-image)
                        (place-images (list WORM-SEGMENT)
                                      (list worm-body-posn)
                                      an-image))
                      GRID
                      (list (make-posn 50 90)
                            (make-posn 60 90)
                            (make-posn 60 100)
                            (make-posn 60 110)
                            (make-posn 60 120)
                            (make-posn 60 130)
                            (make-posn 60 140)))))

(define (game-render game-state)
  (local ((define worm-body-rendered
            (foldr (lambda (worm-body-posn an-image)
                     (place-images (list WORM-SEGMENT)
                                   (list worm-body-posn)
                                   an-image))
                   GRID
                   (worm-body (frame-worm game-state)))))
    (place-images (list FOOD WORM-HEAD)
                  (list (frame-food game-state)
                        (worm-head (frame-worm game-state)))
                  worm-body-rendered)))

;; Game-State -> Game-State
;; does the followng:
;; - moves WORM by WORM-VELOCITY per tick
;; - determines the POSN of a FOOD

(check-expect
 (game-move FRAME-INIT)
 (game-move/existing-food FRAME-INIT))
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
         (make-posn 140 100)))
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
         (make-posn 140 100))))

(define (game-move game-state)
  (local ((define food-eaten?
            (equal? (worm-head (frame-worm game-state))
                    (frame-food game-state))))
    (cond [food-eaten? (game-move/new-food game-state)]
          [else (game-move/existing-food game-state)])))

;; Posn Direction -> Posn
;; moves WORM-HEAD in a-direction

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

;; Game-State -> Game-State
;; does the following:
;; - moves WORM by WORM-VELOCITY
;; - places a new FOOD on GRID

(check-random
 (game-move/new-food FRAME-INIT)
 (frame (worm (head-move (make-posn 50 100) RIGHT)
              (cons (make-posn 50 100) '())
              RIGHT)
        (food-create/test (make-posn 150 100)
                          (list (head-move (make-posn 50 100) RIGHT)
                                (cons (make-posn 50 100) '())))))
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
         (make-posn 80 210)))
 (frame (worm (head-move (make-posn 140 100) RIGHT)
              (cons (make-posn 140 100)
                    (list (make-posn 50 90)
                          (make-posn 60 90)
                          (make-posn 60 100)
                          (make-posn 60 110)
                          (make-posn 60 120)
                          (make-posn 60 130)
                          (make-posn 60 140)))
              RIGHT)
        (food-create/test (make-posn 80 210)
                          (cons (head-move (make-posn 140 100) RIGHT)
                                (cons (make-posn 140 100)
                                      (list (make-posn 50 90)
                                            (make-posn 60 90)
                                            (make-posn 60 100)
                                            (make-posn 60 110)
                                            (make-posn 60 120)
                                            (make-posn 60 130)
                                            (make-posn 60 140)))))))

(define (game-move/new-food game-state)
  (local ((define head-move/new-food
            (head-move (worm-head (frame-worm game-state))
                       (worm-direction (frame-worm game-state))))
          (define body-move/new-food
            (cons (worm-head (frame-worm game-state))
                  (worm-body (frame-worm game-state))))
          ;; Posn -> Posn
          ;; creates the Posn for new FOOD
          (define (food-create old-food-posn)
            (local (;; a random Posn for new FOOD
                    (define test-food-posn
                      (make-posn
                       (* GRID-CELL (random 1 (/ GRID-COLUMNS GRID-CELL)))
                       (* GRID-CELL (random 1 (/ GRID-COLUMNS GRID-CELL)))))
                    ;; Posn -> Posn
                    ;; returns a FOOD Posn that is not:
                    ;; - equal to old-food-posn
                    ;; - of the same Posn as WORM in next frame
                    (define (food-check-create a-test-food-posn)
                      (local ((define future-worm-posn
                                (cons head-move/new-food body-move/new-food)))
                        (cond [(or (equal? old-food-posn a-test-food-posn)
                                   (own-member? a-test-food-posn future-worm-posn))
                               (food-create old-food-posn)]
                              [else a-test-food-posn]))))
              (food-check-create test-food-posn))))
    (frame (worm head-move/new-food
                 body-move/new-food
                 (worm-direction (frame-worm game-state)))
           (food-create (frame-food game-state)))))

;; X [List-of X] -> Boolean
;; checks if x is an element of a-list

(check-expect (own-member? 1 (list 1 2 3)) #t)
(check-expect (own-member? 0 (list 1 2 3)) #f)

(define (own-member? x a-list)
  (ormap
   (lambda (list-item) (equal? list-item x))
   a-list))

;; Posn Posn [List-of Posn] -> Posn
;; for testing food-create

(check-satisfied
 (food-create/test (make-posn 80 210) (list (make-posn 10 10)))
 (lambda (a-posn) (or (not (equal? a-posn (make-posn 80 210)))
                      (not (equal? a-posn (make-posn 10 10))))))

(define (food-create/test old-food-posn worm-posn)
  (local (;; creates Posn for new FOOD
          (define test-food-posn
            (make-posn
             (* GRID-CELL (random 1 (/ GRID-COLUMNS GRID-CELL)))
             (* GRID-CELL (random 1 (/ GRID-COLUMNS GRID-CELL)))))
          ;; Posn -> Posn
          ;; returns a FOOD Posn that is not:
          ;; - equal to old-food-posn
          ;; - of the same Posn as WORM in next frame
          (define (food-check-create a-test-food-posn)
            (cond [(or (equal? old-food-posn a-test-food-posn)
                       (own-member? a-test-food-posn worm-posn))
                   (food-create/test old-food-posn worm-posn)]
                  [else a-test-food-posn])))
    (food-check-create test-food-posn)))

;; Game-State -> Game-State
;; does the following:
;; - moves WORM by WORM-VELOCITY
;; - retains the Posn of existing FOOD

(check-expect
 (game-move/existing-food FRAME-INIT)
 (frame (worm (head-move (make-posn 50 100) RIGHT)
              '()
              RIGHT)
        (make-posn 150 100)))
(check-expect
 (game-move/existing-food
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
              (append (list (make-posn 50 100))
                      (list (make-posn 50 90)
                            (make-posn 60 90)
                            (make-posn 60 100)
                            (make-posn 60 110)
                            (make-posn 60 120)
                            (make-posn 60 130)))
              DOWN)
        (make-posn 150 150)))

(define (game-move/existing-food game-state)
  (frame (worm (head-move (worm-head (frame-worm game-state))
                          (worm-direction (frame-worm game-state)))
               (body-move (worm-head (frame-worm game-state))
                          (worm-body (frame-worm game-state)))
               (worm-direction (frame-worm game-state)))
         (frame-food game-state)))

;; Posn [List-of Posn] -> [List-of Posn]
;; moves WORM's body according to its head's movement

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
  (but-last (cons head-posn body-posns)))

;; [NEList-of X] -> [NEList-of X]
;; returns all but the last element of a [NEList-of X]

(check-expect
 (but-last (list "first" "second" "third" "last"))
 (list "first" "second" "third"))

(define (but-last l-x)
  (reverse (cdr (reverse l-x))))

;; Game-State -> Game-State
;; updates WORM's direction according to given Key-Event

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
  (local (;; Directon Key-Event -> Direction
          ;; changes the directon of WORM without a body
          (define (worm-direction-change/empty direction key-event)
            (cond [(string=? key-event LEFT) LEFT]
                  [(string=? key-event RIGHT) RIGHT]
                  [(string=? key-event UP) UP]
                  [(string=? key-event DOWN) DOWN]
                  [else direction]))
          ;; Direction Key-Event -> Direction
          ;; changes the direction of a worm with a body
          (define (worm-direction-change direction key-event)
            (cond [(and (string=? key-event LEFT) (not (string=? direction RIGHT))) LEFT]
                  [(and (string=? key-event RIGHT) (not (string=? direction LEFT))) RIGHT]
                  [(and (string=? key-event UP) (not (string=? direction DOWN))) UP]
                  [(and (string=? key-event DOWN) (not (string=? direction UP))) DOWN]
                  [else direction])))
    (frame (worm (worm-head (frame-worm game-state))
                 (worm-body (frame-worm game-state))
                 ((cond [(empty? (worm-body (frame-worm game-state))) worm-direction-change/empty]
                        [else worm-direction-change])
                  (worm-direction (frame-worm game-state)) key-event))
           (frame-food game-state))))

;; Game-State -> Boolean
;; stops the game when one of the following is true:
;; - WORM has run into a wall
;; - WORM has run into itself

(check-expect (worm-hurt? (frame (worm (make-posn 0 (/ GRID-ROWS 2)) '() LEFT) (make-posn 100 100))) #t)
(check-expect (worm-hurt? (frame (worm (make-posn GRID-LIMIT (/ GRID-ROWS 2)) '() LEFT) (make-posn 100 100))) #t)
(check-expect (worm-hurt? (frame (worm (make-posn (/ GRID-COLUMNS 2) 0) '() LEFT) (make-posn 100 100))) #t)
(check-expect (worm-hurt? (frame (worm (make-posn (/ GRID-COLUMNS 2) GRID-LIMIT) '() LEFT) (make-posn 100 100))) #t)
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
      (worm-hit-body? (head-move (worm-head (frame-worm game-state))
                                 (worm-direction (frame-worm game-state)))
                      (body-move (worm-head (frame-worm game-state))
                                 (worm-body (frame-worm game-state))))))

;; Posn -> Boolean
;; checks if WORM has hit a wall

(check-expect (worm-hit-wall? (make-posn 0 (/ GRID-ROWS 2))) #t)
(check-expect (worm-hit-wall? (make-posn GRID-LIMIT (/ GRID-ROWS 2))) #t)
(check-expect (worm-hit-wall? (make-posn (/ GRID-COLUMNS 2) 0)) #t)
(check-expect (worm-hit-wall? (make-posn (/ GRID-COLUMNS 2) GRID-LIMIT)) #t)
(check-expect (worm-hit-wall? (worm-head WORM-INIT)) #f)

(define (worm-hit-wall? head-posn)
  (or (<= (posn-x head-posn) 0)
      (>= (posn-x head-posn) GRID-LIMIT)
      (<= (posn-y head-posn) 0)
      (>= (posn-y head-posn) GRID-LIMIT)))

;; Posn [List-of Posn] -> Boolean
;; checks if WORM has run into its own body

(define (worm-hit-body? head-posn body-posns)
  (own-member? head-posn body-posns))

;; Game-State -> Image
;; renders the gameover screen of the game

(check-expect
 (game-over (frame (worm (make-posn 0 (/ GRID-ROWS 2)) '() LEFT) (make-posn 100 100)))
 (place-image (text (string-append GAME-OVER/WALL "0") GAME-OVER-FONT-SIZE GAME-OVER-FONT-COLOR)
              (posn-x GAME-OVER-POSN)
              (posn-y GAME-OVER-POSN)
              (game-render (frame (worm (make-posn 0 (/ GRID-ROWS 2)) '() LEFT) (make-posn 100 100)))))
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
 (place-image (text (string-append GAME-OVER/BODY "5") GAME-OVER-FONT-SIZE GAME-OVER-FONT-COLOR)
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
  (local (;; Worm -> Image
          ;; renders the gameover message.
          (define (game-over-message worm)
            (text (string-append
                   (cond [(worm-hit-body?
                           (head-move (worm-head (frame-worm game-state))
                                      (worm-direction (frame-worm game-state)))
                           (body-move (worm-head (frame-worm game-state))
                                      (worm-body (frame-worm game-state))))
                          GAME-OVER/BODY]
                         [else GAME-OVER/WALL])
                   (number->string (length (worm-body worm))))
                  GAME-OVER-FONT-SIZE GAME-OVER-FONT-COLOR)))
    (place-image (game-over-message (frame-worm game-state))
                 (posn-x GAME-OVER-POSN)
                 (posn-y GAME-OVER-POSN)
                 (game-render game-state))))

;;; application

(test)
