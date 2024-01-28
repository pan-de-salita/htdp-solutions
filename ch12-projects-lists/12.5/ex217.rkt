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

(define GAME-SPEED 0.5)

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
;; - the posn of each of the worm's body segments
;; - the direction it's travelling towards.
(define WORM-INIT (worm (make-posn 50 100) '() RIGHT))

;; a Game-State is a Worm.
;; i.e. the movement of a Worm.

;;; functions

;; main function.

(define (worm-main game-state)
  (big-bang game-state
            [to-draw worm-render]
            [on-tick worm-move GAME-SPEED]
            [on-key worm-control]))

;; Game-State -> Image
;; renders the image of a worm onto GRID.

(check-expect
 (worm-render WORM-INIT)
 (place-image WORM-HEAD 50 100 GRID))
(check-expect
 (worm-render
  (worm (make-posn 50 100)
        (list (make-posn 40 100)
              (make-posn 30 100)
              (make-posn 20 100))
        RIGHT))
 (place-image
  WORM-SEGMENT 40 100
  (worm-render
   (worm (make-posn 50 100)
         (list (make-posn 30 100)
               (make-posn 20 100))
         RIGHT))))

(define (worm-render game-state)
  (cond [(empty? (worm-body game-state))
         (place-image WORM-HEAD
                      (posn-x (worm-head game-state))
                      (posn-y (worm-head game-state))
                      GRID)]
        [else (place-image WORM-SEGMENT
                           (posn-x (car (worm-body game-state)))
                           (posn-y (car (worm-body game-state)))
                           (worm-render (worm (worm-head game-state)
                                              (cdr (worm-body game-state))
                                              (worm-direction game-state))))]))

;; Game-State -> Game-State
;; moves a worm by WORM-VELOCITY per tick.

(check-expect
 (worm-move WORM-INIT)
 (worm (make-posn 60 100)
       '()
       RIGHT))
(check-expect
 (worm-move
  (worm (make-posn 50 100)
        (list (make-posn 60 100)
              (make-posn 70 100)
              (make-posn 80 100))
        LEFT))
 (worm (make-posn 40 100)
       (list (make-posn 50 100)
             (make-posn 60 100)
             (make-posn 70 100))
       LEFT))
(check-expect
 (worm-move
  (worm (make-posn 50 100)
        (list (make-posn 50 110)
              (make-posn 50 120)
              (make-posn 50 130))
        UP))
 (worm (make-posn 50 90)
       (list (make-posn 50 100)
             (make-posn 50 110)
             (make-posn 50 120))
       UP))
(check-expect
 (worm-move
  (worm (make-posn 50 100)
        (list (make-posn 50 90)
              (make-posn 60 90)
              (make-posn 60 100))
        DOWN))
 (worm (make-posn 50 110)
       (list (make-posn 50 100)
             (make-posn 50 90)
             (make-posn 60 90))
       DOWN))

(define (worm-move game-state)
  (worm (head-move (worm-head game-state) (worm-direction game-state))
        (body-move (worm-head game-state) (worm-body game-state))
        (worm-direction game-state)))

;; Posn -> Posn
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
 (worm-control WORM-INIT "down")
 (worm (make-posn 50 100)
       '()
       DOWN))
(check-expect
 (worm-control
  (worm (make-posn 50 100)
        (list (make-posn 60 100)
              (make-posn 70 100)
              (make-posn 80 100))
        LEFT)
  "up")
 (worm (make-posn 50 100)
       (list (make-posn 60 100)
             (make-posn 70 100)
             (make-posn 80 100))
       UP))
(check-expect
 (worm-control
  (worm (make-posn 50 100)
        (list (make-posn 50 110)
              (make-posn 50 120)
              (make-posn 50 130))
        UP)
  "right")
 (worm (make-posn 50 100)
       (list (make-posn 50 110)
             (make-posn 50 120)
             (make-posn 50 130))
       RIGHT))
(check-expect
 (worm-control
  (worm (make-posn 50 100)
        (list (make-posn 50 90)
              (make-posn 60 90)
              (make-posn 60 100))
        DOWN)
  "left")
 (worm (make-posn 50 100)
       (list (make-posn 50 90)
             (make-posn 60 90)
             (make-posn 60 100))
       LEFT))
(check-expect
 (worm-control
  (worm (make-posn 50 100)
        (list (make-posn 50 90)
              (make-posn 60 90)
              (make-posn 60 100))
        DOWN)
  "o")
 (worm (make-posn 50 100)
       (list (make-posn 50 90)
             (make-posn 60 90)
             (make-posn 60 100))
       DOWN))

(define (worm-control game-state key-event)
  (worm (worm-head game-state)
        (worm-body game-state)
        ((cond [(empty? worm-body) direction-change/empty]
               [else direction-change])
         (worm-direction game-state) key-event)))

;; Direction Key-Event -> Direction
;; changes the direction of a worm without a body.

(check-expect (direction-change (worm-direction WORM-INIT) "down") DOWN)

(define (direction-change/empty direction key-event)
  (cond [(string=? key-event LEFT) LEFT]
        [(string=? key-event RIGHT) RIGHT]
        [(string=? key-event UP) UP]
        [(string=? key-event DOWN) DOWN]
        [else direction]))

;; Direction Key-Event -> Direction
;; changes the direction of a worm with a body.

(check-expect (direction-change LEFT "up") UP)
(check-expect (direction-change UP "right") RIGHT)
(check-expect (direction-change DOWN "left") LEFT)
(check-expect (direction-change DOWN "o") DOWN)

(define (direction-change direction key-event)
  (cond [(and (string=? key-event LEFT) (not (string=? direction RIGHT))) LEFT]
        [(and (string=? key-event RIGHT) (not (string=? direction LEFT))) RIGHT]
        [(and (string=? key-event UP) (not (string=? direction DOWN))) UP]
        [(and (string=? key-event DOWN) (not (string=? direction UP))) DOWN]
        [else direction]))

;;; application

(test)
;; (worm-main WORM-INIT)

;; unused for now:

;; ;; Game-State -> Boolean
;; ;; stops the game when one of the following is true:
;; ;; - the worm has run into a wall
;; ;; - the worm has run into itself.

;; (check-expect (worm-hurt? (worm (make-posn GRID-LIMIT-LEFT (/ GRID-ROWS 2)) '() LEFT)) #t)
;; (check-expect (worm-hurt? (worm (make-posn GRID-LIMIT-RIGHT (/ GRID-ROWS 2)) '() LEFT)) #t)
;; (check-expect (worm-hurt? (worm (make-posn (/ GRID-COLUMNS 2) GRID-LIMIT-UP) '() LEFT)) #t)
;; (check-expect (worm-hurt? (worm (make-posn (/ GRID-COLUMNS 2) GRID-LIMIT-DOWN) '() LEFT)) #t)
;; (check-expect (worm-hurt? WORM-INIT) #f)

;; (define (worm-hurt? game-state)
;;   (worm-hit-wall? (worm-head game-state)))

;; ;; Game-State -> Boolean
;; ;; checks if the worm has run into a wall.

;; (check-expect (worm-hit-wall? (make-posn GRID-LIMIT-LEFT (/ GRID-ROWS 2))) #t)
;; (check-expect (worm-hit-wall? (make-posn GRID-LIMIT-RIGHT (/ GRID-ROWS 2))) #t)
;; (check-expect (worm-hit-wall? (make-posn (/ GRID-COLUMNS 2) GRID-LIMIT-UP)) #t)
;; (check-expect (worm-hit-wall? (make-posn (/ GRID-COLUMNS 2) GRID-LIMIT-DOWN)) #t)
;; (check-expect (worm-hit-wall? (worm-head WORM-INIT)) #f)

;; (define (worm-hit-wall? head)
;;   (or (<= (posn-x head) GRID-LIMIT-LEFT)
;;       (>= (posn-x head) GRID-LIMIT-RIGHT)
;;       (<= (posn-y head) GRID-LIMIT-UP)
;;       (>= (posn-y head) GRID-LIMIT-DOWN)))

;; ;; Game-State -> Image
;; ;; renders the gameover screen of the game.

;; (check-expect
;;  (game-over (worm (make-posn GRID-LIMIT-LEFT (/ GRID-ROWS 2)) '() LEFT))
;;  (place-image (game-over-text (worm-body (worm (make-posn GRID-LIMIT-LEFT (/ GRID-ROWS 2)) '() LEFT)))
;;               (posn-x GAME-OVER-POSN)
;;               (posn-y GAME-OVER-POSN)
;;               (worm-render (worm (make-posn GRID-LIMIT-LEFT (/ GRID-ROWS 2)) '() LEFT))))

;; (define (game-over game-state)
;;   (place-image (game-over-text (worm-body game-state))
;;                (posn-x GAME-OVER-POSN)
;;                (posn-y GAME-OVER-POSN)
;;                (worm-render game-state)))

;; ;; List-of-Posns -> Image
;; ;; renders the gameover message.

;; (check-expect
;;  (game-over-text '())
;;  (text "worm hit wall: 0" GAME-OVER-FONT-SIZE GAME-OVER-FONT-COLOR))
;; (check-expect
;;  (game-over-text (list (make-posn (* GRID-CELL 5) (* GRID-CELL 10))))
;;  (text "worm hit wall: 1" GAME-OVER-FONT-SIZE GAME-OVER-FONT-COLOR))
;; (check-expect
;;  (game-over-text
;;   (list (make-posn (* GRID-CELL 5) (* GRID-CELL 10))
;;         (make-posn (+ 10 (* GRID-CELL 5)) (* GRID-CELL 10))
;;         (make-posn (+ 20 (* GRID-CELL 5)) (* GRID-CELL 10))))
;;  (text "worm hit wall: 3" GAME-OVER-FONT-SIZE GAME-OVER-FONT-COLOR))

;; (define (game-over-text list-of-posns)
;;   (text (string-append "worm hit wall: " (number->string (length list-of-posns)))
;;         GAME-OVER-FONT-SIZE GAME-OVER-FONT-COLOR))
