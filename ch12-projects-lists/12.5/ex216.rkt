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
            [on-key worm-control]
            [stop-when worm-hurt? game-over]))

;; Game-State -> Image
;; renders the image of a Worm onto GRID.

(check-expect
 (worm-render (worm (make-posn GRID-CELL (/ GRID-ROWS 2)) '() RIGHT))
 (place-image WORM-HEAD GRID-CELL (/ GRID-ROWS 2) GRID))
(check-expect
 (worm-render (worm (make-posn (/ GRID-COLUMNS 4) (/ GRID-ROWS 2)) '() RIGHT))
 (place-image WORM-HEAD (/ GRID-COLUMNS 4) (/ GRID-ROWS 2) GRID))
(check-expect
 (worm-render (worm (make-posn (- GRID-COLUMNS GRID-CELL) (/ GRID-ROWS 2)) '() RIGHT))
 (place-image WORM-HEAD (- GRID-COLUMNS GRID-CELL) (/ GRID-ROWS 2) GRID))

(define (worm-render game-state)
  (place-image WORM-HEAD (posn-x (worm-head game-state)) (posn-y (worm-head game-state)) GRID))

;; Game-State -> Game-State
;; moves a Worm by WORM-VELOCITY per tick.

(check-expect
 (worm-move (worm (make-posn (/ GRID-COLUMNS 4) (/ GRID-ROWS 2)) '() LEFT))
 (worm (make-posn (- (/ GRID-COLUMNS 4) WORM-VELOCITY) (/ GRID-ROWS 2))
       '()
       "left"))
(check-expect
 (worm-move (worm (make-posn (/ GRID-COLUMNS 4) (/ GRID-ROWS 2)) '() RIGHT))
 (worm (make-posn (+ (/ GRID-COLUMNS 4) WORM-VELOCITY) (/ GRID-ROWS 2))
       '()
       "right"))
(check-expect
 (worm-move (worm (make-posn (/ GRID-COLUMNS 4) (/ GRID-ROWS 2)) '() UP))
 (worm (make-posn (/ GRID-COLUMNS 4) (- (/ GRID-ROWS 2) WORM-VELOCITY))
       '()
       "up"))
(check-expect
 (worm-move (worm (make-posn (/ GRID-COLUMNS 4) (/ GRID-ROWS 2)) '() DOWN))
 (worm (make-posn (/ GRID-COLUMNS 4) (+ (/ GRID-ROWS 2) WORM-VELOCITY))
       '()
       "down"))

(define (worm-move game-state)
  (worm (cond [(string=? (worm-direction game-state) LEFT)
               (make-posn (- (posn-x (worm-head game-state)) WORM-VELOCITY)
                          (posn-y (worm-head game-state)))]
              [(string=? (worm-direction game-state) RIGHT)
               (make-posn (+ (posn-x (worm-head game-state)) WORM-VELOCITY)
                          (posn-y (worm-head game-state)))]
              [(string=? (worm-direction game-state) UP)
               (make-posn (posn-x (worm-head game-state))
                          (- (posn-y (worm-head game-state)) WORM-VELOCITY))]
              [(string=? (worm-direction game-state) DOWN)
               (make-posn (posn-x (worm-head game-state))
                          (+ (posn-y (worm-head game-state)) WORM-VELOCITY))])
        (worm-body game-state)
        (worm-direction game-state)))

;; Game-State -> Game-State
;; updates a Worm's direction according to given Key-Event.

(check-expect
 (worm-control (worm (make-posn (/ GRID-COLUMNS 4) (/ GRID-ROWS 2)) '() LEFT) "left")
 (worm (make-posn (/ GRID-COLUMNS 4) (/ GRID-ROWS 2)) '() LEFT))
(check-expect
 (worm-control (worm (make-posn (/ GRID-COLUMNS 4) (/ GRID-ROWS 2)) '() LEFT) "right")
 (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() RIGHT))
(check-expect
 (worm-control (worm (make-posn (/ GRID-COLUMNS 4) (/ GRID-ROWS 2)) '() LEFT) "up")
 (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() UP))
(check-expect
 (worm-control (worm (make-posn (/ GRID-COLUMNS 4) (/ GRID-ROWS 2)) '() LEFT) "down")
 (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() DOWN))
(check-expect
 (worm-control (worm (make-posn (/ GRID-COLUMNS 4) (/ GRID-ROWS 2)) '() LEFT) "o")
 (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() LEFT))

(define (worm-control game-state key-event)
  (worm (worm-head game-state)
        (worm-body game-state)
        (cond [(string=? key-event LEFT) LEFT]
              [(string=? key-event RIGHT) RIGHT]
              [(string=? key-event UP) UP]
              [(string=? key-event DOWN) DOWN]
              [else (worm-direction game-state)])))

;; Game-State -> Boolean
;; stops the game when one of the following is true:
;; - the worm has run into a wall
;; - the worm has run into itself.

(check-expect (worm-hurt? (worm (make-posn GRID-LIMIT-LEFT (/ GRID-ROWS 2)) '() LEFT)) #t)
(check-expect (worm-hurt? (worm (make-posn GRID-LIMIT-RIGHT (/ GRID-ROWS 2)) '() LEFT)) #t)
(check-expect (worm-hurt? (worm (make-posn (/ GRID-COLUMNS 2) GRID-LIMIT-UP) '() LEFT)) #t)
(check-expect (worm-hurt? (worm (make-posn (/ GRID-COLUMNS 2) GRID-LIMIT-DOWN) '() LEFT)) #t)
(check-expect (worm-hurt? WORM-INIT) #f)

(define (worm-hurt? game-state)
  (worm-hit-wall? (worm-head game-state)))

;; Game-State -> Boolean
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

;; Game-State -> Image
;; renders the gameover screen of the game.

(check-expect
 (game-over (worm (make-posn GRID-LIMIT-LEFT (/ GRID-ROWS 2)) '() LEFT))
 (place-image (game-over-text (worm-body (worm (make-posn GRID-LIMIT-LEFT (/ GRID-ROWS 2)) '() LEFT)))
              (posn-x GAME-OVER-POSN)
              (posn-y GAME-OVER-POSN)
              (worm-render (worm (make-posn GRID-LIMIT-LEFT (/ GRID-ROWS 2)) '() LEFT))))

(define (game-over game-state)
  (place-image (game-over-text (worm-body game-state))
               (posn-x GAME-OVER-POSN)
               (posn-y GAME-OVER-POSN)
               (worm-render game-state)))

;; List-of-Posns -> Image
;; renders the gameover message.

(check-expect
 (game-over-text '())
 (text "worm hit wall: 0" GAME-OVER-FONT-SIZE GAME-OVER-FONT-COLOR))
(check-expect
 (game-over-text (list (make-posn (* GRID-CELL 5) (* GRID-CELL 10))))
 (text "worm hit wall: 1" GAME-OVER-FONT-SIZE GAME-OVER-FONT-COLOR))
(check-expect
 (game-over-text
  (list (make-posn (* GRID-CELL 5) (* GRID-CELL 10))
        (make-posn (+ 10 (* GRID-CELL 5)) (* GRID-CELL 10))
        (make-posn (+ 20 (* GRID-CELL 5)) (* GRID-CELL 10))))
 (text "worm hit wall: 3" GAME-OVER-FONT-SIZE GAME-OVER-FONT-COLOR))


(define (game-over-text list-of-posns)
  (text (string-append "worm hit wall: " (number->string (length list-of-posns)))
        GAME-OVER-FONT-SIZE GAME-OVER-FONT-COLOR))

;;; application

(test)
(worm-main WORM-INIT)
