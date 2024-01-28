#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

;;; data and constant definitions

(define GRID-CELL 5)
(define GRID-ROWS (+ (* GRID-CELL 40) GRID-CELL))
(define GRID-COLUMNS GRID-ROWS)
(define GRID (empty-scene GRID-ROWS GRID-COLUMNS "black"))

(define WORM-RADIUS GRID-CELL)
(define WORM-DIAMETER (* WORM-RADIUS 2))
(define WORM-SEGMENT (circle WORM-RADIUS "solid" "darkpink"))
(define WORM-HEAD (circle WORM-RADIUS "solid" "maroon"))
(define WORM-VELOCITY WORM-DIAMETER)

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
(define WORM-INIT (worm (make-posn (/ GRID-ROWS 4) (/ GRID-ROWS 2)) '() "right"))

;; a Game-State is a Worm.
;; i.e. the movement of a Worm.

;;; functions

;; main function.

(define (worm-main game-state)
  (big-bang game-state
            [to-draw worm-render]
            [on-tick worm-move 0.5]
            [on-key worm-control]))

;; Game-State -> Image
;; renders the image of a Worm onto GRID.

(check-expect
 (worm-render (worm (make-posn GRID-CELL (/ GRID-COLUMNS 2)) '() RIGHT))
 (place-image WORM-HEAD GRID-CELL (/ GRID-COLUMNS 2) GRID))
(check-expect
 (worm-render (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() RIGHT))
 (place-image WORM-HEAD (/ GRID-ROWS 4) (/ GRID-COLUMNS 2) GRID))
(check-expect
 (worm-render (worm (make-posn (- GRID-ROWS GRID-CELL) (/ GRID-ROWS 2)) '() RIGHT))
 (place-image WORM-HEAD (- GRID-ROWS GRID-CELL) (/ GRID-ROWS 2) GRID))

(define (worm-render game-state)
  (place-image WORM-HEAD (posn-x (worm-head game-state)) (posn-y (worm-head game-state)) GRID))

;; Game-State -> Game-State
;; moves a Worm by WORM-VELOCITY per tick.

(check-expect
 (worm-move (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() LEFT))
 (worm (make-posn (- (/ GRID-ROWS 4) WORM-VELOCITY) (/ GRID-COLUMNS 2))
       '()
       "left"))
(check-expect
 (worm-move (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() RIGHT))
 (worm (make-posn (+ (/ GRID-ROWS 4) WORM-VELOCITY) (/ GRID-COLUMNS 2))
       '()
       "right"))
(check-expect
 (worm-move (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() UP))
 (worm (make-posn (/ GRID-ROWS 4) (- (/ GRID-COLUMNS 2) WORM-VELOCITY))
       '()
       "up"))
(check-expect
 (worm-move (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() DOWN))
 (worm (make-posn (/ GRID-ROWS 4) (+ (/ GRID-COLUMNS 2) WORM-VELOCITY))
       '()
       "down"))

(define (worm-move game-state)
  (worm (cond [(string=? (worm-direction game-state) LEFT)
               (make-posn (- (posn-x (worm-head game-state)) WORM-VELOCITY) (posn-y (worm-head game-state)))]
              [(string=? (worm-direction game-state) RIGHT)
               (make-posn (+ (posn-x (worm-head game-state)) WORM-VELOCITY) (posn-y (worm-head game-state)))]
              [(string=? (worm-direction game-state) UP)
               (make-posn (posn-x (worm-head game-state)) (- (posn-y (worm-head game-state)) WORM-VELOCITY))]
              [(string=? (worm-direction game-state) DOWN)
               (make-posn (posn-x (worm-head game-state)) (+ (posn-y (worm-head game-state)) WORM-VELOCITY))])
        (worm-body game-state)
        (worm-direction game-state)))

;; Game-State -> Game-State
;; updates a Worm's direction according to given Key-Event.

(check-expect
 (worm-control (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() LEFT) "left")
 (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() LEFT))
(check-expect
 (worm-control (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() LEFT) "right")
 (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() RIGHT))
(check-expect
 (worm-control (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() LEFT) "up")
 (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() UP))
(check-expect
 (worm-control (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() LEFT) "down")
 (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() DOWN))
(check-expect
 (worm-control (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() LEFT) "o")
 (worm (make-posn (/ GRID-ROWS 4) (/ GRID-COLUMNS 2)) '() LEFT))

(define (worm-control game-state key-event)
  (worm (worm-head game-state)
        (worm-body game-state)
        (cond [(string=? key-event LEFT) LEFT]
              [(string=? key-event RIGHT) RIGHT]
              [(string=? key-event UP) UP]
              [(string=? key-event DOWN) DOWN]
              [else (worm-direction game-state)])))

;;; application

(test)
(worm-main WORM-INIT)
