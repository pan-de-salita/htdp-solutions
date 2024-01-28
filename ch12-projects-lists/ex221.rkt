#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;;;; constants and data definitions

(define BLOCK-LENGTH 10) ;; blocks are square
(define GRID-WIDTH 10) ;; # of blocks, horizontally
(define GRID-HEIGHT GRID-WIDTH) ;; # of blocks, vertically
(define GRID-SIZE (* GRID-WIDTH BLOCK-LENGTH))
(define GRID (empty-scene GRID-SIZE GRID-SIZE "black"))
(define GRID-BOTTOM (* BLOCK-LENGTH (- GRID-HEIGHT 1)))
(define GAME-SPEED 0.5)
;; NOTE: max BLOCKS within a GRID is:
;; (* (* (- GRID-WIDTH 1) BLOCK-LENGTH) (* (- GRID-HEIGHT 1) BLOCK-LENGTH))

(define BLOCK
  (overlay
   (square (- BLOCK-LENGTH 1) "solid" "red")
   (square BLOCK-LENGTH "outline" "black")))

(struct block [x y] #:transparent)
;; a Block is a structure:
;;   (make-block Number Number)
;; i.e. (block x y) depicts a block whose left corner
;; is (* BLOCK-LENGTH x) pixels from the left and
;; (* BLOCK-LENGTH y) pixels from the top.

(struct tetris [block landscape] #:transparent)
;; a Tetris is a structure:
;;   (tetris Block Landscape)
;; a Landscape is one of:
;; - '()
;; - (cons Block Landscape)
;; i.e. (tetris b0 (list b1 b2 ...)) means b0 is the
;; dropping block, while b1, b2, and ... are resting.
(define BLOCK-INIT-X (* BLOCK-LENGTH 5))
(define BLOCK-INIT-Y (* BLOCK-LENGTH 1))
(define BLOCK-INIT (block BLOCK-INIT-X BLOCK-INIT-Y))
(define LANDSCAPE-INIT '())
(define TETRIS-INIT (tetris BLOCK-INIT LANDSCAPE-INIT))

;; a Game-State is a Tetris.

;;;; examples for testing

(define landscape-0 '())
(define block-0 (block (* BLOCK-LENGTH 5) (* BLOCK-LENGTH 3)))
(define tetris-0 (tetris block-0 landscape-0))

(define block-landed-0 (block (* BLOCK-LENGTH 5) GRID-BOTTOM))
(define block-on-block-0 (block (* BLOCK-LENGTH 5) (* BLOCK-LENGTH (- GRID-HEIGHT 2))))
(define landscape-1 (list block-on-block-0 block-landed-0))
(define tetris-1 (tetris block-0 landscape-1))

(define block-landed-1 (block (* BLOCK-LENGTH 4) GRID-BOTTOM))
(define block-on-block-1 (block (* BLOCK-LENGTH 4) (* BLOCK-LENGTH (- GRID-HEIGHT 2))))
(define block-on-block-2 (block (* BLOCK-LENGTH 4) (* BLOCK-LENGTH (- GRID-HEIGHT 3))))
(define landscape-2 (list block-on-block-2
                          block-on-block-1
                          block-landed-1
                          block-on-block-0
                          block-landed-0))
(define tetris-2 (tetris block-0 landscape-2))

(define block-1 (block (* BLOCK-LENGTH 5) (* BLOCK-LENGTH (- GRID-HEIGHT 3))))
(define block-landed-2 (block (* BLOCK-LENGTH 1) GRID-BOTTOM))
(define block-landed-3 (block (* BLOCK-LENGTH 2) GRID-BOTTOM))
(define block-landed-4 (block (* BLOCK-LENGTH 3) GRID-BOTTOM))
(define block-landed-5 (block (* BLOCK-LENGTH 6) GRID-BOTTOM))
(define block-landed-6 (block (* BLOCK-LENGTH 7) GRID-BOTTOM))
(define block-landed-7 (block (* BLOCK-LENGTH 8) GRID-BOTTOM))
(define block-landed-8 (block (* BLOCK-LENGTH 9) GRID-BOTTOM))
(define block-landed-9 (block (* BLOCK-LENGTH 10) GRID-BOTTOM))
(define landscape-3 (list block-on-block-2
                          block-on-block-1
                          block-landed-1
                          block-landed-1
                          block-landed-2
                          block-landed-3
                          block-landed-4
                          block-landed-5
                          block-landed-6
                          block-landed-7
                          block-landed-8
                          block-on-block-0
                          block-landed-0
                          block-landed-9))
(define tetris-3 (tetris block-1 landscape-3))

;;;; functions

;; main function.

(define (tetris-main game-state)
  (big-bang game-state
            [to-draw tetris-render]
            [on-tick tetris-move GAME-SPEED]))

;; Game-State -> Image
;; renders an instance of Tetris into an image onto GRID.

(check-expect
 (tetris-render tetris-0)
 (place-image BLOCK (block-x (tetris-block tetris-0)) (block-y (tetris-block tetris-0)) GRID))
(check-expect
 (tetris-render tetris-1)
 (place-image BLOCK (block-x (tetris-block tetris-1)) (block-y (tetris-block tetris-1))
              (landscape-render (tetris-landscape tetris-1))))
(check-expect
 (tetris-render tetris-2)
 (place-image BLOCK (block-x (tetris-block tetris-2)) (block-y (tetris-block tetris-2))
              (landscape-render (tetris-landscape tetris-2))))
(check-expect
 (tetris-render tetris-3)
 (place-image BLOCK (block-x (tetris-block tetris-3)) (block-y (tetris-block tetris-3))
              (landscape-render (tetris-landscape tetris-3))))

(define (tetris-render game-state)
  (place-image BLOCK (block-x (tetris-block game-state)) (block-y (tetris-block game-state))
               (landscape-render (tetris-landscape game-state))))

;; Landscape -> Image
;; renders a Landscape into an image.

(check-expect
 (landscape-render (tetris-landscape tetris-0))
 GRID)
(check-expect
 (landscape-render (tetris-landscape tetris-1))
 (place-image BLOCK
              (block-x (car (tetris-landscape tetris-1)))
              (block-y (car (tetris-landscape tetris-1)))
              (landscape-render (cdr (tetris-landscape tetris-1)))))
(check-expect
 (landscape-render (tetris-landscape tetris-2))
 (place-image BLOCK
              (block-x (car (tetris-landscape tetris-2)))
              (block-y (car (tetris-landscape tetris-2)))
              (landscape-render (cdr (tetris-landscape tetris-2)))))
(check-expect
 (landscape-render (tetris-landscape tetris-3))
 (place-image BLOCK
              (block-x (car (tetris-landscape tetris-3)))
              (block-y (car (tetris-landscape tetris-3)))
              (landscape-render (cdr (tetris-landscape tetris-3)))))

(define (landscape-render landscape)
  (cond [(empty? landscape) GRID]
        [else (place-image BLOCK (block-x (car landscape)) (block-y (car landscape))
                           (landscape-render (cdr landscape)))]))

;; Game-State -> Game-State
;; does the following:
;; - if the current block has landed on the ground or on
;;   another block, creates a new instance of Tetris
;; - else, moves it in a straight line from the top
;;   of the GRID by BLOCK-LENGTH per clock tick.

(check-expect
 (tetris-move tetris-0)
 (tetris (block-move (tetris-block tetris-0))
         (tetris-landscape tetris-0)))
(check-expect
 (tetris-move tetris-1)
 (tetris (block-move (tetris-block tetris-1))
         (tetris-landscape tetris-1)))
(check-expect
 (tetris-move tetris-2)
 (tetris (block-move (tetris-block tetris-2))
         (tetris-landscape tetris-2)))
(check-random
 (tetris-move tetris-3)
 (tetris-create (tetris-block tetris-3) (tetris-landscape tetris-3)))

(define (tetris-move game-state)
  (cond [(block-landed? (tetris-block game-state) (tetris-landscape game-state))
         (tetris-create (tetris-block game-state) (tetris-landscape game-state))]
        [else (tetris (block-move (tetris-block game-state))
                      (tetris-landscape game-state))]))

;; Block Landscape -> Boolean
;; checks if the current block has landed on GRID-BOTTOM or
;; on another block.

(check-expect (block-landed? block-0 landscape-3) #f)
(check-expect (block-landed? block-1 landscape-3) #t)
(check-expect (block-landed? block-on-block-0 landscape-0) #t)

(define (block-landed? current-block current-landscape)
  (or (= (block-y (block-move current-block)) GRID-BOTTOM)
      (not (boolean? (member (block-move current-block) current-landscape)))))

;; Block -> Boolean
;; checks if the current block will land on GRID-BOTTOM.

(check-expect (block-landed?/grid-bottom block-0) #f)
(check-expect (block-landed?/grid-bottom block-on-block-0) #t)

(define (block-landed?/grid-bottom current-block)
  (= (block-y (block-move current-block)) GRID-BOTTOM))

;; Block Landscape -> Boolean
;; checks if the current block will land on another block.

(check-expect (block-landed?/on-block block-0 landscape-3) #f)
(check-expect (block-landed?/on-block block-1 landscape-3) #t)

(define (block-landed?/on-block current-block current-landscape)
  (not (boolean? (member (block-move current-block) current-landscape))))

;; Block Landscape -> Tetris
;; does the following:
;; - adds the current block to the current Landscape and
;; - create another block that descends from a random column.

(check-random
 (tetris-create block-1 landscape-3)
 (tetris (block-create block-0)
         (cons block-1 landscape-3)))
(check-random
 (tetris-create (block (* BLOCK-LENGTH 5) (* BLOCK-LENGTH (- GRID-HEIGHT 2))) landscape-0)
 (tetris (block-create (block (* BLOCK-LENGTH 5) (* BLOCK-LENGTH (- GRID-HEIGHT 2))))
         (cons (block-move (block (* BLOCK-LENGTH 5) (* BLOCK-LENGTH (- GRID-HEIGHT 2)))) landscape-0)))

(define (tetris-create current-block current-landscape)
  (tetris (block-create current-block)
          (cons (cond [(block-landed?/on-block current-block current-landscape) current-block]
                      [else (block-move current-block)])
                current-landscape)))

;; Block -> Block
;; creates a new block at a random column.

(check-random
 (block-create block-0)
 (block (block-generate (block-x block-0)) BLOCK-INIT-Y))

(define (block-create current-block)
  (block (block-generate (block-x current-block)) BLOCK-INIT-Y))

;; Number -> Number
;; generates an x-coordinate for a block.

(check-random
 (block-generate 10)
 (block-check-generate 10 (* (random 1 GRID-WIDTH) BLOCK-LENGTH)))

(define (block-generate block-x)
  (block-check-generate block-x (* (random 1 GRID-WIDTH) BLOCK-LENGTH)))

;; Number Number -> Number
;; returns a new x-coordinate if the old and new x-coordinates
;; are equal.

(check-expect (block-check-generate 10 20) 20)
(check-random (block-check-generate 10 10) (block-generate 10))

(define (block-check-generate block-x-old block-x-new)
  (cond [(= block-x-old block-x-new) (block-generate block-x-old)]
        [else block-x-new]))

;; Block -> Block
;; moves the current block in a straight line from the top of
;; the GRID by BLOCK-LENGTH per clock tick.

(check-expect
 (block-move (tetris-block tetris-0))
 (block (block-x (tetris-block tetris-0))
        (+ (block-y (tetris-block tetris-0)) BLOCK-LENGTH)))

(define (block-move current-block)
  (block (block-x current-block)
         (+ (block-y current-block) BLOCK-LENGTH)))

;;;; application

(test)
(tetris-main TETRIS-INIT)
