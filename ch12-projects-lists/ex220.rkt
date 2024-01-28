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
;; is (* x BLOCK-LENGTH) pixels from the left and
;; (* y BLOCK-LENGTH) pixels from the top.

(struct tetris [block landscape] #:transparent)
;; a Tetris is a structure:
;;   (tetris Block Landscape)
;; a Landscape is one of:
;; - '()
;; - (cons Block Landscape)
;; i.e. (tetris b0 (list b1 b2 ...)) means b0 is the
;; dropping block, while b1, b2, and ... are resting.

;;;; examples for testing

(define landscape-0 '())
(define block-0 (block (* BLOCK-LENGTH 5) (* BLOCK-LENGTH 2)))
(define tetris-0 (tetris block-0 landscape-0))

(define block-landed-0 (block (* BLOCK-LENGTH 5) (* BLOCK-LENGTH (- GRID-HEIGHT 1))))
(define block-on-block-0 (block (* BLOCK-LENGTH 5) (* BLOCK-LENGTH (- GRID-HEIGHT 2))))
(define landscape-1 (list block-on-block-0 block-landed-0))
(define tetris-1 (tetris block-0 landscape-1))

(define block-landed-1 (block (* BLOCK-LENGTH 4) (* BLOCK-LENGTH (- GRID-HEIGHT 1))))
(define block-on-block-1 (block (* BLOCK-LENGTH 4) (* BLOCK-LENGTH (- GRID-HEIGHT 2))))
(define block-on-block-2 (block (* BLOCK-LENGTH 4) (* BLOCK-LENGTH (- GRID-HEIGHT 3))))
(define landscape-2 (list block-on-block-2
                          block-on-block-1
                          block-landed-1
                          block-on-block-0
                          block-landed-0))
(define tetris-2 (tetris block-0 landscape-2))

(define block-landed-2 (block (* BLOCK-LENGTH 1) (* BLOCK-LENGTH (- GRID-HEIGHT 1))))
(define block-landed-3 (block (* BLOCK-LENGTH 2) (* BLOCK-LENGTH (- GRID-HEIGHT 1))))
(define block-landed-4 (block (* BLOCK-LENGTH 3) (* BLOCK-LENGTH (- GRID-HEIGHT 1))))
(define block-landed-5 (block (* BLOCK-LENGTH 6) (* BLOCK-LENGTH (- GRID-HEIGHT 1))))
(define block-landed-6 (block (* BLOCK-LENGTH 7) (* BLOCK-LENGTH (- GRID-HEIGHT 1))))
(define block-landed-7 (block (* BLOCK-LENGTH 8) (* BLOCK-LENGTH (- GRID-HEIGHT 1))))
(define block-landed-8 (block (* BLOCK-LENGTH 9) (* BLOCK-LENGTH (- GRID-HEIGHT 1))))
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
                          block-landed-0))
(define tetris-3 (tetris block-0 landscape-3))

;;;; functions

;; Tetris -> Image
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

;;;; application

(test)
