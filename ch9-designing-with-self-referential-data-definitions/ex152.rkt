#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

(define BLOCK (square 5 "outline" "red"))

;; an N is one of:
;; - 0
;; - (add1 N)
;; i.e.: counting numbers

;; N Image -> Image
;; produces a column of n copies of img.
(check-expect (col 0 BLOCK) empty-image)
(check-expect (col 2 BLOCK) (above BLOCK BLOCK empty-image))

(define (col n img)
  (cond [(zero? n) empty-image]
        [(positive? n)
         (above img (col (sub1 n) img))]))

;; N Image -> Image
;; produces a row of n copies of img.
(check-expect (row 0 BLOCK) empty-image)
(check-expect (row 2 BLOCK) (beside BLOCK BLOCK empty-image))

(define (row n img)
  (cond [(zero? n) empty-image]
        [(positive? n)
         (beside img (row (sub1 n) img))]))

(test)
