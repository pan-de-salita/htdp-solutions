#lang racket
(require test-engine/racket-tests
         2htdp/image)

;; [List-of X] [List-of X] -> [List-of X]
;; like append, concatenates the items of two lists

(check-expect (append-from-fold '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

(define (append-from-fold l-x l-y)
  (foldr (lambda (x y) (cons x y)) l-y l-x))

;; using foldl instead returns (cons (reverse l-x) l-y)

;; [List-of Number] -> Number
;; computes the sum of a [List-of Number]

(check-expect (sum-from-fold '(1 2 3 4)) 10)

(define (sum-from-fold l-num)
  (foldr (lambda (current-num previous-num) (+ current-num previous-num)) 0 l-num))

;; [List-of Number] -> Number
;; computes the product of a [List-of Number]

(check-expect (product-from-fold '(1 2 3 4)) 24)

(define (product-from-fold l-num)
  (foldr (lambda (current-num previous-num) (* current-num previous-num)) 1 l-num))

;; Image manipulation from fold -----------

(define DOT (circle 10 "solid" "red"))
(define TEST-L-IMG (make-list 5 DOT))

;; [List-of Image] -> Image
;; horizontally composes a [List-of Image]

(check-expect (beside-from-fold TEST-L-IMG)
              (beside DOT DOT DOT DOT DOT empty-image))

(define (beside-from-fold l-img)
  (foldr (lambda (current-img previous-img)
           (beside current-img previous-img))
         empty-image l-img))

;; [List-of Image] -> Image
;; stacks a [List-of Image] vertically

(check-expect (above-from-fold TEST-L-IMG)
              (above DOT DOT DOT DOT DOT empty-image))

(define (above-from-fold l-img)
  (foldr (lambda (current-img previous-img)
           (above current-img previous-img))
         empty-image l-img))

;; ----------------------------------------

(test)
