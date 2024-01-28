#lang racket
(require test-engine/racket-tests
         2htdp/image)

;; [List-of X] [List-of X] -> [List-of X]
;; concatenates the items of two lists head-l and tail-l

(check-expect (append-from-fold '() '()) '())
;; NOTE: if tail-l is empty, the result is head-l
(check-expect (append-from-fold '(1 2 3) '()) '(1 2 3))
(check-expect (append-from-fold '() '(4 5 6)) '(4 5 6))
(check-expect (append-from-fold '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

(define (append-from-fold head-l tail-l)
  (foldr cons tail-l head-l))

;; if foldl were used instead of foldr, then the order of items
;; on head-l would be reversed:
;;
;; (foldr cons '(4 5 6) '(1 2 3)) == '(1 2 3 4 5 6)
;; (foldl cons '(4 5 6) '(1 2 3)) == '(3 2 1 4 5 6)

;; [List-of Number] -> Number
;; computes the sum of a list of numbers

(check-expect (sum-from-fold '()) 0)
(check-expect (sum-from-fold '(1 2 3 4 5)) 15)

(define (sum-from-fold l-num)
  (foldr + 0 l-num))

;; [List-of Number] -> Number
;; computes the product of a list of numbers

(check-expect (product-from-fold '()) 1)
(check-expect (product-from-fold '(1 2 3 4 5)) 120)

(define (product-from-fold l-num)
  (foldr * 1 l-num))

;;;; folding images ------------------------------------------------------------

(define DOT (circle 10 "solid" "red"))

;; [List-of Image] -> Image
;; places the images on a list of images beside each other

(check-expect (beside/fold '()) empty-image)
(check-expect (beside/fold `(,DOT ,DOT ,DOT)) (beside DOT DOT DOT))

(define (beside/fold l-img)
  (foldr beside empty-image l-img))

;; [List-of Image] -> Image
;; stacks the images on a list of images above each other

(check-expect (above/fold '()) empty-image)
(check-expect (above/fold `(,DOT ,DOT ,DOT)) (above DOT DOT DOT))

(define (above/fold l-img)
  (foldr above empty-image l-img))

(test)
