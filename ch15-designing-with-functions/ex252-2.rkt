#lang racket
(require test-engine/racket-tests)
(require lang/posn)
(require 2htdp/image)

;; [List-of Number] -> Number
(check-expect (product (list 1)) 1)
(check-expect (product (list 1 2 3)) 6)

(define (product l)
  (cond [(empty? l) 1]
        [else (* (car l) (product (cdr l)))]))

;; graphical constants
(define emt (empty-scene 100 100))
(define dot (circle 3 "solid" "red"))

;; [List-of Posn] -> Image
(check-expect
 (image* (list (make-posn 50 50)))
 (place-dot (make-posn 50 50) emt))
(check-expect
 (image* (list (make-posn 50 50) (make-posn 80 42)))
 (place-images (list dot dot)
               (list (make-posn 50 50) (make-posn 80 42))
               emt))

(define (image* l)
  (cond [(empty? l) emt]
        [else (place-dot (car l) (image* (cdr l)))]))

;; Posn Image -> Image
(check-expect
 (place-dot (make-posn 50 50) emt)
 (place-image dot 50 50 emt))

(define (place-dot p img)
  (place-image dot (posn-x p) (posn-y p) img))

;;;; abstraction

;; [X Y] [List-of X] Y [X Y -> Y] -> Y
(check-expect (fold2 (list 1) 1  *) 1)
(check-expect (fold2 (list 1 2 3) 1 *) 6)
(check-expect
 (fold2 (list (make-posn 50 50)) emt place-dot)
 (place-dot (make-posn 50 50) emt))
(check-expect
 (fold2 (list (make-posn 50 50) (make-posn 80 42)) emt place-dot)
 (place-images (list dot dot)
               (list (make-posn 50 50) (make-posn 80 42))
               emt))

(define (fold2 x b F)
  (cond [(empty? x) b]
        [else (F (car x) (fold2 (cdr x) b F))]))

(test)
