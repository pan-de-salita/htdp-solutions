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

;; [List-of Any] -> Any
(check-expect (fold2 (list 1) *) 1)
(check-expect (fold2 (list 1 2 3) *) 6)
(check-expect (fold2 (list 1) +) 1)
(check-expect (fold2 (list 1 2 3) +) 6)
(check-expect
 (fold2 (list (make-posn 50 50)) place-dot)
 (place-dot (make-posn 50 50) emt))
(check-expect
 (fold2 (list (make-posn 50 50) (make-posn 80 42)) place-dot)
 (place-images (list dot dot)
               (list (make-posn 50 50) (make-posn 80 42))
               emt))

(define (fold2 x R)
  (cond [(empty? (cdr x)) (base-case (car x) R)]
        [else (R (car x) (fold2 (cdr x) R))]))

;; Any -> Any
(check-expect (base-case 3 *) (* 3 1))
(check-expect (base-case 3 +) (+ 3 0))
(check-expect
 (base-case (make-posn 50 50) place-dot)
 (place-dot (make-posn 50 50) emt))

(define (base-case x R)
  (cond [(number? x) (R x (operand R))]
        [else (R x emt)]))

;; Procedure -> Number
(define (operand R)
  (if (equal? R *) 1 0))

;; [List-of Number] -> Number
;; NOTE: for testing.
(check-expect (product-from-abstract (list 1)) (product (list 1)))
(check-expect (product-from-abstract (list 1 2 3)) (product (list 1 2 3)))

(define (product-from-abstract l)
  (fold2 l *))

;; [List-of Image] -> Image
;; NOTE: for testing.
(check-expect
 (image*-from-abstract (list (make-posn 50 50)))
 (image* (list (make-posn 50 50))))
(check-expect
 (image*-from-abstract (list (make-posn 50 50) (make-posn 80 42)))
 (image* (list (make-posn 50 50) (make-posn 80 42))))

(define (image*-from-abstract l)
  (fold2 l place-dot))

(test)
