#lang racket
(require test-engine/racket-tests
         lang/posn
         2htdp/image)

;; a Shape is a function:
;;   [Posn -> Boolean]
;; i.e. if s is a shape and p a Posn, (s p)
;; produces #t if p is in s, #f otherwise

;; Shape Posn -> Boolean
;; is some point p inside some shape s?
(check-expect (inside? (mk-point 3 4) (make-posn 3 4)) #t)
(check-expect (inside? (mk-point 3 4) (make-posn 3 0)) #f)
(check-expect (inside? (mk-circle 3 4 5) (make-posn 0 0)) #t)
(check-expect (inside? (mk-circle 3 4 5) (make-posn 0 9)) #f)
(check-expect (inside? (mk-circle 3 4 5) (make-posn -1 3)) #t)

(define (inside? s p)
  (s p))

;; Number Number -> Shape
;; represents a point at (x,y)
(check-expect [(mk-point 3 4) (make-posn 3 4)] #t)
(check-expect [(mk-point 3 4) (make-posn 3 0)] #f)

(define (mk-point x y)
  (lambda (p) (and (= (posn-x p) x) (= (posn-y p) y))))

;; Number Number Number -> Shape
;; creates a representation for a circle of radius r
;; located at (center-x, center-y)

(check-expect [(mk-circle 3 4 5) (make-posn 0 0)] #t)
(check-expect [(mk-circle 3 4 5) (make-posn 0 9)] #f)
(check-expect [(mk-circle 3 4 5) (make-posn -1 3)] #t)

(define (mk-circle center-x center-y r)
  ;; [Posn -> Boolean]
  (lambda (p)
    (<= (distance-between center-x center-y p) r)))

;; Number Number Posn -> Number
;; computes the distance between the points (x,y) and Posn p

(check-expect (distance-between 3 4 (make-posn 0 0)) 5)
(check-within (distance-between 3 4 (make-posn 0 9)) 5.8 0.1)
(check-within (distance-between 3 4 (make-posn -1 3)) 4.1 0.1)

(define (distance-between x y p)
  (sqrt (+ (sqr (- x (posn-x p)))
           (sqr (- y (posn-y p))))))

(test)
