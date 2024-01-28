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

(define (inside? s p)
  (s p))

;; Number Number -> Shape
;; represents a point at (x,y)

(check-expect (inside? (mk-point 3 4) (make-posn 3 4)) #t)
(check-expect (inside? (mk-point 3 4) (make-posn 3 0)) #f)

(define (mk-point x y)
  (lambda (p) (and (= (posn-x p) x) (= (posn-y p) y))))

;; Number Number Number -> Shape
;; creates a representation for a circle of radius r
;; located at (center-x, center-y)

(check-expect (inside? (mk-circle 3 4 5) (make-posn 0 0)) #t)
(check-expect (inside? (mk-circle 3 4 5) (make-posn 0 9)) #f)
(check-expect (inside? (mk-circle 3 4 5) (make-posn -1 3)) #t)

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

;; Number Number Number Number -> Shape
;; represents a width by height rectangle whose
;; upper-left corner is located at (ul-x, ul-y)

(check-expect (inside? (mk-rect 0 0 10 3) (make-posn 0 0)) #t)
(check-expect (inside? (mk-rect 2 3 10 3) (make-posn 4 5)) #t)
(check-expect (inside? (mk-rect 2 3 10 3) (make-posn 13 5)) #f)

(define (mk-rect ul-x ul-y width height)
  ;; [Posn -> Boolean]
  (lambda (p)
    (and (<= ul-x (posn-x p) (+ ul-x width))
         (<= ul-y (posn-y p) (+ ul-y height)))))

;; Shape Shape -> Shape
;; combines two Shapes into one

(define (mk-combination s1 s2)
  ;; [Posn -> Boolean]
  (lambda (p) (or (inside? s1 p) (inside? s2 p))))

(define circle1 (mk-circle 3 4 5))
(define rectangle1 (mk-rect 0 3 10 3))
(define union1 (mk-combination circle1 rectangle1))

(check-expect (inside? union1 (make-posn 0 0)) #true)
(check-expect (inside? union1 (make-posn 0 9)) #false)
(check-expect (inside? union1 (make-posn -1 3)) #true)

(test)
