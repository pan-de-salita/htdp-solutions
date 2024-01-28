#lang racket

(define-struct point [x y z])
(define-struct none [])

;; 1.
(make-point 1 2 3)
;; is a structure value and therefore a value

;; 2.
(make-point (make-point 1 2 3) 4 5)
;; is a structure value and therefore a value

;; 3.
(make-point (+ 1 2) 3 4)
;; is a structure value and therefore a value

;; 4.
(make-none)
;; is a structure value and therefore a value

;; 5.
(make-point (point-x (make-point 1 2 3)) 4 5)
;; (make-point (point-x (make-point 1 2 3)) 4 5) == (make-point 1 4 5)
;; is a structure value and therefore a value
