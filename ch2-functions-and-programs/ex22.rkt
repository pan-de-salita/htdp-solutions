;; Exercise 22. Use DrRacket's stepper on this program fragment:
;;
;; (define (distance-to-origin x y)
;;   (sqrt (+ (sqr x) (sqr y))))
;; (distance-to-origin 3 4)

#lang racket
(require racket/trace)

(define (distance-to-origin x y)
  (sqrt (+ (sqr x) (sqr y))))
(distance-to-origin 3 4)
