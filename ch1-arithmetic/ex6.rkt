;; Exercise 6. Add the following line to the definitions area:
;;
;; (define cat [image of cat])
;;
;; Create an expression that counts the number of pixels in the image.

#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

;; image -> number
;; Purpose: Calculates the number of pixels -- or area -- of an image
(define (image img)
  (* (image-width img)
     (image-height img)))
