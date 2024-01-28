;; Exercise 16. Define the function image-area, which counts the number of pixels in
;; a given image. See exercise 6 for ideas.

#lang htdp/bsl
(require test-engine/racket-tests)
(require 2htdp/image)

;; image -> number
;; Pupose: Computes the area -- or the number of pixels -- in a given image
(define (image-area img)
  (* (image-width img)
     (image-height img)))

(check-expect (image-area (circle 13 "solid" "black"))
              (* (* 13 2) (* 13 2)))
(test)
