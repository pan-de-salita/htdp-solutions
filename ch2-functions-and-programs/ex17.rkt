;; Exercise 17. Define the function image-classify, which consumes an image and
;; conditionally produces "tall" if the image is taller than wide, "wide" if it is wider
;; than tall, or "square" if its width and height are the same.

#lang htdp/bsl
(require test-engine/racket-tests)
(require 2htdp/image)

;; image -> string
;; Purpose: Comsumes an image and conditionally produces "tall" if the imag is taller than wide,
;;          "wide" if it is wider than tall, else "square"
(define (image-classify img)
  (cond ((> (image-height img) (image-width img)) "tall")
        ((> (image-width img) (image-height img)) "wide")
        (else "square")))

(check-expect (image-classify (rectangle 10 20 "solid" "black")) "tall")
(check-expect (image-classify (rectangle 20 10 "solid" "black")) "wide")
(check-expect (image-classify (rectangle 10 10 "solid" "black")) "square")
(test)
