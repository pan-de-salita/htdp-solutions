#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

; We use numbers to represent the pixels of an image.
; Image -> Number
; Purpose: computes the number of pixels in a given image.
; given: (square 10 "solid" "black"), expect: 100
; given: (triangle 10 "solid" "black"), expect: 90
(define (image-area img)
  (* (image-width img)
     (image-height img)))

; Test cases
(check-expect (image-area (square 10 "solid" "black")) 100)
(check-expect (image-area (triangle 10 "solid" "black")) 90)
