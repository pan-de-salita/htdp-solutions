;; Exercise 8. Create a conditional expression that computes whether the image is tall or wide. An
;; image should be labeled "tall" if its height is larger than or equal to its width;
;; otherwise it is "wide".
;;
;; And then try to create an expression that computes whether a picture is "tall", "wide",
;; or "square".

#lang htdp/bsl
(require test-engine/racket-tests)
(require 2htdp/image)

;; image -> string
;; Purpose: Checks if an image is taller than it is wide, wider than it is tall, or otherwise
(define (image-descriptor img)
  (cond ((< (image-height img) (image-width img)) "wide")
        ((> (image-height img) (image-width img)) "tall")
        (else "square")))

(check-expect (image-descriptor (rectangle 20 10 "solid" "black")) "wide")
(check-expect (image-descriptor (rectangle 10 20 "solid" "black")) "tall")
(check-expect (image-descriptor (rectangle 10 10 "solid" "black")) "square")
(test)
