;; Exercise 5. use the 2htdp/image teachpack to create the image of a simple boat or
;; tree. Make sure you can easily change the scale of the entire image.

#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

;; number number color -> image
;; Purpose: Computes an image of a boat by joining and reorienting the image produced
;;          via the funtion boat-half
(define (boat width height color)
  (overlay/xy (boat-half width height color)
              (* width 2) 0
              (flip-horizontal (boat-half width height color))))

;; number number color -> image
;; Purpose: Computes an image of a side of the boat (a right-angle triangle)
(define (boat-side width height color)
  (right-triangle width height
                  "solid" color))

;; number number color -> image
;; Purpose: Computes an image of the body of the boat (a rectangle)
(define (boat-body width height color)
  (rectangle width height
             "solid" color))

;; number number color -> image
;; Purpose: Computes one half of the boat by joining the images produced via
;;          the functins boat-side and boat-body
(define (boat-half width height color)
  (overlay/xy (boat-body width height color)
              (- width) 0
              (rotate 180 (boat-side width height color))))

(check-expect (image-width (boat 20 20 "white")) 80)
(check-expect (image-height (boat 20 20 "white")) 20)
(test)
