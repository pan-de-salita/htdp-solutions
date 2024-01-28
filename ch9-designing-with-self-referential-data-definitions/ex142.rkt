#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

;; a List-of-Images is one of:
;; - '()
;; - (cons Image List-of-Images)
;; i.e.: a collection of images
(define list-of-images-example-0
  '())
(define list-of-images-example-1
  (cons (square 2 "solid" "black") '()))
(define list-of-images-example-2
  (cons (square 2 "solid" "black")
        (cons (square 3 "solid" "red") '())))
(define list-of-images-example-3
  (cons (square 4 "solid" "green")
        (cons (square 2 "solid" "black")
              (cons (square 3 "solid" "red") '()))))

;; an Image-or-False is one of:
;; - Image
;; - #false
(define image/false-example-0 (square 2 "solid" "black"))
(define image/false-example-1 (square 3 "solid" "red"))
(define image/false-example-2 (square 4 "solid" "green"))
(define image/false-example-3 #false)

;; List-of-Images PositiveNumber -> Image-or-False
;; produces the first image on the given List-of-Images whose
;; area is not the given PositiveNumber squared; returns #false
;; if no such image is found
(check-expect (ill-sized list-of-images-example-0 2) image/false-example-3)
(check-expect (ill-sized list-of-images-example-1 2) image/false-example-3)
(check-expect (ill-sized list-of-images-example-2 2) image/false-example-1)
(check-expect (ill-sized list-of-images-example-3 2) image/false-example-2)

(define (ill-sized list-of-images positive-number)
  (cond
    [(empty? list-of-images)
     #false]
    [else
     (if (not
          (area==number?
           (image-width (first list-of-images))
           (image-height (first list-of-images))
           positive-number))
         (first list-of-images)
         (ill-sized (rest list-of-images) positive-number))]))

;; PositiveNumber PositiveNumber PositiveNumber -> Boolean
;; checks if the area of an image is equal to a given PositiveNumber
(check-expect (area==number? 2 2 3) #false)
(check-expect (area==number? 2 2 2) #true)

(define (area==number? width height positive-number)
  (= (* width height) (sqr positive-number)))

(test)
