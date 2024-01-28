#lang racket
(require test-engine/racket-tests
         2htdp/image
         lang/posn)

(define CANVAS (empty-scene 200 200 "dimgrey"))
(define DOT (circle 5 "solid" "red"))

;; [List-of Posn] -> Image
;; places red circles on a canvas given a list of Posns

(check-expect (dots '()) CANVAS)
(check-expect
 (dots (list (make-posn 12 31)))
 (place-images (list DOT)
               (list (make-posn 12 31))
               CANVAS))
(check-expect
 (dots (list (make-posn 12 31)
             (make-posn 80 75)
             (make-posn 125 156)))
 (place-images (list DOT DOT DOT)
               (list (make-posn 12 31)
                     (make-posn 80 75)
                     (make-posn 125 156))
               CANVAS))

(define (dots l-posn)
  (local (;; Posn Image -> Image
          ;; (NOTE how this signature mimics the structure
          ;; of the auxiliary function used in foldr)
          ;; renders an individual DOT onto img
          (define (add-single-dot posn img)
            (place-image DOT (posn-x posn) (posn-y posn) img)))
    (foldr add-single-dot CANVAS l-posn)))

#|

key: at most times, start with the desired output and find
an abstraction that has the same or a more general output

with this in mind, recall that the signature for foldl or
foldr is:

  [X Y] [X Y -> Y] Y [List-of X] -> Y

where we can plug in any data collection we want as the output

applied to our function, we arrive at the signature:

  [Posn Image] [Posn Image -> Image] Image [List-of Posn] -> Image

which used with foldr produces:

  (foldl add-single-dot CANVAS l-posn)

giving us our final output

|#

(test)
