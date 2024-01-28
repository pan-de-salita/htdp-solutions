#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require lang/posn)

;;; constants
(define SEAT (square 10 "outline" "black"))
(define BALLOON (circle 3.5 "solid" "red"))
(define DEFAULT-COLUMNS 10)
(define DEFAULT-ROWS 20)

;;; data definitions
;; an N is one of:
;; - 0
;; - (add1 N)
;; i.e.: counting numbers.

;; a List-of-Posns is one of:
;; - '()
;; - (cons Posn List-of-Posns)
;; i.e.: a list of Posns.
(define LOP-EXAMPLE-0 '())
(define LOP-EXAMPLE-1
  (cons (make-posn (* (image-width SEAT) 2)
                   (* (image-height SEAT) 4))
        (cons (make-posn (* (image-width SEAT) 9)
                         (* (image-height SEAT) 3)) '())))
(define LOP-EXAMPLE-2
  (cons (make-posn (* (image-width SEAT) 1)
                   (* (image-height SEAT) 2))
        (cons (make-posn (* (image-width SEAT) 2)
                         (* (image-height SEAT) 4))
              (cons (make-posn (* (image-width SEAT) 3)
                               (* (image-height SEAT) 6))
                    (cons (make-posn (* (image-width SEAT) 4)
                                     (* (image-height SEAT) 8))
                          (cons (make-posn (* (image-width SEAT) 5)
                                           (* (image-height SEAT) 10))
                                (cons (make-posn (* (image-width SEAT) 6)
                                                 (* (image-height SEAT) 12))
                                      (cons (make-posn (* (image-width SEAT) 7)
                                                       (* (image-height SEAT) 14))
                                            (cons (make-posn (* (image-width SEAT) 8)
                                                             (* (image-height SEAT) 16))
                                                  (cons (make-posn (* (image-width SEAT) 9)
                                                                   (* (image-height SEAT) 18)) '()))))))))))

;;; function definitions
;; N Image -> Image
;; produces a row of n copies of img.
(check-expect (col 0 SEAT) empty-image)
(check-expect (col 2 SEAT) (beside SEAT SEAT empty-image))

(define (col n img)
  (cond [(zero? n) empty-image]
        [(positive? n)
         (beside img (col (sub1 n) img))]))

;; N Image -> Image
;; produces a column of n copies of img.
(check-expect (row 0 SEAT) empty-image)
(check-expect (row 2 SEAT) (above SEAT SEAT empty-image))

(define (row n img)
  (cond [(zero? n) empty-image]
        [(positive? n)
         (above img (row (sub1 n) img))]))

;; N N -> Image
;; creates the image of a lecture hall of cols by rows seats.
(check-expect (draw-lecture-hall DEFAULT-COLUMNS DEFAULT-ROWS)
              (overlay (col DEFAULT-COLUMNS (row DEFAULT-ROWS SEAT))
                       (empty-scene (* DEFAULT-COLUMNS (image-width SEAT))
                                    (* DEFAULT-ROWS (image-width SEAT))
                                    "dimgrey")))

(define (draw-lecture-hall cols rows)
  (cond [(or (zero? cols) (zero? rows))
         empty-image]
        [(and (positive? cols) (positive? rows))
         (overlay (col cols (row rows SEAT))
                  (empty-scene (* cols (image-width SEAT))
                               (* rows (image-width SEAT))
                               "dimgrey"))]))

;; List-of-Posns -> Image
;; creates the image of lecture hall with red dots added as
;; specified by the Posns in the given List-of-Posns, here
;; called lop.
(check-expect (add-balloons LOP-EXAMPLE-0) (draw-lecture-hall DEFAULT-COLUMNS DEFAULT-ROWS))
(check-expect (add-balloons LOP-EXAMPLE-1)
              (place-image BALLOON
                           (posn-x (first LOP-EXAMPLE-1))
                           (posn-y (first LOP-EXAMPLE-1))
                           (add-balloons (rest LOP-EXAMPLE-1))))
(check-expect (add-balloons LOP-EXAMPLE-2)
              (place-image BALLOON
                           (posn-x (first LOP-EXAMPLE-2))
                           (posn-y (first LOP-EXAMPLE-2))
                           (add-balloons (rest LOP-EXAMPLE-2))))

(define (add-balloons lop)
  (cond [(empty? lop)
         (draw-lecture-hall DEFAULT-COLUMNS DEFAULT-ROWS)]
        [(cons? lop)
         (place-image BALLOON
                      (posn-x (first lop))
                      (posn-y (first lop))
                      (add-balloons (rest lop)))]))

;;; application
(test)

(beside (add-balloons LOP-EXAMPLE-0)
        (add-balloons LOP-EXAMPLE-1)
        (add-balloons LOP-EXAMPLE-2))
