#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;;; constants
(define SEAT (square 10 "outline" "black"))
(define BALLOON (circle 3.5 "solid" "red"))
(define DEFAULT-COLUMNS 10)
(define DEFAULT-ROWS 20)
(define LECTURE-HALL-COLOR "dimgrey")

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

(define-struct balloon [to-throw positions])
;; a Balloon is a structure:
;;     (make-balloon N List-of-Posns)
;; i.e. describes the amount of balloons yet to be
;; thrown and their position to be added into a
;; List-of-Posns.
(define BALLOON-EXAMPLE-0 (make-balloon 0 '()))
(define BALLOON-EXAMPLE-1 (make-balloon 3 LOP-EXAMPLE-1))
(define BALLOON-EXAMPLE-2 (make-balloon 5 LOP-EXAMPLE-2))

;;; function definitions

;; N -> Balloon
;; main function.
(define (riot balloon-amount)
  (if (zero? balloon-amount)
      "riot postponed"
      (big-bang (make-balloon balloon-amount '())
                [to-draw throw-balloons]
                [on-tick next-balloon 1]
                [check-with valid-balloon?]
                [stop-when no-more-balloons? throw-balloons])))

;; Balloon -> Image
;; creates the image of lecture hall with ballloons added as
;; specified by the Posns in the given Balloon structure.
(check-expect (throw-balloons BALLOON-EXAMPLE-1)
              (draw-balloons (balloon-positions BALLOON-EXAMPLE-1)))

(define (throw-balloons balloons)
  (draw-balloons (balloon-positions balloons)))

;; List-of-Posns -> Image
;; draws the balloons on the lecture hall according to Posns
;; in provided List-of-Posns.
(check-expect (draw-balloons LOP-EXAMPLE-1)
              (place-image BALLOON
                           (posn-x (first LOP-EXAMPLE-1))
                           (posn-y (first LOP-EXAMPLE-1))
                           (draw-balloons (rest LOP-EXAMPLE-1))))

(define (draw-balloons lop)
  (if (empty? lop)
      (draw-lecture-hall DEFAULT-COLUMNS DEFAULT-ROWS)
      (place-image BALLOON
                   (posn-x (first lop))
                   (posn-y (first lop))
                   (draw-balloons (rest lop)))))

;; N -> Image
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
                                    LECTURE-HALL-COLOR)))

(define (draw-lecture-hall cols rows)
  (cond [(or (zero? cols) (zero? rows))
         empty-image]
        [(and (positive? cols) (positive? rows))
         (overlay (col cols (row rows SEAT))
                  (empty-scene (* cols (image-width SEAT))
                               (* rows (image-width SEAT))
                               LECTURE-HALL-COLOR))]))

;; Balloon -> Balloon
;; creates a new balloon posn per second.
(check-expect (next-balloon BALLOON-EXAMPLE-0) BALLOON-EXAMPLE-0)
(check-random (next-balloon (make-balloon 3 '()))
              (make-balloon 2
                            (add-posn
                             (balloon-positions (make-balloon 3 '())))))

(define (next-balloon balloon)
  (if (zero? (balloon-to-throw balloon))
      balloon
      (make-balloon (sub1 (balloon-to-throw balloon))
                    (add-posn (balloon-positions balloon)))))

;; List-of-Posns -> List-of-Posns
;; adds an entry to given List-of-Posns.
(check-random (add-posn LOP-EXAMPLE-1)
              (cons (make-posn
                     (random (* DEFAULT-COLUMNS (image-width SEAT)))
                     (random (* DEFAULT-ROWS (image-height SEAT))))
                    LOP-EXAMPLE-1))

(define (add-posn lop)
  (cons (make-posn
         (random (* DEFAULT-COLUMNS (image-width SEAT)))
         (random (* DEFAULT-ROWS (image-height SEAT))))
        lop))

;; Balloon -> Boolean
;; ends the program when there are no more balloons left
;; to throw
(check-expect (no-more-balloons? BALLOON-EXAMPLE-0) #true)
(check-expect (no-more-balloons? BALLOON-EXAMPLE-1) #false)

(define (no-more-balloons? balloon)
  (zero? (balloon-to-throw balloon)))

;; Any -> Boolean
;; checks if input is an element of the balloon structure.
(check-expect (valid-balloon? BALLOON-EXAMPLE-0) #true)
(check-expect (valid-balloon? BALLOON-EXAMPLE-1) #true)
(check-expect (valid-balloon? (make-balloon 4 empty-image)) #false)
(check-expect (valid-balloon? (make-balloon "a string" (make-posn 3 3))) #false)

(define (valid-balloon? input)
  (and (number? (balloon-to-throw input))
       (or (zero? (balloon-to-throw input))
           (positive? (balloon-to-throw input)))
       (or (empty? (balloon-positions input))
           (and (cons? (balloon-positions input))
                (posn? (first (balloon-positions input)))))))

;;; application
(test)

(riot 5)
