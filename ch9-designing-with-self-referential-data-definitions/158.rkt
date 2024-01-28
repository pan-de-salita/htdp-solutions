#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;;; constants

(define HEIGHT 80)
(define WIDTH 100)
(define BACKGROUND (empty-scene WIDTH HEIGHT "dimgrey"))

(define SHOT (triangle 3 "solid" "red"))
(define YSHOTS-START (- HEIGHT (/ (image-height SHOT) 2)))
(define YSHOTS-LIMIT (- 0 (/ (image-height SHOT) 2)))
(define XSHOTS (/ WIDTH 2))
(define YDELTA-SHOTS 1)

(define LAUNCH " ")

;;; data definitions

;; a ShotWorld is a List-of-Numbers
;; i.e. each number on such a list represents
;; the y-coordinate of a shot.
(define SW-EX0 '())
(define SW-EX1 (cons YSHOTS-START '()))
(define SW-EX2
  (cons (/ HEIGHT 2)
        (cons YSHOTS-START '())))

;;; functions

;; ShotWorld -> Image
;; adds the image of a shot for each y on sw at
;; (XSHOTS,y) to BACKGROUND
(check-expect (to-image SW-EX0) BACKGROUND)
(check-expect (to-image SW-EX1)
              (place-image SHOT
                           XSHOTS (first SW-EX1)
                           (to-image (rest SW-EX1))))
(check-expect (to-image SW-EX2)
              (place-image SHOT
                           XSHOTS (first SW-EX2)
                           (to-image (rest SW-EX2))))

(define (to-image sw)
  (cond
    [(and (cons? sw) (number? (first sw)))
     (place-image SHOT XSHOTS (first sw)
                  (to-image (rest sw)))]
    [else BACKGROUND]))

;; ShotWorld KeyEvent -> ShotWorld
;; adds a shot to the world if the player presses
;; the space bar.
(check-expect (keyh SW-EX0 " ") (cons YSHOTS-START SW-EX0))
(check-expect (keyh SW-EX0 "l") SW-EX0)

(define (keyh sw ke)
  (cond
    [(key=? ke LAUNCH) (cons YSHOTS-START sw)]
    [else sw]))

;; ShotWorld -> ShotWorld
;; determines the y-coordinate of a launched shot.
(check-expect (tock SW-EX0) SW-EX0)
(check-expect (tock SW-EX1) (cons (- (first SW-EX1) YDELTA-SHOTS) '()))
(check-expect (tock SW-EX2)
              (cons (- (/ HEIGHT 2) YDELTA-SHOTS)
                    (cons (- (first SW-EX1) YDELTA-SHOTS) '())))
(check-expect (tock (cons YSHOTS-LIMIT '()))
              (cons (- YSHOTS-LIMIT YDELTA-SHOTS) '()))
(check-expect (tock (cons (- YSHOTS-LIMIT 1)'()))
              (cons '() '()))

(define (tock sw)
  (cond
    [(empty? sw) sw]
    [(cons? sw)
     (cons (new-shot-posn (first sw))
           (tock (rest sw)))]))

;; Number -> Number
;; determines the new y-coordinate of a fired shot:
;; - if the shot is within frame, moves the former up
;;   by YDELTA-SHOTS
;; - if the shot is out of frame, converts the y-coordinate
;;   to an empty list
(check-expect (new-shot-posn YSHOTS-START) (- YSHOTS-START YDELTA-SHOTS))
(check-expect (new-shot-posn (/ HEIGHT 2)) (- (/ HEIGHT 2) YDELTA-SHOTS))
(check-expect (new-shot-posn YSHOTS-LIMIT) (- YSHOTS-LIMIT YDELTA-SHOTS))
(check-expect (new-shot-posn (- YSHOTS-LIMIT 1)) '())

(define (new-shot-posn y-cor)
  (cond [(and (number? y-cor) (>= y-cor YSHOTS-LIMIT))
         (- y-cor YDELTA-SHOTS)]
        [else '()]))

;; ShotWorld -> ShotWorld
;; main function
(define (launch-shot sw)
  (big-bang sw
            [to-draw to-image]
            [on-key keyh]
            [on-tick tock]))

;;; application
(test)
(launch-shot SW-EX0)
