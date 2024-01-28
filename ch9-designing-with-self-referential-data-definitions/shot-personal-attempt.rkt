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

;; a Shot is a Number
;; i.e. the y-coordinate of a shot

;; a List-of-Shots is one of:
;; - '()
;; - (cons Shot List-of-Shots)
;; i.e. the collection of shots fired
(define LOS-EXAMPLE-0 '())
(define LOS-EXAMPLE-1 (cons YSHOTS-START '()))
(define LOS-EXAMPLE-2 (cons (/ HEIGHT 2) '()))

;;; functions

;; List-of-Shots -> Image
;; places a SHOT in BACKGROUND according to
;; List-of-Shots (los) given
(check-expect (render-shot LOS-EXAMPLE-0) BACKGROUND)
(check-expect (render-shot LOS-EXAMPLE-1)
              (place-image SHOT
                           XSHOTS YSHOTS-START
                           (render-shot (rest LOS-EXAMPLE-1))))
(check-expect (render-shot LOS-EXAMPLE-2)
              (place-image SHOT
                           XSHOTS (/ HEIGHT 2)
                           (render-shot (rest LOS-EXAMPLE-2))))

(define (render-shot los)
  (cond
    [(empty? los) BACKGROUND]
    [(cons? los)
     (place-image SHOT
                  XSHOTS (first los)
                  (render-shot (rest los)))]))

;; List-of-Shots KeyEvent -> List-of-Shots
;; controls the behavior of a shot according to
;; user's KeyEvent:
;; - " " (LAUNCH) | sets x-coordinate of shot to
;;   bottom of BACKGROUND if los is empty
(check-expect (control-shot LOS-EXAMPLE-0 " ") LOS-EXAMPLE-1)
(check-expect (control-shot LOS-EXAMPLE-1 " ") LOS-EXAMPLE-1)
(check-expect (control-shot LOS-EXAMPLE-2 " ") LOS-EXAMPLE-2)
(check-expect (control-shot LOS-EXAMPLE-0 "l") LOS-EXAMPLE-0)
(check-expect (control-shot LOS-EXAMPLE-1 "l") LOS-EXAMPLE-1)
(check-expect (control-shot LOS-EXAMPLE-2 "l") LOS-EXAMPLE-2)

(define (control-shot los ke)
  (cond
    [(not (and (key=? ke LAUNCH) (empty? los))) los]
    [else (cons YSHOTS-START '())]))

;; List-of-Shots -> List-of-Shots
;; if shot is launched, controls its y-coordinate
;; according to clock tick
(check-expect (shot-per-tick LOS-EXAMPLE-0) LOS-EXAMPLE-0)
(check-expect (shot-per-tick LOS-EXAMPLE-1) (cons (- YSHOTS-START YDELTA-SHOTS) '()))
(check-expect (shot-per-tick LOS-EXAMPLE-2) (cons (- (/ HEIGHT 2) YDELTA-SHOTS) '()))
(check-expect (shot-per-tick (cons YSHOTS-LIMIT '())) (cons (- YSHOTS-LIMIT 1) '()))
(check-expect (shot-per-tick (cons (- YSHOTS-LIMIT 1) '())) '())

(define (shot-per-tick los)
  (cond
    [(empty? los) los]
    [(cons? los) (new-shot-posn los)]))

;; List-of-Shots -> List-of-Shots
;; raises a shot by YDELTA-SHOTS if it is within
;; YSHOTS-LIMIT, else returns an empty List-of-Shots
(check-expect (new-shot-posn LOS-EXAMPLE-1) (cons (- YSHOTS-START YDELTA-SHOTS) '()))
(check-expect (new-shot-posn LOS-EXAMPLE-2) (cons (- (/ HEIGHT 2) YDELTA-SHOTS) '()))
(check-expect (new-shot-posn (cons YSHOTS-LIMIT '())) (cons (- YSHOTS-LIMIT 1) '()))
(check-expect (new-shot-posn (cons (- YSHOTS-LIMIT 1) '())) '())

(define (new-shot-posn los)
  (if (>= (first los) YSHOTS-LIMIT)
      (cons (- (first los) YDELTA-SHOTS) '())
      '()))

;; List-of-Shots -> List-of-Shots
;; main function
(define (launch-shot los)
  (big-bang los
            [to-draw render-shot]
            [on-key control-shot]
            [on-tick shot-per-tick]))

;;; application
(launch-shot LOS-EXAMPLE-0)

(test)
