;; Exercise 47. Design a world program that maintains
;; a happiness gauge.

#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require test-engine/racket-tests)

;; Definitions
(define H-CEILING 20)
(define H-PER-TICK 0.1)
(define H-PER-DOWN-KEY 1/5)
(define H-PER-UP-KEY -1/3)

(define H-FRAME
  (rectangle (* H-CEILING 16) (* H-CEILING 1.5) "solid" "black"))
(define PX-BETWEEN-BAR-FRAME 10)

(define H-FULL
  (rectangle
    (+ (* H-CEILING 15) PX-BETWEEN-BAR-FRAME)
    (* H-CEILING 1)
    "solid" "red"))

(define H-START
  (overlay/align/offset
    "left" "center"
    H-FULL
    (- (/ PX-BETWEEN-BAR-FRAME 2)) 0
    H-FRAME))

(define BACKGROUND
  (empty-scene (* H-CEILING 20) (* H-CEILING 3)))

;; Functions

;; NOTE
;; A WorldState is either a Number or a String.
;; Interpretation: Either the number of clock ticks or
;; keystroke since the program started.

(define (h-change ws)
  (overlay/align/offset
    "left" "center"
    [rectangle
      (- (image-width H-FULL) ws)
      (image-height H-FULL)
      "solid" "red"]
    (- (/ PX-BETWEEN-BAR-FRAME 2)) 0
    H-FRAME))

(define (render ws)
  (cond
    [(= 0 ws) H-START]
    [else (h-change ws)]))

(define (tick-handler ws)
  (+ ws H-PER-TICK))

(define (key-handler ws a-key)
  (cond [(string=? a-key "down") (+ ws H-PER-DOWN-KEY)]
        [(string=? a-key "up") (+ ws H-PER-UP-KEY)]
        [else ws]))

(define (depressed? ws)
  (= 0 (round (- ws (image-width H-FULL)))))

(define (gauge-prog ws)
  (big-bang ws
    [to-draw render]
    [on-tick tick-handler]
    [on-key key-handler]
    [stop-when depressed?]))
