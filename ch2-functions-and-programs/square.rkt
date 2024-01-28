#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)

(define (number->square s)
  (square s "solid" "red"))

(define (reset s ke)
  100)

(big-bang 100
          [to-draw number->square]
          [on-tick sub1]
          [stop-when zero?]
          [on-key reset])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (big-bang cw0
;;           [on-tick tock]
;;           [on-key ke-h]
;;           [on-mouse me-h]
;;           [to-draw render]
;;           [stop-when end?]
;;           ...)

;; (define cw1 (ke-h cw0 "a"))
;; (define cw2 (tock cw1))
;; (define cw3 (me-h cw2 "button-down" 90 100))

;; (me-h
;;  (tock
;;   (ke-h cw0 "a"))
;;  "button-down" 90 100)
