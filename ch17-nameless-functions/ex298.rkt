#lang racket
(require
 test-engine/racket-tests
 2htdp/image
 2htdp/universe)

(define DOT-RADIUS 3)
(define DOT-X 50)
(define MT-SCENE-WIDTH 60)
(define MT-SCENE-HEIGHT 60)

(define WORLD-START 0)
(define WORLD-END MT-SCENE-HEIGHT)
(define WORLD-RATE 1/30)

(define DOT (circle DOT-RADIUS "solid" "red"))
(define MT-SCENE (empty-scene MT-SCENE-WIDTH MT-SCENE-HEIGHT "dimgrey"))

;; an ImageStream is a function:
;;   [Number -> Image]
;; i.e. a stream s denotes a series of images
;; example of an ImageStream:
(define (CREATE-DOT-SCENE height)
  (place-image DOT DOT-X height MT-SCENE))

;; ImageStream Number -> Number
;; clone of animate

(check-expect (my-animate CREATE-DOT-SCENE WORLD-END) WORLD-END)

(define (my-animate img-stream end)
  (big-bang WORLD-START ;; WorldState; is a Number
            [to-draw (render img-stream)]
            [on-tick tock WORLD-RATE]
            [stop-when (end-reached? end)]))

;; ImageStream -> Image
;; renders an instance of img-stream according to time-passed

(check-expect
 [(render CREATE-DOT-SCENE) 0]
 (place-image DOT DOT-X 0 MT-SCENE))

(define (render img-stream)
  (lambda (time-passed) (img-stream time-passed)))

;; Number -> Number
;; adds 1 to time-passed per clock tick

(check-expect (tock 0) 1)

(define (tock time-passed)
  (add1 time-passed))

;; Number -> Boolean
;; stops my-animate when n is reached

(check-expect [(end-reached? 30) 10] #f)
(check-expect [(end-reached? 30) 30] #t)

(define (end-reached? n)
  (lambda (time-passed) (= time-passed n)))

(test)
