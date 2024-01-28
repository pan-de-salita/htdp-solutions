#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

(define MESSAGE-0 "light=? | both arguments invalid; traffic light expected")
(define MESSAGE-1 "light=? | first argument invalid; traffic light expected")
(define MESSAGE-2 "light=? | second argument invalid; traffic light expected")

;; Any -> Boolean
;; is the given value an element of TrafficLight
;; (TrafficLight defined in earlier exercise)
(check-expect (light? "red") #t)
(check-expect (light? "green") #t)
(check-expect (light? "yellow") #t)
(check-expect (light? "potato") #f)
(check-expect (light? 12) #f)
(check-expect (light? empty-image) #f)
(check-expect (light? #f) #f)

(define (light? x)
  (cond [(string? x) (or (string=? "red" x)
                         (string=? "green" x)
                         (string=? "yellow" x))]
        [else #f]))

;; Any Any -> Boolean
;; are the two values elements of TrafficLight and,
;; if so, are they equal
(check-expect (light=? "red" "red") #t)
(check-expect (light=? "red" "green") #f)
(check-expect (light=? "green" "green") #t)
(check-expect (light=? "yellow" "yellow") #t)
(check-error (light=? "potato" "potato") MESSAGE-0)
(check-error (light=? "potato" "red") MESSAGE-1)
(check-error (light=? "red" empty-image) MESSAGE-2)

(define (light=? a-value another-value)
  (if (and (light? a-value) (light? another-value))
      (string=? a-value another-value)
      (error (cond
               [(and (boolean=? (light? a-value) #f) (boolean=? (light? another-value) #f))
                MESSAGE-0]
               [(boolean=? (light? a-value) #f)
                MESSAGE-1]
               [(boolean=? (light? another-value) #f)
                MESSAGE-2]))))

(test)
