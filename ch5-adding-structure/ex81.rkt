#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;; an Hour is a Number from 0 through 24, that is, [0,24]
;; interpretation: the number of hours in a day

;; a Minute is a Number from 0 through 60, that is, [0,60]
;; interpretation: the number of minutes in an hour

;; a Second is a Number from 0 through 60, that is, [0,60]
;; interpretation: the number of seconds in a minute

(define MINUTES/HOUR 60)
(define SECONDS/MINUTE 60)

(define-struct time [hours minutes seconds])
;; a Time is a structure:
;;  (make-time Hour Minute Second)

(define time-example (make-time 12 30 2))

;; Time -> Number
;; returns the number of seconds that have passed since midnight
(check-expect (time->seconds time-example) 45002)

(define (time->seconds time-entry)
  (+ (hours->seconds (time-hours time-entry))
     (minutes->seconds (time-minutes time-entry))
     (time-seconds time-entry)))

;; Number -> Number
;; converts hours to seconds
(check-expect (hours->seconds 12) (* 12 MINUTES/HOUR SECONDS/MINUTE))

(define (hours->seconds hours)
  (minutes->seconds (* hours MINUTES/HOUR)))

;; Number -> Number
;; converts minutes to seconds
(check-expect (minutes->seconds 30) (* 30 SECONDS/MINUTE))

(define (minutes->seconds minutes)
  (* minutes SECONDS/MINUTE))

(test)
