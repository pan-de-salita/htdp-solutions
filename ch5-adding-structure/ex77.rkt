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

(define-struct time [hour minute sec])
;; a Time is a structure:
;;  (make-time Hour Minute Second)
