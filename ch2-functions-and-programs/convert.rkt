#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)

;; Number -> Number
;; Purpose: Converts a temperature measured on a Fahrenheit thermometer
;; into a Celsius temperature.
;; NOTE: Conversion formula: C = 5/9 x (f - 32)
(define (C f)
  (* 5/9 (- f 32)))

;; File -> File
;; Purpose: Consumes two file names: `in` for the file where the Fahrenheit
;; temperature is found and `out` for where we want the Celsius result.
(define (convert in out)
  (write-file out (string-append
                   (number->string
                    (C
                     (string->number
                      (read-file in))))
                   "\n")))
