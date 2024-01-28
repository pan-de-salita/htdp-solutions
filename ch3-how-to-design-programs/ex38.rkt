#lang htdp/bsl
(require test-engine/racket-tests)

; We use strings to represent words.
; String -> String
; Purpose: takes a string and reproduces it with the last character removed.
; given: "hello", expect: "hell"
; given: "world", expect: "worl"
(define (string-remove-last str)
  (substring str 0 (- (string-length str) 1)))

; Test cases
(check-expect (string-remove-last "hello") "hell")
(check-expect (string-remove-last "world") "worl")
