#lang htdp/bsl
(require test-engine/racket-tests)

; We use strings to represent words.
; String -> String
; Purpose: takes a string and reproduces it with the first character removed.
; given: "hello", expect: "ello"
; given: "world", expect: "orld"
(define (string-rest str)
  (substring str 1))

; Test cases
(check-expect (string-rest "hello") "ello")
(check-expect (string-rest "world") "orld")
