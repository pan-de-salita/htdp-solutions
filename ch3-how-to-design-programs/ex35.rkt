#lang htdp/bsl
(require test-engine/racket-tests)

; We use strings to represent words.
; String -> String
; Purpose: extracts the last character from a non-empty string.
; given: "hello" for str, expect: "o"
; given: "world" for str, expect: "d"
(define (string-last str)
  (substring str (- (string-length str) 1) (string-length str)))

; Test cases
(check-expect (string-last "hello") "o")
(check-expect (string-last "world") "d")
