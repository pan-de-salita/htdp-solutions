#lang htdp/bsl
(require test-engine/racket-tests)

; We use strings to represent words.
; String -> String
; Purpose: extracts the first character from a non-empty string.
; given: "hello" for str, expect: "h"
; given: "world" for str, expect: "w"
(define (string-first str)
  (substring str 0 1))

; Test cases
(check-expect (string-first "hello") "h")
(check-expect (string-first "world") "w")
