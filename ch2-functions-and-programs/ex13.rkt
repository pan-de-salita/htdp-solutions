;; Exercise 13. Define the functon string-first, which extracts the first 1String from
;; a non-empty string.

#lang htdp/bsl
(require test-engine/racket-tests)

;; string -> 1string
;; Purpose: Extracts the first 1string from a non-empty string.
(define (string-first str)
  (if (empty? str)
      "invalid; empty string"
      (substring str 0 1)))

(check-expect (string-first "hello world") "h")
(test)
