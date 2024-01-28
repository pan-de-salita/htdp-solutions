;; Exercise 14. Define the functon string-last, which extracts the last 1String from
;; a non-empty string.

#lang htdp/bsl
(require test-engine/racket-tests)

;; string -> 1string
;; Purpose: Extracts the last 1string from a non-empty string
(define (string-last str)
  (if (empty? str)
      "invalid; empty string"
      (substring str
                 (- (string-length str) 1)
                 (string-length str))))

(check-expect (string-last "hello world") "d")
(test)
