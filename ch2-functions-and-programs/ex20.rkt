;; Exercise 20. Define the function string-delete, which consumes a string plus a
;; number i and deletes the ith position from str. Assume i is a number between 0
;; (inclusive) and the length of the given string (exclusive). See exercise for ideas.
;; Can string-delete deal with empty strings?

#lang htdp/bsl
(require test-engine/racket-tests)
(require 2htdp/image)

;; string -> string
;; Purpose: Consumes a string str plus a number i and deletes the ith position from str.
(define (string-delete str i)
  (cond ((= (string-length str) 0) "invalid; empty string")
        ((or (< i 0) (> i (string-length str))) "invalid; given number cannot be applied")
        (else (string-append (substring str 0 i)
                             (substring str (+ i 1) (string-length str))))))

(check-expect (string-delete "hello" 1) "hllo")
(check-expect (string-delete "" 0) "invalid; empty string")
(check-expect (string-delete "length" 20) "invalid; given number cannot be applied")
(test)
