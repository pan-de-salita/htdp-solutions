;; Exercise 19. Define the functin string-insert, which consumes a string str plus
;; a number i and inserts "_" at the ith position of str. Assume i is a number between
;; 0 and the length of the given string (inclusive). See exercise 3 for ideas. Ponder
;; how string-insert copes with "".

#lang htdp/bsl
(require test-engine/racket-tests)
(require 2htdp/image)

;; string -> string
;; Purpose: Consumes a string str plus a number i and inserts "_" at the ith position of str
(define (string-insert str i)
  (if (or (> i (string-length str)) (< i 0))
      "invalid; given number is greater than length of given string"
      (if (eq? str "")
          "_"
          (string-append (substring str 0 i)
                         "_"
                         (substring str i (string-length str))))))

(check-expect (string-insert "hello" 3) "hel_lo")
(check-expect (string-insert "hello" 10) "invalid; given number is greater than length of given string")
(check-expect (string-insert "" 2) "invalid; given number is greater than length of given string")
(check-expect (string-insert "" 0) "_")
(test)
