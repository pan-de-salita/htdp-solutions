;; Exercise 18. Define the function string-join, which consumes two strings and
;; appends them with "_" in between. See exercise 2 for ideas.

#lang htdp/bsl
(require test-engine/racket-tests)
(require 2htdp/image)

;; string string -> string
;; Purpose: Consumes two strings and appends them with "_" in between
(define (string-join str1 str2)
  (string-append str1 "_" str2))

(check-expect (string-join "hello" "world") "hello_world")
(test)
