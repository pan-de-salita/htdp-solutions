;; Exercise 3. Ad the following two lines to the definitions area:
;;
;; (define str "helloworld")
;; (define i 5)
;;
;; Then create an expression using string primitives that adds "_" at position i. In
;; general this means the resulting string is no longer than the original one; here the
;; expected result is "hello_world".
;;
;; Position means i characters from the left of the string, but programmers start
;; counting at 0. Thus, the 5th letter in this example is "w", because the 0th letter is
;; "h".

#lang htdp/bsl
(require test-engine/racket-tests)

;; string number -> string
;; Purpose: Adds "_" at position i of a given string.
(define (underscore-at-i str i)
  (string-append (substring str 0 i)
                 "_"
                 (substring str i (string-length str))))

(check-expect (underscore-at-i "helloworld" 5) "hello_world")
(check-expect (underscore-at-i "positionmeans" 8) "position_means")
(test)
