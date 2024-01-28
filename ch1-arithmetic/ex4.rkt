;; Exercise 4. Use the same setup as in exercise 3 to create an expression that deletes
;; the ith position from str. Clearly this expression creates a shorter string than the
;; given one. Which values for i are legitimate?

#lang htdp/bsl
(require test-engine/racket-tests)

;; string number -> string
;; Purpose: Deletes the ith position from str
;; NOTE: 0 < i < (- (string-length str) 1), else error.
(define (delete-at-i str i)
  (string-append (substring str 0 i)
                 (substring str (+ i 1) (string-length str))))

(check-expect (delete-at-i "helloworld" 5) "helloorld")
(check-expect (delete-at-i "helloworld" (- (string-length "helloworld") 1)) "helloworl")
(test)
