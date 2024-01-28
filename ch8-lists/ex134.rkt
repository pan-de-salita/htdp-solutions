#lang racket
(require test-engine/racket-tests)

;; String ListOfStrings -> Boolean
;; determines whether a user-provided string occurs
;; on a given list of strings.
(check-expect (contains? "cat"
                         (cons "cat" (cons "dog" (cons "bird" '()))))
              #true)
(check-expect (contains? "mouse"
                         (cons "keyboard" (cons "laptop" (cons "earphones" '()))))
              #false)

(define (contains? a-string a-list)
  (cond
    [(empty? a-list) #false]
    [(cons? a-list)
     (or (string=? a-string (first a-list))
         (contains? a-string (rest a-list)))]))

(test)
