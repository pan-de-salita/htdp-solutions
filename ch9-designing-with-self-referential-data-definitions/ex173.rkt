#lang racket
(require test-engine/racket-tests)

;; a List-of-Strings is one of:
;; - '()
;; - (cons String '())
(define a-list-example-0 '())
(define a-list-example-1 (cons "a" '()))
(define a-list-example-2 (cons "b" (cons "a" '())))

;; Any -> Number
;; determines how many strings occur on a List-of-Strings if
;; a List-of-Strings is provided
(check-expect (how-many a-list-example-0) 0)
(check-expect (how-many a-list-example-1) 1)
(check-expect (how-many a-list-example-2) 2)
(check-error (how-many (cons 32 (cons "string" '()))) "how-many: list of strings required")
(check-error (how-many #true) "how-many: list of strings required")

(define (how-many a-list-of-strings)
  (if (list-of-strings? a-list-of-strings)
      (cond
        [(empty? a-list-of-strings) 0]
        [else (add1 (how-many (rest a-list-of-strings)))])
      (error "how-many: list of strings required")))

;; Any -> Boolean
;; determines if a given input is a List-of-Strings
(check-expect (list-of-strings? a-list-example-2) #true)
(check-expect (list-of-strings? (cons 12 (cons "a" '()))) #false)
(check-expect (list-of-strings? 43) #false)
(check-expect (list-of-strings? #true) #false)

(define (list-of-strings? input)
  (if (list? input)
      (or (empty? input)
          (and (string? (first input))
               (list-of-strings? (rest input))))
      #false))

(test)
