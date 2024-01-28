#lang racket
(require test-engine/racket-tests)

;; a List-of-Booleans is one of:
;; - '()
;; - (cons Boolean List-of-Booleans)
;; i.e.: a sequence of Booleans
(define a-list-of-booleans-example-0
  '())
(define a-list-of-booleans-example-1
  (cons #true '()))
(define a-list-of-booleans-example-2
  (cons #false '()))
(define a-list-of-booleans-example-3
  (cons #true
        (cons #false '())))


;; List-of-Booleans -> Boolean
;; determines if all values on a List-of-Booleans
;; is #true
(check-expect (all-true? a-list-of-booleans-example-0) #true)
(check-expect (all-true? a-list-of-booleans-example-1) #true)
(check-expect (all-true? a-list-of-booleans-example-2) #false)
(check-expect (all-true? a-list-of-booleans-example-3) #false)

(define (all-true? a-list-of-booleans)
  (cond
    [(empty? a-list-of-booleans)
     #true]
    [else
     (and (boolean=? (first a-list-of-booleans) #true)
          (all-true? (rest a-list-of-booleans)))]))

;; List-of-Booleans -> Boolean
;; determines if at least value on a List-of-Booleans
;; is #true
(check-expect (one-true? a-list-of-booleans-example-0) #true)
(check-expect (one-true? a-list-of-booleans-example-1) #true)
(check-expect (one-true? a-list-of-booleans-example-2) #false)
(check-expect (one-true? a-list-of-booleans-example-3) #true)

(define (one-true? a-list-of-booleans)
  (if (and (cons? a-list-of-booleans)
           (boolean=? (first a-list-of-booleans) #false))
      #false
      (cond
        [(empty? a-list-of-booleans)
         #true]
        [else
         (or (boolean=? (first a-list-of-booleans) #true)
             (one-true? (rest a-list-of-booleans)))])))

(test)
