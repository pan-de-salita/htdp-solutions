#lang racket
(require test-engine/racket-tests)

;; a Non-Empty-List-of-Booleans is one of:
;; - (cons Boolean '())
;; - (cons Boolean Non-Empty-List-of-Booleans)
;; i.e.: non-empty lists of booleans
(define NON-EMPTY-LIST-OF-BOOLEANS-EXAMPLE-0
  (cons #true '()))
(define NON-EMPTY-LIST-OF-BOOLEANS-EXAMPLE-1
  (cons #true
        (cons #true '())))
(define NON-EMPTY-LIST-OF-BOOLEANS-EXAMPLE-2
  (cons #true
        (cons #false '())))
(define NON-EMPTY-LIST-OF-BOOLEANS-EXAMPLE-3
  (cons #false
        (cons #true '())))
(define NON-EMPTY-LIST-OF-BOOLEANS-EXAMPLE-4
  (cons #false
        (cons #false '())))

;; Non-Empty-List-of-Booleans -> Boolean
;; determines if all values on a Non-Empty-List-of-Booleans
;; is #true
(check-expect (all-true? NON-EMPTY-LIST-OF-BOOLEANS-EXAMPLE-0) #true)
(check-expect (all-true? NON-EMPTY-LIST-OF-BOOLEANS-EXAMPLE-1) #true)
(check-expect (all-true? NON-EMPTY-LIST-OF-BOOLEANS-EXAMPLE-2) #false)
(check-expect (all-true? NON-EMPTY-LIST-OF-BOOLEANS-EXAMPLE-3) #false)
(check-expect (all-true? NON-EMPTY-LIST-OF-BOOLEANS-EXAMPLE-4) #false)

(define (all-true? non-empty-list-of-booleans)
  (cond
    [(empty? (rest non-empty-list-of-booleans))
     (boolean=? (first non-empty-list-of-booleans) #true)] ;; YE uses (first non-empty-list-of-booleans), which is more clever
    [else
     (and (boolean=? (first non-empty-list-of-booleans) #true)
          (all-true? (rest non-empty-list-of-booleans)))]))

;; Non-Empty-List-of-Booleans -> Boolean
;; determines if at least value one element of a Non-Empty-List-of-Booleans
;; is #true
(check-expect (one-true? NON-EMPTY-LIST-OF-BOOLEANS-EXAMPLE-0) #true)
(check-expect (one-true? NON-EMPTY-LIST-OF-BOOLEANS-EXAMPLE-1) #true)
(check-expect (one-true? NON-EMPTY-LIST-OF-BOOLEANS-EXAMPLE-2) #true)
(check-expect (one-true? NON-EMPTY-LIST-OF-BOOLEANS-EXAMPLE-3) #true)
(check-expect (one-true? NON-EMPTY-LIST-OF-BOOLEANS-EXAMPLE-4) #false)

(define (one-true? non-empty-list-of-booleans)
  (cond
    [(empty? (rest non-empty-list-of-booleans))
     (boolean=? (first non-empty-list-of-booleans) #true)] ;; YE uses (first non-empty-list-of-booleans), which is more clever
    [else
     (or (boolean=? (first non-empty-list-of-booleans) #true)
         (one-true? (rest non-empty-list-of-booleans)))]))

(test)
