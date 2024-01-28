#lang racket
(require test-engine/racket-tests)

(define-struct child [father mother name year eyes] #:transparent)
;; a Child is a structure:
;;   (make-child Child Child String Number String)
(define-struct no-parent [] #:transparent)
(define NP (make-no-parent))
;; an FT (short for family tree) is one of:
;; - NP
;; - (make-child FT FT String Number String)

;; oldest generation:
(define CARL (make-child NP NP "Carl" 1926 "green"))
(define BETTINA (make-child NP NP "Bettina" 1926 "green"))

;; middle generation:
(define ADAM (make-child CARL BETTINA "Adam" 1950 "hazel"))
(define DAVE (make-child CARL BETTINA "Dave" 1955 "black"))
(define EVA (make-child CARL BETTINA "Eva" 1965 "blue"))
(define FRED (make-child NP NP "Fred" 1966 "pink"))

;; youngest generation:
(define GUSTAV (make-child FRED EVA "Gustav" 1988 "brown"))

;; FT -> Number
;; counts the child structures in an FT
(define (count-persons an-ftree)
  (match an-ftree
    [(? no-parent?) 0]
    [(? child?)
     (+ 1
        (count-persons (child-father an-ftree))
        (count-persons (child-mother an-ftree)))]))

(check-expect (count-persons CARL) 1)
(check-expect (count-persons BETTINA) 1)
(check-expect (count-persons ADAM) 3)
(check-expect (count-persons DAVE) 3)
(check-expect (count-persons EVA) 3)
(check-expect (count-persons FRED) 1)
(check-expect (count-persons GUSTAV) 5)

;; FT -> Number
;; calculates the total age of all child structures within an-ftree
(define (total-age an-ftree current-year)
  (match an-ftree
    [(? no-parent?) 0]
    [(? child?)
     (+ (- current-year (child-year an-ftree))
        (total-age (child-father an-ftree) current-year)
        (total-age (child-mother an-ftree) current-year))]))

(check-expect (total-age CARL 2023) 97)
(check-expect (total-age BETTINA 2023) 97)
(check-expect (total-age ADAM 2023) 267)
(check-expect (total-age DAVE 2023) 262)
(check-expect (total-age EVA 2023) 252)
(check-expect (total-age FRED 2023) 57)
(check-expect (total-age GUSTAV 2023) 344)

;; FT Number -> Number
;; calculates the average age of all child structures in an-ftree
(define (average-age an-ftree current-year)
  (/ (total-age an-ftree current-year) (count-persons an-ftree)))

(check-expect (average-age CARL 2023) 97)
(check-expect (average-age BETTINA 2023) 97)
(check-expect (average-age ADAM 2023) 89)
(check-expect (average-age DAVE 2023) 262/3)
(check-expect (average-age EVA 2023) 84)
(check-expect (average-age FRED 2023) 57)
(check-expect (average-age GUSTAV 2023) 344/5)

(test)
