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

;; youngest generation
(define GUSTAV (make-child FRED EVA "Gustav" 1988 "brown"))

;; an FF (short for family forest) is one of:
;; - '()
;; - [List-of FT]
;; a family forest represents several familits (say, a town)
;; and their ancestor trees

(define FF1 (list CARL BETTINA))
(define FF2 (list FRED EVA))
(define FF3 (list FRED EVA CARL))

;; FT Number -> Number
;; returns the total age of all Child instances in an FT

(check-expect (total-age-in-tree CARL 2023)
              (- 2023 1926))
(check-expect (total-age-in-tree BETTINA 2023)
              (- 2023 1926))
(check-expect (total-age-in-tree ADAM 2023)
              (+ (- 2023 1950)
                 (- 2023 1926)
                 (- 2023 1926)))
(check-expect (total-age-in-tree DAVE 2023)
              (+ (- 2023 1955)
                 (- 2023 1926)
                 (- 2023 1926)))
(check-expect (total-age-in-tree EVA 2023)
              (+ (- 2023 1965)
                 (- 2023 1926)
                 (- 2023 1926)))
(check-expect (total-age-in-tree FRED 2023)
              (- 2023 1966))
(check-expect (total-age-in-tree GUSTAV 2023)
              (+ (- 2023 1988)
                 (- 2023 1966)
                 (- 2023 1965)
                 (- 2023 1926)
                 (- 2023 1926)))

(define (total-age-in-tree ft current-year)
  (match ft
    [(? no-parent?) 0]
    [(? child?) (+ (- current-year (child-year ft))
                   (total-age-in-tree (child-father ft) current-year)
                   (total-age-in-tree (child-mother ft) current-year))]))

;; FF Number -> Number
;; returns the total age of all Child instances in an FF

(check-expect (total-age-in-forest FF1 2023)
              (+ (total-age-in-tree CARL 2023) (total-age-in-tree BETTINA 2023)))
(check-expect (total-age-in-forest FF2 2023)
              (+ (total-age-in-tree FRED 2023) (total-age-in-tree EVA 2023)))
(check-expect (total-age-in-forest FF3 2023)
              (+ (total-age-in-tree FRED 2023) (total-age-in-tree EVA 2023) (total-age-in-tree CARL 2023) ))

(define (total-age-in-forest ff current-year)
  (foldr + 0 (map (lambda (ft) (total-age-in-tree ft current-year)) ff)))

;; FT -> Number
;; returns the total number of Child instances within an FT

(check-expect (total-children-in-tree CARL) 1)
(check-expect (total-children-in-tree BETTINA) 1)
(check-expect (total-children-in-tree ADAM) 3)
(check-expect (total-children-in-tree DAVE) 3)
(check-expect (total-children-in-tree EVA) 3)
(check-expect (total-children-in-tree FRED) 1)
(check-expect (total-children-in-tree GUSTAV) 5)

(define (total-children-in-tree ft)
  (match ft
    [(? no-parent?) 0]
    [(? child?) (+ 1
                   (total-children-in-tree (child-father ft))
                   (total-children-in-tree (child-mother ft)))]))

;; FF -> Number
;; returns the total number of Child instances within an FF

(check-expect (total-children-in-forest FF1)
              (+ (total-children-in-tree CARL) (total-children-in-tree BETTINA)))
(check-expect (total-children-in-forest FF2)
              (+ (total-children-in-tree FRED) (total-children-in-tree EVA)))
(check-expect (total-children-in-forest FF3)
              (+ (total-children-in-tree FRED) (total-children-in-tree EVA) (total-children-in-tree CARL)))

(define (total-children-in-forest ff)
  (foldr + 0 (map total-children-in-tree ff)))

;; FF Number -> Number
;; returns the average age of all Child instances in an FF

(check-expect (average-age-in-forest FF1 2023)
              (/ (total-age-in-forest FF1 2023) (total-children-in-forest FF1)))
(check-expect (average-age-in-forest FF2 2023)
              (/ (total-age-in-forest FF2 2023) (total-children-in-forest FF2)))
(check-expect (average-age-in-forest FF3 2023)
              (/ (total-age-in-forest FF3 2023) (total-children-in-forest FF3)))

(define (average-age-in-forest ff current-year)
  (/ (total-age-in-forest ff current-year) (total-children-in-forest ff)))

(test)
