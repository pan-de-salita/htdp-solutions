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

;; FT -> Number
;; counts the child-structures in an FT
(define (count-persons an-ftree)
  (cond [(no-parent? an-ftree) 0]
        [else (+ 1
                 (count-persons (child-father an-ftree))
                 (count-persons (child-mother an-ftree)))]))

(check-expect (count-persons CARL) 1)
(check-expect (count-persons BETTINA) 1)
(check-expect (count-persons ADAM) 3)
(check-expect (count-persons DAVE) 3)
(check-expect (count-persons EVA) 3)
(check-expect (count-persons FRED) 1)
(check-expect (count-persons GUSTAV) 5)

(test)
