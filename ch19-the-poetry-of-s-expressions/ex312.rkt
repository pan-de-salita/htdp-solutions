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

;; FT -> [List-of String]
;; produces a list of all eye colors in an-ftree

(define (eye-colors an-ftree)
  (match an-ftree
    [(? no-parent?) '()]
    [(? child?)
     (append `(,(child-eyes an-ftree))
             (eye-colors (child-father an-ftree))
             (eye-colors (child-mother an-ftree)))]))

(check-expect (eye-colors CARL) (list "green"))
(check-expect (eye-colors BETTINA) (list "green"))
(check-expect (eye-colors ADAM) (list "hazel" "green" "green"))
(check-expect (eye-colors DAVE) (list "black" "green" "green"))
(check-expect (eye-colors EVA) (list "blue" "green" "green"))
(check-expect (eye-colors FRED) (list "pink"))
(check-expect (eye-colors GUSTAV) (list "brown" "pink" "blue" "green" "green"))

(test)
