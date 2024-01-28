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

;; FT -> Boolean
;; our "friend's" incorrect function to check if any family
;; members before an-ftree's immediate child strucure has blue eyes
(define (blue-eyed-ancestor?/wrong an-ftree)
  (match an-ftree
    [(? no-parent?) #f]
    [(? child?)
     (or (blue-eyed-ancestor?/wrong (child-father an-ftree))
         (blue-eyed-ancestor?/wrong (child-mother an-ftree)))]))

(check-expect (blue-eyed-ancestor?/wrong CARL) #f)
(check-expect (blue-eyed-ancestor?/wrong BETTINA) #f)
(check-expect (blue-eyed-ancestor?/wrong ADAM) #f)
(check-expect (blue-eyed-ancestor?/wrong DAVE) #f)
(check-expect (blue-eyed-ancestor?/wrong EVA) #f)
(check-expect (blue-eyed-ancestor?/wrong FRED) #f)
(check-expect (blue-eyed-ancestor?/wrong GUSTAV) (not #t)) ;; a correct solution would return #t

;; their solution returns only #f because the function isn't designed to
;; check whether "blue" figures within an FT. to fix this, we can use
;; the previously designed blue-eyed-color to be an auxiliary function
;; for checking whether a child's ancestors possess blue eyes:

;; FT -> Boolean
;; checks if an-ftree contains a child structure with
;; "blue" in the eyes field
(define (blue-eyed-child? an-ftree)
  (match an-ftree
    [(? no-parent?) #f]
    [(child father mother name year eyes)
     (or (string=? eyes "blue")
         (blue-eyed-child? father)
         (blue-eyed-child? mother))]))

(check-expect (blue-eyed-child? CARL) #f)
(check-expect (blue-eyed-child? BETTINA) #f)
(check-expect (blue-eyed-child? ADAM) #f)
(check-expect (blue-eyed-child? DAVE) #f)
(check-expect (blue-eyed-child? EVA) #t)
(check-expect (blue-eyed-child? FRED) #f)
(check-expect (blue-eyed-child? GUSTAV) #t)

;; FT -> Boolean
;; checks if any family members before an-ftree's immediate
;; child strucure has blue eyes
(define (blue-eyed-ancestor? an-ftree)
  (match an-ftree
    [(? no-parent?) #f]
    [(child father mother name year eyes)
     (or (blue-eyed-child? father)
         (blue-eyed-child? mother))]))

(check-expect (blue-eyed-ancestor? CARL) #f)
(check-expect (blue-eyed-ancestor? BETTINA) #f)
(check-expect (blue-eyed-ancestor? ADAM) #f)
(check-expect (blue-eyed-ancestor? DAVE) #f)
(check-expect (blue-eyed-ancestor? EVA) #f)
(check-expect (blue-eyed-ancestor? FRED) #f)
(check-expect (blue-eyed-ancestor? GUSTAV) #t)

(test)
