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

;; FT -> Boolean
;; checks if an-ftree contains a child structure with
;; "blue" in the eyes field

(check-expect (blue-eyed-child? CARL) #f)
(check-expect (blue-eyed-child? BETTINA) #f)
(check-expect (blue-eyed-child? ADAM) #f)
(check-expect (blue-eyed-child? DAVE) #f)
(check-expect (blue-eyed-child? EVA) #t)
(check-expect (blue-eyed-child? FRED) #f)
(check-expect (blue-eyed-child? GUSTAV) #t) ;; Eva has blue eyes

(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #f]
    [else (or (string=? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree)))]))

;; an FF (short for family forest) is one of:
;; - '()
;; - [List-of FT]
;; a family forest represents several familits (say, a town)
;; and their ancestor trees

(define FF1 (list CARL BETTINA))
(define FF2 (list FRED EVA))
(define FF3 (list FRED EVA CARL))

;; [List-of FT] -> Boolean
;; checks if an FF contains a Child with "blue" in the eyes field

(check-expect (blue-eyed-child-in-forest?.v2 FF1) #f)
(check-expect (blue-eyed-child-in-forest?.v2 FF2) #t)
(check-expect (blue-eyed-child-in-forest?.v2 FF3) #t)

(define (blue-eyed-child-in-forest?.v2 a-family-forest)
  (for/or ([a-family-tree a-family-forest])
    (blue-eyed-child? a-family-tree)))

;; [List-of FT] -> Boolean
;; like blue-eyed-child-in-forest?.v2, checks if an FF contains a Child
;; with "blue" in the eyes field

(check-expect (blue-eyed-child-in-forest?.v3 FF1) #f)
(check-expect (blue-eyed-child-in-forest?.v3 FF2) #t)
(check-expect (blue-eyed-child-in-forest?.v3 FF3) #t)

(define (blue-eyed-child-in-forest?.v3 a-family-forest)
  (match a-family-forest
    [(? empty?) #f]
    [(cons first-family-tree rest-of-family-forest)
     (or (blue-eyed-child? first-family-tree)
         (blue-eyed-child-in-forest?.v3 rest-of-family-forest))]))

(test)
