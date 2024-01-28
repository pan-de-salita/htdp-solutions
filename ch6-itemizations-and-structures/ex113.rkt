#lang racket
(require test-engine/racket-tests)
(require lang/posn)

;; predicate for SIGS

;; Any -> SIGS
;; is s an example of the SIGS collection
(define (sigs-given? s)
  (if (not
       (and (sigs? s)
            (posn? (sigs-ufo s))
            (tank? (sigs-tank s))
            (or (false? (sigs-missile/#f s)) (posn? (sigs-missile/#f s)))))
      #f
      #t))

;; predicate for Coordinate

;; Any -> Coordinate
;; are y, x, and p elements of the Coordinate collection
(define (coordinate? y x p)
  (if (not
       (and (negative? y)
            (positive? x)
            (posn? p)))
      #false
      #true))

;; predicate for VAnimal

;; Any -> VAnimal
;; are vc and vch elements of the VAnimal collection
(define (v-animal? va)
  (or (VCat? va) (VCham? va)))
