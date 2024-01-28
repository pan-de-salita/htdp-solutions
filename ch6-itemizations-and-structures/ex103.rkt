#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)

;;; constant and structure definitions

;; the following are PositiveNumbers:
;; - X
;; - Y
;; - Z
;; describes the dimensions of a cage.

;; a CageVolume is a structure:
;;  (make-cage-volume X Y Z)
;; used to describe the dimensions of a cage;
;; facilitates the calculation of its volume.
(define-struct cage-dimension [x y z])

(define CAGE-DIMENSION-TEST
  (make-cage-dimension 400 400 400))

;; an AnimalDimension is a structure:
;;  (make-animal-size X Y Z)
;; used to describe the dimensions of an animal;
;; facilitates the calculation of its size.
(define-struct animal-dimension [x y z])

(define ANIMAL-DIMENSION-TEST
  (make-animal-dimension 300 300 300))

;; a Spider is a structure:
;;  (make-spider PositiveNumber AnimalDimension)
;; describes the following traits of a spider:
;; - Number of remaining legs post-transport
;; - AnimalDimension of given spider
(define-struct spider [legs-left dimensions])

(define SPIDER-TEST
  (make-spider 8 (make-animal-dimension 50 20 50)))

;; an Elephant is a structure:
;;  (make-elephant AnimalDimension)
;; describes the AnimalDimension of given elephant.
(define-struct elephant [dimensions])

(define ELEPHANT-TEST
  (make-elephant (make-animal-dimension 500 500 500)))

;; a Girth is a Number.
;; describes the circumference of a Boa.

;; a Boa is a structure:
;;  (make-boa PositiveNumber PositiveNumber)
;; decribes the X and Girth of given boa.
(define-struct boa [x girth])

(define BOA-TEST
  (make-boa 300 100))

;; an Armadillo is a structure:
;;  (make-armadillo PositiveNumber AnimalDimension)
;; describes the following:
;; - the length of the armadillo's tail
;; - its AnimalDimension
(define-struct armadillo [tail-length dimensions])

(define ARMADILLO-TEST
  (make-armadillo 30 (make-animal-dimension 200 100 100)))

;; a ZooAnimal is one of the following structures:
;; - (make-spider PositiveNumber AnimalDimension)
;; - (make-elephant AnimalDimension)
;; - (make-boa PositiveNumber PositiveNumber)
;; - (make-armadillo PositiveNumber AnimalDimension)
;; describes a valid ZooAnimal entry.

;;; function definitions

;; PositiveNumber CageVolume -> Boolean
;; checks to see if size of a given zoo animal fits within
;; volume of a given cage.
(check-expect (fits? SPIDER-TEST CAGE-DIMENSION-TEST) #t)
(check-expect (fits? ELEPHANT-TEST CAGE-DIMENSION-TEST) #f)
(check-expect (fits? BOA-TEST CAGE-DIMENSION-TEST) #t)
(check-expect (fits? ARMADILLO-TEST CAGE-DIMENSION-TEST) #t)

(define (fits? zoo-animal cage)
  (<= (size-of zoo-animal) (volume-of cage)))

;; CageDimension -> PositiveNumber
;; computes the size of cages.
(check-expect (volume-of CAGE-DIMENSION-TEST) 64000000)

(define (volume-of given-cage)
  (* (cage-dimension-x given-cage)
     (cage-dimension-y given-cage)
     (cage-dimension-z given-cage)))

;; ZooAnimal -> PositiveNumber
;; computes the size of the given ZooAnimal.
(check-expect (size-of SPIDER-TEST) (compute-spider-size (spider-dimensions SPIDER-TEST)))
(check-expect (size-of ELEPHANT-TEST) (compute-elephant-size (elephant-dimensions ELEPHANT-TEST)))
(check-expect (size-of BOA-TEST) (compute-boa-size (boa-x BOA-TEST) (boa-girth BOA-TEST)))
(check-expect (size-of ARMADILLO-TEST) (compute-armadillo-size (armadillo-tail-length ARMADILLO-TEST)
                                                               (armadillo-dimensions ARMADILLO-TEST)))

(define (size-of zoo-animal)
  (cond [(spider? zoo-animal)
         (compute-spider-size (spider-dimensions zoo-animal))]
        [(elephant? zoo-animal)
         (compute-elephant-size (elephant-dimensions zoo-animal))]
        [(boa? zoo-animal)
         (compute-boa-size (boa-x zoo-animal) (boa-girth zoo-animal))]
        [(armadillo? zoo-animal)
         (compute-armadillo-size (armadillo-tail-length zoo-animal)
                                 (armadillo-dimensions zoo-animal))]))

;; Spider -> PositiveNumber
;; computes the size of Spiders.
(check-expect (compute-spider-size (spider-dimensions SPIDER-TEST)) 50000)

(define (compute-spider-size given-spider-dimensions)
  (* (animal-dimension-x given-spider-dimensions)
     (animal-dimension-y given-spider-dimensions)
     (animal-dimension-z given-spider-dimensions)))

;; Elephant -> PositiveNumber
;; computes the size of Elephants.
(check-expect (compute-elephant-size (elephant-dimensions ELEPHANT-TEST)) 125000000)

(define (compute-elephant-size given-elephant-dimensions)
  (* (animal-dimension-x given-elephant-dimensions)
     (animal-dimension-y given-elephant-dimensions)
     (animal-dimension-z given-elephant-dimensions)))

;; Boa -> PositiveNumber
;; computes the size of Boas.
(check-expect (compute-boa-size (boa-x BOA-TEST) (boa-girth BOA-TEST)) 238732)

(define (compute-boa-size given-boa-x given-boa-girth)
  (exact-round
   (* (* (sqr (/ (/ given-boa-girth pi) 2)) pi) given-boa-x)))

;; Armadillo -> PositiveNumber
;; computes the size of Armadillos.
(check-expect (compute-armadillo-size
               (armadillo-tail-length ARMADILLO-TEST)
               (armadillo-dimensions ARMADILLO-TEST))
              2300000)

(define (compute-armadillo-size given-armadillo-tail-length given-armadillo-dimensions)
  (* (+ (animal-dimension-x given-armadillo-dimensions)
        given-armadillo-tail-length)
     (animal-dimension-y given-armadillo-dimensions)
     (animal-dimension-z given-armadillo-dimensions)))

(test)
