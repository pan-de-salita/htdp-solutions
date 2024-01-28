#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)

;;; constant and structure definitions

;; a Vehicle is a structure:
;;  (make-vehicle PositiveNumber String PositiveNumber)
;; describes the following of a vehicle:
;; - passenger capacity
;; - license plate number
;; - fuel consumption (miles per gallon)
(define-struct vehicle [capacity license-plate fuel-consumption])

(define AUTOMOBILE-TEST (make-vehicle 5 "XYZ123" 33))
(define VAN-TEST (make-vehicle 11 "ABC321" 48))
(define BUS-TEST (make-vehicle 90 "DEF456" 10))
(define SUV-TEST (make-vehicle 11 "EFG123" 31))

;;; function definitions

;; Vehicle -> String
;; if input is a Vehicle, identifies the type of Vehicle consumed;
;; types of acceptable Vehicles:
;; - automobile
;; - van
;; - bus
;; - suv
(check-expect (type-of AUTOMOBILE-TEST) (identify AUTOMOBILE-TEST))
(check-expect (type-of VAN-TEST) (identify VAN-TEST))
(check-expect (type-of BUS-TEST) (identify BUS-TEST))
(check-expect (type-of SUV-TEST) (identify SUV-TEST))
(check-expect (type-of (make-posn 1 2)) "not a vehicle")

(define (type-of machine)
  (if (vehicle? machine)
      (identify machine)
      "not a vehicle"))

;; Vehicle -> String
;; identifies the type of Vehicle consumed.
(check-expect (identify AUTOMOBILE-TEST) "automobile")
(check-expect (identify VAN-TEST) "van")
(check-expect (identify BUS-TEST) "bus")
(check-expect (identify SUV-TEST) "suv")

(define (identify machine)
  (cond [(and (...(vehicle-capacity machine)...)
              (...(vehicle-license-plate machine)...)
              (...(vehicle-fuel-consumpton machine)...))
         "automobile"]
        [(and (...(vehicle-capacity machine)...)
              (...(vehicle-license-plate machine)...)
              (...(vehicle-fuel-consumpton machine)...))
         "van"]
        [(and (...(vehicle-capacity machine)...)
              (...(vehicle-license-plate machine)...)
              (...(vehicle-fuel-consumpton machine)...))
         "bus"]
        [(and (...(vehicle-capacity machine)...)
              (...(vehicle-license-plate machine)...)
              (...(vehicle-fuel-consumpton machine)...))
         "suv"]))
